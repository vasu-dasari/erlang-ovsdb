%%% Copyright (c) 2020 Vasu Dasari vdasari@gmail.com
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at:
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @author Vasu Dasari
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2020
%%%-------------------------------------------------------------------

-module(ovsdb_client_tests).
-author("Vasu Dasari").

-include_lib("eunit/include/eunit.hrl").

-export([ setup/0, teardown/1, sync_call/3, wait_sync_call/1]).
-export([packet/1]).

protocol_test_() -> {
    foreach,
    fun setup/0,
    fun teardown/1,
    [
        fun echo/1,
        fun list_dbs/1,
        fun get_schema/1,
        fun get_schema_version/1
    ]
}.

setup() ->
    application:ensure_all_started(ovsdb),
    lager:set_loglevel(lager_console_backend, error),

    ovs_cmd(init),
    ok = ovsdb_client:start(get_server(),
        #{database => <<"Open_vSwitch">>}),

    verify_ovs(show, "is_connected: true"),
    #{pid => ovsdb_client}.

teardown(_) ->
    application:stop(ovsdb).

echo(Opts) ->
    {"echo",
        fun() ->
            ?assertEqual({ok,[]}, ovsdb_client:echo(Opts))
        end
    }.

list_dbs(Opts) ->
    {"list_dbs",
        fun() ->
            ?assertEqual(
                {ok,[<<"Open_vSwitch">>,<<"_Server">>]},
                ovsdb_client:list_dbs(Opts)
            )
        end}.

get_schema(Opts) ->
    {"get_schema",
        fun() ->
            ?assertMatch(
                {ok, #{<<"name">> := <<"Open_vSwitch">>}},
                ovsdb_client:get_schema(Opts)
            )
        end
    }.

get_schema_version(Opts) ->
    {"get_schema_version",
        fun() ->
            {ok, Version} = ovsdb_client:get_schema_version(Opts),
            verify_ovs(schema_version, binary_to_list(Version))
        end
    }.

%%%===================================================================
%%% Helper functions
%%%===================================================================

sync_call(M, F, A) ->
    Ref = make_ref(),
    Parent = self(),
    spawn(fun
        () ->
            Parent ! {sync_call, Ref, apply(M, F, A)}
    end),
    Ref.

wait_sync_call(Ref) ->
    receive
        {sync_call, Ref, Reply} -> Reply
    end.

packet(echo) -> <<
    16#7b, 16#22, 16#69, 16#64, 16#22, 16#3a, 16#22, 16#65, 16#63, 16#68, 16#6f, 16#22, 16#2c, 16#22, 16#6d, 16#65,
    16#74, 16#68, 16#6f, 16#64, 16#22, 16#3a, 16#22, 16#65, 16#63, 16#68, 16#6f, 16#22, 16#2c, 16#22, 16#70, 16#61,
    16#72, 16#61, 16#6d, 16#73, 16#22, 16#3a, 16#5b, 16#5d, 16#7d
>>;
packet(echo_reply) -> <<
	16#7b, 16#22, 16#65, 16#72, 16#72, 16#6f, 16#72, 16#22, 16#3a, 16#6e, 16#75, 16#6c, 16#6c, 16#2c, 16#22, 16#69,
	16#64, 16#22, 16#3a, 16#22, 16#65, 16#63, 16#68, 16#6f, 16#22, 16#2c, 16#22, 16#72, 16#65, 16#73, 16#75, 16#6c,
	16#74, 16#22, 16#3a, 16#5b, 16#5d, 16#7d
>>;
packet(_) ->
    error(not_reached).

get_server() ->
    os:getenv("OVSDB_SERVER", application:get_env(ovsdb, eunit_ovsdb_server, "tcp:127.0.0.1:6640")).

-define(ovs_vsctl, "ovs-vsctl --db  " ++ get_server() ++ " ").
-define(ovsdb_client, "ovsdb-client ").

verify_ovs(Cmd, Match) ->
    ?assertEqual(ok, lists:foldl(fun
        (_, [] = Acc) ->
            OvsCmd = ovs_cmd(Cmd),
            case re:run(OvsCmd, Match) of
                {match, _} -> ok;
                _ -> timer:sleep(1000), Acc
            end;
        (_, Acc) ->
            Acc
    end, [], lists:seq(1, 10))).

ovs_cmd(Cmd) ->
    {done, _, Output} = cmd(Cmd),
    erlang:binary_to_list(Output).

cmd(init) ->
    erlsh:oneliner(?ovs_vsctl ++ "init");
cmd(show) ->
    erlsh:oneliner(?ovs_vsctl ++ "show");
cmd(schema_version) ->
    erlsh:oneliner(?ovsdb_client ++ "get-schema-version " ++ get_server()).
