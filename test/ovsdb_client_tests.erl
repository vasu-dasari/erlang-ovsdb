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

-export([setup/0, teardown/1, test_setup/0, verify_ovs/2, ovs_cmd/1]).
-export([sync_call/3, wait_sync_call/1]).

protocol_test_() ->
    {setup,
        fun setup/0,
        fun teardown/1,
        fun(S) -> {foreach, fun test_setup/0,
            [{N, fun() -> F(S) end} || {N, F} <- [
                {"echo",                        fun echo/1}
                ,{"list_dbs",                   fun list_dbs/1}
                ,{"get_schema",                 fun get_schema/1}
                ,{"get_schema_version",         fun get_schema_version/1}
                ,{"list_tables",                fun list_tables/1}
                ,{"list_columns",               fun list_columns/1}
                ,{"transaction",                fun transaction/1}
                ,{"dump entire db",             fun dump_db/1}
                ,{"dump a table",               fun dump_table_db/1}
                ,{"dump a column of a table ",  fun dump_table_column_db/1}
            ]]
        } end
    }.

setup() ->
    application:ensure_all_started(ovsdb),
    lager:set_loglevel(lager_console_backend, error),

    ovs_cmd(init),
    ovsdb_utils:ovs_connect(),

    verify_ovs(show, "is_connected: true"),
    #{pid => ovsdb_client}.

teardown(_) ->
    application:stop(ovsdb).

test_setup() ->
    ovs_cmd(init).

echo(Opts) ->
    ?assertEqual({ok,[]}, ovsdb_client:echo(Opts)).

list_dbs(Opts) ->
    ?assertEqual(
        {ok,[<<"Open_vSwitch">>,<<"_Server">>]},
        ovsdb_client:list_dbs(Opts)
    ).

get_schema(Opts) ->
    ?assertMatch(
        {ok, #{<<"name">> := <<"Open_vSwitch">>}},
        ovsdb_client:get_schema(Opts)
    ).

get_schema_version(Opts) ->
    {ok, Version} = ovsdb_client:get_schema_version(Opts),
    verify_ovs(schema_version, binary_to_list(Version)).

list_tables(Opts) ->
    Result = ovsdb_client:list_tables(Opts),
    ?assertMatch({ok, _}, Result),
    {ok, Tables} = Result,
    ?assertEqual(true, lists:member(<<"Bridge">>, Tables)).

list_columns(Opts) ->
    Result = ovsdb_client:list_columns(<<"Bridge">>, Opts),
    ?assertMatch({ok, _}, Result),
    {ok, Columns} = Result,
    ?assertEqual(true, lists:member(<<"datapath_id">>, Columns)).

transaction(Opts) ->
    ?assertMatch(
        {ok, [#{<<"rows">> := [#{<<"bridges">> := _}]}]},
        ovsdb_client:transaction([ovsdb_ops:select([<<"_uuid">>, <<"bridges">>], <<"Open_vSwitch">>, [])], Opts)
    ),
    ?assertMatch(
        {ok, [#{<<"rows">> := _}]},
        ovsdb_client:transaction([ovsdb_ops:select([], <<"Open_vSwitch">>, [])], Opts)
    ).

dump_db(Opts) ->
    ?assertMatch(
        {ok, #{<<"Open_vSwitch">> := _, <<"Bridge">> := _}},
        ovsdb_client:dump(<<>>, [], Opts)
    ).

dump_table_db(Opts) ->
    ?assertMatch(
        {ok, [#{<<"ovs_version">> := _}]},
        ovsdb_client:dump(<<"Open_vSwitch">>, [], Opts)
    ).

dump_table_column_db(Opts) ->
    ?assertMatch(
        {ok, [#{<<"ovs_version">> := _}]},
        ovsdb_client:dump(<<"Open_vSwitch">>, [<<"ovs_version">>], Opts)
    ).

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

-define(ovs_vsctl, "ovs-vsctl --db  " ++ ovsdb_utils:get_server() ++ " ").
-define(ovsdb_client, "ovsdb-client ").

verify_ovs(Cmd, Match) ->
    Ret = lists:foldl(fun
        (_, [] = Acc) when is_tuple(Match) ->
            case jsone:try_decode(erlang:list_to_binary(ovs_cmd(Cmd))) of
                {ok, #{<<"data">> := Match}} -> ok;
                _ -> timer:sleep(200), Acc
            end;
        (_, [] = Acc) ->
            case re:run(ovs_cmd(Cmd), Match) of
                {match, _} -> ok;
                _ -> timer:sleep(200), Acc
            end;
        (_, Acc) ->
            Acc
    end, [], lists:seq(1, 10)),
    case Ret == ok of
        false ->
            OvsOut = ovs_cmd(Cmd),
            MatchResult = re:run(OvsOut, Match),
            ?assertMatch(
                {match, _},
                MatchResult,
                lists:flatten(io_lib:format("Got ~p, expecting ~p", [OvsOut, Match]))
            );
        _ ->
            ok
    end.

ovs_cmd(Cmd) ->
    {done, _, Output} = cmd(Cmd),
    erlang:binary_to_list(Output).

cmd(init) ->
    erlsh:oneliner(?ovs_vsctl ++ "init");
cmd(show) ->
    erlsh:oneliner(?ovs_vsctl ++ "show");
cmd(list_br) ->
    erlsh:oneliner(?ovs_vsctl ++ "list-br");
cmd({list, Table, Column, Row}) ->
    erlsh:oneliner(?ovs_vsctl ++ " -f json " ++ "--column " ++ Column ++ " list " ++ Table ++ " " ++ Row);
cmd({list_ports, BrName}) ->
    erlsh:oneliner(?ovs_vsctl ++ "list-ports " ++ BrName);
cmd({list_ifaces, BrName}) ->
    erlsh:oneliner(?ovs_vsctl ++ "list-ifaces " ++ BrName);
cmd(schema_version) ->
    erlsh:oneliner(?ovsdb_client ++ "get-schema-version " ++ ovsdb_utils:get_server());
cmd({vsctl, Cmd}) when is_list(Cmd) ->
    erlsh:oneliner(?ovs_vsctl ++ Cmd);
cmd({ovsdb, Cmd}) when is_list(Cmd) ->
    erlsh:oneliner(?ovsdb_client ++ Cmd ++ " " ++ ovsdb_utils:get_server()).
