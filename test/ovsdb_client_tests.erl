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

-define(OVSDB_Server_Ip, {127, 0, 0, 1}).
-define(Server_Port, 20000).

protocol_test_() -> {
    foreach,
    fun setup/0,
    fun teardown/1,
    [
        fun echo/1,
        fun echo_reply/1,
        fun list_dbs/1,
        fun get_schema/1
    ]
}.

setup() ->
    application:ensure_all_started(ovsdb),
    lager:set_loglevel(lager_console_backend, error),


    {ok, ServerSocket} = gen_tcp:listen(?Server_Port,
        [binary, {reuseaddr, true}, {active, false},
            {ip, ?OVSDB_Server_Ip}]),

    ok = ovsdb_client:start(?OVSDB_Server_Ip, ?Server_Port,
        #{database => <<"Open_vSwitch">>}),
    {ok, ClientSocket} = gen_tcp:accept(ServerSocket),
    {ovsdb_client, ServerSocket, ClientSocket}.

teardown({_Tid, ServerSocket, ClientSocket}) ->
    ok = gen_tcp:close(ServerSocket),
    ok = gen_tcp:close(ClientSocket),
    application:stop(ovsdb).

echo({_Pid, _ServerSocket, ClientSocket}) ->
    {"echo",
        fun() ->
            ok = gen_tcp:send(ClientSocket, packet(echo)),
            {ok, BinMsg} = gen_tcp:recv(ClientSocket, 0),
            ?assertMatch(BinMsg, packet(echo_reply)),
            gen_tcp:close(ClientSocket)
        end}.

echo_reply({_Pid, _ServerSocket, ClientSocket}) ->
    {"echo_reply",
        fun() ->
            Ref = sync_call(ovsdb_client, echo, []),
            {ok, BinMsg} = gen_tcp:recv(ClientSocket, 0),
            ok = gen_tcp:send(ClientSocket, packet(echo_reply)),
            ?assertMatch(BinMsg, packet(echo)),
            ?assertMatch({ok,[]}, wait_sync_call(Ref)),
            gen_tcp:close(ClientSocket)
        end}.

list_dbs({_Pid, _ServerSocket, ClientSocket}) ->
    {"list_dbs",
        fun() ->
            generic_method(ClientSocket, list_dbs, [])
        end}.

get_schema({_Pid, _ServerSocket, ClientSocket}) ->
    {"get_schema",
        fun() ->
            generic_method(ClientSocket, get_schema, [#{}])
        end
    }.

generic_method(ClientSocket, Method, Args) ->
    Ref = sync_call(ovsdb_client, Method, Args),
    {ok, BinMsg} = gen_tcp:recv(ClientSocket, 0),
    Message = jsone:try_decode(BinMsg),
    ?assertMatch({ok, #{}, <<>>}, Message),
    MethodBin = erlang:atom_to_binary(Method, utf8),
    {ok, #{<<"id">> := Id, <<"method">> := MethodBin}, <<>>} = Message,
    ok = gen_tcp:send(ClientSocket, jsone:encode(#{
        <<"id">> => Id,
        <<"result">> => [<<"Hello">>, <<"World">>],
        <<"error">> => null
    })),
    ?assertMatch({ok, [<<"Hello">>, <<"World">>]}, wait_sync_call(Ref)),
    gen_tcp:close(ClientSocket).

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

