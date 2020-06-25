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
%%% Created : 25. Jun 2020
%%%-------------------------------------------------------------------

-module(ovsdb_vsctl_tests).
-author("Vasu Dasari").

-include_lib("eunit/include/eunit.hrl").

vsctl_test_() ->
    {setup,
        fun ovsdb_client_tests:setup/0,
        fun ovsdb_client_tests:teardown/1,
        fun(S) -> {foreach, fun ovsdb_client_tests:test_setup/0,
            [{N, fun() -> F(S) end} || {N, F} <- [
                {"add_br",                      fun add_br/1}
                ,{"add_port",                   fun add_port/1}
                ,{"del_port",                   fun del_port/1}
                ,{"del_br",                     fun del_br/1}
            ]]
        } end
    }.

add_br(Opts) ->
    ?assertEqual(
        ok,
        ovsdb_vsctl:add_br(<<"br1">>, Opts)
    ),
    ovsdb_client_tests:verify_ovs(list_br, "br1").

del_br(Opts) ->
    ?assertEqual(
        ok,
        ovsdb_vsctl:del_br(<<"br1">>, Opts)
    ),
    ovsdb_client_tests:verify_ovs(list_br, "").

add_port(Opts) ->
    ?assertEqual(
        ok,
        ovsdb_vsctl:add_port(<<"br1">>, <<"br1-eth1">>, Opts)
    ),
    ovsdb_client_tests:verify_ovs({list_ports, "br1"}, "br1-eth1").

del_port(Opts) ->
    ?assertEqual(
        ok,
        ovsdb_vsctl:del_port(<<"br1">>, <<"br1-eth1">>, Opts)
    ),
    ovsdb_client_tests:verify_ovs({list_ports, "br1"}, "").

