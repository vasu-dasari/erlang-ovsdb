%%-------------------------------------------------------------------
%% Copyright (c) 2020 Vasu Dasari vdasari@gmail.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at:
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @author Vasu Dasari
%% @doc Helper utilities module
%% @private
%% @end
%% Created : 15. Jun 2020 7:20 AM
%%-------------------------------------------------------------------
-module(ovsdb_utils).
-author("Vasu Dasari").

-include("logger.hrl").
%% API
-export([pretty_print/1, backtrace/0, whocalledme/0, to_binstring/1]).
-export([
    unit_test_ovs/0
    , ovs_connect/0, get_server/0]).

pretty_print(Item) ->
    io_lib:format("~s",[io_lib_pretty:print(Item)]).

backtrace() ->
    try throw({ok,whocalledme})
    catch
        _:_:StackTrace ->
            ?INFO("StackTrace ~n~s",
                [pretty_print(StackTrace)])
    end.

whocalledme() ->
    try throw({ok,whocalledme})
    catch
        _:_:StackTrace ->
            StackTrace
    end.

to_binstring([I|_] = Term) when is_list(Term), not is_binary(I) ->
    erlang:list_to_binary(Term);
to_binstring(Term) when is_atom(Term) ->
    erlang:atom_to_binary(Term, utf8);
to_binstring(Term) when is_number(Term) ->
    erlang:integer_to_binary(Term);
to_binstring(Term) ->
    Term.

ovs_connect() ->
    ok = ovsdb_client:start(get_server(), #{database => <<"Open_vSwitch">>}).

get_server() ->
    os:getenv("OVSDB_SERVER", "tcp:10.1.123.20:6640").

%% Script to do sanity test with OVS. This will be in place till docker based tests are implemented.
unit_test_ovs() ->
    ok = ovsdb_client:start("tcp:10.1.123.20:6640", #{database => <<"Open_vSwitch">>}),
    timer:sleep(1000),

    {ok,_} = ovsdb_client:list_dbs(),
    {ok,_} = ovsdb_client:get_schema(#{database => <<"_Server">>}),
    {ok,_} = ovsdb_client:list_columns(<<"Bridge">>),

    {ok,_} = ovsdb_client:transaction([ovsdb_ops:select([<<"_uuid">>, <<"bridges">>], <<"Open_vSwitch">>, [])]),
    {ok,_} = ovsdb_client:transaction([ovsdb_ops:select([], <<"Open_vSwitch">>, [])], #{}),

    {ok,_} = ovsdb_client:transaction(ovsdb_ops:select("*", <<"Bridge">>, [])),
    {ok,_} = ovsdb_client:transaction(ovsdb_ops:select([<<"flood_vlans">>, <<"name">>], <<"Bridge">>, [])),

    {ok,_} = ovsdb_client:transaction(ovsdb_ops:select("*", <<"Bridge">>, [])),
    {ok,_} = ovsdb_client:transaction(ovsdb_ops:select([<<"flood_vlans">>, <<"name">>], <<"Bridge">>, [])),
    {ok,_} = ovsdb_client:transaction(ovsdb_ops:select([<<"flood_vlans">>, <<"name">>], <<"Bridge">>, [{<<"name">>, <<"==">>, <<"br0">>}])),


    {ok,_} = ovsdb_client:monitor("hello", <<"Bridge">>),
    {ok,_} = ovsdb_client:monitor_cancel("hello"),

    {ok,_} = ovsdb_client:echo(),

    {ok,_} = ovsdb_client:lock("hello"),
    {ok,_} = ovsdb_client:unlock("hello"),

    {ok,_} = ovsdb_client:transaction([ovsdb_ops:select([<<"_uuid">>, <<"bridges">>], <<"Open_vSwitch">>, [])]),

    {ok,_} = ovsdb_client:dump(<<"Port">>, []),
    {ok,_} = ovsdb_client:dump(<<"Open_vSwitch">>, []),
    {ok,_} = ovsdb_client:dump(<<"Bridge">>, []),

    ok = ovsdb_vsctl:vsctl(add_br, #{br_name => <<"br1">>}),
    ok = ovsdb_vsctl:vsctl(add_br, #{br_name => <<"br2">>}),

    ok = ovsdb_vsctl:vsctl(add_port, #{br_name => <<"br1">>, port_name => <<"br1-eth1">>}),
    ok = ovsdb_vsctl:vsctl(add_port, #{br_name => <<"br1">>, port_name => <<"br1-eth2">>}),
    ok = ovsdb_vsctl:vsctl(add_port, #{br_name => <<"br2">>, port_name => <<"br2-eth1">>}),
    ok = ovsdb_vsctl:vsctl(add_port, #{br_name => <<"br2">>, port_name => <<"br2-eth2">>}),

    ok = ovsdb_vsctl:vsctl(del_port, #{br_name => <<"br1">>, port_name => <<"br1-eth1">>}),
    ok = ovsdb_vsctl:vsctl(del_port, #{br_name => <<"br1">>, port_name => <<"br1-eth2">>}),
    ok = ovsdb_vsctl:vsctl(del_port, #{br_name => <<"br2">>, port_name => <<"br2-eth1">>}),
    ok = ovsdb_vsctl:vsctl(del_port, #{br_name => <<"br2">>, port_name => <<"br2-eth2">>}),

    ok = ovsdb_vsctl:vsctl(del_br, #{br_name => <<"br1">>}),
    ok = ovsdb_vsctl:vsctl(del_br, #{br_name => <<"br2">>}),
    ok.
