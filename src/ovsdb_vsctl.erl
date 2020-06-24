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
%% @doc Module providing APIs similar to that od ovs-vsctl utility
%%
%% @end
%% Created : 17. Jun 2020
%%-------------------------------------------------------------------

-module(ovsdb_vsctl).
-author("Vasu Dasari").

-include("ovsdb_client.hrl").

%% API
-export([vsctl/1, vsctl/2]).

-export([process_vsctl/3]).

-export([
]).

vsctl(Cmd) -> vsctl(Cmd, #{}).

vsctl(Op, #{br_name := BrName} = Opts) when Op == add_br; Op == del_br ->
    BrInfo = case ovsdb_client:transaction(
        ovsdb_ops:select("*", <<"Bridge">>, [{<<"name">>, <<"==">>, BrName}]), Opts) of
        {ok,[#{<<"rows">> := [B]}]} when is_map(B) ->
            B;
        _ ->
            #{}
    end,
    do_vsctl(Op, BrInfo, Opts);
vsctl(Op, #{br_name := BrName, port_name := PortName} = Opts) when Op == add_port; Op == del_port ->
    {BrInfo, PortInfo} = case ovsdb_client:transaction([
        ovsdb_ops:select("*", <<"Bridge">>, [{<<"name">>, <<"==">>, BrName}]),
        ovsdb_ops:select("*", <<"Port">>, [{<<"name">>, <<"==">>, PortName}])
    ], Opts) of
        {ok,[#{<<"rows">> := [B]}, #{<<"rows">> := [P]}]} when is_map(B), is_map(P) ->
            {B, P};
        {ok,[#{<<"rows">> := [B]}, #{<<"rows">> := []}]} when is_map(B) ->
            {B, #{}};
        Error ->
            ?INFO("Error ~s", [ovsdb_utils:pretty_print(Error)]),
            {#{}, #{}}
    end,
    do_vsctl(Op, {BrInfo, PortInfo}, Opts);
vsctl(Cmd, _) ->
    ?INFO("~p: Not supported", [Cmd]),
    not_yet.

process_vsctl(Cmd, Opts, State) ->
    ?INFO("~p: Opts ~p", [Cmd, Opts]),
    State.

do_vsctl(Op, BrInfo, Opts) when Op == add_br; Op == del_br ->
    bridge_cmd(Op, BrInfo, Opts);
do_vsctl(Op, {BrInfo, PortInfo}, Opts) when Op == add_port; Op == del_port ->
    port_cmd(Op, BrInfo, PortInfo, Opts);
do_vsctl(_Op, _BrInfo, _Opts) ->
    ok.

bridge_cmd(del_br, #{<<"_uuid">> := Br_Uuid}, #{br_name := BrName} = Opts) ->

    {ok, [#{<<"_uuid">> := Open_vSwitch_Uuid, <<"bridges">> := Bridges}]} =
        ovsdb_client:dump(<<"Open_vSwitch">>, ['_uuid', bridges]),

    BridgesSet = case Bridges of
        [<<"set">>, B] -> B;
        _ -> [Bridges]
    end,

    ovsdb_client:transaction([
        ovsdb_ops:delete(<<"Bridge">>, [{name, '==', BrName}]),
        ovsdb_ops:update(<<"Open_vSwitch">>,
            [{<<"_uuid">>, <<"==">> ,Open_vSwitch_Uuid}],
            #{bridges => [set, BridgesSet -- [Br_Uuid]]}),
        ovsdb_ops:mutate(<<"Open_vSwitch">>,
            [{<<"_uuid">>, <<"==">> ,Open_vSwitch_Uuid}],
            [{<<"next_cfg">>, <<"+=">>, 1}]),
        ovsdb_ops:commit(true)
    ], Opts);

bridge_cmd(add_br, BrInfo, #{br_name := BrName} = Opts) when map_size(BrInfo) == 0 ->

    {ok, [#{<<"_uuid">> := Open_vSwitch_Uuid, <<"bridges">> := Bridges}]} =
        ovsdb_client:dump(<<"Open_vSwitch">>, ['_uuid', bridges]),

    Interface = #{name => BrName, type => <<"internal">>},
    Port = #{name => BrName, interfaces => [<<"named-uuid">>, <<"interface_uuid">>]},
    Bridge = #{name => BrName, ports => [<<"named-uuid">>, <<"port_uuid">>], protocols => <<"OpenFlow10">>},

    BridgesSet = case Bridges of
        [<<"set">>, B] -> B;
        _ -> [Bridges]
    end,

    ovsdb_client:transaction([
        ovsdb_ops:insert(<<"Interface">>, Interface, <<"interface_uuid">>),
        ovsdb_ops:insert(<<"Port">>, Port, <<"port_uuid">>),
        ovsdb_ops:insert(<<"Bridge">>, Bridge, <<"bridge_uuid">>),
        ovsdb_ops:update(<<"Open_vSwitch">>,
            [{<<"_uuid">>, <<"==">>, Open_vSwitch_Uuid}],
            #{bridges => [set, BridgesSet ++ [[<<"named-uuid">>, <<"bridge_uuid">>]]]}),
        ovsdb_ops:mutate(<<"Open_vSwitch">>,
            [{<<"_uuid">>, <<"==">>, Open_vSwitch_Uuid}],
            [{<<"next_cfg">>, <<"+=">>, 1}]),
        ovsdb_ops:commit(true)
    ], Opts);

bridge_cmd(_Op, _PortInfo, _Opts) ->
    ?INFO("Unhandled ~p", [{_Op, _PortInfo, _Opts}]),
    {ok, #{}}.

port_cmd(Op, #{<<"ports">> := [<<"uuid">>, _] = Ports} = BrInfo, PortInfo, Opts) ->
    port_cmd(Op, BrInfo#{<<"ports">> := [<<"set">>, [Ports]]}, PortInfo, Opts);

port_cmd(add_port,
        #{<<"_uuid">> := Br_Uuid, <<"ports">> := [<<"set">>, Ports]},
        PortInfo,
        #{port_name := PortName} = Opts) when map_size(PortInfo) == 0 ->

    Interface = #{name => PortName},
    Port = #{name => PortName, interfaces => [<<"named-uuid">>, <<"interface_uuid">>]},
    ovsdb_client:transaction([
        ovsdb_ops:insert(<<"Interface">>, Interface, <<"interface_uuid">>),
        ovsdb_ops:insert(<<"Port">>, Port, <<"port_uuid">>),
        ovsdb_ops:update(<<"Bridge">>,
            [{<<"_uuid">>, <<"==">>, Br_Uuid}],
            #{ports => [set, [[<<"named-uuid">>, <<"port_uuid">>] | Ports]]}),
        ovsdb_ops:mutate(<<"Open_vSwitch">>,
            [{<<"_uuid">>, <<"==">>, get_Open_vSwitch_Uuid(Opts)}],
            [{<<"next_cfg">>, <<"+=">>, 1}]),
        ovsdb_ops:commit(true)
    ], Opts);

port_cmd(del_port,
        #{<<"_uuid">> := Br_Uuid, <<"ports">> := [<<"set">>, Ports]},
        #{<<"_uuid">> := Port_Uuid},
        #{port_name := PortName} = Opts) ->

    ovsdb_client:transaction([
        ovsdb_ops:delete(<<"Port">>, [{name, '==', PortName}]),
        ovsdb_ops:update(<<"Bridge">>,
            [{<<"_uuid">>, <<"==">> ,Br_Uuid}],
            #{ports => [set, Ports -- [Port_Uuid]]}),
        ovsdb_ops:mutate(<<"Open_vSwitch">>,
            [{<<"_uuid">>, <<"==">> ,get_Open_vSwitch_Uuid(Opts)}],
            [{<<"next_cfg">>, <<"+=">>, 1}]),
        ovsdb_ops:commit(true)
    ], Opts);

port_cmd(_Op, _BrInfo, _PortInfo, _Opts) ->
    ?INFO("Unhandled ~p", [{_Op, _BrInfo, _PortInfo, _Opts}]),
    {ok, #{}}.

get_Open_vSwitch_Uuid(Opts) ->
    {ok, [#{<<"_uuid">> := Open_vSwitch_Uuid}]} =
        ovsdb_client:dump(<<"Open_vSwitch">>, ['_uuid'], Opts),
    Open_vSwitch_Uuid.
