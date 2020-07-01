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
%% @doc Module providing APIs in lines similar to that of ovs-vsctl utility
%%
%% @end
%% Created : 17. Jun 2020
%%-------------------------------------------------------------------

-module(ovsdb_vsctl).
-author("Vasu Dasari").

-include("ovsdb_client.hrl").

%% API
-export([
    add_br/2, del_br/2
    , add_port/3, del_port/3
    , add_bond/4, del_bond/3
    , add_bond_iface/4, del_bond_iface/4
    , add_tunnel_port/4, del_tunnel_port/3
]).

-export([vsctl/1, vsctl/2]).

-export([process_vsctl/3, trace/1]).

-export([
]).

-type vsctl_returns()       :: ok | error | {ok, term()} | {error, term()}.
-type tunnel_type()         :: gre | vxlan.

%% @doc Add/Modify a bridge to switch
%%
%% This is quivalent to
%%    $ ovs-vsctl --may-exist add-br br1 ...
%% Options Supported:
%%      datapath_id
%%      datapath_type
%%      fail_mode
%%      protocols
-spec add_br(unicode:chardata(), ovsdb_client:opts()) -> vsctl_returns().
add_br(BrName, Opts) ->
    vsctl(add_br, Opts#{br_name => BrName}).

%% @doc Deletes a bridge to switch
%%
%% This is quivalent to
%%    $ ovs-vsctl del-br br1 ...
-spec del_br(unicode:chardata(), ovsdb_client:opts()) -> vsctl_returns().
del_br(BrName, Opts) ->
    vsctl(del_br, Opts#{br_name => BrName}).

%% @doc Add/modify port to a bridge
%%
%% This is quivalent to
%%    $ ovs-vsctl add-port br1 br1-eth1
%% Bridge of type netdev will be created if it does not exists
-spec add_port(unicode:chardata(), unicode:chardata(), ovsdb_client:opts()) -> vsctl_returns().
add_port(BrName, PortName, Opts) ->
    vsctl(add_port, Opts#{br_name => BrName, port_name => PortName, iface_list => [PortName]}).

%% @doc Delete port from a bridge
%%
%% This is quivalent to
%%    $ ovs-vsctl del-port br1 br1-eth1
-spec del_port(unicode:chardata(), unicode:chardata(), ovsdb_client:opts()) -> vsctl_returns().
del_port(BrName, PortName, Opts) ->
    vsctl(del_port, Opts#{br_name => BrName, port_name => PortName, iface_list => [PortName]}).

%% @doc Create or modify bond interface
%%
%% This is quivalent to
%%    $ ovs-vsctl --may-exist add-bond br1 br1-bond1 bond1-eth1 bond2-eth2...
%% Options Supported:
%%      lacp
%%      bond_mode
-spec add_bond(unicode:chardata(), unicode:chardata(), list(), ovsdb_client:opts()) -> ovsdb_client:rpc_return().
add_bond(BrName, BondName, IfaceList, Opts) ->
    bond(add_port, BrName, BondName, IfaceList, Opts).

%% @doc Add an interface to a bond
%%
%% This is equivalent to:
%%    $ ovs-vsctl add-bond-iface br1 br1-eth1
-spec add_bond_iface(unicode:chardata(), unicode:chardata(),
        list() | unicode:chardata(), ovsdb_client:opts()) -> ovsdb_client:rpc_return().
add_bond_iface(BrName, BondName, Iface, Opts) when is_binary(Iface) ->
    add_bond_iface(BrName, BondName, [Iface], Opts);
add_bond_iface(BrName, BondName, IfaceList, Opts) ->
    bond(add_port,  BrName, BondName, IfaceList, Opts).

%% @doc Delete an interface to a bond
%%
%% This is equivalent to:
%%    $ ovs-vsctl del-bond-iface br1 br1-eth1
-spec del_bond_iface(unicode:chardata(), unicode:chardata(),
        list() | unicode:chardata(), ovsdb_client:opts()) -> ovsdb_client:rpc_return().
del_bond_iface(BrName, BondName, Iface, Opts) when is_binary(Iface) ->
    del_bond_iface(BrName, BondName, [Iface], Opts);
del_bond_iface(BrName, BondName, IfaceList, Opts) ->
    bond(del_bond_iface,  BrName, BondName, IfaceList, Opts).

%% @doc Delete a bond port
%%
%% This is equivalent to:
%%    $ ovs-vsctl del-bond br1 br1-bond1
-spec del_bond(unicode:chardata(), unicode:chardata(), ovsdb_client:opts()) -> ovsdb_client:rpc_return().
del_bond(BrName, BondName, Opts) ->
    del_port(BrName, BondName, Opts).

bond(Op, BrName, BondName, IfaceList, Opts) ->
    vsctl(Op, Opts#{
        br_name => BrName, port_name => BondName, iface_list => IfaceList
    }).

%% @doc Add/Modify OVS Tunnel port
%%
%% This comand is equivalent to:
%%      $ ovs-vsctl add-port br0 a1_b1 -- \
%%              set interface a1_b1 type=vxlan options:key=1 options:remote_ip=172.31.1.1
%% Options supported are:
%%      key, remote_ip, local_ip, dst_mac, src_mac, vlan_id, out_port
-spec add_tunnel_port(unicode:chardata(), unicode:chardata(), tunnel_type(), ovsdb_client:opts()) -> ovsdb_client:rpc_return().
add_tunnel_port(BrName, Name, TunnelType, Opts) ->
    tunnel(add_port, BrName, Name, TunnelType, Opts).

%% @doc Delete a tunnel port
%%
%%
-spec del_tunnel_port(unicode:chardata(), unicode:chardata(), ovsdb_client:opts()) -> ovsdb_client:rpc_return().
del_tunnel_port(BrName, Name, Opts) ->
    vsctl(del_port, Opts#{br_name => BrName, port_name => Name, iface_list => [Name]}).

tunnel(Op, BrName, Name, TunnelType, Opts) ->
    vsctl(Op, Opts#{
        br_name => BrName, port_name => Name, type => TunnelType, iface_list => [Name]
    }).

%% @private
vsctl(Cmd) -> vsctl(Cmd, #{}).

%% @private
vsctl(Op, #{br_name := BrName} = Opts) when Op == add_br; Op == del_br ->
    BrInfo = case ovsdb_client:transaction(
        ovsdb_ops:select("*", <<"Bridge">>, [{<<"name">>, <<"==">>, BrName}]), Opts) of
        {ok,[#{<<"rows">> := [B]}]} when is_map(B) ->
            B;
        _ ->
            #{}
    end,
    do_vsctl(Op, BrInfo, Opts);
vsctl(Op, #{br_name := BrName, port_name := PortName} = Opts)
    when Op == add_port; Op == del_port; Op == del_bond_iface  ->

    Request = [
        ovsdb_ops:select("*", <<"Bridge">>, [{<<"name">>, <<"==">>, BrName}]),
        ovsdb_ops:select("*", <<"Port">>, [{<<"name">>, <<"==">>, PortName}])
    ] ++ case maps:get(iface_list, Opts, []) of
        [] -> [];
        IfaceList ->
            lists:foldl(fun
                (Iface, Acc) ->
                    [ovsdb_ops:select("*", <<"Interface">>, [{<<"name">>, <<"==">>, Iface}]) | Acc]
            end, [], IfaceList)
    end,
    {ok, Response} = ovsdb_client:transaction(Request, Opts),

    DbInfo = lists:foldl(fun
        ({#{table := Table}, #{<<"rows">> := [Rsp]}}, Acc) when Table == <<"Bridge">>; Table == <<"Port">> ->
            Acc#{Table => Rsp};
        ({#{table := Table, where := [[_,_,Iface]]}, #{<<"rows">> := [Rsp]}}, Acc) ->
            OldIfaces = maps:get(Table, Acc, #{}),
            Acc#{Table => OldIfaces#{Iface => Rsp}};
        (_, Acc) ->
            Acc
    end, #{<<"Bridge">> => #{}, <<"Port">> => #{}, <<"Interface">> => #{}}, lists:zip(Request, Response)),

    case {maps:get(<<"Bridge">>, DbInfo), maps:get(<<"Port">>, DbInfo)}  of
        {B, _} when (map_size(B) == 0) and (Op == add_port) ->
            vsctl(add_br, Opts),
            vsctl(Op, Opts);
        {B, P} when ((map_size(B) == 0) or (map_size(P) == 0)) and ((Op == del_port) or (Op == del_bond_iface))->
            {error, invalid_configuration};
        _ ->
            do_vsctl(Op, DbInfo, Opts)
    end;

vsctl(Cmd, _) ->
    ?INFO("~p: Not supported", [Cmd]),
    not_yet.

%% @private
process_vsctl(Cmd, Opts, State) ->
    ?INFO("~p: Opts ~p", [Cmd, Opts]),
    State.

do_vsctl(Op, BrInfo, Opts) when Op == add_br; Op == del_br ->
    bridge_cmd(Op, BrInfo, Opts);
do_vsctl(Op, #{<<"Bridge">> := BrInfo, <<"Port">> := PortInfo, <<"Interface">> := IfaceList},
        Opts) when Op == add_port; Op == del_port ->
    port_cmd(Op, BrInfo, PortInfo, IfaceList,  Opts);
do_vsctl(Op, #{<<"Bridge">> := BrInfo, <<"Port">> := PortInfo, <<"Interface">> := IfaceList},
        Opts) when Op == del_bond_iface ->
    bond_cmd(Op, BrInfo, PortInfo, IfaceList, Opts);
do_vsctl(_Op, _BrInfo, _Opts) ->
    ok.

bridge_cmd(del_br, #{<<"_uuid">> := Br_Uuid}, #{br_name := BrName} = Opts) ->
    {ok, [#{<<"_uuid">> := Open_vSwitch_Uuid} = OvSInfo]} =
        ovsdb_client:dump(<<"Open_vSwitch">>, ['_uuid', bridges]),

    {ok, _} = ovsdb_client:transaction([
        ovsdb_ops:delete(<<"Bridge">>, [{name, '==', BrName}]),
        ovsdb_ops:update(<<"Open_vSwitch">>,
            [{<<"_uuid">>, <<"==">> ,Open_vSwitch_Uuid}],
            #{bridges => [set, get_set(OvSInfo, <<"bridges">>) -- [Br_Uuid]]}),
        ovsdb_ops:mutate(<<"Open_vSwitch">>,
            [{<<"_uuid">>, <<"==">> ,Open_vSwitch_Uuid}],
            [{<<"next_cfg">>, <<"+=">>, 1}]),
        ovsdb_ops:commit(true)
    ], Opts),
    ok;

bridge_cmd(add_br, BrInfo, #{br_name := BrName} = Opts) when map_size(BrInfo) == 0 ->
    {ok, [#{<<"_uuid">> := Open_vSwitch_Uuid} = OvSInfo]} =
        ovsdb_client:dump(<<"Open_vSwitch">>, ['_uuid', bridges]),

    InterfaceOps = ovsdb_ops:insert(<<"Interface">>,
        #{name => BrName, type => <<"internal">>},
        ovsdb_utils:uuid(interfaces, BrName)),

    PortOps = ovsdb_ops:insert(<<"Port">>,
        #{name => BrName, interfaces => [<<"named-uuid">>, ovsdb_utils:uuid(interfaces, BrName)]},
        ovsdb_utils:uuid(port, BrName)),

    BaseBrInfo = #{name => BrName, ports => [<<"named-uuid">>, ovsdb_utils:uuid(port, BrName)]},

    NewBrInfo = get_extra_config(bridge, BaseBrInfo, Opts),

    BridgeOps = ovsdb_ops:insert(<<"Bridge">>,
        maps:merge(NewBrInfo, BaseBrInfo), ovsdb_utils:uuid(bridge, BrName)),

    OvS_Ops = ovsdb_ops:update(<<"Open_vSwitch">>,
        [{<<"_uuid">>, <<"==">>, Open_vSwitch_Uuid}],
        #{bridges => [set, [[<<"named-uuid">>, ovsdb_utils:uuid(bridge, BrName)] | get_set(OvSInfo, <<"bridges">>)]]}),

    Request = get_wait_ops(<<"Open_vSwitch">>, OvSInfo, #{<<"bridges">> => ""}) ++
        [InterfaceOps, PortOps, BridgeOps, OvS_Ops] ++ [
        ovsdb_ops:mutate(<<"Open_vSwitch">>,
            [{<<"_uuid">>, <<"==">>, Open_vSwitch_Uuid}],
            [{<<"next_cfg">>, <<"+=">>, 1}]),
        ovsdb_ops:commit(true)
    ],
    transaction(Request);

bridge_cmd(add_br, #{<<"_uuid">> := Br_Uuid} = BrInfo, Opts) ->
    case get_extra_config(bridge, BrInfo, Opts) of
        NewBrInfo when map_size(NewBrInfo) /= 0 ->
            BridgeOps = ovsdb_ops:update(<<"Bridge">>,
                [{<<"_uuid">>, <<"==">>, Br_Uuid}], NewBrInfo),
            Request = [BridgeOps] ++ [
                ovsdb_ops:mutate(<<"Open_vSwitch">>,
                    [{<<"_uuid">>, <<"==">>, get_Open_vSwitch_Uuid(Opts)}],
                    [{<<"next_cfg">>, <<"+=">>, 1}]),
                ovsdb_ops:commit(true)
            ],
            transaction(Request);
        _ ->
            ok
    end;

bridge_cmd(_Op, _PortInfo, _Opts) ->
    ?INFO("Unhandled ~p", [{_Op, _PortInfo, _Opts}]),
    ok.

port_cmd(add_port,
        #{<<"_uuid">> := Br_Uuid} = BrInfo,
        PortInfo, IfaceInfo,
        #{port_name := PortName, iface_list := IfaceList} = Opts) when map_size(PortInfo) == 0 ->

    #{ops := IfaceOps, uuids := IfaceUuids} = lists:foldl(fun
        (Iface, #{ops := OpsAcc, uuids := UuidsAcc} = Acc) ->
            case maps:get(Iface, IfaceInfo, #{}) of
                R when map_size(R) == 0 ->
                    ExtraConfig = get_extra_config(interface, #{}, Opts),
                    Acc#{
                        ops => [
                            ovsdb_ops:insert(<<"Interface">>,
                                ExtraConfig#{name => Iface},
                                ovsdb_utils:uuid(interface, Iface))
                            | OpsAcc],
                        uuids => [[<<"named-uuid">>, ovsdb_utils:uuid(interface, Iface)] | UuidsAcc]
                    };
                _ ->
                    Acc
            end
    end, #{ops => [], uuids => []}, IfaceList),

    PortOps = case IfaceOps /= [] of
        true when map_size(PortInfo) == 0 ->
            ExtraConfig = get_extra_config(port, #{}, Opts),

            PortInsert = ovsdb_ops:insert(<<"Port">>,
                ExtraConfig#{name => PortName,
                    interfaces => [set, IfaceUuids]
                }, ovsdb_utils:uuid(port, PortName)),
            BridgeUpdate = ovsdb_ops:update(
                <<"Bridge">>,
                [{<<"_uuid">>, <<"==">>, Br_Uuid}],
                #{ports => [set, [[<<"named-uuid">>, ovsdb_utils:uuid(port, PortName)] | get_set(BrInfo, <<"ports">>)]]}
            ),
            [PortInsert, BridgeUpdate];
        true ->
            [ovsdb_ops:update(
                <<"Port">>,
                [{<<"_uuid">>, <<"==">>, maps:get(<<"_uuid">>, PortInfo)}],
                #{interfaces => [set, IfaceUuids ++ get_set(PortInfo, <<"interfaces">>)]})];
        _ ->
            []
    end,

    case PortOps == [] of
        true ->
            ok;
        _ ->
            Request = get_wait_ops(<<"Bridge">>, BrInfo, #{<<"ports">> => ""}) ++
                IfaceOps ++ PortOps ++ [
                ovsdb_ops:mutate(<<"Open_vSwitch">>,
                    [{<<"_uuid">>, <<"==">>, get_Open_vSwitch_Uuid(Opts)}],
                    [{<<"next_cfg">>, <<"+=">>, 1}]),
                ovsdb_ops:commit(true)
            ],
            transaction(Request)
    end;

port_cmd(add_port, _BrInfo,
        #{<<"_uuid">> := Port_Uuid} = PortInfo,
        IfaceInfo, #{iface_list := IfaceList} = Opts) ->

    IfaceOps = lists:foldl(fun
        (IfName, Acc) ->
            case maps:get(IfName, IfaceInfo, #{}) of
                #{<<"_uuid">> := If_Uuid} = IfaceMap ->
                    case get_extra_config(interface, IfaceMap, Opts) of
                        NewIfInfo when map_size(NewIfInfo) /= 0 ->
                            [ovsdb_ops:update(<<"Interface">>,
                                    [{<<"_uuid">>, <<"==">>, If_Uuid}], NewIfInfo)];
                        _ ->
                            []
                    end;
                _ ->
                    IfUuid = ovsdb_utils:uuid(interface, IfName),
                    NewIfMap = get_extra_config(interface, #{}, Opts),
                    [
                        ovsdb_ops:insert(<<"Interface">>, NewIfMap#{name => IfName}, IfUuid),
                        ovsdb_ops:update(
                            <<"Port">>,
                            [{<<"_uuid">>, <<"==">>, maps:get(<<"_uuid">>, PortInfo)}],
                            #{interfaces => [set, [[<<"named-uuid">>, IfUuid] | get_set(PortInfo, <<"interfaces">>)]]})
                    ]
            end ++ Acc
    end, [], IfaceList),

    PortOps = case get_extra_config(port, PortInfo, Opts) of
        NewPortInfo when map_size(NewPortInfo) /= 0 ->
            [ovsdb_ops:update(<<"Port">>,
                [{<<"_uuid">>, <<"==">>, Port_Uuid}], NewPortInfo)];
        _ ->
            []
    end,

    case PortOps ++ IfaceOps of
        [] ->
            ok;
        Ops ->
            Request = Ops ++ [
                ovsdb_ops:mutate(<<"Open_vSwitch">>,
                    [{<<"_uuid">>, <<"==">>, get_Open_vSwitch_Uuid(Opts)}],
                    [{<<"next_cfg">>, <<"+=">>, 1}]),
                ovsdb_ops:commit(true)
            ],
            transaction(Request)
    end;

port_cmd(Op,
        #{<<"_uuid">> := Br_Uuid, <<"ports">> := [<<"set">>, Ports]},
        #{<<"_uuid">> := Port_Uuid}, _,
        #{port_name := PortName} = Opts) when Op == del_port; Op == del_bond ->

    {ok,_} = ovsdb_client:transaction([
        ovsdb_ops:delete(<<"Port">>, [{name, '==', PortName}]),
        ovsdb_ops:update(<<"Bridge">>,
            [{<<"_uuid">>, <<"==">> ,Br_Uuid}],
            #{ports => [set, Ports -- [Port_Uuid]]}),
        ovsdb_ops:mutate(<<"Open_vSwitch">>,
            [{<<"_uuid">>, <<"==">> ,get_Open_vSwitch_Uuid(Opts)}],
            [{<<"next_cfg">>, <<"+=">>, 1}]),
        ovsdb_ops:commit(true)
    ], Opts),
    ok;

port_cmd(_Op, _BrInfo, _PortInfo, _, _Opts) ->
    ?INFO("Unhandled ~p", [{_Op, _BrInfo, _PortInfo, _Opts}]),
    ok.

bond_cmd(del_bond_iface, _, PortInfo, IfaceInfo, Opts) when is_map(PortInfo) ->
    #{ops := IfaceOps, uuids := IfaceUuids} = maps:fold(fun
        (Iface, #{<<"_uuid">> := Uuid}, #{ops := OpsAcc, uuids := UuidsAcc} = Acc) ->
            Acc#{
                ops => [ovsdb_ops:delete(<<"Interface">>, [{name, '==', Iface}]) | OpsAcc],
                uuids => [Uuid | UuidsAcc]
            }
    end, #{ops => [], uuids => []}, IfaceInfo),

    PortOps = case IfaceOps /= [] of
        true when map_size(PortInfo) == 0 ->
            [];
        true ->
            [ovsdb_ops:update(
                <<"Port">>,
                [{<<"_uuid">>, <<"==">>, maps:get(<<"_uuid">>, PortInfo)}],
                #{interfaces => [set, get_set(PortInfo, <<"interfaces">>) -- IfaceUuids]})];
        _ ->
            []
    end,

    case PortOps == [] of
        true ->
            ok;
        _ ->
            Request = IfaceOps ++ PortOps ++ [
                ovsdb_ops:mutate(<<"Open_vSwitch">>,
                    [{<<"_uuid">>, <<"==">>, get_Open_vSwitch_Uuid(Opts)}],
                    [{<<"next_cfg">>, <<"+=">>, 1}]),
                ovsdb_ops:commit(true)
            ],
            transaction(Request)
    end;

bond_cmd(_Op, _BrInfo, _PortInfo, _IfaceList, _Opts) ->
    ?INFO("bond_cmd: Unhandled~n~s", [ovsdb_utils:pretty_print({_Op, _BrInfo, _PortInfo, _IfaceList, _Opts})]),
    ok.

transaction(Request) ->
    {ok,Response} = ovsdb_client:transaction(Request),

    ?DEBUG("Request~n~s", [ovsdb_utils:pretty_print(Request)]),
    ?DEBUG("Response~n~s", [ovsdb_utils:pretty_print(Response)]),
    lists:foldl(fun
        ({Req, #{<<"error">> := _} = Rsp}, _) ->
            ?WARNING("Errors:~n~s", [ovsdb_utils:pretty_print({Req, Rsp})]),
            {error, [Req, Rsp]};
        (_, Acc) ->
            Acc
    end, ok, lists:zip(Request, Response)).

get_Open_vSwitch_Uuid(Opts) ->
    {ok, [#{<<"_uuid">> := Open_vSwitch_Uuid}]} =
        ovsdb_client:dump(<<"Open_vSwitch">>, ['_uuid'], Opts),
    Open_vSwitch_Uuid.

trace(on) ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c)
    ,dbg:tpl(?MODULE, [])
;

trace(off) ->
    dbg:stop().

get_set(Map, Key) ->
    case maps:get(Key, Map, not_found) of
        [<<"set">>, Value] ->
            Value;
        not_found ->
            [];
        Value ->
            [Value]
    end.

get_extra_config(bridge, BrInfo, Opts) ->
    maps:fold(fun
        (Key, Value, Acc) when Key == datapath_id ->
            get_extra_other_config(<<"other_config">>, Key, Value, Acc);
        (Key, Value, Acc) when
            Key == datapath_type;
            Key == fail_mode
            ->
            get_extra_config(BrInfo, Key, Value, Acc);
        (Key, Value, Acc) when
            Key == protocols
            ->
            get_extra_config(BrInfo, Key, Value, Acc);
        (_,_, Acc) ->
            Acc
    end, #{}, Opts);
get_extra_config(interface, BrInfo, Opts) ->
    maps:fold(fun
        (Key, Value, Acc) when
            Key == key;
            Key == remote_ip;
            Key == local_ip;
            Key == dst_mac;
            Key == src_mac;
            Key == vlan_id;
            Key == out_port
            ->
            %% key, remote_ip, local_ip, dst_mac, src_mac, vlan_id, out_port
            get_extra_other_config(<<"options">>, Key, Value, Acc);
        (Key, Value, Acc) when
            Key == admin_state;
            Key == type
            ->
            get_extra_config(BrInfo, Key, Value, Acc);
        (_,_, Acc) ->
            Acc
    end, #{}, Opts);
get_extra_config(port, BrInfo, Opts) ->
    maps:fold(fun
        (Key, Value, Acc) when
            Key == bond_mode;
            Key == lacp
            ->
            get_extra_config(BrInfo, Key, Value, Acc);
        (_,_, Acc) ->
            Acc
    end, #{}, Opts).

get_extra_config(DbMap, Key, Value, Opts) ->
    Column = ovsdb_utils:to_binstring(Key),
    case maps:get(Column, DbMap, not_found) of
        V when V =:= Value ->
            Opts;
        _ when is_list(Value)->
            Opts#{Column => [set, Value]};
        _ ->
            Opts#{Column => Value}
    end.

get_extra_other_config(OtherKey, Key, Value, Opts) ->
    [map, OldPairs] = maps:get(OtherKey, Opts, [map, []]),
    Opts#{OtherKey => [map, [[ovsdb_utils:to_binstring(Key), Value] | OldPairs]]}.

get_wait_ops(Tabls, DbMap, NewMap) ->
    maps:fold(fun
        (Key, _Value, Acc) ->
            [ovsdb_ops:wait(Tabls,
                [{<<"_uuid">>, '==', maps:get(<<"_uuid">>, DbMap)}],
                [#{Key => [set, get_set(DbMap, Key)]}],
                '==',
                [Key]
            ) | Acc]
    end, [], NewMap).

