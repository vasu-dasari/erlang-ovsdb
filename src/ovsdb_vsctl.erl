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
    set_controller/3, del_controller/2
    , add_br/3, set_br/3, del_br/2
    , add_port/3, set_port/3, set_iface/3, del_port/3
    , get_br/2, get_br/3, get_port/2, get_port/3
    , get_iface/2, get_iface/3, get_ifaces_on_bridge/3
    , add_bond/4, set_bond/3, del_bond/3
    , add_bond_iface/4, del_bond_iface/4
    , add_tunnel_port/5, del_tunnel_port/3
]).

-export([vsctl/1, vsctl/2, set/4, get/4]).

-export([process_vsctl/3, trace/1]).

-export([
]).

-type vsctl_returns()       :: ok | error | {ok, term()} | {error, term()}.
-type tunnel_type()         :: gre | vxlan.
-type table_name()          :: unicode:chardata().
-type table_key()           :: all | unicode:chardata() | {unicode:chardata(), unicode:chardata()}.
-type table_params()        :: map().
-type vsctl_get_returns()   :: vsctl_returns() | list() | map().

%% @doc Set table entries in ovsdb
%%
%% This API is equivalent to executing command of form  ovs-vsctl set Table Table Id Config Tuples
%%  ovs-vsctl set int vxlan1 type=vxlan
-spec set(table_name(), table_key(), table_params(), ovsdb_client:opts()) -> vsctl_returns().
set(Table, Key, Params, Opts) when map_size(Params) /= 0 ->
    vsctl(set, Opts#{table => Table, key => Key, config => Params});
set(_Table, _Key, _Params, _Opts) ->
    ok.

%% @doc Get table entries in ovsdb
%%
%% This API is equivalent to executing command of form  ovs-vsctl --columns Columns list Table Table-Id
-spec get(table_name(), table_key(), list(), ovsdb_client:opts()) -> vsctl_get_returns().
get(Table, Key, [], Opts) ->
    vsctl(get, Opts#{table => Table, key => Key, columns => "*"});
get(Table, Key, Columns, Opts) ->
    vsctl(get, Opts#{table => Table, key => Key,
        columns => [
            ovsdb_utils:to_binstring(C) || C <- Columns
        ]}).

%% @doc Set Openflow Controller to Bridge
%%
%% This is equivalant to
%%      $ ovs-vsctl set-controller br0 tcp:10.1.1.1:6653
-spec set_controller(unicode:chardata(), unicode:chardata(), ovsdb_client:opts()) -> vsctl_returns().
set_controller(BrName, Controller, Opts) ->
    vsctl(add_controller, Opts#{controller => Controller, br_name => BrName}).

%% @doc Delete Openflow Controller from Bridge
%%
%% This is equivalant to
%%      $ ovs-vsctl del-controller br0
-spec del_controller(unicode:chardata(), ovsdb_client:opts()) -> vsctl_returns().
del_controller(BrName, Opts) ->
    vsctl(del_controller, Opts#{br_name => BrName}).

%% @doc Add/Modify a bridge to switch
%%
%% For the API to return ok, when the entry is already present, set may_exist option to true
%% This is quivalent to
%%    $ ovs-vsctl --may-exist add-br br1 ...
-spec add_br(unicode:chardata(), map(), ovsdb_client:opts()) -> vsctl_returns().
add_br(BrName, BrOptions, Opts) ->
    vsctl(add_br, Opts#{br_name => BrName, bridge_config => BrOptions}).

%% @doc Set bridge options
%%
%% This function will return error if bridge does not exist
-spec set_br(table_key(), table_params(), ovsdb_client:opts()) -> vsctl_returns().
set_br(BrName, BrOptions, Opts) ->
    set(<<"Bridge">>, BrName, BrOptions, Opts).

%% @doc Get bridge info from switch
%%
%% This is quivalent to
%%    $ ovs-vsctl del-br br1 ...
-spec get_br(table_key(), ovsdb_client:opts()) -> vsctl_returns().
get_br(BrName, Opts) ->
    get_br(BrName, [], Opts).
get_br(BrName, Columns, Opts) ->
    get(<<"Bridge">>, BrName, Columns, Opts).

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
%% Options supported:
%%      Interface options: admin_state, ofport_request
-spec add_port(unicode:chardata(), unicode:chardata(), ovsdb_client:opts()) -> vsctl_returns().
add_port(BrName, PortName, Opts) ->
    add_port(BrName, PortName, #{}, Opts).

add_port(BrName, PortName, Params, Opts) ->
    case vsctl(add_port, Opts#{br_name => BrName, port_name => PortName, iface_list => [PortName]}) of
        ok ->
            set_port(PortName, Params, Opts);
        R ->
            R
    end.

%% @doc Set port parameters
%%
%% This function will return error if port does not exist
-spec set_port(table_key(), table_params(), ovsdb_client:opts()) -> vsctl_returns().
set_port(PortName, Params, Opts) ->
    set(<<"Port">>, PortName, Params, Opts).

%% @doc Set other interface parameters
%%
%% This function will return error if interface does not exist
-spec set_iface(table_key(), table_params(), ovsdb_client:opts()) -> vsctl_returns().
set_iface(IfName, Params, Opts) ->
    set(<<"Interface">>, IfName, Params, Opts).

%% @doc Get port information
%%
%% Gets port information for PortName. To get information of all ports available use all option.
-spec get_port(table_key(), ovsdb_client:opts()) -> vsctl_returns().
get_port(PortName, Opts) ->
    get_port(PortName, [], Opts).
get_port(PortName, Columns, Opts) ->
    get(<<"Port">>, PortName, Columns, Opts).

%% @doc Get interface information
%%
%% Gets interface information for IfName. To get information of all interfaces available use all option.
get_iface(IfName, Opts) ->
    get_iface(IfName, [], Opts).
get_iface(IfName, Columns, Opts) ->
    get(<<"Interface">>, IfName, Columns, Opts).

%% @doc Get all interfacdes on a bridge
-spec get_ifaces_on_bridge(table_key(), list(), ovsdb_client:opts()) -> [vsctl_get_returns()].
get_ifaces_on_bridge(BrName, Columns, Opts) ->
    Ports = get_set(get(<<"Bridge">>, BrName, [ports], Opts), <<"ports">>),
    Interfaces = [get_set(
        get(<<"Port">>, {<<"_uuid">>, Uuid}, [interfaces], Opts), <<"interfaces">>
    ) || Uuid <- Ports],
    lists:flatten(
        [get(<<"Interface">>, {<<"_uuid">>, Uuid}, Columns, Opts) || [Uuid] <- Interfaces]
    ).

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

%% @doc Modify interface level parameters of a a bond port
%%
%% This function fetches member links of the specified port and then applies the params on all those interfaces
-spec set_bond(table_key(), table_params(), ovsdb_client:opts()) -> vsctl_returns().
set_bond(BondName, Params, Opts) ->
    case get(<<"Port">>, BondName, [interfaces], Opts) of
        #{<<"interfaces">> :=  [<<"set">>, UuidList]} ->
            lists:foldl(fun
                (Uuid, ok) ->
                    set_iface({<<"_uuid">>, Uuid}, Params, Opts);
                (_Uuid, Acc) ->
                    Acc
            end, ok, UuidList);
        _ ->
            {error, does_not_exist}
    end.

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
-spec add_tunnel_port(table_key(), unicode:chardata(), tunnel_type(), table_params(), ovsdb_client:opts()) -> ovsdb_client:rpc_return().
add_tunnel_port(BrName, Name, TunnelType, Params, Opts) ->
    case add_port(BrName, Name, #{}, Opts) of
        ok ->
            set_iface(Name, Params#{type => TunnelType}, Opts);
        Ret ->
            Ret
    end.

%% @doc Delete a tunnel port
%%
%%
-spec del_tunnel_port(unicode:chardata(), unicode:chardata(), ovsdb_client:opts()) -> ovsdb_client:rpc_return().
del_tunnel_port(BrName, Name, Opts) ->
    vsctl(del_port, Opts#{br_name => BrName, port_name => Name, iface_list => [Name]}).

%% @private
vsctl(Cmd) -> vsctl(Cmd, #{}).

vsctl(Op, Opts) ->
    try do_vsctl(Op, Opts) of
        Return ->
            Return
    catch
        Error:Reason:StackTrace ->
            ?ERROR("Failed:~n    Op ~p, Opts ~p~n    Error ~p, Reason ~p~n    StackTrace ~n~s",
                [Op, Opts, Error, Reason, ovsdb_utils:pretty_print(StackTrace)]),
            {error, Error}
    end.

%% @private
do_vsctl(Op, #{br_name := BrName} = Opts) when Op == add_controller ->
    BrMap = get(<<"Bridge">>, BrName, [], Opts),
    do_vsctl(Op, #{<<"Bridge">> => BrMap, <<"Controller">> => #{}}, Opts);

do_vsctl(Op, #{table := Table, key := Key} = Opts) when Op == set ->
    Conditions = case Key of
        {K, V} ->
            [{ovsdb_utils:to_binstring(K), <<"==">>, V}];
        _ ->
            [{<<"name">>, <<"==">>, Key}]
    end,
    case ovsdb_client:transaction(
        ovsdb_ops:select("*", Table, Conditions), Opts) of
        {ok,[#{<<"rows">> := [#{} = Entry]}]} ->
            do_vsctl(Op, Entry, Opts);
        _ ->
            {error, does_not_exist}
    end;

do_vsctl(Op, #{table := Table, key := Key, columns := Columns} = Opts) when Op == get ->
    Conditions = case Key of
        {K, V} ->
            [{ovsdb_utils:to_binstring(K), <<"==">>, V}];
        all ->
            [{<<"name">>, <<"!=">>, <<"">>}];
        _ ->
            [{<<"name">>, <<"==">>, Key}]
    end,
    case ovsdb_client:transaction(
        ovsdb_ops:select(Columns, Table, Conditions), Opts) of
        {ok, List} when Key == all ->
            lists:flatten([maps:get(<<"rows">>, M) || M <- List]);
        {ok,[#{<<"rows">> := [#{} = Entry]}]} ->
            Entry;
        _ ->
            {error, does_not_exist}
    end;

do_vsctl(Op, #{br_name := BrName} = Opts) when Op == del_controller ->
    {ok,[#{<<"rows">> := [#{<<"controller">> := CtrlrUuId} = BrMap]}]} =
        ovsdb_client:transaction(
            ovsdb_ops:select("*", <<"Bridge">>, [{<<"name">>, <<"==">>, BrName}]), Opts),
    {ok,[#{<<"rows">> := [CtrlrMap]}]} =
        ovsdb_client:transaction(
            ovsdb_ops:select("*", <<"Controller">>, [{<<"_uuid">>, <<"==">>, CtrlrUuId}]), Opts),
    do_vsctl(Op, #{<<"Bridge">> => BrMap, <<"Controller">> => CtrlrMap}, Opts);
do_vsctl(Op, #{br_name := BrName} = Opts) when Op == add_br;Op == del_br ->
    BrInfo = case ovsdb_client:transaction(
        ovsdb_ops:select("*", <<"Bridge">>, [{<<"name">>, <<"==">>, BrName}]), Opts) of
        {ok,[#{<<"rows">> := [B]}]} when is_map(B) ->
            B;
        _ ->
            #{}
    end,
    do_vsctl(Op, BrInfo, Opts);
do_vsctl(Op, #{br_name := BrName, port_name := PortName} = Opts)
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
            do_vsctl(add_br, Opts),
            do_vsctl(Op, Opts);
        {B, P} when ((map_size(B) == 0) or (map_size(P) == 0)) and
                        ((Op == del_port) or (Op == del_bond_iface)) ->
            {error, invalid_configuration};
        _ ->
            do_vsctl(Op, DbInfo, Opts)
    end;

do_vsctl(Cmd, _) ->
    ?INFO("~p: Not supported", [Cmd]),
    not_yet.

%% @private
process_vsctl(Cmd, Opts, State) ->
    ?INFO("~p: Opts ~p", [Cmd, Opts]),
    State.

do_vsctl(Op, #{<<"Bridge">> := BrInfo, <<"Controller">> := CtrlrInfo},
        Opts) when Op == add_controller; Op == del_controller ->
    controller_cmd(Op, BrInfo, CtrlrInfo, Opts);
do_vsctl(Op, BrInfo, Opts) when Op == add_br; Op == del_br ->
    bridge_cmd(Op, BrInfo, Opts);
do_vsctl(Op, #{<<"Bridge">> := BrInfo, <<"Port">> := PortInfo, <<"Interface">> := IfaceList},
        Opts) when Op == add_port; Op == del_port ->
    port_cmd(Op, BrInfo, PortInfo, IfaceList,  Opts);
do_vsctl(Op, #{<<"Bridge">> := BrInfo, <<"Port">> := PortInfo, <<"Interface">> := IfaceList},
        Opts) when Op == add_port; Op == del_port ->
    port_cmd(Op, BrInfo, PortInfo, IfaceList,  Opts);
do_vsctl(Op, #{<<"Bridge">> := BrInfo, <<"Port">> := PortInfo, <<"Interface">> := IfaceList},
        Opts) when Op == del_bond_iface ->
    bond_cmd(Op, BrInfo, PortInfo, IfaceList, Opts);
do_vsctl(Op, Entry, #{table := Table} = Opts) when Op == set ->
    set_cmd(Op, Table, Entry, Opts);
do_vsctl(_Op, _BrInfo, _Opts) ->
    ok.

controller_cmd(add_controller,
        #{<<"controller">> := BrCtrlrUuid},
        #{<<"_uuid">> := CtrlUuid}, _) when BrCtrlrUuid == CtrlUuid ->
    ok;
controller_cmd(del_controller,
        #{<<"controller">> := BrCtrlrUuid} = BrInfo,
        #{<<"_uuid">> := CtrlUuid, <<"target">> := Target}, Opts) when BrCtrlrUuid == CtrlUuid ->
    Request = [
        ovsdb_ops:delete(<<"Controller">>, [{target, '==', Target}]),
        ovsdb_ops:update(<<"Bridge">>,
            [{<<"_uuid">>, <<"==">>, maps:get(<<"_uuid">>, BrInfo)}],
            #{controller => [set, []]}),
        ovsdb_ops:mutate(<<"Open_vSwitch">>,
            [{<<"_uuid">>, <<"==">>, get_Open_vSwitch_Uuid(Opts)}],
            [{<<"next_cfg">>, <<"+=">>, 1}]),
        ovsdb_ops:commit(true)
    ],
    transaction(Request, Opts);
controller_cmd(add_controller,
        #{<<"controller">> := BrCtrlrUuid} = BrInfo, CtrlrInfo,
        #{controller := Target} = Opts) when map_size(CtrlrInfo) == 0 ->
    Request =
        get_wait_ops(<<"Bridge">>, BrInfo, #{<<"controller">> => BrCtrlrUuid}) ++ [
        ovsdb_ops:insert(<<"Controller">>,
            #{target => Target}, ovsdb_utils:uuid(controller, Target)),
        ovsdb_ops:update(
            <<"Bridge">>,
            [{<<"_uuid">>, <<"==">>, maps:get(<<"_uuid">>, BrInfo)}],
            #{controller => [<<"named-uuid">>,  ovsdb_utils:uuid(controller, Target)]}),
        ovsdb_ops:mutate(<<"Open_vSwitch">>,
            [{<<"_uuid">>, <<"==">>, get_Open_vSwitch_Uuid(Opts)}],
            [{<<"next_cfg">>, <<"+=">>, 1}]),
        ovsdb_ops:commit(true)
    ],
    transaction(Request, Opts);
controller_cmd(_Op, _BrInfo, _CtrlrInfo, _Opts) ->
    {error, invalid_configuration}.

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
        NewBrInfo, ovsdb_utils:uuid(bridge, BrName)),

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
    transaction(Request, Opts);

bridge_cmd(add_br, _BrInfo, Opts) ->
    already_exists(Opts);

bridge_cmd(_Op, _PortInfo, _Opts) ->
    ?INFO("Unhandled ~p", [{_Op, _PortInfo, _Opts}]),
    error.

set_cmd(set, Table, #{<<"_uuid">> := Uuid} = Entry, #{config := Config} = Opts) ->
    case get_extra_config(process, #{}, Config) of
        ConfigMap when map_size(ConfigMap) /= 0 ->
            ?DEBUG("ConfigMap ~p", [ConfigMap]),
            Keys = [<<"options">>],
            WaitOps = lists:foldl(fun
                (Key, Acc) ->
                    case maps:get(<<"options">>, ConfigMap, not_found) of
                        not_found ->
                            Acc;
                        _ ->
                            [ovsdb_ops:wait(
                                Table,
                                [{<<"_uuid">>, '==', maps:get(<<"_uuid">>, Entry)}],
                                [#{Key => maps:get(Key, Entry)}],
                                '==',
                                [Key]
                            ) | Acc]
                    end
            end, [], Keys),

            OvsdbOps = ovsdb_ops:update(Table,
                [{<<"_uuid">>, <<"==">>, Uuid}], ConfigMap),
            Request = WaitOps ++ [OvsdbOps] ++ [
                ovsdb_ops:mutate(<<"Open_vSwitch">>,
                    [{<<"_uuid">>, <<"==">>, get_Open_vSwitch_Uuid(Opts)}],
                    [{<<"next_cfg">>, <<"+=">>, 1}]),
                ovsdb_ops:commit(true)
            ],
            transaction(Request, Opts);
        _ ->
            ok
    end.

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
            transaction(Request, Opts)
    end;

port_cmd(add_port, _BrInfo,
        #{<<"_uuid">> := Port_Uuid} = PortInfo,
        IfaceInfo, #{iface_list := IfaceList} = Opts) ->

    IfaceOps = lists:foldl(fun
        (IfName, Acc) ->
            case maps:get(IfName, IfaceInfo, #{}) of
                #{<<"_uuid">> := If_Uuid} ->
                    case get_extra_config(interface, #{}, Opts) of
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

    PortOps = case get_extra_config(port, #{}, Opts) of
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
            transaction(Request, Opts)
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
            transaction(Request, Opts)
    end;

bond_cmd(_Op, _BrInfo, _PortInfo, _IfaceList, _Opts) ->
    ?INFO("bond_cmd: Unhandled~n~s", [ovsdb_utils:pretty_print({_Op, _BrInfo, _PortInfo, _IfaceList, _Opts})]),
    ok.

transaction(Request, Opts) ->
    {ok,Response} = ovsdb_client:transaction(Request, Opts),

    ?DEBUG("Request~n~s", [ovsdb_utils:pretty_print(Request)]),
    ?DEBUG("Response~n~s", [ovsdb_utils:pretty_print(Response)]),
    lists:foldl(fun
        ({Req, #{<<"error">> := _} = Rsp}, _) ->
            ?WARNING("Errors:~n~s", [ovsdb_utils:pretty_print({Req, Rsp})]),
            {error, [Req, Rsp]};
        (_, Acc) ->
            Acc
    end, ok, ovsdb_utils:zipWithPadding(Request, Response)).

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

get_extra_config(port, BrInfo, #{port_config := Config}) ->
    get_extra_config(process, BrInfo, Config);

get_extra_config(interface, BrInfo, #{intf_config := Config}) ->
    get_extra_config(process, BrInfo, Config);

get_extra_config(bridge, BrInfo, #{bridge_config := Config}) ->
    get_extra_config(process, BrInfo, Config);

get_extra_config(process, ConfigMap, ConfigParams) ->
    maps:fold(fun
        (Key, Value, Acc) when is_map(Value) ->
            Map = maps:fold(fun
                (K1, V1, A1) ->
                    [[ovsdb_utils:to_binstring(K1), V1] | A1]
            end, [], Value),
            Acc#{ovsdb_utils:to_binstring(Key) => [map, Map]};
        (Key, Value, Acc) when is_list(Value) ->
            Acc#{ovsdb_utils:to_binstring(Key) => [set, Value]};
        (Key, Value, Acc) ->
            Acc#{ovsdb_utils:to_binstring(Key) => Value}
    end, ConfigMap, ConfigParams);
get_extra_config(_, ConfigMap, _Opts) ->
    ConfigMap.

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

already_exists(Opts) ->
    case maps:get(may_exist, Opts, ok) of
        true ->
            ok;
        _ ->
            {error, already_exists}
    end.
