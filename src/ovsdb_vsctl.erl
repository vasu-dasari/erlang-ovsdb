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
]).

-export([vsctl/1, vsctl/2]).

-export([process_vsctl/3, trace/1]).

-export([
]).

-type vsctl_returns()       :: ok | error | {ok, term()} | {error, term()}.

%% @doc Add/Modify a bridge to switch
%%
-spec add_br(unicode:chardata(), ovsdb_client:opts()) -> vsctl_returns().
add_br(BrName, Opts) ->
    vsctl(add_br, Opts#{br_name => BrName}).

%% @doc Deletes a bridge to switch
%%
-spec del_br(unicode:chardata(), ovsdb_client:opts()) -> vsctl_returns().
del_br(BrName, Opts) ->
    vsctl(add_br, Opts#{br_name => BrName}).

%% @doc Add/modify port to a bridge
%%
%% Bridge of type netdev will be created if it does not exists
-spec add_port(unicode:chardata(), unicode:chardata(), ovsdb_client:opts()) -> vsctl_returns().
add_port(BrName, PortName, Opts) ->
    vsctl(add_port, Opts#{br_name => BrName, port_name => PortName}).

%% @doc Add/modify port to a bridge
%%
-spec del_port(unicode:chardata(), unicode:chardata(), ovsdb_client:opts()) -> vsctl_returns().
del_port(BrName, PortName, Opts) ->
    vsctl(del_port, Opts#{br_name => BrName, port_name => PortName}).

%% @doc Create or modify bond interface
%%
-spec add_bond(unicode:chardata(), unicode:chardata(), list(), ovsdb_client:opts()) -> ovsdb_client:rpc_return().
add_bond(BrName, BondName, IfaceList, Opts) ->
    bond(add_bond, BrName, BondName, IfaceList, Opts).

add_bond_iface(BrName, BondName, Iface, Opts) when is_binary(Iface) ->
    add_bond_iface(BrName, BondName, [Iface], Opts);
add_bond_iface(BrName, BondName, IfaceList, Opts) ->
    bond(add_bond,  BrName, BondName, IfaceList, Opts).

del_bond_iface(BrName, BondName, Iface, Opts) when is_binary(Iface) ->
    del_bond_iface(BrName, BondName, [Iface], Opts);
del_bond_iface(BrName, BondName, IfaceList, Opts) ->
    bond(del_bond_iface,  BrName, BondName, IfaceList, Opts).

del_bond(BrName, BondName, Opts) ->
    del_port(BrName, BondName, Opts).

bond(Op, BrName, BondName, IfaceList, Opts) ->
    vsctl(Op, Opts#{
        br_name => BrName, port_name => BondName, iface_list => IfaceList
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
    when Op == add_port; Op == del_port; Op == add_bond; Op == del_bond_iface  ->

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
        {B, _} when (map_size(B) == 0) and (Op == add_port orelse Op == add_bond) ->
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
do_vsctl(Op, #{<<"Bridge">> := BrInfo, <<"Port">> := PortInfo},
        Opts) when Op == add_port; Op == del_port ->
    port_cmd(Op, BrInfo, PortInfo, Opts);
do_vsctl(Op, #{<<"Bridge">> := BrInfo, <<"Port">> := PortInfo, <<"Interface">> := IfaceList},
        Opts) when Op == add_bond; Op == del_bond_iface ->
    bond_cmd(Op, BrInfo, PortInfo, IfaceList, Opts);
do_vsctl(_Op, _BrInfo, _Opts) ->
    ok.

bridge_cmd(del_br, #{<<"_uuid">> := Br_Uuid}, #{br_name := BrName} = Opts) ->

    {ok, [#{<<"_uuid">> := Open_vSwitch_Uuid, <<"bridges">> := Bridges}]} =
        ovsdb_client:dump(<<"Open_vSwitch">>, ['_uuid', bridges]),

    BridgesSet = case Bridges of
        [<<"set">>, B] -> B;
        _ -> [Bridges]
    end,

    {ok, _} = ovsdb_client:transaction([
        ovsdb_ops:delete(<<"Bridge">>, [{name, '==', BrName}]),
        ovsdb_ops:update(<<"Open_vSwitch">>,
            [{<<"_uuid">>, <<"==">> ,Open_vSwitch_Uuid}],
            #{bridges => [set, BridgesSet -- [Br_Uuid]]}),
        ovsdb_ops:mutate(<<"Open_vSwitch">>,
            [{<<"_uuid">>, <<"==">> ,Open_vSwitch_Uuid}],
            [{<<"next_cfg">>, <<"+=">>, 1}]),
        ovsdb_ops:commit(true)
    ], Opts),
    ok;

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

    {ok, _} = ovsdb_client:transaction([
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
    ], Opts),
    ok;

bridge_cmd(_Op, _PortInfo, _Opts) ->
    ?INFO("Unhandled ~p", [{_Op, _PortInfo, _Opts}]),
    ok.

port_cmd(Op, #{<<"ports">> := [<<"uuid">>, _] = Ports} = BrInfo, PortInfo, Opts) ->
    port_cmd(Op, BrInfo#{<<"ports">> := [<<"set">>, [Ports]]}, PortInfo, Opts);

port_cmd(add_port,
        #{<<"_uuid">> := Br_Uuid, <<"ports">> := [<<"set">>, Ports]},
        PortInfo,
        #{port_name := PortName} = Opts) when map_size(PortInfo) == 0 ->

    Interface = #{name => PortName},
    Port = #{name => PortName, interfaces => [<<"named-uuid">>, <<"interface_uuid">>]},
    {ok,_} = ovsdb_client:transaction([
        ovsdb_ops:insert(<<"Interface">>, Interface, <<"interface_uuid">>),
        ovsdb_ops:insert(<<"Port">>, Port, <<"port_uuid">>),
        ovsdb_ops:update(<<"Bridge">>,
            [{<<"_uuid">>, <<"==">>, Br_Uuid}],
            #{ports => [set, [[<<"named-uuid">>, <<"port_uuid">>] | Ports]]}),
        ovsdb_ops:mutate(<<"Open_vSwitch">>,
            [{<<"_uuid">>, <<"==">>, get_Open_vSwitch_Uuid(Opts)}],
            [{<<"next_cfg">>, <<"+=">>, 1}]),
        ovsdb_ops:commit(true)
    ], Opts),
    ok;

port_cmd(Op,
        #{<<"_uuid">> := Br_Uuid, <<"ports">> := [<<"set">>, Ports]},
        #{<<"_uuid">> := Port_Uuid},
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

port_cmd(_Op, _BrInfo, _PortInfo, _Opts) ->
    ?INFO("Unhandled ~p", [{_Op, _BrInfo, _PortInfo, _Opts}]),
    ok.

bond_cmd(add_bond,
        #{<<"_uuid">> := Br_Uuid} = BrInfo,
        PortInfo, IfaceInfo,
        #{port_name := BondName, iface_list:= IfaceList} = Opts) ->

    #{ops := IfaceOps, uuids := IfaceUuids} = lists:foldl(fun
        (Iface, #{ops := OpsAcc, uuids := UuidsAcc} = Acc) ->
            case maps:get(Iface, IfaceInfo, #{}) of
                R when map_size(R) == 0 ->
                    Acc#{
                        ops => [ovsdb_ops:insert(<<"Interface">>, #{name => Iface}, ovsdb_utils:uuid(interface, Iface)) | OpsAcc],
                        uuids => [[<<"named-uuid">>, ovsdb_utils:uuid(interface, Iface)] | UuidsAcc]
                    };
                _ ->
                    Acc
            end
    end, #{ops => [], uuids => []}, IfaceList),

    PortOps = case IfaceOps /= [] of
        true when map_size(PortInfo) == 0 ->
            PortInsert = ovsdb_ops:insert(<<"Port">>,
                #{name => BondName,
                    interfaces => [set, IfaceUuids]
                }, ovsdb_utils:uuid(port, BondName)),
            BridgeUpdate = ovsdb_ops:update(
                <<"Bridge">>,
                [{<<"_uuid">>, <<"==">>, Br_Uuid}],
                #{ports => [set, [[<<"named-uuid">>, ovsdb_utils:uuid(port, BondName)] | get_set(BrInfo, <<"ports">>)]]}
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
            Request = IfaceOps ++ PortOps ++ [
                ovsdb_ops:mutate(<<"Open_vSwitch">>,
                    [{<<"_uuid">>, <<"==">>, get_Open_vSwitch_Uuid(Opts)}],
                    [{<<"next_cfg">>, <<"+=">>, 1}]),
                ovsdb_ops:commit(true)
            ],
            transaction(Request)
    end;

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