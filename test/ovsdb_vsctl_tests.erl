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

-export([del_bond_iface/1, del_bond/1, get_json_output/1]).

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
                ,{"add_bond",                   fun add_bond/1}
                ,{"add_bond_iface",             fun add_bond_iface/1}
                ,{"del_bond_iface",             fun del_bond_iface/1}
                ,{"del_bond",                   fun del_bond/1}
            ]]
        } end
    }.

controller_test_() ->
    {setup,
        fun controller_setup/0,
        fun controller_teardown/1,
        fun(S) -> {foreach, fun ovsdb_client_tests:test_setup/0,
            [{N, fun() -> F(S) end} || {N, F} <- [
                {"add controller",              fun controller_set/1}
                , {"delete controller",         fun controller_delete/1}
                , {"replace controller",        fun controller_replace/1}
                , {"share controller",          fun controller_share/1}
            ]]
        } end
    }.

bridge_test_() ->
    {setup,
        fun bridge_setup/0,
        fun bridge_teardown/1,
        fun(S) -> {foreach, fun ovsdb_client_tests:test_setup/0,
            [{N, fun() -> F(S) end} || {N, F} <- [
                {"datapath_type",               fun bridge_datapath_type/1}
                , {"datapath_id",               fun bridge_datapath_id/1}
                , {"fail_mode",                 fun bridge_fail_mode/1}
                , {"protocols",                 fun bridge_protocols/1}
            ]]
        } end
    }.


bond_test_() ->
    {setup,
        fun bond_setup/0,
        fun bond_teardown/1,
        fun(S) -> {foreach, fun ovsdb_client_tests:test_setup/0,
            [{N, fun() -> F(S) end} || {N, F} <- [
                {"lacp",                        fun bond_lacp/1}
                , {"bond_mode",                 fun bond_bond_mode/1}
            ]]
        } end
    }.

interface_test_() ->
    {setup,
        fun interface_setup/0,
        fun interface_teardown/1,
        fun(S) -> {foreach, fun ovsdb_client_tests:test_setup/0,
            [{N, fun() -> F(S) end} || {N, F} <- [
                {"modify admin_state on port",      fun interface_admin_state_port/1}
                , {"modify admin_state on bond",    fun interface_admin_state_bond/1}
            ]]
        } end
    }.

tunnel_test_() ->
    {setup,
        fun tunnel_setup/0,
        fun tunnel_teardown/1,
        fun(S) -> {foreach, fun ovsdb_client_tests:test_setup/0,
            [{N, fun() -> F(S) end} || {N, F} <- [
                {"add tunnel port",                 fun tunnel_add_port/1}
                , {"delete tunnel port",            fun tunnel_del_port/1}
                , {"vxlan tunnel",                  fun tunnel_vxlan/1}
                , {"fully specified vxlan tunnel",  fun tunnel_vxlan_fst/1}
            ]]
        } end
    }.

%%%===================================================================
%%% Helper functions
%%%===================================================================

get_json_output(Cmd) ->
    Output = jsone:decode(erlang:list_to_binary(ovsdb_client_tests:ovs_cmd(Cmd))),
    ?assertMatch(#{<<"data">> := _}, Output),
    case Output of
        #{<<"data">> := [[[<<"map">>, Data]]]} -> Data;
        #{<<"data">> := [[Data]]} -> Data;
        #{<<"data">> := Data} -> Data;
        #{<<"data">> := []} -> []
    end.

%%%===================================================================
%%% Worker functions
%%%===================================================================

add_br(Opts) ->
    ?assertEqual(
        ok,
        ovsdb_vsctl:add_br(<<"br1">>, Opts)
    ),
    ovsdb_client_tests:verify_ovs(list_br, "br1"),
    ovsdb_client_tests:ovs_cmd({vsctl, "del-br br1"}).

del_br(Opts) ->
    ovsdb_client_tests:ovs_cmd({vsctl, "add-br br1 -- set Bridge br1 datapath_type=netdev"}),
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

add_bond(Opts) ->
    ?assertEqual(
        ok,
        ovsdb_vsctl:add_bond(<<"br1">>, <<"br1-bond1">>, [<<"bond1-eth1">>, <<"bond1-eth2">>], Opts)
    ),
    ?assertEqual(
        ok,
        ovsdb_vsctl:add_bond(<<"br1">>, <<"br1-bond2">>, [<<"bond2-eth1">>, <<"bond2-eth2">>], Opts)
    ),
    ovsdb_client_tests:verify_ovs({list_ports, "br1"}, "br1-bond1br1-bond2"),
    ovsdb_client_tests:verify_ovs({list_ifaces, "br1"}, "bond1-eth1bond1-eth2bond2-eth1bond2-eth2"),
    ovsdb_client_tests:ovs_cmd({vsctl, "del-br br1"}).

add_bond_iface(Opts) ->
    ovsdb_client_tests:ovs_cmd({vsctl, "add-br br1 -- set Bridge br1 datapath_type=netdev"}),
    ovsdb_client_tests:ovs_cmd({vsctl, "add-bond br1 br1-bond1 bond1-eth1 bond1-eth2"}),
    ?assertEqual(ok,
        ovsdb_vsctl:add_bond_iface(<<"br1">>, <<"br1-bond">>, <<"bond1-eth3">>, Opts)),
    ?assertEqual(ok,
        ovsdb_vsctl:add_bond_iface(<<"br1">>, <<"br1-bond">>, [<<"bond1-eth3">>, <<"bond1-eth4">>], Opts)),
    ovsdb_client_tests:verify_ovs({list_ports, "br1"}, "br1-bond"),
    ovsdb_client_tests:verify_ovs({list_ifaces, "br1"}, "bond1-eth3bond1-eth4"),
    ovsdb_client_tests:ovs_cmd({vsctl, "del-br br1"}).

del_bond_iface(Opts) ->
    ovsdb_client_tests:ovs_cmd({vsctl, "add-br br1 -- set Bridge br1 datapath_type=netdev"}),
    ovsdb_client_tests:ovs_cmd({vsctl, "add-bond br1 br1-bond1 bond1-eth1 bond1-eth2 bond1-eth3 bond1-eth4"}),
    ?assertEqual(ok,
        ovsdb_vsctl:del_bond_iface(<<"br1">>, <<"br1-bond1">>, [<<"bond1-eth2">>, <<"bond1-eth3">>], Opts),
        "API Delete multiple member link"
    ),
    ?assertEqual(ok,
        ovsdb_vsctl:del_bond_iface(<<"br1">>, <<"br1-bond1">>, <<"bond1-eth4">>, Opts),
        "API Delete single member link"
    ),
    ?assertNotEqual(ok,
        ovsdb_vsctl:del_bond_iface(<<"br1">>, <<"br1-bond1">>, <<"bond1-eth1">>, Opts),
        "Cannot delete last interface in a bond"
    ),
    ovsdb_client_tests:verify_ovs({list_ifaces, "br1"}, "bond1-eth1"),
    ovsdb_client_tests:ovs_cmd({vsctl, "del-br br1"}).

del_bond(Opts) ->
    ovsdb_client_tests:ovs_cmd({vsctl, "add-br br1 -- set Bridge br1 datapath_type=netdev"}),
    ovsdb_client_tests:ovs_cmd({vsctl, "add-bond br1 br1-bond1 bond1-eth1 bond1-eth2"}),
    ?assertEqual(ok,
        ovsdb_vsctl:del_bond(<<"br1">>, <<"br1-bond1">>, Opts)),
    ovsdb_client_tests:verify_ovs({list_ports, "br1"}, ""),
    ovsdb_client_tests:ovs_cmd({vsctl, "del-br br1"}).

controller_setup() ->
    Opts = ovsdb_client_tests:setup(),
    ?assertEqual(ok, ovsdb_vsctl:add_br(<<"br1">>, Opts#{datapath_type => <<"netdev">>})),
    ?assertEqual(ok, ovsdb_vsctl:add_br(<<"br2">>, Opts#{datapath_type => <<"netdev">>})),
    ?assertEqual(ok,
        ovsdb_vsctl:add_port(<<"br-underlay">>, <<"br2-eth1">>, Opts)),
    Opts.

controller_teardown(Opts) ->
    ?assertEqual(ok, ovsdb_vsctl:del_br(<<"br1">>, Opts)),
    ?assertEqual(ok, ovsdb_vsctl:del_br(<<"br2">>, Opts)),
    ovsdb_client_tests:teardown(Opts).

controller_set(Opts) ->
    CtrlrIp = <<"tcp:10.1.1.1:6653">>,
    ?assertEqual(ok,
        ovsdb_vsctl:set_controller(<<"br1">>, CtrlrIp, Opts)
    ),
    BrData = get_json_output({list, "Bridge", "controller", "br1"}),
    ?assertMatch(
        [<<"uuid">>,_], BrData
    ),
    [<<"uuid">>, Uuid] = BrData,
    ?assertEqual(
        CtrlrIp,
        get_json_output({list, "Controller", "target", erlang:binary_to_list(Uuid)})
    ).

controller_delete(Opts) ->
    CtrlrIp = <<"tcp:10.1.1.1:6653">>,
    ?assertEqual(ok, ovsdb_vsctl:set_controller(<<"br1">>, CtrlrIp, Opts)),
    ?assertEqual(ok,
        ovsdb_vsctl:del_controller(<<"br1">>, Opts)
    ),
    ?assertMatch([<<"set">>,[]], get_json_output({list, "Bridge", "controller", "br1"})),
    ?assertEqual([], get_json_output({list, "Controller", "target", ""})).

controller_replace(Opts) ->
    CtrlrIp1 = <<"tcp:10.1.1.1:6653">>,
    CtrlrIp2 = <<"tcp:10.1.1.2:6653">>,
    ?assertEqual(ok, ovsdb_vsctl:set_controller(<<"br1">>, CtrlrIp1, Opts)),

    ?assertEqual(ok, ovsdb_vsctl:set_controller(<<"br1">>, CtrlrIp2, Opts)),
    BrData = get_json_output({list, "Bridge", "controller", "br1"}),
    ?assertMatch(
        [<<"uuid">>,_], BrData
    ),
    ?assertEqual(
        CtrlrIp2,
        get_json_output({list, "Controller", "target", ""})
    ).

controller_share(Opts) ->
    CtrlrIp = <<"tcp:10.1.1.1:6653">>,
    ?assertEqual(ok, ovsdb_vsctl:set_controller(<<"br1">>, CtrlrIp, Opts)),
    ?assertEqual(ok, ovsdb_vsctl:set_controller(<<"br2">>, CtrlrIp, Opts)),

    BrData1 = get_json_output({list, "Bridge", "controller", "br1"}),
    ?assertMatch(
        [<<"uuid">>,_], BrData1
    ),
    BrData2 = get_json_output({list, "Bridge", "controller", "br2"}),
    ?assertMatch(
        [<<"uuid">>,_], BrData2
    ),
    ?assertNotEqual(BrData1, BrData2),
    ?assertEqual(
        [[<<"tcp:10.1.1.1:6653">>],[<<"tcp:10.1.1.1:6653">>]],
        get_json_output({list, "Controller", "target", ""})
    ).

bridge_setup() ->
    Opts = ovsdb_client_tests:setup(),
    ?assertEqual(ok, ovsdb_vsctl:add_br(<<"br1">>, Opts)),
    Opts.

bridge_teardown(Opts) ->
    ?assertEqual(ok, ovsdb_vsctl:del_br(<<"br1">>, Opts)),
    ovsdb_client_tests:teardown(Opts).

bridge_datapath_type(Opts) ->
    ?assertEqual(ok, ovsdb_vsctl:add_br(<<"br1">>, Opts#{datapath_type => <<"netdev">>})),
    ovsdb_client_tests:verify_ovs({list, "Bridge", "datapath_type", "br1"}, "netdev"),
    ?assertEqual(ok, ovsdb_vsctl:add_br(<<"br1">>, Opts#{datapath_type => <<"system">>})),
    ovsdb_client_tests:verify_ovs({list, "Bridge", "datapath_type", "br1"}, "system").

bridge_datapath_id(Opts) ->
    ?assertEqual(ok, ovsdb_vsctl:add_br(<<"br1">>, Opts#{datapath_id => <<"0000000000000001">>})),
    ovsdb_client_tests:verify_ovs({list, "Bridge", "other_config", "br1"}, "\"0000000000000001\""),
    ?assertEqual(ok, ovsdb_vsctl:add_br(<<"br1">>, Opts#{datapath_id => <<"0000000000000002">>})),
    ovsdb_client_tests:verify_ovs({list, "Bridge", "other_config", "br1"}, "\"0000000000000002\"").

bridge_fail_mode(Opts) ->
    ?assertEqual(ok, ovsdb_vsctl:add_br(<<"br1">>, Opts#{fail_mode => <<"secure">>})),
    ovsdb_client_tests:verify_ovs({list, "Bridge", "fail_mode", "br1"}, "\"secure\"").

bridge_protocols(Opts) ->
    ?assertEqual(ok, ovsdb_vsctl:add_br(<<"br1">>, Opts#{protocols => [<<"OpenFlow13">>]})),
    ovsdb_client_tests:verify_ovs({list, "Bridge", "protocols", "br1"}, "\"OpenFlow13\""),
    ?assertEqual(ok, ovsdb_vsctl:add_br(<<"br1">>, Opts#{protocols => [<<"OpenFlow13">>, <<"OpenFlow14">>]})),
    ovsdb_client_tests:verify_ovs({list, "Bridge", "protocols", "br1"}, "\"OpenFlow13\",\"OpenFlow14\"").

bond_setup() ->
    Opts = ovsdb_client_tests:setup(),
    ?assertEqual(ok,
        ovsdb_vsctl:add_bond(<<"br1">>, <<"br1-bond1">>,
            [<<"bond1-eth1">>, <<"bond1-eth2">>], Opts)),
    Opts.

bond_teardown(Opts) ->
    ?assertEqual(ok, ovsdb_vsctl:del_br(<<"br1">>, Opts)),
    ovsdb_client_tests:teardown(Opts).

bond_lacp(Opts) ->
    ?assertEqual(ok,
        ovsdb_vsctl:add_bond(<<"br1">>, <<"br1-bond1">>, [<<"bond1-eth1">>, <<"bond1-eth2">>], Opts#{lacp => <<"active">>})),
    ovsdb_client_tests:verify_ovs({list, "Port", "lacp", "br1-bond1"}, [[<<"active">>]]),
    ?assertEqual(ok,
        ovsdb_vsctl:add_bond(<<"br1">>, <<"br1-bond1">>, [<<"bond1-eth1">>, <<"bond1-eth2">>], Opts#{lacp => <<"passive">>})),
    ovsdb_client_tests:verify_ovs({list, "Port", "lacp", "br1-bond1"}, [[<<"passive">>]]),
    ?assertEqual(ok,
        ovsdb_vsctl:add_bond(<<"br1">>, <<"br1-bond1">>, [<<"bond1-eth1">>, <<"bond1-eth2">>], Opts#{lacp => <<"off">>})),
    ovsdb_client_tests:verify_ovs({list, "Port", "lacp", "br1-bond1"}, [[<<"off">>]]).

bond_bond_mode(Opts) ->
    ?assertEqual(ok,
        ovsdb_vsctl:add_bond(<<"br1">>, <<"br1-bond1">>, [<<"bond1-eth1">>, <<"bond1-eth2">>], Opts#{bond_mode => <<"balance-tcp">>})),
    ovsdb_client_tests:verify_ovs({list, "Port", "bond_mode", "br1-bond1"}, [[<<"balance-tcp">>]]),
    ?assertEqual(ok,
        ovsdb_vsctl:add_bond(<<"br1">>, <<"br1-bond1">>, [<<"bond1-eth1">>, <<"bond1-eth2">>], Opts#{bond_mode => <<"balance-slb">>})),
    ovsdb_client_tests:verify_ovs({list, "Port", "bond_mode", "br1-bond1"}, [[<<"balance-slb">>]]),
    ?assertEqual(ok,
        ovsdb_vsctl:add_bond(<<"br1">>, <<"br1-bond1">>, [<<"bond1-eth1">>, <<"bond1-eth2">>], Opts#{bond_mode => <<"active-backup">>})),
    ovsdb_client_tests:verify_ovs({list, "Port", "bond_mode", "br1-bond1"}, [[<<"active-backup">>]]).

interface_setup() ->
    Opts = ovsdb_client_tests:setup(),
    ?assertEqual(ok,
        ovsdb_vsctl:add_br(<<"br1">>, Opts#{})),
    ?assertEqual(ok,
        ovsdb_vsctl:add_bond(<<"br1">>, <<"br1-bond1">>,
            [<<"bond1-eth1">>, <<"bond1-eth2">>], Opts)),
    ?assertEqual(ok,
        ovsdb_vsctl:add_port(<<"br1">>, <<"br1-eth1">>, Opts)),
    Opts.

interface_teardown(Opts) ->
    ?assertEqual(ok, ovsdb_vsctl:del_br(<<"br1">>, Opts)),
    ovsdb_client_tests:teardown(Opts).

interface_admin_state_port(Opts) ->
    ?assertEqual(ok,
        ovsdb_vsctl:add_port(<<"br1">>, <<"br1-eth1">>, Opts#{admin_state => <<"up">>})),
%%    ovsdb_client_tests:verify_ovs({vsctl, "list Interface br1-eth1"}, "admin_state         : up"),
    ?assertEqual(ok,
        ovsdb_vsctl:add_port(<<"br1">>, <<"br1-eth1">>, Opts#{admin_state => <<"down">>})).
%%    ovsdb_client_tests:verify_ovs({vsctl, "list Interface br1-eth1"}, "admin_state         : down").

interface_admin_state_bond(Opts) ->
    ?assertEqual(ok,
        ovsdb_vsctl:add_bond(<<"br1">>, <<"br1-bond1">>,
            [<<"bond1-eth1">>, <<"bond1-eth2">>], Opts#{admin_state => <<"up">>})),
%%    ovsdb_client_tests:verify_ovs({list, "Port", "admin_state", "br1-bond1"}, "up"),
    ?assertEqual(ok,
        ovsdb_vsctl:add_bond(<<"br1">>, <<"br1-bond1">>,
            [<<"bond1-eth1">>, <<"bond1-eth2">>], Opts#{admin_state => <<"down">>})).
%%    ovsdb_client_tests:verify_ovs({list, "Port", "admin_state", "br1-bond1"}, "down"),

tunnel_setup() ->
    Opts = ovsdb_client_tests:setup(),
    ?assertEqual(ok, ovsdb_vsctl:add_br(<<"br1">>, Opts#{datapath_type => <<"netdev">>})),
    ?assertEqual(ok, ovsdb_vsctl:add_br(<<"br2">>, Opts#{datapath_type => <<"netdev">>})),
    ?assertEqual(ok,
        ovsdb_vsctl:add_port(<<"br-underlay">>, <<"br2-eth1">>, Opts)),
    Opts.

tunnel_teardown(Opts) ->
    ?assertEqual(ok, ovsdb_vsctl:del_br(<<"br1">>, Opts)),
    ?assertEqual(ok, ovsdb_vsctl:del_br(<<"br2">>, Opts)),
    ovsdb_client_tests:teardown(Opts).

tunnel_add_port(Opts) ->
    ?assertEqual(ok,
        ovsdb_vsctl:add_tunnel_port(<<"br1">>, <<"br1-vxlan1">>, vxlan, Opts#{remote_ip => <<"1.1.1.2">>})),
    Data = get_json_output({list, "Interface", "options", "br1-vxlan1"}),
    ?assertEqual([
        [<<"remote_ip">>,<<"1.1.1.2">>]
    ], lists:sort(Data)).

tunnel_vxlan(Opts) ->
    ?assertEqual(ok,
        ovsdb_vsctl:add_tunnel_port(<<"br1">>, <<"br1-vxlan1">>, vxlan,
            Opts#{
                key => <<"1000">>, remote_ip => <<"1.1.1.2">>
            })),
    Data = get_json_output({list, "Interface", "options", "br1-vxlan1"}),
    ?assertEqual([
        [<<"key">>,<<"1000">>], [<<"remote_ip">>,<<"1.1.1.2">>]
    ], lists:sort(Data)).

tunnel_vxlan_fst(Opts) ->
    ?assertEqual(ok,
        ovsdb_vsctl:add_port(<<"br2">>, <<"br2-eth1">>, Opts)
    ),
    ?assertEqual(ok,
        ovsdb_vsctl:add_tunnel_port(<<"br1">>, <<"br1-vxlan2">>, vxlan,
            Opts#{
                key => <<"2000">>, remote_ip => <<"1.1.1.2">>, local_ip => <<"1.1.1.1">>,
                dst_mac => <<"00:00:01:01:01:02">>, src_mac => <<"00:00:01:01:01:02">>,
                vlan_id => <<"100">>, out_port => <<"br2-eth1">>
            })
    ),
    Data = get_json_output({list, "Interface", "options", "br1-vxlan2"}),
    ?assertEqual([
        [<<"dst_mac">>,<<"00:00:01:01:01:02">>],
        [<<"key">>,<<"2000">>],
        [<<"local_ip">>,<<"1.1.1.1">>],
        [<<"out_port">>,<<"br2-eth1">>],
        [<<"remote_ip">>,<<"1.1.1.2">>],
        [<<"src_mac">>,<<"00:00:01:01:01:02">>],
        [<<"vlan_id">>,<<"100">>]
    ], lists:sort(Data)).

tunnel_del_port(Opts) ->
    ?assertEqual(ok,
        ovsdb_vsctl:add_tunnel_port(<<"br1">>, <<"br1-vxlan1">>, vxlan, Opts#{remote_ip => <<"1.1.1.2">>})),
    ?assertEqual(ok,
        ovsdb_vsctl:del_tunnel_port(<<"br1">>, <<"br1-vxlan1">>, Opts)),
    ovsdb_client_tests:verify_ovs({list, "Interface", "options", "br1-vxlan1"}, "no row").