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
%% @doc Module that deals with message transport methods
%% @private
%% @end
%% Created : 15. Jun 2020 2:44 PM
%%-------------------------------------------------------------------
-module(ovsdb_comms).
-author("Vasu Dasari").

-include("ovsdb_client.hrl").

%% API
-export([connect/1, restart/2, recv_data/2, send_data/3]).

recv_data(<<>>, #ovsdb_state{socket = Socket} = State) ->
    ok = setopts(tcp, Socket, [{active, once}]),
    State;
recv_data(Data, #ovsdb_state{socket = Socket, msg_buffer = MsgBuf} = State) ->
    CompleteBuf = <<MsgBuf/binary, Data/binary>>,
    case jsone:try_decode(CompleteBuf) of
        {ok, Message, Left} ->
            ?DEBUG("Received ~s", [ovsdb_utils:pretty_print(Message)]),
            recv_data(Left,
                ovsdb_protocol:process_message(Message, State#ovsdb_state{msg_buffer = <<>>}));
        {error, _Error} ->
            ok = setopts(tcp, Socket, [{active, once}]),
            State#ovsdb_state{msg_buffer = CompleteBuf}
    end.

send_data(From, #{id := 0} = Message, State) ->
    send_data(From, Message#{id => ?next_msg_id()}, State);
send_data(From, #{id := Id} = Message,
        #ovsdb_state{
            socket = Socket,
            proto = Protocol,
            pending_messages = PendingMessages} = State) ->
    ?DEBUG("Sending ~s", [ovsdb_utils:pretty_print(Message)]),
    ok = send(Protocol, Socket, jsone:encode(Message)),
    State#ovsdb_state{
        pending_messages = maps:put(Id, #{ from => From, reply => noreply }, PendingMessages)
    }.

connect(#ovsdb_state{
    proc = Proc,
    proto = Protocol,
    ip_addr = Host,
    port = Port,
    timeout = Timeout} = State) ->
    Lib = transport_lib(Protocol),
    case Lib:connect(Host, Port, opts(Protocol)) of
        {ok, Socket} ->
            ?INFO("~p: Connected: Socket ~p", [Proc, Socket]),
            ok = setopts(Protocol, Socket, [{active, once}]),
            self() ! connected,
            State#ovsdb_state{
                socket = Socket
            };
        {error, _Reason} ->
            erlang:send_after(Timeout, self(), connect),
            State
    end;
connect(State) ->
    State.

restart(Reason,
        #ovsdb_state{
            proc = Proc,
            proto = Protocol,
            socket = Socket,
            timeout = Timeout} = State) ->
    ?INFO("~p: Restart reason: ~p", [Proc, Reason]),
    ok = close(Protocol, Socket),
    erlang:send_after(Timeout, self(), connect),
    State#ovsdb_state{socket = not_connected}.

opts(tcp) ->
    [binary, {reuseaddr, true}, {active, once}];
opts(ssl) ->
    opts(tcp) ++ [{verify, verify_peer},
        {fail_if_no_peer_cert, true}]
        ++ [{cert, base64:decode(Cert)}
        || {ok, Cert} <- [application:get_env(linc, certificate)]]
        ++ [{key, {'RSAPrivateKey', base64:decode(Key)}}
            || {ok, Key} <- [application:get_env(linc, rsa_private_key)]].

setopts(_, not_connected, _) ->
    ok;
setopts(ssl, Socket, Opts) ->
    ssl:setopts(Socket, Opts);
setopts(tcp, Socket, Opts) ->
    inet:setopts(Socket, Opts).

send(_, not_connected, _) ->
    ok;
send(Protocol, Socket, Data) ->
    (transport_lib(Protocol)):send(Socket, Data).

close(_, not_connected) ->
    ok;
close(Protocol, Socket) ->
    Lib = transport_lib(Protocol),
    Lib:close(Socket).

transport_lib(Protocol) ->
    case Protocol of
        tcp -> gen_tcp;
        ssl -> ssl
    end.
