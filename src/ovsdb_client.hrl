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
%% @doc Header file sharing common data structures across all modules.
%% @private
%% @end
%% Created : 15. Jun 2020 3:27 PM
%%-------------------------------------------------------------------
-author("Vasu Dasari").

-include("ovsdb_api.hrl").
-include("logger.hrl").

-define(SERVER, ovsdb_client).

-define(init_msg_id(),
    erlang:put(method_id,1)
).

-define(next_msg_id(),
    erlang:put(method_id,(erlang:get(method_id)+1))
).

-type dst() :: pid() | port() | (RegName :: atom()) | {RegName :: atom(), Node :: node()}.
-type json_value()  :: jsone:json_value().
-type db_name() :: unicode:chardata().
-type ip_addr() :: inet:socket_address() | inet:hostname().
-type proto_type() :: tcp | ssl.

-record(ovsdb_state, {
    proc = ?SERVER          :: dst(),
    proto = tcp             :: proto_type(),
    ip_addr = 'any'         :: ip_addr(),
    port = 0                :: inet:port_number(),
    timeout = 5000          :: non_neg_integer(),
    socket = not_connected  :: gen_tcp:socket() | ssl:sslsocket() | atom(),
    database = <<>>         :: db_name(),
    pending_messages = #{}  :: map(),
    locks_map = #{}         :: map(),
    msg_buffer = <<>>       :: binary()
}).