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
%% @doc OVSDB Client Module
%%
%% This Module is the primary interface to OVSDB library
%%
%% @end
%%
%% Created : 15. Jun 2020
%%-------------------------------------------------------------------
-module(ovsdb_client).
-author("Vasu Dasari").

-include("ovsdb_client.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([
    start/2, start/3,
    get_database/0, get_database/1
]).

-export([
    list_dbs/0, list_dbs/1,
    get_schema/0, get_schema/1,
    transaction/1, transaction/2,
    cancel/1, cancel/2,
    monitor/2, monitor/3,
    monitor_cancel/1, monitor_cancel/2,
    lock/1, lock/2,
    steal/1, steal/2,
    unlock/1, unlock/2,
    echo/0, echo/1
]).

-export([
    list_columns/1, list_columns/2,
    list_tables/0, list_tables/1,
    dump/2, dump/3
]).

-export([
    get_proc/1
]).

-compile({no_auto_import,[monitor/3]}).

-export_type([opts/0, rpc_return/0]).

-type rpc_return() :: {ok, term()} | {error, term()} | not_connected.
-type db_table()    :: iolist() | binary().
-type ovsdb_ops()   :: map() | list().
-type opts()        :: #{
            pid => dst(),
            database => db_name(),
            br_name => iolist(),
            port_name => iolist()
        }.

%%%===================================================================
%%% API
%%%===================================================================

%% Retrieve pid from the options
%% @private
get_proc(#{pid := Pid}) -> Pid;
get_proc(_) -> ?SERVER.

%% @doc Starts TCP connection
%%
%% Establishes TCP connection with OVSDB server identified by IpAddr and Port. Optionally
%% user can set defaul database to be used in future transactions, by specifying opts with
%% Opts = #{database => "DbName"
%% If a connection is already in polace, it would drop it and restarts new session if endpoint
%% is different.
-spec start(ip_addr(), inet:port_number(), opts()) -> ok.
start(IpAddr, Port, Opts) ->
    gen_server:call(get_proc(Opts), {start, IpAddr, Port, Opts}).

%% @doc Starts TCP connection
%%
%% Establishes TCP connection with OVSDB server identified by "IpAddr:Port" string format.
start(IpPortStr, Opts) when is_list(IpPortStr) ->
    [IpStr, PortStr] = string:split(IpPortStr, ":"),
    Port = list_to_integer(PortStr),
    {ok, IpAddr} = inet:ip(IpStr),
    start(IpAddr, Port, Opts).

%% @private
get_database() ->
    get_database(?SERVER).
%% @private
-spec get_database(dst()) -> db_name().
get_database(Dst) ->
    gen_server:call(Dst, get_database).

%%%===================================================================
%%% OVSDB Wire Protocol: Rpc Methods
%%%===================================================================
%% @doc Lists available databases
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.1">4.1.1. List Databases</a>
-spec list_dbs(opts()) -> rpc_return().
list_dbs(Opts) ->    ovsdb_protocol:list_dbs(Opts).
%% @equiv list_dbs(#{})
list_dbs() ->       list_dbs(#{}).

%% @doc Get database schema
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.2">4.1.2. Get Schema</a>
-spec get_schema(opts()) -> rpc_return().
get_schema(Opts) -> ovsdb_protocol:get_schema(Opts).
%% @equiv get_schema(#{})
get_schema()      -> get_schema(#{}).

%% @doc Perform OVSDB Transaction
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.3">4.1.3. Transaction</a>
-spec transaction(ovsdb_ops(), opts()) -> rpc_return().
transaction(Operation, Opts) -> ovsdb_protocol:transaction(Operation, Opts).
%% @equiv transaction(Operation, #{})
transaction(Operation)      -> transaction(Operation, #{}).

%% @doc Cancel Transaction
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.4">4.1.4. Cancel</a>
-spec cancel(ovsdb_ops(), opts()) -> rpc_return().
cancel(Operation, Opts) -> ovsdb_protocol:cancel(Operation, Opts).
%% @equiv cancel(Operation, #{})
cancel(Operation) -> cancel(Operation, #{}).

%% @doc Monitor
%%
%% Caller initiates monitor session identified by an Id. This Id need to be used to cancel/stop
%% monitor operation. All monitored events will be sent to caller process with following
%% syntax.
%%
%% ```
%%    {ovsdb_monitor, Id, Update}
%% '''
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.5">4.1.5. Monitor</a>
-spec monitor(json_value(), term(), opts()) -> rpc_return().
monitor(Id, Select, Opts) -> ovsdb_protocol:monitor(Id, Select, Opts).
%% @equiv monitor(Id, Select, #{})
monitor(Id, Select) -> monitor(Id, Select, #{}).

%% @doc Cancel Monitor Operation
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.7">4.1.7. Monitor Cancellation</a>
-spec monitor_cancel(json_value(), opts()) -> rpc_return().
monitor_cancel(Id, Opts) -> ovsdb_protocol:monitor_cancel(Id, Opts).
%% @equiv monitor_cancel(Id, #{})
monitor_cancel(Id) -> monitor_cancel(Id, #{}).

%% @doc Lock Database
%%
%% This function returns with status of database getting locked or not. If it is not locked,
%% a notification will be sent when the database is locked. And it would like this:
%%
%% ```
%%    {ovsdb_notification, Method, LockId}
%%          Method: locked | stolen
%% '''
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.8">4.1.8. Lock Operations</a>
-spec lock(json_value(), opts()) -> rpc_return().
lock(Id, Opts) -> ovsdb_protocol:lock_ops(lock, Id, Opts).
%% @equiv lock(Id, #{})
lock(Id) -> lock(Id, #{}).

%% @doc Steal lock
%%
%% This method would forcefully grab the database access which was previously locked
%% by Id by another process. This operation would notify the process which originally
%% had lock that the lock is stolen.
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.8">4.1.8. Lock Operations</a>
-spec steal(json_value(), opts()) -> rpc_return().
steal(Id, Opts) -> ovsdb_protocol:lock_ops(steal, Id, Opts).
%% @equiv steal(Id, #{})
steal(Id) -> steal(Id, #{}).

%% @doc Unlock database
%%
%% Unlock the database by releasing the lock.
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.8">4.1.8. Lock Operations</a>
-spec unlock(json_value(), opts()) -> rpc_return().
unlock(Id, Opts) -> ovsdb_protocol:lock_ops(unlock, Id, Opts).
%% @equiv unlock(Id, #{})
unlock(Id) -> unlock(Id, #{}).

%% @doc Echo
%%
%% This can be used to check by caller if the session is active. By default, ovsdb_client
%% performs echo - echo-reply handshakes to makesure session is active. This function can
%% be used for debugging.
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.11">4.1.11. Echo</a>
-spec echo(opts()) -> rpc_return().
echo(Opts) -> ovsdb_protocol:echo(Opts).
%% @equiv echo()
echo() -> echo(#{}).

%% @doc Get columns of table
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.3">4.1.3. Transaction</a>
-spec list_columns(db_table(), opts()) -> rpc_return().
list_columns(Table, Opts) -> ovsdb_protocol:list_columns(Table, Opts).
%% @equiv list_columns(Table, #{})
list_columns(Table)      -> list_columns(Table, #{}).

%% @doc Get a list of tables
-spec list_tables(opts()) -> rpc_return().
list_tables(Opts) -> ovsdb_protocol:list_tables(Opts).
%% @equiv list_tables(#{})
list_tables()      -> list_tables(#{}).

%% Get contents of database
-spec dump(db_table(), list()) -> rpc_return().
dump(Table, Columns) ->
    dump(Table, Columns, #{}).

dump(Table, [], Opts) -> dump(Table, "*", Opts);
dump(Table, Columns, Opts) when is_binary(Table) ->
    case transaction(ovsdb_ops:select(Columns, Table, []), Opts) of
        {ok, [#{<<"rows">> := Info}]} ->
            {ok, Info};
        Error ->
            {error, Error}
    end.

%%%===================================================================
%%% OVSDB Database Operations
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init([]) ->
    self() ! {init},
    {ok, #ovsdb_state{}}.

%% @hidden
handle_call(Request, From, State) ->
    try process_call(Request, From, State) of
        {reply, ok, _} = Return ->
            ?DEBUG("call: Request From ~p, Returns ~p~n~p", [From, ok, Request]),
            Return;
        {reply, NotOk, _} = Return when is_atom(NotOk) ->
            ?DEBUG("call: Request From ~p, Returns ~p~n~p", [From, NotOk, Request]),
            Return;
        Return ->
            Return
    catch
        Error:Reason:StackTrace ->
            ?ERROR("Failed:~n    Request ~p~n    From ~p~n    Error ~p, Reason ~p~n    StackTrace ~n~s",
                [Request, From, Error, Reason, ovsdb_utils:pretty_print(StackTrace)]),
            {reply, Error, State}
    end.

%% @hidden
handle_cast(Request, State) ->
    ?DEBUG("cast: Request ~p", [Request]),
    try process_cast(Request, State) of
        Return ->
            Return
    catch
        Error:Reason:StackTrace ->
            ?ERROR("Failed:~n    Request ~p~n    Error ~p, Reason ~p~n    StackTrace ~n~s",
                [Request, Error, Reason, ovsdb_utils:pretty_print(StackTrace)]),
            {noreply, State}
    end.

%% @hidden
handle_info(Info, State) ->
    ?DEBUG("info: Request ~p", [Info]),
    try process_info_msg(Info, State) of
        Return ->
            Return
    catch
        Error:Reason:StackTrace ->
            ?ERROR("Failed:~n    Request ~p~n    Error ~p, Reason ~p~n    StackTrace ~n~s",
                [Info, Error, Reason, ovsdb_utils:pretty_print(StackTrace)]),
            {noreply, State}
    end.

%% @hidden
terminate(_Reason, _State) ->
    ?INFO("~s going down: ~p", [?MODULE, _Reason]),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
process_call({start, IpAddr, Port, Opts}, _, State) ->
    ?INFO("Starting ovsdb with ~p:~p, Opts: ~p", [IpAddr, Port, Opts]),
    self() ! connect,
    {reply, ok, State#ovsdb_state{
        ip_addr = IpAddr,
        port = Port,
        database = maps:get(database, Opts, <<>>)
    }};
process_call(get_database, _, #ovsdb_state{database = DbName} = State) ->
    {reply, DbName, State};
process_call(_, _, #ovsdb_state{socket = not_connected} = State) ->
    {reply, not_connected, State};
process_call({lock, Op, Lock, FromPid, Data}, From, #ovsdb_state{locks_map = LocksMap} = State) ->
    NewLockMap = case Op of
        unlock -> maps:remove(Lock, LocksMap);
        _ -> LocksMap#{Lock => FromPid}
    end,
    {noreply, ovsdb_comms:send_data(From, Data, State#ovsdb_state{locks_map = NewLockMap})};
process_call({send, Data}, From, State) ->
    {noreply, ovsdb_comms:send_data(From, Data, State)};
process_call(Request, _, State) ->
    ?INFO("call: Unhandled Request ~p", [Request]),
    {reply, ok, State}.

process_cast({send, Data}, State) ->
    {noreply, ovsdb_comms:send_data(nil, Data, State)};
process_cast(Request, State) ->
    ?INFO("cast: Request~n~p", [Request]),
    {noreply, State}.

process_info_msg({init}, State) ->
    ?init_msg_id(),
    {noreply, State};
process_info_msg(connect, State) ->
    {noreply, ovsdb_comms:connect(State)};
process_info_msg(connected, State) ->
    {noreply, State};
process_info_msg({Type, Socket}, #ovsdb_state{socket = Socket} = State)
    when Type == tcp_closed orelse Type == ssl_closed ->
    {noreply, ovsdb_comms:restart(Type, State)};
process_info_msg({Type, Socket, Reason}, #ovsdb_state{socket = Socket} = State)
    when Type == tcp_error orelse Type == ssl_error ->
    {noreply, ovsdb_comms:restart(Reason, State)};
process_info_msg({'EXIT', Socket, Reason}, #ovsdb_state{socket = Socket} = State) ->
    {noreply, ovsdb_comms:restart(Reason, State)};
process_info_msg({Proto, Socket, Data}, #ovsdb_state{socket = Socket, proto = Proto} = State) ->
    {noreply, ovsdb_comms:recv_data(Data, State)};
process_info_msg(Request, State) ->
    ?INFO("info: Request~n~p", [Request]),
    {noreply, State}.
