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

-export([start/2]).

-export([
    list_dbs/0, list_dbs/1,
    get_schema/1, get_schema/2,
    transaction/2, transaction/3,
    cancel/2, cancel/3,
    monitor/3, monitor/4,
    monitor_cancel/1, monitor_cancel/2,
    lock/1, lock/2,
    steal/1, steal/2,
    unlock/1, unlock/2,
    echo/0, echo/1
]).

-export([
    get_columns/2, get_columns/3,
    get_tables/1, get_tables/2,
    get_content/3, get_content/2]).

%%-Format of API return values
-type rpc_return() :: {ok, term()} | {error, {term(), term()}} | not_connected.
-type db_name()     :: iolist().
-type db_table()    :: iolist().
-type select()      :: iolist() | map().
-type ovsdb_ops()   :: map().

%%%===================================================================
%%% API
%%%===================================================================
start(IpPortStr, Opts) when is_list(IpPortStr) ->
    [IpStr, PortStr] = string:split(IpPortStr, ":"),
    Port = list_to_integer(PortStr),
    {ok, IpAddr} = inet:ip(IpStr),
    start(IpAddr, Port, Opts).

start(IpAddr, Port, Opts) ->
    start(?MODULE, IpAddr, Port, Opts).
start(Dst, IpAddr, Port, Opts) ->
    gen_server:call(Dst, {start, IpAddr, Port, Opts}).

%%%===================================================================
%%% OVSDB Wire Protocol: Rpc Methods
%%%===================================================================
%% @doc Lists available databases
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.1">4.1.1. List Databases</a>
-spec list_dbs(dst()) -> rpc_return().
list_dbs(Dst) ->    ovsdb_protocol:list_dbs(Dst).
%% @equiv list_dbs(ovsdb_client)
list_dbs() ->       list_dbs(?SERVER).

%% @doc Get database schema
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.2">4.1.2. Get Schema</a>
-spec get_schema(dst(), db_name()) -> rpc_return().
get_schema(Dst, DbName) -> ovsdb_protocol:get_schema(Dst, DbName).
%% @equiv get_schema(ovsdb_client, DbName)
get_schema(DbName)      -> get_schema(?SERVER, DbName).

%% @doc Perform OVSDB Transaction
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.3">4.1.3. Transaction</a>
-spec transaction(dst(), db_name(), ovsdb_ops()) -> rpc_return().
transaction(Dst, DbName, Operation) -> ovsdb_protocol:transaction(Dst, DbName, Operation).
%% @equiv transaction(ovsdb_client, DbName, Operation)
transaction(DbName, Operation)      -> transaction(?SERVER, DbName, Operation).

%% @doc Cancel Transaction
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.4">4.1.4. Cancel</a>
-spec cancel(dst(), db_name(), ovsdb_ops()) -> rpc_return().
cancel(Dst, DbName, Operation) -> ovsdb_protocol:cancel(Dst, DbName, Operation).
%% @equiv cancel(ovsdb_client, DbName, Operation)
cancel(DbName, Operation) -> cancel(?SERVER, DbName, Operation).

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
-spec monitor(dst(), json_value(), db_name(), term()) -> rpc_return().
monitor(Dst, Id, DbName, Select) -> ovsdb_protocol:monitor(Dst, Id, DbName, Select).
%% @equiv monitor(ovsdb_client, Id, DbName, Select)
monitor(Id, DbName, Select) -> monitor(?SERVER, Id, DbName, Select).

%% @doc Cancel Monitor Operation
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.7">4.1.7. Monitor Cancellation</a>
-spec monitor_cancel(dst(), json_value()) -> rpc_return().
monitor_cancel(Dst, Id) -> ovsdb_protocol:monitor_cancel(Dst, Id).
%% @equiv monitor_cancel(ovsdb_client, Id)
monitor_cancel(Id) -> monitor_cancel(?SERVER, Id).

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
-spec lock(dst(), json_value()) -> rpc_return().
lock(Dst, Id) -> ovsdb_protocol:lock_ops(Dst, lock, Id).
%% @equiv lock(ovsdb_client, Id)
lock(Id) -> lock(?SERVER, Id).

%% @doc Steal lock
%%
%% This method would forcefully grab the database access which was previously locked
%% by Id by another process. This operation would notify the process which originally
%% had lock that the lock is stolen.
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.8">4.1.8. Lock Operations</a>
-spec steal(dst(), json_value()) -> rpc_return().
steal(Dst, Id) -> ovsdb_protocol:lock_ops(Dst, steal, Id).
%% @equiv steal(ovsdb_client, Id)
steal(Id) -> steal(?SERVER, Id).

%% @doc Unlock database
%%
%% Unlock the database by releasing the lock.
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.8">4.1.8. Lock Operations</a>
-spec unlock(dst(), json_value()) -> rpc_return().
unlock(Dst, Id) -> ovsdb_protocol:lock_ops(Dst, unlock, Id).
%% @equiv unlock(ovsdb_client, Id)
unlock(Id) -> unlock(?SERVER, Id).

%% @doc Echo
%%
%% This can be used to check by caller if the session is active. By default, ovsdb_client
%% performs echo - echo-reply handshakes to makesure session is active. This function can
%% be used for debugging.
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.11">4.1.11. Echo</a>
-spec echo(dst()) -> rpc_return().
echo(Dst) -> ovsdb_protocol:echo(Dst).
%% @equiv echo()
echo() -> echo(?SERVER).

%% @doc Get columns of table
%%
%% Reference <a href="https://tools.ietf.org/html/rfc7047#section-4.1.3">4.1.3. Transaction</a>
-spec get_columns(dst(), db_name(), db_table()) -> rpc_return().
get_columns(Dst, DbName, Table) -> ovsdb_protocol:get_columns(Dst, DbName, Table).
%% @equiv get_columns(ovsdb_client, DbName, Table)
get_columns(DbName, Table)      -> get_columns(?SERVER, DbName, Table).

%% Get a list of tables
-spec get_tables(dst(), db_name()) -> rpc_return().
get_tables(Dst, DbName) -> ovsdb_protocol:get_tables(Dst, DbName).
%% @equiv get_tables(ovsdb_client, DbName)
get_tables(DbName)      -> get_tables(?SERVER, DbName).

%% Get contents of database
-spec get_content(dst(), db_name(), select()) -> rpc_return().
get_content(Dst, DbName, Select) -> ovsdb_protocol:get_content(Dst, DbName, Select).
%% @equiv get_content(ovsdb_client, DbName, Select)
get_content(DbName, Select)      -> get_content(?SERVER, DbName, Select).

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
process_call({start, IpAddr, Port, _Opts}, _, State) ->
    ?INFO("Starting ovsdb with ~p:~p", [IpAddr, Port]),
    self() ! connect,
    {reply, ok, State#ovsdb_state{
        ip_addr = IpAddr,
        port = Port
    }};
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
