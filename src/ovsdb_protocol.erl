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
%% @doc OVSDB RPC Wire Protocol Methods implementation
%% @private
%% @end
%% Created : 15. Jun 2020 2:30 PM
%%-------------------------------------------------------------------
-module(ovsdb_protocol).
-author("Vasu Dasari").

-include("ovsdb_client.hrl").

%% API
-export([
    list_dbs/1,
    get_schema/1,
    transaction/2,
    cancel/2,
    monitor/3,
    monitor_cancel/2,
    lock_ops/3,
    echo/1
]).

-export([
    list_columns/2,
    list_tables/1,
    get_content/2
]).

-export([
    process_message/2
    , rpc2map/3]).

list_dbs(Opts) ->
    gen_server:call(get_proc(Opts), {send, rpc2map(list_dbs, 0, [])}).

get_schema(Opts) ->
    gen_server:call(get_proc(Opts), {send, rpc2map(get_schema, 0, [get_database(Opts)])}).

transaction(Operation, Opts) when not is_list(Operation) ->
    transaction([Operation], Opts);
transaction(Operations, Opts) when is_list(Operations) ->
    gen_server:call(get_proc(Opts), {send, rpc2map(transact, 0, [get_database(Opts)] ++ Operations)}).

cancel(Operation, Opts) when not is_list(Operation) ->
    cancel(Opts, [Operation]);
cancel(Operations, Opts) when is_list(Operations) ->
    gen_server:call(get_proc(Opts), {send, rpc2map(cancel, 0, [get_database(Opts)] ++ Operations)}).

monitor(Id, Select, Opts) ->
    monitor(self(), Id, Select, Opts).

monitor(FromPid, Id, Table, Opts) when is_binary(Table); is_atom(Table) ->
    case list_columns(Table, Opts) of
        {ok, Columns} ->
            monitor(FromPid, Id,
                [#{Table => #{<<"columns">> => Columns}}], Opts
            );
        R -> R
    end;
monitor(FromPid, Id, Reqs, Opts) when is_list(Reqs) ->
    gen_server:call(get_proc(Opts), {send,
        rpc2map(monitor, Id, [get_database(Opts), id_with_pid(Id, FromPid)] ++ Reqs)}).

monitor_cancel(Id, Opts) ->
    gen_server:call(get_proc(Opts), {send,
        rpc2map(monitor_cancel, Id, [id_with_pid(Id, self())])}).

lock_ops(Op, Id, Opts) when is_list(Id) ->
    lock_ops(Op, list_to_binary(Id), Opts);
lock_ops(Op, Id, Opts) when Op == lock; Op == steal; Op == unlock ->
    gen_server:call(get_proc(Opts), {lock, Op, Id, self(), rpc2map(Op, Id, [Id])}).

echo(Opts) ->
    gen_server:call(get_proc(Opts), {send, rpc2map(echo, "echo", [])}).

list_columns(Table, Opts) ->
    case get_schema(Opts) of
        {ok, #{<<"tables">> := #{Table := #{<<"columns">> := ColSchema}}}} ->
            {ok, maps:keys(ColSchema)};
        Ret ->
            Ret
    end.

list_tables(Opts) ->
    case get_schema(Opts) of
        {ok, #{<<"tables">> := TableSchema}} ->
            {ok, maps:keys(TableSchema)};
        Ret ->
            Ret
    end.

get_content(Table, Opts) when is_binary(Table); is_atom(Table) ->
    case list_columns(Table, Opts) of
        {ok, Columns} ->
            get_content([#{Table => #{<<"columns">> => Columns}}], Opts);
        R -> R
    end;
get_content(Operations, Opts) ->
    transaction(Operations, Opts).

process_message(
        #{<<"method">> := <<"echo">>, <<"id">> := Id, <<"params">> := Params}, State) ->
    ok = gen_server:cast(self(), {send, rpc2map(echo_reply, Id, Params)}),
    State;
process_message(
        #{<<"method">> := <<"update">>, <<"params">> := [PidInfo, Update]},
        State) ->
    [Id, PidList] = string:split(PidInfo, ","),
    erlang:send(
        erlang:list_to_pid(erlang:binary_to_list(PidList)),
        {ovsdb_monitor, Id, Update}),
    State;
process_message(
        #{<<"method">> := Method, <<"params">> := [LockId]},
        #ovsdb_state{locks_map = LocksMap} = State)
    when Method == <<"locked">>; Method == <<"stolen">> ->
    ok = case maps:get(LockId, LocksMap, not_found) of
        not_found -> ok;
        Pid ->
            Pid ! {ovsdb_notification, erlang:binary_to_atom(Method, utf8), LockId},
            ok
    end,
    State;
process_message(
        #{<<"error">> := Error, <<"id">> := Id} = Message,
        #ovsdb_state{pending_messages = PendingMsgs} = State) ->
    Result = maps:get(<<"result">>, Message, #{}),
    case maps:get(Id, PendingMsgs, not_found) of
        not_found ->
            State;
        #{from := From} ->
            ReplyData = case Error of
                null -> {ok, Result};
                _ -> {error, {Error, Result}}
            end,
            gen_server:reply(From, ReplyData),
            State#ovsdb_state{pending_messages = maps:remove(Id, PendingMsgs)}
    end.

%%%===================================================================
%%% Rpc Request as Erlang Map
%%%===================================================================
rpc2map(Method, Id, Params) when is_list(Id)->
    rpc2map(Method, erlang:list_to_binary(Id), Params);
rpc2map(echo, Id, Params) ->
    #{id => Id, method => echo, params => Params};
rpc2map(echo_reply, Id, Params) ->
    #{id => Id, error => null, result => Params};
rpc2map(list_dbs, Id, Params) ->
    #{id => Id, method => list_dbs, params => Params};
rpc2map(get_schema, Id, Params) ->
    #{id => Id, method => get_schema, params => Params};
rpc2map(transact, Id, Params) ->
    #{id => Id, method => transact, params => Params};
rpc2map(cancel, Id, Params) ->
    #{id => Id, method => cancel, params => Params};
rpc2map(monitor, Id, Params = [_DbName, _JsonVal, _MonitorRequests]) ->
    #{id => Id, method => monitor, params => Params};
rpc2map(update, Id, Params) ->
    #{id => Id, method => update, params => Params};
rpc2map(monitor_cancel, Id, Params) ->
    #{id => Id, method => monitor_cancel, params => Params};
rpc2map(lock, Id, Params) ->
    #{id => Id, method => lock, params => Params};
rpc2map(steal, Id, Params) ->
    #{id => Id, method => steal, params => Params};
rpc2map(unlock, Id, Params) ->
    #{id => Id, method => unlock, params => Params};
rpc2map(Invalid, _, _) ->
    { error, { not_supported, Invalid }}.

id_with_pid(Id, FromPid) ->
    erlang:list_to_binary(Id ++ "," ++ erlang:pid_to_list(FromPid)).

get_database(#{database := DbName}) -> DbName;
get_database(Opts) -> ovsdb_client:get_database(get_proc(Opts)).

get_proc(Opts) -> ovsdb_client:get_proc(Opts).