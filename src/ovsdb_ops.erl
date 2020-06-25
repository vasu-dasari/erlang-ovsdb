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
%% @doc OVSDB Database manipulation methods
%%
%% @end
%% Created : 16. Jun 2020 7:39 AM
%%-------------------------------------------------------------------
-module(ovsdb_ops).
-author("Vasu Dasari").
-include("ovsdb_client.hrl").

%% API
-export([
    insert/2, insert/3,
    select/3,
    update/3,
    mutate/3,
    delete/2,
    wait/4, wait/5,
    commit/1,
    abort/0,
    comment/1,
    assert/1
]).

insert(Table, Row) ->
    #{op => insert, table => Table, row => Row}.

insert(Table, Row, Id) ->
    maps:merge(insert(Table, Row), #{<<"uuid-name">> => Id}).

select("*", Table, Conditions) ->
    #{op => select, table => Table,
        where => conditions(Conditions)};
select(Columns, Table, Conditions) ->
    #{op => select, table => Table,
        where => conditions(Conditions), columns => Columns}.

update(Table, Conditions, Row) ->
    #{op => update, table => Table,
        where => conditions(Conditions), row => Row}.

mutate(Table, Conditions, Mutations) ->
    #{op => mutate, table => Table,
        where => conditions(Conditions),
        mutations => conditions(Mutations)}.

delete(Table, Conditions) ->
    #{op => delete, table => Table, where => conditions(Conditions)}.

wait(Table, Conditions, Rows, Until) ->
    wait(Table, Conditions, Rows, Until, []).
wait(Table, Conditions, Rows, Until, Columns) ->
    #{op => wait, timeout => 10, table => Table,
        where => conditions(Conditions),
        columns => Columns, until => Until, rows => Rows}.

commit(Mode) ->
    #{op => commit, durable => Mode}.

abort() ->
    #{op => abort}.

comment(Comment) ->
    #{op => comment, comment => Comment}.

assert(Lock) ->
    #{op => assert, lock => Lock}.

%%%===================================================================
%%% Helpers
%%%===================================================================
conditions(Conditions) ->
    lists:foldl(fun
        ({C, F, V}, Acc) ->
            [[ovsdb_utils:to_binstring(C),
                ovsdb_utils:to_binstring(F),
                V] | Acc];
        (#ovsdb_condition{column = C, function = F, value = V}, Acc) ->
            [[ovsdb_utils:to_binstring(C),
                ovsdb_utils:to_binstring(F),
                V] | Acc]
    end, [], Conditions).

