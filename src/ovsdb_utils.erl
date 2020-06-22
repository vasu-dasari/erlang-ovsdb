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
%% @doc Helper utilities module
%% @private
%% @end
%% Created : 15. Jun 2020 7:20 AM
%%-------------------------------------------------------------------
-module(ovsdb_utils).
-author("Vasu Dasari").

-include("logger.hrl").
%% API
-export([pretty_print/1, backtrace/0, whocalledme/0, to_binstring/1]).

pretty_print(Item) ->
    io_lib:format("~s",[io_lib_pretty:print(Item)]).

backtrace() ->
    try throw({ok,whocalledme})
    catch
        _:_:StackTrace ->
            ?INFO("StackTrace ~n~s",
                [pretty_print(StackTrace)])
    end.

whocalledme() ->
    try throw({ok,whocalledme})
    catch
        _:_:StackTrace ->
            StackTrace
    end.

to_binstring([I|_] = Term) when is_list(Term), not is_binary(I) ->
    erlang:list_to_binary(Term);
to_binstring(Term) when is_atom(Term) ->
    erlang:atom_to_binary(Term, utf8);
to_binstring(Term) when is_number(Term) ->
    erlang:integer_to_binary(Term);
to_binstring(Term) ->
    Term.