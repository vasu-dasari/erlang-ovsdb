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
%%% Created : 06. Jul 2020
%%%-------------------------------------------------------------------

-module(ovsdb_bhvr).
-author("Vasu Dasari").

%% API
-export([]).

-callback handle_message(
        Request :: term(),
        State :: term()) ->
    {'ok', State::term()} | {'error', Reason::term()}.

-callback notify(
        Op :: term(),
        Message :: term(),
        State :: term()) ->
    {'ok', State::term()} | {'error', Reason::term()}.

-callback info(
        Request :: term(),
        State :: term()) ->
    {'ok', State::term()} | {'error', Reason::term()}.

