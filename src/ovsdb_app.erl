%%%-------------------------------------------------------------------
%% @doc ovsdb public API
%% @private
%% @end
%%%-------------------------------------------------------------------

-module(ovsdb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ovsdb_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
