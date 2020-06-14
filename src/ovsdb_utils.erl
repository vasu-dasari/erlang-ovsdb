%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Jun 2020 7:20 AM
%%%-------------------------------------------------------------------
-module(ovsdb_utils).
-author("vdasari").

-include("logger.hrl").
%% API
-export([pretty_print/1, backtrace/0, whocalledme/0]).

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
