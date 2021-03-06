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
%% @doc
%% @private
%% @end
%% Created : 15. Jun 2020 7:20 AM
%%-------------------------------------------------------------------
-author("Vasu Dasari").

%%------------------------------------------------------------------------------
%% Logging macros
%%------------------------------------------------------------------------------

-define(DEBUG(Msg),
    ok = lager:debug(Msg)).
-define(DEBUG(Msg, Args),
    ok = lager:debug(Msg, Args)).

-define(INFO(Msg),
    ok = lager:info(Msg)).
-define(INFO(Msg, Args),
    ok = lager:info(Msg, Args)).

-define(WARNING(Msg),
    ok = lager:warning(Msg)).
-define(WARNING(Msg, Args),
    ok = lager:warning(Msg, Args)).

-define(ERROR(Msg),
    ok = lager:error(Msg)).
-define(ERROR(Msg, Args),
    ok = lager:error(Msg, Args)).
