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
%%% Created : 18. Jun 2020
%%%-------------------------------------------------------------------

-author("Vasu Dasari").

-type ops_function()    :: ('<' | '<=' | '==' | '!=' | '>=' | '>' | 'includes' | 'excludes').
-record(ovsdb_condition, {
    column      :: json_value(),
    function    :: ops_function(),
    value       :: json_value()
}).
