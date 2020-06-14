%%%-------------------------------------------------------------------
%% @doc ovsdb top level supervisor.
%% @private
%% @end
%%%-------------------------------------------------------------------

-module(ovsdb_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(Process(Name, Type),
    {Name, {Name, start_link, []}, permanent, 2000, Type, [Name]}).

-define(Process(Name, Type, Args),
    {Name, {Name, start_link, [Args]}, permanent, 2000, Type, [Name]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    _Restart = permanent,
    _Shutdown = 2000,
    _Type = worker,

    ProcessList = [
        ?Process(ovsdb_client,worker)
    ],

    {ok, {SupFlags,ProcessList}}.


%% internal functions
