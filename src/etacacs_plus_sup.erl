%%%-------------------------------------------------------------------
%% @doc etacacs_plus top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(etacacs_plus_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

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
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [srv_spec(),
                  db_spec()],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

srv_spec() ->
    #{id => etacacs_plus_server     % mandatory
      , start => {etacacs_plus_server,start_link,[]} % mandatory
      %%restart => restart(),   % optional
      %%shutdown => shutdown(), % optional
      %%type => worker(),       % optional
      %%modules => modules()
     }.   % optional

db_spec() ->
    #{id => etacacs_plus_db     % mandatory
      , start => {etacacs_plus_db,start_link,[]} % mandatory
      %%restart => restart(),   % optional
      %%shutdown => shutdown(), % optional
      %%type => worker(),       % optional
      %%modules => modules()
     }.   % optional
