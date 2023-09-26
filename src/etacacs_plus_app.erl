%%%-------------------------------------------------------------------
%% @doc etacacs_plus public API
%% @end
%%%-------------------------------------------------------------------

-module(etacacs_plus_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    etacacs_plus_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
