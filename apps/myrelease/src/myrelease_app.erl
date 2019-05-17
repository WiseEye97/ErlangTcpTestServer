%%%-------------------------------------------------------------------
%% @doc myrelease public API
%% @end
%%%-------------------------------------------------------------------

-module(myrelease_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    myrelease_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
