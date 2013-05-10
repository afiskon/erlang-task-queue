-module(task_queue_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    task_queue_fake_sup:start_link().

stop(_State) ->
    ok.
