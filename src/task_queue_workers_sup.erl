-module(task_queue_workers_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     transient, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Options) ->
    supervisor:start_link(?MODULE, Options).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(Options) ->
    MaxR = proplists:get_value(workers_max_r, Options, 0),
    MaxT = proplists:get_value(workers_max_t, Options, 1),
    WorkersNum = proplists:get_value(workers_num, Options, 10),
    WorkersSpec = [
            ?CHILD(worker_id(N), task_queue_worker, worker, [Options])
            || N <- lists:seq(1, WorkersNum)
        ],
    {ok, {{one_for_one, MaxR, MaxT}, WorkersSpec}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

worker_id(N) ->
    list_to_binary(["worker_" | integer_to_list(N)]).
