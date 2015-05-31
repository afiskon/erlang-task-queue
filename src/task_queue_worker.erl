-module(task_queue_worker).

-behaviour(gen_server).

%% API
-export([
        start_link/1
    ]).

%% gen_server callbacks
-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).

-record(state, {
        worker_module :: atom(),
        worker_state :: any(),
        task_manager :: pid()
    }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Options) ->
    self() ! { init, Options },
    {ok, not_inited}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ init, Options }, not_inited) ->
    WorkerModule = proplists:get_value(worker_module, Options),
    WorkerModuleArgs = proplists:get_value(worker_module_args, Options),
    WorkerState = erlang:apply(WorkerModule, init, [ WorkerModuleArgs ]),

    TaskManager = proplists:get_value(task_manager, Options),
    TaskManager ! { get_task, self() },
    {noreply, #state{
            worker_module = WorkerModule,
            worker_state = WorkerState,
            task_manager = TaskManager
        }};

handle_info({task, Task},
        #state{
            worker_module = WorkerModule,
            worker_state = WorkerState,
            task_manager = TaskManager } = State) ->
    {ok, NewWorkerState} = erlang:apply(WorkerModule, process_task, [Task, WorkerState]),
    TaskManager ! { get_task, self() },
    {noreply, State#state{ worker_state = NewWorkerState }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{ worker_module = WorkerModule, worker_state = WorkerState }) ->
    erlang:apply(WorkerModule, terminate, [Reason, WorkerState]),
    ok.

code_change(
        OldVsn,
        #state{ worker_module = WorkerModule, worker_state = WorkerState } = State,
        Extra) ->
    {ok, NewWorkerState} = erlang:apply(WorkerModule, code_change, [OldVsn, WorkerState, Extra]),
    {ok, State#state{ worker_state = NewWorkerState } }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
