-module(task_queue_manager).

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
        queue = queue:new() :: queue(),
        waiting_workers = [] :: [pid()],
        monitors = dict:new() :: dict(),
        unique_tasks = false :: boolean(),
        queue_items = sets:new() :: set(),
        queue_length = 0 :: non_neg_integer() % queue:len() is O(N)
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
    {ok, #state{
            unique_tasks = proplists:get_value(unique_tasks, Options, false)
        }}.

handle_call(len, _From, #state{ queue_length = QueueLength } = State) ->
    {reply, QueueLength, State};

handle_call(unique_tasks, _From, #state{ unique_tasks = UniqueTasks } = State) ->
    {reply, UniqueTasks, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({Method, Task}, State)
        when Method =:= in;
             Method =:= in_r ->
    NewState =
        case there_are_waiting_workers(State) of
            true ->
                Worker = get_first_waiting_worker(State),
                Worker ! { task, Task},
                del_first_waiting_worker(State);
            false ->
                enqueue_task(Method, Task, State)
        end,
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({get_task, WorkerPid}, #state{ queue_length = 0 } = State) ->
    {noreply, add_waiting_worker(WorkerPid, State)};

handle_info({get_task, WorkerPid}, State) ->
    {Task, NewState} = dequeue_task(State),
    WorkerPid ! { task, Task },
    {noreply, NewState};

handle_info({'DOWN', _MonitorRef, _Type, Pid, _Info}, #state{} = State) ->
    {noreply, del_died_waiting_worker(Pid, State)};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

enqueue_task(Method, Task,
        #state{
            unique_tasks = false,
            queue = Queue,
            queue_length = QueueLength } = State) ->
    State#state{
        queue = queue:Method(Task, Queue),
        queue_length = QueueLength + 1
    };

enqueue_task(Method, Task,
        #state{
            unique_tasks = true,
            queue = Queue,
            queue_items = QueueItems,
            queue_length = QueueLength } = State) ->
    case sets:is_element(Task, QueueItems) of
        true -> State;
        false ->
            State#state{
                queue = queue:Method(Task, Queue),
                queue_items = sets:add_element(Task, QueueItems),
                queue_length = QueueLength + 1
            }
    end.

dequeue_task(#state{ queue = Queue, queue_items = QueueItems, queue_length = QueueLength } = State) ->
    {{value, Task}, NewQueue} = queue:out(Queue),
    NewState = State#state{
            queue = NewQueue,
            queue_items = sets:del_element(Task, QueueItems),
            queue_length = QueueLength - 1 
        },
    { Task, NewState }.

there_are_waiting_workers(#state{ waiting_workers = []}) ->
    false;

there_are_waiting_workers(#state{ waiting_workers = [ _ | _ ]}) ->
    true.

get_first_waiting_worker(#state{ waiting_workers = [ Worker | _ ]}) ->
    Worker.

add_waiting_worker(Worker,
        #state{ monitors = MonitorDict, waiting_workers = WaitingWorkers } = State) ->
    MonitorRef = erlang:monitor(process, Worker),
    State#state{
        monitors = dict:store(Worker, MonitorRef, MonitorDict),
        waiting_workers = [Worker | WaitingWorkers ]
    }.

del_first_waiting_worker(
        #state{
                monitors = MonitorDict,
                waiting_workers = [ Worker | WaitingWorkersTail ]
            } = State) ->
    {ok, MonitorRef} = dict:find(Worker, MonitorDict),
    erlang:demonitor(MonitorRef),
    State#state{
            monitors = dict:erase(Worker, MonitorDict),
            waiting_workers = WaitingWorkersTail
        }.

del_died_waiting_worker(Worker,
        #state{
                monitors = MonitorDict,
                waiting_workers = WaitingWorkers
            } = State) ->
    State#state{
            monitors = dict:erase(Worker, MonitorDict),
            waiting_workers = WaitingWorkers -- [Worker]
        }.

