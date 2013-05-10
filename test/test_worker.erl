-module(test_worker).

-export([
        init/1,
        process_task/2,
        terminate/2,
        code_change/3
    ]).

-behaviour(task_queue).
-record(state, {
        tasks_counter = 0 :: non_neg_integer(),
        init_args = [] :: any(),
        notify_when_terminated :: pid()
    }).

init(Args) ->
    #state{ init_args = Args, notify_when_terminated = bebebe }.

process_task({send, Message, Receiver}, #state{ tasks_counter = TasksCounter } = State) ->
    Receiver ! { task_complete, Message },
    {ok, State#state{ tasks_counter = TasksCounter + 1 }};

process_task({sleep, Time}, #state{ tasks_counter = TasksCounter } = State) ->
    timer:sleep(Time),
    {ok, State#state{ tasks_counter = TasksCounter + 1 }};

process_task(inc_tasks_counter, #state{ tasks_counter = TasksCounter } = State) ->
    {ok, State#state{ tasks_counter = TasksCounter + 1 }};

process_task({get_init_args, Receiver}, #state{ init_args = Args, tasks_counter = TasksCounter } = State) ->
    Receiver ! { task_complete, Args },
    {ok, State#state{ tasks_counter = TasksCounter + 1 }};

process_task({get_tasks_counter, Receiver}, #state{ tasks_counter = TasksCounter } = State) ->
    Receiver ! { task_complete, TasksCounter },
    {ok, State#state{ tasks_counter = TasksCounter + 1 }};

process_task({suicide_after, Timeout}, #state{ tasks_counter = TasksCounter } = State) ->
    Parent = self(),
    spawn(fun() ->
        timer:sleep(Timeout),
        erlang:exit(Parent, kill) 
    end),
    {ok, State#state{ tasks_counter = TasksCounter + 1 }};

process_task({notify_when_terminated, Pid}, #state{ tasks_counter = TasksCounter } = State) ->
    erlang:process_flag(trap_exit, true),
    {ok, State#state{
            notify_when_terminated = Pid,
            tasks_counter = TasksCounter + 1
        }}.

terminate(_Reason, #state{ notify_when_terminated = Pid })
        when is_pid(Pid) ->
    Pid ! { worker_terminated, self() },
    ok;

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
