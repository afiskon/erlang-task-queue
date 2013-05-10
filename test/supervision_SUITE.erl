-module(supervision_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        start_stop_test,
        start_link_stop_test,
        multiple_task_queues_test,
        workers_num_test,
        sup_kill_test,
        workers_sup_kill_test,
        queue_manager_kill_test,
        worker_kill_test,
        worker_restart_test
    ].

start_stop_test(_) ->
    ProcessCountBefore = erlang:system_info(process_count),
    {ok, TaskQueue} = task_queue:start(test_worker, []),
    ProcessCountAfter = erlang:system_info(process_count),
    ?assert(ProcessCountAfter > ProcessCountBefore),
    task_queue:stop(TaskQueue),
    timer:sleep(100),
    ?assertEqual(ProcessCountBefore, erlang:system_info(process_count)).

start_link_stop_test(_) ->
    {ok, TaskQueue} = task_queue:start_link(test_worker, []),
    erlang:process_flag(trap_exit, true),
    task_queue:stop(TaskQueue),
    receive
        Msg ->
            ?assertMatch({'EXIT', _, shutdown}, Msg)
    after 100 ->
        throw(timeout)
    end.

multiple_task_queues_test(_) ->
    {ok, TaskQueue1} = task_queue:start_link(test_worker, []),
    {ok, TaskQueue2} = task_queue:start_link(test_worker, []),
    ?assertNotEqual(TaskQueue1, TaskQueue2).

sup_kill_test(_) ->
    ProcessCountBefore = erlang:system_info(process_count),
    TaskQueueSup = start_and_get_supervisor(),
    erlang:exit(TaskQueueSup, kill),
    timer:sleep(100),
    ?assertEqual(ProcessCountBefore, erlang:system_info(process_count)).

workers_num_test(_) ->
    TaskQueueSup = start_and_get_supervisor([{workers_num, 42}]),
    Children = supervisor:which_children(TaskQueueSup),
    {_, WorkersSup, _, _} = lists:keyfind(<<"task_queue_workers_sup">>, 1, Children),
    WorkerList = supervisor:which_children(WorkersSup),
    ?assertEqual(42, length(WorkerList)).

workers_sup_kill_test(_) ->
    ProcessCountBefore = erlang:system_info(process_count),
    TaskQueueSup = start_and_get_supervisor(),
    Children = supervisor:which_children(TaskQueueSup),
    {_, WorkersSup, _, _} = lists:keyfind(<<"task_queue_workers_sup">>, 1, Children),
    erlang:exit(WorkersSup, kill),
    timer:sleep(100),
    ?assertEqual(ProcessCountBefore, erlang:system_info(process_count)).

queue_manager_kill_test(_) ->
    ProcessCountBefore = erlang:system_info(process_count),
    TaskQueueSup = start_and_get_supervisor(),
    Children = supervisor:which_children(TaskQueueSup),
    {_, QueueManager, _, _} = lists:keyfind(<<"task_queue_manager">>, 1, Children),
    erlang:exit(QueueManager, kill),
    timer:sleep(100),
    ?assertEqual(ProcessCountBefore, erlang:system_info(process_count)).

worker_kill_test(_) ->
    ProcessCountBefore = erlang:system_info(process_count),
    TaskQueueSup = start_and_get_supervisor(),
    Children = supervisor:which_children(TaskQueueSup),
    {_, WorkersSup, _, _} = lists:keyfind(<<"task_queue_workers_sup">>, 1, Children),
    WorkerList = supervisor:which_children(WorkersSup),
    {_, Worker, _, _} = lists:keyfind(<<"worker_10">>, 1, WorkerList),
    erlang:exit(Worker, kill),
    timer:sleep(100),
    ?assertEqual(ProcessCountBefore, erlang:system_info(process_count)).

worker_restart_test(_) ->
    ProcessCountBefore = erlang:system_info(process_count),
    TaskQueueSup = start_and_get_supervisor([{workers_max_r, 5}, {workers_max_t, 10}]),
    ProcessCountAfter = erlang:system_info(process_count),
    Children = supervisor:which_children(TaskQueueSup),
    {_, WorkersSup, _, _} = lists:keyfind(<<"task_queue_workers_sup">>, 1, Children),
    [{_, FirstWorker, _, _} | RestWorkers ] = supervisor:which_children(WorkersSup),
    erlang:exit(FirstWorker, kill),
    timer:sleep(100),
    ?assertEqual(ProcessCountAfter, erlang:system_info(process_count)),
    [erlang:exit(W, kill) || {_, W, _, _} <- lists:sublist(RestWorkers, 4)],
    timer:sleep(100),
    ?assertEqual(ProcessCountAfter, erlang:system_info(process_count)),
    [erlang:exit(W, kill) || {_, W, _, _} <- lists:sublist(RestWorkers, 5)],
    timer:sleep(100),
    ?assertEqual(ProcessCountBefore, erlang:system_info(process_count)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_and_get_supervisor() ->
    start_and_get_supervisor([]).

start_and_get_supervisor(Options) ->
    ProcessCountBefore = erlang:system_info(process_count),
    {ok, TaskQueue} = task_queue:start(test_worker, [], Options),
    ProcessCountAfter = erlang:system_info(process_count),
    ?assert(ProcessCountAfter > ProcessCountBefore),
    {links, [TaskQueueSup]} = erlang:process_info(TaskQueue, links),
    TaskQueueSup.
