-module(basic_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        behaviour_info_test,
        empty_queue_test,
        task_queue_in_test,
        task_queue_in_r_test,
        task_order_test,
        worker_state_test,
        worker_args_test,
        queue_length_test,
        unique_tasks_test,
        multiple_queues_test,
        queue_stopped_test,
        worker_terminate_test,
        monitor_test,
        md5_bruteforce_test
    ].

behaviour_info_test(_) ->
    Callbacks = task_queue:behaviour_info(callbacks),
    ?assertEqual(true, is_list(Callbacks)),
    Other = task_queue:behaviour_info(bebebe),
    ?assertEqual(undefined, Other).

empty_queue_test(_) ->
    {ok, TaskQueue} = task_queue:start(test_worker, []),
    ?assertEqual(0, task_queue:len(TaskQueue)),
    ?assertEqual(true, task_queue:is_empty(TaskQueue)).

task_queue_in_test(_) ->
    in_in_r_test(in).

task_queue_in_r_test(_) ->
    in_in_r_test(in_r).

task_order_test(_) ->
    {ok, TaskQueue} = task_queue:start(test_worker, [], [{workers_num, 1}]),
    task_queue:in({sleep, 200}, TaskQueue),
    task_queue:in({send, <<"in1">>, self()}, TaskQueue),
    task_queue:in_r({send, <<"in_r">>, self()}, TaskQueue),
    task_queue:in({send, <<"in2">>, self()}, TaskQueue),
    receive
        Msg1 -> ?assertEqual({task_complete, <<"in_r">>}, Msg1)
    after 500 ->
        throw(timeout1)
    end,
    receive
        Msg2 -> ?assertEqual({task_complete, <<"in1">>}, Msg2)
    after 500 ->
        throw(timeout2)
    end,
    receive
        Msg3 -> ?assertEqual({task_complete, <<"in2">>}, Msg3)
    after 500 ->
        throw(timeout3)
    end.

worker_state_test(_) ->
    {ok, TaskQueue} = task_queue:start(test_worker, []),
    task_queue:in({get_tasks_counter, self()}, TaskQueue),
    receive
        Msg ->
            ?assertEqual({task_complete, 0}, Msg)
    after 100 ->
        throw(timeout)
    end,
    [ task_queue:in(inc_tasks_counter, TaskQueue) || _N <- lists:seq(1,100) ],
    task_queue:in({get_tasks_counter, self()}, TaskQueue),
    receive
        {task_complete, TasksCounter} ->
            ?assert(TasksCounter > 0),
            ?assert(TasksCounter < 90)
    after 100 ->
        throw(timeout)
    end.

worker_args_test(_) ->
    InitArgs = [arg1, arg2, arg3],
    {ok, TaskQueue} = task_queue:start(test_worker, InitArgs),
    task_queue:in({get_init_args, self()}, TaskQueue),
    receive
        {task_complete, Result} ->
            ?assertEqual(InitArgs, Result)
    after 100 ->
        throw(timeout)
    end.

queue_length_test(_) ->
    WorkersNum = 42,
    MessagesNum = 100,
    {ok, TaskQueue} = task_queue:start(test_worker, [], [{workers_num, WorkersNum}]),
    ?assertEqual(false, task_queue:unique_tasks(TaskQueue)),
    [ task_queue:in({sleep, 1000}, TaskQueue) || _N <- lists:seq(1,MessagesNum) ],
    timer:sleep(100),
    ?assertEqual(MessagesNum - WorkersNum, task_queue:len(TaskQueue)),
    ?assertEqual(false, task_queue:is_empty(TaskQueue)).

unique_tasks_test(_) ->
    WorkersNum = 42,
    UniqueMessagesNum = 50,
    MessagesNum = UniqueMessagesNum * 2,
    {ok, TaskQueue} =
        task_queue:start(
            test_worker, [],
            [{workers_num, WorkersNum},{unique_tasks, true}]),
    ?assertEqual(true, task_queue:unique_tasks(TaskQueue)),
    [ task_queue:in({sleep, 1000 + Uniq}, TaskQueue) ||
        _N <- lists:seq(1, MessagesNum div UniqueMessagesNum),
        Uniq <- lists:seq(1, UniqueMessagesNum) ],
    timer:sleep(100),
    ?assert(task_queue:len(TaskQueue) < MessagesNum - WorkersNum),
    ?assertNotEqual(UniqueMessagesNum - WorkersNum, task_queue:len(TaskQueue)).

multiple_queues_test(_) ->
    ArgsOfQueueA = [ queue_a ],
    ArgsOfQueueB = [ queue_a ],
    {ok, TaskQueueA} = task_queue:start(test_worker, ArgsOfQueueA),
    {ok, TaskQueueB} = task_queue:start(test_worker, ArgsOfQueueB),
    task_queue:in({get_init_args, self()}, TaskQueueA),
    task_queue:in({get_init_args, self()}, TaskQueueB),
    receive
        {task_complete, ArgsOfQueueA} -> ?assert(true)
    after 100 ->
        throw(timeout1)
    end,
    receive
        {task_complete, ArgsOfQueueB} -> ?assert(true);
        Msg -> throw({unexpected_message, Msg})
    after 100 ->
        throw(timeout2)
    end.

queue_stopped_test(_) ->
    {ok, TaskQueue} = task_queue:start(test_worker, []),
    task_queue:stop(TaskQueue),
    timer:sleep(100),
    ?assertExit({noproc, _}, task_queue:unique_tasks(TaskQueue)),
    ?assertExit({noproc, _}, task_queue:len(TaskQueue)),
    ?assertExit({noproc, _}, task_queue:is_empty(TaskQueue)).

worker_terminate_test(_) ->
    {ok, TaskQueue} = task_queue:start(test_worker, []),
    task_queue:in({notify_when_terminated, self()}, TaskQueue),
    timer:sleep(100),
    task_queue:stop(TaskQueue),
    receive
        Msg -> ?assertMatch({worker_terminated, _}, Msg)
    after 100 ->
        throw(timeout)
    end.

monitor_test(_) ->
    {ok, TaskQueue} =
        task_queue:start(
            test_worker, [],
            [{workers_num, 1}, {workers_max_r, 5}, {workers_max_t, 10}]),
    task_queue:in({suicide_after, 100}, TaskQueue),
    timer:sleep(200),
    task_queue:in({send, <<"task1">>, self()}, TaskQueue),
    task_queue:in({send, <<"task2">>, self()}, TaskQueue),
    receive
        {task_complete, <<"task1">>} -> ?assert(true)
    after 100 ->
        throw(timeout1)
    end,
    receive
        {task_complete, <<"task2">>} -> ?assert(true)
    after 100 ->
        throw(timeout2)
    end.

md5_bruteforce_test(_) ->
    Hash = <<96,217,158,88,214,106,94,15,79,137,236,61,221,29,154,128>>,
    {ok, TaskQueue} = task_queue:start(bruteforce_worker, [], [{workers_num, 10}]),
    Alph = lists:seq($a, $z),
    [task_queue:in({md5_bruteforce, [A], Alph, Hash, self()}, TaskQueue)
        || A <- Alph],
    receive
        {password_found, Pass} -> ?assertEqual("rome", Pass)
    after 10000 ->
        throw(timeout)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

in_in_r_test(Method) ->
    {ok, TaskQueue} = task_queue:start(test_worker, []),
    task_queue:Method({send, <<"msg123">>, self()}, TaskQueue),
    receive
        Msg ->
            ?assertEqual({task_complete, <<"msg123">>}, Msg),
            ?assertEqual(true, task_queue:is_empty(TaskQueue))
    after 100 ->
        throw(timeout)
    end.

