Erlang Task Queue Behaviour
---------------------------
Module example:

    -module(bruteforce_worker).
    
    -behaviour(task_queue).
    
    -export([
            init/1,
            process_task/2,
            terminate/2,
            code_change/3
        ]).
    
    -record(state, {}).
    
    init(_Args) ->
        #state{}.
    
    process_task({md5_bruteforce, Postfix, Alph, Hash, Receiver}, State) ->
        [ begin 
            Password = [Prefix1, Prefix2, Prefix3 | Postfix],
            case crypto:md5(Password) =:= Hash of
                true ->
                    Receiver ! { password_found, Password };
                false ->
                    ok
            end
          end || Prefix1 <- Alph, Prefix2 <- Alph, Prefix3 <- Alph ],
        {ok, State}.
    
    terminate(_Reason, _State) ->
        ok.
    
    code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

Usage:

    eax@home> make
    ...
    eax@home> erl -pa ebin -pa ./test
    1> {ok, TaskQueue} = task_queue:start_link(bruteforce_worker, [], [{workers_num, 10},{unique_tasks, false}]).
    {ok,<0.133.0>}
    2> task_queue:is_empty(TaskQueue).
    true
    3> Hash = crypto:md5("hello").
    <<93,65,64,42,188,75,42,118,185,113,157,145,16,23,197,146>>
    4> Alph = lists:seq($a, $z).
    "abcdefghijklmnopqrstuvwxyz"
    5> Parent = self().
    <0.33.0>
    6> spawn(fun() -> [task_queue:in({md5_bruteforce, [A,B], Alph, Hash, Parent}, TaskQueue) || A <- Alph, B <- Alph] end).
    <0.149.0>
    7> flush().
    ok
    7> flush().
    Shell got {password_found,"hello"}
    ok
    8> task_queue:stop(TaskQueue).
    true
    ** exception error: shutdown
