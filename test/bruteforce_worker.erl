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
