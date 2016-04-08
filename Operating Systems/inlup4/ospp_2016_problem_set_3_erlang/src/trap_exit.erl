-module(trap_exit).
-export([start/0]).

start() ->
    process_flag(trap_exit, true),
    spawn_link(fun() -> exit(bang) end),
    receive 
        {'EXIT', PID, Reason} -> 
            io:format("~p Process ~w terminated with reason ~w!~n", [self(), PID, Reason])
    end.
