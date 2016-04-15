%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4, manage_calc_workers/4, combine_results/4, manage_calc_workers/5]).
-include_lib("eunit/include/eunit.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Split converts all the numbers from decimal to their own base.
%% It also splits up the numbers in to a list, which allows them 
%% to be paired against eachother for the calculation.
-spec split (A, Base) -> List when
  A::integer(),
  Base::integer(),
  List::[integer()].

split (0, _) ->
  [];

split (A, Base) ->
  Rest = A div Base,
  Result = A rem Base,
  [Result | split (Rest, Base)].


%% @doc insert_at inserts Object at position Index at list, moving the list instead of replacing.
%% Primarily used to make sure the result from our workers are in the correct order,
%% something which they generally do anyway since they'll all be running on the same OSthread, but
%% just in case.
-spec insert_at(List, Object, Index) -> List when
  List::[integer()],
  Object::integer(),
  Index::integer().

insert_at([], Object, _Index) ->
  [Object];

insert_at([H|T], Object, 0) ->
  [H|Object] ++ T;

insert_at([H|T], Object, Index) ->
  [H] ++ insert_at(T, Object, Index - 1).


%% @doc combine_result is the "listener" in this program, simply listens to messages and enters
%% them in the correct order (using insert_at), in the correct list.
-spec combine_results(ResultList, CarryList, Owner, WorkerPIDList) -> 
    {ResultList, CarryList} when
  ResultList::[integer()],
  CarryList::[integer()],
  Owner::integer(),
  WorkerPIDList::[integer()].

combine_results(ResultList,CarryList,Owner, []) ->
  Owner ! {ResultList, CarryList};

combine_results(ResultList, CarryList, Owner, [_H|T]) ->
  receive
    {Index, {Result, Carry}} ->
      NewResult = insert_at(ResultList, Result, Index), %%[Result | ResultList], 
      NewCarryList = insert_at(CarryList, Carry, Index), %%[Carry | CarryList], %%
      combine_results(NewResult, NewCarryList, Owner, T)
  end.

%% @doc Used to "Fan Out" to all the workers, sending them the correct carries and receiving 
%% the correct input, then sends that output to combine_results
-spec send_carry(Combinator, WorkerPIDList, Carry) -> ok when
  Combinator::integer(),
  WorkerPIDList::[integer()],
  Carry::integer().

send_carry(Combinator, [], _Carry) -> 
  receive
    {Index, {Result, NewCarry}} ->
      Combinator ! {Index, {Result, NewCarry}}
  end,
  exit(success);

send_carry(Combinator, [Worker|TailWorkers], Carry) ->
  Worker ! {self(), Carry},
  receive
    {Index, {Result, NewCarry}} ->
      Combinator ! {Index, {Result, NewCarry}},
      send_carry(Combinator, TailWorkers, NewCarry)
  end.

%% @doc send_carry/4 adds an optional {SleepMin, SleepMax} to add in a random sleep process
-spec send_carry(Combinator, WorkerPIDList, Carry, {SleepMin, SleepMax}) -> ok when
  Combinator::integer(),
  WorkerPIDList::[integer()],
  Carry::integer(),
  SleepMin::integer(),
  SleepMax::integer().

send_carry(_Combinator, [], _Carry, {_SleepMin,_SleepMax}) -> 
  exit(success);

send_carry(Combinator, [Worker|TailWorkers], Carry, {SleepMin, SleepMax}) ->
  Worker ! {self(), Carry},
  receive
    {Index, {Result, NewCarry}} ->
      %%Randdomize between SleepMin and SleepMax, how?
      timer:sleep(SleepMin*1000),
      Combinator ! {Index, {Result, NewCarry}},
      send_carry(Combinator, TailWorkers, NewCarry, {SleepMin, SleepMax})
    after (SleepMax + SleepMin) ->
      exit(success)
  end.


%% @doc The process responsible for spawning the right amount of workers and giving them the
%% correct info to calculate, returns a list containing all the worker PIDs. /3 simply moves it
%% to /4
-spec spawn_calc_workers(A, B, Base) -> WorkerPIDList when
  A::[integer()],
  B::[integer()],
  Base::integer(),
  WorkerPIDList::[integer()].

spawn_calc_workers(A, B, Base) -> 
  spawn_calc_workers(A, B, Base, 0).

%% @doc The process responsible for spawning the right amount of workers and giving them the
%% correct info to calculate, returns a list containing all the worker PIDs.
-spec spawn_calc_workers(A, B, Base, Index) -> WorkerPIDList when
  A::[integer()],
  B::[integer()],
  Base::integer(),
  Index::integer(),
  WorkerPIDList::[integer()].

spawn_calc_workers([], [], Base, Index) ->
  [spawn(calc, calc_worker_spec, [0, 0, Base, Index])];
spawn_calc_workers([HA|TA], [], Base, Index) ->
  [spawn(calc, calc_worker_spec, [HA, 0, Base, Index]) 
  | spawn_calc_workers(TA, [], Base, Index + 1)];
spawn_calc_workers([], [HB|TB], Base, Index) ->
  [spawn(calc, calc_worker_spec, [0, HB, Base, Index]) 
  | spawn_calc_workers([], TB, Base, Index + 1)];
spawn_calc_workers([HA|TA], [HB|TB], Base, Index) ->
  [spawn(calc, calc_worker_spec, [HA, HB, Base, Index]) 
  | spawn_calc_workers(TA, TB, Base, Index + 1)].

%% @doc The "Parent" of all the workers, the process responsible for the calculation and 
%% concurrent part of this program.
-spec manage_calc_workers(A, B, Base, Parent) -> {Result, CarryList} when
  A::[integer()],
  B::[integer()],
  Base::integer(),
  Parent::integer(),
  Result::[integer()],
  CarryList::[integer()].

manage_calc_workers(A, B, Base, Parent) ->
  WorkerPIDList = spawn_calc_workers(A, B, Base), %%Reverse this list?
  CombinePID = spawn(add, combine_results, [[], [], Parent, WorkerPIDList]),
  send_carry(CombinePID, WorkerPIDList, 0).

-spec manage_calc_workers(A, B, Base, Parent, Options) -> {Result, CarryList} when
  A::[integer()],
  B::[integer()],
  Base::integer(),
  Parent::integer(),
  Result::[integer()],
  CarryList::[integer()],
  Option::atom() | tuple(),
  Options::[Option].

manage_calc_workers(A, B, Base, Parent, [sleep, {SleepMin, SleepMax}]) -> 
  WorkerPIDList = spawn_calc_workers(A, B, Base), %%Reverse this list?
  CombinePID = spawn(add, combine_results, [[], [], Parent, WorkerPIDList]),
  send_carry(CombinePID, WorkerPIDList, 0, {SleepMin, SleepMax});

manage_calc_workers(A, B, Base, Parent, []) ->
  manage_calc_workers(A, B, Base, Parent);

manage_calc_workers(A, B, Base, Parent, _Options) ->
  manage_calc_workers(A, B, Base, Parent).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start(A, B, Base) -> Result when
  A::integer(),
  B::integer(),
  Base::integer(),
  Result::[integer()].

-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

%% @doc Accepts 3 ints as input, A and B are the values to add together and Base is the base 
%% in which the numbers should be shown and the result be.
%% Input is only allowed in decimalform
start(A,B, Base) ->
    AList = split (A, Base), 
    BList = split (B, Base),
    %%Split to create fractions for our workers to work with.

    %%We spawn a process that deals with everything for us
    spawn(add, manage_calc_workers, [AList, BList, Base, self()]),
    
    %%Result will come to us, {Result, Carries} in [integer()].
    receive
      {ResultList, CarryList} ->
    
	    Carry = lists:concat(lists:reverse(CarryList)),
	    AL = lists:concat(lists:reverse(AList)),
	    BL = lists:concat(lists:reverse(BList)),
	    Result = lists:concat(lists:reverse(ResultList)),
	    io:fwrite("\n \n  "),
	    io:fwrite(Carry),
	    io:fwrite("\n  "),
	    repeat("-",length(Carry)),
	    io:fwrite("\n  "),
	    repeat(" ",(length(Result)-length(AL))),
	    io:fwrite(AL),
	    io:fwrite("\n  "),
	    repeat(" ",(length(AL)-length(BL))),
	    repeat(" ",length(Result)-length(AL)),
	    io:fwrite(BL),
	    io:fwrite("\n+ "),
	    repeat("-",length(Result)),
	    io:fwrite("\n  "),
	    io:fwrite(Result),
	    io:fwrite("\n \n \n")
    end.



%% @doc Accepts 3 ints as input, A and B are the values to add together and Base is the base 
%% in which the numbers should be shown and the result be. /4 also allows an optional touple 
%% which can contain any of the options listed in the specifications (Update this).
%% Input is only allowed in decimalform
start(A,B,Base, Options) ->
  AList = split (A, Base), 
  BList = split (B, Base),
  %%Split to create fractions for our workers to work with.d
  io:write(Options),
  %%We spawn a process that deals with everything for us
  spawn(add, manage_calc_workers, [AList, BList, Base, self(), Options]),
  %%Result will come to us, {Result, Carries} in [integer()].
  receive
    {ResultList, CarryList} ->
	    Carry = lists:concat(lists:reverse(CarryList)),
	    AL = lists:concat(lists:reverse(AList)),
	    BL = lists:concat(lists:reverse(BList)),
	    Result = lists:concat(lists:reverse(ResultList)),
	    io:fwrite("\n \n  "),
	    io:fwrite(Carry),
	    io:fwrite("\n  "),
	    repeat("-",length(Carry)),
	    io:fwrite("\n  "),
	    repeat(" ",(length(Result)-length(AL))),
	    io:fwrite(AL),
	    io:fwrite("\n  "),
	    repeat(" ",(length(AL)-length(BL))),
	    repeat(" ",length(Result)-length(AL)),
	    io:fwrite(BL),
	    io:fwrite("\n+ "),
	    repeat("-",length(Result)),
	    io:fwrite("\n  "),
	    io:fwrite(Result),
	    io:fwrite("\n \n \n")

  end.

repeat(_,N) when N < 1 ->
    io:fwrite("");

repeat(X,N) ->
    L=lists:flatten(lists:duplicate(N,X)),    
    io:fwrite(L).

%================TESTS==========================%


calc_dec_test_() ->
    A = 10,
    B = 5,
    Base=10,
    AList = split(A,Base),
    BList = split(B,Base),
    
    
    
spawn(add,manage_calc_workers,[AList,BList,Base,self()]),

receive
    {Result} ->
	
	[?_assertEqual(lists:reverse(Result),[0,1,5])]
end.

calc_bin_test_() ->
    A = 10,
    B = 5,
    Base = 2,

    AList = split (A,Base),
    BList = split (B,Base),
    
    
spawn(add,manage_calc_workers,[AList,BList,Base,self()]),
receive
    {Result} ->
	[?_assertEqual(lists:reverse(Result),[0,1,1,1,1])]
end.


calc_hex_test_() ->
    A = 10,
    B = 5,
    Base = 16,
    
    AList = split (A,Base),
    BList = split (B,Base),
    
spawn(add,manage_calc_workers,[AList,BList,Base,self()]),
     
    receive
	{Result} ->
	    [?_assertEqual(lists:reverse(Result),[0,15])]
    end.	      
