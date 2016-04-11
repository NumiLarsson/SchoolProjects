%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4, manage_calc_workers/4, combine_results/3]).
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
-spec combine_results(ResultList, CarryList, Owner) -> 
    {ResultList, CarryList} when
  ResultList::[integer()],
  CarryList::[integer()],
  Owner::integer().

combine_results(ResultList, CarryList, Owner) ->
  receive
    {Index, {Result, Carry}} ->
      NewResult = insert_at(ResultList, Result, Index), %%[Result | ResultList], 
      NewCarryList = insert_at(CarryList, Carry, Index), %%[Carry | CarryList], %%
      combine_results(NewResult, NewCarryList, Owner)
    after 2000 ->
      Owner ! {ResultList, CarryList}
  end.

%% @doc Used to "Fan Out" to all the workers, sending them the correct carries and receiving 
%% the correct input, then sends that output to combine_results
-spec send_carry(Combinator, WorkerPIDList, Carry) -> ok when
  Combinator::integer(),
  WorkerPIDList::[integer()],
  Carry::integer().

send_carry(_Combinator, [], _Carry) -> 
  ok;

send_carry(Combinator, [Worker|TailWorkers], Carry) ->
  Worker ! {self(), Carry},
  receive
    {Index, {Result, NewCarry}} ->
      Combinator ! {Index, {Result, NewCarry}},
      send_carry(Combinator, TailWorkers, NewCarry)
    after 2000 ->
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
  CombinePID = spawn(add, combine_results, [[], [], Parent]),
  send_carry(CombinePID, WorkerPIDList, 0).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start(A, B, Base) -> Result when
  A::integer(),
  B::integer(),
  Base::integer(),
  Result::[integer()].

%% @doc Accepts 3 ints as input, A and B are the values to add together and Base is the base 
%% in which the numbers should be shown and the result be.
start(A,B, Base) ->
    %% Split numbers to create pairs at most.
    %% Create extra 0s in front of the shorter number if necessary.
    %% create a list for all the 
    AList = split (A, Base), 
    BList = split (B, Base),

    spawn(add, manage_calc_workers, [AList, BList, Base, self()]),
    
    receive
      {ResultList, CarryList} ->
        io:fwrite("\n\nResult:"),
        io:write(lists:reverse(ResultList)),
        io:fwrite("\n\nCarries:"),
        io:write(lists:reverse(CarryList)),
        io:fwrite("\n\nA:"),
        io:write(AList),
        io:fwrite("\n\nB:"),
        io:write(BList),
        lists:reverse(ResultList);
        %% Remember to change print 15 / 14 / 13 / 12 / 11 / 10 to
        %%                          f  /  e /  d /  c /  b /  a
        %%Print (Carries),
        %% Print (AList),
        %% Print (BList),
        %% Print _the_line_,
        %% Print (Result)
      A -> 
        io:fwrite("\nMessage is fucked up")
      after 20000 ->
        io:fwrite("Something went wrong")
    end.



%% @doc Accepts 3 ints as input, A and B are the values to add together and Base is the base 
%% in which the numbers should be shown and the result be. /4 also allows an optional touple 
%% which can contain any of the options listed in the specifications (Update this).
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base, Options) ->
    tbi,
    A,
    B,
    Base,
    Options.
