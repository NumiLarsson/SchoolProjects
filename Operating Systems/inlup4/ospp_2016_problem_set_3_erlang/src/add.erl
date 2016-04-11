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
  List::[integer].

split (0, _) ->
  [];

split (A, Base) ->
  Rest = A div Base,
  Result = A rem Base,
  [Result | split (Rest, Base)].



-spec insert_at(List, Object, Index) -> List when
  List::[],
  Object::integer(),
  Index::integer().

insert_at([], Object, _Index) ->
  [Object];

insert_at([H|T], Object, 0) ->
  [H|Object] ++ T;

insert_at([H|T], Object, Index) ->
  [H] ++ insert_at(T, Object, Index - 1).



-spec combine_results(ResultList, CarryList, Owner) -> 
    {ResultList, CarryList} when
  ResultList::[interger],
  CarryList::[interger],
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


-spec send_carry(Combinator, WorkerPIDList, Carry) -> ok when
  Combinator::integer(),
  WorkerPIDList::[integer],
  Carry::integer.

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

-spec spawn_calc_workers(A, B, Base) -> WorkerPIDList when
  A::[integer],
  B::[integer],
  Base::integer(),
  WorkerPIDList::[integer].

spawn_calc_workers(A, B, Base) -> 
  spawn_calc_workers(A, B, Base, 0).

-spec spawn_calc_workers(A, B, Base, Index) -> WorkerPIDList when
  A::[integer],
  B::[integer],
  Base::integer(),
  Index::integer(),
  WorkerPIDList::[integer].

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


-spec manage_calc_workers(A, B, Base, Parent) -> {Result, CarryList} when
  A::[integer],
  B::[integer],
  Base::integer(),
  Parent::integer(),
  Result::[integer],
  CarryList::[integer].

manage_calc_workers(A, B, Base, Parent) ->
  WorkerPIDList = spawn_calc_workers(A, B, Base), %%Reverse this list?
  CombinePID = spawn(add, combine_results, [[], [], self()]),
  send_carry(CombinePID, WorkerPIDList, 0),


receive
    {ResultList, CarryList} ->
      Parent ! {ResultList, CarryList}
  end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start(A, B, Base) -> Result when
  A::integer(),
  B::integer(),
  Base::integer(),
  Result::[integer].

%% @doc TODO: add documentation
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
        lists:reverse(ResultList)
        %% Remember to change print 15 / 14 / 13 / 12 / 11 / 10 to
        %%                          f  /  e /  d /  c /  b /  a
        %%Print (Carries),
        %% Print (AList),
        %% Print (BList),
        %% Print _the_line_,
        %% Print (Result)
      after 20000 ->
        io:fwrite("Something went wrong")
    end.



%% @doc TODO: add documentation
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
