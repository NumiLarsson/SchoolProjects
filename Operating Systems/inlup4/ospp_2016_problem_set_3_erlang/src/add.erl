%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4, actor_manager/5, 
        create_actors/5, calc_worker/5,
        make_lists_listener/4]).
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

-spec calc_worker (A, B, CarryIn, Base, Index) -> {Index, {Result, CarryOut}} when
  A::integer(),
  B::integer(),
  Base::integer(),
  CarryIn::integer(),
  Index::integer(),
  Result::integer(),
  CarryOut::integer().

calc_worker (A, B, CarryIn, Base, Index) ->
  PreResult = A + B + CarryIn,
  CarryOut = PreResult div Base,
  if 
    CarryOut == 1 ->
      Result = PreResult - Base;
    true ->
      Result = PreResult
  end,
  receive 
    From ->
      From ! {Index, {Result, CarryOut}}
  end.


-spec create_actors (A, B, Carry, Base, Index) -> PIDList when
  A::[integer],
  B::[integer],
  Carry::integer(),
  Base::integer(),
  Index::integer(),
  PIDList::[integer].

create_actors ([], [], _Carry, _Base, _Index) ->
  [];

create_actors ([ HeadA | TailA ], [], Carry, Base, Index) -> 
  [spawn(
        add, 
        calc_worker, 
        [HeadA, 0, Carry, Base, Index]
        ) |
  create_actors(TailA, [], Carry, Base, (Index+1))];


create_actors ([], [HeadB|TailB], Carry, Base, Index) -> 
  [spawn(
        add, 
        calc_worker, 
        [0, HeadB, Carry, Base, Index]
        ) |
  create_actors([], TailB, Carry, Base, (Index+1)) ];
 
create_actors ([HeadA|TailA], [HeadB|TailB], Carry, Base, Index) ->
  [spawn(
        add, 
        calc_worker, 
        [HeadA, HeadB, Carry, Base, Index]
        ) |
  create_actors(TailA, TailB, Carry, Base, (Index+1)) ].

-spec fan_out_PID (PID, PIDList) -> ok when
  PID::integer(),
  PIDList::(integer).

fan_out_PID (_, []) ->
  ok;

fan_out_PID (PID, [Head | Tail]) ->
  Head ! PID,
  fan_out_PID(PID, Tail).

-spec actor_manager (A, B, Base, Parent, SpeculativeWorking) -> ok when
  A::[integer],
  B::[integer],
  Base::integer(),
  Parent::integer(),
  SpeculativeWorking::integer().
  %%SpeculativeWorking is either 0 (saying no speculations) or 1.

actor_manager (A, B, Base, Parent, SpeculativeWorking) ->
  if 
    SpeculativeWorking == 1 ->
      %%Speculative working is enabled
      tbi;
    true ->
      PIDList = create_actors(A, B, 0, Base, 0),
      List_Manager_PID = spawn(
        add,
        make_lists_listener,
        [PIDList, [], [], Parent]),
      fan_out_PID(List_Manager_PID, PIDList)
  end.

-spec put_list_index(Object, List, Index) -> List when
  Object::integer(),
  List::[integer],
  Index::integer().

put_list_index(Object, List, 0) ->
  List ++ [Object];

put_list_index(Object, [_|Tail], 0) -> 
  [Object|Tail];

put_list_index(Object, [ Head | Tail], Index) ->
  [Head | put_list_index(Object, Tail, (Index - 1) )].

-spec make_lists_listener(PIDList, WorkList, CarryList, Parent) -> WorkList when
  PIDList::[integer],
  WorkList::[integer],
  CarryList::[integer],
  Parent::integer().

make_lists_listener([], WorkList, CarryList, Parent) -> 
  Parent ! {WorkList, CarryList};

make_lists_listener([_|Tail], ResultList, CarryList, Parent) -> 
  receive
    {Index, {Result, CarryOut}} ->
      make_lists_listener(Tail, 
                          put_list_index(Result, ResultList, Index),
                          put_list_index(CarryOut, CarryList, Index),
                          Parent)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start(A, B, Base) -> Result when
  A::integer(),
  B::integer(),
  Base::integer(),
  Result::integer().

%% @doc TODO: add documentation
start(A,B, Base) ->
    %% Split numbers to create pairs at most.
    %% Create extra 0s in front of the shorter number if necessary.
    %% create a list for all the 
    AList = split (A, Base), 
    BList = split (B, Base),
    SpeculativeWorking = 0, %% 1 is on.
    spawn(
      add, 
      actor_manager, 
      [AList, BList, Base, self(), SpeculativeWorking]
      ),
    receive
      {X, Y} ->
        [lists:reverse(X),AList, BList, lists:reverse(Y)]
        %% Remember to change print 15 / 14 / 13 / 12 / 11 / 10 to
        %%                          f  /  e /  d /  c /  b /  a
        %%Print (Carries),
        %% Print (AList),
        %% Print (BList),
        %% Print _the_line_,
        %% Print (Result)
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
