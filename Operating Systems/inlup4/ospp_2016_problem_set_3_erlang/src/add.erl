%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4]).
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

-spec calc_worker (A, B, CarryIn, Base) -> {Result, CarryOut} when
  A::integer(),
  B::integer(),
  Base::integer(),
  CarryIn::integer(),
  Result::integer(),
  CarryOut::integer().

calc_worker (A, B, CarryIn, Base) ->
  Result = A + B + CarryIn,
  CarryOut = Result div Base,
  {Result, CarryOut}.


-spec actor_manager (A, B, Base, SpeculativeWorking) -> ok when
  A::[integer],
  B::[integer],
  Base::integer(),
  SpeculativeWorking::integer().
  %%SpeculativeWorking is either 0 (saying no speculations) or 1.

actor_manager (A, B, Base, SpeculativeWorking) ->
  if 
    SpeculativeWorking == 1 ->
      %%Speculative working is enabled
      tbi.
    true ->
      




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(A,B, Base) ->
    %% Split numbers to create pairs at most.
    %% Create extra 0s in front of the shorter number if necessary.
    %% create a list for all the 
    NumberLists = { lists:reverse(split (A, Base)), 
                    lists:reverse(split (B, Base)) 
                    }.

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
