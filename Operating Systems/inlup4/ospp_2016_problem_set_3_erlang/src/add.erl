%% @doc Erlang mini project.
-module(add).
-export([start/1, start/4]).

%% @doc TODO: add documentation
-spec start(A) -> [integer] when 
      A::integer().

start(A) ->
    FixedA = lists:reverse(splitNumbers(A)).

start_calc(A, B, Base) ->
    B,
    Base,
    splitNumbers(A).

-spec calc(A,B,Base,Carry) -> {Result, Carry} when
      A::integer(),
      B::integer(),
      Base::integer(),
      Carry::integer(),
      Result::integer().

calc(A,B,Base,Carry) ->
    Result = A + B + Carry,
    if 
	(Result > Base) ->
	    WithCarry = Result - Carry,
	    {WithCarry, 1};
	true ->
	    {Result, 0}
    end.


-spec addZeros(List, Number) -> List when
      List::[integer],
      Number::integer().

addZeroes(List, 0) ->
    List;

addZeros(List, Number) ->
    addZeroes([0] ++ List, Number - 1).


-spec splitNumbers(Number) -> List when
      Number::integer(),
      List::[integer].

splitNumbers(0) -> [];

splitNumbers(Number) ->
    [(Number rem 10) | splitNumbers(Number div 10)].

%% @doc TODO: add documentation
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base, Options) ->
    A,
    B,
    Base,
    Options,
    tbi.
