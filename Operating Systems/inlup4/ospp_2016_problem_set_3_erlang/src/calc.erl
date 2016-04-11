-module(calc).
-export([calc_worker/4, calc_worker_spec/4]).


%% @doc calc_carry_out simply takes a number and calculates the carry out it'll have.

-spec calc_carry_out(Number, Base) -> {Result, Carry} when
  Number::integer(),
  Base::integer(),
  Result::integer(),
  Carry::integer().

calc_carry_out(Number, Base) ->
  Result = Number rem Base,
  Carry = Number div Base,
  {Result, Carry}.

%% @doc calc_worker_spec is an actor that calculates a simple addition, receiving the carryin 
%% through a message from the process that want's the return.	_spec signifies that this worker
%% works by speculative work, where it calculates both possible combinations before it is asked
%% to return an answer.

-spec calc_worker_spec (A, B, Base, Index) -> {Index, {Result, CarryOut}} when
  A::integer(),
  B::integer(),
  Base::integer(),
  Index::integer(),
  Result::integer(),
  CarryOut::integer().

calc_worker_spec (A, B, Base, Index) ->
  ResCarryIn1 = A + B + 1,
  ResCarryIn0 = A + B, %% + 0,

  {Result1, CarryOut1} = calc_carry_out(ResCarryIn1, Base),
  {Result0, CarryOut0} = calc_carry_out(ResCarryIn0, Base),

  receive 
    {From, 1} ->
      From ! {Index, {Result1, CarryOut1}},
      exit(success);
    {From, 0} ->
      From ! {Index, {Result0, CarryOut0}},
      exit(success)
  end.

%% @doc calc_worker adds two integers and make sure that they don't have overflow, passes eventual
%% carries back to whoever pokes it.
-spec calc_worker(A, B, Base, Index) -> {Index, {Result, CarryOut}} when
	A::integer(),
	B::integer(),
	Base::integer(),
	Index::integer(),
	Result::integer(),
	CarryOut::integer().

calc_worker(A, B, Base, Index) ->
	ResNoCarry = A + B,

	receive
		{From, 1} ->
			Result = ResNoCarry + 1,
			CarryOut = calc_carry_out(Result, Base),
			From ! {Index, {Result, CarryOut}},
			exit(success);
		{From, 0} ->
			CarryOut = calc_carry_out(ResNoCarry, Base),
			From ! {Index, {ResNoCarry, CarryOut}},
			exit(success)
	end.