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

%% @doc calc_worker is an actor that calculates a simple addition, receiving the carryin 
%% through a message from the process that want's the return.

-spec calc_worker (A, B, Base, Index) -> {Index, {Result, CarryOut}} when
  A::integer(),
  B::integer(),
  Base::integer(),
  Index::integer(),
  Result::integer(),
  CarryOut::integer().

calc_worker (A, B, Base, Index) ->
  ResCarryIn1 = A + B + 1,
  ResCarryIn0 = A + B, %% + 0,

  {Result1, CarryOut1} = calc_carry_out(ResCarryIn1, Base),
  {Result0, CarryOut0} = calc_carry_out(ResCarryIn0, Base),

  receive 
    {From, 1} ->
      From ! {Index, {Result1, CarryOut1}};
    {From, 0} ->
      From ! {Index, {Result0, CarryOut0}}
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

    receive
      {Results, Carries} ->
        [
          lists:reverse(Results),
          lists:reverse(AList), 
          lists:reverse(BList), 
          lists:reverse(Carries)
        ]
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
