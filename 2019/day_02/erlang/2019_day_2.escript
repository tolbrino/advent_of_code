#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname adventofcode

-mode(compile).

%% generic

main(["--test"]) ->
  test();
main(_) ->
  Input1 = prepare_1(input()),
  Result1 = result_1(solve_1(Input1)),
  io:format(user, "Result Part 1: ~p~n", [Result1]),
  Result2 = result_2(solve_2(input())),
  io:format(user, "Result Part 2: ~p~n", [Result2]),
  ok.

%% specific

result_1([A | _]) ->
  A.

result_2({Noun, Verb}) ->
  (100 * Noun) + Verb.

prepare_1(List) ->
  prepare(List, 12, 2).

prepare([A, _, _ | Rest], N, V) ->
  [A, N, V | Rest].

solve_1(Input) ->
  solve_1(1, Input, Input).

solve_1(_, [99 | _], Program) ->
  Program;

solve_1(Pos, [Op, A, B, D | _], Program) when Op == 1; Op == 2 ->
  V1 = lists:nth(A + 1, Program),
  V2 = lists:nth(B + 1, Program),
  Value = case Op of
            1 ->
              V1 + V2;
            2 ->
              V1 * V2
  end,
  Program1 = replace_at_pos(D, Value, Program),
  Pos1 = Pos + 4,
  {_, Next} = lists:split(Pos1 - 1, Program1),
  solve_1(Pos1, Next, Program1).

replace_at_pos(Pos, Value, List) ->
  {Left, [_ | Right]} = lists:split(Pos, List),
  Left ++ [Value | Right].

solve_2(Input) ->
  Pairs = lists:flatten(
            lists:map(
              fun(E) ->
                  lists:zip([E || _ <- lists:seq(0, 99)], lists:seq(0, 99))
              end, lists:seq(0, 99))),
  [Pair | _] = lists:dropwhile(
                 fun({N, V}) ->
                     19690720 =/= result_1(solve_1(prepare(Input, N, V)))
                 end, Pairs),
  Pair.

test() ->
  [2,0,0,0,99] = solve_1([1,0,0,0,99]),
  [2,3,0,6,99]= solve_1([2,3,0,3,99]),
  [2,4,4,5,99,9801] = solve_1([2,4,4,5,99,0]),
  [30,1,1,4,2,5,6,0,99] = solve_1([1,1,1,4,99,5,6,0,99]),
  ok.

%% problem inputs

input() ->
  [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,1,23,6,27,1,6,27,31,1,13,31,35,1,13,35,39,1,39,13,43,2,43,9,47,2,6,47,51,1,51,9,55,1,55,9,59,1,59,6,63,1,9,63,67,2,67,10,71,2,71,13,75,1,10,75,79,2,10,79,83,1,83,6,87,2,87,10,91,1,91,6,95,1,95,13,99,1,99,13,103,2,103,9,107,2,107,10,111,1,5,111,115,2,115,9,119,1,5,119,123,1,123,9,127,1,127,2,131,1,5,131,0,99,2,0,14,0].
