#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname adventofcode

-mode(compile).

%% generic

main(["--test"]) ->
  test();
main(_) ->
  {Input1, Input2} = input(),
  Result1 = solve_1(Input1, Input2),
  io:format(user, "Result Part 1: ~p~n", [Result1]),
  Result2 = solve_2(Input1, Input2),
  io:format(user, "Result Part 2: ~p~n", [Result2]),
  ok.

%% specific

solve_1(Current, Upper) ->
  solve_int(0, Current, Upper, fun(D) -> check_1(D) end).

solve_2(Current, Upper) ->
  solve_int(0, Current, Upper, fun(D) -> check_2(D) end).

solve_int(N, Current, Upper, Fun) ->
  N1 = case Fun(Current) of
    true ->
      N+1;
    false ->
      N
  end,
  case Current == Upper of
    true ->
      N1;
    false ->
      solve_int(N1, next(Current, Upper), Upper, Fun)
  end.

next(Digits, Digits) -> Digits;
next(Digits, Limit) ->
  {Next1, _, Valid} = lists:foldr(
                   fun
                     (N, {[], true, Valid}) ->
                       {[N], true, Valid};
                     (N, {[N0|_]=Next, true, Valid}) ->
                       {[N|Next], true, Valid andalso N =< N0};
                     (9, {[], _Done, Valid}) ->
                       {[0], false, Valid};
                     (9, {[N0|_]=Next, _Done, Valid}) ->
                       {[0|Next], false, Valid andalso 0 =< N0};
                     (N, {[], _Done, Valid}) ->
                       {[N+1], true, Valid};
                     (N, {[9=N0|_]=Next, _Done, Valid}) ->
                       {[N+1|Next], true, Valid andalso (N+1) =< N0};
                     (N, {[N0|_]=Next, _Done, Valid}) ->
                       {[N+1|Next], true, Valid andalso (N+1) =< N0}
                   end, {[], false, true}, Digits),
  case Valid of
    false ->
      next(Next1, Limit);
    true ->
      Next1
  end.

check_1(Digits) ->
  {_, Check1} = lists:foldl(
                          fun(N, {N0, CheckAdj}) ->
                              CheckAdj1 = CheckAdj orelse N == N0,
                              {N, CheckAdj1}
                          end, {-1, false}, Digits),
  Check1.

check_2(Digits) ->
  {_, _, _, Check1, _} = lists:foldl(
                          fun
                            (N, {N2, N1, N0, CheckAdj, 1}) ->
                              % checks end
                              CheckAdj1 = CheckAdj orelse (N == N2 andalso N2 =/= N1)
                                                   orelse (N =/= N2 andalso N2 == N1 andalso N1 =/= N0),
                              {N, N2, N1, CheckAdj1, 0};
                            (N, {N2, N1, -1, CheckAdj, Rem}) ->
                              % checks start
                              CheckAdj1 = CheckAdj orelse (N =/= N2 andalso N2 == N1),
                              {N, N2, N1, CheckAdj1, Rem-1};
                            (N, {N2, N1, N0, CheckAdj, Rem}) ->
                              CheckAdj1 = CheckAdj orelse (N =/= N2 andalso N2 == N1 andalso N1 =/= N0),
                              {N, N2, N1, CheckAdj1, Rem-1}
                          end, {-1, -2, -2, false, length(Digits)}, Digits),
  Check1.

test() ->
  true = check_1([1,1,1,1,1,1]),
  true = check_1([2,2,3,4,5,0]),
  false = check_1([1,2,3,7,8,9]),
  true = check_2([1,1,2,2,3,3]),
  true = check_2([2,4,5,6,6,7]),
  false = check_2([1,2,3,4,4,4]),
  true = check_1([1,1,1,1,2,2]),
  ok.

%% problem inputs

input() ->
  {[2,4,0,9,2,0], [7,8,9,8,5,7]}.
