#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname adventofcode

-mode(compile).

%% generic

main(["--test"]) ->
  test();
main(_) ->
  Result = solve(input()),
  io:format(user, "Result: ~p~n", [Result]),
  ok.

%% specific

solve([]) ->
  0;
solve([Mass | Rest]) ->
  fuel(Mass) + solve(Rest).

fuel(Mass) ->
  (Mass div 3) - 2.

test() ->
  {12, 2} = {12, fuel(12)},
  {14, 2} = {14, fuel(14)},
  {1969, 654} = {1969, fuel(1969)},
  {100756, 33583} = {100756, fuel(100756)},
  ok.

%% problem inputs

input() ->
  [ 141107
  , 119016
  , 145241
  , 72264
  , 116665
  , 81420
  , 88513
  , 128809
  , 145471
  , 81570
  , 124798
  , 75370
  , 84988
  , 71634
  , 135275
  , 96992
  , 53376
  , 62414
  , 148277
  , 135418
  , 82475
  , 137707
  , 105051
  , 83450
  , 102673
  , 88390
  , 100849
  , 94528
  , 135709
  , 63945
  , 126413
  , 70107
  , 84734
  , 119176
  , 85769
  , 115276
  , 137511
  , 61806
  , 92892
  , 121640
  , 93726
  , 146526
  , 95812
  , 132556
  , 103885
  , 78776
  , 55826
  , 120257
  , 61131
  , 79179
  , 130698
  , 97153
  , 121985
  , 61159
  , 103585
  , 148674
  , 84067
  , 110085
  , 138473
  , 105495
  , 112393
  , 144411
  , 73328
  , 125955
  , 58075
  , 136147
  , 124106
  , 81185
  , 138847
  , 69814
  , 127104
  , 86090
  , 67666
  , 102333
  , 99546
  , 98280
  , 99062
  , 129433
  , 125353
  , 77609
  , 71240
  , 71791
  , 146046
  , 113685
  , 121381
  , 122715
  , 147789
  , 53981
  , 140926
  , 81528
  , 121789
  , 106627
  , 73745
  , 67509
  , 144140
  , 119238
  , 82417
  , 129215
  , 75663
  , 106842
  ].


