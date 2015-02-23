%%%========================================================================
%%% File: cubes.erl
%%%
%%% Find the smallest cube for which exactly `N` permutations of its digits
%%% are cube.
%%%
%%% Author: Enrique Fernandez <efcasado@gmail.com>
%%% Date:   February, 2015
%%%========================================================================
-module(cubes).

%% Include EUnit headers only if the module is compiled with the -DTEST
%% macro (e.g., erlc -DTEST cubes.erl).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% If this module is compiled with the -DTEST macro, all functions get
%% exported. Otherwise, only `smallest/1` gets exported.
-ifdef(TEST).
-compile(export_all).
-else.
-export([smallest/1]).
-endif.

%% Type definitions
-type digit()  :: 0..9.
-type digits() :: nonempty_list(digit()).


%%=========================================================================
%% API
%%=========================================================================

%%-------------------------------------------------------------------------
%% @doc
%% Find the smallest cube for which exactly `N` permutations of its digits
%% are cube.
%% @end
%%-------------------------------------------------------------------------
-spec smallest(pos_integer()) -> pos_integer().
smallest(1) ->
    1;
smallest(N) when N > 0 ->
    '_smallest'(1, #{}, N, 1).

'_smallest'(Num, Acc, N, PrevNDigits) ->
    Cube    = cube(Num),
    Digits  = to_digits(Cube),
    NDigits = length(Digits),
    Acc1    =
        case NDigits > PrevNDigits of
            true  ->
                #{};
            false ->
                Acc
        end,
    {Perms = [P| _], Acc2} = perms(Digits, Acc1),
    case length(Perms) of
        N ->
            from_digits(P);
        _ ->
            '_smallest'(Num + 1, Acc2, N, NDigits)
    end.

%%=========================================================================
%%  Local functions
%%=========================================================================

%%-------------------------------------------------------------------------
%% @doc
%% Given the digits of a cube and a map containing the digits of previously
%% analysed cubes, store the cube in the map and return and updated version
%% of the map along with a (sorted) list of the previously analysed cubes
%% whose digits are a permutation of the given cube.
%% @end
%%-------------------------------------------------------------------------
-spec perms(digits(), map()) -> {list(digits()), map()}.
perms(Digits, Acc) ->
    SortedDigits = lists:sort(Digits),
    Cubes1       = maps:get(SortedDigits, Acc, []),
    Cubes2       = lists:sort([Digits| Cubes1]),
    Acc1         = maps:put(SortedDigits, Cubes2, Acc),
    {Cubes2, Acc1}.

%%-------------------------------------------------------------------------
%% @doc
%% Convert the provided number into a list of digits.
%%
%% Example:
%%   to_digits(1987) -> [1, 9, 8, 7]
%% @end
%%-------------------------------------------------------------------------
-spec to_digits(non_neg_integer()) -> digits().
to_digits(Num) ->
    [ D - 48 || D <- integer_to_list(Num) ].

%%-------------------------------------------------------------------------
%% @doc
%% Convert the provided list of digits into a number.
%%
%% Example:
%%   from_digits([1, 9, 8, 7]) -> 1987
%% @end
%%-------------------------------------------------------------------------
-spec from_digits(digits()) -> non_neg_integer().
from_digits(Digits) when length(Digits) /= 0 ->
    list_to_integer([ D + 48 || D <- Digits ]).

%%-------------------------------------------------------------------------
%% @doc
%% Return the cube (i.e., X^3) of the provided integer.
%% @end
%%-------------------------------------------------------------------------
-spec cube(integer()) -> integer().
cube(N) ->
    N * N * N.


%%=========================================================================
%%  Unit tests
%%=========================================================================

%% Unit tests are also included in the module if the macro `TEST` is
%% defined at compile time (i.e., erlc -DTEST ...).

-ifdef(TEST).

cube_test_() ->
    [
     ?_assertEqual(1,  cube(1)),
     ?_assertEqual(8,  cube(2)),
     ?_assertEqual(27, cube(3))
    ].

from_digits_error_test_() ->
    [
     ?_assertMatch({'EXIT', {function_clause, _StackTrace}}, catch from_digits([]))
    ].

from_digits_test_() ->
    [
     ?_assertEqual(1,  from_digits([1])),
     ?_assertEqual(1,  from_digits([0,1])),
     ?_assertEqual(10, from_digits([1,0]))
    ].

to_digits_test_() ->
    [
     ?_assertEqual([1],   to_digits(1)),
     ?_assertEqual([1,0], to_digits(10))
    ].

smallest_test_() ->
    [
      ?_assertEqual(1,            smallest(1)),
      ?_assertEqual(125,          smallest(2)),
      ?_assertEqual(41063625,     smallest(3)),
      ?_assertEqual(1006012008,   smallest(4)),
      ?_assertEqual(127035954683, smallest(5))
    ].

-endif.
