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
-export([smallest/1, clear_cache/0]).
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
smallest(N) when N > 0 ->
    init_cache(),
    Digits  = [1| lists:duplicate(N * (N - 1), 0)],
    Num     = from_digits(Digits),
    '_smallest'(round(cbrt(Num)), N).

'_smallest'(Num0, N) ->
    Num = cube(Num0),
    Digits = to_digits(Num),
    Perms  = perms(Digits),
    cache(Digits, Perms),
    Perms1 = [ from_digits(P) || P <- Perms ],
    case cubes(Perms1) of
        N -> Num;
        _ -> '_smallest'(Num0 + 1, N)
    end.


%%=========================================================================
%% Local functions
%%=========================================================================

%%-------------------------------------------------------------------------
%% @doc
%% Given a list of integers, count the number of cubes.
%% @end
%%-------------------------------------------------------------------------
-spec cubes(list(integer())) -> non_neg_integer().
cubes(List) ->
    lists:foldl(fun(Number, Count) ->
                        case is_cube(Number) of
                            true  ->
                                Count + 1;
                            false ->
                                Count
                        end
                end,
                0,
                List).

%%-------------------------------------------------------------------------
%% @doc
%% Compute all the permutations for the provided list of digits.
%%
%% Example:
%%     perms([1,2,3]) -> [ [1, 2, 3], [2, 3, 1], [3, 1, 2] ]
%% @end
%%-------------------------------------------------------------------------
-spec perms(digits()) -> list(digits()).
perms(Digits) ->
    [ P || P <- lists:usort('_perms'(Digits)), 0 =/= hd(P) ].

'_perms'([]) ->
    [ [] ];
'_perms'(Ds) ->
    case cached(Ds) of
        false ->
            Perms = [ [H| T] || H <- Ds, T <- '_perms'(Ds -- [H]) ],
            cache(Ds, Perms),
            Perms;
        {true, Perms} ->
            Perms
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Convert the provided non-negative integer into a list of digits.
%% @end
%%-------------------------------------------------------------------------
-spec to_digits(non_neg_integer()) -> list(digit()).
to_digits(Integer) ->
    [ D - 48 || D <- integer_to_list(Integer) ].

%%-------------------------------------------------------------------------
%% @doc
%% Convert the provided list of digits into an integer.
%% @end
%%-------------------------------------------------------------------------
-spec from_digits(nonempty_list(digit())) -> integer().
from_digits(List) when length(List) =/= 0 ->
    %% Conver its digit to its ASCII representation
    List1 = [ D + 48 || D <- List ],
    list_to_integer(List1).

%%-------------------------------------------------------------------------
%% @doc
%% Return true if the provided positive integer is cube.
%%
%% A positive integer is considered cube if its cube root is an integer.
%% @end
%%-------------------------------------------------------------------------
-spec is_cube(pos_integer()) -> boolean().
is_cube(N) ->
    CubicRoot = cbrt(N),
    cube(ceil(CubicRoot)) == N.

%%-------------------------------------------------------------------------
%% @doc
%% Return the cube (i.e., X^3) of the provided integer.
%% @end
%%-------------------------------------------------------------------------
-spec cube(integer()) -> integer().
cube(N) ->
    N * N * N.

%%-------------------------------------------------------------------------
%% @doc
%% Compute the cube root of the provided positive integer.
%% @end
%%-------------------------------------------------------------------------
-spec cbrt(non_neg_integer()) -> float().
cbrt(N) when N >= 0 ->
    %% The cube root of a number X can also be expressed as X^(1/3).
    math:pow(N, 1/3).

%%-------------------------------------------------------------------------
%% @doc
%% Return the smallest integer greater than or equal to the provided
%% floating point number.
%% @end
%%-------------------------------------------------------------------------
-spec ceil(float()) -> integer().
ceil(Float) when is_float(Float) andalso Float < 0 ->
    trunc(Float);
ceil(Float) when is_float(Float) ->
    T = trunc(Float),
    case Float - T of
        0.0 -> T;
        _   -> T + 1
    end.


%%=========================================================================
%% Memoization
%%=========================================================================

-spec init_cache() -> 'ok'.
init_cache() ->
    case catch ets:new(cache, [named_table]) of
        cache ->
            ok;
        {'EXIT', {badarg, _StackTrace}} ->
            ok
    end.

-spec clear_cache() -> 'ok'.
clear_cache() ->
    case catch ets:delete(cache) of
        true  ->
            ok;
        false ->
            ok;
        {'EXIT', {badarg, _StackTrace}} ->
            ok
    end.

-spec cache(digits(), list(digits())) -> 'ok'.
cache(Digits, Perms) ->
    ets:insert(cache, {Digits, Perms}),
    ok.

-spec cached(digits()) -> {'true', list(digits())} | 'false'.
cached(Digits) ->
    case ets:lookup(cache, Digits) of
        [] ->
            false;
        [{Digits, Perms}] ->
            {true, Perms}
    end.


%%=========================================================================
%% Unit tests
%%=========================================================================

%% If this modules was compiled with the `-DTEST` macro, all unit tests can
%% be executed running `eunit:test(cubes, [verbose])`.

-ifdef(TEST).

to_digits_test() ->
    [0]        = to_digits(0),
    [1, 2, 3]  = to_digits(123).

from_digits_error_test() ->
    {'EXIT', {function_clause, _StackTrace}} = (catch from_digits([])).

from_digits_test() ->
    0   = from_digits([0]),
    123 = from_digits([1, 2, 3]).

ceil_error_test() ->
    {'EXIT', {function_clause, _StackTrace}} = (catch ceil(1)).

ceil_test() ->
    1  = ceil( 1.0),
    1  = ceil( 1.00),
    %% We do not have enough precision so this floating point number gets
    %% converted to 1.0 at compile time.
    1  = ceil( 1.0000000000000000000000000000000000000000000000000001),
    %% We do not have enough precision so this floating point number gets
    %% converted to 1.1 at compile time.
    2 = ceil( 1.1000000000000000000000000000000000000000000000000001),
    2 = ceil( 1.00001),
    2 = ceil( 1.1),
    2 = ceil( 1.9),
   -1 = ceil(-1.0),
   -1 = ceil(-1.00),
    %% We do not have enough precision so this floating point number gets
    %% converted to -1.0 at compile time.
   -1  = ceil(-1.0000000000000000000000000000000000000000000000000001),
    %% We do not have enough precision so this floating point number gets
    %% converted to -1.1 at compile time.
   -1 = ceil(-1.1000000000000000000000000000000000000000000000000001),
   -1 = ceil(-1.00001),
   -1 = ceil(-1.1),
   -1 = ceil(-1.9).

is_cube_test() ->
    true  = is_cube(0),
    true  = is_cube(1),
    false = is_cube(2),
    false = is_cube(3),
    true  = is_cube(8),
    true  = is_cube(27),
    true  = is_cube(64),
    false = is_cube(175615),
    true  = is_cube(175616),
    false = is_cube(175617).


perms_test() ->
    init_cache(),
    [ [1,2], [2,1] ] = perms([1,2]).

smallest_error_test() ->
    {'EXIT', {function_clause, _StackTrace}} = (catch smallest(0)).

smallest_test_() ->
    [
     ?_assertEqual(1,   smallest(1)),
     ?_assertEqual(125, smallest(2)),
     {timeout, 15, ?_assertEqual(41063625, smallest(3))}
    ].

-endif.
