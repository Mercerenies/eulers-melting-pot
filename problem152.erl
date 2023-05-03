
-module(problem152).
-export([start/0]).

-record(fraction, {numerator, denominator}).
-record(set_in_context, {inner_set, context}).

upper_limit() ->
    80.

gcd(A, 0) ->
    A;
gcd(A, B) ->
    gcd(B, A rem B).

frac_zero() ->
    #fraction{numerator = 0, denominator = 1}.

frac_neg(#fraction{numerator = N, denominator = D}) ->
    #fraction{numerator = -N, denominator = D}.

frac_add(#fraction{numerator = N1, denominator = D1}, #fraction{numerator = N2, denominator = D2}) ->
    N = N1 * D2 + D1 * N2,
    D = D1 * D2,
    Gcd = gcd(N, D),
    #fraction{numerator = N div Gcd, denominator = D div Gcd}.

frac_sub(F1, F2) ->
    frac_add(F1, frac_neg(F2)).

is_prime(2) ->
    true;
is_prime(P) ->
    is_prime_rec(P, 2).

is_prime_rec(P, X) ->
    if
        P rem X == 0 -> false;
        X * X < P -> is_prime_rec(P, X + 1);
        true -> true
    end.

powerlist([]) ->
    [[]];
powerlist([X|Xs]) ->
    Rec = powerlist(Xs),
    lists:append(Rec, lists:map(fun(Ys) -> [X|Ys] end, Rec)).

makeset(Set, Context) ->
    #set_in_context{inner_set = Set, context = Context}.

% Takes two set_in_context values. Returns nil if they're
% incompatible, or the join if compatible.
joinsets(Set1, Set2) ->
    #set_in_context{inner_set = S1, context = C1} = Set1,
    #set_in_context{inner_set = S2, context = C2} = Set2,
    Universe = sets:union(C1, C2),
    CommonUniverse = sets:to_list(sets:intersection(C1, C2)),
    case lists:all(fun(X) -> sets:is_element(X, S1) == sets:is_element(X, S2) end, CommonUniverse) of
        true -> #set_in_context{inner_set = sets:union(S1, S2), context = Universe};
        false -> nil
    end.

inv_square(X) ->
    #fraction{numerator = 1, denominator = X * X}.

sum_inv_squares(Xs) ->
    Fractions = lists:map(fun inv_square/1, Xs),
    lists:foldl(fun frac_add/2, frac_zero(), Fractions).

% Precondition: p is an odd prime.
analyze_prime(P) ->
    Multiples = lists:seq(P, upper_limit(), P),
    Universe = sets:from_list(Multiples),
    lists:filtermap(
      fun(Candidate) ->
              Sum = sum_inv_squares(Candidate),
              #fraction{numerator = _, denominator = D} = Sum,
              if
                  D rem P == 0 -> false;
                  true -> {true, makeset(sets:from_list(Candidate), Universe)}
              end
      end,
      powerlist(Multiples)
     ).

% Takes lists of set_in_context.
full_merge(Xs, Ys) ->
    [joinsets(X, Y) || X <- Xs, Y <- Ys].

% Takes lists of set_in_context.
merge(Xs, Ys) ->
    lists:filter(fun(X) -> X =/= nil end, full_merge(Xs, Ys)).

build_target_sums_dict(AllPossibleTuples) ->
    Lists = lists:map(fun(SetInContext) ->
                              #set_in_context{inner_set = Set, context = _} = SetInContext,
                              sets:to_list(Set)
                      end,
                      AllPossibleTuples),
    lists:foldl(
      fun(X, Acc) ->
              Sum = sum_inv_squares(X),
              Key = frac_sub(#fraction{numerator = 1, denominator = 2}, Sum),
              dict:update_counter(Key, 1, Acc)
      end,
      dict:new(),
      Lists
     ).

% Helper for dictionaries
find_or(Key, Default, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Value} -> Value;
        error -> Default
    end.

start() ->
    PrimesToPrecompute = lists:filter(fun is_prime/1, lists:seq(5, upper_limit())),
    X0 = [makeset(sets:new(), sets:new())],
    AllPossibleTuples = lists:foldl(fun(X, Acc) -> merge(Acc, analyze_prime(X)) end, X0, PrimesToPrecompute),
    BruteForceValues = lists:filter(fun(X) ->
                                            not lists:any(fun(P) -> X rem P == 0 end, PrimesToPrecompute)
                                    end, lists:seq(2, upper_limit())),
    TargetSums = build_target_sums_dict(AllPossibleTuples),
    SolutionCount = lists:foldl(
                      fun
                          ([], Acc) ->
                              % Empty sum is not a valid sum :)
                              Acc;
                          (Xs, Acc) ->
                              Sum = sum_inv_squares(Xs),
                              Acc + find_or(Sum, 0, TargetSums)
                      end,
                      0,
                      powerlist(BruteForceValues)
                     ),
    io:format("~B~n", [SolutionCount]).
