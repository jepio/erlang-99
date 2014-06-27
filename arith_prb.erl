-module(arith_prb).
-import(list_prb,[range/2]).
-compile(export_all).

%% Solutions to the Arithmetic problems
%%


%% 2.01 Check if number is prime
is_prime(2) ->
    true;
is_prime(X) when X < 2; X rem 2 == 0 ->
    false;
is_prime(X) ->
    %% Using my own range function, could be done cleaner.
    is_prime(X,range(3,math:sqrt(X))).
is_prime(X,[H|_]) when X rem H == 0 ->
    false;
is_prime(X,[_|T]) ->
    is_prime(X,T);
is_prime(_X,[]) ->
    true.
