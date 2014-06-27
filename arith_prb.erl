-module(arith_prb).
-import(list_prb,[range/2,encode/1]).
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

%% 2.02 Prime factors of a number
prime_fact(X) when X > 1 ->
    prime_fact(X,2,[]).
prime_fact(1,_,Acc) ->
    lists:reverse(Acc);
prime_fact(X,N,Acc) when X rem N == 0 ->
    prime_fact(X div N,N,[N|Acc]);
prime_fact(X,N,Acc) ->
    prime_fact(X,N+1,Acc).
%% I was trying to use the is_prime function somewhere but then I decided that
%% things are faster if I don't bother :P

%% 2.03 Encoded prime factors
enc_prime_fact(X) ->
    encode(prime_fact(X)).

%% 2.04 List of prime numbers in range
list_prime(Low,High) when Low < 3 ->
    list_prime(3,High,[2]);
list_prime(Low,High) ->
    list_prime(Low,High,[]).
list_prime(Curr,High,Acc) when Curr > High ->
    lists:reverse(Acc);
list_prime(Curr,High,Acc) when Curr rem 2 == 0 ->
    list_prime(Curr+1,High,Acc);
list_prime(Curr,High,Acc) ->
    case is_prime(Curr) of
        true ->
            list_prime(Curr+2,High,[Curr|Acc]);
        false ->
            list_prime(Curr+2,High,Acc)
    end.

%% 2.05 Goldbach's conjecture
%% this isn't elegant, but is a side effect of the fact that I start checking
%% with 3.
goldbach(4) ->
    [2,2];
goldbach(X) when X > 2, X rem 2 == 0 ->
    goldbach(X,3).
goldbach(X,Y) ->
    case {is_prime(X-Y),is_prime(Y)} of
        {true,true} ->
            [Y,X-Y];
        _Else ->
            goldbach(X,Y+2)
    end.

