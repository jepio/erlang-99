-module(arith_prb).
-import(list_prb,[range/2,encode/1]).
-export([is_prime/1,prime_fact/1,enc_prime_fact/1,list_prime/2,goldbach/1,
         goldbach_list/2,goldbach_list/3,gcd/2,coprime/2,phi/1,phi2/1,
         test_phi/1,test_avg/1]).
%%-compile(export_all).

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
    EX=encode(prime_fact(X)),
    reverse_elements(EX).
reverse_elements(List) ->
    reverse_elements(List,[]).
reverse_elements([],Acc) ->
    lists:reverse(Acc);
reverse_elements([H|T],Acc) ->
    reverse_elements(T,[lists:reverse(H)|Acc]).

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

%% 2.06 A list of Goldbach compositions
goldbach_list(Start,Stop) -> goldbach_list(Start,Stop,0).
goldbach_list(Start,Stop,Limit) ->
    case Start rem 2 of
        1 -> goldbach_list(Start+1,Stop,[],Limit);
        0 -> goldbach_list(Start,Stop,[],Limit)
    end.
goldbach_list(Curr,Stop,_Acc,_) when Curr > Stop ->
    %lists:reverse(Acc);
    ok;
goldbach_list(Curr,Stop,Acc,Limit) ->
    case goldbach(Curr) of
        [Lo, Hi] when Lo > Limit ->
            NewAcc = [[Curr,[Lo,Hi]]|Acc],
            io:format("~B = ~B + ~B ~n",[Curr|[Lo,Hi]]);
        _Else ->
            NewAcc = Acc
    end,
    goldbach_list(Curr+2,Stop,NewAcc,Limit).

%% 2.07 Greatest common divisor
gcd(N,M) when M < N ->
    gcd(M,N);
gcd(0,M) -> M;
gcd(N,M) -> gcd(M rem N,N).

%% 2.08 Coprime
coprime(X,Y) -> coprime(X,Y,gcd(X,Y)).
coprime(_X,_Y,1) -> yes;
coprime(_X,_Y,_) -> no.

%% 2.09 Euler's totient function
phi(1) -> 1;
phi(X) ->
    Nums = range(1,X-1),
    phi(X,Nums,0).
phi(_X,[],Acc) -> Acc;
phi(X,[H|T],Acc) ->
    case coprime(X,H) of
        yes -> phi(X,T,Acc+1);
        no -> phi(X,T,Acc)
    end.

%% 2.10 Other way of calculating Euler's totient
phi2(X) ->
    phi2(X,enc_prime_fact(X),1).
phi2(X,[[P,M]|T],Acc) ->
    phi2(X,T,Acc*(P-1)*pow(P,M-1));
phi2(_,[],Acc) -> Acc.

pow(X,Y) -> pow(X,Y,X).
pow(_,0,_) -> 1;
pow(_,1,Acc) -> Acc ;
pow(X,Y,Acc) -> pow(X,Y-1,Acc*X).

%% 2.11 Testing the performance of both methods
test_phi(X) ->
    {T1,V1} = timer:tc(arith_prb,phi,[X]),
    {T2,V2} = timer:tc(arith_prb,phi2,[X]),
    {{T1,V1},{T2,V2},T1-T2}.

test_avg(N) ->
    test_avg(N,{0,0},N).
test_avg(I,{AT1,AT2},N) when I > 0 ->
    {{T1,_},{T2,_},_} = test_phi(10090),
    test_avg(I-1,{AT1+T1/N,AT2+T2/N},N);
test_avg(0,Acc,_) -> Acc.
