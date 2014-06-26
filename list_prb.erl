-module(list_prb).
-export([last/1,penlast/1,element_at/2,numelements/1,reverse/1,
         pali/1,flat/1,compress/1,pack/1,encode/1,encode_mod/1,
         decode/1,direct_enc/1,dupli/1,dupli_n/2,drop/2,split/2,
         slice/3,rotate/2,remove_at/2,insert_at/3,range/2,
         rnd_select/2,draw_rnd/2,rnd_permu/1,comb/2,group3/1,
         group/2,lsort/1,fsort/1]).
         
%% For testing
%% -compile(export_all).
%% A collection of solutions to the list-problems found on
%% https://sites.google.com/site/prologsite/prolog-problems


%% 1.01 Last element
last([])->
    false;
last(X) ->
    [H|_T] = lists:reverse(X),
    H.
    
%% 1.02 Before last element
penlast([]) ->
    [];
penlast([_]) ->
    [];
penlast(X) ->
    [_H,P|_T] = lists:reverse(X),
    P.

%% 1.03 N'th element of a list
element_at([H|_],1) ->
    H;
element_at([_|T],N) ->
    element_at(T,N-1);
element_at([],_N) ->
    [].

%% 1.04 Number of elements in a list
numelements(X) -> 
    numelements(X,0).
numelements([],Acc) -> Acc;
numelements([_|T],Acc) -> 
    numelements(T,Acc+1).

%% 1.05 Reverse a list
reverse(X) -> 
    reverse(X,[]).
reverse([],Acc) -> Acc;
reverse([H|T],Acc) -> 
    reverse(T,[H|Acc]).

%% 1.06 Find out if list is a palindrome
pali([]) -> false;
pali(X) ->
    X == reverse(X).
    
%% 1.07 Flatten a nested list structure
flat(X) -> 
    reverse(flat(X,[])).
flat([],Acc) -> 
    Acc;
flat([H|T],Acc) ->
    case is_list(H) of
        true ->     
            flat(T,flat(H,Acc));
        false -> 
            flat(T,[H|Acc])
    end.
    
%% 1.08  Eliminate consecutive duplicates
compress(X) ->
    compress(X,[]).
compress([],Acc) ->
    reverse(Acc);
compress([H|T],[H|AT]) ->
        compress(T,[H|AT]);
compress([H|T],Acc) -> compress(T,[H|Acc]).

%% 1.09 Pack cons. duplicates into lists
pack([H|T]) ->    
    pack(T,[H],[]).
pack([],X,Acc) -> 
    reverse([X|Acc]);
pack([H|T],[H|TempT],Acc) ->
    pack(T,[H,H|TempT],Acc);
pack([H|T],Temp,Acc) ->
    pack(T,[H],[Temp|Acc]).
    
%% 1.10 Run-length encoding
encode(X) ->
    XX = pack(X),
    encode(XX,[]).
encode([],Acc) ->
    reverse(Acc);
encode([H|T],Acc) ->
    [H1|_] = H,
    encode(T,[[numelements(H),H1]|Acc]).
    
%% 1.11 Modified run-length encoding
encode_mod(X) ->
    XX = encode(X),
    encode_mod(XX,[]).
encode_mod([],Acc) ->
    reverse(Acc);
encode_mod([[1,C]|T],Acc) ->
    encode_mod(T,[C|Acc]);
encode_mod([[N,C]|T],Acc) ->
    encode_mod(T,[[N,C]|Acc]).
    
%% 1.12 Decode run-length encoding
decode(X) ->
    decode(X,[]).
decode([], Acc) ->
    reverse(Acc);
decode([H|T],Acc) when is_list(H) ->
    [N,C] = H,
    New = lists:duplicate(N,C),
    decode(T,prepend(New,Acc));
decode([X|T],Acc) ->
    decode(T,[X|Acc]).

% To avoid using the ++ operator, which copies the list  
prepend([],C) ->
    C;
prepend([H|T],C) ->
    prepend(T,[H|C]).
    
%% 1.13 Direct run-length encoding
direct_enc([H|T]) ->
    direct_enc(T,[1,H],[]).
direct_enc([],[N,Curr],Acc) ->
    case N of
        1 -> reverse([Curr|Acc]);
        N -> reverse([[N,Curr]|Acc])
    end;
direct_enc([Curr|T],[N,Curr],Acc) ->
    direct_enc(T,[N+1,Curr],Acc);
direct_enc([H|T],[N,Curr],Acc) when N > 1 ->
    direct_enc(T,[1,H],[[N,Curr]|Acc]);
direct_enc([H|T],[1,Curr],Acc) ->
    direct_enc(T,[1,H],[Curr|Acc]).

%% 1.14 Duplicate elements
dupli(X) ->
    dupli(X, []).
dupli([],Acc) ->
    reverse(Acc);
dupli([H|T],Acc) ->
    dupli(T,[H,H|Acc]).

%% 1.15 Duplicate N-times
dupli_n(X,N) ->
    dupli_n(X,N,[]).
dupli_n([],_N,Acc) ->
    reverse(Acc);
dupli_n([H|T],N,Acc) ->
    dupli_n(T,N,prepend_n([H],N,Acc)).
    
% For some reason when I had a clause inside dupli_n for single
% item lists, it didn't work. So a new function:
prepend_n([_],0,Acc) ->
    Acc;
prepend_n([H],N,Acc) ->
    NAcc = prepend([H],Acc),
    prepend_n([H],N-1,NAcc).
    
%% 1.16 Drop every N'th element
drop(X,N) ->
    drop(X,1,N,[]).
drop([],_,_,Acc) ->
    reverse(Acc);
drop([_H|T],N,N,Acc) ->
    drop(T,1,N,Acc);
drop([H|T],I,N,Acc) ->
    drop(T,I+1,N,[H|Acc]).
    
%% 1.17 Split into two parts
split(X,N) ->
    split(X,N,[],[]).
split([],_N,Acc1,Acc2) ->
    RAcc1 = reverse(Acc1),
    [RAcc1,Acc2];
split(X,0,Acc1,_) ->
    split([],0,Acc1,X);
split([H|T],N,Acc1,Acc2) ->
    split(T,N-1,[H|Acc1],Acc2).
    
%% 1.18 Extract slice
slice(X,N,M) ->
    slice(X,N-1,M,[]).
slice(_,0,0,Acc) ->
    reverse(Acc);
slice([],_,_,Acc) ->
    reverse(Acc);
slice([H|T],0,M,Acc) ->
    slice(T,0,M-1,[H|Acc]);
slice([_H|T],N,M,Acc) ->
    slice(T,N-1,M-1,Acc).
    
%% 1.19 Rotate list
rotate(X,N) when N >= 0 ->
    sra(X,N);
rotate(X,N) when N < 0 ->
    M = numelements(X),
    sra(X,M+N).
    
% s(plit) r(everse) a(ppend)
sra(X,N) ->
    [A,B] = split(X,N),
    B ++ A.
    
%% 1.20 Remove N'th element
remove_at(X,N) ->
    remove_at(X,N,[]).
remove_at([H|T],1,Acc) ->
    [H, reverse(Acc) ++ T];
remove_at([H|T],N,Acc) when N > 0 ->
    remove_at(T,N-1,[H|Acc]).
    
%% 1.21 Insert element at given position
insert_at(X,L,N) ->
    insert_at(X,L,N,[]).
insert_at([],L,_,Acc) ->
    reverse([L|Acc]);
insert_at([H|T],L,1,Acc) ->
    reverse(prepend(T,[H,L|Acc]));
insert_at([H|T],L,N,Acc) ->
    insert_at(T,L,N-1,[H|Acc]).
    
%% 1.22 Create list of range
range(N,M) when N < M ->
    range(N,M,[]).
range(M,M,Acc) ->
    reverse([M|Acc]);
range(N,M,Acc) ->
    range(N+1,M,[N|Acc]).
    
%% 1.23 Random select
rnd_select(X,N) when N =< length(X) ->
    random:seed(now()),
    rnd_select(X,N,[]).
rnd_select(_,0,Acc) ->
    Acc;
rnd_select(X,N,Acc) ->
    I = random:uniform(numelements(X)),
    [Element,List]=remove_at(X,I),
    rnd_select(List,N-1,[Element|Acc]).
    
%% 1.24 Draw random from range
draw_rnd(N,Max) when N =< Max ->
    R = range(1,Max),
    rnd_select(R,N).
    
%% 1.25 Random permutation
rnd_permu(X) ->
    N = numelements(X),
    rnd_select(X,N).
    
%% 1.26 All combinations
comb(0,_) ->
    [[]];
comb(_,[]) ->
    [];
comb(N,[H|T]) ->
% This expression is brilliant, too bad I didn't come up with it myself :(
    [[H|L] || L <- comb(N-1,T)] ++ comb(N,T).
    
%% 1.27a) Group a list of 9 into disjoint subsets of 2,3 and 4.
group3(X) ->
    G1 = selectN(2,[X],[[]]),
    R1 = subtract(X,G1),
    G2 = selectN(3,R1,G1),
    R2 = subtract(X,G2),
    selectN(4,R2,G2).

selectN(N,X,Y) ->
    selectN(N,X,Y,[]).
selectN(_,[],[],Acc) ->
    Acc;
selectN(N,[H|T],[AH|AT],Acc) ->
    X = [ AH++[L] || L <- comb(N,H) ],
    selectN(N,T,AT,Acc++X).

subtract(X,G) ->
    [ X -- flat(L) || L <- G].

%% 1.27b) generalized version of the above.
group(List,GroupNums) ->
    group(List,[[]],GroupNums,[List]).
group(_,G1,[],_) ->
    G1;
group(List,G1,[GroupHead|GroupTail],R1) ->
    G2 = selectN(GroupHead,R1,G1),
    R2 = subtract(List,G2),
    group(List,G2,GroupTail,R2).
% This time I can say I did it on my own :)

%% 1.28a) Sort lists according to length with qsort :)
lsort(L) ->
    KL = makekeys(L),
    SKL = keysort(KL),
    removekeys(SKL).

% make lenght keys on all elements
makekeys(L) ->
    makekeys(L,[]).
makekeys([],Acc) ->
    reverse(Acc);
makekeys([H|T],Acc) ->
    makekeys(T,[[length(H),H]|Acc]).
 
% remove keys from elements 
removekeys(L) ->
    removekeys(L,[]).
removekeys([],Acc) ->
    reverse(Acc);
removekeys([H|T],Acc) ->
    [_,RemovedKey] = H,
    removekeys(T,[RemovedKey|Acc]).

% sort according to key    
keysort([]) ->
    [];
keysort([H|T]) ->
    [Piv,_] = H,
    Lon = [ [Len,X] || [Len,X] <- T, Len > Piv],
    Sho = [ [Len,X] || [Len,X] <- T, Len  < Piv],
    Eq = [ [Len,X] || [Len,X] <- T, Len  =:= Piv],
    keysort(Sho) ++ [H|Eq] ++ keysort(Lon).
 
%% 1.28b) Sort lists according to the frequency
fsort(L) ->
    KL = makekeys2(L),
    SKL = keysort(KL),
    removekeys(SKL).

% make frequency keys on all elements
makekeys2(L) ->
    Lengths = find_length(L),
    F = find_freq(Lengths),
    makekeys2(L,F,[]).
makekeys2([],_,Acc) ->
    reverse(Acc);
makekeys2([H|T],F,Acc) ->
    makekeys2(T,F,[[get_freq(H,F),H]|Acc]).
    
% various useful functions:
% create list of sublist lengths
find_length(List) ->
    find_length(List,[]).
find_length([],Acc) ->
    reverse(Acc);
find_length([H|T],Acc) ->
    find_length(T,[length(H)|Acc]).

% create an encoded list of frequencies of lengths
find_freq(Lengths) ->
    Sorted = lists:sort(Lengths),
    encode(Sorted).

% get the sublist frequency number
get_freq(H,F) when is_list(H) ->
    get_freq(length(H),F);
get_freq(L,[[N,L]|_]) ->
    N;
get_freq(L,[_|T]) ->
    get_freq(L,T).
