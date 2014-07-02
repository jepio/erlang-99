-module(logic_prb).
-export([land/2,lor/2,lnot/1,lnand/2,lnor/2,lxor/2,lequ/2,table/1,logic_table/1,
         limpl/2,grey_code/1,huffman_test/0,huffman_code/1]).
%%-compile(export_all).


%% 3.01 Truth tables for logical expressions

land(X,Y) ->
    case [X,Y] of
        [true,true] -> true;
        _ -> false
    end.

lor(X,Y) ->
    case [X,Y] of
        [true,_] -> true;
        [_, true] -> true;
        _ -> false
    end.

lnot(false) -> true;
lnot(true) -> false.

lnand(X,Y) -> lnot(land(X,Y)).

lnor(X,Y) -> lnot(lor(X,Y)).

lxor(_X,_X) -> false;
lxor(_X,_Y) -> true.

limpl(X,Y) ->
    case[X,Y] of
        [true, false] -> false;
        _ -> true
    end.

lequ(_X,_X) -> true;
lequ(_X,_Y) -> false.

%% Function to print the logic table of a function
%% Usage:
%% logic_prb:table(fun logic_prb:limpl/2).
table(F) when is_function(F,2) ->
    Logic = logic_table(2), % was only added later 
    table(Logic,F).

table([H|T],F) ->
    [L1,L2] = H,
    Res = F(L1,L2),
    io:format("~6s ~6s ~6s~n",[L1,L2,Res]),
    table(T,F);
table([],_) ->
    ok.

%% 3.02 Define logical operators
%% This can't be done in Erlang so I won't try.
%% Some keywords that can be looked up here are parse transform
%% or match specification.

%% 3.03 Generalize the truth table for logical expressions taking arbitrary
%% amounts of parameters.
%% Unfortunately this also can't be done in Erlang, since function clauses and
%% defined pattern matching are an integral part of the language.
%% I can write a function that generates a table of logic cases.

logic_table(1) ->
    [[true], [false]];
logic_table(N) ->
    AllTrue=[ [true|X] || X <- logic_table(N-1)],
    AllFalse=[ [false|X] || X <- logic_table(N-1)],
    AllTrue ++ AllFalse. % I was considering to use | but it won't work,
                         % I'd need to do it element by element.

%% 3.04 Construct the N-bit grey code which is actually almost the same as 
%% the above.

grey_code(1) ->
    ["0","1"];
grey_code(N) ->
    FirstPart=[string:concat("0",X) || X <- grey_code(N-1)],
    SecondPart=[string:concat("1",X) || X <- lists:reverse(grey_code(N-1))],
    FirstPart ++ SecondPart.


%% 3.05 Huffman code (this one has a difficulty level of ***, so let's see how
%% it goes).
%%

huffman_test() ->
    Frequencies = [{a,45},{b,13},{c,12},{d,16},{e,9},{f,5}],
    huffman_code(Frequencies).

huffman_code(List) ->
    Tree = tree(List),
    Encoding = encode(Tree),
    Encoding.

tree([{N, _}|[]]) ->
    N;
tree(N) ->
    [{N1,C1},{N2,C2}|Rest] = lists:keysort(2,N),
    tree([{{N1,N2},C1+C2}|Rest]).

encode({Left,Right}) ->
    encode(Left,"1") ++ encode(Right,"0").
encode({Left,Right},Code) ->
    encode(Left,Code++"1") ++ encode(Right,Code++"0");
encode(Value,Code) ->
    [{Value,Code}].

