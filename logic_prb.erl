-module(logic_prb).

-compile(export_all).


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

lxor(X,X) -> false;
lxor(X,Y) -> true.

limpl(X,Y) ->
    case[X,Y] of
        [true, false] -> false;
        _ -> true
    end.

lequ(X,X) -> true;
lequ(X,Y) -> false.

%% Function to print the logic table of a function
%% Usage:
%% logic_prb:table(fun logic_prb:limpl/2).
table(F) when is_function(F,2) ->
    Logic = [[true,true],
             [true,false],
             [false,true],
             [false,false]],
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


