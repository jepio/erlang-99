-module(tree_prb).
-export([istree/1,bal_tree/1,sym_tree/1]).
%-compile([export_all]).

%% Solutions to the problems concerning binary trees.
%%


%% 4.01 Check whether a term represents a binary tree
%%
%% I think this is the proper way to do this: with pattern matching.

istree({_,L,R}) ->
    istree(L) and istree(R);
istree(nil) -> true;
istree(_) -> false.

%% 4.02 Construct completely balanced tree
%%
%% for this to work I had to make trees be lists, not tuples.

bal_tree(0) -> [nil];
bal_tree(1) ->
    [[x,nil,nil]];
bal_tree(N) when N rem 2 == 0 ->
    [N1,N2] = [N div 2, N div 2 -1],
    [ [x, Y, Z] || Y <- bal_tree(N1), Z <- bal_tree(N2)] ++
    [ [x, Y, Z] || Y <- bal_tree(N2), Z <- bal_tree(N1)];
bal_tree(N) ->
    N1 = (N-1) div 2,
    [ [x, Y, Z] || Y <- bal_tree(N1), Z <- bal_tree(N1)].


%% 4.03 Symmetric binary trees
%%

sym_tree(Tree) ->
    {_, L, R} = Tree,
    sym_tree(L, R).

sym_tree(nil, X) when not is_atom(X) ->
    false;
sym_tree(X, nil) when not is_atom(X) ->
    false;
sym_tree(nil, nil) ->
    true;
sym_tree(L, R) ->
    {LX, LL, LR} = L,
    {RX, RL, RR} = R,
    (LX =:= RX) andalso sym_tree(LL, RR) andalso sym_tree(LR, RL).
