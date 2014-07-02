-module(tree_prb).
-compile([export_all]).

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

bal_tree(0) -> nil;
bal_tree(1) ->
    [{x,nil,nil}];
bal_tree(N) when N rem 2 == 0 ->
    [N1,N2] = [N div 2, N div 2 -1],
    [{x,bal_tree(N1),bal_tree(N2)},{x,bal_tree(N2),bal_tree(N1)}];
bal_tree(N) ->
    N1 = (N-1) div 2,
    {x,bal_tree(N1),bal_tree(N1)}.
%% Currently the tree is kind of "packed" (as in lists represent cases). 
%% Have not figured out how to do this differently yet.

