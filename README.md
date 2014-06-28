# 99 Problems but Erlang ain't one
## Introduction
The *99 Prolog Problems* were formulated many years ago by a teacher working at the University of Applied Sciences in Biel-Bienne, Switzerland. They were meant to teach students how to think in a way that is compatible with logical programming, allowing them to improve their skills in the process.

While functional programming isn't logical programming and Erlang is not Prolog, it used to be based on it and the syntax has remained quite similar<sup>*</sup>. Therefore I have decided to solve the 99 problems using Erlang.

## Problems
I am solving the remade problems as they are stated on this [page](https://sites.google.com/site/prologsite/prolog-problems). The solutions are grouped into categories, starting with lists and ending at graphs. 

## Solutions
The solutions will be grouped into modules according to the categories mentioned in the previous section.

## Testing
To test the functions you can compile and run them from the Erlang shell. An example of doing so:
```erlang
$ erl
1> c(list_prb).
{ok,list_prb}
2> list_prb:reverse([1,2,3]).
[3,2,1]
```
Once I'm done with all the problems I will consider writing unit tests for all functions.

## Progress
* List problems - done, available in the file `list_prb.erl`.
* Arithmetic problems - done, available in the file `arith_prb.erl`.
* Logic and Codes problems - started, file `logic_prb.erl`.

### Final words
I hope that these solutions will be of inspiration to someone who, like me, decides to learn Erlang.

<br>
<br>
<sup>*</sup> I must admit, however, that reading Prolog code I can mostly see what it is suggesting but I don't have a clue how it does what it does :)

