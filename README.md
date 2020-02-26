A program for creating arithemtic expression that evaluate to a certain number,

Overview
========
This program receives a list of input numbers and a target number. Using
all input numbers it creates an arithmetic expression using only addition,
subtraction, multiplication and division operations, that evaluates to the
target number. In case this is not possible, it finds the expression that
evaluates to a number that is a close to the target as possible.

Usage:
======
The program can be used by passing the input numbers and the target number
on the command line:
```
makeexpr number [number ...] target
```
The best expression found will then be printed on stdout.
As an example,
```
makeexpr 1 3 4 6 24
```
will result in output
```
6/(1-3/4) = 24
```


