#!/bin/bash

. $TESTS/functions.sh

test_compile rextchoice-01 "
channel a
channel b

P = [] {a -> P, b -> P}" "P" \
"process P;
process P.0;
prefix P.0 = a -> P;
process P.1;
prefix P.1 = b -> P;
rextchoice P = [] {P.0,P.1};"

test_compile rextchoice-02 "
channel a
channel b
channel c

P = [] {a -> P, b -> P, c -> P}" "P" \
"process P;
process P.0;
prefix P.0 = a -> P;
process P.1;
prefix P.1 = b -> P;
process P.2;
prefix P.2 = c -> P;
rextchoice P = [] {P.0,P.1,P.2};"
