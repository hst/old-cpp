#!/bin/bash

. $TESTS/functions.sh

test_compile prefix-01 "
channel a

P = a -> P" "P" "process P;
prefix P = a -> P;"

test_compile prefix-02 "
channel a
channel b
channel c

P = a -> b -> c -> P" "P" "process P;
process P.0;
process P.1;
prefix P.1 = c -> P;
prefix P.0 = b -> P.1;
prefix P = a -> P.0;"
