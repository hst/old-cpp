#!/bin/bash

. $TESTS/functions.sh

test_compile lighthouse-19 \
"channel a, b

A = a -> A
B = b -> B
MAIN = A ||| B" \
MAIN \
"process MAIN;
event a;
process A;
prefix A = a -> A;
event b;
process B;
prefix B = b -> B;
interleave MAIN = A ||| B;"
