#!/bin/bash

. $TESTS/functions.sh

test_compile channels-01 "
channel a: {0..5}

P(0) = a.0 -> STOP
P(x) = a.x -> P(x-1)" "P(5)" "event a.5;
process P.1;
event a.4;
process P.2;
event a.3;
process P.3;
event a.2;
process P.4;
event a.1;
process P.5;
event a.0;
process P.6;
prefix P.6 = a.0 -> STOP;
prefix P.5 = a.1 -> P.6;
prefix P.4 = a.2 -> P.5;
prefix P.3 = a.3 -> P.4;
prefix P.2 = a.4 -> P.3;
prefix P.1 = a.5 -> P.2;"
