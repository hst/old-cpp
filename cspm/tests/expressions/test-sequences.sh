#!/bin/bash

. $TESTS/functions.sh

test_expression seq-literal "<1,2,3>" "<1,2,3>"
test_expression seq-closed-ranged "<1..10>" "<1,2,3,4,5,6,7,8,9,10>"
test_expression seq-concat-01 "<1,2,3> ^ <4,5,6>" "<1,2,3,4,5,6>"
test_expression seq-concat-02 "<1,2,3> ^ <4,5,6> == <1..6>" "True"
test_expression seq-equality "<1,2,3> == <2,1,3>" "False"
test_expression seq-length-01 "#<1,2,3>" "3"
test_expression seq-length-02 "length(<1,2,3>)" "3"
test_expression seq-head-01 "head(<1>)" "1"
test_expression seq-head-02 "head(<1,2,3>)" "1"
test_expression seq-head-03 "head(<{1,2},{3}>) == {1,2}" "True"
test_expression seq-tail-01 "tail(<1>)" "<>"
test_expression seq-tail-02 "tail(<1,2,3>)" "<2,3>"
test_expression seq-set-01 "set(<1>) == {1}" True
test_expression seq-set-02 "set(<1,2>) == {1,2}" True
test_expression seq-null-01 "null(<>)" True
test_expression seq-null-02 "null(<1>)" False
test_expression seq-null-03 "null(<{1}>)" False
test_expression seq-elem-01 "elem(1,<>)" False
test_expression seq-elem-02 "elem(1,<1>)" True
test_expression seq-elem-03 "elem(1,<5,4,3,2,1>)" True
