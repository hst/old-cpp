#!/bin/bash

. $TESTS/functions.sh

test_expression seq-literal "<1,2,3>" "<1,2,3>"
test_expression seq-closed-ranged "<1..10>" "<1,2,3,4,5,6,7,8,9,10>"
test_expression seq-concat-01 "<1,2,3> ^ <4,5,6>" "<1,2,3,4,5,6>"
test_expression seq-concat-02 "<1,2,3> ^ <4,5,6> == <1..6>" "True"
test_expression seq-equality "<1,2,3> == <2,1,3>" "False"
