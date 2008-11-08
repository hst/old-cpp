#!/bin/bash

. $TESTS/functions.sh

test_expression set-literal-01 "{1,2,3}" "{1,2,3}"
test_expression set-literal-02 "{2,1,3}" "{2,1,3}"
test_expression set-closed-ranged "{1..10}" "{1,2,3,4,5,6,7,8,9,10}"
test_expression set-equality "{1,2,3} == {2,1,3}" "True"
