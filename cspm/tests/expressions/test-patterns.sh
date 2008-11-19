#!/bin/bash

. $TESTS/functions.sh

test_script_expression patterns-01 "<x,y> = <1,2>" "x" "1"
test_script_expression patterns-02 "<x>^y = tail(<1,2,3>)" "x" "2"
test_script_expression patterns-03 "<x>^y = tail(<1,2,3>)" "y" "<3>"
