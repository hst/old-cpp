#!/bin/bash

. $TESTS/functions.sh

test_expression dot-01 "1.2" "1.2"

test_script_expression dot-02 "
x = 1.2" "x.3" "1.2.3"

test_script_expression dot-03 "
x = 2.3" "1.x" "1.2.3"

test_script_expression dot-04 "
x = 1.2" "x.3.4" "1.2.3.4"

test_script_expression dot-05 "
x = 3.4" "1.2.x" "1.2.3.4"

test_script_expression dot-06 "
x = 2.3" "1.x.4" "1.2.3.4"

test_script_expression dot-07 "
x = 1.2
y = 3.4" "x.y" "1.2.3.4"
