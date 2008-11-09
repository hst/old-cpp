#!/bin/bash

. $TESTS/functions.sh

test_expression lambda-01 "\ x @ x + 2" "\ [\"\"] x: (x + 2)"

test_script_expression lambda-02 "P = \\ x @ x + 2" "P(2)" "4"

test_script_expression newline-01 "
P = \\ x @ x + 2" "P(2)" "4"
