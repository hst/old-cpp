#!/bin/bash

. $TESTS/functions.sh

test_expression lambda-01 "\ x @ x + 2" "\ [\"rootExpr\"] {(x) @ (x + 2)}"

test_script_expression lambda-02 "P = \\ x @ x + 2" "P(2)" "4"

test_script_expression newline-01 "
P = \\ x @ x + 2" "P(2)" "4"

test_script_expression function-01 "a(x) = x + 5" "a(5)" "10"

test_script_expression function-length \
"length'(<>) = 0
length'(<_>^xs) = 1 + length'(xs)" \
"<length'(<>), length'(<1>), length'(<1,2,3,4,5>)>" "<0,1,5>"

test_script_expression function-reverse \
"reverse(<>) = <>
reverse(<x>^xs) = reverse(xs)^<x>" \
"<reverse(<>), reverse(<1>), reverse(<1,2,3,4,5>)>" \
'<<>,<1>,<5,4,3,2,1>>'
