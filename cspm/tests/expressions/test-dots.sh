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

test_script_expression dot-nametype-01 "
nametype A = {0..1}
nametype B = A.A" "B == {0.0,0.1,1.0,1.1}" True

test_script_expression dot-nametype-02 "
nametype A = {0..1}
nametype B = A.A.A" "B == {0.0.0,0.0.1,0.1.0,0.1.1,1.0.0,1.0.1,1.1.0,1.1.1}" True

test_script_expression dot-nametype-03 "
nametype A = {0..1}
nametype B = A.A
nametype C = A.B" "C == {0.0.0,0.0.1,0.1.0,0.1.1,1.0.0,1.0.1,1.1.0,1.1.1}" True

# a matches 1, b matches 2
test_script_expression dot-lambda-01 "
f(a.b) = a + b" "f(1.2)" 3

# a matches 1, b matches 2
test_script_expression dot-lambda-02 "
g(a.b) = b.a" "g(1.2)" "2.1"

# a matches 1, b matches 2.3
test_script_expression dot-lambda-03 "
g(a.b) = b.a" "g(1.2.3)" "2.3.1"

# 1 matches 1, b matches 2.3
test_script_expression dot-lambda-04 "
h(1.b) = b.1" "h(1.2.3)" "2.3.1"

# a matches 1, but 3 doesn't match 2.3
test_script_expression dot-lambda-05 "
i(a.3) = 3.a" "i(1.2.3)" "Bottom"

# a matches 1, b matches 2, c matches 3
test_script_expression dot-lambda-06 "
j(a.b.c) = c.b.a" "j(1.2.3)" "3.2.1"
