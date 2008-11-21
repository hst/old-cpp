#!/bin/bash

. $TESTS/functions.sh

test_expression numeric-literal 1 1
test_expression math-01 "1 + 1" 2
test_expression math-02 "1 - 1" 0
test_expression math-03 "1 * 2" 2
test_expression math-04 "5 / 2" 2
test_expression math-05 "5 % 2" 1
test_expression math-06 "(1 + 2) * 4" 12

test_expression num-equality-01 "1 == 1" True
test_expression num-equality-02 "1 + 1 == 2" True
test_expression num-equality-03 "1 + 2 == 2" False

test_expression num-inequality-01 "1 != 1" False
test_expression num-inequality-02 "1 + 1 != 2" False
test_expression num-inequality-03 "1 + 2 != 2" True

test_expression num-lt-01 "1 < 2" True
test_expression num-lt-02 "2 < 2" False
test_expression num-lt-03 "3 < 2" False

test_expression num-lte-01 "1 <= 2" True
test_expression num-lte-02 "2 <= 2" True
test_expression num-lte-03 "3 <= 2" False

test_expression num-gt-01 "1 > 2" False
test_expression num-gt-02 "2 > 2" False
test_expression num-gt-03 "3 > 2" True

test_expression num-gte-01 "1 >= 2" False
test_expression num-gte-02 "2 >= 2" True
test_expression num-gte-03 "3 >= 2" True

test_script_expression math-defs-01 "
X = 5
A = X + 2" "A" "7"
