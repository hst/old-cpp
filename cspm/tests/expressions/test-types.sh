#!/bin/bash

. $TESTS/functions.sh

test_script_expression nametype-01 \
"nametype A = {0..5}" \
"A == {0..5}" True

# Tuple expressions don't have their special meaning inside a set
# literal.

test_script_expression nametype-tuple-01 \
"nametype A = {({0},{0}), ({1},{1})}" \
"A == {({0},{0}),({1},{1})}" True

# A nametype definition can be a simple variable reference.

test_script_expression nametype-variable-01 \
"T = {0..5}
nametype A = T" \
"A == {0..5}" True

# The variable doesn't even technically have to have a set value,
# though that would seem daft.

test_script_expression nametype-variable-02 \
"T = 5
nametype A = T" \
"A == 5" True

# You can call all of the built-in set functions.  (Or at least, those
# that return a set.)

test_script_expression nametype-builtin-01 \
"nametype A = union({0..5}, {6..10})" \
"A == {0..10}" True

test_script_expression nametype-builtin-02 \
"nametype A = inter({0..6}, {4..10})" \
"A == {4..6}" True

test_script_expression nametype-builtin-03 \
"nametype A = diff({0..10}, {4..10})" \
"A == {0..3}" True

# You can call a user-defined function, but you have to pass it at
# least one argument, and all of the arguments must be valid type
# expressions.  (No numeric literals, for instance!)

test_script_expression nametype-function-01 \
"T(x) = {0..x}
T5 = T(5)
nametype A = T5" \
"A == {0..5}" True

test_script_expression nametype-function-02 \
"T(x) = inter(x,{0..5})
nametype A = T({2..10})" \
"A == {2..5}" True

test_script_expression nametype-function-03 \
"T() = {0..5}
U = T()
nametype A = U" \
"A == {0..5}" True

# Tuple expressions build a product set — i.e., a set of tuples,
# rather than a tuple of sets.

test_script_expression nametype-tuple-01 \
"nametype A = ({0,1}, {2,3})" \
"A == {(0,2),(0,3),(1,2),(1,3)}" True
