#!/bin/bash

. $TESTS/functions.sh

# Nametypes

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

# Datatypes

test_script_expression datatype-SimpleColour-01 \
"datatype SimpleColour = Red | Green | Blue" \
"SimpleColour == {Red,Green,Blue}" True

test_script_expression datatype-SimpleColour-02 \
"datatype SimpleColour = Red | Green | Blue" \
"Red" "[:Red:]"

test_script_expression datatype-SimpleColour-03 \
"datatype SimpleColour = Red | Green | Blue" \
"Green" "[:Green:]"

test_script_expression datatype-SimpleColour-04 \
"datatype SimpleColour = Red | Green | Blue" \
"Blue" "[:Blue:]"

test_script_expression datatype-complex-T-01 \
"datatype T = A.{0..3} | B.Set({0,1}) | C" \
"T == {A.0,A.1,A.2,A.3,B.{},B.{0},B.{1},B.{0,1},C}" True

test_script_expression datatype-complex-T-02 \
"datatype T = A.{0..3} | B.Set({0,1}) | C" \
"A" "[:A:]"

test_script_expression datatype-complex-T-03 \
"datatype T = A.{0..3} | B.Set({0,1}) | C" \
"B" "[:B:]"

test_script_expression datatype-complex-T-04 \
"datatype T = A.{0..3} | B.Set({0,1}) | C" \
"C" "[:C:]"

test_script_expression datatype-ComplexColour-Black \
"Gun = {0..15}
datatype ComplexColour = RGB.Gun.Gun.Gun | Grey.Gun | Black | White

make_colour(r.g.b) =
  if r!=g or g!=b then RGB.r.g.b else
  if r==0 then Black else
  if r==15 then White else Grey.r" \
"make_colour(0.0.0) == Black" "True"

test_script_expression datatype-ComplexColour-Grey8 \
"Gun = {0..15}
datatype ComplexColour = RGB.Gun.Gun.Gun | Grey.Gun | Black | White

make_colour(r.g.b) =
  if r!=g or g!=b then RGB.r.g.b else
  if r==0 then Black else
  if r==15 then White else Grey.r" \
"make_colour(8.8.8) == (Grey.8)" "True"

test_script_expression datatype-ComplexColour-White \
"Gun = {0..15}
datatype ComplexColour = RGB.Gun.Gun.Gun | Grey.Gun | Black | White

make_colour(r.g.b) =
  if r!=g or g!=b then RGB.r.g.b else
  if r==0 then Black else
  if r==15 then White else Grey.r" \
"make_colour(15.15.15) == White" "True"

test_script_expression datatype-ComplexColour-RGB \
"Gun = {0..15}
datatype ComplexColour = RGB.Gun.Gun.Gun | Grey.Gun | Black | White

make_colour(r.g.b) =
  if r!=g or g!=b then RGB.r.g.b else
  if r==0 then Black else
  if r==15 then White else Grey.r" \
"make_colour(1.2.3) == (RGB.1.2.3)" "True"
