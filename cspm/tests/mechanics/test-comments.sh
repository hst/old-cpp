#!/bin/bash

. $TESTS/functions.sh

test_script_expression line-comment-1 \
"-- This is a comment

a = 5   -- a simple number
b = 6   -- another number

c = a +   -- add the two together
    b" \
"c" \
"11"

test_script_expression block-comment-1 \
"{-
   This is a comment
 -}

a = 5   {- a simple number -}
b = 6   {- another number  -}

c = a +   {- add the two together
 -} b" \
"c" \
"11"

test_script_expression block-comment-2 \
"{-
   This is a comment
   {-
     Block comments should nest.
   -}
   -- Technically line comments are good inside block comments, too,
   -- but this has no real meaning since "--" has no special meaning
   -- in a block comment.
 -}

a = 5
b = 6

c = a + b" \
"c" \
"11"
