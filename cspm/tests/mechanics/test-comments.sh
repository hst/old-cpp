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
