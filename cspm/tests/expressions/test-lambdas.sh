#!/bin/bash

. $TESTS/functions.sh

test_expression lambda-01 "\ x @ x + 2" "\ [\"\"] x: (x + 2)"
