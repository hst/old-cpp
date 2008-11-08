#!/bin/bash

# Usage: run-tests.sh
# Runs all of the CSPM tests.  You should set $DIST_BUILD variable to
# the location of Cabal's dist/build directory.

export DIST_BUILD
export TESTS=$DIST_BUILD/tests

$TESTS/quickcheck-tests.sh
$TESTS/expressions/test-numbers.sh
$TESTS/expressions/test-sequences.sh
$TESTS/expressions/test-sets.sh
$TESTS/expressions/test-booleans.sh
$TESTS/expressions/test-lambdas.sh
