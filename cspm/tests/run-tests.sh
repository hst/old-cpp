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
$TESTS/expressions/test-lets.sh
$TESTS/expressions/test-patterns.sh

$TESTS/processes/test-prefix.sh
$TESTS/processes/test-extchoice.sh
$TESTS/processes/test-intchoice.sh
$TESTS/processes/test-timeout.sh
$TESTS/processes/test-seqcomp.sh
$TESTS/processes/test-interleave.sh
$TESTS/processes/test-iparallel.sh
$TESTS/processes/test-aparallel.sh
$TESTS/processes/test-hide.sh
$TESTS/processes/test-rextchoice.sh

$TESTS/processes/test-newlines.sh
