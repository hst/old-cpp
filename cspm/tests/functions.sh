#!/bin/bash

test_expression () {
    NAME="$1"
    shift

    EXPRESSION="$1"
    shift

    EXPECTED="$1"
    shift

    $DIST_BUILD/cspm/cspm evaluate -s "" "$EXPRESSION" > $TESTS/actual
    echo "$EXPRESSION = $EXPECTED" > $TESTS/expected

    echo -n "$NAME:"

    if cmp $TESTS/expected $TESTS/actual > /dev/null 2>/dev/null; then
        echo " passed."
    else
        echo " FAILED."
        echo "Expected:"
        cat $TESTS/expected
        echo "Actual:"
        cat $TESTS/actual
        exit 1
    fi
}