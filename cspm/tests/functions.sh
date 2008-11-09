#!/bin/bash

test_expression () {
    NAME="$1"
    EXPRESSION="$2"
    EXPECTED="$3"

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

test_script_expression () {
    NAME="$1"
    SCRIPT="$2"
    EXPRESSION="$3"
    EXPECTED="$4"

    $DIST_BUILD/cspm/cspm evaluate -s "$SCRIPT" "$EXPRESSION" > $TESTS/actual
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

test_compile () {
    NAME="$1"
    SCRIPT="$2"
    EXPRESSION="$3"
    EXPECTED="$4"

    $DIST_BUILD/cspm/cspm compile -s "$SCRIPT" "$EXPRESSION" > $TESTS/actual
    echo "$EXPECTED" > $TESTS/expected

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
