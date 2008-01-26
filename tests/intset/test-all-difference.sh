#!/bin/bash

cd `dirname "$0"`

for c in `cat intset-tests`; do
    echo -n "$c... "
    if ./test-difference < $c.diff-input; then
        echo PASS
    else
        echo FAIL
        exit 1
    fi
done
