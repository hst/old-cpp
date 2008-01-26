#!/bin/bash

cd `dirname "$0"`

for c in `cat map-tests`; do
    echo -n "$c... "

    if ./test-eventmap-pairs < $c.input | cmp - $c.output; then
        echo PASS
    else
        echo FAIL
        exit 1
    fi
done
