#!/bin/bash

cd `dirname "$0"`

for c in `cat csp-tests`; do
    echo -n "$c... "
    if ./test-csp0 < $c.csp0 | cmp - $c.output; then
        echo PASS
    else
        echo FAIL
        exit 1
    fi
done
