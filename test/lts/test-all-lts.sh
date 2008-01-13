#!/bin/bash

cd `dirname "$0"`

for c in `cat lts-tests`; do
    echo -n "$c... "
    if ./test-lts < $c.lts | cmp - $c.output; then
        echo PASS
    else
        echo FAIL
        exit 1
    fi
done
