#!/bin/bash

. $TESTS/functions.sh

test_expression bool-lit-01 "true" True
test_expression bool-lit-02 "false" False
test_expression bool-and-truth-table-01 "false and false" False
test_expression bool-and-truth-table-02 "false and true" False
test_expression bool-and-truth-table-03 "true and false" False
test_expression bool-and-truth-table-04 "true and true" True
test_expression bool-or-truth-table-01 "false or false" False
test_expression bool-or-truth-table-02 "false or true" True
test_expression bool-or-truth-table-03 "true or false" True
test_expression bool-or-truth-table-04 "true or true" True
test_expression bool-not-truth-table-01 "not false" True
test_expression bool-not-truth-table-02 "not true" False
