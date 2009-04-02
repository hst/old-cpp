#!/bin/bash

. $TESTS/functions.sh

test_expression bool-lit-01 "true" True
test_expression bool-lit-02 "false" False

test_expression bool-type-01 "member(true,Bool)" True
test_expression bool-type-02 "member(false,Bool)" True
test_expression bool-type-03 "member(0,Bool)" False
test_expression bool-type-04 "member((true,false),Bool)" False
test_expression bool-type-05 "member(0 == 0,Bool)" True
test_expression bool-type-06 "member(0 != 0,Bool)" True

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

test_expression bool-nested-01 "(true or false) and true" True
test_expression bool-nested-02 "(true and false) or true" True
