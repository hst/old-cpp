#!/bin/bash

. $TESTS/functions.sh

test_expression set-literal-01 "{1,2,3}" "{1,2,3}"
test_expression set-literal-02 "{2,1,3}" "{2,1,3}"
test_expression set-closed-ranged "{1..10}" "{1,2,3,4,5,6,7,8,9,10}"
test_expression set-equality "{1,2,3} == {2,1,3}" "True"
test_expression set-cardinality "card({1,2,3})" "3"
test_expression set-union-01 "union({1,2}, {3,4}) == {1,3,2,4}" "True"
test_expression set-union-02 "union({1,2}, {2,3}) == {1,2,3}" "True"
test_expression set-inter-01 "inter({1,2}, {3,4}) == {}" "True"
test_expression set-inter-02 "inter({1,2}, {2,3}) == {2}" "True"
test_expression set-diff-01 "diff({1,2}, {3,4}) == {1,2}" "True"
test_expression set-diff-02 "diff({1,2}, {2,3}) == {1}" "True"
test_expression set-Union-00 "Union({}) == {}" "True"
test_expression set-Union-01 "Union({{1,2}, {3,4}}) == {1,3,2,4}" "True"
test_expression set-Union-02 "Union({{1,2}, {2,3}}) == {1,2,3}" "True"
test_expression set-Inter-00 "Inter({}) == {}" "True"
test_expression set-Inter-01 "Inter({{1,2}, {3,4}}) == {}" "True"
test_expression set-Inter-02 "Inter({{1,2}, {2,3}}) == {2}" "True"
test_expression set-Set-00 "Set({}) == {{}}" "True"
test_expression set-Set-01 "Set({1,2}) == {{},{1},{2},{1,2}}" "True"
test_expression set-empty-01 "empty({})" True
test_expression set-empty-02 "empty({1})" False
test_expression set-empty-03 "empty({<1>})" False
test_expression set-member-01 "member(1,{})" False
test_expression set-member-02 "member(1,{1})" True
test_expression set-member-03 "member(1,{5,4,3,2,1})" True
