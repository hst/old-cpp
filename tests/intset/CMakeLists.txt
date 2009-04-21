set(TESTS
  test01
)

# Copy the test data from the source directory to the build directory.

foreach(_test ${TESTS})
  foreach(_ext input output diff-input int-input overlap-input
          union-input superset-input)
    configure_file(
      ${CMAKE_CURRENT_SOURCE_DIR}/${_test}.${_ext}
      ${CMAKE_CURRENT_BINARY_DIR}/${_test}.${_ext}
      COPYONLY
    )
  endforeach(_ext)
endforeach(_test)

# Finally, add compilation targets for all of the test executables.

add_executable(intset/intset test-intset.cc)
set_target_properties(
  intset/intset
  PROPERTIES
  OUTPUT_NAME test-intset
)
target_link_libraries(intset/intset hst)
foreach(_test ${TESTS})
  add_test(
    intset:roundtrip:${_test}
    bash -c "./test-intset < ${_test}.input | cmp - ${_test}.output"
  )
endforeach(_test)

add_executable(intset/difference test-difference.cc)
set_target_properties(
  intset/difference
  PROPERTIES
  OUTPUT_NAME test-difference
)
target_link_libraries(intset/difference hst)
foreach(_test ${TESTS})
  add_test(
    intset:difference:${_test}
    bash -c "./test-difference < ${_test}.diff-input"
  )
endforeach(_test)

add_executable(intset/intersection test-intersection.cc)
set_target_properties(
  intset/intersection
  PROPERTIES
  OUTPUT_NAME test-intersection
)
target_link_libraries(intset/intersection hst)
foreach(_test ${TESTS})
  add_test(
    intset:intersection:${_test}
    bash -c "./test-intersection < ${_test}.int-input"
  )
endforeach(_test)

add_executable(intset/overlap test-overlap.cc)
set_target_properties(
  intset/overlap
  PROPERTIES
  OUTPUT_NAME test-overlap
)
target_link_libraries(intset/overlap hst)
foreach(_test ${TESTS})
  add_test(
    intset:overlap:${_test}
    bash -c "./test-overlap < ${_test}.overlap-input"
  )
endforeach(_test)

add_executable(intset/union test-union.cc)
set_target_properties(
  intset/union
  PROPERTIES
  OUTPUT_NAME test-union
)
target_link_libraries(intset/union hst)
foreach(_test ${TESTS})
  add_test(
    intset:union:${_test}
    bash -c "./test-union < ${_test}.union-input"
  )
endforeach(_test)

add_executable(intset/superset test-superset.cc)
set_target_properties(
  intset/superset
  PROPERTIES
  OUTPUT_NAME test-superset
)
target_link_libraries(intset/superset hst)
foreach(_test ${TESTS})
  add_test(
    intset:superset:${_test}
    bash -c "./test-superset < ${_test}.superset-input"
  )
endforeach(_test)
