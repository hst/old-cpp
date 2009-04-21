set(TESTS
  test01
)

# Copy the test data from the source directory to the build directory.

foreach(_test ${TESTS})
  foreach(_ext input output diff-input int-input overlap-input union-input)
    configure_file(
      ${CMAKE_CURRENT_SOURCE_DIR}/${_test}.${_ext}
      ${CMAKE_CURRENT_BINARY_DIR}/${_test}.${_ext}
      COPYONLY
    )
  endforeach(_ext)
endforeach(_test)

# Finally, add compilation targets for all of the test executables.

add_executable(intsetset/intsetset test-intsetset.cc)
target_link_libraries(intsetset/intsetset hst)
set_target_properties(
  intsetset/intsetset
  PROPERTIES
  OUTPUT_NAME test-intsetset
)
foreach(_test ${TESTS})
  add_test(
    intsetset:roundtrip:${_test}
    bash -c "./test-intsetset < ${_test}.input | cmp - ${_test}.output"
  )
endforeach(_test)

add_executable(intsetset/difference test-difference.cc)
set_target_properties(
  intsetset/difference
  PROPERTIES
  OUTPUT_NAME test-difference
)
target_link_libraries(intsetset/difference hst)
foreach(_test ${TESTS})
  add_test(
    intsetset:difference:${_test}
    bash -c "./test-difference < ${_test}.diff-input"
  )
endforeach(_test)

add_executable(intsetset/intersection test-intersection.cc)
set_target_properties(
  intsetset/intersection
  PROPERTIES
  OUTPUT_NAME test-intersection
)
target_link_libraries(intsetset/intersection hst)
foreach(_test ${TESTS})
  add_test(
    intsetset:intersection:${_test}
    bash -c "./test-intersection < ${_test}.int-input"
  )
endforeach(_test)

add_executable(intsetset/overlap test-overlap.cc)
set_target_properties(
  intsetset/overlap
  PROPERTIES
  OUTPUT_NAME test-overlap
)
target_link_libraries(intsetset/overlap hst)
foreach(_test ${TESTS})
  add_test(
    intsetset:overlap:${_test}
    bash -c "./test-overlap < ${_test}.overlap-input"
  )
endforeach(_test)

add_executable(intsetset/union test-union.cc)
set_target_properties(
  intsetset/union
  PROPERTIES
  OUTPUT_NAME test-union
)
target_link_libraries(intsetset/union hst)
foreach(_test ${TESTS})
  add_test(
    intsetset:union:${_test}
    bash -c "./test-union < ${_test}.union-input"
  )
endforeach(_test)
