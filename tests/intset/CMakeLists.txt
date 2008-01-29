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

add_executable(test-intset test-intset.cc)
target_link_libraries(test-intset hst)
foreach(_test ${TESTS})
  add_test(
    intset:roundtrip:${_test}
    bash -c "./test-intset < ${_test}.input | cmp - ${_test}.output"
  )
endforeach(_test)

add_executable(test-difference test-difference.cc)
target_link_libraries(test-difference hst)
foreach(_test ${TESTS})
  add_test(
    intset:difference:${_test}
    bash -c "./test-difference < ${_test}.diff-input"
  )
endforeach(_test)

add_executable(test-intersection test-intersection.cc)
target_link_libraries(test-intersection hst)
foreach(_test ${TESTS})
  add_test(
    intset:intersection:${_test}
    bash -c "./test-intersection < ${_test}.int-input"
  )
endforeach(_test)

add_executable(test-overlap test-overlap.cc)
target_link_libraries(test-overlap hst)
foreach(_test ${TESTS})
  add_test(
    intset:overlap:${_test}
    bash -c "./test-overlap < ${_test}.overlap-input"
  )
endforeach(_test)

add_executable(test-union test-union.cc)
target_link_libraries(test-union hst)
foreach(_test ${TESTS})
  add_test(
    intset:union:${_test}
    bash -c "./test-union < ${_test}.union-input"
  )
endforeach(_test)
