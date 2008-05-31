set(TESTS
  00-basic1
  00-basic2
  00-basic3
  00-basic4
  00-basic5
  00-basic6
  80-roscoe-p40-a
  80-roscoe-p40-b
  80-roscoe-p52-a
  80-roscoe-p52-b
)

# Copy the test data from the source directory to the build directory.

foreach(_test ${TESTS})
  foreach(_ext csp0 traces failures)
    configure_file(
      ${CMAKE_CURRENT_SOURCE_DIR}/${_test}.${_ext}
      ${CMAKE_CURRENT_BINARY_DIR}/${_test}.${_ext}
      COPYONLY
    )
  endforeach(_ext)
endforeach(_test)

# Finally, add compilation targets for all of the test executables.

add_executable(test-traces-refinement test-traces-refinement.cc)
target_link_libraries(test-traces-refinement hst)
foreach(_test ${TESTS})
  add_test(
    refinement:traces:${_test}
    bash -c "./test-traces-refinement < ${_test}.csp0 |
       cmp - ${_test}.traces"
  )
endforeach(_test)

add_executable(test-failures-refinement test-failures-refinement.cc)
target_link_libraries(test-failures-refinement hst)
foreach(_test ${TESTS})
  add_test(
    refinement:failures:${_test}
    bash -c "./test-failures-refinement < ${_test}.csp0 |
       cmp - ${_test}.failures"
  )
endforeach(_test)
