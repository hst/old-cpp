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
  bugs-9-superset
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

add_executable(refinement/traces test-traces-refinement.cc)
set_target_properties(
  refinement/traces
  PROPERTIES
  OUTPUT_NAME test-traces-refinement
)
target_link_libraries(refinement/traces hst)
foreach(_test ${TESTS})
  add_test(
    refinement:traces:${_test}
    bash -c "./test-traces-refinement < ${_test}.csp0 |
       cmp - ${_test}.traces"
  )
endforeach(_test)

add_executable(refinement/failures test-failures-refinement.cc)
set_target_properties(
  refinement/failures
  PROPERTIES
  OUTPUT_NAME test-failures-refinement
)
target_link_libraries(refinement/failures hst)
foreach(_test ${TESTS})
  add_test(
    refinement:failures:${_test}
    bash -c "./test-failures-refinement < ${_test}.csp0 |
       cmp - ${_test}.failures"
  )
endforeach(_test)
