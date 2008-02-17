set(TESTS
  "00-basic1 1"
  "00-basic2 1"
  "00-basic3 1"
  "00-basic4 0"
  "80-roscoe-p40-a 1"
  "80-roscoe-p40-b 1"
  "80-roscoe-p52-a 1"
  "80-roscoe-p52-b 1"
)

# Copy the test data from the source directory to the build directory.

foreach(_test ${TESTS})
  separate_arguments(_test)
  list(GET _test 0 _test_file)

  foreach(_ext csp0)
    configure_file(
      ${CMAKE_CURRENT_SOURCE_DIR}/${_test_file}.${_ext}
      ${CMAKE_CURRENT_BINARY_DIR}/${_test_file}.${_ext}
      COPYONLY
    )
  endforeach(_ext)
endforeach(_test)

# Finally, add compilation targets for all of the test executables.

add_executable(test-traces-refinement test-traces-refinement.cc)
target_link_libraries(test-traces-refinement hst)
foreach(_test ${TESTS})
  separate_arguments(_test)
  list(GET _test 0 _test_file)
  list(GET _test 1 _traces_result)

  add_test(
    refinement:traces:${_test_file}
    bash -c "[ `./test-traces-refinement < ${_test_file}.csp0` == '${_traces_result}' ]"
  )
endforeach(_test)
