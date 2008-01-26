# Copy the test scripts from the source directory to the build
# directory.

set(TEST_FILES
  test-all-difference.sh
  test-all-intersection.sh
  test-all-overlap.sh
  test-all-union.sh
  intsetset-tests
)

foreach(_test_file ${TEST_FILES})
  configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/${_test_file}
    ${CMAKE_CURRENT_BINARY_DIR}/${_test_file}
    COPYONLY
  )
endforeach(_test_file)

# Copy the test data files over.  We already have a file that defines
# all of the tests, so we can just read it in and loop through it to
# perform the copy.  (First we have to convert the newlines into
# semicolons to turn it into a proper CMake list.)

file(READ intsetset-tests TEST_DATA_FILES)
string(REGEX REPLACE "\n" ";" TEST_DATA_FILES ${TEST_DATA_FILES})

foreach(_test_data_file ${TEST_DATA_FILES})
  foreach(_ext diff-input int-input overlap-input union-input)
    configure_file(
      ${CMAKE_CURRENT_SOURCE_DIR}/${_test_data_file}.${_ext}
      ${CMAKE_CURRENT_BINARY_DIR}/${_test_data_file}.${_ext}
      COPYONLY
    )
  endforeach(_ext)
endforeach(_test_data_file)

# Finally, add compilation targets for all of the test executables.

add_executable(test-difference test-difference.cc)
target_link_libraries(test-difference hst)
add_test(intsetset:difference test-all-difference.sh)

add_executable(test-intersection test-intersection.cc)
target_link_libraries(test-intersection hst)
add_test(intsetset:intersection test-all-intersection.sh)

add_executable(test-overlap test-overlap.cc)
target_link_libraries(test-overlap hst)
add_test(intsetset:overlap test-all-overlap.sh)

add_executable(test-union test-union.cc)
target_link_libraries(test-union hst)
add_test(intsetset:union test-all-union.sh)
