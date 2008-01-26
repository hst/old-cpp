# Copy the test scripts from the source directory to the build
# directory.

set(TEST_FILES
  test-all-eventmap-count.sh
  test-all-eventmap-pairs.sh
  map-tests
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

file(READ map-tests TEST_DATA_FILES)
string(REGEX REPLACE "\n" ";" TEST_DATA_FILES ${TEST_DATA_FILES})

foreach(_test_data_file ${TEST_DATA_FILES})
  foreach(_ext count input output)
    configure_file(
      ${CMAKE_CURRENT_SOURCE_DIR}/${_test_data_file}.${_ext}
      ${CMAKE_CURRENT_BINARY_DIR}/${_test_data_file}.${_ext}
      COPYONLY
    )
  endforeach(_ext)
endforeach(_test_data_file)

# Finally, add compilation targets for all of the test executables.

add_executable(test-eventmap-count test-eventmap-count.cc)
target_link_libraries(test-eventmap-count hst)
add_test(eventmap:count test-all-eventmap-count.sh)

add_executable(test-eventmap-pairs test-eventmap-pairs.cc)
target_link_libraries(test-eventmap-pairs hst)
add_test(eventmap:pairs test-all-eventmap-pairs.sh)
