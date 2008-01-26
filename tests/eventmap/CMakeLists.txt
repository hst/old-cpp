set(TESTS
  empty-map
  functional
  not-functional
  duplicates
)

# Copy the test data from the source directory to the build directory.

foreach(_test ${TESTS})
  foreach(_ext count input output)
    configure_file(
      ${CMAKE_CURRENT_SOURCE_DIR}/${_test}.${_ext}
      ${CMAKE_CURRENT_BINARY_DIR}/${_test}.${_ext}
      COPYONLY
    )
  endforeach(_ext)
endforeach(_test)

# Finally, add compilation targets for all of the test executables.

add_executable(test-eventmap-count test-eventmap-count.cc)
target_link_libraries(test-eventmap-count hst)
foreach(_test ${TESTS})
  add_test(
    eventmap:count:${_test}
    bash -c "./test-eventmap-count < ${_test}.input | cmp - ${_test}.count"
  )
endforeach(_test)

add_executable(test-eventmap-pairs test-eventmap-pairs.cc)
target_link_libraries(test-eventmap-pairs hst)
foreach(_test ${TESTS})
  add_test(
    eventmap:pairs:${_test}
    bash -c "./test-eventmap-pairs < ${_test}.input | cmp - ${_test}.output"
  )
endforeach(_test)
