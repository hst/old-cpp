set(TESTS
  test01
  test02
  test03
  test04
  test05
)

# Copy the test data from the source directory to the build directory.

foreach(_test ${TESTS})
  foreach(_ext input output)
    configure_file(
      ${CMAKE_CURRENT_SOURCE_DIR}/${_test}.${_ext}
      ${CMAKE_CURRENT_BINARY_DIR}/${_test}.${_ext}
      COPYONLY
    )
  endforeach(_ext)
endforeach(_test)

# Finally, add compilation targets for all of the test executables.

add_executable(test-equivalences test-equivalences.cc)
target_link_libraries(test-equivalences hst)
foreach(_test ${TESTS})
  add_test(
    equivalences:${_test}
    bash -c "./test-equivalences < ${_test}.input | cmp - ${_test}.output"
  )
endforeach(_test)
