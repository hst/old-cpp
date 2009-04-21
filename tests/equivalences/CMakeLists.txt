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

add_executable(equivalences/roundtrip test-equivalences.cc)
set_target_properties(
  equivalences/roundtrip
  PROPERTIES
  OUTPUT_NAME test-equivalences
)
target_link_libraries(equivalences/roundtrip hst)
foreach(_test ${TESTS})
  add_test(
    equivalences:roundtrip:${_test}
    bash -c "./test-equivalences < ${_test}.input | cmp - ${_test}.output"
  )
endforeach(_test)
