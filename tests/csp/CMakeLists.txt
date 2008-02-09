set(TESTS
  00-prefix
  00-extchoice1
  00-extchoice2
  00-extchoice3
  00-intchoice1
  00-intchoice2
  00-interrupt1
  00-interrupt2
  00-seqcomp1
  00-seqcomp2
  00-interleave1
  00-interleave2
  00-interleave3
  00-interleave4
  00-interleave5
  00-iparallel1
  00-iparallel2
  00-iparallel3
  00-iparallel4
  00-aparallel1
  00-aparallel2
  00-aparallel3
  00-hide1
  00-hide2
  00-hide3
  00-rename1
  00-rename2
  00-rename3
  10-extchoice-intchoice
  10-intchoice-extchoice
)

# Copy the test data from the source directory to the build directory.

foreach(_test ${TESTS})
  foreach(_ext csp0 output prenormal-output)
    configure_file(
      ${CMAKE_CURRENT_SOURCE_DIR}/${_test}.${_ext}
      ${CMAKE_CURRENT_BINARY_DIR}/${_test}.${_ext}
      COPYONLY
    )
  endforeach(_ext)
endforeach(_test)

# Finally, add compilation targets for all of the test executables.

add_executable(test-csp0 test-csp0.cc)
target_link_libraries(test-csp0 hst)
foreach(_test ${TESTS})
  add_test(
    csp:compile:${_test}
    bash -c "./test-csp0 < ${_test}.csp0 | cmp - ${_test}.output"
  )
endforeach(_test)

add_executable(test-prenormalize test-prenormalize.cc)
target_link_libraries(test-prenormalize hst)
foreach(_test ${TESTS})
  add_test(
    csp:prenormalize:${_test}
    bash -c "./test-prenormalize < ${_test}.csp0 | 
       cmp - ${_test}.prenormal-output"
  )
endforeach(_test)
