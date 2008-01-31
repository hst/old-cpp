bison_file(parser/intset.yy)
bison_file(parser/intsetset.yy)
bison_file(parser/eventmap.yy)
bison_file(parser/lts.yy)
bison_file(parser/csp0.yy)

set_source_files_properties(
  parser/scanner.cc
  ${CMAKE_CURRENT_BINARY_DIR}/parser/intset.tab.cc
  ${CMAKE_CURRENT_BINARY_DIR}/parser/intsetset.tab.cc
  ${CMAKE_CURRENT_BINARY_DIR}/parser/eventmap.tab.cc
  ${CMAKE_CURRENT_BINARY_DIR}/parser/lts.tab.cc
  ${CMAKE_CURRENT_BINARY_DIR}/parser/csp0.tab.cc
  COMPILE_FLAGS
  "-I${CMAKE_CURRENT_BINARY_DIR}/parser"
)

set(LIBHST_SOURCES
  zobrist.cc
  intset.cc
  intsetset.cc
  eventmap.cc
  lts.cc
  csp-basic.cc
  csp-prefix.cc
  csp-extchoice.cc
  csp-intchoice.cc
  csp-interrupt.cc
  csp-seqcomp.cc
  csp-interleave.cc
  csp-interface-parallel.cc
  csp-alphabetized-parallel.cc
  csp-hide.cc
  csp-rename.cc
  parser/scanner.cc
  ${CMAKE_CURRENT_BINARY_DIR}/parser/intset.tab.cc
  ${CMAKE_CURRENT_BINARY_DIR}/parser/intsetset.tab.cc
  ${CMAKE_CURRENT_BINARY_DIR}/parser/eventmap.tab.cc
  ${CMAKE_CURRENT_BINARY_DIR}/parser/lts.tab.cc
  ${CMAKE_CURRENT_BINARY_DIR}/parser/csp0.tab.cc
)

add_library(hst SHARED ${LIBHST_SOURCES})
target_link_libraries(hst ${JUDY_LIBRARIES})

install(
  TARGETS hst
  LIBRARY DESTINATION lib
)
