add_subdirectory(bin)

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
  "-I${CMAKE_CURRENT_BINARY_DIR}/parser -fno-strict-aliasing"
)

set(LIBHST_SOURCES
  zobrist.cc
  intset.cc
  intsetset.cc
  eventmap.cc
  event-stateset-map.cc
  state-stateset-map.cc
  equivalence.cc
  lts.cc
  csp-basic.cc
  assertions/refine.cc
  normalization/bisimulate.cc
  normalization/closure.cc
  normalization/divergent.cc
  normalization/normalize.cc
  normalization/normalized-lts.cc
  normalization/prenormalize.cc
  operators/prefix.cc
  operators/extchoice.cc
  operators/intchoice.cc
  operators/timeout.cc
  operators/seqcomp.cc
  operators/interleave.cc
  operators/interface-parallel.cc
  operators/alphabetized-parallel.cc
  operators/hide.cc
  operators/rename.cc
  parser/scanner.cc
  ${CMAKE_CURRENT_BINARY_DIR}/parser/intset.tab.cc
  ${CMAKE_CURRENT_BINARY_DIR}/parser/intsetset.tab.cc
  ${CMAKE_CURRENT_BINARY_DIR}/parser/eventmap.tab.cc
  ${CMAKE_CURRENT_BINARY_DIR}/parser/lts.tab.cc
  ${CMAKE_CURRENT_BINARY_DIR}/parser/csp0.tab.cc
)

add_library(hst SHARED ${LIBHST_SOURCES})
target_link_libraries(hst ${JUDY_LIBRARIES})
set_target_properties(hst
  PROPERTIES
    VERSION 0.0.0
    SOVERSION 0
)

install(
  TARGETS hst
  LIBRARY DESTINATION lib
)
