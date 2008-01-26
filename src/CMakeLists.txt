set(LIBHST_SOURCES
  zobrist.cc
  intset.cc
  intsetset.cc
  eventmap.cc
  io-basic.cc
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
  csp-naught.cc
)

add_library(hst SHARED ${LIBHST_SOURCES})
target_link_libraries(hst ${JUDY_LIBRARIES})

install(
  TARGETS hst
  LIBRARY DESTINATION lib
)
