set(HST_INCLUDE_FILES
  zobrist.hh
  intset.hh
  intsetset.hh
  types.hh
  eventmap.hh
  event-stateset-map.hh
  state-stateset-map.hh
  equivalence.hh
  lts.hh
  normalized-lts.hh
  assertions.hh
  csp.hh
# csp-macros.hh               /* internal header file */
# parser/scanner.hh           /* internal header file */
)

install(
  FILES ${HST_INCLUDE_FILES}
  DESTINATION include/hst
)
