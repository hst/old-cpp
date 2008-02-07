set(HST_INCLUDE_FILES
  zobrist.hh
  intset.hh
  intsetset.hh
  types.hh
  eventmap.hh
  proxy-iterator.hh
  event-stateset-map.hh
  lts.hh
  normalized-lts.hh
  csp.hh
# csp-macros.hh               /* internal header file */
# parser/scanner.hh           /* internal header file */
)

install(
  FILES ${HST_INCLUDE_FILES}
  DESTINATION install/hst
)
