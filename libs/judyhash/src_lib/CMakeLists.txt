set(JUDYHASH_INCLUDE_FILES
  judy_map.h
  judy_map_kdcell.h
  judy_map_l.h
  judy_map_m.h
  judy_set.h
  judy_set_cell.h
  judy_set_l.h
  judy_set_m.h
)

set(JUDYHASH_INCLUDE_JUDYARRAY_FILES
  judyarray/judy_common.h
  judyarray/judy_funcs_wrappers.h
  judyarray/judy_map_common.h
  judyarray/judy_mapset_common.h
  judyarray/judy_mapset_list.h
  judyarray/judy_mapset_map.h
  judyarray/judy_set_common.h
)

install(
  FILES ${JUDYHASH_INCLUDE_FILES}
  DESTINATION include
)

install(
  FILES ${JUDYHASH_INCLUDE_JUDYARRAY_FILES}
  DESTINATION include/judyarray
)
