JUDYHASH_DIR = $(srcdir)/libs/judyhash/src_lib

AUTOMAKE_OPTIONS = subdir-objects
AM_CPPFLAGS = -I$(srcdir)/include -I$(JUDYHASH_DIR) $(JUDY_CPPFLAGS)
AM_LDFLAGS = $(JUDY_LDFLAGS)
AM_LIBS = -lJudy

lib_LTLIBRARIES = src/libhst.la

include_HEADERS = \
  $(JUDYHASH_DIR)/judy_map.h \
  $(JUDYHASH_DIR)/judy_map_kdcell.h \
  $(JUDYHASH_DIR)/judy_map_l.h \
  $(JUDYHASH_DIR)/judy_map_m.h \
  $(JUDYHASH_DIR)/judy_set.h \
  $(JUDYHASH_DIR)/judy_set_cell.h \
  $(JUDYHASH_DIR)/judy_set_l.h \
  $(JUDYHASH_DIR)/judy_set_m.h

judyarrayincludedir = $(includedir)/judyarray
judyarrayinclude_HEADERS = \
  $(JUDYHASH_DIR)/judyarray/judy_common.h \
  $(JUDYHASH_DIR)/judyarray/judy_funcs_wrappers.h \
  $(JUDYHASH_DIR)/judyarray/judy_map_common.h \
  $(JUDYHASH_DIR)/judyarray/judy_mapset_common.h \
  $(JUDYHASH_DIR)/judyarray/judy_mapset_list.h \
  $(JUDYHASH_DIR)/judyarray/judy_mapset_map.h \
  $(JUDYHASH_DIR)/judyarray/judy_set_common.h

hstincludedir = $(includedir)/hst
hstinclude_HEADERS = \
  include/hst/zobrist.hh \
  include/hst/intset.hh \
  include/hst/io.hh

src_libhst_la_SOURCES = \
  include/hst/zobrist.hh \
  include/hst/intset.hh \
  include/hst/io.hh \
  include/hst/io-macros.hh \
  src/zobrist.cc \
  src/intset.cc \
  src/io-basic.cc

EXTRA_DIST=\
  src/dirstamp \
  test/intset/test-all-difference.sh \
  test/intset/test-all-intersection.sh \
  test/intset/test-all-overlap.sh \
  test/intset/test-all-union.sh

LDADD=src/libhst.la -lJudy
check_PROGRAMS=\
  test/intset/test-difference \
  test/intset/test-intersection \
  test/intset/test-overlap \
  test/intset/test-union

test_intset_test_difference_SOURCES = test/intset/test-difference.cc
test_intset_test_intersection_SOURCES = test/intset/test-intersection.cc
test_intset_test_overlap_SOURCES = test/intset/test-overlap.cc
test_intset_test_union_SOURCES = test/intset/test-union.cc

check-PROGRAMS: $(check_PROGRAMS)

TESTS=\
  test/intset/test-all-difference.sh \
  test/intset/test-all-intersection.sh \
  test/intset/test-all-overlap.sh \
  test/intset/test-all-union.sh
