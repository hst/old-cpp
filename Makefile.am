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
  include/hst/intsetset.hh \
  include/hst/types.hh \
  include/hst/eventmap.hh \
  include/hst/lts.hh \
  include/hst/csp.hh \
  include/hst/io.hh

src_libhst_la_SOURCES = \
  include/hst/zobrist.hh \
  include/hst/intset.hh \
  include/hst/intsetset.hh \
  include/hst/types.hh \
  include/hst/eventmap.hh \
  include/hst/lts.hh \
  include/hst/csp.hh \
  include/hst/csp-macros.hh \
  include/hst/io.hh \
  include/hst/io-macros.hh \
  src/zobrist.cc \
  src/intset.cc \
  src/intsetset.cc \
  src/eventmap.cc \
  src/io-basic.cc \
  src/lts.cc \
  src/csp-basic.cc \
  src/csp-prefix.cc \
  src/csp-naught.cc

EXTRA_DIST=\
  src/dirstamp \
  test/intset/test-all-difference.sh \
  test/intset/test-all-intersection.sh \
  test/intset/test-all-overlap.sh \
  test/intset/test-all-union.sh \
  test/intsetset/test-all-difference.sh \
  test/intsetset/test-all-intersection.sh \
  test/intsetset/test-all-overlap.sh \
  test/intsetset/test-all-union.sh \
  test/eventmap/test-all-eventmap-count.sh \
  test/eventmap/test-all-eventmap-pairs.sh \
  test/lts/test-all-lts.sh \
  test/csp/test-all-csp0.sh

LDADD=src/libhst.la -lJudy
check_PROGRAMS=\
  test/intset/test-difference \
  test/intset/test-intersection \
  test/intset/test-overlap \
  test/intset/test-union \
  test/intsetset/test-difference \
  test/intsetset/test-intersection \
  test/intsetset/test-overlap \
  test/intsetset/test-union \
  test/eventmap/test-eventmap-count \
  test/eventmap/test-eventmap-pairs \
  test/lts/test-lts \
  test/csp/test-csp0

test_intset_test_difference_SOURCES = test/intset/test-difference.cc
test_intset_test_intersection_SOURCES = test/intset/test-intersection.cc
test_intset_test_overlap_SOURCES = test/intset/test-overlap.cc
test_intset_test_union_SOURCES = test/intset/test-union.cc

test_intsetset_test_difference_SOURCES = test/intsetset/test-difference.cc
test_intsetset_test_intersection_SOURCES = test/intsetset/test-intersection.cc
test_intsetset_test_overlap_SOURCES = test/intsetset/test-overlap.cc
test_intsetset_test_union_SOURCES = test/intsetset/test-union.cc

test_eventmap_test_eventmap_count_SOURCES = test/eventmap/test-eventmap-count.cc
test_eventmap_test_eventmap_pairs_SOURCES = test/eventmap/test-eventmap-pairs.cc

test_lts_test_lts_SOURCES = test/lts/test-lts.cc

test_csp_test_csp0_SOURCES = test/csp/test-csp0.cc

check-PROGRAMS: $(check_PROGRAMS)

TESTS=\
  test/intset/test-all-difference.sh \
  test/intset/test-all-intersection.sh \
  test/intset/test-all-overlap.sh \
  test/intset/test-all-union.sh \
  test/intsetset/test-all-difference.sh \
  test/intsetset/test-all-intersection.sh \
  test/intsetset/test-all-overlap.sh \
  test/intsetset/test-all-union.sh \
  test/eventmap/test-all-eventmap-count.sh \
  test/eventmap/test-all-eventmap-pairs.sh \
  test/lts/test-all-lts.sh \
  test/csp/test-all-csp0.sh
