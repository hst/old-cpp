AC_INIT(hst, 0.1)
AC_PREREQ(2.59)

AC_CONFIG_SRCDIR(src/dirstamp)

AM_INIT_AUTOMAKE

AC_PROG_CXX
AC_PROG_CXX_C_O

AC_PROG_LIBTOOL

AC_CHECK_SIZEOF([int])

AC_LIB_JUDY
if test "$found_judy" = "no"; then
   AC_MSG_ERROR([cannot find Judy libraries])
fi

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
