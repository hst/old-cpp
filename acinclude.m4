# Created by Douglas Creager, March 23, 2007.
# Modified from AX_LIB_MYSQL, downloaded from the Autoconf Macro Archive.
#
# SYNOPSIS
#
#   AX_LIB_JUDY()
#
# DESCRIPTION
#
#   This macro provides tests of availability of the Judy library.
#
#   The --with-judy option takes one of three possible values:
#
#   no - do not check for the Judy client library
#
#   yes - do check for the Judy library in standard locations
#
#   path - prefix of the Judy installation; this directory should
#   contain include/ and lib/ subdirectories containing the Judy
#   development files
#
#   This macro calls:
#
#     AC_SUBST(JUDY_CFLAGS)
#     AC_SUBST(JUDY_LDFLAGS)
#
#   And sets:
#
#     HAVE_JUDY
#
# LAST MODIFICATION
#
#   2007-03-23
#
# COPYRIGHT
#
#   Copyright (c) 2007 Douglas Creager <dcreager@alum.mit.edu>
#
#   Copying and distribution of this file, with or without
#   modification, are permitted in any medium without royalty provided
#   the copyright notice and this notice are preserved.

AC_DEFUN([AC_LIB_JUDY],
[
    JUDY_PREFIX=""
    JUDY_CFLAGS=""
    JUDY_LDFLAGS=""

    AC_ARG_WITH([judy],
        AC_HELP_STRING([--with-judy=@<:@ARG@:>@],
            [use Judy library @<:@default=yes@:>@, optionally specify prefix path to Judy files]
        ),
        [
        if test "$withval" = "no"; then
            want_judy="no"
        elif test "$withval" = "yes"; then
            want_judy="yes"
        else
            want_judy="yes"
            JUDY_PREFIX="$withval"
        fi
        ],
        [want_judy="yes"]
    )

    dnl
    dnl Check Judy libraries
    dnl

    if test "$want_judy" = "yes"; then

        AC_MSG_CHECKING([for Judy libraries])

        AC_LANG_PUSH([C])

        if test "$JUDY_PREFIX" = ""; then
            JUDY_CPPFLAGS=""
            JUDY_LDFLAGS=""
        else
            JUDY_CPPFLAGS="-I$JUDY_PREFIX/include"
            JUDY_LDFLAGS="-L$JUDY_PREFIX/lib"
        fi

        ac_judy_saved_cppflags="$CPPFLAGS"
        ac_judy_saved_ldflags="$LDFLAGS"
        ac_judy_saved_libs="$LIBS"

        CPPFLAGS="$JUDY_CPPFLAGS $CPPFLAGS"
        LDFLAGS="$JUDY_LDFLAGS $LDFLAGS"
        LIBS="-lJudy $LIBS"

        AC_DEFINE([HAVE_JUDY], [1],
            [Define to 1 if Judy libraries are available])

        AC_LINK_IFELSE(
        AC_LANG_PROGRAM([[@%:@include <Judy.h>]],
                        [[
                          Pvoid_t  array = NULL;
                          Judy1Set(&array, 0, NULL);
                        ]]),
        [
            found_judy="yes"
            AC_MSG_RESULT([yes])

            AC_DEFINE(HAVE_JUDY)
        ],
        [
            found_judy="no"
            AC_MSG_RESULT([no])
        ])

        AC_LANG_POP([C])

        CPPFLAGS="$ac_judy_saved_cppflags"
        LDFLAGS="$ac_judy_saved_ldflags"
        LIBS="$ac_judy_saved_libs"
    fi


    AC_SUBST([JUDY_CPPFLAGS])
    AC_SUBST([JUDY_LDFLAGS])
])
