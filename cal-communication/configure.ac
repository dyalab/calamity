#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT([cal-coms], [1.0], [mschack@mines.edu])
AM_INIT_AUTOMAKE([-Wall subdir-objects])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])

# Checks for programs.
AC_USE_SYSTEM_EXTENSIONS
AC_PROG_CC
AC_PROG_CC_C99
AM_PROG_AR
AC_PROG_LIBTOOL

AC_C_RESTRICT
AC_C_INLINE
AC_C_CONST

## Amino ##
PKG_CHECK_MODULES([AMINO],
                  [amino],
                  [HAVE_AMINO=yes])
CFLAGS="$CFLAGS $AMINO_CFLAGS"
LDFLAGS="$LDFLAGS $AMINO_LDFLAGS"

## SNS ##
PKG_CHECK_MODULES([SNS],
		  [sns],
		  [HAVE_SNS=yes])
CFLAGS="$CFLAGS $SNS_CFLAGS"

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
