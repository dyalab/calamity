#                                               -*Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT([point_gen], [1.0], [mschack@mines.edu])
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

dnl AC_SEARCH_LIBS([pthread_create],[pthread])
dnl AC_SEARCH_LIBS([clock_gettime],[rt])
dnl AC_SEARCH_LIBS([timer_settime],[rt])
dnl AC_SEARCH_LIBS([dlopen],[dl])

dnl AC_SEARCH_LIBS([dgemm_],[openblas blas])
dnl AC_SEARCH_LIBS([dgesvd_],[lapack])

# Enable maximum warnings
m4_ifdef([AX_CFLAGS_WARN_ALL],
         [AX_CFLAGS_WARN_ALL])

dnl The AX_CHECK_COMPILER_FLAGS maco was renamed to AX_CHECK_COMPILE_FLAG
dnl Checking for both versions
m4_ifdef([AX_CHECK_COMPILE_FLAG],
         [AC_DEFUN([APPEND_FLAG],
                   [AX_CHECK_COMPILE_FLAG([$1], [CFLAGS="$1 $CFLAGS"])])])

m4_ifdef([AX_CHECK_COMPILER_FLAGS],
         [AC_DEFUN([APPEND_FLAG],
                   [AX_CHECK_COMPILER_FLAGS([$1], [CFLAGS="$1 $CFLAGS"])])])

dnl If we found the flag checking macro, check some flags
m4_ifdef([APPEND_FLAG],
         [APPEND_FLAG([-Wextra])
          APPEND_FLAG([-Wconversion])
          APPEND_FLAG([-Wpointer-arith])
          APPEND_FLAG([-Wfloat-equal])
          APPEND_FLAG([-Wshadow])
          APPEND_FLAG([-Wwrite-strings])
          APPEND_FLAG([-Wc++-compat])])



## TMkit ##
PKG_CHECK_MODULES([AMINO],
		  [amino],
		  [HAVE_AMINO=yes])

CFLAGS="$CFLAGS $AMINO_CFLAGS"
LDFLAGS="$LDFLAGS $AMINO_CFLAGS"


AC_CONFIG_FILES([Makefile])

AC_OUTPUT

AC_MSG_NOTICE([])
AC_MSG_NOTICE([CONFIGURATION SUMMARY])
AC_MSG_NOTICE([=====================])
AC_MSG_NOTICE([PREFIX:          $prefix])
AC_MSG_NOTICE([C FLAGS:  	$CFLAGS])
