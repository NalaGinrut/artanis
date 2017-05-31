#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT([Artanis], 
	m4_esyscmd([build-aux/git-version-gen .tarball-version]),
	[bug-artanis@gnu.org])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR(.ARTANIS-VERSION)
AC_CONFIG_HEADERS([config.h])

. $srcdir/.ARTANIS-VERSION
ARTANIS_VERSION="$PACKAGE_VERSION"

# Checks for programs.
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

# Check for Guile
GUILE_PKG([2.2])

GUILE_PROGS([2.2])
GUILE_SITE_DIR([2.2])

AC_CONFIG_FILES([Makefile bin/art])
AC_OUTPUT
