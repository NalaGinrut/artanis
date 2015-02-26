#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT([Artanis], 
	m4_esyscmd([build-aux/git-version-gen .tarball-version]),
	[bug-artanis@gnu.org])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR(ARTANIS-VERSION)
AC_CONFIG_HEADERS([config.h])

. $srcdir/ARTANIS-VERSION
ARTANIS_VERSION="$PACKAGE_VERSION"

# Checks for programs.
AC_PROG_CXX
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h malloc.h stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_FORK
AC_CHECK_FUNCS([memchr mkdir])

# Check for Guile
GUILE_PKG([2.2 2.0])
PKG_CHECK_MODULES([GUILE], [guile-$GUILE_EFFECTIVE_VERSION >= 2.0.11])
AC_PATH_PROG([GUILECONFIG], [guile-config], :)

GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

# Check guile-dbi
AC_CHECK_LIB([guile-dbi], [init_db_handle_type], ,[AC_MSG_ERROR(No guile-dbi was found. Maybe you haven't installed it? http://home.gna.org/guile-dbi/)])

LT_INIT([dlopen disable-fast-install disable-static win32-dll])

AC_CONFIG_FILES([Makefile bin/art])
AC_OUTPUT
