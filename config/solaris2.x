#							-*- shell-script -*-
#
# This file is part of the HDF5 build script.  It is processed shortly
# after configure starts and defines, among other things, flags for
# the various compile modes.
#
# See BlankForm in this directory for details

# The default compiler is `sunpro cc'
if test "X-" =  "X-$CC"; then
  CC=cc
  CC_BASENAME=cc
fi

# Try gcc compiler flags
. $srcdir/config/gnu-flags

# Try solaris native compiler flags
if test "X-" = "X-$cc_flags_set"; then
  CFLAGS="$CFLAGS -erroff=%none -DBSD_COMP"
  DEBUG_CFLAGS="-g -xildoff"
  DEBUG_CPPFLAGS=
  PROD_CFLAGS="-O -s"
  PROD_CPPFLAGS=
  PROFILE_CFLAGS=-xpg
  PROFILE_CPPFLAGS=
  cc_flags_set=yes
# Special linking flag is needed to build with Fortran on Solaris 5.9
    system_version="`uname -r`"
    case "$system_version" in
	5.9*)
	    LDFLAGS="$LDFLAGS -xopenmp=stubs"
        ;;
    esac

  # Turn off optimization flag for SUNpro compiler versions 4.x which
  # have an optimization bug.  Version 5.0 works.
  ($CC -V 2>&1) | grep -s 'cc: .* C 4\.' >/dev/null 2>&1 \
       && PROD_CFLAGS="`echo $PROD_CFLAGS | sed -e 's/-O//'`"
fi

# Add socket lib for the Stream Virtual File Driver
LIBS="$LIBS -lsocket"

# The default Fortran 90 compiler

#
# HDF5 integers
#
# 	R_LARGE is the number of digits for the bigest integer supported.
#	R_INTEGER is the number of digits in INTEGER
#
# (for the Sparc Solaris architechture)
#
R_LARGE=18
R_INTEGER=9
HADDR_T='SELECTED_INT_KIND(R_LARGE)'
HSIZE_T='SELECTED_INT_KIND(R_LARGE)'
HSSIZE_T='SELECTED_INT_KIND(R_LARGE)'
HID_T='SELECTED_INT_KIND(R_INTEGER)'
SIZE_T='SELECTED_INT_KIND(R_INTEGER)'
OBJECT_NAMELEN_DEFAULT_F=-1

if test "X-" = "X-$FC"; then
  FC=f90
fi

if test "X-" = "X-$f9x_flags_set"; then
  F9XSUFFIXFLAG=""
  FSEARCH_DIRS=""
  FCFLAGS="$FCFLAGS"
  DEBUG_FCFLAGS=""
  PROD_FCFLAGS=""
  PROFILE_FCFLAGS=""
  f9x_flags_set=yes
fi

# The default C++ compiler

# The default compiler is `sunpro cc'
if test -z "$CXX"; then
  CXX=CC
  CXX_BASENAME=CC
fi

# Try gcc compiler flags
#. $srcdir/config/gnu-flags

cxx_version="`$CXX -V 2>&1 |grep 'WorkShop' |\
                sed 's/.*WorkShop.*C++ \([0-9\.]*\).*/\1/'`"

cxx_vers_major=`echo $cxx_version | cut -f1 -d.`
cxx_vers_minor=`echo $cxx_version | cut -f2 -d.`
cxx_vers_patch=`echo $cxx_version | cut -f3 -d.`

# Specify the "-features=tmplife" if the compiler can handle this...
if test -n "$cxx_version"; then
  if test $cxx_vers_major -ge 5 -a $cxx_vers_minor -ge 3 -o $cxx_vers_major -gt 5; then
    CXXFLAGS="$CXXFLAGS -features=tmplife"
  fi
fi

# Try solaris native compiler flags
if test -z "$cxx_flags_set"; then
  CXXFLAGS="$CXXFLAGS -instances=static"
  CPPFLAGS="$CPPFLAGS -LANG:std"
  DEBUG_CXXFLAGS=-g
  DEBUG_CPPFLAGS=
  PROD_CXXFLAGS="-O -s"
  PROD_CPPFLAGS=
  PROFILE_CXXFLAGS=-xpg
  PROFILE_CPPFLAGS=
  cxx_flags_set=yes
fi
