#							-*- shell-script -*-
#
# This file is part of the HDF5 build script.  It is processed shortly
# after configure starts and defines, among other things, flags for
# the various compile modes.
#
# See BlankForm in this directory for details

# The default compiler is `MIPSpro CC'
if test -z "$CXX"; then
    CXX=CC
    CXX_BASENAME=CC
fi

# Try native compiler flags
if test -z "$cxx_flags_set"; then
# -LANG:std required for std use; -ptused causes templates used to be
# instantiated
    CPPFLAGS="-LANG:std -ptused"

# libCio is a default library, since libtool before 1.5 doesn't fully 
# support C++ yet, default libraries must be explicitly specified.
# A new macro is used for this temporary and specific task so it 
# won't polute the existing configuration 
    DEFAULT_LIBS="-lCio"

    DEBUG_CXXFLAGS=-g
    DEBUG_CPPFLAGS=
    PROD_CXXFLAGS="-O -s"
    PROD_CPPFLAGS=
    PROFILE_CXXFLAGS=-xpg
    PROFILE_CPPFLAGS=
    cxx_flags_set=yes
fi
