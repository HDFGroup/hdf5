#							-*- shell-script -*-

# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from help@hdfgroup.org.

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
    CPPFLAGS="$CPPFLAGS -LANG:std -ptused"

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
