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


if test -z "$CXX"; then
    CXX=cxx
    CXX_BASENAME=cxx
fi


case $CXX_BASENAME in
    g++)
	CXXFLAGS="$CXXFLAGS -Wsign-compare" #Only works for some versions
	DEBUG_CXXFLAGS="-g -fverbose-asm"
	DEBUG_CPPFLAGS=
	PROD_CXXFLAGS="-O3 -fomit-frame-pointer"
	PROD_CPPFLAGS=
	PROFILE_CXXFLAGS="-pg"
	PROFILE_CPPFLAGS=
	;;

    *)
	CXXFLAGS="$CXXFLAGS -tlocal -D__USE_STD_IOSTREAM "
	DEBUG_CXXFLAGS="-g"
	DEBUG_CPPFLAGS=
	PROD_CXXFLAGS="-O"
	PROD_CPPFLAGS=
	PROFILE_CXXFLAGS="-pg"
	PROFILE_CPPFLAGS=
	;;
esac

