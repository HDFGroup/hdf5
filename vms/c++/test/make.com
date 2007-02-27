$!#
$!# Copyright by The HDF Group.
$!# Copyright by the Board of Trustees of the University of Illinois.
$!# All rights reserved.
$!#
$!# This file is part of HDF5.  The full HDF5 copyright notice, including
$!# terms governing use, modification, and redistribution, is contained in
$!# the files COPYING and Copyright.html.  COPYING can be found at the root
$!# of the source code distribution tree; Copyright.html can be found at the
$!# root level of an installed copy of the electronic HDF5 document set and
$!# is linked from the top-level documents page.  It can also be found at
$!# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
$!# access to either file, you may request a copy from help@hdfgroup.org.
$!#
$! Makefile for VMS systems.
$!
$! Make HDF5 C++ library tests
$!
$! cxxopt = "/float=ieee_float/standard=strict_ansi/define=H5_VMS"
$
$ ccc := cxx 'cxxopt /include=([-.-.src], [-.-.test], [-.src])
$! 
$!
$!                               
$ ccc tattr
$ ccc tfile
$ ccc tcompound
$ ccc tfilter
$ ccc trefer
$ ccc ttypes
$ ccc tvlstr
$ ccc th5s
$ ccc h5cpputil
$ ccc testhdf5 
$ type sys$input
       Creating testhdf5
$ cxxlink  testhdf5,tattr,tfile, th5s, -
           tcompound, tfilter, trefer, tvlstr, ttypes, h5cpputil, -
           [-.src]hdf5_cplus.olb/lib, [-.-.test]libh5test.olb/lib, -
           [-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ !                              
$ ccc dsets
$ type sys$input
        Creating dsets
$ cxxlink  dsets, h5cpputil, -
           [-.src]hdf5_cplus.olb/lib, [-.-.test]libh5test.olb/lib, -
           [-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
 	Done
$ exit
