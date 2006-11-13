$!#
$!# Copyright by the Board of Trustees of the University of Illinois.
$!# All rights reserved.
$!#
$!# This file is part of HDF5.  The full HDF5 copyright notice, including
$!# terms governing use, modification, and redistribution, is contained in
$!# the files COPYING and Copyright.html.  COPYING can be found at the root
$!# of the source code distribution tree; Copyright.html can be found at the
$!# root level of an installed copy of the electronic HDF5 document set and
$!# is linked from the top-level documents page.  It can also be found at
$!# http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
$!# access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.
$!#
$! Makefile for VMS systems.
$!
$! Make h5repack tool 
$!
$! ccopt = "/float=ieee_float"
$
$ ccc := cc 'ccopt /include=([-.-.src], [-.lib], [-.-.test])
$ type sys$input
	Creating h5repack
$!
$ cobj= " h5repack_main, h5repack, h5repack_copy, h5repack_filters, " +-
        "h5repack_list, h5repack_opttable, h5repack_parse, h5repack_refs," +-
        "h5repack_verify,"+-
        "testh5repack_main, testh5repack_attr, testh5repack_dset, "+-
        "testh5repack_make, testh5repack_util "

$!                               
$ ccc 'cobj 
$ type sys$input
       Creating h5repack
$ link/exe=h5repack.exe -
           h5repack_main, h5repack, h5repack_copy, h5repack_filters, -
           h5repack_list, h5repack_opttable, h5repack_parse, -
           h5repack_verify, -
           [-.lib]libh5tools.olb/lib,[-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib 
$ type sys$input
	Created  h5repacktst
$
$ type sys$input
       Creating h5repacktst
$ link/exe=h5repacktst.exe -
           testh5repack_main, testh5repack_attr, testh5repack_dset, -
           testh5repack_make, testh5repack_util, -
           h5repack, h5repack_copy, h5repack_filters, -
           h5repack_list, h5repack_opttable, h5repack_parse, -
           h5repack_verify, -
           [-.lib]libh5tools.olb/lib,[-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
!               sys$clib/lib
$ type sys$input
	Created  h5repacktst
$!
$ exit
