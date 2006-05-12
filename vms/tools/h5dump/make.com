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
$! Make h5dump tool 
$!
$! ccopt = "/float=ieee_float"
$
$ ccc := cc 'ccopt /include=([-.-.src], [-.lib])
$ 
$
$ cobj= "h5dump.c, h5dumpgentest.c "

$                               
$ ccc 'cobj 
$ type sys$input
       Creating h5dumpgentest
$ link     h5dumpgentest, -
           [-.lib]libh5tools.olb/lib,[-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib 
$ type sys$input
 	Created  h5dumpgentest
$
$ type sys$input
       Creating h5dump
$ link     h5dump, -
           [-.lib]libh5tools.olb/lib,[-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
	Created  h5dump
$!
$ exit
