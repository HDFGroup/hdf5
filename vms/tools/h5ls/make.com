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
$! Make h5ls tool 
$!
$ ccopt = "/float=ieee_float"
$
$ ccc := cc 'ccopt /debug/define=H5_VMS/include=([-.-.src], [-.lib])
$ type sys$input
	Creating h5ls
$!
$ cobj= "h5ls.c "

$!                               
$ ccc 'cobj 
$
$ type sys$input
       Creating h5ls
$ link     h5ls, -
           [-.lib]libh5tools.olb/lib,[-.-.src]hdf5.olb/lib
!               sys$clib/lib
$ type sys$input
	Created  h5ls
$!
