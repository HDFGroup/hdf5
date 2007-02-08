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
$!
$! h5ls testing script
$!
$ define sys$output h5ls.out
$ define sys$error  h5ls.err
$ h5ls :== $sys$sysusers:[pourmale.hdf5.tools.h5ls]h5ls.exe

$ h5ls -w80 -h
$ h5ls -w80 -help
$ h5ls -w80 -?

$! test simple command
$ h5ls -w80 tall.h5
$ h5ls -w80 -r -d tall.h5
$ h5ls -w80 tgroup.h5

$! test for displaying groups
$ h5ls -w80 -r -g tgroup.h5

$! test for displaying simple space datasets
$ h5ls -w80 -r -d tdset.h5

$! test for displaying soft links
$ h5ls -w80 -r tslink.h5

$! tests for hard links
$ h5ls -w80 thlink.h5

$! tests for compound data types
$ h5ls -w80 -r -d tcompound.h5

$!test for the nested compound type
$ h5ls -w80 -r -d tnestedcomp.h5

$! test for loop detection
$ h5ls -w80 -r -d tloop.h5

$! test for string 
$ h5ls -w80 -r -d tstr.h5

$! test test file created from lib SAF team
$ h5ls -w80 -r -d tsaf.h5

$! test for variable length data types
$ h5ls -w80 -r -d tvldtypes1.h5

$! test for array data types
$ h5ls -w80 -r -d tarray1.h5

$! test for empty data
$ h5ls -w80 -d tempty.h5

$! test for all dataset types written to attributes
$! enable -S for avoiding printing NATIVE types
$ h5ls -w80 -v -S tattr2.h5

