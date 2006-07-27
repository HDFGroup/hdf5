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
$!
$ !
$ ! This command file tests h5repack utility. The command file has to
$ ! run in the [hdf5-top.tools.testfiles] directory.
$ !
$ !
$ ! Define h5repack and h5diff symbols
$ !
$! set message/notext/nofacility/noidentification/noseverity
$ current_dir = F$DIRECTRY()
$ len = F$LENGTH(current_dir)
$ temp = F$EXTRACT(0, len-10, current_dir)
$ h5diff_dir = temp + "H5DIFF]"
$ h5diff :== $sys$disk:'h5diff_dir'h5diff.exe
$ h5repack_dir = temp + "H5REPACK]"
$ h5repack :== $sys$disk:'h5repack_dir'h5repack.exe
$ !
$ !
$ ! h5repack tests
$ !
$

$!# copy files (these files have no filters) 
$ CALL TOOLTEST test0.h5
$ CALL TOOLTEST test1.h5
$ CALL TOOLTEST test2.h5
$ CALL TOOLTEST test3.h5
$ CALL TOOLTEST test4.h5
$ CALL TOOLTEST test5.h5
$ 
$ # use test4.h5 to write some filters  (this file has  no filters)
$ 
$! # gzip with individual object
$ CALL TOOLTEST test4.h5 "-f dset1:""GZIP""=1  -l dset1:""CHUNK""=20x10"
$   
$! # gzip for all 
$ CALL TOOLTEST test4.h5 "-f ""GZIP""=1"
$! 
$! # shuffle with individual object
$ CALL TOOLTEST test4.h5 "-f dset2:""SHUF""  -l dset2:""CHUNK""=20x10"
$!   
$! 
$! # shuffle for all
$ CALL TOOLTEST test4.h5 "-f """HUF"""
$!   
$! # fletcher32  with individual object
$ CALL TOOLTEST test4.h5 "-f dset2:""FLET""  -l dset2:""CHUNK""=20x10"
$! 
$! # fletcher32 for all
$ CALL TOOLTEST test4.h5 "-f ""FLET"""
$ 
$! ###########################################################
$! # the following tests assume the input files have filters
$! ###########################################################
$! 
$! # deflate copy
$ CALL TOOLTEST test_deflate.h5
$! 
$! # deflate remove
$ CALL TOOLTEST test_deflate.h5 "-f dset_deflate:""NONE"""
$!     
$! # shuffle copy
$ CALL TOOLTEST test_shuffle.h5
$! 
$! # shuffle remove
$ CALL TOOLTEST test_shuffle.h5 "-f dset_shuffle:""NONE"""
$! 
$! # fletcher32 copy
$ CALL TOOLTEST test_fletcher32.h5
$! 
$! # fletcher32 remove
$ CALL TOOLTEST test_fletcher32.h5 "-f dset_fletcher32:""NONE"""
$! 
$! # nbit copy
$ CALL TOOLTEST test_nbit.h5
$! 
$! # nbit remove
$ CALL TOOLTEST test_nbit.h5 "-f dset_nbit:""NONE"""
$! 
$! # nbit add
$ CALL TOOLTEST test_nbit.h5 "-f dset_int31:""NBIT"""
$! 
$! # scaleoffset add
$ CALL TOOLTEST test_scaleoffset.h5 "-f dset_none:""S""+""O""=31"
$! 
$! # scaleoffset copy
$ CALL TOOLTEST test_scaleoffset.h5
$! 
$! # scaleoffset remove
$ CALL TOOLTEST test_scaleoffset.h5 "-f dset_scaleoffset:""NONE"""
$! 
$! #limit
$ CALL TOOLTEST test4.h5 "-f ""GZIP""=1 -m 1024"
$! 
$! #file
$ CALL TOOLTEST test4.h5 "-e h5repack_info.txt"
$! 
$! 
$! #########################################################
$! # layout options (these files have no filters)
$! #########################################################
$!
$ CALL TOOLTEST test4.h5 "-l dset2:""CHUNK""=20x10"
$ CALL TOOLTEST test4.h5 "-l """HUNK""=20x10"
$ CALL TOOLTEST test4.h5 "-l dset2:""CONTI"""
$ CALL TOOLTEST test4.h5 "-l ""CONTI"""
$ CALL TOOLTEST test4.h5 "-l dset2:""COMPA"""
$ CALL TOOLTEST test4.h5 "-l ""COMPA"""
$! 
$! 
$! ################################################################
$! # layout conversions (file has no filters)
$! ###############################################################
$! 
$ CALL TOOLTEST test4.h5 "-l dset_compact:""CONTI"""
$ CALL TOOLTEST test4.h5 "-l dset_compact:""CHUNK""=2x5" 
$ CALL TOOLTEST test4.h5 "-l dset_compact:""COMPA"""
$ CALL TOOLTEST test4.h5 "-l dset_contiguous:""COMPA"""
$ CALL TOOLTEST test4.h5 "-l dset_contiguous:""CHUNK""=3x6"
$ CALL TOOLTEST test4.h5 "-l dset_contiguous:""CONTI"""
$ CALL TOOLTEST test4.h5 "-l dset_chunk:""COMPA"""
$ CALL TOOLTEST test4.h5 "-l dset_chunk:""CONTI"""
$ CALL TOOLTEST test4.h5 "-l dset_chunk:""CHUNK""=18x13"
$!
$!
$TOOLTEST: SUBROUTINE

$ len =  F$LENGTH(P1)
$ base = F$EXTRACT(0,len-3,P1)
$ output = base + "out.h5"
$ output_err = base + ".err"
$
$ begin = "Testing h5repack"
$ !
$ ! Run the test and save output in the 'actual' file
$ !
$ define/nolog sys$error  'output_err'
$ ON ERROR THEN CONTINUE
$ h5repack -i 'P1 -o 'output' 'P2
$ h5diff 'P1 'output'
$ deassign sys$error
$ if F$SEARCH(output_err) .NES. ""
$ then
$    result = "PASSED"
$    line = F$FAO("!16AS !20AS !43AS !70AS", begin, P1, P2, result) 
$  else
$    result = "*FAILED*"
$    line = F$FAO("!16AS !20AS !42AS !69AS", begin, P1, P2, result) 
$ endif
$ !
$ ! Print test result
$ ! 
$  write sys$output line
$ ! 
$ del *out.h5;*
$ !
$ENDSUBROUTINE

