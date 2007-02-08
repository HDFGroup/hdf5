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
$ !
$ ! This command file tests h5copy utility. The command file has to
$ ! run in the [hdf5-top.tools.testfiles] directory.
$ !
$ !
$ ! Define h5copy symbols
$ !
$ current_dir = F$DIRECTRY()
$ len = F$LENGTH(current_dir)
$ temp = F$EXTRACT(0, len-10, current_dir)
$ h5copy_dir = temp + "H5COPY]"
$ h5copy :== $sys$disk:'h5copy_dir'h5copy.exe
$ !
$ !
$ ! h5copy tests
$ !
$

$ !# copy files 
$ CALL TOOLTEST "test1.h5/array test1_out.h5/array"
$ CALL TOOLTEST "test1.h5/integer test1_out.h5/integer_copy"
$ CALL TOOLTEST "test1.h5/g1 test1_out.h5/g1"
$ !
$ !
$TOOLTEST: SUBROUTINE

$
$ begin = "Testing h5copy"
$ !
$ ! Run the test and save output in the 'actual' file
$ !
$ define/nolog sys$error  h5copy_temp.err
$ ON ERROR THEN CONTINUE
$ h5copy 'P1 
$ deassign sys$error
$ if F$SEARCH("h5copy_temp.err") .EQS. "" 
$ then
$    result = "PASSED"
$    line = F$FAO("!16AS !53AS !70AS", begin, P1, result) 
$ else
$    result = "*FAILED*"
$    line = F$FAO("!16AS !52AS !69AS", begin, P1, result) 
$ endif
$ !
$ ! Delete error file if any print test result
$ !
$ if F$SEARCH ("*.err;*") .NES. ""
$ then
$     del *.err;*
$ endif
$ !
$  write sys$output line
$ ! 
$ del *out.h5;*
$ !
$ENDSUBROUTINE

