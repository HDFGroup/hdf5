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
$ !
$ ! This command file tests h5import utility. The command file has to
$ ! run in the [hdf5-top.tools.h5import.testfiles] directory.
$ !
$ !
$ ! Define symbols
$ !
$ current_dir = F$DIRECTRY()
$ len = F$LENGTH(current_dir)
$ temp = F$EXTRACT(0, len-19, current_dir)
$ h5dump_dir = temp + "H5DUMP]"
$ h5dump :== $sys$disk:'h5dump_dir'h5dump.exe
$ h5import_dir = temp + "H5IMPORT]"
$ h5import :== $sys$disk:'h5import_dir'h5import.exe
$ h5importtest :== $sys$disk:'h5import_dir'h5importtest.exe
$ !
$ ! Define output for diff command that compares expected and actual
$ ! outputs of h5dump
$ !
$ create h5dump.log
$ !
$ ! h5import tests
$ !
$ ! Run h5importtest to create input files
$ !
$ h5importtest
$ CALL TOOLTEST "ASCII I32 rank 3 - Output BE " -
       "txtin32 -c textin32 -o" test1.h5
$ !
$ CALL TOOLTEST  "ASCII I16 rank 3 - Output LE - CHUNKED - extended" - 
       "txtin16 -c textin16 -o" test2.h5
$ !
$ CALL TOOLTEST - 
       "ASCII I8 - rank 3 - Output I16 LE-Chunked+Extended+Compressed " -
       "txtin16 -c textin8  -o" test3.h5
$ !
$ CALL TOOLTEST  "ASCII UI32 - rank 3 - Output BE " - 
       "in1 -c textuin32 -o" test4.h5
$ !
$ CALL TOOLTEST  "ASCII UI16 - rank 2 - Output LE+Chunked+Compressed " -
       "in1 -c textuin16 -o" test5.h5
$ !
$ CALL TOOLTEST  "ASCII F32 - rank 3 - Output LE " -
       "fp1 -c textfp32 -o" test6.h5
$ !
$ CALL TOOLTEST - 
       "ASCII F64 - rank 3 - Output BE + CHUNKED+Extended+Compressed " -
       "fp2 -c textfp64 -o" test7.h5
$ !
$ CALL TOOLTEST - 
       "BINARY F64 - rank 3 - Output LE+CHUNKED+Extended+Compressed " -
       "bfp64 -c conbfp64 -o" test8.h5
$ !
$ CALL TOOLTEST  "BINARY I16 - rank 3 - Output order LE + CHUNKED + extended " -
       "bin16 -c conbin16 -o" test9.h5
$ !
$ CALL TOOLTEST  -
       "BINARY I8 - rank 3 - Output I16LE + Chunked+Extended+Compressed " -
       "bin8 -c conbin8  -o" test10.h5
$ !
$ CALL TOOLTEST  "BINARY I32 - rank 3 - Output BE + CHUNKED " -
       "bin32 -c conbin32 -o" test11.h5
$ !
$ CALL TOOLTEST  "BINARY UI16 - rank 3 - Output byte BE + CHUNKED " -
       "buin16 -c conbuin16 -o" test12.h5
$ !
$ CALL TOOLTEST  "BINARY UI32 - rank 3 - Output LE + CHUNKED " -
       "buin32 -c conbuin32 -o" test13.h5
$
$
$ 
$TOOLTEST: SUBROUTINE
$
$ len =  F$LENGTH(P3)
$ base = F$EXTRACT(0,len-3,P3)
$ actual = base + "out.h5"
$ actual_dump = base + "out.txt"
$ actual_dump_err = base + "out.err"
$ expected_dump = base + ".txt"
$ expected_dump_err = base + ".err"
$
$ begin = "Testing"
$ !
$ ! Run h5import with output in the 'actual' file
$ !
$ ON ERROR THEN CONTINUE
$ h5import 'P2 'actual'
$ define/nolog sys$output 'actual_dump'
$ define/nolog sys$error  'actual_dump_err'
$ !
$ ! Dump the actual and expected files
$ !
$ h5dump 'actual'
$ deassign sys$output
$ deassign sys$error
$ if F$SEARCH(actual_dump_err) .NES. ""
$ then
$ set message/notext/nofacility/noidentification/noseverity
$    append 'actual_dump_err' 'actual_dump'
$ set message/ntext/facility/identification/severity
$ endif
$ define/nolog sys$output 'expected_dump'
$ define/nolog sys$error  'expected_dump_err'
$ h5dump 'P3
$ deassign sys$output
$ deassign sys$error
$ if F$SEARCH(expected_dump_err) .NES. ""
$ then
$ set message/notext/nofacility/noidentification/noseverity
$    append 'expected_dump_err' 'expected_dump'
$ set message/ntext/facility/identification/severity
$ endif
$ !
$ ! Compare the results
$ !
$ diff/output=h5dump_temp/ignore=(spacing,trailing_spaces,blank_lines)-
                                'actual_dump' -
                                'expected_dump'
$ open/read temp_out h5dump_temp.dif
$ !
$READ_DATA:
$ read/end_of_file=end_read temp_out record1
$ !
$ ! Skip blank lines
$ !
$ if record1 .EQS. "" then goto READ_DATA
$ !
$ ! Find record with "Number" and exit the loop
$ !
$ len = F$LENGTH(record1)
$ pos = F$LOCATE("Number", record1)
$ !
$ if pos .EQ. 0  
$ then 
$     err_code = F$EXTRACT(len-1,1,record1)
$     goto END_READ
$ endif
$ !
$ goto READ_DATA
$
$ !
$END_READ: 
$ close temp_out
$ !
$ ! File names are different, so we allow only one difference
$ ! in h5dump 
$ if err_code .eqs. "1" 
$  then
$    result = "PASSED"
$    line = F$FAO("!8AS !62AS !80AS", begin, P1, result) 
$  else
$    result = "*FAILED*"
$    line = F$FAO("!8AS !61AS !79AS", begin, P1, result) 
$ endif
$ !
$ ! Print test result
$ ! 
$  write sys$output line
$ ! 
$ ! Append the result to the log file 
$ !
$ append h5dump_temp.dif h5dump.log
$ !
$ ! Delete temporary files
$ if F$SEARCH("*out.h5;*") then del *out.h5;*
$ if F$SEARCH("*.dif;*")   then del *.dif;*
$ if F$SEARCH("*.err;*")   then del *.err;*
$ !
$ !
$ENDSUBROUTINE

