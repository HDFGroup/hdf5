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
$ ! This command file tests h5diff utility. The command file has to
$ ! run in the [hdf5-top.tools.testfiles] directory.
$ !
$ !
$ ! Define h5diff symbol
$ !
$! set message/notext/nofacility/noidentification/noseverity
$ current_dir = F$DIRECTRY()
$ len = F$LENGTH(current_dir)
$ temp = F$EXTRACT(0, len-10, current_dir)
$ h5diff_dir = temp + "H5DIFF]"
$ h5diff :== $sys$disk:'h5diff_dir'h5diff.exe
$ !
$ ! Define output for diff command that compares expected and actual
$ ! outputs of h5diff
$ !
$ create h5diff.log
$ !
$ ! h5diff tests
$ !
$

$!# 1.0
$ CALL TOOLTEST h5diff_10.txt "-h"
$!
$!# 1.1 normal mode
$ CALL TOOLTEST h5diff_11.txt  "file1.h5 file2.h5" 
$!
$!# 1.2 normal mode with objects
$ CALL TOOLTEST h5diff_12.txt  "file1.h5 file2.h5  g1/dset1 g1/dset2"
$!
$!# 1.3 report mode
$ CALL TOOLTEST h5diff_13.txt "file1.h5 file2.h5 -r"
$!
$!# 1.4 report  mode with objects
$ CALL TOOLTEST h5diff_14.txt  "file1.h5 file2.h5  -r g1/dset1 g1/dset2"
$!
$!# 1.5 with -d
$ CALL TOOLTEST h5diff_15.txt "file1.h5 file2.h5 -r -d 5 g1/dset3 g1/dset4"
$!
$!# 1.6 with -p
$ CALL TOOLTEST h5diff_16.txt "file1.h5 file2.h5 -r -p 0.05 g1/dset3 g1/dset4"
$!
$!# 1.7 verbose mode
$ CALL TOOLTEST h5diff_17.txt "file1.h5 file2.h5 -v"  
$!
$!# 1.8 quiet mode 
$ CALL TOOLTEST h5diff_18.txt "file1.h5 file2.h5 -q"
$!
$!# 1.9.1 with -p (int)
$ CALL TOOLTEST h5diff_191.txt "file1.h5 file1.h5 -v -p 0.02 g1/dset5 g1/dset6"
$!
$!# 1.9.2 with -p (unsigned long_long)
$ CALL TOOLTEST h5diff_192.txt "file1.h5 file1.h5 -v -p 0.02 g1/dset7 g1/dset8"
$!
$!
$!# ##############################################################################
$!# # not comparable types
$!# ##############################################################################
$!
$!# 2.0
$ CALL TOOLTEST h5diff_20.txt "file3.h5 file3.h5 -v dset g1"
$
$!# 2.1
$ CALL TOOLTEST h5diff_21.txt "file3.h5 file3.h5 -v dset l1"
$!
$!# 2.2
$ CALL TOOLTEST h5diff_22.txt "file3.h5 file3.h5 -v dset t1"
$!
$!# ##############################################################################
$!# # compare groups, types, links (no differences and differences)
$!# ##############################################################################
$!
$!# 2.3
$ CALL TOOLTEST h5diff_23.txt "file3.h5 file3.h5 -v g1 g1"
$!
$!# 2.4
$ CALL TOOLTEST h5diff_24.txt "file3.h5 file3.h5 -v t1 t1"
$!
$!# 2.5
$ CALL TOOLTEST h5diff_25.txt "file3.h5 file3.h5 -v l1 l1" 
$!
$!# 2.6
$ CALL TOOLTEST h5diff_26.txt "file3.h5 file3.h5 -v g1 g2"
$!
$!# 2.7
$ CALL TOOLTEST h5diff_27.txt "file3.h5 file3.h5 -v t1 t2"
$!
$!# 2.8
$ CALL TOOLTEST h5diff_28.txt "file3.h5 file3.h5 -v l1 l2"
$!
$!
$!
$!# ##############################################################################
$!# # Dataset types
$!# ##############################################################################
$
$!# 5.0
$ CALL TOOLTEST h5diff_50.txt "file4.h5 file4.h5 -v dset0a dset0b"
$!
$!# 5.1
$ CALL TOOLTEST h5diff_51.txt "file4.h5 file4.h5 -v dset1a dset1b"
$!
$!# 5.2
$ CALL TOOLTEST h5diff_52.txt "file4.h5 file4.h5 -v dset2a dset2b"
$!
$!# 5.3
$ CALL TOOLTEST h5diff_53.txt "file4.h5 file4.h5 -v dset3a dset4b"
$!
$!# 5.4
$ CALL TOOLTEST h5diff_54.txt "file4.h5 file4.h5 -v dset4a dset4b"
$!
$!# 5.5
$ CALL TOOLTEST h5diff_55.txt "file4.h5 file4.h5 -v dset5a dset5b"
$!
$!# 5.6
$ CALL TOOLTEST h5diff_56.txt "file4.h5 file4.h5 -v dset6a dset6b"
$!
$!# 5.7
$ CALL TOOLTEST h5diff_57.txt "file4.h5 file4.h5 -v dset7a dset7b"
$!
$#! 5.8 (region reference)
$ CALL TOOLTEST h5diff_58.txt "file7.h5 file8.h5 -v refreg"
$!
$!# ##############################################################################
$!# # Error messages
$!# ##############################################################################
$!
$!
$!# 6.0: Check if the command line number of arguments is less than 3
$ CALL TOOLTEST h5diff_600.txt "file1.h5" 
$!
$!# 6.1: Check for invalid options
$ CALL TOOLTEST h5diff_601.txt "file1.h5 file2.h5 -x" 
$!
$!# ##############################################################################
$!# # -d 
$!# ##############################################################################
$!
$!# 6.2: no value
$ CALL TOOLTEST h5diff_602.txt "file1.h5 file2.h5  -d g1/dset3 g1/dset4"
$!
$!# 6.3: negative value
$ CALL TOOLTEST h5diff_603.txt "file1.h5 file2.h5  -d -4 g1/dset3 g1/dset4"
$!
$!# 6.4: zero
$ CALL TOOLTEST h5diff_604.txt "file1.h5 file2.h5  -d 0 g1/dset3 g1/dset4"
$!
$!# 6.5: non number
$ CALL TOOLTEST h5diff_605.txt "file1.h5 file2.h5  -d u g1/dset3 g1/dset4"
$!
$!# 6.6: hexadecimal
$ CALL TOOLTEST h5diff_606.txt "file1.h5 file2.h5 -d 0x1 g1/dset3 g1/dset4"
$!
$!# 6.7: string
$ CALL TOOLTEST h5diff_607.txt "file1.h5 file2.h5  -d "1" g1/dset3 g1/dset4"
$!
$!# 6.8: repeated option
$ CALL TOOLTEST h5diff_608.txt "file1.h5 file2.h5  -d 1 -d 2 g1/dset3 g1/dset4"
$!
$!# 6.9: number larger than biggest difference
$ CALL TOOLTEST h5diff_609.txt "file1.h5 file2.h5  -d 200 g1/dset3 g1/dset4"
$!
$!# 6.10: number smaller than smallest difference
$ CALL TOOLTEST h5diff_610.txt "file1.h5 file2.h5  -d 1 g1/dset3 g1/dset4"
$!
$!
$!# ##############################################################################
$!# # -p
$!# ##############################################################################
$!
$!
$!# 6.11: no value
$ CALL TOOLTEST h5diff_611.txt "file1.h5 file2.h5 -r -p g1/dset3 g1/dset4"
$!
$!# 6.12: negative value
$ CALL TOOLTEST h5diff_612.txt "file1.h5 file2.h5 -p -4 g1/dset3 g1/dset4"
$!
$!# 6.13: zero
$ CALL TOOLTEST h5diff_613.txt "file1.h5 file2.h5 -p 0 g1/dset3 g1/dset4"
$!
$!# 6.14: non number
$ CALL TOOLTEST h5diff_614.txt "file1.h5 file2.h5 -p u g1/dset3 g1/dset4"
$!
$!# 6.15: hexadecimal
$ CALL TOOLTEST h5diff_615.txt "file1.h5 file2.h5 -p 0x1 g1/dset3 g1/dset4"
$!
$!# 6.16: string
$ CALL TOOLTEST h5diff_616.txt "file1.h5 file2.h5 -p "0.21" g1/dset3 g1/dset4"
$!
$!# 6.17: repeated option
$ CALL TOOLTEST h5diff_617.txt "file1.h5 file2.h5 -p 0.21 -p 0.22 g1/dset3 g1/dset4"
$!
$!# 6.18: number larger than biggest difference
$ CALL TOOLTEST h5diff_618.txt "file1.h5 file2.h5 -p 2 g1/dset3 g1/dset4"
$!
$!# 6.19: number smaller than smallest difference
$ CALL TOOLTEST h5diff_619.txt "file1.h5 file2.h5 -p 0.005 g1/dset3 g1/dset4"
$!
$!
$!
$!# ##############################################################################
$!# # -n
$!# ##############################################################################
$!
$!
$!# 6.20: no value
$ CALL TOOLTEST h5diff_620.txt "file1.h5 file2.h5 -n g1/dset3 g1/dset4"
$!
$!# 6.21: negative value
$ CALL TOOLTEST h5diff_621.txt "file1.h5 file2.h5 -n -4 g1/dset3 g1/dset4"
$!
$!# 6.22: zero
$ CALL TOOLTEST h5diff_622.txt "file1.h5 file2.h5 -n 0 g1/dset3 g1/dset4"
$!
$!# 6.23: non number
$ CALL TOOLTEST h5diff_623.txt "file1.h5 file2.h5 -n u g1/dset3 g1/dset4"
$!
$!# 6.24: hexadecimal
$ CALL TOOLTEST h5diff_624.txt "file1.h5 file2.h5 -n 0x1 g1/dset3 g1/dset4"
$!
$!# 6.25: string
$ CALL TOOLTEST h5diff_625.txt "file1.h5 file2.h5 -n "2" g1/dset3 g1/dset4"
$!
$!# 6.26: repeated option
$ CALL TOOLTEST h5diff_626.txt "file1.h5 file2.h5 -n 2 -n 3 g1/dset3 g1/dset4"
$!
$!# 6.27: number larger than biggest difference
$ CALL TOOLTEST h5diff_627.txt "file1.h5 file2.h5 -n 200 g1/dset3 g1/dset4"
$!
$!# 6.28: number smaller than smallest difference
$ CALL TOOLTEST h5diff_628.txt "file1.h5 file2.h5 -n 1 g1/dset3 g1/dset4"
$!
$!# ##############################################################################
$!# 6.29  non valid files
$!# ##############################################################################
$!
$ CALL TOOLTEST h5diff_629.txt "file1.h6 file2.h6"
$!
$!# ##############################################################################
$!# 7.  attributes
$!# ##############################################################################
$!
$ CALL TOOLTEST h5diff_70.txt "file5.h5 file6.h5 -v"
$!
$!# ##############################################################################
$!# 8.  all dataset datatypes
$!# ##############################################################################
$!
$ CALL TOOLTEST h5diff_80.txt "file7.h5 file8.h5 -v"
$!
$!# 9. compare a file with itself
$!
$ CALL TOOLTEST h5diff_90.txt "file1.h5 file1.h5"
$!
$!
$!
$TOOLTEST: SUBROUTINE
$
$ len =  F$LENGTH(P1)
$ base = F$EXTRACT(0,len-2,P1)
$ actual = base + "out"
$ actual_err = base + "err"
$
$ begin = "Testing h5diff "
$ !
$ ! Run the test and save output in the 'actual' file
$ !
$ define/nolog sys$output 'actual'
$ define/nolog sys$error  'actual_err'
$ write  sys$output "#############################"
$ write  sys$output "Expected output for 'h5diff ''P2''"
$ write  sys$output "#############################"
$ ON ERROR THEN CONTINUE
$ h5diff 'P2
$ deassign sys$output
$ deassign sys$error
$ if F$SEARCH(actual_err) .NES. ""
$ then
$ set message/notext/nofacility/noidentification/noseverity
$    append 'actual_err' 'actual'
$ set message/ntext/facility/identification/severity
$ endif
$ !
$ ! Compare the results
$ !
$ diff/output=h5diff_temp/ignore=(spacing,trailing_spaces,blank_lines) 'actual' 'P1'
$ open/read temp_out h5diff_temp.dif
$ read temp_out record1
$ close temp_out
$ !
$ ! Extract error code and format output line
$ !
$ len = F$LENGTH(record1)
$ err_code = F$EXTRACT(len-1,1,record1)
$ if err_code .eqs. "0" 
$  then
$    result = "PASSED"
$    line = F$FAO("!15AS !50AS !70AS", begin, P2, result) 
$  else
$    result = "*FAILED*"
$    line = F$FAO("!15AS !49AS !69AS", begin, P2, result) 
$ endif
$ !
$ ! Print test result
$ ! 
$  write sys$output line
$ ! 
$ ! Append the result to the log file 
$ !
$ append h5diff_temp.dif h5diff.log
$ !
$ ! Delete temporary files
$ !
$! del *.out;*
$! del *.dif;*
$ !
$ENDSUBROUTINE

