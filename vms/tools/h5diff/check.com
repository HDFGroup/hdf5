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
$! h5diff testing script
$!
$ define sys$output h5diff.out
$ define sys$error  h5diff.err
$ h5diff :== $sys$sysusers:[pourmale.hdf5.tools.h5diff]h5diff.exe



$! 1.0
$ h5diff -h

$! 1.1 normal mode
$ h5diff file1.h5 file2.h5 

$! 1.2 normal mode with objects
$ h5diff file1.h5 file2.h5  g1/dset1 g1/dset2

$! 1.3 report mode
$ h5diff file1.h5 file2.h5 -r

$! 1.4 report  mode with objects
$ h5diff file1.h5 file2.h5  -r g1/dset1 g1/dset2

$! 1.5 with -d
$ h5diff file1.h5 file2.h5 -r -d 5 g1/dset3 g1/dset4

$! 1.6 with -p
$ h5diff file1.h5 file2.h5 -r -p 0.05 g1/dset3 g1/dset4

$! 1.7 verbose mode
$ h5diff file1.h5 file2.h5 -v  

$! 1.8 quiet mode 
$ h5diff file1.h5 file2.h5 -q

$! ##############################################################################
$! # not comparable types
$! ##############################################################################

$! 2.0
$ h5diff file3.h5 file3.h5 -v dset group

$! 2.1
$ h5diff file3.h5 file3.h5 -v dset link

$! 2.2
$ h5diff file3.h5 file3.h5 -v dset type

$! 2.3
$ h5diff file3.h5 file3.h5 -v group group

$! 2.4
$ h5diff file3.h5 file3.h5 -v type type

$! 2.5
$ h5diff file3.h5 file3.h5 -v link link


$! ##############################################################################
$! # Dataset types
$! ##############################################################################

$! 5.0
$ h5diff file4.h5 file4.h5 -v dset0a dset0b

$! 5.1
$ h5diff file4.h5 file4.h5 -v dset1a dset1b

$! 5.2
$ h5diff file4.h5 file4.h5 -v dset2a dset2b

$! 5.3
$ h5diff file4.h5 file4.h5 -v dset3a dset4b

$! 5.4
$ h5diff file4.h5 file4.h5 -v dset4a dset4b

$! 5.5
$ h5diff file4.h5 file4.h5 -v dset5a dset5b

$! 5.6
$ h5diff file4.h5 file4.h5 -v dset6a dset6b

$! 5.7
$ h5diff file4.h5 file4.h5 -v dset7a dset7b

$! 5.8 long_long test; different format of long_long print  in Linux and IRIX
$!$! h5diff h5diff_58.txt file4.h5 file4.h5 dset8a dset8b

$! ##############################################################################
$! # Error messages
$! ##############################################################################


$! 6.0: Check if the command line number of arguments is less than 3
$ h5diff file1.h5 

$! 6.1: Check for invalid options
$ h5diff file1.h5 file2.h5 -x 

$! ##############################################################################
$! # -d 
$! ##############################################################################

$! 6.2: no value
$ h5diff file1.h5 file2.h5  -d g1/dset3 g1/dset4

$! 6.3: negative value
$ h5diff file1.h5 file2.h5  -d -4 g1/dset3 g1/dset4

$! 6.4: zero
$ h5diff file1.h5 file2.h5  -d 0 g1/dset3 g1/dset4

$! 6.5: non number
$ h5diff file1.h5 file2.h5  -d u g1/dset3 g1/dset4

$! 6.6: hexadecimal
$ h5diff file1.h5 file2.h5 -d 0x1 g1/dset3 g1/dset4

$! 6.7: string
$ h5diff file1.h5 file2.h5  -d "1" g1/dset3 g1/dset4

$! 6.8: repeated option
$ h5diff file1.h5 file2.h5  -d 1 -d 2 g1/dset3 g1/dset4

$! 6.9: number larger than biggest difference
$ h5diff file1.h5 file2.h5  -d 200 g1/dset3 g1/dset4

$! 6.10: number smaller than smallest difference
$ h5diff file1.h5 file2.h5  -d 1 g1/dset3 g1/dset4


$! ##############################################################################
$! # -p
$! ##############################################################################


$! 6.11: no value
$ h5diff file1.h5 file2.h5 -r -p g1/dset3 g1/dset4

$! 6.12: negative value
$ h5diff file1.h5 file2.h5 -p -4 g1/dset3 g1/dset4

$! 6.13: zero
$ h5diff file1.h5 file2.h5 -p 0 g1/dset3 g1/dset4

$! 6.14: non number
$ h5diff file1.h5 file2.h5 -p u g1/dset3 g1/dset4

$! 6.15: hexadecimal
$ h5diff file1.h5 file2.h5 -p 0x1 g1/dset3 g1/dset4

$! 6.16: string
$ h5diff file1.h5 file2.h5 -p "0.21" g1/dset3 g1/dset4

$! 6.17: repeated option
$ h5diff file1.h5 file2.h5 -p 0.21 -p 0.22 g1/dset3 g1/dset4

$! 6.18: number larger than biggest difference
$ h5diff file1.h5 file2.h5 -p 2 g1/dset3 g1/dset4

$! 6.19: number smaller than smallest difference
$ h5diff file1.h5 file2.h5 -p 0.005 g1/dset3 g1/dset4



$! ##############################################################################
$! # -n
$! ##############################################################################


$! 6.20: no value
$ h5diff file1.h5 file2.h5 -n g1/dset3 g1/dset4

$! 6.21: negative value
$ h5diff file1.h5 file2.h5 -n -4 g1/dset3 g1/dset4

$! 6.22: zero
$ h5diff file1.h5 file2.h5 -n 0 g1/dset3 g1/dset4

$! 6.23: non number
$ h5diff file1.h5 file2.h5 -n u g1/dset3 g1/dset4

$! 6.24: hexadecimal
$ h5diff file1.h5 file2.h5 -n 0x1 g1/dset3 g1/dset4

$! 6.25: string
$ h5diff file1.h5 file2.h5 -n "2" g1/dset3 g1/dset4

$! 6.26: repeated option
$ h5diff file1.h5 file2.h5 -n 2 -n 3 g1/dset3 g1/dset4

$! 6.27: number larger than biggest difference
$ h5diff file1.h5 file2.h5 -n 200 g1/dset3 g1/dset4

$! 6.28: number smaller than smallest difference
$ h5diff file1.h5 file2.h5 -n 1 g1/dset3 g1/dset4

$! ##############################################################################
$! # non valid files
$! ##############################################################################

$ h5diff file1.h6 file2.h6

$! ##############################################################################
$! # attributes
$! ##############################################################################

$ h5diff file5.h5 file6.h5 -v

$! ##############################################################################
$! # all dataset datatypes
$! ##############################################################################

$ h5diff file7.h5 file8.h5 -v

