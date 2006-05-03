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
$! Make HDF5 Fortran library
$!
$! ccopt = "/float=ieee_float/define=H5_VMS"
$! fcopt = "/float=ieee_float/define=H5_VMS"
$ ccc := cc 'ccopt /include=[-.-.src]
$ fff := fortran 'fcopt 
$ type sys$input
       Creating and running H5test_kind to generate H5fortran_detect.f90
$!
$ fff  H5test_kind.f90
$ link H5test_kind
$ define/user_mode sys$output H5fortran_detect.f90
$ run  H5test_kind
$!
$ type sys$input
       Creating and running H5fortran_detect.f90 to generate H5fort_type_defines.h
$!
$ fff  H5fortran_detect.f90 
$ link H5fortran_detect 
$ define/user_mode sys$output H5fort_type_defines.h
$ run  H5fortran_detect 
$!
$ type sys$input
 	Creating and running H5match_types to generate H5fortran_types.f90
$!
$ ccc  H5match_types.c
$ link H5match_types
$ run  H5match_types
$!
$!
$ type sys$input
	Creating  HDF5 Fortran library
$!
$ cobj="H5f90kit, H5_f, H5Af, H5Df, H5Ef, H5Ff, H5Gf, "+-
       "H5If, H5Pf, H5Rf, H5Sf, H5Tf, H5Zf"
$ ffiles= "H5_ff.f90, H5Aff.f90, H5Dff.f90, H5Eff.f90,"+-
          "H5Fff.f90, H5Gff.f90, H5Iff.f90, H5Pff.f90, H5Rff.f90, H5Sff.f90,"+-
          "H5Tff.f90, H5Zff.f90, HDF5.f90"
$ fobj="H5fortran_flags, H5f90global, "+-
       "H5fortran_types, H5_ff, H5Aff, H5Dff, H5Eff,"+-
       "H5Fff, H5Gff, H5Iff, H5Pff, H5Rff, H5Sff,"+-
       "H5Tff, H5Zff, HDF5"
$!
$ ccc 'cobj 
$ fff H5fortran_flags.f90
$ fff H5fortran_types.f90
$ fff H5f90global.f90
$ 
$ fff 'ffiles
$ library/create []hdf5_fortran  'cobj
$ library/replace []hdf5_fortran  'fobj
$ type sys$input
       Done
$ exit
