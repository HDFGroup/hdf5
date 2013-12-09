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
$! Makefile for VMS systems.
$!
$! Make HDF5 Fortran examples
$!
$ fcopt = "/float=ieee_float/define=H5_VMS"
$ define zlib_dir SYS$SYSUSERS:[lu.zlib-1_2_5]
$ fff := fortran 'fcopt /module=[-.-.include]
$
$ type sys$input
	Compiling HDF5 Fortran examples
$!
$ ffiles="h5_rdwt.f90, h5_subset.f90, hyperslab.f90, "+-
         "h5_crtatt.f90, h5_crtgrp.f90, h5_crtgrpar.f90, h5_cmprss,"+-
         "h5_crtgrpd.f90, h5_crtdat.f90, selectele.f90, h5_extend.f90,"+-
         "refobjexample.f90, refregexample.f90, mountexample.f90,"+-
         "compound.f90"
$!
$ fff 'ffiles
$ type sys$input

        Creating h5_crtdat
$ link       h5_crtdat ,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating h5_rdwt 
$ link       h5_rdwt,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating h5_subset
$ link       h5_subset,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating hyperslab
$ link       hyperslab,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating h5_cmprss
$ link       h5_cmprss,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input


        Creating h5_crtatt
$ link       h5_crtatt,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating h5_crtgrp
$ link       h5_crtgrp,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating h5_crtgrpar
$ link       h5_crtgrpar,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating h5_crtgrpd
$ link       h5_crtgrpd,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating selectele
$ link       selectele,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating h5_extend
$ link       h5_extend,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating refobjexample
$ link       refobjexample,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating refregexample
$ link       refregexample,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating mountexample
$ link       mountexample,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating compound
$ link       compound,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ exit
