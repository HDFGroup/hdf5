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
$! Make HDF5 Fortran examples
$!
$ fcopt = "/float=ieee_float"
$ fff := fortran 'fcopt /debug/define=H5_VMS/nooptimize/module=[-.src]
$
$ type sys$input
	Compiling HDF5 Fortran examples
$!
$ ffiles="dsetexample.f90, fileexample.f90, rwdsetexample.f90, "+-
         "attrexample.f90, groupexample.f90, grpsexample.f90, "+-
         "grpdsetexample.f90, hyperslab.f90, selectele.f90, grpit.f90,"+-
         "refobjexample.f90, refregexample.f90, mountexample.f90,"+-
         "compound.f90"
      

$!
$ fff 'ffiles
$ type sys$input

        Creating dsetexample 
$ link       dsetexample,-
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib
$ type sys$input

        Creating fileexample
$ link       fileexample,-
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib
$ type sys$input

        Creating rwdsetexample
$ link       rwdsetexample,-
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib
$ type sys$input

        Creating attrexample
$ link       attrexample,-
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib
$ type sys$input

        Creating groupexample
$ link       groupexample,-
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib
$ type sys$input

        Creating grpsexample
$ link       grpsexample,-
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib
$ type sys$input

        Creating grpdsetexample
$ link       grpdsetexample,-
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib
$ type sys$input

        Creating hyperslab
$ link       hyperslab ,-
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib
$ type sys$input

        Creating selectele
$ link       selectele,-
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib
$ type sys$input

        Creating grpit
$ link       grpit,-
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib
$ type sys$input

        Creating refobjexample
$ link       refobjexample,-
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib
$ type sys$input

        Creating refregexample
$ link       refregexample,-
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib
$ type sys$input

        Creating mountexample
$ link       mountexample,-
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib
$ type sys$input

        Creating compound
$ link       compound,-
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib

$ type sys$input


	Running Fortran examples

$ type sys$input

	Running dsetexample 
$ run dsetexample
$
$ type sys$input 

	Running fileexample
$ run fileexample
$
$ type sys$input

	Running rwdsetexample
$ run rwdsetexample
$
$ type sys$input

	Running attrexample
$ run attrexample
$
$ type sys$input

	Running groupexample
$ run groupexample
$
$ type sys$input

	Running grpsexample
$ run grpsexample
$
$ type sys$input

	Running grpdsetexample
$ run grpdsetexample
$
$ type sys$input

	Running hyperslab
$ run hyperslab
$
$ type sys$input

	Running selectele
$ run selectele
$
$ type sys$input

	Running grpit
$ run grpit
$
$ type sys$input

	Running refobjexample
$ run refobjexample
$
$ type sys$input

	Running refregexample
$ run refregexample
$
$ type sys$input

	Running mountexample
$ run mountexample
$
$ type sys$input

	Running compound
$ run compound
$
