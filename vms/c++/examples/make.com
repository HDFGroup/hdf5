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
$! Make HDF5 C++ examples
$!
$! cxxopt = "/float=ieee_float/standard=strict_ansi/define=H5_VMS"
$
$ ccc := cxx 'cxxopt /include=([-.-.src], [-.src])
$!
$!
$ cxxobj= "create.cxx, readdata.cxx, writedata.cxx, compound.cxx,"+-
          "extend_ds.cxx, chunks.cxx, h5group.cxx"
$! 
$!                              
$ ccc 'cxxobj
$ type sys$input

       Creating create 
$ cxxlink  create, -
           [-.src]hdf5_cplus.olb/lib, -
           [-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

       Creating readdata  
$ cxxlink  readdata, -
           [-.src]hdf5_cplus.olb/lib, -
           [-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input

       Creating writedata  
$ cxxlink  writedata, -
           [-.src]hdf5_cplus.olb/lib, -
           [-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ !                              
$ type sys$input

       Creating compound 
$ cxxlink  compound, -
           [-.src]hdf5_cplus.olb/lib, -
           [-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input

       Creating extend_ds  
$ cxxlink  extend_ds, -
           [-.src]hdf5_cplus.olb/lib, -
           [-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input

       Creating chunks 
$ cxxlink  chunks, -
           [-.src]hdf5_cplus.olb/lib, -
           [-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input

       Creating h5group 
$ cxxlink  h5group, -
           [-.src]hdf5_cplus.olb/lib, -
           [-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ exit
