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
$!
$! This file copies all make files from the VMS directory to the 
$! source directories and builds libraries, tests, and utilties
$!
$ copy [.c__.examples]*.com     [-.c__.examples]
$ copy [.c__.src]make.com       [-.c__.src]
$ copy [.c__.test]*.com         [-.c__.test]
$ copy [.fortran.examples]*.com [-.fortran.examples]
$ copy [.fortran.src]make.com   [-.fortran.src]
$ copy [.fortran.test]*.com     [-.fortran.test]
$ copy [.src]make.com           [-.src]
$ copy [.src]h5pubconf.h        [-.src]
$ copy [.test]*.com             [-.test]
$ copy [.examples]*.com         [-.examples]
$ copy [.tools.h5dump]*.com     [-.tools.h5dump]
$ copy [.tools.h5ls]*.com       [-.tools.h5ls]
$ copy [.tools.h5diff]*.com     [-.tools.h5diff]
$ copy [.tools.h5repack]*.com   [-.tools.h5repack]
$ copy [.tools.h5import]*.com   [-.tools.h5import]
$ copy [.tools.h5jam]*.com      [-.tools.h5jam]
$ copy [.tools.lib]make.com     [-.tools.lib]
$!
$! Define location of ZLIB library. If you do not have it on your system, download
$! source code from http://www.zlib.net/, build and install on your system
$ define zlib_dir sys$sysusers:[pourmale.zlib-1_2_3]
$!
$! Set up compilation flags here
$! Do not remove define=H5_VMS and standard=strict_ansi qualifiers.
$!
$ ccopt == "/float=ieee_float/define=H5_VMS/debug/nooptimize/include=zlib_dir"
$ fcopt == "/float=ieee_float/define=H5_VMS/debug/nooptimize/include=zlib_dir"
$ cxxopt == "/float=ieee_float/define=H5_VMS/debug/nooptimize/"+-
            "standard=strict_ansii/include=zlib_dir"
$!
$!
$ hdf5top     = F$DIRECTORY()
$ len         = F$LENGTH(hdf5top)
$ hdf5top_dir = F$EXTRACT(0, len-4, hdf5top)
$!
$ hdf5src              = hdf5top_dir + "SRC]"
$ hdf5test             = hdf5top_dir + "TEST]"
$ hdf5examples         = hdf5top_dir + "EXAMPLES]"
$ hdf5tools_lib        = hdf5top_dir + "TOOLS.LIB]"
$ hdf5tools_h5diff     = hdf5top_dir + "TOOLS.H5DIFF]"
$ hdf5tools_h5dump     = hdf5top_dir + "TOOLS.H5DUMP]"
$ hdf5tools_h5ls       = hdf5top_dir + "TOOLS.H5LS]"
$ hdf5tools_h5repack   = hdf5top_dir + "TOOLS.H5REPACK]"
$ hdf5tools_h5jam      = hdf5top_dir + "TOOLS.H5JAM]"
$ hdf5tools_h5import   = hdf5top_dir + "TOOLS.H5IMPORT]"
$ hdf5fortran_examples = hdf5top_dir + "FORTRAN.EXAMPLES]"
$ hdf5fortran_src      = hdf5top_dir + "FORTRAN.SRC]"
$ hdf5fortran_test     = hdf5top_dir + "FORTRAN.TEST]"
$ hdf5cxx_src          = hdf5top_dir + "C__.SRC]"
$ hdf5cxx_test         = hdf5top_dir + "C__.TEST]"
$ hdf5cxx_examples     = hdf5top_dir + "C__.EXAMPLES]"

$ write sys$output "Building C library"
$ set def 'hdf5src'
$ @make.com
$!
$!
$ write sys$output "Building C library tests"
$ set def 'hdf5test'
$ @make.com
$!
$ write sys$output "Building tools library"
$ set def 'hdf5tools_lib'
$ @make.com
$!
$ write sys$output "Building h5diff"
$ set def 'hdf5tools_h5diff'
$ @make.com
$!
$ write sys$output "Building h5dump"
$ set def 'hdf5tools_h5dump'
$ @make.com
$!
$ write sys$output "Building h5repack"
$ set def 'hdf5tools_h5repack'
$ @make.com
$!
$ write sys$output "Building h5ls"
$ set def 'hdf5tools_h5ls'
$ @make.com
$!
$ write sys$output "Building h5jam"
$ set def 'hdf5tools_h5jam'
$ @make.com
$!
$ write sys$output "Building h5import"
$ set def 'hdf5tools_h5import'
$ @make.com
$!
$ write sys$output "Building Fortran library"
$ set def 'hdf5fortran_src'
$ @make.com
$!
$ write sys$output "Building Fortran library tests"
$ set def 'hdf5fortran_test'
$ @make.com
$!
$ write sys$output "Building C++ library"
$ set def 'hdf5cxx_src'
$ rename *.cpp *.cxx
$ @make.com
$!
$ write sys$output "Building C++ library tests"
$ set def 'hdf5cxx_test'
$ rename *.cpp *.cxx
$ @make.com
$!
$ write sys$output "Building C examples"
$ set def 'hdf5examples'
$ @make.com
$!
$ write sys$output "Building Fortran examples"
$ set def 'hdf5fortran_examples'
$ @make.com
$!
$ write sys$output "Building C++ examples"
$ set def 'hdf5cxx_examples'
$ rename *.cpp *.cxx
$ @make.com
$!
$ set def 'hdf5top'
$ exit
