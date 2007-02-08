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
$! Make HDF5 C examples
$!
$! ccopt = "/float=ieee_float/nowarnings/define=H5_VMS"
$!
$ ccc := cc 'ccopt /include=([-.src])
$ type sys$input
 	Compiling  C examples
$!
$ cobj= "h5_write.c, h5_read.c, h5_extend_write.c, h5_chunk_read.c, "+-
        "h5_compound.c, h5_group.c, h5_select.c, h5_attribute.c,  "+- 
        "h5_mount.c, h5_ref2reg.c, h5_reference.c, h5_drivers.c "
$!                               
$ ccc 'cobj 
$
$ type sys$input
       Creating h5_write 
$ link     h5_write, -
            [-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
       Creating h5_read 
$ link     h5_read, -
            [-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
       Creating h5_extend_write
$ link     h5_extend_write, -
            [-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
       Creating h5_chunk_read 
$ link     h5_chunk_read, -
            [-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
       Creating h5_compound 
$ link     h5_compound, -
            [-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
       Creating h5_group 
$ link     h5_group, -
            [-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
       Creating h5_select 
$ link     h5_select, -
            [-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
       Creating h5_attribute 
$ link     h5_attribute, -
            [-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
       Creating h5_mount 
$ link     h5_mount, -
            [-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
       Creating h5_reference 
$ link     h5_reference, -
            [-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
       Creating h5_ref2reg
$ link     h5_ref2reg, -
            [-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
       Creating h5_drivers
$ link     h5_drivers, -
            [-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ exit
