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
$! Make HDF5 library tests
$!
$! ccopt = "/float=ieee_float/define=H5_VMS"
$
$ ccc := cc 'ccopt /include=([-.src])
$ type sys$input
 	Creating  testhdf5
$
$ cobj= "h5test.c, testframe.c, testhdf5.c, tarray.c, tattr.c, tconfig.c, "+-
        "tchecksum.c,"+- 
        "tfile.c, tgenprop.c, th5o.c, th5s.c, theap.c, tid.c, titerate.c,"+- 
        "tmeta.c, tmisc.c, ttime.c, trefer.c, trefstr.c,"+-
        "tselect.c, tsohm.c, tskiplist.c, ttst.c, tunicode.c, tvltypes.c,"+-
        "tvlstr.c, cache_common.c"
$!                              
$ ccc 'cobj 
$ library/create/replace []libh5test h5test, testframe, cache_common
$ type sys$input
       Creating libh5test
$ link     testhdf5,tarray,tattr,tchecksum,tconfig, -
           tfile,tgenprop,th5o,th5s,theap,tid,titerate, -
           tmeta,tmisc,ttime,trefer,trefstr, -
           tselect,tsohm,tskiplist,ttst,tunicode,tvltypes, -
           tvlstr, -
           libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$
$!
$ type sys$input
       Creating lheap test
$
$ ccc  lheap
$ link lheap, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating fheap test
$
$ ccc  fheap
$ link fheap, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating ohdr test
$ ccc  ohdr

$ link ohdr, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating stab test
$ ccc  stab
$ link stab, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating gheap test
$ ccc  gheap
$ link gheap, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating btree2 test
$ ccc  btree2
$ link btree2, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
 
$!
$ type sys$input
       Creating cache test
$ ccc  cache
$ link cache, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating cache_api test
$ ccc  cache_api
$ link cache_api, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating pool test
$ ccc  pool 
$ link pool, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating hyperslab test
$ ccc  hyperslab
$ link hyperslab, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating istore test
$ ccc  istore 
$ link istore, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating bittests test
$ ccc  bittests
$ link bittests, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating dt_arith test
$ ccc  dt_arith
$ link dt_arith, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating dtypes test
$ ccc  dtypes 
$ link dtypes, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating dsets tests
$ ccc  dsets 
$ link dsets, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating cmpd_dset test
$ ccc  cmpd_dset
$ link cmpd_dset, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating extend test
$ ccc  extend
$ link extend, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating external test
$ ccc  external
$ link external, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating objcopy test
$ ccc  objcopy
$ link objcopy, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating links test
$ ccc  links
$ link links, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating unlink test
$ ccc  unlink
$ link unlink, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating big test
$ ccc  big
$ link big, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating mtime test
$ ccc  mtime
$ link mtime, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating fillval test
$ ccc  fillval
$ link fillval, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating mount test
$ ccc  mount
$ link mount, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating flush1 test
$ ccc  flush1
$ link flush1, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating flush2 test
$ ccc  flush2
$ link flush2, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating enum test
$ ccc  enum
$ link enum, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating set_extent test
$ ccc  set_extent
$ link set_extent, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating ttsafe test
$ ccc  ttsafe
$ link ttsafe, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating getname test
$ ccc  getname
$ link getname, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating vfd test
$ ccc  vfd
$ link vfd, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating ntypes test
$ ccc  ntypes
$ link ntypes, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating dangle test
$ ccc  dangle 
$ link dangle, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating dtransform test
$ ccc  dtransform
$ link dtransform, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating reserved test
$ ccc  reserved
$ link reserved, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Creating cross_read test
$ ccc  cross_read
$ link cross_read, -
       libh5test.olb/lib,[-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input
       Done with tests compilation
$ exit 
