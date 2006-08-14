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
$! Make HDF5 library
$!
$! ccopt = "/float=ieee_float/define=H5_VMS"
$ ccc := cc 'ccopt 
$ ccc h5detect.c
$ link h5detect
$ type sys$input
        Running h5detect to create h5tinit.c
$ define/user_mode sys$output h5tinit.c
$ run h5detect
$
$ type sys$input
         Creating  HDF5 library
$!
$ cobj= "H5, H5A, H5AC, H5B, H5B2, H5B2cache,"+-
        "H5Bcache, H5B2dbg, H5B2test, H5B2int, H5B2stat, H5C, H5CS,"+-  
        "H5D, H5Dcontig, H5Dcompact,"+-
        "H5Defl, H5Dio, H5Distore, H5Doh, H5Dmpio, H5Dselect, H5Dtest ,"+-
        "H5E, H5F, H5Fdbg, H5Fmount, H5Fsfile, H5Fsuper, H5FD, H5FDcore,"+-
        "H5FDfamily, H5FDlog, H5FDmpi, H5FDmpio,"+-
        "H5FDmpiposix, H5FDmulti, H5FDsec2, H5FDstdio,"+-
        "H5FDstream, H5FL, H5FO,"+-
        "H5FS, H5FScache, H5FSdbg, H5FSsection,"+-
        "H5G, H5Gdeprec, H5Gent, H5Glink, H5Gloc, H5Gname, H5Gnode, H5Gstab,"+-
        "H5Gobj, H5Goh, H5Gtest, H5Gtraverse,"+-
        "H5HF, H5HFcache, H5HFdbg, H5HFman, H5HFtest, H5HFstat,"+-
        "H5HFdblock, H5HFdtable, H5HFhdr, H5HFiblock,"+-
        "H5HFiter, H5HFsection, H5HFspace,"+-
        "H5HG, H5HGdbg, H5HL, H5HLdbg, H5HP, H5I, H5MF, H5MM,"+-
        "H5MP, H5MPtest,H5L, H5Lexternal, H5O, H5Oattr, H5Obogus, H5Ocache,"+-
        "H5Ocont, H5Odtype, H5Oefl, H5Ofill, H5Oginfo, H5Olayout,"+-
        "H5Olinfo, H5Olink, H5Omtime,"+-
        "H5Oname, H5Onull, H5Opline, H5Osdspace, H5Oshared, H5Ostab,"+-
        "H5P, H5Pacpl, H5Pdcpl, H5Pdxpl, H5Pfapl, H5Pfcpl, H5Pgcpl, H5Plapl, "+-
        "H5Pocpl, H5Ptest"
$ cobj1= "H5R, H5RC,"+-
        "H5RS, H5S, H5Sall, H5Shyper, H5Smpio, H5Snone, H5Spoint,"+-
        "H5Sselect, H5Stest,"+-
        "H5SL, H5ST, H5T, H5Tarray, H5Tbit, H5Tcommit,"+-
        "H5Tcompound, H5Tconv, H5Tcset, H5Tenum, H5Tfields, H5Tfixed,"+-
        "H5Tfloat, H5Tinit, H5Tnative, H5Toffset, H5Toh, H5Topaque, H5Torder,"+-
        "H5Tpad, H5Tprecis, H5Tstrpad, H5Tvlen, H5TS, H5V, H5Z,"+-
        "H5Zdeflate, H5Zfletcher32, H5Znbit, H5Zshuffle, H5Zszip,"+-
        "H5Zscaleoffset, H5Ztrans"
$!
$ ccc 'cobj
$ ccc 'cobj1 
$ library/create []hdf5
$ library/insert []hdf5 'cobj, 'cobj1
$ type sys$input
	Done
$!
