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
$! Make HDF5 C++ library
$!
$! cxxopt = "/float=ieee_float/standard=strict_ansi/define=H5_VMS"
$ ccc := cxx 'cxxopt /include=([-.-.src])
$!
$! type sys$input
$!	Creating  HDF5 C++ library
$!
$ lib/create/object hdf5_cplus.olb
$ ccc H5Exception.cxx
$ lib/insert/object hdf5_cplus H5Exception
$ ccc H5IdComponent.cxx
$ lib/insert/object hdf5_cplus H5IdComponent
$ ccc H5Library.cxx
$ lib/insert/object hdf5_cplus H5Library
$ ccc H5Attribute.cxx
$ lib/insert/object hdf5_cplus H5Attribute
$ ccc H5Object.cxx
$ lib/insert/object hdf5_cplus H5Object
$ ccc H5PropList.cxx 
$ lib/insert/object hdf5_cplus H5PropList
$ ccc H5FaccProp.cxx
$ lib/insert/object hdf5_cplus H5FaccProp
$ ccc H5FcreatProp.cxx
$ lib/insert/object hdf5_cplus H5FcreatProp
$ ccc H5DcreatProp.cxx
$ lib/insert/object hdf5_cplus H5DcreatProp
$ ccc H5DxferProp.cxx 
$ lib/insert/object hdf5_cplus H5DxferProp
$ ccc H5DataType.cxx
$ lib/insert/object hdf5_cplus H5DataType
$ ccc H5DataSpace.cxx
$ lib/insert/object hdf5_cplus H5DataSpace
$ ccc H5AbstractDs.cxx
$ lib/insert/object hdf5_cplus H5AbstractDs
$ ccc H5AtomType.cxx
$ lib/insert/object hdf5_cplus H5AtomType
$ ccc H5PredType.cxx
$ lib/insert/object hdf5_cplus H5PredType
$ ccc H5EnumType.cxx
$ lib/insert/object hdf5_cplus H5EnumType
$ ccc H5IntType.cxx
$ lib/insert/object hdf5_cplus H5IntType
$ ccc H5FloatType.cxx
$ lib/insert/object hdf5_cplus H5FloatType
$ ccc H5StrType.cxx
$ lib/insert/object hdf5_cplus H5StrType
$ ccc H5ArrayType.cxx
$ lib/insert/object hdf5_cplus H5ArrayType
$ ccc H5VarLenType.cxx
$ lib/insert/object hdf5_cplus H5VarLenType
$ ccc H5CompType.cxx
$ lib/insert/object hdf5_cplus H5CompType
$ ccc H5DataSet.cxx
$ lib/insert/object hdf5_cplus H5DataSet
$ ccc H5CommonFG.cxx
$ lib/insert/object hdf5_cplus H5CommonFG
$ ccc H5Group.cxx
$ lib/insert/object hdf5_cplus H5Group
$ ccc H5File.cxx
$ lib/insert/object hdf5_cplus H5File
$!
$ type sys$input
$	Done
$ exit
 
