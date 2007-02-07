@REM Copyright by The HDF Group.
@REM Copyright by the Board of Trustees of the University of Illinois.
@REM All rights reserved.
@REM
@REM This file is part of HDF5.  The full HDF5 copyright notice, including
@REM terms governing use, modification, and redistribution, is contained in
@REM the files COPYING and Copyright.html.  COPYING can be found at the root
@REM of the source code distribution tree; Copyright.html can be found at the
@REM root level of an installed copy of the electronic HDF5 document set and
@REM is linked from the top-level documents page.  It can also be found at
@REM http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
@REM access to either file, you may request a copy from help@hdfgroup.org.


@REM File Name: hdf5bt.bat
@REM This batch file is used to build and test HDF5 Libraries and Tools.
@REM There are 4 options for this batch file:
@REM   1. hdf5bt                -- Build and test HDF5 tools and c library
@REM   2. hdf5bt enablecpp      -- Build and test HDF5 tools and c/c++ library
@REM   3. hdf5bt enablefortran  -- Build and test HDF5 tools and c/fortran library
@REM   4. hdf5bt enableall      -- Build and test HDF5 tools and c/c++/fortran library
@REM By Xuan Bai
@REM Created: Aug. 16, 2004
@REM Last Updated: Aug. 19, 2004

@ECHO OFF

type nul > hdf5_results.txt
echo ***************************************************************************** >> hdf5_results.txt
echo                         Build and Test HDF5 Library and Tools >> hdf5_results.txt
echo ***************************************************************************** >> hdf5_results.txt

call hdf5build %1
more build_results.txt >> hdf5_results.txt
del build_results.txt
call hdf5check %1
more check_results.txt >> hdf5_results.txt
del check_results.txt
