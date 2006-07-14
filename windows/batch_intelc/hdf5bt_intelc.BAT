@REM Copyright by the Board of Trustees of the University of Illinois.
@REM All rights reserved.
@REM
@REM This file is part of HDF5.  The full HDF5 copyright notice, including
@REM terms governing use, modification, and redistribution, is contained in
@REM the files COPYING and Copyright.html.  COPYING can be found at the root
@REM of the source code distribution tree; Copyright.html can be found at the
@REM root level of an installed copy of the electronic HDF5 document set and
@REM is linked from the top-level documents page.  It can also be found at
@REM http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
@REM access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.


@REM File Name : hdf5bt_intelc.bat
@REM Purpose   : Building and Testing HDF5 with INTEL C++ Compiler
@REM Written By: Fang GUO
@REM Date      : May 27, 2005
@REM Update    : June 7, 2005

@REM This batch file is used to build and test HDF5 Libraries and Tools.
@REM There are 2options for this batch file:
@REM   1. hdf5bt_intelc                -- Build and test HDF5 tools and c library
@REM   2. hdf5bt_intelc enablecpp      -- Build and test HDF5 tools and c/c++ library

@ECHO OFF

type nul > hdf5_results_intelc.txt
echo ***************************************************************************** >> hdf5_results_intelc.txt
echo                         Build and Test HDF5 Library and Tools >> hdf5_results_intelc.txt
echo ***************************************************************************** >> hdf5_results_intelc.txt

call hdf5build_intelc %1
more build_results_intelc.txt >> hdf5_results_intelc.txt
del build_results_intelc.txt

echo Checking building results with Intel C++ Compiler
echo.
call hdf5check %1
more check_results.txt >> hdf5_results_intelc.txt
del check_results.txt
