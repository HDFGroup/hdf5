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

@REM File Name : hdf5bt_net.bat
@REM Purpose   : Building and Testing HDF5 with MSVS .NET
@REM Written By: Fang GUO
@REM Date      : May 27, 2005
@REM Update    : June 7, 2005

@REM There are 2 options for this batch file:
@REM   1. hdf5bt                -- Build and test HDF5 tools and c library
@REM   2. hdf5bt enablecpp      -- Build and test HDF5 tools and c/c++ library

@ECHO OFF

type nul > hdf5_results_net.txt
echo ***************************************************************************** >> hdf5_results_net.txt
echo                         Build and Test HDF5 Library and Tools >> hdf5_results_net.txt
echo ***************************************************************************** >> hdf5_results_net.txt

call hdf5build_net %1
more build_results_net.txt >> hdf5_results_net.txt
del build_results_net.txt

call hdf5check %1
more check_results.txt >> hdf5_results_net.txt
del check_results.txt
