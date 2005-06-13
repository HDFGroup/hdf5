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
