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
