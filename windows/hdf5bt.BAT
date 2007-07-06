@rem Copyright by The HDF Group.
@rem Copyright by the Board of Trustees of the University of Illinois.
@rem All rights reserved.
@rem
@rem This file is part of HDF5.  The full HDF5 copyright notice, including
@rem terms governing use, modification, and redistribution, is contained in
@rem the files COPYING and Copyright.html.  COPYING can be found at the root
@rem of the source code distribution tree; Copyright.html can be found at the
@rem root level of an installed copy of the electronic HDF5 document set and
@rem is linked from the top-level documents page.  It can also be found at
@rem http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
@rem access to either file, you may request a copy from help@hdfgroup.org.

@echo OFF

:: File Name: hdf5bt.bat
:: This batch file is used to build and test HDF5 Libraries and Tools.
:: There are currently only 2 options for this batch file:
::   hdf5bt /?             -- Print a usage message and quit.
::   hdf5bt                -- Build and test HDF5 tools and C/C++ libraries
:: By Xuan Bai
:: Created: Aug. 16, 2004
:: Last Updated: July 6, 2007

:: This batch file makes the following assumptions:
::    - Visual Studio .NET is installed, and VS71COMNTOOLS variable is set
::    - The directory structure is setup from a fresh source copy
::    - copy_hdf.bat has already been run from the ./windows directory
::    - Visual Studio already contains the required paths for external libraries
::    - szip and zlib DLLs are already placed in an accessible directory
::    - If HDF5_EXT_SZIP or HDF5_EXT_ZLIB are not set, then %SZIP_NAME% and %ZLIB_NAME%
::      will be used, respectively (defined in hdfbuild.BAT)

:: By default, only C and C++ libraries are built and tested.

if defined hdf5_debug echo ON

if "x%1"=="x" goto main
goto help

:: Print a help message
:help
echo.Builds and tests HDF5 Libraries and Tools.
echo.
echo %0 [OPTION]
echo.
echo.Please use one of the following options!
echo.
echo.   %0                   Build and tests HDF5 C/C++ Library and Tools
echo.   %0 /?                Help information

if defined hdf5_debug pause
goto :eof


:: This is where the magic happens
:main

call :setup

echo.Build and testing all HDF5 libraries and tools
echo.

echo.***************************************************************************** >> %bt_results%
echo.                        Build and Test HDF5 Library and Tools >> %bt_results%
echo.***************************************************************************** >> %bt_results%
echo. >> %bt_results%

call :build
if errorlevel 1 (
    echo.Error building HDF5 libraries!
    goto error
)

call :test
if errorlevel 1 (
    echo.Error testing HDF5 libraries!
    goto error
)

echo. All HDF5 libraries and tools build and tested successfully!
echo. All HDF5 libraries and tools build and tested successfully! >> %build_results%

call :cleanup

if defined hdf5_debug pause
goto :eof


:: Setup our environment
:setup

:: Put us in the directory of the batch file.
pushd %~dp0

:: Constants
if "x%bt_results%"=="x" set bt_results="%CD%\bt_results.txt"
if "x%build_results%"=="x" set build_results="%CD%\build_results.txt"

:: Create our results file
type nul > %bt_results%

if defined hdf5_debug pause
goto :eof


:: Build HDF5 libraries and tools
:build

call hdf5build.BAT
type %build_results% >> %bt_results%

if defined hdf5_debug pause
goto :eof


:: Test our libraries and tools
:test

call hdf5check enablecpp
type check_results.txt >> %bt_results%

if defined hdf5_debug pause
goto :eof


:: Handle errors
:error

:: For now, our error handling just consists of calling cleanup, and exiting
echo.hdf5bt failed.
echo.hdf5bt failed. >> %bt_results%
call :cleanup
set errorlevel=1

if defined hdf5_debug pause
goto :eof


:: Cleanup our environment
:cleanup

set bt_results=
set build_results=
popd

if defined hdf5_debug pause
goto :eof
