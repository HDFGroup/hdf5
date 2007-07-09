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

:: File Name: hdf5build.bat
:: This batch file is used to build HDF5 Libraries and Tools.
:: There are currently only 2 options for this batch file:
::   1. hdf5build                -- Print a usage message and quit
::   1. hdf5build                -- Build HDF5 tools and c library
:: By Xuan Bai
:: Created: Aug. 16, 2004
:: Last Updated: July 7, 2007

:: This batch file makes the following assumptions:
::    - Visual Studio .NET is installed, and VS71COMNTOOLS variable is set
::    - The directory structure is setup from a fresh source copy
::    - copy_hdf.bat has already been run from the ./windows directory
::    - Visual Studio already contains the required paths for external libraries
::    - szip and zlib DLLs are already placed in an accessible directory
::    - If hdf5_ext_szip or hdf5_ext_zlib are not set, then %szip_name% and %zlib_name%
::      will be used, respectively.

:: By default, only C and C++ libraries are built.

if defined hdf5_debug echo ON

if "x%1"=="x" goto main
goto help

:: Print a help message
:help
echo.Builds HDF5 Libraries and Tools.
echo.
echo %0 [OPTION]
echo.
echo.Please use one of the following options!
echo.
echo.   %0                   Build HDF5 C Library and Tools
echo.   %0 /?                Help information
goto :eof



:: This is where the magic happens
:main

call :setup

echo.Building HDF5 Libraries and Tools
echo.

echo.***************************************************************************** >> %build_results%
echo.                        Build HDF5 Libraries and Tools >> %build_results%
echo.***************************************************************************** >> %build_results%
echo. >> %build_results%


call :h5tinit
if errorlevel 1 (
    echo.   Error building h5tinit!
    echo.   Error building h5tinit! >> %build_results%
    goto error
)

call :all
if errorlevel 1 (
    echo.   Error building HDF5 libraries!
    echo.   Error building HDF5 libraries! >> %build_results%

    goto error
)

echo. All HDF5 libraries and tools build successfully!
echo. All HDF5 libraries and tools build successfully! >> %build_results%

call :cleanup

if defined hdf5_debug pause
goto :eof

:: Setup our environment
:setup

echo.   Setting up environment >> %build_results%

:: Put us in the directory of the batch file.
pushd %~dp0

:: Constants
if "x%zlib_name%"=="x" set zlib_name=zdll.lib
if "x%szip_name%"=="x" set szip_name=szlibdll.lib
if "x%build_results%"=="x" set build_results="%CD%\build_results.txt"

:: Create our results file
type nul > %build_results%

:: Set external libraries if they aren't already set
if x%hdf5_ext_zlib%==x set hdf5_ext_zlib=%zlib_name%
if x%hdf5_ext_szip%==x set hdf5_ext_szip=%szip_name%

:: By default, use the Visual Studio .NET environment
call "%vs71comntools%\vsvars32.bat" >> %build_results%

if defined hdf5_debug pause
goto :eof


:: Build H5tinit.exe and generate h4tinit.c
:h5tinit

echo.   Generating H5tint.c

echo.***************************************************************************** >> %build_results%
echo.                        Build H5Tinit.exe >> %build_results%
echo.***************************************************************************** >> %build_results%
echo. >> %build_results%
devenv .\windows\misc\typegen\typegen.sln /rebuild Debug >> %build_results% 2>&1
if errorlevel 1 goto :eof

:: Use output to generate h5tinit.c
.\src\h5tinit.exe > .\src\h5tinit.c

if defined hdf5_debug pause
goto :eof


:: Build the HDF5 libraries.  By default, C and C++ libraries are built.
:all

echo.   Building HDF5

echo.***************************************************************************** >> %build_results%
echo.                        Build HDF5 C Library and Tools >> %build_results%
echo.***************************************************************************** >> %build_results%
echo. >> %build_results%

:: Build both debug and release versions
for %%a in (debug release) DO (
    echo.       Building %%a libraries...
    echo.       Building %%a libraries... >> %build_results%
    devenv .\windows\proj\all\all.sln /rebuild %%a >> %build_results% 2>&1
    if errorlevel 1 goto :eof
)

if defined hdf5_debug pause
goto :eof


:: Handle errors
:error

:: For now, our error handling just consists of calling cleanup, and setting errorlevel
echo.HDF5 build failed.
echo.HDF5 build failed. >> %build_results%
call :cleanup
set errorlevel=1

if defined hdf5_debug pause
goto :eof


:: Cleanup our environment
:cleanup

set zlib_name=
set szip_name=
set build_results=
popd

if defined hdf5_debug pause
goto :eof
