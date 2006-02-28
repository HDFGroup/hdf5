::  Copyright by the Board of Trustees of the University of Illinois.
::  All rights reserved.
:: 
::  This file is part of HDF5.  The full HDF5 copyright notice, including
::  terms governing use, modification, and redistribution, is contained in
::  the files COPYING and Copyright.html.  COPYING can be found at the root
::  of the source code distribution tree; Copyright.html can be found at the
::  root level of an installed copy of the electronic HDF5 document set and
::  is linked from the top-level documents page.  It can also be found at
::  http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
::  access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.

::  File Name : hdf5build_net.bat
::  Purpose   : Building HDF5 with MSVS .NET
::  Written By: Fang GUO
::  Date      : May 27, 2005
::  Update    : June 7, 2005

::  There are 2 options for this batch file:
::    1. hdf5build_net                -- Build HDF5 tools and c library
::    2. hdf5build_net enablecpp      -- Build HDF5 tools and c/c++ library
@echo off

if "%1"=="/?" GOTO HELP
if not %1.==. (
	if not "%1"=="/?" (
				if not "%1"=="enablecpp" goto WRONG
	)
)
   
call convert_to_vcproj
type nul > build_results_net.txt
type nul > all_debug.log
type nul >all_release.log
echo ***************************************************************************** >> build_results_net.txt
echo                         Build H5Tinit.exe >> build_results_net.txt
echo ***************************************************************************** >> build_results_net.txt

cd windows\misc\typegen
devenv typegen.sln /rebuild Debug /project h5tinit /out ..\..\..\h5tinit.log
cd ..\..\..\
more h5tinit.log >> build_results_net.txt
del h5tinit.log

cd src
h5tinit.exe > h5tinit.c
cd ..\


@ECHO OFF
if %1.==. GOTO BUILDC
if %1==enablecpp GOTO BUILDCPP

:BUILDC

echo ***************************************************************************** >> build_results_net.txt
echo                         Build HDF5 C Library and Tools >> build_results_net.txt
echo ***************************************************************************** >> build_results_net.txt

echo Starting Building HDF5 C Libraries!

cd windows\proj\all
devenv all.sln /rebuild debug >> ..\..\..\all_debug.log
devenv all.sln /rebuild release >> ..\..\..\all_release.log
cd ..\..\..\
more all_debug.log >> build_results_net.txt
more all_release.log >> build_results_net.txt
del all_debug.log
del all_release.log
GOTO END

:BUILDCPP
echo ***************************************************************************** >> build_results_net.txt
echo                         Build HDF5 C/C++ Libraries and Tools >> build_results_net.txt
echo ***************************************************************************** >> build_results_net.txt

echo Starting Building HDF5 C and C++ Libraries!

cd windows\proj\all
devenv all.sln /rebuild debug >> ..\..\..\all_debug.log
for %%i in (hdf5_cpp hdf5_hl_cpp testhdf5_cpp dsets_cpp hl_test_table_cpp) do (
devenv all.sln /build debug /project %%i >>..\..\..\all_debug.log
devenv all.sln /build debug /project %%idll >>..\..\..\all_debug.log
)

devenv all.sln /rebuild release >>  ..\..\..\all_release.log
for %%i in (hdf5_cpp hdf5_hl_cpp testhdf5_cpp dsets_cpp hl_test_table_cpp) do (
devenv all.sln /build release /project %%i >>..\..\..\all_release.log
devenv all.sln /build release /project %%idll >>..\..\..\all_release.log

)

cd ..\..\..\

more all_debug.log >> build_results_net.txt
more all_release.log >> build_results_net.txt
del all_debug.log
del all_release.log
GOTO END

:WRONG
echo.
echo. The syntax of the command is incorrect.

:HELP
echo.
echo Please use one of the following options!
echo.
echo.    hdf5build_net                   Build HDF5 C Library and Tools
echo.    hdf5build_net enablecpp         Build HDF5 C/C++ Libraries and Tools
echo.    hdf5build /?                    Help information
echo.
:END
