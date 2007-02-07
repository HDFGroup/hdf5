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


@REM File Name : hdf5build_inelc.bat
@REM Purpose   : Building HDF5 with INTEL C++ Compiler
@REM Written By: Fang GUO
@REM Date      : May 27, 2005
@REM Update    : June 7, 2005

@REM There are 2 options for this batch file:
@REM   1. hdf5build_intelc               -- Build HDF5 tools and c library
@REM   2. hdf5build_intelc enablecpp      -- Build HDF5 tools and c/c++ library
@echo off

if "%1"=="/?" GOTO HELP
if not %1.==. (
	if not "%1"=="/?" (
				if not "%1"=="enablecpp" goto WRONG
	)
)

type nul > build_results_intelc.txt
type nul > all_debug.log
type nul > all_release.log
type nul > h5tinit.log

call convert_to_vcproj
call convert_to_icproj

echo ***************************************************************************** >> build_results_intelc.txt
echo                         Build H5Tinit.exe >> build_results_intelc.txt
echo ***************************************************************************** >> build_results_intelc.txt

cd windows\misc\typegen
devenv typegen.sln /rebuild Debug /IntelSpecific Intel >> ..\..\..\h5tinit.log
cd ..\..\..\
more h5tinit.log >> build_results_intelc.txt
del h5tinit.log
cd src
h5tinit.exe > h5tinit.c
cd ..\

if %1.==. GOTO BUILDC
if "%1"=="enablecpp" GOTO BUILDCPP

:BUILDC

echo Start Building HDF5 C libraries ...

echo ***************************************************************************** >> build_results_intelc.txt
echo                         Build HDF5 C Library and Tools >> build_results_intelc.txt
echo ***************************************************************************** >> build_results_intelc.txt

cd windows\proj\all
devenv all.sln /rebuild debug /IntelSpecific Intel >> ..\..\..\all_debug.log
devenv all.sln /rebuild release /IntelSpecific Intel >> ..\..\..\all_release.log 
cd ..\..\..\
more all_debug.log >> build_results_intelc.txt
more all_release.log >> build_results_intelc.txt
del all_debug.log
del all_release.log
echo. 
echo. End Building HDF5 C Libraries!
GOTO END

:BUILDCPP
echo.
echo. Start Building HDF5 C/C++ libraries Using Intel ...
echo ***************************************************************************** >> build_results_intelc.txt
echo                         Build HDF5 C/C++ Libraries and Tools >> build_results_intelc.txt
echo ***************************************************************************** >> build_results_intelc.txt

cd windows\proj\all
devenv all.sln /rebuild debug /IntelSpecific Intel >> ..\..\..\all_debug.log

for %%i in (hdf5_cpp hdf5_hl_cpp testhdf5_cpp dsets_cpp hl_test_table_cpp) do (
devenv all.sln /build debug /project %%i /IntelSpecific Intel>>..\..\..\all_debug.log
devenv all.sln /build debug /project %%idll /IntelSpecific Intel>>..\..\..\all_debug.log
)

devenv all.sln /rebuild release /IntelSpecific Intel >> ..\..\..\all_release.log
for %%i in (hdf5_cpp hdf5_hl_cpp testhdf5_cpp dsets_cpp hl_test_table_cpp) do (
devenv all.sln /build release /project %%i /IntelSpecific Intel>>..\..\..\all_release.log
devenv all.sln /build release /project %%idll /IntelSpecific Intel>>..\..\..\all_release.log
)

cd ..\..\..\

more all_debug.log >> build_results_intelc.txt
more all_release.log >> build_results_intelc.txt
del all_debug.log
del all_release.log
echo.
echo. End Building C++ Libraries
GOTO END

:WRONG
echo.
echo. The syntax of the command is incorrect.

:HELP
echo.
echo. Please use one of the following options!
echo.
echo.    hdf5build_intelc                   Build HDF5 C Library and Tools
echo.    hdf5build_intelc enablecpp         Build HDF5 C/C++ Libraries and Tools
echo.    hdf5build_intelc /?                Help information
echo.

:END
