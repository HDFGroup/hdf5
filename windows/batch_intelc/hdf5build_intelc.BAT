@REM File Name : hdf5build_inelc.bat
@REM Purpose   : Building HDF5 with INTEL C++ Compiler
@REM Written By: Fang GUO
@REM Date      : May 27, 2005
@REM Update    : June 7, 2005

@REM There are 2 options for this batch file:
@REM   1. hdf5build_intelc               -- Build HDF5 tools and c library
@REM   2. hdf5build_intelc enablecpp      -- Build HDF5 tools and c/c++ library
@echo off

if %1.==. GOTO BUILDC
if "%1"=="/?" GOTO HELP
if %1==enablecpp GOTO BUILDCPP
GOTO WRONG

:BUILDC

call convert_to_icproj %1

type nul > build_results_intelc.txt
type nul > all_debug.log
type nul > all_release.log
type nul > h5tinit.log

echo Start Building HDF5 C libraries ...

echo ***************************************************************************** >> build_results_intelc.txt
echo                         Build H5Tinit.exe >> build_results_intelc.txt
echo ***************************************************************************** >> build_results_intelc.txt

cd windows\misc\typegen
ICProjConvert80 typegen.sln /IC >> ..\..\..\h5tinit.log
devenv typegen.sln /rebuild Debug /IntelSpecific Intel >> ..\..\..\h5tinit.log
cd ..\..\..\
more h5tinit.log >> build_results_intelc.txt
del h5tinit.log

echo ***************************************************************************** >> build_results_intelc.txt
echo                         Build HDF5 C Library and Tools >> build_results_intelc.txt
echo ***************************************************************************** >> build_results_intelc.txt

cd src
h5tinit.exe > h5tinit.c

cd ..\windows\proj\all
devenv all.sln /rebuild debug /IntelSpecific Intel >> ..\..\..\all_debug.log
devenv all.sln /rebuild release /IntelSpecific Intel >> ..\..\..\all_release.log 
cd ..\..\..\
more all_debug.log >> build_results_intelc.txt
more all_release.log >> build_results_intelc.txt
del all_debug.log
del all_release.log
GOTO END

:BUILDCPP
call convert_to_icproj
call convert_to_icproj %1

type nul > build_results_intelc.txt
type nul > all_debug.log
type nul > all_release.log
type nul > h5tinit.log

echo ***************************************************************************** >> build_results_intelc.txt
echo                         Build H5Tinit.exe >> build_results_intelc.txt
echo ***************************************************************************** >> build_results_intelc.txt

cd windows\misc\typegen
ICProjConvert80 typegen.sln /IC >> ..\..\..\h5tinit.log
devenv typegen.sln /rebuild Debug /IntelSpecific Intel >> ..\..\..\h5tinit.log
cd ..\..\..\
more h5tinit.log >> build_results_intelc.txt
del h5tinit.log

echo ***************************************************************************** >> build_results_intelc.txt
echo                         Build HDF5 C/C++ Libraries and Tools >> build_results_intelc.txt
echo ***************************************************************************** >> build_results_intelc.txt

cd src
h5tinit.exe > h5tinit.c

cd ..\windows\proj\all
devenv all.sln /rebuild debug /IntelSpecific Intel >> ..\..\..\all_debug.log
devenv all.sln /rebuild release /IntelSpecific Intel >> ..\..\..\all_release.log
cd ..\..\..\

more all_debug.log >> build_results_intelc.txt
more all_release.log >> build_results_intelc.txt
del all_debug.log
del all_release.log
GOTO END

:WRONG
echo The syntax of the command is incorrect.
echo.

:HELP
echo Builds HDF5 Libraries and Tools.
echo.
echo hdf5build [OPTION]
echo.
echo Please use one of the following options!
echo.
echo    hdf5build                   Build HDF5 C Library and Tools
echo    hdf5build enablecpp         Build HDF5 C/C++ Libraries and Tools
echo    hdf5build enablefortran     Build HDF5 C/Fortran Libraries and Tools
echo    hdf5build enableall         Build HDF5 C/C++/Fortran Libraries and Tools
echo    hdf5build /?                Help information

:END
