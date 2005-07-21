@REM File Name : hdf5build_net.bat
@REM Purpose   : Building HDF5 with MSVS .NET
@REM Written By: Fang GUO
@REM Date      : May 27, 2005
@REM Update    : June 7, 2005

@REM There are 2 options for this batch file:
@REM   1. hdf5build_net                -- Build HDF5 tools and c library
@REM   2. hdf5build_net enablecpp      -- Build HDF5 tools and c/c++ library


@ECHO OFF
if %1.==. GOTO BUILDC
if "%1"=="/?" GOTO HELP
if %1==enablecpp GOTO BUILDCPP
GOTO WRONG

:BUILDC

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

echo ***************************************************************************** >> build_results_net.txt
echo                         Build HDF5 C Library and Tools >> build_results_net.txt
echo ***************************************************************************** >> build_results_net.txt

cd src
h5tinit.exe > h5tinit.c

echo Starting Building HDF5 C Libraries!

cd ..\windows\proj\all
devenv all.sln /rebuild debug >> ..\..\..\all_debug.log
devenv all.sln /rebuild release >> ..\..\..\all_release.log
cd ..\..\..\
more all_debug.log >> build_results_net.txt
more all_release.log >> build_results_net.txt
del all_debug.log
del all_release.log
GOTO END

:BUILDCPP

type nul > build_results_net.txt
type nul > all_debug.log
type nul > all_release.log
echo ***************************************************************************** >> build_results_net.txt
echo                         Build H5Tinit.exe >> build_results_net.txt
echo ***************************************************************************** >> build_results_net.txt

cd windows\misc\typegen
devenv typegen.sln /rebuild Debug /project h5tinit /out ..\..\..\h5tinit.log
cd ..\..\..\
more h5tinit.log >> build_results_net.txt
del h5tinit.log

echo ***************************************************************************** >> build_results_net.txt
echo                         Build HDF5 C/C++ Libraries and Tools >> build_results_net.txt
echo ***************************************************************************** >> build_results_net.txt

cd src
h5tinit.exe > h5tinit.c

echo Starting Building HDF5 C and C++ Libraries!

cd ..\windows\proj\all
devenv all.sln /rebuild debug >> ..\..\..\all_debug.log
devenv all.sln /build debug /project hdf5_cppdll >>..\..\..\all_debug.log
devenv all.sln /build debug /project hdf5_cpp >>..\..\..\all_debug.log
devenv all.sln /build debug /project hdf5_hl_cpp >>..\..\..\all_debug.log
::devenv all.sln /build debug /project hdf5_hl_cppdll >>..\..\..\all_debug.log
devenv all.sln /build debug /project testhdf5_cpp  >>..\..\..\all_debug.log
devenv all.sln /build debug /project testhdf5_cppdll  >>..\..\..\all_debug.log
devenv all.sln /build debug /project dsets_cpp  >>..\..\..\all_debug.log
devenv all.sln /build debug /project dsets_cppdll  >>..\..\..\all_debug.log
devenv all.sln /build debug /project hl_test_table_cpp  >>..\..\..\all_debug.log
::devenv all.sln /build debug /project hl_test_table_cppdll  >>..\..\..\all_debug.log

devenv all.sln /rebuild release >>  ..\..\..\all_release.log
devenv all.sln /build release /project hdf5_cppdll >>..\..\..\all_release.log
devenv all.sln /build release /project hdf5_cpp >>..\..\..\all_release.log
devenv all.sln /build release /project hdf5_hl_cpp >>..\..\..\all_release.log
::devenv all.sln /build release /project hdf5_hl_cppdll >>..\..\..\all_release.log
devenv all.sln /build release /project testhdf5_cpp  >>..\..\..\all_release.log
devenv all.sln /build release /project testhdf5_cppdll  >>..\..\..\all_release.log
devenv all.sln /build release /project dsets_cpp  >>..\..\..\all_release.log
devenv all.sln /build release /project dsets_cppdll  >>..\..\..\all_release.log
devenv all.sln /build release /project hl_test_table_cpp  >>..\..\..\all_release.log
::devenv all.sln /build release /project hl_test_table_cppdll  >>..\..\..\all_release.log

cd ..\..\..\

more all_debug.log >> build_results_net.txt
more all_release.log >> build_results_net.txt
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
echo    hdf5build_net                   Build HDF5 C Library and Tools
echo    hdf5build_net enablecpp         Build HDF5 C/C++ Libraries and Tools
echo    hdf5build /?                    Help information

:END