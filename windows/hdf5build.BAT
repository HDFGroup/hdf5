@REM File Name: hdf5build.bat
@REM This batch file is used to build HDF5 Libraries and Tools.
@REM There are 4 options for this batch file:
@REM   1. hdf5build                -- Build HDF5 tools and c library
@REM   2. hdf5build enablecpp      -- Build HDF5 tools and c/c++ library
@REM   3. hdf5build enablefortran  -- Build HDF5 tools and c/fortran library
@REM   4. hdf5build enableall      -- Build HDF5 tools and c/c++/fortran library
@REM By Xuan Bai
@REM Created: Aug. 16, 2004
@REM Last Updated: Oct. 6, 2004

@ECHO OFF
if %1.==. GOTO BUILDC
if "%1"=="/?" GOTO HELP
if %1==enablecpp GOTO BUILDCPP
if %1==enablefortran GOTO BUILDF90
if %1==enableall GOTO BUILDALL
GOTO WRONG

:BUILDC
type nul > build_results.txt
echo ***************************************************************************** >> build_results.txt
echo                         Build H5Tinit.exe >> build_results.txt
echo ***************************************************************************** >> build_results.txt

cd windows\misc\typegen\h5tinit
msdev h5tinit.dsp  /make "h5tinit - Win32 Debug" /Rebuild /out ..\..\..\..\h5tinit.log
cd ..\..\..\..\
more h5tinit.log >> build_results.txt
del h5tinit.log

echo ***************************************************************************** >> build_results.txt
echo                         Build HDF5 C Library and Tools >> build_results.txt
echo ***************************************************************************** >> build_results.txt

cd src
h5tinit.exe > h5tinit.c

cd ..\windows\proj\all
msdev all.dsw /make "all - ALL" /Rebuild /out ..\..\..\all.log
cd ..\..\..\
more all.log >> build_results.txt
del all.log
GOTO END

:BUILDCPP
type nul > build_results.txt
echo ***************************************************************************** >> build_results.txt
echo                         Build H5Tinit.exe >> build_results.txt
echo ***************************************************************************** >> build_results.txt

cd windows\misc\typegen\h5tinit
msdev h5tinit.dsp  /make "h5tinit - Win32 Debug" /Rebuild /out ..\..\..\..\h5tinit.log
cd ..\..\..\..\
more h5tinit.log >> build_results.txt
del h5tinit.log

echo ***************************************************************************** >> build_results.txt
echo                         Build HDF5 C/C++ Libraries and Tools >> build_results.txt
echo ***************************************************************************** >> build_results.txt

cd src
h5tinit.exe > h5tinit.c

cd ..\windows\proj\all
msdev all.dsw /make "hl_test_table_cpp - ALL" "testhdf5_cppdll - ALL" "testhdf5_cpp - ALL" "dsets_cppdll - ALL" "dsets_cpp - ALL" "all - ALL" /Rebuild /out ..\..\..\all.log
cd ..\..\..\
more all.log >> build_results.txt
del all.log
GOTO END

:BUILDF90
type nul > build_results.txt
echo ***************************************************************************** >> build_results.txt
echo                         Build H5Tinit.exe >> build_results.txt
echo ***************************************************************************** >> build_results.txt

cd windows\misc\typegen\h5tinit
msdev h5tinit.dsp  /make "h5tinit - Win32 Debug" /Rebuild /out ..\..\..\..\h5tinit.log
cd ..\..\..\..\
more h5tinit.log >> build_results.txt
del h5tinit.log

echo ***************************************************************************** >> build_results.txt
echo                         Generate H5f90i_gen.h and H5fortran_types.f90
echo ***************************************************************************** >> build_results.txt

REM Generate H5fortran_detect.f90
cd windows\misc\H5fortran_detect_gen
dfdev H5fortran_detect_gen.dsp /make "H5fortran_detect_gen - Win32 Debug" /Rebuild /out ..\..\..\H5fortran_detect_gen.log
cd ..\..\..\fortran\src
H5fortran_detect_gen.exe > H5fortran_detect.f90
del H5fortran_detect_gen.exe
cd ..\..\
more H5fortran_detect_gen.log >> build_results.txt
del H5fortran_detect_gen.log

REM Generate H5fort_type_defines.h
cd windows\misc\fortrantypegen
dfdev H5fortran_detect.dsp /make "H5fortran_detect - Win32 Debug" /Rebuild /out ..\..\..\H5fortran_detect.log
cd ..\..\..\fortran\src
H5fortran_detect.exe > H5fort_type_defines.h
del H5fortran_detect.exe
cd ..\..\
more H5fortran_detect.log >> build_results.txt
del H5fortran_detect.log

REM Generate H5f90i_gen.h and H5fortran_types.f90
cd windows\misc\matchtypegen
dfdev H5match_types.dsp /make "H5match_types - Win32 Debug" /Rebuild /out ..\..\..\H5match_types.log
cd ..\..\..\fortran\src
H5match_types.exe
del H5match_types.exe
cd ..\..\
more H5match_types.log >> build_results.txt
del H5match_types.log

echo ***************************************************************************** >> build_results.txt
echo                         Build HDF5 C/Fortran Libraries and Tools >> build_results.txt
echo ***************************************************************************** >> build_results.txt

cd src
h5tinit.exe > h5tinit.c

cd ..\windows\proj\all
msdev all.dsw /make "hl_test_image_fortran - ALL" "hl_test_lite_fortran - ALL" "hl_test_table_fortran - ALL" "testhdf5_fortrandll - ALL" "testhdf5_fortran - ALL" "flush2_fortrandll - ALL" "flush2_fortran - ALL" "flush1_fortrandll - ALL" "flush1_fortran - ALL" "all - ALL" /Rebuild /out ..\..\..\all.log
cd ..\..\..\
more all.log >> build_results.txt
del all.log
GOTO END

:BUILDALL
type nul > build_results.txt
echo ***************************************************************************** >> build_results.txt
echo                         Build H5Tinit.exe >> build_results.txt
echo ***************************************************************************** >> build_results.txt

cd windows\misc\typegen\h5tinit
msdev h5tinit.dsp  /make "h5tinit - Win32 Debug" /Rebuild /out ..\..\..\..\h5tinit.log
cd ..\..\..\..\
more h5tinit.log >> build_results.txt
del h5tinit.log
echo ***************************************************************************** >> build_results.txt
echo                         Generate H5f90i_gen.h and H5fortran_types.f90
echo ***************************************************************************** >> build_results.txt

REM Generate H5fortran_detect.f90
cd windows\misc\H5fortran_detect_gen
dfdev H5fortran_detect_gen.dsp /make "H5fortran_detect_gen - Win32 Debug" /Rebuild /out ..\..\..\H5fortran_detect_gen.log
cd ..\..\..\fortran\src
H5fortran_detect_gen.exe > H5fortran_detect.f90
del H5fortran_detect_gen.exe
cd ..\..\
more H5fortran_detect_gen.log >> build_results.txt
del H5fortran_detect_gen.log

REM Generate H5fort_type_defines.h
cd windows\misc\fortrantypegen
dfdev H5fortran_detect.dsp /make "H5fortran_detect - Win32 Debug" /Rebuild /out ..\..\..\H5fortran_detect.log
cd ..\..\..\fortran\src
H5fortran_detect.exe > H5fort_type_defines.h
del H5fortran_detect.exe
cd ..\..\
more H5fortran_detect.log >> build_results.txt
del H5fortran_detect.log

REM Generate H5f90i_gen.h and H5fortran_types.f90
cd windows\misc\matchtypegen
dfdev H5match_types.dsp /make "H5match_types - Win32 Debug" /Rebuild /out ..\..\..\H5match_types.log
cd ..\..\..\fortran\src
H5match_types.exe
del H5match_types.exe
cd ..\..\
more H5match_types.log >> build_results.txt
del H5match_types.log

echo ***************************************************************************** >> build_results.txt
echo                         Build HDF5 C/C++/Fortran Libraries and Tools >> build_results.txt
echo ***************************************************************************** >> build_results.txt

cd src
h5tinit.exe > h5tinit.c

cd ..\windows\proj\all
msdev all.dsw /make "hl_test_image_fortran - ALL" "hl_test_lite_fortran - ALL" "hl_test_table_fortran - ALL" "testhdf5_fortrandll - ALL" "testhdf5_fortran - ALL" "flush2_fortrandll - ALL" "flush2_fortran - ALL" "flush1_fortrandll - ALL" "flush1_fortran - ALL" "testhdf5_cppdll - ALL" "testhdf5_cpp - ALL" "dsets_cppdll - ALL" "dsets_cpp - ALL" "all - ALL" /Rebuild /out ..\..\..\all.log
cd ..\..\..\
more all.log >> build_results.txt
del all.log
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