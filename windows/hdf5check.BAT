@REM File Name: hdf5check.bat
@REM This batch file is used to test HDF5 Libraries and Tools.
@REM There are 4 options for this batch file:
@REM   1. hdf5check                -- HDF5 tools and c library tests
@REM   2. hdf5check enablecpp      -- HDF5 tools and c/c++ library tests
@REM   3. hdf5check enablefortran  -- HDF5 tools and c/fortran library tests
@REM   4. hdf5check enableall      -- HDF5 tools and c/c++/fortran library tests
@REM By Xuan Bai
@REM Created: Aug. 12, 2004
@REM Last Updated: Aug. 16, 2004

@ECHO OFF
echo install_dll
call install_dll

echo install_hldll
call install_hldll

type nul > check_results.txt
echo ***************************************************************************** >> check_results.txt
echo                         HDF5 C Library Tests -- Release>> check_results.txt
echo ***************************************************************************** >> check_results.txt

cd test
echo hdf5test release
call hdf5test release >> ..\check_results.txt

echo. >> ..\check_results.txt
echo ***************************************************************************** >> ..\check_results.txt
echo                         HDF5 Timing Test -- Release>> ..\check_results.txt
echo ***************************************************************************** >> ..\check_results.txt
echo hdf5timingtest release
call hdf5timingtest release >> ..\check_results.txt

cd ..\tools
echo. >> ..\check_results.txt
echo testhdf5tools Release
call testhdf5tools Release
more toolstest_release.txt >> ..\check_results.txt
del toolstest_release.txt
cd ..

echo ***************************************************************************** >> check_results.txt
echo                         HDF5 High Level C Library Tests -- Release>> check_results.txt
echo ***************************************************************************** >> check_results.txt

cd hl\test
echo test_hdf5_hl Release
call test_hdf5_hl Release >> ..\..\check_results.txt
cd ..\..

echo. >> check_results.txt
echo ***************************************************************************** >> check_results.txt
echo                         HDF5 C Library Tests -- Debug >> check_results.txt
echo ***************************************************************************** >> check_results.txt

cd test
echo hdf5test debug
call hdf5test debug >> ..\check_results.txt

echo. >> ..\check_results.txt
echo ***************************************************************************** >> ..\check_results.txt
echo                         HDF5 Timing Test -- Debug>> ..\check_results.txt
echo ***************************************************************************** >> ..\check_results.txt
echo hdf5timingtest debug
call hdf5timingtest debug >> ..\check_results.txt

cd ..\tools
echo. >> ..\check_results.txt
echo testhdf5tools Debug 
call testhdf5tools Debug
more toolstest_debug.txt >> ..\check_results.txt
del toolstest_debug.txt
cd ..

echo ***************************************************************************** >> check_results.txt
echo                         HDF5 High Level C Library Tests -- Debug>> check_results.txt
echo ***************************************************************************** >> check_results.txt

cd hl\test
echo test_hdf5_hl Debug
call test_hdf5_hl Debug >> ..\..\check_results.txt
cd ..\..

echo. >> check_results.txt
echo ***************************************************************************** >> check_results.txt
echo                         HDF5 C Library Tests -- Release DLL >> check_results.txt
echo ***************************************************************************** >> check_results.txt

cd test
echo hdf5test release dll
call hdf5test release dll >> ..\check_results.txt

echo. >> ..\check_results.txt
echo ***************************************************************************** >> ..\check_results.txt
echo                         HDF5 Timing Test -- Release DLL>> ..\check_results.txt
echo ***************************************************************************** >> ..\check_results.txt
echo hdf5timingtest release dll
call hdf5timingtest release dll >> ..\check_results.txt

cd ..\tools
echo. >> ..\check_results.txt
echo testhdf5tools Release dll
call testhdf5tools Release dll
more toolstest_releasedll.txt >> ..\check_results.txt
del toolstest_releasedll.txt
cd ..
echo. >> check_results.txt

echo ***************************************************************************** >> check_results.txt
echo                         HDF5 High Level C Library Tests -- Release DLL>> check_results.txt
echo ***************************************************************************** >> check_results.txt

cd hl\test
echo test_hdf5_hl Release dll
call test_hdf5_hl Release dll>> ..\..\check_results.txt
cd ..\..

echo. >> check_results.txt
echo ***************************************************************************** >> check_results.txt
echo                         HDF5 C Library Tests -- Debug DLL >> check_results.txt
echo ***************************************************************************** >> check_results.txt

cd test
echo hdf5test debug dll
call hdf5test debug dll >> ..\check_results.txt

echo. >> ..\check_results.txt
echo ***************************************************************************** >> ..\check_results.txt
echo                         HDF5 Timing Test -- Debug DLL>> ..\check_results.txt
echo ***************************************************************************** >> ..\check_results.txt
echo hdf5timingtest debug dll
call hdf5timingtest debug dll >> ..\check_results.txt

cd ..\tools
echo. >> ..\check_results.txt
echo testhdf5tools Debug dll
call testhdf5tools Debug dll
more toolstest_debugdll.txt >> ..\check_results.txt
del toolstest_debugdll.txt
cd ..

echo ***************************************************************************** >> check_results.txt
echo                         HDF5 High Level C Library Tests -- Debug DLL>> check_results.txt
echo ***************************************************************************** >> check_results.txt

cd hl\test
echo test_hdf5_hl Debug dll
call test_hdf5_hl Debug dll>> ..\..\check_results.txt
cd ..\..

echo. >> check_results.txt

if "%1"=="enablecpp" (
echo. >> check_results.txt
echo install_cppdll
call install_cppdll
echo install_hlcppdll
call install_hlcppdll
cd c++\test
echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 C++ Library Tests -- Release >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo hdf5cpptest release
call hdf5cpptest release >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 High Level C++ Library Tests -- Release >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
cd ..\..\hl\c++\test
echo test_hdf5_hl_cpp Release
call test_hdf5_hl_cpp Release >> ..\..\..\check_results.txt
cd ..\..\..\c++\test
echo. >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 C++ Library Tests -- Debug >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo hdf5cpptest debug
call hdf5cpptest debug >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 High Level C++ Library Tests -- Debug >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
cd ..\..\hl\c++\test
echo test_hdf5_hl_cpp Debug
call test_hdf5_hl_cpp Debug >> ..\..\..\check_results.txt
cd ..\..\..\c++\test
echo. >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 C++ Library Tests -- Release DLL >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo hdf5cpptest release dll
call hdf5cpptest release dll >> ..\..\check_results.txt
echo. >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 High Level C++ Library Tests -- Release DLL >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
cd ..\..\hl\c++\test
echo test_hdf5_hl_cpp Release dll
call test_hdf5_hl_cpp Release dll>> ..\..\..\check_results.txt
cd ..\..\..\c++\test
echo. >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 C++ Library Tests -- Debug DLL >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo hdf5cpptest debug dll
call hdf5cpptest debug dll >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 High Level C++ Library Tests -- Debug DLL>> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
cd ..\..\hl\c++\test
echo test_hdf5_hl_cpp Debug dll
call test_hdf5_hl_cpp Debug dll>> ..\..\..\check_results.txt
echo. >> ..\..\..\check_results.txt

cd ..\..\..\
)


if "%1"=="enablefortran" (
echo. >> check_results.txt
echo install_f90dll
call install_f90dll
echo install_hlf90dll
call install_hlf90dll
cd fortran\test
echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 Fortran Library Tests -- Release >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo testhdf5_fortran release
call testhdf5_fortran release >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 High Level Fortran Library Tests -- Release >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt

cd ..\..\hl\fortran\test
echo test_hdf5_hl_fortran Release
call test_hdf5_hl_fortran Release >> ..\..\..\check_results.txt

cd ..\..\..\fortran\test
echo. >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 Fortran Library Tests -- Debug >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo testhdf5_fortran debug
call testhdf5_fortran debug >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 High Level Fortran Library Tests -- Debug >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt

cd ..\..\hl\fortran\test
echo test_hdf5_hl_fortran Debug
call test_hdf5_hl_fortran Debug >> ..\..\..\check_results.txt

cd ..\..\..\fortran\test
echo. >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 Fortran Library Tests -- Release DLL >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo testhdf5_fortran release dll
call testhdf5_fortran release dll >> ..\..\check_results.txt
echo. >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 Hight Level Fortran Library Tests -- Release DLL >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
cd ..\..\hl\fortran\test
echo test_hdf5_hl_fortran release dll
call test_hdf5_hl_fortran release dll >> ..\..\..\check_results.txt
cd ..\..\..\fortran\test
echo. >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 Fortran Library Tests -- Debug DLL >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo testhdf5_fortran debug dll
call testhdf5_fortran debug dll >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 Hight Level Fortran Library Tests -- Debug DLL >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
cd ..\..\hl\fortran\test
echo test_hdf5_hl_fortran release dll
call test_hdf5_hl_fortran release dll >> ..\..\..\check_results.txt

echo. >> ..\..\..\check_results.txt
cd ..\..\..\
)


if "%1"=="enableall" (
echo. >> check_results.txt
echo install_cppdll
call install_cppdll
echo install_hlcppdll
call install_hlcppdll
cd c++\test
echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 C++ Library Tests -- Release >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo hdf5cpptest release
call hdf5cpptest release >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 High Level C++ Library Tests -- Release >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
cd ..\..\hl\c++\test
echo test_hdf5_hl_cpp Release
call test_hdf5_hl_cpp Release >> ..\..\..\check_results.txt
cd ..\..\..\c++\test
echo. >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 C++ Library Tests -- Debug >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo hdf5cpptest debug
call hdf5cpptest debug >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 High Level C++ Library Tests -- Debug >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
cd ..\..\hl\c++\test
echo test_hdf5_hl_cpp Debug
call test_hdf5_hl_cpp Debug >> ..\..\..\check_results.txt
cd ..\..\..\c++\test
echo. >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 C++ Library Tests -- Release DLL >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo hdf5cpptest release dll
call hdf5cpptest release dll >> ..\..\check_results.txt
echo. >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 High Level C++ Library Tests -- Release DLL >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
cd ..\..\hl\c++\test
echo test_hdf5_hl_cpp Release dll
call test_hdf5_hl_cpp Release dll>> ..\..\..\check_results.txt
cd ..\..\..\c++\test
echo. >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 C++ Library Tests -- Debug DLL >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo hdf5cpptest debug dll
call hdf5cpptest debug dll >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 High Level C++ Library Tests -- Debug DLL >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
cd ..\..\hl\c++\test
echo test_hdf5_hl_cpp Debug dll
call test_hdf5_hl_cpp Debug dll>> ..\..\..\check_results.txt
echo. >> ..\..\..\check_results.txt

cd ..\..\..\

echo install_f90dll
call install_f90dll
echo install_hlf90dll
call install_hlf90dll
cd fortran\test
echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 Fortran Library Tests -- Release >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo testhdf5_fortran release
call testhdf5_fortran release >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 High Level Fortran Library Tests -- Release >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt

cd ..\..\hl\fortran\test
echo test_hdf5_hl_fortran Release
call test_hdf5_hl_fortran Release >> ..\..\..\check_results.txt

cd ..\..\..\fortran\test
echo. >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 Fortran Library Tests -- Debug >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo testhdf5_fortran debug
call testhdf5_fortran debug >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 High Level Fortran Library Tests -- Debug >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt

cd ..\..\hl\fortran\test
echo test_hdf5_hl_fortran Debug
call test_hdf5_hl_fortran Debug >> ..\..\..\check_results.txt

cd ..\..\..\fortran\test
echo. >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 Fortran Library Tests -- Release DLL >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo testhdf5_fortran release dll
call testhdf5_fortran release dll >> ..\..\check_results.txt
echo. >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 Hight Level Fortran Library Tests -- Release DLL >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
cd ..\..\hl\fortran\test
echo test_hdf5_hl_fortran release dll
call test_hdf5_hl_fortran release dll >> ..\..\..\check_results.txt
cd ..\..\..\fortran\test
echo. >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 Fortran Library Tests -- Debug DLL >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
echo testhdf5_fortran debug dll
call testhdf5_fortran debug dll >> ..\..\check_results.txt

echo ***************************************************************************** >> ..\..\check_results.txt
echo                         HDF5 High Level Fortran Library Tests -- Debug DLL >> ..\..\check_results.txt
echo ***************************************************************************** >> ..\..\check_results.txt
cd ..\..\hl\fortran\test
echo test_hdf5_hl_fortran release dll
call test_hdf5_hl_fortran release dll >> ..\..\..\check_results.txt

echo. >> ..\..\..\check_results.txt
cd ..\..\..\
)

type nul > number_failed.txt
type nul > tests_results.txt
find /C "FAIL" check_results.txt > number_failed.txt
find "FAIL" check_results.txt > tests_failed.txt
for /f "tokens=1,2*" %%a in (
 number_failed.txt
) do ( if %%c==0 (
          echo All HDF5 Tests Passed! >> tests_results.txt
)      else (
          echo The Following HDF5 Tests Failed: >> tests_results.txt
          more /e +2 tests_failed.txt >> tests_results.txt
)
)

del number_failed.txt
del tests_failed.txt
