@ECHO OFF
REM This batch file is used to test HDF5 C++ examples.
REM By Xuan Bai
REM Created on: 10/20/2004
REM Last Modified: 10/20/2004

if %1.==. GOTO WRONG
if "%1"=="/?" GOTO HELP
if %1==release GOTO RELEASE
if %1==debug GOTO DEBUG
GOTO WRONG

:RELEASE
if %2.==. GOTO REL
if %2==dll GOTO RELDLL
GOTO WRONG

:REL
type nul > %1.txt
cd cppexamplesREL
createtest >> ..\%1.txt
readdatatest >> ..\%1.txt
writedatatest >> ..\%1.txt
compoundtest >> ..\%1.txt
extend_dstest >> ..\%1.txt
chunkstest >> ..\%1.txt
h5grouptest >> ..\%1.txt
cd ..
fc %1.txt expected.out >temp.txt
if %ERRORLEVEL%==0 (
   echo All HDF5 C++ examples tests passed.
) else (
   echo HDF5 C++ examples tests failed.
   more temp.txt
)
del temp.txt
GOTO END

:RELDLL
type nul > %1.txt
cd cppexamplesRELDLL
createtestdll >> ..\%1.txt
readdatatestdll >> ..\%1.txt
writedatatestdll >> ..\%1.txt
compoundtestdll >> ..\%1.txt
extend_dstestdll >> ..\%1.txt
chunkstestdll >> ..\%1.txt
h5grouptestdll >> ..\%1.txt
cd ..
fc %1.txt expected.out >temp.txt
if %ERRORLEVEL%==0 (
   echo All HDF5 C++ examples tests passed.
) else (
   echo HDF5 C++ examples tests failed.
   more temp.txt
)
del temp.txt
GOTO END

:DEBUG
if %2.==. GOTO DBG
if %2==dll GOTO DBGDLL
GOTO WRONG

:DBG
type nul > %1.txt
cd cppexamplesDBG
createtest >> ..\%1.txt
readdatatest >> ..\%1.txt
writedatatest >> ..\%1.txt
compoundtest >> ..\%1.txt
extend_dstest >> ..\%1.txt
chunkstest >> ..\%1.txt
h5grouptest >> ..\%1.txt
cd ..
fc %1.txt expected.out >temp.txt
if %ERRORLEVEL%==0 (
   echo All HDF5 C++ examples tests passed.
) else (
   echo HDF5 C++ examples tests failed.
   more temp.txt
)
del temp.txt
GOTO END

:DBGDLL
type nul > %1.txt
cd cppexamplesDBGDLL
createtestdll >> ..\%1.txt
readdatatestdll >> ..\%1.txt
writedatatestdll >> ..\%1.txt
compoundtestdll >> ..\%1.txt
extend_dstestdll >> ..\%1.txt
chunkstestdll >> ..\%1.txt
h5grouptestdll >> ..\%1.txt
cd ..
fc %1.txt expected.out >temp.txt
if %ERRORLEVEL%==0 (
   echo All HDF5 C++ examples tests passed.
) else (
   echo HDF5 C++ examples tests failed.
   more temp.txt
)
del temp.txt
GOTO END

:WRONG
echo The syntax of the command is incorrect.
echo.

:HELP
echo Tests HDF5 C++ examples.
echo.
echo testcppExamples [OPTION]
echo.
echo Please use one of the following options!
echo.
echo    testcppExamples release        test HDF5 C++ examples -- release version
echo    testcppExamples release dll    test HDF5 C++ examples -- release dll version
echo    testcppExamples debug          test HDF5 C++ examples -- debug version
echo    testcppExamples debug dll      test HDF5 C++ examples -- debug dll version
echo    testcppExamples /?             Help information
echo.

:END