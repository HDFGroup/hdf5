@ECHO OFF
REM This batch file is used to test HDF5 C examples.
REM by Xuan Bai
REM Created: 09/09/2004
REM Last Modified: 10/16/2004

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
cd examplesREL
attributetest >> ..\%1.txt
compoundtest >> ..\%1.txt
extendwritetest >> ..\%1.txt
grouptest >> ..\%1.txt
selectest >> ..\%1.txt
writetest >> ..\%1.txt
chunkread >> ..\%1.txt
readtest >> ..\%1.txt
cd ..
more /e +3 testExamples_exp_output.txt  > output.txt
fc %1.txt output.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo All HDF5 C examples tests passed.
) else (
   echo HDF5 C examples tests failed.
   echo.
   more temp.txt
)
del output.txt
del temp.txt
GOTO END

:RELDLL
type nul > %1.txt
cd examplesRELDLL
attributetestdll >> ..\%1.txt
compoundtestdll >> ..\%1.txt
extendwritetestdll >> ..\%1.txt
grouptestdll >> ..\%1.txt
selectestdll >> ..\%1.txt
writetestdll >> ..\%1.txt
chunkreaddll >> ..\%1.txt
readtestdll >> ..\%1.txt
cd ..
more /e +3 testExamples_exp_output.txt  > output.txt
fc %1.txt output.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo All HDF5 C examples tests passed.
) else (
   echo HDF5 C examples tests failed.
   echo.
   more temp.txt
)
del output.txt
del temp.txt
GOTO END

:DEBUG
if %2.==. GOTO DBG
if %2==dll GOTO DBGDLL
GOTO WRONG

:DBG
type nul > %1.txt
cd examplesDBG
attributetest >> ..\%1.txt
compoundtest >> ..\%1.txt
extendwritetest >> ..\%1.txt
grouptest >> ..\%1.txt
selectest >> ..\%1.txt
writetest >> ..\%1.txt
chunkread >> ..\%1.txt
readtest >> ..\%1.txt
cd ..
more /e +3 testExamples_exp_output.txt  > output.txt
fc %1.txt output.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo All HDF5 C examples tests passed.
) else (
   echo HDF5 C examples tests failed.
   echo.
   more temp.txt
)
del output.txt
del temp.txt
GOTO END

:DBGDLL
type nul > %1.txt
cd examplesDBGDLL
attributetestdll >> ..\%1.txt
compoundtestdll >> ..\%1.txt
extendwritetestdll >> ..\%1.txt
grouptestdll >> ..\%1.txt
selectestdll >> ..\%1.txt
writetestdll >> ..\%1.txt
chunkreaddll >> ..\%1.txt
readtestdll >> ..\%1.txt
cd ..
more /e +3 testExamples_exp_output.txt  > output.txt
fc %1.txt output.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo All HDF5 C examples tests passed.
) else (
   echo HDF5 C examples tests failed.
   echo.
   more temp.txt
)
del output.txt
del temp.txt
GOTO END

:WRONG
echo The syntax of the command is incorrect.
echo.

:HELP
echo Tests HDF5 C examples.
echo.
echo testExamples [OPTION]
echo.
echo Please use one of the following options!
echo.
echo    testExamples release        test HDF5 C examples -- release version
echo    testExamples release dll    test HDF5 C examples -- release dll version
echo    testExamples debug          test HDF5 C examples -- debug version
echo    testExamples debug dll      test HDF5 C examples -- debug dll version
echo    testExamples /?             Help information
echo.

:END