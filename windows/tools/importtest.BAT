@REM Copyright by the Board of Trustees of the University of Illinois.
@REM All rights reserved.
@REM
@REM This file is part of HDF5.  The full HDF5 copyright notice, including
@REM terms governing use, modification, and redistribution, is contained in
@REM the files COPYING and Copyright.html.  COPYING can be found at the root
@REM of the source code distribution tree; Copyright.html can be found at the
@REM root level of an installed copy of the electronic HDF5 document set and
@REM is linked from the top-level documents page.  It can also be found at
@REM http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
@REM access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.

@echo off

::This batch file is for h5import tests
::Usage
:: importtest release  -- release static version
:: importtest debug  -- debug static version
:: importtest release dll -- release dll version
:: importtest debug dll -- debug dll version
:: Written By: Fang GUO
:: Date      : Jan. 27, 2006

:: Track total number of testing errors
   set /A totalerr=0
   set /A totalskip=0

::**********************************************
:: Generate a new temp directory for test h5diff
::**********************************************

:: Make a tempory directory for difftest
		mkdir temptest

:: Run h5importtst
cd testfiles\h5importtst\%1
h5importtst.exe

cd ..\..\..\

::****************************************************
:: Set the Environment Variables & Change Dir to .EXE
::****************************************************

		:: The first incoming parameter should be configuration (debug or release)
		   set p1=%1
		:: The second parameter should be the types of library (dll or blank)
		   set p2=%2
			
    :: Set up some variables for convenience
       :: tempResults will contain the tests results temporarily
       set tempResults=importtest%2_%1.txt
       :: exefile will contain the file name of the executable file
       set exefile=h5import%p2%
       :: tooltest is the sub-batch file which will be called by tool test
       set tooltest=..\..\tooltest
       ::Handle the expected file and the actual output file
       ::ln is the total line numbers to be deleted from the actual output
       set ln=1
       ::ln_exp is the total line numbers to be deleted from the expected output
			 set ln_exp=4

       ::Set some variables to contain .h5 file and its path for convenience
       ::Create the file to contain the tests output
       type nul > %tempResults%
       
       :: Set up Variables
       :: h5import%2 ..\..\testfiles\h5importtst\%1\txtin32 -c ..\..\h5import\testfiles\textin32 -o ..\..\temptest\act_h5\test1.h5

       ::Set up variables for h5import input files
       for %%i in (testfiles\h5importtst\%1\t*) do set %%~ni=..\..\testfiles\h5importtst\%p1%\%%~ni
       for %%i in (testfiles\h5importtst\%1\b*) do set %%~ni=..\..\testfiles\h5importtst\%p1%\%%~ni

       ::Set up variables for h5import configure files
       for %%i in (h5import\testfiles\text*) do set %%~ni=..\..\h5import\testfiles\%%~ni
       for %%i in (h5import\testfiles\con*) do set %%~ni=..\..\h5import\testfiles\%%~ni
       for %%i in (in1 fp1 fp2) do ( set %%i=..\..\h5import\testfiles\%%i)

       ::Set up variables for h5import output files
       for %%i in (h5import\testfiles\test*) do set out.%%~ni=..\..\temptest\out.%%~ni.h5
       
       ::Change directory to the directory where the .exe file is located
       cd h5import%p2%\%p1%

::*********************************************
:: Test each function by calling TOOLTEST.BAT
::*********************************************
::USAGE of importtest.bat
::1. Set flag
::2. Call batch file tooltest.bat followed with the expected file name
::3. Example


echo. 
echo.****************************************************
echo.       h5import%2 %1   T E S T S            
echo.****************************************************

::TESTING "ASCII I32 rank 3 - Output BE
set flag=%txtin32% -c %textin32% -o %out.test1%
call %tooltest% textin32 test1.h5

::TESTING "ASCII I16 rank 3 - Output LE - CHUNKED - extended" 
set flag=%txtin16% -c %textin16% -o %out.test2%
call %tooltest% textin16.txt test2.h5

::TESTING "ASCII I8 - rank 3 - Output I16 LE-Chunked+Extended+Compressed " 
set flag=%txtin16% -c %textin8% -o %out.test3%
call %tooltest% textin8.txt test3.h5

::TESTING "ASCII UI32 - rank 3 - Output BE" 
set flag=%in1% -c %textuin32% -o %out.test4%
call %tooltest% textuin32.txt test4.h5

::TESTING "ASCII UI16 - rank 2 - Output LE+Chunked+Compressed " 
set flag=%in1% -c %textuin16% -o %out.test5%
call %tooltest% textuin16.txt test5.h5

::TESTING "ASCII F32 - rank 3 - Output LE " 
set flag=%fp1% -c %textfp32% -o %out.test6%
call %tooltest% textfp32.txt test6.h5

::TESTING "ASCII F64 - rank 3 - Output BE + CHUNKED+Extended+Compressed " 
set flag=%fp2% -c %textfp64% -o %out.test7%
call %tooltest% textfp64.txt test7.h5

::TESTING "BINARY F64 - rank 3 - Output LE+CHUNKED+Extended+Compressed " 
set flag=%bfp64% -c %conbfp64% -o %out.test8%
call %tooltest% conbfp64.txt test8.h5

::TESTING "BINARY I16 - rank 3 - Output order LE + CHUNKED + extended " 
set flag=%bin16% -c %conbin16% -o %out.test9%
call %tooltest% conbin16.txt test9.h5

::TESTING "BINARY I8 - rank 3 - Output I16LE + Chunked+Extended+Compressed " 
set flag=%bin8% -c %conbin8% -o %out.test10%
call %tooltest% conbin8.txt test10.h5

::TESTING "BINARY I32 - rank 3 - Output BE + CHUNKED " 
set flag=%bin32% -c %conbin32% -o %out.test11%
call %tooltest% conbin32.txt test11.h5

::TESTING "BINARY UI16 - rank 3 - Output byte BE + CHUNKED " 
set flag=%buin16% -c %conbuin16% -o %out.test12%
call %tooltest% conbuin.txt test12.h5

::TESTING "BINARY UI32 - rank 3 - Output LE + CHUNKED " 
set flag=%buin32% -c %conbuin32% -o %out.test13%
call %tooltest% conbuin32.txt test13.h5

::Change directory to hdf5\tools
cd ..\..

::Check if any of tests failed or skiped for the time being
echo.
if "%totalerr%"=="0" (
echo. All of the %exefile% %p1% Tests Passed!
echo. All of the %exefile% %p1% Tests Passed! >> %tempResults%
) else (
echo. %exefile% %p1% Tests Finished with %totalerr% Errors!  
echo. %exefile% %p1% Tests Finished with %totalerr% Errors!>> %tempResults%
)

if not "%totalskip%"=="0" (
echo. %totalskip% Tests in total Skiped!  >> %tempResults%
echo. %totalskip% Tests in total Skiped!
find "SKIPED" %tempResults% | more +2
)

::Clean up Environment Variables
for %%i in (p1 p2 tempResults exefile tooltest ln ln_exp totalskip totalerr) do set %%i=
set flag=

::Clean up the variables set for h5import input files
for %%i in (testfiles\h5importtst\%1\t*) do set %%~ni=
for %%i in (testfiles\h5importtst\%1\b*) do set %%~ni=

::Clean up the variables set for h5import configure files
for %%i in (h5import\testfiles\text*) do set %%~ni=
for %%i in (h5import\testfiles\con*) do set %%~ni=
for %%i in (in1 fp1 fp2) do ( set %%i=..\..\h5import\testfiles\%%i)

::Clean up the variables set for h5import output files
for %%i in (h5import\testfiles\test*) do (
set out.%%~ni=
)
::Clean up Temporary Files
rmdir /s/q temptest


