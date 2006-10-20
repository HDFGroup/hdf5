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

::This batch file is for h5stat tests.
::Usage
:: stattest release  -- release static version
:: stattest debug  -- debug static version
:: stattest release dll -- release dll version
:: stattest debug dll -- debug dll version
:: Written By:MuQun Yang
:: Date      : Oct. 18, 2006

:: Track total number of testing errors
   set /A totalerr=0
   set /A totalskip=0

::**********************************************
:: Generate a new temp directory for test h5stat
::**********************************************

:: Make a tempory directory for stattest
		mkdir temptest

::****************************************************
:: Set the Environment Variables & Change Dir to .EXE
::****************************************************

		:: The first incoming parameter should be configuration (debug or release)
		   set p1=%1
		:: The second parameter should be the types of library (dll or blank)
		   set p2=%2
			
    :: Set up some variables for convenience
       :: tempResults will contain the tests results temporarily
       set tempResults=stattest%p2%_%p1%.txt
       :: exefile will contain the file name of the executable file
       set exefile=h5stat%p2%
       :: tooltest is the sub-batch file which will be called by tool test
       set tooltest=..\..\tooltest
       ::Handle the expected file and the actual output file
       ::ln is the total line numbers to be deleted from the actual output
       set ln=1
       ::ln_exp is the total line numbers to be deleted from the expected output
			 set ln_exp=4
       
       :: NOTE1
       ::Set some variables to contain .h5 file and its path for convenience
       
       set file1=..\..\misc\testfiles\h5stat_filters.h5     

       ::Create the file to contain the tests output
       type nul > %tempResults%
       
       ::Change directory to the directory where the .exe file is located
       cd h5stat%p2%\%p1%

::*********************************************
:: Test each function by calling TOOLTEST.BAT
::*********************************************
::USAGE of stattest.bat
::1. Set flag
::2. Call batch file tooltest.bat followed with the expected file name
::3. Example
::If we want to test h5stat \testfiles\file1.h5 
::At first, we need to set the flag like
::set flag=%file1% 
::Secondly, we call batch file tooltest.bat by
::call %tooltest% h5stat_1.txt

echo. 
echo.****************************************************
echo.       H5stat%2 %1   T E S T S            
echo.****************************************************
 
::--------------
::Common usage
::--------------

::1.0 help message
set flag=-h
call %tooltest% h5stat_help1.ddl 

::1.1 another way to generate "help message"
set flag=--help
call %tooltest% h5stat_help2.ddl

::1.2 all statisical information
set flag=%file1% 
call %tooltest% h5stat_filters.ddl   

::1.3 print file information
set flag=-f %file1% 
call %tooltest% h5stat_filters-file.ddl 
 
::1.4 print file metadata information
set flag=-F %file1% 
call %tooltest% h5stat_filters-F.ddl 
 
::1.5 print dataset information
set flag=-d %file1% 
call %tooltest% h5stat_filters-d.ddl 
 
::1.6 print group information
set flag=-g %file1% 
call %tooltest% h5stat_filters-g.ddl 
 
::1.7 print datatype metadata
set flag=%file1% -dT
::call %tooltest% h5stat_filters-dT.ddl 
 

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

::NOTE1 CLEAN
::Clean up Environment Variables
for %%i in (p1 p2 tempResults exefile tooltest ln ln_exp totalskip totalerr file1) do set %%i=
set flag=

::Clean up Temporary Files
:: rmdir /s/q temptest


