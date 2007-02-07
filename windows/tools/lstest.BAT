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

@echo off

::This batch file is for h5ls tests
::Usage
:: lstest release  -- release static version
:: lstest debug  -- debug static version
:: lstest release dll -- release dll version
:: lstest debug dll -- debug dll version
:: Written By: Fang GUO
:: Date      : Jan. 27, 2006


:: Track total number of testing errors
   set /A totalerr=0
   set /A totalskip=0


::*******************************************************
:: Generate a new temp directory for test h5dump 
::*******************************************************
		mkdir temptest

::******************************************************************
:: Set the Environment Variables & Change Dir to Dir with h5dump.EXE
::******************************************************************

		:: The first incoming parameter should be configuration (debug or release)
		   set p1=%1
		:: The second parameter should be the types of library (dll or blank)
		   set p2=%2

    :: Before comparison between expected file and the actual outputfile, 
    :: Ignore the first line in actual outputfile and the first four lines
    :: in the expected output file
       
       set ln=0
			 set ln_exp=3

    :: Save the tests output into a temporary file 			
       set tempResults=lstest%2_%1.txt
       type nul > %tempResults%

    :: Define the .exe file based on the second parameter
       set exefile=h5ls%p2%
    
    :: For convenience, set a variable for the sub batch file 
       set tooltest=..\..\tooltest
   
    :: For convenience, set variable for all testfiles/*.h5 files
     for %%i in (testfiles\*.h5) do set %%~ni=..\..\%%i
    
    :: Change the directory to dir including .exe file
       cd h5ls%p2%\%p1%

::Test h5ls with different options

echo.****************************************************
echo.       h5ls%2 %1   T E S T S            
echo.****************************************************

:: The paramter following %tooltest% is the expected output file
:: Path of the expected output file is not needed

:: Toss in a bunch of tests.  Not sure if they are the right kinds.
:: test the help syntax
set flag=-w80 -h
call %tooltest% help-1.ls

set flag=-w80 -help
call %tooltest% help-2.ls

set flag=-w80 -?
call %tooltest% help-3.ls

:: test simple command
set flag=-w80 %tall%
call %tooltest% tall-1.ls

set flag=-w80 -r -d %tall%
call %tooltest% tall-2.ls

set flag=-w80 %tgroup%
call %tooltest% tgroup.ls

:: test for displaying groups
set flag=-w80 -r -g %tgroup%
call %tooltest% tgroup-1.ls

:: test for displaying simple space datasets
set flag=-w80 -r -d %tdset%
call %tooltest% tdset-1.ls

:: test for displaying soft links
set flag=-w80 -r %tslink%
call %tooltest% tslink-1.ls

:: tests for hard links
set flag=-w80 %thlink%
call %tooltest% thlink-1.ls

:: tests for compound data types
set flag=-w80 -r -d %tcompound%
call %tooltest% tcomp-1.ls

::test for the nested compound type
set flag=-w80 -r -d %tnestedcomp%
call %tooltest% tnestcomp-1.ls

:: test for loop detection
set flag=-w80 -r -d %tloop%
call %tooltest% tloop-1.ls

:: test for string 
set flag=-w80 -r -d %tstr%
call %tooltest% tstr-1.ls

:: test test file created from lib SAF team
set flag=-w80 -r -d %tsaf%
call %tooltest% tsaf.ls

:: test for variable length data types
set flag=-w80 -r -d %tvldtypes1%
call %tooltest% tvldtypes1.ls

:: test for array data types
set flag=-w80 -r -d %tarray1%
call %tooltest% tarray1.ls

:: test for empty data
set flag=-w80 -d %tempty%
call %tooltest% tempty.ls

:: test for all dataset types written to attributes
:: enable -S for avoiding printing NATIVE types
:: need batch mask.bat
set flag=-w80 -v -S %tattr2%
call %tooltest% tattr2.ls MASK

if %totalerr%==0 (
echo. All of the %exefile% %p1% Tests Passed!
echo. All of the %exefile% %p1% Tests Passed! >> %tempResults%
) else (
echo. %exefile% %p1% Tests Finished with %totalerr% Errors!  
echo. %exefile% %p1% Tests Finished with %totalerr% Errors!>> %tempResults%
)

if not %totalskip%==0 (
echo. %totalskip% Tests in total Skiped!  >> %tempResults%
echo. %totalskip% Tests in total Skiped!
echo. 
find "SKIPED" %tempResults% | more +2
)


cd ..\..
rmdir /s/q temptest

for %%i in (testfiles\*.h5) do set %%~ni=
set test_szip=
set output_szip=
for %%v in (p1 p2 tempResults exefile tooltest ln ln_exp tsplit_file tmulti tfamily05d totalskip) do set %%v=

