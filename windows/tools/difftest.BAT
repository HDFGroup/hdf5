q@REM Copyright by the Board of Trustees of the University of Illinois.
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

::This batch file is for h5diff tests.
::Usage
:: difftest release  -- release static version
:: difftest debug  -- debug static version
:: difftest release dll -- release dll version
:: difftest debug dll -- debug dll version
:: Written By: Fang GUO
:: Date      : Jan. 12, 2006

:: Track total number of testing errors
   set /A totalerr=0
   set /A totalskip=0

::**********************************************
:: Generate a new temp directory for test h5diff
::**********************************************

:: Make a tempory directory for difftest
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
       set tempResults=difftest%p2%_%p1%.txt
       :: exefile will contain the file name of the executable file
       set exefile=h5diff%p2%
       :: tooltest is the sub-batch file which will be called by tool test
       set tooltest=..\..\tooltest
       ::Handle the expected file and the actual output file
       ::ln is the total line numbers to be deleted from the actual output
       set ln=1
       ::ln_exp is the total line numbers to be deleted from the expected output
			 set ln_exp=4
       
       :: NOTE1
       ::Set some variables to contain .h5 file and its path for convenience
       ::When you add new h5diff tests, check if the .h5 files is inside this for loop.
       ::For example, if you will add a new h5diff test using hdf5\tools\testfiles\file9.h5
       ::You need to add "9" after 8 or somewhere inside the ()
       ::The testing file names have been renamed. The following lines are not valid.
       :: for %%v in (1 2 3 4 5 6 7 8) do set file%%v=..\..\testfiles\file%%v.h5     
       set file1=..\..\testfiles\h5diff_basic1.h5
       set file2=..\..\testfiles\h5diff_basic2.h5
       set file3=..\..\testfiles\h5diff_types.h5
       set file4=..\..\testfiles\h5diff_dtypes.h5
       set file5=..\..\testfiles\h5diff_attr1.h5
       set file6=..\..\testfiles\h5diff_attr2.h5
       set file7=..\..\testfiles\h5diff_dset1.h5
       set file8=..\..\testfiles\h5diff_dset2.h5
       set file9=..\..\testfiles\h5diff_hyper1.h5
       set file10=..\..\testfiles\h5diff_hyper2.h5
       ::Create the file to contain the tests output
       type nul > %tempResults%
       
       ::Change directory to the directory where the .exe file is located
       cd h5diff%p2%\%p1%

::*********************************************
:: Test each function by calling TOOLTEST.BAT
::*********************************************
::USAGE of difftest.bat
::1. Set flag
::2. Call batch file tooltest.bat followed with the expected file name
::3. Example
::If we want to test h5diff ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 g1/dset1 g1/dset2
::At first, we need to set the flag like
::set flag=%file1% %file2% g1/dset1 g1/dset2
::Secondly, we call batch file tooltest.bat by
::call %tooltest% h5diff_12.txt

echo. 
echo.****************************************************
echo.       H5diff%2 %1   T E S T S            
echo.****************************************************
 
::--------------
::Common usage
::--------------

::1.0
set flag=-h
call %tooltest% h5diff_10.txt 

::1.1 normal mode
set flag=%file1% %file2%
call %tooltest% h5diff_11.txt 

::1.2 normal mode with objects
set flag=%file1% %file2% g1/dset1 g1/dset2
call %tooltest% h5diff_12.txt   

::1.3 report mode
set flag=%file1% %file2% -r
call %tooltest% h5diff_13.txt 
 
::1.4 report  mode with objects
set flag=%file1% %file2% -r g1/dset1 g1/dset2
call %tooltest% h5diff_14.txt 
 
::1.5 with -d
set flag=%file1% %file2% -r -d 5 g1/dset3 g1/dset4
call %tooltest% h5diff_15.txt 
 
::1.6 with -p
set flag=%file1% %file1% g1/dset9 g1/dset10 -p 0.01 -v
call %tooltest% h5diff_16.txt 
 
::1.7 verbose mode
set flag=%file1% %file2% -v
call %tooltest% h5diff_17.txt 
 
::1.8 quiet mode 
set flag=%file1% %file2% -q
call %tooltest% h5diff_18.txt 

::#1.9.1 with -p (int)
set flag=%file1% %file1% -v -p 0.02 g1/dset5 g1/dset6
call %tooltest% h5diff_191.txt 

::#1.9.2 with -p (unsigned long_long)
set flag=%file1% %file1% -v -p 0.02 g1/dset7 g1/dset8
call %tooltest% h5diff_192.txt 

::---------------------
::Not Comparable Types
::---------------------

::2.0
set flag=%file3% %file3% -v dset g1
call %tooltest% h5diff_20.txt 

::2.1
set flag=%file3% %file3% -v dset l1
call %tooltest% h5diff_21.txt 
 

::2.2
set flag=%file3% %file3% -v dset t1
call %tooltest% h5diff_22.txt  
 

::2.3
set flag=%file3% %file3% -v g1 g1
call %tooltest% h5diff_23.txt 

::2.4
set flag=%file3% %file3% -v t1 t1
call %tooltest% h5diff_24.txt  
 
::2.5
set flag=%file3% %file3% -v l1 l1
call %tooltest% h5diff_25.txt  

::2.6
set flag=%file3% %file3% -v g1 g2
call %tooltest% h5diff_26.txt 

::2.7
set flag=%file3% %file3% -v t1 t2
call %tooltest% h5diff_27.txt  
 
::2.8
set flag=%file3% %file3% -v l1 l2
call %tooltest% h5diff_28.txt  



::-------------
:: Dataset types
:: -------------

::5.0
set flag=%file4% %file4% -v dset0a dset0b
call %tooltest% h5diff_50.txt  
 

::5.1
set flag=%file4% %file4% -v dset1a dset1b
call %tooltest% h5diff_51.txt  
 

::5.2
set flag=%file4% %file4% -v dset2a dset2b
call %tooltest% h5diff_52.txt 
 

::5.3
set flag=%file4% %file4% -v dset3a dset4b
call %tooltest% h5diff_53.txt 
 

::5.4
set flag=%file4% %file4% -v dset4a dset4b
call %tooltest% h5diff_54.txt 
 

::5.5
set flag=%file4% %file4% -v dset5a dset5b
call %tooltest% h5diff_55.txt  
 

::5.6
set flag=%file4% %file4% -v dset6a dset6b
call %tooltest% h5diff_56.txt 
 

::5.7
set flag=%file4% %file4% -v dset7a dset7b
call %tooltest% h5diff_57.txt  
 

::5.8 long_long test; different format of long_long print  in Linux and IRIX
set flag=%file4% %file4% dset8a dset8b
call %tooltest% h5diff_58.txt  SKIP

:: --------------
:: Error messages
:: --------------

::6.0: Check if the command line number of arguments is less than 3
set flag=%file1%
call %tooltest% h5diff_600.txt 
 
::6.1: Check for invalid options
set flag=%file1% %file2% -x
call %tooltest% h5diff_601.txt   

:: ----
:: -d 
:: ----

::6.2: no value
set flag=%file1% %file2% -d g1/dset3 g1/dset4
call %tooltest% h5diff_602.txt  
 

::6.3: negative value
set flag=%file1% %file2% -d -4 g1/dset3 g1/dset4
call %tooltest% h5diff_603.txt  
 

::6.4: zero
set flag=%file1% %file2% -d 0 g1/dset3 g1/dset4
call %tooltest% h5diff_604.txt  
 

::6.5: non number
set flag=%file1% %file2% -d u g1/dset3 g1/dset4
call %tooltest% h5diff_605.txt  
 

::6.6: hexadecimal
set flag=%file1% %file2% -d 0x1 g1/dset3 g1/dset4
call %tooltest% h5diff_606.txt 
 

::6.7: string
set flag=%file1% %file2% -d "1" g1/dset3 g1/dset4
call %tooltest% h5diff_607.txt  
 

::6.8: repeated option
set flag=%file1% %file2% -d 1 -d 2 g1/dset3 g1/dset4
call %tooltest% h5diff_608.txt  
 

::6.9: number larger than biggest difference
set flag=%file1% %file2% -d 200 g1/dset3 g1/dset4
call %tooltest% h5diff_609.txt  
 

::6.10: number smaller than smallest difference
set flag=%file1% %file2% -d 1 g1/dset3 g1/dset4
call %tooltest% h5diff_610.txt  
 
::---
::-p
::---


::6.11: no value
set flag=%file1% %file2% -r -p g1/dset3 g1/dset4
call %tooltest% h5diff_611.txt  
 


::6.12: negative value
set flag=%file1% %file2% -p -4 g1/dset3 g1/dset4
call %tooltest% h5diff_612.txt  
 


::6.13: zero
set flag=%file1% %file2% -p 0 g1/dset3 g1/dset4
call %tooltest% h5diff_613.txt  
 


::6.14: non number
set flag=%file1% %file2% -p u g1/dset3 g1/dset4
call %tooltest% h5diff_614.txt  
 

::6.15: hexadecimal
set flag=%file1% %file2% -p 0x1 g1/dset3 g1/dset4
call %tooltest% h5diff_615.txt  
 

::6.16: string
set flag=%file1% %file2% -p "0.21" g1/dset3 g1/dset4
call %tooltest% h5diff_616.txt  
 

::6.17: repeated option
set flag=%file1% %file2% -p 0.21 -p 0.22 g1/dset3 g1/dset4
call %tooltest% h5diff_617.txt  
 

::6.18: number larger than biggest difference
set flag=%file1% %file2% -p 2 g1/dset3 g1/dset4
call %tooltest% h5diff_618.txt  
 

::6.19: number smaller than smallest difference
set flag=%file1% %file2% -p 0.005 g1/dset3 g1/dset4
call %tooltest% h5diff_619.txt  
 

::---
::-n 
::---

::6.20: no value
set flag=%file1% %file2% -n g1/dset3 g1/dset4
call %tooltest% h5diff_620.txt  
 

::6.21: negative value
set flag=%file1% %file2% -n -4 g1/dset3 g1/dset4
call %tooltest% h5diff_621.txt  
 

::6.22: zero
set flag=%file1% %file2% -n 0 g1/dset3 g1/dset4
call %tooltest% h5diff_622.txt  
 

::6.23: non number
set flag=%file1% %file2% -n u g1/dset3 g1/dset4
call %tooltest% h5diff_623.txt  
 

::6.24: hexadecimal
set flag=%file1% %file2% -n 0x1 g1/dset3 g1/dset4
call %tooltest% h5diff_624.txt  
 

::6.25: string
set flag=%file1% %file2% -n "2" g1/dset3 g1/dset4
call %tooltest% h5diff_625.txt  
 


::6.26: repeated option
set flag=%file1% %file2% -n 2 -n 3 g1/dset3 g1/dset4
call %tooltest% h5diff_626.txt  
 

::6.27: number larger than biggest difference
set flag=%file1% %file2% -n 200 g1/dset3 g1/dset4
call %tooltest% h5diff_627.txt  
 

::6.28: number smaller than smallest difference
set flag=%file1% %file2% -n 1 g1/dset3 g1/dset4
call %tooltest% h5diff_628.txt  
 

::6.29:  non valid %files

set flag=file1.h6      file2.h6
call %tooltest% h5diff_629.txt  

::--------------
::7.  attributes
::--------------

set flag=%file5% %file6% -v
call %tooltest% h5diff_70.txt  


::-------------------------
::8.  all dataset datatypes
::-------------------------

set flag=%file7% %file8% -v
call %tooltest% h5diff_80.txt  
 

::-----------------------------
::9. compare a %file with itself
::-----------------------------

set flag=%file1% %file1%
call %tooltest% h5diff_90.txt  

::-----------------------------
::10. read by hyperslab, print indexes
::-----------------------------

set flag=%file9% %file10% -v
call %tooltest% h5diff_100.txt  

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
for %%i in (p1 p2 tempResults exefile tooltest ln ln_exp totalskip totalerr) do set %%i=
for %%v in (1 2 3 4 5 6 7 8) do set file%%v=
set flag=

::Clean up Temporary Files
rmdir /s/q temptest


