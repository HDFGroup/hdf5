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

::This batch file will be called by the following batch files
::---dumptest.bat
::---difftest.bat
::---importtest.bat
::---lstest.bat
::---repacktest.bat
::---stattest.bat
:: Written By: Fang GUO
:: Date      : Jan. 12, 2006
:: Modified By: MuQun Yang
:: Date      : Oct. 19, 2006

   
     :: Setting the starting column number for "PASSED/FAILED/SKIPED"
     set flagmax=65     

:: Decide which tool will be test
:: Define an environment variable to decide which test should run

:: Find string "dll" inside %exefile% and remove it
     set test_exefile=%exefile:dll=%

     if %test_exefile%==h5repack goto REPACK
     if %test_exefile%==h5diff goto COMMON
     if %test_exefile%==h5dump goto DUMP
     if %test_exefile%==h5ls goto COMMON
     if %test_exefile%==h5stat goto COMMON
     if %test_exefile%==h5import goto IMPORT

:: This block is for Repack test 
:REPACK
     ::---------------------------------- 
     ::Test for h5repack or h5repackdll
     ::----------------------------------

     :: Get the input parameters 
		 :: Example case for h5repack
		 :: if %1 is ..\..\testfiles\test1.h5
     :: This line will set variable %testinput% equal to ..\..\testfiles\test1.h5

     set testinput=%1
      
     :: %testinput:~16% will get all of the chars of %testinput% except the first 16 chars
     :: Example case: %testinput:~16% will give you test1.h5
     :: The whole line will set varialbe %testoutput% equal to ..\..\temptest\out.test1.h5

		 set testoutput=..\..\temptest\out.%testinput:~16%

 		 ::Set exp_file equal to the second parameter
		 :: Example case: %exp_file% will be equal to %nodiff%
     set exp_file=%2

		 :: Set a variable to be the path to the real output 
     set actual_output=..\..\temptest\temp.txt

     :: Extract the string for printing results
     :: Variable flagout is for printing purpose only
     :: These lines will get rid of the paths inside %flag% and set it to %flagout%
     :: Example case:
     :: %flag% was set inside repacktest.bat to 
     :: "-i ..\..\testfiles\test1.h5  -o ..\..\temptest\out.test1.h5"
     :: The following two lines will set %flagout% equal to
     :: "-i test1.h5  -o ..\..\temptest\out.test1.h5"

     set flagout=%flag:..\..\testfiles\t=t%
     set flagout=%flagout:..\..\temptest\t=t%

     :: "." will be used as a delimiter in the for loop for printing output
     :: But "." inside ".txt" and".." will not be treated as a delimiter. So replace it
     :: with a "#" and recover it before printing results.

     set flagout=%flagout:.txt=#txt%
     set flagout=%flagout:..=##%
 
     :: This for loop uses "." as a delimiter and gets the first and the fourth tokens 
     :: and assign it to var1 and var4 
		 ::Example case: 
     :: %flagout% now is "-i test1.h5  -o ##\##\temptest\out.test1.h5"
     :: This for loop will set %var1% as "-i test1" and %var4% as "h5"
     
     for /f "tokens=1,4 delims=." %%a in ("%flagout%") do (
  	 set var1=%%a
     set var4=%%b
     )

     ::Add in extension for input file 
     ::Example case: %var1% is "-i test1" in previous step
     :: This step will set %var1% as %var1% as "-i test1.h5"
     set var1=%var1%.h5

     ::Check for output file name
     ::Example case: %var4% is ".h5" in previous step
     ::This step will set it to nothing
     set var4=%var4:~2%

     ::Check if %var4% is blank, we will print the "flagout" with the "var1" only
     if "%var4%"=="" (
     set flagout=%var1%
     goto CHOICE
     )

     ::If %var4% is not blank, for example in the "file test" we have flags like
     ::set flag=-i %test4% -o %output4% -e ..\..\testfiles\h5repack_info.txt
     ::In this case, we want to print the %var4% out 
     ::Recover "." 
     set var4=%var4:#=.%
     set flagout=%var1% %var4%
          
     ::Based on the third parameter, we will go to different part.
     :: GTEST means general test, no need to check zlib and szlib
     
     :CHOICE
     if "%3"=="" goto GTEST
     if "%3"=="SKIP" goto SKIP
     if "%3"=="zlib" (
        if "%4"=="" goto ZLIB
        if "%4"=="szip" goto ZSLIB
        goto SKIP
     )
     if "%3"=="szip" (
       if "%4"=="" goto SZIP
       goto SKIP
     )
     goto SKIP

     :ZLIB
     if "%HDF5_EXT_ZLIB%"=="zdll.lib" goto GTEST
     goto SKIP

     :SZIP
     if "%HDF5_EXT_SZIP%"=="szlibdll.lib" (
		     if %HDF5_SZIP_ENCODE%==1 goto GTEST
     )
     goto SKIP

     :ZSLIB
     if "%HDF5_EXT_ZLIB%"=="zdll.lib" (
         if "%HDF5_EXT_SZIP%"=="szlibdll.lib" (
		     if %HDF5_SZIP_ENCODE%==1 goto GTEST
     )
     )
     goto SKIP 

     ::Run the .exe file with the specified flag and generate %testoutput%
     ::Compare the expected and the actual output and save the comparison
     ::results into .\..\temptest\temp.txt(%actual_output%)
     ::Example case: 
     ::Expected output(%testinput%) is "..\..\testfiles\test1.h5"
     ::Actual output(%testoutput%) is "..\..\temptest\out.test1.h5"
     ::Save the comparion results into .\..\temptest\temp.txt
			
     :GTEST
     %exefile% %flag%
     ..\..\h5diff%p2%\%p1%\h5diff%p2% %testinput% %testoutput% > %actual_output% 2>&1
     goto RESULTS
     
:: End of Repack tests


::H5diff and H5ls Tests 
:COMMON
    :: ------------------------------------------------------
    :: Test for Tools which only need to run .exe and compare
    :: actual_output and expected_output
    :: ------------------------------------------------------

         :: Call tooltest with the following parameters set in difftest.bat
	       :: 1. expected_outputfile.txt 
	       :: 2. flags
       
         ::Set a flaghandle for output tests results
		     set flagout=%flag:..\..\testfiles\=%
		     set flagout=%flagout:..\..\temptest\=%	     
         if "%2%"=="SKIP" goto SKIP
         ::Set the file name and path for the expected and actual outputs
     		 set exp_file=..\..\temptest\%1 
         set actual_output=..\..\temptest\out.%1

         ::Run .exe file with flag and rediect the output to a temp file
         %exefile% %flag% > ..\..\temptest\temp.txt 2>&1 | more

         if "%2%"=="MASK" goto MASK
                      
         more /e +%ln% ..\..\temptest\temp.txt > %actual_output%
	
	 if %test_exefile%==h5stat (
         	more /e +%ln_exp% ..\..\misc\testfiles\%1 > %exp_file%
         )	else (
      		more /e +%ln_exp% ..\..\testfiles\%1 > %exp_file%
	 )
         ::Clean up temporary file
		     del ..\..\temptest\temp.txt
         goto RESULTS

:MASK
         call ..\..\mask ..\..\temptest\temp.txt %actual_output%
         call ..\..\mask ..\..\testfiles\%1 %exp_file%
         more /e +1 %actual_output% > ..\..\temptest\tempa%1
         more /e +4 %exp_file% > ..\..\temptest\tempe%1
         move /Y ..\..\temptest\tempa%1 %actual_output%
         move /Y ..\..\temptest\tempe%1 %exp_file%
         del ..\..\temptest\temp.txt
       
         goto RESULTS
				
:DUMP
    :: -----------------------------
    :: Test for h5dump or h5dumpdll
    :: ------------------------------

        if "%2"=="" goto COMMON
        if "%2"=="zlib" (
          if "%3"=="" goto DUMPZLIB
          if "%3"=="szip" goto DUMPSZ
        )
        if "%2"=="szip" goto DUMPSZIP
        if "%2"=="SKIP" goto SKIP

   :DUMPZLIB
      if "%HDF5_EXT_ZLIB%"=="zdll.lib" goto COMMON
      goto SKIP

   :DUMPSZIP
      if "%HDF5_EXT_SZIP%"=="szlibdll.lib" goto COMMON
      goto SKIP

   :DUMPSZ
      if "%HDF5_EXT_ZLIB%"=="zdll.lib" (
        if "%HDF5_EXT_SZIP%"=="szlibdll.lib" goto COMMON
      )
      goto SKIP

:IMPORT

    set exp_file=..\..\temptest\exp_%1
    set actual_output=..\..\temptest\act_%1
    set flagout=%flag:..\..\testfiles\=%
    set flagout=%flagout:..\..\h5import\=%
    set flagout=%flagout:..\..\temptest\=%
    %exefile% %flag%
    ..\..\h5dump%p2%\%p1%\h5dump%p2% ..\..\h5import\testfiles\%2 | more +%ln% >%exp_file% 2>&1
    ..\..\h5dump%p2%\%p1%\h5dump%p2% ..\..\temptest\out.%2 | more +%ln%>%actual_output% 2>&1
    

:RESULTS
    ::-------------------
    ::Echo tests results
    ::-------------------
     fc %exp_file% %actual_output% > ..\..\temp_fc.txt
		 if %ERRORLEVEL%==0 (
     set result=PASSED
     ) else (
     set result=FAILED
     set /A totalerr=totalerr+1 
     )
     goto ALIGN

:SKIP
     set flagout=%flag:..\..\testfiles\=%
     set flagout=%flagout:..\..\temptest\=%
     set result=-SKIPED-
     set /A totalskip=totalskip+1
		 goto ALIGN

:ALIGN
     ::Set a var to count the # of chars for %flagout%
     set flaghandle=%flagout:"=**%

     set /A n=0 
		 if not "%flaghandle%"=="" goto ALOOP 
		
		 :ALOOP 
		 set firstchar=%flaghandle:~0,1%
		 set /A n=n+1
		 set flaghandle=%flaghandle:~1%
		 if not "%flaghandle%"=="" goto ALOOP 

     :AOUT 
     ::Test if the length of any flags is over the maximum setting
     if /I %n% GTR %flagmax% goto APRINT

		 if not %n%==%flagmax% ( 
		 set flagout=%flagout%@
		 set /A n=n+1 
		 ) 
		 if not %n%==%flagmax% goto AOUT
		 
     :APRINT
     ::Recover " and space 
     set flagout=%flagout:@= %
     set flagout=%flagout:**=" %
     echo %exefile% %flagout% %result%
     echo %exefile% %flagout% %result% >> ..\..\%tempResults% 
		 if "%result%"=="FAILED" ( 
     ::more ..\..\temp_fc.txt
		 more ..\..\temp_fc.txt >> ..\..\%tempResults% 
		 ) 

:END
 
    ::Clean up temporary file
    ::For repacktest, there will be a tempory file under directory tools\temptest
    if defined actual_ouput del %actual_output%
    		
    ::clean the environment variables
    for %%v in (exp_file actual_output flag  testinput testoutput test_exefile var1 var4 flagout flaghandle result tests) do set %%v=


  
  
