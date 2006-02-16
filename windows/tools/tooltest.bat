@echo off
:: Copyright by the Board of Trustees of the University of Illinois.
:: All rights reserved.
::
:: This file is part of HDF5.  The full HDF5 copyright notice, including
:: terms governing use, modification, and redistribution, is contained in
:: the files COPYING and Copyright.html.  COPYING can be found at the root
:: of the source code distribution tree; Copyright.html can be found at the
:: root level of an installed copy of the electronic HDF5 document set and
:: is linked from the top-level documents page.  It can also be found at
:: http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
:: access to either file, you may request a copy from hdfhelpncsa.uiuc.edu.

::This batch file will be called by the following batch files
::---dumptest.bat
::---difftest.bat
::---importtest.bat
::---lstest.bat
::---repacktest.bat
:: Written By: Fang GUO
:: Date      : Jan. 12, 2006

   
     :: Setting the starting column number for "PASSED/FAILED/SKIPED"
     set flagmax=65     

:: Decide which tool will be test
     set test_exefile=%exefile:dll=%
     if %test_exefile%==h5repack goto REPACK
     if %test_exefile%==h5diff goto COMMON
     if %test_exefile%==h5dump goto DUMP
     if %test_exefile%==h5ls goto COMMON
     if %test_exefile%==h5import goto IMPORT

:REPACK
     ::---------------------------------- 
     ::Test for h5repack or h5repackdll
     ::----------------------------------
     set testinput=%1
     set testoutput=..\..\temptest\out.%testinput:~16%
     set exp_file=%2
     set actual_output=..\..\temptest\temp.txt

     :: Extract the string for printing results
     :: Handle flags with .txt inside
     set flagout=%flag:..\..\testfiles\t=t%
     set flagout=%flagout:..\..\temptest\t=t%
     set flagout=%flagout:.txt=#txt%
     set flagout=%flagout:..=##%
 
     for /f "tokens=1,4 delims=." %%a in ("%flagout%") do (
  	 set var1=%%a
     set var4=%%b
     )
     set var1=%var1%.h5
     set var4=%var4:~2%
     if "%var4%"=="" (
     set flagout=%var1%
     goto CHOICE
     )
     set var4=%var4:#=.%
     set flagout=%var1% %var4%
          
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

     :GTEST
     %exefile% %flag%
     ..\..\h5diff%p2%\%p1%\h5diff%p2% %testinput% %testoutput% > %actual_output% 2>&1
     goto RESULTS
     
 
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
         more /e +%ln_exp% ..\..\testfiles\%1 > %exp_file%
                 
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
     set result=SKIPED
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
     more ..\..\temp_fc.txt
		 more ..\..\temp_fc.txt >> ..\..\%tempResults% 
		 ) 

:END
 
    ::Clean up temporary file
    ::For repacktest, there will be a tempory file under directory tools\temptest
    if defined actual_ouput del %actual_output%
    		
    ::clean the environment variables
    for %%v in (exp_file actual_output flag  testinput testoutput test_exefile var1 var4 flagout flaghandle result tests) do set %%v=


  
  
