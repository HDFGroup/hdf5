@echo off
rem
rem Copyright by The HDF Group.
rem Copyright by the Board of Trustees of the University of Illinois.
rem All rights reserved.
rem
rem This file is part of HDF5.  The full HDF5 copyright notice, including
rem terms governing use, modification, and redistribution, is contained in
rem the files COPYING and Copyright.html.  COPYING can be found at the root
rem of the source code distribution tree; Copyright.html can be found at the
rem root level of an installed copy of the electronic HDF5 document set and
rem is linked from the top-level documents page.  It can also be found at
rem http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
rem access to either file, you may request a copy from help@hdfgroup.org.
rem
rem Tests for the h5diff tool
rem
rem    Created:  Scott Wegner, 8/22/07
rem    Modified: Scott Wegner, 11/19/07
rem

setlocal enabledelayedexpansion
pushd %~dp0

rem ############################################################################
rem test file names 
rem ############################################################################


set file1=h5diff_basic1.h5
set file2=h5diff_basic2.h5
set file3=h5diff_types.h5
set file4=h5diff_dtypes.h5
set file5=h5diff_attr1.h5
set file6=h5diff_attr2.h5
set file7=h5diff_dset1.h5
set file8=h5diff_dset2.h5
set file9=h5diff_hyper1.h5
set file10=h5diff_hyper2.h5


rem The tool name
set h5diff=h5diff%2
rem The path of the tool binary
set h5diff_bin=%CD%\..\%h5diff%\%1\%h5diff%

set /a nerrors=0
set verbose=yes
rem default to run h5diff tests
set pmode=

if not exist .\testfiles mkdir .\testfiles

rem Parse options
rem On Windows, we don't parse, because we only want to worry about
rem debug/release and dll  --SJW 9/5/07

goto main


rem Print a line-line message left justified in a field of 70 characters
rem beginning with the word "Testing".
rem
:testing
    set test_msg=Testing
    for %%a in (%*) do (
        if %%a neq PASSED (
        if %%a neq *FAILED* (
            set test_msg=!test_msg! %%~nxa
        ) )
    )
    set test_msg=%test_msg%                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b


rem Function STDOUT_FILTER isn't technically needed on Windows, because this
rem script will never run on platforms that require it.  However, include empty
rem interface for consistency.  --SJW 8/22/07
:stdout_filter
    exit /b
    

rem Function STDERR_FILTER isn't technically needed on Windows, because this
rem script will never run on platforms that require it.  However, include empty
rem interface for consistency.  --SJW 8/22/07
:stderr_filter
    exit /b
    
    
    
rem Run a test and print PASS or *FAIL*.  If a test fails then increment
rem the `nerrors' global variable and (if verbose is set) display the
rem difference between the actual output and the expected output. The
rem expected output is given as the first argument to this function and
rem the actual output file is calculated by replacing the `.ddl' with
rem `.out'.  The actual output is not removed if HDF5_NOCLEANUP has a
rem non-zero value.
rem
:tooltest
    set expect=%CD%\testfiles\%1
    set actual=%CD%\testfiles\%~n1.out
    set actual_err=%CD%\testfiles\~n1.err
    set actual_sav=%actual%-sav
    set actual_err_sav=%actual_err%-sav
    
    rem We define %params% here because Windows `shift` command doesn't affect
    rem the %* variable.  --SJW 8/22/07
    set params=
    for /f "tokens=2*" %%a in ("%*") do (
        if "%%b"=="" (
            set params=%%a
        ) else (
            set params=%%a %%b
        )
    )
    rem Parallel mode not actually supported, but included for consistency.
    if defined pmode (
        rem do nothing
    )
    
    rem Run test.
    (
        echo.#############################
        rem Remove quotes here, because Linux 'echo' command strips them
        echo.Expected output for 'h5diff %params:"=%'
        echo.#############################
        pushd testfiles
        %h5diff_bin% %params%
        popd
    ) > %actual% 2> %actual_err%
    rem save actual and actual_err in case they are needed later.
    copy /y %actual% %actual_sav% > nul
    call :stdout_filter %actual%
    copy /y %actual_err% %actual_err_sav% > nul
    call :stderr_filter %actual_err%
    type %actual_err% >> %actual%
    
    if not exist %expect% (
        rem Create the expect file if it doesn't yet exist.
        call :testing CREATED %h5diff% %params%
        copy /y %actual% %expect% > nul
    ) else (
        fc /w %expect% %actual% > nul
        if !errorlevel! equ 0 (
            call :testing PASSED %h5diff% %params%
        ) else (
            call :testing *FAILED* %h5diff% %params%
            echo.    Expected result ^(%expect%^) differs from actual result ^(%actual%^)
            set /a nerrors=!nerrors!+1
            if "yes"=="%verbose%" fc /w %actual% %expect%
        )
    )
        
    rem Clean up output file
    if not defined hdf5_nocleanup (
        del /f %actual% %actual_err% %actual_sav% %actual_err_sav%
    )
    
    exit /b
    
    
rem Print a "SKIP" message
:skip
    call :testing -SKIP- %h5diff% %*
    
    exit /b
    
    
:main
rem ############################################################################
rem ############################################################################
rem #                       T H E   T E S T S                                ###
rem ############################################################################
rem ############################################################################

rem ############################################################################
rem # Common usage
rem ############################################################################


    rem 1.0
    call :tooltest h5diff_10.txt -h

    rem 1.1 normal mode
    call :tooltest h5diff_11.txt  %file1% %file2% 

    rem 1.2 normal mode with objects
    call :tooltest h5diff_12.txt  %file1% %file2%  g1/dset1 g1/dset2

    rem 1.3 report mode
    call :tooltest h5diff_13.txt -r %file1% %file2%

    rem 1.4 report  mode with objects
    call :tooltest h5diff_14.txt -r %file1% %file2% g1/dset1 g1/dset2

    rem 1.5 with -d
    call :tooltest h5diff_15.txt --report --delta=5 %file1% %file2% g1/dset3 g1/dset4

    rem 1.6.1 with -p (int)
    call :tooltest h5diff_16_1.txt -v -p 0.02 %file1% %file1% g1/dset5 g1/dset6

    rem 1.6.2 with -p (unsigned long_long)
    call :tooltest h5diff_16_2.txt --verbose --relative=0.02 %file1% %file1% g1/dset7 g1/dset8

    rem 1.6.3 with -p (double)
    call :tooltest h5diff_16_3.txt -v -p 0.02 %file1% %file1% g1/dset9 g1/dset10

    rem 1.7 verbose mode
    call :tooltest h5diff_17.txt -v %file1% %file2% 

    rem 1.8 quiet mode 
    call :tooltest h5diff_18.txt -q %file1% %file2% 

    rem ##############################################################################
    rem # not comparable types
    rem ##############################################################################

    rem 2.0
    call :tooltest h5diff_20.txt -v %file3% %file3% dset g1

    rem 2.1
    call :tooltest h5diff_21.txt -v %file3% %file3% dset l1

    rem 2.2
    call :tooltest h5diff_22.txt -v %file3% %file3% dset t1

    rem ##############################################################################
    rem # compare groups, types, links (no differences and differences)
    rem ##############################################################################

    rem 2.3
    call :tooltest h5diff_23.txt -v %file3% %file3% g1 g1

    rem 2.4
    call :tooltest h5diff_24.txt -v %file3% %file3% t1 t1

    rem 2.5
    call :tooltest h5diff_25.txt -v %file3% %file3% l1 l1 

    rem 2.6
    call :tooltest h5diff_26.txt -v %file3% %file3% g1 g2

    rem 2.7
    call :tooltest h5diff_27.txt -v %file3% %file3% t1 t2

    rem 2.8
    call :tooltest h5diff_28.txt -v %file3% %file3% l1 l2



    rem ##############################################################################
    rem # Dataset datatypes
    rem ##############################################################################

    rem 5.0
    call :tooltest h5diff_50.txt -v %file4% %file4% dset0a dset0b

    rem 5.1
    call :tooltest h5diff_51.txt -v %file4% %file4% dset1a dset1b

    rem 5.2
    call :tooltest h5diff_52.txt -v %file4% %file4% dset2a dset2b

    rem 5.3
    call :tooltest h5diff_53.txt -v %file4% %file4% dset3a dset4b

    rem 5.4
    call :tooltest h5diff_54.txt -v %file4% %file4% dset4a dset4b

    rem 5.5
    call :tooltest h5diff_55.txt -v %file4% %file4% dset5a dset5b

    rem 5.6
    call :tooltest h5diff_56.txt -v %file4% %file4% dset6a dset6b

    rem 5.7
    call :tooltest h5diff_57.txt -v %file4% %file4% dset7a dset7b

    rem 5.8 (region reference)
    call :tooltest h5diff_58.txt -v %file7% %file8% refreg

    rem ##############################################################################
    rem # Error messages
    rem ##############################################################################


    rem 6.0: Check if the command line number of arguments is less than 3
    call :tooltest h5diff_600.txt %file1% 

    rem 6.1: Check for invalid options
    rem Comment this test on Windows out for now.  The reason is that currently no
    rem expected output exists.  This means that one will be created for the first
    rem testrun, but then never deleted.  The expected output has an absolute path
    rem in it, so it will fail for subsequent tests that use a different path.
    rem -- SJW 11/17/07
    rem call :tooltest h5diff_601.txt -x %file1% %file2% 

    rem ##############################################################################
    rem # -d 
    rem ##############################################################################

    rem 6.2: no value
    call :tooltest h5diff_602.txt -d %file1% %file2% g1/dset3 g1/dset4

    rem 6.3: negative value
    call :tooltest h5diff_603.txt -d -4 %file1% %file2% g1/dset3 g1/dset4

    rem 6.4: zero
    call :tooltest h5diff_604.txt -d 0 %file1% %file2% g1/dset3 g1/dset4

    rem 6.5: non number
    call :tooltest h5diff_605.txt -d u %file1% %file2% g1/dset3 g1/dset4

    rem 6.6: hexadecimal
    call :tooltest h5diff_606.txt -d 0x1 %file1% %file2% g1/dset3 g1/dset4

    rem 6.7: string
    call :tooltest h5diff_607.txt -d "1" %file1% %file2%  g1/dset3 g1/dset4

    rem 6.8: repeated option
    call :tooltest h5diff_608.txt -d 1 -d 2 %file1% %file2% g1/dset3 g1/dset4

    rem 6.9: number larger than biggest difference
    call :tooltest h5diff_609.txt -d 200 %file1% %file2%  g1/dset3 g1/dset4

    rem 6.10: number smaller than smallest difference
    call :tooltest h5diff_610.txt -d 1 %file1% %file2%  g1/dset3 g1/dset4


    rem ##############################################################################
    rem # -p
    rem ##############################################################################


    rem 6.11: no value
    call :tooltest h5diff_611.txt -r -p %file1% %file2% g1/dset3 g1/dset4

    rem 6.12: negative value
    call :tooltest h5diff_612.txt -p -4 %file1% %file2% g1/dset3 g1/dset4

    rem 6.13: zero
    call :tooltest h5diff_613.txt -p 0 %file1% %file2% g1/dset3 g1/dset4

    rem 6.14: non number
    call :tooltest h5diff_614.txt -p u %file1% %file2% g1/dset3 g1/dset4

    rem 6.15: hexadecimal
    call :tooltest h5diff_615.txt -p 0x1 %file1% %file2% g1/dset3 g1/dset4

    rem 6.16: string
    call :tooltest h5diff_616.txt -p "0.21" %file1% %file2% g1/dset3 g1/dset4

    rem 6.17: repeated option
    call :tooltest h5diff_617.txt -p 0.21 -p 0.22 %file1% %file2% g1/dset3 g1/dset4

    rem 6.18: number larger than biggest difference
    call :tooltest h5diff_618.txt -p 2 %file1% %file2% g1/dset3 g1/dset4

    rem 6.19: number smaller than smallest difference
    call :tooltest h5diff_619.txt -p 0.005 %file1% %file2% g1/dset3 g1/dset4



    rem ##############################################################################
    rem # -n
    rem ##############################################################################


    rem 6.20: no value
    call :tooltest h5diff_620.txt -n %file1% %file2% g1/dset3 g1/dset4

    rem 6.21: negative value
    call :tooltest h5diff_621.txt -n -4 %file1% %file2% g1/dset3 g1/dset4

    rem 6.22: zero
    call :tooltest h5diff_622.txt -n 0 %file1% %file2% g1/dset3 g1/dset4

    rem 6.23: non number
    call :tooltest h5diff_623.txt -n u %file1% %file2% g1/dset3 g1/dset4

    rem 6.24: hexadecimal
    call :tooltest h5diff_624.txt -n 0x1 %file1% %file2% g1/dset3 g1/dset4

    rem 6.25: string
    call :tooltest h5diff_625.txt -n "2" %file1% %file2% g1/dset3 g1/dset4

    rem 6.26: repeated option
    call :tooltest h5diff_626.txt -n 2 -n 3 %file1% %file2% g1/dset3 g1/dset4

    rem 6.27: number larger than biggest difference
    call :tooltest h5diff_627.txt --count=200 %file1% %file2% g1/dset3 g1/dset4

    rem 6.28: number smaller than smallest difference
    call :tooltest h5diff_628.txt -n 1 %file1% %file2% g1/dset3 g1/dset4

    rem 6.29  non valid files
    call :tooltest h5diff_629.txt file1.h6 file2.h6

    rem ##############################################################################
    rem 7.  attributes
    rem ##############################################################################
    call :tooltest h5diff_70.txt -v %file5% %file6%

    rem ##############################################################################
    rem 8.  all dataset datatypes
    rem ##############################################################################
    call :tooltest h5diff_80.txt -v %file7% %file8%

    rem 9. compare a file with itself
    call :tooltest h5diff_90.txt -v %file2% %file2%

    rem 10. read by hyperslab, print indexes
    call :tooltest h5diff_100.txt -v %file9% %file10%

    rem 11. floating point comparison
    rem Not tested on Windows due to difference in formatting of scientific 
    rem notation  --SJW 8/23/07
    rem call :tooltest h5diff_101.txt -v %file1% %file1% g1/d1  g1/d2
    rem call :tooltest h5diff_102.txt -v %file1% %file1% g1/fp1 g1/fp2

    rem ##############################################################################
    rem # END
    rem ##############################################################################

    if %nerrors% equ 0 (
       echo.All %h5diff% tests passed.
    )

    popd
    endlocal & exit /b %nerrors%
    
