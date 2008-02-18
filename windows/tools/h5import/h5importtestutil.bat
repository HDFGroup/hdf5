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
rem HDF Utilities Test script
rem
rem    Created:  Scott Wegner, 8/27/07
rem    Modified:
rem

setlocal enabledelayedexpansion
pushd %~dp0

rem The tool name
set h5import=h5import%2
rem The path of the tool binary
set h5import_bin=%CD%\..\%h5import%\%1\%h5import%.exe

rem The h5importtest tool name
set h5importtest=..\testfiles\h5importtst\%1\h5importtst
rem The path of the h5importtst tool binary
set h5importtest_bin=%CD%\%h5importtest%.exe

rem The h5dump tool name
set h5dump=..\h5dump%2\%1\h5dump%2
rem The path of the h5dump tool binary
set h5dump_bin=%CD%\%h5dump%

rem initialize errors variables
set errors=0

goto main

:testing
    set test_msg=Testing 
    for %%a in (%*) do (
        if %%a neq PASSED (
        if %%a neq *FAILED* (
            set test_msg=!test_msg! %%~nxa
        ) )
    )
    rem We need to replace PERCENT-ZERO here with "%0" for the tfamily test.
    rem --SJW 8/24/07
    set test_msg=!test_msg:PERCENT-ZERO=%%0!                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b

    
:tooltest
    set err=0
    %h5import_bin% %*
    %h5dump_bin% %5 > log2
    
    pushd tmp_testfiles
    %h5dump_bin% %5 > log1
    popd
    
    fc /w tmp_testfiles\log1 log2 | find "FC: no diff" > nul
    if %errorlevel% neq 0 set err=1
    del /f log2 tmp_testfiles\log1
    if "%err%"=="1" (
        set /a errors=!errors!+1
        call :testing *FAILED* %testing%
    ) else (
        call :testing: PASSED %testing%
    )
    
    exit /b
    
    
:main
    echo.
    echo.==============================
    echo.H5IMPORT tests started
    echo.==============================

    if exist %h5import_bin% (
    if exist %h5importtest_bin% (
    rem echo.** Testing h5import  ***
    
    del /f output.h5 log1 tx* b* *.dat 2> nul
    
    if not exist tmp_testfiles mkdir tmp_testfiles
    copy /y testfiles\*.h5 tmp_testfiles > nul
    
    %h5importtest_bin%
    
    rem On Linux, they call TESTING here, and output pass/fail from TOOLTEST.
    rem On Windows, echo gives a carriage return, so we store the TESTING params
    rem and call TESTING from TOOLTEST.  --SJW 8/27/07
    set testing=ASCII I32 rank 3 - Output BE 
    call :tooltest txtin32 -c %CD%\testfiles\textin32 -o test1.h5

    set testing=ASCII I16 rank 3 - Output LE - CHUNKED - extended
    call :tooltest txtin16 -c %CD%\testfiles\textin16 -o test2.h5

    set testing=ASCII I8 - rank 3 - Output I16 LE-Chunked+Extended+Compressed
    call :tooltest txtin16 -c %CD%\testfiles\textin8  -o test3.h5

    set testing=ASCII UI32 - rank 3 - Output BE
    call :tooltest %CD%\testfiles\in1 -c %CD%\testfiles\textuin32 -o test4.h5

    set testing=ASCII UI16 - rank 2 - Output LE+Chunked+Compressed
    call :tooltest %CD%\testfiles\in1 -c %CD%\testfiles\textuin16 -o test5.h5

    set testing=ASCII F32 - rank 3 - Output LE
    call :tooltest %CD%\testfiles\fp1 -c %CD%\testfiles\textfp32 -o test6.h5

    set testing=ASCII F64 - rank 3 - Output BE + CHUNKED+Extended+Compressed
    call :tooltest %CD%\testfiles\fp2 -c %CD%\testfiles\textfp64 -o test7.h5

    set testing=BINARY F64 - rank 3 - Output LE+CHUNKED+Extended+Compressed
    call :tooltest bfp64 -c %CD%\testfiles\conbfp64 -o test8.h5

    set testing=BINARY I16 - rank 3 - Output order LE + CHUNKED + extended
    call :tooltest bin16 -c %CD%\testfiles\conbin16 -o test9.h5

    set testing=BINARY I8 - rank 3 - Output I16LE + Chunked+Extended+Compressed
    call :tooltest bin8 -c %CD%\testfiles\conbin8  -o test10.h5

    set testing=BINARY I32 - rank 3 - Output BE + CHUNKED
    call :tooltest bin32 -c %CD%\testfiles\conbin32 -o test11.h5

    set testing=BINARY UI16 - rank 3 - Output byte BE + CHUNKED
    call :tooltest buin16 -c %CD%\testfiles\conbuin16 -o test12.h5

    set testing=BINARY UI32 - rank 3 - Output LE + CHUNKED
    call :tooltest buin32 -c %CD%\testfiles\conbuin32 -o test13.h5

    set testing=STR 
    call :tooltest %CD%\testfiles\txtstr -c %CD%\testfiles\textstr -o teststr.h5

    del /f tx* b* *.dat
    del /f test*.h5
    rmdir /s /q tmp_testfiles
    
    ) else (
        echo.** h5importtest not avaiable ***
        set /a errors=!errors!+1
    )
    ) else (
        echo.** h5import not avaiable ***
        set /a errors=!errors!+1
    )
    
    rem
    rem Check error results
    if %errors% equ 0 (
        echo.======================================
        echo. H5IMPORT Utilities tests have passed.
        echo.======================================
    ) else (
        echo.======================================
        echo. H5IMPORT Utilities tests encountered errors
        echo.======================================
    )
    
    popd
    endlocal & exit /b %errors%
    