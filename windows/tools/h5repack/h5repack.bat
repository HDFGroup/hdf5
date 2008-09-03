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
rem Tests for the h5repack tool
rem
rem    Created:  Scott Wegner, 8/28/07
rem    Modified:
rem

setlocal enabledelayedexpansion
pushd %~dp0

set h5pubconf=%CD%\..\..\src\h5pubconf.h

rem On Windows, the function :detect_filter sets these for us
call :detect_filter szip
call :detect_filter deflate
call :detect_filter shuffle
call :detect_filter fletcher32
call :detect_filter nbit
call :detect_filter scaleoffset

rem The tool name
set h5repack=h5repack%2
rem The path of the tool binary
set h5repack_bin=%CD%\..\%h5repack%\%1\%h5repack%

rem The h5diff tool name
set h5diff=..\h5diff%2\%1\h5diff%2
rem The path of the h5diff tool binary
set h5diff_bin=%CD%\%h5diff%

set h5detectszip=testh5repack_detect_szip%2
set h5detectszip_bin=%CD%\..\testfiles\%h5detectszip%\%1\%h5detectszip%


set info_file=testfiles\h5repack.info

set file0=h5repack_fill.h5
set file1=h5repack_objs.h5
set file2=h5repack_attr.h5
set file3=h5repack_hlink.h5
set file4=h5repack_layout.h5
set file5=h5repack_early.h5
set file7=h5repack_szip.h5
set file8=h5repack_deflate.h5
set file9=h5repack_shuffle.h5
set file10=h5repack_fletcher.h5
set file11=h5repack_filters.h5
set file12=h5repack_nbit.h5
set file13=h5repack_soffset.h5


set nerrors=0
set verbose=yes


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
    set test_msg=!test_msg!                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b

    
rem Print a line-line message left justified in a field of 70 characters
rem beginning with the word "Verifying".
rem
:verify
    set test_msg=Verifying h5diff output
    for %%a in (%*) do (
        if %%a neq PASSED (
        if %%a neq *FAILED* (
            set test_msg=!test_msg! %%~nxa
        ) )
    )
    set test_msg=!test_msg!                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b

    
rem Print a message that a test has been skipped (because a required filter
rem was unavailable)
:skip
    call :testing -SKIP- %h5repack% %*
    exit /b
    
    
rem Call the h5diff tool
rem
:difftest
    %h5diff_bin% -q -c %*
    if %errorlevel% neq 0 (
        call :verify *FAILED* %*
        set /a nerrors=!nerrors!+1
    ) else (
        call :verify PASSED %*
    )
    
    exit /b
    
    
rem Call h5repack
rem
:tooltest

    rem Run test.
    set infile=%CD%\testfiles\%1
    rem Linux uses a $path variable here, but it is unneccessary, and will
    rem corrupt our Windows PATH if we use it.  --SJW 8/28/07
    rem set path=%CD%
    rem set outfile=%path%\out.%1
    set outfile=%CD%\out.%1
    
    rem We define %params% here because Windows `shift` command doesn't affect
    rem the %* variable.  --SJW 8/28/07
    if "%2"=="" (
        set params=
    ) else (
        set params=%*
        set params=!params:* =!
    )
    %h5repack_bin% %params% %infile% %outfile%
    
    if %errorlevel% neq 0 (
        call :testing *FAILED* %*
        set /a nerrors=!nerrors!+1
    ) else (
        call :testing PASSED %*
        call :difftest %infile% %outfile%
    )
    del /f %outfile%
    
    exit /b
    
    
rem Call h5repack with old syntax
rem
:tooltest0

    rem Run test.
    set infile=%CD%\testfiles\%1
    rem Linux uses a $path variable here, but it is unneccessary, and will
    rem corrupt our Windows PATH if we use it.  --SJW 8/28/07
    rem set path=%CD%
    rem set outfile=%path%\out.%1
    set outfile=%CD%\out.%1
    
    rem We define %params% here because Windows `shift` command doesn't affect
    rem the %* variable.  --SJW 8/28/07
    if "%2"=="" (
        set params=
    ) else (
        set params=%*
        set params=!params:* =!
    )
    %h5repack_bin% -i %infile% -o %outfile% %params%
    
    if %errorlevel% neq 0 (
        call :testing *FAILED* %*
        set /a nerrors=!nerrors!+1
    ) else (
        call :testing PASSED %*
        call :difftest %infile% %outfile%
    )
    del /f %outfile%
    
    exit /b
    
    
rem This is a Windows-specific function that detects if the filter passed
rem should be enabled for this test script.  It searches H5pubconf.h for the
rem string "#define H5_HAVE_FILTER_%1" and sets the variable "use_filter_%1"
rem accordingly.  On other platforms, this variable is set in the Makefile.
rem If we find a better way to test this in the future, we should use it.
rem --SJW 9/4/07
:detect_filter
    findstr /b /i /c:"#define H5_HAVE_FILTER_%1" %h5pubconf% > nul
    if %errorlevel% equ 0 (
        set use_filter_%1=yes
    ) else (
        set use_filter_%1=no
    )
    
    exit /b


rem
rem The tests
rem We use the files generated by h5repacktst
rem Each run generates "<file>.out.h5" and the tool h5diff is used to
rem compare the input and output files
rem
rem the tests are the same as the program h5repacktst, but run from the CLI 
rem
:main

    rem See which filters are usable (and skip tests for filters we
    rem don't have).  Do this by searching H5pubconf.h to see which
    rem filters are defined.

    rem detect whether the encoder is present. 
    set use_filter_szip_encoder=no
    if "%use_filter_szip%"=="yes" (
        for /f %%a in ('%h5detectszip_bin%') do set use_filter_szip_encoder=%%a
    )

    rem copy files (these files have no filters) 
    call :tooltest %file0%
    call :tooltest %file1%
    call :tooltest %file2%
    call :tooltest %file3%
    call :tooltest %file4%
    call :tooltest %file5%


    rem use %file4% to write some filters  (this file has  no filters)

    rem gzip with individual object
    set arg=%file4% -f dset1:GZIP=1  -l dset1:CHUNK=20x10
    if not "%use_filter_deflate%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )
      
    rem gzip for all 
    set arg=%file4% -f GZIP=1
    if not "%use_filter_deflate%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem szip with individual object
    set arg=%file4% -f dset2:SZIP=8,EC  -l dset2:CHUNK=20x10
    if not "%use_filter_szip_encoder%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_szip%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem szip for all
    set arg=%file4% -f SZIP=8,NN
    if not "%use_filter_szip_encoder%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_szip%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg% 
    )

    rem shuffle with individual object
    set arg=%file4% -f dset2:SHUF  -l dset2:CHUNK=20x10
    if not "%use_filter_shuffle%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg% 
    )
      

    rem shuffle for all
    set arg=%file4% -f SHUF
    if not "%use_filter_shuffle%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )
      
    rem fletcher32  with individual object
    set arg=%file4% -f dset2:FLET  -l dset2:CHUNK=20x10
    if not "%use_filter_fletcher32%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem fletcher32 for all
    set arg=%file4% -f FLET
    if not "%use_filter_fletcher32%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem all filters
    set arg=%file4% -f dset2:SHUF -f dset2:FLET -f dset2:SZIP=8,NN -f dset2:GZIP=1 -l dset2:CHUNK=20x10
    rem On Windows we must check each filter individually, because we don't have
    rem -o flag like Linux.  --SJW 8/28/07
    if not "%use_filter_szip_encoder%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_szip%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_shuffle%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_fletcher32%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_deflate%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )
      
    rem ##########################################################
    rem  the following tests assume the input files have filters
    rem ##########################################################

    rem szip copy
    set arg=%file7%
    if not "%use_filter_szip_encoder%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_szip%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )
      
    rem szip remove
    set arg=%file7% --filter=dset_szip:NONE
    if not "%use_filter_szip_encoder%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_szip%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )
      
    rem deflate copy
    set arg=%file8%
    if not "%use_filter_deflate%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem deflate remove
    set arg=%file8% -f dset_deflate:NONE
    if not "%use_filter_deflate%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )
        
    rem shuffle copy
    set arg=%file9%
    if not "%use_filter_shuffle%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem shuffle remove
    set arg=%file9% -f dset_shuffle:NONE
    if not "%use_filter_shuffle%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem fletcher32 copy
    set arg=%file10%
    if not "%use_filter_fletcher32%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem fletcher32 remove
    set arg=%file10% -f dset_fletcher32:NONE
    if not "%use_filter_fletcher32%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem nbit copy
    set arg=%file12%
    if not "%use_filter_nbit%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem nbit remove
    set arg=%file12% -f dset_nbit:NONE
    if not "%use_filter_nbit%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem nbit add
    set arg=%file12% -f dset_int31:NBIT
    if not "%use_filter_nbit%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem scaleoffset copy
    set arg=%file13%
    if not "%use_filter_scaleoffset%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem scaleoffset add
    set arg=%file13% -f dset_none:SOFF=31,IN
    if not "%use_filter_scaleoffset%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem scaleoffset remove
    set arg=%file13% -f dset_scaleoffset:NONE
    if not "%use_filter_scaleoffset%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem remove all  filters
    set arg=%file11% -f NONE
    if not "%use_filter_fletcher32%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_deflate%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_szip%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_szip_encoder%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_shuffle%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_nbit%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_scaleoffset%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem filter conversions

    set arg=%file8% -f dset_deflate:SZIP=8,NN
    if not "%use_filter_szip_encoder%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_szip%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_deflate%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg% 
    )

    set arg=%file7% -f dset_szip:GZIP=1
    if not "%use_filter_szip_encoder%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_szip%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_deflate%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg% 
    )


    rem limit
    set arg=%file4% -f GZIP=1 -m 1024
    if not "%use_filter_deflate%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem file
    set arg=%file4% -e %info_file%
    if not "%use_filter_deflate%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )


    rem ########################################################
    rem  layout options (these files have no filters)
    rem ########################################################

    call :tooltest %file4% --layout=dset2:CHUNK=20x10
    call :tooltest %file4% -l CHUNK=20x10
    call :tooltest %file4% -l dset2:CONTI
    call :tooltest %file4% -l CONTI
    call :tooltest %file4% -l dset2:COMPA
    call :tooltest %file4% -l COMPA


    rem ###############################################################
    rem  layout conversions (file has no filters)
    rem ##############################################################

    set arg1=%file4% -l dset_compact:CONTI
    set arg2=%file4% -l dset_compact:CHUNK=2x5
    set arg3=%file4% -l dset_compact:COMPA
    set arg4=%file4% -l dset_contiguous:COMPA
    set arg5=%file4% -l dset_contiguous:CHUNK=3x6
    set arg6=%file4% -l dset_contiguous:CONTI
    set arg7=%file4% -l dset_chunk:COMPA
    set arg8=%file4% -l dset_chunk:CONTI
    set arg9=%file4% -l dset_chunk:CHUNK=18x13
    call :tooltest %arg1%
    call :tooltest %arg2%
    call :tooltest %arg3%
    call :tooltest %arg4%
    call :tooltest %arg5%
    call :tooltest %arg6%
    call :tooltest %arg7%
    call :tooltest %arg8%
    call :tooltest %arg9%

    rem Native option
    set arg=%file1% -n
    call :tooltest %arg%


    rem latest file format with long switches. use FILE4=h5repack_layout.h5 (no filters)
    set arg=%file4% --layout CHUNK=20x10 --filter GZIP=1 --minimum=10 --native --latest --compact=8 --indexed=6 --ssize=8[:dtype]
    if not "%use_filter_deflate%"=="yes" (
       call :skip %arg%
    ) else (
       call :tooltest %arg%
    )

    rem latest file format with short switches. use FILE4=h5repack_layout.h5 (no filters)
    set arg=%file4% -l CHUNK=20x10 -f GZIP=1 -m 10 -n -L -c 8 -d 6 -s 8[:dtype]
    if not "%use_filter_deflate%"=="yes" (
     call :skip %arg%
    ) else (
     call :tooltest %arg%
    )
    
    rem several global filters

    set arg=%file4% --filter GZIP=1 --filter SHUF
    if not "%use_filter_deflate%"=="yes" (
        call :skip %arg%
    ) else if not "%use_filter_shuffle%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest %arg%
    )

    rem syntax of -i infile -o outfile
    rem latest file format with short switches. use FILE4=h5repack_layout.h5 (no filters)
    set arg=%file4% -l CHUNK=20x10 -f GZIP=1 -m 10 -n -L -c 8 -d 6 -s 8[:dtype]
    if not "%use_filter_deflate%"=="yes" (
        call :skip %arg%
    ) else (
        call :tooltest0 %arg%
    )
    
    rem add a userblock to file
    set arg=%file1% -u ublock.bin -b 2048
    call :tooltest %arg%
    
    rem add alignment
    set arg=%file1% -t 1 -a 1
    call :tooltest %arg%
    
    
    if %nerrors% equ 0 (
        echo.All %h5repack% tests passed.
    )
    
    popd
    endlocal & exit /b %nerrors%
    
