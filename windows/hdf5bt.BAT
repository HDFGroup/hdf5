@echo OFF
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


rem File Name: hdf5bt.bat
rem This batch file is used to build and test HDF5 Libraries and Tools.
rem This batch file takes the following options:
rem .  /fort             Build and test HDF5 with Fortran libraries
rem .  /useenv           Build HDF5 using compiler settings defined
rem .                    in the environment, rather than the IDE.
rem .  /?                Help information
rem By Xuan Bai
rem Created: Aug. 16, 2004
rem Last Updated: Scott Wegner, 10/1/07

rem This batch file makes the following assumptions:
rem    - The appropriate version of Visual Studio is installed and setup
rem    - The directory structure is setup from a fresh source copy
rem    - copy_hdf.bat has already been run from the ./windows directory
rem    - Visual Studio already contains the required paths for external libraries
rem    - szip and zlib DLLs are already placed in an accessible directory
rem    - hdf5_ext_szip or hdf5_ext_zlib have been set accordingly
rem    - if building with the /useenv option, szip and zlib paths have been added
rem      to %include% and %libpath% as necessary.

rem By default, only C and C++ libraries are built and tested.

setlocal enabledelayedexpansion
pushd %~dp0

set nerrors=0
goto main

rem Print a help message
:help

    echo.Builds and tests HDF5 Libraries and Tools.
    echo.
    echo Usage: %~nx0 [OPTION]
    echo.
    echo.   /fort             Build and test HDF5 with Fortran libraries
    echo.   /useenv           Build HDF5 using compiler settings defined
    echo.                     in the environment, rather than the IDE.
    echo.   /?                Help information

    exit /b 0


rem Parse through the parameters sent to file, and set appropriate variables
:parse_params

    for %%a in (%*) do (
        if "%%a"=="/fort" (
            rem Enable Fortran
            set hdf5_enablefortran=true
            
        ) else if "%%a"=="/useenv" (
            rem Pass /useenv flag to devenv
            set hdf5_useenv=true
            
        ) else if "%%a"=="/?" (
            rem Set errorlevel 1 and send to help
            call :help
            exit /b 1
            
        ) else (
            rem Set errorlevel 2 to send to help if we receive a bad parameter
            echo.Unknown option: %%a
            call :help
            exit /b 2
        )
    )
    
    exit /b 0

    
rem Setup our environment
:setup

    rem All we need to do here is setup the parameters that will be sent to
    rem hdf5build and hdf5check.
    set hdf5build_params=
    set hdf5check_params=enablecpp
    
    if defined hdf5_enablefortran (
        set hdf5build_params=%hdf5build_params% /fort
        set hdf5check_params=enableall
    )
    
    if defined hdf5_useenv (
        set hdf5build_params=%hdf5build_params% /useenv
    )
    
    exit /b 0


rem Build HDF5 libraries and tools
:build

    call hdf5build.bat %hdf5build_params%

    exit /b


rem Test our libraries and tools
:test

    call hdf5check %hdf5check_params%

    exit /b


rem Handle errors
:error

    rem For now, our error handling consists of setting nerrors and quitting
    echo.HDF5 build-test failed.
    set /a nerrors=%nerrors%+1
    goto end
    
    rem We'll never really get here, but we keep this line for consistency.
    exit /b


rem This is where the magic happens
:main

    call :parse_params %*
    if %errorlevel% neq 0 (
        if !errorlevel! equ 1 (
            rem This isn't an error case-- this means /? was specified.  Simply
            rem quit.
            goto end
            
        ) else (
            rem Error case.
            echo.Error parsing parameters!
            goto error
        )
    )

    call :setup

    echo.*****************************************************************************
    echo.                        Build and Test HDF5 Library and Tools
    echo.*****************************************************************************
    echo.

    call :build
    if %errorlevel% neq 0 (
        echo.Error building HDF5 libraries!
        goto error
    )

    call :test
    if %errorlevel% neq 0 (
        echo.Error testing HDF5 libraries!
        goto error
    )

    if "%nerrors%"=="0" (
        echo. All HDF5 libraries and tools build and tested successfully!
    )
    rem Fall through to end

:end
    popd
    endlocal & exit /b %nerrors%
