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
rem access to either file, you may request a copy from helphdfgroup.org.


rem File Name: hdf5build.bat
rem This batch file is used to build HDF5 Libraries and Tools.
rem This batch file takes the following options:
rem .  /fort             Build HDF5 with Fortran libraries
rem .  /useenv           Build HDF5 using compiler settings defined
rem .                    in the environment, rather than the IDE.
rem .  /?                Help information
rem By Xuan Bai
rem Created: Aug. 16, 2004
rem Last Updated: July 13, 2007

rem This batch file makes the following assumptions:
rem    - The appropriate version of Visual Studio is installed and setup
rem    - The directory structure is setup from a fresh source copy
rem    - copy_hdf.bat has already been run from the ./windows directory
rem    - Visual Studio already contains the required paths for external libraries
rem    - szip and zlib DLLs are already placed in an accessible directory
rem    - hdf5_ext_szip or hdf5_ext_zlib have been set accordingly
rem    - if building with the /useenv option, szip and zlib paths have been added
rem      to %include% and %libpath% as necessary.

rem By default, only C and C++ libraries are built.

setlocal enabledelayedexpansion
pushd %~dp0

set nerrors=0
goto main

rem Print a help message
:help

    echo.Builds HDF5 Libraries and Tools.
    echo.
    echo.Usage: %~nx0 [OPTION]
    echo.
    echo.   /fort             Build HDF5 with Fortran libraries
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

    rem Constants
    
    echo.Setting up environment

    rem By default, use all.sln, unless hdf5_enablefortran is set
    if defined hdf5_enablefortran (
        echo.Building Fortran projects enabled
        set hdf5_sln="%CD%\windows\proj\all_fortran\all_fortran.sln"
    ) else (
        set hdf5_sln="%CD%\windows\proj\all\all.sln"
    )
    
    
    rem Setup Visual Studio environment.  By default, use the Visual Studio 
    rem 2005 environment.
    
    set ccflags=
    
    rem Currently VS2005 is the only compiler supported.  --SJW 9/26/07
    echo.Using Visual Studio 2005
    
    if not defined hdf5_enablefortran (
        if defined vs80comntools (
            rem This sets the Visual Studio path and environment variables
            call "%vs80comntools%\vsvars32.bat"
        ) else (
            echo.Error: Cannot setup Visual Studio 2005 environment.  Please
            echo.make sure VS80COMNTOOLS is defined in the environment.
            exit /b 1
        )
        
    ) else (
        echo.with Intel Visual Fortran 9.1
        
        if defined ifort_compiler91 (
            rem This sets the Intel Fortran 9.1 environment, as well as
            rem setting the appropriate Visual Studio environment
            
            rem Assume 32-bit environment
            call "%ifort_compiler91%\IA32\Bin\ifortvars.bat"
        ) else (
            echo.Error: Cannot setup Intel Fortran 9.1 environment.  Please
            echo.make sure IFORT_COMPILER91 is defined in the environment.
            exit /b 1
        )
    )
        
    if defined hdf5_useenv (
        rem This will tell Visual Studio to use include, library, etc. paths
        rem defined by %INCLUDE% %LIBPATH%, etc.  Assume the user has already
        rem added external library paths to these variables.
        set ccflags=%ccflags% /useenv
    )

    exit /b 0


rem Build the HDF5 libraries.  By default, C and C++ libraries are built.
:all

    echo.Building HDF5

    echo.*****************************************************************************
    echo.                        Build HDF5 Library and Tools
    echo.*****************************************************************************
    echo.

    rem Build both debug and release versions
    for %%a in (debug release) DO (
        echo.Building %%a libraries...
        devenv .\windows\proj\all\all.sln %ccflags% /rebuild %%a
        if !errorlevel! neq 0 exit /b
    )

    exit /b


rem Handle errors
:error

    rem For now, our error handling consists of setting nerrors and quitting
    echo.HDF5 build failed.
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
    if %errorlevel% neq 0 (
        echo.Error setting up build environment.
        goto error
    )

    echo.Building HDF5 Libraries and Tools
    echo.

    echo.*****************************************************************************
    echo.                        Build HDF5 Libraries and Tools
    echo.*****************************************************************************
    echo.


    call :all
    if %errorlevel% neq 0 (
        echo.Error building HDF5 libraries!
        goto error
    )

    if "%nerrors%"=="0" (
        echo. All HDF5 libraries and tools build successfully!
    )
    rem Fall through to end

:end
    popd
    endlocal & exit /b %nerrors%
