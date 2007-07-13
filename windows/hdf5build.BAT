@rem Copyright by The HDF Group.
@rem Copyright by the Board of Trustees of the University of Illinois.
@rem All rights reserved.
@rem
@rem This file is part of HDF5.  The full HDF5 copyright notice, including
@rem terms governing use, modification, and redistribution, is contained in
@rem the files COPYING and Copyright.html.  COPYING can be found at the root
@rem of the source code distribution tree; Copyright.html can be found at the
@rem root level of an installed copy of the electronic HDF5 document set and
@rem is linked from the top-level documents page.  It can also be found at
@rem http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
@rem access to either file, you may request a copy from help@hdfgroup.org.

@echo OFF

:: File Name: hdf5build.bat
:: This batch file is used to build HDF5 Libraries and Tools.
:: There are currently only 2 options for this batch file:
::   1. hdf5build                -- Print a usage message and quit
::   1. hdf5build                -- Build HDF5 tools and c library
:: By Xuan Bai
:: Created: Aug. 16, 2004
:: Last Updated: July 13, 2007

:: This batch file makes the following assumptions:
::    - The appropriate version of Visual Studio is installed and setup
::    - The directory structure is setup from a fresh source copy
::    - copy_hdf.bat has already been run from the ./windows directory
::    - Visual Studio already contains the required paths for external libraries
::    - szip and zlib DLLs are already placed in an accessible directory
::    - If hdf5_ext_szip or hdf5_ext_zlib are not set, then %szip_name% and %zlib_name%
::      will be used, respectively.

:: By default, only C and C++ libraries are built.

goto main

:: Print a help message
:help

    echo.Builds HDF5 Libraries and Tools.
    echo.
    echo %~nx0 [OPTION]
    echo.
    echo.Please use one of the following options!
    echo.
    echo.   %~nx0                   Build HDF5 C Library and Tools
    echo.   %~nx0 /?                Help information

    exit /b 0


:: Parse through the parameters sent to file, and set appropriate variables
:parse_params

    for %%a in (%*) do (
        if "%1"=="/vs8" (
            :: Use VS2005 as our compiler
            set hdf5_use_vs2005=true
            
        ) else if "%1"=="/fort" (
            :: Enable Fortran
            set hdf5_enablefortran=true
            
        ) else if "%1"=="/?" (
            :: Set errorlevel 1 and send to help
            call :help
            exit /b 1
            
        ) else (
            :: Set errorlevel 2 to send to help if we receive a bad parameter (or /?)
            call :help
            exit /b 2
        )
    )
    
    exit /b 0


:: Setup our environment
:setup

    echo.   Setting up environment >> %build_results%

    :: Put us in the directory of the batch file.
    pushd %~dp0

    :: Constants
    if "x%build_results%"=="x" set build_results="%CD%\build_results.txt"
    type nul > %build_results%
    
    if "x%zlib_name%"=="x" set zlib_name=zdll.lib
    if "x%szip_name%"=="x" set szip_name=szlibdll.lib
    
    :: By default, use all.sln, unless hdf5_enablefortran is set
    if defined hdf5_enablefortran (
        echo.Building Fortran projects enabled
        echo.Building Fortran projects enabled >> %build_results%
        set hdf5_sln="%CD%\windows\proj\all_fortran\all_fortran.sln"
    ) else set hdf5_sln="%CD%\windows\proj\all\all.sln"
    
    :: Set external libraries if they aren't already set
    if x%hdf5_ext_zlib%==x set hdf5_ext_zlib=%zlib_name%
    if x%hdf5_ext_szip%==x set hdf5_ext_szip=%szip_name%

    
    :: Setup Visual Studio environment.  By default, use the Visual Studio .NET environment
    if defined hdf5_use_vs2005 (
        echo.Using Visual Studio 2005
        echo.Using Visual Studio 2005 >> %build_results%
        call "%vs80comntools%\vsvars32.bat" >> %build_results%
    ) else (
        echo.Using Visual Studio .NET 2003
        echo.Using Visual Studio .NET 2003 >> %build_results%
        call "%vs71comntools%\vsvars32.bat" >> %build_results%
        
        :: If we are building Fortran libraries with .NET, we need to manually edit
        :: some project files
        if defined hdf5_enablefortran (
            echo.Building Fortran from commandline with .NET not yet implemented!
            echo.Building Fortran from commandline with .NET not yet implemented! >> %build_results%
            exit /b 1
    )

    exit /b 0


:: Build H5tinit.exe and generate h4tinit.c
:h5tinit

    echo.   Generating H5tint.c

    echo.***************************************************************************** >> %build_results%
    echo.                        Build H5Tinit.exe >> %build_results%
    echo.***************************************************************************** >> %build_results%
    echo. >> %build_results%
    devenv .\windows\misc\typegen\typegen.sln /rebuild Debug >> %build_results% 2>&1
    if %errorlevel% neq 0 exit /b

    :: Use output to generate h5tinit.c
    .\src\h5tinit.exe > .\src\h5tinit.c

    exit /b


:: Build the HDF5 libraries.  By default, C and C++ libraries are built.
:all

    echo.   Building HDF5

    echo.***************************************************************************** >> %build_results%
    echo.                        Build HDF5 C Library and Tools >> %build_results%
    echo.***************************************************************************** >> %build_results%
    echo. >> %build_results%

    :: Build both debug and release versions
    for %%a in (debug release) DO (
        echo.       Building %%a libraries...
        echo.       Building %%a libraries... >> %build_results%
        devenv .\windows\proj\all\all.sln /rebuild %%a >> %build_results% 2>&1
        if %errorlevel% neq 0 exit /b
    )

    exit /b


:: Handle errors
:error

    :: For now, our error handling just consists of calling cleanup, and setting errorlevel
    echo.HDF5 build failed.
    echo.HDF5 build failed. >> %build_results%
    call :cleanup

    exit /b 1


:: Cleanup our environment
:cleanup

    set zlib_name=
    set szip_name=
    set hdf5_enablefortran=
    set hdf5_use_vs2005=

    :: Don't unset build_results if it is being used by hdf5bt.BAT
    if "x%bt_results%"=="x" set build_results=
    popd

    exit /b 0


:: This is where the magic happens
:main

    call :parse_params %*
    if %errorlevel% neq 0 (
        if %errorlevel% equ 2 echo.Error parsing parameters!
        exit /b
    )
    
    call :setup

    echo.Building HDF5 Libraries and Tools
    echo.

    echo.***************************************************************************** >> %build_results%
    echo.                        Build HDF5 Libraries and Tools >> %build_results%
    echo.***************************************************************************** >> %build_results%
    echo. >> %build_results%


    call :h5tinit
    if %errorlevel% neq 0 (
        echo.   Error building h5tinit!
        echo.   Error building h5tinit! >> %build_results%
        goto error
    )

    call :all
    if %errorlevel% neq 0 (
        echo.   Error building HDF5 libraries!
        echo.   Error building HDF5 libraries! >> %build_results%
        goto error
    )

    echo. All HDF5 libraries and tools build successfully!
    echo. All HDF5 libraries and tools build successfully! >> %build_results%

    call :cleanup

    exit /b 0
