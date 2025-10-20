@echo off
REM jextract-generate.bat
REM Cross-platform jextract wrapper for generating HDF5 FFM bindings (Windows)
REM
REM Usage: jextract-generate.bat <hdf5_install_dir> <output_dir> <jextract_home>
REM
REM Arguments:
REM   hdf5_install_dir - Directory containing HDF5 headers and libraries
REM   output_dir       - Directory where generated Java sources will be placed
REM   jextract_home    - Directory containing jextract installation

setlocal enabledelayedexpansion

if "%~3"=="" (
    echo Usage: %~nx0 ^<hdf5_install_dir^> ^<output_dir^> ^<jextract_home^>
    exit /b 1
)

set "HDF5_DIR=%~1"
set "OUTPUT_DIR=%~2"
set "JEXTRACT_HOME=%~3"

REM Validate inputs
if not exist "%HDF5_DIR%" (
    echo Error: HDF5 directory not found: %HDF5_DIR%
    exit /b 1
)

if not exist "%HDF5_DIR%\include\hdf5.h" (
    echo Error: hdf5.h not found in %HDF5_DIR%\include\
    exit /b 1
)

if not exist "%JEXTRACT_HOME%\bin\jextract.exe" (
    if not exist "%JEXTRACT_HOME%\bin\jextract.bat" (
        echo Error: jextract not found: %JEXTRACT_HOME%\bin\jextract
        exit /b 1
    )
)

REM Create output directory if it doesn't exist
if not exist "%OUTPUT_DIR%" mkdir "%OUTPUT_DIR%"

echo =========================================
echo Generating FFM bindings with jextract
echo =========================================
echo HDF5 directory: %HDF5_DIR%
echo Output directory: %OUTPUT_DIR%
echo Jextract: %JEXTRACT_HOME%\bin\jextract
echo.

REM Run jextract
"%JEXTRACT_HOME%\bin\jextract" ^
  --include-dir "%HDF5_DIR%\include" ^
  --output "%OUTPUT_DIR%" ^
  --target-package org.hdfgroup.javahdf5 ^
  --library hdf5 ^
  "%HDF5_DIR%\include\hdf5.h"

if %ERRORLEVEL% neq 0 (
    echo.
    echo Error: Jextract failed with exit code %ERRORLEVEL%
    exit /b %ERRORLEVEL%
)

echo.
echo Jextract completed successfully!
echo Generated files in: %OUTPUT_DIR%

endlocal
