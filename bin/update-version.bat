@echo off
REM Copyright by The HDF Group.
REM All rights reserved.
REM
REM This file is part of HDF5.  The full HDF5 copyright notice, including
REM terms governing use, modification, and redistribution, is contained in
REM the LICENSE file, which can be found at the root of the source code
REM distribution tree, or in https://www.hdfgroup.org/licenses.
REM If you do not have access to either file, you may request a copy from
REM help@hdfgroup.org.

REM A script to regenerate all version-templated files from VERSION.txt
REM
REM This script reads VERSION.txt and regenerates all files that contain
REM version information (H5public.h, README.md, CHANGELOG.md, Java files, etc.)
REM
REM Usage:
REM   1. Edit VERSION.txt to set the new version
REM   2. Run this script: bin\update-version.bat
REM   3. Review changes: git diff
REM   4. Commit if correct: git add ... && git commit

setlocal enabledelayedexpansion

REM Determine repository root directory
set "SCRIPT_DIR=%~dp0"
set "HDF5_ROOT=%SCRIPT_DIR%.."

echo ==========================================
echo HDF5 Version File Regeneration Script
echo ==========================================
echo.

REM Change to repository root
cd /d "%HDF5_ROOT%"

REM Check that VERSION.txt exists
if not exist "VERSION.txt" (
    echo ERROR: VERSION.txt not found in %HDF5_ROOT%
    exit /b 1
)

REM Read and display current version
echo Reading version from VERSION.txt...
for /f "tokens=1,2 delims==" %%a in ('findstr "^MAJOR=" VERSION.txt') do set "MAJOR=%%b"
for /f "tokens=1,2 delims==" %%a in ('findstr "^MINOR=" VERSION.txt') do set "MINOR=%%b"
for /f "tokens=1,2 delims==" %%a in ('findstr "^RELEASE=" VERSION.txt') do set "RELEASE=%%b"
for /f "tokens=1,2 delims==" %%a in ('findstr "^SUBRELEASE=" VERSION.txt') do set "SUBRELEASE=%%b"

if "%SUBRELEASE%"=="" (
    set "VERSION_STR=%MAJOR%.%MINOR%.%RELEASE%"
) else (
    set "VERSION_STR=%MAJOR%.%MINOR%.%RELEASE%-%SUBRELEASE%"
)

echo   Version: %VERSION_STR%
echo   Components: MAJOR=%MAJOR%, MINOR=%MINOR%, RELEASE=%RELEASE%, SUBRELEASE=%SUBRELEASE%
echo.

REM Create a temporary build directory
set "BUILD_DIR=%TEMP%\hdf5-version-update-%RANDOM%"
echo Creating temporary build directory: %BUILD_DIR%

REM Run CMake to regenerate version files
echo.
echo Running CMake to regenerate version-templated files...
echo (This may take a moment...)
echo.

REM Run CMake with version regeneration enabled
cmake -S "%HDF5_ROOT%" -B "%BUILD_DIR%" ^
    -DHDF5_REGENERATE_VERSION_FILES=ON ^
    -DHDF5_BUILD_JAVA=ON ^
    > "%BUILD_DIR%\cmake_output.log" 2>&1

if errorlevel 1 (
    echo X CMake configuration failed. Check log:
    type "%BUILD_DIR%\cmake_output.log"
    rmdir /s /q "%BUILD_DIR%"
    exit /b 1
)

echo + CMake configuration successful

REM Cleanup temporary directory
echo Cleaning up temporary directory...
rmdir /s /q "%BUILD_DIR%"

REM List files that were changed
echo.
echo ==========================================
echo Version files have been regenerated
echo ==========================================
echo.
echo The following files have been updated:
echo.

REM Check if git is available
where git >nul 2>&1
if %errorlevel% equ 0 (
    REM Show changed files with git
    git diff --stat VERSION.txt ^
        src\H5public.h ^
        README.md ^
        release_docs\CHANGELOG.md ^
        config\cmake\scripts\HDF5config.cmake ^
        config\examples\HDF5AsSubdirMacros.cmake ^
        java\hdf\hdf5lib\H5.java ^
        java\test\TestH5.java ^
        java\src-jni\hdf\hdf5lib\H5.java ^
        java\src-jni\test\TestH5.java 2>nul

    echo.
    echo Review changes with:
    echo   git diff
    echo.
    echo If changes look correct, commit them with:
    echo   git add VERSION.txt src\H5public.h README.md release_docs\CHANGELOG.md ^
    echo           config\cmake\scripts\HDF5config.cmake ^
    echo           config\examples\HDF5AsSubdirMacros.cmake ^
    echo           java\hdf\hdf5lib\H5.java java\test\TestH5.java ^
    echo           java\src-jni\hdf\hdf5lib\H5.java java\src-jni\test\TestH5.java
    echo   git commit -m "Update version to %VERSION_STR%"
) else (
    echo   src\H5public.h
    echo   README.md
    echo   release_docs\CHANGELOG.md
    echo   config\cmake\scripts\HDF5config.cmake
    echo   config\examples\HDF5AsSubdirMacros.cmake
    echo   java\hdf\hdf5lib\H5.java
    echo   java\test\TestH5.java
    echo   java\src-jni\hdf\hdf5lib\H5.java
    echo   java\src-jni\test\TestH5.java
    echo.
    echo Review and commit these files as needed.
)

echo.
echo Done!

endlocal
