@echo off
rem Set up the Visual Studio and Intel oneAPI compiler environment in the
rem calling cmd step.
rem Adapted from scripts/build_windows.bat in
rem https://github.com/oneapi-src/oneapi-ci
rem Copyright 2022 Intel Corporation, MIT License.
rem Usage, at the start of a cmd step: call "%ONEAPI_VARS%" || exit /b 1

set "ONEAPI_DIR=C:\Program Files (x86)\Intel\oneAPI"

rem vcvarsall puts the CMake and Ninja bundled with Visual Studio first in
rem PATH. Keep the ones the job set up, such as a pinned CMake version.
set "ONEAPI_KEEP_cmake="
set "ONEAPI_KEEP_ninja="
for %%t in (cmake ninja) do (
  for /f "delims=" %%p in ('where %%t.exe 2^>nul') do (
    if not defined ONEAPI_KEEP_%%t set "ONEAPI_KEEP_%%t=%%~dpp"
  )
)

if not exist "%ONEAPI_DIR%\setvars-vcvarsall.bat" (echo "%ONEAPI_DIR%\setvars-vcvarsall.bat" not found & exit /b 1)
call "%ONEAPI_DIR%\setvars-vcvarsall.bat" %ONEAPI_VS_VERSION%

set "ONEAPI_COMPILER_VERSION="
for /f "tokens=* usebackq" %%f in (`dir /b /ad "%ONEAPI_DIR%\compiler\" 2^>nul ^| findstr /V latest ^| sort`) do set "ONEAPI_COMPILER_VERSION=%%f"
if not defined ONEAPI_COMPILER_VERSION (echo No compiler version directory found in "%ONEAPI_DIR%\compiler" & exit /b 1)
call "%ONEAPI_DIR%\compiler\%ONEAPI_COMPILER_VERSION%\env\vars.bat"

rem An empty PATH entry would mean the current directory, so only add the
rem directories that were found
if defined ONEAPI_KEEP_ninja set "PATH=%ONEAPI_KEEP_ninja%;%PATH%"
if defined ONEAPI_KEEP_cmake set "PATH=%ONEAPI_KEEP_cmake%;%PATH%"

where ifx.exe >nul 2>&1 || (echo ifx not found after setting up the oneAPI environment & exit /b 1)
where icx.exe >nul 2>&1 || (echo icx not found after setting up the oneAPI environment & exit /b 1)
exit /b 0
