@echo off
rem Install Intel oneAPI components from an offline installer.
rem Adapted from scripts/install_windows.bat in
rem https://github.com/oneapi-src/oneapi-ci
rem Copyright 2022 Intel Corporation, MIT License.
rem Usage: install.bat <installer URL> <components>

set "URL=%~1"
set "COMPONENTS=%~2"

curl.exe --fail --location --output "%RUNNER_TEMP%\oneapi.exe" --url "%URL%" --retry 5 --retry-delay 5 || exit /b 1
start "" /b /wait "%RUNNER_TEMP%\oneapi.exe" -s -x -f "%RUNNER_TEMP%\oneapi_extracted" --log "%RUNNER_TEMP%\oneapi_extract.log"
del "%RUNNER_TEMP%\oneapi.exe"
if not exist "%RUNNER_TEMP%\oneapi_extracted\bootstrapper.exe" (
  echo Extracting the oneAPI installer failed
  if exist "%RUNNER_TEMP%\oneapi_extract.log" type "%RUNNER_TEMP%\oneapi_extract.log"
  exit /b 1
)

"%RUNNER_TEMP%\oneapi_extracted\bootstrapper.exe" -s --action install --components=%COMPONENTS% --eula=accept -p=NEED_VS2017_INTEGRATION=0 -p=NEED_VS2019_INTEGRATION=0 -p=NEED_VS2022_INTEGRATION=0 --log-dir="%RUNNER_TEMP%\oneapi_logs"
set "INSTALLER_EXIT_CODE=%ERRORLEVEL%"
rd /s /q "%RUNNER_TEMP%\oneapi_extracted"
rem The runner's temporary directory is discarded, so show the logs on failure
if not "%INSTALLER_EXIT_CODE%"=="0" (
  echo The oneAPI installer failed with exit code %INSTALLER_EXIT_CODE%
  for %%f in ("%RUNNER_TEMP%\oneapi_logs\*.log") do (
    echo ===== %%~nxf
    type "%%f"
  )
)
exit /b %INSTALLER_EXIT_CODE%
