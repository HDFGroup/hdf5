@echo off
rem Install Intel oneAPI components from an offline installer, as
rem oneapi-src/oneapi-ci scripts/install_windows.bat does.
rem Usage: install.bat <installer URL> <components>

set "URL=%~1"
set "COMPONENTS=%~2"

curl.exe --fail --location --output "%RUNNER_TEMP%\oneapi.exe" --url "%URL%" --retry 5 --retry-delay 5 || exit /b 1
start "" /b /wait "%RUNNER_TEMP%\oneapi.exe" -s -x -f "%RUNNER_TEMP%\oneapi_extracted" --log "%RUNNER_TEMP%\oneapi_extract.log"
del "%RUNNER_TEMP%\oneapi.exe"

"%RUNNER_TEMP%\oneapi_extracted\bootstrapper.exe" -s --action install --components=%COMPONENTS% --eula=accept -p=NEED_VS2017_INTEGRATION=0 -p=NEED_VS2019_INTEGRATION=0 -p=NEED_VS2022_INTEGRATION=0 --log-dir="%RUNNER_TEMP%\oneapi_logs"
set "INSTALLER_EXIT_CODE=%ERRORLEVEL%"
rd /s /q "%RUNNER_TEMP%\oneapi_extracted"
exit /b %INSTALLER_EXIT_CODE%
