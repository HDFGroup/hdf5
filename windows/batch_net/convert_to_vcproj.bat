@REM Copyright by the Board of Trustees of the University of Illinois.
@REM All rights reserved.
@REM
@REM This file is part of HDF5.  The full HDF5 copyright notice, including
@REM terms governing use, modification, and redistribution, is contained in
@REM the files COPYING and Copyright.html.  COPYING can be found at the root
@REM of the source code distribution tree; Copyright.html can be found at the
@REM root level of an installed copy of the electronic HDF5 document set and
@REM is linked from the top-level documents page.  It can also be found at
@REM http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
@REM access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.

@REM File Name: convert_to_vcproj.bat
@REM Purpose: Convert Visual C++ 6.0 project format to Visual Studio .net project format.
@REM Written By: Fang GUO
@REM Date: May 27, 2005
@REM Update: June 7, 2005

::@echo off

type nul > convert.log
echo.>>convert.log
echo This batch file will convert all .dsp files in HDF5 C,  >> convert.log
echo C++ and Fortran library to .vcproj  format >> convert.log

CScript //H:CScript //Nologo

echo.
echo. START converting files .dsp to .vcproj
echo.

for /R  %CD%\windows\ %%i in (*.vcproj) do (
del %%i
)

for /R  %CD%\windows\ %%i in (*.dsp) do (
convert.js %%i %%~pi%%~ni.vcproj >> convert.log
)

echo.
echo. END converting files .dsp to .vcproj
echo.
