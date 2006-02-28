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


@REM File Name: convert_to_icproj.bat
@REM Purpose: Convert Visual C++ 6.0 project format to Visual Studio .net project format.
@REM Written By: Fang GUO
@REM Date: May 27, 2005
@REM Update: June 7, 2005
@REM Purpose: Convert Visual C++ 7.0 project format to Intel C++ project format

::@ECHO OFF

type nul >convert_intel.log
echo.
echo START Converting files in .vcproj to .icproj 
echo.

::Clean up if .icproj exists
for /R  %CD%\windows\ %%i in (*.icproj) do (
del %%i
)

echo.
echo START Converting files in .vcproj to .icproj 
echo.

::Start Converting
for /R %CD%\windows %%i in (*.vcproj) do (
ICProjConvert80 %%i /IC >>convert_intel.log
)

echo.
echo END Converting files in .vcproj to .icproj 
echo.