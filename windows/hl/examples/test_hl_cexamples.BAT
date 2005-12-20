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

@ECHO OFF
@:: This batch file is used to test HDF5 High Level C examples.
@:: Written by   : Fang GUO
@:: Created      : 07/26/2005
@:: Last Modified: 

if %1.==. GOTO WRONG
if "%1"=="/?" GOTO HELP
if "%1"=="release" GOTO CONFIG
if "%1"=="debug" GOTO CONFIG

GOTO WRONG

:CONFIG
if "%2"=="dll" GOTO TEST
if %2.==. GOTO TEST
GOTO WRONG

:TEST
echo Testing %1 %2 version of High Level examples 
echo.
HLCexamples%1%2\ex_lite%2 
HLCexamples%1%2\ex_images%2 
HLCexamples%1%2\ex_table%2

GOTO END

:WRONG
echo The syntax of the command is incorrect.
echo Use test_hl_cexamples /? for usage informtaion
echo.
GOTO END

:HELP
echo Tests HDF5 High Level C examples.
echo.
echo test_hl_cexamples [OPTION]
echo.
echo Please use one of the following options!
echo.
echo test_hl_cexamples release     test HDF5 HL C examples -- release version
echo test_hl_cexamples release dll test HDF5 HL C examples -- release dll version
echo test_hl_cexamples debug       test HDF5 HL C examples -- debug version
echo test_hl_cexamples debug dll   test HDF5 HL C examples -- debug dll version
echo test_hl_cexamples /?          Help information
echo.
GOTO END

:END
