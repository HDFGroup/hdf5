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
@:: This batch file is used to install HDF5 High Level C 
@:: Examples' executable files.
@:: Written by   : Fang GUO
@:: Created on   : 07/25/2005
@:: Last Modified: 03/08/2006

cd hl/examples

mkdir HLCexamplesRELEASE
mkdir HLCexamplesRELEASEDLL
mkdir HLCexamplesDEBUG
mkdir HLCexamplesDEBUGDLL

for %%i in (images lite table ds packet) do (
copy ex_%%i\debug\ex_%%i.exe HLCexamplesDEBUG\
copy ex_%%i\release\ex_%%i.exe HLCexamplesRELEASE\
copy ex_%%idll\debug\ex_%%idll.exe HLCexamplesDEBUGDLL\
copy ex_%%idll\release\ex_%%idll.exe HLCexamplesRELEASEDLL\
)
