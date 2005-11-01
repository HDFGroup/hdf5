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
@:: Last Modified: 

cd hl/hl_c_examples

mkdir HLCexamplesRELEASE
mkdir HLCexamplesRELEASEDLL
mkdir HLCexamplesDEBUG
mkdir HLCexamplesDEBUGDLL

cd ex_images
copy debug\ex_images.exe ..\HLCexamplesDEBUG\
copy release\ex_images.exe ..\HLCexamplesRELEASE\
cd ..

cd ex_imagesdll
copy debug\ex_imagesdll.exe ..\HLCexamplesDEBUGDLL\
copy release\ex_imagesdll.exe ..\HLCexamplesRELEASEDLL\
cd ..

cd ex_lite
copy debug\ex_lite.exe ..\HLCexamplesDEBUG\
copy release\ex_lite.exe ..\HLCexamplesRELEASE\
cd ..

cd ex_litedll
copy debug\ex_litedll.exe ..\HLCexamplesDEBUGDLL\
copy release\ex_litedll.exe ..\HLCexamplesRELEASEDLL\
cd ..


cd ex_table
copy debug\ex_table.exe ..\HLCexamplesDEBUG\
copy release\ex_table.exe ..\HLCexamplesRELEASE\
cd ..

cd ex_tabledll
copy debug\ex_tabledll.exe ..\HLCexamplesDEBUGDLL\
copy release\ex_tabledll.exe ..\HLCexamplesRELEASEDLL\
cd ..
