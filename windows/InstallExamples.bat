@REM Copyright by The HDF Group.
@REM Copyright by the Board of Trustees of the University of Illinois.
@REM All rights reserved.
@REM
@REM This file is part of HDF5.  The full HDF5 copyright notice, including
@REM terms governing use, modification, and redistribution, is contained in
@REM the files COPYING and Copyright.html.  COPYING can be found at the root
@REM of the source code distribution tree; Copyright.html can be found at the
@REM root level of an installed copy of the electronic HDF5 document set and
@REM is linked from the top-level documents page.  It can also be found at
@REM http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
@REM access to either file, you may request a copy from help@hdfgroup.org.


@ECHO OFF
REM This batch file is used to install HDF5 C examples'
REM executable files.
REM By Xuan Bai
REM Created on: 9/20/2004
REM Last Modified: 10/27/2004

cd examples

mkdir examplesREL
mkdir examplesRELDLL
mkdir examplesDBG
mkdir examplesDBGDLL

cd attributetest
copy debug\attributetest.exe ..\examplesDBG\
copy release\attributetest.exe ..\examplesREL\
cd ..

cd attributetestdll
copy debug\attributetestdll.exe ..\examplesDBGDLL\
copy release\attributetestdll.exe ..\examplesRELDLL\
cd ..

cd chunkread
copy debug\chunkread.exe ..\examplesDBG\
copy release\chunkread.exe ..\examplesREL\
cd ..

cd chunkreaddll
copy debug\chunkreaddll.exe ..\examplesDBGDLL\
copy release\chunkreaddll.exe ..\examplesRELDLL\
cd ..

cd compoundtest
copy debug\compoundtest.exe ..\examplesDBG\
copy release\compoundtest.exe ..\examplesREL\
cd ..

cd compoundtestdll
copy debug\compoundtestdll.exe ..\examplesDBGDLL\
copy release\compoundtestdll.exe ..\examplesRELDLL\
cd ..

cd extendwritetest
copy debug\extendwritetest.exe ..\examplesDBG\
copy release\extendwritetest.exe ..\examplesREL\
cd ..

cd extendwritetestdll
copy debug\extendwritetestdll.exe ..\examplesDBGDLL\
copy release\extendwritetestdll.exe ..\examplesRELDLL\
cd ..

cd grouptest
copy debug\grouptest.exe ..\examplesDBG\
copy release\grouptest.exe ..\examplesREL\
cd ..

cd grouptestdll
copy debug\grouptestdll.exe ..\examplesDBGDLL\
copy release\grouptestdll.exe ..\examplesRELDLL\
cd ..

cd intermgrouptest
copy debug\intermgrouptest.exe ..\examplesDBG\
copy release\intermgrouptest.exe ..\examplesREL\
cd ..

cd intermgrouptestdll
copy debug\intermgrouptestdll.exe ..\examplesDBGDLL\
copy release\intermgrouptestdll.exe ..\examplesRELDLL\
cd ..

cd readtest
copy debug\readtest.exe ..\examplesDBG\
copy release\readtest.exe ..\examplesREL\
cd ..

cd readtestdll
copy debug\readtestdll.exe ..\examplesDBGDLL\
copy release\readtestdll.exe ..\examplesRELDLL\
cd ..

cd selectest
copy debug\selectest.exe ..\examplesDBG\
copy release\selectest.exe ..\examplesREL\
cd ..

cd selectestdll
copy debug\selectestdll.exe ..\examplesDBGDLL\
copy release\selectestdll.exe ..\examplesRELDLL\
cd ..

cd writetest
copy debug\writetest.exe ..\examplesDBG\
copy release\writetest.exe ..\examplesREL\
cd ..

cd writetestdll
copy debug\writetestdll.exe ..\examplesDBGDLL\
copy release\writetestdll.exe ..\examplesRELDLL\
cd ..

cd ..