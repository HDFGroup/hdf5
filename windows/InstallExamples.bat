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