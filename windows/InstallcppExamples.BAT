@ECHO OFF
REM This batch file is used to install HDF5 C++ examples'
REM executable files.
REM By Xuan Bai
REM Created on: 10/20/2004
REM Last Modified: 10/27/2004

cd c++\examples

mkdir cppexamplesREL
mkdir cppexamplesRELDLL
mkdir cppexamplesDBG
mkdir cppexamplesDBGDLL

cd chunkstest
copy debug\chunkstest.exe ..\cppexamplesDBG\
copy release\chunkstest.exe ..\cppexamplesREL\
cd ..

cd chunkstestdll
copy debug\chunkstestdll.exe ..\cppexamplesDBGDLL\
copy release\chunkstestdll.exe ..\cppexamplesRELDLL\
cd ..

cd compoundtest
copy debug\compoundtest.exe ..\cppexamplesDBG\
copy release\compoundtest.exe ..\cppexamplesREL\
cd ..

cd compoundtestdll
copy debug\compoundtestdll.exe ..\cppexamplesDBGDLL\
copy release\compoundtestdll.exe ..\cppexamplesRELDLL\
cd ..

cd createtest
copy debug\createtest.exe ..\cppexamplesDBG\
copy release\createtest.exe ..\cppexamplesREL\
cd ..

cd createtestdll
copy debug\createtestdll.exe ..\cppexamplesDBGDLL\
copy release\createtestdll.exe ..\cppexamplesRELDLL\
cd ..

cd extend_dstest
copy debug\extend_dstest.exe ..\cppexamplesDBG\
copy release\extend_dstest.exe ..\cppexamplesREL\
cd ..

cd extend_dstestdll
copy debug\extend_dstestdll.exe ..\cppexamplesDBGDLL\
copy release\extend_dstestdll.exe ..\cppexamplesRELDLL\
cd ..

cd h5grouptest
copy debug\h5grouptest.exe ..\cppexamplesDBG\
copy release\h5grouptest.exe ..\cppexamplesREL\
cd ..

cd h5grouptestdll
copy debug\h5grouptestdll.exe ..\cppexamplesDBGDLL\
copy release\h5grouptestdll.exe ..\cppexamplesRELDLL\
cd ..

cd readdatatest
copy debug\readdatatest.exe ..\cppexamplesDBG\
copy release\readdatatest.exe ..\cppexamplesREL\
cd ..

cd readdatatestdll
copy debug\readdatatestdll.exe ..\cppexamplesDBGDLL\
copy release\readdatatestdll.exe ..\cppexamplesRELDLL\
cd ..

cd writedatatest
copy debug\writedatatest.exe ..\cppexamplesDBG\
copy release\writedatatest.exe ..\cppexamplesREL\
cd ..

cd writedatatestdll
copy debug\writedatatestdll.exe ..\cppexamplesDBGDLL\
copy release\writedatatestdll.exe ..\cppexamplesRELDLL\
cd ..

cd ..\..