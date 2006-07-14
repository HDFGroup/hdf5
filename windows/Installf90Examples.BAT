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
REM This batch file is used to install HDF5 Fortran examples'
REM executable files.
REM By Xuan Bai
REM Created on: 10/20/2004
REM Last Modified: 10/28/2004

cd fortran\examples

mkdir f90examplesREL
mkdir f90examplesRELDLL
mkdir f90examplesDBG
mkdir f90examplesDBGDLL

cd attreexampletest
copy debug\attreexampletest.exe ..\f90examplesDBG\
copy release\attreexampletest.exe ..\f90examplesREL\
cd ..

cd attreexampletestdll
copy debug\attreexampletestdll.exe ..\f90examplesDBGDLL\
copy release\attreexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd compoundtest
copy debug\compoundtest.exe ..\f90examplesDBG\
copy release\compoundtest.exe ..\f90examplesREL\
cd ..

cd compoundtestdll
copy debug\compoundtestdll.exe ..\f90examplesDBGDLL\
copy release\compoundtestdll.exe ..\f90examplesRELDLL\
cd ..

cd dsetexampletest
copy debug\dsetexampletest.exe ..\f90examplesDBG\
copy release\dsetexampletest.exe ..\f90examplesREL\
cd ..

cd dsetexampletestdll
copy debug\dsetexampletestdll.exe ..\f90examplesDBGDLL\
copy release\dsetexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd fileexampletest
copy debug\fileexampletest.exe ..\f90examplesDBG\
copy release\fileexampletest.exe ..\f90examplesREL\
cd ..

cd fileexampletestdll
copy debug\fileexampletestdll.exe ..\f90examplesDBGDLL\
copy release\fileexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd groupexampletest
copy debug\groupexampletest.exe ..\f90examplesDBG\
copy release\groupexampletest.exe ..\f90examplesREL\
cd ..

cd groupexampletestdll
copy debug\groupexampletestdll.exe ..\f90examplesDBGDLL\
copy release\groupexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd grpdsetexampletest
copy debug\grpdsetexampletest.exe ..\f90examplesDBG\
copy release\grpdsetexampletest.exe ..\f90examplesREL\
cd ..

cd grpdsetexampletestdll
copy debug\grpdsetexampletestdll.exe ..\f90examplesDBGDLL\
copy release\grpdsetexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd grpittest
copy debug\grpittest.exe ..\f90examplesDBG\
copy release\grpittest.exe ..\f90examplesREL\
cd ..

cd grpittestdll
copy debug\grpittestdll.exe ..\f90examplesDBGDLL\
copy release\grpittestdll.exe ..\f90examplesRELDLL\
cd ..

cd grpsexampletest
copy debug\grpsexampletest.exe ..\f90examplesDBG\
copy release\grpsexampletest.exe ..\f90examplesREL\
cd ..

cd grpsexampletestdll
copy debug\grpsexampletestdll.exe ..\f90examplesDBGDLL\
copy release\grpsexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd hyperslabtest
copy debug\hyperslabtest.exe ..\f90examplesDBG\
copy release\hyperslabtest.exe ..\f90examplesREL\
cd ..

cd hyperslabtestdll
copy debug\hyperslabtestdll.exe ..\f90examplesDBGDLL\
copy release\hyperslabtestdll.exe ..\f90examplesRELDLL\
cd ..

cd mountexampletest
copy debug\mountexampletest.exe ..\f90examplesDBG\
copy release\mountexampletest.exe ..\f90examplesREL\
cd ..

cd mountexampletestdll
copy debug\mountexampletestdll.exe ..\f90examplesDBGDLL\
copy release\mountexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd refobjexampletest
copy debug\refobjexampletest.exe ..\f90examplesDBG\
copy release\refobjexampletest.exe ..\f90examplesREL\
cd ..

cd refobjexampletestdll
copy debug\refobjexampletestdll.exe ..\f90examplesDBGDLL\
copy release\refobjexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd refregexampletest
copy debug\refregexampletest.exe ..\f90examplesDBG\
copy release\refregexampletest.exe ..\f90examplesREL\
cd ..

cd refregexampletestdll
copy debug\refregexampletestdll.exe ..\f90examplesDBGDLL\
copy release\refregexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd rwdsetexampletest
copy debug\rwdsetexampletest.exe ..\f90examplesDBG\
copy release\rwdsetexampletest.exe ..\f90examplesREL\
cd ..

cd rwdsetexampletestdll
copy debug\rwdsetexampletestdll.exe ..\f90examplesDBGDLL\
copy release\rwdsetexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd selecteletest
copy debug\selecteletest.exe ..\f90examplesDBG\
copy release\selecteletest.exe ..\f90examplesREL\
cd ..

cd selecteletestdll
copy debug\selecteletestdll.exe ..\f90examplesDBGDLL\
copy release\selecteletestdll.exe ..\f90examplesRELDLL\
cd ..

cd ..\..