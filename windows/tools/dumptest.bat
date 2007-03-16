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

@echo off

::This batch file is for h5dump general and xml tests.
::Usage
:: dumptest release  -- release static version
:: dumptest debug  -- debug static version
:: dumptest release dll -- release dll version
:: dumptest debug dll -- debug dll version
:: Written By: Fang GUO
:: Date      : Jan. 12, 2006


:: Track total number of testing errors
   set /A totalerr=0
   set /A totalskip=0


::*******************************************************
:: Generate a new temp directory for test h5dump 
::*******************************************************
		:: Make a temporary directory for h5dump test
    mkdir temptest

::******************************************************************
:: Set the Environment Variables & Change Dir to Dir with h5dump.EXE
::******************************************************************

		:: The first incoming parameter should be configuration (debug or release)
		   set p1=%1
		:: The second parameter should be the types of library (dll or blank)
		   set p2=%2

    :: Before comparison between expected file and the actual outputfile, 
    :: Ignore the first line in actual outputfile and the first four lines
    :: in the expected output file
       
       set ln=1
			 set ln_exp=4

    :: Save the tests output into a temporary file 			
       set tempResults=dumptest%2_%1.txt
       type nul > %tempResults%

    :: Define the .exe file based on the second parameter
       set exefile=h5dump%p2%
    
    :: For convenience, set a variable for the sub batch file 
       set tooltest=..\..\tooltest
   
    :: For convenience, set variable for all testfiles/*.h5 files
       for %%i in (testfiles\t*.h5) do set %%~ni=..\..\%%i
    
    :: Special cases: the following are not .h5 files but needed in the tests       
       for %%i in (tsplit_file tmulti) do (
         set %%i=..\..\testfiles\%%i
       )
       
    :: Havn't find a way to use % inside a variable name
			 set tfamily05d=..\..\testfiles\tfamily%%05d.h5

    :: Change the directory to dir including .exe file
       cd h5dump%p2%\%p1%

::Test h5dump with different options

echo.****************************************************
echo.       h5dump%2 %1   T E S T S            
echo.****************************************************

::--------------------------
::test for displaying groups
::--------------------------
set flag=%tgroup%
call %tooltest% tgroup-1.ddl

::---------------------------------------
::test for displaying the selected groups
::---------------------------------------
set flag=--group=/g2 --group / -g /y %tgroup%
call %tooltest% tgroup-2.ddl

:: -----------------------------------------
:: test for displaying simple space datasets
::------------------------------------------
set flag=%tdset%
call %tooltest% tdset-1.ddl

::--------------------------------------
::test for displaying selected datasets
::--------------------------------------
set flag=-H -d dset1 -d /dset2 --dataset=dset3 %tdset%
call %tooltest% tdset-2.ddl

::------------------------------
::test for displaying attributes
::------------------------------
set flag=%tattr%
call %tooltest% tattr-1.ddl

::---------------------------------------------------------------------------
::test for displaying the selected attributes of string type and scalar space
::---------------------------------------------------------------------------
set flag=-a /attr1 --attribute /attr4 --attribute=/attr5 %tattr%
call %tooltest% tattr-2.ddl

::----------------------------------
::test for header and error messages
::----------------------------------
set flag=--header -a /attr2 --attribute=/attr %tattr%
call %tooltest% tattr-3.ddl

::-----------------------------------------------------------------------------
::test for displaying attributes in shared datatype (also in group and dataset)
::-----------------------------------------------------------------------------
set flag=%tnamed_dtype_attr% 
call %tooltest% tnamed_dtype_attr.ddl

::------------------------------
::test for displaying soft links
::------------------------------
set flag=%tslink%
call %tooltest% tslink-1.ddl

set flag=%tudlink%
call %tooltest% tudlink-1.ddl
::-------------------------------------
::test for displaying the selected link
::-------------------------------------
set flag=-l slink2 %tslink%
call %tooltest% tslink-2.ddl

set flag=-l udlink2 %tudlink%
call %tooltest% tudlink-2.ddl

::--------------------
::tests for hard links
::--------------------
set flag=%thlink%
call %tooltest% thlink-1.ddl

set flag=-d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 %thlink%
call %tooltest% thlink-2.ddl

set flag=-d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 %thlink%
call %tooltest% thlink-3.ddl

set flag=-g /g1 %thlink%
call %tooltest% thlink-4.ddl

set flag=-d /dset1 -g /g2 -d /g1/dset2 %thlink%
call %tooltest% thlink-5.ddl

::-----------------------------
::tests for compound data types
::-----------------------------
set flag=%tcompound%
call %tooltest% tcomp-1.ddl

::-------------------------
::test for named data types
::-------------------------
set flag=-t /type1 --datatype /type2 --datatype=/group1/type3 %tcompound%
call %tooltest% tcomp-2.ddl

::--------------------
::test for unamed type
::--------------------
set flag=-t /#6632:0 -g /group2 %tcompound%
call %tooltest% tcomp-3.ddl

::----------------------------------
::test complicated compound datatype
::----------------------------------
set flag=%tcompound_complex%
call %tooltest% tcomp-4.ddl

::---------------------------------
::test for the nested compound type
::---------------------------------
set flag=%tnestedcomp%
call %tooltest% tnestcomp-1.ddl

::----------------
::test for options
::----------------
set flag=%tall%
call %tooltest% tall-1.ddl

set flag=--header -g /g1/g1.1 -a attr2 %tall%
call %tooltest% tall-2.ddl

set flag=-d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink %tall%
call %tooltest% tall-3.ddl

::------------------------
::test for loop detection
::------------------------
set flag=%tloop%
call %tooltest% tloop-1.ddl

::---------------
::test for string 
::---------------
set flag=%tstr%
call %tooltest% tstr-1.ddl

set flag=%tstr2%
call %tooltest% tstr-2.ddl

::-------------------------------------
::test for file created by Lib SAF team
::-------------------------------------
set flag=%tsaf%
call %tooltest% tsaf.ddl

::---------------------------------------
::test for file with variable length data
::---------------------------------------
set tvldtypes=..\..\testfiles\tvldtypes
for %%v in (1 2 3 4 5) do (
set flag=%tvldtypes%%%v.h5
call %tooltest% tvldtypes%%v.ddl
)
set tvldtypes=

::----------------------------------------------
::test for file with variable length string data
::----------------------------------------------
set flag=%tvlstr%
call %tooltest% tvlstr.ddl

::------------------------------
::test for files with array data
::------------------------------
set tarray=..\..\testfiles\tarray
for /L %%v in (1,1,7) do (
set flag=%tarray%%%v%.h5
call %tooltest% tarray%%v.ddl
)
set tarray=

::------------------------------
::test for files with empty data
::------------------------------
set flag=%tempty%
call %tooltest% tempty.ddl

::---------------------------------------------
::test for files with groups that have comments
::---------------------------------------------
set flag=%tgrp_comments%
call %tooltest% tgrp_comments.ddl

::---------------------------
::test the --filedriver flag
::---------------------------
set flag=--filedriver=split %tsplit_file%
call %tooltest% tsplit_file.ddl

::Special: Find a way to echo environment
set flag=--filedriver=family %tfamily05d%
call %tooltest% tfamily.ddl

set flag=--filedriver=multi %tmulti%
call %tooltest% tmulti.ddl

::----------------------------------------------------------------
::test for files with group names which reach > 1024 bytes in size
::----------------------------------------------------------------
set flag=-w157 %tlarge_objname%
call %tooltest% tlarge_objname.ddl

::-------------------------------------------
::test '-A' to suppress data but print attr's
::-------------------------------------------
set flag=-A %tall%
call %tooltest% tall-2A.ddl

::---------------------------------------------------------
::test '-r' to print attributes in ASCII instead of decimal
::---------------------------------------------------------
set flag=-A -r %tall%
call %tooltest% tall-2B.ddl

::---------------
::test Subsetting
::---------------
set flag=--dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 %tall%
call %tooltest% tall-4s.ddl

set flag=-d "/g1/g1.1/dset1.1.2[0;2;10;]" %tall%
call %tooltest% tall-5s.ddl

set flag=-d "/dset1[1,1;;;]" %tdset%
call %tooltest% tdset-3s.ddl

::set flag=-d "/dset1[;3,2;4,4;1,4]" %tdset2%
::call %tooltest% tdset2-1s.ddl

::----------------------------------------------------
::test printing characters in ASCII instead of decimal
::----------------------------------------------------
set flag=-r %tchar%
call %tooltest% tchar1.ddl

::---------------------
::test failure handling
::---------------------

::1. Missing file name
set flag= 
call %tooltest% tnofilename.ddl

:: rev. 2004

::---------------------
::tests for super block
::---------------------
set flag=-H -B -d dset %tfcontents1%
call %tooltest% tboot1.ddl

set flag=-B %tfcontents2%
call %tooltest% tboot2.ddl

::------------------------------------
:: test -p with a non existing dataset
::------------------------------------
set flag=-p -d bogus %tfcontents1%
call %tooltest% tperror.ddl

::----------------------
::test for file contents
::----------------------
set flag=-n %tfcontents1%
call %tooltest% tcontents.ddl

::-------------------------
::tests for storage layout
::-------------------------

for %%v in (compact chunked external) do (
set flag=-H -p -d  %%v %tfilters%
call %tooltest% t%%v.ddl
)

::contiguous (tcontiguos.ddl or tcontiguous.ddl)
set flag=-H -p -d contiguous %tfilters%
call %tooltest% tcontiguos.ddl

::-----------
::fill values
::-----------
set flag=-p %tfvalues%
call %tooltest% tfill.ddl

::-----------------------------------------------
::several datatype, with references , print path
::-----------------------------------------------
set flag=%tattr2%
call %tooltest% treference.ddl

::------------------------------------------
::escape/not escape non printable characters
::------------------------------------------
set flag=-e %tstr3%
call %tooltest% tstringe.ddl

set flag=%tstr3%
call %tooltest% tstring.ddl

::----------------------------------
::char data as ASCII with non escape
::----------------------------------
set flag=-r -d str4 %tstr3%
call %tooltest% tstring2.ddl

::-----------------------------
::array indices print/not print
::-----------------------------
set flag=%taindices%
call %tooltest% tindicesyes.ddl

set flag=-y %taindices%
call %tooltest% tindicesno.ddl


::-----------------------------
::array indices with subsetting
::-----------------------------

set flag=-d 1d -s 3 -c 40 %taindices%
call %tooltest% tindicessub1.ddl

set flag=-d 2d -s 1,3 -c 6,4 %taindices%
call %tooltest% tindicessub2.ddl

set flag=-d 3d -s 0,1,3 -c 2,6,4 %taindices%
call %tooltest% tindicessub3.ddl

set flag=-d 4d -s 0,0,1,3 -c 2,2,6,4 %taindices%
call %tooltest% tindicessub4.ddl

::-----------------
::tests for filters
::-----------------

:: SZIP
set flag=-H -p -d szip %tfilters%
call %tooltest% tszip.ddl szip

::deflate
set flag=-H -p -d deflate %tfilters%
call %tooltest% tdeflate.ddl zlib

::shuffle
set flag=-H -p -d shuffle %tfilters%
call %tooltest% tshuffle.ddl

::fletcher32
set flag=-H -p -d fletcher32 %tfilters%
call %tooltest% tfletcher32.ddl

::nbit
set flag=-H -p -d nbit %tfilters%
call %tooltest% tnbit.ddl

::scaleoffset
set flag=-H -p -d scaleoffset %tfilters%
call %tooltest% tscaleoffset.ddl

::all
set flag=-H -p -d all  %tfilters%
call %tooltest% tallfilters.ddl zlib szip

::user defined
set flag=-H  -p -d myfilter  %tfilters%
call %tooltest% tuserfilter.ddl

::test for displaying objects with very long names
set flag=%tlonglinks%
call %tooltest% tlonglinks.ddl

::dimensions over 4GB, print boundary 
set flag=-d dset4gb -s 4294967284 -c 22 %tbigdims%
call %tooltest% tbigdims.ddl

::hyperslab read
set flag=%thyperslab%
call %tooltest% thyperslab.ddl

::test for displaying dataset and attribute of null space
set flag=%tnullspace%
call %tooltest% tnullspace.ddl


::test for long double (Some systems do not have long double)
set flag=%tldouble%
call %tooltest% tldouble.ddl SKIP

::Test for vms
set flag=%tvms%
call %tooltest% tvms.ddl


::test for binary output
set flag=-d integer -o out1.bin -b LE %tbinary%
call %tooltest% tbin1.ddl

set flag=-d float   -o out2.bin -b BE %tbinary%
call %tooltest% tbin2.ddl

set flag=-d array   -o out3.bin -b MEMORY %tbinary%
call %tooltest% tbin3.ddl

set flag=-d double  -o out4.bin -b FILE %tbinary%
call %tooltest% tbin4.ddl

::test for dataset region references 
set flag=%tdatareg%
call %tooltest% tregref.ddl

echo. 
echo.****************************************************
echo.       h5dump%2 %1 XML T E S T S            
echo.****************************************************

::test XML
set flag=--xml %tall%
call %tooltest%  tall.h5.xml

set flag=--xml %tattr%
call %tooltest%  tattr.h5.xml 

set flag=--xml %tbitfields%
call %tooltest%  tbitfields.h5.xml

set flag=--xml %tcompound%
call %tooltest%  tcompound.h5.xml

set flag=--xml %tcompound2%
call %tooltest%  tcompound2.h5.xml

set flag=--xml %tdatareg%
call %tooltest%  tdatareg.h5.xml

set flag=--xml %tdset%
call %tooltest%  tdset.h5.xml

set flag=--xml %tdset2%
call %tooltest%  tdset2.h5.xml

set flag=--xml %tenum%
call %tooltest%  tenum.h5.xml

set flag=--xml %tgroup%
call %tooltest%  tgroup.h5.xml

set flag=--xml %thlink%
call %tooltest%  thlink.h5.xml SKIP

set flag=--xml %tloop%
call %tooltest%  tloop.h5.xml

set flag=--xml %tloop2%
call %tooltest%  tloop2.h5.xml

set flag=--xml %tmany%
call %tooltest%  tmany.h5.xml SKIP

set flag=--xml %tnestedcomp%
call %tooltest%  tnestedcomp.h5.xml

set flag=--xml %tcompound_complex%
call %tooltest%  tcompound_complex.h5.xml

set flag=--xml %tobjref%
call %tooltest%  tobjref.h5.xml

set flag=--xml %topaque%
call %tooltest%  topaque.h5.xml

set flag=--xml %tslink%
call %tooltest%  tslink.h5.xml

set flag=--xml %tstr%
call %tooltest%  tstr.h5.xml

set flag=--xml %tstr2%
call %tooltest%  tstr2.h5.xml

set flag=--xml %tref%
call %tooltest%  tref.h5.xml

set flag=--xml %tname-amp%
call %tooltest%  tname-amp.h5.xml

set flag=--xml %tname-apos%
call %tooltest%  tname-apos.h5.xml

set flag=--xml %tname-gt%
call %tooltest%  tname-gt.h5.xml

set flag=--xml %tname-lt%
call %tooltest%  tname-lt.h5.xml

set flag=--xml %tname-quot%
call %tooltest%  tname-quot.h5.xml

set flag=--xml %tname-sp%
call %tooltest%  tname-sp.h5.xml

set flag=--xml %tstring%
call %tooltest%  tstring.h5.xml

set flag=--xml %tstring-at%
call %tooltest%  tstring-at.h5.xml

set flag=--xml %tref-escapes%
call %tooltest%  tref-escapes.h5.xml

set flag=--xml %tref-escapes-at%
call %tooltest%  tref-escapes-at.h5.xml

set flag=--xml %tnodata%
call %tooltest%  tnodata.h5.xml

set flag=--xml %tarray1%
call %tooltest%  tarray1.h5.xml

set flag=--xml %tarray2%
call %tooltest%  tarray2.h5.xml

set flag=--xml %tarray3%
call %tooltest%  tarray3.h5.xml

set flag=--xml %tarray6%
call %tooltest%  tarray6.h5.xml

set flag=--xml %tarray7%
call %tooltest%  tarray7.h5.xml

set flag=--xml %tvldtypes1%
call %tooltest%  tvldtypes1.h5.xml

set flag=--xml %tvldtypes2%
call %tooltest%  tvldtypes2.h5.xml

set flag=--xml %tvldtypes3%
call %tooltest%  tvldtypes3.h5.xml

set flag=--xml %tvldtypes4%
call %tooltest%  tvldtypes4.h5.xml

set flag=--xml %tvldtypes5%
call %tooltest%  tvldtypes5.h5.xml

set flag=--xml %tvlstr%
call %tooltest%  tvlstr.h5.xml

set flag=--xml %tsaf%
call %tooltest%  tsaf.h5.xml

set flag=--xml %tempty%
call %tooltest%  tempty.h5.xml

set flag=--xml %tnamed_dtype_attr%
call %tooltest%  tnamed_dtype_attr.h5.xml


::Test dataset and attribute of null space.  Commented out:
::wait until the XML schema is updated for null space. 
set flag=--xml tnulspace.h5
call %tooltest%  tnullspace.h5.xml SKIP

::other options for xml

set flag=--xml --use-dtd %tempty%
call %tooltest%  tempty-dtd.h5.xml 

set flag=--xml -u %tempty%
call %tooltest%  tempty-dtd-2.h5.xml

set flag=--xml -X ":" %tempty%
call %tooltest%  tempty-nons.h5.xml

set flag=--xml --xml-ns=":" %tempty%
call %tooltest%  tempty-nons-2.h5.xml

::Some of these combinations are syntactically correct but
::the URLs are dummies 

set flag=--xml -X "thing:" %tempty%
call %tooltest%  tempty-ns.h5.xml

set flag=--xml --xml-ns="thing:" %tempty%
call %tooltest%  tempty-ns-2.h5.xml

set flag=--xml --xml-ns=":" --xml-dtd="http://somewhere.net" %tempty%
call %tooltest%  tempty-nons-uri.h5.xml

set flag=--xml --use-dtd --xml-dtd="http://somewhere.net" %tempty%
call %tooltest%  tempty-dtd-uri.h5.xml

set flag=--xml -A %tall%
call %tooltest%  tall-2A.h5.xml

::------------------
::End of -xml Tests
::------------------

cd ..\..

echo.
if %totalerr%==0 (
echo. All of the %exefile% %p1% Tests Passed!
echo. All of the %exefile% %p1% Tests Passed! >> %tempResults%
) else (
echo. %exefile% %p1% Tests Finished with %totalerr% Errors!  
echo. %exefile% %p1% Tests Finished with %totalerr% Errors!>> %tempResults%
)

if not %totalskip%==0 (
echo. %totalskip% Tests in total Skiped!  >> %tempResults%
echo. %totalskip% Tests in total Skiped!
echo. 
find "SKIPED" %tempResults% | more +2
)

rmdir /s/q temptest

for %%i in (testfiles\t*.h5) do set %%~ni=
set test_szip=
set output_szip=
for %%v in (p1 p2 tempResults exefile tooltest ln ln_exp tsplit_file tmulti tfamily05d totalskip) do set %%v=
for %%i in (tsplit_file tmulti) do set %%i=
set tfamily05d=
