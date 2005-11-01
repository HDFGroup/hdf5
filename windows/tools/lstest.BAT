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

@ ECHO OFF

mkdir temptest

cd h5ls%2\%1

h5ls%2 -w80 -h > ..\..\temptest\help-1.results 2>..\..\temptest\help-1_error.results
more ..\..\temptest\help-1_error.results >> ..\..\temptest\help-1.results
del ..\..\temptest\help-1_error.results

h5ls%2 -w80 -help > ..\..\temptest\help-2.results 2>..\..\temptest\help-2_error.results
more ..\..\temptest\help-2_error.results >> ..\..\temptest\help-2.results
del ..\..\temptest\help-2_error.results

h5ls%2 -w80 -? > ..\..\temptest\help-3.results 2>..\..\temptest\help-3_error.results
more ..\..\temptest\help-3_error.results >> ..\..\temptest\help-3.results
del ..\..\temptest\help-3_error.results


h5ls%2 -w80 ..\..\testfiles\tall.h5 > ..\..\temptest\tall-1.results 2>..\..\temptest\tall-1_error.results
more ..\..\temptest\tall-1_error.results >> ..\..\temptest\tall-1.results
del ..\..\temptest\tall-1_error.results

h5ls%2 -w80 -r -d ..\..\testfiles\tall.h5 > ..\..\temptest\tall-2.results 2>..\..\temptest\tall-2_error.results
more ..\..\temptest\tall-2_error.results >> ..\..\temptest\tall-2.results
del ..\..\temptest\tall-2_error.results

h5ls%2 -w80 ..\..\testfiles\tgroup.h5 > ..\..\temptest\tgroup.results 2>..\..\temptest\tgroup_error.results
more ..\..\temptest\tgroup_error.results >> ..\..\temptest\tgroup.results
del ..\..\temptest\tgroup_error.results

h5ls%2 -w80 -r -g ..\..\testfiles\tgroup.h5 > ..\..\temptest\tgroup-1.results 2>..\..\temptest\tgroup-1_error.results
more ..\..\temptest\tgroup-1_error.results >> ..\..\temptest\tgroup-1.results
del ..\..\temptest\tgroup-1_error.results

h5ls%2 -w80 -r -d ..\..\testfiles\tdset.h5 > ..\..\temptest\tdset-1.results 2>..\..\temptest\tdset-1_error.results
more ..\..\temptest\tdset-1_error.results >> ..\..\temptest\tdset-1.results
del ..\..\temptest\tdset-1_error.results

h5ls%2 -w80 -r ..\..\testfiles\tslink.h5 > ..\..\temptest\tslink-1.results 2>..\..\temptest\tslink-1_error.results
more ..\..\temptest\tslink-1_error.results >> ..\..\temptest\tslink-1.results
del ..\..\temptest\tslink-1_error.results

h5ls%2 -w80 ..\..\testfiles\thlink.h5 > ..\..\temptest\thlink-1.results 2>..\..\temptest\thlink-1_error.results
more ..\..\temptest\thlink-1_error.results >> ..\..\temptest\thlink-1.results
del ..\..\temptest\thlink-1_error.results

h5ls%2 -w80 -r -d ..\..\testfiles\tcompound.h5 > ..\..\temptest\tcomp-1.results 2>..\..\temptest\tcomp-1_error.results
more ..\..\temptest\tcomp-1_error.results >> ..\..\temptest\tcomp-1.results
del ..\..\temptest\tcomp-1_error.results

h5ls%2 -w80 -r -d ..\..\testfiles\tnestedcomp.h5 > ..\..\temptest\tnestcomp-1.results 2>..\..\temptest\tnestcomp-1_error.results
more ..\..\temptest\tnestcomp-1_error.results >> ..\..\temptest\tnestcomp-1.results
del ..\..\temptest\tnestcomp-1_error.results

h5ls%2 -w80 -r -d ..\..\testfiles\tloop.h5 > ..\..\temptest\tloop-1.results 2>..\..\temptest\tloop-1_error.results
more ..\..\temptest\tloop-1_error.results >> ..\..\temptest\tloop-1.results
del ..\..\temptest\tloop-1_error.results

h5ls%2 -w80 -r -d ..\..\testfiles\tstr.h5 > ..\..\temptest\tstr-1.results 2>..\..\temptest\tstr-1_error.results
more ..\..\temptest\tstr-1_error.results >> ..\..\temptest\tstr-1.results
del ..\..\temptest\tstr-1_error.results

h5ls%2 -w80 -r -d ..\..\testfiles\tsaf.h5 > ..\..\temptest\tsaf.results 2>..\..\temptest\tsaf_error.results
more ..\..\temptest\tsaf_error.results >> ..\..\temptest\tsaf.results
del ..\..\temptest\tsaf_error.results

h5ls%2 -w80 -r -d ..\..\testfiles\tvldtypes1.h5 > ..\..\temptest\tvldtypes1.results 2>..\..\temptest\tvldtypes1_error.results
more ..\..\temptest\tvldtypes1_error.results >> ..\..\temptest\tvldtypes1.results
del ..\..\temptest\tvldtypes1_error.results

h5ls%2 -w80 -r -d ..\..\testfiles\tarray1.h5 > ..\..\temptest\tarray1.results 2>..\..\temptest\tarray1_error.results
more ..\..\temptest\tarray1_error.results >> ..\..\temptest\tarray1.results
del ..\..\temptest\tarray1_error.results

h5ls%2 -w80 -d ..\..\testfiles\tempty.h5 > ..\..\temptest\tempty.results 2>..\..\temptest\tempty_error.results
more ..\..\temptest\tempty_error.results >> ..\..\temptest\tempty.results
del ..\..\temptest\tempty_error.results

h5ls%2 -w80 -v -S ..\..\testfiles\tattr2.h5 > ..\..\temptest\tattr2.results 2>..\..\temptest\tattr2_error.results
more ..\..\temptest\tattr2_error.results >> ..\..\temptest\tattr2.results
del ..\..\temptest\tattr2_error.results

cd ..\..\temptest
mkdir fctemp

REM ##############################################################################
REM ##############################################################################
REM ###			        T H E   T E S T S                              ###
REM ##############################################################################
REM ##############################################################################

type nul > ..\lstest%2_%1.txt
echo ========================================== >> ..\lstest%2_%1.txt
echo Testing lstest.bat >> ..\lstest%2_%1.txt
echo ========================================== >> ..\lstest%2_%1.txt

REM Toss in a bunch of tests.  Not sure if they are the right kinds.
REM test the help syntax
call ..\deleteline help-1.results 1 
cd ..\testfiles
call ..\deleteline help-1.ls 4
cd ..\temptest\fctemp
fc help-1.ls help-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -h                                                   PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -h                                                   FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline help-2.results 1 
cd ..\testfiles
call ..\deleteline help-2.ls 4
cd ..\temptest\fctemp
fc help-2.ls help-2.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -help                                                PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -help                                                FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline help-3.results 1 
cd ..\testfiles
call ..\deleteline help-3.ls 4
cd ..\temptest\fctemp
fc help-3.ls help-3.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -?                                                   PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -?                                                   FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

REM test simple command
call ..\deleteline tall-1.results 1 
cd ..\testfiles
call ..\deleteline tall-1.ls 4
cd ..\temptest\fctemp
fc tall-1.ls tall-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 tall.h5                                              PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 tall.h5                                              FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tall-2.results 1 
cd ..\testfiles
call ..\deleteline tall-2.ls 4
cd ..\temptest\fctemp
fc tall-2.ls tall-2.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -r -d tall.h5                                        PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -r -d tall.h5                                        FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

call ..\deleteline tgroup.results 1 
cd ..\testfiles
call ..\deleteline tgroup.ls 4
cd ..\temptest\fctemp
fc tgroup.ls tgroup.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 tgroup.h5                                            PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 tgroup.h5                                            FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

REM test for displaying groups
call ..\deleteline tgroup-1.results 1 
cd ..\testfiles
call ..\deleteline tgroup-1.ls 4
cd ..\temptest\fctemp
fc tgroup-1.ls tgroup-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -r -g tgroup.h5                                      PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -r -g tgroup.h5                                      FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

REM test for displaying simple space datasets
call ..\deleteline tdset-1.results 1 
cd ..\testfiles
call ..\deleteline tdset-1.ls 4
cd ..\temptest\fctemp
fc tdset-1.ls tdset-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -r -d tdset.h5                                       PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -r -d tdset.h5                                       FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

REM test for displaying soft links
call ..\deleteline tslink-1.results 1 
cd ..\testfiles
call ..\deleteline tslink-1.ls 4
cd ..\temptest\fctemp
fc tslink-1.ls tslink-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -r tslink.h5                                         PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -r tslink.h5                                         FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

REM tests for hard links
call ..\deleteline thlink-1.results 1 
cd ..\testfiles
call ..\deleteline thlink-1.ls 4
cd ..\temptest\fctemp
fc thlink-1.ls thlink-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -w80 thlink.h5                                       PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -w80 thlink.h5                                       FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

REM tests for compound data types
call ..\deleteline tcomp-1.results 1 
cd ..\testfiles
call ..\deleteline tcomp-1.ls 4
cd ..\temptest\fctemp
fc tcomp-1.ls tcomp-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -r -d tcompound.h5                                   PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -r -d tcompound.h5                                   FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

REM test for the nested compound type
call ..\deleteline tnestcomp-1.results 1 
cd ..\testfiles
call ..\deleteline tnestcomp-1.ls 4
cd ..\temptest\fctemp
fc tnestcomp-1.ls tnestcomp-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -r -d tnestedcomp.h5                                 PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -r -d tnestedcomp.h5                                 FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

REM test for loop detection
call ..\deleteline tloop-1.results 1 
cd ..\testfiles
call ..\deleteline tloop-1.ls 4
cd ..\temptest\fctemp
fc tloop-1.ls tloop-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -r -d tloop.h5                                       PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -r -d tloop.h5                                       FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

REM test for string 
call ..\deleteline tstr-1.results 1 
cd ..\testfiles
call ..\deleteline tstr-1.ls 4
cd ..\temptest\fctemp
fc tstr-1.ls tstr-1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -r -d tstr.h5                                        PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -r -d tstr.h5                                        FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

REM test test file created from lib SAF team
call ..\deleteline tsaf.results 1 
cd ..\testfiles
call ..\deleteline tsaf.ls 4
cd ..\temptest\fctemp
fc tsaf.ls tsaf.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -r -d tsaf.h5                                        PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -r -d tsaf.h5                                        FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

REM test for variable length data types
call ..\deleteline tvldtypes1.results 1 
cd ..\testfiles
call ..\deleteline tvldtypes1.ls 4
cd ..\temptest\fctemp
fc tvldtypes1.ls tvldtypes1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -r -d tvldtypes1.h5                                  PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -r -d tvldtypes1.h5                                  FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

REM test for array data types
call ..\deleteline tarray1.results 1 
cd ..\testfiles
call ..\deleteline tarray1.ls 4
cd ..\temptest\fctemp
fc tarray1.ls tarray1.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -r -d tarray1.h5                                     PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -r -d tarray1.h5                                     FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

REM test for empty data
call ..\deleteline tempty.results 1 
cd ..\testfiles
call ..\deleteline tempty.ls 4
cd ..\temptest\fctemp
fc tempty.ls tempty.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -d tempty.h5                                         PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -d tempty.h5                                         FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

REM test for all dataset types written to attributes
REM enable -S for avoiding printing NATIVE types
call ..\mask tattr2.results tattr2_temp.results
del tattr2.results
rename tattr2_temp.results tattr2.results
call ..\deleteline tattr2.results 1
cd ..\testfiles
call ..\mask tattr2.ls tattr2_temp.ls
del tattr2.ls
rename tattr2_temp.ls tattr2.ls
call ..\deleteline tattr2.ls 4
cd ..\temptest\fctemp
fc tattr2.ls tattr2.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5ls -w80 -v -S tattr2.h5                                      PASSED >> ..\..\lstest%2_%1.txt
) else (
   echo Testing h5ls -w80 -v -S tattr2.h5                                      FAILED >> ..\..\lstest%2_%1.txt
   more temp.txt >> ..\..\lstest%2_%1.txt
)
del temp.txt
cd ..

cd ..

rmdir /s/q temptest