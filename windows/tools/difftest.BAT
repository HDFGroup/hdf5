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
cd h5diff%2\%1
h5diff%2 -h > ..\..\temptest\h5diff_10.results 2>..\..\temptest\h5diff_10_error.results
more ..\..\temptest\h5diff_10_error.results >> ..\..\temptest\h5diff_10.results
del ..\..\temptest\h5diff_10_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 > ..\..\temptest\h5diff_11.results 2>..\..\temptest\h5diff_11_error.results
more ..\..\temptest\h5diff_11_error.results >> ..\..\temptest\h5diff_11.results
del ..\..\temptest\h5diff_11_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 g1/dset1 g1/dset2 > ..\..\temptest\h5diff_12.results 2>..\..\temptest\h5diff_12_error.results
more ..\..\temptest\h5diff_12_error.results >> ..\..\temptest\h5diff_12.results
del ..\..\temptest\h5diff_12_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -r > ..\..\temptest\h5diff_13.results 2>..\..\temptest\h5diff_13_error.results
more ..\..\temptest\h5diff_13_error.results >> ..\..\temptest\h5diff_13.results
del ..\..\temptest\h5diff_13_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -r g1/dset1 g1/dset2 > ..\..\temptest\h5diff_14.results 2>..\..\temptest\h5diff_14_error.results
more ..\..\temptest\h5diff_14_error.results >> ..\..\temptest\h5diff_14.results
del ..\..\temptest\h5diff_14_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -r -d 5 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_15.results 2>..\..\temptest\h5diff_15_error.results
more ..\..\temptest\h5diff_15_error.results >> ..\..\temptest\h5diff_15.results
del ..\..\temptest\h5diff_15_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -r -p 0.05 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_16.results 2>..\..\temptest\h5diff_16_error.results
more ..\..\temptest\h5diff_16_error.results >> ..\..\temptest\h5diff_16.results
del ..\..\temptest\h5diff_16_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -v > ..\..\temptest\h5diff_17.results 2>..\..\temptest\h5diff_17_error.results
more ..\..\temptest\h5diff_17_error.results >> ..\..\temptest\h5diff_17.results
del ..\..\temptest\h5diff_17_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -q > ..\..\temptest\h5diff_18.results 2>..\..\temptest\h5diff_18_error.results
more ..\..\temptest\h5diff_18_error.results >> ..\..\temptest\h5diff_18.results
del ..\..\temptest\h5diff_18_error.results

h5diff%2 ..\..\testfiles\file3.h5 ..\..\testfiles\file3.h5 -v dset group > ..\..\temptest\h5diff_20.results 2>..\..\temptest\h5diff_20_error.results
more ..\..\temptest\h5diff_20_error.results >> ..\..\temptest\h5diff_20.results
del ..\..\temptest\h5diff_20_error.results

h5diff%2 ..\..\testfiles\file3.h5 ..\..\testfiles\file3.h5 -v dset link > ..\..\temptest\h5diff_21.results 2>..\..\temptest\h5diff_21_error.results
more ..\..\temptest\h5diff_21_error.results >> ..\..\temptest\h5diff_21.results
del ..\..\temptest\h5diff_21_error.results

h5diff%2 ..\..\testfiles\file3.h5 ..\..\testfiles\file3.h5 -v dset type > ..\..\temptest\h5diff_22.results 2>..\..\temptest\h5diff_22_error.results
more ..\..\temptest\h5diff_22_error.results >> ..\..\temptest\h5diff_22.results
del ..\..\temptest\h5diff_22_error.results

h5diff%2 ..\..\testfiles\file3.h5 ..\..\testfiles\file3.h5 -v group group > ..\..\temptest\h5diff_23.results 2>..\..\temptest\h5diff_23_error.results
more ..\..\temptest\h5diff_23_error.results >> ..\..\temptest\h5diff_23.results
del ..\..\temptest\h5diff_23_error.results

h5diff%2 ..\..\testfiles\file3.h5 ..\..\testfiles\file3.h5 -v type type > ..\..\temptest\h5diff_24.results 2>..\..\temptest\h5diff_24_error.results
more ..\..\temptest\h5diff_24_error.results >> ..\..\temptest\h5diff_24.results
del ..\..\temptest\h5diff_24_error.results

h5diff%2 ..\..\testfiles\file3.h5 ..\..\testfiles\file3.h5 -v link link > ..\..\temptest\h5diff_25.results 2>..\..\temptest\h5diff_25_error.results
more ..\..\temptest\h5diff_25_error.results >> ..\..\temptest\h5diff_25.results
del ..\..\temptest\h5diff_25_error.results


h5diff%2 ..\..\testfiles\file4.h5 ..\..\testfiles\file4.h5 -v dset0a dset0b > ..\..\temptest\h5diff_50.results 2>..\..\temptest\h5diff_50_error.results
more ..\..\temptest\h5diff_50_error.results >> ..\..\temptest\h5diff_50.results
del ..\..\temptest\h5diff_50_error.results

h5diff%2 ..\..\testfiles\file4.h5 ..\..\testfiles\file4.h5 -v dset1a dset1b > ..\..\temptest\h5diff_51.results 2>..\..\temptest\h5diff_51_error.results
more ..\..\temptest\h5diff_51_error.results >> ..\..\temptest\h5diff_51.results
del ..\..\temptest\h5diff_51_error.results

h5diff%2 ..\..\testfiles\file4.h5 ..\..\testfiles\file4.h5 -v dset2a dset2b > ..\..\temptest\h5diff_52.results 2>..\..\temptest\h5diff_52_error.results
more ..\..\temptest\h5diff_52_error.results >> ..\..\temptest\h5diff_52.results
del ..\..\temptest\h5diff_52_error.results

h5diff%2 ..\..\testfiles\file4.h5 ..\..\testfiles\file4.h5 -v dset3a dset4b > ..\..\temptest\h5diff_53.results 2>..\..\temptest\h5diff_53_error.results
more ..\..\temptest\h5diff_53_error.results >> ..\..\temptest\h5diff_53.results
del ..\..\temptest\h5diff_53_error.results

h5diff%2 ..\..\testfiles\file4.h5 ..\..\testfiles\file4.h5 -v dset4a dset4b > ..\..\temptest\h5diff_54.results 2>..\..\temptest\h5diff_54_error.results
more ..\..\temptest\h5diff_54_error.results >> ..\..\temptest\h5diff_54.results
del ..\..\temptest\h5diff_54_error.results

h5diff%2 ..\..\testfiles\file4.h5 ..\..\testfiles\file4.h5 -v dset5a dset5b > ..\..\temptest\h5diff_55.results 2>..\..\temptest\h5diff_55_error.results
more ..\..\temptest\h5diff_55_error.results >> ..\..\temptest\h5diff_55.results
del ..\..\temptest\h5diff_55_error.results

h5diff%2 ..\..\testfiles\file4.h5 ..\..\testfiles\file4.h5 -v dset6a dset6b > ..\..\temptest\h5diff_56.results 2>..\..\temptest\h5diff_56_error.results
more ..\..\temptest\h5diff_56_error.results >> ..\..\temptest\h5diff_56.results
del ..\..\temptest\h5diff_56_error.results

h5diff%2 ..\..\testfiles\file4.h5 ..\..\testfiles\file4.h5 -v dset7a dset7b > ..\..\temptest\h5diff_57.results 2>..\..\temptest\h5diff_57_error.results
more ..\..\temptest\h5diff_57_error.results >> ..\..\temptest\h5diff_57.results
del ..\..\temptest\h5diff_57_error.results

REM h5diff%2 ..\..\testfiles\file4.h5 ..\..\testfiles\file4.h5 dset8a dset8b > ..\..\temptest\h5diff_58.results 2>..\..\temptest\h5diff_58_error.results
REM more ..\..\temptest\h5diff_58_error.results >> ..\..\temptest\h5diff_58.results
REM del ..\..\temptest\h5diff_58_error.results

h5diff%2 ..\..\testfiles\file1.h5 > ..\..\temptest\h5diff_600.results 2>..\..\temptest\h5diff_600_error.results
more ..\..\temptest\h5diff_600_error.results >> ..\..\temptest\h5diff_600.results
del ..\..\temptest\h5diff_600_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -x > ..\..\temptest\h5diff_601.results 2>..\..\temptest\h5diff_601_error.results
more ..\..\temptest\h5diff_601_error.results >> ..\..\temptest\h5diff_601.results
del ..\..\temptest\h5diff_601_error.results


h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -d g1/dset3 g1/dset4 > ..\..\temptest\h5diff_602.results 2>..\..\temptest\h5diff_602_error.results
more ..\..\temptest\h5diff_602_error.results >> ..\..\temptest\h5diff_602.results
del ..\..\temptest\h5diff_602_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -d -4 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_603.results 2>..\..\temptest\h5diff_603_error.results
more ..\..\temptest\h5diff_603_error.results >> ..\..\temptest\h5diff_603.results
del ..\..\temptest\h5diff_603_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -d 0 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_604.results 2>..\..\temptest\h5diff_604_error.results
more ..\..\temptest\h5diff_604_error.results >> ..\..\temptest\h5diff_604.results
del ..\..\temptest\h5diff_604_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -d u g1/dset3 g1/dset4 > ..\..\temptest\h5diff_605.results 2>..\..\temptest\h5diff_605_error.results
more ..\..\temptest\h5diff_605_error.results >> ..\..\temptest\h5diff_605.results
del ..\..\temptest\h5diff_605_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -d 0x1 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_606.results 2>..\..\temptest\h5diff_606_error.results
more ..\..\temptest\h5diff_606_error.results >> ..\..\temptest\h5diff_606.results
del ..\..\temptest\h5diff_606_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -d "1" g1/dset3 g1/dset4 > ..\..\temptest\h5diff_607.results 2>..\..\temptest\h5diff_607_error.results
more ..\..\temptest\h5diff_607_error.results >> ..\..\temptest\h5diff_607.results
del ..\..\temptest\h5diff_607_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -d 1 -d 2 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_608.results 2>..\..\temptest\h5diff_608_error.results
more ..\..\temptest\h5diff_608_error.results >> ..\..\temptest\h5diff_608.results
del ..\..\temptest\h5diff_608_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -d 200 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_609.results 2>..\..\temptest\h5diff_609_error.results
more ..\..\temptest\h5diff_609_error.results >> ..\..\temptest\h5diff_609.results
del ..\..\temptest\h5diff_609_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -d 1 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_610.results 2>..\..\temptest\h5diff_610_error.results
more ..\..\temptest\h5diff_610_error.results >> ..\..\temptest\h5diff_610.results
del ..\..\temptest\h5diff_610_error.results


h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -r -p g1/dset3 g1/dset4 > ..\..\temptest\h5diff_611.results 2>..\..\temptest\h5diff_611_error.results
more ..\..\temptest\h5diff_611_error.results >> ..\..\temptest\h5diff_611.results
del ..\..\temptest\h5diff_611_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -p -4 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_612.results 2>..\..\temptest\h5diff_612_error.results
more ..\..\temptest\h5diff_612_error.results >> ..\..\temptest\h5diff_612.results
del ..\..\temptest\h5diff_612_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -p 0 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_613.results 2>..\..\temptest\h5diff_613_error.results
more ..\..\temptest\h5diff_613_error.results >> ..\..\temptest\h5diff_613.results
del ..\..\temptest\h5diff_613_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -p u g1/dset3 g1/dset4 > ..\..\temptest\h5diff_614.results 2>..\..\temptest\h5diff_614_error.results
more ..\..\temptest\h5diff_614_error.results >> ..\..\temptest\h5diff_614.results
del ..\..\temptest\h5diff_614_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -p 0x1 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_615.results 2>..\..\temptest\h5diff_615_error.results
more ..\..\temptest\h5diff_615_error.results >> ..\..\temptest\h5diff_615.results
del ..\..\temptest\h5diff_615_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -p "0.21" g1/dset3 g1/dset4 > ..\..\temptest\h5diff_616.results 2>..\..\temptest\h5diff_616_error.results
more ..\..\temptest\h5diff_616_error.results >> ..\..\temptest\h5diff_616.results
del ..\..\temptest\h5diff_616_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -p 0.21 -p 0.22 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_617.results 2>..\..\temptest\h5diff_617_error.results
more ..\..\temptest\h5diff_617_error.results >> ..\..\temptest\h5diff_617.results
del ..\..\temptest\h5diff_617_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -p 2 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_618.results 2>..\..\temptest\h5diff_618_error.results
more ..\..\temptest\h5diff_618_error.results >> ..\..\temptest\h5diff_618.results
del ..\..\temptest\h5diff_618_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -p 0.005 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_619.results 2>..\..\temptest\h5diff_619_error.results
more ..\..\temptest\h5diff_619_error.results >> ..\..\temptest\h5diff_619.results
del ..\..\temptest\h5diff_619_error.results


h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -n g1/dset3 g1/dset4 > ..\..\temptest\h5diff_620.results 2>..\..\temptest\h5diff_620_error.results
more ..\..\temptest\h5diff_620_error.results >> ..\..\temptest\h5diff_620.results
del ..\..\temptest\h5diff_620_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -n -4 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_621.results 2>..\..\temptest\h5diff_621_error.results
more ..\..\temptest\h5diff_621_error.results >> ..\..\temptest\h5diff_621.results
del ..\..\temptest\h5diff_621_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -n 0 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_622.results 2>..\..\temptest\h5diff_622_error.results
more ..\..\temptest\h5diff_622_error.results >> ..\..\temptest\h5diff_622.results
del ..\..\temptest\h5diff_622_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -n u g1/dset3 g1/dset4 > ..\..\temptest\h5diff_623.results 2>..\..\temptest\h5diff_623_error.results
more ..\..\temptest\h5diff_623_error.results >> ..\..\temptest\h5diff_623.results
del ..\..\temptest\h5diff_623_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -n 0x1 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_624.results 2>..\..\temptest\h5diff_624_error.results
more ..\..\temptest\h5diff_624_error.results >> ..\..\temptest\h5diff_624.results
del ..\..\temptest\h5diff_624_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -n "2" g1/dset3 g1/dset4 > ..\..\temptest\h5diff_625.results 2>..\..\temptest\h5diff_625_error.results
more ..\..\temptest\h5diff_625_error.results >> ..\..\temptest\h5diff_625.results
del ..\..\temptest\h5diff_625_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -n 2 -n 3 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_626.results 2>..\..\temptest\h5diff_626_error.results
more ..\..\temptest\h5diff_626_error.results >> ..\..\temptest\h5diff_626.results
del ..\..\temptest\h5diff_626_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -n 200 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_627.results 2>..\..\temptest\h5diff_627_error.results
more ..\..\temptest\h5diff_627_error.results >> ..\..\temptest\h5diff_627.results
del ..\..\temptest\h5diff_627_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file2.h5 -n 1 g1/dset3 g1/dset4 > ..\..\temptest\h5diff_628.results 2>..\..\temptest\h5diff_628_error.results
more ..\..\temptest\h5diff_628_error.results >> ..\..\temptest\h5diff_628.results
del ..\..\temptest\h5diff_628_error.results

h5diff%2 file1.h6 file2.h6 > ..\..\temptest\h5diff_629.results 2>..\..\temptest\h5diff_629_error.results
more ..\..\temptest\h5diff_629_error.results >> ..\..\temptest\h5diff_629.results
del ..\..\temptest\h5diff_629_error.results


h5diff%2 ..\..\testfiles\file5.h5 ..\..\testfiles\file6.h5 -v > ..\..\temptest\h5diff_70.results 2>..\..\temptest\h5diff_70_error.results
more ..\..\temptest\h5diff_70_error.results >> ..\..\temptest\h5diff_70.results
del ..\..\temptest\h5diff_70_error.results

h5diff%2 ..\..\testfiles\file7.h5 ..\..\testfiles\file8.h5 -v > ..\..\temptest\h5diff_80.results 2>..\..\temptest\h5diff_80_error.results
more ..\..\temptest\h5diff_80_error.results >> ..\..\temptest\h5diff_80.results
del ..\..\temptest\h5diff_80_error.results

h5diff%2 ..\..\testfiles\file1.h5 ..\..\testfiles\file1.h5 > ..\..\temptest\h5diff_90.results 2>..\..\temptest\h5diff_90_error.results
more ..\..\temptest\h5diff_90_error.results >> ..\..\temptest\h5diff_90.results
del ..\..\temptest\h5diff_90_error.results

cd ..\..\temptest
mkdir fctemp

REM ##############################################################################
REM ##############################################################################
REM ###			        T H E   T E S T S                              ###
REM ##############################################################################
REM ##############################################################################


REM ##############################################################################
REM ## Common usage
REM ##############################################################################

type nul > ..\difftest%2_%1.txt
echo ========================================== >> ..\difftest%2_%1.txt
echo Testing difftest.bat >> ..\difftest%2_%1.txt
echo ========================================== >> ..\difftest%2_%1.txt

REM 1.0
call ..\deleteline h5diff_10.results 1 
cd ..\testfiles
call ..\deleteline h5diff_10.txt 4
cd ..\temptest\fctemp
fc h5diff_10.txt h5diff_10.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff -h                                                      PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff -h                                                      FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 1.1 normal mode
call ..\deleteline h5diff_11.results 1 
cd ..\testfiles
call ..\deleteline h5diff_11.txt 4
cd ..\temptest\fctemp
fc h5diff_11.txt h5diff_11.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5                                       PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5                                       FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 1.2 normal mode with objects
call ..\deleteline h5diff_12.results 1 
cd ..\testfiles
call ..\deleteline h5diff_12.txt 4
cd ..\temptest\fctemp
fc h5diff_12.txt h5diff_12.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 g1/dset1 g1/dset2                     PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 g1/dset1 g1/dset2                     FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 1.3 report mode
call ..\deleteline h5diff_13.results 1 
cd ..\testfiles
call ..\deleteline h5diff_13.txt 4
cd ..\temptest\fctemp
fc h5diff_13.txt h5diff_13.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -r                                    PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -r                                    FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 1.4 report mode with objects
call ..\deleteline h5diff_14.results 1 
cd ..\testfiles
call ..\deleteline h5diff_14.txt 4
cd ..\temptest\fctemp
fc h5diff_14.txt h5diff_14.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -r g1/dset1 g1/dset2                  PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -r g1/dset1 g1/dset2                  FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 1.5 with -d
call ..\deleteline h5diff_15.results 1 
cd ..\testfiles
call ..\deleteline h5diff_15.txt 4
cd ..\temptest\fctemp
fc h5diff_15.txt h5diff_15.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -r -d 5 g1/dset3 g1/dset4             PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -r -d 5 g1/dset3 g1/dset4             FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 1.6 with -p
call ..\deleteline h5diff_16.results 1 
cd ..\testfiles
call ..\deleteline h5diff_16.txt 4
cd ..\temptest\fctemp
fc h5diff_16.txt h5diff_16.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -r -p 0.05 g1/dset3 g1/dset4          PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -r -p 0.05 g1/dset3 g1/dset4          FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 1.7 verbose mode
call ..\deleteline h5diff_17.results 1 
cd ..\testfiles
call ..\deleteline h5diff_17.txt 4
cd ..\temptest\fctemp
fc h5diff_17.txt h5diff_17.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -v                                    PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -v                                    FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 1.8 quiet mode
call ..\deleteline h5diff_18.results 1 
cd ..\testfiles
call ..\deleteline h5diff_18.txt 4
cd ..\temptest\fctemp
fc h5diff_18.txt h5diff_18.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -q                                    PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -q                                    FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM ##############################################################################
REM ## not comparable types
REM ##############################################################################

REM 2.0
call ..\deleteline h5diff_20.results 1 
cd ..\testfiles
call ..\deleteline h5diff_20.txt 4
cd ..\temptest\fctemp
fc h5diff_20.txt h5diff_20.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file3.h5 file3.h5 -v dset group                         PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file3.h5 file3.h5 -v dset group                         FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 2.1
call ..\deleteline h5diff_21.results 1 
cd ..\testfiles
call ..\deleteline h5diff_21.txt 4
cd ..\temptest\fctemp
fc h5diff_21.txt h5diff_21.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file3.h5 file3.h5 -v dset link                          PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file3.h5 file3.h5 -v dset link                          FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 2.2
call ..\deleteline h5diff_22.results 1 
cd ..\testfiles
call ..\deleteline h5diff_22.txt 4
cd ..\temptest\fctemp
fc h5diff_22.txt h5diff_22.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file3.h5 file3.h5 -v dset type                          PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file3.h5 file3.h5 -v dset type                          FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 2.3
call ..\deleteline h5diff_23.results 1 
cd ..\testfiles
call ..\deleteline h5diff_23.txt 4
cd ..\temptest\fctemp
fc h5diff_23.txt h5diff_23.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file3.h5 file3.h5 -v group group                        PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file3.h5 file3.h5 -v group group                        FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 2.4
call ..\deleteline h5diff_24.results 1 
cd ..\testfiles
call ..\deleteline h5diff_24.txt 4
cd ..\temptest\fctemp
fc h5diff_24.txt h5diff_24.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file3.h5 file3.h5 -v type type                          PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file3.h5 file3.h5 -v type type                          FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 2.5
call ..\deleteline h5diff_25.results 1 
cd ..\testfiles
call ..\deleteline h5diff_25.txt 4
cd ..\temptest\fctemp
fc h5diff_25.txt h5diff_25.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file3.h5 file3.h5 -v link link                          PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file3.h5 file3.h5 -v link link                          FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM ##############################################################################
REM ## Dataset types
REM ##############################################################################

REM 5.0
call ..\deleteline h5diff_50.results 1 
cd ..\testfiles
call ..\deleteline h5diff_50.txt 4
cd ..\temptest\fctemp
fc h5diff_50.txt h5diff_50.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file4.h5 file4.h5 -v dset0a dset0b                      PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file4.h5 file4.h5 -v dset0a dset0b                      FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 5.1
call ..\deleteline h5diff_51.results 1 
cd ..\testfiles
call ..\deleteline h5diff_51.txt 4
cd ..\temptest\fctemp
fc h5diff_51.txt h5diff_51.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file4.h5 file4.h5 -v dset1a dset1b                      PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file4.h5 file4.h5 -v dset1a dset1b                      FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 5.2
call ..\deleteline h5diff_52.results 1 
cd ..\testfiles
call ..\deleteline h5diff_52.txt 4
cd ..\temptest\fctemp
fc h5diff_52.txt h5diff_52.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file4.h5 file4.h5 -v dset2a dset2b                      PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file4.h5 file4.h5 -v dset2a dset2b                      FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 5.3
call ..\deleteline h5diff_53.results 1 
cd ..\testfiles
call ..\deleteline h5diff_53.txt 4
cd ..\temptest\fctemp
fc h5diff_53.txt h5diff_53.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file4.h5 file4.h5 -v dset3a dset4b                      PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file4.h5 file4.h5 -v dset3a dset4b                      FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 5.4
call ..\deleteline h5diff_54.results 1 
cd ..\testfiles
call ..\deleteline h5diff_54.txt 4
cd ..\temptest\fctemp
fc h5diff_54.txt h5diff_54.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file4.h5 file4.h5 -v dset4a dset4b                      PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file4.h5 file4.h5 -v dset4a dset4b                      FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 5.5
call ..\deleteline h5diff_55.results 1 
cd ..\testfiles
call ..\deleteline h5diff_55.txt 4
cd ..\temptest\fctemp
fc h5diff_55.txt h5diff_55.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file4.h5 file4.h5 -v dset5a dset5b                      PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file4.h5 file4.h5 -v dset5a dset5b                      FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 5.6
call ..\deleteline h5diff_56.results 1 
cd ..\testfiles
call ..\deleteline h5diff_56.txt 4
cd ..\temptest\fctemp
fc h5diff_56.txt h5diff_56.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file4.h5 file4.h5 -v dset6a dset6b                      PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file4.h5 file4.h5 -v dset6a dset6b                      FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 5.7
call ..\deleteline h5diff_57.results 1 
cd ..\testfiles
call ..\deleteline h5diff_57.txt 4
cd ..\temptest\fctemp
fc h5diff_57.txt h5diff_57.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file4.h5 file4.h5 -v dset7a dset7b                      PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file4.h5 file4.h5 -v dset7a dset7b                      FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 5.8
REM call ..\deleteline h5diff_58.results 1 
REM cd ..\testfiles
REM call ..\deleteline h5diff_58.txt 4
REM cd ..\temptest\fctemp
REM fc h5diff_58.txt h5diff_58.results >temp.txt
REM if %ERRORLEVEL%==0 (
REM    echo Testing h5diff file4.h5 file4.h5 dset8a dset8b                     PASSED >> ..\..\difftest%2_%1.txt
REM ) else (
REM    echo Testing h5diff file4.h5 file4.h5 dset8a dset8b                     FAILED >> ..\..\difftest%2_%1.txt
REM    more temp.txt >> ..\..\difftest%2_%1.txt
REM )
REM del temp.txt
REM cd ..

REM ##############################################################################
REM ## Error messages
REM ##############################################################################

REM 6.0: Check if the command line number of arguments is less than 3
call ..\deleteline h5diff_600.results 1 
cd ..\testfiles
call ..\deleteline h5diff_600.txt 4
cd ..\temptest\fctemp
fc h5diff_600.txt h5diff_600.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5                                                PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5                                                FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.1: Check for invalid options
call ..\deleteline h5diff_601.results 1 
cd ..\testfiles
call ..\deleteline h5diff_601.txt 4
cd ..\temptest\fctemp
fc h5diff_601.txt h5diff_601.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -x                                    PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -x                                    FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM ##############################################################################
REM ## -d 
REM ##############################################################################

REM 6.2: no value
call ..\deleteline h5diff_602.results 1 
cd ..\testfiles
call ..\deleteline h5diff_602.txt 4
cd ..\temptest\fctemp
fc h5diff_602.txt h5diff_602.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -d g1/dset3 g1/dset4                  PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -d g1/dset3 g1/dset4                  FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.3: negative value
call ..\deleteline h5diff_603.results 1 
cd ..\testfiles
call ..\deleteline h5diff_603.txt 4
cd ..\temptest\fctemp
fc h5diff_603.txt h5diff_603.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -d -4 g1/dset3 g1/dset4               PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -d -4 g1/dset3 g1/dset4               FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.4: zero
call ..\deleteline h5diff_604.results 1 
cd ..\testfiles
call ..\deleteline h5diff_604.txt 4
cd ..\temptest\fctemp
fc h5diff_604.txt h5diff_604.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -d 0 g1/dset3 g1/dset4                PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -d 0 g1/dset3 g1/dset4                FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.5: non number
call ..\deleteline h5diff_605.results 1 
cd ..\testfiles
call ..\deleteline h5diff_605.txt 4
cd ..\temptest\fctemp
fc h5diff_605.txt h5diff_605.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -d u g1/dset3 g1/dset4                PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -d u g1/dset3 g1/dset4                FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.6: hexadecimal
call ..\deleteline h5diff_606.results 1 
cd ..\testfiles
call ..\deleteline h5diff_606.txt 4
cd ..\temptest\fctemp
fc h5diff_606.txt h5diff_606.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -d 0x1 g1/dset3 g1/dset4              PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -d 0x1 g1/dset3 g1/dset4              FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.7: string
call ..\deleteline h5diff_607.results 1 
cd ..\testfiles
call ..\deleteline h5diff_607.txt 4
cd ..\temptest\fctemp
fc h5diff_607.txt h5diff_607.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -d "1" g1/dset3 g1/dset4              PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -d "1" g1/dset3 g1/dset4              FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.8: repeated option
call ..\deleteline h5diff_608.results 1 
cd ..\testfiles
call ..\deleteline h5diff_608.txt 4
cd ..\temptest\fctemp
fc h5diff_608.txt h5diff_608.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -d 1 -d 2 g1/dset3 g1/dset4           PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -d 1 -d 2 g1/dset3 g1/dset4           FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.9: number larger than biggest difference
call ..\deleteline h5diff_609.results 1 
cd ..\testfiles
call ..\deleteline h5diff_609.txt 4
cd ..\temptest\fctemp
fc h5diff_609.txt h5diff_609.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -d 200 g1/dset3 g1/dset4              PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -d 200 g1/dset3 g1/dset4              FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.10: number smaller than smallest difference
call ..\deleteline h5diff_610.results 1 
cd ..\testfiles
call ..\deleteline h5diff_610.txt 4
cd ..\temptest\fctemp
fc h5diff_610.txt h5diff_610.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -d 1 g1/dset3 g1/dset4                PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -d 1 g1/dset3 g1/dset4                FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..


REM ##############################################################################
REM ## -p
REM ##############################################################################


REM 6.11: no value
call ..\deleteline h5diff_611.results 1 
cd ..\testfiles
call ..\deleteline h5diff_611.txt 4
cd ..\temptest\fctemp
fc h5diff_611.txt h5diff_611.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -r -p g1/dset3 g1/dset4               PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -r -p g1/dset3 g1/dset4               FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.12: negative value
call ..\deleteline h5diff_612.results 1 
cd ..\testfiles
call ..\deleteline h5diff_612.txt 4
cd ..\temptest\fctemp
fc h5diff_612.txt h5diff_612.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -p -4 g1/dset3 g1/dset4               PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -p -4 g1/dset3 g1/dset4               FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.13: zero
call ..\deleteline h5diff_613.results 1 
cd ..\testfiles
call ..\deleteline h5diff_613.txt 4
cd ..\temptest\fctemp
fc h5diff_613.txt h5diff_613.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -p 0 g1/dset3 g1/dset4                PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -p 0 g1/dset3 g1/dset4                FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.14: non number
call ..\deleteline h5diff_614.results 1 
cd ..\testfiles
call ..\deleteline h5diff_614.txt 4
cd ..\temptest\fctemp
fc h5diff_614.txt h5diff_614.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -p u g1/dset3 g1/dset4                PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -p u g1/dset3 g1/dset4                FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.15: hexadecimal
call ..\deleteline h5diff_615.results 1 
cd ..\testfiles
call ..\deleteline h5diff_615.txt 4
cd ..\temptest\fctemp
fc h5diff_615.txt h5diff_615.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -p 0x1 g1/dset3 g1/dset4              PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -p 0x1 g1/dset3 g1/dset4              FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.16: string
call ..\deleteline h5diff_616.results 1 
cd ..\testfiles
call ..\deleteline h5diff_616.txt 4
cd ..\temptest\fctemp
fc h5diff_616.txt h5diff_616.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -p "0.21" g1/dset3 g1/dset4           PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -p "0.21" g1/dset3 g1/dset4           FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.17: repeated option
call ..\deleteline h5diff_617.results 1 
cd ..\testfiles
call ..\deleteline h5diff_617.txt 4
cd ..\temptest\fctemp
fc h5diff_617.txt h5diff_617.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -p 0.21 -p 0.22 g1/dset3 g1/dset4     PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -p 0.21 -p 0.22 g1/dset3 g1/dset4     FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.18: number larger than biggest difference
call ..\deleteline h5diff_618.results 1 
cd ..\testfiles
call ..\deleteline h5diff_618.txt 4
cd ..\temptest\fctemp
fc h5diff_618.txt h5diff_618.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -p 2 g1/dset3 g1/dset4                PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -p 2 g1/dset3 g1/dset4                FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.19: number smaller than smallest difference
call ..\deleteline h5diff_619.results 1 
cd ..\testfiles
call ..\deleteline h5diff_619.txt 4
cd ..\temptest\fctemp
fc h5diff_619.txt h5diff_619.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -p 0.005 g1/dset3 g1/dset4            PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -p 0.005 g1/dset3 g1/dset4            FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM ##############################################################################
REM ## -n
REM ##############################################################################

REM 6.20: no value
call ..\deleteline h5diff_620.results 1 
cd ..\testfiles
call ..\deleteline h5diff_620.txt 4
cd ..\temptest\fctemp
fc h5diff_620.txt h5diff_620.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -n g1/dset3 g1/dset4                  PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -n g1/dset3 g1/dset4                  FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.21: negative value
call ..\deleteline h5diff_621.results 1 
cd ..\testfiles
call ..\deleteline h5diff_621.txt 4
cd ..\temptest\fctemp
fc h5diff_621.txt h5diff_621.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -n -4 g1/dset3 g1/dset4               PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -n -4 g1/dset3 g1/dset4               FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.22: zero
call ..\deleteline h5diff_622.results 1 
cd ..\testfiles
call ..\deleteline h5diff_622.txt 4
cd ..\temptest\fctemp
fc h5diff_622.txt h5diff_622.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -n 0 g1/dset3 g1/dset4                PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -n 0 g1/dset3 g1/dset4                FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.23: non number
call ..\deleteline h5diff_623.results 1 
cd ..\testfiles
call ..\deleteline h5diff_623.txt 4
cd ..\temptest\fctemp
fc h5diff_623.txt h5diff_623.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -n u g1/dset3 g1/dset4                PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -n u g1/dset3 g1/dset4                FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.24: hexadecimal
call ..\deleteline h5diff_624.results 1 
cd ..\testfiles
call ..\deleteline h5diff_624.txt 4
cd ..\temptest\fctemp
fc h5diff_624.txt h5diff_624.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -n 0x1 g1/dset3 g1/dset4              PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -n 0x1 g1/dset3 g1/dset4              FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.25: string
call ..\deleteline h5diff_625.results 1 
cd ..\testfiles
call ..\deleteline h5diff_625.txt 4
cd ..\temptest\fctemp
fc h5diff_625.txt h5diff_625.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -n "2" g1/dset3 g1/dset4              PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -n "2" g1/dset3 g1/dset4              FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.26: repeated option
call ..\deleteline h5diff_626.results 1 
cd ..\testfiles
call ..\deleteline h5diff_626.txt 4
cd ..\temptest\fctemp
fc h5diff_626.txt h5diff_626.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -n 2 -n 3 g1/dset3 g1/dset4           PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -n 2 -n 3 g1/dset3 g1/dset4           FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.27: number larger than biggest difference
call ..\deleteline h5diff_627.results 1 
cd ..\testfiles
call ..\deleteline h5diff_627.txt 4
cd ..\temptest\fctemp
fc h5diff_627.txt h5diff_627.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -n 200 g1/dset3 g1/dset4              PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -n 200 g1/dset3 g1/dset4              FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM 6.28: number smaller than smallest difference
call ..\deleteline h5diff_628.results 1 
cd ..\testfiles
call ..\deleteline h5diff_628.txt 4
cd ..\temptest\fctemp
fc h5diff_628.txt h5diff_628.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file2.h5 -n 1 g1/dset3 g1/dset4                PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file2.h5 -n 1 g1/dset3 g1/dset4                FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM ##############################################################################
REM ## non valid files
REM ##############################################################################

call ..\deleteline h5diff_629.results 0 
cd ..\testfiles
call ..\deleteline h5diff_629.txt 3
cd ..\temptest\fctemp
fc h5diff_629.txt h5diff_629.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h6 file2.h6                                       PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h6 file2.h6                                       FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM ##############################################################################
REM ## attributes
REM ##############################################################################

call ..\deleteline h5diff_70.results 1 
cd ..\testfiles
call ..\deleteline h5diff_70.txt 4
cd ..\temptest\fctemp
fc h5diff_70.txt h5diff_70.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file5.h5 file6.h5 -v                                    PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file5.h5 file6.h5 -v                                    FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM ##############################################################################
REM ## all dataset datatypes
REM ##############################################################################

call ..\deleteline h5diff_80.results 1 
cd ..\testfiles
call ..\deleteline h5diff_80.txt 4
cd ..\temptest\fctemp
fc h5diff_80.txt h5diff_80.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file7.h5 file8.h5 -v                                    PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file7.h5 file8.h5 -v                                    FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM ##############################################################################
REM ## Same File Comparison
REM ##############################################################################

call ..\deleteline h5diff_90.results 1 
cd ..\testfiles
call ..\deleteline h5diff_90.txt 4
cd ..\temptest\fctemp
fc h5diff_90.txt h5diff_90.results >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing h5diff file1.h5 file1.h5                                       PASSED >> ..\..\difftest%2_%1.txt
) else (
   echo Testing h5diff file1.h5 file1.h5                                       FAILED >> ..\..\difftest%2_%1.txt
   more temp.txt >> ..\..\difftest%2_%1.txt
)
del temp.txt
cd ..

REM ##############################################################################
REM ## END
REM ##############################################################################

cd ..

rmdir /s/q temptest
