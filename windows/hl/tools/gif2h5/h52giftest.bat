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

::This batch file is for h52gif tests.
::Usage
:: h52giftest release  -- release static version
:: h52giftest debug  -- debug static version
:: h52giftest release dll -- release dll version
:: h52giftest debug dll -- debug dll version
:: Written By: Scott Wegner
:: Date      : April 5, 2005


REM Set up our environment

mkdir temptest

set TESTFILE1=..\testfiles\h52giftst.h5
set TESTFILE2=..\testfiles\image1.gif

set H52GIF=..\gifconv%2\h52gif%2\%1\h52gif%2.exe
set GIF2H5=..\gifconv%2\gif2h5%2\%1\gif2h5%2.exe


REM The tests

REM TOOLTEST1 $TESTFILE1 image1.gif -i image
%H52GIF% %TESTFILE1% image1.gif -i image 2>%1 > temptest\gifconv_1.results
if %ERRORLEVEL% NEQ 0 (
   echo Testing h52gif h52giftst.h5 image1.gif -i image                        FAILED
   type temptest\gifconv_1.results
) else (
   echo Testing h52gif h52giftst.h5 image1.gif -i image                        PASSED
)
del temptest\gifconv_1.results

REM TOOLTEST2 $TESTFILE2 image1.h5
%GIF2H5% %TESTFILE2% image1.h5 2>%1 > temptest\gifconv_2.results
if %ERRORLEVEL% NEQ 0 (
   echo Testing gif2h5 image1.gif image1.h5                                    FAILED
   type temptest\gifconv_2.results
) else (
   echo Testing gif2h5 image1.gif image1.h5                                    PASSED
)
del temptest\gifconv_2.results


REM Cleanup
set TESTFILE1=
set TESTFILE2=

set H52GIF=
set GIF2H5=

rmdir /s/q temptest