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
cd temptest
mkdir act_h5
mkdir exp_h5
mkdir fctemp
cd ..

copy h5import\testfiles\*.h5 temptest\exp_h5\. >temp.txt
del temp.txt

cd testfiles\h5importtst\%1
h5importtst

cd ..\..\..\h5import%2\%1

REM TESTING "ASCII I32 rank 3 - Output BE " ;
h5import%2 ..\..\testfiles\h5importtst\%1\txtin32 -c ..\..\h5import\testfiles\textin32 -o ..\..\temptest\act_h5\test1.h5
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\act_h5\test1.h5 >..\..\temptest\textin32_act.txt
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\exp_h5\test1.h5 >..\..\temptest\textin32_exp.txt

REM TESTING "ASCII I16 rank 3 - Output LE - CHUNKED - extended" 
h5import%2 ..\..\testfiles\h5importtst\%1\txtin16 -c ..\..\h5import\testfiles\textin16 -o ..\..\temptest\act_h5\test2.h5
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\act_h5\test2.h5 >..\..\temptest\textin16_act.txt
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\exp_h5\test2.h5 >..\..\temptest\textin16_exp.txt

REM TESTING "ASCII I8 - rank 3 - Output I16 LE-Chunked+Extended+Compressed " 
h5import%2 ..\..\testfiles\h5importtst\%1\txtin16 -c ..\..\h5import\testfiles\textin8 -o ..\..\temptest\act_h5\test3.h5
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\act_h5\test3.h5 >..\..\temptest\textin8_act.txt
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\exp_h5\test3.h5 >..\..\temptest\textin8_exp.txt

REM TESTING "ASCII UI32 - rank 3 - Output BE" 
h5import%2 ..\..\h5import\testfiles\in1 -c ..\..\h5import\testfiles\textuin32 -o ..\..\temptest\act_h5\test4.h5
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\act_h5\test4.h5 >..\..\temptest\textuin32_act.txt
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\exp_h5\test4.h5 >..\..\temptest\textuin32_exp.txt

REM TESTING "ASCII UI16 - rank 2 - Output LE+Chunked+Compressed " 
h5import%2 ..\..\h5import\testfiles\in1 -c ..\..\h5import\testfiles\textuin16 -o ..\..\temptest\act_h5\test5.h5
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\act_h5\test5.h5 >..\..\temptest\textuin16_act.txt
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\exp_h5\test5.h5 >..\..\temptest\textuin16_exp.txt

REM TESTING "ASCII F32 - rank 3 - Output LE " 
h5import%2 ..\..\h5import\testfiles\fp1 -c ..\..\h5import\testfiles\textfp32 -o ..\..\temptest\act_h5\test6.h5
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\act_h5\test6.h5 >..\..\temptest\textfp32_act.txt
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\exp_h5\test6.h5 >..\..\temptest\textfp32_exp.txt

REM TESTING "ASCII F64 - rank 3 - Output BE + CHUNKED+Extended+Compressed " 
h5import%2 ..\..\h5import\testfiles\fp2 -c ..\..\h5import\testfiles\textfp64 -o ..\..\temptest\act_h5\test7.h5
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\act_h5\test7.h5 >..\..\temptest\textfp64_act.txt
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\exp_h5\test7.h5 >..\..\temptest\textfp64_exp.txt

REM TESTING "BINARY F64 - rank 3 - Output LE+CHUNKED+Extended+Compressed " 
h5import%2 ..\..\testfiles\h5importtst\%1\bfp64 -c ..\..\h5import\testfiles\conbfp64 -o ..\..\temptest\act_h5\test8.h5
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\act_h5\test8.h5 >..\..\temptest\conbfp64_act.txt
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\exp_h5\test8.h5 >..\..\temptest\conbfp64_exp.txt

REM TESTING "BINARY I16 - rank 3 - Output order LE + CHUNKED + extended " 
h5import%2 ..\..\testfiles\h5importtst\%1\bin16 -c ..\..\h5import\testfiles\conbin16 -o ..\..\temptest\act_h5\test9.h5
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\act_h5\test9.h5 >..\..\temptest\conbin16_act.txt
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\exp_h5\test9.h5 >..\..\temptest\conbin16_exp.txt

REM TESTING "BINARY I8 - rank 3 - Output I16LE + Chunked+Extended+Compressed " 
h5import%2 ..\..\testfiles\h5importtst\%1\bin8 -c ..\..\h5import\testfiles\conbin8 -o ..\..\temptest\act_h5\test10.h5
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\act_h5\test10.h5 >..\..\temptest\conbin8_act.txt
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\exp_h5\test10.h5 >..\..\temptest\conbin8_exp.txt

REM TESTING "BINARY I32 - rank 3 - Output BE + CHUNKED " 
h5import%2 ..\..\testfiles\h5importtst\%1\bin32 -c ..\..\h5import\testfiles\conbin32 -o ..\..\temptest\act_h5\test11.h5
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\act_h5\test11.h5 >..\..\temptest\conbin32_act.txt
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\exp_h5\test11.h5 >..\..\temptest\conbin32_exp.txt

REM TESTING "BINARY UI16 - rank 3 - Output byte BE + CHUNKED " 
h5import%2 ..\..\testfiles\h5importtst\%1\buin16 -c ..\..\h5import\testfiles\conbuin16 -o ..\..\temptest\act_h5\test12.h5
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\act_h5\test12.h5 >..\..\temptest\conbuin16_act.txt
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\exp_h5\test12.h5 >..\..\temptest\conbuin16_exp.txt

REM TESTING "BINARY UI32 - rank 3 - Output LE + CHUNKED " 
h5import%2 ..\..\testfiles\h5importtst\%1\buin32 -c ..\..\h5import\testfiles\conbuin32 -o ..\..\temptest\act_h5\test13.h5
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\act_h5\test13.h5 >..\..\temptest\conbuin32_act.txt
..\..\h5dump%2\%1\h5dump%2 ..\..\temptest\exp_h5\test13.h5 >..\..\temptest\conbuin32_exp.txt

cd ..\..\temptest

type nul > ..\importtest%2_%1.txt
echo ========================================== >> ..\importtest%2_%1.txt
echo Testing importtest.bat >> ..\importtest%2_%1.txt
echo ========================================== >> ..\importtest%2_%1.txt

call ..\deleteline textin32_act.txt  1
call ..\deleteline textin32_exp.txt  1
call ..\deleteline textin16_act.txt  1
call ..\deleteline textin16_exp.txt  1
call ..\deleteline textin8_act.txt   1
call ..\deleteline textin8_exp.txt   1
call ..\deleteline textuin32_act.txt 1
call ..\deleteline textuin32_exp.txt 1
call ..\deleteline textuin16_act.txt 1
call ..\deleteline textuin16_exp.txt 1
call ..\deleteline textfp32_act.txt  1
call ..\deleteline textfp32_exp.txt  1
call ..\deleteline textfp64_act.txt  1
call ..\deleteline textfp64_exp.txt  1
call ..\deleteline conbfp64_act.txt  1
call ..\deleteline conbfp64_exp.txt  1
call ..\deleteline conbin16_act.txt  1
call ..\deleteline conbin16_exp.txt  1
call ..\deleteline conbin8_act.txt   1
call ..\deleteline conbin8_exp.txt   1
call ..\deleteline conbin32_act.txt  1
call ..\deleteline conbin32_exp.txt  1
call ..\deleteline conbuin16_act.txt 1
call ..\deleteline conbuin16_exp.txt 1
call ..\deleteline conbuin32_act.txt 1
call ..\deleteline conbuin32_exp.txt 1

cd fctemp
fc textin32_act.txt  textin32_exp.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing ASCII I32 rank 3 - Output BE                                   PASSED >> ..\..\importtest%2_%1.txt
) else (
   echo Testing ASCII I32 rank 3 - Output BE                                   FAILED >> ..\..\importtest%2_%1.txt
   more temp.txt >> ..\..\importtest%2_%1.txt
)
del temp.txt

fc textin16_act.txt  textin16_exp.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing ASCII I16 rank 3 - Output LE - CHUNKED - extended              PASSED >> ..\..\importtest%2_%1.txt
) else (
   echo Testing ASCII I16 rank 3 - Output LE - CHUNKED - extended              FAILED >> ..\..\importtest%2_%1.txt
   more temp.txt >> ..\..\importtest%2_%1.txt
)
del temp.txt

fc textin8_act.txt   textin8_exp.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing ASCII I8 - rank 3 - Output I16 LE-Chunked+Extended+Compressed  PASSED >> ..\..\importtest%2_%1.txt
) else (
   echo Testing ASCII I8 - rank 3 - Output I16 LE-Chunked+Extended+Compressed  FAILED >> ..\..\importtest%2_%1.txt
   more temp.txt >> ..\..\importtest%2_%1.txt
)
del temp.txt

fc textuin32_act.txt textuin32_exp.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing ASCII UI32 - rank 3 - Output BE                                PASSED >> ..\..\importtest%2_%1.txt
) else (
   echo Testing ASCII UI32 - rank 3 - Output BE                                FAILED >> ..\..\importtest%2_%1.txt
   more temp.txt >> ..\..\importtest%2_%1.txt
)
del temp.txt

fc textuin16_act.txt textuin16_exp.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing ASCII UI16 - rank 2 - Output LE+Chunked+Compressed             PASSED >> ..\..\importtest%2_%1.txt
) else (
   echo Testing ASCII UI16 - rank 2 - Output LE+Chunked+Compressed             FAILED >> ..\..\importtest%2_%1.txt
   more temp.txt >> ..\..\importtest%2_%1.txt
)
del temp.txt

fc textfp32_act.txt  textfp32_exp.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing ASCII F32 - rank 3 - Output LE                                 PASSED >> ..\..\importtest%2_%1.txt
) else (
   echo Testing ASCII F32 - rank 3 - Output LE                                 FAILED >> ..\..\importtest%2_%1.txt
   more temp.txt >> ..\..\importtest%2_%1.txt
)
del temp.txt

fc textfp64_act.txt  textfp64_exp.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing ASCII F64 - rank 3 - Output BE + CHUNKED+Extended+Compressed   PASSED >> ..\..\importtest%2_%1.txt
) else (
   echo Testing ASCII F64 - rank 3 - Output BE + CHUNKED+Extended+Compressed   FAILED >> ..\..\importtest%2_%1.txt
   more temp.txt >> ..\..\importtest%2_%1.txt
)
del temp.txt

fc conbfp64_act.txt  conbfp64_exp.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing BINARY F64 - rank 3 - Output LE+CHUNKED+Extended+Compressed    PASSED >> ..\..\importtest%2_%1.txt
) else (
   echo Testing BINARY F64 - rank 3 - Output LE+CHUNKED+Extended+Compressed    FAILED >> ..\..\importtest%2_%1.txt
   more temp.txt >> ..\..\importtest%2_%1.txt
)
del temp.txt

fc conbin16_act.txt  conbin16_exp.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing BINARY I16 - rank 3 - Output order LE + CHUNKED + extended     PASSED >> ..\..\importtest%2_%1.txt
) else (
   echo Testing BINARY I16 - rank 3 - Output order LE + CHUNKED + extended     FAILED >> ..\..\importtest%2_%1.txt
   more temp.txt >> ..\..\importtest%2_%1.txt
)
del temp.txt

fc conbin8_act.txt   conbin8_exp.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing BINARY I8 - rank 3 - Output I16LE + Chunked+Extended+Compress  PASSED >> ..\..\importtest%2_%1.txt
) else (
   echo Testing BINARY I8 - rank 3 - Output I16LE + Chunked+Extended+Compress  FAILED >> ..\..\importtest%2_%1.txt
   more temp.txt >> ..\..\importtest%2_%1.txt
)
del temp.txt

fc conbin32_act.txt  conbin32_exp.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing BINARY I32 - rank 3 - Output BE + CHUNKED                      PASSED >> ..\..\importtest%2_%1.txt
) else (
   echo Testing BINARY I32 - rank 3 - Output BE + CHUNKED                      FAILED >> ..\..\importtest%2_%1.txt
   more temp.txt >> ..\..\importtest%2_%1.txt
)
del temp.txt

fc conbuin16_act.txt conbuin16_exp.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing BINARY UI16 - rank 3 - Output byte BE + CHUNKED                PASSED >> ..\..\importtest%2_%1.txt
) else (
   echo Testing BINARY UI16 - rank 3 - Output byte BE + CHUNKED                FAILED >> ..\..\importtest%2_%1.txt
   more temp.txt >> ..\..\importtest%2_%1.txt
)
del temp.txt

fc conbuin32_act.txt conbuin32_exp.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo Testing BINARY UI32 - rank 3 - Output LE + CHUNKED                     PASSED >> ..\..\importtest%2_%1.txt
) else (
   echo Testing BINARY UI32 - rank 3 - Output LE + CHUNKED                     FAILED >> ..\..\importtest%2_%1.txt
   more temp.txt >> ..\..\importtest%2_%1.txt
)
del temp.txt

cd ..
cd ..

rmdir /s/q temptest