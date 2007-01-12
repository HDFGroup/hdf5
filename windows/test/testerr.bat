@REM This batch file tests HDF5 error API and backward compatibility with 1.6.
@REM If you want to use 1.8 with 1.6 compatibility,To the functionality of error APIs, 
@REM You need to do the following two things:
@REM 1. Uncomment out /*#define H5_WANT_H5_V1_6_COMPAT 1*/ inside h5pubconf.h.
@REM 2. Set up an environment variable H5_WANT_H5_V1_6_COMPAT to 1
@REM Remember to undo 1 and 2 after you test if you are not going to use 1.6 backward compatibility option.
@echo off
@REM Goto the correct directory
cd error_api%2\%1
@REM Copy the executable files
copy /Y ..\..\error_compat%2\%1\error_compat%2.exe . >tempcopy.out 2>tempcopy.err
copy /Y ..\..\testfiles\err* . >tempcopy.out 2>tempcopy.err
@REM use sed to replace the changable number or string(such as line number, thread IDs etc.)
@REM We also want to combine the standard output and standard error of API tests into one file.
@REM Note the different NAME between 1.6 and 1.8...
if "%H5_WANT_H5_V1_6_COMPAT%"=="1" (
error_compat%2 >actual_test.out 2>actual_test.err
sed -e "s/thread [0-9]*/thread (IDs)/" -e "s/: .*\.c /: (file name) /" actual_test.err >temp1.err
sed -e "s/line [0-9]*/line (number)/" -e "s/v[1-9]*\.[0-9]*\./version (number)\./" temp1.err >temp2.err
sed -e "s/[1-9]*\.[0-9]*\.[0-9]*[^)]*/version (number)/" temp2.err >temp3.err
more /e actual_test.out >actual_temp.err
more /e temp3.err >>actual_temp.err
error_api%2 >actual_compat.out 2>actual_compat.err
) else (
error_api%2 >actual_test.out 2>actual_test.err
sed -e "s/thread [0-9]*/thread (IDs)/" -e "s/: .*\.c /: (file name) /" actual_test.err >temp1.err
sed -e "s/line [0-9]*/line (number)/" -e "s/v[1-9]*\.[0-9]*\./version (number)\./" temp1.err >temp2.err
sed -e "s/[1-9]*\.[0-9]*\.[0-9]*[^)]*/version (number)/" temp2.err >temp3.err
more /e actual_test.out >actual_temp.err
more /e temp3.err >>actual_temp.err
error_compat%2 >actual_compat.out 2>actual_compat.err
)
@REM We also want to filter out the first three lines of the expected output in order to 
@REM get the "no difference output"  between actual and expected results by using fc command.
if "%H5_WANT_H5_V1_6_COMPAT%"=="1" (
	more /e +3 error_test_2 >>err_compat_temp
	more /e +3 err_compat_1 >>expect_temp.err
) else (
	more /e +3 error_test_1 >>expect_temp.err
	more /e +3 err_compat_2 >>err_compat_temp
)

fc actual_temp.err expect_temp.err > fc_temp.out 2>fc_temp.err
if %ERRORLEVEL%==0 (
echo. All error API tests                                                   PASSED
)else (
echo. error API tests                                                       FAILED
)
fc actual_compat.out err_compat_temp > fc_temp.out 2>fc_temp.err
if %ERRORLEVEL%==0 (
echo. All error API compatible tests passed                                 PASSED
)else (
echo. error API compatible tests                                            FAILED
)
del *.err
del *.out
del *temp
del error_compat%2.exe
del *.h5
cd ..\..

