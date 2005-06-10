@REM File Name   : copy_hdf.bat
@REM Purpose     : Copy all Files in the following formats from Windows to 
                   approapriate directory: .bat .c .f90 .h .txt .js 
@REM             : 
@REM Written By  : Muqun Yang
@REM Last Update : June 7, 2005 by Fang GUO

copy src\H5Tinit.c ..\src
copy src\H5pubconf.h ..\src
copy fortran\src\H5f90i_gen.h ..\fortran\src
copy fortran\src\H5fortran_types.f90 ..\fortran\src
xcopy /e/i/Y *.bat ..\
copy batch_net\convert.js ..\batch_net\
copy examples\testExamples_exp_output.txt ..\examples

cd ../batch_net
copy *.* ..\

cd ../batch_intelc
copy *.* ..\
@echo off
cd ..
rmdir /s/q batch_net
rmdir /s/q batch_intelc

cd windows

