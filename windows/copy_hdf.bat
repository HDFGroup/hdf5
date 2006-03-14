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

@REM File Name   : copy_hdf.bat
@REM Purpose     : Copy all Files in the following formats from Windows to 
@REM               approapriate directory: .bat .c .f90 .h .txt .js 
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
copy hl\fortran\examples\*.f90 ..\hl\fortran\examples\

cd ../batch_net
copy *.* ..\

cd ../batch_intelc
copy *.* ..\
@echo off
cd ..
rmdir /s/q batch_net
rmdir /s/q batch_intelc

cd windows

