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

REM This batch file will be used to test HDF5 High Level Fortran Library.
REM By Xuan Bai
REM Last Modified on: 07/15/2005

echo.
echo ===============================================
echo Testing hl_test_lite_fortran%2 -- %1
echo ===============================================
hl_test_lite_fortran%2\%1\hl_test_lite_fortran%2

echo.
echo ===============================================
echo Testing hl_test_image_fortran%2 -- %1
echo ===============================================
hl_test_image_fortran%2\%1\hl_test_image_fortran%2

echo.
echo ===============================================
echo Testing hl_test_table_fortran%2 -- %1
echo ===============================================
hl_test_table_fortran%2\%1\hl_test_table_fortran%2