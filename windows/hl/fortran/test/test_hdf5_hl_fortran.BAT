@ECHO OFF

REM This batch file will be used to test HDF5 High Level Fortran Library.
REM By Xuan Bai
REM Last Modified on: 12/12/2004

echo.
echo ===============================================
echo Testing hl_test_lite_fortran -- %1
echo ===============================================
hl_test_lite_fortran\%1\hl_test_lite_fortran

echo.
echo ===============================================
echo Testing hl_test_image_fortran -- %1
echo ===============================================
hl_test_image_fortran\%1\hl_test_image_fortran

echo.
echo ===============================================
echo Testing hl_test_table_fortran -- %1
echo ===============================================
hl_test_table_fortran\%1\hl_test_table_fortran