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