@ECHO OFF

REM This batch file will be used to test HDF5 High Level Library.
REM By Xuan Bai
REM Last Modified on: 12/12/2004

echo.
echo ===============================================
echo Testing hl_test_lite -- %1
echo ===============================================
hl_test_lite\%1\hl_test_lite

echo.
echo ===============================================
echo Testing hl_test_image -- %1
echo ===============================================
hl_test_image\%1\hl_test_image

echo.
echo ===============================================
echo Testing hl_test_table -- %1
echo ===============================================
hl_test_table\%1\hl_test_table