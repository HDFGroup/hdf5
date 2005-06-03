@ECHO OFF

REM This batch file will be used to test HDF5 High Level Library.
REM By Xuan Bai
REM Last Modified on: 06/03/2005 by Fang GUO

echo.
echo ===============================================
echo Testing hl_test_ds -- %1
echo ===============================================
hl_test_ds\%1\hl_test_ds

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

echo.
echo ===============================================
echo Testing hl_test_packet -- %1
echo ===============================================
hl_test_packet\%1\hl_test_packet