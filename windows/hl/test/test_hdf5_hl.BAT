@ECHO OFF

REM This batch file will be used to test HDF5 High Level Library.
REM By Xuan Bai
REM Last Modified on: 07/14/2005 by Fang GUO

echo.
echo ===============================================
echo Testing hl_test_ds%2 -- %1
echo ===============================================
hl_test_ds%2\%1\hl_test_ds%2

echo.
echo ===============================================
echo Testing hl_test_lite%2 -- %1
echo ===============================================
hl_test_lite%2\%1\hl_test_lite%2

echo.
echo ===============================================
echo Testing hl_test_image%2 -- %1
echo ===============================================
hl_test_image%2\%1\hl_test_image%2

echo.
echo ===============================================
echo Testing hl_test_table%2 -- %1
echo ===============================================
hl_test_table%2\%1\hl_test_table%2

echo.
echo ===============================================
echo Testing hl_test_packet%2 -- %1
echo ===============================================
hl_test_packet%2\%1\hl_test_packet%2