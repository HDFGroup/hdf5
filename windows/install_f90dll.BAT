@REM File name: install_f90dll.bat
@REM This batch file is used to copy HDF5 Fortran library DLLs into system folder.
@REM By Xuan Bai
@REM Created: Aug. 12, 2004
@REM Last Updated: Aug. 16, 2004

@ ECHO OFF
copy proj\hdf5_f90cstubdll\debug\hdf5_f90cstubddll.dll %SystemRoot%\system >temp.txt
copy proj\hdf5_f90cstubdll\release\hdf5_f90cstubdll.dll %SystemRoot%\system >temp.txt
copy proj\hdf5_fortrandll\debug\hdf5_fortranddll.dll %SystemRoot%\system >temp.txt
copy proj\hdf5_fortrandll\release\hdf5_fortrandll.dll %SystemRoot%\system >temp.txt
copy fortran\test\libtest_cstubdll\debug\libtest_cstubddll.dll %SystemRoot%\system >temp.txt
copy fortran\test\libtest_cstubdll\release\libtest_cstubdll.dll %SystemRoot%\system >temp.txt
copy fortran\test\libtest_fortrandll\debug\libtest_fortranddll.dll %SystemRoot%\system >temp.txt
copy fortran\test\libtest_fortrandll\release\libtest_fortrandll.dll %SystemRoot%\system >temp.txt
del temp.txt