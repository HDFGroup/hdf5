@REM File name: install_cpp.bat
@REM This batch file is used to copy HDF5 c++ library DLLs into system folder.
@REM By Xuan Bai
@REM Created: Aug. 12, 2004
@REM Last Updated: Aug. 16, 2004

@ ECHO OFF
copy proj\hdf5_cppdll\debug\hdf5_cppddll.dll %SystemRoot%\system >temp.txt
copy proj\hdf5_cppdll\release\hdf5_cppdll.dll %SystemRoot%\system >temp.txt
del temp.txt
