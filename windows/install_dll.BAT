@REM File name: install_dll.bat
@REM This batch file is used to copy HDF5 c library DLLs into system folder.
@REM By Xuan Bai
@REM Created: Aug. 12, 2004
@REM Last Updated: Aug. 16, 2004

@ ECHO OFF
copy proj\hdf5dll\debug\hdf5ddll.dll %SystemRoot%\system >temp.txt
copy proj\hdf5dll\release\hdf5dll.dll %SystemRoot%\system >temp.txt
copy test\libtestD\debug\libtestDd.dll %SystemRoot%\system >temp.txt
copy test\libtestD\release\libtestD.dll %SystemRoot%\system >temp.txt
del temp.txt