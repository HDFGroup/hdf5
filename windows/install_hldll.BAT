@REM File name: install_hldll.bat
@REM This batch file is used to copy HDF5 c library high level DLLs into system folder.
@REM By Fang GUO
@REM Created: July 14, 2005
@REM Last Updated: 

@ ECHO OFF
copy proj\hdf5_hldll\Debug\hdf5_hldlld.dll %SystemRoot%\system >temp.txt
copy proj\hdf5_hldll\Release\hdf5_hldll.dll %SystemRoot%\system >temp.txt
del temp.txt