@REM File name: install_hlcppdll.bat
@REM This batch file is used to copy HDF5 c++ library high level DLLs into system folder.
@REM By Fang GUO
@REM Created: July 14, 2005
@REM Last Updated: 

@ ECHO OFF
copy proj\hdf5_hl_cppdll\Release\hdf5_hl_cppdll.dll %SystemRoot%\system >temp.txt
copy proj\hdf5_hl_cppdll\Debug\hdf5_hl_cppdlld.dll %SystemRoot%\system >temp.txt
del temp.txt