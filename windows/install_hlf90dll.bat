@REM File name: install_hlf90dll.bat
@REM This batch file is used to copy HDF5 fortran library high level DLLs into system folder.
@REM By Fang GUO
@REM Created: July 15, 2005
@REM Last Updated: 

@ECHO OFF
copy proj\hdf5_hl_fortrandll\Debug\hdf5_hl_fortrandlld.dll %SystemRoot%\system >temp.txt
copy proj\hdf5_hl_f90cstubdll\Debug\hdf5_hl_f90cstubdlld.dll %SystemRoot%\system >temp.txt
copy proj\hdf5_hl_fortrandll\Release\hdf5_hl_fortrandll.dll %SystemRoot%\system >temp.txt
copy proj\hdf5_hl_f90cstubdll\Release\hdf5_hl_f90cstubdll.dll %SystemRoot%\system >temp.txt

del temp.txt