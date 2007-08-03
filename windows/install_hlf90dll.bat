@REM Copyright by The HDF Group.
@REM Copyright by the Board of Trustees of the University of Illinois.
@REM All rights reserved.
@REM
@REM This file is part of HDF5.  The full HDF5 copyright notice, including
@REM terms governing use, modification, and redistribution, is contained in
@REM the files COPYING and Copyright.html.  COPYING can be found at the root
@REM of the source code distribution tree; Copyright.html can be found at the
@REM root level of an installed copy of the electronic HDF5 document set and
@REM is linked from the top-level documents page.  It can also be found at
@REM http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
@REM access to either file, you may request a copy from help@hdfgroup.org.


@REM File name: install_hlf90dll.bat
@REM This batch file is used to copy HDF5 fortran library high level DLLs into system folder.
@REM By Fang GUO
@REM Created: July 15, 2005
@REM Last Updated: 

@ECHO OFF
copy proj\hdf5_hl_fortrandll\Debug\hdf5_hl_fortranddll.dll %SystemRoot%\system >temp.txt
copy proj\hdf5_hl_f90cstubdll\Debug\hdf5_hl_f90cstubddll.dll %SystemRoot%\system >temp.txt
copy proj\hdf5_hl_fortrandll\Release\hdf5_hl_fortrandll.dll %SystemRoot%\system >temp.txt
copy proj\hdf5_hl_f90cstubdll\Release\hdf5_hl_f90cstubdll.dll %SystemRoot%\system >temp.txt

del temp.txt