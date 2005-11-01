@REM Copyright by the Board of Trustees of the University of Illinois.
@REM All rights reserved.
@REM
@REM This file is part of HDF5.  The full HDF5 copyright notice, including
@REM terms governing use, modification, and redistribution, is contained in
@REM the files COPYING and Copyright.html.  COPYING can be found at the root
@REM of the source code distribution tree; Copyright.html can be found at the
@REM root level of an installed copy of the electronic HDF5 document set and
@REM is linked from the top-level documents page.  It can also be found at
@REM http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
@REM access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.


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