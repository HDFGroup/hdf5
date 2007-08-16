@rem Copyright by The HDF Group.
@rem Copyright by the Board of Trustees of the University of Illinois.
@rem All rights reserved.
@rem
@rem This file is part of HDF5.  The full HDF5 copyright notice, including
@rem terms governing use, modification, and redistribution, is contained in
@rem the files COPYING and Copyright.html.  COPYING can be found at the root
@rem of the source code distribution tree; Copyright.html can be found at the
@rem root level of an installed copy of the electronic HDF5 document set and
@rem is linked from the top-level documents page.  It can also be found at
@rem http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
@rem access to either file, you may request a copy from help@hdfgroup.org.


@rem File name: install_dll.bat
@rem This batch file is used to copy HDF5 c library DLLs into system folder.
@rem By Xuan Bai
@rem Created: Aug. 12, 2004
@rem Last Updated: Aug. 16, 2004

@ ECHO OFF
copy proj\hdf5dll\debug\hdf5ddll.dll %SystemRoot%\system >temp.txt
copy proj\hdf5dll\release\hdf5dll.dll %SystemRoot%\system >temp.txt

rem We need this conditional because vs6 uses libtestD rather than libtestdll,
rem Remove later when we fix the VS6 project name.
if exist copy test\libtestdll\debug\libtestddll.dll (
    copy test\libtestdll\debug\libtestddll.dll %SystemRoot%\system >temp.txt
    copy test\libtestdll\release\libtestdll.dll %SystemRoot%\system >temp.txt
) else (
    copy test\libtestD\debug\libtestDd.dll %SystemRoot%\system >temp.txt
    copy test\libtestD\release\libtestD.dll %SystemRoot%\system >temp.txt
)
del temp.txt
