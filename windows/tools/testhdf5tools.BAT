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

@ ECHO OFF

REM  This batch file includes hdf5 tool(h5dump, h5diff, h5ls, h5import, and h5repack) testings.
REM  testhdf5tools release  -- release static version
REM  testhdf5tools debug  -- debug static version
REM  testhdf5tools release dll -- release dll version
REM  testhdf5tools debug dll -- debug dll version
REM  By: Xuan Bai
REM  Created: August 2, 2004
REM  Last Modified: August 2, 2004

@type nul > toolstest_%1%2.txt
@echo ***************************************************************************** >> toolstest_%1%2.txt
@echo            HDF5 Tools Tests -- %1 %2 >> toolstest_%1%2.txt
@echo ***************************************************************************** >> toolstest_%1%2.txt

call dumptest %1 %2
@more dumptest%2_%1.txt >> toolstest_%1%2.txt
@del dumptest%2_%1.txt


call difftest %1 %2
@more difftest%2_%1.txt >> toolstest_%1%2.txt
@del difftest%2_%1.txt

call lstest %1 %2
@more lstest%2_%1.txt >> toolstest_%1%2.txt
@del lstest%2_%1.txt

call importtest %1 %2
@more importtest%2_%1.txt >> toolstest_%1%2.txt
@del importtest%2_%1.txt

call repacktest %1 %2
@more repacktest%2_%1.txt >> toolstest_%1%2.txt
@del repacktest%2_%1.txt

call stattest %1 %2
@more stattest%2_%1.txt >> toolstest_%1%2.txt
@del stattest%2_%1.txt

set p2=%2
if "%2"=="dll" (
echo "DLL version is not available for h5copy,h5jam or h5unjam, SKIP the test" >>toolstest_%1%2.txt
)	else (
	call jamtest %1 %2
        @more jamtest%2_%1.txt >> toolstest_%1%2.txt
        @del jamtest%2_%1.txt
	call copytest %1 %2
        @more copytest%2_%1.txt >> toolstest_%1%2.txt
        @del copytest%2_%1.txt
)

