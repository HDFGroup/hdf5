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
 

