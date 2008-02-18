@echo OFF
rem Copyright by The HDF Group.
rem Copyright by the Board of Trustees of the University of Illinois.
rem All rights reserved.
rem
rem This file is part of HDF5.  The full HDF5 copyright notice, including
rem terms governing use, modification, and redistribution, is contained in
rem the files COPYING and Copyright.html.  COPYING can be found at the root
rem of the source code distribution tree; Copyright.html can be found at the
rem root level of an installed copy of the electronic HDF5 document set and
rem is linked from the top-level documents page.  It can also be found at
rem http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
rem access to either file, you may request a copy from help@hdfgroup.org.


rem This batch file is used to install HDF5 High Level C 
rem Examples' executable files.
rem Written by   : Fang GUO
rem Created on   : 07/25/2005
rem Last Modified: 2/18/2008

pushd hl\examples

mkdir HLCexamplesRELEASE
mkdir HLCexamplesRELEASEDLL
mkdir HLCexamplesDEBUG
mkdir HLCexamplesDEBUGDLL

for %%i in (ex_image1 ex_lite1 ex_table01 ex_ds1 ptExampleFL) do (
    copy %%i\debug\%%i.exe HLCexamplesDEBUG\
    copy %%i\release\%%i.exe HLCexamplesRELEASE\
    copy %%idll\debug\%%idll.exe HLCexamplesDEBUGDLL\
    copy %%idll\release\%%idll.exe HLCexamplesRELEASEDLL\
)

popd
