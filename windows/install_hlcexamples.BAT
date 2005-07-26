@ECHO OFF
@:: This batch file is used to install HDF5 High Level C 
@:: Examples' executable files.
@:: Written by   : Fang GUO
@:: Created on   : 07/25/2005
@:: Last Modified: 

cd hl/hl_c_examples

mkdir HLCexamplesRELEASE
mkdir HLCexamplesRELEASEDLL
mkdir HLCexamplesDEBUG
mkdir HLCexamplesDEBUGDLL

cd ex_images
copy debug\ex_images.exe ..\HLCexamplesDEBUG\
copy release\ex_images.exe ..\HLCexamplesRELEASE\
cd ..

cd ex_imagesdll
copy debug\ex_imagesdll.exe ..\HLCexamplesDEBUGDLL\
copy release\ex_imagesdll.exe ..\HLCexamplesRELEASEDLL\
cd ..

cd ex_lite
copy debug\ex_lite.exe ..\HLCexamplesDEBUG\
copy release\ex_lite.exe ..\HLCexamplesRELEASE\
cd ..

cd ex_litedll
copy debug\ex_litedll.exe ..\HLCexamplesDEBUGDLL\
copy release\ex_litedll.exe ..\HLCexamplesRELEASEDLL\
cd ..


cd ex_table
copy debug\ex_table.exe ..\HLCexamplesDEBUG\
copy release\ex_table.exe ..\HLCexamplesRELEASE\
cd ..

cd ex_tabledll
copy debug\ex_tabledll.exe ..\HLCexamplesDEBUGDLL\
copy release\ex_tabledll.exe ..\HLCexamplesRELEASEDLL\
cd ..
