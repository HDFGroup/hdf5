mkdir hdf5lib
cd hdf5lib
mkdir debug
cd debug
mkdir bin
mkdir bindll
mkdir dll
mkdir lib
mkdir include
cd ..
mkdir release
cd release
mkdir bin
mkdir bindll
mkdir dll
mkdir lib
mkdir include 
cd ..
cd ..


move src\*.h hdf5lib\debug\include
move proj\hdf5\debug\*.mod hdf5lib\debug\include
del hdf5lib\debug\include\*private.h
move proj\hdf5\debug\hdf5.lib hdf5lib\debug\lib
move proj\hdf5dll\debug\hdf5ddll.lib hdf5lib\debug\dll
move proj\hdf5dll\debug\hdf5ddll.dll hdf5lib\debug\dll

move tools\h5dump\debug\h5dump.exe hdf5lib\debug\bin
move tools\h5import\debug\h5import.exe hdf5lib\debug\bin
move tools\h5repart\debug\h5repart.exe hdf5lib\debug\bin
move tools\h5ls\debug\h5ls.exe hdf5lib\debug\bin
move tools\h5debug\debug\h5debug.exe hdf5lib\debug\bin
move tools\gifconv\debug\gif2h5.exe hdf5lib\debug\bin
move tools\gifconv\debug\h52gif.exe hdf5lib\debug\bin

move tools\h5dumpdll\debug\h5dumpdll.exe hdf5lib\debug\bindll
move tools\h5importdll\debug\h5importdll.exe hdf5lib\debug\bindll
move tools\h5repartdll\debug\h5repartdll.exe hdf5lib\debug\bindll
move tools\h5lsdll\debug\h5lsdll.exe hdf5lib\debug\bindll
move tools\h5debugdll\debug\h5debugdll.exe hdf5lib\debug\bindll


move src\*.h hdf5lib\release\include
move proj\hdf5\release\*.mod hdf5lib\release\include
del hdf5lib\release\include\*private.h
move proj\hdf5\release\hdf5.lib hdf5lib\release\lib
move proj\hdf5dll\release\hdf5dll.lib hdf5lib\release\dll
move proj\hdf5dll\release\hdf5dll.dll hdf5lib\release\dll

move tools\h5dump\release\h5dump.exe hdf5lib\release\bin
move tools\h5import\release\h5import.exe hdf5lib\release\bin
move tools\h5repart\release\h5repart.exe hdf5lib\release\bin
move tools\h5ls\release\h5ls.exe hdf5lib\release\bin
move tools\h5debug\release\h5debug.exe hdf5lib\release\bin
move tools\gifconv\release\gif2h5.exe hdf5lib\release\bin
move tools\gifconv\release\h52gif.exe hdf5lib\release\bin

move tools\h5dumpdll\release\h5dumpdll.exe hdf5lib\release\bindll
move tools\h5importdll\release\h5importdll.exe hdf5lib\release\bindll
move tools\h5repartdll\release\h5repartdll.exe hdf5lib\release\bindll
move tools\h5lsdll\release\h5lsdll.exe hdf5lib\release\bindll
move tools\h5debugdll\release\h5debugdll.exe hdf5lib\release\bindll



