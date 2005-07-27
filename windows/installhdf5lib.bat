@REM This batch file is used to install HDF5 libraries and tools
@REM Last Updated: 11/03/2004

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

@REM Install C libraries and tools
copy src\*.h hdf5lib\debug\include
del hdf5lib\debug\include\*private.h
copy proj\hdf5\debug\hdf5d.lib hdf5lib\debug\lib
copy proj\hdf5dll\debug\hdf5ddll.lib hdf5lib\debug\dll
copy proj\hdf5dll\debug\hdf5ddll.dll hdf5lib\debug\dll

copy tools\gifconv\debug\h52gif.exe hdf5lib\debug\bin
copy tools\gifconv\debug\gif2h5.exe hdf5lib\debug\bin
copy tools\h5debug\debug\h5debug.exe hdf5lib\debug\bin
copy tools\h5diff\debug\h5diff.exe hdf5lib\debug\bin
copy tools\h5dump\debug\h5dump.exe hdf5lib\debug\bin
copy tools\h5import\debug\h5import.exe hdf5lib\debug\bin
copy tools\h5jam\debug\h5jam.exe hdf5lib\debug\bin
copy tools\h5ls\debug\h5ls.exe hdf5lib\debug\bin
copy tools\h5repack\debug\h5repack.exe hdf5lib\debug\bin
copy tools\h5repart\debug\h5repart.exe hdf5lib\debug\bin
copy tools\h5unjam\debug\h5unjam.exe hdf5lib\debug\bin

copy tools\gifconvdll\debug\h52gifdll.exe hdf5lib\debug\bindll
copy tools\gifconvdll\debug\gif2h5dll.exe hdf5lib\debug\bindll
copy tools\h5debugdll\debug\h5debugdll.exe hdf5lib\debug\bindll
copy tools\h5diffdll\debug\h5diffdll.exe hdf5lib\debug\bindll
copy tools\h5dumpdll\debug\h5dumpdll.exe hdf5lib\debug\bindll
copy tools\h5importdll\debug\h5importdll.exe hdf5lib\debug\bindll
copy tools\h5lsdll\debug\h5lsdll.exe hdf5lib\debug\bindll
copy tools\h5repackdll\debug\h5repackdll.exe hdf5lib\debug\bindll
copy tools\h5repartdll\debug\h5repartdll.exe hdf5lib\debug\bindll

copy src\*.h hdf5lib\release\include
del hdf5lib\release\include\*private.h
copy proj\hdf5\release\hdf5.lib hdf5lib\release\lib
copy proj\hdf5dll\release\hdf5dll.lib hdf5lib\release\dll
copy proj\hdf5dll\release\hdf5dll.dll hdf5lib\release\dll

copy tools\gifconv\release\h52gif.exe hdf5lib\release\bin
copy tools\gifconv\release\gif2h5.exe hdf5lib\release\bin
copy tools\h5debug\release\h5debug.exe hdf5lib\release\bin
copy tools\h5diff\release\h5diff.exe hdf5lib\release\bin
copy tools\h5dump\release\h5dump.exe hdf5lib\release\bin
copy tools\h5import\release\h5import.exe hdf5lib\release\bin
copy tools\h5jam\release\h5jam.exe hdf5lib\release\bin
copy tools\h5ls\release\h5ls.exe hdf5lib\release\bin
copy tools\h5repack\release\h5repack.exe hdf5lib\release\bin
copy tools\h5repart\release\h5repart.exe hdf5lib\release\bin
copy tools\h5unjam\release\h5unjam.exe hdf5lib\release\bin

copy tools\gifconvdll\release\h52gifdll.exe hdf5lib\release\bindll
copy tools\gifconvdll\release\gif2h5dll.exe hdf5lib\release\bindll
copy tools\h5debugdll\release\h5debugdll.exe hdf5lib\release\bindll
copy tools\h5diffdll\release\h5diffdll.exe hdf5lib\release\bindll
copy tools\h5dumpdll\release\h5dumpdll.exe hdf5lib\release\bindll
copy tools\h5importdll\release\h5importdll.exe hdf5lib\release\bindll
copy tools\h5lsdll\release\h5lsdll.exe hdf5lib\release\bindll
copy tools\h5repackdll\release\h5repackdll.exe hdf5lib\release\bindll
copy tools\h5repartdll\release\h5repartdll.exe hdf5lib\release\bindll

@REM Install HDF5 High Level Libraries
copy hl\src\*.h hdf5lib\debug\include
copy "hl\c++\src\*.h" hdf5lib\debug\include
copy proj\hdf5_hl_fortran\debug\*.mod  hdf5lib\debug\include
copy proj\hdf5_hl_fortrandll\debug\*.mod  hdf5lib\debug\include


copy proj\hdf5_hl\debug\hdf5_hld.lib hdf5lib\debug\lib
copy proj\hdf5_hl_cpp\debug\hdf5_hl_cppd.lib hdf5lib\debug\lib
copy proj\hdf5_hl_fortran\debug\hdf5_hl_fortrand.lib hdf5lib\debug\lib

copy proj\hdf5_hldll\debug\hdf5_hldlld.lib hdf5lib\debug\dll
copy proj\hdf5_hldll\debug\hdf5_hldlld.dll hdf5lib\debug\dll

copy proj\hdf5_hl_cppdll\debug\hdf5_hl_cppdlld.lib hdf5lib\debug\dll
copy proj\hdf5_hl_cppdll\debug\hdf5_hl_cppdlld.dll hdf5lib\debug\dll

copy proj\hdf5_hl_fortrandll\debug\hdf5_hl_fortrandlld.lib hdf5lib\debug\dll
copy proj\hdf5_hl_fortrandll\debug\hdf5_hl_fortrandlld.dll hdf5lib\debug\dll

copy proj\hdf5_hl_f90cstubdll\debug\hdf5_hl_f90cstubdlld.lib hdf5lib\debug\dll
copy proj\hdf5_hl_f90cstubdll\debug\hdf5_hl_f90cstubdlld.dll hdf5lib\debug\dll

copy hl\src\*.h hdf5lib\release\include
copy "hl\c++\src\*.h" hdf5lib\release\include
copy proj\hdf5_hl_fortran\release\*.mod  hdf5lib\release\include
copy proj\hdf5_hl_fortrandll\release\*.mod  hdf5lib\release\include

copy proj\hdf5_hl\release\hdf5_hl.lib hdf5lib\release\lib
copy proj\hdf5_hl_cpp\release\hdf5_hl_cpp.lib hdf5lib\release\lib
copy proj\hdf5_hl_fortran\release\hdf5_hl_fortranr.lib hdf5lib\release\lib

copy proj\hdf5_hldll\release\hdf5_hldll.lib hdf5lib\release\dll
copy proj\hdf5_hldll\release\hdf5_hldll.dll hdf5lib\release\dll

copy proj\hdf5_hl_cppdll\release\hdf5_hl_cppdll.lib hdf5lib\release\dll
copy proj\hdf5_hl_cppdll\release\hdf5_hl_cppdll.dll hdf5lib\release\dll

copy proj\hdf5_hl_fortrandll\release\hdf5_hl_fortrandll.lib hdf5lib\release\dll
copy proj\hdf5_hl_fortrandll\release\hdf5_hl_fortrandll.dll hdf5lib\release\dll

copy proj\hdf5_hl_f90cstubdll\release\hdf5_hl_f90cstubdll.lib hdf5lib\release\dll
copy proj\hdf5_hl_f90cstubdll\release\hdf5_hl_f90cstubdll.dll hdf5lib\release\dll

@REM Install C++ libraries and tools
rename c++ cpp

copy cpp\src\*.h hdf5lib\debug\include
copy proj\hdf5_cpp\debug\hdf5_cppd.lib hdf5lib\debug\lib
copy proj\hdf5_cppdll\debug\hdf5_cppddll.lib hdf5lib\debug\dll
copy proj\hdf5_cppdll\debug\hdf5_cppddll.dll hdf5lib\debug\dll


copy cpp\src\*.h hdf5lib\release\include
copy proj\hdf5_cpp\release\hdf5_cpp.lib hdf5lib\release\lib
copy proj\hdf5_cppdll\release\hdf5_cppdll.lib hdf5lib\release\dll
copy proj\hdf5_cppdll\release\hdf5_cppdll.dll hdf5lib\release\dll

rename cpp c++

@REM Install Fortran libraries and tools
copy proj\hdf5_fortran\debug\*.mod hdf5lib\debug\include
copy proj\hdf5_fortran\debug\hdf5_fortrand.lib hdf5lib\debug\lib
copy proj\hdf5_fortrandll\debug\*.mod hdf5lib\debug\include
copy proj\hdf5_fortrandll\debug\hdf5_fortranddll.lib hdf5lib\debug\dll
copy proj\hdf5_fortrandll\debug\hdf5_fortranddll.dll hdf5lib\debug\dll
copy proj\hdf5_f90cstubdll\debug\hdf5_f90cstubddll.lib hdf5lib\debug\dll
copy proj\hdf5_f90cstubdll\debug\hdf5_f90cstubddll.dll hdf5lib\debug\dll

copy proj\hdf5_fortran\release\*.mod hdf5lib\release\include
copy proj\hdf5_fortran\release\hdf5_fortran.lib hdf5lib\release\lib
copy proj\hdf5_fortrandll\release\*.mod hdf5lib\release\include
copy proj\hdf5_fortrandll\release\hdf5_fortrandll.lib hdf5lib\release\dll
copy proj\hdf5_fortrandll\release\hdf5_fortrandll.dll hdf5lib\release\dll
copy proj\hdf5_f90cstubdll\release\hdf5_f90cstubdll.lib hdf5lib\release\dll
copy proj\hdf5_f90cstubdll\release\hdf5_f90cstubdll.dll hdf5lib\release\dll