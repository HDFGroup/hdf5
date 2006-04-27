# Microsoft Developer Studio Project File - Name="hdf5_fortrandll" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=hdf5_fortrandll - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "hdf5_fortrandll.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "hdf5_fortrandll.mak" CFG="hdf5_fortrandll - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "hdf5_fortrandll - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "hdf5_fortrandll - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "hdf5_fortrandll - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "../../../proj/hdf5_fortrandll/Release"
# PROP Intermediate_Dir "../../../proj/hdf5_fortrandll/Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /dll /nologo /warn:nofileopt
# ADD F90 /compile_only /define:"HDF5F90_WINDOWS" /define:"BUILD_HDF5_DLL" /include:"Release/" /dll /nologo /threads /warn:nofileopt
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "HDF5FORT_CSTUB_USEDLL" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386

!ELSEIF  "$(CFG)" == "hdf5_fortrandll - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "../../../proj/hdf5_fortrandll/Debug"
# PROP Intermediate_Dir "../../../proj/hdf5_fortrandll/Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /include:"Debug/" /dll /nologo /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /define:"HDF5F90_WINDOWS" /define:"BUILD_HDF5_DLL" /fpscomp:symbols /include:"Debug/" /dll /nologo /threads /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "HDF5FORT_CSTUB_USEDLL" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /out:"../../../proj/hdf5_fortrandll/Debug/hdf5_fortranddll.dll   " /pdbtype:sept

!ENDIF 

# Begin Target

# Name "hdf5_fortrandll - Win32 Release"
# Name "hdf5_fortrandll - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\..\..\fortran\src\H5_ff.f90
DEP_F90_H5_FF=\
	"..\..\..\proj\hdf5_fortrandll\Release\h5global.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Aff.f90
DEP_F90_H5AFF=\
	"..\..\..\proj\hdf5_fortrandll\Release\h5global.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Dff.f90
DEP_F90_H5DFF=\
	"..\..\..\proj\hdf5_fortrandll\Release\h5global.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Eff.f90
DEP_F90_H5EFF=\
	"..\..\..\proj\hdf5_fortrandll\Release\h5global.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5f90global.f90
DEP_F90_H5F90=\
	"..\..\..\proj\hdf5_fortrandll\Release\h5fortran_types.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Fff.f90
DEP_F90_H5FFF=\
	"..\..\..\proj\hdf5_fortrandll\Release\h5global.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5fortran_flags.f90
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5fortran_types.f90
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Gff.f90
DEP_F90_H5GFF=\
	"..\..\..\proj\hdf5_fortrandll\Release\h5global.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Iff.f90
DEP_F90_H5IFF=\
	"..\..\..\proj\hdf5_fortrandll\Release\h5global.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Pff.f90
DEP_F90_H5PFF=\
	"..\..\..\proj\hdf5_fortrandll\Release\h5global.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Rff.f90
DEP_F90_H5RFF=\
	"..\..\..\proj\hdf5_fortrandll\Release\h5global.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Sff.f90
DEP_F90_H5SFF=\
	"..\..\..\proj\hdf5_fortrandll\Release\h5global.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Tff.f90
DEP_F90_H5TFF=\
	"..\..\..\proj\hdf5_fortrandll\Release\h5global.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Zff.f90
DEP_F90_H5ZFF=\
	"..\..\..\proj\hdf5_fortrandll\Release\h5global.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\HDF5.f90
DEP_F90_HDF5_=\
	"..\..\..\proj\hdf5_fortrandll\Release\H5A.mod"\
	"..\..\..\proj\hdf5_fortrandll\Release\H5D.mod"\
	"..\..\..\proj\hdf5_fortrandll\Release\H5E.mod"\
	"..\..\..\proj\hdf5_fortrandll\Release\H5F.mod"\
	"..\..\..\proj\hdf5_fortrandll\Release\H5G.mod"\
	"..\..\..\proj\hdf5_fortrandll\Release\h5global.mod"\
	"..\..\..\proj\hdf5_fortrandll\Release\H5I.mod"\
	"..\..\..\proj\hdf5_fortrandll\Release\H5LIB.mod"\
	"..\..\..\proj\hdf5_fortrandll\Release\H5P.mod"\
	"..\..\..\proj\hdf5_fortrandll\Release\H5R.mod"\
	"..\..\..\proj\hdf5_fortrandll\Release\H5S.mod"\
	"..\..\..\proj\hdf5_fortrandll\Release\H5T.mod"\
	"..\..\..\proj\hdf5_fortrandll\Release\H5Z.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
