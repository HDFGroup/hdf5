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
# ADD F90 /compile_only /define:"HDF5F90_WINDOWS BUILD_HDF5_DLL" /include:"Release/" /dll /nologo /threads /warn:nofileopt
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
NODEP_F90_H5_FF=\
	".\5GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Aff.f90
NODEP_F90_H5AFF=\
	".\5GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Dff.f90
NODEP_F90_H5DFF=\
	".\5GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Eff.f90
NODEP_F90_H5EFF=\
	".\5GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5f90global.f90
NODEP_F90_H5F90=\
	".\5FORTRAN_TYPES.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Fff.f90
NODEP_F90_H5FFF=\
	".\Release\H5GLOBAL.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5fortran_flags.f90
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5fortran_types.f90
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Gff.f90
NODEP_F90_H5GFF=\
	".\Release\H5GLOBAL.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Iff.f90
NODEP_F90_H5IFF=\
	".\Release\H5GLOBAL.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Pff.f90
NODEP_F90_H5PFF=\
	".\Release\H5GLOBAL.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Rff.f90
NODEP_F90_H5RFF=\
	".\Release\H5GLOBAL.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Sff.f90
NODEP_F90_H5SFF=\
	".\Release\H5GLOBAL.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Tff.f90
NODEP_F90_H5TFF=\
	".\Release\H5GLOBAL.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Zff.f90
NODEP_F90_H5ZFF=\
	".\Release\H5GLOBAL.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\HDF5.f90
NODEP_F90_HDF5_=\
	".\Release\H5A.MOD"\
	".\Release\H5D.MOD"\
	".\Release\H5E.MOD"\
	".\Release\H5F.MOD"\
	".\Release\H5G.MOD"\
	".\Release\H5GLOBAL.MOD"\
	".\Release\H5I.MOD"\
	".\Release\H5LIB.MOD"\
	".\Release\H5P.MOD"\
	".\Release\H5R.MOD"\
	".\Release\H5S.MOD"\
	".\Release\H5T.MOD"\
	".\Release\H5Z.MOD"\
	
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
