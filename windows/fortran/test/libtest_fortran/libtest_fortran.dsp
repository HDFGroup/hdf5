# Microsoft Developer Studio Project File - Name="libtest_fortran" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=libtest_fortran - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libtest_fortran.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libtest_fortran.mak" CFG="libtest_fortran - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libtest_fortran - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libtest_fortran - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "libtest_fortran - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\..\..\fortran\test\libtest_fortran\Release"
# PROP Intermediate_Dir "..\..\..\..\fortran\test\libtest_fortran\Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD F90 /compile_only /define:"HDF5F90_WINDOWS" /include:"Release/" /include:"..\..\..\..\proj\hdf5_fortran\Release/" /nologo /warn:nofileopt /module:"..\..\..\..\proj\hdf5_fortran\Release/"
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\..\..\..\src" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\..\..\fortran\test\libtest_fortran\Release\libtest_fortranr.lib"

!ELSEIF  "$(CFG)" == "libtest_fortran - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\..\..\fortran\test\libtest_fortran\Debug"
# PROP Intermediate_Dir "..\..\..\..\fortran\test\libtest_fortran\Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /debug:full /define:"HDF5F90_WINDOWS" /include:"Debug/" /include:"..\..\..\..\proj\hdf5_fortran\Debug/" /nologo /warn:argument_checking /warn:nofileopt /module:"..\..\..\..\proj\hdf5_fortran\Debug/"
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "..\..\..\..\src" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\..\..\fortran\test\libtest_fortran\Debug\libtest_fortrand.lib"

!ENDIF 

# Begin Target

# Name "libtest_fortran - Win32 Release"
# Name "libtest_fortran - Win32 Debug"
# Begin Source File

SOURCE=..\..\..\..\fortran\test\t.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\fortran\test\t.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\fortran\test\tf.f90
NODEP_F90_TF_F9=\
	"..\..\..\..\fortran\test\libtest_fortran\Debug\H5GLOBAL.mod"\
	
# End Source File
# End Target
# End Project
