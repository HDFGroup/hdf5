# Microsoft Developer Studio Project File - Name="hdf5_f90cstub" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=hdf5_f90cstub - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "hdf5_f90cstub.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "hdf5_f90cstub.mak" CFG="hdf5_f90cstub - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "hdf5_f90cstub - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "hdf5_f90cstub - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "hdf5_f90cstub - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\..\proj\hdf5_f90cstub\Release"
# PROP Intermediate_Dir "..\..\..\proj\hdf5_f90cstub\Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\..\..\src" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "hdf5_f90cstub - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\..\proj\hdf5_f90cstub\Debug"
# PROP Intermediate_Dir "..\..\..\proj\hdf5_f90cstub\Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "..\..\..\src" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\..\proj\hdf5_f90cstub\Debug\hdf5_f90cstubd.lib"

!ENDIF 

# Begin Target

# Name "hdf5_f90cstub - Win32 Release"
# Name "hdf5_f90cstub - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\..\..\fortran\src\H5_f.c
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Af.c
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Df.c
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Ef.c
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5f90kit.c
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Ff.c
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Gf.c
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5If.c
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Pf.c
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Rf.c
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Sf.c
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Tf.c
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5Zf.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=..\..\..\fortran\src\H5f90.h
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5f90i.h
# End Source File
# Begin Source File

SOURCE=..\..\..\fortran\src\H5f90proto.h
# End Source File
# End Group
# End Target
# End Project
