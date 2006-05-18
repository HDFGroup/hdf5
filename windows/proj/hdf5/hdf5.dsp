# Microsoft Developer Studio Project File - Name="hdf5" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (ALPHA) Static Library" 0x0604
# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=hdf5 - Win32 AlphaDbg
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "hdf5.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "hdf5.mak" CFG="hdf5 - Win32 AlphaDbg"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "hdf5 - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "hdf5 - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "hdf5 - Win32 AlphaDbg" (based on "Win32 (ALPHA) Static Library")
!MESSAGE "hdf5 - Win32 AlphaRel" (based on "Win32 (ALPHA) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "hdf5 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\..\proj\hdf5\Release"
# PROP Intermediate_Dir "..\..\..\proj\hdf5\Release"
# PROP Target_Dir ""
F90=df.exe
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"..\..\..\proj\hdf5\Release/" /include:"Release/" /nologo /warn:nofileopt
CPP=cl.exe
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
RSC=rc.exe
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\..\proj\hdf5\Debug"
# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"
# PROP Target_Dir ""
F90=df.exe
# ADD BASE F90 /compile_only /debug:full /include:"Debug/" /nologo /warn:nofileopt
# ADD F90 /browser /compile_only /debug:full /include:"..\..\..\proj\hdf5\Debug/" /include:"Debug/" /nologo /warn:nofileopt
CPP=cl.exe
# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /FD /c
RSC=rc.exe
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\..\proj\hdf5\Debug\hdf5d.lib"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "hdf5___W"
# PROP BASE Intermediate_Dir "hdf5___W"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
CPP=cl.exe
# ADD BASE CPP /nologo /Gt0 /W3 /GX /Z7 /Od /I "..\..\src" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /FD /c
# ADD CPP /nologo /Gt0 /W3 /GX /Z7 /Od /I "..\..\src" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /FD /c
F90=df.exe
# ADD BASE F90 /browser /compile_only /debug:full /include:"hdf5___W/" /nologo /warn:nofileopt
# ADD F90 /browser /compile_only /debug:full /include:"Debug/" /nologo /warn:nofileopt
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "hdf5___0"
# PROP BASE Intermediate_Dir "hdf5___0"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
CPP=cl.exe
# ADD BASE CPP /nologo /Gt0 /W3 /GX /O2 /I "..\..\src" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /Gt0 /W3 /GX /O2 /I "..\..\src" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
F90=df.exe
# ADD BASE F90 /compile_only /include:"hdf5___0/" /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "hdf5 - Win32 Release"
# Name "hdf5 - Win32 Debug"
# Name "hdf5 - Win32 AlphaDbg"
# Name "hdf5 - Win32 AlphaRel"
# Begin Group "source"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\..\src\H5.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5A.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5AC.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5B.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5B2.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5B2cache.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5B2dbg.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5B2int.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5B2stat.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5B2test.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Bcache.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5C.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5D.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5dbg.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Dcompact.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Dcontig.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Defl.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Dio.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Distore.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Dmpio.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Doh.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Dselect.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Dtest.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5E.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5F.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FD.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Fdbg.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDcore.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDfamily.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDlog.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDmpi.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDmpio.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDmpiposix.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDmulti.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDsec2.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDstdio.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDstream.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FL.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Fmount.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FO.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FS.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FScache.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FSdbg.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Fsfile.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Fsuper.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5G.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gent.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Glink.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gloc.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gname.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gnode.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gobj.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Goh.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gstab.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gtest.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gtraverse.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HF.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFcache.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFdbg.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFdblock.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFdtable.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFhdr.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFiblock.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFint.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFiter.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFsection.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFspace.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFstat.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFtest.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HG.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HGdbg.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HL.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HLdbg.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HP.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5I.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5MF.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5MM.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5MP.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5MPtest.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5O.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Oattr.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Obogus.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ocache.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ocont.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Odtype.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Oefl.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ofill.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Oginfo.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Olayout.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Olinfo.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Olink.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Omtime.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Oname.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Onull.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Opline.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Osdspace.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Oshared.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ostab.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5P.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pacpl.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pdcpl.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pdxpl.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pfapl.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pfcpl.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pgcpl.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pocpl.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ptest.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5R.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5RC.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5RS.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5S.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Sall.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Shyper.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5SL.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Smpio.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Snone.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Spoint.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Sselect.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5ST.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Stest.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5T.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tarray.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tbit.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tcommit.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tcompound.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tconv.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tcset.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tenum.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tfields.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tfixed.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tfloat.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tinit.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tnative.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Toffset.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Toh.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Topaque.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Torder.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tpad.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tprecis.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tstrpad.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tvlen.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5V.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Z.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Zdeflate.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Zfletcher32.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Znbit.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Zscaleoffset.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Zshuffle.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Zszip.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ztrans.c

!IF  "$(CFG)" == "hdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\proj\hdf5\Debug"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# End Group
# Begin Group "header"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\..\src\H5ACprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5ACpublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Apkg.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Aprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Apublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Bprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Bpublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5BTpkg.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Cprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Cpublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Dpkg.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Dprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Dpublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Eprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Epublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Fprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Fpublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gpkg.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gpublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HGprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HGpublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HLprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HLpublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Iprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ipublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5MFprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5MMprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5MMpublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5MPpkg.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Oprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Opublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ppublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5private.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5public.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5RCprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Rprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Rpublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5SHpkg.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5SHprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5SHpublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5SLprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Sprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Spublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tpkg.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tpublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Vprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Zprivate.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Zpublic.h
# End Source File
# Begin Source File

SOURCE=..\..\..\src\hdf5.h
# End Source File
# End Group
# End Target
# End Project
