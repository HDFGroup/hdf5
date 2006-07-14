# Microsoft Developer Studio Project File - Name="toolslibD" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (ALPHA) Static Library" 0x0604
# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=toolslibD - Win32 AlphaDbg
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "toolslibD.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "toolslibD.mak" CFG="toolslibD - Win32 AlphaDbg"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "toolslibD - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "toolslibD - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "toolslibD - Win32 AlphaDbg" (based on "Win32 (ALPHA) Static Library")
!MESSAGE "toolslibD - Win32 AlphaRel" (based on "Win32 (ALPHA) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "toolslibD - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\..\tools\toolslibD\Release"
# PROP Intermediate_Dir "..\..\..\tools\toolslibD\Release"
# PROP Target_Dir ""
F90=df.exe
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
CPP=cl.exe
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\..\..\src" /I "..\..\..\tools\lib" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_HDF5USEDLL_" /YX /FD /c
RSC=rc.exe
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "toolslibD - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\..\tools\toolslibD\Debug"
# PROP Intermediate_Dir "..\..\..\tools\toolslibD\Debug"
# PROP Target_Dir ""
F90=df.exe
# ADD BASE F90 /compile_only /debug:full /include:"Debug/" /nologo /warn:nofileopt
# ADD F90 /compile_only /debug:full /include:"Debug/" /nologo /warn:nofileopt
CPP=cl.exe
# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MDd /W3 /GX /Z7 /Od /I "..\..\..\src" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_HDF5USEDLL_" /YX /FD /c
RSC=rc.exe
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaDbg"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "toolslib"
# PROP BASE Intermediate_Dir "toolslib"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "toolslib"
# PROP Intermediate_Dir "toolslib"
# PROP Target_Dir ""
CPP=cl.exe
# ADD BASE CPP /nologo /Gt0 /W3 /GX /Z7 /Od /I ".." /I "..\..\src" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_HDF5USEDLL_" /YX /FD /c
# ADD CPP /nologo /Gt0 /W3 /GX /Z7 /Od /I ".." /I "..\..\src" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_HDF5USEDLL_" /YX /FD /c
F90=df.exe
# ADD BASE F90 /compile_only /debug:full /include:"toolslib/" /nologo /warn:nofileopt
# ADD F90 /compile_only /debug:full /include:"toolslib/" /nologo /warn:nofileopt
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaRel"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "toolsli0"
# PROP BASE Intermediate_Dir "toolsli0"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "toolsli0"
# PROP Intermediate_Dir "toolsli0"
# PROP Target_Dir ""
CPP=cl.exe
# ADD BASE CPP /nologo /Gt0 /W3 /GX /O2 /I ".." /I "..\..\src" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_HDF5USEDLL_" /YX /FD /c
# ADD CPP /nologo /Gt0 /W3 /GX /O2 /I ".." /I "..\..\src" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_HDF5USEDLL_" /YX /FD /c
F90=df.exe
# ADD BASE F90 /compile_only /include:"toolsli0/" /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"toolsli0/" /nologo /warn:nofileopt
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "toolslibD - Win32 Release"
# Name "toolslibD - Win32 Debug"
# Name "toolslibD - Win32 AlphaDbg"
# Name "toolslibD - Win32 AlphaRel"
# Begin Group "source"

# PROP Default_Filter "*.c"
# Begin Source File

SOURCE=..\..\..\tools\lib\h5diff.c

!IF  "$(CFG)" == "toolslibD - Win32 Release"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 Debug"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5diff_array.c

!IF  "$(CFG)" == "toolslibD - Win32 Release"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 Debug"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5diff_attr.c

!IF  "$(CFG)" == "toolslibD - Win32 Release"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 Debug"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5diff_dset.c

!IF  "$(CFG)" == "toolslibD - Win32 Release"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 Debug"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5diff_util.c

!IF  "$(CFG)" == "toolslibD - Win32 Release"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 Debug"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5tools.c

!IF  "$(CFG)" == "toolslibD - Win32 Release"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 Debug"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5tools_filters.c

!IF  "$(CFG)" == "toolslibD - Win32 Release"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 Debug"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5tools_ref.c

!IF  "$(CFG)" == "toolslibD - Win32 Release"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 Debug"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5tools_str.c

!IF  "$(CFG)" == "toolslibD - Win32 Release"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 Debug"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5tools_type.c

!IF  "$(CFG)" == "toolslibD - Win32 Release"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 Debug"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5tools_utils.c

!IF  "$(CFG)" == "toolslibD - Win32 Release"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 Debug"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5trav.c

!IF  "$(CFG)" == "toolslibD - Win32 Release"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 Debug"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5trav_table.c

!IF  "$(CFG)" == "toolslibD - Win32 Release"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 Debug"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "toolslibD - Win32 AlphaRel"

!ENDIF 

# End Source File
# End Group
# Begin Group "headers"

# PROP Default_Filter "*.h"
# Begin Source File

SOURCE=..\..\..\tools\lib\h5diff.h
# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5tools.h
# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5tools_ref.h
# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5tools_str.h
# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5tools_utils.h
# End Source File
# Begin Source File

SOURCE=..\..\..\tools\lib\h5trav.h
# End Source File
# End Group
# End Target
# End Project
