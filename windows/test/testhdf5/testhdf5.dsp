# Microsoft Developer Studio Project File - Name="testhdf5" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103
# TARGTYPE "Win32 (ALPHA) Console Application" 0x0603

CFG=testhdf5 - Win32 AlphaDbg
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "testhdf5.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "testhdf5.mak" CFG="testhdf5 - Win32 AlphaDbg"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "testhdf5 - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "testhdf5 - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "testhdf5 - Win32 AlphaDbg" (based on "Win32 (ALPHA) Console Application")
!MESSAGE "testhdf5 - Win32 AlphaRel" (based on "Win32 (ALPHA) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "../../../test/testhdf5/Release"
# PROP Intermediate_Dir "../../../test/testhdf5/Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
F90=df.exe
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
CPP=cl.exe
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\..\..\src" /I "..\..\..\test" /D "_CONSOLE" /D "_MBCS" /D "NDEBUG" /D "WIN32" /YX /FD /c
RSC=rc.exe
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib WS2_32.lib $(HDF5_EXT_ZLIB) $(HDF5_EXT_SZIP) /nologo /subsystem:console /machine:I386 /nodefaultlib:"libcd.lib" /include:"_gethostname@8"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "../../../test/testhdf5/Debug"
# PROP Intermediate_Dir "../../../test/testhdf5/Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
F90=df.exe
# ADD BASE F90 /compile_only /debug:full /include:"Debug/" /nologo /warn:nofileopt
# ADD F90 /compile_only /debug:full /include:"Debug/" /nologo /warn:nofileopt
CPP=cl.exe
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "..\..\..\src" /I "..\..\..\test" /D "_CONSOLE" /D "_MBCS" /D "_DEBUG" /D "WIN32" /YX /FD /c
RSC=rc.exe
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib WS2_32.lib $(HDF5_EXT_ZLIB) $(HDF5_EXT_SZIP) /nologo /subsystem:console /debug /machine:I386 /nodefaultlib:"libc.lib" /include:"_gethostname@8" /pdbtype:sept

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "testhdf5"
# PROP BASE Intermediate_Dir "testhdf5"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
CPP=cl.exe
# ADD BASE CPP /nologo /Gt0 /W3 /GX /Zi /Od /I "..\..\src" /I "..\..\test" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /Gt0 /W3 /GX /Zi /Od /I "..\..\src" /I "..\..\test" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
F90=df.exe
# ADD BASE F90 /compile_only /debug:full /include:"testhdf5/" /nologo /warn:nofileopt
# ADD F90 /compile_only /debug:full /include:"Debug/" /nologo /warn:nofileopt
RSC=rc.exe
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /debug /machine:ALPHA /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /debug /machine:ALPHA /pdbtype:sept

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "testhdf0"
# PROP BASE Intermediate_Dir "testhdf0"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
CPP=cl.exe
# ADD BASE CPP /nologo /Gt0 /W3 /GX /O2 /I "..\..\src" /I "..\..\test" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /Gt0 /W3 /GX /O2 /I "..\..\src" /I "..\..\test" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
F90=df.exe
# ADD BASE F90 /compile_only /include:"testhdf0/" /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
RSC=rc.exe
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /machine:ALPHA
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /machine:ALPHA

!ENDIF 

# Begin Target

# Name "testhdf5 - Win32 Release"
# Name "testhdf5 - Win32 Debug"
# Name "testhdf5 - Win32 AlphaDbg"
# Name "testhdf5 - Win32 AlphaRel"
# Begin Source File

SOURCE=..\..\..\test\tarray.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\tattr.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\tconfig.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\testframe.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\testhdf5.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\tfile.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\tgenprop.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\th5s.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\theap.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\tid.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\titerate.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\tmeta.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\tmisc.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\trefer.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\trefstr.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\tselect.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\tskiplist.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\ttime.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\ttst.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\tunicode.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\tvlstr.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\test\tvltypes.c

!IF  "$(CFG)" == "testhdf5 - Win32 Release"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 Debug"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "testhdf5 - Win32 AlphaRel"

!ENDIF 

# End Source File
# End Target
# End Project
