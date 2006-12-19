# Microsoft Developer Studio Project File - Name="hdf5dll" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102
# TARGTYPE "Win32 (ALPHA) Dynamic-Link Library" 0x0602

CFG=hdf5dll - Win32 AlphaDbg
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "hdf5dll.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "hdf5dll.mak" CFG="hdf5dll - Win32 AlphaDbg"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "hdf5dll - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "hdf5dll - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "hdf5dll - Win32 AlphaDbg" (based on "Win32 (ALPHA) Dynamic-Link Library")
!MESSAGE "hdf5dll - Win32 AlphaRel" (based on "Win32 (ALPHA) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "../../../proj/hdf5dll/Release"
# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
F90=df.exe
# ADD BASE F90 /compile_only /include:"Release/" /dll /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"../../../proj/hdf5dll/Release/" /include:"Release/" /dll /nologo /warn:nofileopt
CPP=cl.exe
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_HDF5DLL_" /D "ZLIB_DLL" /D "_SZDLL_" /YX /FD /c
MTL=midl.exe
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
RSC=rc.exe
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib $(HDF5_EXT_ZLIB) $(HDF5_EXT_SZIP) /nologo /subsystem:windows /dll /machine:I386

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "../../../proj/hdf5dll/Debug"
# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
F90=df.exe
# ADD BASE F90 /compile_only /debug:full /include:"Debug/" /dll /nologo /warn:nofileopt
# ADD F90 /compile_only /debug:full /include:"../../../proj/hdf5dll/Debug/" /include:"Debug/" /dll /nologo /warn:nofileopt
CPP=cl.exe
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_HDF5DLL_" /D "ZLIB_DLL" /D "_SZDLL_" /YX /FD /c
MTL=midl.exe
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
RSC=rc.exe
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib $(HDF5_EXT_ZLIB) $(HDF5_EXT_SZIP) /nologo /subsystem:windows /dll /debug /machine:I386 /out:"../../../proj/hdf5dll/Debug/hdf5ddll.dll" /pdbtype:sept

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "hdf5dll_"
# PROP BASE Intermediate_Dir "hdf5dll_"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
CPP=cl.exe
# ADD BASE CPP /nologo /Gt0 /W3 /GX /Zi /Od /I "..\..\src" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_HDF5DLL_" /YX /FD /MTd /c
# ADD CPP /nologo /Gt0 /W3 /GX /Zi /Od /I "..\..\src" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_HDF5DLL_" /YX /FD /MDd /c
F90=df.exe
# ADD BASE F90 /compile_only /debug:full /include:"hdf5dll_/" /dll /nologo /warn:nofileopt
# ADD F90 /compile_only /debug:full /include:"Debug/" /dll /nologo /warn:nofileopt
MTL=midl.exe
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
RSC=rc.exe
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /dll /debug /machine:ALPHA /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /dll /debug /machine:ALPHA /pdbtype:sept

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "hdf5dll0"
# PROP BASE Intermediate_Dir "hdf5dll0"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
CPP=cl.exe
# ADD BASE CPP /nologo /MT /Gt0 /W3 /GX /O2 /I "..\..\src" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_HDF5DLL_" /YX /FD /c
# ADD CPP /nologo /MD /Gt0 /W3 /GX /O2 /I "..\..\src" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_HDF5DLL_" /YX /FD /c
F90=df.exe
# ADD BASE F90 /compile_only /include:"hdf5dll0/" /dll /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"Release/" /dll /nologo /warn:nofileopt
MTL=midl.exe
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
RSC=rc.exe
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /dll /machine:ALPHA
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /dll /machine:ALPHA

!ENDIF 

# Begin Target

# Name "hdf5dll - Win32 Release"
# Name "hdf5dll - Win32 Debug"
# Name "hdf5dll - Win32 AlphaDbg"
# Name "hdf5dll - Win32 AlphaRel"
# Begin Group "source"

# PROP Default_Filter "*.c"
# Begin Source File

SOURCE=..\..\..\src\H5.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5A.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Abtree2.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5AC.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Adense.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Adeprec.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Aint.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5B.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5B2.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5B2cache.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5B2dbg.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5B2int.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5B2stat.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5B2test.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Bcache.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5C.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5checksum.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5D.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5dbg.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Dcompact.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Dcontig.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Defl.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Dio.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Distore.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Dmpio.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Doh.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Dselect.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Dtest.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5E.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5F.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FD.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Fdbg.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDcore.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDfamily.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDlog.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDmpi.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDmpio.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDmpiposix.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDmulti.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDsec2.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDstdio.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FDstream.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ffake.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FL.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Fmount.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FO.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FS.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FScache.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FSdbg.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Fsfile.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5FSsection.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Fsuper.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5G.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gbtree2.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gcompact.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gdense.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gdeprec.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gent.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Glink.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gloc.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gname.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gnode.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gobj.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Goh.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gstab.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gtest.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Gtraverse.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HF.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFbtree2.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFcache.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFdbg.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFdblock.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFdtable.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFhdr.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFhuge.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFiblock.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFiter.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFman.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFsection.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFspace.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFstat.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFtest.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HFtiny.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HG.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HGdbg.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HL.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HLdbg.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5HP.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5I.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5L.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Lexternal.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5MF.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5MM.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5MP.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5MPtest.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5O.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Oalloc.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Oattr.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Oattribute.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Obogus.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ocache.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ocont.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ocopy.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Odbg.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Odtype.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Oefl.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ofill.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Oginfo.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Olayout.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Olinfo.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Olink.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Omessage.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Omtime.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Oname.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Onull.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Opline.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Osdspace.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Oshared.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ostab.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Otest.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5P.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pacpl.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pdcpl.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pdxpl.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pfapl.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pfcpl.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pfmpl.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pgcpl.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Plapl.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Plcpl.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pocpl.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pocpypl.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Pstrcpl.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ptest.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5R.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5RC.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5RS.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5S.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Sall.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Shyper.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5SL.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5SM.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5SMbtree2.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5SMcache.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Smpio.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Snone.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Spoint.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Sselect.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5ST.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Stest.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5system.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5T.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tarray.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tbit.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tcommit.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tcompound.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tconv.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tcset.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tenum.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tfields.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tfixed.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tfloat.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5timer.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tinit.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tnative.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Toffset.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Toh.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Topaque.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Torder.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tpad.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tprecis.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5trace.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tstrpad.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Tvlen.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5V.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Z.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Zdeflate.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Zfletcher32.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Znbit.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Zscaleoffset.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Zshuffle.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Zszip.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\src\H5Ztrans.c

!IF  "$(CFG)" == "hdf5dll - Win32 Release"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Release"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 Debug"

# PROP Intermediate_Dir "../../../proj/hdf5dll/Debug"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaDbg"

!ELSEIF  "$(CFG)" == "hdf5dll - Win32 AlphaRel"

!ENDIF 

# End Source File
# End Group
# Begin Group "header "

# PROP Default_Filter "*.h"
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
