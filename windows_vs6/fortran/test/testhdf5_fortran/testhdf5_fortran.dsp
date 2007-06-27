# Microsoft Developer Studio Project File - Name="testhdf5_fortran" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=testhdf5_fortran - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "testhdf5_fortran.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "testhdf5_fortran.mak" CFG="testhdf5_fortran - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "testhdf5_fortran - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "testhdf5_fortran - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "testhdf5_fortran - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "../../../../fortran/test/testhdf5_fortran/Release"
# PROP Intermediate_Dir "../../../../fortran/test/testhdf5_fortran/Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"Release/" /include:"..\..\..\..\proj\hdf5_fortran\Release/" /nologo /warn:nofileopt /module:"..\..\..\proj\hdf5_fortran\Release/"
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\..\..\..\src" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib WS2_32.lib $(HDF5_EXT_ZLIB) $(HDF5_EXT_SZIP) /nologo /subsystem:console /machine:I386 /nodefaultlib:"libcd.lib"

!ELSEIF  "$(CFG)" == "testhdf5_fortran - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "../../../../fortran/test/testhdf5_fortran/Debug"
# PROP Intermediate_Dir "../../../../fortran/test/testhdf5_fortran/Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /debug:full /include:"Debug/" /include:"..\..\..\..\proj\hdf5_fortran\Debug/" /nologo /warn:argument_checking /warn:nofileopt /module:"..\..\..\..\proj\hdf5_fortran\Debug/"
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "..\..\..\..\src" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib WS2_32.lib $(HDF5_EXT_ZLIB) $(HDF5_EXT_SZIP) /nologo /subsystem:console /debug /machine:I386 /nodefaultlib:"libcd.lib" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "testhdf5_fortran - Win32 Release"
# Name "testhdf5_fortran - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\..\..\..\fortran\test\fortranlib_test.f90
NODEP_F90_FORTR=\
	"..\..\..\..\proj\hdf5_fortran\Debug\HDF5.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\..\fortran\test\tH5A.f90
NODEP_F90_TH5A_=\
	"..\..\..\..\proj\hdf5_fortran\Debug\HDF5.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\..\fortran\test\tH5D.f90
NODEP_F90_TH5D_=\
	"..\..\..\..\proj\hdf5_fortran\Debug\HDF5.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\..\fortran\test\tH5E.f90
NODEP_F90_TH5E_=\
	"..\..\..\..\proj\hdf5_fortran\Debug\HDF5.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\..\fortran\test\tH5F.f90
NODEP_F90_TH5F_=\
	"..\..\..\..\proj\hdf5_fortran\Debug\HDF5.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\..\fortran\test\tH5G.f90
NODEP_F90_TH5G_=\
	"..\..\..\..\proj\hdf5_fortran\Debug\HDF5.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\..\fortran\test\tH5I.f90
NODEP_F90_TH5I_=\
	"..\..\..\..\proj\hdf5_fortran\Debug\HDF5.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\..\fortran\test\tH5P.f90
NODEP_F90_TH5P_=\
	"..\..\..\..\proj\hdf5_fortran\Debug\HDF5.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\..\fortran\test\tH5R.f90
NODEP_F90_TH5R_=\
	"..\..\..\..\proj\hdf5_fortran\Debug\HDF5.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\..\fortran\test\tH5S.f90
NODEP_F90_TH5S_=\
	"..\..\..\..\proj\hdf5_fortran\Debug\HDF5.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\..\fortran\test\tH5Sselect.f90
NODEP_F90_TH5SS=\
	"..\..\..\..\proj\hdf5_fortran\Debug\HDF5.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\..\fortran\test\tH5T.f90
NODEP_F90_TH5T_=\
	"..\..\..\..\proj\hdf5_fortran\Debug\HDF5.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\..\fortran\test\tH5VL.f90
NODEP_F90_TH5VL=\
	"..\..\..\..\proj\hdf5_fortran\Debug\HDF5.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\..\..\..\fortran\test\tH5Z.f90
NODEP_F90_TH5Z_=\
	"..\..\..\..\proj\hdf5_fortran\Debug\HDF5.MOD"\
	
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
