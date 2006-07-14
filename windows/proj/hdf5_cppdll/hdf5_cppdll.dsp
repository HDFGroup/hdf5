# Microsoft Developer Studio Project File - Name="hdf5_cppdll" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=hdf5_cppdll - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "hdf5_cppdll.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "hdf5_cppdll.mak" CFG="hdf5_cppdll - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "hdf5_cppdll - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "hdf5_cppdll - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "hdf5_cppdll - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "../../../proj/hdf5_cppdll/Release"
# PROP Intermediate_Dir "../../../proj/hdf5_cppdll/Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /include:"Release/"
# ADD F90 /include:"Release/"
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "HDF5_CPPDLL_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\..\..\src" /D "HDF5_CPPDLL_EXPORTS" /D "_HDF5USEDLL_" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386

!ELSEIF  "$(CFG)" == "hdf5_cppdll - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "../../../proj/hdf5_cppdll/Debug"
# PROP Intermediate_Dir "../../../proj/hdf5_cppdll/Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /include:"Debug/"
# ADD F90 /include:"Debug/"
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "HDF5_CPPDLL_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\..\..\src" /D "HDF5_CPPDLL_EXPORTS" /D "_HDF5USEDLL_" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo /o"Debug/hdf5_cppddll.bsc"
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /out:"../../../proj/hdf5_cppdll/Debug/hdf5_cppddll.dll" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "hdf5_cppdll - Win32 Release"
# Name "hdf5_cppdll - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE="..\..\..\c++\src\H5AbstractDs.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5ArrayType.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5AtomType.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5Attribute.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5CommonFG.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5CompType.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5DataSet.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5DataSpace.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5DataType.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5DcreatProp.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5DxferProp.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5EnumType.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5Exception.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5FaccProp.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5FcreatProp.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5File.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5FloatType.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5Group.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5IdComponent.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5IntType.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5Library.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5Object.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5PredType.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5PropList.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5StrType.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5VarLenType.cpp"
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE="..\..\..\c++\src\H5AbstractDs.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5Alltypes.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5ArrayType.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5AtomType.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5Attribute.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5Classes.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5CommonFG.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5CompType.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5Cpp.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5DataSet.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5DataSpace.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5DataType.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5DcreatProp.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5DxferProp.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5EnumType.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5Exception.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5FaccProp.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5FcreatProp.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5File.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5FloatType.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5Group.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5IdComponent.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5Include.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5IntType.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5Library.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5Object.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5PredType.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5PropList.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5StrType.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\c++\src\H5VarLenType.h"
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
