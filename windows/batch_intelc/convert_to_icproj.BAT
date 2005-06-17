@REM File Name: convert_to_icproj.bat
@REM Purpose: Convert Visual C++ 6.0 project format to Visual Studio .net project format.
@REM Written By: Fang GUO
@REM Date: May 27, 2005
@REM Update: June 7, 2005
@REM Purpose: Convert Visual C++ 7.0 project format to Intel C++ project format
@REM There are 2 options for this batch file:
@REM   1. convert_to_icproj                -- Convert only C project files
@REM   2. convert_to_icproj enablecpp      -- Convert C and C++ project files


@ECHO OFF
if %1.==. GOTO convertC
if "%1"=="/?" GOTO HELP
if %1==enablecpp GOTO convertCPP
GOTO WRONG

:convertC

type nul >convert_intel.log
echo.
echo Converting project files in directory windows\proj
echo.


  ICProjConvert80 windows\proj\hdf5\hdf5.vcproj /IC  >> convert_intel.log
  ICProjConvert80 windows\proj\hdf5_cpp\hdf5_cpp.vcproj /IC  >> convert_intel.log
  ICProjConvert80 windows\proj\hdf5_cppdll\hdf5_cppdll.vcproj /IC  >> convert_intel.log
  ICProjConvert80 windows\proj\hdf5_hl\hdf5_hl.vcproj /IC  >> convert_intel.log
  ICProjConvert80 windows\proj\hdf5_hl_fortran\hdf5_hl_fortran.vcproj /IC  >> convert_intel.log
  ICProjConvert80 windows\proj\hdf5dll\hdf5dll.vcproj /IC  >> convert_intel.log
  ICProjConvert80 windows\proj\hdf5_hl_cpp\hdf5_hl_cpp.vcproj /IC  >> convert_intel.log

echo.
echo Converting project files in directory windows\test
echo.

  
ICProjConvert80  windows\test\big\big.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\bigdll\bigdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\bittests\bittests.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\bittestsdll\bittestsdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\blocktrack\blocktrack.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\blocktrackdll\blocktrackdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\btree2\btree2.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\btree2dll\btree2dll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\cache\cache.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\cachedll\cachedll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\chunk\chunk.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\chunkdll\chunkdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\cmpd_dset\cmpd_dset.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\cmpd_dsetdll\cmpd_dsetdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\dangle\dangle.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\dangledll\dangledll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\dt_arith\dt_arith.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\dt_arithdll\dt_arithdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\dsets\dsets.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\dsetsdll\dsetsdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\dtransform\dtransform.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\dtransformdll\dtransformdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\dtypes\dtypes.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\dtypesdll\dtypesdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\enum\enum.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\enumdll\enumdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\extend\extend.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\extenddll\extenddll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\external\external.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\externaldll\externaldll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\file_handle\file_handle.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\file_handledll\file_handledll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\filename\filename.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\filenamedll\filenamedll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\fillval\fillval.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\fillvaldll\fillvaldll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\flush1\flush1.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\flush1dll\flush1dll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\flush2\flush2.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\flush2dll\flush2dll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\getname\getname.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\getnamedll\getnamedll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\getub\getub.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\gheap\gheap.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\gheapdll\gheapdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\hyperslab\hyperslab.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\hyperslabdll\hyperslabdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\iopipe\iopipe.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\iopipedll\iopipedll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\istore\istore.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\istoredll\istoredll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\lheap\lheap.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\lheapdll\lheapdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\libtest\libtest.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\libtestD\libtestD.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\links\links.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\linksdll\linksdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\mount\mount.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\mountdll\mountdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\mtime\mtime.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\mtimedll\mtimedll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\ntypes\ntypes.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\ntypesdll\ntypesdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\ohdr\ohdr.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\ohdrdll\ohdrdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\overhead\overhead.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\overheaddll\overheaddll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\pool\pool.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\pooldll\pooldll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\reserved\reserved.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\reserveddll\reserveddll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\set_extent\set_extent.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\set_extentdll\set_extentdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\sheap\sheap.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\sheapdll\sheapdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\stab\stab.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\stabdll\stabdll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\tellub\tellub.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\testhdf5\testhdf5.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\testhdf5dll\testhdf5dll.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\unlink\unlink.vcproj /IC  >> convert_intel.log
ICProjConvert80  windows\test\unlinkdll\unlinkdll.vcproj /IC  >> convert_intel.log

echo.
echo Converting project files under windows\hl\test
echo.

  ICProjConvert80 windows\hl\test\hl_test_image\hl_test_image.vcproj /IC  >> convert_intel.log
  ICProjConvert80 windows\hl\test\hl_test_lite\hl_test_lite.vcproj /IC  >> convert_intel.log
  ICProjConvert80 windows\hl\test\hl_test_table\hl_test_table.vcproj /IC  >> convert_intel.log
  ICProjConvert80 windows\hl\test\hl_test_table\hl_test_ds.vcproj /IC  >> convert_intel.log
  ICProjConvert80 windows\hl\test\hl_test_table\hl_test_packet.vcproj /IC  >> convert_intel.log

echo.
echo Converting project files under directory windows\tools
echo.
  
ICProjConvert80  windows\tools\gifconvdll\gif2h5dll.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\gifconvdll\h52gifdll.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5diffdll\h5diffdll.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5debug\h5debug.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5debugdll\h5debugdll.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5dumpdll\h5dumpdll.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\gifconv\gif2h5.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5diffdll\h5diffdll.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5repackdll\h5repackdll.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\gifconv\h52gif.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5diff\h5diff.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5ls\h5ls.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5lsdll\h5lsdll.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5dump\h5dump.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5repack\h5repack.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5unjam\h5unjam.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5repart\h5repart.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\talign\talign.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5repartdll\h5repartdll.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5import\h5import.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5importdll\h5importdll.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\taligndll\taligndll.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\h5jam\h5jam.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\testfiles\h5difftst\h5difftst.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\testfiles\h5dumptst\h5dumptst.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\testfiles\h5importtst\h5importtst.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\testfiles\h5jamtst\h5jamtst.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\testfiles\h5repacktst\h5repacktst.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\testfiles\testh5repack_detect_szip\testh5repack_detect_szip.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\testfiles\testh5repack_detect_szipdll\testh5repack_detect_szipdll.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\toolslib\toolslib.vcproj /IC	>>	convert_intel.log
ICProjConvert80  windows\tools\toolslibD\toolslibD.vcproj /IC	>>	convert_intel.log

echo End converting C project Files


GOTO END

:convertCPP

echo **************************************************
echo.
echo Converting C++ project files in directory windows\proj
echo.


  ICProjConvert80 windows\proj\hdf5_cpp\hdf5_cpp.vcproj /IC  >> convert_intel.log
  ICProjConvert80 windows\proj\hdf5_cppdll\hdf5_cppdll.vcproj /IC  >> convert_intel.log
  ICProjConvert80 windows\proj\hdf5_hl_cpp\hdf5_hl_cpp.vcproj /IC  >> convert_intel.log
  ICProjConvert80  windows\c++\test\dsets_cpp\dsets_cpp.vcproj /IC  >> convert_intel.log
  ICProjConvert80  windows\c++\test\dsets_cppdll\dsets_cppdll.vcproj /IC  >> convert_intel.log
  ICProjConvert80  windows\c++\test\testhdf5_cpp\testhdf5_cpp.vcproj /IC  >> convert_intel.log
  ICProjConvert80  windows\c++\test\testhdf5_cppdll\testhdf5_cppdll.vcproj /IC  >> convert_intel.log
  ICProjConvert80 windows\hl\c++\test\hl_test_table_cpp\hl_test_table_cpp.vcproj /IC  >> convert_intel.log
      
echo.
echo End converting C++ project Files
echo.
echo Start Building HDF5 C and C++ libraries ...       
GOTO END

:HELP
echo   There are 2 options for this batch file:
echo   1. convert_to_icproj                -- Convert only C project files
echo   2. convert_to_icproj enablecpp      -- Convert C and C++ project files
GOTO END

:WRONG

echo The syntax of the command is incorrect.
echo.
echo Using the command in the following format:
echo   1. convert_to_icproj                -- Convert only C project files
echo   2. convert_to_icproj enablecpp      -- Convert C and C++ project files
GOTO END

:END
