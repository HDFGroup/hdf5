@REM File Name: convert_to_vcproj.bat
@REM Purpose: Convert Visual C++ 6.0 project format to Visual Studio .net project format.
@REM Written By: Fang GUO
@REM Date: May 27, 2005
@REM Update: June 7, 2005

@echo off

type nul > convert.log
echo.>>convert.log
echo This batch file will convert all .dsp files in HDF5 C,  >> convert.log
echo C++ and Fortran library to .vcproj  format >> convert.log

CScript //H:CScript //Nologo

echo ************************************************************ >> convert.log
echo Converting project files in directory %1\windows\proj >> convert.log
echo ************************************************************ >> convert.log

  convert.js %1\windows\proj\all\all.dsp  %1\windows\proj\all\all.vcproj  >> convert.log
  convert.js %1\windows\proj\hdf5\hdf5.dsp  %1\windows\proj\hdf5\hdf5.vcproj  >> convert.log
  convert.js %1\windows\proj\hdf5_cpp\hdf5_cpp.dsp  %1\windows\proj\hdf5_cpp\hdf5_cpp.vcproj  >> convert.log
  convert.js %1\windows\proj\hdf5_cppdll\hdf5_cppdll.dsp  %1\windows\proj\hdf5_cppdll\hdf5_cppdll.vcproj  >> convert.log
  convert.js %1\windows\proj\hdf5_f90cstub\hdf5_f90cstub.dsp  %1\windows\proj\hdf5_f90cstub\hdf5_f90cstub.vcproj  >> convert.log
  convert.js %1\windows\proj\hdf5_f90cstubdll\hdf5_f90cstubdll.dsp  %1\windows\proj\hdf5_f90cstubdll\hdf5_f90cstubdll.vcproj  >> convert.log
  convert.js %1\windows\proj\hdf5_fortran\hdf5_fortran.dsp  %1\windows\proj\hdf5_fortran\hdf5_fortran.vcproj  >> convert.log
  convert.js %1\windows\proj\hdf5_fortrandll\hdf5_fortrandll.dsp  %1\windows\proj\hdf5_fortrandll\hdf5_fortrandll.vcproj  >> convert.log
  convert.js %1\windows\proj\hdf5_hl\hdf5_hl.dsp  %1\windows\proj\hdf5_hl\hdf5_hl.vcproj  >> convert.log
  convert.js %1\windows\proj\hdf5_hl_fortran\hdf5_hl_fortran.dsp  %1\windows\proj\hdf5_hl_fortran\hdf5_hl_fortran.vcproj  >> convert.log
  convert.js %1\windows\proj\hdf5dll\hdf5dll.dsp  %1\windows\proj\hdf5dll\hdf5dll.vcproj  >> convert.log
  convert.js %1\windows\proj\hdf5_hl_cpp\hdf5_hl_cpp.dsp  %1\windows\proj\hdf5_hl_cpp\hdf5_hl_cpp.vcproj  >> convert.log

echo ************************************************************  >> convert.log
echo Converting project files in directory %1\windows\test >> convert.log
echo ************************************************************ >> convert.log

  convert.js %1\windows\test\big\big.dsp  %1\windows\test\big\big.vcproj  >> convert.log
  convert.js %1\windows\test\bigdll\bigdll.dsp  %1\windows\test\bigdll\bigdll.vcproj  >> convert.log
  convert.js %1\windows\test\bittests\bittests.dsp  %1\windows\test\bittests\bittests.vcproj  >> convert.log
  convert.js %1\windows\test\bittestsdll\bittestsdll.dsp  %1\windows\test\bittestsdll\bittestsdll.vcproj  >> convert.log
  convert.js %1\windows\test\blocktrack\blocktrack.dsp  %1\windows\test\blocktrack\blocktrack.vcproj  >> convert.log
  convert.js %1\windows\test\blocktrackdll\blocktrackdll.dsp  %1\windows\test\blocktrackdll\blocktrackdll.vcproj  >> convert.log
  convert.js %1\windows\test\btree2\btree2.dsp  %1\windows\test\btree2\btree2.vcproj  >> convert.log
  convert.js %1\windows\test\btree2dll\btree2dll.dsp  %1\windows\test\btree2dll\btree2dll.vcproj  >> convert.log
  convert.js %1\windows\test\cache\cache.dsp  %1\windows\test\cache\cache.vcproj  >> convert.log
  convert.js %1\windows\test\cachedll\cachedll.dsp  %1\windows\test\cachedll\cachedll.vcproj  >> convert.log
  convert.js %1\windows\test\chunk\chunk.dsp  %1\windows\test\chunk\chunk.vcproj  >> convert.log
  convert.js %1\windows\test\chunkdll\chunkdll.dsp  %1\windows\test\chunkdll\chunkdll.vcproj  >> convert.log
  convert.js %1\windows\test\cmpd_dset\cmpd_dset.dsp  %1\windows\test\cmpd_dset\cmpd_dset.vcproj  >> convert.log
  convert.js %1\windows\test\cmpd_dsetdll\cmpd_dsetdll.dsp  %1\windows\test\cmpd_dsetdll\cmpd_dsetdll.vcproj  >> convert.log
  convert.js %1\windows\test\dangle\dangle.dsp  %1\windows\test\dangle\dangle.vcproj  >> convert.log
  convert.js %1\windows\test\dangledll\dangledll.dsp  %1\windows\test\dangledll\dangledll.vcproj  >> convert.log
  convert.js %1\windows\test\dt_atomic\dt_atomic.dsp  %1\windows\test\dt_atomic\dt_atomic.vcproj  >> convert.log
  convert.js %1\windows\test\dt_atomicdll\dt_atomicdll.dsp  %1\windows\test\dt_atomicdll\dt_atomicdll.vcproj  >> convert.log
  convert.js %1\windows\test\dsets\dsets.dsp  %1\windows\test\dsets\dsets.vcproj  >> convert.log
  convert.js %1\windows\test\dsetsdll\dsetsdll.dsp  %1\windows\test\dsetsdll\dsetsdll.vcproj  >> convert.log
  convert.js %1\windows\test\dtransform\dtransform.dsp  %1\windows\test\dtransform\dtransform.vcproj  >> convert.log
  convert.js %1\windows\test\dtransformdll\dtransformdll.dsp  %1\windows\test\dtransformdll\dtransformdll.vcproj  >> convert.log
  convert.js %1\windows\test\dtypes\dtypes.dsp  %1\windows\test\dtypes\dtypes.vcproj  >> convert.log
  convert.js %1\windows\test\dtypesdll\dtypesdll.dsp  %1\windows\test\dtypesdll\dtypesdll.vcproj  >> convert.log
  convert.js %1\windows\test\enum\enum.dsp  %1\windows\test\enum\enum.vcproj  >> convert.log
  convert.js %1\windows\test\enumdll\enumdll.dsp  %1\windows\test\enumdll\enumdll.vcproj  >> convert.log
  convert.js %1\windows\test\extend\extend.dsp  %1\windows\test\extend\extend.vcproj  >> convert.log
  convert.js %1\windows\test\extenddll\extenddll.dsp  %1\windows\test\extenddll\extenddll.vcproj  >> convert.log
  convert.js %1\windows\test\external\external.dsp  %1\windows\test\external\external.vcproj  >> convert.log
  convert.js %1\windows\test\externaldll\externaldll.dsp  %1\windows\test\externaldll\externaldll.vcproj  >> convert.log
  convert.js %1\windows\test\file_handle\file_handle.dsp  %1\windows\test\file_handle\file_handle.vcproj  >> convert.log
  convert.js %1\windows\test\file_handledll\file_handledll.dsp  %1\windows\test\file_handledll\file_handledll.vcproj  >> convert.log
  convert.js %1\windows\test\filename\filename.dsp  %1\windows\test\filename\filename.vcproj  >> convert.log
  convert.js %1\windows\test\filenamedll\filenamedll.dsp  %1\windows\test\filenamedll\filenamedll.vcproj  >> convert.log
  convert.js %1\windows\test\fillval\fillval.dsp  %1\windows\test\fillval\fillval.vcproj  >> convert.log
  convert.js %1\windows\test\fillvaldll\fillvaldll.dsp  %1\windows\test\fillvaldll\fillvaldll.vcproj  >> convert.log
  convert.js %1\windows\test\flush1\flush1.dsp  %1\windows\test\flush1\flush1.vcproj  >> convert.log
  convert.js %1\windows\test\flush1dll\flush1dll.dsp  %1\windows\test\flush1dll\flush1dll.vcproj  >> convert.log
  convert.js %1\windows\test\flush2\flush2.dsp  %1\windows\test\flush2\flush2.vcproj  >> convert.log
  convert.js %1\windows\test\flush2dll\flush2dll.dsp  %1\windows\test\flush2dll\flush2dll.vcproj  >> convert.log
  convert.js %1\windows\test\getname\getname.dsp  %1\windows\test\getname\getname.vcproj  >> convert.log
  convert.js %1\windows\test\getnamedll\getnamedll.dsp  %1\windows\test\getnamedll\getnamedll.vcproj  >> convert.log
  convert.js %1\windows\test\getub\getub.dsp  %1\windows\test\getub\getub.vcproj  >> convert.log
  convert.js %1\windows\test\gheap\gheap.dsp  %1\windows\test\gheap\gheap.vcproj  >> convert.log
  convert.js %1\windows\test\gheapdll\gheapdll.dsp  %1\windows\test\gheapdll\gheapdll.vcproj  >> convert.log
  convert.js %1\windows\test\hyperslab\hyperslab.dsp  %1\windows\test\hyperslab\hyperslab.vcproj  >> convert.log
  convert.js %1\windows\test\hyperslabdll\hyperslabdll.dsp  %1\windows\test\hyperslabdll\hyperslabdll.vcproj  >> convert.log
  convert.js %1\windows\test\iopipe\iopipe.dsp  %1\windows\test\iopipe\iopipe.vcproj  >> convert.log
  convert.js %1\windows\test\iopipedll\iopipedll.dsp  %1\windows\test\iopipedll\iopipedll.vcproj  >> convert.log
  convert.js %1\windows\test\istore\istore.dsp  %1\windows\test\istore\istore.vcproj  >> convert.log
  convert.js %1\windows\test\istoredll\istoredll.dsp  %1\windows\test\istoredll\istoredll.vcproj  >> convert.log
  convert.js %1\windows\test\lheap\lheap.dsp  %1\windows\test\lheap\lheap.vcproj  >> convert.log
  convert.js %1\windows\test\lheapdll\lheapdll.dsp  %1\windows\test\lheapdll\lheapdll.vcproj  >> convert.log
  convert.js %1\windows\test\libtest\libtest.dsp  %1\windows\test\libtest\libtest.vcproj  >> convert.log
  convert.js %1\windows\test\libtestD\libtestD.dsp  %1\windows\test\libtestD\libtestD.vcproj  >> convert.log
  convert.js %1\windows\test\links\links.dsp  %1\windows\test\links\links.vcproj  >> convert.log
  convert.js %1\windows\test\linksdll\linksdll.dsp  %1\windows\test\linksdll\linksdll.vcproj  >> convert.log
  convert.js %1\windows\test\mount\mount.dsp  %1\windows\test\mount\mount.vcproj  >> convert.log
  convert.js %1\windows\test\mountdll\mountdll.dsp  %1\windows\test\mountdll\mountdll.vcproj  >> convert.log
  convert.js %1\windows\test\mtime\mtime.dsp  %1\windows\test\mtime\mtime.vcproj  >> convert.log
  convert.js %1\windows\test\mtimedll\mtimedll.dsp  %1\windows\test\mtimedll\mtimedll.vcproj  >> convert.log
  convert.js %1\windows\test\ntypes\ntypes.dsp  %1\windows\test\ntypes\ntypes.vcproj  >> convert.log
  convert.js %1\windows\test\ntypesdll\ntypesdll.dsp  %1\windows\test\ntypesdll\ntypesdll.vcproj  >> convert.log
  convert.js %1\windows\test\ohdr\ohdr.dsp  %1\windows\test\ohdr\ohdr.vcproj  >> convert.log
  convert.js %1\windows\test\ohdrdll\ohdrdll.dsp  %1\windows\test\ohdrdll\ohdrdll.vcproj  >> convert.log
  convert.js %1\windows\test\overhead\overhead.dsp  %1\windows\test\overhead\overhead.vcproj  >> convert.log
  convert.js %1\windows\test\overheaddll\overheaddll.dsp  %1\windows\test\overheaddll\overheaddll.vcproj  >> convert.log
  convert.js %1\windows\test\pool\pool.dsp  %1\windows\test\pool\pool.vcproj  >> convert.log
  convert.js %1\windows\test\pooldll\pooldll.dsp  %1\windows\test\pooldll\pooldll.vcproj  >> convert.log
  convert.js %1\windows\test\reserved\reserved.dsp  %1\windows\test\reserved\reserved.vcproj  >> convert.log
  convert.js %1\windows\test\reserveddll\reserveddll.dsp  %1\windows\test\reserveddll\reserveddll.vcproj  >> convert.log
  convert.js %1\windows\test\set_extent\set_extent.dsp  %1\windows\test\set_extent\set_extent.vcproj  >> convert.log
  convert.js %1\windows\test\set_extentdll\set_extentdll.dsp  %1\windows\test\set_extentdll\set_extentdll.vcproj  >> convert.log
  convert.js %1\windows\test\sheap\sheap.dsp  %1\windows\test\sheap\sheap.vcproj  >> convert.log
  convert.js %1\windows\test\sheapdll\sheapdll.dsp  %1\windows\test\sheapdll\sheapdll.vcproj  >> convert.log
  convert.js %1\windows\test\stab\stab.dsp  %1\windows\test\stab\stab.vcproj  >> convert.log
  convert.js %1\windows\test\stabdll\stabdll.dsp  %1\windows\test\stabdll\stabdll.vcproj  >> convert.log
  convert.js %1\windows\test\tellub\tellub.dsp  %1\windows\test\tellub\tellub.vcproj  >> convert.log
  convert.js %1\windows\test\testhdf5\testhdf5.dsp  %1\windows\test\testhdf5\testhdf5.vcproj  >> convert.log
  convert.js %1\windows\test\testhdf5dll\testhdf5dll.dsp  %1\windows\test\testhdf5dll\testhdf5dll.vcproj  >> convert.log
  convert.js %1\windows\test\unlink\unlink.dsp  %1\windows\test\unlink\unlink.vcproj  >> convert.log
  convert.js %1\windows\test\unlinkdll\unlinkdll.dsp  %1\windows\test\unlinkdll\unlinkdll.vcproj  >> convert.log

echo ************************************************************ >>convert.log
echo Converting project files under %1\windows\c++\test >>convert.log
echo ************************************************************ >>convert.log
  convert.js  %1\windows\c++\test\dsets_cpp\dsets_cpp.dsp  %1\windows\c++\test\dsets_cpp\dsets_cpp.vcproj  >> convert.log
  convert.js  %1\windows\c++\test\dsets_cppdll\dsets_cppdll.dsp  %1\windows\c++\test\dsets_cppdll\dsets_cppdll.vcproj  >> convert.log
  convert.js  %1\windows\c++\test\testhdf5_cpp\testhdf5_cpp.dsp  %1\windows\c++\test\testhdf5_cpp\testhdf5_cpp.vcproj  >> convert.log
  convert.js  %1\windows\c++\test\testhdf5_cppdll\testhdf5_cppdll.dsp  %1\windows\c++\test\testhdf5_cppdll\testhdf5_cppdll.vcproj  >> convert.log

echo ************************************************************ >> convert.log
echo Converting project files under hdf5\%1\windows\fortran\test >> convert.log
echo ************************************************************ >> convert.log

  convert.js  %1\windows\fortran\test\flush1_fortran\flush1_fortran.dsp  %1\windows\fortran\test\flush1_fortran\flush1_fortran.vcproj  >> convert.log
  convert.js  %1\windows\fortran\test\flush1_fortrandll\flush1_fortrandll.dsp  %1\windows\fortran\test\flush1_fortrandll\flush1_fortrandll.vcproj  >> convert.log
  convert.js  %1\windows\fortran\test\flush2_fortran\flush2_fortran.dsp  %1\windows\fortran\test\flush2_fortran\flush2_fortran.vcproj  >> convert.log
  convert.js  %1\windows\fortran\test\flush2_fortrandll\flush2_fortrandll.dsp  %1\windows\fortran\test\flush2_fortrandll\flush2_fortrandll.vcproj  >> convert.log
  convert.js  %1\windows\fortran\test\libtest_cstubdll\libtest_cstubdll.dsp  %1\windows\fortran\test\libtest_cstubdll\libtest_cstubdll.vcproj  >> convert.log
  convert.js  %1\windows\fortran\test\libtest_fortran\libtest_fortran.dsp  %1\windows\fortran\test\libtest_fortran\libtest_fortran.vcproj  >> convert.log
  convert.js  %1\windows\fortran\test\libtest_fortrandll\libtest_fortrandll.dsp  %1\windows\fortran\test\libtest_fortrandll\libtest_fortrandll.vcproj  >> convert.log
  convert.js  %1\windows\fortran\test\testhdf5_fortran\testhdf5_fortran.dsp  %1\windows\fortran\test\testhdf5_fortran\testhdf5_fortran.vcproj  >> convert.log
  convert.js  %1\windows\fortran\test\testhdf5_fortrandll\testhdf5_fortrandll.dsp  %1\windows\fortran\test\testhdf5_fortrandll\testhdf5_fortrandll.vcproj  >> convert.log

echo ************************************************************ >> convert.log
echo Converting project files under %1\windows\hl\c++\test >> convert.log
echo ************************************************************ >> convert.log

convert.js %1\windows\hl\c++\test\hl_test_table_cpp\hl_test_table_cpp.dsp  %1\windows\hl\c++\test\hl_test_table_cpp\hl_test_table_cpp.vcproj  >> convert.log
  
echo ************************************************************ >> convert.log
echo Converting project files under %1\windows\hl\fortran\test >> convert.log
echo ************************************************************ >> convert.log

  convert.js %1\windows\hl\fortran\test\hl_test_image_fortran\hl_test_image_fortran.dsp  %1\windows\hl\fortran\test\hl_test_image_fortran\hl_test_image_fortran.vcproj  >> convert.log
  convert.js %1\windows\hl\fortran\test\hl_test_lite_fortran\hl_test_lite_fortran.dsp  %1\windows\hl\fortran\test\hl_test_lite_fortran\hl_test_lite_fortran.vcproj  >> convert.log
  convert.js %1\windows\hl\fortran\test\hl_test_table_fortran\hl_test_table_fortran.dsp  %1\windows\hl\fortran\test\hl_test_table_fortran\hl_test_table_fortran.vcproj  >> convert.log

echo ************************************************************ >> convert.log
echo Converting project files under %1\windows\hl\test >> convert.log
echo ************************************************************ >> convert.log

  convert.js %1\windows\hl\test\hl_test_image\hl_test_image.dsp  %1\windows\hl\test\hl_test_image\hl_test_image.vcproj  >> convert.log
  convert.js %1\windows\hl\test\hl_test_lite\hl_test_lite.dsp  %1\windows\hl\test\hl_test_lite\hl_test_lite.vcproj  >> convert.log
  convert.js %1\windows\hl\test\hl_test_table\hl_test_table.dsp  %1\windows\hl\test\hl_test_table\hl_test_table.vcproj  >> convert.log
  convert.js %1\windows\hl\test\hl_test_ds\hl_test_ds.dsp  %1\windows\hl\test\hl_test_ds\hl_test_ds.vcproj  >> convert.log
  convert.js %1\windows\hl\test\hl_test_packet\hl_test_packet.dsp  %1\windows\hl\test\hl_test_packet\hl_test_packet.vcproj  >> convert.log

echo ************************************************************ >> convert.log
echo Converting project files under %1\windows\misc >> convert.log
echo ************************************************************ >> convert.log

  convert.js %1\windows\misc\typegen\h5tinit\h5tinit.dsp  %1\windows\misc\typegen\h5tinit\h5tinit.vcproj  >> convert.log

echo ************************************************************ >> convert.log
echo Converting project files under %1\windows\tools >> convert.log
echo ************************************************************ >> convert.log

  convert.js %1\windows\tools\gifconvdll\gif2h5dll.dsp %1\windows\tools\gifconvdll\gif2h5dll.vcproj  >> convert.log
  convert.js %1\windows\tools\gifconvdll\h52gifdll.dsp %1\windows\tools\gifconvdll\h52gifdll.vcproj  >> convert.log
  convert.js %1\windows\tools\h5diffdll\h5diffdll.dsp %1\windows\tools\h5diffdll\h5diffdll.vcproj  >> convert.log
  convert.js %1\windows\tools\h5debug\h5debug.dsp %1\windows\tools\h5debug\h5debug.vcproj  >> convert.log
  convert.js %1\windows\tools\h5debugdll\h5debugdll.dsp %1\windows\tools\h5debugdll\h5debugdll.vcproj  >> convert.log
  convert.js %1\windows\tools\h5dumpdll\h5dumpdll.dsp %1\windows\tools\h5dumpdll\h5dumpdll.vcproj  >> convert.log
  convert.js %1\windows\tools\gifconv\gif2h5.dsp %1\windows\tools\gifconv\gif2h5.vcproj  >> convert.log
  convert.js %1\windows\tools\h5diffdll\h5diffdll.dsp %1\windows\tools\h5diffdll\h5diffdll.vcproj  >> convert.log
  convert.js %1\windows\tools\h5repackdll\h5repackdll.dsp %1\windows\tools\h5repackdll\h5repackdll.vcproj  >> convert.log
  convert.js %1\windows\tools\gifconv\h52gif.dsp %1\windows\tools\gifconv\h52gif.vcproj  >> convert.log
  convert.js %1\windows\tools\h5diff\h5diff.dsp %1\windows\tools\h5diff\h5diff.vcproj  >> convert.log
  convert.js %1\windows\tools\h5ls\h5ls.dsp %1\windows\tools\h5ls\h5ls.vcproj  >> convert.log
  convert.js %1\windows\tools\h5lsdll\h5lsdll.dsp %1\windows\tools\h5lsdll\h5lsdll.vcproj  >> convert.log
  convert.js %1\windows\tools\h5dump\h5dump.dsp %1\windows\tools\h5dump\h5dump.vcproj  >> convert.log
  convert.js %1\windows\tools\h5repack\h5repack.dsp %1\windows\tools\h5repack\h5repack.vcproj  >> convert.log
  convert.js %1\windows\tools\h5unjam\h5unjam.dsp %1\windows\tools\h5unjam\h5unjam.vcproj  >> convert.log
  convert.js %1\windows\tools\h5repart\h5repart.dsp %1\windows\tools\h5repart\h5repart.vcproj  >> convert.log
  convert.js %1\windows\tools\talign\talign.dsp %1\windows\tools\talign\talign.vcproj  >> convert.log
  convert.js %1\windows\tools\h5repartdll\h5repartdll.dsp %1\windows\tools\h5repartdll\h5repartdll.vcproj  >> convert.log
  convert.js %1\windows\tools\h5import\h5import.dsp %1\windows\tools\h5import\h5import.vcproj  >> convert.log
  convert.js %1\windows\tools\h5importdll\h5importdll.dsp %1\windows\tools\h5importdll\h5importdll.vcproj  >> convert.log
  convert.js %1\windows\tools\taligndll\taligndll.dsp %1\windows\tools\taligndll\taligndll.vcproj  >> convert.log
  convert.js %1\windows\tools\h5jam\h5jam.dsp %1\windows\tools\h5jam\h5jam.vcproj  >> convert.log
  convert.js %1\windows\tools\testfiles\h5difftst\h5difftst.dsp %1\windows\tools\testfiles\h5difftst\h5difftst.vcproj  >> convert.log
  convert.js %1\windows\tools\testfiles\h5dumptst\h5dumptst.dsp %1\windows\tools\testfiles\h5dumptst\h5dumptst.vcproj  >> convert.log
  convert.js %1\windows\tools\testfiles\h5importtst\h5importtst.dsp %1\windows\tools\testfiles\h5importtst\h5importtst.vcproj  >> convert.log
  convert.js %1\windows\tools\testfiles\h5jamtst\h5jamtst.dsp %1\windows\tools\testfiles\h5jamtst\h5jamtst.vcproj  >> convert.log
  convert.js %1\windows\tools\testfiles\h5repacktst\h5repacktst.dsp %1\windows\tools\testfiles\h5repacktst\h5repacktst.vcproj  >> convert.log
  convert.js %1\windows\tools\testfiles\testh5repack_detect_szip\testh5repack_detect_szip.dsp %1\windows\tools\testfiles\testh5repack_detect_szip\testh5repack_detect_szip.vcproj  >> convert.log
  convert.js %1\windows\tools\testfiles\h5repart_gentest\h5repart_gentest.dsp %1\windows\tools\testfiles\h5repart_gentest\h5repart_gentest.vcproj  >> convert.log
  convert.js %1\windows\tools\testfiles\h5reparttst\h5reparttst.dsp %1\windows\tools\testfiles\h5reparttst\h5reparttst.vcproj  >> convert.log  
  convert.js %1\windows\tools\toolslib\toolslib.dsp %1\windows\tools\toolslib\toolslib.vcproj  >> convert.log
  convert.js %1\windows\tools\toolslibD\toolslibD.dsp %1\windows\tools\toolslibD\toolslibD.vcproj  >> convert.log

