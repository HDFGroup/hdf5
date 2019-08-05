#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  set (HDF5_REFERENCE_FILES
      ${HDF5_TOOLS_DIR}/testfiles/charsets.ddl
      ${HDF5_TOOLS_DIR}/testfiles/file_space.ddl
      ${HDF5_TOOLS_DIR}/testfiles/filter_fail.ddl
      ${HDF5_TOOLS_DIR}/testfiles/non_existing.ddl
      ${HDF5_TOOLS_DIR}/testfiles/packedbits.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tall-1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tall-2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tall-2A.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tall-2A0.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tall-2B.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tall-3.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tall-4s.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tall-5s.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tall-6.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tall-7.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tall-7N.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tallfilters.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tarray1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tarray1_big.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tarray2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tarray3.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tarray4.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tarray5.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tarray6.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tarray7.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tarray8.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tattr-1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tattr-2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tattr-3.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tattr-4_be.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tattrcontents1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tattrcontents2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tattrintsize.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tattrreg.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tattrregR.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tbin1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tbin1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tbin2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tbin3.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tbin4.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tbinregR.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tbigdims.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tbitnopaque_be.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tbitnopaque_le.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tboot1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tboot2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tboot2A.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tboot2B.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tchar1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tchunked.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tcmpdattrintsize.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tcmpdintarray.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tcmpdints.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tcmpdintsize.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tcompound_complex2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tcomp-1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tcomp-2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tcomp-3.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tcomp-4.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tcompact.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tcontents.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tcontiguos.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tdatareg.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tdataregR.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tdeflate.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tdset-1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tdset-2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tdset-3s.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tempty.ddl
      ${HDF5_TOOLS_DIR}/testfiles/texceedsubstart.ddl
      ${HDF5_TOOLS_DIR}/testfiles/texceedsubcount.ddl
      ${HDF5_TOOLS_DIR}/testfiles/texceedsubstride.ddl
      ${HDF5_TOOLS_DIR}/testfiles/texceedsubblock.ddl
      ${HDF5_TOOLS_DIR}/testfiles/texternal.ddl
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc.ddl
      ${HDF5_TOOLS_DIR}/testfiles/textlinkfar.ddl
      ${HDF5_TOOLS_DIR}/testfiles/textlink.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tfamily.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tfill.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tfletcher32.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tfpformat.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tgroup-1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tgroup-2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tgrp_comments.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tgrpnullspace.ddl
      ${HDF5_TOOLS_DIR}/testfiles/thlink-1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/thlink-2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/thlink-3.ddl
      ${HDF5_TOOLS_DIR}/testfiles/thlink-4.ddl
      ${HDF5_TOOLS_DIR}/testfiles/thlink-5.ddl
      ${HDF5_TOOLS_DIR}/testfiles/thyperslab.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tindicesno.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tindicessub1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tindicessub2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tindicessub3.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tindicessub4.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tindicesyes.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tints4dims.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tints4dimsBlock2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tints4dimsBlockEq.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tints4dimsCount2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tints4dimsCountEq.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tints4dimsStride2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tintsattrs.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tintsnodata.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tlarge_objname.ddl
      #${HDF5_TOOLS_DIR}/testfiles/tldouble.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tlonglinks.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tloop-1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tmulti.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tmultifile.ddl
      #${HDF5_TOOLS_DIR}/testfiles/tqmarkfile.ddl
      #${HDF5_TOOLS_DIR}/testfiles/tstarfile.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tnamed_dtype_attr.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tnestcomp-1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tnestedcmpddt.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tnbit.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tnoattrdata.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tnoattrddl.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tnodata.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tnoddl.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tnoddlfile.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tno-subset.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tnullspace.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tordergr1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tordergr2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tordergr3.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tordergr4.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tordergr5.ddl
      ${HDF5_TOOLS_DIR}/testfiles/torderattr1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/torderattr2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/torderattr3.ddl
      ${HDF5_TOOLS_DIR}/testfiles/torderattr4.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tordercontents1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tordercontents2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/torderlinks1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/torderlinks2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tperror.ddl
      ${HDF5_TOOLS_DIR}/testfiles/trawdatafile.ddl
      ${HDF5_TOOLS_DIR}/testfiles/trawssetfile.ddl
      ${HDF5_TOOLS_DIR}/testfiles/treadfilter.ddl
      ${HDF5_TOOLS_DIR}/testfiles/treadintfilter.ddl
      ${HDF5_TOOLS_DIR}/testfiles/treference.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tsaf.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tscalarattrintsize.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tscalarintattrsize.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tscalarintsize.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tscalarstring.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tscaleoffset.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tshuffle.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tslink-1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tslink-2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tslink-D.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tsplit_file.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tstr-1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tstr-2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tstring.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tstring2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tstringe.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tszip.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tudfilter.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tudlink-1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tudlink-2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tuserfilter.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes1.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes2.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes3.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes4.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes5.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tvlenstr_array.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tvlstr.ddl
      ${HDF5_TOOLS_DIR}/testfiles/tvms.ddl
      ${HDF5_TOOLS_DIR}/testfiles/twidedisplay.ddl
      ${HDF5_TOOLS_DIR}/testfiles/twithddlfile.ddl
      ${HDF5_TOOLS_DIR}/testfiles/h5dump-help.txt
      ${HDF5_TOOLS_DIR}/testfiles/out3.h5import
      ${HDF5_TOOLS_DIR}/testfiles/zerodim.ddl
  )
  set (HDF5_N_REFERENCE_FILES
      tall-3
      tattr-2
      tcomp-2
      thlink-4
      thlink-5
      tslink-2
  )
  set (HDF5_REFERENCE_EXP_FILES
      tall-6.exp
      tnoddlfile.exp
      trawdatafile.exp
      trawssetfile.exp
      tstr2bin2.exp
      tstr2bin6.exp
      twithddl.exp
      twithddlfile.exp
  )
  set (HDF5_REFERENCE_TEST_FILES
      ${HDF5_TOOLS_DIR}/testfiles/charsets.h5
      ${HDF5_TOOLS_DIR}/testfiles/file_space.h5
      ${HDF5_TOOLS_DIR}/testfiles/filter_fail.h5
      ${HDF5_TOOLS_DIR}/testfiles/packedbits.h5
      ${HDF5_TOOLS_DIR}/testfiles/taindices.h5
      ${HDF5_TOOLS_DIR}/testfiles/tall.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray1.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray1_big.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray2.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray3.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray4.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray5.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray6.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray7.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray8.h5
      ${HDF5_TOOLS_DIR}/testfiles/tattr.h5
      ${HDF5_TOOLS_DIR}/testfiles/tattr2.h5
      ${HDF5_TOOLS_DIR}/testfiles/tattr4_be.h5
      ${HDF5_TOOLS_DIR}/testfiles/tattrintsize.h5
      ${HDF5_TOOLS_DIR}/testfiles/tattrreg.h5
      ${HDF5_TOOLS_DIR}/testfiles/tbigdims.h5
      ${HDF5_TOOLS_DIR}/testfiles/tbinary.h5
      ${HDF5_TOOLS_DIR}/testfiles/tbitnopaque.h5
      ${HDF5_TOOLS_DIR}/testfiles/tchar.h5
      ${HDF5_TOOLS_DIR}/testfiles/tcmpdattrintsize.h5
      ${HDF5_TOOLS_DIR}/testfiles/tcmpdintarray.h5
      ${HDF5_TOOLS_DIR}/testfiles/tcmpdints.h5
      ${HDF5_TOOLS_DIR}/testfiles/tcmpdintsize.h5
      ${HDF5_TOOLS_DIR}/testfiles/tcompound.h5
      ${HDF5_TOOLS_DIR}/testfiles/tcompound_complex.h5
      ${HDF5_TOOLS_DIR}/testfiles/tcompound_complex2.h5
      ${HDF5_TOOLS_DIR}/testfiles/tdatareg.h5
      ${HDF5_TOOLS_DIR}/testfiles/tdset.h5
      ${HDF5_TOOLS_DIR}/testfiles/tempty.h5
      ${HDF5_TOOLS_DIR}/testfiles/tsoftlinks.h5
      ${HDF5_TOOLS_DIR}/testfiles/textlinkfar.h5
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc.h5
      ${HDF5_TOOLS_DIR}/testfiles/textlinktar.h5
      ${HDF5_TOOLS_DIR}/testfiles/textlink.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfamily00000.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfamily00001.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfamily00002.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfamily00003.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfamily00004.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfamily00005.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfamily00006.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfamily00007.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfamily00008.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfamily00009.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfamily00010.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfcontents1.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfcontents2.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfilters.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfpformat.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfvalues.h5
      ${HDF5_TOOLS_DIR}/testfiles/tgroup.h5
      ${HDF5_TOOLS_DIR}/testfiles/tgrp_comments.h5
      ${HDF5_TOOLS_DIR}/testfiles/tgrpnullspace.h5
      ${HDF5_TOOLS_DIR}/testfiles/thlink.h5
      ${HDF5_TOOLS_DIR}/testfiles/thyperslab.h5
      ${HDF5_TOOLS_DIR}/testfiles/tints4dims.h5
      ${HDF5_TOOLS_DIR}/testfiles/tintsattrs.h5
      ${HDF5_TOOLS_DIR}/testfiles/tintsnodata.h5
      ${HDF5_TOOLS_DIR}/testfiles/tlarge_objname.h5
      #${HDF5_TOOLS_DIR}/testfiles/tldouble.h5
      ${HDF5_TOOLS_DIR}/testfiles/tlonglinks.h5
      ${HDF5_TOOLS_DIR}/testfiles/tloop.h5
      ${HDF5_TOOLS_DIR}/testfiles/tmulti-b.h5
      ${HDF5_TOOLS_DIR}/testfiles/tmulti-g.h5
      ${HDF5_TOOLS_DIR}/testfiles/tmulti-l.h5
      ${HDF5_TOOLS_DIR}/testfiles/tmulti-o.h5
      ${HDF5_TOOLS_DIR}/testfiles/tmulti-r.h5
      ${HDF5_TOOLS_DIR}/testfiles/tmulti-s.h5
      ${HDF5_TOOLS_DIR}/testfiles/tnamed_dtype_attr.h5
      ${HDF5_TOOLS_DIR}/testfiles/tnestedcomp.h5
      ${HDF5_TOOLS_DIR}/testfiles/tnestedcmpddt.h5
      ${HDF5_TOOLS_DIR}/testfiles/tno-subset.h5
      ${HDF5_TOOLS_DIR}/testfiles/tnullspace.h5
      ${HDF5_TOOLS_DIR}/testfiles/torderattr.h5
      ${HDF5_TOOLS_DIR}/testfiles/tordergr.h5
      ${HDF5_TOOLS_DIR}/testfiles/tsaf.h5
      ${HDF5_TOOLS_DIR}/testfiles/tscalarattrintsize.h5
      ${HDF5_TOOLS_DIR}/testfiles/tscalarintattrsize.h5
      ${HDF5_TOOLS_DIR}/testfiles/tscalarintsize.h5
      ${HDF5_TOOLS_DIR}/testfiles/tscalarstring.h5
      ${HDF5_TOOLS_DIR}/testfiles/tslink.h5
      ${HDF5_TOOLS_DIR}/testfiles/tsplit_file-m.h5
      ${HDF5_TOOLS_DIR}/testfiles/tsplit_file-r.h5
      ${HDF5_TOOLS_DIR}/testfiles/tstr.h5
      ${HDF5_TOOLS_DIR}/testfiles/tstr2.h5
      ${HDF5_TOOLS_DIR}/testfiles/tstr3.h5
      ${HDF5_TOOLS_DIR}/testfiles/tudfilter.h5
      ${HDF5_TOOLS_DIR}/testfiles/tudlink.h5
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes1.h5
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes2.h5
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes3.h5
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes4.h5
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes5.h5
      ${HDF5_TOOLS_DIR}/testfiles/tvlenstr_array.h5
      ${HDF5_TOOLS_DIR}/testfiles/tvlstr.h5
      ${HDF5_TOOLS_DIR}/testfiles/tvms.h5
      ${HDF5_TOOLS_DIR}/testfiles/t128bit_float.h5
      ${HDF5_TOOLS_DIR}/testfiles/zerodim.h5
  )
  set (HDF5_ERROR_REFERENCE_TEST_FILES
      ${PROJECT_SOURCE_DIR}/errfiles/filter_fail.err
      ${PROJECT_SOURCE_DIR}/errfiles/non_existing.err
      ${PROJECT_SOURCE_DIR}/errfiles/tall-1.err
      ${PROJECT_SOURCE_DIR}/errfiles/tall-2A.err
      ${PROJECT_SOURCE_DIR}/errfiles/tall-2A0.err
      ${PROJECT_SOURCE_DIR}/errfiles/tall-2B.err
      ${PROJECT_SOURCE_DIR}/errfiles/tarray1_big.err
      ${PROJECT_SOURCE_DIR}/errfiles/tattrregR.err
      ${PROJECT_SOURCE_DIR}/errfiles/tattr-3.err
      ${PROJECT_SOURCE_DIR}/errfiles/tcomp-3.err
      ${PROJECT_SOURCE_DIR}/errfiles/tdataregR.err
      ${PROJECT_SOURCE_DIR}/errfiles/tdset-2.err
      ${PROJECT_SOURCE_DIR}/errfiles/texceedsubblock.err
      ${PROJECT_SOURCE_DIR}/errfiles/texceedsubcount.err
      ${PROJECT_SOURCE_DIR}/errfiles/texceedsubstart.err
      ${PROJECT_SOURCE_DIR}/errfiles/texceedsubstride.err
      ${PROJECT_SOURCE_DIR}/errfiles/textlink.err
      ${PROJECT_SOURCE_DIR}/errfiles/textlinkfar.err
      ${PROJECT_SOURCE_DIR}/errfiles/textlinksrc.err
      ${PROJECT_SOURCE_DIR}/errfiles/torderlinks1.err
      ${PROJECT_SOURCE_DIR}/errfiles/torderlinks2.err
      ${PROJECT_SOURCE_DIR}/errfiles/tgroup-2.err
      ${PROJECT_SOURCE_DIR}/errfiles/tperror.err
      ${PROJECT_SOURCE_DIR}/errfiles/tslink-D.err
  )

  # make test dir
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")

  #
  # copy test files from source dir to test dir
  #
  foreach (tst_h5_file ${HDF5_REFERENCE_TEST_FILES})
    get_filename_component (fname "${tst_h5_file}" NAME)
    HDFTEST_COPY_FILE("${tst_h5_file}" "${PROJECT_BINARY_DIR}/testfiles/std/${fname}" "h5dump_std_files")
  endforeach ()

  foreach (tst_exp_file ${HDF5_REFERENCE_EXP_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/${tst_exp_file}" "${PROJECT_BINARY_DIR}/testfiles/std/${tst_exp_file}" "h5dump_std_files")
  endforeach ()

  foreach (tst_other_file ${HDF5_REFERENCE_FILES})
    get_filename_component (fname "${tst_other_file}" NAME)
    HDFTEST_COPY_FILE("${tst_other_file}" "${PROJECT_BINARY_DIR}/testfiles/std/${fname}" "h5dump_std_files")
  endforeach ()
  foreach (tst_h5N_file ${HDF5_N_REFERENCE_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/${tst_h5N_file}.ddl" "${PROJECT_BINARY_DIR}/testfiles/std/${tst_h5N_file}-N.ddl" "h5dump_std_files")
  endforeach ()

  foreach (tst_error_file ${HDF5_ERROR_REFERENCE_TEST_FILES})
    get_filename_component (fname "${tst_error_file}" NAME)
    HDFTEST_COPY_FILE("${tst_error_file}" "${PROJECT_BINARY_DIR}/testfiles/std/${fname}" "h5dump_std_files")
  endforeach ()

  # --------------------------------------------------------------------
  # Special file handling
  # --------------------------------------------------------------------
  HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/tbin1.ddl" "${PROJECT_BINARY_DIR}/testfiles/std/tbin1LE.ddl" "h5dump_std_files")

  if (WIN32 OR MINGW)
    configure_file(${HDF5_TOOLS_DIR}/testfiles/tbinregR.exp ${PROJECT_BINARY_DIR}/testfiles/std/tbinregR.exp NEWLINE_STYLE CRLF)
    #file (READ ${HDF5_TOOLS_DIR}/testfiles/tbinregR.exp TEST_STREAM)
    #file (WRITE ${PROJECT_BINARY_DIR}/testfiles/std/tbinregR.exp "${TEST_STREAM}")
  else ()
    HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/tbinregR.exp" "${PROJECT_BINARY_DIR}/testfiles/std/tbinregR.exp" "h5dump_std_files")
  endif ()
  add_custom_target(h5dump_std_files ALL COMMENT "Copying files needed by h5dump_std tests" DEPENDS ${h5dump_std_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  if (NOT BUILD_SHARED_LIBS)
    set (tgt_ext "")
  else ()
    set (tgt_ext "-shared")
  endif ()

  macro (ADD_HELP_TEST testname resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5DUMP-${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump${tgt_ext}> ${ARGN})
      set_tests_properties (H5DUMP-${testname} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      if (last_test)
        set_tests_properties (H5DUMP-${testname} PROPERTIES DEPENDS ${last_test})
      endif ()
      set (last_test "H5DUMP-${testname}")
    else ()
      add_test (
          NAME H5DUMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=h5dump-${testname}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=h5dump-${testname}.txt"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      if (last_test)
        set_tests_properties (H5DUMP-${testname} PROPERTIES DEPENDS ${last_test})
      endif ()
    endif ()
  endmacro ()

  macro (ADD_SKIP_H5_TEST skipresultfile skipresultcode testtype)
    if ("${testtype}" STREQUAL "SKIP")
      if (NOT HDF5_ENABLE_USING_MEMCHECKER)
        add_test (
            NAME H5DUMP-${skipresultfile}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${skipresultfile} ${ARGN}"
        )
        set_property(TEST H5DUMP-${skipresultfile} PROPERTY DISABLED)
      endif ()
    else ()
      ADD_H5_TEST (${skipresultfile} ${skipresultcode} ${ARGN})
    endif ()
  endmacro ()

  macro (ADD_H5_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5DUMP-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump${tgt_ext}> ${ARGN})
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      if (${resultcode})
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
      if (last_test)
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES DEPENDS ${last_test})
      endif ()
    else ()
      add_test (
          NAME H5DUMP-${resultfile}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ${resultfile}.bin
      )
      set_tests_properties (H5DUMP-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES DEPENDS "H5DUMP-${resultfile}-clear-objects")
    endif ()
  endmacro ()

  macro (ADD_H5_TEST_N resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5DUMP-N-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump${tgt_ext}> ${ARGN})
      set_tests_properties (H5DUMP-N-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      if (${resultcode})
        set_tests_properties (H5DUMP-N-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
      if (last_test)
        set_tests_properties (H5DUMP-N-${resultfile} PROPERTIES DEPENDS ${last_test})
      endif ()
    else ()
      add_test (
          NAME H5DUMP-N-${resultfile}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ${resultfile}-N.bin
      )
      set_tests_properties (H5DUMP-N-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      add_test (
          NAME H5DUMP-N-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}-N.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}-N.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-N-${resultfile} PROPERTIES DEPENDS "H5DUMP-N-${resultfile}-clear-objects")
    endif ()
  endmacro ()

  macro (ADD_H5_TEST_EXPORT resultfile targetfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5DUMP-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump${tgt_ext}> ${ARGN} ${resultfile}.txt ${targetfile})
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      if (${resultcode})
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
      if (last_test)
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES DEPENDS ${last_test})
      endif ()
    else ()
      add_test (
          NAME H5DUMP-${resultfile}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ${resultfile}.txt
      )
      set_tests_properties (H5DUMP-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=${ARGN};${resultfile}.txt;${targetfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES DEPENDS "H5DUMP-${resultfile}-clear-objects")
      if(NOT CMAKE_VERSION VERSION_LESS "3.14.0")
        add_test (
            NAME H5DUMP-${resultfile}-output-cmp
            COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol ${resultfile}.txt ${resultfile}.exp
        )
        set_tests_properties (H5DUMP-${resultfile}-output-cmp PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
        set_tests_properties (H5DUMP-${resultfile}-output-cmp PROPERTIES DEPENDS H5DUMP-${resultfile})
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5_TEST_EXPORT_DDL resultfile targetfile resultcode ddlfile)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5DUMP-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump${tgt_ext}> --ddl=${ddlfile}.txt ${ARGN} ${resultfile}.txt ${targetfile})
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      if (${resultcode})
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
      if (last_test)
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES DEPENDS ${last_test})
      endif ()
    else ()
      add_test (
          NAME H5DUMP-${resultfile}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              ${ddlfile}.txt
              ${resultfile}.txt
      )
      set_tests_properties (H5DUMP-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=--ddl=${ddlfile}.txt;${ARGN};${resultfile}.txt;${targetfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES DEPENDS "H5DUMP-${resultfile}-clear-objects")
      if(NOT CMAKE_VERSION VERSION_LESS "3.14.0")
        add_test (
            NAME H5DUMP-${resultfile}-output-cmp
            COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol ${resultfile}.txt ${resultfile}.exp
        )
        set_tests_properties (H5DUMP-${resultfile}-output-cmp PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
        set_tests_properties (H5DUMP-${resultfile}-output-cmp PROPERTIES DEPENDS H5DUMP-${resultfile})
        add_test (
            NAME H5DUMP-${resultfile}-output-cmp-ddl
            COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol ${ddlfile}.txt ${ddlfile}.exp
        )
        set_tests_properties (H5DUMP-${resultfile}-output-cmp-ddl PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
        set_tests_properties (H5DUMP-${resultfile}-output-cmp-ddl PROPERTIES DEPENDS H5DUMP-${resultfile}-output-cmp)
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5_EXPORT_TEST resultfile targetfile resultcode)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5DUMP-output-${resultfile}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ${resultfile}.txt
      )
      set_tests_properties (H5DUMP-output-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      add_test (
          NAME H5DUMP-output-${resultfile}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump${tgt_ext}> ${ARGN} ${resultfile}.txt ${targetfile}
      )
      set_tests_properties (H5DUMP-output-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      set_tests_properties (H5DUMP-output-${resultfile} PROPERTIES DEPENDS H5DUMP-output-${resultfile}-clear-objects)
      if(NOT CMAKE_VERSION VERSION_LESS "3.14.0")
        add_test (
            NAME H5DUMP-output-cmp-${resultfile}
            COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol ${resultfile}.txt ${resultfile}.exp
        )
        set_tests_properties (H5DUMP-output-cmp-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
        set_tests_properties (H5DUMP-output-cmp-${resultfile} PROPERTIES DEPENDS H5DUMP-output-${resultfile})
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5_MASK_TEST resultfile resultcode)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -D "TEST_MASK_ERROR=true"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
  endmacro ()

  macro (ADD_H5_GREP_TEST resultfile resultcode result_check)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${result_check}"
              -P "${HDF_RESOURCES_EXT_DIR}/grepTest.cmake"
      )
    endif ()
  endmacro ()

  macro (ADD_H5ERR_MASK_TEST resultfile resultcode result_errcheck)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -D "TEST_ERRREF=${result_errcheck}"
              -P "${HDF_RESOURCES_EXT_DIR}/grepTest.cmake"
      )
    endif ()
  endmacro ()

  macro (ADD_H5ERR_MASK_ENV_TEST resultfile resultcode result_errcheck envvar envval)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -D "TEST_ERRREF=${result_errcheck}"
              -D "TEST_ENV_VAR:STRING=${envvar}"
              -D "TEST_ENV_VALUE:STRING=${envval}"
              -P "${HDF_RESOURCES_EXT_DIR}/grepTest.cmake"
      )
    endif ()
  endmacro ()

  macro (ADD_H5_TEST_IMPORT conffile resultfile testfile resultcode)
    # If using memchecker add tests without using scripts
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5DUMP-IMPORT-${resultfile}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              ${resultfile}.bin
              ${resultfile}.h5
      )
      set_tests_properties (H5DUMP-IMPORT-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      add_test (
          NAME H5DUMP-IMPORT-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=${ARGN};-o;${resultfile}.bin;${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${conffile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${conffile}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-IMPORT-${resultfile} PROPERTIES DEPENDS "H5DUMP-IMPORT-${resultfile}-clear-objects")
      add_test (NAME H5DUMP-IMPORT-h5import-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5import> ${resultfile}.bin -c ${conffile}.out -o ${resultfile}.h5)
      set_tests_properties (H5DUMP-IMPORT-h5import-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      set_tests_properties (H5DUMP-IMPORT-h5import-${resultfile} PROPERTIES DEPENDS H5DUMP-IMPORT-${resultfile})
      add_test (NAME H5DUMP-IMPORT-h5diff-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> ${testfile} ${resultfile}.h5 /integer /integer)
      set_tests_properties (H5DUMP-IMPORT-h5diff-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      set_tests_properties (H5DUMP-IMPORT-h5diff-${resultfile} PROPERTIES DEPENDS H5DUMP-IMPORT-h5import-${resultfile})
    endif ()
  endmacro ()

  macro (ADD_H5_UD_TEST testname resultcode resultfile)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5DUMP_UD-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
  endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  ADD_HELP_TEST(help 0 -h)

  # test data output redirection
  #ADD_H5_TEST (tnoddl 0 --enable-error-stack -O -y packedbits.h5)
  ADD_H5_TEST (tnoddl 0 --enable-error-stack --ddl -y packedbits.h5)
  #ADD_H5_TEST (tnodata 0 --enable-error-stack -o packedbits.h5)
  ADD_H5_TEST (tnodata 0 --enable-error-stack --output packedbits.h5)
  ADD_H5_TEST (tnoattrddl 0 --enable-error-stack -O -y tattr.h5)
  ADD_H5_TEST (tnoattrdata 0 --enable-error-stack -A -o tattr.h5)
  ADD_H5_TEST_EXPORT (trawdatafile packedbits.h5 0 --enable-error-stack -y -o)
  ADD_H5_TEST_EXPORT (tnoddlfile packedbits.h5 0 --enable-error-stack -O -y -o)
  ADD_H5_TEST_EXPORT (trawssetfile tdset.h5 0 --enable-error-stack -d "/dset1[1,1;;;]" -y -o)

  ADD_H5_TEST_EXPORT_DDL (twithddlfile packedbits.h5 0 twithddl --enable-error-stack --ddl=twithddl.txt -y -o)

  # test for maximum display datasets
  ADD_H5_TEST (twidedisplay 0 --enable-error-stack -w0 packedbits.h5)

  # test for unwritten datasets
  ADD_H5_TEST (tintsnodata 0 --enable-error-stack -p tintsnodata.h5)

  # test for signed/unsigned datasets
  ADD_H5_TEST (packedbits 0 --enable-error-stack packedbits.h5)
  # test for compound signed/unsigned datasets
  ADD_H5_TEST (tcmpdintarray 0 --enable-error-stack tcmpdintarray.h5)
  ADD_H5_TEST (tcmpdints 0 --enable-error-stack tcmpdints.h5)
  ADD_H5_TEST (tcmpdintsize 0 --enable-error-stack tcmpdintsize.h5)
  # test for signed/unsigned scalar datasets
  ADD_H5_TEST (tscalarintsize 0 --enable-error-stack tscalarintsize.h5)
  # test for signed/unsigned attributes
  ADD_H5_TEST (tattrintsize 0 --enable-error-stack tattrintsize.h5)
  # test for compound signed/unsigned attributes
  ADD_H5_TEST (tcmpdattrintsize 0 --enable-error-stack tcmpdattrintsize.h5)
  # test for signed/unsigned scalar attributes
  ADD_H5_TEST (tscalarattrintsize 0 --enable-error-stack tscalarattrintsize.h5)
  # test for string scalar dataset and attribute
  ADD_H5_TEST (tscalarstring 0 --enable-error-stack tscalarstring.h5)
  # test for signed/unsigned scalar datasets with attributes
  ADD_H5_TEST (tscalarintattrsize 0 --enable-error-stack tscalarintattrsize.h5)
  # test for signed/unsigned datasets attributes
  ADD_H5_TEST (tintsattrs 0 --enable-error-stack tintsattrs.h5)
  # test for displaying groups
  ADD_H5_TEST (tgroup-1 0 --enable-error-stack tgroup.h5)
  # test for displaying the selected groups
  ADD_H5ERR_MASK_TEST (tgroup-2 1 "h5dump error: unable to open group \"/y\"" --enable-error-stack --group=/g2 --group / -g /y tgroup.h5)

  # test for displaying simple space datasets
  ADD_H5_TEST (tdset-1 0 --enable-error-stack tdset.h5)
  # test for displaying selected datasets
  ADD_H5ERR_MASK_TEST (tdset-2 1 "h5dump error: unable to get link info from \"dset3\"" --enable-error-stack -H -d dset1 -d /dset2 --dataset=dset3 tdset.h5)

  # test for displaying attributes
  ADD_H5_TEST (tattr-1 0 --enable-error-stack tattr.h5)
  # test for displaying the selected attributes of string type and scalar space
  ADD_H5_TEST (tattr-2 0 --enable-error-stack -a /\\\\/attr1 --attribute /attr4 --attribute=/attr5 tattr.h5)
  ADD_H5_TEST_N (tattr-2 0 --enable-error-stack -N /\\\\/attr1 --any_path /attr4 --any_path=/attr5 tattr.h5)
  # test for header and error messages
  ADD_H5ERR_MASK_TEST (tattr-3 1 "h5dump error: unable to open attribute \"attr\"" --enable-error-stack --header -a /attr2 --attribute=/attr tattr.h5)
  # test for displaying at least 9 attributes on root from a be machine
  ADD_H5_TEST (tattr-4_be 0 --enable-error-stack tattr4_be.h5)
  # test for displaying attributes in shared datatype (also in group and dataset)
  ADD_H5_TEST (tnamed_dtype_attr 0 --enable-error-stack tnamed_dtype_attr.h5)

  # test for displaying soft links and user-defined links
  ADD_H5_TEST (tslink-1 0 --enable-error-stack tslink.h5)
  ADD_H5_TEST (tudlink-1 0 --enable-error-stack tudlink.h5)
  # test for displaying the selected link
  ADD_H5_TEST (tslink-2 0 --enable-error-stack -l slink2 tslink.h5)
  ADD_H5_TEST_N (tslink-2 0 --enable-error-stack -N slink2 tslink.h5)
  ADD_H5_TEST (tudlink-2 0 --enable-error-stack -l udlink2 tudlink.h5)
  # test for displaying dangling soft links
  ADD_H5ERR_MASK_TEST (tslink-D 0 "component not found" --enable-error-stack -d /slink1 tslink.h5)

  # tests for hard links
  ADD_H5_TEST (thlink-1 0 --enable-error-stack thlink.h5)
  ADD_H5_TEST (thlink-2 0 --enable-error-stack -d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 thlink.h5)
  ADD_H5_TEST (thlink-3 0 --enable-error-stack -d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 thlink.h5)
  ADD_H5_TEST (thlink-4 0 --enable-error-stack -g /g1 thlink.h5)
  ADD_H5_TEST_N (thlink-4 0 --enable-error-stack -N /g1 thlink.h5)
  ADD_H5_TEST (thlink-5 0 --enable-error-stack -d /dset1 -g /g2 -d /g1/dset2 thlink.h5)
  ADD_H5_TEST_N (thlink-5 0 --enable-error-stack -N /dset1 -N /g2 -N /g1/dset2 thlink.h5)

  # tests for compound data types
  ADD_H5_TEST (tcomp-1 0 --enable-error-stack tcompound.h5)
  # test for named data types
  ADD_H5_TEST (tcomp-2 0 --enable-error-stack -t /type1 --datatype /type2 --datatype=/group1/type3 tcompound.h5)
  ADD_H5_TEST_N (tcomp-2 0 --enable-error-stack -N /type1 --any_path /type2 --any_path=/group1/type3 tcompound.h5)
  # test for unamed type
  ADD_H5ERR_MASK_TEST (tcomp-3 0 "object '#6632' doesn't exist" "--enable-error-stack;-t;/#6632;-g;/group2;tcompound.h5")
  # test complicated compound datatype
  ADD_H5_TEST (tcomp-4 0 --enable-error-stack tcompound_complex.h5)
  ADD_H5_TEST (tcompound_complex2 0 --enable-error-stack tcompound_complex2.h5)
  # tests for bitfields and opaque data types
  if (H5_WORDS_BIGENDIAN)
    ADD_H5_TEST (tbitnopaque_be 0 --enable-error-stack tbitnopaque.h5)
  else ()
    ADD_H5_TEST (tbitnopaque_le 0 --enable-error-stack tbitnopaque.h5)
  endif ()

  #test for the nested compound type
  ADD_H5_TEST (tnestcomp-1 0 --enable-error-stack tnestedcomp.h5)
  ADD_H5_TEST (tnestedcmpddt 0 --enable-error-stack tnestedcmpddt.h5)

  # test for options
  ADD_H5ERR_MASK_TEST (tall-1 0 "unable to open external file, external link file name = 'somefile'" --enable-error-stack tall.h5)
  ADD_H5_TEST (tall-2 0 --enable-error-stack --header -g /g1/g1.1 -a attr2 tall.h5)
  ADD_H5_TEST (tall-3 0 --enable-error-stack -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5)
  ADD_H5_TEST_N (tall-3 0 --enable-error-stack -N /g2/dset2.1 -N /g1/g1.2/g1.2.1/slink tall.h5)
  ADD_H5_TEST (tall-7 0 --enable-error-stack -a attr1 tall.h5)
  ADD_H5_TEST (tall-7N 0 --enable-error-stack -N attr1 tall.h5)

  # test for loop detection
  ADD_H5_TEST (tloop-1 0 --enable-error-stack tloop.h5)

  # test for string
  ADD_H5_TEST (tstr-1 0 --enable-error-stack tstr.h5)
  ADD_H5_TEST (tstr-2 0 --enable-error-stack tstr2.h5)

  # test for file created by Lib SAF team
  ADD_H5_TEST (tsaf 0 --enable-error-stack tsaf.h5)

  # test for file with variable length data
  ADD_H5_TEST (tvldtypes1 0 --enable-error-stack tvldtypes1.h5)
  ADD_H5_TEST (tvldtypes2 0 --enable-error-stack tvldtypes2.h5)
  ADD_H5_TEST (tvldtypes3 0 --enable-error-stack tvldtypes3.h5)
  ADD_H5_TEST (tvldtypes4 0 --enable-error-stack tvldtypes4.h5)
  ADD_H5_TEST (tvldtypes5 0 --enable-error-stack tvldtypes5.h5)

  #test for file with variable length string data
  ADD_H5_TEST (tvlstr 0 --enable-error-stack tvlstr.h5)
  ADD_H5_TEST (tvlenstr_array 0 --enable-error-stack tvlenstr_array.h5)

  # test for files with array data
  ADD_H5_TEST (tarray1 0 --enable-error-stack tarray1.h5)
  # # added for bug# 2092 - tarray1_big.h5
  ADD_H5ERR_MASK_TEST (tarray1_big 0 "Undefined reference pointer" --enable-error-stack -R tarray1_big.h5)
  ADD_H5_TEST (tarray2 0 --enable-error-stack tarray2.h5)
  ADD_H5_TEST (tarray3 0 --enable-error-stack tarray3.h5)
  ADD_H5_TEST (tarray4 0 --enable-error-stack tarray4.h5)
  ADD_H5_TEST (tarray5 0 --enable-error-stack tarray5.h5)
  ADD_H5_TEST (tarray6 0 --enable-error-stack tarray6.h5)
  ADD_H5_TEST (tarray7 0 --enable-error-stack tarray7.h5)
  ADD_H5_TEST (tarray8 0 --enable-error-stack tarray8.h5)

  # test for wildcards in filename (does not work with cmake)
  #ADD_H5_MASK_TEST (tstarfile 0 --enable-error-stack -H -d Dataset1 tarr*.h5)
  #ADD_H5_MASK_TEST (tqmarkfile 0 --enable-error-stack -H -d Dataset1 tarray?.h5)
  ADD_H5_TEST (tmultifile 0 --enable-error-stack -H -d Dataset1 tarray2.h5 tarray3.h5 tarray4.h5 tarray5.h5 tarray6.h5 tarray7.h5)

  # test for files with empty data
  ADD_H5_TEST (tempty 0 --enable-error-stack tempty.h5)

  # test for files with groups that have comments
  ADD_H5_TEST (tgrp_comments 0 --enable-error-stack tgrp_comments.h5)

  # test the --filedriver flag
  ADD_H5_TEST (tsplit_file 0 --enable-error-stack --filedriver=split tsplit_file)
  ADD_H5_TEST (tfamily 0 --enable-error-stack --filedriver=family tfamily%05d.h5)
  ADD_H5_TEST (tmulti 0 --enable-error-stack --filedriver=multi tmulti)

  # test for files with group names which reach > 1024 bytes in size
  ADD_H5_TEST (tlarge_objname 0 --enable-error-stack -w157 tlarge_objname.h5)

  # test '-A' to suppress data but print attr's
  ADD_H5ERR_MASK_TEST (tall-2A 0 "unable to open external file, external link file name = 'somefile'" --enable-error-stack -A tall.h5)

  # test '-A' to suppress attr's but print data
  ADD_H5ERR_MASK_TEST (tall-2A0 0 "unable to open external file, external link file name = 'somefile'" --enable-error-stack -A 0 tall.h5)

  # test '-r' to print attributes in ASCII instead of decimal
  ADD_H5ERR_MASK_TEST (tall-2B 0 "unable to open external file, external link file name = 'somefile'" --enable-error-stack -A -r tall.h5)

  # test Subsetting
  ADD_H5_TEST (tall-4s 0 --enable-error-stack --dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 tall.h5)
  ADD_H5_TEST (tall-5s 0 --enable-error-stack -d "/g1/g1.1/dset1.1.2[0;2;10;]" tall.h5)
  ADD_H5_TEST (tdset-3s 0 --enable-error-stack -d "/dset1[1,1;;;]" tdset.h5)
  ADD_H5_TEST (tno-subset 0 --enable-error-stack --no-compact-subset -d "AHFINDERDIRECT::ah_centroid_t[0] it=0 tl=0" tno-subset.h5)

  ADD_H5_TEST (tints4dimsCount2 0 --enable-error-stack -d FourDimInts -s 0,0,0,0 -c 2,2,2,2 tints4dims.h5)
  ADD_H5_TEST (tints4dimsBlock2 0 --enable-error-stack -d FourDimInts -s 0,0,0,0 -c 1,1,1,1 -k 2,2,2,2 tints4dims.h5)
  ADD_H5_TEST (tints4dimsStride2 0 --enable-error-stack -d FourDimInts -s 0,0,0,0 -S 2,2,2,2 -c 2,2,2,2 tints4dims.h5)
  ADD_H5_TEST (tints4dimsCountEq 0 --enable-error-stack -d FourDimInts -s 0,0,0,0 -S 2,2,1,1 -k 1,2,1,1 -c 2,2,4,4 tints4dims.h5)
  ADD_H5_TEST (tints4dimsBlockEq 0 --enable-error-stack -d FourDimInts -s 0,0,0,0 -S 2,2,1,1 -c 2,2,1,1 -k 1,2,4,4 tints4dims.h5)

  # test printing characters in ASCII instead of decimal
  ADD_H5_TEST (tchar1 0 --enable-error-stack -r tchar.h5)

  # test datatypes in ASCII and UTF8
  ADD_H5_TEST (charsets 0 --enable-error-stack charsets.h5)

  # rev. 2004
  # tests for super block
  ADD_H5_TEST (tboot1 0 --enable-error-stack -H -B -d dset tfcontents1.h5)
  ADD_H5_TEST (tboot2 0 --enable-error-stack -B tfcontents2.h5)
  ADD_H5_TEST (tboot2A 0 --enable-error-stack --boot-block tfcontents2.h5)
  ADD_H5_TEST (tboot2B 0 --enable-error-stack --superblock tfcontents2.h5)
  ADD_H5_TEST (file_space 0 --enable-error-stack -B file_space.h5)

  # test -p with a non existing dataset
  ADD_H5ERR_MASK_TEST (tperror 1 "h5dump error: unable to get link info from \"bogus\"" --enable-error-stack -p -d bogus tfcontents1.h5)

  # test for file contents
  ADD_H5_TEST (tcontents 0 --enable-error-stack -n tfcontents1.h5)
  ADD_H5_TEST (tordercontents1 0 --enable-error-stack -n --sort_by=name --sort_order=ascending tfcontents1.h5)
  ADD_H5_TEST (tordercontents2 0 --enable-error-stack -n --sort_by=name --sort_order=descending tfcontents1.h5)
  ADD_H5_TEST (tattrcontents1 0 --enable-error-stack -n 1 --sort_order=ascending tall.h5)
  ADD_H5_TEST (tattrcontents2 0 --enable-error-stack -n 1 --sort_order=descending tall.h5)

  # tests for storage layout
  # compact
  ADD_H5_TEST (tcompact 0 --enable-error-stack -H -p -d compact tfilters.h5)
  # contiguous
  ADD_H5_TEST (tcontiguos 0 --enable-error-stack -H -p -d contiguous tfilters.h5)
  # chunked
  ADD_H5_TEST (tchunked 0 --enable-error-stack -H -p -d chunked tfilters.h5)
  # external
  ADD_H5_TEST (texternal 0 --enable-error-stack -H -p -d external tfilters.h5)

  # fill values
  ADD_H5_TEST (tfill 0 --enable-error-stack -p tfvalues.h5)

  # several datatype, with references , print path
  ADD_H5_TEST (treference 0 --enable-error-stack  tattr2.h5)

  # escape/not escape non printable characters
  ADD_H5_TEST (tstringe 0 --enable-error-stack -e tstr3.h5)
  ADD_H5_TEST (tstring 0 --enable-error-stack tstr3.h5)
  # char data as ASCII with non escape
  ADD_H5_TEST (tstring2 0 --enable-error-stack -r -d str4 tstr3.h5)

  # array indices print/not print
  ADD_H5_TEST (tindicesyes 0 --enable-error-stack taindices.h5)
  ADD_H5_TEST (tindicesno 0 --enable-error-stack -y taindices.h5)

  ########## array indices with subsetting
  # 1D case
  ADD_H5_TEST (tindicessub1 0 --enable-error-stack -d 1d -s 1 -S 10 -c 2  -k 3 taindices.h5)

  # 2D case
  ADD_H5_TEST (tindicessub2 0 --enable-error-stack -d 2d -s 1,2  -S 3,3 -c 3,2 -k 2,2 taindices.h5)

  # 3D case
  ADD_H5_TEST (tindicessub3 0 --enable-error-stack -d 3d -s 0,1,2 -S 1,3,3 -c 2,2,2  -k 1,2,2  taindices.h5)

  # 4D case
  ADD_H5_TEST (tindicessub4 0 --enable-error-stack -d 4d -s 0,0,1,2  -c 2,2,3,2 -S 1,1,3,3 -k 1,1,2,2  taindices.h5)

  # Exceed the dimensions for subsetting
  ADD_H5_TEST (texceedsubstart 1 --enable-error-stack -d 1d -s 1,3 taindices.h5)
  ADD_H5_TEST (texceedsubcount 1 --enable-error-stack -d 1d -c 1,3 taindices.h5)
  ADD_H5_TEST (texceedsubstride 1 --enable-error-stack -d 1d -S 1,3 taindices.h5)
  ADD_H5_TEST (texceedsubblock 1 --enable-error-stack -d 1d -k 1,3 taindices.h5)

  # tests for filters
  # SZIP
  ADD_H5_TEST (tszip 0 --enable-error-stack -H -p -d szip tfilters.h5)

  # deflate
  ADD_H5_TEST (tdeflate 0 --enable-error-stack -H -p -d deflate tfilters.h5)

  # shuffle
  ADD_H5_TEST (tshuffle 0 --enable-error-stack -H -p -d shuffle tfilters.h5)

  # fletcher32
  ADD_H5_TEST (tfletcher32 0 --enable-error-stack -H -p -d fletcher32  tfilters.h5)

  # nbit
  ADD_H5_TEST (tnbit 0 --enable-error-stack -H -p -d nbit  tfilters.h5)

  # scaleoffset
  ADD_H5_TEST (tscaleoffset 0 --enable-error-stack -H -p -d scaleoffset  tfilters.h5)

  # all
  ADD_H5_TEST (tallfilters 0 --enable-error-stack -H -p -d all  tfilters.h5)

  # user defined
  ADD_H5_TEST (tuserfilter 0 --enable-error-stack -H  -p -d myfilter  tfilters.h5)


# See which filters are usable (and skip tests for filters we
# don't have).  Do this by searching H5pubconf.h to see which
# filters are defined.

# detect whether the encoder is present.
  if (H5_HAVE_FILTER_DEFLATE)
    set (USE_FILTER_DEFLATE "true")
  endif ()

  if (H5_HAVE_FILTER_SZIP)
    set (USE_FILTER_SZIP "true")
  endif ()

  if (USE_FILTER_DEFLATE)
    # data read internal filters
    ADD_H5_TEST (treadintfilter 0 --enable-error-stack -d deflate -d shuffle -d fletcher32 -d nbit -d scaleoffset tfilters.h5)
    if (HDF5_ENABLE_SZIP_SUPPORT)
      # data read all filters
      ADD_H5_TEST (treadfilter 0 --enable-error-stack -d all -d szip tfilters.h5)
    endif ()
  endif ()

  # test for displaying objects with very long names
  ADD_H5_TEST (tlonglinks 0 --enable-error-stack tlonglinks.h5)

  # dimensions over 4GB, print boundary
  ADD_H5_TEST (tbigdims 0 --enable-error-stack -d dset4gb -s 4294967284 -c 22 tbigdims.h5)

  # hyperslab read
  ADD_H5_TEST (thyperslab 0 --enable-error-stack thyperslab.h5)

  # test for displaying dataset and attribute of null space
  ADD_H5_TEST (tnullspace 0 --enable-error-stack tnullspace.h5)
  ADD_H5_TEST (tgrpnullspace 0 -p --enable-error-stack tgrpnullspace.h5)

  # test for displaying dataset and attribute of space with 0 dimension size
  ADD_H5_TEST (zerodim 0 --enable-error-stack zerodim.h5)

  # test for long double (some systems do not have long double)
  #ADD_H5_TEST (tldouble 0 --enable-error-stack tldouble.h5)

  # test for vms
  ADD_H5_TEST (tvms 0 --enable-error-stack tvms.h5)

  # test for binary output
  ADD_H5_TEST (tbin1LE 0 --enable-error-stack -d integer -o tbin1LE.bin -b LE tbinary.h5)

  # test for string binary output
  ADD_H5_EXPORT_TEST (tstr2bin2 tstr2.h5 0 --enable-error-stack -d /g2/dset2 -b -o)
  ADD_H5_EXPORT_TEST (tstr2bin6 tstr2.h5 0 --enable-error-stack -d /g6/dset6 -b -o)

  # NATIVE default. the NATIVE test can be validated with h5import/h5diff
#  ADD_H5_TEST_IMPORT (tbin1 out1D tbinary.h5 0 --enable-error-stack -d integer -b)

  if (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ADD_H5_TEST (tbin2 0 --enable-error-stack -b BE -d float -o tbin2.bin tbinary.h5)
  endif ()

  # the NATIVE test can be validated with h5import/h5diff
#  ADD_H5_TEST_IMPORT (tbin3 out3D tbinary.h5 0 --enable-error-stack -d integer -b NATIVE)

  if (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ADD_H5_TEST (tbin4 0 --enable-error-stack -d double -b FILE -o tbin4.bin tbinary.h5)
  endif ()

  # test for dataset region references
  ADD_H5_TEST (tdatareg 0 --enable-error-stack tdatareg.h5)
  ADD_H5ERR_MASK_TEST (tdataregR 0 "Undefined reference pointer" --enable-error-stack -R tdatareg.h5)
  ADD_H5_TEST (tattrreg 0 --enable-error-stack tattrreg.h5)
  ADD_H5ERR_MASK_TEST (tattrregR 0 "Undefined reference pointer" -R --enable-error-stack tattrreg.h5)
  ADD_H5_EXPORT_TEST (tbinregR tdatareg.h5 0 --enable-error-stack -d /Dataset1 -s 0 -R -y -o)

  # tests for group creation order
  # "1" tracked, "2" name, root tracked
  ADD_H5_TEST (tordergr1 0 --enable-error-stack --group=1 --sort_by=creation_order --sort_order=ascending tordergr.h5)
  ADD_H5_TEST (tordergr2 0 --enable-error-stack --group=1 --sort_by=creation_order --sort_order=descending tordergr.h5)
  ADD_H5_TEST (tordergr3 0 --enable-error-stack -g 2 -q name -z ascending tordergr.h5)
  ADD_H5_TEST (tordergr4 0 --enable-error-stack -g 2 -q name -z descending tordergr.h5)
  ADD_H5_TEST (tordergr5 0 --enable-error-stack -q creation_order tordergr.h5)

  # tests for attribute order
  ADD_H5_TEST (torderattr1 0 --enable-error-stack -H --sort_by=name --sort_order=ascending torderattr.h5)
  ADD_H5_TEST (torderattr2 0 --enable-error-stack -H --sort_by=name --sort_order=descending torderattr.h5)
  ADD_H5_TEST (torderattr3 0 --enable-error-stack -H --sort_by=creation_order --sort_order=ascending torderattr.h5)
  ADD_H5_TEST (torderattr4 0 --enable-error-stack -H --sort_by=creation_order --sort_order=descending torderattr.h5)

  # tests for link references and order
  ADD_H5ERR_MASK_TEST (torderlinks1 0 "unable to open external file, external link file name = 'fname'" --enable-error-stack --sort_by=name --sort_order=ascending tfcontents1.h5)
  ADD_H5ERR_MASK_TEST (torderlinks2 0 "unable to open external file, external link file name = 'fname'" --enable-error-stack --sort_by=name --sort_order=descending tfcontents1.h5)

  # tests for floating point user defined printf format
  ADD_H5_TEST (tfpformat 0 --enable-error-stack -m %.7f tfpformat.h5)

  # tests for traversal of external links
  ADD_H5ERR_MASK_TEST (textlinksrc 0 "Too many soft links in path" --enable-error-stack textlinksrc.h5)
  ADD_H5ERR_MASK_TEST (textlinkfar 0 "Too many soft links in path" --enable-error-stack textlinkfar.h5)

  # test for dangling external links
  ADD_H5ERR_MASK_TEST (textlink 0 "unable to open external file, external link file name = 'anotherfile'" --enable-error-stack textlink.h5)

  # test for error stack display (BZ2048)
  ADD_H5ERR_MASK_ENV_TEST (filter_fail 1 "filter plugins disabled" "HDF5_PLUGIN_PRELOAD" "::" --enable-error-stack filter_fail.h5)

  # test for -o -y for dataset with attributes
  ADD_H5_TEST_EXPORT (tall-6 tall.h5 0 --enable-error-stack -d /g1/g1.1/dset1.1.1 -y -o)

  # test for non-existing file
  ADD_H5_TEST (non_existing 1 --enable-error-stack tgroup.h5 non_existing.h5)

  # test to verify HDFFV-9407: long double full precision
  ADD_H5_GREP_TEST (t128bit_float 1 "1.123456789012345" -m %.35Lf t128bit_float.h5)

##############################################################################
###    P L U G I N  T E S T S
##############################################################################
if (BUILD_SHARED_LIBS)
  ADD_H5_UD_TEST (h5dump_plugin_test 0 tudfilter --enable-error-stack tudfilter.h5)
endif ()

##############################################################################
##############################################################################
###                         V F D   T E S T S                              ###
##############################################################################
##############################################################################

if (HDF5_TEST_VFD)
  include (CMakeVFDTests.cmake)
endif ()
