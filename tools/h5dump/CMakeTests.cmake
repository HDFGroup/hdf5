
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
  
  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  SET (HDF5_REFERENCE_FILES
      ${HDF5_TOOLS_SRC_DIR}/testfiles/charsets.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/file_space.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/filter_fail.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/packedbits.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tall-1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tall-2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tall-2A.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tall-2A0.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tall-2B.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tall-3.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tall-4s.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tall-5s.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tall-6.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tall-7.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tall-7N.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tallfilters.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray1_big.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray3.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray4.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray5.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray6.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray7.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray8.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattr-1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattr-2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattr-3.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattr-4_be.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattrcontents1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattrcontents2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattrintsize.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattrreg.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattrregR.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tbin1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tbin1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tbin2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tbin3.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tbin4.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tbinregR.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tbigdims.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tboot1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tboot2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tboot2A.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tboot2B.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tchar1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tchunked.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcmpdattrintsize.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcmpdintarray.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcmpdints.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcmpdintsize.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcomp-1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcomp-2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcomp-3.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcomp-4.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcompact.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcontents.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcontiguos.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tdatareg.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tdataregR.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tdeflate.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tdset-1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tdset-2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tdset-3s.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tempty.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/texceedsubstart.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/texceedsubcount.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/texceedsubstride.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/texceedsubblock.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/texternal.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/textlinksrc.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/textlinkfar.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/textlink.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfill.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfletcher32.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfpformat.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tgroup-1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tgroup-2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tgrp_comments.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/thlink-1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/thlink-2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/thlink-3.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/thlink-4.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/thlink-5.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/thyperslab.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tindicesno.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tindicessub1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tindicessub2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tindicessub3.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tindicessub4.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tindicesyes.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tintsattrs.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tlarge_objname.ddl
      #${HDF5_TOOLS_SRC_DIR}/testfiles/tldouble.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tlonglinks.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tloop-1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tmulti.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tmultifile.ddl
      #${HDF5_TOOLS_SRC_DIR}/testfiles/tqmarkfile.ddl
      #${HDF5_TOOLS_SRC_DIR}/testfiles/tstarfile.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnamed_dtype_attr.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnestcomp-1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnestedcmpddt.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnbit.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnoattrdata.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnoattrddl.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnodata.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnoddl.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnoddlfile.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tno-subset.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnullspace.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tordergr1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tordergr2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tordergr3.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tordergr4.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tordergr5.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/torderattr1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/torderattr2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/torderattr3.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/torderattr4.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tordercontents1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tordercontents2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/torderlinks1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/torderlinks2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tperror.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/trawdatafile.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/trawssetfile.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/treadfilter.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/treadintfilter.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/treference.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tsaf.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tscalarattrintsize.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tscalarintattrsize.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tscalarintsize.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tscalarstring.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tscaleoffset.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tshuffle.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tslink-1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tslink-2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tslink-D.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tsplit_file.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstr-1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstr-2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstring.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstring2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstringe.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tszip.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tudlink-1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tudlink-2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tuserfilter.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes3.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes4.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes5.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvlstr.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvms.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/twidedisplay.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/twithddlfile.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/h5dump-help.txt
      ${HDF5_TOOLS_SRC_DIR}/testfiles/out3.h5import
      ${HDF5_TOOLS_SRC_DIR}/testfiles/zerodim.ddl
  )
  SET (HDF5_REFERENCE_EXP_FILES
      tall-6.exp
      tnoddlfile.exp
      trawdatafile.exp
      trawssetfile.exp
      tstr2bin2.exp
      tstr2bin6.exp
      twithddl.exp
      twithddlfile.exp
  )
  SET (HDF5_REFERENCE_TEST_FILES
      ${HDF5_TOOLS_SRC_DIR}/testfiles/charsets.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/file_space.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/filter_fail.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/packedbits.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/taindices.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tall.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray1.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray1_big.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray2.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray3.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray4.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray5.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray6.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray7.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray8.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattr.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattr2.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattr4_be.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattrintsize.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattrreg.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tbigdims.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tbinary.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tchar.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcmpdattrintsize.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcmpdintarray.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcmpdints.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcmpdintsize.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcompound.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcompound_complex.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tdatareg.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tdset.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tempty.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tsoftlinks.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/textlinkfar.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/textlinksrc.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/textlinktar.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/textlink.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00000.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00001.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00002.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00003.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00004.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00005.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00006.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00007.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00008.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00009.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00010.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfcontents1.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfcontents2.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfilters.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfpformat.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfvalues.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tgroup.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tgrp_comments.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/thlink.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/thyperslab.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tintsattrs.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tlarge_objname.h5
      #${HDF5_TOOLS_SRC_DIR}/testfiles/tldouble.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tlonglinks.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tloop.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tmulti-b.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tmulti-g.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tmulti-l.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tmulti-o.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tmulti-r.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tmulti-s.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnamed_dtype_attr.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnestedcomp.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnestedcmpddt.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tno-subset.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnullspace.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/torderattr.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tordergr.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tsaf.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tscalarattrintsize.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tscalarintattrsize.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tscalarintsize.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tscalarstring.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tslink.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tsplit_file-m.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tsplit_file-r.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstr.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstr2.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstr3.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tudlink.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes1.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes2.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes3.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes4.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes5.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvlstr.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvms.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/zerodim.h5
  )
  SET (HDF5_ERROR_REFERENCE_TEST_FILES
      ${PROJECT_SOURCE_DIR}/errfiles/filter_fail.err
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
  FILE (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")

  #
  # copy test files from source dir to test dir
  #
  FOREACH (tst_h5_file ${HDF5_REFERENCE_TEST_FILES})
    GET_FILENAME_COMPONENT(fname "${tst_h5_file}" NAME)
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/std/${fname}")
    #MESSAGE (STATUS " Copying ${tst_h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${tst_h5_file} ${dest}
    )
  ENDFOREACH (tst_h5_file ${HDF5_REFERENCE_TEST_FILES})
  
  FOREACH (tst_exp_file ${HDF5_REFERENCE_EXP_FILES})
    IF (WIN32 AND NOT CYGWIN)
      FILE (READ ${HDF5_TOOLS_SRC_DIR}/testfiles/${tst_exp_file} TEST_STREAM)
      FILE (WRITE ${PROJECT_BINARY_DIR}/testfiles/std/${tst_exp_file} "${TEST_STREAM}")
    ELSE (WIN32 AND NOT CYGWIN)
      ADD_CUSTOM_COMMAND (
          TARGET     h5dump
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different  ${HDF5_TOOLS_SRC_DIR}/testfiles/${tst_exp_file}  ${PROJECT_BINARY_DIR}/testfiles/std/${tst_exp_file}
      )
    ENDIF (WIN32 AND NOT CYGWIN)
  ENDFOREACH (tst_exp_file ${HDF5_REFERENCE_EXP_FILES})

  FOREACH (tst_other_file ${HDF5_REFERENCE_FILES})
    GET_FILENAME_COMPONENT(fname "${tst_other_file}" NAME)
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/std/${fname}")
    #MESSAGE (STATUS " Copying ${tst_other_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${tst_other_file} ${dest}
    )
  ENDFOREACH (tst_other_file ${HDF5_REFERENCE_FILES})

  FOREACH (tst_error_file ${HDF5_ERROR_REFERENCE_TEST_FILES})
    GET_FILENAME_COMPONENT(fname "${tst_error_file}" NAME)
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/std/${fname}")
    #MESSAGE (STATUS " Copying ${tst_error_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${tst_error_file} ${dest}
    )
  ENDFOREACH (tst_error_file ${HDF5_ERROR_REFERENCE_TEST_FILES})

  # --------------------------------------------------------------------
  # Special file handling
  # --------------------------------------------------------------------
  ADD_CUSTOM_COMMAND (
      TARGET     h5dump
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different  ${HDF5_TOOLS_SOURCE_DIR}/testfiles/tbin1.ddl  ${PROJECT_BINARY_DIR}/testfiles/std/tbin1LE.ddl
  )
  
  IF (WIN32 AND NOT CYGWIN)
    FILE (READ ${HDF5_TOOLS_SRC_DIR}/testfiles/tbinregR.exp TEST_STREAM)
    FILE (WRITE ${PROJECT_BINARY_DIR}/testfiles/std/tbinregR.exp "${TEST_STREAM}")
  ELSE (WIN32 AND NOT CYGWIN)
    ADD_CUSTOM_COMMAND (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different  ${HDF5_TOOLS_SRC_DIR}/testfiles/tbinregR.exp  ${PROJECT_BINARY_DIR}/testfiles/std/tbinregR.exp
    )
  ENDIF (WIN32 AND NOT CYGWIN)
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_HELP_TEST testname resultcode)
    # If using memchecker add tests without using scripts
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5DUMP-${testname} COMMAND $<TARGET_FILE:h5dump> ${ARGN})
      SET_TESTS_PROPERTIES (H5DUMP-${testname} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5DUMP-${testname} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
      SET (last_test "H5DUMP-${testname}")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5DUMP-h5dump-${testname}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove h5dump-${testname}.out h5dump-${testname}.out.err
      )
      SET_TESTS_PROPERTIES (H5DUMP-h5dump-${testname}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      ADD_TEST (
          NAME H5DUMP-h5dump-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=h5dump-${testname}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=h5dump-${testname}.txt"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5DUMP-h5dump-${testname} PROPERTIES DEPENDS "H5DUMP-h5dump-${testname}-clear-objects")
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_HELP_TEST)

  MACRO (ADD_SKIP_H5_TEST skipresultfile skipresultcode testtype)
    IF (${testtype} STREQUAL "SKIP")
      IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
        ADD_TEST (
            NAME H5DUMP-${skipresultfile}-SKIPPED
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${skipresultfile} ${ARGN}"
        )
      ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ELSE (${testtype} STREQUAL "SKIP")
      ADD_H5_TEST (${skipresultfile} ${skipresultcode} ${ARGN})
    ENDIF (${testtype} STREQUAL "SKIP")
  ENDMACRO (ADD_SKIP_H5_TEST)

  MACRO (ADD_H5_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5DUMP-${resultfile} COMMAND $<TARGET_FILE:h5dump> ${ARGN})
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      IF (NOT ${resultcode} STREQUAL "0")
        SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      ENDIF (NOT ${resultcode} STREQUAL "0")
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5DUMP-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove ${resultfile}.bin ${resultfile}.out ${resultfile}.out.err
      )
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      ADD_TEST (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES DEPENDS "H5DUMP-${resultfile}-clear-objects")
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_TEST file)

  MACRO (ADD_H5_TEST_N resultfile resultcode)
    # If using memchecker add tests without using scripts
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5DUMP-N-${resultfile} COMMAND $<TARGET_FILE:h5dump> ${ARGN})
      SET_TESTS_PROPERTIES (H5DUMP-N-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      IF (NOT ${resultcode} STREQUAL "0")
        SET_TESTS_PROPERTIES (H5DUMP-N-${resultfile} PROPERTIES WILL_FAIL "true")
      ENDIF (NOT ${resultcode} STREQUAL "0")
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5DUMP-N-${resultfile} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5DUMP-N-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove ${resultfile}-N.bin ${resultfile}-N.out ${resultfile}-N.out.err
      )
      SET_TESTS_PROPERTIES (H5DUMP-N-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      ADD_TEST (
          NAME H5DUMP-N-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}-N.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5DUMP-N-${resultfile} PROPERTIES DEPENDS "H5DUMP-N-${resultfile}-clear-objects")
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_TEST_N file)

  MACRO (ADD_H5_TEST_EXPORT resultfile targetfile resultcode)
    # If using memchecker add tests without using scripts
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5DUMP-${resultfile} COMMAND $<TARGET_FILE:h5dump> ${ARGN} ${resultfile}.txt ${targetfile})
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      IF (NOT ${resultcode} STREQUAL "0")
        SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      ENDIF (NOT ${resultcode} STREQUAL "0")
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5DUMP-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove ${resultfile}.txt ${resultfile}.out ${resultfile}.out.err
      )
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      ADD_TEST (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN};${resultfile}.txt;${targetfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES DEPENDS "H5DUMP-${resultfile}-clear-objects")
      ADD_TEST (
          NAME H5DUMP-${resultfile}-output-cmp
          COMMAND ${CMAKE_COMMAND}
                -E compare_files ${resultfile}.txt ${resultfile}.exp
      )
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile}-output-cmp PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile}-output-cmp PROPERTIES DEPENDS H5DUMP-${resultfile})
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_TEST_EXPORT file)

  MACRO (ADD_H5_TEST_EXPORT_DDL resultfile targetfile resultcode ddlfile)
    # If using memchecker add tests without using scripts
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5DUMP-${resultfile} COMMAND $<TARGET_FILE:h5dump> --ddl=${ddlfile}.txt ${ARGN} ${resultfile}.txt ${targetfile})
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      IF (NOT ${resultcode} STREQUAL "0")
        SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      ENDIF (NOT ${resultcode} STREQUAL "0")
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5DUMP-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove ${ddlfile}.txt ${resultfile}.txt ${resultfile}.out ${resultfile}.out.err
      )
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      ADD_TEST (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=--ddl=${ddlfile}.txt;${ARGN};${resultfile}.txt;${targetfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES DEPENDS "H5DUMP-${resultfile}-clear-objects")
      ADD_TEST (
          NAME H5DUMP-${resultfile}-output-cmp
          COMMAND ${CMAKE_COMMAND}
                -E compare_files ${resultfile}.txt ${resultfile}.exp
      )
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile}-output-cmp PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile}-output-cmp PROPERTIES DEPENDS H5DUMP-${resultfile})
      ADD_TEST (
          NAME H5DUMP-${resultfile}-output-cmp-ddl
          COMMAND ${CMAKE_COMMAND}
                -E compare_files ${ddlfile}.txt ${ddlfile}.exp
      )
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile}-output-cmp-ddl PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile}-output-cmp-ddl PROPERTIES DEPENDS H5DUMP-${resultfile}-output-cmp)
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_TEST_EXPORT_DDL file)

  MACRO (ADD_H5_EXPORT_TEST resultfile targetfile resultcode)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5DUMP-output-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove ${resultfile}.txt
      )
      SET_TESTS_PROPERTIES (H5DUMP-output-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      ADD_TEST (
          NAME H5DUMP-output-${resultfile}
          COMMAND $<TARGET_FILE:h5dump> ${ARGN} ${resultfile}.txt ${targetfile}
      )
      SET_TESTS_PROPERTIES (H5DUMP-output-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      SET_TESTS_PROPERTIES (H5DUMP-output-${resultfile} PROPERTIES DEPENDS H5DUMP-output-${resultfile}-clear-objects)
      ADD_TEST (
          NAME H5DUMP-output-cmp-${resultfile}
          COMMAND ${CMAKE_COMMAND}
                -E compare_files ${resultfile}.txt ${resultfile}.exp
      )
      SET_TESTS_PROPERTIES (H5DUMP-output-cmp-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      SET_TESTS_PROPERTIES (H5DUMP-output-cmp-${resultfile} PROPERTIES DEPENDS H5DUMP-output-${resultfile})
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_EXPORT_TEST file)

  MACRO (ADD_H5_MASK_TEST resultfile resultcode)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5DUMP-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove ${resultfile}.out ${resultfile}.out.err
      )
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      ADD_TEST (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -D "TEST_MASK_ERROR=true"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES DEPENDS "H5DUMP-${resultfile}-clear-objects")
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_MASK_TEST file)

  MACRO (ADD_H5ERR_MASK_TEST resultfile resultcode)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5DUMP-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove ${resultfile}.out ${resultfile}.out.err
      )
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      ADD_TEST (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -D "TEST_ERRREF=${resultfile}.err"
              -D "TEST_MASK_ERROR=true"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES DEPENDS "H5DUMP-${resultfile}-clear-objects")
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5ERR_MASK_TEST file)

  MACRO (ADD_H5ERR_MASK_ENV_TEST resultfile resultcode envvar envval)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5DUMP-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove ${resultfile}.out ${resultfile}.out.err
      )
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      ADD_TEST (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -D "TEST_ERRREF=${resultfile}.err"
              -D "TEST_MASK_ERROR=true"
              -D "TEST_ENV_VAR:STRING=${envvar}"
              -D "TEST_ENV_VALUE:STRING=${envval}"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES DEPENDS "H5DUMP-${resultfile}-clear-objects")
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5ERR_MASK_ENV_TEST)

  MACRO (ADD_H5_TEST_IMPORT conffile resultfile testfile resultcode)
    # If using memchecker add tests without using scripts
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5DUMP-IMPORT-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove ${conffile}.out ${conffile}.out.err ${resultfile}.bin ${resultfile}.h5
      )
      SET_TESTS_PROPERTIES (H5DUMP-IMPORT-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      ADD_TEST (
          NAME H5DUMP-IMPORT-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN};-o;${resultfile}.bin;${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${conffile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${conffile}.ddl"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5DUMP-IMPORT-${resultfile} PROPERTIES DEPENDS "H5DUMP-IMPORT-${resultfile}-clear-objects")
      ADD_TEST (NAME H5DUMP-IMPORT-h5import-${resultfile} COMMAND h5import ${resultfile}.bin -c ${conffile}.out -o ${resultfile}.h5)
      SET_TESTS_PROPERTIES (H5DUMP-IMPORT-h5import-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      SET_TESTS_PROPERTIES (H5DUMP-IMPORT-h5import-${resultfile} PROPERTIES DEPENDS H5DUMP-IMPORT-${resultfile})
      ADD_TEST (NAME H5DUMP-IMPORT-h5diff-${resultfile} COMMAND h5diff ${testfile} ${resultfile}.h5 /integer /integer)
      SET_TESTS_PROPERTIES (H5DUMP-IMPORT-h5diff-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
      SET_TESTS_PROPERTIES (H5DUMP-IMPORT-h5diff-${resultfile} PROPERTIES DEPENDS H5DUMP-IMPORT-h5import-${resultfile})
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_TEST_IMPORT file)

##############################################################################
##############################################################################
###           T H E   T E S T S                                          HDF5_ENABLE_USING_MEMCHECKER  ###
##############################################################################
##############################################################################

  IF (HDF5_ENABLE_USING_MEMCHECKER)
    # Remove any output file left over from previous test run
    ADD_TEST (
      NAME H5DUMP-clearall-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          h5dump-help.out
          charsets.out
          charsets.out.err
          file_space.out
          file_space.out.err
          filter_fail.out
          filter_fail.out.err
          packedbits.out
          packedbits.out.err
          tall-1.out
          tall-1.out.err
          tall-2.out
          tall-2.out.err
          tall-2A.out
          tall-2A.out.err
          tall-2A0.out
          tall-2A0.out.err
          tall-2B.out
          tall-2B.out.err
          tall-3.out
          tall-3.out.err
          tall-4s.out
          tall-4s.out.err
          tall-5s.out
          tall-5s.out.err
          tall-6.txt
          tall-6.out
          tall-6.out.err
          tall-7.out
          tall-7.out.err
          tall-7N.out
          tall-7N.out.err
          tallfilters.out
          tallfilters.out.err
          tarray1.out
          tarray1.out.err
          tarray1_big.out
          tarray1_big.out.err
          tarray2.out
          tarray2.out.err
          tarray3.out
          tarray3.out.err
          tarray4.out
          tarray4.out.err
          tarray5.out
          tarray5.out.err
          tarray6.out
          tarray6.out.err
          tarray7.out
          tarray7.out.err
          tarray8.out
          tarray8.out.err
          tattr-1.out
          tattr-1.out.err
          tattr-2.out
          tattr-2.out.err
          tattr-3.out
          tattr-3.out.err
          tattr-4_be.out
          tattr-4_be.out.err
          tattrcontents1.out
          tattrcontents1.out.err
          tattrcontents2.out
          tattrcontents2.out.err
          tattrintsize.out
          tattrintsize.out.err
          tattrreg.out
          tattrreg.out.err
          tattrregR.out
          tattrregR.out.err
          tbin1LE.bin
          tbinregR.txt
          tbinregR.out
          tbinregR.out.err
          tbigdims.out
          tbigdims.out.err
          tboot1.out
          tboot1.out.err
          tboot2.out
          tboot2.out.err
          tboot2A.out
          tboot2A.out.err
          tboot2B.out
          tboot2B.out.err
          tchar1.out
          tchar1.out.err
          tchunked.out
          tchunked.out.err
          tcmpdattrintsize.out
          tcmpdattrintsize.out.err
          tcmpdintarray.out
          tcmpdintarray.out.err
          tcmpdints.out
          tcmpdints.out.err
          tcmpdintsize.out
          tcmpdintsize.out.err
          tcomp-1.out
          tcomp-1.out.err
          tcomp-2.out
          tcomp-2.out.err
          tcomp-3.out
          tcomp-3.out.err
          tcomp-4.out
          tcomp-4.out.err
          tcompact.out
          tcompact.out.err
          tcontents.out
          tcontents.out.err
          tcontiguos.out
          tcontiguos.out.err
          tdatareg.out
          tdatareg.out.err
          tdataregR.out
          tdataregR.out.err
          tdeflate.out
          tdeflate.out.err
          tdset-1.out
          tdset-1.out.err
          tdset-2.out
          tdset-2.out.err
          tdset-3s.out
          tdset-3s.out.err
          tempty.out
          tempty.out.err
          texternal.out
          texternal.out.err
          textlinksrc.out
          textlinksrc.out.err
          textlinkfar.out
          textlinkfar.out.err
          textlink.out
          textlink.out.err
          tfamily.out
          tfamily.out.err
          tfill.out
          tfill.out.err
          tfletcher32.out
          tfletcher32.out.err
          tfpformat.out
          tfpformat.out.err
          tgroup-1.out
          tgroup-1.out.err
          tgroup-2.out
          tgroup-2.out.err
          tgrp_comments.out
          tgrp_comments.out.err
          thlink-1.out
          thlink-1.out.err
          thlink-2.out
          thlink-2.out.err
          thlink-3.out
          thlink-3.out.err
          thlink-4.out
          thlink-4.out.err
          thlink-5.out
          thlink-5.out.err
          thyperslab.out
          thyperslab.out.err
          tindicesno.out
          tindicesno.out.err
          tindicessub1.out
          tindicessub1.out.err
          tindicessub2.out
          tindicessub2.out.err
          tindicessub3.out
          tindicessub3.out.err
          tindicessub4.out
          tindicessub4.out.err
          texceedsubstart.out
          texceedsubstart.out.err
          texceedsubcount.out
          texceedsubcount.out.err
          texceedsubstride.out
          texceedsubstride.out.err
          texceedsubblock.out
          texceedsubblock.out.err
          tindicesyes.out
          tindicesyes.out.err
          tintsattrs.out
          tintsattrs.out.err
          tlarge_objname.out
          tlarge_objname.out.err
          tldouble.out
          tldouble.out.err
          tlonglinks.out
          tlonglinks.out.err
          tloop-1.out
          tloop-1.out.err
          tmulti.out
          tmulti.out.err
          tmultifile.out
          tmultifile.out.err
#          tqmarkfile.out
#          tqmarkfile.out.err
#          tstarfile.out
#          tstarfile.out.err
          tnamed_dtype_attr.out
          tnamed_dtype_attr.out.err
          tnbit.out
          tnbit.out.err
          tnestcomp-1.out
          tnestcomp-1.out.err
          tnestedcmpddt.out
          tnestedcmpddt.out.err
          tnoattrdata.out
          tnoattrdata.out.err
          tnoattrddl.out
          tnoattrddl.out.err
          tnodata.out
          tnodata.out.err
          tnoddl.out
          tnoddl.out.err
          tnoddlfile.out
          tnoddlfile.out.err
          tno-subset.out
          tno-subset.out.err
          tnullspace.out
          tnullspace.out.err
          tordergr1.out
          tordergr1.out.err
          tordergr2.out
          tordergr2.out.err
          tordergr3.out
          tordergr3.out.err
          tordergr4.out
          tordergr4.out.err
          tordergr5.out
          tordergr5.out.err
          torderattr1.out
          torderattr1.out.err
          torderattr2.out
          torderattr2.out.err
          torderattr3.out
          torderattr3.out.err
          torderattr4.out
          torderattr4.out.err
          tordercontents1.out
          tordercontents1.out.err
          tordercontents2.out
          tordercontents2.out.err
          torderlinks1.out
          torderlinks1.out.err
          torderlinks2.out
          torderlinks2.out.err
          tperror.out
          tperror.out.err
          trawdatafile.out
          trawdatafile.out.err
          trawdatafile.txt
          trawssetfile.out
          trawssetfile.out.err
          trawssetfile.txt
          treadfilter.out
          treadfilter.out.err
          treadintfilter.out
          treadintfilter.out.err
          treference.out
          treference.out.err
          tsaf.out
          tsaf.out.err
          tscalarattrintsize.out
          tscalarattrintsize.out.err
          tscalarintattrsize.out
          tscalarintattrsize.out.err
          tscalarintsize.out
          tscalarintsize.out.err
          tscalarstring.out
          tscalarstring.out.err
          tscaleoffset.out
          tscaleoffset.out.err
          tshuffle.out
          tshuffle.out.err
          tslink-1.out
          tslink-1.out.err
          tslink-2.out
          tslink-2.out.err
          tslink-D.out
          tslink-D.out.err
          tsplit_file.out
          tsplit_file.out.err
          tstr-1.out
          tstr-1.out.err
          tstr-2.out
          tstr-2.out.err
          tstr2bin2.txt
          tstr2bin6.txt
          tstring.out
          tstring.out.err
          tstring2.out
          tstring2.out.err
          tstringe.out
          tstringe.out.err
          tszip.out
          tszip.out.err
          tudlink-1.out
          tudlink-1.out.err
          tudlink-2.out
          tudlink-2.out.err
          tuserfilter.out
          tuserfilter.out.err
          tvldtypes1.out
          tvldtypes1.out.err
          tvldtypes2.out
          tvldtypes2.out.err
          tvldtypes3.out
          tvldtypes3.out.err
          tvldtypes4.out
          tvldtypes4.out.err
          tvldtypes5.out
          tvldtypes5.out.err
          tvlstr.out
          tvlstr.out.err
          tvms.out
          tvms.out.err
          twidedisplay.out
          twidedisplay.out.err
          twithddl.txt
          twithddlfile.out
          twithddlfile.out.err
          twithddlfile.txt
          zerodim.out
          zerodim.out.err
    )
    SET_TESTS_PROPERTIES (H5DUMP-clearall-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std")
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5DUMP-clearall-objects PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "H5DUMP-clearall-objects")
  ENDIF (HDF5_ENABLE_USING_MEMCHECKER)

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
  ADD_H5ERR_MASK_TEST (tgroup-2 1 --enable-error-stack --group=/g2 --group / -g /y tgroup.h5)

  # test for displaying simple space datasets
  ADD_H5_TEST (tdset-1 0 --enable-error-stack tdset.h5)
  # test for displaying selected datasets
  ADD_H5ERR_MASK_TEST (tdset-2 1 --enable-error-stack -H -d dset1 -d /dset2 --dataset=dset3 tdset.h5)

  # test for displaying attributes
  ADD_H5_TEST (tattr-1 0 --enable-error-stack tattr.h5)
  # test for displaying the selected attributes of string type and scalar space
  ADD_H5_TEST (tattr-2 0 --enable-error-stack -a /\\\\/attr1 --attribute /attr4 --attribute=/attr5 tattr.h5)
  ADD_H5_TEST_N (tattr-2 0 --enable-error-stack -N /\\\\/attr1 --any_path /attr4 --any_path=/attr5 tattr.h5)
  # test for header and error messages
  ADD_H5ERR_MASK_TEST (tattr-3 1 --enable-error-stack --header -a /attr2 --attribute=/attr tattr.h5)
  # test for displaying attributes in shared datatype (also in group and dataset)
  ADD_H5_TEST (tnamed_dtype_attr 0 --enable-error-stack tnamed_dtype_attr.h5)
  # test for displaying at least 9 attributes on root from a be machine
  ADD_H5_TEST (tattr-4_be 0 --enable-error-stack tattr4_be.h5)

  # test for displaying soft links and user-defined links
  ADD_H5_TEST (tslink-1 0 --enable-error-stack tslink.h5)
  ADD_H5_TEST (tudlink-1 0 --enable-error-stack tudlink.h5)
  # test for displaying the selected link
  ADD_H5_TEST (tslink-2 0 --enable-error-stack -l slink2 tslink.h5)
  ADD_H5_TEST_N (tslink-2 0 --enable-error-stack -N slink2 tslink.h5)
  ADD_H5_TEST (tudlink-2 0 --enable-error-stack -l udlink2 tudlink.h5)
  # test for displaying dangling soft links
  ADD_H5ERR_MASK_TEST (tslink-D 0 --enable-error-stack -d /slink1 tslink.h5)

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
  ADD_H5ERR_MASK_TEST (tcomp-3 0 "--enable-error-stack;-t;/#6632;-g;/group2;tcompound.h5")
  # test complicated compound datatype
  ADD_H5_TEST (tcomp-4 0 --enable-error-stack tcompound_complex.h5)

  #test for the nested compound type
  ADD_H5_TEST (tnestcomp-1 0 --enable-error-stack tnestedcomp.h5)
  ADD_H5_TEST (tnestedcmpddt 0 --enable-error-stack tnestedcmpddt.h5)

  # test for options
  ADD_H5ERR_MASK_TEST (tall-1 0 --enable-error-stack tall.h5)
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

  # test for files with array data
  ADD_H5_TEST (tarray1 0 --enable-error-stack tarray1.h5)
  # # added for bug# 2092 - tarray1_big.h5
  ADD_H5ERR_MASK_TEST (tarray1_big 0 --enable-error-stack -R tarray1_big.h5)
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
  ADD_H5ERR_MASK_TEST (tall-2A 0 --enable-error-stack -A tall.h5)

  # test '-A' to suppress attr's but print data
  ADD_H5ERR_MASK_TEST (tall-2A0 0 --enable-error-stack -A 0 tall.h5)

  # test '-r' to print attributes in ASCII instead of decimal
  ADD_H5ERR_MASK_TEST (tall-2B 0 --enable-error-stack -A -r tall.h5)

  # test Subsetting
  ADD_H5_TEST (tall-4s 0 --enable-error-stack --dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 tall.h5)
  ADD_H5_TEST (tall-5s 0 --enable-error-stack -d "/g1/g1.1/dset1.1.2[0;2;10;]" tall.h5)
  ADD_H5_TEST (tdset-3s 0 --enable-error-stack -d "/dset1[1,1;;;]" tdset.h5)
  ADD_H5_TEST (tno-subset 0 --enable-error-stack --no-compact-subset -d "AHFINDERDIRECT::ah_centroid_t[0] it=0 tl=0" tno-subset.h5)

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
  ADD_H5ERR_MASK_TEST (tperror 1 --enable-error-stack -p -d bogus tfcontents1.h5)

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
  IF (H5_HAVE_FILTER_DEFLATE)
    SET (USE_FILTER_DEFLATE "true")
  ENDIF (H5_HAVE_FILTER_DEFLATE)

  IF (H5_HAVE_FILTER_SZIP)
    SET (USE_FILTER_SZIP "true")
  ENDIF (H5_HAVE_FILTER_SZIP)

  IF (H5_HAVE_FILTER_SHUFFLE)
    SET (USE_FILTER_SHUFFLE "true")
  ENDIF (H5_HAVE_FILTER_SHUFFLE)

  IF (H5_HAVE_FILTER_FLETCHER32)
    SET (USE_FILTER_FLETCHER32 "true")
  ENDIF (H5_HAVE_FILTER_FLETCHER32)

  IF (H5_HAVE_FILTER_NBIT)
    SET (USE_FILTER_NBIT "true")
  ENDIF (H5_HAVE_FILTER_NBIT)

  IF (H5_HAVE_FILTER_SCALEOFFSET)
    SET (USE_FILTER_SCALEOFFSET "true")
  ENDIF (H5_HAVE_FILTER_SCALEOFFSET)

  IF (USE_FILTER_DEFLATE AND USE_FILTER_SHUFFLE AND USE_FILTER_FLETCHER32 AND USE_FILTER_NBIT AND USE_FILTER_SCALEOFFSET)
    # data read internal filters
    ADD_H5_TEST (treadintfilter 0 --enable-error-stack -d deflate -d shuffle -d fletcher32 -d nbit -d scaleoffset tfilters.h5)
    IF (HDF5_ENABLE_SZIP_SUPPORT)
      # data read all filters
      ADD_H5_TEST (treadfilter 0 --enable-error-stack -d all -d szip tfilters.h5)
    ENDIF (HDF5_ENABLE_SZIP_SUPPORT)
  ENDIF (USE_FILTER_DEFLATE AND USE_FILTER_SHUFFLE AND USE_FILTER_FLETCHER32 AND USE_FILTER_NBIT AND USE_FILTER_SCALEOFFSET)

  # test for displaying objects with very long names
  ADD_H5_TEST (tlonglinks 0 --enable-error-stack tlonglinks.h5)

  # dimensions over 4GB, print boundary 
  ADD_H5_TEST (tbigdims 0 --enable-error-stack -d dset4gb -s 4294967284 -c 22 tbigdims.h5)

  # hyperslab read
  ADD_H5_TEST (thyperslab 0 --enable-error-stack thyperslab.h5)
    
  # test for displaying dataset and attribute of null space
  ADD_H5_TEST (tnullspace 0 --enable-error-stack tnullspace.h5)

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
  ADD_H5_TEST_IMPORT (tbin1 out1D tbinary.h5 0 --enable-error-stack -d integer -b)

  IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ADD_H5_TEST (tbin2 0 --enable-error-stack -b BE -d float -o tbin2.bin tbinary.h5)
  ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)

  # the NATIVE test can be validated with h5import/h5diff
  ADD_H5_TEST_IMPORT (tbin3 out3D tbinary.h5 0 --enable-error-stack -d integer -b NATIVE)

  IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ADD_H5_TEST (tbin4 0 --enable-error-stack -d double -b FILE -o tbin4.bin tbinary.h5)
  ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)

  # test for dataset region references 
  ADD_H5_TEST (tdatareg 0 --enable-error-stack tdatareg.h5)
  ADD_H5ERR_MASK_TEST (tdataregR 0 --enable-error-stack -R tdatareg.h5)
  ADD_H5ERR_MASK_TEST (tattrregR 0 -R --enable-error-stack tattrreg.h5)
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
  ADD_H5ERR_MASK_TEST (torderlinks1 0 --enable-error-stack --sort_by=name --sort_order=ascending tfcontents1.h5)
  ADD_H5ERR_MASK_TEST (torderlinks2 0 --enable-error-stack --sort_by=name --sort_order=descending tfcontents1.h5)
  
  # tests for floating point user defined printf format
  ADD_H5_TEST (tfpformat 0 --enable-error-stack -m %.7f tfpformat.h5)

  # tests for traversal of external links
  ADD_H5ERR_MASK_TEST (textlinksrc 0 --enable-error-stack textlinksrc.h5)
  ADD_H5ERR_MASK_TEST (textlinkfar 0 --enable-error-stack textlinkfar.h5)

  # test for dangling external links
  ADD_H5ERR_MASK_TEST (textlink 0 --enable-error-stack textlink.h5)

  # test for error stack display (BZ2048)
  ADD_H5ERR_MASK_ENV_TEST (filter_fail 1 "HDF5_PLUGIN_PRELOAD" "::" --enable-error-stack filter_fail.h5)

  # test for -o -y for dataset with attributes
  ADD_H5_TEST_EXPORT (tall-6 tall.h5 0 --enable-error-stack -d /g1/g1.1/dset1.1.1 -y -o)
