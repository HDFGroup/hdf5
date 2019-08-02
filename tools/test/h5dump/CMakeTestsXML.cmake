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

  #
  # copy XML test files from source dir to test dir
  #
  set (HDF5_XML_REFERENCE_TEST_FILES
      ${HDF5_TOOLS_DIR}/testfiles/tall.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray1.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray2.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray3.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray6.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray7.h5
      ${HDF5_TOOLS_DIR}/testfiles/tattr.h5
      ${HDF5_TOOLS_DIR}/testfiles/tbitfields.h5
      ${HDF5_TOOLS_DIR}/testfiles/tcompound.h5
      ${HDF5_TOOLS_DIR}/testfiles/tcompound2.h5
      ${HDF5_TOOLS_DIR}/testfiles/tcompound_complex.h5
      ${HDF5_TOOLS_DIR}/testfiles/tdatareg.h5
      ${HDF5_TOOLS_DIR}/testfiles/tdset.h5
      ${HDF5_TOOLS_DIR}/testfiles/tdset2.h5
      ${HDF5_TOOLS_DIR}/testfiles/tempty.h5
      ${HDF5_TOOLS_DIR}/testfiles/tenum.h5
      ${HDF5_TOOLS_DIR}/testfiles/textlink.h5
      ${HDF5_TOOLS_DIR}/testfiles/tfpformat.h5
      ${HDF5_TOOLS_DIR}/testfiles/tgroup.h5
      ${HDF5_TOOLS_DIR}/testfiles/thlink.h5
      ${HDF5_TOOLS_DIR}/testfiles/tloop.h5
      ${HDF5_TOOLS_DIR}/testfiles/tloop2.h5
      ${HDF5_TOOLS_DIR}/testfiles/tmany.h5
      ${HDF5_TOOLS_DIR}/testfiles/tname-amp.h5
      ${HDF5_TOOLS_DIR}/testfiles/tname-apos.h5
      ${HDF5_TOOLS_DIR}/testfiles/tname-gt.h5
      ${HDF5_TOOLS_DIR}/testfiles/tname-lt.h5
      ${HDF5_TOOLS_DIR}/testfiles/tname-quot.h5
      ${HDF5_TOOLS_DIR}/testfiles/tname-sp.h5
      ${HDF5_TOOLS_DIR}/testfiles/tnamed_dtype_attr.h5
      ${HDF5_TOOLS_DIR}/testfiles/test35.nc
      ${HDF5_TOOLS_DIR}/testfiles/tnestedcomp.h5
      ${HDF5_TOOLS_DIR}/testfiles/tnodata.h5
      ${HDF5_TOOLS_DIR}/testfiles/tnullspace.h5
      ${HDF5_TOOLS_DIR}/testfiles/tobjref.h5
      ${HDF5_TOOLS_DIR}/testfiles/topaque.h5
      ${HDF5_TOOLS_DIR}/testfiles/torderattr.h5
      ${HDF5_TOOLS_DIR}/testfiles/tref.h5
      ${HDF5_TOOLS_DIR}/testfiles/tref-escapes.h5
      ${HDF5_TOOLS_DIR}/testfiles/tref-escapes-at.h5
      ${HDF5_TOOLS_DIR}/testfiles/tsaf.h5
      ${HDF5_TOOLS_DIR}/testfiles/tslink.h5
      ${HDF5_TOOLS_DIR}/testfiles/tstring.h5
      ${HDF5_TOOLS_DIR}/testfiles/tstring-at.h5
      ${HDF5_TOOLS_DIR}/testfiles/tstr.h5
      ${HDF5_TOOLS_DIR}/testfiles/tstr2.h5
      ${HDF5_TOOLS_DIR}/testfiles/tudlink.h5
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes1.h5
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes2.h5
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes3.h5
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes4.h5
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes5.h5
      ${HDF5_TOOLS_DIR}/testfiles/tvlstr.h5
  )
  set (HDF5_XML_REFERENCE_FILES
      ${HDF5_TOOLS_DIR}/testfiles/tall.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tall-2A.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tarray1.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tarray2.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tarray3.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tarray6.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tarray7.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tattr.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tbitfields_be.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tbitfields_le.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tcompound_complex.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tcompound.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tcompound2.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tdatareg.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tdset.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tdset2.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tempty.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tempty-dtd.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tempty-dtd-2.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tempty-dtd-uri.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tempty-nons.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tempty-nons-2.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tempty-nons-uri.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tempty-ns.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tempty-ns-2.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tenum.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/test35.nc.xml
      ${HDF5_TOOLS_DIR}/testfiles/textlink.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tfpformat.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tgroup.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/thlink.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tloop.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tloop2.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tmany.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tname-amp.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tname-apos.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tnamed_dtype_attr.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tname-gt.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tname-lt.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tname-quot.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tname-sp.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tnestedcomp.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tnodata.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tnullspace.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tobjref.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/topaque.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/torderattr1.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/torderattr2.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/torderattr3.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/torderattr4.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tref.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tref-escapes.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tref-escapes-at.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tsaf.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tslink.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tstr.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tstr2.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tstring.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tstring-at.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tudlink.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes1.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes2.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes3.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes4.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes5.h5.xml
      ${HDF5_TOOLS_DIR}/testfiles/tvlstr.h5.xml
  )

  foreach (tst_xml_h5_file ${HDF5_XML_REFERENCE_TEST_FILES})
    get_filename_component(fname "${tst_xml_h5_file}" NAME)
    HDFTEST_COPY_FILE("${tst_xml_h5_file}" "${PROJECT_BINARY_DIR}/testfiles/xml/${fname}" "h5dump_xml_files")
  endforeach ()

  foreach (tst_xml_other_file ${HDF5_XML_REFERENCE_FILES})
    get_filename_component(fname "${tst_xml_other_file}" NAME)
    HDFTEST_COPY_FILE("${tst_xml_other_file}" "${PROJECT_BINARY_DIR}/testfiles/xml/${fname}" "h5dump_xml_files")
  endforeach ()
  add_custom_target(h5dump_xml_files ALL COMMENT "Copying files needed by h5dump_xml tests" DEPENDS ${h5dump_xml_files_list})

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

  macro (ADD_XML_SKIP_H5_TEST skipresultfile skipresultcode testtype)
    if ("${testtype}" STREQUAL "SKIP")
      if (NOT HDF5_ENABLE_USING_MEMCHECKER)
        add_test (
            NAME H5DUMP_XML-${skipresultfile}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${skipresultfile}.xml --xml ${ARGN}"
        )
        set_property(TEST H5DUMP_XML-${skipresultfile} PROPERTY DISABLED)
      endif ()
    else ()
      ADD_XML_H5_TEST (${skipresultfile} ${skipresultcode} ${ARGN})
    endif ()
  endmacro ()

  macro (ADD_XML_H5_TEST resultfile resultcode)
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5DUMP_XML-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump${tgt_ext}> --xml ${ARGN})
      set_tests_properties (H5DUMP_XML-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/xml")
      if (${resultcode})
        set_tests_properties (H5DUMP_XML-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
      if (last_xml_test)
        set_tests_properties (H5DUMP_XML-${resultfile} PROPERTIES DEPENDS ${last_xml_test})
      endif ()
    else ()
      add_test (
          NAME H5DUMP_XML-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=--xml;${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/xml"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.xml"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
  endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  ########## test XML
  ADD_XML_H5_TEST (tall.h5 0 tall.h5)
  ADD_XML_H5_TEST (tattr.h5 0 tattr.h5)
  if (H5_WORDS_BIGENDIAN)
    ADD_XML_H5_TEST (tbitfields_be.h5 0 tbitfields.h5)
  else ()
    ADD_XML_H5_TEST (tbitfields_le.h5 0 tbitfields.h5)
  endif ()
  ADD_XML_H5_TEST (tcompound.h5 0 tcompound.h5)
  ADD_XML_H5_TEST (tcompound2.h5 0 tcompound2.h5)
  ADD_XML_H5_TEST (tdatareg.h5 0 tdatareg.h5)
  ADD_XML_H5_TEST (tdset.h5 0 tdset.h5)
  ADD_XML_H5_TEST (tdset2.h5 0 tdset2.h5)
  ADD_XML_H5_TEST (tenum.h5 0 tenum.h5)
  ADD_XML_H5_TEST (tgroup.h5 0 tgroup.h5)
  ADD_XML_H5_TEST (thlink.h5 0 thlink.h5)
  ADD_XML_H5_TEST (tloop.h5 0 tloop.h5)
  ADD_XML_H5_TEST (tloop2.h5 0 tloop2.h5)
  ADD_XML_H5_TEST (tmany.h5 0 tmany.h5)
  ADD_XML_H5_TEST (tnestedcomp.h5 0 tnestedcomp.h5)
  ADD_XML_H5_TEST (tcompound_complex.h5 0 tcompound_complex.h5)
  ADD_XML_H5_TEST (tobjref.h5 0 tobjref.h5)
  ADD_XML_H5_TEST (topaque.h5 0 topaque.h5)
  ADD_XML_H5_TEST (tslink.h5 0 tslink.h5)
  ADD_XML_H5_TEST (tudlink.h5 0 tudlink.h5)
  ADD_XML_H5_TEST (textlink.h5 0 textlink.h5)
  ADD_XML_H5_TEST (tstr.h5 0 tstr.h5)
  ADD_XML_H5_TEST (tstr2.h5 0 tstr2.h5)
  ADD_XML_H5_TEST (tref.h5 0 tref.h5)
  ADD_XML_H5_TEST (tname-amp.h5 0 tname-amp.h5)
  ADD_XML_H5_TEST (tname-apos.h5 0 tname-apos.h5)
  ADD_XML_H5_TEST (tname-gt.h5 0 tname-gt.h5)
  ADD_XML_H5_TEST (tname-lt.h5 0 tname-lt.h5)
  ADD_XML_H5_TEST (tname-quot.h5 0 tname-quot.h5)
  ADD_XML_H5_TEST (tname-sp.h5 0 tname-sp.h5)
  ADD_XML_H5_TEST (tstring.h5 0 tstring.h5)
  ADD_XML_H5_TEST (tstring-at.h5 0 tstring-at.h5)
  ADD_XML_H5_TEST (tref-escapes.h5 0 tref-escapes.h5)
  ADD_XML_H5_TEST (tref-escapes-at.h5 0 tref-escapes-at.h5)
  ADD_XML_H5_TEST (tnodata.h5 0 tnodata.h5)
  ADD_XML_H5_TEST (tarray1.h5 0 tarray1.h5)
  ADD_XML_H5_TEST (tarray2.h5 0 tarray2.h5)
  ADD_XML_H5_TEST (tarray3.h5 0 tarray3.h5)
  ADD_XML_H5_TEST (tarray6.h5 0 tarray6.h5)
  ADD_XML_H5_TEST (tarray7.h5 0 tarray7.h5)
  ADD_XML_H5_TEST (tvldtypes1.h5 0 tvldtypes1.h5)
  ADD_XML_H5_TEST (tvldtypes2.h5 0 tvldtypes2.h5)
  ADD_XML_H5_TEST (tvldtypes3.h5 0 tvldtypes3.h5)
  ADD_XML_H5_TEST (tvldtypes4.h5 0 tvldtypes4.h5)
  ADD_XML_H5_TEST (tvldtypes5.h5 0 tvldtypes5.h5)
  ADD_XML_H5_TEST (tvlstr.h5 0 tvlstr.h5)
  ADD_XML_H5_TEST (tsaf.h5 0 tsaf.h5)
  ADD_XML_H5_TEST (tempty.h5 0 tempty.h5)
  ADD_XML_H5_TEST (tnamed_dtype_attr.h5 0 tnamed_dtype_attr.h5)
  ADD_XML_H5_TEST (tnullspace.h5 0 tnullspace.h5)
  ## So is dataspace with 0 dimension size.
  ##  ADD_XML_H5_TEST (zerodim.h5 0 zerodim.h5)

  # other options for xml

  ADD_XML_H5_TEST (tempty-dtd.h5 0 --use-dtd tempty.h5)
  ADD_XML_H5_TEST (tempty-dtd-2.h5 0 -u tempty.h5)

  ADD_XML_H5_TEST (tempty-nons-2.h5 0 --xml-ns=: tempty.h5)

  ## Some of these combinations are syntactically correct but
  ##  the URLs are dummies
  ADD_XML_H5_TEST (tempty-ns.h5 0 -X thing: tempty.h5)
  ADD_XML_H5_TEST (tempty-ns-2.h5 0 --xml-ns=thing: tempty.h5)
  ADD_XML_H5_TEST (tempty-nons-uri.h5 0 --xml-ns=: --xml-dtd=http://somewhere.net tempty.h5)
  ADD_XML_H5_TEST (tempty-dtd-uri.h5 0 --use-dtd --xml-dtd=http://somewhere.net tempty.h5)

  ADD_XML_H5_TEST (tall-2A.h5 0 -A tall.h5)


  # tests for attribute order
  ADD_XML_H5_TEST (torderattr1.h5 0 -H --sort_by=name --sort_order=ascending torderattr.h5)
  ADD_XML_H5_TEST (torderattr2.h5 0 -H --sort_by=name --sort_order=descending torderattr.h5)
  ADD_XML_H5_TEST (torderattr3.h5 0 -H --sort_by=creation_order --sort_order=ascending torderattr.h5)
  ADD_XML_H5_TEST (torderattr4.h5 0 -H --sort_by=creation_order --sort_order=descending torderattr.h5)

  # tests for floating point user defined printf format
  ADD_XML_H5_TEST (tfpformat.h5 0 -u -m %.7f tfpformat.h5)

  # test for HDFFV-10256 issue
  ADD_XML_H5_TEST (test35.nc 0 test35.nc)

