#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
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
  set (HDF5_XML_REFERENCE_ONLY_FILES
      tbitfields.h5
      tcompound2.h5
      tdset2.h5
      tenum.h5
      test35.nc
      tloop2.h5
      tmany.h5
      tname-amp.h5
      tname-apos.h5
      tname-gt.h5
      tname-lt.h5
      tname-quot.h5
      tname-sp.h5
      tnodata.h5
      tobjref.h5
      topaque.h5
      tref.h5
      tref-escapes.h5
      tref-escapes-at.h5
      tstring.h5
      tstring-at.h5
  )

  set (HDF5_XML_REFERENCE_TEST_FILES
      tall.h5
      tarray1.h5
      tarray2.h5
      tarray3.h5
      tarray6.h5
      tarray7.h5
      tattr.h5
      tcompound.h5
      tcompound_complex.h5
      tdatareg.h5
      tdset.h5
      tempty.h5
      textlink.h5
      tfloat16.h5
      tfloat16_be.h5
      tfpformat.h5
      tgroup.h5
      thlink.h5
      tloop.h5
      tnamed_dtype_attr.h5
      tnestedcomp.h5
      tnullspace.h5
      torderattr.h5
      tsaf.h5
      tslink.h5
      tstr.h5
      tstr2.h5
      tudlink.h5
      tvldtypes1.h5
      tvldtypes2.h5
      tvldtypes3.h5
      tvldtypes4.h5
      tvldtypes5.h5
      tvlstr.h5
  )
  set (HDF5_XML_REFERENCE_FILES
      tall.h5.xml
      tall-2A.h5.xml
      tarray1.h5.xml
      tarray2.h5.xml
      tarray3.h5.xml
      tarray6.h5.xml
      tarray7.h5.xml
      tattr.h5.xml
      tbitfields_be.h5.xml
      tbitfields_le.h5.xml
      tcompound_complex.h5.xml
      tcompound.h5.xml
      tcompound2.h5.xml
      tdatareg.h5.xml
      tdset.h5.xml
      tdset2.h5.xml
      tempty.h5.xml
      tempty-dtd.h5.xml
      tempty-dtd-2.h5.xml
      tempty-dtd-uri.h5.xml
      tempty-nons.h5.xml
      tempty-nons-2.h5.xml
      tempty-nons-uri.h5.xml
      tempty-ns.h5.xml
      tempty-ns-2.h5.xml
      tenum.h5.xml
      test35.nc.xml
      textlink.h5.xml
      tfloat16.h5.xml
      tfloat16_be.h5.xml
      tfpformat.h5.xml
      tgroup.h5.xml
      thlink.h5.xml
      tloop.h5.xml
      tloop2.h5.xml
      tmany.h5.xml
      tname-amp.h5.xml
      tname-apos.h5.xml
      tnamed_dtype_attr.h5.xml
      tname-gt.h5.xml
      tname-lt.h5.xml
      tname-quot.h5.xml
      tname-sp.h5.xml
      tnestedcomp.h5.xml
      tnodata.h5.xml
      tnullspace.h5.xml
      tobjref.h5.xml
      topaque.h5.xml
      torderattr1.h5.xml
      torderattr2.h5.xml
      torderattr3.h5.xml
      torderattr4.h5.xml
      tref.h5.xml
      tref-escapes.h5.xml
      tref-escapes-at.h5.xml
      tsaf.h5.xml
      tslink.h5.xml
      tstr.h5.xml
      tstr2.h5.xml
      tstring.h5.xml
      tstring-at.h5.xml
      tudlink.h5.xml
      tvldtypes1.h5.xml
      tvldtypes2.h5.xml
      tvldtypes3.h5.xml
      tvldtypes4.h5.xml
      tvldtypes5.h5.xml
      tvlstr.h5.xml
  )

  foreach (tst_xml_h5_file ${HDF5_XML_REFERENCE_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${tst_xml_h5_file}" "${PROJECT_BINARY_DIR}/testfiles/xml/${tst_xml_h5_file}" "h5dump_xml_files")
  endforeach ()

  foreach (tst_xmlonly_h5_file ${HDF5_XML_REFERENCE_ONLY_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/xml/${tst_xmlonly_h5_file}" "${PROJECT_BINARY_DIR}/testfiles/xml/${tst_xmlonly_h5_file}" "h5dump_xml_files")
  endforeach ()

  foreach (tst_xml_other_file ${HDF5_XML_REFERENCE_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/xml/${tst_xml_other_file}" "${PROJECT_BINARY_DIR}/testfiles/xml/${tst_xml_other_file}" "h5dump_xml_files")
  endforeach ()
  add_custom_target(h5dump_xml_files ALL COMMENT "Copying files needed by h5dump_xml tests" DEPENDS ${h5dump_xml_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  macro (ADD_XML_SKIP_H5_TEST skipresultfile skipresultcode testtype)
    if ("${testtype}" STREQUAL "SKIP")
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5DUMP_XML-${skipresultfile}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${skipresultfile}.xml --xml ${ARGN}"
        )
        set_property(TEST H5DUMP_XML-${skipresultfile} PROPERTY DISABLED true)
      endif ()
    else ()
      ADD_XML_H5_TEST (${skipresultfile} ${skipresultcode} ${ARGN})
    endif ()
  endmacro ()

  macro (ADD_XML_H5_TEST resultfile resultcode)
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME H5DUMP_XML-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump> --xml ${ARGN})
      if (${resultcode})
        set_tests_properties (H5DUMP_XML-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
    else ()
      add_test (
          NAME H5DUMP_XML-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=--xml;${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/xml"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.xml"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (H5DUMP_XML-${resultfile} PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/xml"
    )
    if ("H5DUMP_XML-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5DUMP_XML-${resultfile} PROPERTIES DISABLED true)
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

  # Add tests for _Float16 type
  ADD_XML_H5_TEST (tfloat16.h5 0 tfloat16.h5)
  ADD_XML_H5_TEST (tfloat16_be.h5 0 tfloat16_be.h5)

  # tests for floating point user defined printf format
  ADD_XML_H5_TEST (tfpformat.h5 0 -u -m %.7f tfpformat.h5)

  # test for HDFFV-10256 issue
  ADD_XML_H5_TEST (test35.nc 0 test35.nc)

