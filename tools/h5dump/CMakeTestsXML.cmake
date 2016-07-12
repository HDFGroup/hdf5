
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

  #
  # copy XML test files from source dir to test dir
  #
  set (HDF5_XML_REFERENCE_TEST_FILES
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tall.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray1.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray2.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray3.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray6.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray7.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattr.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tbitfields.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcompound.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcompound2.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcompound_complex.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tdatareg.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tdset.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tdset2.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tempty.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tenum.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/textlink.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfpformat.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tgroup.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/thlink.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tloop.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tloop2.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tmany.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tname-amp.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tname-apos.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tname-gt.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tname-lt.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tname-quot.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tname-sp.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnamed_dtype_attr.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnestedcomp.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnodata.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tobjref.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/topaque.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/torderattr.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tref.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tref-escapes.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tref-escapes-at.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tsaf.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tslink.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstring.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstring-at.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstr.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstr2.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tudlink.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes1.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes2.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes3.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes4.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes5.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvlstr.h5
  )
  set (HDF5_XML_REFERENCE_FILES
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tall.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tall-2A.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray1.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray2.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray3.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray6.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray7.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tattr.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tbitfields.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcompound_complex.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcompound.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcompound2.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tdatareg.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tdset.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tdset2.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tempty.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tempty-dtd.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tempty-dtd-2.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tempty-dtd-uri.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tempty-nons.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tempty-nons-2.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tempty-nons-uri.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tempty-ns.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tempty-ns-2.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tenum.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/textlink.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfpformat.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tgroup.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/thlink.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tloop.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tloop2.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tmany.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tname-amp.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tname-apos.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnamed_dtype_attr.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tname-gt.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tname-lt.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tname-quot.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tname-sp.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnestedcomp.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnodata.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tobjref.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/topaque.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/torderattr1.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/torderattr2.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/torderattr3.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/torderattr4.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tref.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tref-escapes.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tref-escapes-at.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tsaf.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tslink.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstr.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstr2.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstring.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tstring-at.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tudlink.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes1.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes2.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes3.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes4.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvldtypes5.h5.xml
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tvlstr.h5.xml
  )

  foreach (tst_xml_h5_file ${HDF5_XML_REFERENCE_TEST_FILES})
    get_filename_component(fname "${tst_xml_h5_file}" NAME)
    HDFTEST_COPY_FILE("${tst_xml_h5_file}" "${PROJECT_BINARY_DIR}/testfiles/xml/${fname}" "h5dump_xml_files")
  endforeach (tst_xml_h5_file ${HDF5_XML_REFERENCE_TEST_FILES})

  foreach (tst_xml_other_file ${HDF5_XML_REFERENCE_FILES})
    get_filename_component(fname "${tst_xml_other_file}" NAME)
    HDFTEST_COPY_FILE("${tst_xml_other_file}" "${PROJECT_BINARY_DIR}/testfiles/xml/${fname}" "h5dump_xml_files")
  endforeach (tst_xml_other_file ${HDF5_XML_REFERENCE_FILES})
  add_custom_target(h5dump_xml_files ALL COMMENT "Copying files needed by h5dump_xml tests" DEPENDS ${h5dump_xml_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_XML_SKIP_H5_TEST skipresultfile skipresultcode testtype)
    if (${testtype} STREQUAL "SKIP")
      if (NOT HDF5_ENABLE_USING_MEMCHECKER)
        add_test (
            NAME H5DUMP-XML-${skipresultfile}-SKIPPED
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${skipresultfile}.xml --xml ${ARGN}"
        )
      endif (NOT HDF5_ENABLE_USING_MEMCHECKER)
    else (${testtype} STREQUAL "SKIP")
      ADD_XML_H5_TEST (${skipresultfile} ${skipresultcode} ${ARGN})
    endif (${testtype} STREQUAL "SKIP")
  ENDMACRO (ADD_XML_SKIP_H5_TEST)

  MACRO (ADD_XML_H5_TEST resultfile resultcode)
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5DUMP-XML-${resultfile} COMMAND $<TARGET_FILE:h5dump> --xml ${ARGN})
      set_tests_properties (H5DUMP-XML-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/xml")
      if (NOT ${resultcode} STREQUAL "0")
        set_tests_properties (H5DUMP-XML-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
      if (NOT "${last_xml_test}" STREQUAL "")
        set_tests_properties (H5DUMP-XML-${resultfile} PROPERTIES DEPENDS ${last_xml_test})
      endif ()
    else (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5DUMP-XML-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove ${resultfile}.out ${resultfile}.out.err
      )
      set_tests_properties (H5DUMP-XML-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/xml")
      add_test (
          NAME H5DUMP-XML-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=--xml;${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/xml"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.xml"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-XML-${resultfile} PROPERTIES DEPENDS "H5DUMP-XML-${resultfile}-clear-objects")
    endif (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_XML_H5_TEST file)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  if (HDF5_ENABLE_USING_MEMCHECKER)
    # Remove any output file left over from previous test run
    add_test (
      NAME H5DUMP-XML-clearall-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove
          tall.h5.out
          tall.h5.out.err
          tall-2A.h5.out
          tall-2A.h5.out.err
          tarray1.h5.out
          tarray1.h5.out.err
          tarray2.h5.out
          tarray2.h5.out.err
          tarray3.h5.out
          tarray3.h5.out.err
          tarray6.h5.out
          tarray6.h5.out.err
          tarray7.h5.out
          tarray7.h5.out.err
          tattr.h5.out
          tattr.h5.out.err
          tbitfields.h5.out
          tbitfields.h5.out.err
          tcompound.h5.out
          tcompound.h5.out.err
          tcompound2.h5.out
          tcompound2.h5.out.err
          tcompound_complex.h5.out
          tcompound_complex.h5.out.err
          tdatareg.h5.out
          tdatareg.h5.out.err
          tdset.h5.out
          tdset.h5.out.err
          tdset2.h5.out
          tdset2.h5.out.err
          tempty-dtd-2.h5.out
          tempty-dtd-2.h5.out.err
          tempty-dtd-uri.h5.out
          tempty-dtd-uri.h5.out.err
          tempty-dtd.h5.out
          tempty-dtd.h5.out.err
          tempty-nons-2.h5.out
          tempty-nons-2.h5.out.err
          tempty-nons-uri.h5.out
          tempty-nons-uri.h5.out.err
          tempty-nons.h5.out
          tempty-nons.h5.out.err
          tempty-ns-2.h5.out
          tempty-ns-2.h5.out.err
          tempty-ns.h5.out
          tempty-ns.h5.out.err
          tempty.h5.out
          tempty.h5.out.err
          tenum.h5.out
          tenum.h5.out.err
          textlink.h5.out
          textlink.h5.out.err
          tfpformat.h5.out
          tfpformat.h5.out.err
          tgroup.h5.out
          tgroup.h5.out.err
          thlink.h5.out
          thlink.h5.out.err
          tloop.h5.out
          tloop.h5.out.err
          tloop2.h5.out
          tloop2.h5.out.err
          tmany.h5.out
          tmany.h5.out.err
          tname-amp.h5.out
          tname-amp.h5.out.err
          tname-apos.h5.out
          tname-apos.h5.out.err
          tname-gt.h5.out
          tname-gt.h5.out.err
          tname-lt.h5.out
          tname-lt.h5.out.err
          tname-quot.h5.out
          tname-quot.h5.out.err
          tname-sp.h5.out
          tname-sp.h5.out.err
          tnamed_dtype_attr.h5.out
          tnamed_dtype_attr.h5.out.err
          tnestedcomp.h5.out
          tnestedcomp.h5.out.err
          tnodata.h5.out
          tnodata.h5.out.err
          tnoname.h5.out
          tnoname.h5.out.err
          tobjref.h5.out
          tobjref.h5.out.err
          topaque.h5.out
          topaque.h5.out.err
          torderattr1.h5.out
          torderattr1.h5.out.err
          torderattr2.h5.out
          torderattr2.h5.out.err
          torderattr3.h5.out
          torderattr3.h5.out.err
          torderattr4.h5.out
          torderattr4.h5.out.err
          tref-escapes-at.h5.out
          tref-escapes-at.h5.out.err
          tref-escapes.h5.out
          tref-escapes.h5.out.err
          tref.h5.out
          tref.h5.out.err
          tsaf.h5.out
          tsaf.h5.out.err
          tslink.h5.out
          tslink.h5.out.err
          tstr.h5.out
          tstr.h5.out.err
          tstr2.h5.out
          tstr2.h5.out.err
          tstring.h5.out
          tstring.h5.out.err
          tstring-at.h5.out
          tstring-at.h5.out.err
          tudlink.h5.out
          tudlink.h5.out.err
          tvldtypes1.h5.out
          tvldtypes1.h5.out.err
          tvldtypes2.h5.out
          tvldtypes2.h5.out.err
          tvldtypes3.h5.out
          tvldtypes3.h5.out.err
          tvldtypes4.h5.out
          tvldtypes4.h5.out.err
          tvldtypes5.h5.out
          tvldtypes5.h5.out.err
          tvlstr.h5.out
          tvlstr.h5.out.err
    )
    set_tests_properties (H5DUMP-XML-clearall-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/xml")
    if (NOT "${last_xml_test}" STREQUAL "")
      set_tests_properties (H5DUMP-XML-clearall-objects PROPERTIES DEPENDS ${last_xml_test})
    endif (NOT "${last_xml_test}" STREQUAL "")
    set (last_test "H5DUMP-XML-clearall-objects")
  endif (HDF5_ENABLE_USING_MEMCHECKER)

  ########## test XML
  ADD_XML_H5_TEST (tall.h5 0 tall.h5)
  ADD_XML_H5_TEST (tattr.h5 0 tattr.h5)
  ADD_XML_H5_TEST (tbitfields.h5 0 tbitfields.h5)
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
  ##Test dataset and attribute of null space.  Commented out:
  ## wait until the XML schema is updated for null space.
  ##  ADD_XML_H5_TEST (tnullspace.h5 0 tnulspace.h5)
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

