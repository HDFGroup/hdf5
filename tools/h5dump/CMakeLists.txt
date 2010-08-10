cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_H5DUMP)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SOURCE_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

# --------------------------------------------------------------------
# Add the h5dump and test executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5dump ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump.c)
H5_NAMING (h5dump)
IF (WIN32)
  IF (MSVC)
    IF (NOT BUILD_SHARED_LIBS)
      SET_TARGET_PROPERTIES (h5dump
          PROPERTIES
              LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
      ) 
    ENDIF (NOT BUILD_SHARED_LIBS)
  ENDIF (MSVC)
ENDIF (WIN32)
TARGET_LINK_LIBRARIES (h5dump  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES h5dump)

IF (BUILD_TESTING)
  IF (NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (h5dumpgentest ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dumpgentest.c)
    H5_NAMING (h5dumpgentest)
    IF (WIN32)
      IF (MSVC)
        SET_TARGET_PROPERTIES (h5dumpgentest
            PROPERTIES
                LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
        ) 
      ENDIF (MSVC)
    ENDIF (WIN32)
    TARGET_LINK_LIBRARIES (h5dumpgentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    
    #ADD_TEST (NAME h5dumpgentest COMMAND $<TARGET_FILE:h5dumpgentest>)

    SET (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES}
    #    h5dumpgentest
    )
  ENDIF (NOT BUILD_SHARED_LIBS)
  
    #-- Copy all the HDF5 files from the test directory into the source directory
  SET (HDF5_REFERENCE_FILES
      tall-1.ddl
      tall-2.ddl
      tall-2A.ddl
      tall-2B.ddl
      tall-3.ddl
      tall-4s.ddl
      tall-5s.ddl
      tallfilters.ddl
      tarray1.ddl
      tarray2.ddl
      tarray3.ddl
      tarray4.ddl
      tarray5.ddl
      tarray6.ddl
      tarray7.ddl
      tarray8.ddl
      tattr-1.ddl
      tattr-2.ddl
      tattr-3.ddl
      tattrreg.ddl
      tattrregR.ddl
      tbigdims.ddl
      tboot1.ddl
      tboot2.ddl
      tchar1.ddl
      tchunked.ddl
      tcomp-1.ddl
      tcomp-2.ddl
      tcomp-3.ddl
      tcomp-4.ddl
      tcompact.ddl
      tcontents.ddl
      tcontiguos.ddl
      tdatareg.ddl
      tdataregR.ddl
      tdeflate.ddl
      tdset-1.ddl
      tdset-2.ddl
      tdset-3s.ddl
      tempty.ddl
      texternal.ddl
      textlinksrc.ddl
      textlinkfar.ddl
      tfamily.ddl
      tfill.ddl
      tfletcher32.ddl
      tfpformat.ddl
      tgroup-1.ddl
      tgroup-2.ddl
      tgrp_comments.ddl
      thlink-1.ddl
      thlink-2.ddl
      thlink-3.ddl
      thlink-4.ddl
      thlink-5.ddl
      thyperslab.ddl
      tindicesno.ddl
      tindicessub1.ddl
      tindicessub2.ddl
      tindicessub3.ddl
      tindicessub4.ddl
      tindicesyes.ddl
      tlarge_objname.ddl
      #tldouble.ddl
      tlonglinks.ddl
      tloop-1.ddl
      tmulti.ddl
      tnamed_dtype_attr.ddl
      tnestcomp-1.ddl
      tnbit.ddl
      tnofilename.ddl
      tnullspace.ddl
      tordergr1.ddl
      tordergr2.ddl
      tordergr3.ddl
      tordergr4.ddl
      tordergr5.ddl
      torderattr1.ddl
      torderattr2.ddl
      torderattr3.ddl
      torderattr4.ddl
      tperror.ddl
      treference.ddl
      tsaf.ddl
      tscaleoffset.ddl
      tshuffle.ddl
      tslink-1.ddl
      tslink-2.ddl
      tsplit_file.ddl
      tstr-1.ddl
      tstr-2.ddl
      tstring.ddl
      tstring2.ddl
      tstringe.ddl
      tszip.ddl
      tudlink-1.ddl
      tudlink-2.ddl
      tuserfilter.ddl
      tvldtypes1.ddl
      tvldtypes2.ddl
      tvldtypes3.ddl
      tvldtypes4.ddl
      tvldtypes5.ddl
      tvlstr.ddl
      tvms.ddl
  )
  SET (HDF5_REFERENCE_TEST_FILES
      tbin1.ddl
      tbin1.ddl
      tbin2.ddl
      tbin3.ddl
      tbin4.ddl
      out3.h5import
      taindices.h5
      tall.h5
      tarray1.h5
      tarray2.h5
      tarray3.h5
      tarray4.h5
      tarray5.h5
      tarray6.h5
      tarray7.h5
      tarray8.h5
      tattr.h5
      tattr2.h5
      tattrreg.h5
      tbigdims.h5
      tbinary.h5
      tchar.h5
      tcompound.h5
      tcompound_complex.h5
      tdatareg.h5
      tdset.h5
      tempty.h5
      textlinkfar.h5
      textlinksrc.h5
      textlinktar.h5
      tfamily00000.h5
      tfamily00001.h5
      tfamily00002.h5
      tfamily00003.h5
      tfamily00004.h5
      tfamily00005.h5
      tfamily00006.h5
      tfamily00007.h5
      tfamily00008.h5
      tfamily00009.h5
      tfamily00010.h5
      tfcontents1.h5
      tfcontents2.h5
      tfilters.h5
      tfpformat.h5
      tfvalues.h5
      tgroup.h5
      tgrp_comments.h5
      thlink.h5
      thyperslab.h5
      tlarge_objname.h5
      #tldouble.h5
      tlonglinks.h5
      tloop.h5
      tmulti-b.h5
      tmulti-g.h5
      tmulti-l.h5
      tmulti-o.h5
      tmulti-r.h5
      tmulti-s.h5
      tnamed_dtype_attr.h5
      tnestedcomp.h5
      tnullspace.h5
      torderattr.h5
      tordergr.h5
      tsaf.h5
      tslink.h5
      tsplit_file-m.h5
      tsplit_file-r.h5
      tstr.h5
      tstr2.h5
      tstr3.h5
      tudlink.h5
      tvldtypes1.h5
      tvldtypes2.h5
      tvldtypes3.h5
      tvldtypes4.h5
      tvldtypes5.h5
      tvlstr.h5
      tvms.h5
  )

  FOREACH (ddl_file ${HDF5_REFERENCE_FILES})
    SET (ddldest "${PROJECT_BINARY_DIR}/${ddl_file}")
    #MESSAGE (STATUS " Translating ${ddl_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${XLATE_UTILITY}
        ARGS       ${HDF5_TOOLS_SOURCE_DIR}/testfiles/${ddl_file} ${ddldest} -l3
    )
  ENDFOREACH (ddl_file ${HDF5_REFERENCE_FILES})

  FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/${h5_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_SOURCE_DIR}/testfiles/${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
  
  IF (HDF5_USE_H5DUMP_PACKED_BITS)
    #-- Copy all the HDF5 files from the test directory into the source directory
    SET (HDF5_REFERENCE_PBITS
        tnofilename-with-packed-bits.ddl
        tpbitsArray.ddl
        tpbitsCompound.ddl
        tpbitsIncomplete.ddl
        tpbitsLengthExceeded.ddl
        tpbitsLengthPositive.ddl
        tpbitsMax.ddl
        tpbitsMaxExceeded.ddl
        tpbitsOffsetExceeded.ddl
        tpbitsOffsetNegative.ddl
        tpbitsOverlapped.ddl
        tpbitsSigned.ddl
        tpbitsUnsigned.ddl
    )
    SET (HDF5_REFERENCE_TEST_PBITS
        packedbits.h5
    )

    FOREACH (ddl_pbits ${HDF5_REFERENCE_PBITS})
      SET (ddldest "${PROJECT_BINARY_DIR}/${ddl_pbits}")
      #MESSAGE (STATUS " Translating ${ddl_pbits}")
      ADD_CUSTOM_COMMAND (
          TARGET     h5dump
          POST_BUILD
          COMMAND    ${XLATE_UTILITY}
          ARGS       ${HDF5_TOOLS_SOURCE_DIR}/testfiles/${ddl_pbits} ${ddldest} -l3
      )
    ENDFOREACH (ddl_pbits ${HDF5_REFERENCE_PBITS})

    FOREACH (h5_pbits ${HDF5_REFERENCE_TEST_PBITS})
      SET (dest "${PROJECT_BINARY_DIR}/${h5_pbits}")
      #MESSAGE (STATUS " Copying ${h5_pbits}")
      ADD_CUSTOM_COMMAND (
          TARGET     h5dump
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${HDF5_TOOLS_SOURCE_DIR}/testfiles/${h5_pbits} ${dest}
      )
    ENDFOREACH (h5_pbits ${HDF5_REFERENCE_TEST_PBITS})

  ENDIF (HDF5_USE_H5DUMP_PACKED_BITS)
  
  ADD_CUSTOM_COMMAND (
      TARGET     h5dump
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${HDF5_TOOLS_SOURCE_DIR}/testfiles/tbin1.ddl ${PROJECT_BINARY_DIR}/tbin1LE.ddl
  )
  
  ADD_CUSTOM_COMMAND (
      TARGET     h5dump
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${HDF5_TOOLS_SOURCE_DIR}/testfiles/tbinregR.exp ${PROJECT_BINARY_DIR}/tbinregR.exp
  )
  
  MACRO (ADD_SKIP_H5_TEST skipresultfile skipresultcode testtype)
    IF (${testtype} STREQUAL "SKIP")
      MESSAGE (STATUS "SKIP ${skipresultfile} ${ARGN}")
    ELSE (${testtype} STREQUAL "SKIP")
      ADD_H5_TEST (${skipresultfile} ${skipresultcode} ${ARGN})
    ENDIF (${testtype} STREQUAL "SKIP")
  ENDMACRO (ADD_SKIP_H5_TEST)

  MACRO (ADD_H5_TEST resultfile resultcode)
    ADD_TEST (
        NAME H5DUMP-${resultfile}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_OUTPUT=${resultfile}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=${resultfile}.ddl"
            -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
    )
  ENDMACRO (ADD_H5_TEST file)

  MACRO (ADD_H5_EXPORT_TEST resultfile targetfile resultcode)
    ADD_TEST (
        NAME H5DUMP-output-${resultfile}
        COMMAND $<TARGET_FILE:h5dump> -d /Dataset1 -s 0 -R -y -o ${PROJECT_BINARY_DIR}/${resultfile}.txt ${PROJECT_BINARY_DIR}/${targetfile}
    )
    IF (WIN32 AND NOT MINGW)
      FILE (READ ${PROJECT_BINARY_DIR}/${resultfile}.exp TEST_STREAM)
      FILE (WRITE ${PROJECT_BINARY_DIR}/${resultfile}.exp "${TEST_STREAM}")
    ENDIF (WIN32 AND NOT MINGW)
    ADD_TEST (
        NAME H5DUMP-output-cmp-${resultfile}
        COMMAND ${CMAKE_COMMAND}
              -E compare_files ${PROJECT_BINARY_DIR}/${resultfile}.txt ${PROJECT_BINARY_DIR}/${resultfile}.exp
    )
  ENDMACRO (ADD_H5_EXPORT_TEST file)

  # Determine which filters are available
  SET (USE_FILTER_SZIP H5_HAVE_FILTER_SZIP)
  SET (USE_FILTER_DEFLATE H5_HAVE_FILTER_DEFLATE)
  SET (USE_FILTER_SHUFFLE H5_HAVE_FILTER_SHUFFLE)
  SET (USE_FILTER_FLETCHER32 H5_HAVE_FILTER_FLETCHER32)
  SET (USE_FILTER_NBIT H5_HAVE_FILTER_NBIT)
  SET (USE_FILTER_SCALEOFFSET H5_HAVE_FILTER_SCALEOFFSET)

# Determine if H5dump packed bits feature is included
  SET (USE_PACKED_BITS HDF5_USE_H5DUMP_PACKED_BITS)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################
  # test for displaying groups
  ADD_H5_TEST (tgroup-1 0 tgroup.h5)
  # test for displaying the selected groups
  ADD_H5_TEST (tgroup-2 1 --group=/g2 --group / -g /y tgroup.h5)

  # test for displaying simple space datasets
  ADD_H5_TEST (tdset-1 0 tdset.h5)
  # test for displaying selected datasets
  ADD_H5_TEST (tdset-2 1 -H -d dset1 -d /dset2 --dataset=dset3 tdset.h5)

  # test for displaying attributes
  ADD_H5_TEST (tattr-1 0 tattr.h5)
  # test for displaying the selected attributes of string type and scalar space
  ADD_H5_TEST (tattr-2 0 -a /attr1 --attribute /attr4 --attribute=/attr5 tattr.h5)
  # test for header and error messages
  ADD_H5_TEST (tattr-3 1 --header -a /attr2 --attribute=/attr tattr.h5)
  # test for displaying attributes in shared datatype (also in group and dataset)
  ADD_H5_TEST (tnamed_dtype_attr 0 tnamed_dtype_attr.h5)

  # test for displaying soft links and user-defined links
  ADD_H5_TEST (tslink-1 0 tslink.h5)
  ADD_H5_TEST (tudlink-1 0 tudlink.h5)
  # test for displaying the selected link
  ADD_H5_TEST (tslink-2 0 -l slink2 tslink.h5)
  ADD_H5_TEST (tudlink-2 0 -l udlink2 tudlink.h5)

  # tests for hard links
  ADD_H5_TEST (thlink-1 0 thlink.h5)
  ADD_H5_TEST (thlink-2 0 -d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 thlink.h5)
  ADD_H5_TEST (thlink-3 0 -d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 thlink.h5)
  ADD_H5_TEST (thlink-4 0 -g /g1 thlink.h5)
  ADD_H5_TEST (thlink-5 0 -d /dset1 -g /g2 -d /g1/dset2 thlink.h5)

  # tests for compound data types
  ADD_H5_TEST (tcomp-1 0 tcompound.h5)
  # test for named data types
  ADD_H5_TEST (tcomp-2 0 -t /type1 --datatype /type2 --datatype=/group1/type3 tcompound.h5)
  # test for unamed type 
  #ADD_H5_TEST (tcomp-3 0 -t /#6632 -g /group2 tcompound.h5)
  # test complicated compound datatype
  ADD_H5_TEST (tcomp-4 0 tcompound_complex.h5)

  #test for the nested compound type
  ADD_H5_TEST (tnestcomp-1 0 tnestedcomp.h5)

  # test for options
  ADD_H5_TEST (tall-1 0 tall.h5)
  ADD_H5_TEST (tall-2 0 --header -g /g1/g1.1 -a attr2 tall.h5)
  ADD_H5_TEST (tall-3 0 -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5)

  # test for loop detection
  ADD_H5_TEST (tloop-1 0 tloop.h5)

  # test for string 
  ADD_H5_TEST (tstr-1 0 tstr.h5)
  ADD_H5_TEST (tstr-2 0 tstr2.h5)

  # test for file created by Lib SAF team
  ADD_H5_TEST (tsaf 0 tsaf.h5)

  # test for file with variable length data
  ADD_H5_TEST (tvldtypes1 0 tvldtypes1.h5)
  ADD_H5_TEST (tvldtypes2 0 tvldtypes2.h5)
  ADD_H5_TEST (tvldtypes3 0 tvldtypes3.h5)
  ADD_H5_TEST (tvldtypes4 0 tvldtypes4.h5)
  ADD_H5_TEST (tvldtypes5 0 tvldtypes5.h5)

  #test for file with variable length string data
  ADD_H5_TEST (tvlstr 0 tvlstr.h5)

  # test for files with array data
  ADD_H5_TEST (tarray1 0 tarray1.h5)
  ADD_H5_TEST (tarray2 0 tarray2.h5)
  ADD_H5_TEST (tarray3 0 tarray3.h5)
  ADD_H5_TEST (tarray4 0 tarray4.h5)
  ADD_H5_TEST (tarray5 0 tarray5.h5)
  ADD_H5_TEST (tarray6 0 tarray6.h5)
  ADD_H5_TEST (tarray7 0 tarray7.h5)
  ADD_H5_TEST (tarray8 0 tarray8.h5)

  # test for files with empty data
  ADD_H5_TEST (tempty 0 tempty.h5)

  # test for files with groups that have comments
  ADD_H5_TEST (tgrp_comments 0 tgrp_comments.h5)

  # test the --filedriver flag
  ADD_H5_TEST (tsplit_file 0 --filedriver=split tsplit_file)
  ADD_H5_TEST (tfamily 0 --filedriver=family tfamily%05d.h5)
  ADD_H5_TEST (tmulti 0 --filedriver=multi tmulti)

  # test for files with group names which reach > 1024 bytes in size
  ADD_H5_TEST (tlarge_objname 0 -w157 tlarge_objname.h5)

  # test '-A' to suppress data but print attr's
  ADD_H5_TEST (tall-2A 0 -A tall.h5)

  # test '-r' to print attributes in ASCII instead of decimal
  ADD_H5_TEST (tall-2B 0 -A -r tall.h5)

  # test Subsetting
  ADD_H5_TEST (tall-4s 0 --dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 tall.h5)
  ADD_H5_TEST (tall-5s 0 -d "/g1/g1.1/dset1.1.2[0;2;10;]" tall.h5)
  ADD_H5_TEST (tdset-3s 0 -d "/dset1[1,1;;;]" tdset.h5)

  # test printing characters in ASCII instead of decimal
  ADD_H5_TEST (tchar1 0 -r tchar.h5)

  # test failure handling
  # Missing file name
  IF (${USE_PACKED_BITS})
    ADD_H5_TEST (tnofilename-with-packed-bits 1)
  ELSE (${USE_PACKED_BITS})
    ADD_H5_TEST (tnofilename 1)
  ENDIF (${USE_PACKED_BITS})

  # rev. 2004

  # tests for super block
  ADD_H5_TEST (tboot1 0 -H -B -d dset tfcontents1.h5)
  ADD_H5_TEST (tboot2 0 -B tfcontents2.h5)

  # test -p with a non existing dataset
  ADD_H5_TEST (tperror 1 -p -d bogus tfcontents1.h5)

  # test for file contents
  ADD_H5_TEST (tcontents 0 -n tfcontents1.h5)

  # tests for storage layout
  # compact
  ADD_H5_TEST (tcompact 0 -H -p -d compact tfilters.h5)
  # contiguous
  ADD_H5_TEST (tcontiguos 0 -H -p -d contiguous tfilters.h5)
  # chunked
  ADD_H5_TEST (tchunked 0 -H -p -d chunked tfilters.h5)
  # external 
  ADD_H5_TEST (texternal 0 -H -p -d external tfilters.h5)

  # fill values
  ADD_H5_TEST (tfill 0 -p tfvalues.h5)

  # several datatype, with references , print path
  ADD_H5_TEST (treference 0  tattr2.h5)

  # escape/not escape non printable characters
  ADD_H5_TEST (tstringe 0 -e tstr3.h5)
  ADD_H5_TEST (tstring 0 tstr3.h5)
  # char data as ASCII with non escape
  ADD_H5_TEST (tstring2 0 -r -d str4 tstr3.h5)

  # array indices print/not print
  ADD_H5_TEST (tindicesyes 0 taindices.h5)
  ADD_H5_TEST (tindicesno 0 -y taindices.h5)

  ########## array indices with subsetting
  # 1D case
  ADD_H5_TEST (tindicessub1 0 -d 1d -s 1 -S 10 -c 2  -k 3 taindices.h5)

  # 2D case
  ADD_H5_TEST (tindicessub2 0 -d 2d -s 1,2  -S 3,3 -c 3,2 -k 2,2 taindices.h5)

  # 3D case
  ADD_H5_TEST (tindicessub3 0 -d 3d -s 0,1,2 -S 1,3,3 -c 2,2,2  -k 1,2,2  taindices.h5)

  # 4D case
  ADD_H5_TEST (tindicessub4 0 -d 4d -s 0,0,1,2  -c 2,2,3,2 -S 1,1,3,3 -k 1,1,2,2  taindices.h5)

  # tests for filters
  # SZIP
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SZIP)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SZIP)
  ADD_SKIP_H5_TEST (tszip 0 ${TESTTYPE} -H -p -d szip tfilters.h5)

  # deflate
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_DEFLATE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_DEFLATE)
  ADD_SKIP_H5_TEST (tdeflate 0 ${TESTTYPE} -H -p -d deflate tfilters.h5)

  # shuffle
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SHUFFLE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SHUFFLE)
  ADD_SKIP_H5_TEST (tshuffle 0 ${TESTTYPE} -H -p -d shuffle tfilters.h5)

  # fletcher32
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_FLETCHER32)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_FLETCHER32)
  ADD_SKIP_H5_TEST (tfletcher32 0 ${TESTTYPE} -H -p -d fletcher32  tfilters.h5)

  # nbit
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_NBIT)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_NBIT)
  ADD_SKIP_H5_TEST (tnbit 0 ${TESTTYPE} -H -p -d nbit  tfilters.h5)

  # scaleoffset
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SCALEOFFSET)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SCALEOFFSET)
  ADD_SKIP_H5_TEST (tscaleoffset 0 ${TESTTYPE} -H -p -d scaleoffset  tfilters.h5)

  # all
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SZIP OR NOT USE_FILTER_SHUFFLE OR NOT USE_FILTER_FLETCHER32 OR NOT USE_FILTER_DEFLATE OR NOT USE_FILTER_NBIT OR NOT USE_FILTER_SCALEOFFSET)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SZIP OR NOT USE_FILTER_SHUFFLE OR NOT USE_FILTER_FLETCHER32 OR NOT USE_FILTER_DEFLATE OR NOT USE_FILTER_NBIT OR NOT USE_FILTER_SCALEOFFSET)
  ADD_SKIP_H5_TEST (tallfilters 0 ${TESTTYPE} -H -p -d all  tfilters.h5)

  # user defined
  ADD_H5_TEST (tuserfilter 0 -H  -p -d myfilter  tfilters.h5)

  # test for displaying objects with very long names
  ADD_H5_TEST (tlonglinks 0 tlonglinks.h5)

  # dimensions over 4GB, print boundary 
  ADD_H5_TEST (tbigdims 0 -d dset4gb -s 4294967284 -c 22 tbigdims.h5)

  # hyperslab read
  ADD_H5_TEST (thyperslab 0 thyperslab.h5)
    
  # test for displaying dataset and attribute of null space
  ADD_H5_TEST (tnullspace 0 tnullspace.h5)

  # test for long double (some systems do not have long double)
  #ADD_H5_TEST (tldouble 0 tldouble.h5)

  # test for vms
  ADD_H5_TEST (tvms 0 tvms.h5)

  # test for binary output
  ADD_H5_TEST (tbin1LE 0 -d integer -o out1LE.bin -b LE tbinary.h5)

  # NATIVE default. the NATIVE test can be validated with h5import/h5diff
  ADD_H5_TEST (tbin1 0 -d integer -o out1.bin -b tbinary.h5)
  ADD_TEST (NAME H5DUMP-clear-out1 COMMAND ${CMAKE_COMMAND} -E remove out1.h5)
  ADD_TEST (NAME H5DUMP-h5import-out1 COMMAND h5import out1.bin -c out3.h5import -o out1.h5)
  ADD_TEST (NAME H5DUMP-h5diff-out1 COMMAND h5diff tbinary.h5 out1.h5 /integer /integer)

  ADD_H5_TEST (tbin2 0 -b BE -d float -o out2.bin tbinary.h5)

  # the NATIVE test can be validated with h5import/h5diff
  ADD_H5_TEST (tbin3 0 -d integer -o out3.bin -b NATIVE tbinary.h5)
  ADD_TEST (NAME H5DUMP-clear-out3 COMMAND ${CMAKE_COMMAND} -E remove out3.h5)
  ADD_TEST (NAME H5DUMP-h5import-out3 COMMAND h5import out3.bin -c out3.h5import -o out3.h5)
  ADD_TEST (NAME H5DUMP-h5diff-out3 COMMAND h5diff tbinary.h5 out3.h5 /integer /integer -q)

  ADD_H5_TEST (tbin4 0 -d double -b FILE -o out4.bin tbinary.h5)
   
  # Clean up binary output files
  IF (NOT HDF5_NOCLEANUP)
    ADD_TEST (
        NAME H5DUMP-clear-objects
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            out1.bin
            out1LE.bin
            out2.bin
            out3.bin
            out4.bin
            out1.h5
            out3.h5
    )
  ENDIF (NOT HDF5_NOCLEANUP)

  # test for dataset region references 
  ADD_H5_TEST (tdatareg 0 tdatareg.h5)
  ADD_H5_TEST (tdataregR 0 -R tdatareg.h5)
  ADD_H5_TEST (tattrreg 0 tattrreg.h5)
  ADD_H5_TEST (tattrregR 0 -R tattrreg.h5)
  ADD_H5_EXPORT_TEST (tbinregR tdatareg.h5 0 -d /Dataset1 -s 0 -R -y -o)

  # tests for group creation order
  # "1" tracked, "2" name, root tracked
  ADD_H5_TEST (tordergr1 0 --group=1 --sort_by=creation_order --sort_order=ascending tordergr.h5)
  ADD_H5_TEST (tordergr2 0 --group=1 --sort_by=creation_order --sort_order=descending tordergr.h5)
  ADD_H5_TEST (tordergr3 0 -g 2 -q name -z ascending tordergr.h5)
  ADD_H5_TEST (tordergr4 0 -g 2 -q name -z descending tordergr.h5)
  ADD_H5_TEST (tordergr5 0 -q creation_order tordergr.h5)

  # tests for attribute order
  ADD_H5_TEST (torderattr1 0 -H --sort_by=name --sort_order=ascending torderattr.h5)
  ADD_H5_TEST (torderattr2 0 -H --sort_by=name --sort_order=descending torderattr.h5)
  ADD_H5_TEST (torderattr3 0 -H --sort_by=creation_order --sort_order=ascending torderattr.h5)
  ADD_H5_TEST (torderattr4 0 -H --sort_by=creation_order --sort_order=descending torderattr.h5)

  # tests for floating point user defined printf format
  ADD_H5_TEST (tfpformat 0 -m %.7f tfpformat.h5)

  # tests for traversal of external links
  ADD_H5_TEST (textlinksrc 0 textlinksrc.h5)
  ADD_H5_TEST (textlinkfar 0 textlinkfar.h5)

  # test for dataset packed bits 
  SET (TESTTYPE "TEST")
  IF (NOT ${USE_PACKED_BITS})
    SET (TESTTYPE "SKIP")
  ENDIF (NOT ${USE_PACKED_BITS})
  # Limits:
  # Maximum number of packed bits is 8 (for now).
  # Maximum integer size is 8 (for now).
  # Maximun Offset is 7 (Maximum size - 1).
  # Maximum Offset+Length is 8 (Maximum size).
  # Tests:
  # Normal operation on both signed and unsigned int datasets.
  # Their rawdata output should be the same.
  ADD_SKIP_H5_TEST (tpbitsSigned 0 ${TESTTYPE} -d /DS08BITS -M 0,2,2,6 packedbits.h5)
  ADD_SKIP_H5_TEST (tpbitsUnsigned 0 ${TESTTYPE} -d /DU08BITS -M 0,2,2,6 packedbits.h5)
  # Overlapped packed bits.
  ADD_SKIP_H5_TEST (tpbitsOverlapped 0 ${TESTTYPE} -d /DS08BITS -M 0,1,1,1,2,1,0,3 packedbits.h5)
  # Maximum number of packed bits.
  ADD_SKIP_H5_TEST (tpbitsMax 0 ${TESTTYPE} -d /DS08BITS -M 0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1 packedbits.h5)
  # Compound type.
  ADD_SKIP_H5_TEST (tpbitsCompound 0 ${TESTTYPE} -d /dset1 -M 0,1,1,1 tcompound.h5)
  # Array type.
  ADD_SKIP_H5_TEST (tpbitsArray 0 ${TESTTYPE} -d /Dataset1 -M 0,1,1,1 tarray1.h5)
  # Test Error handling.
  # Too many packed bits requested. Max is 8 for now.
  ADD_SKIP_H5_TEST (tpbitsMaxExceeded 1 ${TESTTYPE} -d /DS08BITS -M 0,1,0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1 packedbits.h5)
  # Offset too large. Max is 7 (8-1) for now.
  ADD_SKIP_H5_TEST (tpbitsOffsetExceeded 1 ${TESTTYPE} -d /DS08BITS -M 8,1 packedbits.h5)
  # Bad offset, must not be negative.
  ADD_SKIP_H5_TEST (tpbitsOffsetNegative 1 ${TESTTYPE} -d /DS08BITS -M -1,1 packedbits.h5)
  # Bad length, must not be positive.
  ADD_SKIP_H5_TEST (tpbitsLengthPositive 1 ${TESTTYPE} -d /DS08BITS -M 4,0 packedbits.h5)
  # Offset+Length is too large. Max is 8 for now.
  ADD_SKIP_H5_TEST (tpbitsLengthExceeded 1 ${TESTTYPE} -d /DS08BITS -M 2,7 packedbits.h5)
  # Incomplete pair of packed bits request.
  ADD_SKIP_H5_TEST (tpbitsIncomplete 1 ${TESTTYPE} -d /DS08BITS -M 0,2,2,1,0,2,2, packedbits.h5)
   
ENDIF (BUILD_TESTING)

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------
INSTALL (
    TARGETS
        h5dump
    RUNTIME DESTINATION
        bin/tools
    COMPONENT
        toolsapplications
)
