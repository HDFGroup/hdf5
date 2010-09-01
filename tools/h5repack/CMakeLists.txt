cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_H5REPACK)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_TEST_SRC_DIR})

# --------------------------------------------------------------------
# Add h5Repack executables
# --------------------------------------------------------------------
SET (REPACK_COMMON_SRCS
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_copy.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_filters.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_opttable.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_parse.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_refs.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_verify.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack.c
)

ADD_EXECUTABLE (h5repack ${REPACK_COMMON_SRCS} ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_main.c)
H5_NAMING (h5repack)
IF (WIN32)
  IF (MSVC)
    IF (NOT BUILD_SHARED_LIBS)
      SET_TARGET_PROPERTIES (h5repack
          PROPERTIES
              LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
      ) 
    ENDIF (NOT BUILD_SHARED_LIBS)
  ENDIF (MSVC)
ENDIF (WIN32)
TARGET_LINK_LIBRARIES (h5repack  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES h5repack)

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

IF (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add h5Repack test executables
  # --------------------------------------------------------------------
  ADD_EXECUTABLE (testh5repack_detect_szip ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testh5repack_detect_szip.c)
  H5_NAMING (testh5repack_detect_szip)
  IF (WIN32)
    IF (MSVC)
      IF (NOT BUILD_SHARED_LIBS)
        SET_TARGET_PROPERTIES (testh5repack_detect_szip
            PROPERTIES
                LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
        ) 
      ENDIF (NOT BUILD_SHARED_LIBS)
    ENDIF (MSVC)
  ENDIF (WIN32)
  TARGET_LINK_LIBRARIES (testh5repack_detect_szip ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})

  ADD_TEST (NAME testh5repack_detect_szip COMMAND $<TARGET_FILE:testh5repack_detect_szip>)
  IF (HDF5_ENABLE_SZIP_SUPPORT)
    IF (HDF5_ENABLE_SZIP_ENCODING)
      SET (passRegex "yes")
      SET_TESTS_PROPERTIES (testh5repack_detect_szip PROPERTIES PASS_REGULAR_EXPRESSION "yes")
    ELSE (HDF5_ENABLE_SZIP_ENCODING)
      SET (passRegex "no")
      SET_TESTS_PROPERTIES (testh5repack_detect_szip PROPERTIES PASS_REGULAR_EXPRESSION "no")
    ENDIF (HDF5_ENABLE_SZIP_ENCODING)
  ELSE (HDF5_ENABLE_SZIP_SUPPORT)
    SET (passRegex "no")
    SET_TESTS_PROPERTIES (testh5repack_detect_szip PROPERTIES PASS_REGULAR_EXPRESSION "no")
  ENDIF (HDF5_ENABLE_SZIP_SUPPORT)

  ADD_EXECUTABLE (h5repacktest ${REPACK_COMMON_SRCS} ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repacktst.c)
  H5_NAMING (h5repacktest)
  IF (WIN32)
    IF (MSVC)
      IF (NOT BUILD_SHARED_LIBS)
        SET_TARGET_PROPERTIES (h5repacktest
            PROPERTIES
                LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
        ) 
      ENDIF (NOT BUILD_SHARED_LIBS)
    ENDIF (MSVC)
  ENDIF (WIN32)
  TARGET_LINK_LIBRARIES (h5repacktest  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})

  ADD_TEST (NAME h5repacktest COMMAND $<TARGET_FILE:h5repacktest>)
    
  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  SET (HDF5_REFERENCE_TEST_FILES
      h5repack_ext.bin
      ublock.bin
      h5repack.info
      h5repack_attr.h5
      h5repack_attr_refs.h5
      h5repack_deflate.h5
      h5repack_early.h5
      h5repack_ext.h5
      h5repack_fill.h5
      h5repack_filters.h5
      h5repack_fletcher.h5
      h5repack_hlink.h5
      h5repack_layout.h5
      h5repack_layouto.h5
      h5repack_named_dtypes.h5
      h5repack_nbit.h5
      h5repack_objs.h5
      h5repack_refs.h5
      h5repack_shuffle.h5
      h5repack_soffset.h5
      h5repack_szip.h5
  )
  SET (HDF5_COMMON_TEST_FILES
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
      h5repack_filters.h5.ddl
  )

  FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/${h5_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5repack
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})

  FOREACH (h5c_file ${HDF5_COMMON_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/${h5c_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5repack
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/${h5c_file} ${dest}
    )
  ENDFOREACH (h5c_file ${HDF5_COMMON_TEST_FILES})
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_H5_TEST testname testtype testfile)
    IF (${testtype} STREQUAL "SKIP")
      ADD_TEST (
          NAME H5REPACK-${testname}
          COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out.${testfile}"
      )
    ELSE (${testtype} STREQUAL "SKIP")
      ADD_TEST (
          NAME H5REPACK-${testname}
          COMMAND $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out.${testfile}
      )
      ADD_TEST (
          NAME H5REPACK-DIFF_${testname}
          COMMAND $<TARGET_FILE:h5diff> ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out.${testfile}
      )
    ENDIF (${testtype} STREQUAL "SKIP")
  ENDMACRO (ADD_H5_TEST testname testfile testtype)

  MACRO (ADD_H5_CMP_TEST testname testfilter testtype resultcode resultfile)
    IF (${testtype} STREQUAL "SKIP")
      ADD_TEST (
          NAME H5REPACK-${testname}
          COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out.${resultfile}"
      )
    ELSE (${testtype} STREQUAL "SKIP")
      ADD_TEST (
          NAME H5REPACK-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5repack>"
              -D "TEST_ARGS:STRING=${ARGN};testfiles/${resultfile};testfiles/out.${resultfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_FILTER:STRING=${testfilter}"
              -D "TEST_REFERENCE=testfiles/${resultfile}.ddl"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
    ENDIF (${testtype} STREQUAL "SKIP")
  ENDMACRO (ADD_H5_CMP_TEST file)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  # --------------------------------------------------------------------
  # test file names 
  # --------------------------------------------------------------------
  SET (INFO_FILE testfiles/h5repack.info)

  SET (FILE0 h5repack_fill.h5)
  SET (FILE1 h5repack_objs.h5)
  SET (FILE2 h5repack_attr.h5)
  SET (FILE3 h5repack_hlink.h5)
  SET (FILE4 h5repack_layout.h5)
  SET (FILE5 h5repack_early.h5)
  SET (FILE7 h5repack_szip.h5)
  SET (FILE8 h5repack_deflate.h5)
  SET (FILE9 h5repack_shuffle.h5)
  SET (FILE10 h5repack_fletcher.h5)
  SET (FILE11 h5repack_filters.h5)
  SET (FILE12 h5repack_nbit.h5)
  SET (FILE13 h5repack_soffset.h5)
  SET (FILE14 h5repack_layouto.h5 )     # A file with an older version of the layout message (copy of test/tlayouto.h5)
  SET (FILE15 h5repack_named_dtypes.h5)
  SET (FILE16 tfamily%05d.h5)           # located in common testfiles folder
  SET (FILE_REF h5repack_refs.h5)
  SET (FILE_ATTR_REF h5repack_attr_refs.h5)

#
# The tests
# We use the files generated by h5repacktst
# Each run generates "<file>.out.h5" and the tool h5diff is used to
# compare the input and output files
#
# the tests are the same as the program h5repacktst, but run from the CLI 
#

# See which filters are usable (and skip tests for filters we
# don't have).  Do this by searching H5pubconf.h to see which
# filters are defined.

# detect whether the encoder is present. 
SET (USE_FILTER_SZIP_ENCODER "no")
IF (HDF5_ENABLE_SZIP_ENCODING)
  SET (USE_FILTER_SZIP_ENCODER ${testh5repack_detect_szip})
ENDIF (HDF5_ENABLE_SZIP_ENCODING)

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
  
# copy files (these files have no filters) 
ADD_H5_TEST (fill "TEST" ${FILE0})
ADD_H5_TEST (objs "TEST" ${FILE1})
ADD_H5_TEST (attr "TEST" ${FILE2})
ADD_H5_TEST (hlink "TEST" ${FILE3})
ADD_H5_TEST (layout "TEST" ${FILE4})
ADD_H5_TEST (early "TEST" ${FILE5})

# use $FILE4 to write some filters  (this file has  no filters)

# gzip with individual object
SET (arg ${FILE4} -f dset1:GZIP=1  -l dset1:CHUNK=20x10)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_DEFLATE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_DEFLATE)
ADD_H5_TEST (gzip_individual ${TESTTYPE} ${arg})
  
# gzip for all 
SET (arg ${FILE4} -f GZIP=1)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_DEFLATE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_DEFLATE)
ADD_H5_TEST (gzip_all ${TESTTYPE} ${arg})

# szip with individual object
SET (arg ${FILE4} -f dset2:SZIP=8,EC  -l dset2:CHUNK=20x10)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
ADD_H5_TEST (szip_individual ${TESTTYPE} ${arg}) 

# szip for all
SET (arg ${FILE4} -f SZIP=8,NN)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
ADD_H5_TEST (szip_all ${TESTTYPE} ${arg}) 

# shuffle with individual object
SET (arg ${FILE4} -f dset2:SHUF  -l dset2:CHUNK=20x10)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_SHUFFLE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_SHUFFLE)
ADD_H5_TEST (shuffle_individual ${TESTTYPE} ${arg}) 

# shuffle for all
SET (arg ${FILE4} -f SHUF)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_SHUFFLE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_SHUFFLE)
ADD_H5_TEST (shuffle_all ${TESTTYPE} ${arg})
  
# fletcher32  with individual object
SET (arg ${FILE4} -f dset2:FLET  -l dset2:CHUNK=20x10)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_FLETCHER32)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_FLETCHER32)
ADD_H5_TEST (fletcher_individual ${TESTTYPE} ${arg})

# fletcher32 for all
SET (arg ${FILE4} -f FLET)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_FLETCHER32)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_FLETCHER32)
ADD_H5_TEST (fletcher_all ${TESTTYPE} ${arg})

# all filters
SET (arg ${FILE4} -f dset2:SHUF -f dset2:FLET -f dset2:SZIP=8,NN -f dset2:GZIP=1 -l dset2:CHUNK=20x10)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP OR NOT USE_FILTER_SHUFFLE OR NOT USE_FILTER_FLETCHER32 OR NOT USE_FILTER_DEFLATE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP OR NOT USE_FILTER_SHUFFLE OR NOT USE_FILTER_FLETCHER32 OR NOT USE_FILTER_DEFLATE)
ADD_H5_TEST (all_filters ${TESTTYPE} ${arg})

# verbose gzip with individual object
SET (arg ${FILE11} -v -f /dset_deflate:GZIP=9)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_DEFLATE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_DEFLATE)
ADD_H5_CMP_TEST (gzip_verbose_filters "O?...ing file[^\n]+\n" ${TESTTYPE} 0 ${arg})
  
###########################################################
# the following tests assume the input files have filters
###########################################################

# szip copy
SET (arg ${FILE7})
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
ADD_H5_TEST (szip_copy ${TESTTYPE} ${arg})
  
# szip remove
SET (arg ${FILE7} --filter=dset_szip:NONE)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
ADD_H5_TEST (szip_remove ${TESTTYPE} ${arg})
  
# deflate copy
SET (arg ${FILE8})
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_DEFLATE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_DEFLATE)
ADD_H5_TEST (deflate_copy ${TESTTYPE} ${arg})

# deflate remove
SET (arg ${FILE8} -f dset_deflate:NONE)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_DEFLATE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_DEFLATE)
ADD_H5_TEST (deflate_remove ${TESTTYPE} ${arg})
    
# shuffle copy
SET (arg ${FILE9})
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_SHUFFLE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_SHUFFLE)
ADD_H5_TEST (shuffle_copy ${TESTTYPE} ${arg})

# shuffle remove
SET (arg ${FILE9} -f dset_shuffle:NONE)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_SHUFFLE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_SHUFFLE)
ADD_H5_TEST (shuffle_remove ${TESTTYPE} ${arg})

# fletcher32 copy
SET (arg ${FILE10})
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_FLETCHER32)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_FLETCHER32)
ADD_H5_TEST (fletcher_copy ${TESTTYPE} ${arg})

# fletcher32 remove
SET (arg ${FILE10} -f dset_fletcher32:NONE)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_FLETCHER32)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_FLETCHER32)
ADD_H5_TEST (fletcher_remove ${TESTTYPE} ${arg})

# nbit copy
SET (arg ${FILE12})
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_NBIT)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_NBIT)
ADD_H5_TEST (nbit_copy ${TESTTYPE} ${arg})

# nbit remove
SET (arg ${FILE12} -f dset_nbit:NONE)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_NBIT)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_NBIT)
ADD_H5_TEST (nbit_remove ${TESTTYPE} ${arg})

# nbit add
SET (arg ${FILE12} -f dset_int31:NBIT)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_NBIT)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_NBIT)
ADD_H5_TEST (nbit_add ${TESTTYPE} ${arg})

# scaleoffset copy
SET (arg ${FILE13})
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_SCALEOFFSET)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_SCALEOFFSET)
ADD_H5_TEST (scale_copy ${TESTTYPE} ${arg})

# scaleoffset add
SET (arg ${FILE13} -f dset_none:SOFF=31,IN)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_SCALEOFFSET)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_SCALEOFFSET)
ADD_H5_TEST (scale_add ${TESTTYPE} ${arg})

# scaleoffset remove
SET (arg ${FILE13} -f dset_scaleoffset:NONE)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_SCALEOFFSET)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_SCALEOFFSET)
ADD_H5_TEST (scale_remove ${TESTTYPE} ${arg})

# remove all  filters
SET (arg ${FILE11} -f NONE)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_FLETCHER32 OR NOT USE_FILTER_DEFLATE OR NOT USE_FILTER_SZIP OR NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SHUFFLE OR NOT USE_FILTER_NBIT OR NOT USE_FILTER_SCALEOFFSET)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_FLETCHER32 OR NOT USE_FILTER_DEFLATE OR NOT USE_FILTER_SZIP OR NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SHUFFLE OR NOT USE_FILTER_NBIT OR NOT USE_FILTER_SCALEOFFSET)
ADD_H5_TEST (remove_all ${TESTTYPE} ${arg})

#filter conversions

SET (arg ${FILE8} -f dset_deflate:SZIP=8,NN)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP OR NOT USE_FILTER_DEFLATE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP OR NOT USE_FILTER_DEFLATE)
ADD_H5_TEST (deflate_convert ${TESTTYPE} ${arg}) 

SET (arg ${FILE7} -f dset_szip:GZIP=1)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_SZIP OR NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_DEFLATE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_SZIP OR NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_DEFLATE)
ADD_H5_TEST (szip_convert ${TESTTYPE} ${arg}) 

#limit
SET (arg ${FILE4} -f GZIP=1 -m 1024)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_DEFLATE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_DEFLATE)
ADD_H5_TEST (deflate_limit ${TESTTYPE} ${arg})

#file
SET (arg ${FILE4} -e ${INFO_FILE})
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_DEFLATE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_DEFLATE)
ADD_H5_TEST (deflate_file ${TESTTYPE} ${arg}) 

#########################################################
# layout options (these files have no filters)
#########################################################

ADD_H5_TEST (dset2_chunk_20x10 "TEST" ${FILE4} --layout=dset2:CHUNK=20x10)
ADD_H5_TEST (chunk_20x10 "TEST" ${FILE4} -l CHUNK=20x10)
ADD_H5_TEST (dset2_conti "TEST" ${FILE4} -l dset2:CONTI)
ADD_H5_TEST (conti "TEST" ${FILE4} -l CONTI)
ADD_H5_TEST (dset2_compa "TEST" ${FILE4} -l dset2:COMPA)
ADD_H5_TEST (compa "TEST" ${FILE4} -l COMPA)


################################################################
# layout conversions (file has no filters)
###############################################################

ADD_H5_TEST (dset_compa_conti "TEST" ${FILE4} -l dset_compact:CONTI)
ADD_H5_TEST (dset_compa_chunk "TEST" ${FILE4} -l dset_compact:CHUNK=2x5)
ADD_H5_TEST (dset_compa_compa "TEST" ${FILE4} -l dset_compact:COMPA)
ADD_H5_TEST (dset_conti_compa "TEST" ${FILE4} -l dset_contiguous:COMPA)
ADD_H5_TEST (dset_conti_chunk "TEST" ${FILE4} -l dset_contiguous:CHUNK=3x6)
ADD_H5_TEST (dset_conti_conti "TEST" ${FILE4} -l dset_contiguous:CONTI)
ADD_H5_TEST (chunk_compa "TEST" ${FILE4} -l dset_chunk:COMPA)
ADD_H5_TEST (chunk_conti "TEST" ${FILE4} -l dset_chunk:CONTI)
ADD_H5_TEST (chunk_18x13 "TEST" ${FILE4} -l dset_chunk:CHUNK=18x13)

# Native option
# Do not use FILE1, as the named dtype will be converted to native, and h5diff will
# report a difference.
ADD_H5_TEST (native_fill "TEST" ${FILE0} -n)
ADD_H5_TEST (native_attr "TEST" ${FILE2} -n)


# latest file format with long switches. use FILE4=h5repack_layout.h5 (no filters)
SET (arg ${FILE4} --layout CHUNK=20x10 --filter GZIP=1 --minimum=10 --native --latest --compact=8 --indexed=6 --ssize=8[:dtype])
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_DEFLATE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_DEFLATE)
ADD_H5_TEST (layout_long_switches ${TESTTYPE} ${arg})

# latest file format with short switches. use FILE4=h5repack_layout.h5 (no filters)
SET (arg ${FILE4} -l CHUNK=20x10 -f GZIP=1 -m 10 -n -L -c 8 -d 6 -s 8[:dtype])
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_DEFLATE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_DEFLATE)
ADD_H5_TEST (layout_short_switches ${TESTTYPE} ${arg})

# several global filters
SET (arg ${FILE4} --filter GZIP=1 --filter SHUF)
SET (TESTTYPE "TEST")
IF (NOT USE_FILTER_DEFLATE OR NOT USE_FILTER_SHUFFLE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_DEFLATE OR NOT USE_FILTER_SHUFFLE)
ADD_H5_TEST (global_filters ${TESTTYPE} ${arg})

# syntax of -i infile -o outfile
# latest file format with short switches. use FILE4=h5repack_layout.h5 (no filters)
SET (arg ${FILE4} -l CHUNK=20x10 -f GZIP=1 -m 10 -n -L -c 8 -d 6 -s 8[:dtype])
SET (TESTTYPE "LEGACY")
IF (NOT USE_FILTER_DEFLATE)
  SET (TESTTYPE "SKIP")
ENDIF (NOT USE_FILTER_DEFLATE)
#ADD_H5_TEST (old_style_layout_short_switches ${TESTTYPE} ${arg})

# add a userblock to file
SET (arg ${FILE1} -u ${PROJECT_BINARY_DIR}/testfiles/ublock.bin -b 2048)
ADD_H5_TEST (add_userblock "TEST" ${arg})

# add alignment
SET (arg ${FILE1} -t 1 -a 1)
ADD_H5_TEST (add_alignment "TEST" ${arg})

# Check repacking file with old version of layout message (should get upgraded
#       to new version and be readable, etc.)
ADD_H5_TEST (upgrade_layout "TEST" ${FILE14})

# test for datum size > H5TOOLS_MALLOCSIZE
ADD_H5_TEST (gt_mallocsize "TEST" ${FILE1} -f GZIP=1)

# Check repacking file with committed datatypes in odd configurations
ADD_H5_TEST (committed_dt "TEST" ${FILE15})

# tests family driver (file is located in common testfiles folder, uses TOOLTEST1
#ADD_H5_TEST ( family "FAMILY" ${FILE16})

# test various references (bug 1814 and 1726)
ADD_H5_TEST (bug1814 "TEST" ${FILE_REF})

# test attribute with various references (bug 1797)
# the references in attribute of compund or vlen datatype 
# TODO: include this test when code portion is completed.
ADD_H5_TEST (bug1797 "SKIP" ${FILE_ATTR_REF})
  
ENDIF (BUILD_TESTING)

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------
INSTALL (
    TARGETS
        h5repack
    RUNTIME DESTINATION
        bin/tools
    COMPONENT
        toolsapplications
)
