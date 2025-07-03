#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

# --------------------------------------------------------------------
# Copy all the test files from source directory to test directory
# --------------------------------------------------------------------
set (LIST_HDF5_TESTLS_FILES
    tdset2.h5
    tdset_idx.h5
)

set (LIST_HDF5_TEST_FILES
    tall.h5
    tarray1.h5
    tattr2.h5
    tattrreg.h5
    tcomplex.h5
    tcomplex_be.h5
    tcompound.h5
    tdatareg.h5
    tdset.h5
    tempty.h5
    textlink.h5
    textlinksrc.h5
    textlinktar.h5
    tfloat16.h5
    tfloat16_be.h5
    tgroup.h5
    tgrp_comments.h5
    tgrpnullspace.h5
    thlink.h5
    tloop.h5
    tnestedcomp.h5
    tsaf.h5
    tslink.h5
    tsoftlinks.h5
    tstr.h5
    tudfilter.h5
    tudlink.h5
    tvldtypes1.h5
)
set (LIST_OTHER_TEST_FILES
    help-1.ls
    help-2.ls
    help-3.ls
    nosuchfile.ls
    tall-1.ls
    tall-2.ls
    tall-3.ls
    tarray1.ls
    tattr2.ls
    tattrreg_le.ls
    tattrreg_be.ls
    tcomp-1.ls
    tcomplex.ls
    tcomplex_be.ls
    tcomplex_be_nosupport.ls
    tcomplex_nosupport.ls
    tdataregbe.ls
    tdataregle.ls
    tdset-1.ls
    tdset2-1.ls
    tdset2-2.ls
    tdset_idx-1.ls
    tdset_idx-2.ls
    tempty.ls
    textlink-1.ls
    textlinksrc-1.ls
    textlinksrc-2.ls
    textlinksrc-3.ls
    textlinksrc-4.ls
    textlinksrc-5.ls
    textlinksrc-6.ls
    textlinksrc-7.ls
    textlinksrc-1-old.ls
    textlinksrc-2-old.ls
    textlinksrc-3-old.ls
    textlinksrc-6-old.ls
    textlinksrc-7-old.ls
    textlinksrc-nodangle-1.ls
    textlinksrc-nodangle-2.ls
    tfloat16.ls
    tfloat16_nosupport.ls
    tfloat16_be.ls
    tfloat16_be_nosupport.ls
    tgroup.ls
    tgroup-1.ls
    tgroup-2.ls
    tgroup-3.ls
    tgrpnullspace.ls
    tgrp_comments.ls
    thlinks-nodangle-1.ls
    thlink-1.ls
    tloop-1.ls
    tmultifile.ls
    tnestcomp-1.ls
    tnestcomp-2.ls
    tnestcomp-3.ls
    tnestcomp-4.ls
    tsaf.ls
    tslink-1.ls
    tsoftlinks-1.ls
    tsoftlinks-2.ls
    tsoftlinks-3.ls
    tsoftlinks-4.ls
    tsoftlinks-5.ls
    tsoftlinks-nodangle-1.ls
    tstr-1.ls
    tudfilter.ls
    tudlink-1.ls
    tvldtypes1.ls
    tvldtypes2le.ls
    tvldtypes2be.ls
)

set (H5LS_S3PROXY_TEST_FILES
    tgroup.h5
)

file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/S3TEST")
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/S3TEST/testfiles")

# copy the list of test files
foreach (listlsfiles ${LIST_HDF5_TESTLS_FILES})
  HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/testfiles/${listlsfiles}" "${PROJECT_BINARY_DIR}/testfiles/${listlsfiles}" "h5ls_files")
endforeach ()
foreach (listfiles ${LIST_HDF5_TEST_FILES})
  HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/testfiles/${listfiles}" "${PROJECT_BINARY_DIR}/testfiles/${listfiles}" "h5ls_files")
endforeach ()
foreach (listothers ${LIST_OTHER_TEST_FILES})
  HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/h5ls/expected/${listothers}" "${PROJECT_BINARY_DIR}/testfiles/${listothers}" "h5ls_files")
endforeach ()
foreach (lists3file ${H5LS_S3PROXY_TEST_FILES})
  HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/testfiles/${lists3file}" "${PROJECT_BINARY_DIR}/S3TEST/testfiles/${lists3file}" "h5ls_files")
endforeach ()
add_custom_target(h5ls_files ALL COMMENT "Copying files needed by h5ls tests" DEPENDS ${h5ls_files_list})


##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

macro (ADD_H5_TEST resultfile resultcode)
  # If using memchecker add tests without using scripts
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME H5LS-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5ls> ${ARGN})
    set_tests_properties (H5LS-${resultfile} PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
    )
    if ("${resultcode}" STREQUAL "1")
      set_tests_properties (H5LS-${resultfile} PROPERTIES WILL_FAIL "true")
    endif ()
  else ()
    # Remove any output file left over from previous test run
    add_test (
        NAME H5LS-${resultfile}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5ls>"
            -D "TEST_ARGS=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
            -D "TEST_OUTPUT=${resultfile}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=${resultfile}.ls"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (H5LS-${resultfile} PROPERTIES
      WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  if ("H5LS-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (H5LS-${resultfile} PROPERTIES DISABLED true)
  endif ()
endmacro ()

macro (ADD_H5_ERR_TEST resultfile resultcode result_errcheck)
  # If using memchecker add tests without using scripts
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME H5LS-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5ls> ${ARGN})
    set_tests_properties (H5LS-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
    if ("${resultcode}" STREQUAL "1")
      set_tests_properties (H5LS-${resultfile} PROPERTIES WILL_FAIL "true")
    endif ()
  else ()
    add_test (
        NAME H5LS-${resultfile}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5ls>"
            -D "TEST_ARGS=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
            -D "TEST_OUTPUT=${resultfile}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=${resultfile}.ls"
            -D "TEST_ERRREF=${result_errcheck}"
            -D "TEST_SKIP_COMPARE=true"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (H5LS-${resultfile} PROPERTIES
      WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  if ("H5LS-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (H5LS-${resultfile} PROPERTIES DISABLED true)
  endif ()
endmacro ()

macro (ADD_H5_UD_TEST testname resultcode resultfile)
  if (NOT HDF5_ENABLE_USING_MEMCHECKER)
    add_test (
        NAME H5LS_UD-${testname}-${resultfile}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5ls>"
            -D "TEST_ARGS=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
            -D "TEST_OUTPUT=${resultfile}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=${resultfile}.ls"
            -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
            -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (H5LS_UD-${testname}-${resultfile} PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
    )
    if ("H5LS_UD-${testname}-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5LS_UD-${testname}-${resultfile} PROPERTIES DISABLED true)
    endif ()
  endif ()
endmacro ()

macro (ADD_H5_S3TEST resultfile resultcode credtype urlscheme urlpath)
  # If using memchecker add tests without using scripts
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME H5LS_S3TEST-${resultfile}_${urlscheme}_${credtype} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5ls> ${ARGN})
    set_tests_properties (H5LS-${resultfile}_${urlscheme}_${credtype} PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/S3TEST"
    )
    if ("${resultcode}" STREQUAL "1")
      set_tests_properties (H5LS_S3TEST-${resultfile}_${urlscheme}_${credtype} PROPERTIES WILL_FAIL "true")
    endif ()
  else ()
    add_test (
        NAME H5LS_S3TEST-${resultfile}_${urlscheme}_${credtype}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5ls>"
            -D "TEST_ARGS=--enable-error-stack;${ARGN};${urlscheme}://${urlpath}/${resultfile}.h5"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/S3TEST"
            -D "TEST_OUTPUT=${resultfile}_${urlscheme}_${credtype}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=${resultfile}.ls"
            -D "TEST_ENV_VAR:STRING=AWS_SHARED_CREDENTIALS_FILE"
            -D "TEST_ENV_VALUE:STRING=${CMAKE_BINARY_DIR}/credentials"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (H5LS_S3TEST-${resultfile}_${urlscheme}_${credtype} PROPERTIES
      FIXTURES_REQUIRED h5ls_s3_proxy
      ENVIRONMENT "${h5ls_s3tests_env}"
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/S3TEST
  )
  if ("H5LS_S3TEST-${resultfile}_${urlscheme}_${credtype}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (H5LS_S3TEST-${resultfile}_${urlscheme}_${credtype} PROPERTIES DISABLED true)
  endif ()
endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

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

# test the help syntax
ADD_H5_TEST (help-1 0 -w80 -h)
ADD_H5_TEST (help-2 0 -w80 --help)
ADD_H5_TEST (help-3 0 -w80 -?)

# test simple command
ADD_H5_TEST (tall-1 0 -w80 tall.h5)
ADD_H5_TEST (tall-2 0 -w80 -r -d tall.h5)
ADD_H5_TEST (tall-3 0 -w80 -r -d -v -a tall.h5)
ADD_H5_TEST (tgroup 0 -w80 tgroup.h5)
ADD_H5_TEST (tgroup-3 0 -w80 tgroup.h5/g1)

# test page buffer cache command
ADD_H5_TEST (tall-pbc 0 -w80 --page-buffer-size=8192 tall.h5)

# test for displaying groups
# The following combination of arguments is expected to return an error message
# and return value 1
ADD_H5_ERR_TEST (tgroup-1 1 "option not compatible" -w80 -r -g tgroup.h5)
ADD_H5_TEST (tgroup-2 0 -w80 -g tgroup.h5/g1)

# test for files with groups that have long comments
ADD_H5_TEST (tgrp_comments 0 -w80 -v -g tgrp_comments.h5/glongcomment)

# test for displaying simple space datasets
ADD_H5_TEST (tdset-1 0 -w80 -r -d tdset.h5)

# tests for displaying chunked datasets
ADD_H5_TEST (tdset2-1 0 -w80 -r -d tdset2.h5)
ADD_H5_TEST (tdset2-2 0 -w80 -r -d -v -a tdset2.h5)

# test for displaying soft links (dangle)
ADD_H5_TEST (tslink-1 0 -w80 -r tslink.h5)

# test for displaying more soft links with --follow-symlinks
ADD_H5_TEST (tsoftlinks-1 0 --follow-symlinks tsoftlinks.h5)
ADD_H5_TEST (tsoftlinks-2 0 --follow-symlinks -r tsoftlinks.h5)
ADD_H5_TEST (tsoftlinks-3 0 --follow-symlinks tsoftlinks.h5/group1)
ADD_H5_TEST (tsoftlinks-4 0 --follow-symlinks -r tsoftlinks.h5/group1)
ADD_H5_TEST (tsoftlinks-5 0 --follow-symlinks tsoftlinks.h5/soft_dset1)

# test for displaying external and user-defined links with --follow-symlinks
ADD_H5_TEST (textlink-1 0 -w80 -r textlink.h5)
ADD_H5_TEST (textlinksrc-1 0 -w80 --follow-symlinks -r textlinksrc.h5)
ADD_H5_TEST (textlinksrc-2 0 -w80 --follow-symlinks -rv textlinksrc.h5/ext_link5)
ADD_H5_TEST (textlinksrc-3 0 -w80 --follow-symlinks -r textlinksrc.h5/ext_link1)
ADD_H5_TEST (textlinksrc-4 0 -w80 -r textlinksrc.h5)
ADD_H5_TEST (textlinksrc-5 0 -w80 -r textlinksrc.h5/ext_link1)
ADD_H5_TEST (textlinksrc-6 0 -w80 --follow-symlinks textlinksrc.h5)
ADD_H5_TEST (textlinksrc-7 0 -w80 --follow-symlinks textlinksrc.h5/ext_link1)
ADD_H5_TEST (tudlink-1 0 -w80 -r tudlink.h5)

# test for displaying external links with -E
# the option -E will be depriciated but keep it for backward compatibility
ADD_H5_TEST (textlinksrc-1-old 0 -w80 -Er textlinksrc.h5)
ADD_H5_TEST (textlinksrc-2-old 0 -w80 -Erv textlinksrc.h5/ext_link5)
ADD_H5_TEST (textlinksrc-3-old 0 -w80 -Er textlinksrc.h5/ext_link1)
ADD_H5_TEST (textlinksrc-6-old 0 -w80 -E textlinksrc.h5)
ADD_H5_TEST (textlinksrc-7-old 0 -w80 -E textlinksrc.h5/ext_link1)

# tests for no-dangling-links
# if this option is given on dangling link, h5ls should return exit code 1
# when used alone , expect to print out help and return exit code 1
ADD_H5_ERR_TEST (textlinksrc-nodangle-1 1 "no-dangling-links must be used" -w80 --no-dangling-links textlinksrc.h5)
# external dangling link - expected exit code 1
ADD_H5_TEST (textlinksrc-nodangle-2 1 -w80 --follow-symlinks --no-dangling-links textlinksrc.h5)
# soft dangling link - expected exit code 1
ADD_H5_TEST (tsoftlinks-nodangle-1 1 -w80 --follow-symlinks --no-dangling-links tsoftlinks.h5)
# when used file with no dangling links - expected exit code 0
ADD_H5_TEST (thlinks-nodangle-1 0 -w80 --follow-symlinks --no-dangling-links thlink.h5)

# tests for _Float16 type
if (${${HDF_PREFIX}_HAVE__FLOAT16})
  # If support is available for _Float16 type, the second test
  # will fail as the type will be printed out as "native _Float16"
  # rather than "IEEE 16-bit little-endian float".
  if (H5_WORDS_BIGENDIAN)
    ADD_H5_TEST (tfloat16_be 0 -w80 -v tfloat16_be.h5)
    ADD_H5_TEST (tfloat16_be_nosupport 0 -w80 -v tfloat16_be.h5)
    set_tests_properties (H5LS-tfloat16_be_nosupport PROPERTIES WILL_FAIL "true")
  else ()
    ADD_H5_TEST (tfloat16 0 -w80 -v tfloat16.h5)
    ADD_H5_TEST (tfloat16_nosupport 0 -w80 -v tfloat16.h5)
    set_tests_properties (H5LS-tfloat16_nosupport PROPERTIES WILL_FAIL "true")
  endif ()
else ()
  # If support is NOT available for _Float16 type, the first two tests
  # will fail as the types will be printed out as
  # "IEEE 16-bit little-endian float" and "IEEE 16-bit big-endian float"
  # rather than "native _Float16"
  ADD_H5_TEST (tfloat16 0 -w80 -v tfloat16.h5)
  set_tests_properties (H5LS-tfloat16 PROPERTIES WILL_FAIL "true")
  ADD_H5_TEST (tfloat16_be 0 -w80 -v tfloat16_be.h5)
  set_tests_properties (H5LS-tfloat16_be PROPERTIES WILL_FAIL "true")
  ADD_H5_TEST (tfloat16_nosupport 0 -w80 -v tfloat16.h5)
  ADD_H5_TEST (tfloat16_be_nosupport 0 -w80 -v tfloat16_be.h5)
endif ()

# tests for complex numbers
if (${${HDF_PREFIX}_HAVE_COMPLEX_NUMBERS})
  # If support is available for complex numbers, the second test
  # will fail as the type will be printed out as "native float _Complex",
  # for example, rather than "complex number of native float".
  if (H5_WORDS_BIGENDIAN)
    ADD_H5_TEST (tcomplex_be 0 -w80 -v tcomplex_be.h5)
    ADD_H5_TEST (tcomplex_be_nosupport 0 -w80 -v tcomplex_be.h5)
    set_tests_properties (H5LS-tcomplex_be_nosupport PROPERTIES WILL_FAIL "true")
  else ()
    ADD_H5_TEST (tcomplex 0 -w80 -v tcomplex.h5)
    ADD_H5_TEST (tcomplex_nosupport 0 -w80 -v tcomplex.h5)
    set_tests_properties (H5LS-tcomplex_nosupport PROPERTIES WILL_FAIL "true")
  endif ()
else ()
  # If support is NOT available for complex numbers, the first two tests
  # will fail as the types will be printed out as "complex number of native float"
  # or "complex number of IEEE 32-bit little-endian float", for example, rather
  # than "native float _Complex". One of the second two tests will also fail,
  # depending on the endian-ness of the machine, as the types will be printed
  # out as "complex number of IEEE 32-bit little-endian float", for example,
  # rather than "complex number of native float".
  ADD_H5_TEST (tcomplex 0 -w80 -v tcomplex.h5)
  set_tests_properties (H5LS-tcomplex PROPERTIES WILL_FAIL "true")
  ADD_H5_TEST (tcomplex_be 0 -w80 -v tcomplex_be.h5)
  set_tests_properties (H5LS-tcomplex_be PROPERTIES WILL_FAIL "true")

  if (H5_WORDS_BIGENDIAN)
    ADD_H5_TEST (tcomplex_nosupport 0 -w80 -v tcomplex.h5)
    set_tests_properties (H5LS-tcomplex_nosupport PROPERTIES WILL_FAIL "true")
    ADD_H5_TEST (tcomplex_be_nosupport 0 -w80 -v tcomplex_be.h5)
  else ()
    ADD_H5_TEST (tcomplex_nosupport 0 -w80 -v tcomplex.h5)
    ADD_H5_TEST (tcomplex_be_nosupport 0 -w80 -v tcomplex_be.h5)
    set_tests_properties (H5LS-tcomplex_be_nosupport PROPERTIES WILL_FAIL "true")
  endif ()
endif ()

# test for wildcards in filename (does not work with cmake)
#  ADD_H5_TEST (tstarfile 0 -w80 t*link.h5)
#  ADD_H5_TEST (tqmarkfile 0 -w80 t?link.h5)
ADD_H5_TEST (tmultifile 0 -w80 thlink.h5 tslink.h5)

# tests for hard links
ADD_H5_TEST (thlink-1 0 -w80 thlink.h5)

# tests for compound data types
ADD_H5_TEST (tcomp-1 0 -w80 -r -d tcompound.h5)

#test for the nested compound type
ADD_H5_TEST (tnestcomp-1 0 -w80 -r -d tnestedcomp.h5)

ADD_H5_TEST (tnestcomp-2 0 -w80 -r -d -S tnestedcomp.h5)

ADD_H5_TEST (tnestcomp-3 0 -w80 -r -d -l tnestedcomp.h5)

ADD_H5_TEST (tnestcomp-4 0 -w80 -r -d -l -S tnestedcomp.h5)

# test for loop detection
ADD_H5_TEST (tloop-1 0 -w80 -r -d tloop.h5)

# test for string
ADD_H5_TEST (tstr-1 0 -w80 -r -d tstr.h5)

# test test file created from lib SAF team
ADD_H5_TEST (tsaf 0 -w80 -r -d tsaf.h5)

# test for variable length data types
ADD_H5_TEST (tvldtypes1 0 -w80 -r -d tvldtypes1.h5)

# test for array data types
ADD_H5_TEST (tarray1 0 -w80 -r -d tarray1.h5)

# test for empty data
ADD_H5_TEST (tempty 0 -w80 -d tempty.h5)

# test for displaying dataset and attribute of null space
ADD_H5_TEST (tgrpnullspace 0 -w80 -v -S tgrpnullspace.h5)

# test for all dataset types written to attributes
# enable -S for avoiding printing NATIVE types
ADD_H5_TEST (tattr2 0 -w80 -v -S tattr2.h5)

# test for attribute with region references wo verbose mode
# ( HDFFV-7838, )
if (H5_WORDS_BIGENDIAN)
  ADD_H5_TEST (tattrreg_be 0 -w80 -v -d tattrreg.h5)
else ()
  ADD_H5_TEST (tattrreg_le 0 -w80 -v -d tattrreg.h5)
endif ()

# test for non-existing file
ADD_H5_ERR_TEST (nosuchfile 1 "unable to open file" nosuchfile.h5)

# test for variable length data types in verbose mode
if (H5_WORDS_BIGENDIAN)
  ADD_H5_TEST (tvldtypes2be 0 -v tvldtypes1.h5)
else ()
  ADD_H5_TEST (tvldtypes2le 0 -v tvldtypes1.h5)
endif ()

# test for dataset region references data types in verbose mode
if (H5_WORDS_BIGENDIAN)
  ADD_H5_TEST (tdataregbe 0 -v tdatareg.h5)
else ()
  ADD_H5_TEST (tdataregle 0 -v tdatareg.h5)
endif ()

# test for file with datasets that use Fixed Array chunk indices
if (USE_FILTER_DEFLATE)
  # data read internal filters
  ADD_H5_TEST (tdset_idx-1 0 -w80 -d tdset_idx.h5)
  ADD_H5_TEST (tdset_idx-2 0 -w80 -d -v -a tdset_idx.h5)
endif ()


##############################################################
##############################################################
###           S 3   T E S T S                              ###
##############################################################
##############################################################
if (HDF5_ENABLE_ROS3_VFD_DOCKER_PROXY)
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/buckets")
  set (h5ls_s3tests_port 9003)

  # Setup environment for tests.
  # The AWS_ENDPOINT_URL environment variable is set to work
  # around an issue in aws-c-s3 when using localhost URLs
  # directly.
  # The HDF5_ROS3_VFD_FORCE_PATH_STYLE environment variable is
  # set to force the ROS3 VFD to use path-style requests for
  # compatibility with s3proxy.
  # AWS region is required by the ROS3 VFD - set a default to
  # use when one isn't supplied
  # AWS_PROFILE is set in order to use the correct testing
  # credentials created in CMakeTests.cmake
  set (h5ls_s3tests_env
    "AWS_ENDPOINT_URL=http://localhost:${h5ls_s3tests_port}"
    "HDF5_ROS3_VFD_FORCE_PATH_STYLE=1"
    "AWS_REGION=us-east-2"
    "AWS_PROFILE=ros3_vfd_test"
  )

  add_test (
      NAME H5LS-start-proxy
      COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=${DOCKER_EXECUTABLE}"
          -D "TEST_PRODUCT=andrewgaul/s3proxy"
          -D "TEST_PORT=${h5ls_s3tests_port}"
          -D "TEST_ARGS:STRING=s3proxy-local-h5ls"
          -D "TEST_BUCKET:STRING=h5lsros3"
          -D "TEST_FILES:STRING=tgroup.h5"
          -D "TEST_ACLS:STRING=anon"
          -D "TEST_EXPECT=0"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/S3TEST"
          -D "TEST_ENV_VAR:STRING=AWS_SHARED_CREDENTIALS_FILE"
          -D "TEST_ENV_VALUE:STRING=${CMAKE_BINARY_DIR}/credentials"
          -P "${HDF_RESOURCES_DIR}/runProxy.cmake"
  )
  set_tests_properties (H5LS-start-proxy PROPERTIES FIXTURES_SETUP h5ls_s3_proxy)
  add_test (
      NAME H5LS-stop-proxy
      COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=${DOCKER_EXECUTABLE}"
          -D "TEST_ARGS:STRING=s3proxy-local-h5ls"
          -D "TEST_EXPECT=0"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/S3TEST"
          -P "${HDF_RESOURCES_DIR}/stopProxy.cmake"
  )
  set_tests_properties (H5LS-stop-proxy PROPERTIES FIXTURES_CLEANUP h5ls_s3_proxy)

  ADD_H5_S3TEST (tgroup 0 anon http localhost:${h5ls_s3tests_port}/h5lsros3 --vfd-name=ros3 --s3-cred=\(,,\))
  ADD_H5_S3TEST (tgroup 0 anon s3 h5lsros3 --vfd-name=ros3 --s3-cred=\(,,\) --endpoint-url=http://localhost:${h5ls_s3tests_port})
  ADD_H5_S3TEST (tgroup 0 profile http localhost:${h5ls_s3tests_port}/h5lsros3 --vfd-name=ros3)
  ADD_H5_S3TEST (tgroup 0 profile s3 h5lsros3 --vfd-name=ros3 --endpoint-url=http://localhost:${h5ls_s3tests_port})
endif ()

##############################################################################
###    P L U G I N  T E S T S
##############################################################################
if (BUILD_SHARED_LIBS)
  ADD_H5_UD_TEST (h5ls_plugin_test 0 tudfilter -w80 -d tudfilter.h5)
endif ()
