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
# Copy all the HDF5 files from the test directory into the source directory
# --------------------------------------------------------------------
set (HDF5_REFERENCE_FILES
    h5stat_err_refcount
    h5stat_err_old_layout
    h5stat_err_old_fill
    h5stat_help1
    h5stat_help2
    h5stat_notexist
    h5stat_nofile
    h5stat_filters
    h5stat_filters-cache
    h5stat_filters-file
    h5stat_filters-F
    h5stat_filters-d
    h5stat_filters-g
    h5stat_filters-dT
    h5stat_filters-UD
    h5stat_filters-UT
    h5stat_tsohm
    h5stat_newgrat
    h5stat_newgrat-UG
    h5stat_newgrat-UA
    h5stat_idx
    h5stat_links1
    h5stat_links2
    h5stat_links3
    h5stat_links4
    h5stat_links5
    h5stat_dims1
    h5stat_dims2
    h5stat_numattrs1
    h5stat_numattrs2
    h5stat_numattrs3
    h5stat_numattrs4
)
set (HDF5_REFERENCE_TEST_FILES
    h5stat_err_refcount.h5
    h5stat_err_old_layout.h5
    h5stat_err_old_fill.h5
    h5stat_filters.h5
    h5stat_idx.h5
    h5stat_tsohm.h5
    h5stat_newgrat.h5
    h5stat_threshold.h5
)

set (H5STAT_S3PROXY_TEST_FILES
    h5stat_threshold.h5
)

file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/S3TEST")
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/S3TEST/testfiles")

foreach (ddl_file ${HDF5_REFERENCE_FILES})
  HDFTEST_COPY_FILE ("${PROJECT_SOURCE_DIR}/expected/${ddl_file}.ddl" "${PROJECT_BINARY_DIR}/${ddl_file}.ddl" "h5stat_files")
endforeach ()
foreach (h5_file ${HDF5_REFERENCE_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/${h5_file}" "h5stat_files")
endforeach ()
foreach (lists3file ${H5STAT_S3PROXY_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/testfiles/${lists3file}" "${PROJECT_BINARY_DIR}/S3TEST/testfiles/${lists3file}" "h5stat_files")
endforeach ()
add_custom_target (h5stat_files ALL COMMENT "Copying files needed by h5stat tests" DEPENDS ${h5stat_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

macro (ADD_H5_TEST resultfile resultcode)
  # If using memchecker add tests without using scripts
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME H5STAT-${resultfile} COMMAND $<TARGET_FILE:h5stat> ${ARGN})
    if (${resultcode})
      set_tests_properties (H5STAT-${resultfile} PROPERTIES WILL_FAIL "true")
    endif ()
  else ()
    add_test (
        NAME H5STAT-${resultfile}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5stat>"
            -D "TEST_ARGS=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_OUTPUT=${resultfile}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=${resultfile}.ddl"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (H5STAT-${resultfile} PROPERTIES
      ENVIRONMENT "${CROSSCOMPILING_PATH}"
      WORKING_DIRECTORY "${PROJECT_BINARY_DIR}"
  )
  if ("H5STAT-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (H5STAT-${resultfile} PROPERTIES DISABLED true)
  endif ()
endmacro ()

macro (ADD_H5_ERR_TEST resultfile resultcode errtext)
  # If using memchecker add tests without using scripts
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME H5STAT-${resultfile} COMMAND $<TARGET_FILE:h5stat> ${ARGN})
    if (${resultcode})
      set_tests_properties (H5STAT-${resultfile} PROPERTIES WILL_FAIL "true")
    endif ()
  else ()
    add_test (
        NAME H5STAT-${resultfile}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5stat>"
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_OUTPUT=${resultfile}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=${resultfile}.mty"
            -D "TEST_ERRREF=${errtext}"
            -D "TEST_SKIP_COMPARE=true"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (H5STAT-${resultfile} PROPERTIES
      ENVIRONMENT "${CROSSCOMPILING_PATH}"
      WORKING_DIRECTORY "${PROJECT_BINARY_DIR}"
  )
  if ("H5STAT-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (H5STAT-${resultfile} PROPERTIES DISABLED true)
  endif ()
endmacro ()

macro (ADD_H5_CMP_TEST resultfile resultcode errtext)
  # If using memchecker add tests without using scripts
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME H5STAT-${resultfile} COMMAND $<TARGET_FILE:h5stat> ${ARGN})
    if (${resultcode})
      set_tests_properties (H5STAT-${resultfile} PROPERTIES WILL_FAIL "true")
    endif ()
  else ()
    add_test (
        NAME H5STAT-${resultfile}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5stat>"
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_OUTPUT=${resultfile}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=${resultfile}.ddl"
            -D "TEST_ERRREF=${errtext}"
            -D "TEST_GREP_COMPARE=TRUE"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (H5STAT-${resultfile} PROPERTIES
      ENVIRONMENT "${CROSSCOMPILING_PATH}"
      WORKING_DIRECTORY "${PROJECT_BINARY_DIR}"
  )
  if ("H5STAT-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (H5STAT-${resultfile} PROPERTIES DISABLED true)
  endif ()
endmacro ()

macro (ADD_H5_S3TEST resultfile resultcode credtype urlscheme urlpath)
  # If using memchecker add tests without using scripts
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME H5STAT_S3TEST-${resultfile}_${urlscheme}_${credtype} COMMAND $<TARGET_FILE:h5stat> ${ARGN})
    if (${resultcode})
      set_tests_properties (H5STAT_S3TEST-${resultfile}_${urlscheme}_${credtype} PROPERTIES WILL_FAIL "true")
    endif ()
  else ()
    add_test (
        NAME H5STAT_S3TEST-${resultfile}_${urlscheme}_${credtype}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5stat>"
            -D "TEST_ARGS=--enable-error-stack=2;${ARGN};${urlscheme}://${urlpath}/${resultfile}.h5"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/S3TEST"
            -D "TEST_OUTPUT=${resultfile}_${urlscheme}_${credtype}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=${resultfile}.ddl"
            -D "TEST_ENV_VAR:STRING=AWS_SHARED_CREDENTIALS_FILE"
            -D "TEST_ENV_VALUE:STRING=${CMAKE_BINARY_DIR}/credentials"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (H5STAT_S3TEST-${resultfile}_${urlscheme}_${credtype} PROPERTIES
      FIXTURES_REQUIRED h5stat_s3_proxy
      ENVIRONMENT "${h5stat_s3tests_env};${CROSSCOMPILING_PATH}"
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/S3TEST
  )
  if ("H5STAT_S3TEST-${resultfile}_${urlscheme}_${credtype}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (H5STAT_S3TEST-${resultfile}_${urlscheme}_${credtype} PROPERTIES DISABLED true)
  endif ()
endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

# Test for help flag
ADD_H5_TEST (h5stat_help1 0 -h)
ADD_H5_TEST (h5stat_help2 0 --help)

# Test when h5stat a file that does not exist
ADD_H5_ERR_TEST (h5stat_notexist 1 "unable to open file" notexist.h5)
ADD_H5_CMP_TEST (h5stat_nofile 1 "missing file name" '')

# Test file with groups, compressed datasets, user-applied filters, etc.
# h5stat_filters.h5 is a copy of ../../testfiles/tfilters.h5 as of release 1.8.0-alpha4
ADD_H5_TEST (h5stat_filters 0 h5stat_filters.h5)
ADD_H5_TEST (h5stat_filters-file 0 -f h5stat_filters.h5)
ADD_H5_TEST (h5stat_filters-F 0 -F h5stat_filters.h5)
ADD_H5_TEST (h5stat_filters-d 0 -d h5stat_filters.h5)
ADD_H5_TEST (h5stat_filters-g 0 -g h5stat_filters.h5)
ADD_H5_TEST (h5stat_filters-dT 0 -dT h5stat_filters.h5)
ADD_H5_TEST (h5stat_filters-UD 0 -D h5stat_filters.h5)
ADD_H5_TEST (h5stat_filters-UT 0 -T h5stat_filters.h5)
# Test for page buffer cache size option
ADD_H5_TEST (h5stat_filters-cache 0 --enable-error-stack=2 --page-buffer-size=8192 h5stat_filters.h5)
# h5stat_tsohm.h5 is a copy of ../../../test/tsohm.h5 generated by tsohm.c
# as of release 1.8.7-snap0 (on a 64-bit machine)
ADD_H5_TEST (h5stat_tsohm 0 h5stat_tsohm.h5)
# h5stat_newgrat.h5 is generated by h5stat_gentest.c
ADD_H5_TEST (h5stat_newgrat 0 h5stat_newgrat.h5)
ADD_H5_TEST (h5stat_newgrat-UG 0 -G h5stat_newgrat.h5)
ADD_H5_TEST (h5stat_newgrat-UA 0 -A h5stat_newgrat.h5)
# h5stat_idx.h5 is generated by h5stat_gentest.c
ADD_H5_TEST (h5stat_idx 0 h5stat_idx.h5)
#
# Tests for -l (--links) option on h5stat_threshold.h5:
#   -l 0 (incorrect threshold value)
#   -g -l 8
#   --links=8
#   --links=20 -g
ADD_H5_ERR_TEST (h5stat_err1_links 1 "Invalid threshold for small groups" -l 0 h5stat_threshold.h5)
ADD_H5_TEST (h5stat_links1 0 -g -l 8 h5stat_threshold.h5)
ADD_H5_TEST (h5stat_links2 0 --links=8 h5stat_threshold.h5)
ADD_H5_TEST (h5stat_links3 0 --links=20 -g h5stat_threshold.h5)
#
# Tests for -l (--links) option on h5stat_newgrat.h5:
#   -g
#   -g -l 40000
ADD_H5_TEST (h5stat_links4 0 -g h5stat_newgrat.h5)
ADD_H5_TEST (h5stat_links5 0 -g -l 40000 h5stat_newgrat.h5)
#
# Tests for -m (--dims) option on h5stat_threshold.h5
#   -d --dims=-1 (incorrect threshold value)
#   -gd -m 5
#   -d --di=15
ADD_H5_ERR_TEST (h5stat_err1_dims 1 "Invalid threshold for small datasets" -d --dims=-1 h5stat_threshold.h5)
ADD_H5_TEST (h5stat_dims1 0 -gd -m 5 h5stat_threshold.h5)
ADD_H5_TEST (h5stat_dims2 0 -d --dims=15 h5stat_threshold.h5)
#
# Tests for -a option on h5stat_threshold.h5
#   -a -2 (incorrect threshold value)
#   --numattrs (without threshold value)
#   -AS -a 10
#   -a 1
#   -A --numattrs=25
ADD_H5_ERR_TEST (h5stat_err1_numattrs 1 "Invalid threshold for small # of attributes" -a -2 h5stat_threshold.h5)
ADD_H5_ERR_TEST (h5stat_err2_numattrs 1 "Invalid threshold for small # of attributes" --numattrs h5stat_threshold.h5)
ADD_H5_TEST (h5stat_numattrs1 0 -AS -a 10 h5stat_threshold.h5)
ADD_H5_TEST (h5stat_numattrs2 0 -a 1 h5stat_threshold.h5)
ADD_H5_TEST (h5stat_numattrs3 0 -A --numattrs=25 h5stat_threshold.h5)
#
# Tests for -a option on h5stat_newgrat.h5
#   -A -a 100
ADD_H5_TEST (h5stat_numattrs4 0 -A -a 100 h5stat_newgrat.h5)
#
# Tests to verify HDFFV-10333:
# h5stat_err_refcount.h5 is generated by h5stat_gentest.c
# h5stat_err_old_layout.h5 and h5stat_err_old_fill.h5: see explanation in h5stat_gentest.c
ADD_H5_CMP_TEST (h5stat_err_refcount 1 "unable to traverse objects" h5stat_err_refcount.h5)
ADD_H5_CMP_TEST (h5stat_err_old_layout 1 "unable to traverse objects" h5stat_err_old_layout.h5)
ADD_H5_CMP_TEST (h5stat_err_old_fill 1 "unable to traverse objects" h5stat_err_old_fill.h5)
#
#

##############################################################
##############################################################
###           S 3   T E S T S                              ###
##############################################################
##############################################################
if (HDF5_ENABLE_ROS3_VFD_DOCKER_PROXY)
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/buckets")
  set (h5stat_s3tests_port 9004)

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
  set (h5stat_s3tests_env
    "AWS_ENDPOINT_URL=http://localhost:${h5stat_s3tests_port}"
    "HDF5_ROS3_VFD_FORCE_PATH_STYLE=1"
    "AWS_REGION=us-east-2"
    "AWS_PROFILE=ros3_vfd_test"
  )

  add_test (
      NAME H5STAT-start-proxy
      COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=${DOCKER_EXECUTABLE}"
          -D "TEST_PRODUCT=andrewgaul/s3proxy"
          -D "TEST_PORT=${h5stat_s3tests_port}"
          -D "TEST_ARGS:STRING=s3proxy-local-h5stat"
          -D "TEST_BUCKET:STRING=h5statros3"
          -D "TEST_FILES:STRING=h5stat_threshold.h5"
          -D "TEST_ACLS:STRING=anon"
          -D "TEST_EXPECT=0"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/S3TEST"
          -D "TEST_ENV_VAR:STRING=AWS_SHARED_CREDENTIALS_FILE"
          -D "TEST_ENV_VALUE:STRING=${CMAKE_BINARY_DIR}/credentials"
          -P "${HDF_RESOURCES_DIR}/runProxy.cmake"
  )
  set_tests_properties (H5STAT-start-proxy PROPERTIES FIXTURES_SETUP h5stat_s3_proxy)
  add_test (
      NAME H5STAT-stop-proxy
      COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=${DOCKER_EXECUTABLE}"
          -D "TEST_ARGS:STRING=s3proxy-local-h5stat"
          -D "TEST_EXPECT=0"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/S3TEST"
          -P "${HDF_RESOURCES_DIR}/stopProxy.cmake"
  )
  set_tests_properties (H5STAT-stop-proxy PROPERTIES FIXTURES_CLEANUP h5stat_s3_proxy)

  ADD_H5_S3TEST (h5stat_threshold 0 anon http localhost:${h5stat_s3tests_port}/h5statros3 --vfd-name=ros3 --s3-cred=\(,,\))
  ADD_H5_S3TEST (h5stat_threshold 0 anon s3 h5statros3 --vfd-name=ros3 --s3-cred=\(,,\) --endpoint-url=http://localhost:${h5stat_s3tests_port})
  ADD_H5_S3TEST (h5stat_threshold 0 profile http localhost:${h5stat_s3tests_port}/h5statros3 --vfd-name=ros3)
  ADD_H5_S3TEST (h5stat_threshold 0 profile s3 h5statros3 --vfd-name=ros3 --endpoint-url=http://localhost:${h5stat_s3tests_port})
  ADD_H5_S3TEST (h5stat_threshold 0 filename s3 h5statros3 --endpoint-url=http://localhost:${h5stat_s3tests_port})
endif ()
