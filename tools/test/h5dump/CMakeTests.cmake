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

include (${HDF_CONFIG_DIR}/HDF5Macros.cmake)

# System-independent path separator
if (WIN32)
  set (CMAKE_SEP "\;")
else ()
  set (CMAKE_SEP ":")
endif ()

# --------------------------------------------------------------------
# Copy all the HDF5 files from the test directory into the source directory
# --------------------------------------------------------------------
set (HDF5_REFERENCE_FILES
    charsets.ddl
    err_attr_dspace.ddl
    file_space.ddl
    file_space_cache.ddl
    filter_fail.ddl
    non_existing.ddl
    infinite_loop.ddl
    packedbits.ddl
    tall-1.ddl
    tall-2.ddl
    tall-2A.ddl
    tall-2A0.ddl
    tall-2B.ddl
    tall-3.ddl
    tall-4s.ddl
    tall-5s.ddl
    tall-6.ddl
    tall-7.ddl
    tall-7N.ddl
    tallfilters.ddl
    tarray1.ddl
    tarray1_big.ddl
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
    tattr-4_be.ddl
    tattrcontents1.ddl
    tattrcontents2.ddl
    tattrintsize.ddl
    tattrreg.ddl
    tattrregR.ddl
    tbfloat16.ddl
    tbfloat16_be.ddl
    tbin1.ddl
    tbin1.ddl
    tbin2.ddl
    tbin3.ddl
    tbin4.ddl
    tbinregR.ddl
    tbigdims.ddl
    tbitnopaque_be.ddl
    tbitnopaque_le.ddl
    tboot1.ddl
    tboot2.ddl
    tboot2A.ddl
    tboot2B.ddl
    tchar1.ddl
    tchunked.ddl
    tcmpdattrintsize.ddl
    tcmpdintarray.ddl
    tcmpdints.ddl
    tcmpdintsize.ddl
    tcomplex.ddl
    tcomplex_be.ddl
    tcomplex_be_info.ddl
    tcomplex_info.ddl
    tcompound_complex2.ddl
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
    texceedsubstart.ddl
    texceedsubcount.ddl
    texceedsubstride.ddl
    texceedsubblock.ddl
    texternal.ddl
    textlinksrc.ddl
    textlinkfar.ddl
    textlink.ddl
    tfamily.ddl
    tfill.ddl
    tfletcher32.ddl
    tfloatsattrs.ddl
    tfloat8.ddl
    tfloat16.ddl
    tfloat16_be.ddl
    tfpformat.ddl
    tgroup-1.ddl
    tgroup-2.ddl
    tgrp_comments.ddl
    tgrpnullspace.ddl
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
    tints4dims.ddl
    tints4dimsBlock2.ddl
    tints4dimsBlockEq.ddl
    tints4dimsCount2.ddl
    tints4dimsCountEq.ddl
    tints4dimsStride2.ddl
    tintsattrs.ddl
    tintsnodata.ddl
    tlarge_objname.ddl
    tldouble.ddl
    tldouble_scalar.ddl
    tlonglinks.ddl
    tloop-1.ddl
    tmulti.ddl
    tmultifile.ddl
    #tqmarkfile.ddl
    #tstarfile.ddl
    tnamed_dtype_attr.ddl
    tnestcomp-1.ddl
    tnestedcmpddt.ddl
    tnbit.ddl
    tnoattrdata.ddl
    tnoattrddl.ddl
    tnodata.ddl
    tnoddl.ddl
    tnoddlfile.ddl
    tno-subset.ddl
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
    tordercontents1.ddl
    tordercontents2.ddl
    torderlinks1.ddl
    torderlinks2.ddl
    tperror.ddl
    trawdatafile.ddl
    trawssetfile.ddl
    treadfilter.ddl
    treadintfilter.ddl
    treference.ddl
    tsaf.ddl
    tscalarattrintsize.ddl
    tscalarintattrsize.ddl
    tscalarintsize.ddl
    tscalarstring.ddl
    tscaleoffset.ddl
    tshuffle.ddl
    tslink-1.ddl
    tslink-2.ddl
    tslink-D.ddl
    tsplit_file.ddl
    tstr-1.ddl
    tstr-2.ddl
    tstring.ddl
    tstring2.ddl
    tstringe.ddl
    tszip.ddl
    tudfilter.ddl
    tudlink-1.ddl
    tudlink-2.ddl
    tuserfilter.ddl
    tvldtypes1.ddl
    tvldtypes2.ddl
    tvldtypes3.ddl
    tvldtypes4.ddl
    tvldtypes5.ddl
    tvlenstr_array.ddl
    tvlstr.ddl
    tvms.ddl
    twidedisplay.ddl
    twithddlfile.ddl
    h5dump-help.txt
    out3.h5import
    zerodim.ddl
    #STD_REF_OBJ files
    trefer_attrR.ddl
    trefer_compatR.ddl
    trefer_extR.ddl
    trefer_grpR.ddl
    trefer_obj_delR.ddl
    trefer_objR.ddl
    trefer_paramR.ddl
    trefer_reg_1dR.ddl
    trefer_regR.ddl
    # Onion VFD files
    tst_onion_objs.ddl
    tst_onion_dset_ext.ddl
    tst_onion_dset_1d.ddl
    tst_onion_revision_count.ddl
)
set (HDF5_N_REFERENCE_FILES
    tall-3.ddl
    tattr-2.ddl
    tcomp-2.ddl
    thlink-4.ddl
    thlink-5.ddl
    tslink-2.ddl
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
    charsets.h5
    err_attr_dspace.h5
    file_space.h5
    filter_fail.h5
    packedbits.h5
    taindices.h5
    tall.h5
    tarray1.h5
    tarray1_big.h5
    tarray2.h5
    tarray3.h5
    tarray4.h5
    tarray5.h5
    tarray6.h5
    tarray7.h5
    tarray8.h5
    tattr.h5
    tattr2.h5
    tattr4_be.h5
    tattrintsize.h5
    tattrreg.h5
    tbfloat16.h5
    tbfloat16_be.h5
    tbigdims.h5
    tbinary.h5
    tbitnopaque.h5
    tchar.h5
    tcmpdattrintsize.h5
    tcmpdintarray.h5
    tcmpdints.h5
    tcmpdintsize.h5
    tcomplex.h5
    tcomplex_be.h5
    tcompound.h5
    tcompound_complex.h5
    tcompound_complex2.h5
    tdatareg.h5
    tdset.h5
    tempty.h5
    tsoftlinks.h5
    textlinkfar.h5
    textlinksrc.h5
    textlinktar.h5
    textlink.h5
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
    tfloatsattrs.h5
    tfloat8.h5
    tfloat16.h5
    tfloat16_be.h5
    tfpformat.h5
    tfvalues.h5
    tgroup.h5
    3790_infinite_loop.h5
    tgrp_comments.h5
    tgrpnullspace.h5
    thlink.h5
    thyperslab.h5
    tints4dims.h5
    tintsattrs.h5
    tintsnodata.h5
    tlarge_objname.h5
    tldouble.h5
    tldouble_scalar.h5
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
    tnestedcmpddt.h5
    tno-subset.h5
    tnullspace.h5
    torderattr.h5
    tordergr.h5
    tsaf.h5
    tscalarattrintsize.h5
    tscalarintattrsize.h5
    tscalarintsize.h5
    tscalarstring.h5
    tslink.h5
    tsplit_file-m.h5
    tsplit_file-r.h5
    tstr.h5
    tstr2.h5
    tstr3.h5
    tudfilter.h5
    tudlink.h5
    tvldtypes1.h5
    tvldtypes2.h5
    tvldtypes3.h5
    tvldtypes4.h5
    tvldtypes5.h5
    tvlenstr_array.h5
    tvlstr.h5
    tvms.h5
    t128bit_float.h5
    tCVE_2018_11206_fill_old.h5
    tCVE_2018_11206_fill_new.h5
    zerodim.h5
    tCVE-2021-37501_attr_decode.h5
    #STD_REF_OBJ files
    trefer_attr.h5
    trefer_compat.h5
    trefer_ext1.h5
    trefer_ext2.h5
    trefer_grp.h5
    trefer_obj_del.h5
    trefer_obj.h5
    trefer_param.h5
    trefer_reg_1d.h5
    trefer_reg.h5
    # Onion VFD files
    tst_onion_objs.h5
    tst_onion_objs.h5.onion
    tst_onion_dset_ext.h5
    tst_onion_dset_ext.h5.onion
    tst_onion_dset_1d.h5
    tst_onion_dset_1d.h5.onion
)

set (H5DUMP_S3PROXY_TEST_FILES
    tattrintsize.h5
)

# make test dir
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/S3TEST")
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/S3TEST/testfiles")

#
# copy test files from source dir to test dir
#
foreach (tst_h5_file ${HDF5_REFERENCE_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/testfiles/${tst_h5_file}" "${PROJECT_BINARY_DIR}/testfiles/std/${tst_h5_file}" "h5dump_std_files")
endforeach ()

foreach (tst_exp_file ${HDF5_REFERENCE_EXP_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5dump/exportfiles/${tst_exp_file}" "${PROJECT_BINARY_DIR}/testfiles/std/${tst_exp_file}" "h5dump_std_files")
endforeach ()

foreach (tst_other_file ${HDF5_REFERENCE_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5dump/expected/${tst_other_file}" "${PROJECT_BINARY_DIR}/testfiles/std/${tst_other_file}" "h5dump_std_files")
endforeach ()

foreach (tst_h5N_file ${HDF5_N_REFERENCE_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5dump/expected/${tst_h5N_file}" "${PROJECT_BINARY_DIR}/testfiles/std/${tst_h5N_file}-N" "h5dump_std_files")
endforeach ()

foreach (tst_s3_file ${H5DUMP_S3PROXY_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/testfiles/${tst_s3_file}" "${PROJECT_BINARY_DIR}/S3TEST/testfiles/${tst_s3_file}" "h5dump_std_files")
endforeach ()

# --------------------------------------------------------------------
# Special file handling
# --------------------------------------------------------------------
HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5dump/expected/tbin1.ddl" "${PROJECT_BINARY_DIR}/testfiles/std/tbin1LE.ddl" "h5dump_std_files")

# Certain versions of Visual Studio produce rounding differences compared with the reference data of the tfloatsattr test
if (WIN32 AND (CMAKE_VS_WINDOWS_TARGET_PLATFORM_VERSION VERSION_LESS 10.0.18362.0))
  configure_file (${HDF5_TOOLS_TST_DIR}/h5dump/exportfiles/tbinregR.exp ${PROJECT_BINARY_DIR}/testfiles/std/tbinregR.exp NEWLINE_STYLE CRLF)
else ()
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5dump/exportfiles/tbinregR.exp" "${PROJECT_BINARY_DIR}/testfiles/std/tbinregR.exp" "h5dump_std_files")
endif ()
add_custom_target (h5dump_std_files ALL COMMENT "Copying files needed by h5dump_std tests" DEPENDS ${h5dump_std_files_list})

# --------------------------------------------------------------------
# Copy test files for each external VOL connector
# --------------------------------------------------------------------
set (h5dump_vol_files_list "")
foreach (external_vol_tgt ${HDF5_EXTERNAL_VOL_TARGETS})
  HDF5_GET_VOL_TGT_INFO (${external_vol_tgt} ext_vol_dir_name vol_env)
  # Setup testfiles directory
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${ext_vol_dir_name}/testfiles/std")

  # Generate test files
  add_test (NAME ${external_vol_tgt}-h5dumpgentest COMMAND $<TARGET_FILE:h5gentest> --h5dump)
  set_tests_properties (${external_vol_tgt}-h5dumpgentest PROPERTIES
      ENVIRONMENT "${vol_env}"
      WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/${ext_vol_dir_name}/testfiles/std"
      FIXTURES_SETUP "h5dump_vol_files"
  )

  # These aren't HDF5 files, just copy them to the VOL's subdirectory
  foreach (tst_exp_file ${HDF5_REFERENCE_EXP_FILES})
    HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5dump/exportfiles/${tst_exp_file}" "${PROJECT_BINARY_DIR}/${ext_vol_dir_name}/testfiles/std/${tst_exp_file}" "h5dump_vol_files")
  endforeach ()

  foreach (tst_other_file ${HDF5_REFERENCE_FILES})
    HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5dump/expected/${tst_other_file}" "${PROJECT_BINARY_DIR}/${ext_vol_dir_name}/testfiles/std/${tst_other_file}" "h5dump_vol_files")
  endforeach ()

  foreach (tst_h5N_file ${HDF5_N_REFERENCE_FILES})
    HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5dump/expected/${tst_h5N_file}" "${PROJECT_BINARY_DIR}/${ext_vol_dir_name}/testfiles/std/${tst_h5N_file}-N" "h5dump_vol_files")
  endforeach ()

  # Don't copy s3 files for each VOL connector, since the s3 files are specific to the native-exlusive ROS3 VFD

  # --------------------------------------------------------------------
  # Special file handling
  # --------------------------------------------------------------------
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5dump/expected/tbin1.ddl" "${PROJECT_BINARY_DIR}/${ext_vol_dir_name}/testfiles/std/tbin1LE.ddl" "h5dump_vol_files")
  
  # Certain versions of Visual Studio produce rounding differences compared with the reference data of the tfloatsattr test
  if (WIN32 AND (CMAKE_VS_WINDOWS_TARGET_PLATFORM_VERSION VERSION_LESS 10.0.18362.0))
    configure_file (${HDF5_TOOLS_TST_DIR}/h5dump/exportfiles/tbinregR.exp ${PROJECT_BINARY_DIR}/${ext_vol_dir_name}/testfiles/std/tbinregR.exp NEWLINE_STYLE CRLF)
  else ()
    HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5dump/exportfiles/tbinregR.exp" "${PROJECT_BINARY_DIR}/${ext_vol_dir_name}/testfiles/std/tbinregR.exp" "h5dump_vol_files")
  endif ()
endforeach ()
add_custom_target (h5dump_vol_files ALL COMMENT "Copying files needed by h5dump VOL tests" DEPENDS ${h5dump_vol_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

macro (ADD_HELP_TEST testname resultcode)
  # If using memchecker add tests without using scripts
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME H5DUMP-${testname} COMMAND $<TARGET_FILE:h5dump> ${ARGN})
  else ()
    add_test (
        NAME H5DUMP-${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
            -D "TEST_OUTPUT=h5dump-${testname}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=h5dump-${testname}.txt"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (H5DUMP-${testname} PROPERTIES
      WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      ENVIRONMENT "${CROSSCOMPILING_PATH}"
  )
  if ("H5DUMP-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (H5DUMP-${testname} PROPERTIES DISABLED true)
  endif ()
endmacro ()

#
# Perform h5dump according to passed parameters
#
# Usage: ADD_H5_TEST(<testname> <required_args> [optional_args] [flags])
#
# REQUIRED POSITIONAL ARGUMENT:
#   testname - name of test to add
#
# REQUIRED KEYWORD ARGUMENTS:
#   TARGET_FILE <filename> - the file to target with h5dump
#   RESULT_CODE <code>    - expected return code after test execution. 0 is success
#
# OPTIONAL FLAG ARGUMENTS:
#   BINARY_OUTPUT - whether to pass the binary output flag (-b) to h5dump
#                   requires OUTPUT_FILE
#   MASK_ERROR - whether to mask out error stack info from output reference file or the .err ref file
#   GREP_COMPARE - whether to perform a grep comparison on the output file
#   BINFILE - if provided, use h5dump to put binary output into <testname>.bin
#   SKIP_TEST - if provided, do not add this test
#   NATIVE_ONLY - if provided, only run test with native VOL connector
#
# OPTIONAL KEYWORD ARGUMENTS:
#   APPLY_FILTERS <resultvalue> - If provided, test will apply filters to output before comparison.
#                                 <resultvalue> is used to construct the filter expressions
#   OUTPUT_FILE <filename> - if provided, put h5dump output into <filename>.txt and compare it to <filename>.exp
#   DDL_FILE <ddlname> - if provided, have h5dump generate <ddlname>.ddl and compare it to <ddlname>.exp
#   RESULT_CHECK <string> - if provided, use <string> as TEST_REFERENCE instead of <testname>.ddl
#   H5ERRREF <errref_string> - if provided, expect the error output from h5dump to contain this string
#   ENVVAR <varname> - if provided, set environment variable <varname> to ENVVAL. If memchecker is enabled, does nothing.
#   ENVVAL <value> - if provided, set environment variable ENVVAR to this value. If memchecker is enabled, does nothing.
#
# OPTIONAL MULTI-KEYWORD ARGUMENTS
#   ANY_PATHS <paths>   - The -N/--any_path argument(s) to h5dump.
#
macro (ADD_H5_TEST testname)
  cmake_parse_arguments (ARG
      "BINARY_OUTPUT;MASK_ERROR;GREP_COMPARE;BINFILE;SKIP_TEST;NATIVE_ONLY" # Flags
      "RESULT_CODE;APPLY_FILTERS;TARGET_FILE;OUTPUT_FILE;DDL_FILE;H5ERRREF;ENVVAR;ENVVAL" # Single value args
      "ANY_PATHS" # Multi value args
      ${ARGN}
  )

  # Validate required parameters
  if (NOT DEFINED ARG_RESULT_CODE)
    message (FATAL_ERROR "ADD_H5_TEST: RESULT_CODE is required")
  endif ()

  if (NOT DEFINED ARG_TARGET_FILE)
    message (FATAL_ERROR "ADD_H5_TEST: TARGET_FILE is required")
  endif ()

  # Validate optional parameters
  if (DEFINED ARG_APPLY_FILTERS)
    if ("${ARG_APPLY_FILTERS}" STREQUAL "")
      # default
      set (_FILTER_VAL "1")
    else ()
      set (_FILTER_VAL "${ARG_APPLY_FILTERS}")
    endif ()

    set (filters_in "")
    set (filters_out "")

    list (APPEND filters_in "SIZE [0-9]* \\(${_FILTER_VAL}\\\.[0-9][0-9][0-9]:1 COMPRESSION\\)")
    list (APPEND filters_out "SIZE XXXX (${_FILTER_VAL}.XXX:1 COMPRESSION)")

    # Mask out the h5dump-assigned anonymous committed datatype numbers
    # Datatype number may have a forward slash before #
    list (APPEND filters_in "DATATYPE[ \t]*\"(/?#)[0-9]+\"")
    list (APPEND filters_out "DATATYPE \"#XXXX\"")

    # With -n, oputput that must be filtered appears different
    list (APPEND filters_in "datatype [ \t]*(/?#)[0-9]+")
    list (APPEND filters_out "datatype [ \t]*(/?#)XXXX")
  
    # Size/Offset within file will differ across VOL connectors
    list (APPEND filters_in "OFFSET [0-9]+")
    list (APPEND filters_out "OFFSET XXXX")

    list (APPEND filters_in "SIZE [0-9]+")
    list (APPEND filters_out "SIZE XXXX")
  else ()
    set (filters_in "")
    set (filters_out "")
  endif ()

  if (DEFINED ARG_ANY_PATHS)
    set (temp_path_arg "")
    foreach (arg_path IN LISTS ARG_ANY_PATHS)
      if (arg_path STREQUAL "")
        message (FATAL_ERROR "ADD_H5_TEST: ANY_PATHS requires a path")
      endif ()

      list (APPEND temp_path_arg "--any_path=${arg_path}")
    endforeach ()

    set (ARG_ANY_PATHS ${temp_path_arg})
  endif ()

  # both of these args want to define argument to -o flag
  if (DEFINED ARG_OUTPUT_FILE AND ${ARG_BINFILE}) 
    message  (FATAL_ERROR "ADD_H5_TEST: OUTPUT_FILE and BINFILE are mutually exclusive")
  endif()

  if (DEFINED ARG_OUTPUT_FILE)
    set (ARG_OUTPUT_FILEARGS "-o" "${ARG_OUTPUT_FILE}.txt")
  elseif (${ARG_BINFILE})
    set (ARG_OUTPUT_FILEARGS "-o" "${testname}.bin")
  else ()
    set (ARG_OUTPUT_FILEARGS "")
  endif ()

  # handle arguments that modify ctest testname
  set (ctest_testname ${testname})

  if (${ARG_BINFILE})
    set (ctest_testname "BIN_EXPORT-${testname}")
  elseif (DEFINED ARG_ANY_PATHS)
    set (ctest_testname "N-${testname}")
  elseif (DEFINED ARG_OUTPUT_FILE AND ARG_BINARY_OUTPUT)
    set (ctest_testname "output-${testname}")
  endif ()

  # Set up list of files to clean up
  set (DO_CLEANUP FALSE)
  set (CLEANUP_DEPENDENCIES "")

  if (DEFINED ARG_TARGET_FILE OR DEFINED ARG_OUTPUT_FILE OR DEFINED ARG_DDL_FILE OR DEFINED ARG_ANY_PATHS OR ${ARG_BINFILE})
    set (DO_CLEANUP TRUE)
  endif ()

  if (DEFINED ARG_ENVVAL AND NOT DEFINED ARG_ENVVAR)
    message (FATAL_ERROR "ADD_H5_TEST: ENVVAL requires ENVVAR")
  endif ()

  if (DEFINED ARG_ENVVAR AND NOT DEFINED ARG_ENVVAL)
    message (FATAL_ERROR "ADD_H5_TEST: ENVVAR requires ENVVAL")
  endif ()

  if (DEFINED ARG_DDL_FILE)
    set (ARG_DDL_FILE_CMD "--ddl=${ARG_DDL_FILE}.txt")
  else ()
    set (ARG_DDL_FILE_CMD "")
  endif ()

  if (${ARG_BINARY_OUTPUT})
    if (NOT DEFINED ARG_OUTPUT_FILE)
      message (FATAL_ERROR "ADD_H5_TEST: BINARY_OUTPUT flag requires OUTPUT_FILE")
    endif ()

    set (BINARY_OUTPUT_FLAG "-b")
  else ()
    set (BINARY_OUTPUT_FLAG "")
  endif ()

  if (DEFINED ${ARG_RESULT_CHECK})
    set (ARG_RESULT_CHECK_FILE "${ARG_RESULT_CHECK}")
  else ()
    set (ARG_RESULT_CHECK_FILE "${testname}.ddl")
  endif ()

  # Certain args are fully incompatible with memchecker; skip in these cases
  set(should_skip_test FALSE)

  if (HDF5_ENABLE_USING_MEMCHECKER)
    if (DEFINED ARG_H5ERRREF OR ${ARG_BINFILE} OR ${ARG_GREP_COMPARE})
      set (should_skip_test TRUE)
    endif ()
  endif ()

  if (${ARG_SKIP_TEST})
    set (should_skip_test TRUE)
  endif ()

  if (${ARG_NATIVE_ONLY})
    set (num_ext_vols 0)
  else ()
    list (LENGTH HDF5_EXTERNAL_VOL_TARGETS num_ext_vols)
  endif ()

  # Add a test for the native connector and each external VOL connector
  foreach (vol_idx RANGE 0 ${num_ext_vols})
    set (vol_env "")

    # First, populate VOL info to be passed to tests
    if (${vol_idx} EQUAL 0)
      set (vol "native")
      set (vol_prefix "")
      set (vol_env "${CROSSCOMPILING_PATH}")
      set (workdir "${PROJECT_BINARY_DIR}/testfiles/std")
    else ()
      # An external VOL connector
      math (EXPR vol_idx_fixed "${vol_idx} - 1")
      list (GET HDF5_EXTERNAL_VOL_TARGETS ${vol_idx_fixed} ext_vol_tgt)
      HDF5_GET_VOL_TGT_INFO (${ext_vol_tgt} vol vol_env)

      set (vol_prefix "HDF5_VOL_${vol}-")

      set (workdir "${PROJECT_BINARY_DIR}/${vol}/testfiles/std")
      # Isolate plugin path string
      string (FIND "${vol_env}" "HDF5_PLUGIN_PATH=" vol_plugin_path_posn)

      if (vol_plugin_path_posn GREATER -1)
        # Grab path string after HDF5_PLUGIN_PATH=
        string (LENGTH "HDF5_PLUGIN_PATH=" path_prefix_len)
        math (EXPR vol_plugin_path_posn "${vol_plugin_path_posn} + ${path_prefix_len}")
        string (SUBSTRING "${vol_env}" ${vol_plugin_path_posn} -1 vol_plugin_path)
      else ()
        set (vol_plugin_path "")
      endif ()
    endif () # env VOL arg setup

    # Clean up if test produces artifacts
    if (${DO_CLEANUP})
      add_test (
          NAME ${vol_prefix}H5DUMP-${ctest_testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove
            "${testname}.txt"
            "${ARG_OUTPUT_FILE}.txt"
            "${ARG_DDL_FILE}.txt"
            "${testname}.bin"

      )

      set_tests_properties (${vol_prefix}H5DUMP-${ctest_testname}-clear-objects PROPERTIES
          WORKING_DIRECTORY "${workdir}"
      )
    endif ()

    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER AND NOT ARG_MASK_ERROR AND NOT ARG_GREP_COMPARE AND NOT DEFINED ARG_H5ERRREF)
      add_test (NAME ${vol_prefix}H5DUMP-${ctest_testname} COMMAND $<TARGET_FILE:h5dump> ${ARG_ANY_PATHS} ${ARG_UNPARSED_ARGUMENTS} ${ARG_DDL_FILE_CMD} ${BINARY_OUTPUT_FLAG} ${ARG_OUTPUT_FILEARGS} ${ARG_TARGET_FILE})
      if (${ARG_RESULT_CODE})
        set_tests_properties (${vol_prefix}H5DUMP-${ctest_testname}
            PROPERTIES WILL_FAIL "true"
            ENVIRONMENT "${CROSSCOMPILING_PATH}"
        )
      endif ()
    else ()
      add_test (
          NAME ${vol_prefix}H5DUMP-${ctest_testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARG_ANY_PATHS};${ARG_UNPARSED_ARGUMENTS};${ARG_DDL_FILE_CMD};${BINARY_OUTPUT_FLAG};${ARG_OUTPUT_FILEARGS};${ARG_TARGET_FILE}"
              -D "TEST_FOLDER=${workdir}"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_EXPECT=${ARG_RESULT_CODE}"
              -D "TEST_REFERENCE=${ARG_RESULT_CHECK_FILE}"
              -D "TEST_FILTER:STRING=${filters_in}"
              -D "TEST_FILTER_REPLACE:STRING=${filters_out}"
              -D "TEST_MASK_ERROR:BOOL=${ARG_MASK_ERROR}"
              -D "TEST_GREP_COMPARE:BOOL=${ARG_GREP_COMPARE}"
              -D "TEST_ERRREF=${ARG_H5ERRREF}"
              -D "TEST_ENV_VAR:STRING=${ARG_ENVVAR}"
              -D "TEST_ENV_VALUE:STRING=${ARG_ENVVAL}"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (${vol_prefix}H5DUMP-${ctest_testname} PROPERTIES
        WORKING_DIRECTORY "${workdir}"
    )

    # Set VOL-specific properties
    if (NOT "${vol}" STREQUAL "native")
      set_tests_properties (${vol_prefix}H5DUMP-${ctest_testname} PROPERTIES
          ENVIRONMENT "${vol_env}"
          FIXTURES_REQUIRED "h5dump_vol_files"
      )
    else ()
      set_tests_properties (${vol_prefix}H5DUMP-${ctest_testname} PROPERTIES
          ENVIRONMENT "${vol_env}"
      )
    endif ()

    if ("${vol_prefix}H5DUMP-${ctest_testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (${vol_prefix}H5DUMP-${ctest_testname} PROPERTIES DISABLED true)
    endif ()
      
    set (CLEANUP_DEPENDENCIES "${vol_prefix}H5DUMP-${ctest_testname}")

    if (DEFINED ARG_TARGET_FILE AND DEFINED ARG_OUTPUT_FILE)
      add_test (
          NAME ${vol_prefix}H5DUMP-${ctest_testname}-output-cmp
          COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol ${testname}.txt ${testname}.exp
      )

      set_tests_properties (${vol_prefix}H5DUMP-${ctest_testname}-output-cmp PROPERTIES
          DEPENDS H5DUMP-${ctest_testname}
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
          WORKING_DIRECTORY "${workdir}"
      )

      if ("${vol_prefix}H5DUMP-${ctest_testname}-output-cmp" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (${vol_prefix}H5DUMP-${ctest_testname}-output-cmp PROPERTIES DISABLED true)
      endif ()

      list (APPEND CLEANUP_DEPENDENCIES "${vol_prefix}H5DUMP-${ctest_testname}-output-cmp")
    endif ()

    if (${DO_CLEANUP})
      add_test (
          NAME ${vol_prefix}H5DUMP-${ctest_testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              "${testname}.txt"
              "${ARG_OUTPUT_FILE}.txt"
              "${ARG_DDL_FILE}.txt"
              "${testname}.bin"
      )

      set_tests_properties (${vol_prefix}H5DUMP-${ctest_testname}-clean-objects PROPERTIES
          DEPENDS "${CLEANUP_DEPENDENCIES}"
          WORKING_DIRECTORY "${workdir}"
      )
    endif ()

    # Mark the test as disabled if needed
    if (should_skip_test)
      set_tests_properties (${vol_prefix}H5DUMP-${ctest_testname} PROPERTIES DISABLED true)
    endif ()
  endforeach () # per-VOL loop
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
    set_tests_properties (H5DUMP-IMPORT-${resultfile}-clear-objects PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
    )
    add_test (
        NAME H5DUMP-IMPORT-${resultfile}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
            -D "TEST_ARGS:STRING=${ARGN};-o;${resultfile}.bin;${testfile}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
            -D "TEST_OUTPUT=${conffile}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=${conffile}.ddl"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (H5DUMP-IMPORT-${resultfile} PROPERTIES
        DEPENDS H5DUMP-IMPORT-${resultfile}-clear-objects
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
    )
    if ("H5DUMP-IMPORT-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5DUMP-IMPORT-${resultfile} PROPERTIES DISABLED true)
    endif ()
    add_test (NAME H5DUMP-IMPORT-h5import-${resultfile} COMMAND $<TARGET_FILE:h5import> ${resultfile}.bin -c ${conffile}.out -o ${resultfile}.h5)
    set_tests_properties (H5DUMP-IMPORT-h5import-${resultfile} PROPERTIES
        DEPENDS H5DUMP-IMPORT-${resultfile}
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
    )
    if ("H5DUMP-IMPORT-h5import-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5DUMP-IMPORT-h5import-${resultfile} PROPERTIES DISABLED true)
    endif ()
    add_test (NAME H5DUMP-IMPORT-h5diff-${resultfile} COMMAND $<TARGET_FILE:h5diff> ${testfile} ${resultfile}.h5 /integer /integer)
    set_tests_properties (H5DUMP-IMPORT-h5diff-${resultfile} PROPERTIES
        DEPENDS H5DUMP-IMPORT-h5import-${resultfile}
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
    )
    if ("H5DUMP-IMPORT-h5diff-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5DUMP-IMPORT-h5diff-${resultfile} PROPERTIES DISABLED true)
    endif ()
    add_test (
        NAME H5DUMP-IMPORT-${resultfile}-clean-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            ${resultfile}.bin
            ${resultfile}.h5
    )
    set_tests_properties (H5DUMP-IMPORT-${resultfile}-clean-objects PROPERTIES
        DEPENDS H5DUMP-IMPORT-h5diff-${resultfile}
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
    )
  endif ()
endmacro ()

macro (ADD_H5_UD_TEST testname resultcode resultfile)
  if (NOT HDF5_ENABLE_USING_MEMCHECKER)
    add_test (
        NAME H5DUMP_UD-${testname}-${resultfile}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
            -D "TEST_OUTPUT=${resultfile}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=${resultfile}.ddl"
            -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
            -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (H5DUMP_UD-${testname}-${resultfile} PROPERTIES
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
    )
    if ("H5DUMP_UD-${testname}-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5DUMP_UD-${testname}-${resultfile} PROPERTIES DISABLED true)
    endif ()
  endif ()
endmacro ()

macro (ADD_H5_S3TEST resultfile resultcode credtype urlscheme urlpath)
  # If using memchecker add tests without using scripts
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME H5DUMP_S3TEST-${resultfile}_${urlscheme}_${credtype} COMMAND $<TARGET_FILE:h5dump> ${ARGN})
    if (${resultcode})
      set_tests_properties (H5DUMP_S3TEST-${resultfile}_${urlscheme}_${credtype} PROPERTIES WILL_FAIL "true")
    endif ()
    set_tests_properties (H5DUMP_S3TEST-${resultfile}_${urlscheme}_${credtype} PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/S3TEST"
    )
  else ()
    add_test (
        NAME H5DUMP_S3TEST-${resultfile}_${urlscheme}_${credtype}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
            -D "TEST_ARGS:STRING=--enable-error-stack=2;${ARGN};${urlscheme}://${urlpath}/${resultfile}.h5"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/S3TEST"
            -D "TEST_OUTPUT=${resultfile}_${urlscheme}_${credtype}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=${resultfile}.ddl"
            -D "TEST_ENV_VAR:STRING=AWS_SHARED_CREDENTIALS_FILE"
            -D "TEST_ENV_VALUE:STRING=${CMAKE_BINARY_DIR}/credentials"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (H5DUMP_S3TEST-${resultfile}_${urlscheme}_${credtype} PROPERTIES
      FIXTURES_REQUIRED h5dump_s3_proxy
      ENVIRONMENT "${h5dump_s3tests_env};${CROSSCOMPILING_PATH}"
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/S3TEST
  )
  if ("H5DUMP_S3TEST-${resultfile}_${urlscheme}_${credtype}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (H5DUMP_S3TEST-${resultfile}_${urlscheme}_${credtype} PROPERTIES DISABLED true)
  endif ()
endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

ADD_HELP_TEST (help 0 -h)

# test data output redirection
#ADD_H5_TEST (tnoddl RESULT_CODE RESULT_CODE 0 --enable-error-stack -O -y TARGET_FILE packedbits.h5)
ADD_H5_TEST (tnoddl RESULT_CODE 0 --enable-error-stack --ddl -y TARGET_FILE packedbits.h5)
#ADD_H5_TEST (tnodata RESULT_CODE 0 --enable-error-stack -o TARGET_FILE packedbits.h5)
ADD_H5_TEST (tnodata RESULT_CODE 0 --enable-error-stack --output TARGET_FILE packedbits.h5)
ADD_H5_TEST (tnoattrddl RESULT_CODE 0 --enable-error-stack -O -y TARGET_FILE tattr.h5)
ADD_H5_TEST (tnoattrdata RESULT_CODE 0 --enable-error-stack -A -o TARGET_FILE tattr.h5)
ADD_H5_TEST (trawdatafile OUTPUT_FILE trawdatafile TARGET_FILE packedbits.h5 RESULT_CODE 0 --enable-error-stack -y)
ADD_H5_TEST (tnoddlfile OUTPUT_FILE tnoddlfile TARGET_FILE packedbits.h5 RESULT_CODE 0 --enable-error-stack -O -y)
ADD_H5_TEST (trawssetfile OUTPUT_FILE trawssetfile TARGET_FILE tdset.h5 RESULT_CODE 0 --enable-error-stack -d "/dset1[1,1;;;]" -y)

ADD_H5_TEST (twithddlfile OUTPUT_FILE twithddlfile TARGET_FILE packedbits.h5 RESULT_CODE 0 DDL_FILE twithddl --enable-error-stack --ddl=twithddl.txt -y)

# test for maximum display datasets
ADD_H5_TEST (twidedisplay RESULT_CODE 0 --enable-error-stack -w0 TARGET_FILE packedbits.h5)

# test for unwritten datasets
ADD_H5_TEST (tintsnodata RESULT_CODE 0 --enable-error-stack -p TARGET_FILE tintsnodata.h5)

# test for signed/unsigned datasets
ADD_H5_TEST (packedbits RESULT_CODE 0 --enable-error-stack TARGET_FILE packedbits.h5)
# test for compound signed/unsigned datasets
ADD_H5_TEST (tcmpdintarray RESULT_CODE 0 --enable-error-stack TARGET_FILE tcmpdintarray.h5)
ADD_H5_TEST (tcmpdints RESULT_CODE 0 --enable-error-stack TARGET_FILE tcmpdints.h5)
ADD_H5_TEST (tcmpdintsize RESULT_CODE 0 --enable-error-stack TARGET_FILE tcmpdintsize.h5)
# test for signed/unsigned scalar datasets
ADD_H5_TEST (tscalarintsize RESULT_CODE 0 --enable-error-stack TARGET_FILE tscalarintsize.h5)
# test for signed/unsigned attributes
ADD_H5_TEST (tattrintsize RESULT_CODE 0 --enable-error-stack TARGET_FILE tattrintsize.h5)
# test for compound signed/unsigned attributes
ADD_H5_TEST (tcmpdattrintsize RESULT_CODE 0 --enable-error-stack TARGET_FILE tcmpdattrintsize.h5)
# test for signed/unsigned scalar attributes
ADD_H5_TEST (tscalarattrintsize RESULT_CODE 0 --enable-error-stack TARGET_FILE tscalarattrintsize.h5)
# test for string scalar dataset and attribute
ADD_H5_TEST (tscalarstring RESULT_CODE 0 --enable-error-stack TARGET_FILE tscalarstring.h5)
# test for signed/unsigned scalar datasets with attributes
ADD_H5_TEST (tscalarintattrsize RESULT_CODE 0 --enable-error-stack TARGET_FILE tscalarintattrsize.h5)
# test for signed/unsigned datasets attributes
ADD_H5_TEST (tintsattrs RESULT_CODE 0 --enable-error-stack TARGET_FILE tintsattrs.h5)
# test for displaying groups
ADD_H5_TEST (tgroup-1 RESULT_CODE 0 --enable-error-stack TARGET_FILE tgroup.h5)
# test for displaying the selected groups
ADD_H5_TEST (tgroup-2 RESULT_CODE 1 H5ERRREF "h5dump error: unable to open group \"/y\"" --enable-error-stack --group=/g2 --group / -g /y TARGET_FILE tgroup.h5)

# test for displaying simple space datasets
ADD_H5_TEST (tdset-1 RESULT_CODE 0 --enable-error-stack TARGET_FILE tdset.h5)
# test for displaying selected datasets
ADD_H5_TEST (tdset-2 RESULT_CODE 1 H5ERRREF "h5dump error: unable to get link info from \"dset3\"" --enable-error-stack -H -d dset1 -d /dset2 --dataset=dset3 TARGET_FILE tdset.h5)

# test for displaying attributes
ADD_H5_TEST (tattr-1 RESULT_CODE 0 --enable-error-stack TARGET_FILE tattr.h5)
# test for displaying the selected attributes of string type and scalar space
ADD_H5_TEST (tattr-2 RESULT_CODE 0 --enable-error-stack -a /\\\\/attr1 --attribute /attr4 --attribute=/attr5 TARGET_FILE tattr.h5)
ADD_H5_TEST (tattr-2-N RESULT_CODE 0 --enable-error-stack  TARGET_FILE tattr.h5 ANY_PATHS /\\\\/attr1 /attr4 /attr5)
# test for header and error messages
ADD_H5_TEST (tattr-3 RESULT_CODE 1 H5ERRREF "h5dump error: unable to open attribute \"attr\"" --enable-error-stack --header -a /attr2 --attribute=/attr TARGET_FILE tattr.h5)
# test for displaying at least 9 attributes on root from a be machine
ADD_H5_TEST (tattr-4_be RESULT_CODE 0 --enable-error-stack TARGET_FILE tattr4_be.h5)
# test for displaying attributes in shared datatype (also in group and dataset)
ADD_H5_TEST (tnamed_dtype_attr RESULT_CODE 0 --enable-error-stack TARGET_FILE tnamed_dtype_attr.h5)

# test for displaying soft links and user-defined links
ADD_H5_TEST (tslink-1 RESULT_CODE 0 --enable-error-stack TARGET_FILE tslink.h5)
ADD_H5_TEST (tudlink-1 RESULT_CODE 0 --enable-error-stack TARGET_FILE tudlink.h5)
# test for displaying the selected link
ADD_H5_TEST (tslink-2 RESULT_CODE 0 --enable-error-stack -l slink2 TARGET_FILE tslink.h5)
ADD_H5_TEST (tslink-2-N RESULT_CODE 0 --enable-error-stack TARGET_FILE tslink.h5 ANY_PATHS slink2 )
ADD_H5_TEST (tudlink-2 RESULT_CODE 0 --enable-error-stack -l udlink2 TARGET_FILE tudlink.h5)
# test for displaying dangling soft links
ADD_H5_TEST (tslink-D RESULT_CODE 0 H5ERRREF "component not found" --enable-error-stack -d /slink1 TARGET_FILE tslink.h5)

# tests for hard links
ADD_H5_TEST (thlink-1 RESULT_CODE 0 --enable-error-stack TARGET_FILE thlink.h5)
ADD_H5_TEST (thlink-2 RESULT_CODE 0 --enable-error-stack -d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 TARGET_FILE thlink.h5)
ADD_H5_TEST (thlink-3 RESULT_CODE 0 --enable-error-stack -d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 TARGET_FILE thlink.h5)
ADD_H5_TEST (thlink-4 RESULT_CODE 0 --enable-error-stack -g /g1 TARGET_FILE thlink.h5)
ADD_H5_TEST (thlink-4-N RESULT_CODE 0 --enable-error-stack TARGET_FILE thlink.h5 ANY_PATHS /g1)
ADD_H5_TEST (thlink-5 RESULT_CODE 0 --enable-error-stack -d /dset1 -g /g2 -d /g1/dset2 TARGET_FILE thlink.h5)
ADD_H5_TEST (thlink-5-N RESULT_CODE 0 --enable-error-stack TARGET_FILE thlink.h5 ANY_PATHS /dset1 /g2 /g1/dset2)

# tests for compound data types
ADD_H5_TEST (tcomp-1 RESULT_CODE 0 --enable-error-stack TARGET_FILE tcompound.h5 APPLY_FILTERS 0)
# test for named data types
ADD_H5_TEST (tcomp-2 RESULT_CODE 0 --enable-error-stack -t /type1 --datatype /type2 --datatype=/group1/type3 TARGET_FILE tcompound.h5)
ADD_H5_TEST (tcomp-2-N RESULT_CODE 0 --enable-error-stack TARGET_FILE tcompound.h5 ANY_PATHS /type1 /type2 /group1/type3)
# test for unnamed type - restrict to native VOL since other VOLs will assign different numbers
ADD_H5_TEST (tcomp-3 RESULT_CODE 0 H5ERRREF "object '#6632' doesn't exist" "--enable-error-stack;-t;/#6632;-g;/group2" TARGET_FILE tcompound.h5 NATIVE_ONLY)
# test complicated compound datatype
ADD_H5_TEST (tcomp-4 RESULT_CODE 0 --enable-error-stack TARGET_FILE tcompound_complex.h5)
ADD_H5_TEST (tcompound_complex2 RESULT_CODE 0 --enable-error-stack TARGET_FILE tcompound_complex2.h5)
# tests for bitfields and opaque data types
if (H5_WORDS_BIGENDIAN)
  ADD_H5_TEST (tbitnopaque_be RESULT_CODE 0 --enable-error-stack TARGET_FILE tbitnopaque.h5)
else ()
  ADD_H5_TEST (tbitnopaque_le RESULT_CODE 0 --enable-error-stack TARGET_FILE tbitnopaque.h5)
endif ()

# test for the nested compound type
ADD_H5_TEST (tnestcomp-1 RESULT_CODE 0 --enable-error-stack TARGET_FILE tnestedcomp.h5)
ADD_H5_TEST (tnestedcmpddt RESULT_CODE 0 --enable-error-stack TARGET_FILE tnestedcmpddt.h5)

# test for options
ADD_H5_TEST (tall-1 RESULT_CODE 0 H5ERRREF "unable to open external file, external link file name = 'somefile'" --enable-error-stack TARGET_FILE tall.h5)
ADD_H5_TEST (tall-2 RESULT_CODE 0 --enable-error-stack --header -g /g1/g1.1 -a attr2 TARGET_FILE tall.h5)
ADD_H5_TEST (tall-3 RESULT_CODE 0 --enable-error-stack -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink TARGET_FILE tall.h5)
ADD_H5_TEST (tall-3-N RESULT_CODE 0 --enable-error-stack  TARGET_FILE tall.h5 ANY_PATHS /g2/dset2.1 /g1/g1.2/g1.2.1/slink)
ADD_H5_TEST (tall-7 RESULT_CODE 0 --enable-error-stack -a attr1 TARGET_FILE tall.h5)
ADD_H5_TEST (tall-7N RESULT_CODE 0 --enable-error-stack TARGET_FILE tall.h5 ANY_PATHS attr1)

# test for loop detection
ADD_H5_TEST (tloop-1 RESULT_CODE 0 --enable-error-stack TARGET_FILE tloop.h5)

# test for string
ADD_H5_TEST (tstr-1 RESULT_CODE 0 --enable-error-stack TARGET_FILE tstr.h5)
ADD_H5_TEST (tstr-2 RESULT_CODE 0 --enable-error-stack TARGET_FILE tstr2.h5)

# test for file created by Lib SAF team
ADD_H5_TEST (tsaf RESULT_CODE 0 --enable-error-stack TARGET_FILE tsaf.h5 NATIVE_ONLY)

# test for file with variable length data
ADD_H5_TEST (tvldtypes1 RESULT_CODE 0 --enable-error-stack TARGET_FILE tvldtypes1.h5)
ADD_H5_TEST (tvldtypes2 RESULT_CODE 0 --enable-error-stack TARGET_FILE tvldtypes2.h5)
ADD_H5_TEST (tvldtypes3 RESULT_CODE 0 --enable-error-stack TARGET_FILE tvldtypes3.h5)
ADD_H5_TEST (tvldtypes4 RESULT_CODE 0 --enable-error-stack TARGET_FILE tvldtypes4.h5)
ADD_H5_TEST (tvldtypes5 RESULT_CODE 0 --enable-error-stack TARGET_FILE tvldtypes5.h5)

# test for file with variable length string data
ADD_H5_TEST (tvlstr RESULT_CODE 0 --enable-error-stack TARGET_FILE tvlstr.h5)
ADD_H5_TEST (tvlenstr_array RESULT_CODE 0 --enable-error-stack TARGET_FILE tvlenstr_array.h5)

# test for files with array data
ADD_H5_TEST (tarray1 RESULT_CODE 0 --enable-error-stack TARGET_FILE tarray1.h5)
# # added for bug# 2092 - tarray1_big.h5
ADD_H5_TEST (tarray1_big RESULT_CODE 0 H5ERRREF "NULL token size" --enable-error-stack -R TARGET_FILE tarray1_big.h5)
ADD_H5_TEST (tarray2 RESULT_CODE 0 --enable-error-stack TARGET_FILE tarray2.h5)
ADD_H5_TEST (tarray3 RESULT_CODE 0 --enable-error-stack TARGET_FILE tarray3.h5)
ADD_H5_TEST (tarray4 RESULT_CODE 0 --enable-error-stack TARGET_FILE tarray4.h5)
ADD_H5_TEST (tarray5 RESULT_CODE 0 --enable-error-stack TARGET_FILE tarray5.h5)
ADD_H5_TEST (tarray6 RESULT_CODE 0 --enable-error-stack TARGET_FILE tarray6.h5)
ADD_H5_TEST (tarray7 RESULT_CODE 0 --enable-error-stack TARGET_FILE tarray7.h5)
ADD_H5_TEST (tarray8 RESULT_CODE 0 --enable-error-stack TARGET_FILE tarray8.h5)

# test for wildcards in filename (does not work with cmake)
# ADD_H5_TEST (tstarfile MASK_ERROR RESULT_CODE 0 --enable-error-stack -H -d Dataset1 TARGET_FILE tarr*.h5)
# ADD_H5_TEST (tqmarkfile MASK_ERROR RESULT_CODE 0 --enable-error-stack -H -d Dataset1 TARGET_FILE tarray?.h5)
ADD_H5_TEST (tmultifile RESULT_CODE 0 --enable-error-stack -H -d Dataset1 tarray2.h5 tarray3.h5 tarray4.h5 tarray5.h5 tarray6.h5 TARGET_FILE tarray7.h5)

# test for files with empty data
ADD_H5_TEST (tempty RESULT_CODE 0 --enable-error-stack TARGET_FILE tempty.h5)

# test for files with groups that have comments
ADD_H5_TEST (tgrp_comments RESULT_CODE 0 --enable-error-stack TARGET_FILE tgrp_comments.h5)

# test the --filedriver flag
ADD_H5_TEST (tsplit_file RESULT_CODE 0 --enable-error-stack --filedriver=split TARGET_FILE tsplit_file)
ADD_H5_TEST (tfamily RESULT_CODE 0 --enable-error-stack --filedriver=family TARGET_FILE tfamily%05d.h5)
ADD_H5_TEST (tmulti RESULT_CODE 0 --enable-error-stack --filedriver=multi TARGET_FILE tmulti)

# test for files with group names which reach > 1024 bytes in size
ADD_H5_TEST (tlarge_objname RESULT_CODE 0 --enable-error-stack -w157 TARGET_FILE tlarge_objname.h5)

# test '-A' to suppress data but print attr's
ADD_H5_TEST (tall-2A RESULT_CODE 0 H5ERRREF "unable to open external file, external link file name = 'somefile'" --enable-error-stack -A TARGET_FILE tall.h5)

# test '-A' to suppress attr's but print data
ADD_H5_TEST (tall-2A0 RESULT_CODE 0 H5ERRREF "unable to open external file, external link file name = 'somefile'" --enable-error-stack -A 0 TARGET_FILE tall.h5)

# test '-r' to print attributes in ASCII instead of decimal
ADD_H5_TEST (tall-2B RESULT_CODE 0 H5ERRREF "unable to open external file, external link file name = 'somefile'" --enable-error-stack -A -r TARGET_FILE tall.h5)

# test Subsetting
ADD_H5_TEST (tall-4s RESULT_CODE 0 --enable-error-stack --dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 TARGET_FILE tall.h5)
ADD_H5_TEST (tall-5s RESULT_CODE 0 --enable-error-stack -d "/g1/g1.1/dset1.1.2[0;2;10;]" TARGET_FILE tall.h5)
ADD_H5_TEST (tdset-3s RESULT_CODE 0 --enable-error-stack -d "/dset1[1,1;;;]" TARGET_FILE tdset.h5)
ADD_H5_TEST (tno-subset RESULT_CODE 0 --enable-error-stack --no-compact-subset -d "AHFINDERDIRECT::ah_centroid_t[0] it=0 tl=0" TARGET_FILE  tno-subset.h5)

ADD_H5_TEST (tints4dimsCount2 RESULT_CODE 0 --enable-error-stack -d FourDimInts -s 0,0,0,0 -c 2,2,2,2 TARGET_FILE tints4dims.h5)
ADD_H5_TEST (tints4dimsBlock2 RESULT_CODE 0 --enable-error-stack -d FourDimInts -s 0,0,0,0 -c 1,1,1,1 -k 2,2,2,2 TARGET_FILE tints4dims.h5)
ADD_H5_TEST (tints4dimsStride2 RESULT_CODE 0 --enable-error-stack -d FourDimInts -s 0,0,0,0 -S 2,2,2,2 -c 2,2,2,2 TARGET_FILE tints4dims.h5)
ADD_H5_TEST (tints4dimsCountEq RESULT_CODE 0 --enable-error-stack -d FourDimInts -s 0,0,0,0 -S 2,2,1,1 -k 1,2,1,1 -c 2,2,4,4 TARGET_FILE tints4dims.h5)
ADD_H5_TEST (tints4dimsBlockEq RESULT_CODE 0 --enable-error-stack -d FourDimInts -s 0,0,0,0 -S 2,2,1,1 -c 2,2,1,1 -k 1,2,4,4 TARGET_FILE tints4dims.h5)

# test printing characters in ASCII instead of decimal
ADD_H5_TEST (tchar1 RESULT_CODE 0 --enable-error-stack -r TARGET_FILE tchar.h5)

# test datatypes in ASCII and UTF8
ADD_H5_TEST (charsets RESULT_CODE 0 --enable-error-stack TARGET_FILE charsets.h5)

# rev. 2004
# tests for super block
ADD_H5_TEST (tboot1 RESULT_CODE 0 --enable-error-stack -H -B -d dset TARGET_FILE tfcontents1.h5)
ADD_H5_TEST (tboot2 RESULT_CODE 0 --enable-error-stack -B TARGET_FILE tfcontents2.h5)
ADD_H5_TEST (tboot2A RESULT_CODE 0 --enable-error-stack --boot-block TARGET_FILE tfcontents2.h5)
ADD_H5_TEST (tboot2B RESULT_CODE 0 --enable-error-stack --superblock TARGET_FILE tfcontents2.h5)
ADD_H5_TEST (file_space RESULT_CODE 0 --enable-error-stack -B TARGET_FILE file_space.h5)
ADD_H5_TEST (file_space_cache RESULT_CODE 0 --enable-error-stack=2 --page-buffer-size=16384 -B TARGET_FILE file_space.h5)

# test -p with a non existing dataset
ADD_H5_TEST (tperror RESULT_CODE 1 H5ERRREF "h5dump error: unable to get link info from \"bogus\"" --enable-error-stack -p -d bogus TARGET_FILE tfcontents1.h5)

# test for file contents
ADD_H5_TEST (tcontents RESULT_CODE 0 --enable-error-stack -n TARGET_FILE tfcontents1.h5 APPLY_FILTERS 0)
ADD_H5_TEST (tordercontents1 RESULT_CODE 0 --enable-error-stack -n --sort_by=name --sort_order=ascending TARGET_FILE tfcontents1.h5 APPLY_FILTERS 1)
ADD_H5_TEST (tordercontents2 RESULT_CODE 0 --enable-error-stack -n --sort_by=name --sort_order=descending TARGET_FILE tfcontents1.h5 APPLY_FILTERS 1)
ADD_H5_TEST (tattrcontents1 RESULT_CODE 0 --enable-error-stack -n 1 --sort_order=ascending TARGET_FILE tall.h5)
ADD_H5_TEST (tattrcontents2 RESULT_CODE 0 --enable-error-stack -n 1 --sort_order=descending TARGET_FILE tall.h5)

# tests for storage layout
# compact
ADD_H5_TEST (tcompact RESULT_CODE 0 --enable-error-stack -H -p -d compact TARGET_FILE tfilters.h5)
# contiguous
ADD_H5_TEST (tcontiguos RESULT_CODE 0 --enable-error-stack -H -p -d contiguous TARGET_FILE tfilters.h5)
# chunked
ADD_H5_TEST (tchunked RESULT_CODE 0 --enable-error-stack -H -p -d chunked TARGET_FILE tfilters.h5)
# external
ADD_H5_TEST (texternal RESULT_CODE 0 --enable-error-stack -H -p -d external TARGET_FILE tfilters.h5)

# fill values
ADD_H5_TEST (tfill RESULT_CODE 0 --enable-error-stack -p TARGET_FILE tfvalues.h5 APPLY_FILTERS 1)

# several datatype, with references , print path
ADD_H5_TEST (treference RESULT_CODE 0 --enable-error-stack  TARGET_FILE tattr2.h5)

# escape/not escape non printable characters
ADD_H5_TEST (tstringe RESULT_CODE 0 --enable-error-stack -e TARGET_FILE tstr3.h5)
ADD_H5_TEST (tstring RESULT_CODE 0 --enable-error-stack TARGET_FILE tstr3.h5)
# char data as ASCII with non escape
ADD_H5_TEST (tstring2 RESULT_CODE 0 --enable-error-stack -r -d str4 TARGET_FILE tstr3.h5)

# array indices print/not print
ADD_H5_TEST (tindicesyes RESULT_CODE 0 --enable-error-stack TARGET_FILE taindices.h5)
ADD_H5_TEST (tindicesno RESULT_CODE 0 --enable-error-stack -y TARGET_FILE taindices.h5)

########## array indices with subsetting
# 1D case
ADD_H5_TEST (tindicessub1 RESULT_CODE 0 --enable-error-stack -d 1d -s 1 -S 10 -c 2  -k 3 TARGET_FILE taindices.h5)

# 2D case
ADD_H5_TEST (tindicessub2 RESULT_CODE 0 --enable-error-stack -d 2d -s 1,2  -S 3,3 -c 3,2 -k 2,2 TARGET_FILE taindices.h5)

# 3D case
ADD_H5_TEST (tindicessub3 RESULT_CODE 0 --enable-error-stack -d 3d -s 0,1,2 -S 1,3,3 -c 2,2,2  -k 1,2,2  TARGET_FILE taindices.h5)

# 4D case
ADD_H5_TEST (tindicessub4 RESULT_CODE 0 --enable-error-stack -d 4d -s 0,0,1,2  -c 2,2,3,2 -S 1,1,3,3 -k 1,1,2,2  TARGET_FILE taindices.h5)

# Exceed the dimensions for subsetting
ADD_H5_TEST (texceedsubstart RESULT_CODE 1 H5ERRREF "exceed dataset dims" --enable-error-stack -d 1d -s 1,3 TARGET_FILE taindices.h5)
ADD_H5_TEST (texceedsubcount RESULT_CODE 1 H5ERRREF "exceed dataset dims" --enable-error-stack -d 1d -c 1,3 TARGET_FILE taindices.h5)
ADD_H5_TEST (texceedsubstride RESULT_CODE 1 H5ERRREF "exceed dataset dims" --enable-error-stack -d 1d -S 1,3 TARGET_FILE taindices.h5)
ADD_H5_TEST (texceedsubblock RESULT_CODE 1 H5ERRREF "exceed dataset dims" --enable-error-stack -d 1d -k 1,3 TARGET_FILE taindices.h5)

# tests for filters
# SZIP
# For VOL connectors, test files are generated during testing, which requires the filter itself to be available.
if (HDF5_ENABLE_SZIP_SUPPORT)
  set (SZIP_NATIVE_ONLY "")
else ()
  set (SZIP_NATIVE_ONLY "NATIVE_ONLY")
endif ()

ADD_H5_TEST (tszip RESULT_CODE 0 APPLY_FILTERS 2 --enable-error-stack -H -p -d szip TARGET_FILE tfilters.h5 ${SZIP_NATIVE_ONLY})

# deflate
# For VOL connectors, test files are generated during testing, which requires the filter itself to be available.
if (H5_HAVE_FILTER_DEFLATE)
  set (DEFLATE_NATIVE_ONLY "")
else ()
  set (DEFLATE_NATIVE_ONLY "NATIVE_ONLY")
endif ()
ADD_H5_TEST (tdeflate RESULT_CODE 0 APPLY_FILTERS 2 --enable-error-stack -H -p -d deflate TARGET_FILE tfilters.h5 ${DEFLATE_NATIVE_ONLY})

# shuffle
ADD_H5_TEST (tshuffle RESULT_CODE 0 --enable-error-stack -H -p -d shuffle TARGET_FILE tfilters.h5)

# fletcher32
ADD_H5_TEST (tfletcher32 RESULT_CODE 0 APPLY_FILTERS 0 --enable-error-stack -H -p -d fletcher32  TARGET_FILE tfilters.h5)

# nbit
ADD_H5_TEST (tnbit RESULT_CODE 0 APPLY_FILTERS 1 --enable-error-stack -H -p -d nbit  TARGET_FILE tfilters.h5)

# scaleoffset
ADD_H5_TEST (tscaleoffset RESULT_CODE 0 APPLY_FILTERS 4 --enable-error-stack -H -p -d scaleoffset  TARGET_FILE tfilters.h5)

# all
if (HDF5_ENABLE_SZIP_SUPPORT AND H5_HAVE_FILTER_DEFLATE)
  set (ALL_NATIVE_ONLY "")
else ()
  set (ALL_NATIVE_ONLY "NATIVE_ONLY")
endif ()
ADD_H5_TEST (tallfilters RESULT_CODE 0 APPLY_FILTERS 1 --enable-error-stack -H -p -d all  TARGET_FILE tfilters.h5 APPLY_FILTERS 1 ${ALL_NATIVE_ONLY})

# user defined
ADD_H5_TEST (tuserfilter RESULT_CODE 0 --enable-error-stack -H  -p -d myfilter  TARGET_FILE tfilters.h5)


# See which filters are usable (and skip tests for filters we
# don't have).  Do this by searching H5pubconf.h to see which
# filters are defined.

# detect whether the encoder is present.
if (H5_HAVE_FILTER_DEFLATE)
  # data read internal filters
  ADD_H5_TEST (treadintfilter RESULT_CODE 0 --enable-error-stack -d deflate -d shuffle -d fletcher32 -d nbit -d scaleoffset TARGET_FILE tfilters.h5)
  if (HDF5_ENABLE_SZIP_SUPPORT)
    # data read all filters
    ADD_H5_TEST (treadfilter RESULT_CODE 0 --enable-error-stack -d all -d szip TARGET_FILE tfilters.h5)
  endif ()
endif ()

# test for displaying objects with very long names
ADD_H5_TEST (tlonglinks RESULT_CODE 0 --enable-error-stack TARGET_FILE tlonglinks.h5)

# dimensions over 4GB, print boundary
ADD_H5_TEST (tbigdims RESULT_CODE 0 --enable-error-stack -d dset4gb -s 4294967284 -c 22 TARGET_FILE tbigdims.h5)

# hyperslab read
ADD_H5_TEST (thyperslab RESULT_CODE 0 --enable-error-stack TARGET_FILE thyperslab.h5)

# test for displaying dataset and attribute of null space
ADD_H5_TEST (tnullspace RESULT_CODE 0 --enable-error-stack TARGET_FILE tnullspace.h5)
ADD_H5_TEST (tgrpnullspace RESULT_CODE 0 -p --enable-error-stack TARGET_FILE tgrpnullspace.h5)

# test for displaying dataset and attribute of space with 0 dimension size
ADD_H5_TEST (zerodim RESULT_CODE 0 --enable-error-stack TARGET_FILE zerodim.h5)

# test for long double (some systems do not have long double)
ADD_H5_TEST (tfloatsattrs RESULT_CODE 0 -p --format=%.4g --lformat=%.4Lg --width=80 --enable-error-stack TARGET_FILE tfloatsattrs.h5)
ADD_H5_TEST (tldouble RESULT_CODE 0 --enable-error-stack TARGET_FILE tldouble.h5)
ADD_H5_TEST (tldouble_scalar RESULT_CODE 0 -p --enable-error-stack TARGET_FILE tldouble_scalar.h5)

# Add tests for _Float16 type
# VOL testing must generate the F16 files, which it can only do if F16 is enabled
# - so mark these tests as Native only if Float16 isn't enabled.
if (${${HDF_PREFIX}_HAVE__FLOAT16})
  set (F16_NATIVE_ONLY "")
else ()
  set (F16_NATIVE_ONLY "NATIVE_ONLY")
endif ()

ADD_H5_TEST (tfloat16 RESULT_CODE 0 --enable-error-stack TARGET_FILE tfloat16.h5 ${F16_NATIVE_ONLY})
ADD_H5_TEST (tfloat16_be RESULT_CODE 0 --enable-error-stack TARGET_FILE tfloat16_be.h5 ${F16_NATIVE_ONLY})

# Add tests for bfloat16 type
ADD_H5_TEST (tbfloat16 RESULT_CODE 0 --enable-error-stack TARGET_FILE tbfloat16.h5)
ADD_H5_TEST (tbfloat16_be RESULT_CODE 0 --enable-error-stack TARGET_FILE tbfloat16_be.h5)

# Add test for FP8 types - To avoid printing out of some values that are, or are currently
# interpreted as, NaN values, subsetting is used on specific datasets and printing out of
# attributes is currently disabled. The output format of NaN values is implementation-dependent
# and can vary across platforms, causing output file diffing failures. This should be
# adjusted once HDF5 can properly support non-IEEE floating-point formats.
ADD_H5_TEST (tfloat8 RESULT_CODE 0 --enable-error-stack -A 0 -d /DS8BITSE4M3
             -d /DS8BITSE4M3_ALLVALS --start="0,0" --stride="1,1" --count="15,1" --block="1,16"
             -d /DS8BITSE4M3_ALLVALS_CONVERT -d /DS8BITSE5M2
             -d /DS8BITSE5M2_ALLVALS --start="0,0" --stride="1,1" --count="15,1" --block="1,16"
             -d /DS8BITSE5M2_ALLVALS_CONVERT TARGET_FILE tfloat8.h5)

# Add tests for complex numbers. For portability, use a fixed floating-point
# precision and skip dumping of the "long double _Complex" dataset. The "long
# double _Complex" dataset may display differently across platforms, e.g.
# between Linux and Windows, due to the size of "long double" and is only
# affected by the fixed floating-point precision option when
# sizeof(long double) != sizeof(double). Use -w80 after the floating-point
# format option since specifying a fixed floating-point precision resets h5dump's
# default number of columns value.
ADD_H5_TEST (tcomplex RESULT_CODE 0 --enable-error-stack -m %.6f -w80 -d ArrayDatasetFloatComplex
              -d CompoundDatasetFloatComplex -d DatasetDoubleComplex -d DatasetFloatComplex
              -d VariableLengthDatasetFloatComplex TARGET_FILE tcomplex.h5)
ADD_H5_TEST (tcomplex_info RESULT_CODE 0 --enable-error-stack -p -H -m %.6f -w80 -d ArrayDatasetFloatComplex
              -d CompoundDatasetFloatComplex -d DatasetDoubleComplex -d DatasetFloatComplex
              -d VariableLengthDatasetFloatComplex TARGET_FILE tcomplex.h5)
ADD_H5_TEST (tcomplex_be RESULT_CODE 0 --enable-error-stack -m %.6f -w80 -d ArrayDatasetFloatComplex
              -d CompoundDatasetFloatComplex -d DatasetDoubleComplex -d DatasetFloatComplex
              -d VariableLengthDatasetFloatComplex TARGET_FILE tcomplex_be.h5)
ADD_H5_TEST (tcomplex_be_info RESULT_CODE 0 --enable-error-stack -p -H -m %.6f -w80 -d ArrayDatasetFloatComplex
              -d CompoundDatasetFloatComplex -d DatasetDoubleComplex -d DatasetFloatComplex
              -d VariableLengthDatasetFloatComplex TARGET_FILE tcomplex_be.h5)

# test for vms
ADD_H5_TEST (tvms RESULT_CODE 0 --enable-error-stack TARGET_FILE tvms.h5)

# test for binary output
ADD_H5_TEST (tbin1LE BINFILE RESULT_CODE 0 TARGET_FILE tbinary.h5 --enable-error-stack -d integer -b LE)

# test for string binary output
ADD_H5_TEST (tstr2bin2 OUTPUT_FILE tstr2bin2 TARGET_FILE tstr2.h5 RESULT_CODE 0 --enable-error-stack -d /g2/dset2 BINARY_OUTPUT)
ADD_H5_TEST (tstr2bin6 OUTPUT_FILE tstr2bin6 TARGET_FILE tstr2.h5 RESULT_CODE 0 --enable-error-stack -d /g6/dset6 BINARY_OUTPUT)

# NATIVE default. the NATIVE test can be validated with h5import/h5diff
#  ADD_H5_TEST_IMPORT (tbin1 out1D TARGET_FILE tbinary.h5 0 --enable-error-stack -d integer -b)

if (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ADD_H5_TEST (tbin2 BINFILE RESULT_CODE 0 TARGET_FILE tbinary.h5 --enable-error-stack -b BE -d float)
endif ()

# the NATIVE test can be validated with h5import/h5diff
#  ADD_H5_TEST_IMPORT (tbin3 out3D TARGET_FILE tbinary.h5 0 --enable-error-stack -d integer -b NATIVE)

if (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ADD_H5_TEST (tbin4 BINFILE RESULT_CODE 0 TARGET_FILE tbinary.h5 --enable-error-stack -d double -b FILE)
endif ()

# test for dataset region references
ADD_H5_TEST (tdatareg RESULT_CODE 0 --enable-error-stack TARGET_FILE tdatareg.h5)
ADD_H5_TEST (tdataregR RESULT_CODE 0 H5ERRREF "NULL token size" --enable-error-stack -R TARGET_FILE tdatareg.h5)
ADD_H5_TEST (tattrreg RESULT_CODE 0 --enable-error-stack TARGET_FILE tattrreg.h5)
ADD_H5_TEST (tattrregR RESULT_CODE 0 H5ERRREF "NULL token size" -R --enable-error-stack TARGET_FILE tattrreg.h5)
ADD_H5_TEST (tbinregR OUTPUT_FILE tbinregR TARGET_FILE tdatareg.h5 RESULT_CODE 0 --enable-error-stack -d /Dataset1 -s 0 -R -y)

# test for 1.12 region references
ADD_H5_TEST (trefer_attrR RESULT_CODE 0 --enable-error-stack -R TARGET_FILE trefer_attr.h5)
ADD_H5_TEST (trefer_compatR RESULT_CODE 0 --enable-error-stack -R TARGET_FILE trefer_compat.h5)
ADD_H5_TEST (trefer_extR RESULT_CODE 0 --enable-error-stack -R TARGET_FILE trefer_ext2.h5)
ADD_H5_TEST (trefer_grpR RESULT_CODE 0 --enable-error-stack -R TARGET_FILE trefer_grp.h5)
ADD_H5_TEST (trefer_obj_delR RESULT_CODE 0 --enable-error-stack -R TARGET_FILE trefer_obj_del.h5)
ADD_H5_TEST (trefer_objR RESULT_CODE 0 --enable-error-stack -R TARGET_FILE trefer_obj.h5)
ADD_H5_TEST (trefer_paramR RESULT_CODE 0 --enable-error-stack -R TARGET_FILE trefer_param.h5)
ADD_H5_TEST (trefer_regR RESULT_CODE 0 --enable-error-stack -R TARGET_FILE trefer_reg.h5)
ADD_H5_TEST (trefer_reg_1dR RESULT_CODE 0 --enable-error-stack -R TARGET_FILE trefer_reg_1d.h5)

# tests for group creation order
# "1" tracked, "2" name, root tracked
ADD_H5_TEST (tordergr1 RESULT_CODE 0 --enable-error-stack --group=1 --sort_by=creation_order --sort_order=ascending TARGET_FILE tordergr.h5)
ADD_H5_TEST (tordergr2 RESULT_CODE 0 --enable-error-stack --group=1 --sort_by=creation_order --sort_order=descending TARGET_FILE tordergr.h5)
ADD_H5_TEST (tordergr3 RESULT_CODE 0 --enable-error-stack -g 2 -q name -z ascending TARGET_FILE tordergr.h5)
ADD_H5_TEST (tordergr4 RESULT_CODE 0 --enable-error-stack -g 2 -q name -z descending TARGET_FILE tordergr.h5)
ADD_H5_TEST (tordergr5 RESULT_CODE 0 --enable-error-stack -q creation_order TARGET_FILE tordergr.h5)

# tests for attribute order
ADD_H5_TEST (torderattr1 RESULT_CODE 0 --enable-error-stack -H --sort_by=name --sort_order=ascending TARGET_FILE torderattr.h5)
ADD_H5_TEST (torderattr2 RESULT_CODE 0 --enable-error-stack -H --sort_by=name --sort_order=descending TARGET_FILE torderattr.h5)
ADD_H5_TEST (torderattr3 RESULT_CODE 0 --enable-error-stack -H --sort_by=creation_order --sort_order=ascending TARGET_FILE torderattr.h5)
ADD_H5_TEST (torderattr4 RESULT_CODE 0 --enable-error-stack -H --sort_by=creation_order --sort_order=descending TARGET_FILE torderattr.h5)

# tests for link references and order
ADD_H5_TEST (torderlinks1 RESULT_CODE 0 H5ERRREF "unable to open external file, external link file name = 'fname'" --enable-error-stack --sort_by=name --sort_order=ascending TARGET_FILE tfcontents1.h5 APPLY_FILTERS 1)
ADD_H5_TEST (torderlinks2 RESULT_CODE 0 H5ERRREF "unable to open external file, external link file name = 'fname'" --enable-error-stack --sort_by=name --sort_order=descending TARGET_FILE tfcontents1.h5 APPLY_FILTERS 1)

# tests for floating point user defined printf format
ADD_H5_TEST (tfpformat RESULT_CODE 0 --enable-error-stack --format=%.7f TARGET_FILE tfpformat.h5)

# tests for traversal of external links
ADD_H5_TEST (textlinksrc RESULT_CODE 0 H5ERRREF "Too many soft links in path" --enable-error-stack TARGET_FILE textlinksrc.h5)
ADD_H5_TEST (textlinkfar RESULT_CODE 0 H5ERRREF "Too many soft links in path" --enable-error-stack TARGET_FILE textlinkfar.h5)

# test for dangling external links
ADD_H5_TEST (textlink RESULT_CODE 0 H5ERRREF "unable to open external file, external link file name = 'anotherfile'" --enable-error-stack TARGET_FILE textlink.h5)

# test for error stack display (BZ2048)
ADD_H5_TEST (filter_fail RESULT_CODE 1 H5ERRREF "filter plugins disabled" ENVVAR "HDF5_PLUGIN_PRELOAD" ENVVAL "::" --enable-error-stack TARGET_FILE filter_fail.h5 NATIVE_ONLY)

# test for -o -y for dataset with attributes
ADD_H5_TEST (tall-6 OUTPUT_FILE tall-6 TARGET_FILE tall.h5 RESULT_CODE 0 --enable-error-stack -d /g1/g1.1/dset1.1.1 -y)

# test for non-existing file
ADD_H5_TEST (non_existing RESULT_CODE 1 H5ERRREF "unable to open file" --enable-error-stack tgroup.h5 TARGET_FILE non_existing.h5)

# test to verify github issue#3790: infinite loop closing library
ADD_H5_TEST (infinite_loop RESULT_CODE 1 H5ERRREF "unable to open file" TARGET_FILE 3790_infinite_loop.h5)

# test to verify HDFFV-10333: error similar to H5O_attr_decode in the jira issue
ADD_H5_TEST (err_attr_dspace RESULT_CODE 1 H5ERRREF "error getting attribute information" TARGET_FILE err_attr_dspace.h5)

# test to verify HDFFV-9407: long double full precision
ADD_H5_TEST (t128bit_float RESULT_CODE 1 RESULT_ERRCHECK "1.123456789012345" -m %.35Lg TARGET_FILE t128bit_float.h5 SKIP_TEST)

# test to verify HDFFV-10480: out of bounds read in H5O_fill_new[old]_decode
ADD_H5_TEST (tCVE_2018_11206_fill_old RESULT_CODE 1 H5ERRREF "" TARGET_FILE tCVE_2018_11206_fill_old.h5)
ADD_H5_TEST (tCVE_2018_11206_fill_new RESULT_CODE 1 H5ERRREF "" TARGET_FILE tCVE_2018_11206_fill_new.h5)

# test to verify fix for CVE-2021-37501: multiplication overflow in H5O__attr_decode()
# https://github.com/ST4RF4LL/Something_Found/blob/main/HDF5_v1.13.0_h5dump_heap_overflow.assets/poc
ADD_H5_TEST (tCVE-2021-37501_attr_decode RESULT_CODE 1 H5ERRREF "error getting attribute information" TARGET_FILE tCVE-2021-37501_attr_decode.h5)

# onion VFD tests
ADD_H5_TEST (tst_onion_objs RESULT_CODE 0 --enable-error-stack --vfd-name onion --vfd-info 3 TARGET_FILE tst_onion_objs.h5)
ADD_H5_TEST (tst_onion_dset_ext RESULT_CODE 0 --enable-error-stack --vfd-name onion --vfd-info 1 TARGET_FILE tst_onion_dset_ext.h5)
ADD_H5_TEST (tst_onion_dset_1d RESULT_CODE 0 --enable-error-stack --vfd-name onion --vfd-info 1 TARGET_FILE tst_onion_dset_1d.h5)
ADD_H5_TEST (tst_onion_revision_count RESULT_CODE 0 --enable-error-stack --vfd-name onion --vfd-info revision_count TARGET_FILE tst_onion_objs.h5)


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

##############################################################
##############################################################
###           S 3   T E S T S                              ###
##############################################################
##############################################################
if (HDF5_ENABLE_ROS3_VFD_DOCKER_PROXY)
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/buckets")
  set (h5dump_s3tests_port 9002)

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
  set (h5dump_s3tests_env
      "AWS_ENDPOINT_URL=http://localhost:${h5dump_s3tests_port}"
      "HDF5_ROS3_VFD_FORCE_PATH_STYLE=1"
      "AWS_REGION=us-east-2"
      "AWS_PROFILE=ros3_vfd_test"
  )

  add_test (
      NAME H5DUMP-start-proxy
      COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=${DOCKER_EXECUTABLE}"
          -D "TEST_PRODUCT=andrewgaul/s3proxy"
          -D "TEST_PORT=${h5dump_s3tests_port}"
          -D "TEST_ARGS:STRING=s3proxy-local-h5dump"
          -D "TEST_BUCKET:STRING=h5dumpros3"
          -D "TEST_FILES:STRING=tattrintsize.h5"
          -D "TEST_ACLS:STRING=anon"
          -D "TEST_EXPECT=0"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/S3TEST"
          -D "TEST_ENV_VAR:STRING=AWS_SHARED_CREDENTIALS_FILE"
          -D "TEST_ENV_VALUE:STRING=${CMAKE_BINARY_DIR}/credentials"
          -P "${HDF_RESOURCES_DIR}/runProxy.cmake"
  )
  set_tests_properties (H5DUMP-start-proxy PROPERTIES FIXTURES_SETUP h5dump_s3_proxy)
  add_test (
      NAME H5DUMP-stop-proxy
      COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=${DOCKER_EXECUTABLE}"
          -D "TEST_ARGS:STRING=s3proxy-local-h5dump"
          -D "TEST_EXPECT=0"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/S3TEST"
          -P "${HDF_RESOURCES_DIR}/stopProxy.cmake"
  )
  set_tests_properties (H5DUMP-stop-proxy PROPERTIES FIXTURES_CLEANUP h5dump_s3_proxy)

  ADD_H5_S3TEST (tattrintsize 0 anon http localhost:${h5dump_s3tests_port}/h5dumpros3 --vfd-name=ros3 --s3-cred=\(,,\))
  ADD_H5_S3TEST (tattrintsize 0 anon s3 h5dumpros3 --filedriver=ros3 --vfd-name=ros3 --s3-cred=\(,,\) --endpoint-url=http://localhost:${h5dump_s3tests_port})
  ADD_H5_S3TEST (tattrintsize 0 profile http localhost:${h5dump_s3tests_port}/h5dumpros3 --vfd-name=ros3)
  ADD_H5_S3TEST (tattrintsize 0 profile s3 h5dumpros3 --filedriver=ros3 --vfd-name=ros3 --endpoint-url=http://localhost:${h5dump_s3tests_port})
  ADD_H5_S3TEST (tattrintsize 0 filename s3 h5dumpros3 --endpoint-url=http://localhost:${h5dump_s3tests_port})
endif ()
