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

  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  # tools/testfiles/vds
  set (LIST_VDS_TEST_FILES
      1_a.h5
      1_b.h5
      1_c.h5
      1_d.h5
      1_e.h5
      1_f.h5
      1_vds.h5
      2_a.h5
      2_b.h5
      2_c.h5
      2_d.h5
      2_e.h5
      2_vds.h5
      3_1_vds.h5
      3_2_vds.h5
      4_0.h5
      4_1.h5
      4_2.h5
      4_vds.h5
      5_a.h5
      5_b.h5
      5_c.h5
      5_vds.h5
  )
  set (LIST_DIFF_TEST_FILES
      h5diff_basic1.h5
      h5diff_basic2.h5
      h5diff_types.h5
      h5diff_dtypes.h5
      h5diff_attr1.h5
      h5diff_attr2.h5
      h5diff_attr3.h5
      h5diff_dset1.h5
      h5diff_dset2.h5
      h5diff_dset3.h5
      h5diff_hyper1.h5
      h5diff_hyper2.h5
      h5diff_empty.h5
      h5diff_links.h5
      h5diff_softlinks.h5
      h5diff_linked_softlink.h5
      h5diff_extlink_src.h5
      h5diff_extlink_trg.h5
      h5diff_ext2softlink_src.h5
      h5diff_ext2softlink_trg.h5
      h5diff_dset_zero_dim_size1.h5
      h5diff_dset_zero_dim_size2.h5
      h5diff_danglelinks1.h5
      h5diff_danglelinks2.h5
      h5diff_grp_recurse1.h5
      h5diff_grp_recurse2.h5
      h5diff_grp_recurse_ext1.h5
      h5diff_grp_recurse_ext2-1.h5
      h5diff_grp_recurse_ext2-2.h5
      h5diff_grp_recurse_ext2-3.h5
      h5diff_exclude1-1.h5
      h5diff_exclude1-2.h5
      h5diff_exclude2-1.h5
      h5diff_exclude2-2.h5
      h5diff_exclude3-1.h5
      h5diff_exclude3-2.h5
      h5diff_comp_vl_strs.h5
      h5diff_attr_v_level1.h5
      h5diff_attr_v_level2.h5
      h5diff_enum_invalid_values.h5
      compounds_array_vlen1.h5
      compounds_array_vlen2.h5
      non_comparables1.h5
      non_comparables2.h5
      tudfilter.h5
      tudfilter2.h5
      h5diff_strings1.h5
      h5diff_strings2.h5
      h5diff_eps1.h5
      h5diff_eps2.h5
      # onion VFD files
      h5diff_onion_objs.h5
      h5diff_onion_objs.h5.onion
      h5diff_onion_dset_ext.h5
      h5diff_onion_dset_ext.h5.onion
      h5diff_onion_dset_1d.h5
      h5diff_onion_dset_1d.h5.onion
  )
  set (LIST_HDF5_TEST_FILES
      # tools/testfiles
      tvlstr.h5
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
  )

  set (LIST_OTHER_TEST_FILES
      h5diff_10.txt
      h5diff_100.txt
      h5diff_11.txt
      h5diff_12.txt
      h5diff_13.txt
      h5diff_14.txt
      h5diff_15.txt
      h5diff_16_1.txt
      h5diff_16_2.txt
      h5diff_16_3.txt
      h5diff_17.txt
      h5diff_171.txt
      h5diff_172.txt
      h5diff_18_1.txt
      h5diff_18.txt
      h5diff_20.txt
      h5diff_200.txt
      h5diff_201.txt
      h5diff_202.txt
      h5diff_203.txt
      h5diff_204.txt
      h5diff_205.txt
      h5diff_206.txt
      h5diff_207.txt
      h5diff_208.txt
      h5diff_220.txt
      h5diff_221.txt
      h5diff_222.txt
      h5diff_223.txt
      h5diff_224.txt
      h5diff_21.txt
      h5diff_22.txt
      h5diff_23.txt
      h5diff_24.txt
      h5diff_25.txt
      h5diff_26.txt
      h5diff_27.txt
      h5diff_28.txt
      h5diff_30.txt
      h5diff_300.txt
      h5diff_400.txt
      h5diff_401.txt
      h5diff_402.txt
      h5diff_403.txt
      h5diff_404.txt
      h5diff_405.txt
      h5diff_406.txt
      h5diff_407.txt
      h5diff_408.txt
      h5diff_409.txt
      h5diff_410.txt
      h5diff_411.txt
      h5diff_412.txt
      h5diff_413.txt
      h5diff_414.txt
      h5diff_415.txt
      h5diff_416.txt
      h5diff_417.txt
      h5diff_418.txt
      h5diff_419.txt
      h5diff_420.txt
      h5diff_421.txt
      h5diff_422.txt
      h5diff_423.txt
      h5diff_424.txt
      h5diff_425.txt
      h5diff_450.txt
      h5diff_451.txt
      h5diff_452.txt
      h5diff_453.txt
      h5diff_454.txt
      dangling_link.err
      h5diff_455.txt
      h5diff_456.txt
      h5diff_457.txt
      h5diff_458.txt
      h5diff_459.txt
      h5diff_465.txt
      h5diff_466.txt
      h5diff_467.txt
      h5diff_468.txt
      h5diff_469.txt
      h5diff_471.txt
      h5diff_472.txt
      h5diff_473.txt
      h5diff_474.txt
      h5diff_475.txt
      h5diff_480.txt
      h5diff_481.txt
      h5diff_482.txt
      h5diff_483.txt
      h5diff_484.txt
      h5diff_485.txt
      h5diff_486.txt
      h5diff_487.txt
      h5diff_50.txt
      h5diff_51.txt
      h5diff_52.txt
      h5diff_53.txt
      h5diff_54.txt
      h5diff_55.txt
      h5diff_56.txt
      h5diff_57.txt
      h5diff_58.txt
      h5diff_58_ref.txt
      h5diff_59.txt
      h5diff_500.txt
      h5diff_501.txt
      h5diff_502.txt
      h5diff_503.txt
      h5diff_504.txt
      h5diff_505.txt
      h5diff_506.txt
      h5diff_507.txt
      h5diff_508.txt
      h5diff_509.txt
      h5diff_510.txt
      h5diff_511.txt
      h5diff_512.txt
      h5diff_513.txt
      h5diff_514.txt
      h5diff_515.txt
      h5diff_516.txt
      h5diff_517.txt
      h5diff_518.txt
      h5diff_530.txt
      h5diff_540.txt
      h5diff_60.txt
      h5diff_61.txt
      h5diff_62.txt
      h5diff_63.txt
      h5diff_600.txt
      h5diff_601.txt
      h5diff_601_ERR.err
      h5diff_603.txt
      h5diff_604.txt
      h5diff_605.txt
      h5diff_606.txt
      h5diff_607.txt
      h5diff_608.txt
      h5diff_609.txt
      h5diff_610.txt
      h5diff_612.txt
      h5diff_613.txt
      h5diff_614.txt
      h5diff_615.txt
      h5diff_616.txt
      h5diff_617.txt
      h5diff_618.txt
      h5diff_619.txt
      h5diff_621.txt
      h5diff_622.txt
      h5diff_623.txt
      h5diff_624.txt
      h5diff_625.txt
      h5diff_626.txt
      h5diff_627.txt
      h5diff_628.txt
      h5diff_629.txt
      h5diff_630.txt
      h5diff_631.txt
      h5diff_640.txt
      h5diff_641.txt
      h5diff_642.txt
      h5diff_643.txt
      h5diff_644.txt
      h5diff_645.txt
      h5diff_646.txt
      h5diff_70.txt
      h5diff_700.txt
      h5diff_701.txt
      h5diff_702.txt
      h5diff_703.txt
      h5diff_704.txt
      h5diff_705.txt
      h5diff_706.txt
      h5diff_707.txt
      h5diff_708.txt
      h5diff_709.txt
      h5diff_710.txt
      h5diff_80.txt
      h5diff_800.txt
      h5diff_801.txt
      h5diff_830.txt
      h5diff_90.txt
      h5diff_900.txt
      h5diff_901.txt
      h5diff_902.txt
      h5diff_8625.txt
      h5diff_8639.txt
      h5diff_reg.txt
      h5diff_ud.txt
      h5diff_udfail.err
      h5diff_udfail.txt
      h5diff_v1.txt
      h5diff_v2.txt
      h5diff_v3.txt
      h5diff_vlstr.txt
      h5diff_eps.txt
  )

  set (LIST_WIN_TEST_FILES
      h5diff_101
      h5diff_102
      h5diff_103
      h5diff_104
  )

  # Make testfiles dir under build dir
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
  if (H5_HAVE_PARALLEL)
    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/PAR/testfiles")
  endif ()

  #
  # copy test files from source to build dir
  #
  foreach (h5_tstfiles ${LIST_DIFF_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_tstfiles}" "${PROJECT_BINARY_DIR}/testfiles/${h5_tstfiles}" "h5diff_files")
    if (H5_HAVE_PARALLEL)
      HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_tstfiles}" "${PROJECT_BINARY_DIR}/PAR/testfiles/${h5_tstfiles}" "h5diff_files")
    endif ()
  endforeach ()
  foreach (h5_tstfiles ${LIST_VDS_TEST_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/h5dump/testfiles/vds/${h5_tstfiles}" "${PROJECT_BINARY_DIR}/testfiles/${h5_tstfiles}" "h5diff_files")
    if (H5_HAVE_PARALLEL)
      HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/h5dump/testfiles/vds/${h5_tstfiles}" "${PROJECT_BINARY_DIR}/PAR/testfiles/${h5_tstfiles}" "h5diff_files")
    endif ()
  endforeach ()
  foreach (h5_tstfiles ${LIST_HDF5_TEST_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/h5dump/testfiles/${h5_tstfiles}" "${PROJECT_BINARY_DIR}/testfiles/${h5_tstfiles}" "h5diff_files")
    if (H5_HAVE_PARALLEL)
      HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/h5dump/testfiles/${h5_tstfiles}" "${PROJECT_BINARY_DIR}/PAR/testfiles/${h5_tstfiles}" "h5diff_files")
    endif ()
  endforeach ()
  foreach (h5_tstfiles ${LIST_OTHER_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/${h5_tstfiles}" "${PROJECT_BINARY_DIR}/testfiles/${h5_tstfiles}" "h5diff_files")
    if (H5_HAVE_PARALLEL)
      HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/${h5_tstfiles}" "${PROJECT_BINARY_DIR}/PAR/testfiles/${h5_tstfiles}" "h5diff_files")
    endif ()
  endforeach ()
  # copy second version of tvlstr.h5
  HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/h5dump/testfiles/tvlstr.h5" "${PROJECT_BINARY_DIR}/testfiles/tvlstr2.h5" "h5diff_files")
  if (H5_HAVE_PARALLEL)
      HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/h5dump/testfiles/tvlstr.h5" "${PROJECT_BINARY_DIR}/PAR/testfiles/tvlstr2.h5" "h5diff_files")
  endif ()


  #
  # Overwrite system dependent files (Windows) and not VS2015
  #
  set (COPY_WINDOWS_FILES false)
  # MinGW tests may depend on host system
  #if (MINGW)
  #  set (COPY_WINDOWS_FILES true)
  #endif ()
  if (WIN32 AND MSVC_VERSION LESS 1900)
    set (COPY_WINDOWS_FILES true)
  endif ()
  if (COPY_WINDOWS_FILES)
    foreach (h5_tstfiles ${LIST_WIN_TEST_FILES})
      HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/${h5_tstfiles}w.txt" "${PROJECT_BINARY_DIR}/testfiles/${h5_tstfiles}.txt" "h5diff_files")
      if (H5_HAVE_PARALLEL)
        HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/${h5_tstfiles}w.txt" "${PROJECT_BINARY_DIR}/PAR/testfiles/${h5_tstfiles}.txt" "h5diff_files")
      endif ()
    endforeach ()
  else ()
    foreach (h5_tstfiles ${LIST_WIN_TEST_FILES})
      HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/${h5_tstfiles}.txt" "${PROJECT_BINARY_DIR}/testfiles/${h5_tstfiles}.txt" "h5diff_files")
      if (H5_HAVE_PARALLEL)
        HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/${h5_tstfiles}.txt" "${PROJECT_BINARY_DIR}/PAR/testfiles/${h5_tstfiles}.txt" "h5diff_files")
      endif ()
    endforeach ()
  endif ()
  add_custom_target(h5diff_files ALL COMMENT "Copying files needed by h5diff tests" DEPENDS ${h5diff_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  macro (ADD_H5_TEST resultfile resultcode)
    if (HDF5_TEST_SERIAL)
      ADD_SH5_TEST (${resultfile} ${resultcode} ${ARGN})
    endif ()
    if (H5_HAVE_PARALLEL AND HDF5_TEST_PARALLEL)
      ADD_PH5_TEST (${resultfile} ${resultcode} ${ARGN})
    endif ()
  endmacro ()

  macro (ADD_SH5_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME H5DIFF-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> ${ARGN})
      if (${resultcode})
        set_tests_properties (H5DIFF-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
    else ()
      add_test (
          NAME H5DIFF-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5diff>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.txt"
              -D "TEST_APPEND=EXIT CODE:"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (H5DIFF-${resultfile} PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
    )
    if ("H5DIFF-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5DIFF-${resultfile} PROPERTIES DISABLED true)
    endif ()
  endmacro ()

  macro (ADD_PH5_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME MPI_TEST_H5DIFF-${resultfile} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:ph5diff> ${MPIEXEC_POSTFLAGS} ${ARGN})
      set_tests_properties (MPI_TEST_H5DIFF-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/PAR/testfiles")
      if (${resultcode})
        set_tests_properties (MPI_TEST_H5DIFF-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
    else ()
      add_test (
          NAME MPI_TEST_H5DIFF-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=${MPIEXEC_EXECUTABLE}"
              -D "TEST_ARGS:STRING=${MPIEXEC_NUMPROC_FLAG};${MPIEXEC_MAX_NUMPROCS};${MPIEXEC_PREFLAGS};$<TARGET_FILE:ph5diff>;${MPIEXEC_POSTFLAGS};${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/PAR/testfiles"
              -D "TEST_OUTPUT=${resultfile}.out"
              #-D "TEST_EXPECT=${resultcode}"
              -D "TEST_EXPECT=0" # ph5diff currently always exits with a zero status code due to
                                 # output from some MPI implementations from a non-zero exit code
              -D "TEST_REFERENCE=${resultfile}.txt"
              -D "TEST_APPEND=EXIT CODE:"
              -D "TEST_REF_APPEND=EXIT CODE: [0-9]"
              -D "TEST_REF_FILTER=EXIT CODE: 0"
              -D "TEST_SORT_COMPARE=TRUE"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (MPI_TEST_H5DIFF-${resultfile} PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/PAR/testfiles"
    )
    if ("MPI_TEST_H5DIFF-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (MPI_TEST_H5DIFF-${resultfile} PROPERTIES DISABLED true)
    endif ()
  endmacro ()

  macro (ADD_H5_UD_TEST testname resultcode resultfile)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      if ("${resultcode}" STREQUAL "2")
        add_test (
            NAME H5DIFF_UD-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5diff>"
                -D "TEST_ARGS:STRING=${ARGN}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
                -D "TEST_OUTPUT=${resultfile}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_REFERENCE=${resultfile}.txt"
                -D "TEST_APPEND=EXIT CODE:"
                -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
                -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
      else ()
        add_test (
            NAME H5DIFF_UD-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5diff>"
                -D "TEST_ARGS:STRING=${ARGN}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
                -D "TEST_OUTPUT=${resultfile}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_REFERENCE=${resultfile}.txt"
                -D "TEST_APPEND=EXIT CODE:"
                -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
                -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
      endif ()
      if ("H5DIFF_UD-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DIFF_UD-${testname} PROPERTIES DISABLED true)
      endif ()
    endif ()
  endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  # --------------------------------------------------------------------
  # test file names
  # --------------------------------------------------------------------
  set (FILE1 h5diff_basic1.h5)
  set (FILE2 h5diff_basic2.h5)
  set (FILE3 h5diff_types.h5)
  set (FILE4 h5diff_dtypes.h5)
  set (FILE5 h5diff_attr1.h5)
  set (FILE6 h5diff_attr2.h5)
  set (FILE7 h5diff_dset1.h5)
  set (FILE8 h5diff_dset2.h5)
  set (FILE8A h5diff_dset3.h5)
  set (FILE9 h5diff_hyper1.h5)
  set (FILE10 h5diff_hyper2.h5)
  set (FILE11 h5diff_empty.h5)
  set (FILE12 h5diff_links.h5)
  set (FILE13 h5diff_softlinks.h5)
  set (FILE14 h5diff_linked_softlink.h5)
  set (FILE15 h5diff_extlink_src.h5)
  set (FILE16 h5diff_extlink_trg.h5)
  set (FILE17 h5diff_ext2softlink_src.h5)
  set (FILE18 h5diff_ext2softlink_trg.h5)
  set (FILE19 h5diff_dset_zero_dim_size1.h5)
  set (FILE20 h5diff_dset_zero_dim_size2.h5)
  set (DANGLE_LINK_FILE1 h5diff_danglelinks1.h5)
  set (DANGLE_LINK_FILE2 h5diff_danglelinks2.h5)
  set (GRP_RECURSE_FILE1 h5diff_grp_recurse1.h5)
  set (GRP_RECURSE_FILE2 h5diff_grp_recurse2.h5)
  # group recursive - same structure via external links through files
  set (GRP_RECURSE1_EXT h5diff_grp_recurse_ext1.h5)
  set (GRP_RECURSE2_EXT1 h5diff_grp_recurse_ext2-1.h5)
  set (GRP_RECURSE2_EXT2 h5diff_grp_recurse_ext2-2.h5)
  set (GRP_RECURSE2_EXT3 h5diff_grp_recurse_ext2-3.h5)
  # same structure, same obj name with different value
  set (EXCLUDE_FILE1_1 h5diff_exclude1-1.h5)
  set (EXCLUDE_FILE1_2 h5diff_exclude1-2.h5)
  # different structure and obj names
  set (EXCLUDE_FILE2_1 h5diff_exclude2-1.h5)
  set (EXCLUDE_FILE2_2 h5diff_exclude2-2.h5)
  # Only one file contains unique objs. Common objs are same.
  set (EXCLUDE_FILE3_1 h5diff_exclude3-1.h5)
  set (EXCLUDE_FILE3_2 h5diff_exclude3-2.h5)
  # compound type with multiple vlen string types
  set (COMP_VL_STRS_FILE h5diff_comp_vl_strs.h5)
  # container types (array,vlen) with multiple nested compound types
  set (COMPS_ARRAY_VLEN_FILE1 compounds_array_vlen1.h5)
  set (COMPS_ARRAY_VLEN_FILE2 compounds_array_vlen2.h5)
  # attrs with verbose option level
  set (ATTR_VERBOSE_LEVEL_FILE1 h5diff_attr_v_level1.h5)
  set (ATTR_VERBOSE_LEVEL_FILE2 h5diff_attr_v_level2.h5)
  # strings
  set (STRINGS1 h5diff_strings1.h5)
  set (STRINGS2 h5diff_strings2.h5)
  # epsilon
  set (EPS1 h5diff_eps1.h5)
  set (EPS2 h5diff_eps2.h5)

# VDS tests
  set (FILEV1 1_vds.h5)
  set (FILEV2 2_vds.h5)
  set (FILEV3_1 3_1_vds.h5)
  set (FILEV3_2 3_2_vds.h5)
  set (FILEV4 4_vds.h5)
  set (FILEV5 5_vds.h5)

# ############################################################################
# # Common usage
# ############################################################################

# 1.0
ADD_H5_TEST (h5diff_10 0 -h)

# 1.1 normal mode
ADD_H5_TEST (h5diff_11 1  ${FILE1} ${FILE2})

# 1.2 normal mode with objects
ADD_H5_TEST (h5diff_12 1  ${FILE1} ${FILE2}  g1/dset1 g1/dset2)

# 1.3 report mode
ADD_H5_TEST (h5diff_13 1 -r ${FILE1} ${FILE2})

# 1.4 report  mode with objects
ADD_H5_TEST (h5diff_14 1  -r ${FILE1} ${FILE2} g1/dset1 g1/dset2)

# 1.5 with -d
ADD_H5_TEST (h5diff_15 1 --report --delta=5 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 1.6.1 with -p (int)
ADD_H5_TEST (h5diff_16_1 1 -v -p 0.02 ${FILE1} ${FILE1} g1/dset5 g1/dset6)

# 1.6.2 with -p (unsigned long_long)
ADD_H5_TEST (h5diff_16_2 1 --verbose --relative=0.02 ${FILE1} ${FILE1} g1/dset7 g1/dset8)

# 1.6.3 with -p (double)
ADD_H5_TEST (h5diff_16_3 1 -v -p 0.02 ${FILE1} ${FILE1} g1/dset9 g1/dset10)

# 1.7 verbose mode
ADD_H5_TEST (h5diff_17 1 -v ${FILE1} ${FILE2})

# 1.7 test 32-bit INFINITY
ADD_H5_TEST (h5diff_171 0 -v ${FILE1} ${FILE1} /g1/fp19 /g1/fp19_COPY)

# 1.7 test 64-bit INFINITY
ADD_H5_TEST (h5diff_172 0 -v ${FILE1} ${FILE1} /g1/fp20 /g1/fp20_COPY)

# 1.8 quiet mode
ADD_H5_TEST (h5diff_18 1 -q ${FILE1} ${FILE2})

# 1.8 -v and -q
ADD_H5_TEST (h5diff_18_1 2 -v -q ${FILE1} ${FILE2})

# ##############################################################################
# # not comparable types
# ##############################################################################

# 2.0
ADD_H5_TEST (h5diff_20 0 -v ${FILE3} ${FILE3}  dset g1)

# 2.1
ADD_H5_TEST (h5diff_21 0 -v ${FILE3} ${FILE3} dset l1)

# 2.2
ADD_H5_TEST (h5diff_22 0 -v  ${FILE3} ${FILE3} dset t1)

# ##############################################################################
# # compare groups, types, links (no differences and differences)
# ##############################################################################

# 2.3
ADD_H5_TEST (h5diff_23 0 -v ${FILE3} ${FILE3} g1 g1)

# 2.4
ADD_H5_TEST (h5diff_24 0 -v ${FILE3} ${FILE3} t1 t1)

# 2.5
ADD_H5_TEST (h5diff_25 0 -v ${FILE3} ${FILE3} l1 l1)

# 2.6
ADD_H5_TEST (h5diff_26 0 -v ${FILE3} ${FILE3} g1 g2)

# 2.7
ADD_H5_TEST (h5diff_27 1 -v ${FILE3} ${FILE3} t1 t2)

# 2.8
ADD_H5_TEST (h5diff_28 1 -v ${FILE3} ${FILE3} l1 l2)

# ##############################################################################
# # Enum value tests (may become more comprehensive in the future)
# ##############################################################################

# 3.0
# test enum types which may have invalid values
ADD_H5_TEST (h5diff_30 1 -v h5diff_enum_invalid_values.h5 h5diff_enum_invalid_values.h5 dset1 dset2)


# ##############################################################################
# # Dataset datatypes
# ##############################################################################

# 5.0
ADD_H5_TEST (h5diff_50 1 -v ${FILE4} ${FILE4} dset0a dset0b)

# 5.1
ADD_H5_TEST (h5diff_51 1 -v ${FILE4} ${FILE4} dset1a dset1b)

# 5.2
ADD_H5_TEST (h5diff_52 1 -v ${FILE4} ${FILE4} dset2a dset2b)

# 5.3
ADD_H5_TEST (h5diff_53 1 -v ${FILE4} ${FILE4} dset3a dset4b)

# 5.4
ADD_H5_TEST (h5diff_54 1 -v ${FILE4} ${FILE4} dset4a dset4b)

# 5.5
ADD_H5_TEST (h5diff_55 1 -v ${FILE4} ${FILE4} dset5a dset5b)

# 5.6
ADD_H5_TEST (h5diff_56 1 -v ${FILE4} ${FILE4} dset6a dset6b)

# 5.7
ADD_H5_TEST (h5diff_57 0 -v ${FILE4} ${FILE4} dset7a dset7b)

# 5.8 (region reference)
ADD_H5_TEST (h5diff_58 1 -v2 ${FILE7} ${FILE8} refreg)
ADD_H5_TEST (h5diff_58_ref 1 -v2 ${FILE7} ${FILE8} /g1/reference2D)
# STD_REF_OBJ
ADD_H5_TEST (h5diff_reg 0 -v2 trefer_attr.h5 trefer_ext2.h5 Dataset3 Dataset3)

# test for both dset and attr with same type but with different size
# ( HDDFV-7942 )
ADD_H5_TEST (h5diff_59 0 -v ${FILE4} ${FILE4} dset11a dset11b)

# Strings
# ( HDFFV-10128 )
ADD_H5_TEST (h5diff_60 1 -v ${STRINGS1} ${STRINGS2} string1 string1)
ADD_H5_TEST (h5diff_61 1 -v ${STRINGS1} ${STRINGS2} string2 string2)
ADD_H5_TEST (h5diff_62 1 -v ${STRINGS1} ${STRINGS2} string3 string3)
ADD_H5_TEST (h5diff_63 1 -v ${STRINGS1} ${STRINGS2} string4 string4)

# ##############################################################################
# # Error messages
# ##############################################################################

# 6.0: Check if the command line number of arguments is less than 3
ADD_H5_TEST (h5diff_600 1 ${FILE1})

# 6.1: Check if non-exist object name is specified
ADD_H5_TEST (h5diff_601 2 ${FILE1} ${FILE1} nono_obj)

# ##############################################################################
# # -d
# ##############################################################################

# 6.3: negative value
ADD_H5_TEST (h5diff_603 1 -d -4 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.4: zero
ADD_H5_TEST (h5diff_604 1 -d 0 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.5: non number
ADD_H5_TEST (h5diff_605 1 -d u ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.6: hexadecimal
ADD_H5_TEST (h5diff_606 1 -d 0x1 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.7: string
ADD_H5_TEST (h5diff_607 1 -d "1" ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.8: use system epsilon
ADD_H5_TEST (h5diff_608 1 --use-system-epsilon ${FILE1} ${FILE2}  g1/dset3 g1/dset4)

# 6.9: number larger than biggest difference
ADD_H5_TEST (h5diff_609 0 -d 200 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.10: number smaller than smallest difference
ADD_H5_TEST (h5diff_610 1 -d 1 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# eps: number smaller than epsilon
ADD_H5_TEST (h5diff_eps 0 -v3 -d 1e-16 ${EPS1} ${EPS2})

# ##############################################################################
# # -p
# ##############################################################################

# 6.12: negative value
ADD_H5_TEST (h5diff_612 1 -p -4 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.13: zero
ADD_H5_TEST (h5diff_613 1 -p 0 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.14: non number
ADD_H5_TEST (h5diff_614 1 -p u ${FILE1} ${FILE2}  g1/dset3 g1/dset4)

# 6.15: hexadecimal
ADD_H5_TEST (h5diff_615 1 -p 0x1 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.16: string
ADD_H5_TEST (h5diff_616 1 -p "0.21" ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.17: repeated option
ADD_H5_TEST (h5diff_617 1 -p 0.21 -p 0.22 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.18: number larger than biggest difference
ADD_H5_TEST (h5diff_618 0 -p 2 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.19: number smaller than smallest difference
ADD_H5_TEST (h5diff_619 1 -p 0.005 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# ##############################################################################
# # -n
# ##############################################################################

# 6.21: negative value
ADD_H5_TEST (h5diff_621 1 -n -4 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.22: zero
ADD_H5_TEST (h5diff_622 1 -n 0 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.23: non number
ADD_H5_TEST (h5diff_623 1 -n u ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.24: hexadecimal
ADD_H5_TEST (h5diff_624 1 -n 0x1 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.25: string
ADD_H5_TEST (h5diff_625 1 -n "2" ${FILE1} ${FILE2}  g1/dset3 g1/dset4)

# 6.26: repeated option
ADD_H5_TEST (h5diff_626 1 -n 2 -n 3 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.27: number larger than biggest difference
ADD_H5_TEST (h5diff_627 1 --count=200 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.28: number smaller than smallest difference
ADD_H5_TEST (h5diff_628 1 -n 1 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# Disabling this test as it hangs - LRK 20090618
# 6.29  non valid files
#ADD_H5_TEST (h5diff_629 2 file1.h6 file2.h6)

# ##############################################################################
# # NaN
# ##############################################################################
# 6.30: test (NaN == NaN) must be true based on our documentation -- XCAO
ADD_H5_TEST (h5diff_630 0 -v -d "0.0001" ${FILE1} ${FILE1} g1/fp18 g1/fp18_COPY)
ADD_H5_TEST (h5diff_631 0 -v --use-system-epsilon ${FILE1} ${FILE1} g1/fp18 g1/fp18_COPY)

# ##############################################################################
# 7.  attributes
# ##############################################################################
ADD_H5_TEST (h5diff_70 1 -v ${FILE5} ${FILE6})

# ##################################################
#  attrs with verbose option level
# ##################################################
ADD_H5_TEST (h5diff_700 1 -v1 ${FILE5} ${FILE6})
ADD_H5_TEST (h5diff_701 1 -v2 ${FILE5} ${FILE6})
ADD_H5_TEST (h5diff_702 1 --verbose=1 ${FILE5} ${FILE6})
ADD_H5_TEST (h5diff_703 1 --verbose=2 ${FILE5} ${FILE6})

# same attr number , all same attr name
ADD_H5_TEST (h5diff_704 1 -v2 ${ATTR_VERBOSE_LEVEL_FILE1} ${ATTR_VERBOSE_LEVEL_FILE2} /g)

# same attr number , some same attr name
ADD_H5_TEST (h5diff_705 1 -v2 ${ATTR_VERBOSE_LEVEL_FILE1} ${ATTR_VERBOSE_LEVEL_FILE2} /dset)

# same attr number , all different attr name
ADD_H5_TEST (h5diff_706 1 -v2 ${ATTR_VERBOSE_LEVEL_FILE1} ${ATTR_VERBOSE_LEVEL_FILE2} /ntype)

# different attr number , same attr name (intersected)
ADD_H5_TEST (h5diff_707 1 -v2 ${ATTR_VERBOSE_LEVEL_FILE1} ${ATTR_VERBOSE_LEVEL_FILE2} /g2)

# different attr number , all different attr name
ADD_H5_TEST (h5diff_708 1 -v2 ${ATTR_VERBOSE_LEVEL_FILE1} ${ATTR_VERBOSE_LEVEL_FILE2} /g3)

# when no attributes exist in both objects
ADD_H5_TEST (h5diff_709 0 -v2 ${ATTR_VERBOSE_LEVEL_FILE1} ${ATTR_VERBOSE_LEVEL_FILE2} /g4)

# file vs file
ADD_H5_TEST (h5diff_710 1 -v2 ${ATTR_VERBOSE_LEVEL_FILE1} ${ATTR_VERBOSE_LEVEL_FILE2})

# ##############################################################################
# 8.  all dataset datatypes
# ##############################################################################
ADD_H5_TEST (h5diff_80 1 -v ${FILE7} ${FILE8})

# 9. compare a file with itself
ADD_H5_TEST (h5diff_90 0 -v ${FILE2} ${FILE2})

# 10. read by hyperslab, print indexes
ADD_H5_TEST (h5diff_100 1 -v ${FILE9} ${FILE10})

# 11. floating point comparison
# double value
ADD_H5_TEST (h5diff_101 1 -v ${FILE1} ${FILE1} g1/d1  g1/d2)

# float value
ADD_H5_TEST (h5diff_102 1 -v ${FILE1} ${FILE1} g1/fp1 g1/fp2)

# with --use-system-epsilon for double value. expect less differences
ADD_H5_TEST (h5diff_103 1 -v --use-system-epsilon ${FILE1} ${FILE1} g1/d1
g1/d2)

# with --use-system-epsilon for float value. expect less differences
ADD_H5_TEST (h5diff_104 1 -v --use-system-epsilon ${FILE1} ${FILE1} g1/fp1 g1/fp2)

# not comparable -c flag
ADD_H5_TEST (h5diff_200 0 ${FILE2} ${FILE2} g2/dset1  g2/dset2)

ADD_H5_TEST (h5diff_201 0 -c ${FILE2} ${FILE2} g2/dset1  g2/dset2)

ADD_H5_TEST (h5diff_202 0 -c ${FILE2} ${FILE2} g2/dset2  g2/dset3)

ADD_H5_TEST (h5diff_203 0 -c ${FILE2} ${FILE2} g2/dset3  g2/dset4)

ADD_H5_TEST (h5diff_204 0 -c ${FILE2} ${FILE2} g2/dset4  g2/dset5)

ADD_H5_TEST (h5diff_205 0 -c ${FILE2} ${FILE2} g2/dset5  g2/dset6)

# not comparable in compound
ADD_H5_TEST (h5diff_206 0 -c ${FILE2} ${FILE2} g2/dset7  g2/dset8)

ADD_H5_TEST (h5diff_207 0 -c ${FILE2} ${FILE2} g2/dset8  g2/dset9)

# not comparable in dataspace of zero dimension size
ADD_H5_TEST (h5diff_208 0 -c ${FILE19} ${FILE20})

# non-comparable dataset with comparable attribute, and other comparable datasets.
# All the rest comparables should display differences.
ADD_H5_TEST (h5diff_220 1 -c non_comparables1.h5 non_comparables2.h5 /g1)

# comparable dataset with non-comparable attribute and other comparable attributes.
# Also test non-compatible attributes with different type, dimension, rank.
# All the rest comparables should display differences.
ADD_H5_TEST (h5diff_221 1 -c non_comparables1.h5 non_comparables2.h5 /g2)

# entire file
# All the rest comparables should display differences
ADD_H5_TEST (h5diff_222 1 -c non_comparables1.h5 non_comparables2.h5)

# non-comparable test for common objects (same name) with different object types
# (HDFFV-7644)
ADD_H5_TEST (h5diff_223 0 -c non_comparables1.h5 non_comparables2.h5 /diffobjtypes)
# swap files
ADD_H5_TEST (h5diff_224 0 -c non_comparables2.h5 non_comparables1.h5 /diffobjtypes)

# ##############################################################################
# # Links compare without --follow-symlinks nor --no-dangling-links
# ##############################################################################
# test for bug1749
ADD_H5_TEST (h5diff_300 1 -v ${FILE12} ${FILE12} /link_g1 /link_g2)

# ##############################################################################
# # Links compare with --follow-symlinks Only
# ##############################################################################
# soft links file to file
ADD_H5_TEST (h5diff_400 0 --follow-symlinks -v ${FILE13} ${FILE13})

# softlink vs dset"
ADD_H5_TEST (h5diff_401 1 --follow-symlinks -v ${FILE13} ${FILE13} /softlink_dset1_1 /target_dset2)

# dset vs softlink"
ADD_H5_TEST (h5diff_402 1 --follow-symlinks -v ${FILE13} ${FILE13} /target_dset2 /softlink_dset1_1)

# softlink vs softlink"
ADD_H5_TEST (h5diff_403 1 --follow-symlinks -v ${FILE13} ${FILE13} /softlink_dset1_1 /softlink_dset2)

# extlink vs extlink (FILE)"
ADD_H5_TEST (h5diff_404 0 --follow-symlinks -v ${FILE15} ${FILE15})

# extlink vs dset"
ADD_H5_TEST (h5diff_405 1 --follow-symlinks -v ${FILE15} ${FILE16} /ext_link_dset1 /target_group2/x_dset)

# dset vs extlink"
ADD_H5_TEST (h5diff_406 1 --follow-symlinks -v ${FILE16} ${FILE15} /target_group2/x_dset /ext_link_dset1)

# extlink vs extlink"
ADD_H5_TEST (h5diff_407 1 --follow-symlinks -v ${FILE15} ${FILE15} /ext_link_dset1 /ext_link_dset2)

# softlink vs extlink"
ADD_H5_TEST (h5diff_408 1 --follow-symlinks -v ${FILE13} ${FILE15} /softlink_dset1_1 /ext_link_dset2)

# extlink vs softlink "
ADD_H5_TEST (h5diff_409 1 --follow-symlinks -v ${FILE15} ${FILE13} /ext_link_dset2 /softlink_dset1_1)

# linked_softlink vs linked_softlink (FILE)"
ADD_H5_TEST (h5diff_410 0 --follow-symlinks -v ${FILE14} ${FILE14})

# dset2 vs linked_softlink_dset1"
ADD_H5_TEST (h5diff_411 1 --follow-symlinks -v ${FILE14} ${FILE14} /target_dset2 /softlink1_to_slink2)

# linked_softlink_dset1 vs dset2"
ADD_H5_TEST (h5diff_412 1 --follow-symlinks -v ${FILE14} ${FILE14} /softlink1_to_slink2 /target_dset2)

# linked_softlink_to_dset1 vs linked_softlink_to_dset2"
ADD_H5_TEST (h5diff_413 1 --follow-symlinks -v ${FILE14} ${FILE14} /softlink1_to_slink2 /softlink2_to_slink2)

# group vs linked_softlink_group1"
ADD_H5_TEST (h5diff_414 1 --follow-symlinks -v ${FILE14} ${FILE14} /target_group /softlink3_to_slink2)

# linked_softlink_group1 vs group"
ADD_H5_TEST (h5diff_415 1 --follow-symlinks -v ${FILE14} ${FILE14} /softlink3_to_slink2 /target_group)

# linked_softlink_to_group1 vs linked_softlink_to_group2"
ADD_H5_TEST (h5diff_416 0 --follow-symlinks -v ${FILE14} ${FILE14} /softlink3_to_slink2 /softlink4_to_slink2)

# non-exist-softlink vs softlink"
ADD_H5_TEST (h5diff_417 1 --follow-symlinks -v ${FILE13} ${FILE13} /softlink_noexist /softlink_dset2)

# softlink vs non-exist-softlink"
ADD_H5_TEST (h5diff_418 1 --follow-symlinks -v ${FILE13} ${FILE13} /softlink_dset2 /softlink_noexist)

# non-exist-extlink_file vs extlink"
ADD_H5_TEST (h5diff_419 1 --follow-symlinks -v ${FILE15} ${FILE15} /ext_link_noexist2 /ext_link_dset2)

# exlink vs non-exist-extlink_file"
ADD_H5_TEST (h5diff_420 1 --follow-symlinks -v ${FILE15} ${FILE15} /ext_link_dset2 /ext_link_noexist2)

# extlink vs non-exist-extlink_obj"
ADD_H5_TEST (h5diff_421 1 --follow-symlinks -v ${FILE15} ${FILE15} /ext_link_dset2 /ext_link_noexist1)

# non-exist-extlink_obj vs extlink"
ADD_H5_TEST (h5diff_422 1 --follow-symlinks -v ${FILE15} ${FILE15} /ext_link_noexist1 /ext_link_dset2)

# extlink_to_softlink_to_dset1 vs dset2"
ADD_H5_TEST (h5diff_423 1 --follow-symlinks -v ${FILE17} ${FILE18} /ext_link_to_slink1 /dset2)

# dset2 vs extlink_to_softlink_to_dset1"
ADD_H5_TEST (h5diff_424 1 --follow-symlinks -v ${FILE18} ${FILE17} /dset2 /ext_link_to_slink1)

# extlink_to_softlink_to_dset1 vs extlink_to_softlink_to_dset2"
ADD_H5_TEST (h5diff_425 1 --follow-symlinks -v ${FILE17} ${FILE17} /ext_link_to_slink1 /ext_link_to_slink2)

# ##############################################################################
# # Dangling links compare (--follow-symlinks and --no-dangling-links)
# ##############################################################################
# dangling links --follow-symlinks (FILE to FILE)
ADD_H5_TEST (h5diff_450 1  --follow-symlinks -v ${DANGLE_LINK_FILE1} ${DANGLE_LINK_FILE2})

# dangling links --follow-symlinks and --no-dangling-links (FILE to FILE)
ADD_H5_TEST (h5diff_451 2  --follow-symlinks -v --no-dangling-links  ${DANGLE_LINK_FILE1} ${DANGLE_LINK_FILE2})

# try --no-dangling-links without --follow-symlinks options
ADD_H5_TEST (h5diff_452 2  --no-dangling-links  ${FILE13} ${FILE13})

# dangling link found for soft links (FILE to FILE)
ADD_H5_TEST (h5diff_453 2  --follow-symlinks -v --no-dangling-links  ${FILE13} ${FILE13})

# dangling link found for soft links (obj to obj)
ADD_H5_TEST (h5diff_454 2  --follow-symlinks -v --no-dangling-links  ${FILE13} ${FILE13} /softlink_dset2 /softlink_noexist)

# dangling link found for soft links (obj to obj) Both dangle links
ADD_H5_TEST (h5diff_455 2  --follow-symlinks -v --no-dangling-links  ${FILE13} ${FILE13} /softlink_noexist /softlink_noexist)

# dangling link found for ext links (FILE to FILE)
ADD_H5_TEST (h5diff_456 2  --follow-symlinks -v --no-dangling-links  ${FILE15} ${FILE15})

# dangling link found for ext links (obj to obj). target file exist
ADD_H5_TEST (h5diff_457 2  --follow-symlinks -v --no-dangling-links  ${FILE15} ${FILE15} /ext_link_dset1 /ext_link_noexist1)

# dangling link found for ext links (obj to obj). target file NOT exist
ADD_H5_TEST (h5diff_458 2  --follow-symlinks -v --no-dangling-links  ${FILE15} ${FILE15} /ext_link_dset1 /ext_link_noexist2)

# dangling link found for ext links (obj to obj). Both dangle links
ADD_H5_TEST (h5diff_459 2  --follow-symlinks -v --no-dangling-links  ${FILE15} ${FILE15} /ext_link_noexist1 /ext_link_noexist2)

# dangling link --follow-symlinks (obj vs obj)
# (HDFFV-7836)
ADD_H5_TEST (h5diff_465 0 --follow-symlinks h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /soft_link1)
# (HDFFV-7835)
# soft dangling vs. soft dangling
ADD_H5_TEST (h5diff_466 0 -v --follow-symlinks h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /soft_link1)
# soft link  vs. soft dangling
ADD_H5_TEST (h5diff_467 1 -v --follow-symlinks h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /soft_link2)
# ext dangling vs. ext dangling
ADD_H5_TEST (h5diff_468 0 -v --follow-symlinks h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /ext_link4)
# ext link vs. ext dangling
ADD_H5_TEST (h5diff_469 1 -v --follow-symlinks h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /ext_link2)

#---------------------------------------------------
# dangling links without follow symlink
# (HDFFV-7998)
# test - soft dangle links (same and different paths),
#      - external dangle links (same and different paths)
ADD_H5_TEST (h5diff_471 1 -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5)
ADD_H5_TEST (h5diff_472 0 -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /soft_link1)
ADD_H5_TEST (h5diff_473 1 -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /soft_link4)
ADD_H5_TEST (h5diff_474 0 -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /ext_link4)
ADD_H5_TEST (h5diff_475 1 -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /ext_link1)


# ##############################################################################
# # test for group diff recursively
# ##############################################################################
# root
ADD_H5_TEST (h5diff_500 1 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} / /)
ADD_H5_TEST (h5diff_501 1 -v --follow-symlinks ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} / /)

# root vs group
ADD_H5_TEST (h5diff_502 1 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} / /grp1/grp2/grp3)

# group vs group (same name and structure)
ADD_H5_TEST (h5diff_503 0 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp1 /grp1)

# group vs group (different name and structure)
ADD_H5_TEST (h5diff_504 1 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp1/grp2 /grp1/grp2/grp3)

# groups vs soft-link
ADD_H5_TEST (h5diff_505 0 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp1 /slink_grp1)
ADD_H5_TEST (h5diff_506 0 -v --follow-symlinks ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp1/grp2 /slink_grp2)

# groups vs ext-link
ADD_H5_TEST (h5diff_507 0 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp1 /elink_grp1)
ADD_H5_TEST (h5diff_508 0 -v --follow-symlinks ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp1 /elink_grp1)

# soft-link vs ext-link
ADD_H5_TEST (h5diff_509 0 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /slink_grp1 /elink_grp1)
ADD_H5_TEST (h5diff_510 0 -v --follow-symlinks ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /slink_grp1 /elink_grp1)

# circled ext links
ADD_H5_TEST (h5diff_511 1 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp10 /grp11)
ADD_H5_TEST (h5diff_512 1 -v --follow-symlinks ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp10 /grp11)

# circled soft2ext-link vs soft2ext-link
ADD_H5_TEST (h5diff_513 1 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /slink_grp10 /slink_grp11)
ADD_H5_TEST (h5diff_514 1 -v --follow-symlinks ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /slink_grp10 /slink_grp11)

###############################################################################
# Test for group recursive diff via multi-linked external links
# With follow-symlinks, file $GRP_RECURSE1_EXT and $GRP_RECURSE2_EXT1 should
# be same with the external links.
###############################################################################
# file vs file
ADD_H5_TEST (h5diff_515 1 -v ${GRP_RECURSE1_EXT} ${GRP_RECURSE2_EXT1})
ADD_H5_TEST (h5diff_516 0 -v --follow-symlinks ${GRP_RECURSE1_EXT} ${GRP_RECURSE2_EXT1})
# group vs group
ADD_H5_TEST (h5diff_517 1 -v ${GRP_RECURSE1_EXT} ${GRP_RECURSE2_EXT1} /g1)
ADD_H5_TEST (h5diff_518 0 -v --follow-symlinks ${GRP_RECURSE1_EXT} ${GRP_RECURSE2_EXT1} /g1)

# ##############################################################################
# # Exclude objects (--exclude-path)
# ##############################################################################
#
# Same structure, same names and different value.
#
# Exclude the object with different value. Expect return - same
ADD_H5_TEST (h5diff_480 0 -v --exclude-path /group1/dset3 ${EXCLUDE_FILE1_1} ${EXCLUDE_FILE1_2})
# Verify different by not excluding. Expect return - diff
ADD_H5_TEST (h5diff_481 1 -v ${EXCLUDE_FILE1_1} ${EXCLUDE_FILE1_2})

#
# Different structure, different names.
#
# Exclude all the different objects. Expect return - same
ADD_H5_TEST (h5diff_482 0 -v --exclude-path "/group1" --exclude-path "/dset1" ${EXCLUDE_FILE2_1} ${EXCLUDE_FILE2_2})
# Exclude only some different objects. Expect return - diff
ADD_H5_TEST (h5diff_483 1 -v --exclude-path "/group1" ${EXCLUDE_FILE2_1} ${EXCLUDE_FILE2_2})

# Exclude from group compare
ADD_H5_TEST (h5diff_484 0 -v --exclude-path "/dset3" ${EXCLUDE_FILE1_1} ${EXCLUDE_FILE1_2} /group1)

#
# Only one file contains unique objs. Common objs are same.
# (HDFFV-7837)
#
ADD_H5_TEST (h5diff_485 0 -v --exclude-path "/group1" ${EXCLUDE_FILE3_1} ${EXCLUDE_FILE3_2})
ADD_H5_TEST (h5diff_486 0 -v --exclude-path "/group1" ${EXCLUDE_FILE3_2} ${EXCLUDE_FILE3_1})
ADD_H5_TEST (h5diff_487 1 -v --exclude-path "/group1/dset" ${EXCLUDE_FILE3_1} ${EXCLUDE_FILE3_2})

# ##############################################################################
# # diff various multiple vlen and fixed strings in a compound type dataset
# ##############################################################################
ADD_H5_TEST (h5diff_530 0 -v ${COMP_VL_STRS_FILE} ${COMP_VL_STRS_FILE} /group /group_copy)
# test to verify HDFFV-8625
ADD_H5_TEST (h5diff_8625 0 -v --enable-error-stack ${COMP_VL_STRS_FILE} ${COMP_VL_STRS_FILE} /group/Compound_dset1 /group_copy/Compound_dset3)
# test to verify HDFFV-8639
ADD_H5_TEST (h5diff_8639 0 -v h5diff_attr3.h5 h5diff_attr2.h5 /g1)
ADD_H5_TEST (h5diff_vlstr 0 -v tvlstr.h5 tvlstr2.h5)

# ##############################################################################
# # Test container types (array,vlen) with multiple nested compound types
# # Complex compound types in dataset and attribute
# ##############################################################################
ADD_H5_TEST (h5diff_540 1 -v ${COMPS_ARRAY_VLEN_FILE1} ${COMPS_ARRAY_VLEN_FILE2})

# ##############################################################################
# # Test mutually exclusive options
# ##############################################################################
#
# Test with -d , -p and --use-system-epsilon.
ADD_H5_TEST (h5diff_640 1 -v -d 5 -p 0.05 --use-system-epsilon ${FILE1} ${FILE2} /g1/dset3 /g1/dset4)
ADD_H5_TEST (h5diff_641 1 -v -d 5 -p 0.05 ${FILE1} ${FILE2} /g1/dset3 /g1/dset4)
ADD_H5_TEST (h5diff_642 1 -v -p 0.05 -d 5 ${FILE1} ${FILE2} /g1/dset3 /g1/dset4)
ADD_H5_TEST (h5diff_643 1 -v -d 5 --use-system-epsilon ${FILE1} ${FILE2} /g1/dset3 /g1/dset4)
ADD_H5_TEST (h5diff_644 1 -v --use-system-epsilon -d 5 ${FILE1} ${FILE2} /g1/dset3 /g1/dset4)
ADD_H5_TEST (h5diff_645 1 -v -p 0.05 --use-system-epsilon ${FILE1} ${FILE2} /g1/dset3 /g1/dset4)
ADD_H5_TEST (h5diff_646 1 -v --use-system-epsilon -p 0.05 ${FILE1} ${FILE2} /g1/dset3 /g1/dset4)

# ##############################################################################
# # Test array variances
# ##############################################################################
ADD_H5_TEST (h5diff_800 1 -v ${FILE7} ${FILE8} /g1/array /g1/array)
ADD_H5_TEST (h5diff_801 1 -v ${FILE7} ${FILE8A} /g1/array /g1/array)

# ##############################################################################
# # dataset subsets
# ##############################################################################
#serial only
ADD_SH5_TEST (h5diff_830 1 --enable-error-stack -v ${FILE7} ${FILE8} /g1/array3D[0,0,0;2,2,1;2,2,2;] /g1/array3D[0,0,0;2,2,1;2,2,2;])

# ##############################################################################
# # VDS tests
# ##############################################################################
ADD_H5_TEST (h5diff_v1 0 -v ${FILEV1} ${FILEV2})
ADD_H5_TEST (h5diff_v2 0 -r ${FILEV1} ${FILEV2})
ADD_H5_TEST (h5diff_v3 0 -c ${FILEV1} ${FILEV2})

# ##############################################################################
# # onion VFD tests (serial only)
# ##############################################################################
ADD_SH5_TEST (h5diff_900 1 -r -v --vfd-name-1 onion --vfd-info-1 0 --vfd-name-2 onion --vfd-info-2 1 h5diff_onion_objs.h5 h5diff_onion_objs.h5)
ADD_SH5_TEST (h5diff_901 0 -r -v --vfd-name-1 onion --vfd-info-1 0 --vfd-name-2 onion --vfd-info-2 1 h5diff_onion_dset_ext.h5 h5diff_onion_dset_ext.h5)
ADD_SH5_TEST (h5diff_902 1 -r -v --vfd-name-1 onion --vfd-info-1 0 --vfd-name-2 onion --vfd-info-2 1 h5diff_onion_dset_1d.h5 h5diff_onion_dset_1d.h5)

##############################################################################
###    P L U G I N  T E S T S
##############################################################################
if (BUILD_SHARED_LIBS AND HDF5_TEST_SERIAL)
  ADD_H5_UD_TEST (h5diff_plugin_test 0 h5diff_ud -v tudfilter.h5 tudfilter2.h5)
  ADD_H5_UD_TEST (h5diff_plugin_fail 2 h5diff_udfail -v tudfilter.h5 tudfilter2.h5)
endif ()

# ##############################################################################
# # END
# ##############################################################################
