
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
###           T E S T I N G  S H E L L  S C R I P T S                      ###
##############################################################################

if (UNIX)

  find_program (SH_PROGRAM sh)
  if (SH_PROGRAM)

    ##############################################################################
    #  configure scripts to test dir
    ##############################################################################
    find_package (Perl)
    if (PERL_FOUND)
      configure_file(${HDF5_TEST_SOURCE_DIR}/testflushrefresh.sh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/testflushrefresh.sh @ONLY)
    endif ()
    configure_file(${HDF5_TEST_SOURCE_DIR}/test_usecases.sh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/test_usecases.sh @ONLY)
    configure_file(${HDF5_TEST_SOURCE_DIR}/testswmr.sh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/testswmr.sh @ONLY)
    configure_file(${HDF5_TEST_SOURCE_DIR}/testvdsswmr.sh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/testvdsswmr.sh @ONLY)

    ##############################################################################
    #  copy test programs to test dir
    ##############################################################################
    add_custom_command (
        TARGET     swmr_check_compat_vfd
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:swmr_check_compat_vfd>" "${HDF5_TEST_BINARY_DIR}/H5TEST/swmr_check_compat_vfd"
    )

    add_custom_command (
        TARGET     swmr_check_compat_vfd
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "${HDF5_SOURCE_DIR}/bin/output_filter.sh" "${HDF5_TEST_BINARY_DIR}/H5TEST/bin/output_filter.sh"
    )

    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST/flushrefresh_test")
    add_custom_command (
        TARGET     flushrefresh
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:flushrefresh>" "${HDF5_TEST_BINARY_DIR}/H5TEST/flushrefresh"
    )

    #shell script creates dir
    #file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST/usecases_test")
    add_custom_command (
        TARGET     use_append_mchunks
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:use_append_mchunks>" "${HDF5_TEST_BINARY_DIR}/H5TEST/use_append_mchunks"
    )
    add_custom_command (
        TARGET     use_disable_mdc_flushes
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:use_disable_mdc_flushes>" "${HDF5_TEST_BINARY_DIR}/H5TEST/use_disable_mdc_flushes"
    )
    add_custom_command (
        TARGET     twriteorder
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:twriteorder>" "${HDF5_TEST_BINARY_DIR}/H5TEST/twriteorder"
    )
    add_custom_command (
        TARGET     use_append_chunk
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:use_append_chunk>" "${HDF5_TEST_BINARY_DIR}/H5TEST/use_append_chunk"
    )

    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST/swmr_test")
    add_custom_command (
        TARGET     swmr_generator
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:swmr_generator>" "${HDF5_TEST_BINARY_DIR}/H5TEST/swmr_test/swmr_generator"
    )
    add_custom_command (
        TARGET     swmr_start_write
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:swmr_start_write>" "${HDF5_TEST_BINARY_DIR}/H5TEST/swmr_test/swmr_start_write"
    )
    add_custom_command (
        TARGET     swmr_reader
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:swmr_reader>" "${HDF5_TEST_BINARY_DIR}/H5TEST/swmr_test/swmr_reader"
    )
    add_custom_command (
        TARGET     swmr_writer
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:swmr_writer>" "${HDF5_TEST_BINARY_DIR}/H5TEST/swmr_test/swmr_writer"
    )
    add_custom_command (
        TARGET     swmr_remove_reader
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:swmr_remove_reader>" "${HDF5_TEST_BINARY_DIR}/H5TEST/swmr_test/swmr_remove_reader"
    )
    add_custom_command (
        TARGET     swmr_remove_writer
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:swmr_remove_writer>" "${HDF5_TEST_BINARY_DIR}/H5TEST/swmr_test/swmr_remove_writer"
    )
    add_custom_command (
        TARGET     swmr_addrem_writer
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:swmr_addrem_writer>" "${HDF5_TEST_BINARY_DIR}/H5TEST/swmr_test/swmr_addrem_writer"
    )
    add_custom_command (
        TARGET     swmr_sparse_reader
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:swmr_sparse_reader>" "${HDF5_TEST_BINARY_DIR}/H5TEST/swmr_test/swmr_sparse_reader"
    )
    add_custom_command (
        TARGET     swmr_sparse_writer
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:swmr_sparse_writer>" "${HDF5_TEST_BINARY_DIR}/H5TEST/swmr_test/swmr_sparse_writer"
    )

    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST/vds_swmr_test")
    add_custom_command (
        TARGET     vds_swmr_gen
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:vds_swmr_gen>" "${HDF5_TEST_BINARY_DIR}/H5TEST/vds_swmr_gen"
    )
    add_custom_command (
        TARGET     vds_swmr_writer
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:vds_swmr_writer>" "${HDF5_TEST_BINARY_DIR}/H5TEST/vds_swmr_writer"
    )
    add_custom_command (
        TARGET     vds_swmr_reader
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "$<TARGET_FILE:vds_swmr_reader>" "${HDF5_TEST_BINARY_DIR}/H5TEST/vds_swmr_reader"
    )



    ##############################################################################
    ##############################################################################
    ###           A D D I T I O N A L   T E S T S                              ###
    ##############################################################################
    ##############################################################################
    # H5_CHECK_TESTS
    #---------------
    #    atomic_writer
    #    atomic_reader
    #    filenotclosed
    #    del_many_dense_attrs
    #    flushrefresh
    ##############################################################################
    # autotools script tests
    # error_test and err_compat are built at the same time as the other tests, but executed by testerror.sh.
    # NOT CONVERTED accum_swmr_reader is used by accum.c.
    # NOT CONVERTED atomic_writer and atomic_reader are standalone programs.
    # links_env is used by testlinks_env.sh
    # filenotclosed and del_many_dense_attrs are used by testabort_fail.sh
    # NOT CONVERTED flushrefresh is used by testflushrefresh.sh.
    # NOT CONVERTED use_append_chunk, use_append_mchunks and use_disable_mdc_flushes are used by test_usecases.sh
    # NOT CONVERTED swmr_* files (besides swmr.c) are used by testswmr.sh.
    # NOT CONVERTED vds_swmr_* files are used by testvdsswmr.sh
    # NOT CONVERTED 'make check' doesn't run them directly, so they are not included in TEST_PROG.
    # NOT CONVERTED Also build testmeta, which is used for timings test.  It builds quickly,
    # NOT CONVERTED and this lets automake keep all its test programs in one place.
    ##############################################################################

    ##############################################################################
    ###    S W M R  T E S T S
    ##############################################################################
    #       testflushrefresh.sh: flushrefresh
    #       test_usecases.sh: use_append_chunk, use_append_mchunks, use_disable_mdc_flushes
    #       testswmr.sh: swmr*
    #       testvdsswmr.sh: vds_swmr*
    add_test (H5SHELL-testflushrefresh ${SH_PROGRAM} ${HDF5_TEST_BINARY_DIR}/H5TEST/testflushrefresh.sh)
    set_tests_properties (H5SHELL-testflushrefresh PROPERTIES
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    )
    add_test (H5SHELL-test_usecases ${SH_PROGRAM} ${HDF5_TEST_BINARY_DIR}/H5TEST/test_usecases.sh)
    set_tests_properties (H5SHELL-test_usecases PROPERTIES
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    )
    add_test (H5SHELL-testswmr ${SH_PROGRAM} ${HDF5_TEST_BINARY_DIR}/H5TEST/testswmr.sh)
    set_tests_properties (H5SHELL-testswmr PROPERTIES
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    )
    add_test (H5SHELL-testvdsswmr ${SH_PROGRAM} ${HDF5_TEST_BINARY_DIR}/H5TEST/testvdsswmr.sh)
    set_tests_properties (H5SHELL-testvdsswmr PROPERTIES
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    )

  endif ()
endif ()
