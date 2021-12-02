
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
###           T E S T I N G  S H E L L  S C R I P T S                      ###
##############################################################################

find_program (PWSH NAMES pwsh powershell)
if (PWSH)
    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST/usecases_test")
    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST/swmr_test")
    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST/vds_swmr_test")

    set (srcdir ${HDF5_TEST_SOURCE_DIR})
    set (bindir ${CMAKE_TEST_OUTPUT_DIRECTORY})
    set (testdir ${HDF5_TEST_BINARY_DIR}/H5TEST)
    configure_file(${HDF5_TEST_SOURCE_DIR}/testswmr.pwsh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/testswmr.ps1 @ONLY)
    # test commented out as currently the programs are not allowing another access to the data file
    #add_test (H5SHELL-testswmr ${PWSH} ${HDF5_TEST_BINARY_DIR}/H5TEST/testswmr.ps1)
    #set_tests_properties (H5SHELL-testswmr PROPERTIES
    #        ENVIRONMENT "PATH=$ENV{PATH}:${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
    #        WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    #)
    configure_file(${HDF5_TEST_SOURCE_DIR}/testvdsswmr.pwsh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/testvdsswmr.ps1 @ONLY)
    # test commented out as currently the programs are not allowing another access to the data file
    #add_test (H5SHELL-testvdsswmr ${PWSH} ${HDF5_TEST_BINARY_DIR}/H5TEST/testvdsswmr.ps1)
    #set_tests_properties (H5SHELL-testvdsswmr PROPERTIES
    #        ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
    #        WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    #)
elseif (UNIX)
  find_program (SH_PROGRAM bash)
  if (SH_PROGRAM)
    set (srcdir ${HDF5_TEST_SOURCE_DIR})
    set (bindir ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
    set (testdir ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
    ##############################################################################
    #  configure scripts to test dir
    ##############################################################################
    if (H5_PERL_FOUND)
      configure_file(${HDF5_TEST_SOURCE_DIR}/testflushrefresh.sh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/testflushrefresh.sh @ONLY)
    endif ()
    configure_file(${HDF5_TEST_SOURCE_DIR}/test_usecases.sh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/test_usecases.sh @ONLY)
    configure_file(${HDF5_TEST_SOURCE_DIR}/testswmr.sh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/testswmr.sh @ONLY)
    configure_file(${HDF5_TEST_SOURCE_DIR}/testvdsswmr.sh.in ${HDF5_TEST_BINARY_DIR}/H5TEST/testvdsswmr.sh @ONLY)

    ##############################################################################
    #  copy test programs to test dir
    ##############################################################################
    add_custom_command (
        TARGET     accum_swmr_reader
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different "${HDF5_SOURCE_DIR}/bin/output_filter.sh" "${HDF5_TEST_BINARY_DIR}/H5TEST/bin/output_filter.sh"
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
            ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    )
    add_test (H5SHELL-test_usecases ${SH_PROGRAM} ${HDF5_TEST_BINARY_DIR}/H5TEST/test_usecases.sh)
    set_tests_properties (H5SHELL-test_usecases PROPERTIES
            ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    )
    add_test (H5SHELL-testswmr ${SH_PROGRAM} ${HDF5_TEST_BINARY_DIR}/H5TEST/testswmr.sh)
    set_tests_properties (H5SHELL-testswmr PROPERTIES
            ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    )
    add_test (H5SHELL-testvdsswmr ${SH_PROGRAM} ${HDF5_TEST_BINARY_DIR}/H5TEST/testvdsswmr.sh)
    set_tests_properties (H5SHELL-testvdsswmr PROPERTIES
            ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    )
  endif ()
endif ()
