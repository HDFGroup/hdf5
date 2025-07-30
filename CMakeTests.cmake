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

# -----------------------------------------------------------------------------
# HDF5 CMake Testing and Dashboard Configuration
# -----------------------------------------------------------------------------
# This CMake module configures the testing and dashboard infrastructure for HDF5.
# It sets up test timeouts, test options, and enables/disables various test suites
# (API, VFD, VOL, serial, parallel, Fortran, C++, Java, tools, examples, SWMR, etc.).
# It also configures CTest integration, test express levels, and test directories.
#
# Key Features:
# - Configures DART/CTest timeouts and test express levels.
# - Provides options to enable/disable specific test suites and features.
# - Supports advanced test options (e.g., API async, driver, VFD lists, passthrough VOL).
# - Integrates with CTest and dashboard tools for automated testing.
# - Handles test directory setup for serial, parallel, and API tests.
#
# Usage:
#   HDF5 includes this file from the main CMakeLists.txt to enable and configure
#   HDF5 testing, if testing is enabled (BUILD_TESTING). Adjust options as needed
#   for your build and test requirements.
#
# See comments throughout for details on each option and logic branch.
# -----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Dashboard and Testing Settings
#-----------------------------------------------------------------------------
  set (DART_TESTING_TIMEOUT 1200
      CACHE STRING
      "Timeout in seconds for each test (default 1200=20minutes)"
  )

  # Generate a list of timeouts based on DART_TESTING_TIMEOUT
  math (EXPR CTEST_SHORT_TIMEOUT "${DART_TESTING_TIMEOUT} / 2")
  math (EXPR CTEST_LONG_TIMEOUT "${DART_TESTING_TIMEOUT} * 2")
  math (EXPR CTEST_VERY_LONG_TIMEOUT "${DART_TESTING_TIMEOUT} * 3")

  option (HDF5_DISABLE_TESTS_REGEX "Regex pattern to set execution of specific tests to DISABLED" "")
  mark_as_advanced (HDF5_DISABLE_TESTS_REGEX)

  if (HDF5_ENABLE_ROS3_VFD)
    if (HDF5_ENABLE_ROS3_VFD_DOCKER_PROXY)
      # Create a test credentials file
      file (WRITE "${CMAKE_BINARY_DIR}/credentials" "[default]\naws_access_key_id = remote-identity\naws_secret_access_key = remote-credential\nregion = us-east-2\n\n[ros3_vfd_test]\naws_access_key_id = remote-identity\naws_secret_access_key = remote-credential\nregion = us-east-2\n")
    endif ()
  endif ()

  option (HDF5_TEST_API "Execute HDF5 API tests" ON)
  mark_as_advanced (HDF5_TEST_API)
  cmake_dependent_option (HDF5_TEST_API_INSTALL "Install HDF5 API tests" OFF HDF5_TEST_API OFF)
  mark_as_advanced (HDF5_TEST_API_INSTALL)

  # Enable HDF5 Async API tests
  cmake_dependent_option (HDF5_TEST_API_ENABLE_ASYNC "Enable HDF5 Async API tests" OFF HDF5_TEST_API OFF)
  mark_as_advanced (HDF5_TEST_API_ENABLE_ASYNC)

  # Build and use HDF5 test driver program for API tests
  cmake_dependent_option (HDF5_TEST_API_ENABLE_DRIVER "Enable HDF5 API test driver program" OFF HDF5_TEST_API OFF)
  mark_as_advanced (HDF5_TEST_API_ENABLE_DRIVER)
  if (HDF5_TEST_API_ENABLE_DRIVER)
    set (HDF5_TEST_API_SERVER "" CACHE STRING "Server executable for running API tests")
    mark_as_advanced (HDF5_TEST_API_SERVER)
  endif ()

  option (HDF5_TEST_VFD "Execute tests with different VFDs" OFF)
  mark_as_advanced (HDF5_TEST_VFD)
  cmake_dependent_option (HDF5_TEST_FHEAP_VFD "Execute tests with different VFDs" ON HDF5_TEST_VFD OFF)
  mark_as_advanced (HDF5_TEST_FHEAP_VFD)

  if (HDF5_TEST_VFD)
    # Initialize the list of VFDs to be used for testing and create a test folder for each VFD
    H5_SET_VFD_LIST ()
  endif ()

  option (HDF5_TEST_PASSTHROUGH_VOL "Execute tests with different passthrough VOL connectors" OFF)
  mark_as_advanced (HDF5_TEST_PASSTHROUGH_VOL)
  cmake_dependent_option (HDF5_TEST_FHEAP_PASSTHROUGH_VOL "Execute fheap test with different passthrough VOL connectors" ON HDF5_TEST_PASSTHROUGH_VOL OFF)
  mark_as_advanced (HDF5_TEST_FHEAP_PASSTHROUGH VOL)

  set (H5_TEST_EXPRESS_LEVEL_DEFAULT "3")
  set (HDF_TEST_EXPRESS "${H5_TEST_EXPRESS_LEVEL_DEFAULT}"
      CACHE STRING "Control testing framework (0-3) (0 = exhaustive testing; 3 = quicker testing)")
  mark_as_advanced (HDF_TEST_EXPRESS)
  if (NOT "${HDF_TEST_EXPRESS}" STREQUAL "")
    set (H5_TEST_EXPRESS_LEVEL_DEFAULT "${HDF_TEST_EXPRESS}")
  endif ()

  enable_testing ()
  include (CTest)

  include (${HDF5_SOURCE_DIR}/CTestConfig.cmake)
  configure_file (${HDF_CONFIG_DIR}/CTestCustom.cmake ${HDF5_BINARY_DIR}/CTestCustom.ctest @ONLY)

  option (HDF5_TEST_SERIAL "Execute non-parallel tests" ON)
  mark_as_advanced (HDF5_TEST_SERIAL)

  cmake_dependent_option (HDF5_TEST_TOOLS "Execute tools tests" ON "HDF5_BUILD_TOOLS" OFF)

  cmake_dependent_option (HDF5_TEST_EXAMPLES "Execute tests on examples" ON "HDF5_BUILD_EXAMPLES" OFF)
  mark_as_advanced (HDF5_TEST_EXAMPLES)

  option (HDF5_TEST_SWMR "Execute SWMR tests" ON)
  mark_as_advanced (HDF5_TEST_SWMR)

  cmake_dependent_option (HDF5_TEST_PARALLEL "Execute parallel tests" ON "HDF5_ENABLE_PARALLEL" OFF)
  mark_as_advanced (HDF5_TEST_PARALLEL)

  cmake_dependent_option (HDF5_TEST_FORTRAN "Execute fortran tests" ON "HDF5_BUILD_FORTRAN" OFF)
  mark_as_advanced (HDF5_TEST_FORTRAN)

  cmake_dependent_option (HDF5_TEST_CPP "Execute cpp tests" ON "HDF5_BUILD_CPP_LIB" OFF)
  mark_as_advanced (HDF5_TEST_CPP)

  cmake_dependent_option (HDF5_TEST_JAVA "Execute java tests" ON "HDF5_BUILD_JAVA" OFF)
  mark_as_advanced (HDF5_TEST_JAVA)

  if (NOT HDF5_EXTERNALLY_CONFIGURED)
    if (EXISTS "${HDF5_TEST_SRC_DIR}" AND IS_DIRECTORY "${HDF5_TEST_SRC_DIR}")
      add_subdirectory (test)
    endif ()
    if (H5_HAVE_PARALLEL)
      if (EXISTS "${HDF5_TEST_PAR_DIR}" AND IS_DIRECTORY "${HDF5_TEST_PAR_DIR}")
        add_subdirectory (testpar)
      endif ()
    endif ()
  endif ()
