#-----------------------------------------------------------------------------
# CMake configuration for HDF5 Java test suite
# This file sets up the build, and test execution rules for the HDF5 Java test suite.
# It handles Java test source grouping, JAR creation, test data management, test execution (including VOL tests),
# and integration with the HDF5 Java FFM and core libraries.
#-----------------------------------------------------------------------------
cmake_minimum_required (VERSION 3.26)
project (HDF5_JAVA_JTEST Java)

set (CMAKE_VERBOSE_MAKEFILE 1)

set_directory_properties(PROPERTIES INCLUDE_DIRECTORIES "${HDF5_JAVA_JSRC_SOURCE_DIR};${HDF5_JAVA_JSRC_BINARY_DIR};${HDF5_JAVA_LIB_DIR}")

#-----------------------------------------------------------------------------
# Build FFM-only tests (no hdf.hdf5lib dependencies)
# Build FfmTestSupport first, then test files that depend on it
#-----------------------------------------------------------------------------

# Set classpath for FfmTestSupport compilation (needs FFM bindings)
set (CMAKE_JAVA_INCLUDE_PATH "${HDF5_JAVAHDF5_JARS}")

# Build FfmTestSupport utility class first
file (WRITE ${PROJECT_BINARY_DIR}/FfmTestSupportManifest.txt
"Main-Class: jtest.FfmTestSupport
Enable-Native-Access: ALL-UNNAMED
"
)

add_jar (${HDF5_JAVA_JTEST_LIB_TARGET}_FfmTestSupport
  MANIFEST ${PROJECT_BINARY_DIR}/FfmTestSupportManifest.txt
  FfmTestSupport.java
)

get_target_property (${HDF5_JAVA_JTEST_LIB_TARGET}_FfmTestSupport_JAR_FILE
                     ${HDF5_JAVA_JTEST_LIB_TARGET}_FfmTestSupport JAR_FILE)

add_dependencies (${HDF5_JAVA_JTEST_LIB_TARGET}_FfmTestSupport ${HDF5_JAVA_JSRC_LIB_TARGET})
set_target_properties (${HDF5_JAVA_JTEST_LIB_TARGET}_FfmTestSupport PROPERTIES FOLDER test/java/ffm)

if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_JAVA_JTEST_FfmTestSupport_SRC_FORMAT FfmTestSupport.java)
endif ()

# Build FFM test files (they depend on FfmTestSupport)
set (HDF5_JAVA_JTEST_FFM_TEST_SOURCES
    TestH5Fffm
    TestH5Dffm
    TestH5Sffm
    TestH5Tffm
    TestH5Affm
    TestH5Pffm
    TestH5Effm
    TestH5Gffm
    TestH5Iffm
    TestH5Lffm
    TestH5Rffm
    TestH5Offm
    TestH5VLffm
    TestH5PLffm
    TestH5Zffm
    TestH5FDffm
)

# Update classpath to include FfmTestSupport JAR for test compilation
set (CMAKE_JAVA_INCLUDE_PATH "${HDF5_JAVAHDF5_JARS};${${HDF5_JAVA_JTEST_LIB_TARGET}_FfmTestSupport_JAR_FILE};${HDF5_JAVA_LIB_DIR}/org.junit.jar;${HDF5_JAVA_LIB_DIR}/org.hamcrest.jar;${HDF5_JAVA_LOGGING_JAR};${HDF5_JAVA_LOGGING_SIMPLE_JAR}")

foreach (ffm_test_file ${HDF5_JAVA_JTEST_FFM_TEST_SOURCES})

  file (WRITE ${PROJECT_BINARY_DIR}/${ffm_test_file}FfmManifest.txt
  "Main-Class: jtest.${ffm_test_file}
Enable-Native-Access: ALL-UNNAMED
"
  )

  add_jar (${HDF5_JAVA_JTEST_LIB_TARGET}_${ffm_test_file}
    MANIFEST ${PROJECT_BINARY_DIR}/${ffm_test_file}FfmManifest.txt
    ${ffm_test_file}.java
  )

  get_target_property (${HDF5_JAVA_JTEST_LIB_TARGET}_${ffm_test_file}_JAR_FILE
                       ${HDF5_JAVA_JTEST_LIB_TARGET}_${ffm_test_file} JAR_FILE)

  # FFM tests depend on both javahdf5 JAR and FfmTestSupport JAR
  add_dependencies (${HDF5_JAVA_JTEST_LIB_TARGET}_${ffm_test_file} ${HDF5_JAVA_JSRC_LIB_TARGET} ${HDF5_JAVA_JTEST_LIB_TARGET}_FfmTestSupport)
  set_target_properties (${HDF5_JAVA_JTEST_LIB_TARGET}_${ffm_test_file} PROPERTIES FOLDER test/java/ffm)

  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_JAVA_JTEST_${ffm_test_file}_FFM_SRC_FORMAT ${ffm_test_file}.java)
  endif ()
endforeach ()

# Restore classpath for other tests
set (CMAKE_JAVA_INCLUDE_PATH "${HDF5_JAVAHDF5_JARS};${HDF5_JAVA_LIB_DIR}/org.junit.jar;${HDF5_JAVA_LIB_DIR}/org.hamcrest.jar;${HDF5_JAVA_LOGGING_JAR};${HDF5_JAVA_LOGGING_SIMPLE_JAR}")

HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/h5ex_g_iterate.orig" "${PROJECT_BINARY_DIR}/h5ex_g_iterate.hdf" "${HDF5_JAVA_JTEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/h5ex_g_iterate.orig" "${PROJECT_BINARY_DIR}/h5ex_g_iterateL1.hdf" "${HDF5_JAVA_JTEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/h5ex_g_iterate.orig" "${PROJECT_BINARY_DIR}/h5ex_g_iterateL2.hdf" "${HDF5_JAVA_JTEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/h5ex_g_iterate.orig" "${PROJECT_BINARY_DIR}/h5ex_g_iterateO1.hdf" "${HDF5_JAVA_JTEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/h5ex_g_iterate.orig" "${PROJECT_BINARY_DIR}/h5ex_g_iterateO2.hdf" "${HDF5_JAVA_JTEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/testfiles/trefer_reg.h5" "${PROJECT_BINARY_DIR}/trefer_reg.h5" "${HDF5_JAVA_JTEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/testfiles/trefer_attr.h5" "${PROJECT_BINARY_DIR}/trefer_attr.h5" "${HDF5_JAVA_JTEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/testfiles/tdatareg.h5" "${PROJECT_BINARY_DIR}/tdatareg.h5" "${HDF5_JAVA_JTEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/testfiles/tattrreg.h5" "${PROJECT_BINARY_DIR}/tattrreg.h5" "${HDF5_JAVA_JTEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/testfiles/tintsattrs.h5" "${PROJECT_BINARY_DIR}/tintsattrs.h5" "${HDF5_JAVA_JTEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/testfiles/tfloatsattrs.h5" "${PROJECT_BINARY_DIR}/tfloatsattrs.h5" "${HDF5_JAVA_JTEST_LIB_TARGET}_files")

add_custom_target(${HDF5_JAVA_JTEST_LIB_TARGET}_files ALL COMMENT "Copying files needed by ${HDF5_JAVA_JTEST_LIB_TARGET} tests" DEPENDS ${${HDF5_JAVA_JTEST_LIB_TARGET}_files_list})

if (WIN32)
  set (CMAKE_JAVA_INCLUDE_FLAG_SEP ";")
else ()
  set (CMAKE_JAVA_INCLUDE_FLAG_SEP ":")
endif ()

get_property (target_name TARGET ${HDF5_JAVA_JSRC_LIB_TARGET} PROPERTY OUTPUT_NAME)
set (CMAKE_JAVA_CLASSPATH ".")
foreach (CMAKE_INCLUDE_PATH ${CMAKE_JAVA_INCLUDE_PATH})
  set (CMAKE_JAVA_CLASSPATH "${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${CMAKE_INCLUDE_PATH}")
endforeach ()

if (HDF5_TEST_JAVA AND HDF5_TEST_SERIAL)
  add_test (
    NAME JUnitExt-clear-objects
    COMMAND ${CMAKE_COMMAND} -E remove
        test.h5
        testF2.h5
        testPf00000.h5
        testPf00001.h5
    WORKING_DIRECTORY ${HDF5_BINARY_DIR}/java/jtest
  )
  set_tests_properties (JUnitExt-clear-objects PROPERTIES FIXTURES_SETUP clear_JUnitExt)

  add_test (
    NAME JUnitExt-clean-objects
    COMMAND ${CMAKE_COMMAND} -E remove
        test.h5
        testF2.h5
        testPf00000.h5
        testPf00001.h5
    WORKING_DIRECTORY ${HDF5_BINARY_DIR}/java/jtest
  )
  set_tests_properties (JUnitExt-clean-objects PROPERTIES FIXTURES_CLEANUP clear_JUnitExt)

  #-----------------------------------------------------------------------------
  # Add FFM-only test execution (Test* files, skip FfmTestSupport utility)
  #-----------------------------------------------------------------------------
  foreach (ffm_test_file ${HDF5_JAVA_JTEST_FFM_TEST_SOURCES})
    set (TEST_FFM_JAVA_CLASSPATH "${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${${HDF5_JAVA_JTEST_LIB_TARGET}_${ffm_test_file}_JAR_FILE}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${${HDF5_JAVA_JTEST_LIB_TARGET}_FfmTestSupport_JAR_FILE}")

    add_test (
        NAME JUnitFFM-${ffm_test_file}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_JAVA=${CMAKE_Java_RUNTIME};${CMAKE_Java_RUNTIME_FLAGS}"
            -D "TEST_CLASSPATH:STRING=${TEST_FFM_JAVA_CLASSPATH}"
            -D "TEST_ARGS:STRING=${CMD_ARGS}-ea;org.junit.runner.JUnitCore"
            -D "TEST_PROGRAM=jtest.${ffm_test_file}"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
            -D "TEST_FOLDER=${HDF5_BINARY_DIR}/java/jtest"
            -D "TEST_OUTPUT=JUnitFFM-${ffm_test_file}.out"
            -D "TEST_EXPECT=0"
            -D "TEST_MASK_ERROR=TRUE"
            -D "TEST_SKIP_COMPARE=TRUE"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (JUnitFFM-${ffm_test_file} PROPERTIES
        ENVIRONMENT "HDF5_PLUGIN_PATH=${CMAKE_BINARY_DIR}/testdir2"
        FIXTURES_REQUIRED clear_JUnitExt
        WORKING_DIRECTORY ${HDF5_BINARY_DIR}/java/jtest
    )
    if ("JUnitFFM-${ffm_test_file}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (JUnitFFM-${ffm_test_file} PROPERTIES DISABLED true)
    endif ()
  endforeach ()
endif ()

set (CMAKE_JAVA_INCLUDE_PATH "")

