cmake_minimum_required (VERSION 3.12)
project (HDF5_JAVA_TEST Java)

set (CMAKE_VERBOSE_MAKEFILE 1)

set_directory_properties(PROPERTIES INCLUDE_DIRECTORIES "${HDF5_JAVA_JNI_BINARY_DIR};${HDF5_JAVA_HDF5_LIB_DIR}")

set (HDF5_JAVA_TEST_SOURCES
    TestH5
    TestH5Eparams
    TestH5Eregister
    TestH5Fparams
    TestH5Fbasic
    TestH5F
    TestH5Fswmr
    TestH5Gbasic
    TestH5G
    TestH5Sbasic
    TestH5S
    TestH5Tparams
    TestH5Tbasic
    TestH5T
    TestH5Dparams
    TestH5D
    TestH5Dplist
    TestH5Drw
    TestH5Lparams
    TestH5Lbasic
    TestH5Lcreate
    TestH5R
    TestH5Rref
    TestH5P
    TestH5PData
    TestH5Pfapl
    TestH5Pvirtual
    TestH5Plist
    TestH5A
    TestH5Arw
    TestH5Oparams
    TestH5Obasic
    TestH5Ocreate
    TestH5Ocopy
    TestH5PL
    TestH5VL
    TestH5Z
)

if (NOT HDF5_ENABLE_DEBUG_APIS)
  set (HDF5_JAVA_TEST_SOURCES
      ${HDF5_JAVA_TEST_SOURCES}
      TestH5E
      TestH5Edefault
      TestH5Giterate
  )
endif ()

if (HDF5_ENABLE_ROS3_VFD)
  set (HDF5_JAVA_TEST_SOURCES
      ${HDF5_JAVA_TEST_SOURCES}
      TestH5Pfapls3
  )
endif ()

if (HDF5_ENABLE_HDFS)
  set (HDF5_JAVA_TEST_SOURCES
      ${HDF5_JAVA_TEST_SOURCES}
      TestH5Pfaplhdfs
  )
endif ()

set (CMAKE_JAVA_INCLUDE_PATH "${HDF5_JAVA_LIB_DIR}/junit.jar;${HDF5_JAVA_LIB_DIR}/hamcrest-core.jar;${HDF5_JAVA_JARS};${HDF5_JAVA_LOGGING_JAR};${HDF5_JAVA_LOGGING_SIMPLE_JAR}")

foreach (test_file ${HDF5_JAVA_TEST_SOURCES})

  file (WRITE ${PROJECT_BINARY_DIR}/${test_file}Manifest.txt
  "Main-Class: test.${test_file}
"
  )

  add_jar (${HDF5_JAVA_TEST_LIB_TARGET}_${test_file} MANIFEST ${PROJECT_BINARY_DIR}/${test_file}Manifest.txt ${test_file}.java)

  get_target_property (${HDF5_JAVA_TEST_LIB_TARGET}_${test_file}_JAR_FILE ${HDF5_JAVA_TEST_LIB_TARGET}_${test_file} JAR_FILE)
  #install_jar (${HDF5_JAVA_TEST_LIB_TARGET}_${test_file} ${HJAVA_INSTALL_DATA_DIR}/tests tests)
  #get_target_property (${HDF5_JAVA_TEST_LIB_TARGET}_${test_file}_CLASSPATH ${HDF5_JAVA_TEST_LIB_TARGET}_${test_file} CLASSDIR)

  add_dependencies (${HDF5_JAVA_TEST_LIB_TARGET}_${test_file} ${HDF5_JAVA_HDF5_LIB_TARGET})
  set_target_properties (${HDF5_JAVA_TEST_LIB_TARGET}_${test_file} PROPERTIES FOLDER test/java)
endforeach ()

foreach (h5_file ${HDF5_JAVA_TEST_SOURCES})
  HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/JUnit-${h5_file}.txt" "${PROJECT_BINARY_DIR}/JUnit-${h5_file}.txt" "${HDF5_JAVA_TEST_LIB_TARGET}_files")
endforeach ()

HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/h5ex_g_iterate.orig" "${PROJECT_BINARY_DIR}/h5ex_g_iterate.hdf" "${HDF5_JAVA_TEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/h5ex_g_iterate.orig" "${PROJECT_BINARY_DIR}/h5ex_g_iterateL1.hdf" "${HDF5_JAVA_TEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/h5ex_g_iterate.orig" "${PROJECT_BINARY_DIR}/h5ex_g_iterateL2.hdf" "${HDF5_JAVA_TEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/h5ex_g_iterate.orig" "${PROJECT_BINARY_DIR}/h5ex_g_iterateO1.hdf" "${HDF5_JAVA_TEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/h5ex_g_iterate.orig" "${PROJECT_BINARY_DIR}/h5ex_g_iterateO2.hdf" "${HDF5_JAVA_TEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/trefer_reg.h5" "${PROJECT_BINARY_DIR}/trefer_reg.h5" "${HDF5_JAVA_TEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/trefer_attr.h5" "${PROJECT_BINARY_DIR}/trefer_attr.h5" "${HDF5_JAVA_TEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/tdatareg.h5" "${PROJECT_BINARY_DIR}/tdatareg.h5" "${HDF5_JAVA_TEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/tattrreg.h5" "${PROJECT_BINARY_DIR}/tattrreg.h5" "${HDF5_JAVA_TEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/tintsattrs.h5" "${PROJECT_BINARY_DIR}/tintsattrs.h5" "${HDF5_JAVA_TEST_LIB_TARGET}_files")
HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/tfloatsattrs.h5" "${PROJECT_BINARY_DIR}/tfloatsattrs.h5" "${HDF5_JAVA_TEST_LIB_TARGET}_files")

add_custom_target(${HDF5_JAVA_TEST_LIB_TARGET}_files ALL COMMENT "Copying files needed by ${HDF5_JAVA_TEST_LIB_TARGET} tests" DEPENDS ${${HDF5_JAVA_TEST_LIB_TARGET}_files_list})

if (WIN32)
  set (CMAKE_JAVA_INCLUDE_FLAG_SEP ";")
else ()
  set (CMAKE_JAVA_INCLUDE_FLAG_SEP ":")
endif ()

get_property (target_name TARGET ${HDF5_JAVA_JNI_LIB_TARGET} PROPERTY OUTPUT_NAME)
set (CMD_ARGS "-Dhdf.hdf5lib.H5.loadLibraryName=${target_name}$<$<CONFIG:Debug>:${CMAKE_DEBUG_POSTFIX}>;")

if (HDF5_TEST_JAVA AND HDF5_TEST_SERIAL)
  foreach (test_file ${HDF5_JAVA_TEST_SOURCES})
    set (CMAKE_JAVA_CLASSPATH ".")
    foreach (CMAKE_INCLUDE_PATH ${CMAKE_JAVA_INCLUDE_PATH})
      set (CMAKE_JAVA_CLASSPATH "${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${CMAKE_INCLUDE_PATH}")
    endforeach ()
    set (CMAKE_JAVA_CLASSPATH "${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${${HDF5_JAVA_TEST_LIB_TARGET}_${test_file}_JAR_FILE}")

    add_test (
      NAME JUnit-${test_file}-clearall-objects
      COMMAND ${CMAKE_COMMAND} -E remove
          JUnit-${test_file}.out
          JUnit-${test_file}.out.err
    )
    add_test (
        NAME JUnit-${test_file}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_TESTER=${CMAKE_Java_RUNTIME};${CMAKE_Java_RUNTIME_FLAGS}"
            -D "TEST_CLASSPATH:STRING=${CMAKE_JAVA_CLASSPATH}"
            -D "TEST_ARGS:STRING=${CMD_ARGS}-ea;org.junit.runner.JUnitCore"
            -D "TEST_PROGRAM=test.${test_file}"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
            -D "TEST_FOLDER=${HDF5_BINARY_DIR}/java/test"
            -D "TEST_OUTPUT=JUnit-${test_file}.out"
#            -D "TEST_LOG_LEVEL=trace"
            -D "TEST_EXPECT=0"
            -D "SKIP_APPEND=1"
            -D "TEST_MASK_ERROR=TRUE"
#            -D "TEST_FILTER:STRING=${testfilter}"
            -D "TEST_REFERENCE=JUnit-${test_file}.txt"
            -P "${HDF_RESOURCES_DIR}/jrunTest.cmake"
    )
    set_tests_properties (JUnit-${test_file} PROPERTIES
        ENVIRONMENT "HDF5_PLUGIN_PATH=${CMAKE_BINARY_DIR}/testdir2"
        DEPENDS "JUnit-${test_file}-clearall-objects"
    )
  endforeach ()


  ##############################################################################
  ##############################################################################
  ###                         V O L   T E S T S                              ###
  ##############################################################################
  ##############################################################################

  macro (DO_VOL_TEST voltest volname volinfo volclasspath)
    #message(STATUS "${voltest}-${volname} with ${volinfo}")
    add_test (
        NAME JUnit-VOL-${volname}-${voltest}-clearall-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            JUnit-VOL-${volname}-${voltest}.out
            JUnit-VOL-${volname}-${voltest}.out.err
        WORKING_DIRECTORY ${HDF5_BINARY_DIR}/java/test/${volname}
    )
    add_test (NAME JUnit-VOL-${volname}-${voltest}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_TESTER=${CMAKE_Java_RUNTIME};${CMAKE_Java_RUNTIME_FLAGS}"
            -D "TEST_CLASSPATH:STRING=${volclasspath}"
            -D "TEST_ARGS:STRING=${CMD_ARGS}-ea;org.junit.runner.JUnitCore"
            -D "TEST_PROGRAM=test.${voltest}"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
            -D "TEST_FOLDER=${HDF5_BINARY_DIR}/java/test/${volname}"
            -D "TEST_VOL:STRING=${volinfo}"
            -D "TEST_OUTPUT=JUnit-VOL-${volname}-${voltest}.out"
#            -D "TEST_LOG_LEVEL=trace"
            -D "TEST_EXPECT=0"
            -D "SKIP_APPEND=1"
            -D "TEST_MASK_ERROR=TRUE"
#            -D "TEST_FILTER:STRING=${testfilter}"
            -D "TEST_REFERENCE=JUnit-${volname}-${voltest}.txt"
            -P "${HDF_RESOURCES_DIR}/jvolTest.cmake"
    )
    set_tests_properties (JUnit-VOL-${volname}-${voltest} PROPERTIES
        ENVIRONMENT "HDF5_PLUGIN_PATH=${CMAKE_BINARY_DIR}/testdir2"
        DEPENDS "JUnit-VOL-${volname}-${voltest}-clearall-objects"
        WORKING_DIRECTORY ${HDF5_BINARY_DIR}/java/test/${volname}
    )
  endmacro ()

  if (HDF5_TEST_PASSTHROUGH_VOL)
    set (VOL_LIST
        vol_native
        vol_pass_through1
        vol_pass_through2
    )

    # native VOL = 0
    # pass-through VOL = 1
    set (vol_native native)
    set (vol_pass_through1 "pass_through under_vol=0\;under_info={}")
    set (vol_pass_through2 "pass_through under_vol=1\;under_info={under_vol=0\;under_info={}}")

    foreach (voltest ${VOL_LIST})
      file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${voltest}")
        foreach (h5_file ${HDF5_JAVA_TEST_SOURCES})
          HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/JUnit-${h5_file}.txt" "${PROJECT_BINARY_DIR}/${voltest}/JUnit-${h5_file}.txt" "${HDF5_JAVA_TEST_LIB_TARGET}_${voltest}_files")
        endforeach ()

        HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/h5ex_g_iterate.orig" "${PROJECT_BINARY_DIR}/${voltest}/h5ex_g_iterate.hdf" "${HDF5_JAVA_TEST_LIB_TARGET}_${voltest}_files")
        HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/h5ex_g_iterate.orig" "${PROJECT_BINARY_DIR}/${voltest}/h5ex_g_iterateL1.hdf" "${HDF5_JAVA_TEST_LIB_TARGET}_${voltest}_files")
        HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/h5ex_g_iterate.orig" "${PROJECT_BINARY_DIR}/${voltest}/h5ex_g_iterateL2.hdf" "${HDF5_JAVA_TEST_LIB_TARGET}_${voltest}_files")
        HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/h5ex_g_iterate.orig" "${PROJECT_BINARY_DIR}/${voltest}/h5ex_g_iterateO1.hdf" "${HDF5_JAVA_TEST_LIB_TARGET}_${voltest}_files")
        HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/h5ex_g_iterate.orig" "${PROJECT_BINARY_DIR}/${voltest}/h5ex_g_iterateO2.hdf" "${HDF5_JAVA_TEST_LIB_TARGET}_${voltest}_files")
        HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/trefer_reg.h5" "${PROJECT_BINARY_DIR}/${voltest}/trefer_reg.h5" "${HDF5_JAVA_TEST_LIB_TARGET}_${voltest}_files")
        HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/trefer_attr.h5" "${PROJECT_BINARY_DIR}/${voltest}/trefer_attr.h5" "${HDF5_JAVA_TEST_LIB_TARGET}_${voltest}_files")
        HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/tdatareg.h5" "${PROJECT_BINARY_DIR}/${voltest}/tdatareg.h5" "${HDF5_JAVA_TEST_LIB_TARGET}_${voltest}_files")
        HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/tattrreg.h5" "${PROJECT_BINARY_DIR}/${voltest}/tattrreg.h5" "${HDF5_JAVA_TEST_LIB_TARGET}_${voltest}_files")
        HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/tintsattrs.h5" "${PROJECT_BINARY_DIR}/${voltest}/tintsattrs.h5" "${HDF5_JAVA_TEST_LIB_TARGET}_${voltest}_files")
        HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/tfloatsattrs.h5" "${PROJECT_BINARY_DIR}/${voltest}/tfloatsattrs.h5" "${HDF5_JAVA_TEST_LIB_TARGET}_${voltest}_files")

        add_custom_target(${HDF5_JAVA_TEST_LIB_TARGET}_${voltest}_files ALL COMMENT "Copying files needed by ${HDF5_JAVA_TEST_LIB_TARGET} tests" DEPENDS ${${HDF5_JAVA_TEST_LIB_TARGET}_${voltest}_files_list})

        foreach (volinfo IN LISTS ${voltest})
          foreach (h5_file ${HDF5_JAVA_TEST_SOURCES})
            set (CMAKE_JAVA_CLASSPATH ".")
            foreach (CMAKE_INCLUDE_PATH ${CMAKE_JAVA_INCLUDE_PATH})
              set (CMAKE_JAVA_CLASSPATH "${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${CMAKE_INCLUDE_PATH}")
            endforeach ()
            set (CMAKE_JAVA_CLASSPATH "${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${${HDF5_JAVA_TEST_LIB_TARGET}_${h5_file}_JAR_FILE}")
            DO_VOL_TEST (${h5_file} ${voltest} "${volinfo}" ${CMAKE_JAVA_CLASSPATH})
          endforeach ()
        endforeach ()
    endforeach ()
  endif ()
endif ()
