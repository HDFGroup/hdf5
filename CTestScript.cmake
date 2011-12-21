cmake_minimum_required(VERSION 2.8.4 FATAL_ERROR)
########################################################
# Local variables to set before calling this script
#
#set(CTEST_SOURCE_NAME ${CTEST_SCRIPT_ARG})
#set(CTEST_BINARY_NAME ${CTEST_SOURCE_NAME}/build)
#set(CTEST_DASHBOARD_ROOT ${CTEST_SCRIPT_DIRECTORY})
#set(CTEST_SOURCE_DIRECTORY "${CTEST_DASHBOARD_ROOT}/${CTEST_SOURCE_NAME}")
#set(CTEST_BINARY_DIRECTORY "${CTEST_DASHBOARD_ROOT}/${CTEST_BINARY_NAME}")
#set(CTEST_CMAKE_GENERATOR "[Unix Makefiles | Visual Studio 10 | etc]")
#set(CTEST_BUILD_CONFIGURATION "[Release | Debug | etc]")
#set(CTEST_SITE "name of site [i.e. computer.organization")
#set(CDASH_LOCAL TRUE)
#set(MODEL "[Nightly | Experimental | etc]")
#
#set(SITE_OS_NAME "name of os")
#set(SITE_OS_VERSION "version of os")
#set(SITE_OS_BITS "[32 | 64 | etc]")
#set(SITE_COMPILER_NAME "compiler type")
#set(SITE_COMPILER_VERSION "version of compiler")
#
#set(REPOSITORY_URL "http://svn.path.to/product")
######## optional variables  #############
#set(SITE_BUILDNAME_SUFFIX "optional suffix for build name")
#set(ADD_BUILD_OPTIONS "-Doption_name:option_type=optionvalue")
######## end of optional variables  ######
#
#include(${CTEST_SOURCE_DIRECTORY}/CTestScript.cmake)
########################################################

set(CTEST_BUILD_NAME "${SITE_OS_NAME}-${SITE_OS_VERSION}-${SITE_OS_BITS}-${SITE_COMPILER_NAME}-${SITE_COMPILER_VERSION}")
if(SITE_BUILDNAME_SUFFIX)
  set(CTEST_BUILD_NAME ${CTEST_BUILD_NAME}-${SITE_BUILDNAME_SUFFIX})
endif()
set(BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DSITE:STRING=${CTEST_SITE} -DBUILDNAME:STRING=${CTEST_BUILD_NAME}")

FIND_PACKAGE (Subversion)
set(CTEST_UPDATE_COMMAND ${Subversion_SVN_EXECUTABLE})

set(NEED_REPOSITORY_CHECKOUT 0)

IF (NOT EXISTS "${CTEST_SOURCE_DIRECTORY}")
  SET (NEED_REPOSITORY_CHECKOUT 1)
ENDIF (NOT EXISTS "${CTEST_SOURCE_DIRECTORY}")

IF (${NEED_REPOSITORY_CHECKOUT})
  SET (CTEST_CHECKOUT_COMMAND
    "${CTEST_UPDATE_COMMAND} co ${REPOSITORY_URL} \"${CTEST_SOURCE_DIRECTORY}\" -r HEAD")
ELSE (${NEED_REPOSITORY_CHECKOUT})
  SET (CTEST_CHECKOUT_COMMAND "${CTEST_UPDATE_COMMAND} update")
ENDIF (${NEED_REPOSITORY_CHECKOUT})

set(CTEST_START_WITH_EMPTY_BINARY_DIRECTORY TRUE)

file(MAKE_DIRECTORY "${CTEST_BINARY_DIRECTORY}")

include(${CTEST_SOURCE_DIRECTORY}/CTestConfig.cmake)

ctest_empty_binary_directory(${CTEST_BINARY_DIRECTORY})

#-----------------------------------------------------------------------------
# Send the main script as a note.
list(APPEND CTEST_NOTES_FILES
  "${CTEST_SCRIPT_DIRECTORY}/${CTEST_SCRIPT_NAME}"
  "${CMAKE_CURRENT_LIST_FILE}"
  "${CTEST_SOURCE_DIRECTORY}/config/cmake/cacheinit.cmake"
  )

# Check for required variables.
foreach(req
    CTEST_CMAKE_GENERATOR
    CTEST_SITE
    CTEST_BUILD_NAME
    )
  if(NOT DEFINED ${req})
    message(FATAL_ERROR "The containing script must set ${req}")
  endif()
endforeach(req)

# Print summary information.
foreach(v
    CTEST_SITE
    CTEST_BUILD_NAME
    CTEST_SOURCE_DIRECTORY
    CTEST_BINARY_DIRECTORY
    CTEST_CMAKE_GENERATOR
    CTEST_BUILD_CONFIGURATION
    CTEST_GIT_COMMAND
    CTEST_CHECKOUT_COMMAND
    CTEST_CONFIGURE_COMMAND
    CTEST_SCRIPT_DIRECTORY
    CTEST_USE_LAUNCHERS
    )
  set(vars "${vars}  ${v}=[${${v}}]\n")
endforeach(v)
message("Dashboard script configuration:\n${vars}\n")
#-----------------------------------------------------------------------------

SET (CTEST_CONFIGURE_COMMAND
    "${CMAKE_COMMAND} -C ${CTEST_SOURCE_DIRECTORY}/config/cmake/cacheinit.cmake -DCMAKE_BUILD_TYPE:STRING=${CTEST_BUILD_CONFIGURATION} ${BUILD_OPTIONS} \"-G${CTEST_CMAKE_GENERATOR}\" \"${CTEST_SOURCE_DIRECTORY}\"")

CTEST_START (${MODEL})
CTEST_UPDATE (SOURCE     "${CTEST_SOURCE_DIRECTORY}")
CTEST_CONFIGURE (BUILD   "${CTEST_BINARY_DIRECTORY}")
CTEST_READ_CUSTOM_FILES ("${CTEST_BINARY_DIRECTORY}")
CTEST_BUILD (BUILD       "${CTEST_BINARY_DIRECTORY}")
if(NOT LOCAL_SKIP_TEST)
  CTEST_TEST (BUILD        "${CTEST_BINARY_DIRECTORY}")
#  CTEST_MEMCHECK (BUILD    "${CTEST_BINARY_DIRECTORY}")
#  CTEST_COVERAGE (BUILD    "${CTEST_BINARY_DIRECTORY}")
endif(NOT LOCAL_SKIP_TEST)
CTEST_SUBMIT ()

EXECUTE_PROCESS (COMMAND "cpack"
    WORKING_DIRECTORY ${CTEST_BINARY_DIRECTORY}
    RESULT_VARIABLE cpackResult
    OUTPUT_VARIABLE cpackLog
    ERROR_VARIABLE cpackLog.err
)
FILE (WRITE ${CTEST_BINARY_DIRECTORY}/Testing/cpack.log "${cpackLog.err}" "${cpackLog}")
FILE (GLOB UPLOAD_FILES "build/*.deb" "build/*.rpm" "build/*.exe")
FOREACH (_currentArg ${UPLOAD_FILES})
  GET_FILENAME_COMPONENT (_fn ${_currentArg} NAME)
  EXECUTE_PROCESS (COMMAND wget
      "${CTEST_DROP_METHOD}://${CTEST_DROP_SITE}${CTEST_DROP_BUILD_LOCATION}&fn=${_fn}"
      "--post-file=${_currentArg}"
      "-o${CTEST_BINARY_DIRECTORY}/Testing/upload.log"
      "-q"
  )
  FILE (REMOVE "${CTEST_BINARY_DIRECTORY}/${_currentArg}")
ENDFOREACH (_currentArg ${UPLOAD_FILES})

message("DONE:CTestScript")
