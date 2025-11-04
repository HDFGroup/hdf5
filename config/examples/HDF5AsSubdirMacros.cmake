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
#############################################################################################
# This file contains macros for finding HDF5 and/or building HDF5 using FetchContent
#############################################################################################
#
# This macro is used to build HDF5 as a subdirectory using FetchContent to get the source code
# and build it.  The HDF5 options should be set after the FetchContent_Declare command and before
# the add_subdirectory command..
macro (EXTERNAL_HDF5_LIBRARY compress_type)
  set (HDF5_VERSION "2.0.0")
  set (HDF5_VERSEXT "")
  set (HDF5_VERSION_MAJOR "2.0")
  set (HDF5LIB_TGZ_NAME "hdf5.tar.gz" CACHE STRING "Use HDF5LIB from compressed file" FORCE)
  set (HDF5LIB_TGZ_ORIGPATH "https://github.com/HDFGroup/hdf5/releases/download/snapshot" CACHE STRING "Use HDF5LIB from original location" FORCE)
  set (HDF5LIB_USE_LOCALCONTENT ON CACHE BOOL "Use local file for HDF5LIB FetchContent" FORCE)
  set (HDF5LIB_GIT_URL "https://github.com/HDFGroup/hdf5.git" CACHE STRING "Use HDF5LIB from  GitHub repository" FORCE)
  set (HDF5LIB_GIT_BRANCH "develop" CACHE STRING "" FORCE)
  if (NOT HDF5LIB_USE_LOCALCONTENT)
    set (HDF5LIB_URL ${TGZ_DIR}/${HDF5LIB_TGZ_NAME}) #TGZ_DIR is set in the top level CMakeLists.txt
  else ()
    set (HDF5LIB_URL ${HDF5LIB_TGZ_ORIGPATH}/${HDF5LIB_TGZ_NAME})
  endif ()
  message (VERBOSE "Library HDF5LIB file is ${HDF5LIB_URL}")
  if (${compress_type} MATCHES "GIT")
    FetchContent_Declare (HDF5LIB
        GIT_REPOSITORY ${HDF5LIB_GIT_URL}
        GIT_TAG ${HDF5LIB_GIT_BRANCH}
    )
  elseif (${compress_type} MATCHES "TGZ")
    message (VERBOSE "Library HDF5LIB file ${HDF5LIB_URL}")
    FetchContent_Declare (HDF5LIB
        URL ${HDF5LIB_URL}
        URL_HASH ""
    )
  endif ()
  FetchContent_GetProperties(HDF5LIB)
  if(NOT hdf5lib_POPULATED)
    FetchContent_Populate(HDF5LIB)

# Adjust variables for building HDF5
    set (BUILD_SHARED_LIBS OFF CACHE BOOL "Build Shared Libraries" FORCE)
    set (HDF5_BUILD_CPP_LIB OFF CACHE BOOL "Build C++ support" FORCE)
    set (HDF5_BUILD_FORTRAN OFF CACHE BOOL "Build FORTRAN support" FORCE)
    set (HDF5_BUILD_JAVA OFF CACHE BOOL "Build JAVA support" FORCE)
    set (BUILD_TESTING OFF CACHE BOOL "Build JHDF5 Unit Testing" FORCE)
    set (HDF5_BUILD_EXAMPLES OFF CACHE BOOL "Build JHDF5 Library Examples" FORCE)
    set (HDF5_BUILD_HL_LIB OFF CACHE BOOL "Build JHDF5 HIGH Level HDF5 Library" FORCE)
    set (HDF5_ENABLE_ZLIB_SUPPORT OFF CACHE BOOL "Enable Zlib Filters" FORCE)
    set (HDF5_ENABLE_SZIP_SUPPORT OFF CACHE BOOL "Use SZip Filter" FORCE)

    add_subdirectory(${hdf5lib_SOURCE_DIR} ${hdf5lib_BINARY_DIR})
  endif()

  if (HDF_PACKAGE_NAMESPACE)
    add_library(${HDF_PACKAGE_NAMESPACE}hdf5lib-static ALIAS hdf5-static)
  endif()
  set (H5LIB_STATIC_LIBRARY "${HDF_PACKAGE_NAMESPACE}hdf5lib-static")
  set (H5LIB_LIBRARIES ${H5LIB_STATIC_LIBRARY})
  set (H5LIB_TOOLS ${hdf5lib_BINARY_DIR}/bin})

  set (H5LIB_INCLUDE_DIR_GEN "${hdf5lib_BINARY_DIR}/src")
  set (H5LIB_INCLUDE_DIR "${hdf5lib_SOURCE_DIR}/src")
  set (H5LIB_FOUND 1)
  set (H5LIB_INCLUDE_DIRS ${H5LIB_INCLUDE_DIR_GEN} ${H5LIB_INCLUDE_DIR})
  message (STATUS "HDF5-${HDF5_VERSION} found: INC=${H5LIB_INCLUDE_DIRS} TOOLS=${H5LIB_TOOLS}")
endmacro ()

# this macro is used to find HDF5 in the parent project.  It can be used
# to build HDF5 as a subdirectory using FetchContent or to find HDF5 using
# the find_package command.  The macro should be called in the parent project
macro (HDF5_SUPPORT EXTNAME) #EXTNAME is the extension name used in the parent project
  # H5_CONFIG_DIR and H5_RESOURCES_DIR are set in the top level CMakeLists.txt
  set (CMAKE_MODULE_PATH ${H5${EXTNAME}_CONFIG_DIR} ${H5${EXTNAME}_RESOURCES_DIR} ${CMAKE_MODULE_PATH})
  option (USE_SHARED_LIBS "Use Shared Libraries" ON)

  if (HDF_FETCH_HDF5)
    include (FetchContent)
    EXTERNAL_HDF5_LIBRARY(GIT)
    set (H5${EXTNAME}_HDF5_INCLUDE_DIRS ${H5LIB_INCLUDE_DIRS})
    set (H5${EXTNAME}_HDF5_HAVE_H5PUBCONF_H 1)
    set (H5${EXTNAME}_HDF5_HAVE_HDF5 1)
    set (H5${EXTNAME}_HDF5_HEADER "h5pubconf.h")
    set (H5${EXTNAME}_HDF5_LINK_LIBS ${H5${EXTNAME}_HDF5_LINK_LIBS} ${H5LIB_LIBRARIES})
    if (NOT TARGET h5dump)
      add_executable (h5dump IMPORTED)
    endif()
    set (H5${EXTNAME}_HDF5_DUMP_EXECUTABLE $<TARGET_FILE:h5dump>)
  else ()
    if (NOT H5${EXTNAME}_HDF5_HEADER)
      if (USE_SHARED_LIBS)
        set (FIND_HDF_COMPONENTS C shared)
      else ()
        set (FIND_HDF_COMPONENTS C static)
        set (HDF_BUILD_JAVA OFF CACHE BOOL "Build Java support" FORCE)
        message (STATUS "Using static HDF5 - disable build of Java examples")
      endif ()
      if (HDF_BUILD_FORTRAN)
        set (FIND_HDF_COMPONENTS ${FIND_HDF_COMPONENTS} Fortran)
      endif ()
      if (HDF_BUILD_JAVA)
        set (FIND_HDF_COMPONENTS ${FIND_HDF_COMPONENTS} Java)
        set (HDF5_Java_FOUND 1) #default setting for 1.10.1 and earlier
      endif ()
      message (STATUS "HDF5 find comps: ${FIND_HDF_COMPONENTS}")
      set (SEARCH_PACKAGE_NAME ${HDF5_PACKAGE_NAME})

      find_package (HDF5 NAMES ${SEARCH_PACKAGE_NAME} COMPONENTS ${FIND_HDF_COMPONENTS})
      message (STATUS "HDF5 C libs:${HDF5_FOUND} static:${HDF5_static_C_FOUND} and shared:${HDF5_shared_C_FOUND}")
      message (STATUS "HDF5 Fortran libs: static:${HDF5_static_Fortran_FOUND} and shared:${HDF5_shared_Fortran_FOUND}")
      message (STATUS "HDF5 Java libs: ${HDF5_Java_FOUND}")
      if (HDF5_FOUND)
        if (NOT HDF5_static_C_FOUND AND NOT HDF5_shared_C_FOUND)
          #find library from non-dual-binary package
          set (FIND_HDF_COMPONENTS C)
          if (HDF_BUILD_FORTRAN)
            set (FIND_HDF_COMPONENTS ${FIND_HDF_COMPONENTS} Fortran)
          endif ()
          if (HDF_BUILD_JAVA)
            set (FIND_HDF_COMPONENTS ${FIND_HDF_COMPONENTS} Java)
         endif ()
          message (STATUS "HDF5 find comps: ${FIND_HDF_COMPONENTS}")

          find_package (HDF5 NAMES ${SEARCH_PACKAGE_NAME} COMPONENTS ${FIND_HDF_COMPONENTS})
          message (STATUS "HDF5 libs:${HDF5_FOUND} C:${HDF5_C_FOUND} Fortran:${HDF5_Fortran_FOUND} Java:${HDF5_Java_FOUND}")
          set (H5${EXTNAME}_HDF5_LINK_LIBS ${H5${EXTNAME}_HDF5_LINK_LIBS} ${HDF5_LIBRARIES})
          if (HDF5_PROVIDES_SHARED_LIBS)
            add_definitions (-DH5_BUILT_AS_DYNAMIC_LIB)
          else ()
            add_definitions (-DH5_BUILT_AS_STATIC_LIB)
          endif ()
          if (USE_SHARED_LIBS AND WIN32)
            set_property (TARGET ${HDF5_NAMESPACE}h5dump PROPERTY IMPORTED_LOCATION "${HDF5_TOOLS_DIR}/h5dumpdll")
          else ()
            set_property (TARGET ${HDF5_NAMESPACE}h5dump PROPERTY IMPORTED_LOCATION "${HDF5_TOOLS_DIR}/h5dump")
          endif ()
          if (HDF_BUILD_JAVA)
            set (CMAKE_JAVA_INCLUDE_PATH "${CMAKE_JAVA_INCLUDE_PATH};${HDF5_JAVA_INCLUDE_DIRS}")
            message (STATUS "HDF5 jars:${HDF5_JAVA_INCLUDE_DIRS}")
          endif ()
          set (H5${EXTNAME}_HDF5_DUMP_EXECUTABLE $<TARGET_FILE:${HDF5_NAMESPACE}h5dump>)
        else ()
          if (USE_SHARED_LIBS AND HDF5_shared_C_FOUND)
            set (H5${EXTNAME}_HDF5_LINK_LIBS ${H5${EXTNAME}_HDF5_LINK_LIBS} ${HDF5_C_SHARED_LIBRARY})
            set (HDF5_LIBRARY_PATH ${PACKAGE_PREFIX_DIR}/lib)
          else ()
            set (H5${EXTNAME}_HDF5_LINK_LIBS ${H5${EXTNAME}_HDF5_LINK_LIBS} ${HDF5_C_STATIC_LIBRARY})
          endif ()
          if (HDF5_VERSION VERSION_LESS "1.14.4" AND NOT HDF5_shared_C_FOUND)
            if (NOT TARGET ${HDF5_NAMESPACE}h5dump-shared)
              add_executable (${HDF5_NAMESPACE}h5dump-shared IMPORTED)
            endif ()
            set (H5${EXTNAME}_HDF5_DUMP_EXECUTABLE $<TARGET_FILE:${HDF5_NAMESPACE}h5dump-shared>)
          else ()
            if (NOT TARGET ${HDF5_NAMESPACE}h5dump)
              add_executable (${HDF5_NAMESPACE}h5dump IMPORTED)
           endif()
            set (H5${EXTNAME}_HDF5_DUMP_EXECUTABLE $<TARGET_FILE:${HDF5_NAMESPACE}h5dump>)
          endif()

          if (NOT HDF5_static_Fortran_FOUND AND NOT HDF5_shared_Fortran_FOUND)
            set (HDF_BUILD_FORTRAN OFF CACHE BOOL "Build FORTRAN support" FORCE)
            message (STATUS "HDF5 Fortran libs not found - disable build of Fortran examples")
          else ()
            if (HDF_BUILD_FORTRAN AND ${HDF5_PROVIDES_FORTRAN})
              if (BUILD_SHARED_LIBS AND HDF5_shared_Fortran_FOUND)
                set (H5${EXTNAME}_HDF5_LINK_LIBS ${H5${EXTNAME}_HDF5_LINK_LIBS} ${HDF5_FORTRAN_SHARED_LIBRARY})
              elseif (HDF5_static_Fortran_FOUND)
                set (H5${EXTNAME}_HDF5_LINK_LIBS ${H5${EXTNAME}_HDF5_LINK_LIBS} ${HDF5_FORTRAN_STATIC_LIBRARY})
              else ()
                set (HDF_BUILD_FORTRAN OFF CACHE BOOL "Build FORTRAN support" FORCE)
                message (STATUS "HDF5 Fortran libs not found - disable build of Fortran examples")
              endif ()
            endif ()
          endif ()
          if (HDF_BUILD_JAVA)
            if (${HDF5_PROVIDES_JAVA})
              set (CMAKE_JAVA_INCLUDE_PATH "${CMAKE_JAVA_INCLUDE_PATH};${HDF5_JAVA_INCLUDE_DIRS}")
              if (HDF5_PROVIDES_JNI AND HDF5_Java_FOUND)
                set (H5${EXTNAME}_JAVA_LIBRARY ${HDF5_JAVA_LIBRARY})
                set (H5${EXTNAME}_JAVA_LIBRARIES ${HDF5_JAVA_LIBRARY})
                message (STATUS "HDF5 lib:${H5${EXTNAME}_JAVA_LIBRARY} jars:${HDF5_JAVA_INCLUDE_DIRS}}")
              else ()
                set (H5${EXTNAME}_JAVA_LIBRARY "${HDF5_JAVA_LIBRARY};${HDF5_JAVA_HDF5_LIBRARY}")
                set (H5${EXTNAME}_JAVA_LIBRARIES ${H5${EXTNAME}_JAVA_LIBRARY})
              endif ()
            else ()
              set (HDF_BUILD_JAVA OFF CACHE BOOL "Build Java support" FORCE)
              message (STATUS "HDF5 Java libs not found - disable build of Java examples")
            endif ()
          else ()
            set (HDF_BUILD_JAVA OFF CACHE BOOL "Build Java support" FORCE)
          endif ()
        endif ()
      else ()
        find_package (HDF5) # Legacy find
        #Legacy find_package does not set HDF5_TOOLS_DIR, so we set it here
        set (HDF5_TOOLS_DIR ${HDF5_LIBRARY_DIRS}/../bin)
        #Legacy find_package does not set HDF5_PROVIDES_SHARED_LIBS, so we set it here
        if (USE_SHARED_LIBS AND EXISTS "${HDF5_LIBRARY_DIRS}/libhdf5.so")
          set (HDF5_PROVIDES_SHARED_LIBS 1)
        else ()
          set (HDF5_PROVIDES_SHARED_LIBS 0)
        endif ()
        set (H5${EXTNAME}_HDF5_LINK_LIBS ${H5${EXTNAME}_HDF5_LINK_LIBS} ${HDF5_LIBRARIES})
        add_executable (${HDF5_NAMESPACE}h5dump IMPORTED)
        set_property (TARGET ${HDF5_NAMESPACE}h5dump PROPERTY IMPORTED_LOCATION "${HDF5_TOOLS_DIR}/h5dump")
        set (H5${EXTNAME}_HDF5_DUMP_EXECUTABLE $<TARGET_FILE:${HDF5_NAMESPACE}h5dump>)
      endif ()

      set (HDF5_PACKAGE_NAME ${SEARCH_PACKAGE_NAME})

      if (HDF5_FOUND)
        set (H5${EXTNAME}_HDF5_INCLUDE_DIRS ${HDF5_INCLUDE_DIR})
        set (H5${EXTNAME}_HDF5_HAVE_H5PUBCONF_H 1)
        set (H5${EXTNAME}_HDF5_HAVE_HDF5 1)
        set (H5${EXTNAME}_HDF5_HEADER "h5pubconf.h")
        message (STATUS "HDF5-${HDF5_VERSION_STRING} found: INC=${HDF5_INCLUDE_DIR} TOOLS=${HDF5_TOOLS_DIR}")
      else ()
        message (FATAL_ERROR " HDF5 is Required for HDF5 Examples")
      endif ()
    else ()
      # This project is being called from within another and HDF5 is already configured
      set (H5${EXTNAME}_HDF5_HAVE_H5PUBCONF_H 1)
      set (H5${EXTNAME}_HDF5_HAVE_HDF5 1)
      message (STATUS "HDF5-${HDF5_VERSION_STRING} used")
    endif ()
    if (HDF_BUILD_FORTRAN)
     list (APPEND H5${EXTNAME}_HDF5_INCLUDE_DIRS ${HDF5_INCLUDE_DIR_FORTRAN})
    endif ()
  endif ()
  message (STATUS "HDF5 link libs: ${H5${EXTNAME}_HDF5_LINK_LIBS} Includes: ${H5${EXTNAME}_HDF5_INCLUDE_DIRS}")
endmacro ()

#
# This macro is used to convert HDF5 1.X built CMake hdf5-config.cmake variables to HDF5 2.x built names.
macro (EXTERNAL_HDF5_STATUS) # add argument REV to convert from 2.x to 1.x names
  if (NOT ARGN)
    #-----------------------------------------------------------------------------
    # Languages:
    #-----------------------------------------------------------------------------
    set (${HDF5_PACKAGE_NAME}_PROVIDES_FORTRAN     ${HDF5_PACKAGE_NAME}_BUILD_FORTRAN)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_CPP_LIB     ${HDF5_PACKAGE_NAME}_BUILD_CPP_LIB)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_JAVA        ${HDF5_PACKAGE_NAME}_BUILD_JAVA)
    #-----------------------------------------------------------------------------
    # Features:
    #-----------------------------------------------------------------------------
    set (${HDF5_PACKAGE_NAME}_PROVIDES_HL_LIB          ${HDF5_PACKAGE_NAME}_BUILD_HL_LIB)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_SHARED_LIBS     ${HDF5_PACKAGE_NAME}_BUILD_SHARED_LIBS)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_STATIC_LIBS     ${HDF5_PACKAGE_NAME}_BUILD_STATIC_LIBS)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_THREADS         ${HDF5_PACKAGE_NAME}_ENABLE_THREADSAFE)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_THREADSAFE      ${HDF5_PACKAGE_NAME}_ENABLE_THREADSAFE)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_PARALLEL        ${HDF5_PACKAGE_NAME}_ENABLE_PARALLEL)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_DEPRECATED_SYMBOLS ${HDF5_PACKAGE_NAME}_ENABLE_DEPRECATED_SYMBOLS)
    #-----------------------------------------------------------------------------
    set (${HDF5_PACKAGE_NAME}_PROVIDES_TOOLS           ${HDF5_PACKAGE_NAME}_BUILD_TOOLS)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_STATIC_TOOLS    ${HDF5_PACKAGE_NAME}_BUILD_STATIC_TOOLS)
    #-----------------------------------------------------------------------------
    set (${HDF5_PACKAGE_NAME}_PROVIDES_NONSTANDARD_FEATURE_FLOAT16 ${HDF5_PACKAGE_NAME}_ENABLE_NONSTANDARD_FEATURE_FLOAT16)
    #-----------------------------------------------------------------------------
    set (${HDF5_PACKAGE_NAME}_PROVIDES_ZLIB_SUPPORT   ${HDF5_PACKAGE_NAME}_ENABLE_Z_LIB_SUPPORT)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_SZIP_SUPPORT   ${HDF5_PACKAGE_NAME}_ENABLE_SZIP_SUPPORT)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_SZIP_ENCODING  ${HDF5_PACKAGE_NAME}_ENABLE_SZIP_ENCODING)
    #-----------------------------------------------------------------------------
    set (${HDF5_PACKAGE_NAME}_PROVIDES_MAP_API        ${HDF5_PACKAGE_NAME}_ENABLE_MAP_API)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_DIRECT_VFD     ${HDF5_PACKAGE_NAME}_ENABLE_DIRECT_VFD)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_MIRROR_VFD     ${HDF5_PACKAGE_NAME}_ENABLE_MIRROR_VFD)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_ROS3_VFD       ${HDF5_PACKAGE_NAME}_ENABLE_ROS3_VFD)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_HDFS_VFD       ${HDF5_PACKAGE_NAME}_ENABLE_HDFS)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_SUBFILING_VFD  ${HDF5_PACKAGE_NAME}_ENABLE_SUBFILING_VFD)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_PLUGIN_SUPPORT ${HDF5_PACKAGE_NAME}_ENABLE_PLUGIN_SUPPORT)
  else ()
    #-----------------------------------------------------------------------------
    # Languages:
    #-----------------------------------------------------------------------------
    set (${HDF5_PACKAGE_NAME}_BUILD_FORTRAN     ${HDF5_PACKAGE_NAME}_PROVIDES_FORTRAN)
    set (${HDF5_PACKAGE_NAME}_BUILD_CPP_LIB     ${HDF5_PACKAGE_NAME}_PROVIDES_CPP_LIB)
    set (${HDF5_PACKAGE_NAME}_BUILD_JAVA        ${HDF5_PACKAGE_NAME}_PROVIDES_JAVA)
    #-----------------------------------------------------------------------------
    # Features:
    #-----------------------------------------------------------------------------
    set (${HDF5_PACKAGE_NAME}_BUILD_HL_LIB           ${HDF5_PACKAGE_NAME}_PROVIDES_HL_LIBS)
    set (${HDF5_PACKAGE_NAME}_BUILD_SHARED_LIBS      ${HDF5_PACKAGE_NAME}_PROVIDES_SHARED_LIBS)
    set (${HDF5_PACKAGE_NAME}_BUILD_STATIC_LIBS      ${HDF5_PACKAGE_NAME}_PROVIDES_STATIC_LIB)
    set (${HDF5_PACKAGE_NAME}_ENABLE_THREADSAFE      ${HDF5_PACKAGE_NAME}_PROVIDES_THREADSAFE)
    set (${HDF5_PACKAGE_NAME}_ENABLE_PARALLEL        ${HDF5_PACKAGE_NAME}_PROVIDES_PARALLEL)
    set (${HDF5_PACKAGE_NAME}_ENABLE_DEPRECATED_SYMBOLS ${HDF5_PACKAGE_NAME}_PROVIDES_DEPRECATED_SYMBOLS)
    #-----------------------------------------------------------------------------
    set (${HDF5_PACKAGE_NAME}_BUILD_TOOLS           ${HDF5_PACKAGE_NAME}_PROVIDES_TOOLS)
    set (${HDF5_PACKAGE_NAME}_BUILD_STATIC_TOOLS    ${HDF5_PACKAGE_NAME}_PROVIDES_STATIC_TOOLS)
    #-----------------------------------------------------------------------------
    set (${HDF5_PACKAGE_NAME}_BUILD_NONSTANDARD_FEATURE_FLOAT16 ${HDF5_PACKAGE_NAME}_PROVIDES_NONSTANDARD_FEATURE_FLOAT16)
    #-----------------------------------------------------------------------------
    set (${HDF5_PACKAGE_NAME}_ENABLE_Z_LIB_SUPPORT   ${HDF5_PACKAGE_NAME}_PROVIDES_ZLIB_SUPPORT)
    set (${HDF5_PACKAGE_NAME}_ENABLE_SZIP_SUPPORT    ${HDF5_PACKAGE_NAME}_PROVIDES_SZIP_SUPPORT)
    set (${HDF5_PACKAGE_NAME}_PROVIDES_SZIP_ENCODING  ${HDF5_PACKAGE_NAME}_PROVIDES_SZIP_ENCODING)
    #-----------------------------------------------------------------------------
    set (${HDF5_PACKAGE_NAME}_ENABLE_MAP_API        ${HDF5_PACKAGE_NAME}_PROVIDES_MAP_API)
    set (${HDF5_PACKAGE_NAME}_ENABLE_DIRECT_VFD     ${HDF5_PACKAGE_NAME}_PROVIDES_DIRECT_VFD)
    set (${HDF5_PACKAGE_NAME}_ENABLE_MIRROR_VFD     ${HDF5_PACKAGE_NAME}_PROVIDES_MIRROR_VFD)
    set (${HDF5_PACKAGE_NAME}_ENABLE_ROS3_VFD       ${HDF5_PACKAGE_NAME}_PROVIDES_ROS3_VFD)
    set (${HDF5_PACKAGE_NAME}_ENABLE_HDFS_VFD       ${HDF5_PACKAGE_NAME}_PROVIDES_HDFS)
    set (${HDF5_PACKAGE_NAME}_ENABLE_SUBFILING_VFD  ${HDF5_PACKAGE_NAME}_PROVIDES_SUBFILING_VFD)
    set (${HDF5_PACKAGE_NAME}_ENABLE_PLUGIN_SUPPORT ${HDF5_PACKAGE_NAME}_PROVIDES_PLUGIN_SUPPORT)
  endif ()
endmacro ()

