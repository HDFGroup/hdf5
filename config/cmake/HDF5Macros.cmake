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
#-------------------------------------------------------------------------------
macro (H5_SET_LIB_OPTIONS libtarget libname libtype libpackage)
  set (LIB_OUT_NAME "${libname}")
  # SOVERSION passed in ARGN when shared
  if (${libtype} MATCHES "SHARED")
    set (PACKAGE_SOVERSION ${HDF5_${libpackage}_PACKAGE_SOVERSION})
    set (PACKAGE_COMPATIBILITY ${H5_${libpackage}_SOVERS_INTERFACE}.0.0)
    set (PACKAGE_CURRENT ${H5_${libpackage}_SOVERS_INTERFACE}.${H5_${libpackage}_SOVERS_MINOR}.0)
    if (WIN32)
      set (LIBHDF_VERSION ${HDF5_PACKAGE_VERSION_MAJOR})
    else ()
      set (LIBHDF_VERSION ${HDF5_${libpackage}_PACKAGE_SOVERSION_MAJOR})
    endif ()
    set_target_properties (${libtarget} PROPERTIES VERSION ${PACKAGE_SOVERSION})
    if (WIN32)
        set (${LIB_OUT_NAME} "${LIB_OUT_NAME}-${LIBHDF_VERSION}")
    else ()
        set_target_properties (${libtarget} PROPERTIES SOVERSION ${LIBHDF_VERSION})
    endif ()
    if (CMAKE_C_OSX_CURRENT_VERSION_FLAG)
      set_property(TARGET ${libtarget} APPEND PROPERTY
          LINK_FLAGS "${CMAKE_C_OSX_CURRENT_VERSION_FLAG}${PACKAGE_CURRENT} ${CMAKE_C_OSX_COMPATIBILITY_VERSION_FLAG}${PACKAGE_COMPATIBILITY}"
      )
    endif ()
  endif ()
  HDF_SET_LIB_OPTIONS (${libtarget} ${LIB_OUT_NAME} ${libtype})

  #-- Apple Specific install_name for libraries
  if (APPLE)
    option (HDF5_BUILD_WITH_INSTALL_NAME "Build with library install_name set to the installation path" OFF)
    if (HDF5_BUILD_WITH_INSTALL_NAME)
      set_target_properties (${libtarget} PROPERTIES
          INSTALL_NAME_DIR "${CMAKE_INSTALL_PREFIX}/lib"
          BUILD_WITH_INSTALL_RPATH ${HDF5_BUILD_WITH_INSTALL_NAME}
      )
    endif ()
    if (HDF5_BUILD_FRAMEWORKS)
      if (${libtype} MATCHES "SHARED")
        # adapt target to build frameworks instead of dylibs
        set_target_properties(${libtarget} PROPERTIES
            XCODE_ATTRIBUTE_INSTALL_PATH "@rpath"
            FRAMEWORK TRUE
            FRAMEWORK_VERSION ${HDF5_PACKAGE_VERSION_MAJOR}
            MACOSX_FRAMEWORK_IDENTIFIER org.hdfgroup.${libtarget}
            MACOSX_FRAMEWORK_SHORT_VERSION_STRING ${HDF5_PACKAGE_VERSION_MAJOR}
            MACOSX_FRAMEWORK_BUNDLE_VERSION ${HDF5_PACKAGE_VERSION_MAJOR})
      endif ()
    endif ()
  endif ()
endmacro ()

# Initialize the list of VFDs to be used for testing and create a test folder for each VFD
macro (H5_SET_VFD_LIST)
  set (VFD_LIST
      sec2
      stdio
      core
      core_paged
      split
      multi
      family
      # Splitter VFD currently can't be tested with the h5_fileaccess()
      # approach due to it trying to lock the same W/O file when two
      # files are created/opened with the same FAPL that has the VFD
      # set on it. When tested with the environment variable and a
      # default FAPL, the VFD appends "_wo" to the filename when the
      # W/O path isn't specified, which works for all the tests.
      #splitter
      # Log VFD currently has file space allocation bugs
      #log
      # Onion VFD not currently tested with VFD tests
      #onion
  )

  if (H5_HAVE_DIRECT)
    list (APPEND VFD_LIST direct)
  endif ()
  if (H5_HAVE_PARALLEL)
    # MPI I/O VFD is currently incompatible with too many tests in the VFD test set
    # list (APPEND VFD_LIST mpio)
  endif ()
  if (H5_HAVE_MIRROR_VFD)
    # Mirror VFD needs network configuration, etc. and isn't easy to set
    # reasonable defaults for that info.
    # list (APPEND VFD_LIST mirror)
  endif ()
  if (H5_HAVE_ROS3_VFD)
    # This would require a custom test suite
    # list (APPEND VFD_LIST ros3)
  endif ()
  if (H5_HAVE_LIBHDFS)
    # This would require a custom test suite
    # list (APPEND VFD_LIST hdfs)
  endif ()
  if (H5_HAVE_SUBFILING_VFD)
    # Subfiling has a few VFD test failures to be resolved
    # list (APPEND VFD_LIST subfiling)
  endif ()
  if (H5_HAVE_WINDOWS)
    list (APPEND VFD_LIST windows)
  endif ()
endmacro ()

# Initialize the list of VFDs to be used for testing and create a test folder for each VFD
macro (H5_CREATE_VFD_DIR)
  foreach (vfdtest ${VFD_LIST})
    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${vfdtest}")
  endforeach ()
endmacro ()

# Retrieve the name of the VOL specified by the 
# HDF5_VOL_URLXX/HDF5_VOL_PATHXX variables.
# Output vars are hdf5_vol_name, hdf5_vol_name_upper, hdf5_vol_name_lower
macro(LOAD_VOL_NAME index)
    # Generate fixed-width index number prepended with 0s
    # so VOL sources come in order from 1 - HDF5_MAX_EXTERNAL_VOLS
    set (vol_idx_num_digits 2) # Based on HDF5_MAX_EXTERNAL_VOLS
    set (vol_idx_fixed "${vol_idx}")
    string (LENGTH "${vol_idx_fixed}" vol_idx_len)
    while (vol_idx_len LESS vol_idx_num_digits)
      string (PREPEND vol_idx_fixed "0")
      math (EXPR vol_idx_len "${vol_idx_len}+1")
    endwhile ()

    if (HDF5_VOL_ALLOW_EXTERNAL MATCHES "GIT")
      set (HDF5_VOL_URL${vol_idx_fixed} "" CACHE STRING "Git repository URL of an external HDF5 VOL connector to build")
      mark_as_advanced (HDF5_VOL_URL${vol_idx_fixed})
      set (HDF5_VOL_SOURCE "${HDF5_VOL_URL${vol_idx_fixed}}")
    elseif(HDF5_VOL_ALLOW_EXTERNAL MATCHES "LOCAL_DIR")
      set (HDF5_VOL_PATH${vol_idx_fixed} "" CACHE STRING "Path to the source directory of an external HDF5 VOL connector to build")
      mark_as_advanced (HDF5_VOL_PATH${vol_idx_fixed})
      set (HDF5_VOL_SOURCE "${HDF5_VOL_PATH${vol_idx_fixed}}")
    endif()

    if ("${HDF5_VOL_SOURCE}" STREQUAL "")
      set (hdf5_vol_name "")
      set (hdf5_vol_name_upper "")
      set (hdf5_vol_name_lower "")
    else()
      # Deal with trailing slash in path for LOCAL_DIR case
      if (HDF5_VOL_ALLOW_EXTERNAL MATCHES "LOCAL_DIR")
        # Erase trailing slash
        string (REGEX REPLACE "/$" "" HDF5_VOL_SOURCE ${HDF5_VOL_SOURCE})
      endif()

      # Extract the name of the VOL connector
      string (FIND "${HDF5_VOL_SOURCE}" "/" hdf5_vol_name_pos REVERSE)
      if (hdf5_vol_name_pos EQUAL -1)
        if (HDF5_VOL_ALLOW_EXTERNAL MATCHES "GIT")
          message (SEND_ERROR "Invalid URL '${HDF5_VOL_SOURCE}' specified for HDF5_VOL_URL${vol_idx_fixed}")
        elseif (HDF5_VOL_ALLOW_EXTERNAL MATCHES "LOCAL_DIR")
          message (SEND_ERROR "Invalid source path '${HDF5_VOL_SOURCE}' specified for HDF5_VOL_PATH${vol_idx_fixed}")
        endif()
      endif ()

      math (EXPR hdf5_vol_name_pos "${hdf5_vol_name_pos}+1")

      string (SUBSTRING "${HDF5_VOL_SOURCE}" ${hdf5_vol_name_pos} -1 hdf5_vol_name)
      string (REPLACE ".git" "" hdf5_vol_name "${hdf5_vol_name}")
      string (STRIP "${hdf5_vol_name}" hdf5_vol_name)
      string (TOUPPER "${hdf5_vol_name}" hdf5_vol_name_upper)
      string (TOLOWER "${hdf5_vol_name}" hdf5_vol_name_lower)
    endif()
  endmacro()