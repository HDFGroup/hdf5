cmake_minimum_required (VERSION 3.12)
project (HDF5Examples_C C)

#-----------------------------------------------------------------------------
# Build the C Examples
#-----------------------------------------------------------------------------
add_subdirectory (${PROJECT_SOURCE_DIR}/H5D)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5G)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5T)

if (${H5_LIBVER_DIR} GREATER 16)
#  add_subdirectory (${PROJECT_SOURCE_DIR}/Perf)
  if (USE_SHARED_LIBS AND HDF_BUILD_FILTERS AND HDF5_ENABLE_PLUGIN_SUPPORT)
    add_subdirectory (${PROJECT_SOURCE_DIR}/H5FLT)
  endif ()
endif ()

if (${H5_LIBVER_DIR} GREATER 110)
  add_subdirectory (${PROJECT_SOURCE_DIR}/H5VDS)
endif ()

if (H5_HAVE_PARALLEL AND HDF5_ENABLE_PARALLEL)
  add_subdirectory (${PROJECT_SOURCE_DIR}/H5PAR)
endif ()
