cmake_minimum_required (VERSION 3.18)
project (HDF5Examples_C C)

#-----------------------------------------------------------------------------
# Build the C Examples
#-----------------------------------------------------------------------------
add_subdirectory (${PROJECT_SOURCE_DIR}/TUTR)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5D)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5G)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5T)

if (${H5_LIBVER_DIR} GREATER 16)
#  add_subdirectory (${PROJECT_SOURCE_DIR}/Perf)
  if (USE_SHARED_LIBS AND H5EX_BUILD_FILTERS AND HDF5_PLUGIN_SUPPORT_ENABLED)
    add_subdirectory (${PROJECT_SOURCE_DIR}/H5FLT)
  endif ()
endif ()

if (${H5_LIBVER_DIR} GREATER 110)
  add_subdirectory (${PROJECT_SOURCE_DIR}/H5VDS)
endif ()

if (H5EX_ENABLE_PARALLEL AND H5_HAVE_PARALLEL AND HDF5_PARALLEL_ENABLED)
  add_subdirectory (${PROJECT_SOURCE_DIR}/H5PAR)
endif ()

#-- Add High Level Examples
if (H5EX_BUILD_HL AND HDF5_HL_LIB_ENABLED)
  add_subdirectory (HL)
endif ()

