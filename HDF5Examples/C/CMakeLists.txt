cmake_minimum_required (VERSION 3.26)
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
  if (USE_SHARED_LIBS AND H5EXAMPLE_BUILD_FILTERS AND HDF5_PROVIDES_PLUGIN_SUPPORT)
    add_subdirectory (${PROJECT_SOURCE_DIR}/H5FLT)
  endif ()
endif ()

if (${H5_LIBVER_DIR} GREATER 110)
  add_subdirectory (${PROJECT_SOURCE_DIR}/H5VDS)
endif ()

if (H5EXAMPLE_ENABLE_PARALLEL AND H5_HAVE_PARALLEL AND HDF5_PROVIDES_PARALLEL)
  add_subdirectory (${PROJECT_SOURCE_DIR}/H5PAR)
endif ()

#-- Add High Level Examples
if (H5EXAMPLE_BUILD_HL AND HDF5_PROVIDES_HL_LIB)
  add_subdirectory (HL)
endif ()

