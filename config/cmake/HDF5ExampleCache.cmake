# CMake cache file for external HDF5 filter plugins

#########################
# EXTERNAL cache entries
#########################

# examples are the tests for plugins
set (H5EX_BUILD_TESTING ON CACHE BOOL "Enable H5PL testing" FORCE)
set (H5EX_BUILD_EXAMPLES ${HDF5_BUILD_EXAMPLES} CACHE BOOL "Build H5PL Examples" FORCE)

#preset HDF5 cache vars to this projects libraries instead of searching
set (H5EX_HDF5_HEADER "H5pubconf.h" CACHE STRING "Name of HDF5 header" FORCE)
#set (H5EX_HDF5_INCLUDE_DIRS $<TARGET_PROPERTY:${HDF5_LIBSH_TARGET},INCLUDE_DIRECTORIES> CACHE PATH "HDF5 include dirs" FORCE)
set (H5EX_HDF5_INCLUDE_DIRS "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR}" CACHE PATH "HDF5 include dirs" FORCE)
set (H5EX_HDF5_DIR ${CMAKE_CURRENT_BINARY_DIR} CACHE STRING "HDF5 build folder" FORCE)

if (NOT BUILD_SHARED_LIBS AND BUILD_STATIC_LIBS)
  set (USE_SHARED_LIBS OFF CACHE BOOL "Use Shared Libraries for Examples" FORCE)
  set (H5EX_HDF5_LINK_LIBS ${HDF5_LIB_TARGET} CACHE STRING "HDF5 target" FORCE)
elseif (BUILD_SHARED_LIBS AND NOT BUILD_STATIC_LIBS)
  set (USE_SHARED_LIBS ON CACHE BOOL "Use Shared Libraries for Examples" FORCE)
  set (H5EX_HDF5_LINK_LIBS ${HDF5_LIBSH_TARGET} CACHE STRING "HDF5 target" FORCE)
endif ()

set (H5EX_HDF5_DUMP_EXECUTABLE $<TARGET_FILE:h5dump${tgt_file_ext}> CACHE STRING "HDF5 h5dump target" FORCE)
set (H5EX_HDF5_REPACK_EXECUTABLE $<TARGET_FILE:h5repack${tgt_file_ext}> CACHE STRING "HDF5 h5repack target" FORCE)
