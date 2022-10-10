cmake_minimum_required (VERSION 3.18)
project (HDF5_TEST C)

#################################################################################
#  Test program sources
#################################################################################

macro (ADD_H5_EXE file)
  add_executable (${file} ${HDF5_TEST_SOURCE_DIR}/${file}.c)
  target_include_directories (${file} PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};${HDF5_TEST_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  target_compile_options(${file} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  if (NOT BUILD_SHARED_LIBS)
    TARGET_C_PROPERTIES (${file} STATIC)
    target_link_libraries (${file} PRIVATE ${HDF5_TEST_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (${file} SHARED)
    target_link_libraries (${file} PRIVATE ${HDF5_TEST_LIBSH_TARGET})
  endif ()
  set_target_properties (${file} PROPERTIES FOLDER test)
endmacro ()

##############################################################################
###                S W I M M E R   T E S T  U T I L S                      ###
##############################################################################
set (H5_UTIL_TESTS)

if (HDF5_TEST_SWMR)
  set (H5_UTIL_TESTS ${H5_UTIL_TESTS} swmr_check_compat_vfd)
endif ()

if (H5_UTIL_TESTS)
  foreach (h5_test ${H5_UTIL_TESTS})
    ADD_H5_EXE(${h5_test})
  endforeach ()
endif ()
