cmake_minimum_required (VERSION 3.10)
project (HDF5_HL_TOOLS_H5WATCH C)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
set (H5WATCH_SOURCES
    ${HDF5_HL_TOOLS_H5WATCH_SOURCE_DIR}/h5watch.c
)

#-- Add h5watch program
if (NOT ONLY_SHARED_LIBS)
  add_executable (h5watch ${H5WATCH_SOURCES})
  target_include_directories (h5watch PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5watch STATIC)
  target_link_libraries (h5watch PRIVATE ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (h5watch PROPERTIES FOLDER tools/hl)
  set (H5_DEP_EXECUTABLES h5watch)
endif ()
if (BUILD_SHARED_LIBS)
  add_executable (h5watch-shared ${H5WATCH_SOURCES})
  target_include_directories (h5watch-shared PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5watch-shared SHARED)
  target_link_libraries (h5watch-shared PRIVATE ${HDF5_HL_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} ${HDF5_TOOLS_LIBSH_TARGET})
  set_target_properties (h5watch-shared PROPERTIES FOLDER tools/hl)
  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} h5watch-shared)
endif ()

if (BUILD_TESTING)
  #-- Add swmr_check_compat_vfd program
  set (hl_swmr_check_compat_vfd_SOURCES
      ${HDF5_HL_TOOLS_H5WATCH_SOURCE_DIR}/swmr_check_compat_vfd.c
  )
  add_executable (hl_swmr_check_compat_vfd ${hl_swmr_check_compat_vfd_SOURCES})
  target_include_directories (hl_swmr_check_compat_vfd PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  if (NOT ONLY_SHARED_LIBS)
    TARGET_C_PROPERTIES (hl_swmr_check_compat_vfd STATIC)
    target_link_libraries (hl_swmr_check_compat_vfd PRIVATE ${HDF5_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (hl_swmr_check_compat_vfd SHARED)
    target_link_libraries (hl_swmr_check_compat_vfd PRIVATE ${HDF5_LIBSH_TARGET})
  endif ()
  set_target_properties (hl_swmr_check_compat_vfd PROPERTIES FOLDER tools/hl)

  #-- Add extend_dset program
  set (extend_dset_SOURCES
      ${HDF5_HL_TOOLS_H5WATCH_SOURCE_DIR}/extend_dset.c
  )
  add_executable (extend_dset ${extend_dset_SOURCES})
  target_include_directories (extend_dset PRIVATE "${HDF5_HL_SRC_DIR}/test;${HDF5_HL_SRC_DIR}/src;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  if (NOT ONLY_SHARED_LIBS)
    TARGET_C_PROPERTIES (extend_dset STATIC)
    target_link_libraries (extend_dset PRIVATE ${HDF5_HL_LIB_TARGET} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (extend_dset SHARED)
    target_link_libraries (extend_dset PRIVATE ${HDF5_HL_LIBSH_TARGET} ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} ${HDF5_TOOLS_LIBSH_TARGET})
  endif ()
  set_target_properties (extend_dset PROPERTIES FOLDER tools/hl)

  add_executable (h5watchgentest ${HDF5_HL_TOOLS_H5WATCH_SOURCE_DIR}/h5watchgentest.c)
  target_include_directories (h5watchgentest PRIVATE "${HDF5_HL_SRC_DIR}/src;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  if (NOT ONLY_SHARED_LIBS)
    TARGET_C_PROPERTIES (h5watchgentest STATIC)
    target_link_libraries (h5watchgentest PRIVATE ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (h5watchgentest SHARED)
    target_link_libraries (h5watchgentest PRIVATE ${HDF5_HL_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  endif ()
  set_target_properties (h5watchgentest PROPERTIES FOLDER generator/tools/hl)

  include (CMakeTests.cmake)
endif ()

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
if (HDF5_EXPORTED_TARGETS)
  install (
      TARGETS
          ${H5_DEP_EXECUTABLES}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT hltoolsapplications
  )
endif ()
