cmake_minimum_required (VERSION 3.10)
project (HDF5_HL_TOOLS_GIF2H5 C)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
set (GIF2H5_SOURCES
    ${HDF5_HL_TOOLS_GIF2H5_SOURCE_DIR}/gif.h
    ${HDF5_HL_TOOLS_GIF2H5_SOURCE_DIR}/decompress.c
    ${HDF5_HL_TOOLS_GIF2H5_SOURCE_DIR}/gif2hdf.c
    ${HDF5_HL_TOOLS_GIF2H5_SOURCE_DIR}/gif2mem.c
    ${HDF5_HL_TOOLS_GIF2H5_SOURCE_DIR}/gifread.c
    ${HDF5_HL_TOOLS_GIF2H5_SOURCE_DIR}/writehdf.c
)

#-- Add gif2hdf5 program
if (NOT ONLY_SHARED_LIBS)
  add_executable (gif2h5 ${GIF2H5_SOURCES})
  target_include_directories (gif2h5 PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (gif2h5 STATIC)
  target_link_libraries (gif2h5 PRIVATE ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (gif2h5 PROPERTIES FOLDER tools/hl)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};gif2h5")
  set (H5_DEP_EXECUTABLES
      gif2h5
  )
endif ()

if (BUILD_SHARED_LIBS)
  add_executable (gif2h5-shared ${GIF2H5_SOURCES})
  target_include_directories (gif2h5-shared PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (gif2h5-shared SHARED)
  target_link_libraries (gif2h5-shared PRIVATE ${HDF5_HL_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} ${HDF5_TOOLS_LIBSH_TARGET})
  set_target_properties (gif2h5-shared PROPERTIES FOLDER tools/hl)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};gif2h5-shared")
  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES}
      gif2h5-shared
  )
endif ()

#-- Add h52gif program
set (hdf2gif_SOURCES
    ${HDF5_HL_TOOLS_GIF2H5_SOURCE_DIR}/hdf2gif.c
    ${HDF5_HL_TOOLS_GIF2H5_SOURCE_DIR}/hdfgifwr.c
)
if (NOT ONLY_SHARED_LIBS)
  add_executable (h52gif ${hdf2gif_SOURCES})
  target_include_directories (h52gif PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h52gif STATIC)
  target_link_libraries (h52gif PRIVATE ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (h52gif PROPERTIES FOLDER tools/hl)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h52gif")
  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES}
      h52gif
  )
endif ()

if (BUILD_SHARED_LIBS)
  add_executable (h52gif-shared ${hdf2gif_SOURCES})
  target_include_directories (h52gif-shared PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h52gif-shared SHARED)
  target_link_libraries (h52gif-shared PRIVATE ${HDF5_HL_LIBSH_TARGET} PRIVATE ${HDF5_LIBSH_TARGET} ${HDF5_TOOLS_LIBSH_TARGET})
  set_target_properties (h52gif-shared PROPERTIES FOLDER tools/hl)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h52gif-shared")
  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES}
      h52gif-shared
  )
endif ()

if (BUILD_TESTING)
  # --------------------------------------------------------------------
  # This executable can generate the actual test files - Currently not
  #  used in the CMake Build system as we rely on the test files that are
  #  shipped with HDF5 source archives
  # --------------------------------------------------------------------
  if (HDF5_BUILD_GENERATORS AND NOT ONLY_SHARED_LIBS)
      add_executable (hl_h52gifgentest ${HDF5_HL_TOOLS_GIF2H5_SOURCE_DIR}/h52gifgentst.c)
      target_include_directories (hl_h52gifgentest PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
      TARGET_C_PROPERTIES (hl_h52gifgentest STATIC)
      target_link_libraries (hl_h52gifgentest PRIVATE ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})
      set_target_properties (hl_h52gifgentest PROPERTIES FOLDER generator/tools/hl)

#      add_test (NAME hl_h52gifgentest COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:hl_h52gifgentest>)
  endif ()

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
