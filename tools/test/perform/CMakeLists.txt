cmake_minimum_required (VERSION 3.12)
project (HDF5_TOOLS_TEST_PERFORM C)

# --------------------------------------------------------------------
# Add the executables
# --------------------------------------------------------------------

if (HDF5_BUILD_PERFORM_STANDALONE)
  #-- Adding test for h5perf_serial_alone - io_timer.c includes
  set (h5perf_serial_alone_SOURCES
      ${HDF5_TOOLS_DIR}/lib/io_timer.c
      ${HDF5_TOOLS_SRC_H5PERF_SOURCE_DIR}/sio_perf.c
      ${HDF5_TOOLS_SRC_H5PERF_SOURCE_DIR}/sio_engine.c
      ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/sio_standalone.c
  )
  add_executable (h5perf_serial_alone ${h5perf_serial_alone_SOURCES})
  target_include_directories (h5perf_serial_alone PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};${HDF5_TOOLS_DIR}/lib;${HDF5_TOOLS_SRC_H5PERF_SOURCE_DIR};${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  if (NOT BUILD_SHARED_LIBS)
    TARGET_C_PROPERTIES (h5perf_serial_alone STATIC)
    target_link_libraries (h5perf_serial_alone PRIVATE ${HDF5_LIB_TARGET} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>")
  else ()
    TARGET_C_PROPERTIES (h5perf_serial_alone SHARED)
    target_link_libraries (h5perf_serial_alone PRIVATE ${HDF5_LIBSH_TARGET} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>")
  endif ()
  set_target_properties (h5perf_serial_alone PROPERTIES FOLDER perform)
  set_property (TARGET h5perf_serial_alone APPEND PROPERTY COMPILE_DEFINITIONS STANDALONE)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TOOLS_TEST_PERFORM_h5perf_serial_alone_FORMAT h5perf_serial_alone)
  endif ()
endif ()

#-- Adding test for chunk
set (chunk_SOURCES
    ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/chunk.c
)
add_executable(chunk ${chunk_SOURCES})
target_include_directories (chunk PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (chunk STATIC)
  target_link_libraries (chunk PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (chunk SHARED)
  target_link_libraries (chunk PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (chunk PROPERTIES FOLDER perform)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TOOLS_TEST_PERFORM_chunk_FORMAT chunk)
endif ()

#-- Adding test for iopipe
set (iopipe_SOURCES
    ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/iopipe.c
)
add_executable (iopipe ${iopipe_SOURCES})
target_include_directories (iopipe PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (iopipe STATIC)
  target_link_libraries (iopipe PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (iopipe SHARED)
  target_link_libraries (iopipe PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (iopipe PROPERTIES FOLDER perform)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TOOLS_TEST_PERFORM_iopipe_FORMAT iopipe)
endif ()

#-- Adding test for chunk_cache
set (chunk_cache_SOURCES
    ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/chunk_cache.c
)
add_executable (chunk_cache ${chunk_cache_SOURCES})
target_include_directories (chunk_cache PRIVATE "${HDF5_TEST_SRC_DIR};${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (chunk_cache STATIC)
  target_link_libraries (chunk_cache PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (chunk_cache SHARED)
  target_link_libraries (chunk_cache PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (chunk_cache PROPERTIES FOLDER perform)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TOOLS_TEST_PERFORM_chunk_cache_FORMAT chunk_cache)
endif ()

#-- Adding test for overhead
set (overhead_SOURCES
    ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/overhead.c
)
add_executable (overhead ${overhead_SOURCES})
target_include_directories (overhead PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (overhead STATIC)
  target_link_libraries (overhead PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (overhead SHARED)
  target_link_libraries (overhead PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (overhead PROPERTIES FOLDER perform)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TOOLS_TEST_PERFORM_overhead_FORMAT overhead)
endif ()

#-- Adding test for perf_meta
set (perf_meta_SOURCES
    ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/perf_meta.c
)
add_executable (perf_meta ${perf_meta_SOURCES})
target_include_directories (perf_meta PRIVATE "${HDF5_TEST_SRC_DIR};${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (perf_meta STATIC)
  target_link_libraries (perf_meta PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (perf_meta SHARED)
  target_link_libraries (perf_meta PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (perf_meta PROPERTIES FOLDER perform)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TOOLS_TEST_PERFORM_perf_meta_FORMAT perf_meta)
endif ()

#-- Adding test for zip_perf
set (zip_perf_SOURCES
    ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/zip_perf.c
)
add_executable (zip_perf ${zip_perf_SOURCES})
target_include_directories (zip_perf PRIVATE "${HDF5_TEST_SRC_DIR};${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (zip_perf STATIC)
  target_link_libraries (zip_perf PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET} ${LINK_COMP_LIBS})
else ()
  TARGET_C_PROPERTIES (zip_perf SHARED)
  target_link_libraries (zip_perf PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} ${LINK_COMP_LIBS})
endif ()
set_target_properties (zip_perf PROPERTIES FOLDER perform)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TOOLS_TEST_PERFORM_zip_perf_FORMAT zip_perf)
endif ()

if (H5_HAVE_PARALLEL AND HDF5_TEST_PARALLEL)
  if (HDF5_BUILD_PERFORM_STANDALONE)
    #-- Adding test for h5perf
    set (h5perf_alone_SOURCES
        ${HDF5_TOOLS_DIR}/lib/io_timer.c
        ${HDF5_TOOLS_SRC_H5PERF_SOURCE_DIR}/pio_perf.c
        ${HDF5_TOOLS_SRC_H5PERF_SOURCE_DIR}/pio_engine.c
        ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/pio_standalone.c
    )
    add_executable (h5perf_alone ${h5perf_alone_SOURCES})
    target_include_directories (h5perf_alone PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};${HDF5_TOOLS_DIR}/lib;${HDF5_TOOLS_SRC_H5PERF_SOURCE_DIR};${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    if (NOT BUILD_SHARED_LIBS)
      TARGET_C_PROPERTIES (h5perf_alone STATIC)
      target_link_libraries (h5perf_alone PRIVATE ${HDF5_LIB_TARGET} ${LINK_LIBS} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>")
    else ()
      TARGET_C_PROPERTIES (h5perf_alone SHARED)
      target_link_libraries (h5perf_alone PRIVATE ${HDF5_LIBSH_TARGET} ${LINK_LIBS} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>")
    endif ()
    set_target_properties (h5perf_alone PROPERTIES FOLDER perform)
    set_property (TARGET h5perf_alone
        APPEND PROPERTY COMPILE_DEFINITIONS STANDALONE
    )

    #-----------------------------------------------------------------------------
    # Add Target to clang-format
    #-----------------------------------------------------------------------------
    if (HDF5_ENABLE_FORMATTERS)
      clang_format (HDF5_TOOLS_TEST_PERFORM_h5perf_alone_FORMAT h5perf_alone)
    endif ()
  endif ()
endif ()

if (HDF5_TEST_TOOLS)
  include (CMakeTests.cmake)
endif ()
