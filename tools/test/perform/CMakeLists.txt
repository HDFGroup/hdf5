cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_TEST_PERFORM )

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
add_definitions (${HDF_EXTRA_C_FLAGS})

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TEST_SRC_DIR})
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib )

# --------------------------------------------------------------------
# Add the executables
# --------------------------------------------------------------------
#-- Adding test for h5perf_serial
set (h5perf_serial_SOURCES
    ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/sio_perf.c
    ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/sio_engine.c
)
add_executable (h5perf_serial ${h5perf_serial_SOURCES})
TARGET_NAMING (h5perf_serial STATIC)
TARGET_C_PROPERTIES (h5perf_serial STATIC " " " ")
target_link_libraries (h5perf_serial ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (h5perf_serial PROPERTIES FOLDER perform)

if (HDF5_BUILD_PERFORM_STANDALONE)
  #-- Adding test for h5perf_serial_alone
  set (h5perf_serial_alone_SOURCES
      ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/sio_perf.c
      ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/sio_engine.c
  )
  add_executable (h5perf_serial_alone ${h5perf_serial_alone_SOURCES})
  TARGET_NAMING (h5perf_serial_alone STATIC)
  TARGET_C_PROPERTIES (h5perf_serial_alone STATIC " " " ")
  target_link_libraries (h5perf_serial_alone ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (h5perf_serial_alone PROPERTIES FOLDER perform)
  set_property (TARGET h5perf_serial_alone
      APPEND PROPERTY COMPILE_DEFINITIONS STANDALONE
  )
endif ()

#-- Adding test for chunk
set (chunk_SOURCES
    ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/chunk.c
)
ADD_EXECUTABLE(chunk ${chunk_SOURCES})
TARGET_NAMING (chunk STATIC)
TARGET_C_PROPERTIES (chunk STATIC " " " ")
TARGET_LINK_LIBRARIES(chunk ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (chunk PROPERTIES FOLDER perform)

#-- Adding test for iopipe
set (iopipe_SOURCES
    ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/iopipe.c
)
add_executable (iopipe ${iopipe_SOURCES})
TARGET_NAMING (iopipe STATIC)
TARGET_C_PROPERTIES (iopipe STATIC " " " ")
target_link_libraries (iopipe ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (iopipe PROPERTIES FOLDER perform)

#-- Adding test for overhead
set (overhead_SOURCES
    ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/overhead.c
)
add_executable (overhead ${overhead_SOURCES})
TARGET_NAMING (overhead STATIC)
TARGET_C_PROPERTIES (overhead STATIC " " " ")
target_link_libraries (overhead ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (overhead PROPERTIES FOLDER perform)

#-- Adding test for perf_meta
  set (perf_meta_SOURCES
      ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/perf_meta.c
  )
  add_executable (perf_meta ${perf_meta_SOURCES})
  TARGET_NAMING (perf_meta STATIC)
  TARGET_C_PROPERTIES (perf_meta STATIC " " " ")
  target_link_libraries (perf_meta ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  set_target_properties (perf_meta PROPERTIES FOLDER perform)

#-- Adding test for zip_perf
set (zip_perf_SOURCES
    ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/zip_perf.c
)
add_executable (zip_perf ${zip_perf_SOURCES})
TARGET_NAMING (zip_perf STATIC)
TARGET_C_PROPERTIES (zip_perf STATIC " " " ")
target_link_libraries (zip_perf ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (zip_perf PROPERTIES FOLDER perform)

if (H5_HAVE_PARALLEL AND BUILD_TESTING)
  #-- Adding test for h5perf
  set (h5perf_SOURCES
      ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/pio_perf.c
      ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/pio_engine.c
  )
  add_executable (h5perf ${h5perf_SOURCES})
  TARGET_NAMING (h5perf STATIC)
  TARGET_C_PROPERTIES (h5perf STATIC " " " ")
  target_link_libraries (h5perf ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  set_target_properties (h5perf PROPERTIES FOLDER perform)

  if (HDF5_BUILD_PERFORM_STANDALONE)
    #-- Adding test for h5perf
    set (h5perf_alone_SOURCES
        ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/pio_perf.c
        ${HDF5_TOOLS_TEST_PERFORM_SOURCE_DIR}/pio_engine.c
    )
    add_executable (h5perf_alone ${h5perf_alone_SOURCES})
    TARGET_NAMING (h5perf_alone STATIC)
    TARGET_C_PROPERTIES (h5perf_alone STATIC " " " ")
    target_link_libraries (h5perf_alone ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
    set_target_properties (h5perf_alone PROPERTIES FOLDER perform)
    set_property (TARGET h5perf_alone
        APPEND PROPERTY COMPILE_DEFINITIONS STANDALONE
    )
  endif ()
endif ()

include (CMakeTests.cmake)
