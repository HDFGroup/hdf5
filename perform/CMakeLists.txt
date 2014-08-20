cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5_PERFORM )

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
add_definitions (${HDF_EXTRA_C_FLAGS})

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TEST_SRC_DIR})
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib )

# --------------------------------------------------------------------
# Add the executables
# --------------------------------------------------------------------
#-- Adding test for h5perf_serial
set (h5perf_serial_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/sio_timer.c
    ${HDF5_PERFORM_SOURCE_DIR}/sio_perf.c
    ${HDF5_PERFORM_SOURCE_DIR}/sio_engine.c
)
add_executable (h5perf_serial ${h5perf_serial_SRCS})
TARGET_NAMING (h5perf_serial ${LIB_TYPE})
TARGET_C_PROPERTIES (h5perf_serial " " " ")
target_link_libraries (h5perf_serial ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (h5perf_serial PROPERTIES FOLDER perform)

if (HDF5_BUILD_PERFORM_STANDALONE)
  #-- Adding test for h5perf_serial_alone
  set (h5perf_serial_alone_SRCS
      ${HDF5_PERFORM_SOURCE_DIR}/sio_timer.c
      ${HDF5_PERFORM_SOURCE_DIR}/sio_perf.c
      ${HDF5_PERFORM_SOURCE_DIR}/sio_engine.c
  )
  add_executable (h5perf_serial_alone ${h5perf_serial_alone_SRCS})
  set_property (TARGET h5perf_serial_alone 
      APPEND PROPERTY COMPILE_DEFINITIONS STANDALONE
  )
  TARGET_NAMING (h5perf_serial_alone ${LIB_TYPE})
  TARGET_C_PROPERTIES (h5perf_serial_alone " " " ")
  target_link_libraries (h5perf_serial_alone ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (h5perf_serial_alone PROPERTIES FOLDER perform)
endif (HDF5_BUILD_PERFORM_STANDALONE)

#-- Adding test for chunk
set (chunk_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/chunk.c
)
ADD_EXECUTABLE(chunk ${chunk_SRCS})
TARGET_NAMING (chunk ${LIB_TYPE})
TARGET_C_PROPERTIES (chunk " " " ")
TARGET_LINK_LIBRARIES(chunk ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (chunk PROPERTIES FOLDER perform)

#-- Adding test for iopipe
set (iopipe_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/iopipe.c
)
add_executable (iopipe ${iopipe_SRCS})
TARGET_NAMING (iopipe ${LIB_TYPE})
TARGET_C_PROPERTIES (iopipe " " " ")
target_link_libraries (iopipe ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (iopipe PROPERTIES FOLDER perform)

#-- Adding test for overhead
set (overhead_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/overhead.c
)
add_executable (overhead ${overhead_SRCS})
TARGET_NAMING (overhead ${LIB_TYPE})
TARGET_C_PROPERTIES (overhead " " " ")
target_link_libraries (overhead ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (overhead PROPERTIES FOLDER perform)

#-- Adding test for perf_meta
set (perf_meta_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/perf_meta.c
)
add_executable (perf_meta ${perf_meta_SRCS})
TARGET_NAMING (perf_meta ${LIB_TYPE})
TARGET_C_PROPERTIES (perf_meta " " " ")
target_link_libraries (perf_meta ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
set_target_properties (perf_meta PROPERTIES FOLDER perform)

#-- Adding test for zip_perf
set (zip_perf_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/zip_perf.c
)
add_executable (zip_perf ${zip_perf_SRCS})
TARGET_NAMING (zip_perf ${LIB_TYPE})
TARGET_C_PROPERTIES (zip_perf " " " ")
target_link_libraries (zip_perf ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (zip_perf PROPERTIES FOLDER perform)

if (H5_HAVE_PARALLEL)
  #-- Adding test for h5perf
  set (h5perf_SRCS
      ${HDF5_PERFORM_SOURCE_DIR}/pio_timer.c
      ${HDF5_PERFORM_SOURCE_DIR}/pio_perf.c
      ${HDF5_PERFORM_SOURCE_DIR}/pio_engine.c
  )
  add_executable (h5perf ${h5perf_SRCS})
  TARGET_NAMING (h5perf ${LIB_TYPE})
  TARGET_C_PROPERTIES (h5perf " " " ")
  target_link_libraries (h5perf ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  set_target_properties (h5perf PROPERTIES FOLDER perform)

  if (HDF5_BUILD_PERFORM_STANDALONE)
    #-- Adding test for h5perf
    set (h5perf_alone_SRCS
        ${HDF5_PERFORM_SOURCE_DIR}/pio_timer.c
        ${HDF5_PERFORM_SOURCE_DIR}/pio_perf.c
        ${HDF5_PERFORM_SOURCE_DIR}/pio_engine.c
    )
    add_executable (h5perf_alone ${h5perf_alone_SRCS})
    set_property (TARGET h5perf_alone 
        APPEND PROPERTY COMPILE_DEFINITIONS STANDALONE
    )
    TARGET_NAMING (h5perf_alone ${LIB_TYPE})
    TARGET_C_PROPERTIES (h5perf_alone " " " ")
    target_link_libraries (h5perf_alone ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
    set_target_properties (h5perf_alone PROPERTIES FOLDER perform)
  endif (HDF5_BUILD_PERFORM_STANDALONE)

  if (HDF5_BUILD_PARALLEL_ALL)
    #-- Adding test for benchpar
    set (benchpar_SRCS
        ${HDF5_PERFORM_SOURCE_DIR}/benchpar.c
    )
    add_executable (benchpar ${benchpar_SRCS})
    TARGET_NAMING (benchpar ${LIB_TYPE})
    TARGET_C_PROPERTIES (benchpar " " " ")
    target_link_libraries (benchpar ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
    set_target_properties (benchpar PROPERTIES FOLDER perform)
  endif (HDF5_BUILD_PARALLEL_ALL)
endif (H5_HAVE_PARALLEL)

include (CMakeTests.cmake)
