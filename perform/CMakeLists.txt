cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_PERFORM )

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
ADD_DEFINITIONS (${HDF5_EXTRA_C_FLAGS})

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TEST_SRC_DIR})
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib )

# --------------------------------------------------------------------
# Add the executables
# --------------------------------------------------------------------
#-- Adding test for h5perf_serial
SET (h5perf_serial_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/sio_timer.c
    ${HDF5_PERFORM_SOURCE_DIR}/sio_perf.c
    ${HDF5_PERFORM_SOURCE_DIR}/sio_engine.c
)
ADD_EXECUTABLE (h5perf_serial ${h5perf_serial_SRCS})
TARGET_NAMING (h5perf_serial ${LIB_TYPE})
TARGET_C_PROPERTIES (h5perf_serial " " " ")
TARGET_LINK_LIBRARIES (h5perf_serial ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
SET_TARGET_PROPERTIES (h5perf_serial PROPERTIES FOLDER perform)

IF (HDF5_BUILD_PERFORM_STANDALONE)
  #-- Adding test for h5perf_serial_alone
  SET (h5perf_serial_alone_SRCS
      ${HDF5_PERFORM_SOURCE_DIR}/sio_timer.c
      ${HDF5_PERFORM_SOURCE_DIR}/sio_perf.c
      ${HDF5_PERFORM_SOURCE_DIR}/sio_engine.c
  )
  ADD_EXECUTABLE (h5perf_serial_alone ${h5perf_serial_alone_SRCS})
  SET_PROPERTY (TARGET h5perf_serial_alone 
      APPEND PROPERTY COMPILE_DEFINITIONS STANDALONE
  )
  TARGET_NAMING (h5perf_serial_alone ${LIB_TYPE})
  TARGET_C_PROPERTIES (h5perf_serial_alone " " " ")
  TARGET_LINK_LIBRARIES (h5perf_serial_alone ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  SET_TARGET_PROPERTIES (h5perf_serial_alone PROPERTIES FOLDER perform)
ENDIF (HDF5_BUILD_PERFORM_STANDALONE)

#-- Adding test for chunk
SET (chunk_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/chunk.c
)
ADD_EXECUTABLE(chunk ${chunk_SRCS})
TARGET_NAMING (chunk ${LIB_TYPE})
TARGET_C_PROPERTIES (chunk " " " ")
TARGET_LINK_LIBRARIES(chunk ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
SET_TARGET_PROPERTIES (chunk PROPERTIES FOLDER perform)

#-- Adding test for iopipe
SET (iopipe_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/iopipe.c
)
ADD_EXECUTABLE (iopipe ${iopipe_SRCS})
TARGET_NAMING (iopipe ${LIB_TYPE})
TARGET_C_PROPERTIES (iopipe " " " ")
TARGET_LINK_LIBRARIES (iopipe ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
SET_TARGET_PROPERTIES (iopipe PROPERTIES FOLDER perform)

#-- Adding test for overhead
SET (overhead_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/overhead.c
)
ADD_EXECUTABLE (overhead ${overhead_SRCS})
TARGET_NAMING (overhead ${LIB_TYPE})
TARGET_C_PROPERTIES (overhead " " " ")
TARGET_LINK_LIBRARIES (overhead ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
SET_TARGET_PROPERTIES (overhead PROPERTIES FOLDER perform)

#-- Adding test for perf_meta
SET (perf_meta_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/perf_meta.c
)
ADD_EXECUTABLE (perf_meta ${perf_meta_SRCS})
TARGET_NAMING (perf_meta ${LIB_TYPE})
TARGET_C_PROPERTIES (perf_meta " " " ")
TARGET_LINK_LIBRARIES (perf_meta ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
SET_TARGET_PROPERTIES (perf_meta PROPERTIES FOLDER perform)

#-- Adding test for zip_perf
SET (zip_perf_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/zip_perf.c
)
ADD_EXECUTABLE (zip_perf ${zip_perf_SRCS})
TARGET_NAMING (zip_perf ${LIB_TYPE})
TARGET_C_PROPERTIES (zip_perf " " " ")
TARGET_LINK_LIBRARIES (zip_perf ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_TARGET_PROPERTIES (zip_perf PROPERTIES FOLDER perform)

IF (H5_HAVE_PARALLEL)
  #-- Adding test for h5perf
  SET (h5perf_SRCS
      ${HDF5_PERFORM_SOURCE_DIR}/pio_timer.c
      ${HDF5_PERFORM_SOURCE_DIR}/pio_perf.c
      ${HDF5_PERFORM_SOURCE_DIR}/pio_engine.c
  )
  ADD_EXECUTABLE (h5perf ${h5perf_SRCS})
  TARGET_NAMING (h5perf ${LIB_TYPE})
  TARGET_C_PROPERTIES (h5perf " " " ")
  TARGET_LINK_LIBRARIES (h5perf ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  SET_TARGET_PROPERTIES (h5perf PROPERTIES FOLDER perform)

  IF (HDF5_BUILD_PERFORM_STANDALONE)
    #-- Adding test for h5perf
    SET (h5perf_alone_SRCS
        ${HDF5_PERFORM_SOURCE_DIR}/pio_timer.c
        ${HDF5_PERFORM_SOURCE_DIR}/pio_perf.c
        ${HDF5_PERFORM_SOURCE_DIR}/pio_engine.c
    )
    ADD_EXECUTABLE (h5perf_alone ${h5perf_alone_SRCS})
    SET_PROPERTY (TARGET h5perf_alone 
        APPEND PROPERTY COMPILE_DEFINITIONS STANDALONE
    )
    TARGET_NAMING (h5perf_alone ${LIB_TYPE})
    TARGET_C_PROPERTIES (h5perf_alone " " " ")
    TARGET_LINK_LIBRARIES (h5perf_alone ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
    SET_TARGET_PROPERTIES (h5perf_alone PROPERTIES FOLDER perform)
  ENDIF (HDF5_BUILD_PERFORM_STANDALONE)

  IF (HDF5_BUILD_PARALLEL_ALL)
    #-- Adding test for benchpar
    SET (benchpar_SRCS
        ${HDF5_PERFORM_SOURCE_DIR}/benchpar.c
    )
    ADD_EXECUTABLE (benchpar ${benchpar_SRCS})
    TARGET_NAMING (benchpar ${LIB_TYPE})
    TARGET_C_PROPERTIES (benchpar " " " ")
    TARGET_LINK_LIBRARIES (benchpar ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
    SET_TARGET_PROPERTIES (benchpar PROPERTIES FOLDER perform)
  ENDIF (HDF5_BUILD_PARALLEL_ALL)
ENDIF (H5_HAVE_PARALLEL)

INCLUDE (CMakeTests.cmake)
