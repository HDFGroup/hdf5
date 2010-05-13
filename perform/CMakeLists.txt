cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_PERFORM )

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TEST_SOURCE_DIR})
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SOURCE_DIR}/lib )

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------

#-- Adding test for h5perf_serial
SET (h5perf_serial_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/sio_timer.c
    ${HDF5_PERFORM_SOURCE_DIR}/sio_perf.c
    ${HDF5_PERFORM_SOURCE_DIR}/sio_engine.c
)
ADD_EXECUTABLE (h5perf_serial ${h5perf_serial_SRCS})
H5_NAMING (h5perf_serial)
TARGET_LINK_LIBRARIES (h5perf_serial ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

ADD_TEST (NAME h5perf_serial COMMAND $<TARGET_FILE:h5perf_serial>)

#-- Adding test for chunk
SET (chunk_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/chunk.c
)
ADD_EXECUTABLE(chunk ${chunk_SRCS})
H5_NAMING (chunk)
TARGET_LINK_LIBRARIES(chunk ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

ADD_TEST (NAME chunk COMMAND $<TARGET_FILE:chunk>)

#-- Adding test for iopipe
SET (iopipe_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/iopipe.c
)
ADD_EXECUTABLE (iopipe ${iopipe_SRCS})
H5_NAMING (iopipe)
TARGET_LINK_LIBRARIES (iopipe ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

ADD_TEST (NAME iopipe COMMAND $<TARGET_FILE:iopipe>)

#-- Adding test for overhead
SET (overhead_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/overhead.c
)
ADD_EXECUTABLE (overhead ${overhead_SRCS})
H5_NAMING (overhead)
TARGET_LINK_LIBRARIES (overhead ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

ADD_TEST (NAME overhead COMMAND $<TARGET_FILE:overhead>)

#-- Adding test for perf_meta
SET (perf_meta_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/perf_meta.c
)
ADD_EXECUTABLE (perf_meta ${perf_meta_SRCS})
H5_NAMING (perf_meta)
TARGET_LINK_LIBRARIES (perf_meta ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})

ADD_TEST (NAME perf_meta COMMAND $<TARGET_FILE:perf_meta>)

#-- Adding test for zip_perf
SET (zip_perf_SRCS
    ${HDF5_PERFORM_SOURCE_DIR}/zip_perf.c
)
ADD_EXECUTABLE (zip_perf ${zip_perf_SRCS})
H5_NAMING (zip_perf)
TARGET_LINK_LIBRARIES (zip_perf ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

ADD_TEST (NAME zip_perf COMMAND $<TARGET_FILE:zip_perf> "-h")
