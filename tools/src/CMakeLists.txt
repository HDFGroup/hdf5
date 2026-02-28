cmake_minimum_required (VERSION 3.26)
project (HDF5_TOOLS_SRC C)

#-----------------------------------------------------------------------------
# Option to build documentation
#-----------------------------------------------------------------------------
if (DOXYGEN_FOUND)
  # Contribute tool public headers to the main library documentation input
  # list. Headers are enumerated explicitly here, mirroring the explicit
  # source lists used elsewhere, so that adding a new tool header is tracked
  # by version control and does not require an explanation of why GLOB was
  # used. CMAKE_CURRENT_SOURCE_DIR is set to tools/src/ at this point.
  set_property (GLOBAL APPEND PROPERTY HDF5_DOXYGEN_LIB_INPUTS
    ${CMAKE_CURRENT_SOURCE_DIR}/h5copy/h5copy.h
    ${CMAKE_CURRENT_SOURCE_DIR}/h5diff/h5diff_common.h
    ${CMAKE_CURRENT_SOURCE_DIR}/h5diff/h5diff_main.h
    ${CMAKE_CURRENT_SOURCE_DIR}/h5dump/h5dump.h
    ${CMAKE_CURRENT_SOURCE_DIR}/h5dump/h5dump_ddl.h
    ${CMAKE_CURRENT_SOURCE_DIR}/h5dump/h5dump_defines.h
    ${CMAKE_CURRENT_SOURCE_DIR}/h5dump/h5dump_extern.h
    ${CMAKE_CURRENT_SOURCE_DIR}/h5dump/h5dump_xml.h
    ${CMAKE_CURRENT_SOURCE_DIR}/h5format_convert/h5format_convert.h
    ${CMAKE_CURRENT_SOURCE_DIR}/h5import/h5import.h
    ${CMAKE_CURRENT_SOURCE_DIR}/h5jam/h5jam.h
    ${CMAKE_CURRENT_SOURCE_DIR}/h5ls/h5ls.h
    ${CMAKE_CURRENT_SOURCE_DIR}/h5perf/pio_perf.h
    ${CMAKE_CURRENT_SOURCE_DIR}/h5perf/sio_perf.h
    ${CMAKE_CURRENT_SOURCE_DIR}/h5repack/h5repack.h
    ${CMAKE_CURRENT_SOURCE_DIR}/h5stat/h5stat.h
    ${CMAKE_CURRENT_SOURCE_DIR}/misc/h5clear.h
    ${CMAKE_CURRENT_SOURCE_DIR}/misc/h5debug.h
    ${CMAKE_CURRENT_SOURCE_DIR}/misc/h5delete.h
    ${CMAKE_CURRENT_SOURCE_DIR}/misc/h5mkgrp.h
    ${CMAKE_CURRENT_SOURCE_DIR}/misc/h5repart.h
  )
endif ()

#-- Add the h5diff and test executables
add_subdirectory (h5diff)

#-- Add the h5ls executable
add_subdirectory (h5ls)

#-- Misc Executables
add_subdirectory (misc)

#-- Add the h5import and test executables
add_subdirectory (h5import)

#-- h5repack executables
add_subdirectory (h5repack)

#-- Add the h5dump and test executables
add_subdirectory (h5jam)

#-- Add the h5copy and test executables
add_subdirectory (h5copy)

#-- Add the h5stat and test executables
add_subdirectory (h5stat)

#-- Add the h5dump and test executables
add_subdirectory (h5dump)

#-- Add the h5format_convert and test executables
add_subdirectory (h5format_convert)

#-- h5perf executables
add_subdirectory (h5perf)
