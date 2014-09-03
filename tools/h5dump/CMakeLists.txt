cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5_TOOLS_H5DUMP)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5dump executables
# --------------------------------------------------------------------
add_executable (h5dump
    ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump.c
    ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump_ddl.c
    ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump_xml.c
)
TARGET_NAMING (h5dump ${LIB_TYPE})
TARGET_C_PROPERTIES (h5dump " " " ")
target_link_libraries (h5dump  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5dump PROPERTIES FOLDER tools)

set (H5_DEP_EXECUTABLES h5dump)

if (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5dump test executable
  # --------------------------------------------------------------------
  if (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    add_executable (h5dumpgentest ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dumpgentest.c)
    TARGET_NAMING (h5dumpgentest ${LIB_TYPE})
    TARGET_C_PROPERTIES (h5dumpgentest " " " ")
    target_link_libraries (h5dumpgentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    set_target_properties (h5dumpgentest PROPERTIES FOLDER generator/tools)
    
    #add_test (NAME h5dumpgentest COMMAND $<TARGET_FILE:h5dumpgentest>)
  endif (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)

  include (CMakeTests.cmake)

  include (CMakeTestsPBITS.cmake)

  include (CMakeTestsXML.cmake)
endif (BUILD_TESTING)

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------

#INSTALL_PROGRAM_PDB (h5dump ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5dump
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}
    COMPONENT
        toolsapplications
)
