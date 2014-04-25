cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5_TOOLS_MISC)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)

# --------------------------------------------------------------------
# Add the misc executables
# --------------------------------------------------------------------
#-- Misc Executables
add_executable (h5debug ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5debug.c)
TARGET_NAMING (h5debug ${LIB_TYPE})
TARGET_C_PROPERTIES (h5debug " " " ")
target_link_libraries (h5debug ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (h5debug PROPERTIES FOLDER tools)

add_executable (h5repart ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5repart.c)
TARGET_NAMING (h5repart ${LIB_TYPE})
TARGET_C_PROPERTIES (h5repart " " " ")
target_link_libraries (h5repart ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (h5repart PROPERTIES FOLDER tools)

add_executable (h5mkgrp ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5mkgrp.c)
TARGET_NAMING (h5mkgrp ${LIB_TYPE})
TARGET_C_PROPERTIES (h5mkgrp " " " ")
target_link_libraries (h5mkgrp  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5mkgrp PROPERTIES FOLDER tools)

set (H5_DEP_EXECUTABLES
    h5debug
    h5repart
    h5mkgrp
)

#-----------------------------------------------------------------------------
# Generate the h5cc file containing settings needed to compile programs
#-----------------------------------------------------------------------------
#if (NOT WIN32)
#  configure_file (${HDF5_TOOLS_MISC_SOURCE_DIR}/h5cc.in ${HDF5_BINARY_DIR}/h5cc @ONLY)
#endif (NOT WIN32)

if (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the misc test executables
  # --------------------------------------------------------------------
  if (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    add_executable (h5repart_gentest ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5repart_gentest.c)
    TARGET_NAMING (h5repart_gentest ${LIB_TYPE})
    TARGET_C_PROPERTIES (h5repart_gentest " " " ")
    target_link_libraries (h5repart_gentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    set_target_properties (h5repart_gentest PROPERTIES FOLDER generator/tools)
    #add_test (NAME h5repart_gentest COMMAND $<TARGET_FILE:h5repart_gentest>)
  endif (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)

  add_executable (h5repart_test ${HDF5_TOOLS_MISC_SOURCE_DIR}/repart_test.c)
  TARGET_NAMING (h5repart_test ${LIB_TYPE})
  TARGET_C_PROPERTIES (h5repart_test " " " ")
  target_link_libraries (h5repart_test ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (h5repart_test PROPERTIES FOLDER tools)

  include (CMakeTests.cmake)
endif (BUILD_TESTING)

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------

#INSTALL_PROGRAM_PDB (h5debug ${HDF5_INSTALL_BIN_DIR} toolsapplications)
#INSTALL_PROGRAM_PDB (h5repart ${HDF5_INSTALL_BIN_DIR} toolsapplications)
#INSTALL_PROGRAM_PDB (h5mkgrp ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5debug h5repart h5mkgrp
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}
    COMPONENT
        toolsapplications
)
