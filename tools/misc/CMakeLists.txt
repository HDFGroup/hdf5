cmake_minimum_required (VERSION 3.1.0)
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
TARGET_NAMING (h5debug STATIC)
TARGET_C_PROPERTIES (h5debug STATIC " " " ")
target_link_libraries (h5debug ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (h5debug PROPERTIES FOLDER tools)

add_executable (h5repart ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5repart.c)
TARGET_NAMING (h5repart STATIC)
TARGET_C_PROPERTIES (h5repart STATIC " " " ")
target_link_libraries (h5repart ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (h5repart PROPERTIES FOLDER tools)

add_executable (h5mkgrp ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5mkgrp.c)
TARGET_NAMING (h5mkgrp STATIC)
TARGET_C_PROPERTIES (h5mkgrp STATIC " " " ")
target_link_libraries (h5mkgrp  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5mkgrp PROPERTIES FOLDER tools)

add_executable (h5clear ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5clear.c)
TARGET_NAMING (h5clear STATIC)
TARGET_C_PROPERTIES (h5clear STATIC " " " ")
target_link_libraries (h5clear  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5clear PROPERTIES FOLDER tools)

set (H5_DEP_EXECUTABLES
    h5debug
    h5repart
    h5mkgrp
    h5clear
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
  if (HDF5_BUILD_GENERATORS)
    add_executable (h5repart_gentest ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5repart_gentest.c)
    TARGET_NAMING (h5repart_gentest STATIC)
    TARGET_C_PROPERTIES (h5repart_gentest STATIC " " " ")
    target_link_libraries (h5repart_gentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    set_target_properties (h5repart_gentest PROPERTIES FOLDER generator/tools)
    #add_test (NAME h5repart_gentest COMMAND $<TARGET_FILE:h5repart_gentest>)

    add_subdirectory (${HDF5_TOOLS_MISC_SOURCE_DIR}/vds)

  endif (HDF5_BUILD_GENERATORS)

  add_executable (h5repart_test ${HDF5_TOOLS_MISC_SOURCE_DIR}/repart_test.c)
  TARGET_NAMING (h5repart_test STATIC)
  TARGET_C_PROPERTIES (h5repart_test STATIC " " " ")
  target_link_libraries (h5repart_test ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (h5repart_test PROPERTIES FOLDER tools)

  add_executable (h5clear_gentest ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5clear_gentest.c)
  TARGET_NAMING (h5clear_gentest STATIC)
  TARGET_C_PROPERTIES (h5clear_gentest STATIC " " " ")
  target_link_libraries (h5clear_gentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (h5clear_gentest PROPERTIES FOLDER tools)

  add_executable (clear_open_chk ${HDF5_TOOLS_MISC_SOURCE_DIR}/clear_open_chk.c)
  TARGET_NAMING (clear_open_chk STATIC)
  TARGET_C_PROPERTIES (clear_open_chk STATIC " " " ")
  target_link_libraries (clear_open_chk ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (clear_open_chk PROPERTIES FOLDER tools)

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
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
)
