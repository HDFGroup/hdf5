cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_TOOLS_H5FC)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5format_convert executables
# --------------------------------------------------------------------
add_executable (h5format_convert ${HDF5_TOOLS_H5FC_SOURCE_DIR}/h5format_convert.c)
TARGET_NAMING (h5format_convert STATIC)
TARGET_C_PROPERTIES (h5format_convert STATIC " " " ")
target_link_libraries (h5format_convert  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5format_convert PROPERTIES FOLDER tools)

set (H5_DEP_EXECUTABLES h5format_convert)

if (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5format_convert test executables
  # --------------------------------------------------------------------
  add_executable (h5fc_chk_idx ${HDF5_TOOLS_H5FC_SOURCE_DIR}/h5fc_chk_idx.c)
  TARGET_NAMING (h5fc_chk_idx STATIC)
  TARGET_C_PROPERTIES (h5fc_chk_idx STATIC " " " ")
  target_link_libraries (h5fc_chk_idx ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (h5fc_chk_idx PROPERTIES FOLDER tools)

  if (HDF5_BUILD_GENERATORS)
    add_executable (h5fc_gentest ${HDF5_TOOLS_H5FC_SOURCE_DIR}/h5fc_gentest.c)
    TARGET_NAMING (h5fc_gentest STATIC)
    TARGET_C_PROPERTIES (h5fc_gentest STATIC " " " ")
    target_link_libraries (h5fc_gentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    set_target_properties (h5fc_gentest PROPERTIES FOLDER generator/tools)

    #add_test (NAME h5fc_gentest COMMAND $<TARGET_FILE:h5fc_gentest>)
  endif (HDF5_BUILD_GENERATORS)

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

#INSTALL_PROGRAM_PDB (h5format_convert ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5format_convert
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
)
