cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_TOOLS_H5DIFF)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5diff executables
# --------------------------------------------------------------------
add_executable (h5diff
    ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/h5diff_common.c
    ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/h5diff_main.c
)
TARGET_NAMING (h5diff STATIC)
TARGET_C_PROPERTIES (h5diff STATIC " " " ")
target_link_libraries (h5diff  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5diff PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5diff")

set (H5_DEP_EXECUTABLES h5diff)

if (H5_HAVE_PARALLEL)
  add_executable (ph5diff
      ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/h5diff_common.c
      ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/ph5diff_main.c
  )
  TARGET_NAMING (ph5diff STATIC)
  TARGET_C_PROPERTIES (ph5diff STATIC " " " ")
  target_link_libraries (ph5diff  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (ph5diff PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};ph5diff")
endif (H5_HAVE_PARALLEL)

if (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5diff and test executables
  # --------------------------------------------------------------------
  if (HDF5_BUILD_GENERATORS)
    add_executable (h5diffgentest ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/h5diffgentest.c)
    TARGET_NAMING (h5diffgentest STATIC)
    TARGET_C_PROPERTIES (h5diffgentest STATIC " " " ")
    target_link_libraries (h5diffgentest ${HDF5_LIB_TARGET})
    set_target_properties (h5diffgentest PROPERTIES FOLDER generator/tools)

    #add_test (NAME h5diffgentest COMMAND $<TARGET_FILE:h5diffgentest>)
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

#INSTALL_PROGRAM_PDB (h5diff ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5diff
    EXPORT
        ${HDF5_EXPORTED_TARGETS}
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
)

if (H5_HAVE_PARALLEL)

  #INSTALL_PROGRAM_PDB (ph5diff ${HDF5_INSTALL_BIN_DIR} toolsapplications)

  install (
      TARGETS
          ph5diff
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
  )
endif (H5_HAVE_PARALLEL)
