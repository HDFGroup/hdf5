cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_SRC_H5DIFF)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5diff executables
# --------------------------------------------------------------------
add_executable (h5diff
    ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/h5diff_common.c
    ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/h5diff_main.c
)
TARGET_NAMING (h5diff STATIC)
TARGET_C_PROPERTIES (h5diff STATIC " " " ")
target_link_libraries (h5diff  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5diff PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5diff")

set (H5_DEP_EXECUTABLES h5diff)

if (BUILD_SHARED_LIBS)
  add_executable (h5diff-shared
      ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/h5diff_common.c
      ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/h5diff_main.c
  )
  TARGET_NAMING (h5diff-shared SHARED)
  TARGET_C_PROPERTIES (h5diff-shared SHARED " " " ")
  target_link_libraries (h5diff-shared  ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_target_properties (h5diff-shared PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5diff-shared")

  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} h5diff-shared)
endif ()

if (H5_HAVE_PARALLEL)
  add_executable (ph5diff
      ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/h5diff_common.c
      ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/ph5diff_main.c
  )
  TARGET_NAMING (ph5diff STATIC)
  TARGET_C_PROPERTIES (ph5diff STATIC " " " ")
  target_link_libraries (ph5diff  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (ph5diff PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};ph5diff")
endif ()

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------
if (HDF5_EXPORTED_TARGETS)
  if (BUILD_SHARED_LIBS)
    INSTALL_PROGRAM_PDB (h5diff-shared ${HDF5_INSTALL_BIN_DIR} toolsapplications)
  endif ()
  INSTALL_PROGRAM_PDB (h5diff ${HDF5_INSTALL_BIN_DIR} toolsapplications)

  install (
      TARGETS
          ${H5_DEP_EXECUTABLES}
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
  endif ()
endif ()
