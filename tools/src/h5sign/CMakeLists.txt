cmake_minimum_required (VERSION 3.26)
project (HDF5_TOOLS_SRC_H5SIGN C)

#-----------------------------------------------------------------------------
# CMake configuration for HDF5 h5sign tool
# This file sets up the build, formatting, and installation rules for the h5sign executable.
# h5sign is used to sign HDF5 plugin files with RSA digital signatures.
#-----------------------------------------------------------------------------

# --------------------------------------------------------------------
# Find OpenSSL (required for RSA signing)
# --------------------------------------------------------------------
find_package(OpenSSL REQUIRED)

# --------------------------------------------------------------------
# Add the h5sign executable
# --------------------------------------------------------------------
add_executable (h5sign ${HDF5_TOOLS_SRC_H5SIGN_SOURCE_DIR}/h5sign.c)

# Include directories: tools library, HDF5 source, and OpenSSL
target_include_directories (h5sign
  PRIVATE
    "${HDF5_TOOLS_ROOT_DIR}/lib"
    "${HDF5_SRC_INCLUDE_DIRS}"
    "${HDF5_SRC_BINARY_DIR}"
    "${HDF5_SRC_DIR}"  # For H5PLsig.h
    $<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>
)

# Link against HDF5 tools library, HDF5 library, and OpenSSL
if (HDF5_BUILD_STATIC_TOOLS)
  TARGET_C_PROPERTIES (h5sign STATIC)
  target_link_libraries (h5sign
    PRIVATE
      ${HDF5_TOOLS_LIB_TARGET}
      ${HDF5_LIB_TARGET}
      OpenSSL::Crypto
  )
else ()
  TARGET_C_PROPERTIES (h5sign SHARED)
  target_link_libraries (h5sign
    PRIVATE
      ${HDF5_TOOLS_LIBSH_TARGET}
      ${HDF5_LIBSH_TARGET}
      OpenSSL::Crypto
  )
endif ()

set_target_properties (h5sign PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5sign")

set (H5_DEP_EXECUTABLES h5sign)

# --------------------------------------------------------------------
# Add Target to clang-format
# --------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_H5SIGN_SRC_FORMAT h5sign)
endif ()

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

# --------------------------------------------------------------------
# Rules for Installation of tools using make Install target
# --------------------------------------------------------------------
if (HDF5_EXPORTED_TARGETS)
  foreach (exec ${H5_DEP_EXECUTABLES})
    INSTALL_PROGRAM_PDB (${exec} ${HDF5_INSTALL_BIN_DIR} toolsapplications)
  endforeach ()

  install (
      TARGETS
          ${H5_DEP_EXECUTABLES}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
  )
endif ()
