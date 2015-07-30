cmake_minimum_required (VERSION 3.1.0)
PROJECT(HDF5_HL_F90_SRC C CXX Fortran)

#-----------------------------------------------------------------------------
# configure def file for shared libs on windows
if (WIN32)
  if (BUILD_SHARED_LIBS)
    if (MSVC)
      configure_file (${HDF5_HL_F90_SRC_SOURCE_DIR}/hdf5_hl_fortrandll.def.in ${HDF5_HL_F90_SRC_BINARY_DIR}/hdf5_hl_fortrandll.def @ONLY)
    endif (MSVC)
  endif (BUILD_SHARED_LIBS)
endif (WIN32)

#-----------------------------------------------------------------------------
# Generate the H5LT and H5TB REAL APIs
#-----------------------------------------------------------------------------

add_executable (H5HL_buildiface
      ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5HL_buildiface.F90
  )

if (WIN32 AND MSVC)
  if (BUILD_SHARED_LIBS)
    set_target_properties (H5HL_buildiface
        PROPERTIES
            COMPILE_FLAGS "/MT"
    )
  endif (BUILD_SHARED_LIBS)
  set_target_properties (H5HL_buildiface
      PROPERTIES
          LINK_FLAGS "/SUBSYSTEM:CONSOLE"
  )
endif (WIN32 AND MSVC)
set_target_properties (H5HL_buildiface PROPERTIES
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (
    ${HDF5_HL_SRC_DIR}/src
    ${HDF5_F90_SRC_DIR}/src
    ${HDF5_F90_BINARY_DIR}
    ${CMAKE_Fortran_MODULE_DIRECTORY}
)

#-----------------------------------------------------------------------------
# hl_f90CStub lib
#-----------------------------------------------------------------------------
set (HDF5_HL_F90_C_SRCS
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5DSfc.c
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5LTfc.c
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5IMfc.c
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5IMcc.c
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5TBfc.c
)
set_source_files_properties (${HDF5_HL_F90_C_SRCS} PROPERTIES LANGUAGE C)

set (HDF5_HL_F90_HEADERS ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5LTf90proto.h)

add_library (${HDF5_HL_F90_C_LIB_TARGET} ${LIB_TYPE} ${HDF5_HL_F90_C_SRCS} ${HDF5_HL_F90_HEADERS})
TARGET_C_PROPERTIES (${HDF5_HL_F90_C_LIB_TARGET} ${LIB_TYPE} " " " ")
target_link_libraries (${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_F90_C_LIB_TARGET} ${HDF5_HL_LIB_TARGET})
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_C_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_HL_F90_C_LIB_NAME} ${LIB_TYPE})
set_target_properties (${HDF5_HL_F90_C_LIB_TARGET} PROPERTIES
    FOLDER libraries/hl/fortran
    LINKER_LANGUAGE C
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)

#-----------------------------------------------------------------------------
# Fortran Modules
#-----------------------------------------------------------------------------
set (HDF5_HL_F90_F_BASE_SRCS
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5DSff.F90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5TBff.F90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5LTff.F90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5IMff.F90
)
set (HDF5_HL_F90_F_SRCS
     ${HDF5_HL_F90_F_BASE_SRCS}

     # generated files
     ${HDF5_HL_F90_SRC_BINARY_DIR}/H5LTff_gen.F90
     ${HDF5_HL_F90_SRC_BINARY_DIR}/H5TBff_gen.F90
)

set_source_files_properties (${HDF5_HL_F90_F_SRCS} PROPERTIES LANGUAGE Fortran)

set_source_files_properties (
    ${HDF5_HL_F90_SRC_BINARY_DIR}/H5LTff_gen.F90
    ${HDF5_HL_F90_SRC_BINARY_DIR}/H5TBff_gen.F90
    PROPERTIES GENERATED TRUE
)

set (CMD $<TARGET_FILE:H5HL_buildiface>)
add_custom_target (H5HLgen ALL
    COMMAND ${CMD}
#v3.2    BYPRODUCT ${HDF5_HL_F90_SRC_BINARY_DIR}/H5LTff_gen.F90
#v3.2              ${HDF5_HL_F90_SRC_BINARY_DIR}/H5TBff_gen.F90
    WORKING_DIRECTORY ${HDF5_HL_F90_SRC_BINARY_DIR}
    DEPENDS ${HDF5_HL_F90_F_BASE_SRCS}
)

add_library (${HDF5_HL_F90_LIB_TARGET} ${LIB_TYPE} ${HDF5_HL_F90_F_SRCS})
set (SHARED_LINK_FLAGS " ")
if (WIN32)
  if (BUILD_SHARED_LIBS)
    set_property (TARGET ${HDF5_HL_F90_LIB_TARGET}
      APPEND PROPERTY COMPILE_DEFINITIONS
        BUILD_HDF5_HL_DLL
    )
    if (MSVC)
      set (SHARED_LINK_FLAGS "/DLL /DEF:${HDF5_HL_F90_SRC_BINARY_DIR}/hdf5_hl_fortrandll.def")
    endif (MSVC)
  endif (BUILD_SHARED_LIBS)
  set_property (TARGET ${HDF5_HL_F90_LIB_TARGET}
      APPEND PROPERTY COMPILE_DEFINITIONS
        HDF5F90_WINDOWS
  )
endif (WIN32)
TARGET_FORTRAN_PROPERTIES (${HDF5_HL_F90_LIB_TARGET} ${LIB_TYPE} " " ${SHARED_LINK_FLAGS})
target_link_libraries (${HDF5_HL_F90_LIB_TARGET} ${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_F90_LIB_TARGET})
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_HL_F90_LIB_TARGET} ${HDF5_HL_F90_LIB_NAME} ${LIB_TYPE})
set_target_properties (${HDF5_HL_F90_LIB_TARGET} PROPERTIES
    FOLDER libraries/hl/fortran
    LINKER_LANGUAGE Fortran
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------

if (WIN32)
  set (MOD_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE})
else (WIN32)
  set (MOD_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY})
endif (WIN32)

set (mod_files
    ${MOD_BUILD_DIR}/h5ds.mod
    ${MOD_BUILD_DIR}/h5tb.mod
    ${MOD_BUILD_DIR}/h5tb_const.mod
    ${MOD_BUILD_DIR}/h5lt.mod
    ${MOD_BUILD_DIR}/h5lt_const.mod
    ${MOD_BUILD_DIR}/h5im.mod
)

install (
    FILES
        ${mod_files}
    DESTINATION
        ${HDF5_INSTALL_INCLUDE_DIR}
    COMPONENT
        fortheaders
)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
if (HDF5_EXPORTED_TARGETS)
  if (BUILD_SHARED_LIBS)
    INSTALL_TARGET_PDB (${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_INSTALL_BIN_DIR} hlfortlibraries)
    #INSTALL_TARGET_PDB (${HDF5_HL_F90_LIB_TARGET} ${HDF5_INSTALL_BIN_DIR} hlfortlibraries)
  endif (BUILD_SHARED_LIBS)

  install (
      TARGETS
          ${HDF5_HL_F90_C_LIB_TARGET}
          ${HDF5_HL_F90_LIB_TARGET}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT hlfortlibraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT hlfortlibraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT hlfortlibraries
      FRAMEWORK DESTINATION ${HDF5_INSTALL_FWRK_DIR} COMPONENT hlfortlibraries
  )
endif (HDF5_EXPORTED_TARGETS)
