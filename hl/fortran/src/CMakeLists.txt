cmake_minimum_required (VERSION 3.1.0)
PROJECT(HDF5_HL_F90_SRC C CXX Fortran)

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  file (MAKE_DIRECTORY "${HDF5_HL_F90_SRC_BINARY_DIR}/shared")
  if (WIN32)
    set (MODSH_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/shared/${CMAKE_BUILD_TYPE})
  else (WIN32)
    set (MODSH_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
  endif (WIN32)
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
if (WIN32)
  set (MOD_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/static/${CMAKE_BUILD_TYPE})
else (WIN32)
  set (MOD_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
endif (WIN32)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (
    ${HDF5_HL_SRC_DIR}/src
    ${HDF5_F90_SRC_DIR}/src
    ${HDF5_F90_BINARY_DIR}
    ${CMAKE_Fortran_MODULE_DIRECTORY}
    ${MOD_BUILD_DIR}
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

add_library (${HDF5_HL_F90_C_LIB_TARGET} STATIC ${HDF5_HL_F90_C_SRCS} ${HDF5_HL_F90_HEADERS})
TARGET_C_PROPERTIES (${HDF5_HL_F90_C_LIB_TARGET} STATIC " " " ")
target_link_libraries (${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_F90_C_LIB_TARGET} ${HDF5_HL_LIB_TARGET})
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_C_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_HL_F90_C_LIB_NAME} STATIC)
set_target_properties (${HDF5_HL_F90_C_LIB_TARGET} PROPERTIES
    FOLDER libraries/hl/fortran
    LINKER_LANGUAGE C
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)
set (install_targets ${HDF5_HL_F90_C_LIB_TARGET})

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_library (${HDF5_HL_F90_C_LIBSH_TARGET} SHARED ${HDF5_HL_F90_C_SRCS} ${HDF5_HL_F90_HEADERS})
  TARGET_C_PROPERTIES (${HDF5_HL_F90_C_LIBSH_TARGET} SHARED " " " ")
  target_link_libraries (${HDF5_HL_F90_C_LIBSH_TARGET} ${HDF5_F90_C_LIBSH_TARGET} ${HDF5_HL_LIBSH_TARGET})
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_C_LIBSH_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_HL_F90_C_LIBSH_TARGET} ${HDF5_HL_F90_C_LIB_NAME} SHARED ${HDF5_HL_F_PACKAGE_SOVERSION})
  set_target_properties (${HDF5_HL_F90_C_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/hl/fortran
      LINKER_LANGUAGE C
      COMPILE_DEFINITIONS "H5_BUILT_AS_DYNAMIC_LIB"
      INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
      INTERFACE_COMPILE_DEFINITIONS H5_BUILT_AS_DYNAMIC_LIB=1
  )
  set (install_targets ${install_targets} ${HDF5_HL_F90_C_LIBSH_TARGET})
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

#-----------------------------------------------------------------------------
# Fortran Modules
#-----------------------------------------------------------------------------
set (HDF5_HL_F90_F_SRCS
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5DSff.f90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5TBff.f90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5LTff.f90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5IMff.f90
)
set_source_files_properties (${HDF5_HL_F90_F_SRCS} PROPERTIES LANGUAGE Fortran)

add_library (${HDF5_HL_F90_LIB_TARGET} STATIC ${HDF5_HL_F90_F_SRCS})
TARGET_FORTRAN_PROPERTIES (${HDF5_HL_F90_LIB_TARGET} STATIC " " " ")
target_link_libraries (${HDF5_HL_F90_LIB_TARGET} ${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_F90_LIB_TARGET})
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_HL_F90_LIB_TARGET} ${HDF5_HL_F90_LIB_NAME} STATIC)
set_target_properties (${HDF5_HL_F90_LIB_TARGET} PROPERTIES
    FOLDER libraries/hl/fortran
    LINKER_LANGUAGE Fortran
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
)
if (WIN32)
  set_property (TARGET ${HDF5_HL_F90_LIB_TARGET}
      APPEND PROPERTY COMPILE_DEFINITIONS "HDF5F90_WINDOWS"
  )
endif (WIN32)
set (install_targets ${install_targets} ${HDF5_HL_F90_LIB_TARGET})

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_library (${HDF5_HL_F90_LIBSH_TARGET} SHARED ${HDF5_HL_F90_F_SRCS})
  set (SHARED_LINK_FLAGS " ")
  if (WIN32 AND MSVC)
    set (SHARED_LINK_FLAGS "/DLL")
  endif (WIN32 AND MSVC)
  TARGET_FORTRAN_PROPERTIES (${HDF5_HL_F90_LIBSH_TARGET} SHARED " " ${SHARED_LINK_FLAGS})
  target_link_libraries (${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_HL_F90_C_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET})
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_LIBSH_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_HL_F90_LIB_NAME} SHARED ${HDF5_HL_F_PACKAGE_SOVERSION})
  set_target_properties (${HDF5_HL_F90_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/hl/fortran
      LINKER_LANGUAGE Fortran
      INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
      INTERFACE_COMPILE_DEFINITIONS H5_BUILT_AS_DYNAMIC_LIB=1
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  if (WIN32)
    set_property (TARGET ${HDF5_HL_F90_LIBSH_TARGET}
        APPEND PROPERTY COMPILE_DEFINITIONS "BUILD_HDF5_HL_DLL;HDF5F90_WINDOWS"
    )
  endif (WIN32)
  set (install_targets ${install_targets} ${HDF5_HL_F90_LIBSH_TARGET})
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------


set (mod_files
    ${MOD_BUILD_DIR}/h5ds.mod
    ${MOD_BUILD_DIR}/h5tb.mod
    ${MOD_BUILD_DIR}/h5lt.mod
    ${MOD_BUILD_DIR}/h5im.mod
)

install (
    FILES
        ${mod_files}
    DESTINATION
        ${HDF5_INSTALL_INCLUDE_DIR}/static
    COMPONENT
        fortheaders
)

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  set (modsh_files
      ${MODSH_BUILD_DIR}/h5ds.mod
      ${MODSH_BUILD_DIR}/h5tb.mod
      ${MODSH_BUILD_DIR}/h5lt.mod
      ${MODSH_BUILD_DIR}/h5im.mod
  )
  install (
      FILES
          ${modsh_files}
      DESTINATION
          ${HDF5_INSTALL_INCLUDE_DIR}/shared
      COMPONENT
          fortheaders
  )
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
if (HDF5_EXPORTED_TARGETS)
  if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
    INSTALL_TARGET_PDB (${HDF5_HL_F90_C_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} hlfortlibraries)
    #INSTALL_TARGET_PDB (${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} hlfortlibraries)
  endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

  install (
      TARGETS
          ${install_targets}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT hlfortlibraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT hlfortlibraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT hlfortlibraries
      FRAMEWORK DESTINATION ${HDF5_INSTALL_FWRK_DIR} COMPONENT hlfortlibraries
      INCLUDES DESTINATION include
  )
endif (HDF5_EXPORTED_TARGETS)
