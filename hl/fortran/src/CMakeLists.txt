cmake_minimum_required (VERSION 2.8.11)
PROJECT(HDF5_HL_F90_SRC C CXX Fortran)

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
TARGET_C_PROPERTIES (${HDF5_HL_F90_C_LIB_TARGET} " " " ")
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
set (HDF5_HL_F90_F_SRCS
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5DSff.f90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5TBff.f90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5LTff.f90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5IMff.f90
)

add_library (${HDF5_HL_F90_LIB_TARGET} ${LIB_TYPE} ${HDF5_HL_F90_F_SRCS})
set (SHARED_LINK_FLAGS " ")
if (BUILD_SHARED_LIBS)
  if (WIN32)
    set_property (TARGET ${HDF5_HL_F90_LIB_TARGET} 
      APPEND PROPERTY COMPILE_DEFINITIONS 
        BUILD_HDF5_DLL
  )
    if (MSVC)
      set (SHARED_LINK_FLAGS "/DLL")
    endif (MSVC)
  endif (WIN32)
endif (BUILD_SHARED_LIBS)
if (WIN32)
  set_property (TARGET ${HDF5_HL_F90_LIB_TARGET} 
      APPEND PROPERTY COMPILE_DEFINITIONS 
        HDF5F90_WINDOWS
  )
endif (WIN32)
TARGET_FORTRAN_PROPERTIES (${HDF5_HL_F90_LIB_TARGET} " " ${SHARED_LINK_FLAGS})
set_target_properties (${HDF5_HL_F90_LIB_TARGET} PROPERTIES LINKER_LANGUAGE Fortran)
target_link_libraries (${HDF5_HL_F90_LIB_TARGET} ${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_F90_LIB_TARGET})
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_HL_F90_LIB_TARGET} ${HDF5_HL_F90_LIB_NAME} ${LIB_TYPE})
set_target_properties (${HDF5_HL_F90_LIB_TARGET} PROPERTIES
    FOLDER libraries/hl/fortran
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
if (WIN32)
  install (
      FILES
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5ds.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5tb.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5lt.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5im.mod
      DESTINATION
          ${HDF5_INSTALL_INCLUDE_DIR}
      COMPONENT
          fortheaders
  )
else (WIN32)
  install (
      FILES
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5ds.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5tb.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5lt.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5im.mod
      DESTINATION
          ${HDF5_INSTALL_INCLUDE_DIR}
      COMPONENT
          fortheaders
  )
endif (WIN32)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
if (HDF5_EXPORTED_TARGETS)
  if (BUILD_SHARED_LIBS)
    INSTALL_TARGET_PDB (${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_INSTALL_LIB_DIR} hlfortlibraries)
    #INSTALL_TARGET_PDB (${HDF5_HL_F90_LIB_TARGET} ${HDF5_INSTALL_LIB_DIR} hlfortlibraries)
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
  )
endif (HDF5_EXPORTED_TARGETS)
