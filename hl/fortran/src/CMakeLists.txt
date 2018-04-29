cmake_minimum_required (VERSION 3.10)
project (HDF5_HL_F90_SRC C Fortran)

#-----------------------------------------------------------------------------
# configure def file for shared libs on windows
if (WIN32)
  if (BUILD_SHARED_LIBS)
    if (MSVC)
      configure_file (${HDF5_HL_F90_SRC_SOURCE_DIR}/hdf5_hl_fortrandll.def.in ${HDF5_HL_F90_SRC_BINARY_DIR}/hdf5_hl_fortrandll.def @ONLY)
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Generate the H5LT and H5TB REAL APIs
#-----------------------------------------------------------------------------

add_executable (H5HL_buildiface
      ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5HL_buildiface.F90
  )
target_include_directories(H5HL_buildiface PRIVATE "${HDF5_BINARY_DIR};${HDF5_F90_BINARY_DIR}")
if (WIN32 AND MSVC)
  if (BUILD_SHARED_LIBS)
    set_target_properties (H5HL_buildiface
        PROPERTIES
            COMPILE_FLAGS "/MT"
    )
  endif ()
  set_target_properties (H5HL_buildiface
      PROPERTIES
          LINK_FLAGS "/SUBSYSTEM:CONSOLE"
  )
endif ()
set_target_properties (H5HL_buildiface PROPERTIES
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

if (BUILD_SHARED_LIBS)
  file (MAKE_DIRECTORY "${HDF5_HL_F90_BINARY_DIR}/shared")
  set (MODSH_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/shared/${HDF_CFG_BUILD_TYPE})
endif ()
file (MAKE_DIRECTORY "${HDF5_HL_F90_BINARY_DIR}/static")
set (MOD_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/static/${HDF_CFG_BUILD_TYPE})

#-----------------------------------------------------------------------------
# hl_f90CStub lib
#-----------------------------------------------------------------------------
set (HDF5_HL_F90_C_SOURCES
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5DSfc.c
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5LTfc.c
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5IMfc.c
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5IMcc.c
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5TBfc.c
)
set_source_files_properties (${HDF5_HL_F90_C_SOURCES} PROPERTIES LANGUAGE C)

set (HDF5_HL_F90_HEADERS ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5LTf90proto.h)

add_library (${HDF5_HL_F90_C_LIB_TARGET} STATIC ${HDF5_HL_F90_C_SOURCES} ${HDF5_HL_F90_HEADERS})
target_include_directories(${HDF5_HL_F90_C_LIB_TARGET} PUBLIC "${HDF5_F90_BINARY_DIR}/static")
set_property(TARGET ${HDF5_HL_F90_C_LIB_TARGET} APPEND PROPERTY INCLUDE_DIRECTORIES "${HDF5_SRC_DIR};${HDF5_BINARY_DIR}")
TARGET_C_PROPERTIES (${HDF5_HL_F90_C_LIB_TARGET} STATIC " " " ")
target_link_libraries (${HDF5_HL_F90_C_LIB_TARGET} PUBLIC ${HDF5_F90_C_LIB_TARGET} ${HDF5_HL_LIB_TARGET})
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_C_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_HL_F90_C_LIB_NAME} STATIC 0)
set_target_properties (${HDF5_HL_F90_C_LIB_TARGET} PROPERTIES
    FOLDER libraries/hl/fortran
    LINKER_LANGUAGE C
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)
set (install_targets ${HDF5_HL_F90_C_LIB_TARGET})

if (BUILD_SHARED_LIBS)
  add_library (${HDF5_HL_F90_C_LIBSH_TARGET} SHARED ${HDF5_HL_F90_C_SOURCES} ${HDF5_HL_F90_HEADERS})
  target_include_directories(${HDF5_HL_F90_C_LIBSH_TARGET} PUBLIC "${HDF5_F90_BINARY_DIR}/shared")
  set_property(TARGET ${HDF5_HL_F90_C_LIBSH_TARGET} APPEND PROPERTY INCLUDE_DIRECTORIES "${HDF5_SRC_DIR};${HDF5_BINARY_DIR}")
  TARGET_C_PROPERTIES (${HDF5_HL_F90_C_LIBSH_TARGET} SHARED " " " ")
  target_link_libraries (${HDF5_HL_F90_C_LIBSH_TARGET} PUBLIC ${HDF5_F90_C_LIBSH_TARGET} ${HDF5_HL_LIBSH_TARGET})
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_C_LIBSH_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_HL_F90_C_LIBSH_TARGET} ${HDF5_HL_F90_C_LIB_NAME} SHARED "HL_F")
  set_target_properties (${HDF5_HL_F90_C_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/hl/fortran
      LINKER_LANGUAGE C
      COMPILE_DEFINITIONS "H5_BUILT_AS_DYNAMIC_LIB"
      INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
      INTERFACE_COMPILE_DEFINITIONS H5_BUILT_AS_DYNAMIC_LIB=1
  )
  set (install_targets ${install_targets} ${HDF5_HL_F90_C_LIBSH_TARGET})
endif ()

#-----------------------------------------------------------------------------
# Fortran Modules
#-----------------------------------------------------------------------------
set (HDF5_HL_F90_F_BASE_SOURCES
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5DSff.F90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5TBff.F90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5LTff.F90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5IMff.F90
)

add_custom_command (
    OUTPUT ${HDF5_HL_F90_BINARY_DIR}/static/H5LTff_gen.F90 ${HDF5_HL_F90_BINARY_DIR}/H5TBff_gen.F90
    COMMAND $<TARGET_FILE:H5HL_buildiface>
    WORKING_DIRECTORY ${HDF5_HL_F90_BINARY_DIR}/static
    DEPENDS ${HDF5_HL_F90_F_BASE_SOURCES}
    COMMENT "Generating the H5LTff_gen.F90, H5TBff_gen.F90 files"
)
add_custom_target (H5HLgen ALL
    DEPENDS ${HDF5_HL_F90_BINARY_DIR}/static/H5LTff_gen.F90 ${HDF5_HL_F90_BINARY_DIR}/H5TBff_gen.F90
)
set_source_files_properties (
    ${HDF5_HL_F90_BINARY_DIR}/static/H5LTff_gen.F90
    ${HDF5_HL_F90_BINARY_DIR}/static/H5TBff_gen.F90
    PROPERTIES GENERATED TRUE
)
if (BUILD_SHARED_LIBS)
  add_custom_command (
      OUTPUT ${HDF5_HL_F90_BINARY_DIR}/shared/H5LTff_gen.F90 ${HDF5_HL_F90_BINARY_DIR}/shared/H5TBff_gen.F90
      COMMAND $<TARGET_FILE:H5HL_buildiface>
      WORKING_DIRECTORY ${HDF5_HL_F90_BINARY_DIR}/shared
      DEPENDS ${HDF5_HL_F90_F_BASE_SOURCES}
      COMMENT "Generating the H5LTff_gen.F90, H5TBff_gen.F90 shared files"
  )
  add_custom_target (H5HLgenSH ALL
      DEPENDS ${HDF5_HL_F90_BINARY_DIR}/shared/H5LTff_gen.F90 ${HDF5_HL_F90_BINARY_DIR}/shared/H5TBff_gen.F90
  )
  set_source_files_properties (
      ${HDF5_HL_F90_BINARY_DIR}/shared/H5LTff_gen.F90
      ${HDF5_HL_F90_BINARY_DIR}/shared/H5TBff_gen.F90
      PROPERTIES GENERATED TRUE
  )
endif ()

set (HDF5_HL_F90_F_SOURCES
    ${HDF5_HL_F90_F_BASE_SOURCES}

    # generated files
    ${HDF5_HL_F90_BINARY_DIR}/static/H5LTff_gen.F90
    ${HDF5_HL_F90_BINARY_DIR}/static/H5TBff_gen.F90
)
set_source_files_properties (${HDF5_HL_F90_F_SOURCES} PROPERTIES LANGUAGE Fortran)

if (BUILD_SHARED_LIBS)
  set (HDF5_HL_F90_F_SOURCES_SHARED
      ${HDF5_HL_F90_F_BASE_SOURCES}

      # generated files
      ${HDF5_HL_F90_BINARY_DIR}/shared/H5LTff_gen.F90
      ${HDF5_HL_F90_BINARY_DIR}/shared/H5TBff_gen.F90
  )
  set_source_files_properties (${HDF5_HL_F90_F_SOURCES_SHARED} PROPERTIES LANGUAGE Fortran)
endif ()

add_library (${HDF5_HL_F90_LIB_TARGET} STATIC ${HDF5_HL_F90_F_SOURCES})
TARGET_FORTRAN_PROPERTIES (${HDF5_HL_F90_LIB_TARGET} STATIC " " " ")
target_link_libraries (${HDF5_HL_F90_LIB_TARGET} PUBLIC ${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_F90_LIB_TARGET})
target_include_directories (${HDF5_HL_F90_LIB_TARGET} PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}/static")
if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  target_include_directories (${HDF5_HL_F90_LIB_TARGET} PUBLIC ${MPI_Fortran_INCLUDE_DIRS})
endif ()
set_property(TARGET ${HDF5_HL_F90_LIB_TARGET} APPEND PROPERTY INCLUDE_DIRECTORIES "${HDF5_F90_BINARY_DIR}")
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_HL_F90_LIB_TARGET} ${HDF5_HL_F90_LIB_NAME} STATIC 0)
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
endif ()
set (install_targets ${install_targets} ${HDF5_HL_F90_LIB_TARGET})
add_dependencies(${HDF5_HL_F90_LIB_TARGET} H5HLgen)

if (BUILD_SHARED_LIBS)
  add_library (${HDF5_HL_F90_LIBSH_TARGET} SHARED ${HDF5_HL_F90_F_SOURCES_SHARED})
  set (SHARED_LINK_FLAGS " ")
  if (WIN32 AND MSVC)
    set (SHARED_LINK_FLAGS "/DLL /DEF:${HDF5_HL_F90_SRC_BINARY_DIR}/hdf5_hl_fortrandll.def")
  endif ()
  TARGET_FORTRAN_PROPERTIES (${HDF5_HL_F90_LIBSH_TARGET} SHARED " " ${SHARED_LINK_FLAGS})
  target_link_libraries (${HDF5_HL_F90_LIBSH_TARGET} PUBLIC ${HDF5_HL_F90_C_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET})
  target_link_libraries (${HDF5_HL_F90_LIBSH_TARGET} PRIVATE ${LINK_Fortran_LIBS})
  target_include_directories (${HDF5_HL_F90_LIBSH_TARGET} PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}/shared")
  set_property(TARGET ${HDF5_HL_F90_LIBSH_TARGET} APPEND PROPERTY INCLUDE_DIRECTORIES "${HDF5_F90_BINARY_DIR}")
  if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
    target_include_directories (${HDF5_HL_F90_LIBSH_TARGET} PUBLIC ${MPI_Fortran_INCLUDE_DIRS})
  endif ()
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_LIBSH_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_HL_F90_LIB_NAME} SHARED "HL_F")
  set_target_properties (${HDF5_HL_F90_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/hl/fortran
      LINKER_LANGUAGE Fortran
      COMPILE_DEFINITIONS "H5_BUILT_AS_DYNAMIC_LIB"
      INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
      INTERFACE_COMPILE_DEFINITIONS H5_BUILT_AS_DYNAMIC_LIB=1
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  if (WIN32)
    set_property (TARGET ${HDF5_HL_F90_LIBSH_TARGET}
        APPEND PROPERTY COMPILE_DEFINITIONS "BUILD_HDF5_HL_DLL;HDF5F90_WINDOWS"
    )
  endif ()
  set (install_targets ${install_targets} ${HDF5_HL_F90_LIBSH_TARGET})
  add_dependencies(${HDF5_HL_F90_LIBSH_TARGET} H5HLgenSH)
endif ()

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------


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
        ${HDF5_INSTALL_INCLUDE_DIR}/static
    COMPONENT
        fortheaders
)

if (BUILD_SHARED_LIBS)
  set (modsh_files
      ${MODSH_BUILD_DIR}/h5ds.mod
      ${MODSH_BUILD_DIR}/h5tb.mod
      ${MODSH_BUILD_DIR}/h5tb_const.mod
      ${MODSH_BUILD_DIR}/h5lt.mod
      ${MODSH_BUILD_DIR}/h5lt_const.mod
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
endif ()

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
if (HDF5_EXPORTED_TARGETS)
  if (BUILD_SHARED_LIBS)
    INSTALL_TARGET_PDB (${HDF5_HL_F90_C_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} hlfortlibraries)
    #INSTALL_TARGET_PDB (${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} hlfortlibraries)
  endif ()
  INSTALL_TARGET_PDB (${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_INSTALL_BIN_DIR} hlfortlibraries)
  #INSTALL_TARGET_PDB (${HDF5_HL_F90_LIB_TARGET} ${HDF5_INSTALL_BIN_DIR} hlfortlibraries)

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
endif ()
