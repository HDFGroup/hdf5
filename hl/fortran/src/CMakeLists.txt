cmake_minimum_required (VERSION 3.18)
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

add_executable (H5HL_buildiface ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5HL_buildiface.F90)
target_include_directories (H5HL_buildiface PRIVATE "${HDF5_SRC_BINARY_DIR};${HDF5_F90_BINARY_DIR}")
if (WIN32 AND MSVC)
  if (BUILD_SHARED_LIBS)
    set_target_properties (H5HL_buildiface PROPERTIES COMPILE_FLAGS "/MT")
  endif ()
endif ()
if(MSVC)
  set_property(TARGET H5HL_buildiface PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE")
endif()
set_target_properties (H5HL_buildiface PROPERTIES
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

if (BUILD_SHARED_LIBS)
  file (MAKE_DIRECTORY "${HDF5_HL_F90_BINARY_DIR}/shared")
  set (MODSH_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/shared/${HDF_CFG_BUILD_TYPE})
endif ()
if (BUILD_STATIC_LIBS)
  file (MAKE_DIRECTORY "${HDF5_HL_F90_BINARY_DIR}/static")
  set (MOD_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/static/${HDF_CFG_BUILD_TYPE})
endif ()

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

set (HDF5_HL_F90_HEADERS ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5LTf90proto.h ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5IMcc.h)

if (BUILD_STATIC_LIBS)
  add_library (${HDF5_HL_F90_C_LIB_TARGET} STATIC ${HDF5_HL_F90_C_SOURCES} ${HDF5_HL_F90_HEADERS})
  target_include_directories (${HDF5_HL_F90_C_LIB_TARGET}
      PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};${HDF5_F90_BINARY_DIR}/static;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_options(${HDF5_HL_F90_C_LIB_TARGET} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  TARGET_C_PROPERTIES (${HDF5_HL_F90_C_LIB_TARGET} STATIC)
  target_link_libraries (${HDF5_HL_F90_C_LIB_TARGET} PUBLIC ${HDF5_F90_C_LIB_TARGET} ${HDF5_HL_LIB_TARGET})
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_C_LIB_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_HL_F90_C_LIB_NAME} STATIC 0)
  set_target_properties (${HDF5_HL_F90_C_LIB_TARGET} PROPERTIES
      FOLDER libraries/hl/fortran
      LINKER_LANGUAGE C
  )
  set (install_targets ${HDF5_HL_F90_C_LIB_TARGET})
endif ()
if (BUILD_SHARED_LIBS)
  add_library (${HDF5_HL_F90_C_LIBSH_TARGET} SHARED ${HDF5_HL_F90_C_SOURCES} ${HDF5_HL_F90_HEADERS})
  target_include_directories (${HDF5_HL_F90_C_LIBSH_TARGET}
      PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};${HDF5_F90_BINARY_DIR}/shared;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_options(${HDF5_HL_F90_C_LIBSH_TARGET} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_compile_definitions(${HDF5_HL_F90_C_LIBSH_TARGET} PUBLIC "H5_BUILT_AS_DYNAMIC_LIB")
  TARGET_C_PROPERTIES (${HDF5_HL_F90_C_LIBSH_TARGET} SHARED)
  target_link_libraries (${HDF5_HL_F90_C_LIBSH_TARGET} PUBLIC ${HDF5_F90_C_LIBSH_TARGET} ${HDF5_HL_LIBSH_TARGET})
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_C_LIBSH_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_HL_F90_C_LIBSH_TARGET} ${HDF5_HL_F90_C_LIB_NAME} SHARED "HL_F")
  set_target_properties (${HDF5_HL_F90_C_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/hl/fortran
      LINKER_LANGUAGE C
  )
  set (install_targets ${install_targets} ${HDF5_HL_F90_C_LIBSH_TARGET})
endif ()

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  if (BUILD_STATIC_LIBS)
    clang_format (HDF5_HL_F90_C_SRC_FORMAT ${HDF5_HL_F90_C_LIB_TARGET})
  else ()
    clang_format (HDF5_HL_F90_C_SRC_FORMAT ${HDF5_HL_F90_C_LIBSH_TARGET})
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Fortran Modules
#-----------------------------------------------------------------------------
set (HDF5_HL_F90_F_BASE_SOURCES
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5DSff.F90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5TBff.F90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5LTff.F90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5IMff.F90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5DOff.F90
)

if (BUILD_STATIC_LIBS)
  add_custom_command (TARGET H5HL_buildiface POST_BUILD
      BYPRODUCTS ${HDF5_HL_F90_BINARY_DIR}/static/H5LTff_gen.F90 ${HDF5_HL_F90_BINARY_DIR}/static/H5TBff_gen.F90
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:H5HL_buildiface>
      WORKING_DIRECTORY ${HDF5_HL_F90_BINARY_DIR}/static
      DEPENDS ${HDF5_HL_F90_F_BASE_SOURCES}
      COMMENT "Generating the H5LTff_gen.F90, H5TBff_gen.F90 files"
  )
  add_custom_target (H5HLgen ALL
      DEPENDS H5HL_buildiface ${HDF5_HL_F90_BINARY_DIR}/static/H5LTff_gen.F90 ${HDF5_HL_F90_BINARY_DIR}/static/H5TBff_gen.F90
  )
  set_source_files_properties (
      ${HDF5_HL_F90_BINARY_DIR}/static/H5LTff_gen.F90
      ${HDF5_HL_F90_BINARY_DIR}/static/H5TBff_gen.F90
      PROPERTIES GENERATED TRUE
  )
endif ()
if (BUILD_SHARED_LIBS)
  add_custom_command (TARGET H5HL_buildiface POST_BUILD
      BYPRODUCTS ${HDF5_HL_F90_BINARY_DIR}/shared/H5LTff_gen.F90 ${HDF5_HL_F90_BINARY_DIR}/shared/H5TBff_gen.F90
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:H5HL_buildiface>
      WORKING_DIRECTORY ${HDF5_HL_F90_BINARY_DIR}/shared
      DEPENDS ${HDF5_HL_F90_F_BASE_SOURCES}
      COMMENT "Generating the H5LTff_gen.F90, H5TBff_gen.F90 shared files"
  )
  add_custom_target (H5HLgenSH ALL
      DEPENDS H5HL_buildiface ${HDF5_HL_F90_BINARY_DIR}/shared/H5LTff_gen.F90 ${HDF5_HL_F90_BINARY_DIR}/shared/H5TBff_gen.F90
  )
  set_source_files_properties (
      ${HDF5_HL_F90_BINARY_DIR}/shared/H5LTff_gen.F90
      ${HDF5_HL_F90_BINARY_DIR}/shared/H5TBff_gen.F90
      PROPERTIES GENERATED TRUE
  )
endif ()

if (BUILD_STATIC_LIBS)
  set (HDF5_HL_F90_F_SOURCES
      ${HDF5_HL_F90_F_BASE_SOURCES}

      # generated files
      ${HDF5_HL_F90_BINARY_DIR}/static/H5LTff_gen.F90
      ${HDF5_HL_F90_BINARY_DIR}/static/H5TBff_gen.F90
  )
  set_source_files_properties (${HDF5_HL_F90_F_SOURCES} PROPERTIES LANGUAGE Fortran)
endif ()
if (BUILD_SHARED_LIBS)
  set (HDF5_HL_F90_F_SOURCES_SHARED
      ${HDF5_HL_F90_F_BASE_SOURCES}

      # generated files
      ${HDF5_HL_F90_BINARY_DIR}/shared/H5LTff_gen.F90
      ${HDF5_HL_F90_BINARY_DIR}/shared/H5TBff_gen.F90
  )
  set_source_files_properties (${HDF5_HL_F90_F_SOURCES_SHARED} PROPERTIES LANGUAGE Fortran)
endif ()

if (BUILD_STATIC_LIBS)
  add_library (${HDF5_HL_F90_LIB_TARGET} STATIC ${HDF5_HL_F90_F_SOURCES})
  target_include_directories (${HDF5_HL_F90_LIB_TARGET}
      PRIVATE "${HDF5_F90_BINARY_DIR};${CMAKE_Fortran_MODULE_DIRECTORY}/static;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${HDF5_INSTALL_MODULE_DIR}/static>"
  )
  target_compile_options(${HDF5_HL_F90_LIB_TARGET} PRIVATE "${HDF5_CMAKE_Fortran_FLAGS}")
  target_compile_definitions(${HDF5_HL_F90_LIB_TARGET}
      PUBLIC "$<$<STREQUAL:x${CMAKE_Fortran_SIMULATE_ID},xMSVC>:HDF5F90_WINDOWS>"
      PRIVATE "$<$<STREQUAL:x${CMAKE_Fortran_SIMULATE_ID},xMSVC>:${WIN_COMPILE_FLAGS}>"
  )
  target_link_libraries (${HDF5_HL_F90_LIB_TARGET} PUBLIC ${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_F90_LIB_TARGET})
  if(MSVC)
    set_property(TARGET ${HDF5_HL_F90_LIB_TARGET} PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
  endif()
  set_target_properties (${HDF5_HL_F90_LIB_TARGET} PROPERTIES
      FOLDER libraries/hl/fortran
      LINKER_LANGUAGE Fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
  )
  H5_SET_LIB_OPTIONS (${HDF5_HL_F90_LIB_TARGET} ${HDF5_HL_F90_LIB_NAME} STATIC 0)
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_LIB_TARGET}")
  set (install_targets ${install_targets} ${HDF5_HL_F90_LIB_TARGET})
  add_dependencies(${HDF5_HL_F90_LIB_TARGET} H5HLgen)
endif ()
if (BUILD_SHARED_LIBS)
  if(WIN32)
      set(DLLDEF ${HDF5_HL_F90_SRC_BINARY_DIR}/hdf5_hl_fortrandll.def)
  else()
      set(DLLDEF "")
  endif()
  add_library (${HDF5_HL_F90_LIBSH_TARGET} SHARED ${DLLDEF} ${HDF5_HL_F90_F_SOURCES_SHARED})
  target_include_directories (${HDF5_HL_F90_LIBSH_TARGET}
      PRIVATE "${HDF5_F90_BINARY_DIR};${CMAKE_Fortran_MODULE_DIRECTORY}/shared;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${HDF5_INSTALL_MODULE_DIR}/shared>"
  )
  target_compile_options(${HDF5_HL_F90_LIBSH_TARGET} PRIVATE "${HDF5_CMAKE_Fortran_FLAGS}")
  target_compile_definitions(${HDF5_HL_F90_LIBSH_TARGET}
      PUBLIC "H5_BUILT_AS_DYNAMIC_LIB"
      PRIVATE
          "$<$<STREQUAL:x${CMAKE_Fortran_SIMULATE_ID},xMSVC>:BUILD_HDF5_HL_DLL;HDF5F90_WINDOWS>"
          "$<$<STREQUAL:x${CMAKE_Fortran_SIMULATE_ID},xMSVC>:${WIN_COMPILE_FLAGS}>"
  )
  target_link_libraries (${HDF5_HL_F90_LIBSH_TARGET}
      PUBLIC ${HDF5_HL_F90_C_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET}
      PRIVATE ${LINK_Fortran_LIBS}
  )
  if(MSVC)
    set_property(TARGET ${HDF5_HL_F90_LIBSH_TARGET} PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
  endif()
  set_target_properties (${HDF5_HL_F90_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/hl/fortran
      LINKER_LANGUAGE Fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  H5_SET_LIB_OPTIONS (${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_HL_F90_LIB_NAME} SHARED "HL_F")
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_LIBSH_TARGET}")
  set (install_targets ${install_targets} ${HDF5_HL_F90_LIBSH_TARGET})
  add_dependencies(${HDF5_HL_F90_LIBSH_TARGET} H5HLgenSH)
endif ()

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------

set (mod_export_files
    h5ds.mod
    h5tb.mod
    h5tb_const.mod
    h5lt.mod
    h5lt_const.mod
    h5im.mod
    h5do.mod
)

if (BUILD_STATIC_LIBS)
  foreach (mod_file ${mod_export_files})
    set (mod_files ${mod_files} ${MOD_BUILD_DIR}/${mod_file})
  endforeach ()
  install (
      FILES
          ${mod_files}
      DESTINATION
          ${HDF5_INSTALL_MODULE_DIR}/static
      COMPONENT
          fortheaders
  )
  if (HDF5_INSTALL_MOD_FORTRAN MATCHES "STATIC")
    install (
        FILES
            ${mod_files}
        DESTINATION
            ${HDF5_INSTALL_INCLUDE_DIR}
        COMPONENT
            fortheaders
    )
  endif ()
endif ()
if (BUILD_SHARED_LIBS)
  foreach (mod_file ${mod_export_files})
    set (modsh_files ${modsh_files} ${MODSH_BUILD_DIR}/${mod_file})
  endforeach ()
  install (
      FILES
          ${modsh_files}
      DESTINATION
          ${HDF5_INSTALL_MODULE_DIR}/shared
      COMPONENT
          fortheaders
  )
  if (HDF5_INSTALL_MOD_FORTRAN MATCHES "SHARED")
    install (
        FILES
            ${modsh_files}
        DESTINATION
            ${HDF5_INSTALL_INCLUDE_DIR}
        COMPONENT
            fortheaders
    )
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
if (HDF5_EXPORTED_TARGETS)
  if (BUILD_SHARED_LIBS)
    INSTALL_TARGET_PDB (${HDF5_HL_F90_C_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} hlfortlibraries)
    #INSTALL_TARGET_PDB (${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} hlfortlibraries)
  endif ()
  if (BUILD_STATIC_LIBS)
    INSTALL_TARGET_PDB (${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_INSTALL_LIB_DIR} hlfortlibraries)
    #INSTALL_TARGET_PDB (${HDF5_HL_F90_LIB_TARGET} ${HDF5_INSTALL_LIB_DIR} hlfortlibraries)
  endif ()

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

#-----------------------------------------------------------------------------
# Create pkgconfig files
#-----------------------------------------------------------------------------
set (_PKG_CONFIG_PREFIX ${CMAKE_INSTALL_PREFIX})
set (_PKG_CONFIG_EXEC_PREFIX \${prefix})
set (_PKG_CONFIG_LIBDIR \${exec_prefix}/${HDF5_INSTALL_LIB_DIR})
set (_PKG_CONFIG_INCLUDEDIR \${prefix}/${HDF5_INSTALL_INCLUDE_DIR})
set (_PKG_CONFIG_MODULEDIR \${prefix}/${HDF5_INSTALL_MODULE_DIR})
set (_PKG_CONFIG_LIBNAME "${HDF5_HL_F90_LIB_CORENAME}")
set (_PKG_CONFIG_VERSION "${HDF5_PACKAGE_VERSION}")
set (PKG_CONFIG_LIBNAME "${HDF5_HL_F90_LIB_CORENAME}")
if (${HDF_CFG_NAME} MATCHES "Debug" OR ${HDF_CFG_NAME} MATCHES "Developer")
  set (PKG_CONFIG_LIBNAME "${PKG_CONFIG_LIBNAME}${CMAKE_DEBUG_POSTFIX}")
endif ()

set (_PKG_CONFIG_LIBS_PRIVATE)

if (BUILD_STATIC_LIBS)
  set (_PKG_CONFIG_LIBS "${_PKG_CONFIG_LIBS} -l${PKG_CONFIG_LIBNAME}")
endif ()
if (BUILD_SHARED_LIBS)
  set (_PKG_CONFIG_SH_LIBS "${_PKG_CONFIG_SH_LIBS} -l${PKG_CONFIG_LIBNAME}")
endif ()

set (_PKG_CONFIG_REQUIRES "${HDF5_F90_LIB_CORENAME} = ${HDF5_PACKAGE_VERSION}")
set (_PKG_CONFIG_REQUIRES_PRIVATE "${HDF5_F90_LIB_CORENAME} = ${HDF5_PACKAGE_VERSION}")

configure_file (
    ${HDF_CONFIG_DIR}/libhdf5.fpc.in
    ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_HL_F90_LIB_CORENAME}.pc
    @ONLY
)
install (
    FILES ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_HL_F90_LIB_CORENAME}.pc
    DESTINATION ${HDF5_INSTALL_LIB_DIR}/pkgconfig
    COMPONENT hlfortlibraries
)

if (NOT WIN32 AND NOT MINGW)
  if (HDF5_ENABLE_PARALLEL AND MPI_Fortran_FOUND)
    set (_PKG_CONFIG_COMPILER ${MPI_Fortran_COMPILER})
  else ()
    set (_PKG_CONFIG_COMPILER ${CMAKE_Fortran_COMPILER})
  endif ()
  configure_file (
      ${HDF_RESOURCES_DIR}/libh5cc.in
      ${HDF5_BINARY_DIR}/CMakeFiles/h5hlfc
      @ONLY
  )
  install (
      FILES ${HDF5_BINARY_DIR}/CMakeFiles/h5hlfc
      DESTINATION ${HDF5_INSTALL_BIN_DIR}
      PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
      COMPONENT hlfortlibraries
  )
endif ()
