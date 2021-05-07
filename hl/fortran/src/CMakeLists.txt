cmake_minimum_required (VERSION 3.12)
project (HDF5_HL_F90_SRC C Fortran)

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  file (MAKE_DIRECTORY "${HDF5_HL_F90_BINARY_DIR}/shared")
  set (MODSH_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/shared/${HDF_CFG_BUILD_TYPE})
endif ()
if (NOT ONLY_SHARED_LIBS)
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

if (NOT ONLY_SHARED_LIBS)
  add_library (${HDF5_HL_F90_C_LIB_TARGET} STATIC ${HDF5_HL_F90_C_SOURCES} ${HDF5_HL_F90_HEADERS})
  target_include_directories (${HDF5_HL_F90_C_LIB_TARGET}
      PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};${HDF5_F90_BINARY_DIR}/static;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
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
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_library (${HDF5_HL_F90_C_LIBSH_TARGET} SHARED ${HDF5_HL_F90_C_SOURCES} ${HDF5_HL_F90_HEADERS})
  target_include_directories (${HDF5_HL_F90_C_LIBSH_TARGET}
      PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};${HDF5_F90_BINARY_DIR}/shared;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
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
  if (NOT ONLY_SHARED_LIBS)
    clang_format (HDF5_HL_F90_C_SRC_FORMAT ${HDF5_HL_F90_C_LIB_TARGET})
  else ()
    clang_format (HDF5_HL_F90_C_SRC_FORMAT ${HDF5_HL_F90_C_LIBSH_TARGET})
  endif ()
endif ()

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

if (NOT ONLY_SHARED_LIBS)
  add_library (${HDF5_HL_F90_LIB_TARGET} STATIC ${HDF5_HL_F90_F_SRCS})
  target_include_directories (${HDF5_HL_F90_LIB_TARGET}
      PRIVATE "${HDF5_F90_BINARY_DIR};${CMAKE_Fortran_MODULE_DIRECTORY}/static;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include/static>"
  )
  target_compile_options(${HDF5_HL_F90_LIB_TARGET} PRIVATE "${HDF5_CMAKE_Fortran_FLAGS}")
  target_compile_definitions(${HDF5_HL_F90_LIB_TARGET}
      PUBLIC $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:HDF5F90_WINDOWS>
      PRIVATE $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
  )
  target_link_libraries (${HDF5_HL_F90_LIB_TARGET} PUBLIC ${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_F90_LIB_TARGET})
#  set_property(TARGET ${HDF5_HL_F90_LIB_TARGET} APPEND PROPERTY LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">)
#  set_property(TARGET ${HDF5_HL_F90_LIB_TARGET} APPEND PROPERTY LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>)
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
endif ()
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_library (${HDF5_HL_F90_LIBSH_TARGET} SHARED ${HDF5_HL_F90_F_SRCS})
  target_include_directories (${HDF5_HL_F90_LIBSH_TARGET}
      PRIVATE "${HDF5_F90_BINARY_DIR};${CMAKE_Fortran_MODULE_DIRECTORY}/shared;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include/shared>"
  )
  target_compile_options(${HDF5_HL_F90_LIBSH_TARGET} PRIVATE "${HDF5_CMAKE_Fortran_FLAGS}")
  target_compile_definitions(${HDF5_HL_F90_LIBSH_TARGET}
      PUBLIC "H5_BUILT_AS_DYNAMIC_LIB"
      PRIVATE
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:BUILD_HDF5_HL_DLL;HDF5F90_WINDOWS>
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
  )
  target_link_libraries (${HDF5_HL_F90_LIBSH_TARGET}
      PUBLIC ${HDF5_HL_F90_C_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET}
      PRIVATE ${LINK_Fortran_LIBS}
  )
#  set_property(TARGET ${HDF5_HL_F90_LIBSH_TARGET} APPEND PROPERTY LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">)
#  set_property(TARGET ${HDF5_HL_F90_LIBSH_TARGET} APPEND PROPERTY LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>)
#  set_property(TARGET ${HDF5_HL_F90_LIBSH_TARGET} APPEND PROPERTY LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-DLL">)
#  set_property(TARGET ${HDF5_HL_F90_LIBSH_TARGET} APPEND PROPERTY LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-DEF:${HDF5_HL_F90_SRC_BINARY_DIR}/hdf5_hl_fortrandll.def">)
  if(MSVC)
    set_property(TARGET ${HDF5_HL_F90_LIBSH_TARGET} PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS} -DLL")
  endif()
  set_target_properties (${HDF5_HL_F90_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/hl/fortran
      LINKER_LANGUAGE Fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  H5_SET_LIB_OPTIONS (${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_HL_F90_LIB_NAME} SHARED "HL_F")
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_LIBSH_TARGET}")
  set (install_targets ${install_targets} ${HDF5_HL_F90_LIBSH_TARGET})
endif ()

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------


if (NOT ONLY_SHARED_LIBS)
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
  if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
    INSTALL_TARGET_PDB (${HDF5_HL_F90_C_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} hlfortlibraries)
    #INSTALL_TARGET_PDB (${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} hlfortlibraries)
  endif ()
  if (NOT ONLY_SHARED_LIBS)
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
set (_PKG_CONFIG_LIBDIR \${exec_prefix}/lib)
set (_PKG_CONFIG_INCLUDEDIR \${prefix}/include)
set (_PKG_CONFIG_LIBNAME "${HDF5_HL_F90_LIB_CORENAME}")
set (_PKG_CONFIG_VERSION "${HDF5_PACKAGE_VERSION}")

set (_PKG_CONFIG_LIBS_PRIVATE)

if (NOT ONLY_SHARED_LIBS)
  set (_PKG_CONFIG_LIBS "${_PKG_CONFIG_LIBS} -l${HDF5_HL_F90_LIB_CORENAME}")
endif ()
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  set (_PKG_CONFIG_SH_LIBS "${_PKG_CONFIG_SH_LIBS} -l${HDF5_HL_F90_LIB_CORENAME}")
endif ()

set (_PKG_CONFIG_REQUIRES "${HDF5_F90_LIB_CORENAME}-${HDF5_PACKAGE_VERSION}")
set (_PKG_CONFIG_REQUIRES_PRIVATE "${HDF5_F90_LIB_CORENAME}-${HDF5_PACKAGE_VERSION}")

configure_file (
    ${HDF_CONFIG_DIR}/libhdf5.pc.in
    ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_HL_F90_LIB_CORENAME}-${HDF5_PACKAGE_VERSION}.pc
    @ONLY
)
install (
    FILES ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_HL_F90_LIB_CORENAME}-${HDF5_PACKAGE_VERSION}.pc
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
