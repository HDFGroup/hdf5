cmake_minimum_required (VERSION 3.10)
project (HDF5_F90_SRC C Fortran)

#-----------------------------------------------------------------------------
# configure def file for shared libs on windows
if (WIN32)
  if (BUILD_SHARED_LIBS)
    if (MSVC)
      if (NOT H5_HAVE_PARALLEL)
        set (H5_NOPAREXP ";")
      endif ()
      set (H5_F03EXP ";")
      configure_file (${HDF5_F90_SRC_SOURCE_DIR}/hdf5_fortrandll.def.in ${HDF5_F90_SRC_BINARY_DIR}/hdf5_fortrandll.def @ONLY)
    endif ()
  endif ()
endif ()

# configure for Fortran preprocessor

# Define Parallel variable for passing to H5config_f.inc.cmake
set (CMAKE_H5_HAVE_PARALLEL 0)
if (H5_HAVE_PARALLEL)
  set (CMAKE_H5_HAVE_PARALLEL 1)
endif ()

set (CMAKE_H5_HAVE_FLOAT128 0)
if (HAVE_FLOAT128)
  set (CMAKE_H5_HAVE_FLOAT128 1)
endif ()

configure_file (${HDF5_F90_SRC_SOURCE_DIR}/H5config_f.inc.cmake ${HDF5_F90_BINARY_DIR}/H5config_f.inc @ONLY)
configure_file (${HDF5_F90_SRC_SOURCE_DIR}/H5fort_type_defines.h.cmake ${HDF5_F90_BINARY_DIR}/H5fort_type_defines.h @ONLY)

set (Fortran_COMPILER_ID CMAKE_Fortran_COMPILER_ID)

#-----------------------------------------------------------------------------
# Setup the Fortran auto-detection utilities
# H5_buildiface.F90      used to generate various KIND interfaces
# H5fort_type_defines.h   used to generate H5f90i_gen.h + H5fortran_types.F90
#-----------------------------------------------------------------------------

add_executable (H5_buildiface ${HDF5_F90_SRC_SOURCE_DIR}/H5_buildiface.F90)
target_include_directories (H5_buildiface
    PRIVATE
        "${HDF5_BINARY_DIR};${HDF5_F90_BINARY_DIR}"
)
#if (BUILD_SHARED_LIBS)
#  target_compile_definitions(H5_buildiface
#      PRIVATE
#          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:/MT>
#  )
#endif ()
if(MSVC)
  set_property(TARGET H5_buildiface PROPERTY COMPILE_FLAGS "/MT")
endif()
#set_property(TARGET H5_buildiface APPEND PROPERTY
#    LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#)
if(MSVC)
  set_property(TARGET H5_buildiface PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE")
endif()

set_target_properties (H5_buildiface PROPERTIES
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

if (BUILD_SHARED_LIBS)
  file (MAKE_DIRECTORY "${HDF5_F90_BINARY_DIR}/shared")
  set (MODSH_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/shared/${HDF_CFG_BUILD_TYPE})
endif ()
if (NOT ONLY_SHARED_LIBS)
  file (MAKE_DIRECTORY "${HDF5_F90_BINARY_DIR}/static")
  set (MOD_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/static/${HDF_CFG_BUILD_TYPE})
endif ()

#-----------------------------------------------------------------------------
add_executable (H5match_types
    ${HDF5_F90_BINARY_DIR}/H5fort_type_defines.h
    ${HDF5_F90_SRC_SOURCE_DIR}/H5match_types.c
)
target_include_directories (H5match_types PRIVATE "${HDF5_BINARY_DIR};${HDF5_SRC_DIR};${HDF5_F90_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT ONLY_SHARED_LIBS)
  add_custom_command (
      OUTPUT ${HDF5_F90_BINARY_DIR}/static/H5f90i_gen.h
             ${HDF5_F90_BINARY_DIR}/static/H5fortran_types.F90
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:H5match_types>
      WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}/static
      DEPENDS H5match_types
  )
  set_source_files_properties (${HDF5_F90_BINARY_DIR}/static/H5f90i_gen.h PROPERTIES GENERATED TRUE)
  set_source_files_properties (${HDF5_F90_BINARY_DIR}/static/H5fortran_types.F90 PROPERTIES GENERATED TRUE)
endif ()
if (BUILD_SHARED_LIBS)
  add_custom_command (
      OUTPUT ${HDF5_F90_BINARY_DIR}/shared/H5f90i_gen.h
             ${HDF5_F90_BINARY_DIR}/shared/H5fortran_types.F90
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:H5match_types>
      WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}/shared
      DEPENDS H5match_types
  )
  set_source_files_properties (${HDF5_F90_BINARY_DIR}/shared/H5f90i_gen.h PROPERTIES GENERATED TRUE)
  set_source_files_properties (${HDF5_F90_BINARY_DIR}/shared/H5fortran_types.F90 PROPERTIES GENERATED TRUE)
endif ()

#-----------------------------------------------------------------------------
# f90CStub lib
#-----------------------------------------------------------------------------
set (f90CStub_C_SOURCES
    # normal distribution
    ${HDF5_F90_SRC_SOURCE_DIR}/H5f90kit.c
    ${HDF5_F90_SRC_SOURCE_DIR}/H5_f.c
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Af.c
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Df.c
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Ef.c
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Ff.c
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Gf.c
    ${HDF5_F90_SRC_SOURCE_DIR}/H5If.c
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Lf.c
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Of.c
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Pf.c
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Rf.c
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Sf.c
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Tf.c
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Zf.c
)
set_source_files_properties (${f90CStub_C_SOURCES} PROPERTIES LANGUAGE C)

set (f90CStub_C_HDRS
    # generated files
    ${HDF5_F90_BINARY_DIR}/static/H5f90i_gen.h
)
set (f90CStub_C_SHHDRS
    # generated files
    ${HDF5_F90_BINARY_DIR}/shared/H5f90i_gen.h
)

if (NOT ONLY_SHARED_LIBS)
  add_library (${HDF5_F90_C_LIB_TARGET} STATIC ${f90CStub_C_SOURCES} ${f90CStub_C_HDRS})
  target_include_directories (${HDF5_F90_C_LIB_TARGET}
      PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/static;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  TARGET_C_PROPERTIES (${HDF5_F90_C_LIB_TARGET} STATIC)
  target_link_libraries (${HDF5_F90_C_LIB_TARGET} PUBLIC ${HDF5_LIB_TARGET})
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_F90_C_LIB_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_F90_C_LIB_TARGET} ${HDF5_F90_C_LIB_NAME} STATIC 0)
  set_target_properties (${HDF5_F90_C_LIB_TARGET} PROPERTIES
      FOLDER libraries/fortran
      LINKER_LANGUAGE C
  )
  set (install_targets ${HDF5_F90_C_LIB_TARGET})
endif ()

if (BUILD_SHARED_LIBS)
  add_library (${HDF5_F90_C_LIBSH_TARGET} SHARED ${f90CStub_C_SOURCES} ${f90CStub_C_SHHDRS})
  target_include_directories (${HDF5_F90_C_LIBSH_TARGET}
      PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/shared;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_definitions(${HDF5_F90_C_LIBSH_TARGET}
      PUBLIC "H5_BUILT_AS_DYNAMIC_LIB"
  )
  TARGET_C_PROPERTIES (${HDF5_F90_C_LIBSH_TARGET} SHARED)
  target_link_libraries (${HDF5_F90_C_LIBSH_TARGET} PUBLIC ${HDF5_LIBSH_TARGET})
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_F90_C_LIBSH_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_F90_C_LIBSH_TARGET} ${HDF5_F90_C_LIB_NAME} SHARED "F")
  set_target_properties (${HDF5_F90_C_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/fortran
      LINKER_LANGUAGE C
  )
  set (install_targets ${install_targets} ${HDF5_F90_C_LIBSH_TARGET})
endif ()

#-----------------------------------------------------------------------------
# Fortran Modules
#-----------------------------------------------------------------------------
set (f90_F_GEN_SOURCES
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Aff.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Dff.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Pff.F90
)
if (NOT ONLY_SHARED_LIBS)
  add_custom_command (
      OUTPUT ${HDF5_F90_BINARY_DIR}/static/H5_gen.F90
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:H5_buildiface>
      WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}/static
      DEPENDS ${f90_F_GEN_SOURCES}
      COMMENT "Generating the H5_gen.F90 file"
  )
  add_custom_target (H5gen ALL
      DEPENDS ${HDF5_F90_BINARY_DIR}/static/H5_gen.F90
  )
  set_source_files_properties (${HDF5_F90_BINARY_DIR}/static/H5_gen.F90 PROPERTIES GENERATED TRUE)
endif ()

if (BUILD_SHARED_LIBS)
  add_custom_command (
      OUTPUT ${HDF5_F90_BINARY_DIR}/shared/H5_gen.F90
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:H5_buildiface>
      WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}/shared
      DEPENDS ${f90_F_GEN_SOURCES}
      COMMENT "Generating the H5_gen.F90 shared file"
  )
  add_custom_target (H5genSH ALL
      DEPENDS ${HDF5_F90_BINARY_DIR}/shared/H5_gen.F90
  )
  set_source_files_properties (${HDF5_F90_BINARY_DIR}/shared/H5_gen.F90 PROPERTIES GENERATED TRUE)
endif ()

set (f90_F_BASE_SOURCES
    # normal distribution
    ${HDF5_F90_SRC_SOURCE_DIR}/H5f90global.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5fortkit.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5_ff.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Aff.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Dff.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Eff.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Fff.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Gff.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Iff.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Lff.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Off.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Pff.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Rff.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Sff.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Tff.F90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Zff.F90
)

if (NOT ONLY_SHARED_LIBS)
  set (f90_F_SOURCES
      # generated file
      ${HDF5_F90_BINARY_DIR}/static/H5fortran_types.F90

      ${f90_F_BASE_SOURCES}

      # generated file
      ${HDF5_F90_BINARY_DIR}/static/H5_gen.F90

      # normal distribution
      ${HDF5_F90_SRC_SOURCE_DIR}/HDF5.F90
  )
endif ()
if (BUILD_SHARED_LIBS)
  set (f90_F_SOURCES_SHARED
      # generated file
      ${HDF5_F90_BINARY_DIR}/shared/H5fortran_types.F90

      ${f90_F_BASE_SOURCES}

     # generated file
      ${HDF5_F90_BINARY_DIR}/shared/H5_gen.F90

      # normal distribution
      ${HDF5_F90_SRC_SOURCE_DIR}/HDF5.F90
  )
endif ()

#-----------------------------------------------------------------------------
# Add Main fortran library
#-----------------------------------------------------------------------------
if (NOT ONLY_SHARED_LIBS)
  add_library (${HDF5_F90_LIB_TARGET} STATIC ${f90_F_SOURCES})
  target_include_directories (${HDF5_F90_LIB_TARGET}
      PRIVATE
          "${HDF5_F90_SRC_SOURCE_DIR};${CMAKE_Fortran_MODULE_DIRECTORY}/static;${HDF5_F90_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_INCLUDE_DIRS}>"
      INTERFACE
          "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_definitions(${HDF5_F90_LIB_TARGET}
      PRIVATE
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:HDF5F90_WINDOWS>
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
  )
  target_link_libraries (${HDF5_F90_LIB_TARGET}
      PUBLIC
          ${HDF5_F90_C_LIB_TARGET}
      PRIVATE
          ${LINK_Fortran_LIBS}
          $<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_LIBRARIES}>
  )
#  set_property(TARGET ${HDF5_F90_LIB_TARGET} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#  )
#  set_property(TARGET ${HDF5_F90_LIB_TARGET} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>
#  )
  if(MSVC)
    set_property(TARGET ${HDF5_F90_LIB_TARGET} PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
  endif()
  set_target_properties (${HDF5_F90_LIB_TARGET} PROPERTIES
      FOLDER libraries/fortran
      LINKER_LANGUAGE Fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
  )
  H5_SET_LIB_OPTIONS (${HDF5_F90_LIB_TARGET} ${HDF5_F90_LIB_NAME} STATIC 0)
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_F90_LIB_TARGET}")
  set (install_targets ${install_targets} ${HDF5_F90_LIB_TARGET})
  add_dependencies(${HDF5_F90_LIB_TARGET} H5gen)
endif ()

if (BUILD_SHARED_LIBS)
  add_library (${HDF5_F90_LIBSH_TARGET} SHARED ${f90_F_SOURCES_SHARED})
  target_include_directories (${HDF5_F90_LIBSH_TARGET}
      PRIVATE
          "${HDF5_F90_SRC_SOURCE_DIR};${CMAKE_Fortran_MODULE_DIRECTORY}/shared;${HDF5_F90_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_INCLUDE_DIRS}>"
      INTERFACE
          "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_definitions(${HDF5_F90_LIBSH_TARGET}
      PUBLIC
          "H5_BUILT_AS_DYNAMIC_LIB"
      PRIVATE
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:BUILD_HDF5_DLL;HDF5F90_WINDOWS>
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
  )
  target_link_libraries (${HDF5_F90_LIBSH_TARGET}
      PUBLIC
          ${HDF5_F90_C_LIBSH_TARGET}
      PRIVATE
          ${LINK_Fortran_LIBS} $<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_LIBRARIES}>
  )
#  set_property(TARGET ${HDF5_F90_LIBSH_TARGET} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#  )
#  set_property(TARGET ${HDF5_F90_LIBSH_TARGET} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>
#  )
#  set_property(TARGET ${HDF5_F90_LIBSH_TARGET} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-DLL">
#  )
#  set_property(TARGET ${HDF5_F90_LIBSH_TARGET} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-DEF:${HDF5_F90_SRC_BINARY_DIR}/hdf5_fortrandll.def">
#  )
  if(MSVC)
    set_property(TARGET ${HDF5_F90_LIBSH_TARGET} PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS} /DLL /DEF:${HDF5_F90_SRC_BINARY_DIR}/hdf5_fortrandll.def")
  endif()
  set_target_properties (${HDF5_F90_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/fortran
      LINKER_LANGUAGE Fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_F90_LIBSH_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_F90_LIBSH_TARGET} ${HDF5_F90_LIB_NAME} SHARED "F")
  set (install_targets ${install_targets} ${HDF5_F90_LIBSH_TARGET})
  add_dependencies(${HDF5_F90_LIBSH_TARGET} H5genSH)
endif ()

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
install (
    FILES
        ${HDF5_F90_SRC_SOURCE_DIR}/H5f90.h
        ${HDF5_F90_SRC_SOURCE_DIR}/H5f90i.h
        ${HDF5_F90_SRC_SOURCE_DIR}/H5f90proto.h
    DESTINATION
        ${HDF5_INSTALL_INCLUDE_DIR}
    COMPONENT
        fortheaders
)
if (NOT ONLY_SHARED_LIBS)
  install (
      FILES
          ${HDF5_F90_BINARY_DIR}/static/H5f90i_gen.h
          ${HDF5_F90_BINARY_DIR}/static/H5fortran_types.F90
      DESTINATION
          ${HDF5_INSTALL_INCLUDE_DIR}
      COMPONENT
          fortheaders
  )
else ()
  install (
      FILES
          ${HDF5_F90_BINARY_DIR}/shared/H5f90i_gen.h
          ${HDF5_F90_BINARY_DIR}/shared/H5fortran_types.F90
      DESTINATION
          ${HDF5_INSTALL_INCLUDE_DIR}
      COMPONENT
          fortheaders
  )
endif ()

if (NOT ONLY_SHARED_LIBS)
  set (mod_files
      ${MOD_BUILD_DIR}/h5fortran_types.mod
      ${MOD_BUILD_DIR}/hdf5.mod
      ${MOD_BUILD_DIR}/h5fortkit.mod
      ${MOD_BUILD_DIR}/h5global.mod
      ${MOD_BUILD_DIR}/h5a.mod
      ${MOD_BUILD_DIR}/h5d.mod
      ${MOD_BUILD_DIR}/h5e.mod
      ${MOD_BUILD_DIR}/h5f.mod
      ${MOD_BUILD_DIR}/h5g.mod
      ${MOD_BUILD_DIR}/h5i.mod
      ${MOD_BUILD_DIR}/h5l.mod
      ${MOD_BUILD_DIR}/h5lib.mod
      ${MOD_BUILD_DIR}/h5o.mod
      ${MOD_BUILD_DIR}/h5p.mod
      ${MOD_BUILD_DIR}/h5r.mod
      ${MOD_BUILD_DIR}/h5s.mod
      ${MOD_BUILD_DIR}/h5t.mod
      ${MOD_BUILD_DIR}/h5z.mod
      ${MOD_BUILD_DIR}/h5_gen.mod
  )
  install (
      FILES
          ${mod_files}
      DESTINATION
          ${HDF5_INSTALL_INCLUDE_DIR}/static
      COMPONENT
          fortheaders
  )
endif ()

if (BUILD_SHARED_LIBS)
  set (modsh_files
      ${MODSH_BUILD_DIR}/h5fortran_types.mod
      ${MODSH_BUILD_DIR}/hdf5.mod
      ${MODSH_BUILD_DIR}/h5fortkit.mod
      ${MODSH_BUILD_DIR}/h5global.mod
      ${MODSH_BUILD_DIR}/h5a.mod
      ${MODSH_BUILD_DIR}/h5d.mod
      ${MODSH_BUILD_DIR}/h5e.mod
      ${MODSH_BUILD_DIR}/h5f.mod
      ${MODSH_BUILD_DIR}/h5g.mod
      ${MODSH_BUILD_DIR}/h5i.mod
      ${MODSH_BUILD_DIR}/h5l.mod
      ${MODSH_BUILD_DIR}/h5lib.mod
      ${MODSH_BUILD_DIR}/h5o.mod
      ${MODSH_BUILD_DIR}/h5p.mod
      ${MODSH_BUILD_DIR}/h5r.mod
      ${MODSH_BUILD_DIR}/h5s.mod
      ${MODSH_BUILD_DIR}/h5t.mod
      ${MODSH_BUILD_DIR}/h5z.mod
      ${MODSH_BUILD_DIR}/h5_gen.mod
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
    INSTALL_TARGET_PDB (${HDF5_F90_C_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} fortlibraries)
    #INSTALL_TARGET_PDB (${HDF5_F90_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} fortlibraries)
  endif ()
  if (NOT ONLY_SHARED_LIBS)
    INSTALL_TARGET_PDB (${HDF5_F90_C_LIB_TARGET} ${HDF5_INSTALL_LIB_DIR} fortlibraries)
    #INSTALL_TARGET_PDB (${HDF5_F90_LIB_TARGET} ${HDF5_INSTALL_LIB_DIR} fortlibraries)
  endif ()

  install (
      TARGETS
          ${install_targets}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT fortlibraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT fortlibraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT fortlibraries
      FRAMEWORK DESTINATION ${HDF5_INSTALL_FWRK_DIR} COMPONENT fortlibraries
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
set (_PKG_CONFIG_LIBNAME "${HDF5_F90_LIB_CORENAME}")
set (_PKG_CONFIG_VERSION "${HDF5_PACKAGE_VERSION}")

set (_PKG_CONFIG_LIBS_PRIVATE)

if (NOT ONLY_SHARED_LIBS)
  set (_PKG_CONFIG_LIBS "${_PKG_CONFIG_LIBS} -l${HDF5_F90_LIB_CORENAME}")
endif ()
if (BUILD_SHARED_LIBS)
  set (_PKG_CONFIG_SH_LIBS "${_PKG_CONFIG_SH_LIBS} -l${HDF5_F90_LIB_CORENAME}")
endif ()

set (_PKG_CONFIG_REQUIRES "${HDF5_LIB_CORENAME}")
set (_PKG_CONFIG_REQUIRES_PRIVATE "${HDF5_LIB_CORENAME}")

configure_file (
    ${HDF_RESOURCES_DIR}/libhdf5.pc.in
    ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_F90_LIB_CORENAME}-${HDF5_PACKAGE_VERSION}.pc
    @ONLY
)
install (
    FILES ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_F90_LIB_CORENAME}-${HDF5_PACKAGE_VERSION}.pc
    DESTINATION ${HDF5_INSTALL_LIB_DIR}/pkgconfig
    COMPONENT fortlibraries
)

if (NOT WIN32 AND NOT MINGW)
  set (_PKG_CONFIG_COMPILER ${CMAKE_Fortran_COMPILER})
  configure_file (
      ${HDF_RESOURCES_DIR}/libh5cc.in
      ${HDF5_BINARY_DIR}/CMakeFiles/h5fc
      @ONLY
  )
  install (
      FILES ${HDF5_BINARY_DIR}/CMakeFiles/h5fc
      DESTINATION ${HDF5_INSTALL_BIN_DIR}
      PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
      COMPONENT fortlibraries
  )
endif ()


