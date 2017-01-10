cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_F90_SRC C CXX Fortran)

#-----------------------------------------------------------------------------
# configure def file for shared libs on windows
if (WIN32)
  if (BUILD_SHARED_LIBS)
    if (MSVC)
      if (NOT H5_HAVE_PARALLEL)
        set (H5_NOPAREXP ";")
      endif ()
      if (NOT HDF5_ENABLE_F2003)
        set (H5_NOF03EXP ";")
      else ()
        set (H5_F03EXP ";")
      endif ()
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

configure_file (${HDF5_F90_SRC_SOURCE_DIR}/H5config_f.inc.cmake ${CMAKE_BINARY_DIR}/H5config_f.inc @ONLY)
configure_file (${HDF5_F90_SRC_SOURCE_DIR}/H5fort_type_defines.h.in ${HDF5_F90_BINARY_DIR}/H5fort_type_defines.h @ONLY)

set (Fortran_COMPILER_ID CMAKE_Fortran_COMPILER_ID)

#-----------------------------------------------------------------------------
# Setup the Fortran auto-detection utilities
# H5_buildiface.F90      used to generate various KIND interfaces
# H5fort_type_defines.h   used to generate H5f90i_gen.h + H5fortran_types.F90
#-----------------------------------------------------------------------------

add_executable (H5_buildiface
      ${HDF5_F90_SRC_SOURCE_DIR}/H5_buildiface.F90
  )

if (WIN32 AND MSVC)
  if (BUILD_SHARED_LIBS)
    set_target_properties (H5_buildiface
        PROPERTIES
            COMPILE_FLAGS "/MT"
    )
  endif ()
  set_target_properties (H5_buildiface
      PROPERTIES
          LINK_FLAGS "/SUBSYSTEM:CONSOLE"
  )
endif ()
set_target_properties (H5_buildiface PROPERTIES
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  file (MAKE_DIRECTORY "${HDF5_F90_BINARY_DIR}/shared")
  if (WIN32)
    set (MODSH_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/shared/${CMAKE_BUILD_TYPE})
  else ()
    set (MODSH_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
  endif ()
endif ()
if (WIN32)
  set (MOD_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/static/${CMAKE_BUILD_TYPE})
else ()
  set (MOD_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
endif ()

INCLUDE_DIRECTORIES (${HDF5_F90_BINARY_DIR} ${CMAKE_Fortran_MODULE_DIRECTORY})

#-----------------------------------------------------------------------------
add_executable (H5match_types
    ${HDF5_F90_BINARY_DIR}/H5fort_type_defines.h
    ${HDF5_F90_SRC_SOURCE_DIR}/H5match_types.c
)
set (CMD $<TARGET_FILE:H5match_types>)
add_custom_command (
    OUTPUT ${HDF5_F90_BINARY_DIR}/H5f90i_gen.h
           ${HDF5_F90_BINARY_DIR}/H5fortran_types.F90
    COMMAND ${CMD}
    WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}
    DEPENDS H5match_types
)
set_source_files_properties (${HDF5_F90_BINARY_DIR}/H5f90i_gen.h PROPERTIES GENERATED TRUE)
set_source_files_properties (${HDF5_F90_BINARY_DIR}/H5fortran_types.F90 PROPERTIES GENERATED TRUE)

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
    ${HDF5_F90_BINARY_DIR}/H5f90i_gen.h
)

add_library (${HDF5_F90_C_LIB_TARGET} STATIC ${f90CStub_C_SOURCES} ${f90CStub_C_HDRS})
TARGET_C_PROPERTIES (${HDF5_F90_C_LIB_TARGET} STATIC " " " ")
target_link_libraries (${HDF5_F90_C_LIB_TARGET} ${HDF5_LIB_TARGET} ${LINK_LIBS})
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_F90_C_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_F90_C_LIB_TARGET} ${HDF5_F90_C_LIB_NAME} STATIC)
set_target_properties (${HDF5_F90_C_LIB_TARGET} PROPERTIES
    FOLDER libraries/fortran
    LINKER_LANGUAGE C
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)
set (install_targets ${HDF5_F90_C_LIB_TARGET})

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_library (${HDF5_F90_C_LIBSH_TARGET} SHARED ${f90CStub_C_SOURCES} ${f90CStub_C_HDRS})
  TARGET_C_PROPERTIES (${HDF5_F90_C_LIBSH_TARGET} SHARED " " " ")
  target_link_libraries (${HDF5_F90_C_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} ${LINK_SHARED_LIBS})
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_F90_C_LIBSH_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_F90_C_LIBSH_TARGET} ${HDF5_F90_C_LIB_NAME} SHARED ${HDF5_F_PACKAGE_SOVERSION})
  set_target_properties (${HDF5_F90_C_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/fortran
      LINKER_LANGUAGE C
      COMPILE_DEFINITIONS "H5_BUILT_AS_DYNAMIC_LIB"
      INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
      INTERFACE_COMPILE_DEFINITIONS H5_BUILT_AS_DYNAMIC_LIB=1
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
set (CMD $<TARGET_FILE:H5_buildiface>)
add_custom_command (
    OUTPUT ${HDF5_F90_BINARY_DIR}/H5_gen.F90
    COMMAND ${CMD}
    WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}
    DEPENDS ${f90_F_GEN_SOURCES}
    COMMENT "Generating the H5_gen.F90 file"
)
add_custom_target (H5gen ALL
    DEPENDS ${HDF5_F90_BINARY_DIR}/H5_gen.F90
)
set_source_files_properties (${HDF5_F90_BINARY_DIR}/H5_gen.F90 PROPERTIES GENERATED TRUE)

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  set (CMDSH $<TARGET_FILE:H5_buildiface>)
  add_custom_command (
      OUTPUT ${HDF5_F90_BINARY_DIR}/shared/H5_gen.F90
      COMMAND ${CMDSH}
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

set (f90_F_SOURCES
    # generated file
    ${HDF5_F90_BINARY_DIR}/H5fortran_types.F90

    ${f90_F_BASE_SOURCES}

    # generated file
    ${HDF5_F90_BINARY_DIR}/H5_gen.F90

    # normal distribution
    ${HDF5_F90_SRC_SOURCE_DIR}/HDF5.F90
)
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  set (f90_F_SOURCES_SHARED
      # generated file
      ${HDF5_F90_BINARY_DIR}/H5fortran_types.F90

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
add_library (${HDF5_F90_LIB_TARGET} STATIC ${f90_F_SOURCES})
set (SHARED_LINK_FLAGS " ")
TARGET_FORTRAN_PROPERTIES (${HDF5_F90_LIB_TARGET} STATIC " " " ")
target_link_libraries (${HDF5_F90_LIB_TARGET} ${HDF5_F90_C_LIB_TARGET} ${HDF5_LIB_TARGET})
if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  target_link_libraries (${HDF5_F90_LIB_TARGET} ${MPI_Fortran_LIBRARIES})
endif ()
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_F90_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_F90_LIB_TARGET} ${HDF5_F90_LIB_NAME} STATIC)
set_target_properties (${HDF5_F90_LIB_TARGET} PROPERTIES
    FOLDER libraries/fortran
    LINKER_LANGUAGE Fortran
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
)
if (WIN32)
  set_property (TARGET ${HDF5_F90_LIB_TARGET}
      APPEND PROPERTY COMPILE_DEFINITIONS "HDF5F90_WINDOWS"
  )
endif ()
set (install_targets ${install_targets} ${HDF5_F90_LIB_TARGET})
add_dependencies(${HDF5_F90_LIB_TARGET} H5gen)

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_library (${HDF5_F90_LIBSH_TARGET} SHARED ${f90_F_SOURCES_SHARED})
  set (SHARED_LINK_FLAGS " ")
  if (WIN32 AND MSVC)
    set (SHARED_LINK_FLAGS "/DLL /DEF:${HDF5_F90_SRC_BINARY_DIR}/hdf5_fortrandll.def")
  endif ()
  TARGET_FORTRAN_PROPERTIES (${HDF5_F90_LIBSH_TARGET} SHARED " " ${SHARED_LINK_FLAGS})
  target_link_libraries (${HDF5_F90_LIBSH_TARGET} ${HDF5_F90_C_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
    target_link_libraries (${HDF5_F90_LIBSH_TARGET} ${MPI_Fortran_LIBRARIES})
  endif ()
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_F90_LIBSH_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_F90_LIBSH_TARGET} ${HDF5_F90_LIB_NAME} SHARED ${HDF5_F_PACKAGE_SOVERSION})
  set_target_properties (${HDF5_F90_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/fortran
      LINKER_LANGUAGE Fortran
      COMPILE_DEFINITIONS "H5_BUILT_AS_DYNAMIC_LIB"
      INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
      INTERFACE_COMPILE_DEFINITIONS H5_BUILT_AS_DYNAMIC_LIB=1
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  if (WIN32)
    set_property (TARGET ${HDF5_F90_LIBSH_TARGET}
      APPEND PROPERTY COMPILE_DEFINITIONS "BUILD_HDF5_DLL;HDF5F90_WINDOWS"
    )
  endif ()
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
        ${HDF5_F90_BINARY_DIR}/H5f90i_gen.h
        ${HDF5_F90_BINARY_DIR}/H5fortran_types.F90
    DESTINATION
        ${HDF5_INSTALL_INCLUDE_DIR}
    COMPONENT
        fortheaders
)

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

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
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
  if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
    INSTALL_TARGET_PDB (${HDF5_F90_C_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} fortlibraries)
    #INSTALL_TARGET_PDB (${HDF5_F90_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} fortlibraries)
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
