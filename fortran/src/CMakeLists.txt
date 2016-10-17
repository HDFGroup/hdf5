cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_F90_SRC C CXX Fortran)

#-----------------------------------------------------------------------------
# configure def file for shared libs on windows
if (WIN32)
  if (BUILD_SHARED_LIBS)
    if (MSVC)
      if (NOT H5_HAVE_PARALLEL)
        set (H5_NOPAREXP ";")
      endif (NOT H5_HAVE_PARALLEL)
      if (NOT HDF5_ENABLE_F2003)
        set (H5_NOF03EXP ";")
      else (NOT HDF5_ENABLE_F2003)
        set (H5_F03EXP ";")
      endif (NOT HDF5_ENABLE_F2003)
      configure_file (${HDF5_F90_SRC_SOURCE_DIR}/hdf5_fortrandll.def.in ${HDF5_F90_SRC_BINARY_DIR}/hdf5_fortrandll.def @ONLY)
    endif (MSVC)
  endif (BUILD_SHARED_LIBS)
endif (WIN32)

#-----------------------------------------------------------------------------
# Setup the Fortran auto-detection utilities
# H5test_kind(_SIZEOF,_STORAGE_SIZE).f90 used to generate H5fortran_detect.f90
# H5fortran_detect.f90     used to generate H5fort_type_defines.h
# H5fort_type_defines.h    used to generate H5f90i_gen.h + H5fortran_types.f90
#-----------------------------------------------------------------------------
if (FORTRAN_HAVE_STORAGE_SIZE)
  add_executable (H5test_FortranHavekind
      ${HDF5_F90_SRC_SOURCE_DIR}/H5test_kind_STORAGE_SIZE.f90
  )
  if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
    add_executable (H5test_FortranHavekind-shared
        ${HDF5_F90_SRC_SOURCE_DIR}/H5test_kind_STORAGE_SIZE.f90
    )
  endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  set (H5_TEST_KIND_NAME "h5test_kind_storage_size_mod")
else (FORTRAN_HAVE_STORAGE_SIZE)
    if (FORTRAN_HAVE_SIZEOF)
      add_executable (H5test_FortranHavekind
          ${HDF5_F90_SRC_SOURCE_DIR}/H5test_kind_SIZEOF.f90
      )
      if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
        add_executable (H5test_FortranHavekind-shared
            ${HDF5_F90_SRC_SOURCE_DIR}/H5test_kind_SIZEOF.f90
        )
      endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
      set (H5_TEST_KIND_NAME "h5test_kind_sizeof_mod")
    else (FORTRAN_HAVE_SIZEOF)
      add_executable (H5test_FortranHavekind
          ${HDF5_F90_SRC_SOURCE_DIR}/H5test_kind.f90
      )
      if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
        add_executable (H5test_FortranHavekind-shared
            ${HDF5_F90_SRC_SOURCE_DIR}/H5test_kind.f90
        )
      endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
    endif (FORTRAN_HAVE_SIZEOF)
endif (FORTRAN_HAVE_STORAGE_SIZE)
if (WIN32 AND MSVC)
  if (BUILD_SHARED_LIBS)
    set_target_properties (H5test_FortranHavekind-shared
        PROPERTIES
            COMPILE_FLAGS "/MT"
    )
  endif (BUILD_SHARED_LIBS)
  set_target_properties (H5test_FortranHavekind
      PROPERTIES
          LINK_FLAGS "/SUBSYSTEM:CONSOLE"
  )
endif (WIN32 AND MSVC)
set_target_properties (H5test_FortranHavekind PROPERTIES
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
)
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  set_target_properties (H5test_FortranHavekind-shared PROPERTIES
      LINKER_LANGUAGE Fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

set (CMD $<TARGET_FILE:H5test_FortranHavekind>)
add_custom_command (
    OUTPUT ${HDF5_F90_BINARY_DIR}/H5fortran_detect.f90
    COMMAND ${CMD}
    ARGS > ${HDF5_F90_BINARY_DIR}/H5fortran_detect.f90
    WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}
    DEPENDS H5test_FortranHavekind
)
#-----------------------------------------------------------------------------
add_executable (H5fortran_detect
    ${HDF5_F90_BINARY_DIR}/H5fortran_detect.f90
)
if (WIN32 AND MSVC)
  if (BUILD_SHARED_LIBS)
    set_target_properties (H5fortran_detect
        PROPERTIES
            COMPILE_FLAGS "/MT"
    )
  endif (BUILD_SHARED_LIBS)
  set_target_properties (H5fortran_detect
      PROPERTIES
          LINK_FLAGS "/SUBSYSTEM:CONSOLE"
  )
endif (WIN32 AND MSVC)
set_target_properties (H5fortran_detect PROPERTIES LINKER_LANGUAGE Fortran)

set (CMD $<TARGET_FILE:H5fortran_detect>)
add_custom_command (
    OUTPUT ${HDF5_F90_BINARY_DIR}/H5fort_type_defines.h
    COMMAND ${CMD}
    ARGS > ${HDF5_F90_BINARY_DIR}/H5fort_type_defines.h
    WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}
    DEPENDS H5fortran_detect
)

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  file (MAKE_DIRECTORY "${HDF5_F90_BINARY_DIR}/shared")
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

INCLUDE_DIRECTORIES (${HDF5_F90_BINARY_DIR} ${CMAKE_Fortran_MODULE_DIRECTORY})

#-----------------------------------------------------------------------------
add_executable (H5match_types
    ${HDF5_F90_BINARY_DIR}/H5fort_type_defines.h
    ${HDF5_F90_SRC_SOURCE_DIR}/H5match_types.c
)
set (CMD $<TARGET_FILE:H5match_types>)
add_custom_command (
    OUTPUT ${HDF5_F90_BINARY_DIR}/H5f90i_gen.h
           ${HDF5_F90_BINARY_DIR}/H5fortran_types.f90
    COMMAND ${CMD}
    WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}
    DEPENDS H5match_types
)

#-----------------------------------------------------------------------------
# f90CStub lib
#-----------------------------------------------------------------------------
set (f90CStub_C_SRCS
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

if (H5_HAVE_PARALLEL)
  set (f90CStub_C_SRCS
      ${f90CStub_C_SRCS}
      ${HDF5_F90_SRC_SOURCE_DIR}/H5FDmpiof.c
  )
endif (H5_HAVE_PARALLEL)
set_source_files_properties (${f90CStub_C_SRCS} PROPERTIES LANGUAGE C)

set (f90CStub_C_HDRS
    # generated files
    ${HDF5_F90_BINARY_DIR}/H5f90i_gen.h
)

add_library (${HDF5_F90_C_LIB_TARGET} STATIC ${f90CStub_C_SRCS} ${f90CStub_C_HDRS})
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
  add_library (${HDF5_F90_C_LIBSH_TARGET} SHARED ${f90CStub_C_SRCS} ${f90CStub_C_HDRS})
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
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

#-----------------------------------------------------------------------------
# Fortran 2003 standard
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_F2003)
  # F2003 features are enabled
  set (F_STATUS "_F03")
else (HDF5_ENABLE_F2003)
  # F2003 features are not enabled
  set (F_STATUS "_F90")
endif (HDF5_ENABLE_F2003)

#-----------------------------------------------------------------------------
# Fortran Real Size
#-----------------------------------------------------------------------------
if (FORTRAN_DEFAULT_REAL_NOT_DOUBLE)
  # default real is 4 bytes, so include double signatures
  set (F_DBLE "Include")
else (FORTRAN_DEFAULT_REAL_NOT_DOUBLE)
  # default real is 8 bytes, so exclude double signatures
  set (F_DBLE "Exclude")
endif (FORTRAN_DEFAULT_REAL_NOT_DOUBLE)

#-----------------------------------------------------------------------------
# Fortran Modules
#-----------------------------------------------------------------------------
set (f90_F_SRCS
    # generated files
    ${HDF5_F90_BINARY_DIR}/H5fortran_types.f90

    # normal distribution
    ${HDF5_F90_SRC_SOURCE_DIR}/H5f90global.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5_ff${F_STATUS}.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5_ff.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Aff.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Dff.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Eff.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Fff.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Gff.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Iff.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Lff.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Off.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Pff.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Rff.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Sff.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Tff.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Zff.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Aff${F_STATUS}.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Dff${F_STATUS}.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Eff${F_STATUS}.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Fff${F_STATUS}.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Lff${F_STATUS}.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Off${F_STATUS}.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Pff${F_STATUS}.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Rff${F_STATUS}.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5Tff${F_STATUS}.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5_DBLE_Interface${F_DBLE}.f90
)

#-----------------------------------------------------------------------------
# Add H5FDMPIO if parallel
#-----------------------------------------------------------------------------
if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  set (f90_F_SRCS
    ${f90_F_SRCS}
    ${HDF5_F90_SRC_SOURCE_DIR}/HDF5mpio.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5FDmpioff.f90
  )
else (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  set (f90_F_SRCS
    ${f90_F_SRCS}
    ${HDF5_F90_SRC_SOURCE_DIR}/HDF5.f90
  )
endif (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
set_source_files_properties (${f90_F_SRCS} PROPERTIES LANGUAGE Fortran)

#-----------------------------------------------------------------------------
# Add Main fortran library
#-----------------------------------------------------------------------------
add_library (${HDF5_F90_LIB_TARGET} STATIC ${f90_F_SRCS})
set (SHARED_LINK_FLAGS " ")
TARGET_FORTRAN_PROPERTIES (${HDF5_F90_LIB_TARGET} STATIC " " " ")
target_link_libraries (${HDF5_F90_LIB_TARGET} ${HDF5_F90_C_LIB_TARGET} ${HDF5_LIB_TARGET})
if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  target_link_libraries (${HDF5_F90_LIB_TARGET} ${MPI_Fortran_LIBRARIES})
endif (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
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
endif (WIN32)
set (install_targets ${install_targets} ${HDF5_F90_LIB_TARGET})

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_library (${HDF5_F90_LIBSH_TARGET} SHARED ${f90_F_SRCS})
  set (SHARED_LINK_FLAGS " ")
  if (WIN32 AND MSVC)
    set (SHARED_LINK_FLAGS "/DLL /DEF:${HDF5_F90_SRC_BINARY_DIR}/hdf5_fortrandll.def")
  endif (WIN32 AND MSVC)
  TARGET_FORTRAN_PROPERTIES (${HDF5_F90_LIBSH_TARGET} SHARED " " ${SHARED_LINK_FLAGS})
  target_link_libraries (${HDF5_F90_LIBSH_TARGET} ${HDF5_F90_C_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
    target_link_libraries (${HDF5_F90_LIBSH_TARGET} ${MPI_Fortran_LIBRARIES})
  endif (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
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
  endif (WIN32)
  set (install_targets ${install_targets} ${HDF5_F90_LIBSH_TARGET})
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
install (
    FILES
        ${HDF5_F90_SRC_SOURCE_DIR}/H5f90.h
        ${HDF5_F90_SRC_SOURCE_DIR}/H5f90i.h
        ${HDF5_F90_SRC_SOURCE_DIR}/H5f90proto.h
        ${HDF5_F90_BINARY_DIR}/H5f90i_gen.h
        ${HDF5_F90_BINARY_DIR}/H5fortran_types.f90
    DESTINATION
        ${HDF5_INSTALL_INCLUDE_DIR}
    COMPONENT
        fortheaders
)

set (mod_files
    ${MOD_BUILD_DIR}/h5fortran_types.mod
    ${MOD_BUILD_DIR}/hdf5.mod
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
    ${MOD_BUILD_DIR}/h5a_provisional.mod
    ${MOD_BUILD_DIR}/h5d_provisional.mod
    ${MOD_BUILD_DIR}/h5e_provisional.mod
    ${MOD_BUILD_DIR}/h5f_provisional.mod
    ${MOD_BUILD_DIR}/h5l_provisional.mod
    ${MOD_BUILD_DIR}/h5lib_provisional.mod
    ${MOD_BUILD_DIR}/h5o_provisional.mod
    ${MOD_BUILD_DIR}/h5p_provisional.mod
    ${MOD_BUILD_DIR}/h5r_provisional.mod
    ${MOD_BUILD_DIR}/h5t_provisional.mod
    ${MOD_BUILD_DIR}/h5_dble_interface.mod
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
      ${MODSH_BUILD_DIR}/h5a_provisional.mod
      ${MODSH_BUILD_DIR}/h5d_provisional.mod
      ${MODSH_BUILD_DIR}/h5e_provisional.mod
      ${MODSH_BUILD_DIR}/h5f_provisional.mod
      ${MODSH_BUILD_DIR}/h5l_provisional.mod
      ${MODSH_BUILD_DIR}/h5lib_provisional.mod
      ${MODSH_BUILD_DIR}/h5o_provisional.mod
      ${MODSH_BUILD_DIR}/h5p_provisional.mod
      ${MODSH_BUILD_DIR}/h5r_provisional.mod
      ${MODSH_BUILD_DIR}/h5t_provisional.mod
      ${MODSH_BUILD_DIR}/h5_dble_interface.mod
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
    INSTALL_TARGET_PDB (${HDF5_F90_C_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} fortlibraries)
    #INSTALL_TARGET_PDB (${HDF5_F90_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} fortlibraries)
  endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

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
endif (HDF5_EXPORTED_TARGETS)
