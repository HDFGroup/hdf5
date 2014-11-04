cmake_minimum_required (VERSION 2.8.11)
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
  set (H5_TEST_KIND_NAME "h5test_kind_storage_size_mod")
else (FORTRAN_HAVE_STORAGE_SIZE)
    if (FORTRAN_HAVE_SIZEOF)
      add_executable (H5test_FortranHavekind
          ${HDF5_F90_SRC_SOURCE_DIR}/H5test_kind_SIZEOF.f90
      )
      set (H5_TEST_KIND_NAME "h5test_kind_sizeof_mod")
    else (FORTRAN_HAVE_SIZEOF)
      add_executable (H5test_FortranHavekind
          ${HDF5_F90_SRC_SOURCE_DIR}/H5test_kind.f90
      )
    endif (FORTRAN_HAVE_SIZEOF)
endif (FORTRAN_HAVE_STORAGE_SIZE)
if (WIN32 AND MSVC)
  if (BUILD_SHARED_LIBS)
    set_target_properties (H5test_FortranHavekind
        PROPERTIES
            COMPILE_FLAGS "/MT"
    )
  endif (BUILD_SHARED_LIBS)
  set_target_properties (H5test_FortranHavekind
      PROPERTIES
          LINK_FLAGS "/SUBSYSTEM:CONSOLE"
  )
endif (WIN32 AND MSVC)
set_target_properties (H5test_FortranHavekind PROPERTIES LINKER_LANGUAGE Fortran)

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

add_library (${HDF5_F90_C_LIB_TARGET} ${LIB_TYPE} ${f90CStub_C_SRCS} ${f90CStub_C_HDRS})
TARGET_C_PROPERTIES (${HDF5_F90_C_LIB_TARGET} " " " ")
target_link_libraries (${HDF5_F90_C_LIB_TARGET} ${HDF5_LIB_TARGET} ${LINK_LIBS})
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_F90_C_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_F90_C_LIB_TARGET} ${HDF5_F90_C_LIB_NAME} ${LIB_TYPE})
set_target_properties (${HDF5_F90_C_LIB_TARGET} PROPERTIES
    FOLDER libraries/fortran
    LINKER_LANGUAGE C
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)

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

#-----------------------------------------------------------------------------
# Add Main fortran library
#-----------------------------------------------------------------------------
add_library (${HDF5_F90_LIB_TARGET} ${LIB_TYPE} ${f90_F_SRCS})
set (SHARED_LINK_FLAGS " ")
if (WIN32)
  if (BUILD_SHARED_LIBS)
    set_property (TARGET ${HDF5_F90_LIB_TARGET}
      APPEND PROPERTY COMPILE_DEFINITIONS
        BUILD_HDF5_DLL
    )
    if (MSVC)
      set (SHARED_LINK_FLAGS "/DLL /DEF:${HDF5_F90_SRC_BINARY_DIR}/hdf5_fortrandll.def")
    endif (MSVC)
  endif (BUILD_SHARED_LIBS)
  set_property (TARGET ${HDF5_F90_LIB_TARGET}
      APPEND PROPERTY COMPILE_DEFINITIONS
        HDF5F90_WINDOWS
  )
endif (WIN32)
TARGET_FORTRAN_PROPERTIES (${HDF5_F90_LIB_TARGET} " " ${SHARED_LINK_FLAGS})
set_target_properties (${HDF5_F90_LIB_TARGET} PROPERTIES LINKER_LANGUAGE Fortran)
target_link_libraries (${HDF5_F90_LIB_TARGET} ${HDF5_F90_C_LIB_TARGET} ${HDF5_LIB_TARGET})
if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  target_link_libraries (${HDF5_F90_LIB_TARGET} ${MPI_Fortran_LIBRARIES})
endif (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_F90_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_F90_LIB_TARGET} ${HDF5_F90_LIB_NAME} ${LIB_TYPE})
set_target_properties (${HDF5_F90_LIB_TARGET} PROPERTIES
    FOLDER libraries/fortran
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)

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

if (WIN32)
  install (
      FILES
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5fortran_types.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/hdf5.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5global.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5a.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5d.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5e.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5f.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5g.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5i.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5l.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5lib.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5o.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5p.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5r.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5s.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5t.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5z.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5a_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5d_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5e_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5f_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5l_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5lib_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5o_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5p_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5r_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5t_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/h5_dble_interface.mod
      DESTINATION
          ${HDF5_INSTALL_INCLUDE_DIR}
      COMPONENT
          fortheaders
  )
  if (H5_TEST_KIND_NAME)
    install (
        FILES
            ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/${H5_TEST_KIND_NAME}.mod
        DESTINATION
            ${HDF5_INSTALL_INCLUDE_DIR}
        COMPONENT
            fortheaders
    )
  endif (H5_TEST_KIND_NAME)
else (WIN32)
  install (
      FILES
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5fortran_types.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/hdf5.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5global.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5f.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5a.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5d.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5e.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5f.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5g.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5i.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5l.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5lib.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5o.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5p.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5r.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5s.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5t.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5z.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5a_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5d_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5e_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5e_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5l_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5lib_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5o_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5p_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5r_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5t_provisional.mod
          ${CMAKE_Fortran_MODULE_DIRECTORY}/h5_dble_interface.mod
      DESTINATION
          ${HDF5_INSTALL_INCLUDE_DIR}
      COMPONENT
          fortheaders
  )
  if (H5_TEST_KIND_NAME)
    install (
        FILES
          ${CMAKE_Fortran_MODULE_DIRECTORY}/${H5_TEST_KIND_NAME}.mod
        DESTINATION
            ${HDF5_INSTALL_INCLUDE_DIR}
        COMPONENT
            fortheaders
    )
  endif (H5_TEST_KIND_NAME)
endif (WIN32)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
if (HDF5_EXPORTED_TARGETS)
  if (BUILD_SHARED_LIBS)
    INSTALL_TARGET_PDB (${HDF5_F90_C_LIB_TARGET} ${HDF5_INSTALL_LIB_DIR} fortlibraries)
    #INSTALL_TARGET_PDB (${HDF5_F90_LIB_TARGET} ${HDF5_INSTALL_LIB_DIR} fortlibraries)
  endif (BUILD_SHARED_LIBS)

  install (
      TARGETS
          ${HDF5_F90_C_LIB_TARGET}
          ${HDF5_F90_LIB_TARGET}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT fortlibraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT fortlibraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT fortlibraries
  )
endif (HDF5_EXPORTED_TARGETS)
