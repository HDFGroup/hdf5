cmake_minimum_required (VERSION 3.12)
project (HDF5_F90_SRC C Fortran)

#-----------------------------------------------------------------------------
# configure def file for shared libs on windows
if (WIN32)
  if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
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

set (Fortran_COMPILER_ID CMAKE_Fortran_COMPILER_ID)

#-----------------------------------------------------------------------------
# Setup the Fortran auto-detection utilities
# H5test_kind(_SIZEOF,_STORAGE_SIZE).f90 used to generate H5fortran_detect.f90
# H5fortran_detect.f90     used to generate H5fort_type_defines.h
# H5fort_type_defines.h    used to generate H5f90i_gen.h + H5fortran_types.f90
#-----------------------------------------------------------------------------
if (H5_FORTRAN_HAVE_STORAGE_SIZE)
  add_executable (H5test_FortranHavekind ${HDF5_F90_SRC_SOURCE_DIR}/H5test_kind_STORAGE_SIZE.f90)
  set (H5_TEST_KIND_NAME "h5test_kind_storage_size_mod")
else ()
    if (H5_FORTRAN_HAVE_SIZEOF)
      add_executable (H5test_FortranHavekind ${HDF5_F90_SRC_SOURCE_DIR}/H5test_kind_SIZEOF.f90)
      set (H5_TEST_KIND_NAME "h5test_kind_sizeof_mod")
    else ()
      add_executable (H5test_FortranHavekind ${HDF5_F90_SRC_SOURCE_DIR}/H5test_kind.f90)
    endif ()
endif ()

set_target_properties (H5test_FortranHavekind PROPERTIES
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)
#set_property(TARGET H5test_FortranHavekind APPEND PROPERTY LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">)
#target_compile_definitions(H5test_FortranHavekind PRIVATE $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:/MT>)
if (MSVC)
  set_target_properties (H5test_FortranHavekind PROPERTIES
      LINK_FLAGS "/SUBSYSTEM:CONSOLE"
      COMPILE_FLAGS "/MT"
  )
endif ()
target_include_directories(H5test_FortranHavekind PRIVATE "${HDF5_SRC_BINARY_DIR};${HDF5_F90_BINARY_DIR}")

add_custom_command (
    OUTPUT ${HDF5_F90_BINARY_DIR}/H5fortran_detect.f90
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR}$<TARGET_FILE:H5test_FortranHavekind>
    ARGS > ${HDF5_F90_BINARY_DIR}/H5fortran_detect.f90
    WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}
    DEPENDS H5test_FortranHavekind
)
#-----------------------------------------------------------------------------
add_executable (H5fortran_detect ${HDF5_F90_BINARY_DIR}/H5fortran_detect.f90)
target_include_directories(H5fortran_detect PRIVATE "${HDF5_SRC_BINARY_DIR};${HDF5_F90_BINARY_DIR}")
#if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
#  target_compile_definitions(H5fortran_detect PRIVATE $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:/MT>)
#endif ()
if(MSVC)
  set_property(TARGET H5fortran_detect PROPERTY COMPILE_FLAGS "/MT")
endif()
#set_property(TARGET H5fortran_detect APPEND PROPERTY LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">)
if(MSVC)
  set_property(TARGET H5fortran_detect PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE")
endif()

set_target_properties (H5fortran_detect PROPERTIES
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

add_custom_command (
    OUTPUT ${HDF5_F90_BINARY_DIR}/H5fort_type_defines.h
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR}$<TARGET_FILE:H5fortran_detect>
    ARGS > ${HDF5_F90_BINARY_DIR}/H5fort_type_defines.h
    WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}
    DEPENDS H5fortran_detect
)

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
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
target_include_directories (H5match_types PRIVATE "${HDF5_SRC_BINARY_DIR};${HDF5_SRC_DIR};${HDF5_F90_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT ONLY_SHARED_LIBS)
  add_custom_command (
      OUTPUT ${HDF5_F90_BINARY_DIR}/static/H5f90i_gen.h
             ${HDF5_F90_BINARY_DIR}/static/H5fortran_types.f90
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:H5match_types>
      WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}/static
      DEPENDS H5match_types
  )
  set_source_files_properties (${HDF5_F90_BINARY_DIR}/static/H5f90i_gen.h PROPERTIES GENERATED TRUE)
  set_source_files_properties (${HDF5_F90_BINARY_DIR}/static/H5fortran_types.f90 PROPERTIES GENERATED TRUE)
endif ()
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_custom_command (
      OUTPUT ${HDF5_F90_BINARY_DIR}/shared/H5f90i_gen.h
             ${HDF5_F90_BINARY_DIR}/shared/H5fortran_types.f90
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:H5match_types>
      WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}/shared
      DEPENDS H5match_types
  )
  set_source_files_properties (${HDF5_F90_BINARY_DIR}/shared/H5f90i_gen.h PROPERTIES GENERATED TRUE)
  set_source_files_properties (${HDF5_F90_BINARY_DIR}/shared/H5fortran_types.f90 PROPERTIES GENERATED TRUE)
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
if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  set (f90CStub_C_SOURCES
    ${f90CStub_C_SOURCES}
    ${HDF5_F90_SRC_SOURCE_DIR}/H5FDmpiof.c
  )
endif ()

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
      PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/static;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_options(${HDF5_F90_C_LIB_TARGET} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
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

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_library (${HDF5_F90_C_LIBSH_TARGET} SHARED ${f90CStub_C_SOURCES} ${f90CStub_C_SHHDRS})
  target_include_directories (${HDF5_F90_C_LIBSH_TARGET}
      PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/shared;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_options(${HDF5_F90_C_LIBSH_TARGET} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_compile_definitions(${HDF5_F90_C_LIBSH_TARGET} PUBLIC "H5_BUILT_AS_DYNAMIC_LIB")
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
# Fortran 2003 standard
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_F2003)
  # F2003 features are enabled
  set (F_STATUS "_F03")
else ()
  # F2003 features are not enabled
  set (F_STATUS "_F90")
endif ()

#-----------------------------------------------------------------------------
# Fortran Real Size
#-----------------------------------------------------------------------------
if (H5_FORTRAN_DEFAULT_REAL_NOT_DOUBLE)
  # default real is 4 bytes, so include double signatures
  set (F_DBLE "Include")
else ()
  # default real is 8 bytes, so exclude double signatures
  set (F_DBLE "Exclude")
endif ()

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  if (NOT ONLY_SHARED_LIBS)
    clang_format (HDF5_F90_C_SRC_FORMAT
        ${HDF5_F90_C_LIB_TARGET}
        ${HDF5_F90_SRC_SOURCE_DIR}/H5match_types.c
        ${HDF5_F90_SRC_SOURCE_DIR}/H5f90.h
        ${HDF5_F90_SRC_SOURCE_DIR}/H5f90i.h
        ${HDF5_F90_SRC_SOURCE_DIR}/H5f90proto.h
    )
  else ()
    clang_format (HDF5_F90_C_SRC_FORMAT
        ${HDF5_F90_C_LIBSH_TARGET}
        ${HDF5_F90_SRC_SOURCE_DIR}/H5match_types.c
        ${HDF5_F90_SRC_SOURCE_DIR}/H5f90.h
        ${HDF5_F90_SRC_SOURCE_DIR}/H5f90i.h
        ${HDF5_F90_SRC_SOURCE_DIR}/H5f90proto.h
    )
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Fortran Modules
#-----------------------------------------------------------------------------
set (f90_F_BASE_SOURCES
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
  set (f90_F_BASE_SOURCES
    ${f90_F_BASE_SOURCES}
    ${HDF5_F90_SRC_SOURCE_DIR}/HDF5mpio.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5FDmpioff.f90
  )
else ()
  set (f90_F_BASE_SOURCES
    ${f90_F_BASE_SOURCES}
    ${HDF5_F90_SRC_SOURCE_DIR}/HDF5.f90
  )
endif ()

if (NOT ONLY_SHARED_LIBS)
  set (f90_F_SRCS
      # generated files
      ${HDF5_F90_BINARY_DIR}/static/H5fortran_types.f90

      ${f90_F_BASE_SOURCES}
  )
  set_source_files_properties (${f90_F_SRCS} PROPERTIES LANGUAGE Fortran)
endif ()
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  set (f90_F_SRCS_SHARED
      # generated files
      ${HDF5_F90_BINARY_DIR}/shared/H5fortran_types.f90

      ${f90_F_BASE_SOURCES}
  )
  set_source_files_properties (${f90_F_SRCS_SHARED} PROPERTIES LANGUAGE Fortran)
endif ()

#-----------------------------------------------------------------------------
# Add Main fortran library
#-----------------------------------------------------------------------------
if (NOT ONLY_SHARED_LIBS)
  add_library (${HDF5_F90_LIB_TARGET} STATIC ${f90_F_SRCS})
  target_include_directories (${HDF5_F90_LIB_TARGET}
      PRIVATE "${HDF5_F90_SRC_SOURCE_DIR};${CMAKE_Fortran_MODULE_DIRECTORY}/static;${HDF5_F90_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include/static>"
  )
  target_compile_options(${HDF5_F90_LIB_TARGET} PRIVATE "${HDF5_CMAKE_Fortran_FLAGS}")
  target_compile_definitions(${HDF5_F90_LIB_TARGET}
      PRIVATE
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:HDF5F90_WINDOWS>
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
  )
  target_link_libraries (${HDF5_F90_LIB_TARGET}
      PUBLIC ${HDF5_F90_C_LIB_TARGET}
      PRIVATE
          ${LINK_Fortran_LIBS}
          $<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_LIBRARIES}>
  )
#  set_property(TARGET ${HDF5_F90_LIB_TARGET} APPEND PROPERTY LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">)
#  set_property(TARGET ${HDF5_F90_LIB_TARGET} APPEND PROPERTY LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>)
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
endif ()

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_library (${HDF5_F90_LIBSH_TARGET} SHARED ${f90_F_SRCS_SHARED})
  target_include_directories (${HDF5_F90_LIBSH_TARGET}
      PRIVATE "${HDF5_F90_SRC_SOURCE_DIR};${CMAKE_Fortran_MODULE_DIRECTORY}/shared;${HDF5_F90_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include/shared>"
  )
  target_compile_options(${HDF5_F90_LIBSH_TARGET} PRIVATE "${HDF5_CMAKE_Fortran_FLAGS}")
  target_compile_definitions(${HDF5_F90_LIBSH_TARGET}
      PUBLIC "H5_BUILT_AS_DYNAMIC_LIB"
      PRIVATE
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:BUILD_HDF5_DLL;HDF5F90_WINDOWS>
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
  )
  target_link_libraries (${HDF5_F90_LIBSH_TARGET}
      PUBLIC ${HDF5_F90_C_LIBSH_TARGET}
      PRIVATE ${LINK_Fortran_LIBS} $<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_LIBRARIES}>
  )
#  set_property(TARGET ${HDF5_F90_LIBSH_TARGET} APPEND PROPERTY LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">)
#  set_property(TARGET ${HDF5_F90_LIBSH_TARGET} APPEND PROPERTY LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>)
#  set_property(TARGET ${HDF5_F90_LIBSH_TARGET} APPEND PROPERTY LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-DLL">)
#  set_property(TARGET ${HDF5_F90_LIBSH_TARGET} APPEND PROPERTY LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-DEF:${HDF5_F90_SRC_BINARY_DIR}/hdf5_fortrandll.def">)
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
          ${HDF5_F90_BINARY_DIR}/static/H5fortran_types.f90
      DESTINATION
          ${HDF5_INSTALL_INCLUDE_DIR}
      COMPONENT
          fortheaders
  )
endif ()
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  install (
      FILES
          ${HDF5_F90_BINARY_DIR}/shared/H5f90i_gen.h
          ${HDF5_F90_BINARY_DIR}/shared/H5fortran_types.f90
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
  if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
    set (mod_files
        ${mod_files}
        ${MOD_BUILD_DIR}/HDF5mpio.mod
        ${MOD_BUILD_DIR}/H5FDmpioff.mod
    )
  endif ()
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
  if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
    set (modsh_files
        ${modsh_files}
        ${MODSH_BUILD_DIR}/HDF5mpio.mod
        ${MODSH_BUILD_DIR}/H5FDmpioff.mod
    )
  endif ()
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
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  set (_PKG_CONFIG_SH_LIBS "${_PKG_CONFIG_SH_LIBS} -l${HDF5_F90_LIB_CORENAME}")
endif ()

set (_PKG_CONFIG_REQUIRES "${HDF5_LIB_CORENAME}-${HDF5_PACKAGE_VERSION}")
set (_PKG_CONFIG_REQUIRES_PRIVATE "${HDF5_LIB_CORENAME}-${HDF5_PACKAGE_VERSION}")

configure_file (
    ${HDF_CONFIG_DIR}/libhdf5.pc.in
    ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_F90_LIB_CORENAME}.pc
    @ONLY
)
install (
    FILES ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_F90_LIB_CORENAME}.pc
    DESTINATION ${HDF5_INSTALL_LIB_DIR}/pkgconfig
    COMPONENT fortlibraries
)

if (NOT WIN32 AND NOT MINGW)
  if (HDF5_ENABLE_PARALLEL AND MPI_Fortran_FOUND)
    set (_PKG_CONFIG_COMPILER ${MPI_Fortran_COMPILER})
  else ()
    set (_PKG_CONFIG_COMPILER ${CMAKE_Fortran_COMPILER})
  endif ()
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
