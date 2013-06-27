cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_F90_SRC C CXX Fortran)

#-----------------------------------------------------------------------------
# configure def file for shared libs on windows
IF (WIN32 AND NOT CYGWIN)
  IF (BUILD_SHARED_LIBS)
    IF (MSVC)
      IF (NOT H5_HAVE_PARALLEL)
        SET (H5_NOPAREXP ";")
      ENDIF (NOT H5_HAVE_PARALLEL)
      IF (NOT HDF5_ENABLE_F2003)
        SET (H5_NOF03EXP ";")
      ENDIF (NOT HDF5_ENABLE_F2003)
      CONFIGURE_FILE (${HDF5_F90_SRC_SOURCE_DIR}/hdf5_fortrandll.def.in ${HDF5_F90_SRC_BINARY_DIR}/hdf5_fortrandll.def @ONLY)
    ENDIF (MSVC)
  ENDIF (BUILD_SHARED_LIBS)
ENDIF (WIN32 AND NOT CYGWIN)

#-----------------------------------------------------------------------------
# Setup the Fortran auto-detection utilities
# H5test_kind(_SIZEOF).f90 used to generate H5fortran_detect.f90
# H5fortran_detect.f90     used to generate H5fort_type_defines.h
# H5fort_type_defines.h    used to generate H5f90i_gen.h + H5fortran_types.f90
#-----------------------------------------------------------------------------
IF (FORTRAN_HAVE_SIZEOF)
  ADD_EXECUTABLE (H5test_FortranHavekind
      ${HDF5_F90_SRC_SOURCE_DIR}/H5test_kind_SIZEOF.f90
  )
ELSE (FORTRAN_HAVE_SIZEOF)
  ADD_EXECUTABLE (H5test_FortranHavekind
      ${HDF5_F90_SRC_SOURCE_DIR}/H5test_kind.f90
  )
ENDIF (FORTRAN_HAVE_SIZEOF)
IF (WIN32 AND MSVC)
  IF (BUILD_SHARED_LIBS)
    SET_TARGET_PROPERTIES (H5test_FortranHavekind
        PROPERTIES
            COMPILE_FLAGS "/MT"
    )
  ENDIF (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES (H5test_FortranHavekind
      PROPERTIES
          LINK_FLAGS "/SUBSYSTEM:CONSOLE"
  )
ENDIF (WIN32 AND MSVC)
SET_TARGET_PROPERTIES (H5test_FortranHavekind PROPERTIES LINKER_LANGUAGE Fortran)

SET (CMD $<TARGET_FILE:H5test_FortranHavekind>)
ADD_CUSTOM_COMMAND (
    OUTPUT ${HDF5_F90_BINARY_DIR}/H5fortran_detect.f90
    COMMAND ${CMD}
    ARGS > ${HDF5_F90_BINARY_DIR}/H5fortran_detect.f90
    WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}
    DEPENDS H5test_FortranHavekind
)
#-----------------------------------------------------------------------------
ADD_EXECUTABLE (H5fortran_detect
    ${HDF5_F90_BINARY_DIR}/H5fortran_detect.f90
)
IF (WIN32 AND MSVC)
  IF (BUILD_SHARED_LIBS)
    SET_TARGET_PROPERTIES (H5fortran_detect
        PROPERTIES
            COMPILE_FLAGS "/MT"
    )
  ENDIF (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES (H5fortran_detect
      PROPERTIES
          LINK_FLAGS "/SUBSYSTEM:CONSOLE"
  )
ENDIF (WIN32 AND MSVC)
SET_TARGET_PROPERTIES (H5fortran_detect PROPERTIES LINKER_LANGUAGE Fortran)

SET (CMD $<TARGET_FILE:H5fortran_detect>)
ADD_CUSTOM_COMMAND (
    OUTPUT ${HDF5_F90_BINARY_DIR}/H5fort_type_defines.h
    COMMAND ${CMD}
    ARGS > ${HDF5_F90_BINARY_DIR}/H5fort_type_defines.h
    WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}
    DEPENDS H5fortran_detect
)

INCLUDE_DIRECTORIES (${HDF5_F90_BINARY_DIR} ${CMAKE_Fortran_MODULE_DIRECTORY})
#-----------------------------------------------------------------------------
ADD_EXECUTABLE (H5match_types
    ${HDF5_F90_BINARY_DIR}/H5fort_type_defines.h
    ${HDF5_F90_SRC_SOURCE_DIR}/H5match_types.c
)
SET (CMD $<TARGET_FILE:H5match_types>)
ADD_CUSTOM_COMMAND (
    OUTPUT ${HDF5_F90_BINARY_DIR}/H5f90i_gen.h
           ${HDF5_F90_BINARY_DIR}/H5fortran_types.f90
    COMMAND ${CMD}
    WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}
    DEPENDS H5match_types
)

#-----------------------------------------------------------------------------
# f90CStub lib
#-----------------------------------------------------------------------------
SET (f90CStub_C_SRCS
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

IF (H5_HAVE_PARALLEL)
  SET (f90CStub_C_SRCS
      ${f90CStub_C_SRCS}
      ${HDF5_F90_SRC_SOURCE_DIR}/H5FDmpiof.c
  )
ENDIF (H5_HAVE_PARALLEL)

SET_SOURCE_FILES_PROPERTIES (${f90CStub_C_SRCS} PROPERTIES LANGUAGE C)

SET (f90CStub_C_HDRS
    # generated files
    ${HDF5_F90_BINARY_DIR}/H5f90i_gen.h
)

ADD_LIBRARY (${HDF5_F90_C_LIB_TARGET} ${LIB_TYPE} ${f90CStub_C_SRCS} ${f90CStub_C_HDRS})
TARGET_C_PROPERTIES (${HDF5_F90_C_LIB_TARGET} " " " ")
TARGET_LINK_LIBRARIES (${HDF5_F90_C_LIB_TARGET} ${HDF5_LIB_TARGET} ${LINK_LIBS})
SET_GLOBAL_VARIABLE (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_F90_C_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_F90_C_LIB_TARGET} ${HDF5_F90_C_LIB_NAME} ${LIB_TYPE})
SET_TARGET_PROPERTIES (${HDF5_F90_C_LIB_TARGET} PROPERTIES FOLDER libraries/fortran)
SET_TARGET_PROPERTIES (${HDF5_F90_C_LIB_TARGET} PROPERTIES LINKER_LANGUAGE C)

#-----------------------------------------------------------------------------
# Fortran 2003 standard
#-----------------------------------------------------------------------------
IF (HDF5_ENABLE_F2003)
  # default real is 4 bytes, so include double signatures
  SET (F_STATUS "_F03")
ELSE (HDF5_ENABLE_F2003)
  # default real is 8 bytes, so exclude double signatures
  SET (F_STATUS "_F90")
ENDIF (HDF5_ENABLE_F2003)

#-----------------------------------------------------------------------------
# Fortran Real Size
#-----------------------------------------------------------------------------
IF (FORTRAN_DEFAULT_REAL_NOT_DOUBLE)
  # default real is 4 bytes, so include double signatures
  SET (F_DBLE "Include")
ELSE (FORTRAN_DEFAULT_REAL_NOT_DOUBLE)
  # default real is 8 bytes, so exclude double signatures
  SET (F_DBLE "Exclude")
ENDIF (FORTRAN_DEFAULT_REAL_NOT_DOUBLE)

#-----------------------------------------------------------------------------
# Fortran Modules
#-----------------------------------------------------------------------------
SET (f90_F_SRCS
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
IF (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  SET (f90_F_SRCS
    ${f90_F_SRCS}
    ${HDF5_F90_SRC_SOURCE_DIR}/HDF5mpio.f90
    ${HDF5_F90_SRC_SOURCE_DIR}/H5FDmpioff.f90
  )
ELSE (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  SET (f90_F_SRCS
    ${f90_F_SRCS}
    ${HDF5_F90_SRC_SOURCE_DIR}/HDF5.f90
  )
ENDIF (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)

#-----------------------------------------------------------------------------
# Add Main fortran library
#-----------------------------------------------------------------------------
ADD_LIBRARY (${HDF5_F90_LIB_TARGET} ${LIB_TYPE} ${f90_F_SRCS})
SET (SHARED_LINK_FLAGS " ")
IF (WIN32 AND NOT CYGWIN)
  IF (BUILD_SHARED_LIBS)
    SET_PROPERTY (TARGET ${HDF5_F90_LIB_TARGET}
      APPEND PROPERTY COMPILE_DEFINITIONS
        BUILD_HDF5_DLL
    )
    IF (MSVC)
      SET (SHARED_LINK_FLAGS "/DLL /DEF:${HDF5_F90_SRC_BINARY_DIR}/hdf5_fortrandll.def")
    ENDIF (MSVC)
  ENDIF (BUILD_SHARED_LIBS)
  SET_PROPERTY (TARGET ${HDF5_F90_LIB_TARGET}
      APPEND PROPERTY COMPILE_DEFINITIONS
        HDF5F90_WINDOWS
  )
ENDIF (WIN32 AND NOT CYGWIN)
TARGET_FORTRAN_PROPERTIES (${HDF5_F90_LIB_TARGET} " " ${SHARED_LINK_FLAGS})
SET_TARGET_PROPERTIES (${HDF5_F90_LIB_TARGET} PROPERTIES LINKER_LANGUAGE Fortran)
TARGET_LINK_LIBRARIES (${HDF5_F90_LIB_TARGET} ${HDF5_F90_C_LIB_TARGET} ${HDF5_LIB_TARGET})
IF (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  TARGET_LINK_LIBRARIES (${HDF5_F90_LIB_TARGET} ${MPI_Fortran_LIBRARIES})
ENDIF (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
SET_GLOBAL_VARIABLE (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_F90_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_F90_LIB_TARGET} ${HDF5_F90_LIB_NAME} ${LIB_TYPE})
SET_TARGET_PROPERTIES (${HDF5_F90_LIB_TARGET} PROPERTIES FOLDER libraries/fortran)

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
INSTALL (
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

IF (WIN32 AND NOT CYGWIN)
  INSTALL (
      DIRECTORY
          ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/
      DESTINATION
          ${HDF5_INSTALL_INCLUDE_DIR}
      COMPONENT
          fortheaders
  )
ELSE (WIN32 AND NOT CYGWIN)
  INSTALL (
      DIRECTORY
          ${CMAKE_Fortran_MODULE_DIRECTORY}/
      DESTINATION
          ${HDF5_INSTALL_INCLUDE_DIR}
      COMPONENT
          fortheaders
  )
ENDIF (WIN32 AND NOT CYGWIN)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
IF (HDF5_EXPORTED_TARGETS)

  INSTALL_TARGET_PDB (${HDF5_F90_C_LIB_TARGET} ${HDF5_INSTALL_LIB_DIR} fortlibraries)
  #INSTALL_TARGET_PDB (${HDF5_F90_LIB_TARGET} ${HDF5_INSTALL_LIB_DIR} fortlibraries)

  INSTALL (
      TARGETS
          ${HDF5_F90_C_LIB_TARGET}
          ${HDF5_F90_LIB_TARGET}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT fortlibraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT fortlibraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT fortlibraries
  )
ENDIF (HDF5_EXPORTED_TARGETS)
