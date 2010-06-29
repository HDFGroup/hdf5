cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_HL_F90_EXAMPLES C CXX Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${CMAKE_Fortran_MODULE_DIRECTORY} ${HDF5_F90_BINARY_DIR} ${HDF5_F90_SOURCE_DIR}/src)
LINK_DIRECTORIES (
    ${CMAKE_Fortran_MODULE_DIRECTORY}
    ${HDF5_HL_F90_BINARY_DIR}
    ${HDF5_F90_BINARY_DIR}
    ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
)

SET (examples
    exlite
)

FOREACH (example ${examples})
  ADD_EXECUTABLE (hl_f90_ex_${example} ${HDF5_HL_EXAMPLES_SOURCE_DIR}/${example}.f90)
  H5_NAMING (hl_f90_ex_${example})
  IF (WIN32)
    IF (MSVC)
      IF (BUILD_SHARED_LIBS)
        SET_TARGET_PROPERTIES (hl_f90_ex_${example}
            PROPERTIES
                COMPILE_FLAGS "/dll"
                LINK_FLAGS "/SUBSYSTEM:CONSOLE"
        ) 
    ELSE (BUILD_SHARED_LIBS)
      SET_TARGET_PROPERTIES (hl_f90_ex_${example}
          PROPERTIES
              LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
      ) 
      ENDIF (BUILD_SHARED_LIBS)
    ENDIF (MSVC)
  ENDIF (WIN32)
  TARGET_LINK_LIBRARIES (hl_f90_ex_${example} 
      ${HDF5_HL_F90_LIB_TARGET}
      ${HDF5_F90_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
  SET_TARGET_PROPERTIES (hl_f90_ex_${example} PROPERTIES LINKER_LANGUAGE Fortran)
ENDFOREACH (example ${examples})
