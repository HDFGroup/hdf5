cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_HL_F90_EXAMPLES C CXX Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (
    ${CMAKE_Fortran_MODULE_DIRECTORY}
    ${HDF5_F90_BINARY_DIR}
    ${HDF5_F90_SRC_DIR}/src
)

SET (examples
    exlite
    ex_ds1
)

FOREACH (example ${examples})
  ADD_EXECUTABLE (hl_f90_ex_${example} ${HDF5_HL_F90_EXAMPLES_SOURCE_DIR}/${example}.f90)
  TARGET_NAMING (hl_f90_ex_${example} ${LIB_TYPE})
  TARGET_FORTRAN_PROPERTIES (hl_f90_ex_${example} " " " ")
  TARGET_LINK_LIBRARIES (hl_f90_ex_${example} 
      ${HDF5_HL_F90_LIB_TARGET}
      ${HDF5_F90_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
  SET_TARGET_PROPERTIES (hl_f90_ex_${example} PROPERTIES LINKER_LANGUAGE Fortran)
  SET_TARGET_PROPERTIES (hl_f90_ex_${example} PROPERTIES FOLDER examples/hl/fortran)

ENDFOREACH (example ${examples})

IF (BUILD_TESTING)

  INCLUDE (CMakeTests.cmake)

ENDIF (BUILD_TESTING)
