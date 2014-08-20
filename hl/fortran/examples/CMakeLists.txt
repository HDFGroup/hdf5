cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5_HL_F90_EXAMPLES C CXX Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (
    ${CMAKE_Fortran_MODULE_DIRECTORY}
    ${HDF5_F90_BINARY_DIR}
    ${HDF5_F90_SRC_DIR}/src
)

set (examples
    exlite
    ex_ds1
)

foreach (example ${examples})
  add_executable (hl_f90_ex_${example} ${HDF5_HL_F90_EXAMPLES_SOURCE_DIR}/${example}.f90)
  TARGET_NAMING (hl_f90_ex_${example} ${LIB_TYPE})
  TARGET_FORTRAN_PROPERTIES (hl_f90_ex_${example} " " " ")
  target_link_libraries (hl_f90_ex_${example} 
      ${HDF5_HL_F90_LIB_TARGET}
      ${HDF5_F90_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
  set_target_properties (hl_f90_ex_${example} PROPERTIES LINKER_LANGUAGE Fortran)
  set_target_properties (hl_f90_ex_${example} PROPERTIES FOLDER examples/hl/fortran)

endforeach (example ${examples})

if (BUILD_TESTING)

  include (CMakeTests.cmake)

endif (BUILD_TESTING)
