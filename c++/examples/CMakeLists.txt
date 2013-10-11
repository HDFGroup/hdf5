cmake_minimum_required (VERSION 2.8.10)
# --------------------------------------------------------------------
# Notes: When creating examples they should be prefixed
# with "cpp_ex_". This allows for easier filtering of the examples.
# --------------------------------------------------------------------

PROJECT (HDF5_CPP_EXAMPLES)

#-----------------------------------------------------------------------------
# Define examples
#-----------------------------------------------------------------------------

SET (examples
    create
    readdata
    writedata
    compound
    extend_ds
    chunks
    h5group
)

SET (tutr_examples
    h5tutr_cmprss
    h5tutr_crtdat
    h5tutr_crtatt
    h5tutr_crtgrpar
    h5tutr_crtgrp
    h5tutr_crtgrpd
    h5tutr_extend
    h5tutr_rdwt
    h5tutr_subset
)

FOREACH (example ${examples})
  ADD_EXECUTABLE (cpp_ex_${example} ${HDF5_CPP_EXAMPLES_SOURCE_DIR}/${example}.cpp)
  TARGET_NAMING (cpp_ex_${example} ${LIB_TYPE})
  TARGET_C_PROPERTIES (cpp_ex_${example} " " " ")
  TARGET_LINK_LIBRARIES (cpp_ex_${example} ${HDF5_CPP_LIB_TARGET} ${HDF5_LIB_TARGET})
  SET_TARGET_PROPERTIES (cpp_ex_${example} PROPERTIES FOLDER examples/cpp)
ENDFOREACH (example ${examples})

FOREACH (example ${tutr_examples})
  ADD_EXECUTABLE (cpp_ex_${example} ${HDF5_CPP_EXAMPLES_SOURCE_DIR}/${example}.cpp)
  TARGET_NAMING (cpp_ex_${example} ${LIB_TYPE})
  TARGET_C_PROPERTIES (cpp_ex_${example} " " " ")
  TARGET_LINK_LIBRARIES (cpp_ex_${example} ${HDF5_CPP_LIB_TARGET} ${HDF5_LIB_TARGET})
  SET_TARGET_PROPERTIES (cpp_ex_${example} PROPERTIES FOLDER examples/cpp)
ENDFOREACH (example ${tutr_examples})

IF (BUILD_TESTING)
  INCLUDE (CMakeTests.cmake)
ENDIF (BUILD_TESTING)
