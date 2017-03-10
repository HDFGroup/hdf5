cmake_minimum_required (VERSION 3.1.0)
# --------------------------------------------------------------------
# Notes: When creating examples they should be prefixed
# with "cpp_ex_". This allows for easier filtering of the examples.
# --------------------------------------------------------------------

PROJECT (HDF5_CPP_EXAMPLES)

#-----------------------------------------------------------------------------
# Define examples
#-----------------------------------------------------------------------------

set (examples
    create
    readdata
    writedata
    compound
    extend_ds
    chunks
    h5group
)

set (tutr_examples
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

foreach (example ${examples})
  add_executable (cpp_ex_${example} ${HDF5_CPP_EXAMPLES_SOURCE_DIR}/${example}.cpp)
  TARGET_NAMING (cpp_ex_${example} STATIC)
  TARGET_C_PROPERTIES (cpp_ex_${example} STATIC " " " ")
  target_link_libraries (cpp_ex_${example} ${HDF5_CPP_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (cpp_ex_${example} PROPERTIES FOLDER examples/cpp)
endforeach (example ${examples})

foreach (example ${tutr_examples})
  add_executable (cpp_ex_${example} ${HDF5_CPP_EXAMPLES_SOURCE_DIR}/${example}.cpp)
  TARGET_NAMING (cpp_ex_${example} STATIC)
  TARGET_C_PROPERTIES (cpp_ex_${example} STATIC " " " ")
  target_link_libraries (cpp_ex_${example} ${HDF5_CPP_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (cpp_ex_${example} PROPERTIES FOLDER examples/cpp)
endforeach (example ${tutr_examples})

if (BUILD_TESTING)
  include (CMakeTests.cmake)
endif (BUILD_TESTING)
