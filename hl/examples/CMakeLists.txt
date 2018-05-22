cmake_minimum_required (VERSION 3.10)
project (HDF5_HL_EXAMPLES C)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
set (examples
    ex_lite1
    ex_lite2    #ex_lite2 PROPERTIES DEPENDS ex_lite1)
    ex_lite3
    ptExampleFL
    ex_image1
    ex_image2
    ex_table_01
    ex_table_02
    ex_table_03
    ex_table_04
    ex_table_05
    ex_table_06
    ex_table_07
    ex_table_08
    ex_table_09
    ex_table_10
    ex_table_11
    ex_table_12
    ex_ds1
)

foreach (example ${examples})
  add_executable (hl_ex_${example} ${HDF5_HL_EXAMPLES_SOURCE_DIR}/${example}.c)
  target_include_directories(hl_ex_${example} PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (hl_ex_${example} STATIC)
  target_link_libraries (hl_ex_${example} PRIVATE ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (hl_ex_${example} PROPERTIES FOLDER examples/hl)
endforeach ()

if (BUILD_TESTING)

  include (CMakeTests.cmake)

endif ()
