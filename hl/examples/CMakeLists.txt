cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_HL_EXAMPLES )

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
  TARGET_NAMING (hl_ex_${example} STATIC)
  TARGET_C_PROPERTIES (hl_ex_${example} STATIC " " " ")
  target_link_libraries (hl_ex_${example} ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (hl_ex_${example} PROPERTIES FOLDER examples/hl)
endforeach (example ${examples})

if (BUILD_TESTING)

  include (CMakeTests.cmake)

endif (BUILD_TESTING)
