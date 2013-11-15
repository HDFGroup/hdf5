cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_HL_EXAMPLES )

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
SET (examples
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

FOREACH (example ${examples})
  ADD_EXECUTABLE (hl_ex_${example} ${HDF5_HL_EXAMPLES_SOURCE_DIR}/${example}.c)
  TARGET_NAMING (hl_ex_${example} ${LIB_TYPE})
  TARGET_C_PROPERTIES (hl_ex_${example} " " " ")
  TARGET_LINK_LIBRARIES (hl_ex_${example} ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})
  SET_TARGET_PROPERTIES (hl_ex_${example} PROPERTIES FOLDER examples/hl)
ENDFOREACH (example ${examples})

IF (BUILD_TESTING)

  INCLUDE (CMakeTests.cmake)

ENDIF (BUILD_TESTING)
