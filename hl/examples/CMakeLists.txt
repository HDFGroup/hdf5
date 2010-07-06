cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_HL_EXAMPLES )

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
SET (examples
    ex_lite1
    ex_lite2
    ex_lite3
    ptExampleFL
    ptExampleVL
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
  H5_NAMING (hl_ex_${example})
  IF (WIN32)
    IF (MSVC)
      IF (NOT BUILD_SHARED_LIBS)
        SET_TARGET_PROPERTIES (hl_ex_${example}
            PROPERTIES
                LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
        ) 
      ENDIF (NOT BUILD_SHARED_LIBS)
    ENDIF (MSVC)
  ENDIF (WIN32)
  TARGET_LINK_LIBRARIES (hl_ex_${example} ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})
ENDFOREACH (example ${examples})


