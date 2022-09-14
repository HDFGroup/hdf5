cmake_minimum_required (VERSION 3.18)
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
  target_include_directories (hl_ex_${example} PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  if (NOT BUILD_SHARED_LIBS)
    TARGET_C_PROPERTIES (hl_ex_${example} STATIC)
    target_link_libraries (hl_ex_${example} PRIVATE ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (hl_ex_${example} SHARED)
    target_link_libraries (hl_ex_${example} PRIVATE ${HDF5_HL_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  endif ()
  set_target_properties (hl_ex_${example} PROPERTIES FOLDER examples/hl)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_HL_EXAMPLES_${example}_FORMAT hl_ex_${example})
  endif ()
endforeach ()

if (BUILD_TESTING AND HDF5_TEST_EXAMPLES AND HDF5_TEST_SERIAL)
  include (CMakeTests.cmake)
endif ()
