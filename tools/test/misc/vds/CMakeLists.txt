cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_TEST_MISC_VDS)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

MACRO (ADD_H5_GENERATOR genfile)
  add_executable (${genfile} ${HDF5_TOOLS_TEST_MISC_VDS_SOURCE_DIR}/${genfile}.c)
  TARGET_NAMING (${genfile} STATIC)
  TARGET_C_PROPERTIES (${genfile} STATIC " " " ")
  target_link_libraries (${genfile} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (${genfile} PROPERTIES FOLDER generator/tools)
ENDMACRO (ADD_H5_GENERATOR genfile)

# generator executables
set (H5_GENERATORS
    UC_1_one_dim_gen
    UC_2_two_dims_gen
    UC_3_gaps_gen
    UC_4_printf_gen
    UC_5_stride_gen
)

foreach (gen ${H5_GENERATORS})
  ADD_H5_GENERATOR (${gen})
endforeach (gen ${H5_GENERATORS})
