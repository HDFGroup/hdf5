cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_TEST_MISC_VDS C)

MACRO (ADD_H5_GENERATOR genfile)
  add_executable (${genfile} ${HDF5_TOOLS_TEST_MISC_VDS_SOURCE_DIR}/${genfile}.c)
  target_include_directories (${genfile} PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  if (NOT ONLY_SHARED_LIBS)
    TARGET_C_PROPERTIES (${genfile} STATIC)
    target_link_libraries (${genfile} PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (${genfile} SHARED)
    target_link_libraries (${genfile} PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  endif ()
  set_target_properties (${genfile} PROPERTIES FOLDER generator/tools)
ENDMACRO ()

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
endforeach ()
