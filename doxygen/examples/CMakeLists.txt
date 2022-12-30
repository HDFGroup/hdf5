cmake_minimum_required (VERSION 3.18)
project (HDF5_EXAMPLES C)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
set (examples
    H5D_examples
)

if (H5_HAVE_PARALLEL)
  set (parallel_examples
  )

  if (HDF5_ENABLE_SUBFILING_VFD)
    list (APPEND parallel_examples ph5_subfiling)
  endif ()
endif ()

foreach (example ${examples})
  add_executable (${example} ${HDF5_DOXYGEN_DIR}/examples/${example}.c)
  target_include_directories (${example} PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  if (NOT BUILD_SHARED_LIBS)
    TARGET_C_PROPERTIES (${example} STATIC)
    target_link_libraries (${example} PRIVATE ${HDF5_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (${example} SHARED)
    target_link_libraries (${example} PRIVATE ${HDF5_LIBSH_TARGET})
  endif ()
  set_target_properties (${example} PROPERTIES FOLDER examples)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_EXAMPLES_${example}_FORMAT ${example})
  endif ()
endforeach ()

if (H5_HAVE_PARALLEL)
  foreach (parallel_example ${parallel_examples})
    add_executable (${parallel_example} ${HDF5_DOXYGEN_DIR}/examples/${parallel_example}.c)
    target_include_directories (${parallel_example} PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    if (NOT BUILD_SHARED_LIBS)
      TARGET_C_PROPERTIES (${parallel_example} STATIC)
      target_link_libraries (${parallel_example} PRIVATE ${HDF5_LIB_TARGET} MPI::MPI_C)
    else ()
      TARGET_C_PROPERTIES (${parallel_example} SHARED)
      target_link_libraries (${parallel_example} PRIVATE ${HDF5_LIBSH_TARGET} MPI::MPI_C)
    endif ()
    set_target_properties (${parallel_example} PROPERTIES FOLDER examples)

    #-----------------------------------------------------------------------------
    # Add Target to clang-format
    #-----------------------------------------------------------------------------
    if (HDF5_ENABLE_FORMATTERS)
      clang_format (HDF5_EXAMPLES_${parallel_example}_FORMAT ${parallel_example})
    endif ()
  endforeach ()
endif ()

if (BUILD_TESTING AND HDF5_TEST_EXAMPLES)
  include (CMakeTests.cmake)
endif ()
