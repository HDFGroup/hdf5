cmake_minimum_required (VERSION 3.10)
project (HDF5_TEST_PAR C)

#-----------------------------------------------------------------------------
# Define Tests
#-----------------------------------------------------------------------------

set (testphdf5_SOURCES
    ${HDF5_TEST_PAR_SOURCE_DIR}/testphdf5.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_dset.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_file.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_file_image.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_mdset.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_ph5basic.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_coll_chunk.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_span_tree.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_chunk_alloc.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_filter_read.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_prop.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_coll_md_read.c
)

#-- Adding test for testhdf5
add_executable (testphdf5 ${testphdf5_SOURCES})
target_include_directories (testphdf5
    PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
)
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (testphdf5 STATIC)
  target_link_libraries (testphdf5
      PRIVATE ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>"
  )
else ()
  TARGET_C_PROPERTIES (testphdf5 SHARED)
  target_link_libraries (testphdf5
      PRIVATE ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>"
  )
endif ()
set_target_properties (testphdf5 PROPERTIES FOLDER test/par)

MACRO (ADD_H5P_EXE file)
  add_executable (${file} ${HDF5_TEST_PAR_SOURCE_DIR}/${file}.c)
  target_include_directories (${file}
      PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
  )
  if (NOT BUILD_SHARED_LIBS)
    TARGET_C_PROPERTIES (${file} STATIC)
    target_link_libraries (${file}
        PRIVATE ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>"
                $<$<OR:$<PLATFORM_ID:Windows>,$<PLATFORM_ID:MinGW>>:ws2_32.lib>
    )
  else ()
    TARGET_C_PROPERTIES (${file} SHARED)
    target_link_libraries (${file}
        PRIVATE ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>"
                $<$<OR:$<PLATFORM_ID:Windows>,$<PLATFORM_ID:MinGW>>:ws2_32.lib>
    )
  endif ()
  set_target_properties (${file} PROPERTIES FOLDER test/par)
ENDMACRO (ADD_H5P_EXE file)

set (H5P_TESTS
    t_mpi
    t_bigio
    t_cache
    t_cache_image
    t_pflush1
    t_pflush2
    t_pread
    t_pshutdown
    t_prestart
    t_init_term
    t_shapesame
    t_filters_parallel
)

foreach (h5_testp ${H5P_TESTS})
  ADD_H5P_EXE(${h5_testp})
endforeach ()

include (CMakeTests.cmake)
