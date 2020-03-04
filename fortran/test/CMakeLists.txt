cmake_minimum_required (VERSION 3.12)
project (HDF5_FORTRAN_TESTS C Fortran)

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  file (MAKE_DIRECTORY "${HDF5_FORTRAN_TESTS_BINARY_DIR}/shared")
  set (MODSH_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/shared/${HDF_CFG_BUILD_TYPE})
else ()
  file (MAKE_DIRECTORY "${HDF5_FORTRAN_TESTS_BINARY_DIR}/static")
  set (MOD_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/static/${HDF_CFG_BUILD_TYPE})
endif ()

#-----------------------------------------------------------------------------
# Add Test Lib
#-----------------------------------------------------------------------------
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_library (${HDF5_F90_C_TEST_LIBSH_TARGET} SHARED t.c)
  target_include_directories (${HDF5_F90_C_TEST_LIBSH_TARGET}
      PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/shared;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_definitions(${HDF5_F90_C_TEST_LIBSH_TARGET}
      PUBLIC "H5_BUILT_AS_DYNAMIC_LIB"
  )
  TARGET_C_PROPERTIES (${HDF5_F90_C_TEST_LIBSH_TARGET} SHARED)
  target_link_libraries (${HDF5_F90_C_TEST_LIBSH_TARGET}
      PRIVATE ${HDF5_F90_C_LIBSH_TARGET}
      PUBLIC ${HDF5_TEST_LIBSH_TARGET}
  )
  H5_SET_LIB_OPTIONS (${HDF5_F90_C_TEST_LIBSH_TARGET} ${HDF5_F90_C_TEST_LIB_NAME} SHARED "F")
  set_target_properties (${HDF5_F90_C_TEST_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/test/fortran
      LINKER_LANGUAGE C
  )
else ()
  add_library (${HDF5_F90_C_TEST_LIB_TARGET} STATIC t.c)
  set_source_files_properties (t.c PROPERTIES LANGUAGE C)
  target_include_directories (${HDF5_F90_C_TEST_LIB_TARGET}
      PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/static;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  TARGET_C_PROPERTIES (${HDF5_F90_C_TEST_LIB_TARGET} STATIC)
  target_link_libraries (${HDF5_F90_C_TEST_LIB_TARGET}
      PRIVATE ${HDF5_F90_C_LIB_TARGET}
      PUBLIC ${HDF5_TEST_LIB_TARGET}
  )
  H5_SET_LIB_OPTIONS (${HDF5_F90_C_TEST_LIB_TARGET} ${HDF5_F90_C_TEST_LIB_NAME} STATIC 0)
  set_target_properties (${HDF5_F90_C_TEST_LIB_TARGET} PROPERTIES
      FOLDER libraries/test/fortran
      LINKER_LANGUAGE C
  )
endif ()

# See if the F2008 intrinsic STORAGE_SIZE and C_SIZEOF are supported. If not then
# fall back to F2003. If F2003 not supported then use F90 for the tests.

set_source_files_properties (tf_F90.f90 tf_F03.f90 tf_F08.f90 tf.f90 PROPERTIES LANGUAGE Fortran)
if (H5_FORTRAN_HAVE_STORAGE_SIZE AND H5_FORTRAN_HAVE_C_SIZEOF)
  if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
    add_library (${HDF5_F90_TEST_LIBSH_TARGET} SHARED tf_F08.f90 tf.f90)
  else ()
    add_library (${HDF5_F90_TEST_LIB_TARGET} STATIC tf_F08.f90 tf.f90)
  endif ()
elseif (HDF5_ENABLE_F2003)
  if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
    add_library (${HDF5_F90_TEST_LIBSH_TARGET} SHARED tf_F03.f90 tf.f90)
  else ()
    add_library (${HDF5_F90_TEST_LIB_TARGET} STATIC tf_F03.f90 tf.f90)
  endif ()
else ()
  if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
    add_library (${HDF5_F90_TEST_LIBSH_TARGET} SHARED tf_F90.f90 tf.f90)
  else ()
    add_library (${HDF5_F90_TEST_LIB_TARGET} STATIC tf_F90.f90 tf.f90)
  endif ()
endif ()

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  target_include_directories (${HDF5_F90_TEST_LIBSH_TARGET}
      PRIVATE
          "${CMAKE_Fortran_MODULE_DIRECTORY}/shared;${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/shared"
      INTERFACE
          "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include/shared>"
  )
  target_compile_definitions(${HDF5_F90_TEST_LIBSH_TARGET}
      PUBLIC
          "H5_BUILT_AS_DYNAMIC_LIB"
      PRIVATE
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:BUILD_HDF5_TEST_DLL;HDF5F90_WINDOWS>
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
  )
  target_link_libraries (${HDF5_F90_TEST_LIBSH_TARGET}
      PUBLIC
          ${HDF5_F90_C_TEST_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET} ${HDF5_LIBSH_TARGET}
  )
#  set_property(TARGET ${HDF5_F90_TEST_LIBSH_TARGET} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#  )
#  set_property(TARGET ${HDF5_F90_TEST_LIBSH_TARGET} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>
#  )
#  set_property(TARGET ${HDF5_F90_TEST_LIBSH_TARGET} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-DLL">
#  )
  if(MSVC)
    set_property(TARGET ${HDF5_F90_TEST_LIBSH_TARGET} PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS} -DLL")
  endif()
  set_target_properties (${HDF5_F90_TEST_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/test/fortran
      LINKER_LANGUAGE Fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  H5_SET_LIB_OPTIONS (${HDF5_F90_TEST_LIBSH_TARGET} ${HDF5_F90_TEST_LIB_NAME} SHARED "F")
else ()
  target_include_directories (${HDF5_F90_TEST_LIB_TARGET}
      PRIVATE
          "${CMAKE_Fortran_MODULE_DIRECTORY}/static;${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/static"
      INTERFACE
          "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include/static>"
  )
  target_compile_definitions(${HDF5_F90_TEST_LIB_TARGET}
      PRIVATE
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:HDF5F90_WINDOWS>
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
  )
  target_link_libraries (${HDF5_F90_TEST_LIB_TARGET}
      PUBLIC
          ${HDF5_F90_C_TEST_LIB_TARGET} ${HDF5_F90_LIB_TARGET} ${HDF5_LIB_TARGET}
  )
#  set_property(TARGET ${HDF5_F90_TEST_LIB_TARGET} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#  )
#  set_property(TARGET ${HDF5_F90_TEST_LIB_TARGET} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>
#  )
  if(MSVC)
    set_property(TARGET ${HDF5_F90_TEST_LIB_TARGET} PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
  endif()
  set_target_properties (${HDF5_F90_TEST_LIB_TARGET} PROPERTIES
      FOLDER libraries/test/fortran
      LINKER_LANGUAGE Fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
  )
  H5_SET_LIB_OPTIONS (${HDF5_F90_TEST_LIB_TARGET} ${HDF5_F90_TEST_LIB_NAME} STATIC 0)
endif ()

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------

#-- Adding test for testhdf5_fortran
add_executable (testhdf5_fortran
    fortranlib_test.f90
    tH5A.f90
    tH5D.f90
    tH5E.f90
    tH5F.f90
    tH5G.f90
    tH5I.f90
    tH5P.f90
    tH5R.f90
    tH5S.f90
    tH5Sselect.f90
    tH5T.f90
    tH5VL.f90
    tH5Z.f90
    tHDF5.f90
)
target_compile_options(testhdf5_fortran
    PRIVATE
        $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
)
#set_property(TARGET testhdf5_fortran APPEND PROPERTY
#    LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#)
#set_property(TARGET testhdf5_fortran APPEND PROPERTY
#    LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>
#)
if(MSVC)
  set_property(TARGET testhdf5_fortran  PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
endif()
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  target_include_directories (testhdf5_fortran
      PRIVATE
          "${CMAKE_Fortran_MODULE_DIRECTORY}/shared;${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/shared"
  )
  target_link_libraries (testhdf5_fortran
      PRIVATE
          ${HDF5_F90_TEST_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} $<$<PLATFORM_ID:Windows>:ws2_32.lib>
  )
  set_target_properties (testhdf5_fortran PROPERTIES
      LINKER_LANGUAGE Fortran
      FOLDER test/fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  add_dependencies (testhdf5_fortran ${HDF5_F90_TEST_LIBSH_TARGET})
else ()
  target_include_directories (testhdf5_fortran
      PRIVATE
          "${CMAKE_Fortran_MODULE_DIRECTORY}/static;${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/static"
  )
  target_link_libraries (testhdf5_fortran
      PRIVATE
          ${HDF5_F90_TEST_LIB_TARGET} ${HDF5_F90_LIB_TARGET} ${HDF5_LIB_TARGET} $<$<PLATFORM_ID:Windows>:ws2_32.lib>
  )
  set_target_properties (testhdf5_fortran PROPERTIES
      LINKER_LANGUAGE Fortran
      FOLDER test/fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
  )
  add_dependencies (testhdf5_fortran ${HDF5_F90_TEST_LIB_TARGET})
endif ()

#-- Adding test for testhdf5_fortran_1_8
add_executable (testhdf5_fortran_1_8
    fortranlib_test_1_8.f90
    tH5O.f90
    tH5A_1_8.f90
    tH5G_1_8.f90
    tH5MISC_1_8.f90
    tHDF5_1_8.f90
)
target_compile_options(testhdf5_fortran_1_8
    PRIVATE
        $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
)
#set_property(TARGET testhdf5_fortran_1_8 APPEND PROPERTY
#    LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#)
#set_property(TARGET testhdf5_fortran_1_8 APPEND PROPERTY
#    LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>
#)
if(MSVC)
  set_property(TARGET testhdf5_fortran_1_8 PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
endif()
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  target_include_directories (testhdf5_fortran_1_8
      PRIVATE
          "${CMAKE_Fortran_MODULE_DIRECTORY}/shared;${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/shared"
  )
  target_link_libraries (testhdf5_fortran_1_8
      PRIVATE
          ${HDF5_F90_TEST_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} $<$<PLATFORM_ID:Windows>:ws2_32.lib>
  )
  set_target_properties (testhdf5_fortran_1_8 PROPERTIES
      LINKER_LANGUAGE Fortran
      FOLDER test/fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  add_dependencies (testhdf5_fortran_1_8 ${HDF5_F90_TEST_LIBSH_TARGET})
else ()
  target_include_directories (testhdf5_fortran_1_8
      PRIVATE
          "${CMAKE_Fortran_MODULE_DIRECTORY}/static;${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/static"
  )
  target_link_libraries (testhdf5_fortran_1_8
      PRIVATE
          ${HDF5_F90_TEST_LIB_TARGET} ${HDF5_F90_LIB_TARGET} ${HDF5_LIB_TARGET} $<$<PLATFORM_ID:Windows>:ws2_32.lib>
  )
  set_target_properties (testhdf5_fortran_1_8 PROPERTIES
      LINKER_LANGUAGE Fortran
      FOLDER test/fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
  )
  add_dependencies (testhdf5_fortran_1_8 ${HDF5_F90_TEST_LIB_TARGET})
endif ()

#-- Adding test for fortranlib_test_F03
if (HDF5_ENABLE_F2003)
  add_executable (fortranlib_test_F03
      fortranlib_test_F03.f90
      tH5E_F03.f90
      tH5F_F03.f90
      tH5L_F03.f90
      tH5O_F03.f90
      tH5P_F03.f90
      tH5T_F03.f90
      tHDF5_F03.f90
  )
  target_compile_options(fortranlib_test_F03
      PRIVATE
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
  )
#  set_property(TARGET fortranlib_test_F03 APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#  )
#  set_property(TARGET fortranlib_test_F03 APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>
#  )
  if(MSVC)
    set_property(TARGET fortranlib_test_F03 PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
  endif()
  if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
    target_include_directories (fortranlib_test_F03
        PRIVATE
            "${CMAKE_Fortran_MODULE_DIRECTORY}/shared;${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/shared"
    )
    target_link_libraries (fortranlib_test_F03
        PRIVATE
            ${HDF5_F90_TEST_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} $<$<PLATFORM_ID:Windows>:ws2_32.lib>
    )
    set_target_properties (fortranlib_test_F03 PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
    )
    add_dependencies (fortranlib_test_F03 ${HDF5_F90_TEST_LIBSH_TARGET})
  else ()
    target_include_directories (fortranlib_test_F03
        PRIVATE
            "${CMAKE_Fortran_MODULE_DIRECTORY}/static;${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/static"
    )
    target_link_libraries (fortranlib_test_F03
        PRIVATE
            ${HDF5_F90_TEST_LIB_TARGET} ${HDF5_F90_LIB_TARGET} ${HDF5_LIB_TARGET} $<$<PLATFORM_ID:Windows>:ws2_32.lib>
    )
    set_target_properties (fortranlib_test_F03 PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
    )
    add_dependencies (fortranlib_test_F03 ${HDF5_F90_TEST_LIB_TARGET})
  endif ()
endif ()

#-- Adding test for fflush1
add_executable (fflush1 fflush1.f90)
target_compile_options(fflush1
    PRIVATE
        $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
)
#set_property(TARGET fflush1 APPEND PROPERTY
#    LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#)
#set_property(TARGET fflush1 APPEND PROPERTY
#    LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>
#)
if(MSVC)
  set_property(TARGET fflush1 PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
endif()
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  target_include_directories (fflush1
      PRIVATE
          ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  target_link_libraries (fflush1
      PRIVATE
          ${HDF5_F90_LIBSH_TARGET} ${HDF5_F90_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} $<$<PLATFORM_ID:Windows>:ws2_32.lib>
  )
  set_target_properties (fflush1 PROPERTIES
      LINKER_LANGUAGE Fortran
      FOLDER test/fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  add_dependencies (fflush1 ${HDF5_F90_TEST_LIBSH_TARGET})
else ()
  target_include_directories (fflush1
      PRIVATE
          ${CMAKE_Fortran_MODULE_DIRECTORY}/static
  )
  target_link_libraries (fflush1
      PRIVATE
          ${HDF5_F90_LIB_TARGET} ${HDF5_F90_TEST_LIB_TARGET} ${HDF5_LIB_TARGET} $<$<PLATFORM_ID:Windows>:ws2_32.lib>
  )
  set_target_properties (fflush1 PROPERTIES
      LINKER_LANGUAGE Fortran
      FOLDER test/fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
  )
  add_dependencies (fflush1 ${HDF5_F90_TEST_LIB_TARGET})
endif ()

#-- Adding test for fflush2
add_executable (fflush2 fflush2.f90)
target_compile_options (fflush2
    PRIVATE
        $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
)
#set_property(TARGET fflush2 APPEND PROPERTY
#    LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#)
#set_property(TARGET fflush2 APPEND PROPERTY
#    LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>
#)
if(MSVC)
  set_property(TARGET fflush2 PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
endif()
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  target_include_directories (fflush2
      PRIVATE
          ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  target_link_libraries (fflush2
      PRIVATE
          ${HDF5_F90_TEST_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} $<$<PLATFORM_ID:Windows>:ws2_32.lib>
  )
  set_target_properties (fflush2 PROPERTIES
      LINKER_LANGUAGE Fortran
      FOLDER test/fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  add_dependencies (fflush2 ${HDF5_F90_TEST_LIBSH_TARGET})
else ()
  target_include_directories (fflush2
      PRIVATE
          ${CMAKE_Fortran_MODULE_DIRECTORY}/static
  )
  target_link_libraries (fflush2
      PRIVATE
          ${HDF5_F90_TEST_LIB_TARGET} ${HDF5_F90_LIB_TARGET} ${HDF5_LIB_TARGET} $<$<PLATFORM_ID:Windows>:ws2_32.lib>
  )
  set_target_properties (fflush2 PROPERTIES
      LINKER_LANGUAGE Fortran
      FOLDER test/fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
  )
  add_dependencies (fflush2 ${HDF5_F90_TEST_LIB_TARGET})
endif ()

if (HDF5_TEST_FORTRAN AND HDF5_TEST_SERIAL)
  include (CMakeTests.cmake)
endif ()
