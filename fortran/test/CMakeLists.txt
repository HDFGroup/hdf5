cmake_minimum_required (VERSION 3.10)
project (HDF5_FORTRAN_TESTS C Fortran)

#-----------------------------------------------------------------------------
# Setup the Fortran auto-detection utilities
# H5_test_buildiface.F90         used to generate various KIND test interfaces
#-----------------------------------------------------------------------------

add_executable (H5_test_buildiface ${HDF5_F90_SRC_DIR}/test/H5_test_buildiface.F90)
target_include_directories (H5_test_buildiface
    PRIVATE
        ${HDF5_BINARY_DIR} ${HDF5_F90_BINARY_DIR}
)
#if (BUILD_SHARED_LIBS)
#  target_compile_definitions(H5_test_buildiface
#      PRIVATE
#          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:/MT>
#  )
#endif ()
if(MSVC)
  set_property(TARGET H5_test_buildiface PROPERTY COMPILE_FLAGS "/MT")
endif()
#set_property(TARGET H5_test_buildiface APPEND PROPERTY
#    LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#)
if(MSVC)
  set_property(TARGET H5_test_buildiface PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE")
endif()
set_target_properties (H5_test_buildiface PROPERTIES
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

if (NOT BUILD_SHARED_LIBS)
  file (MAKE_DIRECTORY "${HDF5_FORTRAN_TESTS_BINARY_DIR}/static")
  set (MOD_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/static/${HDF_CFG_BUILD_TYPE})
else ()
  file (MAKE_DIRECTORY "${HDF5_FORTRAN_TESTS_BINARY_DIR}/shared")
  set (MODSH_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/shared/${HDF_CFG_BUILD_TYPE})
endif ()

#-----------------------------------------------------------------------------
# Add Test Lib
#-----------------------------------------------------------------------------
if (NOT BUILD_SHARED_LIBS)
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
else ()
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
endif ()

if (NOT BUILD_SHARED_LIBS)
  add_custom_command (
      OUTPUT ${HDF5_FORTRAN_TESTS_BINARY_DIR}/static/tf_gen.F90
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:H5_test_buildiface>
      WORKING_DIRECTORY ${HDF5_FORTRAN_TESTS_BINARY_DIR}/static
      DEPENDS H5_test_buildiface
      COMMENT "Generating the tf_gen.F90 file"
  )
  add_custom_target (H5testgen ALL
      DEPENDS ${HDF5_FORTRAN_TESTS_BINARY_DIR}/static/tf_gen.F90
  )
  set_source_files_properties (${HDF5_FORTRAN_TESTS_BINARY_DIR}/static/tf_gen.F90 PROPERTIES GENERATED TRUE)
else ()
  add_custom_command (
      OUTPUT ${HDF5_FORTRAN_TESTS_BINARY_DIR}/shared/tf_gen.F90
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:H5_test_buildiface>
      WORKING_DIRECTORY ${HDF5_FORTRAN_TESTS_BINARY_DIR}/shared
      DEPENDS H5_test_buildiface
      COMMENT "Generating the tf_gen.F90 shared file"
  )
  add_custom_target (H5testgenSH ALL
      DEPENDS ${HDF5_FORTRAN_TESTS_BINARY_DIR}/shared/tf_gen.F90
  )
  set_source_files_properties (${HDF5_FORTRAN_TESTS_BINARY_DIR}/shared/tf_gen.F90 PROPERTIES GENERATED TRUE)
endif ()

if (NOT BUILD_SHARED_LIBS)
  set (HDF5_F90_TF_SOURCES
      # generated files
      ${HDF5_FORTRAN_TESTS_BINARY_DIR}/static/tf_gen.F90

      # normal distribution
      tf.F90
  )
  set_source_files_properties (${HDF5_F90_TF_SOURCES} PROPERTIES LANGUAGE Fortran)
else ()
  set (HDF5_F90_TF_SOURCES_SHARED
      # generated file
      ${HDF5_FORTRAN_TESTS_BINARY_DIR}/shared/tf_gen.F90

      # normal distribution
      tf.F90
  )
  set_source_files_properties (${HDF5_F90_TF_SOURCES_SHARED} PROPERTIES LANGUAGE Fortran)
endif ()

if (NOT BUILD_SHARED_LIBS)
  add_library (${HDF5_F90_TEST_LIB_TARGET} STATIC ${HDF5_F90_TF_SOURCES})
  target_include_directories (${HDF5_F90_TEST_LIB_TARGET}
      PRIVATE
          "${CMAKE_Fortran_MODULE_DIRECTORY}/static;${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/static"
      INTERFACE
          "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
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
  add_dependencies(${HDF5_F90_TEST_LIB_TARGET} H5testgen)
else ()
  add_library (${HDF5_F90_TEST_LIBSH_TARGET} SHARED ${HDF5_F90_TF_SOURCES_SHARED})
  target_include_directories (${HDF5_F90_TEST_LIBSH_TARGET}
      PRIVATE
          "${CMAKE_Fortran_MODULE_DIRECTORY}/shared;${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/shared"
      INTERFACE
          "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
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
  add_dependencies(${HDF5_F90_TEST_LIBSH_TARGET} H5testgenSH)
endif ()

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------

#-- Adding test for testhdf5_fortran
add_executable (testhdf5_fortran
    fortranlib_test.F90
    tH5A.F90
    tH5D.F90
    tH5E.F90
    tH5F.F90
    tH5G.F90
    tH5I.F90
    tH5P.F90
    tH5R.F90
    tH5S.F90
    tH5Sselect.F90
    tH5T.F90
    tH5VL.F90
    tH5Z.F90
    tHDF5.F90
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
if (NOT BUILD_SHARED_LIBS)
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
else ()
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
endif ()

#-- Adding test for testhdf5_fortran_1_8
add_executable (testhdf5_fortran_1_8
    fortranlib_test_1_8.F90
    tH5O.F90
    tH5A_1_8.F90
    tH5G_1_8.F90
    tH5MISC_1_8.F90
    tHDF5_1_8.F90
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
if (NOT BUILD_SHARED_LIBS)
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
else ()
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
endif ()

#-- Adding test for fortranlib_test_F03
add_executable (fortranlib_test_F03
    fortranlib_test_F03.F90
    tH5E_F03.F90
    tH5F_F03.F90
    tH5L_F03.F90
    tH5O_F03.F90
    tH5P_F03.F90
    tH5T_F03.F90
    tHDF5_F03.F90
)
target_compile_options(fortranlib_test_F03
    PRIVATE
        $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
)
#set_property(TARGET fortranlib_test_F03 APPEND PROPERTY
#    LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#)
#set_property(TARGET fortranlib_test_F03 APPEND PROPERTY
#    LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>
#)
if(MSVC)
  set_property(TARGET fortranlib_test_F03 PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
endif()
if (NOT BUILD_SHARED_LIBS)
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
else ()
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
endif ()

#-- Adding test for fflush1
add_executable (fflush1 fflush1.F90)
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
if (NOT BUILD_SHARED_LIBS)
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
else ()
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
endif ()

#-- Adding test for fflush2
add_executable (fflush2 fflush2.F90)
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
if (NOT BUILD_SHARED_LIBS)
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
else ()
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
endif ()

include (CMakeTests.cmake)
