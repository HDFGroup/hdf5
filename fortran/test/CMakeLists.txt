cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_FORTRAN_TESTS C CXX Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_F90_BINARY_DIR} ${HDF5_F90_SRC_DIR}/src)

#-----------------------------------------------------------------------------
# Setup the Fortran auto-detection utilities
# H5_test_buildiface.F90         used to generate various KIND test interfaces
#-----------------------------------------------------------------------------

add_executable (H5_test_buildiface
      ${HDF5_F90_SRC_DIR}/test/H5_test_buildiface.F90
  )

if (WIN32 AND MSVC)
  if (BUILD_SHARED_LIBS)
    set_target_properties (H5_test_buildiface
        PROPERTIES
            COMPILE_FLAGS "/MT"
    )
  endif (BUILD_SHARED_LIBS)
  set_target_properties (H5_test_buildiface
      PROPERTIES
          LINK_FLAGS "/SUBSYSTEM:CONSOLE"
  )
endif (WIN32 AND MSVC)
set_target_properties (H5_test_buildiface PROPERTIES
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

if (NOT SKIP_HDF5_FORTRAN_SHARED)
  if (WIN32)
    set (MOD_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/static/\${BUILD_TYPE})
  else (WIN32)
    set (MOD_BUILD_DIR ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
  endif (WIN32)
endif (NOT SKIP_HDF5_FORTRAN_SHARED)

INCLUDE_DIRECTORIES (${CMAKE_Fortran_MODULE_DIRECTORY} ${MOD_BUILD_DIR})

#-----------------------------------------------------------------------------
# Add Test Lib
#-----------------------------------------------------------------------------
add_library (${HDF5_F90_C_TEST_LIB_TARGET} STATIC t.c)
set_source_files_properties (t.c PROPERTIES LANGUAGE C)
TARGET_C_PROPERTIES (${HDF5_F90_C_TEST_LIB_TARGET} STATIC " " " ")
target_link_libraries (${HDF5_F90_C_TEST_LIB_TARGET}
    ${HDF5_F90_C_LIB_TARGET}
    ${HDF5_TEST_LIB_TARGET}
)
H5_SET_LIB_OPTIONS (${HDF5_F90_C_TEST_LIB_TARGET} ${HDF5_F90_C_TEST_LIB_NAME} STATIC)
set_target_properties (${HDF5_F90_C_TEST_LIB_TARGET} PROPERTIES
    FOLDER libraries/test/fortran
    LINKER_LANGUAGE C
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_library (${HDF5_F90_C_TEST_LIBSH_TARGET} SHARED t.c)
  TARGET_C_PROPERTIES (${HDF5_F90_C_TEST_LIBSH_TARGET} SHARED " " " ")
  target_link_libraries (${HDF5_F90_C_TEST_LIBSH_TARGET}
      ${HDF5_F90_C_LIBSH_TARGET}
      ${HDF5_TEST_LIBSH_TARGET}
  )
  H5_SET_LIB_OPTIONS (${HDF5_F90_C_TEST_LIBSH_TARGET} ${HDF5_F90_C_TEST_LIB_NAME} SHARED ${HDF5_PACKAGE_SOVERSION})
  set_target_properties (${HDF5_F90_C_TEST_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/test/fortran
      LINKER_LANGUAGE C
      COMPILE_DEFINITIONS "H5_BUILT_AS_DYNAMIC_LIB"
      INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
      INTERFACE_COMPILE_DEFINITIONS H5_BUILT_AS_DYNAMIC_LIB=1
  )
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

set (HDF5_F90_TF_SRCS
     # generated files
     ${HDF5_F90_BINARY_DIR}/tf_gen.F90
)
set_source_files_properties (
    ${HDF5_F90_BINARY_DIR}/tf_gen.F90
    PROPERTIES GENERATED TRUE
)
set_source_files_properties (tf.F90 ${HDF5_F90_BINARY_DIR}/${HDF5_F90_TF_SRCS} PROPERTIES LANGUAGE Fortran)

set (CMD $<TARGET_FILE:H5_test_buildiface>)
add_custom_target (H5testgen ALL
    COMMAND ${CMD}
#v3.2    BYPRODUCT ${HDF5_F90_BINARY_DIR}/tf_gen.F90
    WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}
    DEPENDS H5_test_buildiface
)

add_library (${HDF5_F90_TEST_LIB_TARGET} STATIC tf.F90 ${HDF5_F90_TF_SRCS})
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
   add_library (${HDF5_F90_TEST_LIBSH_TARGET} SHARED tf.F90 ${HDF5_F90_TF_SRCS})
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

TARGET_FORTRAN_PROPERTIES (${HDF5_F90_TEST_LIB_TARGET} STATIC " " " ")
target_link_libraries (${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_C_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
H5_SET_LIB_OPTIONS (${HDF5_F90_TEST_LIB_TARGET} ${HDF5_F90_TEST_LIB_NAME} STATIC)
target_include_directories (${HDF5_F90_TEST_LIB_TARGET} PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
set_target_properties (${HDF5_F90_TEST_LIB_TARGET} PROPERTIES
    FOLDER libraries/test/fortran
    LINKER_LANGUAGE Fortran
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
)
if (WIN32)
  set_property (TARGET ${HDF5_F90_TEST_LIB_TARGET} APPEND PROPERTY
        COMPILE_DEFINITIONS "HDF5F90_WINDOWS"
    )
endif (WIN32)
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  set (SHARED_LINK_FLAGS " ")
  if (WIN32 AND MSVC)
    set (SHARED_LINK_FLAGS "/DLL")
  endif (WIN32 AND MSVC)
  TARGET_FORTRAN_PROPERTIES (${HDF5_F90_TEST_LIBSH_TARGET} SHARED " " ${SHARED_LINK_FLAGS})
  target_link_libraries (${HDF5_F90_TEST_LIBSH_TARGET}
      ${HDF5_F90_C_TEST_LIBSH_TARGET}
      ${HDF5_F90_LIBSH_TARGET}
      ${HDF5_LIBSH_TARGET}
  )
  H5_SET_LIB_OPTIONS (${HDF5_F90_TEST_LIBSH_TARGET} ${HDF5_F90_TEST_LIB_NAME} SHARED ${HDF5_PACKAGE_SOVERSION})
  target_include_directories (${HDF5_F90_TEST_LIBSH_TARGET} PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
  set_target_properties (${HDF5_F90_TEST_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/test/fortran
      LINKER_LANGUAGE Fortran
      COMPILE_DEFINITIONS "H5_BUILT_AS_DYNAMIC_LIB"
      INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
      INTERFACE_COMPILE_DEFINITIONS H5_BUILT_AS_DYNAMIC_LIB=1
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  if (WIN32)
    set_property (TARGET ${HDF5_F90_TEST_LIBSH_TARGET} APPEND PROPERTY
        COMPILE_DEFINITIONS "BUILD_HDF5_TEST_DLL;HDF5F90_WINDOWS"
    )
  endif (WIN32)
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

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
TARGET_NAMING (testhdf5_fortran STATIC)
TARGET_FORTRAN_PROPERTIES (testhdf5_fortran STATIC " " " ")
target_link_libraries (testhdf5_fortran
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
if (WIN32 AND MSVC)
  target_link_libraries (testhdf5_fortran "ws2_32.lib")
endif (WIN32 AND MSVC)
target_include_directories (testhdf5_fortran PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
set_target_properties (testhdf5_fortran PROPERTIES
    LINKER_LANGUAGE Fortran
    FOLDER test/fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
)
add_dependencies (testhdf5_fortran ${HDF5_F90_TEST_LIB_TARGET})

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_executable (testhdf5_fortran-shared
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
  TARGET_NAMING (testhdf5_fortran-shared SHARED)
  TARGET_FORTRAN_PROPERTIES (testhdf5_fortran-shared SHARED " " " ")
  target_link_libraries (testhdf5_fortran-shared
      ${HDF5_F90_TEST_LIBSH_TARGET}
      ${HDF5_F90_LIBSH_TARGET}
      ${HDF5_LIBSH_TARGET}
  )
  if (WIN32 AND MSVC)
    target_link_libraries (testhdf5_fortran-shared "ws2_32.lib")
  endif (WIN32 AND MSVC)
  target_include_directories (testhdf5_fortran-shared PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
  set_target_properties (testhdf5_fortran-shared PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  add_dependencies (testhdf5_fortran-shared ${HDF5_F90_TEST_LIBSH_TARGET})
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

#-- Adding test for testhdf5_fortran_1_8
add_executable (testhdf5_fortran_1_8
    fortranlib_test_1_8.F90
    tH5O.F90
    tH5A_1_8.F90
    tH5G_1_8.F90
    tH5MISC_1_8.F90
    tHDF5_1_8.F90
)
TARGET_NAMING (testhdf5_fortran_1_8 STATIC)
TARGET_FORTRAN_PROPERTIES (testhdf5_fortran_1_8 STATIC " " " ")
target_link_libraries (testhdf5_fortran_1_8
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
if (WIN32 AND MSVC)
  target_link_libraries (testhdf5_fortran_1_8 "ws2_32.lib")
endif (WIN32 AND MSVC)
target_include_directories (testhdf5_fortran_1_8 PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
set_target_properties (testhdf5_fortran_1_8 PROPERTIES
    LINKER_LANGUAGE Fortran
    FOLDER test/fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
)
add_dependencies (testhdf5_fortran_1_8 ${HDF5_F90_TEST_LIB_TARGET})

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_executable (testhdf5_fortran_1_8-shared
      fortranlib_test_1_8.F90
      tH5O.F90
      tH5A_1_8.F90
      tH5G_1_8.F90
      tH5MISC_1_8.F90
      tHDF5_1_8.F90
  )
  TARGET_NAMING (testhdf5_fortran_1_8-shared SHARED)
  TARGET_FORTRAN_PROPERTIES (testhdf5_fortran_1_8-shared SHARED " " " ")
  target_link_libraries (testhdf5_fortran_1_8-shared
      ${HDF5_F90_TEST_LIBSH_TARGET}
      ${HDF5_F90_LIBSH_TARGET}
      ${HDF5_LIBSH_TARGET}
  )
  if (WIN32 AND MSVC)
    target_link_libraries (testhdf5_fortran_1_8-shared "ws2_32.lib")
  endif (WIN32 AND MSVC)
  target_include_directories (testhdf5_fortran_1_8-shared PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
  set_target_properties (testhdf5_fortran_1_8-shared PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  add_dependencies (testhdf5_fortran_1_8-shared ${HDF5_F90_TEST_LIBSH_TARGET})
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

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
TARGET_NAMING (fortranlib_test_F03 STATIC)
TARGET_FORTRAN_PROPERTIES (fortranlib_test_F03 STATIC " " " ")
target_link_libraries (fortranlib_test_F03
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
if (WIN32 AND MSVC)
  target_link_libraries (fortranlib_test_F03 "ws2_32.lib")
endif (WIN32 AND MSVC)
target_include_directories (fortranlib_test_F03 PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
set_target_properties (fortranlib_test_F03 PROPERTIES
    LINKER_LANGUAGE Fortran
    FOLDER test/fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
)
add_dependencies (fortranlib_test_F03 ${HDF5_F90_TEST_LIB_TARGET})

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_executable (fortranlib_test_F03-shared
      fortranlib_test_F03.F90
      tH5E_F03.F90
      tH5F_F03.F90
      tH5L_F03.F90
      tH5O_F03.F90
      tH5P_F03.F90
      tH5T_F03.F90
      tHDF5_F03.F90
  )
  TARGET_NAMING (fortranlib_test_F03-shared SHARED)
  TARGET_FORTRAN_PROPERTIES (fortranlib_test_F03-shared SHARED " " " ")
  target_link_libraries (fortranlib_test_F03-shared
      ${HDF5_F90_TEST_LIBSH_TARGET}
      ${HDF5_F90_LIBSH_TARGET}
      ${HDF5_LIBSH_TARGET}
  )
  if (WIN32 AND MSVC)
    target_link_libraries (fortranlib_test_F03-shared "ws2_32.lib")
  endif (WIN32 AND MSVC)
  target_include_directories (fortranlib_test_F03-shared PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
  set_target_properties (fortranlib_test_F03-shared PROPERTIES
      LINKER_LANGUAGE Fortran
      FOLDER test/fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  add_dependencies (fortranlib_test_F03-shared ${HDF5_F90_TEST_LIBSH_TARGET})
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

#-- Adding test for fflush1
add_executable (fflush1 fflush1.F90)
TARGET_NAMING (fflush1 STATIC)
TARGET_FORTRAN_PROPERTIES (fflush1 STATIC " " " ")
target_link_libraries (fflush1
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
if (WIN32 AND MSVC)
  target_link_libraries (fflush1 "ws2_32.lib")
endif (WIN32 AND MSVC)
target_include_directories (fflush1 PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
set_target_properties (fflush1 PROPERTIES
    LINKER_LANGUAGE Fortran
    FOLDER test/fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
)
add_dependencies (fflush1 ${HDF5_F90_TEST_LIB_TARGET})

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_executable (fflush1-shared fflush1.F90)
  TARGET_NAMING (fflush1-shared SHARED)
  TARGET_FORTRAN_PROPERTIES (fflush1-shared SHARED " " " ")
  target_link_libraries (fflush1-shared
      ${HDF5_F90_LIBSH_TARGET}
      ${HDF5_F90_TEST_LIBSH_TARGET}
      ${HDF5_LIBSH_TARGET}
  )
  if (WIN32 AND MSVC)
    target_link_libraries (fflush1-shared "ws2_32.lib")
  endif (WIN32 AND MSVC)
  target_include_directories (fflush1-shared PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
  set_target_properties (fflush1-shared PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  add_dependencies (fflush1-shared ${HDF5_F90_TEST_LIBSH_TARGET})
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

#-- Adding test for fflush2
add_executable (fflush2 fflush2.F90)
TARGET_NAMING (fflush2 STATIC)
TARGET_FORTRAN_PROPERTIES (fflush2 STATIC " " " ")
target_link_libraries (fflush2
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
if (WIN32 AND MSVC)
  target_link_libraries (fflush2 "ws2_32.lib")
endif (WIN32 AND MSVC)
target_include_directories (fflush2 PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
set_target_properties (fflush2 PROPERTIES
    LINKER_LANGUAGE Fortran
    FOLDER test/fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
)
add_dependencies (fflush2 ${HDF5_F90_TEST_LIB_TARGET})

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_executable (fflush2-shared fflush2.F90)
  TARGET_NAMING (fflush2-shared SHARED)
  TARGET_FORTRAN_PROPERTIES (fflush2-shared SHARED " " " ")
  target_link_libraries (fflush2-shared
      ${HDF5_F90_TEST_LIBSH_TARGET}
      ${HDF5_F90_LIBSH_TARGET}
      ${HDF5_LIBSH_TARGET}
  )
  if (WIN32 AND MSVC)
    target_link_libraries (fflush2-shared "ws2_32.lib")
  endif (WIN32 AND MSVC)
  target_include_directories (fflush2-shared PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
  set_target_properties (fflush2-shared PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
  add_dependencies (fflush2-shared ${HDF5_F90_TEST_LIBSH_TARGET})
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

include (CMakeTests.cmake)
