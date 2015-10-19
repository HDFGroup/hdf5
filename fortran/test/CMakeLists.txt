cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_FORTRAN_TESTS C CXX Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_F90_BINARY_DIR} ${HDF5_F90_SRC_DIR}/src)

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

# See if the F2008 intrinsic STORAGE_SIZE and C_SIZEOF are supported. If not then
# fall back to F2003. If F2003 not supported then use F90 for the tests.

set_source_files_properties (tf_F90.f90 tf_F03.f90 tf_F08.f90 tf.f90 PROPERTIES LANGUAGE Fortran)
if (FORTRAN_HAVE_STORAGE_SIZE AND FORTRAN_HAVE_C_SIZEOF)
  add_library (${HDF5_F90_TEST_LIB_TARGET} STATIC tf_F08.f90 tf.f90)
  if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
    add_library (${HDF5_F90_TEST_LIBSH_TARGET} SHARED tf_F08.f90 tf.f90)
  endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
elseif (HDF5_ENABLE_F2003)
  add_library (${HDF5_F90_TEST_LIB_TARGET} STATIC tf_F03.f90 tf.f90)
  if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
    add_library (${HDF5_F90_TEST_LIBSH_TARGET} SHARED tf_F03.f90 tf.f90)
  endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
else (FORTRAN_HAVE_STORAGE_SIZE AND FORTRAN_HAVE_C_SIZEOF)
  add_library (${HDF5_F90_TEST_LIB_TARGET} STATIC tf_F90.f90 tf.f90)
  if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
    add_library (${HDF5_F90_TEST_LIBSH_TARGET} SHARED tf_F90.f90 tf.f90)
  endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
endif (FORTRAN_HAVE_STORAGE_SIZE AND FORTRAN_HAVE_C_SIZEOF)

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
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_executable (testhdf5_fortran-shared
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
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

#-- Adding test for testhdf5_fortran_1_8
add_executable (testhdf5_fortran_1_8
    fortranlib_test_1_8.f90
    tH5O.f90
    tH5A_1_8.f90
    tH5G_1_8.f90
    tH5MISC_1_8.f90
    tHDF5_1_8.f90
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
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_executable (testhdf5_fortran_1_8-shared
      fortranlib_test_1_8.f90
      tH5O.f90
      tH5A_1_8.f90
      tH5G_1_8.f90
      tH5MISC_1_8.f90
      tHDF5_1_8.f90
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
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

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
  if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
    add_executable (fortranlib_test_F03-shared
        fortranlib_test_F03.f90
        tH5E_F03.f90
        tH5F_F03.f90
        tH5L_F03.f90
        tH5O_F03.f90
        tH5P_F03.f90
        tH5T_F03.f90
        tHDF5_F03.f90
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
  endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
endif (HDF5_ENABLE_F2003)

#-- Adding test for fflush1
add_executable (fflush1 fflush1.f90)
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
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_executable (fflush1-shared fflush1.f90)
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
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

#-- Adding test for fflush2
add_executable (fflush2 fflush2.f90)
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
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_executable (fflush2-shared fflush2.f90)
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
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)

include (CMakeTests.cmake)
