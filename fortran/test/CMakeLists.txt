cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5_FORTRAN_TESTS C CXX Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${CMAKE_Fortran_MODULE_DIRECTORY} ${HDF5_F90_BINARY_DIR} ${HDF5_F90_SRC_DIR}/src)

#-----------------------------------------------------------------------------
# Add Test Lib
#-----------------------------------------------------------------------------
add_library (${HDF5_F90_C_TEST_LIB_TARGET} ${LIB_TYPE} t.c)
TARGET_C_PROPERTIES (${HDF5_F90_C_TEST_LIB_TARGET} " " " ")
target_link_libraries (${HDF5_F90_C_TEST_LIB_TARGET}
    ${HDF5_F90_C_LIB_TARGET}
    ${HDF5_TEST_LIB_TARGET}
)
H5_SET_LIB_OPTIONS (${HDF5_F90_C_TEST_LIB_TARGET} ${HDF5_F90_C_TEST_LIB_NAME} ${LIB_TYPE})
set_target_properties (${HDF5_F90_C_TEST_LIB_TARGET} PROPERTIES
    FOLDER libraries/test/fortran
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)

# See if the F2008 intrinsic STORAGE_SIZE and C_SIZEOF are supported. If not then
# fall back to F2003. If F2003 not supported then use F90 for the tests.

if (FORTRAN_HAVE_STORAGE_SIZE AND FORTRAN_HAVE_C_SIZEOF)
 add_library (${HDF5_F90_TEST_LIB_TARGET} ${LIB_TYPE} tf_F08.f90 tf.f90)
elseif (HDF5_ENABLE_F2003)
 add_library (${HDF5_F90_TEST_LIB_TARGET} ${LIB_TYPE} tf_F03.f90 tf.f90)
else (FORTRAN_HAVE_STORAGE_SIZE AND FORTRAN_HAVE_C_SIZEOF)
 add_library (${HDF5_F90_TEST_LIB_TARGET} ${LIB_TYPE} tf_F90.f90 tf.f90) 
endif (FORTRAN_HAVE_STORAGE_SIZE AND FORTRAN_HAVE_C_SIZEOF)


set (SHARED_LINK_FLAGS " ")
if (WIN32)
  if (BUILD_SHARED_LIBS)
    set_property (TARGET ${HDF5_F90_TEST_LIB_TARGET} APPEND PROPERTY COMPILE_DEFINITIONS BUILD_HDF5_DLL)
    if (MSVC)
      set (SHARED_LINK_FLAGS "/DLL")
    endif (MSVC)
  endif (BUILD_SHARED_LIBS)
  set_property (TARGET ${HDF5_F90_TEST_LIB_TARGET} APPEND PROPERTY COMPILE_DEFINITIONS HDF5F90_WINDOWS)
endif (WIN32)
TARGET_FORTRAN_PROPERTIES (${HDF5_F90_TEST_LIB_TARGET} " " ${SHARED_LINK_FLAGS})
set_target_properties (${HDF5_F90_TEST_LIB_TARGET} PROPERTIES LINKER_LANGUAGE Fortran)
target_link_libraries (${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_C_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
H5_SET_LIB_OPTIONS (${HDF5_F90_TEST_LIB_TARGET} ${HDF5_F90_TEST_LIB_NAME} ${LIB_TYPE})
set_target_properties (${HDF5_F90_TEST_LIB_TARGET} PROPERTIES
    FOLDER libraries/test/fortran
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)

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
TARGET_NAMING (testhdf5_fortran ${LIB_TYPE})
TARGET_FORTRAN_PROPERTIES (testhdf5_fortran " " " ")
target_link_libraries (testhdf5_fortran 
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
if (WIN32 AND MSVC)
  target_link_libraries (testhdf5_fortran "ws2_32.lib")
endif (WIN32 AND MSVC)
set_target_properties (testhdf5_fortran PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (testhdf5_fortran PROPERTIES FOLDER test/fortran)

#-- Adding test for testhdf5_fortran_1_8
add_executable (testhdf5_fortran_1_8
    fortranlib_test_1_8.f90
    tH5O.f90
    tH5A_1_8.f90
    tH5G_1_8.f90
    tH5MISC_1_8.f90
    tHDF5_1_8.f90
)
TARGET_NAMING (testhdf5_fortran_1_8 ${LIB_TYPE})
TARGET_FORTRAN_PROPERTIES (testhdf5_fortran_1_8 " " " ")
target_link_libraries (testhdf5_fortran_1_8 
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
if (WIN32 AND MSVC)
  target_link_libraries (testhdf5_fortran_1_8 "ws2_32.lib")
endif (WIN32 AND MSVC)
set_target_properties (testhdf5_fortran_1_8 PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (testhdf5_fortran_1_8 PROPERTIES FOLDER test/fortran)

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
  TARGET_NAMING (fortranlib_test_F03 ${LIB_TYPE})
  TARGET_FORTRAN_PROPERTIES (fortranlib_test_F03 " " " ")
  target_link_libraries (fortranlib_test_F03 
      ${HDF5_F90_TEST_LIB_TARGET}
      ${HDF5_F90_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
  if (WIN32 AND MSVC)
    target_link_libraries (fortranlib_test_F03 "ws2_32.lib")
  endif (WIN32 AND MSVC)
  set_target_properties (fortranlib_test_F03 PROPERTIES LINKER_LANGUAGE Fortran)
  set_target_properties (fortranlib_test_F03 PROPERTIES FOLDER test/fortran)
endif (HDF5_ENABLE_F2003)

#-- Adding test for fflush1
add_executable (fflush1 fflush1.f90)
TARGET_NAMING (fflush1 ${LIB_TYPE})
TARGET_FORTRAN_PROPERTIES (fflush1 " " " ")
target_link_libraries (fflush1 
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
if (WIN32 AND MSVC)
  target_link_libraries (fflush1 "ws2_32.lib")
endif (WIN32 AND MSVC)
set_target_properties (fflush1 PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (fflush1 PROPERTIES FOLDER test/fortran)

#-- Adding test for fflush2
add_executable (fflush2 fflush2.f90)
TARGET_NAMING (fflush2 ${LIB_TYPE})
TARGET_FORTRAN_PROPERTIES (fflush2 " " " ")
target_link_libraries (fflush2 
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
if (WIN32 AND MSVC)
  target_link_libraries (fflush2 "ws2_32.lib")
endif (WIN32 AND MSVC)
set_target_properties (fflush2 PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (fflush2 PROPERTIES FOLDER test/fortran)

include (CMakeTests.cmake)
