cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_FORTRAN_TESTS C CXX Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_F90_BINARY_DIR} ${HDF5_F90_SRC_DIR}/src)

#-----------------------------------------------------------------------------
# Add Test Lib
#-----------------------------------------------------------------------------
add_library (${HDF5_F90_C_TEST_LIB_TARGET} ${LIB_TYPE} t.c)
set_source_files_properties (t.c PROPERTIES LANGUAGE C)
TARGET_C_PROPERTIES (${HDF5_F90_C_TEST_LIB_TARGET} ${LIB_TYPE} " " " ")
target_link_libraries (${HDF5_F90_C_TEST_LIB_TARGET}
    ${HDF5_F90_C_LIB_TARGET}
    ${HDF5_TEST_LIB_TARGET}
)
H5_SET_LIB_OPTIONS (${HDF5_F90_C_TEST_LIB_TARGET} ${HDF5_F90_C_TEST_LIB_NAME} ${LIB_TYPE})
set_target_properties (${HDF5_F90_C_TEST_LIB_TARGET} PROPERTIES
    FOLDER libraries/test/fortran
    LINKER_LANGUAGE C
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)

set_source_files_properties (tf.F90 ${HDF5_F90_BINARY_DIR}/tf_gen.F90 PROPERTIES LANGUAGE Fortran)
add_library (${HDF5_F90_TEST_LIB_TARGET} ${LIB_TYPE} tf.F90 ${HDF5_F90_BINARY_DIR}/tf_gen.F90)

set (SHARED_LINK_FLAGS " ")
if (WIN32)
  if (BUILD_SHARED_LIBS)
    set_property (TARGET ${HDF5_F90_TEST_LIB_TARGET} APPEND PROPERTY COMPILE_DEFINITIONS BUILD_HDF5_TEST_DLL)
    if (MSVC)
      set (SHARED_LINK_FLAGS "/DLL")
    endif (MSVC)
  endif (BUILD_SHARED_LIBS)
  set_property (TARGET ${HDF5_F90_TEST_LIB_TARGET} APPEND PROPERTY COMPILE_DEFINITIONS HDF5F90_WINDOWS)
endif (WIN32)
TARGET_FORTRAN_PROPERTIES (${HDF5_F90_TEST_LIB_TARGET} ${LIB_TYPE} " " ${SHARED_LINK_FLAGS})
target_link_libraries (${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_C_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
H5_SET_LIB_OPTIONS (${HDF5_F90_TEST_LIB_TARGET} ${HDF5_F90_TEST_LIB_NAME} ${LIB_TYPE})
set_target_properties (${HDF5_F90_TEST_LIB_TARGET} PROPERTIES
    FOLDER libraries/test/fortran
    LINKER_LANGUAGE Fortran
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

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

set (CMD $<TARGET_FILE:H5_test_buildiface>)
add_custom_command (
    OUTPUT ${HDF5_F90_BINARY_DIR}/tf_gen.F90
    COMMAND ${CMD}
    WORKING_DIRECTORY ${HDF5_F90_BINARY_DIR}
    DEPENDS H5_test_buildiface
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
TARGET_FORTRAN_PROPERTIES (testhdf5_fortran ${LIB_TYPE} " " " ")
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
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

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
TARGET_FORTRAN_PROPERTIES (testhdf5_fortran_1_8 ${LIB_TYPE} " " " ")
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
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

#-- Adding test for fortranlib_test_F03
add_executable (fortranlib_test_F03
    fortranlib_test_F03.f90
    tH5E_F03.f90
    tH5F_F03.f90
    tH5L_F03.f90
    tH5O_F03.f90
    tH5P_F03.f90
    tH5T_F03.F90
    tHDF5_F03.f90
  )
TARGET_NAMING (fortranlib_test_F03 ${LIB_TYPE})
TARGET_FORTRAN_PROPERTIES (fortranlib_test_F03 ${LIB_TYPE} " " " ")
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
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

#-- Adding test for fflush1
add_executable (fflush1 fflush1.f90)
TARGET_NAMING (fflush1 ${LIB_TYPE})
TARGET_FORTRAN_PROPERTIES (fflush1 ${LIB_TYPE} " " " ")
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
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

#-- Adding test for fflush2
add_executable (fflush2 fflush2.f90)
TARGET_NAMING (fflush2 ${LIB_TYPE})
TARGET_FORTRAN_PROPERTIES (fflush2 ${LIB_TYPE} " " " ")
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
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)

include (CMakeTests.cmake)
