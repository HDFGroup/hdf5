#-------------------------------------------------------------------------------
MACRO (H5_SET_LIB_OPTIONS libtarget libname libtype)
  SET (LIB_OUT_NAME "${libname}")
  IF (${libtype} MATCHES "SHARED")
    IF (WIN32)
      SET (LIBHDF_VERSION ${HDF5_PACKAGE_VERSION_MAJOR})
    ELSE (WIN32)
      SET (LIBHDF_VERSION ${HDF5_PACKAGE_VERSION})
    ENDIF (WIN32)
    SET_TARGET_PROPERTIES (${libtarget} PROPERTIES VERSION ${LIBHDF_VERSION})
    IF (WIN32)
        SET (${LIB_OUT_NAME} "${LIB_OUT_NAME}-${HDF5_PACKAGE_SOVERSION}")
    ELSE (WIN32)
        SET_TARGET_PROPERTIES (${libtarget} PROPERTIES SOVERSION ${HDF5_PACKAGE_SOVERSION})
    ENDIF (WIN32)
  ENDIF (${libtype} MATCHES "SHARED")
  HDF_SET_LIB_OPTIONS (${libtarget} ${LIB_OUT_NAME} ${libtype})

  #-- Apple Specific install_name for libraries
  IF (APPLE)
    OPTION (HDF5_BUILD_WITH_INSTALL_NAME "Build with library install_name set to the installation path" OFF)
    IF (HDF5_BUILD_WITH_INSTALL_NAME)
      SET_TARGET_PROPERTIES(${libtarget} PROPERTIES
          LINK_FLAGS "-current_version ${HDF5_PACKAGE_VERSION} -compatibility_version ${HDF5_PACKAGE_VERSION}"
          INSTALL_NAME_DIR "${CMAKE_INSTALL_PREFIX}/lib"
          BUILD_WITH_INSTALL_RPATH ${HDF5_BUILD_WITH_INSTALL_NAME}
      )
    ENDIF (HDF5_BUILD_WITH_INSTALL_NAME)
  ENDIF (APPLE)

ENDMACRO (H5_SET_LIB_OPTIONS)
