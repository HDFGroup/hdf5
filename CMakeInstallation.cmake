
#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
IF (NOT HDF5_INSTALL_NO_DEVELOPMENT)
  INSTALL (
      FILES ${PROJECT_BINARY_DIR}/H5pubconf.h
      DESTINATION ${HDF5_INSTALL_INCLUDE_DIR}
      COMPONENT headers
  )
ENDIF (NOT HDF5_INSTALL_NO_DEVELOPMENT)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
IF (NOT HDF5_EXTERNALLY_CONFIGURED)
  INSTALL (
      EXPORT ${HDF5_EXPORTED_TARGETS}
      DESTINATION ${HDF5_INSTALL_CMAKE_DIR}/${HDF5_PACKAGE}
      FILE ${HDF5_PACKAGE}${HDF_PACKAGE_EXT}-targets.cmake
      COMPONENT configinstall
  )
ENDIF (NOT HDF5_EXTERNALLY_CONFIGURED)

#-----------------------------------------------------------------------------
# Export all exported targets to the build tree for use by parent project
#-----------------------------------------------------------------------------
IF (NOT HDF5_EXTERNALLY_CONFIGURED)
  EXPORT (
      TARGETS ${HDF5_LIBRARIES_TO_EXPORT} ${HDF5_LIB_DEPENDENCIES}
      FILE ${HDF5_PACKAGE}${HDF_PACKAGE_EXT}-targets.cmake
  )
ENDIF (NOT HDF5_EXTERNALLY_CONFIGURED)

#-----------------------------------------------------------------------------
# Configure the hdf5-config.cmake file for the build directory
#-----------------------------------------------------------------------------
SET (HDF5_INCLUDES_BUILD_TIME
    ${HDF5_SRC_DIR} ${HDF5_CPP_SRC_DIR} ${HDF5_HL_SRC_DIR}
    ${HDF5_TOOLS_SRC_DIR} ${HDF5_BINARY_DIR}
)
SET (HDF5_VERSION_STRING @HDF5_PACKAGE_VERSION@)
SET (HDF5_VERSION_MAJOR  @HDF5_PACKAGE_VERSION_MAJOR@)
SET (HDF5_VERSION_MINOR  @HDF5_PACKAGE_VERSION_MINOR@)

CONFIGURE_FILE (
    ${HDF5_RESOURCES_DIR}/hdf5-config.cmake.build.in 
    ${HDF5_BINARY_DIR}/${HDF5_PACKAGE}${HDF_PACKAGE_EXT}-config.cmake @ONLY
)

#-----------------------------------------------------------------------------
# Configure the FindHDF5.cmake file for the install directory
#-----------------------------------------------------------------------------
IF (NOT HDF5_EXTERNALLY_CONFIGURED)
  CONFIGURE_FILE (
      ${HDF5_RESOURCES_DIR}/FindHDF5.cmake.in 
      ${HDF5_BINARY_DIR}/CMakeFiles/FindHDF5${HDF_PACKAGE_EXT}.cmake @ONLY
  )
  INSTALL (
      FILES ${HDF5_BINARY_DIR}/CMakeFiles/FindHDF5${HDF_PACKAGE_EXT}.cmake
      DESTINATION ${HDF5_INSTALL_CMAKE_DIR}/${HDF5_PACKAGE}
      COMPONENT configinstall
  )
ENDIF (NOT HDF5_EXTERNALLY_CONFIGURED)

#-----------------------------------------------------------------------------
# Configure the hdf5-config.cmake file for the install directory
#-----------------------------------------------------------------------------
IF (NOT HDF5_EXTERNALLY_CONFIGURED)
  CONFIGURE_FILE (
      ${HDF5_RESOURCES_DIR}/hdf5-config.cmake.install.in
      ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_PACKAGE}${HDF_PACKAGE_EXT}-config.cmake @ONLY
  )
  INSTALL (
      FILES ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_PACKAGE}${HDF_PACKAGE_EXT}-config.cmake
      DESTINATION ${HDF5_INSTALL_CMAKE_DIR}/${HDF5_PACKAGE}
      COMPONENT configinstall
  )
ENDIF (NOT HDF5_EXTERNALLY_CONFIGURED)

#-----------------------------------------------------------------------------
# Configure the hdf5-config-version .cmake file for the install directory
#-----------------------------------------------------------------------------
IF (NOT HDF5_EXTERNALLY_CONFIGURED)
  CONFIGURE_FILE (
      ${HDF5_RESOURCES_DIR}/hdf5-config-version.cmake.in
      ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_PACKAGE}${HDF_PACKAGE_EXT}-config-version.cmake @ONLY
  )
  INSTALL (
      FILES ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_PACKAGE}${HDF_PACKAGE_EXT}-config-version.cmake
      DESTINATION ${HDF5_INSTALL_CMAKE_DIR}/${HDF5_PACKAGE}
      COMPONENT configinstall
  )
ENDIF (NOT HDF5_EXTERNALLY_CONFIGURED)

#-----------------------------------------------------------------------------
# Configure the libhdf5.settings file for the lib info
#-----------------------------------------------------------------------------
IF (H5_WORDS_BIGENDIAN)
  SET (BYTESEX big-endian)
ELSE (H5_WORDS_BIGENDIAN)
  SET (BYTESEX little-endian)
ENDIF (H5_WORDS_BIGENDIAN)
CONFIGURE_FILE (
    ${HDF5_RESOURCES_DIR}/libhdf5.settings.cmake.in 
    ${HDF5_BINARY_DIR}/libhdf5.settings @ONLY
)
INSTALL (
    FILES ${HDF5_BINARY_DIR}/libhdf5.settings
    DESTINATION ${HDF5_INSTALL_CMAKE_DIR}/${HDF5_PACKAGE}
    COMPONENT libraries
)

#-----------------------------------------------------------------------------
# Configure the HDF518_Examples.cmake file and the examples
#-----------------------------------------------------------------------------
OPTION (HDF5_PACK_EXAMPLES  "Package the HDF5 Library Examples Compressed File" OFF)
IF (HDF5_PACK_EXAMPLES)
  CONFIGURE_FILE (
      ${HDF5_RESOURCES_DIR}/HDF518_Examples.cmake.in 
      ${HDF5_BINARY_DIR}/HDF518_Examples.cmake @ONLY
  )
  INSTALL (
      FILES ${HDF5_BINARY_DIR}/HDF518_Examples.cmake
      DESTINATION ${HDF5_INSTALL_DATA_DIR}
      COMPONENT hdfdocuments
  )
  IF (EXISTS "${HDF5_EXAMPLES_COMPRESSED_DIR}/${HDF5_EXAMPLES_COMPRESSED}")
    INSTALL (
        FILES
            ${HDF5_EXAMPLES_COMPRESSED_DIR}/${HDF5_EXAMPLES_COMPRESSED}
            ${HDF5_SOURCE_DIR}/release_docs/USING_CMake_Examples.txt
        DESTINATION ${HDF5_INSTALL_DATA_DIR}
        COMPONENT hdfdocuments
    )
  ENDIF (EXISTS "${HDF5_EXAMPLES_COMPRESSED_DIR}/${HDF5_EXAMPLES_COMPRESSED}")
ENDIF (HDF5_PACK_EXAMPLES)

#-----------------------------------------------------------------------------
# Configure the README.txt file for the binary package
#-----------------------------------------------------------------------------
SET (BINARY_SYSTEM_NAME ${CMAKE_SYSTEM_NAME})
SET (BINARY_PLATFORM "${CMAKE_SYSTEM_NAME}")
IF (WIN32)
  SET (BINARY_EXAMPLE_ENDING "zip")
  SET (BINARY_INSTALL_ENDING "exe")
  IF (CMAKE_CL_64)
    SET (BINARY_SYSTEM_NAME "win64")
  ELSE (CMAKE_CL_64)
    SET (BINARY_SYSTEM_NAME "win32")
  ENDIF (CMAKE_CL_64)
  IF (${CMAKE_SYSTEM_VERSION} MATCHES "6.1")
    SET (BINARY_PLATFORM "${BINARY_PLATFORM} 7")
  ELSEIF (${CMAKE_SYSTEM_VERSION} MATCHES "6.2")
    SET (BINARY_PLATFORM "${BINARY_PLATFORM} 8")
  ENDIF (${CMAKE_SYSTEM_VERSION} MATCHES "6.1")
  SET (BINARY_PLATFORM "${BINARY_PLATFORM} ${MSVC_C_ARCHITECTURE_ID}")
  IF (${CMAKE_C_COMPILER_VERSION} MATCHES "16.*")
    SET (BINARY_PLATFORM "${BINARY_PLATFORM}, using VISUAL STUDIO 2010")
  ELSEIF (${CMAKE_C_COMPILER_VERSION} MATCHES "15.*")
    SET (BINARY_PLATFORM "${BINARY_PLATFORM}, using VISUAL STUDIO 2008")
  ELSEIF (${CMAKE_C_COMPILER_VERSION} MATCHES "17.*")
    SET (BINARY_PLATFORM "${BINARY_PLATFORM}, using VISUAL STUDIO 2012")
  ELSE (${CMAKE_C_COMPILER_VERSION} MATCHES "16.*")
    SET (BINARY_PLATFORM "${BINARY_PLATFORM}, using VISUAL STUDIO ${CMAKE_C_COMPILER_VERSION}")
  ENDIF (${CMAKE_C_COMPILER_VERSION} MATCHES "16.*")
ELSEIF (APPLE)
  SET (BINARY_EXAMPLE_ENDING "tar.gz")
  SET (BINARY_INSTALL_ENDING "dmg")
  SET (BINARY_PLATFORM "${BINARY_PLATFORM} ${CMAKE_SYSTEM_VERSION} ${CMAKE_SYSTEM_PROCESSOR}")
  SET (BINARY_PLATFORM "${BINARY_PLATFORM}, using ${CMAKE_C_COMPILER_ID} C ${CMAKE_C_COMPILER_VERSION}")
ELSE (WIN32)
  SET (BINARY_EXAMPLE_ENDING "tar.gz")
  SET (BINARY_INSTALL_ENDING "sh")
  SET (BINARY_PLATFORM "${BINARY_PLATFORM} ${CMAKE_SYSTEM_VERSION} ${CMAKE_SYSTEM_PROCESSOR}")
  SET (BINARY_PLATFORM "${BINARY_PLATFORM}, using ${CMAKE_C_COMPILER_ID} C ${CMAKE_C_COMPILER_VERSION}")
ENDIF (WIN32)
IF (HDF4_BUILD_FORTRAN)
  SET (BINARY_PLATFORM "${BINARY_PLATFORM} / ${CMAKE_Fortran_COMPILER_ID} Fortran")
ENDIF (HDF4_BUILD_FORTRAN)

CONFIGURE_FILE (
    ${HDF5_RESOURCES_DIR}/README.txt.cmake.in 
    ${HDF5_BINARY_DIR}/README.txt @ONLY
)

#-----------------------------------------------------------------------------
# Add Document File(s) to CMake Install
#-----------------------------------------------------------------------------
IF (NOT HDF5_EXTERNALLY_CONFIGURED)
  INSTALL (
      FILES
          ${HDF5_SOURCE_DIR}/COPYING
      DESTINATION ${HDF5_INSTALL_DATA_DIR}
      COMPONENT hdfdocuments
  )
  IF (EXISTS "${HDF5_SOURCE_DIR}/release_docs" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/release_docs")
    SET (release_files
        ${HDF5_SOURCE_DIR}/release_docs/USING_HDF5_CMake.txt
        ${HDF5_SOURCE_DIR}/release_docs/COPYING
        ${HDF5_SOURCE_DIR}/release_docs/RELEASE.txt
    )
    IF (WIN32 AND NOT CYGWIN)
      SET (release_files
          ${release_files}
          ${HDF5_SOURCE_DIR}/release_docs/USING_HDF5_VS.txt
      )
    ENDIF (WIN32 AND NOT CYGWIN)
    IF (HDF5_PACK_INSTALL_DOCS)
      SET (release_files
          ${release_files}
          ${HDF5_SOURCE_DIR}/release_docs/INSTALL_CMake.txt
          ${HDF5_SOURCE_DIR}/release_docs/HISTORY-1_8.txt
          ${HDF5_SOURCE_DIR}/release_docs/INSTALL
      )
      IF (WIN32)
        IF (NOT CYGWIN)
          SET (release_files
              ${release_files}
              ${HDF5_SOURCE_DIR}/release_docs/INSTALL_Windows.txt
          )
        ELSE (NOT CYGWIN)
          SET (release_files
              ${release_files}
              ${HDF5_SOURCE_DIR}/release_docs/INSTALL_Cygwin.txt
          )
        ENDIF (NOT CYGWIN)
      ENDIF (WIN32)
      IF (HDF5_ENABLE_PARALLEL)
        SET (release_files
            ${release_files}
            ${HDF5_SOURCE_DIR}/release_docs/INSTALL_parallel
        )
      ENDIF (HDF5_ENABLE_PARALLEL)
    ENDIF (HDF5_PACK_INSTALL_DOCS)
    INSTALL (
        FILES ${release_files}
        DESTINATION ${HDF5_INSTALL_DATA_DIR}
        COMPONENT hdfdocuments
    )
  ENDIF (EXISTS "${HDF5_SOURCE_DIR}/release_docs" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/release_docs")
ENDIF (NOT HDF5_EXTERNALLY_CONFIGURED)

#-----------------------------------------------------------------------------
# Set the cpack variables
#-----------------------------------------------------------------------------
IF (NOT HDF5_EXTERNALLY_CONFIGURED AND NOT HDF5_NO_PACKAGES)
  SET (CPACK_PACKAGE_VENDOR "HDF_Group")
  SET (CPACK_PACKAGE_NAME "${HDF5_PACKAGE_NAME}")
  SET (CPACK_PACKAGE_VERSION "${HDF5_PACKAGE_VERSION}")
  SET (CPACK_PACKAGE_VERSION_MAJOR "${HDF5_PACKAGE_VERSION_MAJOR}")
  SET (CPACK_PACKAGE_VERSION_MINOR "${HDF5_PACKAGE_VERSION_MINOR}")
  SET (CPACK_PACKAGE_VERSION_PATCH "")
  SET (CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_VENDOR}/${CPACK_PACKAGE_NAME}/${CPACK_PACKAGE_VERSION}")
  IF (EXISTS "${HDF5_SOURCE_DIR}/release_docs")
    SET (CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/release_docs/RELEASE.txt")
    SET (CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/release_docs/COPYING")
    SET (CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/release_docs/RELEASE.txt")
  ENDIF (EXISTS "${HDF5_SOURCE_DIR}/release_docs")
  SET (CPACK_PACKAGE_RELOCATABLE TRUE)

  SET (CPACK_GENERATOR "TGZ") 
  IF (WIN32)
    LIST (APPEND CPACK_GENERATOR "NSIS") 
    # Installers for 32- vs. 64-bit CMake:
    #  - Root install directory (displayed to end user at installer-run time)
    #  - "NSIS package/display name" (text used in the installer GUI)
    #  - Registry key used to store info about the installation
    IF (CMAKE_CL_64)
      SET (CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES64")
      SET (CPACK_NSIS_PACKAGE_NAME "${HDF5_PACKAGE_STRING} (Win64)")
      SET (CPACK_PACKAGE_INSTALL_REGISTRY_KEY "${HDF5_PACKAGE_STRING}-${LIB_TYPE} (Win64)")
    ELSE (CMAKE_CL_64)
      SET (CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES")
      SET (CPACK_NSIS_PACKAGE_NAME "${HDF5_PACKAGE_STRING}")
      SET (CPACK_PACKAGE_INSTALL_REGISTRY_KEY "${HDF5_PACKAGE_STRING}-${LIB_TYPE}")
    ENDIF (CMAKE_CL_64)
    SET (CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_VENDOR}\\\\${CPACK_PACKAGE_NAME}\\\\${CPACK_PACKAGE_VERSION}")
    SET (CPACK_MONOLITHIC_INSTALL ON)
    SET (CPACK_NSIS_CONTACT "${HDF5_PACKAGE_BUGREPORT}")
    SET (CPACK_NSIS_MODIFY_PATH ON)
  ELSEIF (APPLE)
    LIST (APPEND CPACK_GENERATOR "DragNDrop") 
    SET (CPACK_COMPONENTS_ALL_IN_ONE_PACKAGE ON)
    SET (CPACK_PACKAGE_DEFAULT_LOCATION "/opt/${CPACK_PACKAGE_NAME}")
    SET (CPACK_PACKAGING_INSTALL_PREFIX "/")
    SET (CPACK_PACKAGE_ICON "${HDF5_RESOURCES_DIR}/hdf.gif")
    SET (CPACK_SET_DESTDIR TRUE) # Required when packaging, and set CMAKE_INSTALL_PREFIX to "/".
    
    IF (HDF5_PACK_MACOSX_BUNDLE)
      LIST (APPEND CPACK_GENERATOR "Bundle")
      SET (CPACK_BUNDLE_NAME "${HDF5_PACKAGE_STRING}")
      SET (CPACK_BUNDLE_LOCATION "/")    # make sure CMAKE_INSTALL_PREFIX ends in /
      SET (CMAKE_INSTALL_PREFIX "/${CPACK_BUNDLE_NAME}.framework/Versions/${CPACK_PACKAGE_VERSION}/${CPACK_PACKAGE_NAME}/")
      SET (CPACK_BUNDLE_ICON "${HDF5_RESOURCES_DIR}/hdf.gif")
      SET (CPACK_BUNDLE_PLIST "${HDF5_BINARY_DIR}/CMakeFiles/Info.plist")
      SET (CPACK_APPLE_GUI_INFO_STRING "HDF5 (Hierarchical Data Format 5) Software Library and Utilities")
      SET (CPACK_APPLE_GUI_COPYRIGHT "Copyright © 2006-2013 by The HDF Group. All rights reserved.")
      SET (CPACK_SHORT_VERSION_STRING "${CPACK_PACKAGE_VERSION}")
      #-----------------------------------------------------------------------------
      # Configure the Info.plist file for the install bundle
      #-----------------------------------------------------------------------------
      CONFIGURE_FILE (
          ${HDF5_RESOURCES_DIR}/CPack.Info.plist.in
          ${HDF5_BINARY_DIR}/CMakeFiles/Info.plist @ONLY
      )
    ENDIF(HDF5_PACK_MACOSX_BUNDLE)
  ELSE (WIN32)
    LIST (APPEND CPACK_GENERATOR "STGZ") 
    SET (CPACK_PACKAGING_INSTALL_PREFIX "/${CPACK_PACKAGE_INSTALL_DIRECTORY}")
    SET (CPACK_COMPONENTS_ALL_IN_ONE_PACKAGE ON)

    SET (CPACK_DEBIAN_PACKAGE_SECTION "Libraries")
    SET (CPACK_DEBIAN_PACKAGE_MAINTAINER "${HDF5_PACKAGE_BUGREPORT}")

#    LIST (APPEND CPACK_GENERATOR "RPM") 
    SET (CPACK_RPM_PACKAGE_RELEASE "1")
    SET (CPACK_RPM_COMPONENT_INSTALL ON)
    SET (CPACK_RPM_PACKAGE_RELOCATABLE ON)
    SET (CPACK_RPM_PACKAGE_LICENSE "BSD-style")
    SET (CPACK_RPM_PACKAGE_GROUP "Development/Libraries")
    SET (CPACK_RPM_PACKAGE_URL "${HDF5_PACKAGE_URL}")
    SET (CPACK_RPM_PACKAGE_SUMMARY "HDF5 is a unique technology suite that makes possible the management of extremely large and complex data collections.")
    SET (CPACK_RPM_PACKAGE_DESCRIPTION 
        "The HDF5 technology suite includes:

    * A versatile data model that can represent very complex data objects and a wide variety of metadata.

    * A completely portable file format with no limit on the number or size of data objects in the collection.

    * A software library that runs on a range of computational platforms, from laptops to massively parallel systems, and implements a high-level API with C, C++, Fortran 90, and Java interfaces.

    * A rich set of integrated performance features that allow for access time and storage space optimizations.

    * Tools and applications for managing, manipulating, viewing, and analyzing the data in the collection.

The HDF5 data model, file format, API, library, and tools are open and distributed without charge.
"
    )
    
    #-----------------------------------------------------------------------------
    # Configure the spec file for the install RPM
    #-----------------------------------------------------------------------------
#    CONFIGURE_FILE ("${HDF5_RESOURCES_DIR}/hdf5.spec.in" "${CMAKE_CURRENT_BINARY_DIR}/${HDF5_PACKAGE_NAME}.spec" @ONLY IMMEDIATE)
#    SET (CPACK_RPM_USER_BINARY_SPECFILE "${CMAKE_CURRENT_BINARY_DIR}/${HDF5_PACKAGE_NAME}.spec")
  ENDIF (WIN32)
  
  # By default, do not warn when built on machines using only VS Express:
  IF (NOT DEFINED CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS)
    SET (CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS ON)
  ENDIF(NOT DEFINED CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS)
  INCLUDE(InstallRequiredSystemLibraries)

  SET (CPACK_INSTALL_CMAKE_PROJECTS "${HDF5_BINARY_DIR};HDF5;ALL;/")
  
  IF (HDF5_PACKAGE_EXTLIBS)
    IF (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "SVN" OR HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "TGZ")
      IF (ZLIB_FOUND AND ZLIB_USE_EXTERNAL)
        SET (CPACK_INSTALL_CMAKE_PROJECTS "${CPACK_INSTALL_CMAKE_PROJECTS};${ZLIB_INCLUDE_DIR_GEN};ZLIB;libraries;/")
        SET (CPACK_INSTALL_CMAKE_PROJECTS "${CPACK_INSTALL_CMAKE_PROJECTS};${ZLIB_INCLUDE_DIR_GEN};ZLIB;headers;/")
        SET (CPACK_INSTALL_CMAKE_PROJECTS "${CPACK_INSTALL_CMAKE_PROJECTS};${ZLIB_INCLUDE_DIR_GEN};ZLIB;configinstall;/")
      ENDIF (ZLIB_FOUND AND ZLIB_USE_EXTERNAL)
      IF (SZIP_FOUND AND SZIP_USE_EXTERNAL)
        SET (CPACK_INSTALL_CMAKE_PROJECTS "${CPACK_INSTALL_CMAKE_PROJECTS};${SZIP_INCLUDE_DIR_GEN};SZIP;libraries;/")
        SET (CPACK_INSTALL_CMAKE_PROJECTS "${CPACK_INSTALL_CMAKE_PROJECTS};${SZIP_INCLUDE_DIR_GEN};SZIP;headers;/")
        SET (CPACK_INSTALL_CMAKE_PROJECTS "${CPACK_INSTALL_CMAKE_PROJECTS};${SZIP_INCLUDE_DIR_GEN};SZIP;configinstall;/")
      ENDIF (SZIP_FOUND AND SZIP_USE_EXTERNAL)
    ENDIF (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "SVN" OR HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "TGZ")
  ENDIF (HDF5_PACKAGE_EXTLIBS)
  
  INCLUDE (CPack)

  #---------------------------------------------------------------------------
  # Now list the cpack commands
  #---------------------------------------------------------------------------
  CPACK_ADD_COMPONENT (hdfapplications 
      DISPLAY_NAME "HDF5 Applications" 
      DEPENDS libraries
      GROUP Applications
  )
  CPACK_ADD_COMPONENT (libraries 
      DISPLAY_NAME "HDF5 Libraries"
      GROUP Runtime
  )
  CPACK_ADD_COMPONENT (headers 
      DISPLAY_NAME "HDF5 Headers" 
      DEPENDS libraries
      GROUP Development
  )
  CPACK_ADD_COMPONENT (hdfdocuments 
      DISPLAY_NAME "HDF5 Documents"
      GROUP Documents
  )
  CPACK_ADD_COMPONENT (configinstall 
      DISPLAY_NAME "HDF5 CMake files" 
      DEPENDS libraries
      GROUP Development
  )
  
  IF (HDF5_BUILD_FORTRAN)
    CPACK_ADD_COMPONENT (fortlibraries 
        DISPLAY_NAME "HDF5 Fortran Libraries" 
        DEPENDS libraries
        GROUP Runtime
    )
    CPACK_ADD_COMPONENT (fortheaders 
        DISPLAY_NAME "HDF5 Fortran Headers" 
        DEPENDS fortlibraries
        GROUP Development
    )
  ENDIF (HDF5_BUILD_FORTRAN)
  
  IF (HDF5_BUILD_CPP_LIB)
    CPACK_ADD_COMPONENT (cpplibraries 
        DISPLAY_NAME "HDF5 C++ Libraries" 
        DEPENDS libraries
        GROUP Runtime
    )
    CPACK_ADD_COMPONENT (cppheaders 
        DISPLAY_NAME "HDF5 C++ Headers" 
        DEPENDS cpplibraries
        GROUP Development
    )
  ENDIF (HDF5_BUILD_CPP_LIB)
  
  IF (HDF5_BUILD_TOOLS)
    CPACK_ADD_COMPONENT (toolsapplications 
        DISPLAY_NAME "HDF5 Tools Applications" 
        DEPENDS toolslibraries
        GROUP Applications
    )
    CPACK_ADD_COMPONENT (toolslibraries 
        DISPLAY_NAME "HDF5 Tools Libraries" 
        DEPENDS libraries
        GROUP Runtime
    )
    CPACK_ADD_COMPONENT (toolsheaders 
        DISPLAY_NAME "HDF5 Tools Headers" 
        DEPENDS toolslibraries
        GROUP Development
    )
  ENDIF (HDF5_BUILD_TOOLS)
  
  IF (HDF5_BUILD_HL_LIB)
    CPACK_ADD_COMPONENT (hllibraries 
        DISPLAY_NAME "HDF5 HL Libraries" 
        DEPENDS libraries
        GROUP Runtime
    )
    CPACK_ADD_COMPONENT (hlheaders 
        DISPLAY_NAME "HDF5 HL Headers" 
        DEPENDS hllibraries
        GROUP Development
    )
    CPACK_ADD_COMPONENT (hltoolsapplications 
        DISPLAY_NAME "HDF5 HL Tools Applications" 
        DEPENDS hllibraries
        GROUP Applications
    )
    CPACK_ADD_COMPONENT (hlcpplibraries 
        DISPLAY_NAME "HDF5 HL C++ Libraries" 
        DEPENDS hllibraries
        GROUP Runtime
    )
    CPACK_ADD_COMPONENT (hlcppheaders 
        DISPLAY_NAME "HDF5 HL C++ Headers" 
        DEPENDS hlcpplibraries
        GROUP Development
    )
    CPACK_ADD_COMPONENT (hlfortlibraries 
        DISPLAY_NAME "HDF5 HL Fortran Libraries" 
        DEPENDS fortlibraries
        GROUP Runtime
    )
  ENDIF (HDF5_BUILD_HL_LIB)
  
ENDIF (NOT HDF5_EXTERNALLY_CONFIGURED AND NOT HDF5_NO_PACKAGES)
