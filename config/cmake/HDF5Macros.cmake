#-------------------------------------------------------------------------------
macro (H5_SET_LIB_OPTIONS libtarget libname libtype)
  set (LIB_OUT_NAME "${libname}")
  if (${libtype} MATCHES "SHARED")
    if (WIN32)
      set (LIBHDF_VERSION ${HDF5_PACKAGE_VERSION_MAJOR})
    else (WIN32)
      set (LIBHDF_VERSION ${HDF5_PACKAGE_VERSION})
    endif (WIN32)
    set_target_properties (${libtarget} PROPERTIES VERSION ${LIBHDF_VERSION})
    if (WIN32)
        set (${LIB_OUT_NAME} "${LIB_OUT_NAME}-${HDF5_PACKAGE_SOVERSION}")
    else (WIN32)
        set_target_properties (${libtarget} PROPERTIES SOVERSION ${HDF5_PACKAGE_SOVERSION})
    endif (WIN32)
  endif (${libtype} MATCHES "SHARED")
  HDF_SET_LIB_OPTIONS (${libtarget} ${LIB_OUT_NAME} ${libtype})

  #-- Apple Specific install_name for libraries
  if (APPLE)
    option (HDF5_BUILD_WITH_INSTALL_NAME "Build with library install_name set to the installation path" OFF)
    if (HDF5_BUILD_WITH_INSTALL_NAME)
      set_target_properties (${libtarget} PROPERTIES
          LINK_FLAGS "-current_version ${HDF5_PACKAGE_VERSION} -compatibility_version ${HDF5_PACKAGE_VERSION}"
          INSTALL_NAME_DIR "${CMAKE_INSTALL_PREFIX}/lib"
          BUILD_WITH_INSTALL_RPATH ${HDF5_BUILD_WITH_INSTALL_NAME}
      )
    endif (HDF5_BUILD_WITH_INSTALL_NAME)
    if (HDF5_BUILD_FRAMEWORKS)
      if (${libtype} MATCHES "SHARED")
        # adapt target to build frameworks instead of dylibs
        set_target_properties(${libtarget} PROPERTIES
            XCODE_ATTRIBUTE_INSTALL_PATH "@rpath"
            FRAMEWORK TRUE
            FRAMEWORK_VERSION ${HDF5_PACKAGE_VERSION_MAJOR}
            MACOSX_FRAMEWORK_IDENTIFIER org.hdfgroup.${libtarget}
            MACOSX_FRAMEWORK_SHORT_VERSION_STRING ${HDF5_PACKAGE_VERSION_MAJOR}
            MACOSX_FRAMEWORK_BUNDLE_VERSION ${HDF5_PACKAGE_VERSION_MAJOR})
      endif (${libtype} MATCHES "SHARED")
    endif (HDF5_BUILD_FRAMEWORKS)
  endif (APPLE)
endmacro (H5_SET_LIB_OPTIONS)

macro (H5_GEN_PERL_FILES)
  find_package (Perl)
  if (PERL_FOUND)
    add_custom_command (
        OUTPUT ${HDF5_BINARY_DIR}/H5Edefin.h
        PRE_BUILD
        COMMAND ${PERL_EXECUTABLE}
        ARGS ${HDF5_SOURCE_DIR}/bin/make_err ${HDF5_SOURCE_DIR}/src/H5err.txt
        DEPENDS ${HDF5_SOURCE_DIR}/src/H5err.txt
        COMMENT " Creating err header"
    )

    add_custom_command (
        OUTPUT ${HDF5_BINARY_DIR}/H5version.h
        PRE_BUILD
        COMMAND ${PERL_EXECUTABLE}
        ARGS ${HDF5_SOURCE_DIR}/bin/make_vers  ${HDF5_SOURCE_DIR}/src/H5vers.txt
        DEPENDS ${HDF5_SOURCE_DIR}/src/H5vers.txt
       COMMENT " Creating API version macro"
    )

    add_custom_command (
        OUTPUT ${HDF5_BINARY_DIR}/H5overflow.h
        PRE_BUILD
        COMMAND ${PERL_EXECUTABLE}
        ARGS ${HDF5_SOURCE_DIR}/bin/make_overflow  ${HDF5_SOURCE_DIR}/src/H5overflow.txt
        DEPENDS ${HDF5_SOURCE_DIR}/src/H5overflow.txt
        COMMENT " Creating Assignment overflow macro"
    )

    add_custom_target(run_perl_scripts ALL
        DEPENDS ${HDF5_BINARY_DIR}/H5Edefin.h ${HDF5_BINARY_DIR}/H5version.h ${HDF5_BINARY_DIR}/H5overflow.h
    )
  else (PERL_FOUND)
    message (STATUS "Cannot generate headers - perl not found")
  endif (PERL_FOUND)
endmacro (H5_GEN_PERL_FILES)

macro (H5_GEN_FLEX_FILES)
  find_package (BISON)
  if (BISON_FOUND)
    find_package (FLEX)
    if (FLEX_FOUND)
      BISON_TARGET (H5LT_PARSER H5LTparse.y ${CMAKE_CURRENT_BINARY_DIR}/H5LTparse.c COMPILE_FLAGS -d)
      if (WIN32)
        FLEX_TARGET (H5LT_SCANNER H5LTanalyze.l ${CMAKE_CURRENT_BINARY_DIR}/H5LTanalyze.c COMPILE_FLAGS --wincompat)
      else (WIN32)
        FLEX_TARGET (H5LT_SCANNER H5LTanalyze.l ${CMAKE_CURRENT_BINARY_DIR}/H5LTanalyze.c)
      endif (WIN32)
      ADD_FLEX_BISON_DEPENDENCY(H5LT_SCANNER H5LT_PARSER)

      if (BISON_H5LT_PARSER_DEFINED AND FLEX_H5LT_SCANNER_DEFINED)
        set_source_files_properties (${FLEX_H5LT_SCANNER_OUTPUTS}
            PROPERTIES OBJECT_DEPENDS ${BISON_H5LT_PARSER_OUTPUTS})
        set (HL_SRCS
            ${BISON_H5LT_PARSER_OUTPUTS}
            ${FLEX_H5LT_SCANNER_OUTPUTS}
        )
        include_directories(${CMAKE_CURRENT_BINARY_DIR})
      else (BISON_H5LT_PARSER_DEFINED AND FLEX_H5LT_SCANNER_DEFINED)
        message (STATUS "Error during generate of files")
      endif (BISON_H5LT_PARSER_DEFINED AND FLEX_H5LT_SCANNER_DEFINED)
    else (FLEX_FOUND)
      message (STATUS "Cannot generate files - flex not found")
    endif (FLEX_FOUND)
  else (BISON_FOUND)
    message (STATUS "Cannot generate files - bison not found")
  endif (BISON_FOUND)
endmacro (H5_GEN_FLEX_FILES)
