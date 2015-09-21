cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_HL_SRC)

if (HDF5_GENERATE_HEADERS)
  find_package (BISON)
  if (BISON_FOUND)
    find_package (FLEX)
    if (FLEX_FOUND)
      BISON_TARGET (H5LT_PARSER H5LTparse.y ${CMAKE_CURRENT_BINARY_DIR}/H5LTparse.c COMPILE_FLAGS "-d -pH5LTyy")
      if (WIN32)
        FLEX_TARGET (H5LT_SCANNER H5LTanalyze.l ${CMAKE_CURRENT_BINARY_DIR}/H5LTanalyze.c COMPILE_FLAGS "--wincompat --nounistd -PH5LTyy")
      else (WIN32)
        FLEX_TARGET (H5LT_SCANNER H5LTanalyze.l ${CMAKE_CURRENT_BINARY_DIR}/H5LTanalyze.c COMPILE_FLAGS "--nounistd -PH5LTyy")
      endif (WIN32)
      ADD_FLEX_BISON_DEPENDENCY(H5LT_SCANNER H5LT_PARSER)

      if (BISON_H5LT_PARSER_DEFINED AND FLEX_H5LT_SCANNER_DEFINED)
        include_directories(${CMAKE_CURRENT_BINARY_DIR})
        set (HL_SRCS
            ${CMAKE_CURRENT_BINARY_DIR}/H5LTanalyze.c
            ${CMAKE_CURRENT_BINARY_DIR}/H5LTparse.c
        )
        set (HL_HEADERS
            ${CMAKE_CURRENT_BINARY_DIR}/H5LTparse.h
        )
        set_source_files_properties (${CMAKE_CURRENT_BINARY_DIR}/H5LTanalyze.c GENERATED)
        set_source_files_properties (${CMAKE_CURRENT_BINARY_DIR}/H5LTparse.c GENERATED)
        set_source_files_properties (${CMAKE_CURRENT_BINARY_DIR}/H5LTparse.h GENERATED)

        add_custom_command (
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/H5LTparse.timestamp
            COMMAND "${CMAKE_COMMAND}"
                -D "FILE_PARSE=H5LTparse.c"
                -P "${HDF_RESOURCES_DIR}/HDF5_Process_Flex_Files.cmake"
            COMMAND ${CMAKE_COMMAND} -E touch ${CMAKE_CURRENT_BINARY_DIR}/H5LTparse.timestamp
            DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/H5LTparse.c
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        )
        add_custom_command (
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/H5LTanalyze.timestamp
            COMMAND "${CMAKE_COMMAND}"
                -D "FILE_ANALYZE=H5LTanalyze.c"
                -P "${HDF_RESOURCES_DIR}/HDF5_Process_Flex_Files.cmake"
            COMMAND ${CMAKE_COMMAND} -E touch ${CMAKE_CURRENT_BINARY_DIR}/H5LTanalyze.timestamp
            DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/H5LTanalyze.c
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        )
        add_custom_target (process_gen_H5LT DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/H5LTparse.timestamp ${CMAKE_CURRENT_BINARY_DIR}/H5LTanalyze.timestamp)

#        add_custom_target (process_gen_H5LT
#            COMMAND "${CMAKE_COMMAND}"
#              -D "FILE_PARSE=${CMAKE_CURRENT_BINARY_DIR}/H5LTparse.c"
#              -D "FILE_ANALYSE=${CMAKE_CURRENT_BINARY_DIR}/H5LTanalyze.c"
#              -P "${HDF5_RESOURCE_DIR}/HDF5_Process_Flex_Files.cmake"
#              DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/H5LTparse.c ${CMAKE_CURRENT_BINARY_DIR}/H5LTanalyze.c"
#              COMMENT "Prepending pragma statements"
#        )
      else (BISON_H5LT_PARSER_DEFINED AND FLEX_H5LT_SCANNER_DEFINED)
        message (STATUS "Error during generate of files")
      endif (BISON_H5LT_PARSER_DEFINED AND FLEX_H5LT_SCANNER_DEFINED)
    else (FLEX_FOUND)
      message (STATUS "Cannot generate files - flex not found")
    endif (FLEX_FOUND)
  else (BISON_FOUND)
    message (STATUS "Cannot generate files - bison not found")
  endif (BISON_FOUND)
else (HDF5_GENERATE_HEADERS)
  set (HL_SRCS
      ${HDF5_HL_SRC_SOURCE_DIR}/H5LTanalyze.c
      ${HDF5_HL_SRC_SOURCE_DIR}/H5LTparse.c
  )
  set (HL_HEADERS
      ${HDF5_HL_SRC_SOURCE_DIR}/H5LTparse.h
  )
endif (HDF5_GENERATE_HEADERS)

#-----------------------------------------------------------------------------
# List Source files
#-----------------------------------------------------------------------------
set (HL_SRCS
    ${HL_SRCS}
    ${HDF5_HL_SRC_SOURCE_DIR}/H5DO.c
    ${HDF5_HL_SRC_SOURCE_DIR}/H5DS.c
    ${HDF5_HL_SRC_SOURCE_DIR}/H5IM.c
    ${HDF5_HL_SRC_SOURCE_DIR}/H5LT.c
    #${HDF5_HL_SRC_SOURCE_DIR}/H5LTanalyze.c
    #${HDF5_HL_SRC_SOURCE_DIR}/H5LTparse.c
    ${HDF5_HL_SRC_SOURCE_DIR}/H5PT.c
    ${HDF5_HL_SRC_SOURCE_DIR}/H5TB.c
)

set (HL_HEADERS
    ${HL_HEADERS}
    ${HDF5_HL_SRC_SOURCE_DIR}/H5DOpublic.h
    ${HDF5_HL_SRC_SOURCE_DIR}/H5DSpublic.h
    ${HDF5_HL_SRC_SOURCE_DIR}/H5IMpublic.h
    #${HDF5_HL_SRC_SOURCE_DIR}/H5LTparse.h
    ${HDF5_HL_SRC_SOURCE_DIR}/H5LTpublic.h
    ${HDF5_HL_SRC_SOURCE_DIR}/H5PTpublic.h
    ${HDF5_HL_SRC_SOURCE_DIR}/H5TBpublic.h
    ${HDF5_HL_SRC_SOURCE_DIR}/hdf5_hl.h
)

add_library (${HDF5_HL_LIB_TARGET} STATIC ${HL_SRCS} ${HL_HEADERS})
TARGET_C_PROPERTIES (${HDF5_HL_LIB_TARGET} STATIC " " " ")
target_link_libraries (${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})
H5_SET_LIB_OPTIONS (${HDF5_HL_LIB_TARGET} ${HDF5_HL_LIB_NAME} STATIC)
set_target_properties (${HDF5_HL_LIB_TARGET} PROPERTIES
    FOLDER libraries/hl
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_LIB_TARGET}")
set (install_targets ${HDF5_HL_LIB_TARGET})
if (HDF5_GENERATE_HEADERS)
  add_dependencies(${HDF5_HL_LIB_TARGET} process_gen_H5LT)
else (HDF5_GENERATE_HEADERS)
  add_dependencies(${HDF5_HL_LIB_TARGET} "${HDF5_HL_SRC_SOURCE_DIR}/H5LTanalyze.c;${HDF5_HL_SRC_SOURCE_DIR}/H5LTparse.c")
endif (HDF5_GENERATE_HEADERS)

if (BUILD_SHARED_LIBS)
  add_library (${HDF5_HL_LIBSH_TARGET} SHARED ${HL_SRCS} ${HL_HEADERS})
  TARGET_C_PROPERTIES (${HDF5_HL_LIBSH_TARGET} SHARED " " " ")
  target_link_libraries (${HDF5_HL_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  H5_SET_LIB_OPTIONS (${HDF5_HL_LIBSH_TARGET} ${HDF5_HL_LIB_NAME} SHARED)
  set_target_properties (${HDF5_HL_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/hl
      COMPILE_DEFINITIONS "H5_BUILT_AS_DYNAMIC_LIB"
      INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
      INTERFACE_COMPILE_DEFINITIONS H5_BUILT_AS_DYNAMIC_LIB=1
  )
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_LIBSH_TARGET}")
  set (install_targets ${install_targets} ${HDF5_HL_LIBSH_TARGET})
  if (HDF5_GENERATE_HEADERS)
    add_dependencies(${HDF5_HL_LIBSH_TARGET} process_gen_H5LT)
  else (HDF5_GENERATE_HEADERS)
    add_dependencies(${HDF5_HL_LIBSH_TARGET} "${HDF5_HL_SRC_SOURCE_DIR}/H5LTanalyze.c;${HDF5_HL_SRC_SOURCE_DIR}/H5LTparse.c")
  endif (HDF5_GENERATE_HEADERS)
endif (BUILD_SHARED_LIBS)

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
install (
    FILES
        ${HL_HEADERS}
    DESTINATION
        ${HDF5_INSTALL_INCLUDE_DIR}
    COMPONENT
        hlheaders
)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
if (HDF5_EXPORTED_TARGETS)
  if (BUILD_SHARED_LIBS)
    INSTALL_TARGET_PDB (${HDF5_HL_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} hllibraries)
  endif (BUILD_SHARED_LIBS)

  install (
      TARGETS
          ${install_targets}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT hllibraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT hllibraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT hllibraries
      FRAMEWORK DESTINATION ${HDF5_INSTALL_FWRK_DIR} COMPONENT hllibraries
  )
endif (HDF5_EXPORTED_TARGETS)
