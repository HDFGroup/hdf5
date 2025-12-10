#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

#
# Read and parse VERSION.txt file
#
function(HDF5_READ_VERSION)
    # Read VERSION.txt line by line
    file(STRINGS "${HDF5_SOURCE_DIR}/VERSION.txt" VERSION_LINES)

    # Parse each line to extract version components
    foreach(LINE IN LISTS VERSION_LINES)
        if(LINE MATCHES "^MAJOR=(.+)$")
            set(MAJOR_VAL "${CMAKE_MATCH_1}")
        elseif(LINE MATCHES "^MINOR=(.+)$")
            set(MINOR_VAL "${CMAKE_MATCH_1}")
        elseif(LINE MATCHES "^RELEASE=(.+)$")
            set(RELEASE_VAL "${CMAKE_MATCH_1}")
        elseif(LINE MATCHES "^SUBRELEASE=(.*)$")
            set(SUBRELEASE_VAL "${CMAKE_MATCH_1}")
        endif()
    endforeach()

    # Validate that we got all required values
    if(NOT DEFINED MAJOR_VAL OR NOT DEFINED MINOR_VAL OR NOT DEFINED RELEASE_VAL)
        message(FATAL_ERROR "VERSION.txt is missing required fields (MAJOR, MINOR, or RELEASE)")
    endif()

    # Default SUBRELEASE to empty string if not defined
    if(NOT DEFINED SUBRELEASE_VAL)
        set(SUBRELEASE_VAL "")
    endif()

    # Set parent scope variables for version components
    set(HDF5_VERS_MAJOR "${MAJOR_VAL}" PARENT_SCOPE)
    set(HDF5_VERS_MINOR "${MINOR_VAL}" PARENT_SCOPE)
    set(HDF5_VERS_RELEASE "${RELEASE_VAL}" PARENT_SCOPE)
    set(HDF5_VERS_SUBRELEASE "${SUBRELEASE_VAL}" PARENT_SCOPE)

    # Generate derived strings
    if("${SUBRELEASE_VAL}" STREQUAL "")
        set(VERS_STR "${MAJOR_VAL}.${MINOR_VAL}.${RELEASE_VAL}")
    else()
        set(VERS_STR "${MAJOR_VAL}.${MINOR_VAL}.${RELEASE_VAL}-${SUBRELEASE_VAL}")
    endif()

    # Set parent scope variables for derived strings
    set(HDF5_VERS_STR "${VERS_STR}" PARENT_SCOPE)
    set(HDF5_VERS_INFO "HDF5 library version: ${VERS_STR}" PARENT_SCOPE)
    set(HDF5_VERSION_PLAIN "${MAJOR_VAL}.${MINOR_VAL}.${RELEASE_VAL}" PARENT_SCOPE)
endfunction()
