#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
# Distributed under the OSI-approved BSD 3-Clause License.  See https://cmake.org/licensing for details.

#.rst:
# UseJavaSymlinks
# ---------------
#
#
#
#
#
# Helper script for UseJava.cmake

if (UNIX AND _JAVA_TARGET_OUTPUT_LINK)
    if (_JAVA_TARGET_OUTPUT_NAME)
        find_program(LN_EXECUTABLE
            NAMES
                ln
        )

        execute_process(
            COMMAND ${LN_EXECUTABLE} -sf "${_JAVA_TARGET_OUTPUT_NAME}" "${_JAVA_TARGET_OUTPUT_LINK}"
            WORKING_DIRECTORY ${_JAVA_TARGET_DIR}
        )
    else ()
        message(SEND_ERROR "FATAL: Can't find _JAVA_TARGET_OUTPUT_NAME")
    endif ()
endif ()
