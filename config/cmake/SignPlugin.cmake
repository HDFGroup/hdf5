#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5. The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

#[=======================================================================[.rst:
SignPlugin
----------

Provides a CMake function to sign plugin libraries when HDF5_REQUIRE_SIGNED_PLUGINS is enabled.

.. command:: sign_plugin_target

  Signs a plugin target using the h5sign tool.

  .. code-block:: cmake

    sign_plugin_target(<target> <plugin_dir>)

  ``target``
    The CMake target to sign (must be a shared library plugin)

  ``plugin_dir``
    The directory where the plugin will be located after build

  This function adds a post-build command that:
  - Signs the plugin using the h5sign tool
  - Uses the test private key (${CMAKE_BINARY_DIR}/private.pem)
  - Only executes if HDF5_REQUIRE_SIGNED_PLUGINS is enabled

#]=======================================================================]

function(sign_plugin_target TARGET PLUGIN_DIR)
  if (HDF5_REQUIRE_SIGNED_PLUGINS)
    add_dependencies(${TARGET} h5sign)
    add_custom_command(
      TARGET ${TARGET}
      POST_BUILD
      COMMAND $<TARGET_FILE:h5sign>
      ARGS -p "${PLUGIN_DIR}/$<TARGET_FILE_NAME:${TARGET}>"
           -k "${CMAKE_BINARY_DIR}/private.pem"
      COMMENT "Signing test plugin ${TARGET} for signature verification"
    )
  endif()
endfunction()
