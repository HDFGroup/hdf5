cmake_minimum_required (VERSION 3.26)
project (HDF5_TOOLS_SRC C)

#-- Add the h5diff and test executables
add_subdirectory (h5diff)

#-- Add the h5ls executable
add_subdirectory (h5ls)

#-- Misc Executables
add_subdirectory (misc)

#-- Add the h5import and test executables
add_subdirectory (h5import)

#-- h5repack executables
add_subdirectory (h5repack)

#-- Add the h5dump and test executables
add_subdirectory (h5jam)

#-- Add the h5copy and test executables
add_subdirectory (h5copy)

#-- Add the h5stat and test executables
add_subdirectory (h5stat)

#-- Add the h5dump and test executables
add_subdirectory (h5dump)

#-- Add the h5format_convert and test executables
add_subdirectory (h5format_convert)

#-- h5perf executables
add_subdirectory (h5perf)

#-- Add the h5sign executable (plugin signing tool)
#   Build h5sign when plugin signature verification is enabled
#   Note: This is separate from HDF5_ENABLE_PLUGIN_SUPPORT which is for external filter plugins.
#   h5sign is needed to sign test plugins and any dynamically loaded plugins.
if (HDF5_REQUIRE_SIGNED_PLUGINS)
  add_subdirectory (h5sign)
endif ()
