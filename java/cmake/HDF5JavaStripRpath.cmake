#-----------------------------------------------------------------------------
# HDF5JavaStripRpath.cmake - Remove runtime search paths from a staged native library
#
# Usage: cmake -DFILE=<library> -DPLATFORM=<linux|macos> -P HDF5JavaStripRpath.cmake
#
# Build-tree libraries carry RPATH/RUNPATH (ELF) or LC_RPATH (Mach-O) entries that
# point into the build directory. Bundled libraries are loaded by
# Hdf5NativeLoader in dependency order and resolved against the already-loaded
# copies, so these entries are removed from the copies placed in the native JARs.
#-----------------------------------------------------------------------------

if (NOT FILE OR NOT EXISTS "${FILE}")
  message (FATAL_ERROR "HDF5JavaStripRpath.cmake: FILE '${FILE}' does not exist")
endif ()

if (PLATFORM STREQUAL "linux")
  file (RPATH_REMOVE FILE "${FILE}")
elseif (PLATFORM STREQUAL "macos")
  execute_process (
    COMMAND otool -l "${FILE}"
    OUTPUT_VARIABLE _load_commands
    COMMAND_ERROR_IS_FATAL ANY
  )
  string (REGEX MATCHALL "cmd LC_RPATH\n[^\n]*\n *path [^\n]+ \\(offset" _rpath_cmds "${_load_commands}")
  foreach (_cmd IN LISTS _rpath_cmds)
    string (REGEX REPLACE ".* path (.+) \\(offset$" "\\1" _rpath "${_cmd}")
    execute_process (
      COMMAND install_name_tool -delete_rpath "${_rpath}" "${FILE}"
      COMMAND_ERROR_IS_FATAL ANY
    )
  endforeach ()
  if (_rpath_cmds)
    # install_name_tool invalidates the signature; arm64 requires a valid one to load
    execute_process (
      COMMAND codesign --force --sign - "${FILE}"
      COMMAND_ERROR_IS_FATAL ANY
    )
  endif ()
endif ()
