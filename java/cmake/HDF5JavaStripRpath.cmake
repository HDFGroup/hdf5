#-----------------------------------------------------------------------------
# HDF5JavaStripRpath.cmake - Adjust runtime search paths of a staged native library
#
# Usage: cmake -DFILE=<library> -DPLATFORM=<linux|macos> -P HDF5JavaStripRpath.cmake
#
# Build-tree libraries carry RPATH/RUNPATH (ELF) or LC_RPATH (Mach-O) entries that
# point into the build directory; these are removed from the copies placed in the
# native JARs. Hdf5NativeLoader extracts every bundled library into one directory
# under its unversioned name and loads them in dependency order.
#   linux: a dependency (DT_NEEDED, e.g. libz.so.1) resolves to the already-loaded
#          bundled library with that SONAME.
#   macos: @rpath/ dependencies (e.g. @rpath/libz.1.dylib) are pointed at
#          @loader_path/<unversioned name> (e.g. @loader_path/libz.dylib), the
#          extracted file next to the loading library.
#-----------------------------------------------------------------------------

if (NOT FILE OR NOT EXISTS "${FILE}")
  message (FATAL_ERROR "HDF5JavaStripRpath.cmake: FILE '${FILE}' does not exist")
endif ()

if (PLATFORM STREQUAL "linux")
  file (RPATH_REMOVE FILE "${FILE}")
elseif (PLATFORM STREQUAL "macos")
  set (_changed FALSE)

  # Delete LC_RPATH entries one at a time until none is left, which also handles duplicates
  foreach (_attempt RANGE 64)
    execute_process (COMMAND otool -l "${FILE}" OUTPUT_VARIABLE _load_commands COMMAND_ERROR_IS_FATAL ANY)
    if (NOT _load_commands MATCHES "cmd LC_RPATH\n[^\n]*\n *path ([^\n]+) \\(offset")
      break ()
    endif ()
    execute_process (COMMAND install_name_tool -delete_rpath "${CMAKE_MATCH_1}" "${FILE}" COMMAND_ERROR_IS_FATAL ANY)
    set (_changed TRUE)
  endforeach ()
  if (_load_commands MATCHES "cmd LC_RPATH\n")
    message (FATAL_ERROR "HDF5JavaStripRpath.cmake: could not remove LC_RPATH entries from ${FILE}")
  endif ()

  string (REGEX MATCHALL "cmd LC_(LOAD|LOAD_WEAK|REEXPORT)_DYLIB\n[^\n]*\n *name @rpath/[^\n]+ \\(offset"
          _rpath_deps "${_load_commands}")
  foreach (_dep IN LISTS _rpath_deps)
    string (REGEX REPLACE ".* name (@rpath/[^\n]+) \\(offset$" "\\1" _old "${_dep}")
    get_filename_component (_name "${_old}" NAME)
    string (REGEX REPLACE "\\.[0-9][0-9.]*\\.dylib$" ".dylib" _name "${_name}")
    execute_process (
      COMMAND install_name_tool -change "${_old}" "@loader_path/${_name}" "${FILE}"
      COMMAND_ERROR_IS_FATAL ANY
    )
    set (_changed TRUE)
  endforeach ()

  if (_changed)
    # install_name_tool invalidates the signature; arm64 requires a valid one to load
    execute_process (COMMAND codesign --force --sign - "${FILE}" COMMAND_ERROR_IS_FATAL ANY)
  endif ()
endif ()
