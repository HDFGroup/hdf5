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
# This file provides functionality for creating pkg-config files for HDF5.
#

# Function to inspect a CMake target or library name/path and extract any
# available information needed when creating an HDF5 pkg-config file that
# makes use of that library.
#
# NOTE: Currently, this function assumes that libraries don't have pkg-config
# support and so returns flags to be added to the "Libs" or "Libs.private"
# field (as determined by the caller) and "Cflags" field in the pkg-config
# file. Ideally, pkg_check_modules() should be used to check if a particular
# library has pkg-config support so that the library can be added to the
# "Requires" or "Requires.private" field instead, as needed. Some additional
# logic will be needed to determine the pkg-config module name from the CMake
# target name or library name.
#
# Input variables:
#
#   - library
#       Specifies the CMake target name or library name/path to inspect.
#
#   - _pkgconfig_libs_outvar
#       Specifies the name of the CMake variable which will be populated
#       with the flags for the library that should be added to the "Libs"
#       or "Libs.private" field (as determined by the caller) of a pkg-config
#       file. After processing the library, the variable pointed to by
#       _pkgconfig_libs_outvar will be a CMake list of flags.
#
#   - _pkgconfig_extra_cflags_outvar
#       Specifies the name of the CMake variable which will be populated
#       with the flags for the library that should be added to the
#       "Cflags" field of a pkg-config file. After processing the library,
#       the variable pointed to by _pkgconfig_extra_cflags_outvar will be
#       a CMake list of flags.
#
#   - _lib_skipped_outvar
#       Specifies the name of the CMake variable which will be set to TRUE
#       if a library was skipped for processing for some reason and FALSE
#       otherwise.
#
# Output variables:
#
#   None
#
function (extract_lib_pkgconfig_info
         library
         _pkgconfig_libs_outvar
         _pkgconfig_extra_cflags_outvar
         _lib_skipped_outvar)
  # Initialize outputs to known values
  unset (${_pkgconfig_libs_outvar} PARENT_SCOPE)
  unset (${_pkgconfig_extra_cflags_outvar} PARENT_SCOPE)
  set (${_lib_skipped_outvar} FALSE PARENT_SCOPE)

  unset (library_deps_list)
  unset (library_cflags_list)

  # Get current project build configuration type in case we need to map
  # to an imported target's config-specific properties. To be safe for
  # now, avoid processing libraries when using a multi-config generator
  # with more than one configuration. pkg-config files are currently
  # generated at configure time, but the build configuration type is
  # determined at build time. If mapping between the current build
  # configuration type and a list of imported library locations (based
  # on the current build configuration type) is needed later on, this
  # function will be unable to properly do so.
  get_cmake_property (is_multiconfig_gen GENERATOR_IS_MULTI_CONFIG)
  if (is_multiconfig_gen)
    list (LENGTH CMAKE_CONFIGURATION_TYPES num_configs)
    if (num_configs GREATER "1")
      message (AUTHOR_WARNING "Cannot generate pkg-config files correctly for multi-config generators")
      set (${_lib_skipped_outvar} TRUE PARENT_SCOPE)
      return ()
    elseif (num_configs EQUAL "1")
      set (project_build_config "${CMAKE_CONFIGURATION_TYPES}")
    else ()
      set (project_build_config Release)
    endif ()
  else ()
    set (project_build_config "${CMAKE_BUILD_TYPE}")
  endif ()
  if (project_build_config STREQUAL "Developer")
    set (project_build_config "Debug")
  endif ()

  # If this is a regular library, add it to the list of libraries for the
  # Libs.private field. Otherwise, do special processing for library targets.
  if (NOT TARGET ${library})
    # Avoid generator expressions related to flags for now. A more complete
    # solution will be needed in order to correctly evaluate these.
    string (GENEX_STRIP "${library}" library_genex_stripped)
    if (NOT "${library}" STREQUAL "${library_genex_stripped}")
      message (AUTHOR_WARNING "Not processing CMake generator expression for library ${library} for pkg-config files")
      set (${_lib_skipped_outvar} TRUE PARENT_SCOPE)
      return ()
    endif ()

    if (IS_ABSOLUTE "${library}" OR "${library}" MATCHES "^-")
      list (APPEND library_deps_list "${library}")
    else ()
      list (APPEND library_deps_list "-l${library}")
    endif ()

    set (${_pkgconfig_libs_outvar} "${library_deps_list}" PARENT_SCOPE)
    return ()
  endif ()

  # Retrieve INTERFACE properties from the target which may be needed in
  # "Cflags" for compiling
  get_target_property (lib_interface_include_dirs ${library} INTERFACE_INCLUDE_DIRECTORIES)
  if (lib_interface_include_dirs)
    foreach (include_dir ${lib_interface_include_dirs})
      # Avoid generator expressions related to flags for now. A more complete
      # solution will be needed in order to correctly evaluate these.
      string (GENEX_STRIP "${include_dir}" include_dir_genex_stripped)
      if (NOT "${include_dir}" STREQUAL "${include_dir_genex_stripped}")
        message (AUTHOR_WARNING "Not processing CMake generator expression ${include_dir} for library ${library} for pkg-config files")
        continue ()
      endif ()

      if ("${include_dir}" MATCHES "^-")
        list (APPEND library_cflags_list "${include_dir}")
      else ()
        list (APPEND library_cflags_list "-I${include_dir}")
      endif ()
    endforeach ()
  endif ()

  get_target_property (lib_interface_compile_opts ${library} INTERFACE_COMPILE_OPTIONS)
  if (lib_interface_compile_opts)
    foreach (compile_opt ${lib_interface_compile_opts})
      # Avoid generator expressions related to flags for now. A more complete
      # solution will be needed in order to correctly evaluate these.
      string (GENEX_STRIP "${compile_opt}" compile_opt_genex_stripped)
      if (NOT "${compile_opt}" STREQUAL "${compile_opt_genex_stripped}")
        message (AUTHOR_WARNING "Not processing CMake generator expression ${compile_opt} for library ${library} for pkg-config files")
        continue ()
      endif ()

      list (APPEND library_cflags_list "${compile_opt}")
    endforeach ()
  endif ()

  get_target_property (lib_interface_compile_defs ${library} INTERFACE_COMPILE_DEFINITIONS)
  if (lib_interface_compile_defs)
    foreach (compile_def ${lib_interface_compile_defs})
      # Avoid generator expressions related to flags for now. A more complete
      # solution will be needed in order to correctly evaluate these.
      string (GENEX_STRIP "${compile_def}" compile_def_genex_stripped)
      if (NOT "${compile_def}" STREQUAL "${compile_def_genex_stripped}")
        message (AUTHOR_WARNING "Not processing CMake generator expression ${compile_def} for library ${library} for pkg-config files")
        continue ()
      endif ()

      list (APPEND library_cflags_list "${compile_def}")
    endforeach ()
  endif ()

  # Determine appropriate flags for linking against library
  unset (lib_name)
  get_target_property (lib_is_imported ${library} IMPORTED)
  get_target_property (lib_type ${library} TYPE)
  if (lib_is_imported AND NOT lib_type STREQUAL "INTERFACE_LIBRARY")
    unset (lib_path)

    # Find a path for the library and process it into a library name for linking
    # against. First, check for a particular IMPORTED_LOCATION_<CONFIG> property
    # and use it if set. Some mapping between the library's imported configurations
    # and the project's current build configuration type may be needed. If that
    # property isn't set, check for the IMPORTED_LOCATION property and use it if
    # set. If neither are set, check for the LOCATION property and use it if set.
    get_target_property (lib_imported_configs ${library} IMPORTED_CONFIGURATIONS)
    if (lib_imported_configs)
      list (LENGTH lib_imported_configs num_configs)
      if (num_configs EQUAL "1")
        get_target_property (lib_path ${library} IMPORTED_LOCATION_${lib_imported_configs})
      elseif (num_configs GREATER "1")
        # Find a suitable configuration of the imported library that matches this
        # project's current build configuration type, if available.
        string (TOUPPER "${project_build_config}" project_build_config_upper)
        get_target_property (lib_path ${library} IMPORTED_LOCATION_${project_build_config_upper})
        if (NOT lib_path)
          get_target_property (lib_path ${library} IMPORTED_LOCATION_${project_build_config})
        endif ()
      endif () # Assume 0 configurations is a problem with the library's configuration; move on 
    endif ()
    if (NOT lib_path)
      get_target_property (lib_path ${library} IMPORTED_LOCATION)
    endif ()
    if (NOT lib_path)
      get_target_property (lib_path ${library} LOCATION)
    endif ()

    # Get base library name without extensions
    if (lib_path)
      # Avoid generator expressions in library locations for now. A more complete
      # solution will be needed in order to correctly evaluate these.
      string (GENEX_STRIP "${lib_path}" lib_path_genex_stripped)
      if (NOT "${lib_path}" STREQUAL "${lib_path_genex_stripped}")
        message (AUTHOR_WARNING "Not processing CMake generator expression ${lib_path} for library ${library} for pkg-config files")
        set (${_lib_skipped_outvar} TRUE PARENT_SCOPE)
        return ()
      endif ()

      cmake_path (GET lib_path STEM lib_name)
    endif ()
  endif ()

  # If a library name couldn't be determined from the LOCATION or IMPORTED_LOCATION(_...)
  # try OUTPUT_NAME. If the target isn't an imported target, but has an OUTPUT_NAME
  # set for it, it's likely an internal target for a filter plugin or similar which
  # will be built at library build time. This is a fragile assumption, but should
  # work for now.
  if (NOT lib_name AND NOT lib_type STREQUAL "INTERFACE_LIBRARY")
    get_target_property (lib_outname ${library} OUTPUT_NAME)
    if (lib_outname)
      # Avoid generator expressions in library names for now. A more complete
      # solution will be needed in order to correctly evaluate these.
      string (GENEX_STRIP "${lib_outname}" lib_outname_genex_stripped)
      if (NOT "${lib_outname}" STREQUAL "${lib_outname_genex_stripped}")
        message (AUTHOR_WARNING "Not processing CMake generator expression ${lib_outname} for library ${library} for pkg-config files")
        set (${_lib_skipped_outvar} TRUE PARENT_SCOPE)
        return ()
      endif ()

      cmake_path (GET lib_outname STEM lib_name)
    endif ()
  endif ()

  if (lib_name)
    string (REGEX REPLACE "^lib" "" lib_name "${lib_name}")
    list (APPEND library_deps_list "-l${lib_name}")
  else ()
    # Issue a warning for non-interface imported targets that we couldn't obtain a
    # valid name for, as the library can't be processed at this point. Currently,
    # it's assumed that non-imported targets without a valid name of some sort won't
    # be needed in a pkg-config file; they are silently skipped.
    if (lib_is_imported AND NOT lib_type STREQUAL "INTERFACE_LIBRARY")
      message (AUTHOR_WARNING "Couldn't obtain a valid name for library ${library} for pkg-config files")
    endif ()

    # Targets of type INTERFACE_LIBRARY are still processed for their compile
    # definitions and link dependencies.
    if (NOT lib_type STREQUAL "INTERFACE_LIBRARY")
      set (${_lib_skipped_outvar} TRUE PARENT_SCOPE)
      return ()
    endif ()
  endif ()

  # Process any transitive link dependencies
  get_target_property (lib_link_libraries ${library} INTERFACE_LINK_LIBRARIES)
  if (lib_link_libraries)
    unset (lib_dep_flags)
    unset (lib_cflags)
    unset (lib_skipped)

    foreach (lib_dep ${lib_link_libraries})
      extract_lib_pkgconfig_info (${lib_dep} lib_dep_flags lib_cflags lib_skipped)
      if (NOT lib_skipped)
        if (lib_dep_flags)
          list (APPEND library_deps_list "${lib_dep_flags}")
        endif ()
        if (lib_cflags)
          list (APPEND library_cflags_list "${lib_cflags}")
        endif ()
      endif ()
    endforeach ()
  endif ()

  # Propagate variables upward in scope
  set (${_pkgconfig_libs_outvar} "${library_deps_list}" PARENT_SCOPE)
  set (${_pkgconfig_extra_cflags_outvar} "${library_cflags_list}" PARENT_SCOPE)
endfunction ()
