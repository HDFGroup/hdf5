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

# -----------------------------------------------------------------------------
# HDF5 public (stable) CMake target names
# -----------------------------------------------------------------------------
#
# This module defines a parallel set of stable, linkage-agnostic names:
#
#   hdf5::hdf5              the C library
#   hdf5::hdf5_hl           the high-level C library
#   hdf5::hdf5_cpp          the C++ bindings
#   hdf5::hdf5_hl_cpp       the high-level C++ bindings
#   hdf5::hdf5_fortran      the Fortran bindings
#   hdf5::hdf5_hl_fortran   the high-level Fortran bindings
#   hdf5::<tool>            each installed tool executable, e.g. hdf5::h5diff
#   HDF5::HDF5              aggregate of the available library targets
#
# Each name is an ALIAS onto the concrete target for the selected linkage, so
# the public name and the legacy (pre-2.3.0) name refer to the same target.
#
# These names match the ones in CMake's FindHDF5 module.
#
# The module defines identical names across the HDF5 build tree,
# add_subdirectory() embeddings, and find_package() installations.
#
# This module is purely additive with respect to the pre-2.3.0 targets. It
# defines new names and neither removes nor alters the existing hdf5-static /
# hdf5-shared targets or the HDF5_<lang>_<LINKAGE>_LIBRARY variables.
#
# The default linkage is shared if shared libraries are available, otherwise
# static.
# -----------------------------------------------------------------------------

# The public library names this module can define. For each one, the caller
# sets H5PUB_CONCRETE_<name>_static and H5PUB_CONCRETE_<name>_shared to the
# corresponding concrete target names before calling
# h5_define_public_targets(), leaving unset whatever was not built.
#
# The H5PUB_ prefix keeps these out of the consuming project's namespace:
# hdf5-config.cmake runs in the consumer's variable scope and unsets them once
# the targets are defined.
set (HDF5_PUBLIC_LIBRARY_NAMES
    hdf5
    hdf5_hl
    hdf5_cpp
    hdf5_hl_cpp
    hdf5_fortran
    hdf5_hl_fortran
)

#-----------------------------------------------------------------------------
# h5_default_public_linkage (<shared_available> <static_available> <out_var>)
#
# Sets <out_var> in the caller's scope to the linkage to use when the consumer
# expresses no preference: prefer shared, fall back to static, empty string if
# neither was built.
#-----------------------------------------------------------------------------
function (h5_default_public_linkage _shared_available _static_available _out_var)
  if (_shared_available)
    set (${_out_var} "shared" PARENT_SCOPE)
  elseif (_static_available)
    set (${_out_var} "static" PARENT_SCOPE)
  else ()
    set (${_out_var} "" PARENT_SCOPE)
  endif ()
endfunction ()

#-----------------------------------------------------------------------------
# h5_define_public_tools ([PREFIX <prefix>] TOOLS <target> ...)
#
# Defines hdf5::<tool> for each named tool executable.
#
# Tool targets are already linkage-agnostic, so this is independent of the
# library linkage selection.
#
# PREFIX is prepended to the concrete target names before lookup: empty in
# HDF5's build tree, HDF_PACKAGE_NAMESPACE in an installed package.
#-----------------------------------------------------------------------------
function (h5_define_public_tools)
  cmake_parse_arguments (H5PUB "" "PREFIX" "TOOLS" ${ARGN})

  foreach (_tool IN LISTS H5PUB_TOOLS)
    set (_concrete "${H5PUB_PREFIX}${_tool}")
    if (NOT TARGET "${_concrete}")
      continue ()
    endif ()

    # Same rule as the libraries: an existing name must refer to our executable,
    # or the consumer runs someone else's under the hdf5:: namespace.
    if (TARGET "hdf5::${_tool}")
      get_target_property (_aliased "hdf5::${_tool}" ALIASED_TARGET)
      if (NOT _aliased STREQUAL _concrete)
        message (FATAL_ERROR
            "hdf5::${_tool} already exists and refers to ${_aliased}, not "
            "${_concrete}. Has another project has claimed an hdf5:: name?")
      endif ()
    else ()
      add_executable ("hdf5::${_tool}" ALIAS "${_concrete}")
    endif ()
  endforeach ()
endfunction ()

#-----------------------------------------------------------------------------
# h5_define_public_targets (LINKAGE <shared|static> [PREFIX <prefix>])
#
# Defines the public library names listed above, plus the HDF5::HDF5
# aggregate, as aliases onto the concrete targets for LINKAGE.
#
# PREFIX has the same meaning as in h5_define_public_tools() above.
#
# Components that were not built are skipped silently. Every name is guarded
# by an existence check, so repeated calls are harmless. A name already
# claimed by another project, referring to a different target, is a fatal
# error -- see the loop below.
#
# The caller must have set H5PUB_CONCRETE_<name>_<linkage> for each public
# library name it wants defined.
#-----------------------------------------------------------------------------
function (h5_define_public_targets)
  cmake_parse_arguments (H5PUB "" "LINKAGE;PREFIX" "" ${ARGN})

  if (NOT H5PUB_LINKAGE)
    message (FATAL_ERROR "h5_define_public_targets: LINKAGE is required")
  endif ()
  if (NOT H5PUB_LINKAGE STREQUAL "shared" AND NOT H5PUB_LINKAGE STREQUAL "static")
    message (FATAL_ERROR
        "h5_define_public_targets: LINKAGE must be \"shared\" or \"static\", got \"${H5PUB_LINKAGE}\"")
  endif ()

  set (_public_targets_defined)

  foreach (_public_name IN LISTS HDF5_PUBLIC_LIBRARY_NAMES)
    set (_concrete "${H5PUB_PREFIX}${H5PUB_CONCRETE_${_public_name}_${H5PUB_LINKAGE}}")

    # Components that were not built have no public name. A same-named target
    # from somewhere else is not ours to check, and must not reach the
    # aggregate either.
    if (NOT TARGET "${_concrete}")
      continue ()
    endif ()

    # A name that already exists is left alone, but must refer to the target we
    # would have aliased.
    if (TARGET "hdf5::${_public_name}")
      get_target_property (_aliased "hdf5::${_public_name}" ALIASED_TARGET)
      if (NOT _aliased STREQUAL _concrete)
        message (FATAL_ERROR
            "hdf5::${_public_name} already exists and refers to ${_aliased}, not "
            "${_concrete} -- another project has claimed the hdf5:: names.")
      endif ()
    else ()
      add_library ("hdf5::${_public_name}" ALIAS "${_concrete}")
    endif ()

    list (APPEND _public_targets_defined "hdf5::${_public_name}")
  endforeach ()

  # HDF5::HDF5 aggregates the available library targets, matching FindHDF5's
  # convenience target. It stands for more than one target, so it is an
  # INTERFACE library rather than an alias, and $<TARGET_FILE:> does not apply
  # to it.
  #
  # A name containing "::" must be IMPORTED or an ALIAS, and IMPORTED targets
  # are scoped to the directory that creates them. GLOBAL is therefore required
  # for an add_subdirectory() embedding.
  if (_public_targets_defined AND NOT TARGET HDF5::HDF5)
    add_library (HDF5::HDF5 INTERFACE IMPORTED GLOBAL)
    set_target_properties (HDF5::HDF5 PROPERTIES
        INTERFACE_LINK_LIBRARIES "${_public_targets_defined}"
    )
  endif ()
endfunction ()
