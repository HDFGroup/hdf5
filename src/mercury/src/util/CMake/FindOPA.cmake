# - Try to find OPA
# Once done this will define
#  OPA_FOUND - System has OpenPA
#  OPA_INCLUDE_DIRS - The OPA include directories
#  OPA_LIBRARIES - The libraries needed to use OPA

find_package(PkgConfig)
pkg_check_modules(PC_OPA QUIET openpa)
# If openpa.pc cannot be found, try to look for mpich2-c.pc
if(NOT PC_OPA_INCLUDEDIRS)
  pkg_check_modules(PC_MPICH2_C QUIET mpich2-c)
  set(PC_OPA_INCLUDEDIR ${PC_MPICH2_C_INCLUDEDIR})
  set(PC_OPA_LIBDIR ${PC_MPICH2_C_LIBDIR})
endif()

find_path(OPA_INCLUDE_DIR opa_primitives.h
  HINTS ${PC_OPA_INCLUDEDIR} ${PC_OPA_INCLUDE_DIRS})

find_library(OPA_LIBRARY NAMES opa libopa
  HINTS ${PC_OPA_LIBDIR} ${PC_OPA_LIBRARY_DIRS})

set(OPA_LIBRARIES ${OPA_LIBRARY})
set(OPA_INCLUDE_DIRS ${OPA_INCLUDE_DIR})

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set OPA_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(OPA DEFAULT_MSG
                                  OPA_LIBRARY OPA_INCLUDE_DIR)

mark_as_advanced(OPA_INCLUDE_DIR OPA_LIBRARY)
