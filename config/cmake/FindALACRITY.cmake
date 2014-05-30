# - Try to find ALACRITY
# Once done this will define
#  ALACRITY_FOUND - System has ALACRITY
#  ALACRITY_INCLUDE_DIRS - The ALACRITY include directories
#  ALACRITY_LIBRARIES - The libraries needed to use ALACRITY

find_path(ALACRITY_INCLUDE_DIR alacrity.h
  HINTS /usr/local/include /usr/include)

find_library(ALACRITY_LIBRARY NAMES alacrity
  PATHS /usr/local/lib /usr/lib)

set(ALACRITY_INCLUDE_DIRS ${ALACRITY_INCLUDE_DIR})
set(ALACRITY_LIBRARIES ${ALACRITY_LIBRARY})

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set ALACRITY_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(ALACRITY DEFAULT_MSG
                                  ALACRITY_INCLUDE_DIR ALACRITY_LIBRARY)

mark_as_advanced(ALACRITY_INCLUDE_DIR ALACRITY_LIBRARY)
