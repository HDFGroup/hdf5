# - Try to find FastBit
# Once done this will define
#  FASTBIT_FOUND - System has FastBit
#  FASTBIT_INCLUDE_DIRS - The FastBit include directories
#  FASTBIT_LIBRARIES - The libraries needed to use FastBit

find_path(FASTBIT_INCLUDE_DIR iapi.h
  HINTS /usr/local/include /usr/include)

find_library(FASTBIT_LIBRARY NAMES fastbit
  PATHS /usr/local/lib /usr/lib)

set(FASTBIT_INCLUDE_DIRS ${FASTBIT_INCLUDE_DIR})
set(FASTBIT_LIBRARIES ${FASTBIT_LIBRARY})

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set FASTBIT_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(FASTBIT DEFAULT_MSG
                                  FASTBIT_INCLUDE_DIR FASTBIT_LIBRARY)

mark_as_advanced(FASTBIT_INCLUDE_DIR FASTBIT_LIBRARY)
