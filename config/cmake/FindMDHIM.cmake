# - Try to find MDHIM
# Once done this will define
#  MDHIM_FOUND - System has MDHIM
#  MDHIM_INCLUDE_DIRS - The MDHIM include directories
#  MDHIM_LIBRARIES - The libraries needed to use MDHIM

find_path(MDHIM_INCLUDE_DIR mdhim.h
  HINTS /usr/local/include /usr/include)

find_library(MDHIM_LIBRARY NAMES mdhim
  PATHS /usr/local/lib /usr/lib)

find_library(LEVELDB_LIBRARY NAMES leveldb
  PATHS /usr/local/lib /usr/lib)

set(MDHIM_INCLUDE_DIRS ${MDHIM_INCLUDE_DIR})
set(MDHIM_LIBRARIES ${MDHIM_LIBRARY} ${LEVELDB_LIBRARY})

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set MDHIM_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(MDHIM DEFAULT_MSG
                                  MDHIM_INCLUDE_DIR MDHIM_LIBRARY LEVELDB_LIBRARY)

mark_as_advanced(MDHIM_INCLUDE_DIR MDHIM_LIBRARY LEVELDB_LIBRARY)
