# - Try to find Berkeley DB
# Once done this will define
#  DB_FOUND - System has Berkeley DB
#  DB_INCLUDE_DIRS - The DB include directories
#  DB_LIBRARIES - The libraries needed to use DB

find_path(DB_INCLUDE_DIR db.h
  HINTS /usr/local/include /usr/include)

find_library(DB_LIBRARY NAMES db
  PATHS /usr/local/lib /usr/lib)

set(DB_INCLUDE_DIRS ${DB_INCLUDE_DIR})
set(DB_LIBRARIES ${DB_LIBRARY})

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set DB_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(DB DEFAULT_MSG
                                  DB_INCLUDE_DIR DB_LIBRARY)

mark_as_advanced(DB_INCLUDE_DIR DB_LIBRARY)
