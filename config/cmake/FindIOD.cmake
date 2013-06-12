#  Try to find IOD library and headers.
#  This file sets the following variables:
#
#  IOD_INCLUDE_DIR, where to find iod.h, etc.
#  IOD_LIBRARIES, the libraries to link against
#  IOD_FOUND, If false, do not try to use IOD.
#
# Also defined, but not for general use are:
#  IOD_LIBRARY, the full path to the iod library.

FIND_PATH( IOD_INCLUDE_DIR iod_api.h
  /usr/local/include
  /usr/include
)

FIND_LIBRARY( IOD_LIBRARY NAMES iod
  /usr/lib
  /usr/local/lib
)

SET( IOD_FOUND "NO" )
IF(IOD_INCLUDE_DIR)
  IF(IOD_LIBRARY)

    SET( IOD_LIBRARIES ${IOD_LIBRARY})
    SET( IOD_FOUND "YES" )

  ELSE(IOD_LIBRARY)
    IF(IOD_FIND_REQUIRED)
      message(SEND_ERROR "Unable to find the requested IOD libraries.")
    ENDIF(IOD_FIND_REQUIRED)
  ENDIF(IOD_LIBRARY)
ENDIF(IOD_INCLUDE_DIR)

MARK_AS_ADVANCED(
  IOD_INCLUDE_DIR
  IOD_LIBRARY
)
