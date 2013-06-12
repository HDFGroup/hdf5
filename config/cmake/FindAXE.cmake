#  Try to find AXE library and headers.
#  This file sets the following variables:
#
#  AXE_INCLUDE_DIR, where to find axe.h, etc.
#  AXE_LIBRARIES, the libraries to link against
#  AXE_FOUND, If false, do not try to use AXE.
#
# Also defined, but not for general use are:
#  AXE_LIBRARY, the full path to the axe library.

FIND_PATH( AXE_INCLUDE_DIR axe.h
  /usr/local/include
  /usr/include
)

FIND_LIBRARY( AXE_LIBRARY NAMES axe
  /usr/lib
  /usr/local/lib
)

SET( AXE_FOUND "NO" )
IF(AXE_INCLUDE_DIR)
  IF(AXE_LIBRARY)

    SET( AXE_LIBRARIES ${AXE_LIBRARY})
    SET( AXE_FOUND "YES" )

  ELSE(AXE_LIBRARY)
    IF(AXE_FIND_REQUIRED)
      message(SEND_ERROR "Unable to find the requested AXE libraries.")
    ENDIF(AXE_FIND_REQUIRED)
  ENDIF(AXE_LIBRARY)
ENDIF(AXE_INCLUDE_DIR)

MARK_AS_ADVANCED(
  AXE_INCLUDE_DIR
  AXE_LIBRARY
)
