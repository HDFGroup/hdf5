########################################################
#  Include file for user options
########################################################

#-----------------------------------------------------------------------------
# Option to Build with User Defined Values
#-----------------------------------------------------------------------------
MACRO (MACRO_USER_DEFINED_LIBS)
  SET (USER_DEFINED_VALUE "FALSE")
ENDMACRO (MACRO_USER_DEFINED_LIBS)

#-------------------------------------------------------------------------------
OPTION (BUILD_USER_DEFINED_LIBS "Build With User Defined Values" OFF)
IF (BUILD_USER_DEFINED_LIBS)
  MACRO_USER_DEFINED_LIBS ()
ENDIF (BUILD_USER_DEFINED_LIBS)
 