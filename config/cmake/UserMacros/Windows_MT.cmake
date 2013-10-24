########################################################
#  Include file for user options
########################################################

# To use this option, copy both the macro and option code
# into the root UserMacros.cmake file. 
# OR add an include to the root UserMacros.cmake file:
# INCLUDE(path_to_file/WINDOWS_MT.cmake)

#-----------------------------------------------------------------------------
# Option to Build with Static CRT libraries on Windows
#-------------------------------------------------------------------------------
MACRO (TARGET_STATIC_CRT_FLAGS)
  IF (MSVC AND NOT BUILD_SHARED_LIBS)
    FOREACH (flag_var
        CMAKE_C_FLAGS CMAKE_C_FLAGS_DEBUG CMAKE_C_FLAGS_RELEASE
        CMAKE_C_FLAGS_MINSIZEREL CMAKE_C_FLAGS_RELWITHDEBINFO
        CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
        CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO)
      IF (${flag_var} MATCHES "/MD")
        STRING (REGEX REPLACE "/MD" "/MT" ${flag_var} "${${flag_var}}")
      ENDIF (${flag_var} MATCHES "/MD")
    ENDFOREACH (flag_var)
    FOREACH (flag_var
        CMAKE_Fortran_FLAGS CMAKE_Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_RELEASE
        CMAKE_Fortran_FLAGS_MINSIZEREL CMAKE_Fortran_FLAGS_RELWITHDEBINFO)
      IF (${flag_var} MATCHES "/libs:dll")
        STRING (REGEX REPLACE "/libs:dll" "/libs:static" ${flag_var} "${${flag_var}}")
      ENDIF (${flag_var} MATCHES "/libs:dll")
    ENDFOREACH (flag_var)
    SET (WIN_COMPILE_FLAGS "")
    SET (WIN_LINK_FLAGS "/NODEFAULTLIB:MSVCRT")
  ENDIF (MSVC AND NOT BUILD_SHARED_LIBS)
ENDMACRO (TARGET_STATIC_CRT_FLAGS)

#-----------------------------------------------------------------------------
OPTION (BUILD_STATIC_CRT_LIBS "Build With Static CRT Libraries" OFF)
IF (BUILD_STATIC_CRT_LIBS)
  TARGET_STATIC_CRT_FLAGS ()
ENDIF (BUILD_STATIC_CRT_LIBS)
 