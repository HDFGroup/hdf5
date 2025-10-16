set (TOOLCHAIN_PREFIX x86_64-w64-mingw32)
set (CMAKE_SYSTEM_NAME Windows)
set (CMAKE_SYSTEM_PROCESSOR x86_64)

# specify the cross compiler
if (NOT DEFINED ENV{CC})
  set (CMAKE_C_COMPILER /usr/bin/${TOOLCHAIN_PREFIX}-gcc)
endif ()
if (NOT DEFINED ENV{CXX})
set (CMAKE_CXX_COMPILER /usr/bin/${TOOLCHAIN_PREFIX}-g++)
endif ()
if (NOT DEFINED ENV{FC})
set (CMAKE_Fortran_COMPILER /usr/bin/${TOOLCHAIN_PREFIX}-gfortran)
# set the resource compiler (RHBZ #652435)
endif ()
if (NOT DEFINED ENV{RC})
set (CMAKE_RC_COMPILER /usr/bin/${TOOLCHAIN_PREFIX}-windres)
endif ()

set (CMAKE_FIND_ROOT_PATH /usr/${TOOLCHAIN_PREFIX}/sys-root/mingw)

# search for programs in the build host directories
set (CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
# for libraries, headers and packages in the target directories
set (CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set (CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set (CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)
set (CMAKE_CROSSCOMPILING_EMULATOR wine64)
set (CROSSCOMPILING_PATH "WINEPATH=/usr/${TOOLCHAIN_PREFIX}/sys-root/mingw/bin/")

set (CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS On CACHE BOOL "Export windows symbols")

set (CMAKE_AR:FILEPATH /usr/bin/${TOOLCHAIN_PREFIX}-ar)
set (CMAKE_RANLIB:FILEPATH /usr/bin/${TOOLCHAIN_PREFIX}-ranlib)
