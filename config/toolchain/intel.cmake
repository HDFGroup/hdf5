# Uncomment the following to use cross-compiling
#set(CMAKE_SYSTEM_NAME Linux)

set(CMAKE_COMPILER_VENDOR "intel")

set(CMAKE_C_COMPILER icx)
if(WIN32)
 set(CMAKE_CXX_COMPILER icx)
else()
  set(CMAKE_CXX_COMPILER icpx)
endif()
set(CMAKE_Fortran_COMPILER ifx)

# the following is used if cross-compiling
set(CMAKE_CROSSCOMPILING_EMULATOR "")
