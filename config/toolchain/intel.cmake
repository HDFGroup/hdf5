# Uncomment the following to use cross-compiling
#set(CMAKE_SYSTEM_NAME Linux)

set(CMAKE_COMPILER_VENDOR "intel")

set(CMAKE_C_COMPILER icc)
set(CMAKE_CXX_COMPILER icpc)
set(CMAKE_Fortran_COMPILER ifort)

# the following is used if cross-compiling
set(CMAKE_CROSSCOMPILING_EMULATOR "")
