# Uncomment the following to use cross-compiling
#set(CMAKE_SYSTEM_NAME Linux)

set(CMAKE_COMPILER_VENDOR "clang")

set(CMAKE_C_COMPILER clang)
set(CMAKE_CXX_COMPILER clang++)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_program(
    CLANG_TIDY_EXE
    NAMES "clang-tidy"
    DOC "Path to clang-tidy executable"
)

set(CMAKE_C_CLANG_TIDY "${CLANG_TIDY_EXE}" -checks=*,clang-analyzer-*,-clang-analyzer-cplusplus*,-readability-*,-google*)
set(CMAKE_CXX_CLANG_TIDY "${CLANG_TIDY_EXE}" -checks=*,clang-analyzer-*,-clang-analyzer-cplusplus*,-readability-*,-google*)

#find_program(
#    CLANG_FORMAT_EXE
#    NAMES "clang-format"
#    DOC "Path to clang-format executable"
#)
#
#set(CMAKE_C_CLANG_FORMAT "${CLANG_FORMAT_EXE}")
#set(CMAKE_CXX_CLANG_FORMAT "${CLANG_FORMAT_EXE}")

# the following is used if cross-compiling
set(CMAKE_CROSSCOMPILING_EMULATOR "")
