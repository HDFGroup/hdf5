# This CMake toolchain file configures cross-compilation settings for building HDF5 on non-native platforms.
# Copy this file and rename it (e.g., to crosscompile-myplatform.cmake).
# Replace the placeholder values with actual values for your target system and compilers.
# Then use this file with CMake by specifying it with the -DCMAKE_TOOLCHAIN_FILE option.

# Set the target system name for cross-compiling
set (CMAKE_SYSTEM_NAME system_name)

# Specify the compiler vendor and compilers for C, C++, and Fortran
set (CMAKE_COMPILER_VENDOR "compiler_name")
set (CMAKE_C_COMPILER compiler_cc)
set (CMAKE_CXX_COMPILER compiler_c++)
set (CMAKE_Fortran_COMPILER compiler_fortran)

# Optional: Specify an emulator for running binaries during cross-compilation
set (CMAKE_CROSSCOMPILING_EMULATOR "")

# Fortran kind and precision settings for cross-compiling
set (PAC_FC_ALL_INTEGER_KINDS "\{x,y,z\}" CACHE INTERNAL "Find available INTEGER KINDs for Fortran")
set (PAC_FC_ALL_REAL_KINDS "\{x,y,z\}" CACHE INTERNAL "Find available REAL KINDs for Fortran")
set (${HDF_PREFIX}_PAC_FC_MAX_REAL_PRECISION X CACHE INTERNAL "Maximum decimal precision for REALs in Fortran")
set (PAC_FORTRAN_NUM_INTEGER_KINDS "Y" CACHE INTERNAL "Number of valid integer kinds for Fortran")
set (PAC_FORTRAN_NUM_REAL_KINDS "Z" CACHE INTERNAL "Number of valid real kinds for Fortran")

# Fortran configuration variables for HDF5
set (${HDF_PREFIX}_H5CONFIG_F_NUM_IKIND "INTEGER, PARAMETER :: num_ikinds = Y")
set (${HDF_PREFIX}_H5CONFIG_F_IKIND "INTEGER, DIMENSION(1:num_ikinds) :: ikind = (/x,y,z/)")

# If using ISO_FORTRAN_ENV, set logical kind variables
if (${PAC_USE_ISO_FORTRAN_ENV})
  set (PAC_FORTRAN_NUM_LOGICAL_KINDS "W" CACHE INTERNAL "Find available LOGICAL KINDs for Fortran")
  set (PAC_FC_ALL_LOGICAL_KINDS "\{x,y,z\}" CACHE INTERNAL "LOGICAL KINDS FOUND for Fortran")
endif ()

# Native kind and sizeof settings for Fortran types
set (PAC_FC_ALL_INTEGER_KINDS_SIZEOF "\{x,y,z\}" CACHE INTERNAL "Find available INTEGER KINDs for Fortran")
set (PAC_FC_ALL_REAL_KINDS_SIZEOF "\{x,y,z\}" CACHE INTERNAL "Find available REAL KINDs for Fortran")
set (PAC_FORTRAN_NATIVE_INTEGER_SIZEOF i CACHE INTERNAL "Find sizeof of native kinds sizeof INTEGER")
set (PAC_FORTRAN_NATIVE_INTEGER_KIND j CACHE INTERNAL "Find sizeof of native kinds kind of INTEGER")
set (PAC_FORTRAN_NATIVE_REAL_SIZEOF k CACHE INTERNAL "Find sizeof of native kinds sizeof REAL")
set (PAC_FORTRAN_NATIVE_REAL_KIND m CACHE INTERNAL "Find sizeof of native kinds kind of REAL")
set (PAC_FORTRAN_NATIVE_DOUBLE_SIZEOF n CACHE INTERNAL "Find sizeof of native kinds sizeof DOUBLE PRECISION")
set (PAC_FORTRAN_NATIVE_DOUBLE_KIND o CACHE INTERNAL "Find sizeof of native kinds kind of DOUBLE PRECISION")
