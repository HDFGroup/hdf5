# Uncomment the following to use cross-compiling
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_COMPILER_VENDOR "CrayLinuxEnvironment")

set(CMAKE_C_COMPILER cc)
set(CMAKE_Fortran_COMPILER ftn)

# the following is used if cross-compiling
set(CMAKE_CROSSCOMPILING_EMULATOR "")

# option to use pre-generated H5Tinit.c file
set(HDF5_USE_PREGEN OFF)
# directory where H5Tinit.c file will be found
#set(HDF5_USE_PREGEN_DIR "/lscratch1/lknox/HDF5_1_10_4/CMake-hdf5-1.10.4")

# option to generate H5Tinit.c by running H5detect on knl compute node during build
set(HDF5_BATCH_H5DETECT ON)
set(HDF5_BATCH_CMD "sbatch")
set(HDF5_BATCH_H5DETECT_SCRIPT "knl_H5detect.sl")
set(MPIEXEC_EXECUTABLE "srun")
