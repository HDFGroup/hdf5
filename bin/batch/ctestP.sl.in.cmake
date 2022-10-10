#!/bin/bash

#SBATCH --nodes=1
#SBATCH -t 00:30:00
#SBATCH --mail-type=BEGIN,END,FAIL
##SBATCH --mail-user=<username>@sandia.gov
#SBATCH --export=ALL
#SBATCH --job-name=h5_ctestP

cd @HDF5_BINARY_DIR@
echo "Run parallel test command. Test output will be in build/ctestP.out"
ctest -S ctest_parallel.cmake >& ctestP.out

echo "Done running ctest parallel command."
touch ctestP.done
