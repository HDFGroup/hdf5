#!/bin/bash

#SBATCH -p knl -C quad,cache
#SBATCH --nodes=1
#SBATCH -t 00:30:00
#SBATCH --mail-type=BEGIN,END,FAIL
##SBATCH --mail-user=<username>@sandia.gov
#SBATCH --export=ALL
#SBATCH --job-name=h5_ctestS

cd @HDF5_BINARY_DIR@
echo "Run command. Test output will be in build/ctestS.out"
ctest -S ctest_serial.cmake >& ctestS.out

echo "Done running command."
touch ctestS.done
