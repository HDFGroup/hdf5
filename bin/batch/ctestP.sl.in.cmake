#!/bin/bash

#SBATCH --nodes=1
#SBATCH -t 00:30:00
#SBATCH --mail-type=BEGIN,END,FAIL
##SBATCH --mail-user=<username>@sandia.gov
#SBATCH --export=ALL
#SBATCH --job-name=h5_ctestP

cd @HDF5_BINARY_DIR@
CMD="ctest . -R MPI_TEST_ -E t_cache_image -C Release -T test"

echo "Run $CMD. Test output will be in build/ctestP.out"
$CMD >& ctestP.out
echo "Done running $CMD"

