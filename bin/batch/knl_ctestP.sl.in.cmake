#!/bin/bash

#SBATCH -p knl -C quad,cache
#SBATCH --nodes=1
#SBATCH -t 00:30:00
#SBATCH --mail-type=BEGIN,END,FAIL
##SBATCH --mail-user=<username>@sandia.gov
#SBATCH --export=ALL
#SBATCH --job-name=h5_ctestP

cd @HDF5_BINARY_DIR@
#run parallel tests except t_cache_image test
ctest . -R MPI_TEST_ -C Release -T test >& ctestP.out

echo "Done running $CMD"

