#!/bin/bash

#SBATCH -p knl -C quad,flat
#SBATCH --nodes=1
#SBATCH -t 01:00:00
#SBATCH --mail-type=BEGIN,END,FAIL
##SBATCH --mail-user=<username>@sandia.gov
#SBATCH --export=ALL
#SBATCH --job-name=h5_ctestS

module load cmake
module load craype-hugepages4M

cd build
CMD="ctest . -E TEST_PAR|H5DIFF|PERFORM -C Release -j 32 -T test"

echo "Run $CMD. Test output will be in build/ctestS.out"
$CMD  >& ctestS.out
echo "Done running $CMD"


#run parallel tests except t_cache_image test
CMD="ctest . -R TEST_PAR|PH5DIFF|PERFORM -E t_cache_image -C Release -T test"

echo "Run $CMD. Test output will be in build/ctestP.out"
$CMD >& ctestP.out
echo "Done running $CMD"

