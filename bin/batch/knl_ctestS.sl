#!/bin/bash

#SBATCH -p knl -C quad,flat
#SBATCH --nodes=1
#SBATCH -t 00:30:00
#SBATCH --mail-type=BEGIN,END,FAIL
##SBATCH --mail-user=<username>@sandia.gov
#SBATCH --export=ALL
#SBATCH --job-name=h5_ctestS

cd build
CMD="ctest . -E TEST_PAR|H5DIFF|PERFORM -C Release -j 32 -T test"

echo "Run $CMD. Test output will be in build/ctestS.out"
$CMD  >& ctestS.out
echo "Done running $CMD"

