#!/bin/bash

#SBATCH -p knl -C quad
#SBATCH --nodes=1
#SBATCH -t 00:10:00
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=<username>@sandia.gov
#SBATCH --export=ALL
#SBATCH --job-name=knl_h5detect


# Inputs:  Build directory, output file name, executable file name (username/email if available).
PROGNAME=H5detect
OUTPUT=H5Tinit.c

CMD="@HDF5_BINARY_DIR@/bin/${PROGNAME} @HDF5_GENERATED_SOURCE_DIR@/${OUTPUT}"
echo "Run $CMD"
srun -n 1 $CMD
echo "Done running $CMD"

