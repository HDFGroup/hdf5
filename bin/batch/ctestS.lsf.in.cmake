#!/bin/tcsh
### LSF syntax
#BSUB -nnodes 1                  #number of nodes
#BSUB -W 29                      #walltime in minutes
#BSUB -G guests                  #account
#BSUB -e ctestSerrors.txt        #stderr
#BSUB -o ctestSoutput.txt        #stdout
#BSUB -J hdf5_ctestS             #job
##BSUB -q pbatch                 #queue to use
#BSUB -q pdebug

cd @HDF5_BINARY_DIR@
echo "Run command. Test output will be in build/ctestS.out"
ctest . -E 'TEST_PAR|PH5DIFF|PERFORM' -C Release -j 32 -T test >& ctestS.out

##$CMD  >& ctestS.out
echo "Done running command."

