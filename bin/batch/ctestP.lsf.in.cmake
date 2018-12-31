#!/bin/tcsh
### LSF syntax
#BSUB -nnodes 1                  #number of nodes
#BSUB -W 30                      #walltime in minutes
#BSUB -G guests                  #account
#BSUB -e ctestPerrors.txt        #stderr
#BSUB -o ctestPoutput.txt        #stdout
#BSUB -J hdf5_ctestP             #job
##BSUB -q pbatch                 #queue to use
#BSUB -q pdebug

##date; hostname
##echo -n 'JobID is '; echo $LSB_JOBID

cd @HDF5_BINARY_DIR@
echo "Run parallel test command. Test output will be in build/ctestP.out"
ctest . -R 'TEST_PAR|PH5DIFF|PERFORM' -E t_cache_image -C Release -T test >& ctestP.out

echo "Done running ctest parallel command."
