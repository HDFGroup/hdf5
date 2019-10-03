#!/bin/bash -l

IN_DIR=`pwd`

#Defaults

HDF5_VER=""
KNL="false"
HDF5_BRANCH=""

# READ COMMAND LINE FOR THE TEST TO RUN
CTEST_OPTS=""
POSITIONAL=()
while [[ $# -gt 0 ]]
do
key="$1"
case $key in
    -v|--version)
    HDF5_VER="$2"
    shift # past argument
    shift # past value
    BRANCH_VERSTR=`echo $HDF5_VER | sed -e s/\\\./_/g`
    HDF5_BRANCH=hdf5_$BRANCH_VERSTR
    ;;
    -a|--account)
    CTEST_OPTS="LOCAL_BATCH_SCRIPT_ARGS=$2,$CTEST_OPTS"
    shift # past argument
    shift # past value
    ;;
    -knl)
    CTEST_OPTS="KNL=true,$CTEST_OPTS"
    shift # past argument
    ;;
    -h|--help)
    echo "USAGE: nnsa.sh [-v,--version x.x.x] [-knl] [-h,--help]

    where:

       -v,--version x.x.x    hdf5 version to test 
                             [default is develop; do not specify version]
       -knl                  compile for KNL [default: no]
       -a,--acount id        batch job account
       -h,--help             show this help text"
    exit 0
    ;;
    *)    # unknown option
    POSITIONAL+=("$1") # save it in an array for later
    shift # past argument
    ;;
esac
done
set -- "${POSITIONAL[@]}" # restore positional parameters

# Get the host name
UNAME="unknown"
if [ -x /usr/bin/uname ]
then
  UNAME=`/usr/bin/uname -n`
  PROC=`/usr/bin/uname -p`
  OSTYPE=`/usr/bin/uname -s`
fi
if [ -x /usr/local/bin/uname ]
then
  UNAME=`/usr/local/bin/uname -n`
  PROC=`/usr/local/bin/uname -p`
  OSTYPE=`/usr/local/bin/uname -s`
fi
if [ -x /bin/uname ]
then
  UNAME=`/bin/uname -n`
  PROC=`/bin/uname -p`
  OSTYPE=`/bin/uname -s`
fi

# Remove the domain name if present
UNAME=`echo $UNAME | sed 's;\..*;;'`

#mkdir -p CI; cd CI
#rm -rf hdf5-* HDF5-* build hdf5.log* Failed* slurm-*.out
#sleep 1

#if [[ $HDF5_BRANCH == "" ]]; then
#  git clone https://git@bitbucket.hdfgroup.org/scm/hdffv/hdf5.git hdf5
#else
#  git clone https://git@bitbucket.hdfgroup.org/scm/hdffv/hdf5.git -b $HDF5_BRANCH hdf5
#fi
#cd hdf5
#HDF5_VER=`bin/h5vers`
#cd ..
#mv hdf5 hdf5-$HDF5_VER

HDF5_VER=`bin/h5vers`
ln -s . hdf5-$HDF5_VER

# Summary of command line inputs
echo "HDF5_VER: $HDF5_VER"
echo "MISC OPTIONS: $CTEST_OPTS"
echo "HDF5_BRANCH: $HDF5_BRANCH"
echo "Current Directory:"
pwd

#sleep 1
#rm -f CTestScript.cmake HDF5config.cmake HDF5options.cmake
sleep 1
ln -s config/cmake/scripts/CTestScript.cmake .
ln -s config/cmake/scripts/HDF5config.cmake .
ln -s config/cmake/scripts/HDF5options.cmake .
echo "Contents of current directory"
ls -l

if [[ $UNAME == cori* ]];then
# Get the curent PrgEnv module setting
    module list &> out
    PRGENV_TYPE=`grep -i PrgEnv out | sed -e 's/.*PrgEnv-\(.*\)\/.*/\1/'`
    module unload cke

    module load cmake/3.14.0
#    module load cmake
# unload the current PrgEnv and compiler associated with PrgEnv
    module unload PrgEnv-$PRGENV_TYPE
    module unload $PRGENV_TYPE
# the modules to test
#   the PrgEnv to test
    MASTER_MOD="PrgEnv-gnu/6.0.4"
    module load gcc/8.2.0
#   the Compiler to switch to:
#    Format: <number of compiler versions to check> <compiler type> <list of compiler versions (modules) ... repeat)
    CC_VER=(2 intel intel/17.0.4 intel/18.0.2 2 gcc gcc/7.3.0 gcc/8.2.0)
    CTEST_OPTS="HPC=sbatch -C haswell,$CTEST_OPTS"

    _CC=cc
    _FC=ftn
    _CXX=CC

elif [[ $UNAME == serrano* ]]; then

    echo 'set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DMPIEXEC_EXECUTABLE:STRING=mpirun")' >> hdf5-$HDF5_VER/config/cmake/scripts/HPC/sbatch-HDF5options.cmake
    echo 'set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DMPIEXEC_PREFLAGS:STRING=--mca;io;ompio")' >> hdf5-$HDF5_VER/config/cmake/scripts/HPC/sbatch-HDF5options.cmake

    perl -i -pe "s/^CMD=\"ctest.*/CMD=\"ctest . -R MPI_TEST_ -E MPI_TEST_testphdf5|MPI_TEST_t_shapesame -C Release -T test\"/" hdf5-$HDF5_VER/bin/batch/ctestP.sl.in.cmake

    module purge
    module load cmake

    MASTER_MOD="openmpi-intel/3.0"
    CC_VER=(2 intel intel/17.0 intel/18.0)
    CTEST_OPTS="HPC=sbatch,$CTEST_OPTS"

    _CC=mpicc
    _FC=mpif90
    _CXX=mpicxx

elif [[ $UNAME == p90* ]]; then

#    echo 'set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DMPIEXEC_EXECUTABLE:STRING=mpirun")' >> hdf5-$HDF5_VER/config/cmake/scripts/HPC/sbatch-HDF5options.cmake
#    echo 'set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DMPIEXEC_PREFLAGS:STRING=--mca;io;ompio")' >> hdf5-$HDF5_VER/config/cmake/scripts/HPC/sbatch-HDF5options.cmake

    echo 'set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_TOOLS:BOOL=OFF")' >> hdf5-$HDF5_VER/config/cmake/scripts/HPC/sbatch-HDF5options.cmake
    echo 'set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_EXAMPLES:BOOL=OFF")' >> hdf5-$HDF5_VER/config/cmake/scripts/HPC/sbatch-HDF5options.cmake

#    perl -i -pe "s/^CMD=\"ctest.*/CMD=\"ctest . -R MPI_TEST_ -E MPI_TEST_testphdf5|MPI_TEST_t_filters_parallel|MPI_TEST_t_shapesame -C Release -T test\"/" hdf5-$HDF5_VER/bin/batch/ctestP.sl.in.cmake

#    module purge
#    module load cmake/3.13.4
#    module load mpi/openmpi3-ppc64le

    MASTER_MOD="openmpi/3.1.4"
    CC_VER=(1 gcc/8.3.0)
    CTEST_OPTS="HPC=sbatch,$CTEST_OPTS"

    _CC=mpicc
    _FC=mpif90
    _CXX=mpicxx 

elif [[ $UNAME == eclipse* ]]; then

    module purge
    module load cmake

    MASTER_MOD="intel-mpi/2018"
    CC_VER=(2 intel intel/16.0 intel/18.0)
    CTEST_OPTS="HPC=sbatch,$CTEST_OPTS"

    _CC=mpicc
    _FC=mpif90
    _CXX=mpicxx

elif [[ $UNAME == quartz* ]]; then

    echo 'set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DMPIEXEC_EXECUTABLE:STRING=mpirun")' >> hdf5-$HDF5_VER/config/cmake/scripts/HPC/sbatch-HDF5options.cmake
    echo 'set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DMPIEXEC_PREFLAGS:STRING=--mca;io;ompio")' >> hdf5-$HDF5_VER/config/cmake/scripts/HPC/sbatch-HDF5options.cmake

    perl -i -pe "s/^CMD=\"ctest.*/CMD=\"ctest . -R MPI_TEST_ -E MPI_TEST_t_bigio -C Release -T test\"/" hdf5-$HDF5_VER/bin/batch/ctestP.sl.in.cmake

    module purge
    module load cmake/3.12.1
    module load intel/16.0.4

    MASTER_MOD="openmpi/4.0.0 openmpi/4.0.0 openmpi/4.0.0"
    CC_VER=(1 intel intel/16.0.4 2 gcc gcc/7.1.0 gcc/8.1.0 1 clang clang/3.9.0)
    CTEST_OPTS="HPC=sbatch,$CTEST_OPTS"

    _CC=mpicc
    _FC=mpif90
    _CXX=mpicxx

elif [[ $UNAME == ray* ]]; then

    perl -i -pe "s/^ctest.*/ctest . -R 'MPI_TEST_' -E 'MPI_TEST_t_filters_parallel|MPI_TEST_testphdf5|MPI_TEST_t_shapesame|MPI_TEST_H5DIFF-h5diff_80' -C Release -T test > & ctestP.out;/" hdf5-$HDF5_VER/bin/batch/ray_ctestP.lsf.in.cmake
    
    module purge
    module load cmake/3.12.1
    module load xl/2016.12.02

    MASTER_MOD="spectrum-mpi spectrum-mpi spectrum-mpi"
    CC_VER=(1 xl xl/2016.12.02 1 gcc gcc/7.3.1 1 clang clang/coral-2018.08.08)
    CTEST_OPTS="HPC=raybsub,$CTEST_OPTS"

    _CC=mpicc
    _FC=mpif90
    _CXX=mpicxx

elif [[ $UNAME == lassen* ]]; then

    echo 'set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DMPIEXEC_EXECUTABLE:STRING=mpirun")' >> hdf5-$HDF5_VER/config/cmake/scripts/HPC/bsub-HDF5options.cmake

    perl -i -pe "s/^ctest.*/ctest . -E MPI_TEST_ -C Release -j 32 -T test >& ctestS.out/" hdf5-$HDF5_VER/bin/batch/ctestS.lsf.in.cmake
    perl -i -pe "s/^ctest.*/ctest . -R MPI_TEST_ -E 'MPI_TEST_testphdf5|MPI_TEST_t_shapesame' -C Release -T test >& ctestP.out/" hdf5-$HDF5_VER/bin/batch/ctestP.lsf.in.cmake
    
    module purge
    module load cmake/3.12.1
    module load xl/2016.12.02

    MASTER_MOD="spectrum-mpi"
    CC_VER=(1 xl xl/2019.02.07)
    #MASTER_MOD="spectrum-mpi spectrum-mpi spectrum-mpi"
    #CC_VER=(1 xl xl/2019.02.07 1 gcc gcc/7.3.1 1 clang clang/coral-2018.08.08)
    CTEST_OPTS="HPC=bsub,$CTEST_OPTS"

    _CC=mpicc
    _FC=mpif90
    _CXX=mpicxx

fi
module list

icnt=-1
for master_mod in $MASTER_MOD; do

  icnt=$(($icnt+1))
  num_CC=${CC_VER[$icnt]} # number of compilers for the  master_mod

  icnt=$(($icnt+1))
  module load $master_mod # Load the master module

  module unload ${CC_VER[$icnt]} # unload the general compiler type

  for i in `seq 1 $num_CC`; do # loop over compiler versions
    icnt=$(($icnt+1))
    cc_ver=${CC_VER[$icnt]} # compiler version
    rm -fr build

    module load $cc_ver # load the compiler with version
    module load $master_mod

    export CC=$_CC
    export FC=$_FC
    export CXX=$_CXX

    module list

    echo "timeout 3h ctest . -S HDF5config.cmake,SITE_BUILDNAME_SUFFIX=\"$HDF5_VER-$master_mod-$cc_ver\",${CTEST_OPTS}MPI=true,BUILD_GENERATOR=Unix,LOCAL_SUBMIT=true,MODEL=HPC -C Release -VV -O hdf5.log"
    timeout 3h ctest . -S HDF5config.cmake,SITE_BUILDNAME_SUFFIX="$HDF5_VER-$master_mod--$cc_ver",${CTEST_OPTS}MPI=true,BUILD_GENERATOR=Unix,LOCAL_SUBMIT=true,MODEL=HPC -C Release -VV -O hdf5.log

    module unload $cc_ver  # unload the compiler with version

    #rm -fr build
  done
  module unload $master_mod #unload master module
done

#Serial only tests
# Turn off parallel options for serial-only tests.  (Just be sure not to include
#  MPI=true in the ctest line.)

if [[ $UNAME == chama* ]]; then

# These are temporary workarounds that should be fixed in *-HDF5options.cmake by moving the 'set (LOCAL_BATCH_SCRIPT_PARALLEL_NAME "ctestP.sl")' (or equivalent ) into the if (DEFINED MPI) block, and otherwise either setting it to "" or maybe moving both of these to that block. 
    echo 'set (LOCAL_BATCH_SCRIPT_PARALLEL_NAME "")' >> hdf5-$HDF5_VER/config/cmake/scripts/HPC/sbatch-HDF5options.cmake
    echo 'set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DLOCAL_BATCH_SCRIPT_PARALLEL_NAME:STRING=${LOCAL_BATCH_SCRIPT_PARALLEL_NAME}")' >> hdf5-$HDF5_VER/config/cmake/scripts/HPC/sbatch-HDF5options.cmake

    module purge
    module load cmake

    CC_VER=(2 clang clang/5.0 clang/4.0)
    CTEST_OPTS="HPC=sbatch,$CTEST_OPTS"

    _CC=clang
    _FC=gfortran
    _CXX=clang++

fi

for i in `seq 1 $num_CC`; do # loop over compiler versions
  icnt=$(($icnt+1))
  cc_ver=${CC_VER[$icnt]} # compiler version
  rm -fr build

  module load $cc_ver # load the compiler with version

  export CC=$_CC
  export FC=$_FC
  export CXX=$_CXX

  module list

  echo "timeout 3h ctest . -S HDF5config.cmake,SITE_BUILDNAME_SUFFIX=\"$HDF5_VER-$cc_ver\",${CTEST_OPTS}BUILD_GENERATOR=Unix,LOCAL_SUBMIT=true,MODEL=HPC -C Release -VV -O hdf5.log"
  timeout 3h ctest . -S HDF5config.cmake,SITE_BUILDNAME_SUFFIX="$HDF5_VER-$cc_ver",${CTEST_OPTS}BUILD_GENERATOR=Unix,LOCAL_SUBMIT=true,MODEL=HPC -C Release -VV -O hdf5.log

  module unload $cc_ver  # unload the compiler with version

  #rm -fr build
done
pwd
cd $IN_DIR

