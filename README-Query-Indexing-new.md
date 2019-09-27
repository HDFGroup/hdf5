
Required libraries and tools
======

Make sure that you have access to relatively recent versions of
automake and autoconf to avoid configuration errors when building the required libraries.

On Cori:
```sh
    module load automake/1.15
    module load autoconf/2.69
```

Fastbit
----
    Download the fast bit library (Version 2.0.3)
    Visit the LBNL fastbit sourceforge site: https://codeforge.lbl.gov/frs/?group_id=44
    Follow the instructions found in the README file to build and install.
    
```sh
    ./configure --prefix=$HOME
    make && make install
```

Berkeley DB
----
    libdb.so is often included with modern Linux distributions so a download may not be necessary.

   libdb.so is often included with modern Linux distributions so a download may not be necessary.
   If otherwise unavailable, the latest Berkeley DB sources are available from:
   https://www.oracle.com/database/technologies/related/berkeleydb-downloads.html
   Follow the instructions found in the README file to build and install.

```sh
    cd db-18.1.32/build/unix
    ../dist/configure --prefix=$HOME
    make && make install
```

Building
====

```sh
    git clone https://bitbucket.hdfgroup.org/projects/HDFFV/repos/hdf5 -b feature/indexing $H5_DIR
    mkdir build && cd build
    ccmake .. (where ".." is the relative path to the HDF5 directory)
```

Type 'c' multiple times and choose suitable options. Recommended options are:

    BUILD_SHARED_LIBS                ON
    CMAKE_C_FLAGS                    for each of these FLAGS, add -dynamic on NERSC machines 
    CMAKE_CXX_FLAGS                  to avoid linking errors.
    CMAKE_EXE_LINKER_FLAGS
    CMAKE_SHARED_LINKER_FLAGS

    HDF5_BUILD_CPP_LIB               OFF  
    HDF5_ENABLE_PARALLEL             ON
    HDF5_ENABLE_FASTBIT_SUPPORT      ON
    HDF5_ENABLE_DB_SUPPORT           ON

Setting include directory and library paths may require you to toggle to
the advanced mode by typing 't'. Validate the support library installation
directories as well as the $HOME/include directory for Fastbit and BerkeleyDB
include paths.   Once you are done and do not see any
errors, type 'g' to generate makefiles. Once you exit the CMake
configuration screen and are ready to build the targets, do:

```sh
    make
```

Testing
====
On a non-slurm linux box which allows MPI applications to be run directly
using mpirun or mpiexec, the set of HDF5 tests (not Query-Indexing related)
can be run to validate the HDF5 build.

```sh
    make test
```

On NERSC machines (e.g. Cori), do the following to interactively run
a simple test.
* Job allocation (e.g. use 4 nodes)
```sh
    salloc -C haswell -N 1 -t 01:00:00 -q interactive --gres=craynetwork:2
```
Run a simple query example
----

* Running a basic query example

The command line format of this test application is:
    ./query [number-of-objects] [metadata plugin-id] [rawdata plugin-id]
        where ID values are:
            0 = none
            1 = dummy
            2 = fastbit
            3 = alacrity
            4 = dummy
            5 = db
            6 = mdhim

We currently only support 0,2,4, and 5

```sh
    cd bin
    srun -N 1 -n 1 -c 2 --mem=25600 --cpu_bind=cores --gres=craynetwork:1 ./query 10 5 2
```
