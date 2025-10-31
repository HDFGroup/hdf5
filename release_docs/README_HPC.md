# Installation Instructions for Parallel HDF5

## 1. Overview

This file contains instructions for installing parallel HDF5 (PHDF5) using
CMake. The document covers:

- **Section 1:** Requirements and prerequisites
- **Section 2:** Obtaining HDF5 source
- **Section 3:** Quick start instructions
- **Section 4:** Automated builds with ctest (HPC systems)
- **Section 5:** Manual CMake configuration
- **Section 6:** Cross-compiling for HPC hardware
- **Section 7:** Running parallel tests
- **Appendix A:** Sample MPI-IO programs

### 1.1. Requirements

PHDF5 requires:

- CMake version 3.26 or greater
- An MPI compiler with MPI-IO support
- A POSIX compliant parallel file system (see References)

You should first consult with your system support staff for information on:

- How to compile an MPI program
- How to run an MPI application
- How to access the parallel file system

Sample MPI-IO C and Fortran programs are provided in Appendix A. Use them to
test your MPI compiler and parallel file system before building HDF5.

### 1.2. Prerequisites for HPC Systems

When building on HPC systems:

1. **Create a working directory** accessible from compute nodes for running tests.
   Use a scratch space or parallel file system - **NOT** your home directory, as
   this typically causes test failures.

2. **Load required modules:**
   - Desired compiler modules (and set CC, FC, CXX if needed)
   - CMake version 3.26 or greater
   - MPI implementation module

3. **For Cray and other systems with recommend compiler wrappers,** set compiler environment variables AFTER loading modules:

   ```bash
   export CC=cc
   export FC=ftn
   export CXX=CC
   ```

### 1.3. Further Help

For installation help, post questions to the HDF Forum or HDF Support:

- **HDF Forum:** <https://forum.hdfgroup.org/>
- **HDF Support:** <https://support.hdfgroup.org/>

Include the output of `uname -a` and the contents of `CMakeCache.txt` and
`CMakeError.log` from your build directory, and the loaded modules if applicable.

---

## 2. Obtaining HDF5 Source

Obtain HDF5 source code from the HDF5 repository or from a release tar file:

```bash
git clone https://github.com/HDFGroup/hdf5.git [-b branch] [directory]
```

If no branch is specified, the `develop` branch will be checked out.
If no directory is specified, source will be in the `hdf5` directory.

For release or snapshot tar files, extract them to your working directory.

> **Note:** When using the ctest automated build method (Section 4), the source
> directory should be named `hdf5-<version>`, where version uses format `1.xx.xx`
> or `2.xx.xx`. Use `bin/h5vers` to determine the version string if needed.

---

## 3. Quick Start Instructions

### 3.1. Using CMake Presets (Recommended for General Builds)

For building with CMake 3.26 or greater using presets:

```bash
cd hdf5
cmake --workflow --preset ci-StdShar-GNUC --fresh
```

> **Note:** Standard presets do not enable parallel by default. To enable parallel
> support, you need to create a custom preset in `CMakeUserPresets.json` that sets
> `-DHDF5_ENABLE_PARALLEL=ON`, or use the standard CMake build approach below.

### 3.2. Standard CMake Build (Recommended for Parallel)

For a basic parallel build:

```bash
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release \
      -DHDF5_ENABLE_PARALLEL=ON \
      -DBUILD_SHARED_LIBS=ON \
      -DBUILD_TESTING=ON \
      ..
cmake --build . --config Release
ctest . -C Release
cmake --install . --prefix /path/to/install
```

### 3.3. Specifying MPI Compiler

CMake can usually find MPI automatically. To specify explicitly:

```bash
cmake -DCMAKE_C_COMPILER=mpicc \
      -DCMAKE_Fortran_COMPILER=mpif90 \
      -DHDF5_ENABLE_PARALLEL=ON \
      ..
```

Or use MPI compiler wrappers:

```bash
export CC=mpicc
export FC=mpif90
cmake -DHDF5_ENABLE_PARALLEL=ON ..
```

### 3.4. Important Configuration Options

| Option | Description |
|--------|-------------|
| `-DHDF5_ENABLE_PARALLEL=ON` | Enable parallel HDF5 (required) |
| `-DBUILD_SHARED_LIBS=ON` | Build shared libraries |
| `-DBUILD_STATIC_LIBS=ON` | Build static libraries |
| `-DHDF5_BUILD_FORTRAN=ON` | Build Fortran interface |
| `-DHDF5_BUILD_CPP_LIB=OFF` | C++ disabled in parallel builds |
| `-DHDF5_ENABLE_THREADSAFE=OFF` | Thread safety disabled in parallel builds |
| `-DHDF5_ENABLE_SUBFILING_VFD=ON` | Enable subfiling VFD (parallel I/O optimization) |
| `-DMPIEXEC_EXECUTABLE=mpiexec` | MPI launcher executable |
| `-DMPIEXEC_NUMPROC_FLAG=-n` | MPI flag for number of processes |
| `-DMPIEXEC_MAX_NUMPROCS=6` | Number of processes for tests |

> **Note:** Some MPI implementations (e.g., OpenMPI 4.0+) disallow oversubscribing
> by default. Set `MPIEXEC_MAX_NUMPROCS` to available processors or fewer, or add
> appropriate flags (e.g., `--oversubscribe` for OpenMPI).

---

## 4. Automated Builds with ctest (HPC Systems)

The ctest command provides automated configure, build, test, and package
workflow for HPC systems with batch schedulers.

### 4.1. Setup Steps

1. Rename source directory to `hdf5-<version>` (e.g., `hdf5-2.0.0`)

2. Copy or link these CMake scripts to your working directory:
   - `hdf5-<version>/config/cmake/scripts/HDF5config.cmake`
   - `hdf5-<version>/config/cmake/scripts/CTestScript.cmake`
   - `hdf5-<version>/config/cmake/scripts/HDF5options.cmake`

3. Your working directory should contain:
   ```
   CTestScript.cmake
   HDF5config.cmake
   HDF5options.cmake
   hdf5-<version>/
   ```

### 4.2. Running ctest

Basic ctest command:

```bash
ctest -S HDF5config.cmake,BUILD_GENERATOR=Unix -C Release -V -O hdf5.log
```

For parallel builds on HPC systems with batch schedulers:

```bash
ctest -S HDF5config.cmake,HPC=sbatch,MPI=true -C Release -V -O hdf5.log
```

#### Available HPC Options

Add after `HDF5config.cmake,` separated by commas:

| Option | Description |
|--------|-------------|
| `HPC=sbatch` | Use SLURM batch system |
| `HPC=bsub` | Use LSF batch system |
| `MPI=true` | Enable parallel (disables C++, Java, threadsafe) |
| `LOCAL_BATCH_SCRIPT_ARGS="--account=<acct>"` | Supply batch job account information |

#### Examples

**SLURM system with parallel:**

```bash
ctest -S HDF5config.cmake,HPC=sbatch,MPI=true \
      -C Release -V -O hdf5.log
```

Use `-VV` instead of `-V` for more detailed logging.

---

## 5. Manual CMake Configuration

If the automated ctest approach is not suitable, you can manually configure
and build HDF5.

### 5.1. Create Build Directory

```bash
mkdir build && cd build
```

### 5.2. Run CMake Configure

Example for parallel build with Fortran on HPC system:

```bash
cmake \
  -C ../hdf5-<version>/config/cmake/cacheinit.cmake \
  -DCMAKE_BUILD_TYPE:STRING=Release \
  -DCMAKE_INSTALL_PREFIX:PATH=/install/path \
  -DHDF5_ENABLE_PARALLEL:BOOL=ON \
  -DHDF5_BUILD_FORTRAN:BOOL=ON \
  -DHDF5_BUILD_CPP_LIB:BOOL=OFF \
  -DHDF5_BUILD_JAVA:BOOL=OFF \
  -DHDF5_ENABLE_THREADSAFE:BOOL=OFF \
  -DHDF5_ENABLE_ZLIB_SUPPORT:BOOL=OFF \
  -DHDF5_ENABLE_SZIP_SUPPORT:BOOL=OFF \
  -DMPIEXEC_EXECUTABLE:STRING=srun \
  -DMPIEXEC_NUMPROC_FLAG:STRING=-n \
  -DMPIEXEC_MAX_NUMPROCS:STRING=6 \
  -G"Unix Makefiles" \
  ../hdf5-<version>
```

### 5.3. Build

```bash
cmake --build . --config Release -j 8
```

### 5.4. Test

For systems where you can run MPI directly:

```bash
ctest . -C Release
```

For batch systems, create and submit batch scripts (see section 7.3).

### 5.5. Install

```bash
cmake --install . --prefix /install/path
```

---

## 6. Cross-Compiling for HPC Hardware

### 6.1. Overview

Cross-compiling is the process of building software on one system architecture (like a login node) to be run on a different architecture (like a compute node). This section provides a historical overview of how this was done on systems that are no longer in service.

### 6.2. Historical Example: Knights Landing (KNL) on Cray XC40

A common historical use case was compiling for Intel Knights Landing (KNL) nodes on Cray XC40 systems, such as the retired Mutrino and Cori machines. These supercomputers had login nodes with a standard CPU architecture (e.g., Haswell) but used the different KNL architecture for their compute nodes.

To build software for KNL, a "module swapping" technique was required. The build process involved:
1. Loading the compiler module for the login node architecture (e.g., `craype-haswell`) to configure the project.

2. Switching to the compiler module for the compute node architecture (e.g., `craype-mic-knl`) before starting the actual compilation.

This process was managed by special CMake toolchain files and custom batch scripts, which were often automated within the `ctest` framework.

### 6.3. Cross-Compilation on Current Systems

The specific hardware (Cray XC40, KNL) and the build procedures described above are historical and no longer in use.

While cross-compilation is less common on many modern, homogeneous HPC clusters, it is still a necessary technique for advanced architectures, such as systems with different processor types or accelerators (e.g., GPUs).

**If you need assistance with cross-compiling for a current HPC system, please contact the facility administrators or The HDF Group (Section 1.3).**

---

## 7. Running Parallel Tests

### 7.1. Test Directory Location

The parallel test suite (`testpar/`) contains tests for Parallel HDF5 and MPI-IO.

By default, tests use the current directory for test files. To specify a
different location (e.g., parallel file system):

```bash
export HDF5_PARAPREFIX=/scratch/username/hdf5test
ctest . -C Release
```

> **Important:** This is critical for performance - avoid using NFS or home
> directories for parallel testing.

### 7.2. Important Test Notes

#### t_mpi

Tests basic MPI-IO features used by Parallel HDF5. Returns non-zero
exit code if required features fail.

**Exception:** Testing files >2GB will print informational messages but not fail,
as HDF5 can use alternative file drivers (e.g., family driver) to handle size limits.

#### t_cache

Performs many small I/O requests. May run slowly on NFS or other
non-parallel file systems. Set `HDF5_PARAPREFIX` to a parallel file system if
this test is slow.

#### Test Express Level

Controls test thoroughness:

```bash
export HDF5_TEST_EXPRESS=3    # Quick tests (default)
export HDF5_TEST_EXPRESS=0    # Exhaustive tests
```

#### Test Timeout

Default is 1200 seconds (20 minutes). Modify in CMake with:

```bash
-DDART_TESTING_TIMEOUT=3600
```

### 7.3. Running Tests on Batch Systems

**For SLURM systems:**

```bash
sbatch -C quad,cache build/bin/batch/ctestS.sl    # Serial tests
sbatch -C quad,cache build/bin/batch/ctestP.sl    # Parallel tests
```

**For LSF systems:**

```bash
bsub build/bin/batch/ctestS.lsf    # Serial tests
bsub build/bin/batch/ctestP.lsf    # Parallel tests
```

Batch scripts are generated during CMake configuration in the build directory.

### 7.4. Running Specific Test Categories

To run specific test suites:

```bash
ctest -R "H5TEST"              # Core library tests
ctest -R "MPI_TEST"            # Parallel/MPI tests
ctest -R "CPP|FORTRAN"         # C++ and Fortran tests
ctest -E "MPI_TEST"            # Exclude parallel tests
ctest --parallel 4             # Run 4 tests in parallel
```

---

## 8. Known Platform Notes

### 8.1. Linux Systems

For MPICH on Linux, ensure >2GB file support by configuring MPICH with:

```bash
-cflags="-D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64"
```

This is available on Linux kernels 2.4 and greater.

### 8.2. Cray Systems

- Use `CC=cc`, `FC=ftn`, `CXX=CC` after loading compiler modules
- Unload `craype-hugepages2M` if loaded (**Note**: This is situational advice and is not a universal rule, but it may be a valid troubleshooting step if you encounter memory-related performance issues or allocation errors.)
- Disable shared libraries if encountering linking issues:
  ```bash
  -DBUILD_SHARED_LIBS=OFF
  ```

### 8.3. OpenMPI

OpenMPI 4.0+ disallows oversubscribing by default. Either:

- Set `MPIEXEC_MAX_NUMPROCS` to actual processor count, or
- Add `--oversubscribe` flag: `-DMPIEXEC_PREFLAGS=--oversubscribe`

---

## References

**POSIX Compliant:** After a write() to a regular file has successfully
returned, any successful read() from each byte position modified by that
write() will return the data that was written. A subsequent write() to the
same byte will overwrite the file data. If a read() can be proven by any
means [e.g., MPI_Barrier()] to occur after a write() of that data, it must
reflect that write(), even if calls are made by different processes.

> Lewin, D. (1994). "POSIX Programmer's Guide (pg. 513-4)". O'Reilly &
> Associates.

---

## Appendix A. Sample MPI-IO Programs

Here are sample MPI-IO C and Fortran programs to test your MPI compiler and
parallel file system before building HDF5. The programs assume they run in
a parallel file system (create test files in current directory). For more
examples, please refer to the following directories:
HDF5Examples/C/H5PAR and HDF5Examples/FORTRAN/H5PAR

### Example Compiling and Running

```bash
mpicc Sample_mpio.c -o c.out
mpiexec -np 4 c.out

mpif90 Sample_mpio.f90 -o f.out
mpiexec -np 4 f.out
```

### Sample_mpio.c

```c
/* Simple MPI-IO program testing if a parallel file can be created.
 * Default filename can be specified via first program argument.
 * Each process writes something, then reads all data back.
 */

#include <stdio.h>
#include <unistd.h>
#include <mpi.h>
#ifndef MPI_FILE_NULL           /*MPIO may be defined in mpi.h already       */
#   include <mpio.h>
#endif

#define DIMSIZE	10		/* dimension size, avoid powers of 2. */
#define PRINTID printf("Proc %d: ", mpi_rank)

int main(int ac, char **av)
{
    char hostname[128];
    int  mpi_size, mpi_rank;
    MPI_File fh;
    char *filename = "./mpitest.data";
    char mpi_err_str[MPI_MAX_ERROR_STRING];
    int  mpi_err_strlen;
    int  mpi_err;
    char writedata[DIMSIZE], readdata[DIMSIZE];
    char expect_val;
    int  i, irank;
    int  nerrors = 0;		/* number of errors */
    MPI_Offset  mpi_off;
    MPI_Status  mpi_stat;

    MPI_Init(&ac, &av);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    /* get file name if provided */
    if (ac > 1){
	filename = *++av;
    }
    if (mpi_rank==0){
	printf("Testing simple MPIO program with %d processes accessing file %s\n",
	    mpi_size, filename);
        printf("    (Filename can be specified via program argument)\n");
    }

    /* show the hostname so that we can tell where the processes are running */
    if (gethostname(hostname, 128) < 0){
	PRINTID;
	printf("gethostname failed\n");
	return 1;
    }
    PRINTID;
    printf("hostname=%s\n", hostname);

    if ((mpi_err = MPI_File_open(MPI_COMM_WORLD, filename,
	    MPI_MODE_RDWR | MPI_MODE_CREATE | MPI_MODE_DELETE_ON_CLOSE,
	    MPI_INFO_NULL, &fh))
	    != MPI_SUCCESS){
	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	PRINTID;
	printf("MPI_File_open failed (%s)\n", mpi_err_str);
	return 1;
    }

    /* each process writes some data */
    for (i=0; i < DIMSIZE; i++)
	writedata[i] = mpi_rank*DIMSIZE + i;
    mpi_off = mpi_rank*DIMSIZE;
    if ((mpi_err = MPI_File_write_at(fh, mpi_off, writedata, DIMSIZE, MPI_BYTE,
	    &mpi_stat))
	    != MPI_SUCCESS){
	MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	PRINTID;
	printf("MPI_File_write_at offset(%ld), bytes (%d), failed (%s)\n",
		(long) mpi_off, (int) DIMSIZE, mpi_err_str);
	return 1;
    };

    /* make sure all processes has done writing. */
    MPI_Barrier(MPI_COMM_WORLD);

    /* each process reads all data and verify. */
    for (irank=0; irank < mpi_size; irank++){
	mpi_off = irank*DIMSIZE;
	if ((mpi_err = MPI_File_read_at(fh, mpi_off, readdata, DIMSIZE, MPI_BYTE,
		&mpi_stat))
		!= MPI_SUCCESS){
	    MPI_Error_string(mpi_err, mpi_err_str, &mpi_err_strlen);
	    PRINTID;
	    printf("MPI_File_read_at offset(%ld), bytes (%d), failed (%s)\n",
		    (long) mpi_off, (int) DIMSIZE, mpi_err_str);
	    return 1;
	};
	for (i=0; i < DIMSIZE; i++){
	    expect_val = irank*DIMSIZE + i;
	    if (readdata[i] != expect_val){
		PRINTID;
		printf("read data[%d:%d] got %d, expect %d\n", irank, i,
			readdata[i], expect_val);
		nerrors++;
	    }
	}
    }
    if (nerrors)
	return 1;

    MPI_File_close(&fh);

    PRINTID;
    printf("all tests passed\n");

    MPI_Finalize();
    return 0;
}
```

### Sample_mpio.f90

```fortran
!
! The following example demonstrates how to create and close a parallel
! file using MPI-IO calls.
!
! USE MPI is the proper way to bring in MPI definitions but many
! MPI Fortran compiler supports the pseudo standard of INCLUDE.
! So, HDF5 uses the INCLUDE statement instead.
!

     PROGRAM MPIOEXAMPLE

     USE mpi

     IMPLICIT NONE

     CHARACTER(LEN=80), PARAMETER :: filename = "filef.h5" ! File name
     INTEGER     ::   ierror  ! Error flag
     INTEGER     ::   fh      ! File handle
     INTEGER     ::   amode   ! File access mode

     call MPI_INIT(ierror)
     amode = MPI_MODE_RDWR + MPI_MODE_CREATE + MPI_MODE_DELETE_ON_CLOSE
     call MPI_FILE_OPEN(MPI_COMM_WORLD, filename, amode, MPI_INFO_NULL, fh, ierror)
     print *, "Trying to create ", filename
     if ( ierror .eq. MPI_SUCCESS ) then
        print *, "MPI_FILE_OPEN succeeded"
        call MPI_FILE_CLOSE(fh, ierror)
     else
        print *, "MPI_FILE_OPEN failed"
     endif

     call MPI_FINALIZE(ierror);
     END PROGRAM
```
