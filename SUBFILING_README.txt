HDF5 version 1.13.0 currently under development
------------------------------------------------------------------------------

This branch of the HDF5 development library provides a prototype implementation
for the ECP ExaIO / STDM15-3 project.  The Sub-Filing functionality is not yet
complete but provides support for HDF5 contigous reads and writes of datasets
via a Sub-filing VOL and a prototype VFD.

HDF5 Sub-filing is designed as a software RAID-0 implementation in which
software controllers called IO-Concentrators provide access to individual data
files.  At present, only a single IO-Concentrator (IOC) is allocated per NODE,
though this can be tuned via an environment variable.

An important detail with respect to the IOC implementation is that this
functionality is provided via a dedicated execution thread on the lowest MPI
rank on each node.   This thread is augmented with a supporting pool of
"worker threads" to off-load the actual file reading and writeing and thereby
improve IOC thread message handling latency. Communication between the
parallel HDF5 application processes and the collection of IOCs is
accomplished by utilizing MPI. A consequence of this reliance on MPI for
IOC messaging is that parallel HDF5 applications *MUST* initialize the MPI
library using MPI_Init_thread, e.g.

int
main(int argc, char **argv)
{
	int mpi_size, mpi_rank;
	int mpi_provides, require = MPI_THREAD_MULTIPLE;
		
	MPI_Init_thread(&argc, &argv, require, &mpi_provides);
	MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
	MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    ...
	MPI_Finalize();
}

NOTE:
On Cori, the default modules providing the 'cc' compiler and access to
MPI libraries are not sufficient for use with SUB-FILING.  In particular,
the default MPI library does not support MPI_THREAD_MULTIPLE. I believe
that it supports Multithreading, but only MPI_THREAD_FUNNELED or potentially
MPI_THREAD_SERIALIZED. The initial benchmarking efforts utilize OpenMPI to
provide the necessary thread safe functionality.

At login, the modules preloaded by default are:

rawarren@cori06:~> module list
Currently Loaded Modulefiles:
  1) modules/3.2.11.4                                 12) gni-headers/5.0.12.0-7.0.1.1_6.31__g3b1768f.ari
  2) altd/2.0                                         13) xpmem/2.2.20-7.0.1.1_4.13__g0475745.ari
  3) darshan/3.1.7                                    14) job/2.2.4-7.0.1.1_3.40__g36b56f4.ari
  4) intel/19.0.3.199                                 15) dvs/2.12_2.2.157-7.0.1.1_9.4__g083131db
  5) craype-network-aries                             16) alps/6.6.58-7.0.1.1_6.8__g437d88db.ari
  6) craype/2.6.2                                     17) rca/2.2.20-7.0.1.1_4.51__g8e3fb5b.ari
  7) cray-libsci/19.06.1                              18) atp/2.1.3
  8) udreg/2.3.2-7.0.1.1_3.36__g8175d3d.ari           19) PrgEnv-intel/6.0.5
  9) ugni/6.0.14.0-7.0.1.1_7.38__ge78e5b0.ari         20) craype-haswell
 10) pmi/5.0.14                                       21) cray-mpich/7.7.10
 11) dmapp/7.1.1-7.0.1.1_4.52__g38cf134.ari           22) craype-hugepages2M
rawarren@cori06:~> 

Interestingly, the default C compiler (or at least a compiler wrapper which
calls the actual C compiler) appears to know about the default MPI and thus
avoids the necessity of users providing an include path (-I<path>) or a library
link path (-L<path>) when creating executables or shared objects (libraries).
As a consequence, we unload these specific modules prior to selecting
alternatives for compilation and for the MPI library implementation.
These defaults need to overridden, e.g.

MODULE UNLOADS::
rawarren@cori06:~> module unload cray-mpich/7.7.10
rawarren@cori06:~> module unload craype/2.6.2

MODULE LOADS::
rawarren@cori06:~> module load gcc/9.3.0
rawarren@cori06:~> module load openmpi/4.0.2





