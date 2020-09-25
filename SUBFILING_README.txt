HDF5 version 1.13.0 currently under development
------------------------------------------------------------------------------

This branch of the HDF5 development library provides a prototype implementation
for the ECP ExaIO / STDM15-3 project.  The Sub-Filing functionality is not yet
complete but provides support for HDF5 contigous reads and writes of datasets
via a Sub-filing VOL and a prototype VFD.

HDF5 Sub-filing is designed as a software RAID-0 implementation in which
software controlers called IO-Concentrators provide access to individual data
files.  At present, only a single IO-Conentrator (IOC) is allocated per NODE,
though is can be tuned via environment variable.

An important detail with respect to the IOC implementation is that this
functionality is provided via a dedicated exeution thread on the lowest MPI
rank on each node.   This thread is augmented with a supporting pool of
"worker threads" to off-load the actual file reads and writes and thereby
improve the IOC thread message handling latencies. Communication between the
parallel HDF5 appplication processes and the collection of IOCs is
accomplished by utilizing MPI. A consequence of this reliance on MPI for
IOC messaging is that parallel HDF5 applications *MUST* initialize MPI
using MPI_Init_thread, e.g.

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
On Cori, the default modules providing the 'cc' (compiler) and access to
MPI libraries are not sufficient for use with SUB-FILING.  In particular,
the default MPI library does not support MPI_THREAD_MULTIPLE. I believe
that supports only MPI_THREAD_FUNNELED or potentially MPI_THREAD_SERIALIZED.
The initial benchmarking efforts utilize OpenMPI to provide the necessary
functionality.

Interestingly, the default C compiler (or at least a compiler wrapper which
calls the actual C compiler) appears to know about the default MPI and thus
avoids the necessity by users to provide an include path or a library link
path when creating executables or libraries. As a consequence, we unload
these specific modules prior to selecting alternatives for compilation and
the MPI library.


