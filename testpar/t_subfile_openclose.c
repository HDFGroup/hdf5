#include <stdio.h>
#include "hdf5.h"
#include "H5FDsubfile_public.h"

#include "mpi.h"

int
main(int argc, char **argv)
{

	int i, mpi_size, mpi_rank;
	int mpi_provides, require = MPI_THREAD_MULTIPLE;
	hid_t subfile_id;

	MPI_Init_thread(&argc, &argv, require, &mpi_provides);
	MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
	MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    H5open();

	if (H5FDsubfiling_init() == SUCCEED) {
		subfile_id = get_subfiling_context();
		printf("[%d] subfile_id = %lx\n", mpi_rank, subfile_id);
	}
	else if (mpi_rank == 0) {
		puts("Error: Unable to initialize subfiling!");
	}

	for(i=0; i < 10; i++) {
		sf_open_subfiles(subfile_id, NULL, O_CREAT|O_TRUNC|O_RDWR);
		sf_close_subfiles(subfile_id);
	}

	MPI_Barrier(MPI_COMM_WORLD);

	H5FDsubfiling_finalize();

    H5close();

	MPI_Finalize();
	return 0;
}
