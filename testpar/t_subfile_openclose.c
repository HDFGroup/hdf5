#include <stdio.h>
#include "hdf5.h"
#include "H5FDsubfile_public.h"

#include "mpi.h"

int
main(int argc, char **argv)
{

	int i, mpi_size, mpi_rank;
	int loop_count = 20;
	int mpi_provides, require = MPI_THREAD_MULTIPLE;
	hid_t subfile_id = -1;

	MPI_Init_thread(&argc, &argv, require, &mpi_provides);
	MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
	MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

	if (argc > 1) {
		int check_value = atoi(argv[1]);
		if (check_value > 0) {
			loop_count = check_value;
		}
	}

    H5open();

	if (H5FDsubfiling_init() == SUCCEED) {
		subfile_id = get_subfiling_context();
		printf("[%d] subfile_id = %lx\n", mpi_rank, subfile_id);
	}
	else if (mpi_rank == 0) {
		puts("Error: Unable to initialize subfiling!");
	}

	for(i=0; i < loop_count; i++) {
		sf_open_subfiles(subfile_id, NULL, O_CREAT|O_TRUNC|O_RDWR);
		sf_close_subfiles(subfile_id);
	}

	H5FDsubfiling_finalize();

	MPI_Barrier(MPI_COMM_WORLD);

    H5close();

	MPI_Finalize();
	return 0;
}
