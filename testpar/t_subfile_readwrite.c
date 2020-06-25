#include <stdio.h>
#include "hdf5.h"
#include "H5FDsubfile_public.h"

#include "mpi.h"

/* Elements per rank */
#define DATA_SIZE 100000

#define WRITE_OP 1
#define READ_OP 2

int mpi_size = -1;
int mpi_rank = -1;

static int test_subfile_op(int op_type, hid_t subfile_id, int64_t offset, int64_t local_elements, void *local_data)
{
	int i, flags = O_RDWR;
	int errors = 0;
	int loop_count = 20;
	int64_t local_data_size = local_elements * sizeof(int);
	int64_t total_data_size = 0;

	int (*subfile_ftn)(hid_t context_id,int64_t offset, int64_t elements, int dtype_extent, void *data) = sf_read_independent;

	double m_startTime, m_endTime;
	double this_time, max_time, min_time, total_time, avg_time;
	double bw;
	const char *OPERATION = "READ";

	if (op_type == WRITE_OP) {
		flags = O_CREAT|O_TRUNC|O_RDWR;
		subfile_ftn = sf_write_independent;
		OPERATION = "WRITE";
	}
	
	for(i=0; i < loop_count; i++) {
		m_startTime = MPI_Wtime();
		if (sf_open_subfiles(subfile_id, NULL, flags) < 0) {
			puts("sf_open_subfiles returned an error!");
			errors++;
			goto done;
		}
		if (subfile_ftn(subfile_id, offset, local_elements, sizeof(int), local_data) < 0) {
			puts("subfile_ftn returned an error!");
			errors++;
			goto done;
		}
		if (sf_close_subfiles(subfile_id) < 0) {
			puts("sf_close_subfiles returned an error!");
			errors++;
			goto done;
		}
		m_endTime = MPI_Wtime();
		this_time = m_endTime - m_startTime;
		if (i == 0) {
			min_time = this_time;
			max_time = this_time;
		}
		else if (this_time < min_time) {
			min_time = this_time;
		}
		if (this_time > max_time) {
			max_time = this_time;
		}
		total_time += this_time;
	}

	total_data_size = local_data_size * mpi_size;
	avg_time = total_time / (double) loop_count;
    bw = ((double)total_data_size)/ avg_time / (1024.0 * 1024.0);
	if (mpi_rank == 0) {
        printf("%s Perf: %lf BW/[MBs] %ld Bytes AvgTime[sec] %lf\n", OPERATION, bw, total_data_size, avg_time);
        fflush(stdout);
	}
done:
	return errors;
}


int
main(int argc, char **argv)
{
	int errors = 0;
	int mpi_provides, require = MPI_THREAD_MULTIPLE;
	hid_t subfile_id = -1;
	double m_startTime, m_endTime;
	double this_time, max_time, min_time, total_time, avg_time;
	double bw;
	int64_t local_elements = DATA_SIZE;
	int64_t local_data_size = 0;
	int64_t offset = 0;
	int *local_data = NULL;
	int *verify_data = NULL;
		
	MPI_Init_thread(&argc, &argv, require, &mpi_provides);
	MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
	MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

	if (argc > 1) {
		int64_t check_value = atoll(argv[1]);
		if (check_value > 0) {
			local_elements = check_value;
		}
	}

    H5open();

	local_data_size = local_elements * sizeof(int);
	local_data = (int *)malloc((size_t)local_data_size);
	if (local_data) {
		int k, base = local_elements * mpi_rank;
		offset = local_data_size * mpi_rank;
		for (k=0; k < local_elements; k++) {
			local_data[k] = k + base;
		}
	}
	else {
		perror("malloc failure");
		errors++;
		goto done;
	}

	verify_data = (int *)malloc((size_t)local_data_size);
	if (verify_data == NULL) {
		perror("malloc failure");
		errors++;
		goto done;
	}

	if (H5FDsubfiling_init() == SUCCEED) {
		subfile_id = get_subfiling_context();
		printf("[%d] subfile_id = %lx\n", mpi_rank, subfile_id);
	}
	else if (mpi_rank == 0) {
		puts("Error: Unable to initialize subfiling!");
		errors++;
		goto done;
	}

	if (test_subfile_op( WRITE_OP, subfile_id, offset, local_elements, local_data)) {
		puts("Subfile writing test returned an error!");
		errors++;
		goto done;
	}
	if (test_subfile_op( READ_OP, subfile_id, offset, local_elements, verify_data)) {
		puts("Subfile reading test returned an error!");
		errors++;
		goto done;
	}

done:

	H5FDsubfiling_finalize();
	MPI_Barrier(MPI_COMM_WORLD);

	if (local_data) {
		free(local_data);
		local_data = NULL;
	}
	
    H5close();

	MPI_Finalize();
	return 0;
}
