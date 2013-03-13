
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mpi.h"
#include "hdf5.h"
#include "iod_api.h"

int main(int argc, char **argv) {
    int my_size, my_rank;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &my_size);
    printf("Server APP processes = %d, my rank is %d\n", my_size, my_rank);

    H5VLiod_start_handler(MPI_COMM_WORLD, MPI_INFO_NULL);

    MPI_Finalize();
    return 0;
}

