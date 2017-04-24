#include "h5dsm_example.h"
#include <time.h>
#include <mpi.h>

int main(int argc, char *argv[]) {
    uuid_t pool_uuid;
    char *pool_grp = NULL;
    hid_t file = -1, dset = -1, file_space = -1, mem_space = -1, fapl = -1;
    hsize_t dims[2] = {4, 6};
    hsize_t start[2], count[2];
    H5VL_daosm_snap_id_t snap_id;
    int buf[4][6];
    int rank, mpi_size;
    char *file_sel_str[2] = {"XXX...", "...XXX"};
    int i, j;

    (void)MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    if(mpi_size != 2)
        PRINTF_ERROR("mpi_size != 2\n");

    /* Seed random number generator */
    srand(time(NULL));

    if(argc < 4 || argc > 5)
        PRINTF_ERROR("argc must be 4 or 5\n");

    /* Parse UUID */
    if(0 != uuid_parse(argv[1], pool_uuid))
        ERROR;

    /* Initialize VOL */
    if(H5VLdaosm_init(MPI_COMM_WORLD, pool_uuid, pool_grp) < 0)
        ERROR;

    /* Set up FAPL */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        ERROR;
    if(H5Pset_fapl_daosm(fapl, MPI_COMM_WORLD, MPI_INFO_NULL) < 0)
        ERROR;
    if(H5Pset_all_coll_metadata_ops(fapl, true) < 0)
        ERROR;

    /* Open file */
    if((file = H5Fopen(argv[2], H5F_ACC_RDWR, fapl)) < 0)
        ERROR;

    /* Open dataset */
    if((dset = H5Dopen2(file, argv[3], H5P_DEFAULT)) < 0)
        ERROR;

    if(rank == 1)
        MPI_Barrier(MPI_COMM_WORLD);

    printf("---------------Rank %d---------------\n", rank);
    printf("Selecting elements denoted with X\n");
    printf("Memory File\n");
    printf("...... %s\n", file_sel_str[rank]);
    for(i = 1; i < 4; i++)
        printf(".XXXX. %s\n", file_sel_str[rank]);

    /* Fill and print buffer */
    printf("Writing data. Buffer is:\n");
    for(i = 0; i < 4; i++) {
        for(j = 0; j < 6; j++) {
            buf[i][j] = rand() % 10;
            printf("%d ", buf[i][j]);
        }
        printf("\n");
    }

    if(rank == 0)
        MPI_Barrier(MPI_COMM_WORLD);


    /* Set up dataspaces */
    if((file_space = H5Screate_simple(2, dims, NULL)) < 0)
        ERROR;
    if((mem_space = H5Screate_simple(2, dims, NULL)) < 0)
        ERROR;
    start[0] = 0;
    start[1] = 3 * rank;
    count[0] = 4;
    count[1] = 3;
    if(H5Sselect_hyperslab(file_space, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        ERROR;
    start[0] = 1;
    start[1] = 1;
    count[0] = 3;
    count[1] = 4;
    if(H5Sselect_hyperslab(mem_space, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        ERROR;

    /* Write data */
    if(H5Dwrite(dset, H5T_NATIVE_INT, mem_space, file_space, H5P_DEFAULT, buf) < 0)
        ERROR;

    /* Save snapshot if requested */
    if(argc == 5) {
        if(H5VLdaosm_snap_create(file, &snap_id) < 0)
            ERROR;
        if(rank == 0)
            printf("Saved snapshot: snap_id = %llu\n", (long long unsigned)snap_id);
    } /* end if */

    /* Close */
    if(H5Dclose(dset) < 0)
        ERROR;
    if(H5Fclose(file) < 0)
        ERROR;
    if(H5Sclose(file_space) < 0)
        ERROR;
    if(H5Sclose(mem_space) < 0)
        ERROR;
    if(H5Pclose(fapl) < 0)
        ERROR;

    printf("Success\n");

    (void)MPI_Finalize();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Fclose(file);
        H5Sclose(file_space);
        H5Sclose(mem_space);
        H5Pclose(fapl);
    } H5E_END_TRY;

    (void)MPI_Finalize();
    return 1;
}

