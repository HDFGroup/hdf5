#include "h5dsm_example.h"
#include <time.h>

int main(int argc, char *argv[]) {
    uuid_t pool_uuid;
    char *pool_grp = NULL;
    hid_t file = -1, attr = -1, fapl = -1;
    int buf[4][6];
    int i, j;
    H5VL_daosm_snap_id_t snap_id;

    (void)MPI_Init(&argc, &argv);

    /* Seed random number generator */
    srand(time(NULL));

    if(argc < 5 || argc > 6)
        PRINTF_ERROR("argc must be 5 or 6\n");

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

    /* Open snapshot if specified */
    if(argc == 6) {
        snap_id = (H5VL_daosm_snap_id_t)atoi(argv[5]);
        printf("Opening snapshot %llu\n", (long long unsigned)snap_id);
        if(H5Pset_daosm_snap_open(fapl, snap_id) < 0)
            ERROR;
    } /* end if */

    /* Open file */
    if((file = H5Fopen(argv[2], H5F_ACC_RDONLY, fapl)) < 0)
        ERROR;

    /* Open attribute */
    if((attr = H5Aopen_by_name(file, argv[3], argv[4], H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    printf("Reading attribute\n");

    /* Initialize buffer */
    for(i = 0; i < 4; i++)
        for(j = 0; j < 6; j++)
            buf[i][j] = -1;

    /* Read data */
    if(H5Aread(attr, H5T_NATIVE_INT, buf) < 0)
        ERROR;

    /* Print buffer */
    printf("Successfully read data. Buffer is:\n");
    for(i = 0; i < 4; i++) {
        for(j = 0; j < 6; j++)
            printf("%d ", buf[i][j]);
        printf("\n");
    }

    /* Close */
    if(H5Aclose(attr) < 0)
        ERROR;
    if(H5Fclose(file) < 0)
        ERROR;
    if(H5Pclose(fapl) < 0)
        ERROR;

    printf("Success\n");

    (void)MPI_Finalize();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Aclose(attr);
        H5Fclose(file);
        H5Pclose(fapl);
    } H5E_END_TRY;

    (void)MPI_Finalize();
    return 1;
}

