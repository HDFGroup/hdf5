#include "h5dsm_example.h"

int main(int argc, char *argv[]) {
    uuid_t pool_uuid;
    char *pool_grp = NULL;
    hid_t file = -1, fapl = -1;
    H5VL_daosm_snap_id_t snap_id;

    (void)MPI_Init(&argc, &argv);
    (void)daos_init();

    if(argc < 3 || argc > 4)
        PRINTF_ERROR("argc must be 3 or 4\n");

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
    if(argc == 4) {
        snap_id = (H5VL_daosm_snap_id_t)atoi(argv[3]);
        printf("Opening snapshot %llu\n", (long long unsigned)snap_id);
        if(H5Pset_daosm_snap_open(fapl, snap_id) < 0)
            ERROR;
    } /* end if */

    /* Open file */
    if((file = H5Fopen_ff(argv[2], H5F_ACC_RDONLY, fapl, NULL)) < 0)
        ERROR;

    /* Close */
    if(H5Fclose_ff(file, -1) < 0)
        ERROR;
    if(H5Pclose(fapl) < 0)
        ERROR;

    printf("Success\n");

    (void)MPI_Finalize();

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose_ff(file, -1);
        H5Pclose(fapl);
    } H5E_END_TRY;

    (void)MPI_Finalize();
    return 1;
}

