#include "h5dsm_example.h"

int main(int argc, char *argv[]) {
    uuid_t pool_uuid;
    char *pool_grp = NULL;
    hid_t file = -1, obj = -1, attr = -1, fapl = -1;
    H5VL_daosm_snap_id_t snap_id;

    (void)MPI_Init(&argc, &argv);

    if(argc < 6 || argc > 7)
        PRINTF_ERROR("argc must be 6 or 7\n");

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

    /* Open snapshot if specified */
    if(argc == 7) {
        snap_id = (H5VL_daosm_snap_id_t)atoi(argv[6]);
        printf("Opening snapshot %llu\n", (long long unsigned)snap_id);
        if(H5Pset_daosm_snap_open(fapl, snap_id) < 0)
            ERROR;
    } /* end if */

    /* Open file */
    if((file = H5Fopen(argv[2], H5F_ACC_RDONLY, fapl)) < 0)
        ERROR;

    /* Open object */
    if(!strcmp(argv[3], "-d") || !strcmp(argv[3], "-D")) {
        if((obj = H5Dopen2(file, argv[4], H5P_DEFAULT)) < 0)
            ERROR;
    }
    else {
        if(strcmp(argv[3], "-g") && strcmp(argv[3], "-G"))
            PRINTF_ERROR("argv[3] must be -d, -D, -g, or -G\n");
        if((obj = H5Gopen2(file, argv[4], H5P_DEFAULT)) < 0)
            ERROR;
    }

    printf("Opening attribute\n");

    /* Open attribute */
    if((attr = H5Aopen(obj, argv[5], H5P_DEFAULT)) < 0)
        ERROR;

    /* Close */
    if(H5Aclose(attr) < 0)
        ERROR;
    if(H5Oclose(obj) < 0)
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
        H5Oclose(obj);
        H5Fclose(file);
        H5Pclose(fapl);
    } H5E_END_TRY;

    (void)MPI_Finalize();
    return 1;
}

