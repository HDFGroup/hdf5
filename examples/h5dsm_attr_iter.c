#include "h5dsm_example.h"

int od_test_g;

herr_t iter_cb(hid_t loc_id, const char *attr_name, const H5A_info_t *ainfo,
        void *op_data) {
    /* Print attribute name and size */
    printf("%s: %d bytes\n", attr_name, (int)ainfo->data_size);

    /* Check op_data */
    if(op_data != &od_test_g)
        PRINTF_ERROR("op_data incorrect");

    return 0;

error:
    return -1;
}

int main(int argc, char *argv[]) {
    uuid_t pool_uuid;
    char *pool_grp = NULL;
    hid_t file = -1, obj = -1, fapl = -1;
    hsize_t num_attr = 0;
    herr_t ret;
    H5VL_daosm_snap_id_t snap_id;

    (void)MPI_Init(&argc, &argv);

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

    printf("Iterating over attributes\n");

    /* Iterate */
    if((ret = H5Aiterate(obj, H5_INDEX_NAME, H5_ITER_NATIVE, &num_attr, iter_cb, &od_test_g)) < 0)
        ERROR;

    printf("Complete.  Number of attributes: %d\n", (int)num_attr);

    /* Close */
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
        H5Oclose(obj);
        H5Fclose(file);
        H5Pclose(fapl);
    } H5E_END_TRY;

    (void)MPI_Finalize();
    return 1;
}

