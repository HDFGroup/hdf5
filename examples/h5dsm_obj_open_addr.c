#include "h5dsm_example.h"

int main(int argc, char *argv[]) {
    uuid_t pool_uuid;
    char *pool_grp = NULL;
    hid_t file = -1, obj = -1, fapl = -1;
    unsigned long long addr;
    H5I_type_t obj_type;
    char *obj_str = NULL;
    H5VL_daosm_snap_id_t snap_id;

    (void)MPI_Init(&argc, &argv);

    if(argc < 4 || argc > 5)
        PRINTF_ERROR("argc must be 4 or 5\n");

    /* Parse UUID */
    if(0 != uuid_parse(argv[1], pool_uuid))
        ERROR;

    /* Parse address */
    addr = strtoull(argv[3], NULL, 16);

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
    if(argc == 5) {
        snap_id = (H5VL_daosm_snap_id_t)atoi(argv[4]);
        printf("Opening snapshot %llu\n", (long long unsigned)snap_id);
        if(H5Pset_daosm_snap_open(fapl, snap_id) < 0)
            ERROR;
    } /* end if */

    /* Open file */
    if((file = H5Fopen(argv[2], H5F_ACC_RDONLY, fapl)) < 0)
        ERROR;

    printf("Opening object by address\n");

    /* Open object */
    if((obj = H5Oopen_by_addr(file, (haddr_t)addr)) < 0)
        ERROR;

    /* Get object type */
    if(H5I_BADID == (obj_type = H5Iget_type(obj)))
        ERROR;

    if(obj_type == H5I_GROUP)
        obj_str = "group";
    else if(obj_type == H5I_DATASET)
        obj_str = "dataset";
    else if(obj_type == H5I_DATATYPE)
        obj_str = "datatype";
    else if(obj_type == H5I_MAP)
        obj_str = "map";
    else
        obj_str = "unknown";
    printf("Object type is %s\n", obj_str);

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

