#include "h5dsm_example.h"

int main(int argc, char *argv[]) {
    uuid_t pool_uuid;
    char *pool_grp = NULL;
    hid_t file = -1, attr = -1, fapl = -1, space = -1, type = -1, acpl = -1, def_acpl = -1;
    int ndims;
    hsize_t dims[2];
    char *name_buf = NULL;
    size_t name_buf_size = 0;
    ssize_t ssize_ret;
    htri_t tri_ret;
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

    printf("Opening attribute\n");

    /* Open attribute */
    if((attr = H5Aopen_by_name(file, argv[3], argv[4], H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /* Check attribute dataspace */
    if((space = H5Aget_space(attr)) < 0)
        ERROR;
    if((ndims = H5Sget_simple_extent_ndims(space)) < 0)
        ERROR;
    if(ndims != 2)
        PRINTF_ERROR("ndims == %d, expected 2\n", ndims);
    if(H5Sget_simple_extent_dims(space, dims, NULL) < 0)
        ERROR;
    if(dims[0] != 4)
        PRINTF_ERROR("dims[0] == %d, expected 4\n", (int)dims[0]);
    if(dims[1] != 6)
        PRINTF_ERROR("dims[1] == %d, expected 6\n", (int)dims[1]);

    /* Check attribute datatype */
    if((type = H5Aget_type(attr)) < 0)
        ERROR;
    if((tri_ret = H5Tequal(type, H5T_NATIVE_INT)) < 0)
        ERROR;
    if(!tri_ret)
        PRINTF_ERROR("datatype does not equal H5T_NATIVE_INT\n");

    /* Check ACPL */
    if((acpl = H5Aget_create_plist(attr)) < 0)
        ERROR;
    if((def_acpl = H5Pcreate(H5P_ATTRIBUTE_CREATE)) < 0)
        ERROR;
    if((tri_ret = H5Pequal(acpl, def_acpl)) < 0)
        ERROR;
    if(!tri_ret)
        PRINTF_ERROR("ACPL does not equal default\n");

    /* Check attribute name */
    if((ssize_ret = H5Aget_name(attr, 0, NULL)) < 0)
        ERROR;
    name_buf_size = strlen(argv[4]) + 1;
    if(ssize_ret != (ssize_t)name_buf_size - 1)
        PRINTF_ERROR("unexpected attribute name size");
    if(NULL == (name_buf = malloc(name_buf_size)))
        ERROR;
    if((ssize_ret = H5Aget_name(attr, name_buf_size, name_buf)) < 0)
        ERROR;
    if(ssize_ret != (ssize_t)name_buf_size - 1)
        PRINTF_ERROR("unexpected attribute name size");
    if(name_buf[name_buf_size - 1] != '\0')
        PRINTF_ERROR("attribute name not terminated");
    if(strcmp(name_buf, argv[4]))
        PRINTF_ERROR("attribute name does not match");

    /* Close */
    if(H5Aclose(attr) < 0)
        ERROR;
    if(H5Fclose(file) < 0)
        ERROR;
    if(H5Pclose(fapl) < 0)
        ERROR;
    if(H5Sclose(space) < 0)
        ERROR;
    if(H5Tclose(type) < 0)
        ERROR;
    if(H5Pclose(acpl) < 0)
        ERROR;
    if(H5Pclose(def_acpl) < 0)
        ERROR;
    free(name_buf);

    printf("Success\n");

    (void)MPI_Finalize();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Aclose(attr);
        H5Fclose(file);
        H5Pclose(fapl);
        H5Sclose(space);
        H5Tclose(type);
        H5Pclose(acpl);
        H5Pclose(def_acpl);
    } H5E_END_TRY;
    free(name_buf);

    (void)MPI_Finalize();
    return 1;
}

