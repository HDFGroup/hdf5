#include "h5dsm_example.h"

int main(int argc, char *argv[]) {
    uuid_t pool_uuid;
    char *pool_grp = NULL;
    hid_t file = -1, dset = -1, fapl = -1, space = -1, type = -1, dcpl = -1, dapl = -1, def_dcpl = -1, def_dapl = -1;
    int ndims;
    hsize_t dims[2];
    htri_t tri_ret;
    H5VL_daosm_snap_id_t snap_id;

    (void)MPI_Init(&argc, &argv);

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

    printf("Opening dataset\n");

    /* Open dataset */
    if((dset = H5Dopen2(file, argv[3], H5P_DEFAULT)) < 0)
        ERROR;

    /* Check dataset dataspace */
    if((space = H5Dget_space(dset)) < 0)
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

    /* Check dataset datatype */
    if((type = H5Dget_type(dset)) < 0)
        ERROR;
    if((tri_ret = H5Tequal(type, H5T_NATIVE_INT)) < 0)
        ERROR;
    if(!tri_ret)
        PRINTF_ERROR("datatype does not equal H5T_NATIVE_INT\n");

    /* Check DCPL */
    if((dcpl = H5Dget_create_plist(dset)) < 0)
        ERROR;
    if((def_dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        ERROR;
    if((tri_ret = H5Pequal(dcpl, def_dcpl)) < 0)
        ERROR;
    if(!tri_ret)
        PRINTF_ERROR("DCPL does not equal default\n");

    /* Check DAPL */
    if((dapl = H5Dget_access_plist(dset)) < 0)
        ERROR;
    if((def_dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        ERROR;
    if((tri_ret = H5Pequal(dapl, def_dapl)) < 0)
        ERROR;
    if(!tri_ret)
        PRINTF_ERROR("DAPL does not equal default\n");

    /* Close */
    if(H5Dclose(dset) < 0)
        ERROR;
    if(H5Fclose(file) < 0)
        ERROR;
    if(H5Pclose(fapl) < 0)
        ERROR;
    if(H5Sclose(space) < 0)
        ERROR;
    if(H5Tclose(type) < 0)
        ERROR;
    if(H5Pclose(dcpl) < 0)
        ERROR;
    if(H5Pclose(dapl) < 0)
        ERROR;
    if(H5Pclose(def_dcpl) < 0)
        ERROR;
    if(H5Pclose(def_dapl) < 0)
        ERROR;

    printf("Success\n");

    (void)MPI_Finalize();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Fclose(file);
        H5Pclose(fapl);
        H5Sclose(space);
        H5Tclose(type);
        H5Pclose(dcpl);
        H5Pclose(dapl);
        H5Pclose(def_dcpl);
        H5Pclose(def_dapl);
    } H5E_END_TRY;

    (void)MPI_Finalize();
    return 1;
}

