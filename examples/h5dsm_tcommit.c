#include "h5dsm_example.h"

#define FILE_NAME "tcommit.h5"

hbool_t verbose_g = 1;

int main(int argc, char *argv[]) {
    uuid_t pool_uuid;
    char *pool_grp = NULL;
    hid_t file = -1, dset = -1, type1 = -1, type2 = -1, type3 = -1, space = -1, fapl = -1;
    hsize_t dims[2] = {4, 2};
    H5O_info_t oinfo;
    htri_t tri_ret;

    (void)MPI_Init(&argc, &argv);

    /* Seed random number generator */
    srand(time(NULL));

    if((argc != 2) && (argc != 3))
        PRINTF_ERROR("argc must be 2 or 3\n");

    if(argc == 3)
        verbose_g = 0;

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

    /* Create file */
    if((file = H5Fcreate(FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        ERROR;

    /* Set up dataspace */
    if((space = H5Screate_simple(2, dims, NULL)) < 0)
        ERROR;

    /* Set up type */
    if((type1 = H5Tcreate(H5T_COMPOUND, sizeof(int) + 1 + sizeof(double))) < 0)
        ERROR;
    if(H5Tinsert(type1, "a", 0, H5T_NATIVE_INT) < 0)
        ERROR;
    if(H5Tinsert(type1, "b", sizeof(int), H5T_NATIVE_CHAR) < 0)
        ERROR;
    if(H5Tinsert(type1, "c", sizeof(int) + 1, H5T_NATIVE_DOUBLE) < 0)
        ERROR;

    /* Copy type */
    if((type2 = H5Tcopy(type1)) < 0)
        ERROR;

    /* Commit type */
    if(verbose_g)
        printf("Committing datatype \"dtype\"\n");
    if(H5Tcommit2(file, "dtype", type2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        ERROR;
    if(H5Oget_info(type2, &oinfo) < 0)
        ERROR;

    /* Create dataset using committed datatype */
    if(verbose_g)
        printf("Creating dataset \"dset\" using committed datatype \"dtype\"\n");
    if((dset = H5Dcreate2(file, "dset", type2, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /* Get dataset's datatype */
    if(verbose_g)
        printf("Getting dataset \"dset\"\'s datatype\n");
    if((type3 = H5Dget_type(dset)) < 0)
        ERROR;

    /* Check all types are equal */
    if(verbose_g)
        printf("Checking that all types are equal\n");
    if((tri_ret = H5Tequal(type1, type2)) < 0)
        ERROR;
    if(!tri_ret)
        PRINTF_ERROR("Types not equal");
    if((tri_ret = H5Tequal(type1, type3)) < 0)
        ERROR;
    if(!tri_ret)
        PRINTF_ERROR("Types not equal");
    if(verbose_g)
        printf("Success\n");

    /* Close and reopen file */
    if(verbose_g)
        printf("Reopening file\n");
    if(H5Tclose(type2) < 0)
        ERROR;
    type2 = -1;
    if(H5Tclose(type3) < 0)
        ERROR;
    type3 = -1;
    if(H5Dclose(dset) < 0)
        ERROR;
    dset = -1;
    if(H5Fclose(file) < 0)
        ERROR;
    file = -1;
    if((file = H5Fopen(FILE_NAME, H5F_ACC_RDONLY, fapl)) < 0)
        ERROR;

    /* Open committed datatype */
    if(verbose_g)
        printf("Opening committed datatype \"dtype\"\n");
    if((type2 = H5Topen2(file, "dtype", H5P_DEFAULT)) < 0)
        ERROR;

    /* Open dataset using committed datatype */
    if(verbose_g)
        printf("Opening dataset \"dset\" using committed datatype \"dtype\"\n");
    if((dset = H5Dopen2(file, "dset", H5P_DEFAULT)) < 0)
        ERROR;

    /* Get dataset's datatype */
    if(verbose_g)
        printf("Getting dataset \"dset\"\'s datatype\n");
    if((type3 = H5Dget_type(dset)) < 0)
        ERROR;

    /* Check all types are equal */
    if(verbose_g)
        printf("Checking that all types are equal\n");
    if((tri_ret = H5Tequal(type1, type2)) < 0)
        ERROR;
    if(!tri_ret)
        PRINTF_ERROR("Types not equal");
    if((tri_ret = H5Tequal(type1, type3)) < 0)
        ERROR;
    if(!tri_ret)
        PRINTF_ERROR("Types not equal");
    if(verbose_g)
        printf("Success\n");

    /* Close and reopen file */
    if(verbose_g)
        printf("Reopening file\n");
    if(H5Tclose(type2) < 0)
        ERROR;
    type2 = -1;
    if(H5Tclose(type3) < 0)
        ERROR;
    type3 = -1;
    if(H5Dclose(dset) < 0)
        ERROR;
    dset = -1;
    if(H5Fclose(file) < 0)
        ERROR;
    file = -1;
    if((file = H5Fopen(FILE_NAME, H5F_ACC_RDONLY, fapl)) < 0)
        ERROR;

    /* H5Oopen committed datatype */
    if(verbose_g)
        printf("Opening committed datatype \"dtype\" using H5Oopen\n");
    if((type2 = H5Oopen(file, "dtype", H5P_DEFAULT)) < 0)
        ERROR;

    /* Check types are equal */
    if(verbose_g)
        printf("Checking that types are equal\n");
    if((tri_ret = H5Tequal(type1, type2)) < 0)
        ERROR;
    if(!tri_ret)
        PRINTF_ERROR("Types not equal");
    if(verbose_g)
        printf("Success\n");

    /* Close and reopen file */
    if(verbose_g)
        printf("Reopening file\n");
    if(H5Tclose(type2) < 0)
        ERROR;
    type2 = -1;
    if(H5Fclose(file) < 0)
        ERROR;
    file = -1;
    if((file = H5Fopen(FILE_NAME, H5F_ACC_RDONLY, fapl)) < 0)
        ERROR;

    /* H5Oopen committed datatype */
    if(verbose_g)
        printf("Opening committed datatype \"dtype\" using H5Oopen_by_addr\n");
    if((type2 = H5Oopen_by_addr(file, oinfo.addr)) < 0)
        ERROR;

    /* Check types are equal */
    if(verbose_g)
        printf("Checking that types are equal\n");
    if((tri_ret = H5Tequal(type1, type2)) < 0)
        ERROR;
    if(!tri_ret)
        PRINTF_ERROR("Types not equal");
    if(verbose_g)
        printf("Success\n");

    /*
     * Close
     */
    if(H5Tclose(type1) < 0)
        ERROR;
    if(H5Tclose(type2) < 0)
        ERROR;
    if(H5Fclose(file) < 0)
        ERROR;
    if(H5Sclose(space) < 0)
        ERROR;
    if(H5Pclose(fapl) < 0)
        ERROR;

    printf("Success\n");

    (void)MPI_Finalize();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Tclose(type1);
        H5Tclose(type2);
        H5Tclose(type3);
        H5Dclose(dset);
        H5Fclose(file);
        H5Sclose(space);
        H5Pclose(fapl);
    } H5E_END_TRY;

    (void)MPI_Finalize();
    return 1;
}

