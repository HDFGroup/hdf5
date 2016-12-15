#include "h5dsm_example.h"

int main(int argc, char *argv[]) {
    uuid_t pool_uuid;
    char *pool_grp = "daos_tier0";
    hid_t file = -1, grp = -1, trans = -1, fapl = -1;
    uint64_t trans_num;

    (void)MPI_Init(&argc, &argv);
    (void)daos_init();

    if(argc != 4)
        PRINTF_ERROR("argc != 4\n");

    /* Parse UUID */
    if(0 != uuid_parse(argv[1], pool_uuid))
        ERROR;

    /* Set up FAPL */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        ERROR;
    if(H5Pset_fapl_daosm(fapl, MPI_COMM_WORLD, MPI_INFO_NULL, pool_uuid, pool_grp) < 0)
        ERROR;

    /* Open file */
    if((file = H5Fopen_ff(argv[2], H5F_ACC_RDWR, fapl, &trans)) < 0)
        ERROR;

    /* Get next transaction */
    if(H5TRget_trans_num(trans, &trans_num) < 0)
        ERROR;
    if(H5TRclose(trans) < 0)
        ERROR;
    if((trans = H5TRcreate(file, trans_num + 1)) < 0)
        ERROR;

    printf("Creating group - transaction number = %llu\n", (long long unsigned)(trans_num + 1));

    /* Create group */
    if((grp = H5Gcreate_ff(file, argv[3], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, trans)) < 0)
        ERROR;

    /* Commit transaction */
    if(H5TRcommit(trans) < 0)
        ERROR;

    /* Close */
    if(H5Gclose_ff(grp, -1) < 0)
        ERROR;
    if(H5TRclose(trans) < 0)
        ERROR;
    if(H5Fclose_ff(file, -1) < 0)
        ERROR;
    if(H5Pclose(fapl) < 0)
        ERROR;

    printf("Success\n");

    (void)daos_fini();
    (void)MPI_Finalize();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose_ff(grp, -1);
        H5TRclose(trans);
        H5Fclose_ff(file, -1);
        H5Pclose(fapl);
    } H5E_END_TRY;

    (void)daos_fini();
    (void)MPI_Finalize();
    return 1;
}

