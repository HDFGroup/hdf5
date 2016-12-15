#include "h5dsm_example.h"
#include <time.h>

int main(int argc, char *argv[]) {
    uuid_t pool_uuid;
    char *pool_grp = "daos_tier0";
    hid_t file = -1, dset = -1, trans = -1, space = -1, fapl = -1;
    hid_t nfile = -1, ndset = -1;
    hsize_t dims[1] = {256 * 1024};
    int *buf = NULL;
    int i;

    (void)MPI_Init(&argc, &argv);
    (void)daos_init();

    /* Seed random number generator */
    srand(time(NULL));

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

    /* Create file */
    if((file = H5Fcreate_ff(argv[2], H5F_ACC_TRUNC, H5P_DEFAULT, fapl, &trans)) < 0)
        ERROR;

    /* Create native file */
    if((file = H5Fcreate(argv[2], H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /* Set up dataspace */
    if((space = H5Screate_simple(1, dims, NULL)) < 0)
        ERROR;

    /* Create dataset */
    if((dset = H5Dcreate_ff(file, argv[3], H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, trans)) < 0)
        ERROR;

    /* Create native dataset */
    if((ndset = H5Dcreate2(nfile, argv[3], H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /* Allocate buffer */
    if(NULL == (buf = (int *)malloc(dims[0] * sizeof(int))))
        ERROR;

    /* Fill buffer */
    for(i = 0; i < dims[0]; i++)
        buf[i] = rand();

    /* Write data */
    printf("Writing daos-m dataset\n");
    if(H5Dwrite_ff(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf, trans) < 0)
        ERROR;
    
    /* Write native data */
    printf("Writing native dataset\n");
    if(H5Dwrite(ndset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        ERROR;

    /* Commit transaction */
    if(H5TRcommit(trans) < 0)
        ERROR;

    /* Close */
    if(H5Dclose_ff(dset, -1) < 0)
        ERROR;
    if(H5Dclose(ndset) < 0)
        ERROR;
    if(H5TRclose(trans) < 0)
        ERROR;
    if(H5Fclose_ff(file, -1) < 0)
        ERROR;
    if(H5Fclose(nfile) < 0)
        ERROR;
    if(H5Pclose(fapl) < 0)
        ERROR;
    free(buf);
    buf = NULL;

    printf("Success\n");

    (void)daos_fini();
    (void)MPI_Finalize();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose_ff(dset, -1);
        H5Dclose(ndset);
        H5TRclose(trans);
        H5Fclose_ff(file, -1);
        H5Fclose(nfile);
        H5Pclose(fapl);
    } H5E_END_TRY;

    free(buf);

    (void)daos_fini();
    (void)MPI_Finalize();
    return 1;
}

