#include "h5dsm_example.h"
#include <time.h>

int main(int argc, char *argv[]) {
    uuid_t pool_uuid;
    char *pool_grp = "daos_tier0";
    hid_t file = -1, dset = -1, trans = -1, fapl = -1;
    hid_t nfile = -1, ndset = -1;
    hsize_t dims[1] = {256 * 1024};
    int *buf = NULL, nbuf = NULL;
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

    /* Open file */
    if((file = H5Fopen_ff(argv[2], H5F_ACC_RDWR, fapl, &trans)) < 0)
        ERROR;

    /* Create native file */
    if((file = H5Fopen(argv[2], H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        ERROR;

    /* Create dataset */
    if((dset = H5Dopen_ff(file, argv[3], H5P_DEFAULT, trans)) < 0)
        ERROR;

    /* Open native dataset */
    if((ndset = H5Dopen2(nfile, argv[3], H5P_DEFAULT)) < 0)
        ERROR;

    /* Allocate buffers */
    if(NULL == (buf = (int *)malloc(dims[0] * sizeof(int))))
        ERROR;
    if(NULL == (nbuf = (int *)malloc(dims[0] * sizeof(int))))
        ERROR;

    /* Read data */
    printf("Reading daos-m dataset\n");
    if(H5Dread_ff(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf, trans) < 0)
        ERROR;
    
    /* Write native data */
    printf("Reading native dataset\n");
    if(H5Dread(ndset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        ERROR;

    /* Compare data */
    if(memcmp(buf, nbuf, dims[0] * sizeof(int)))
        PRINTF_ERROR("Buffers differ\n");
    else
        printf("Buffers are equal\n");

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
    free(nbuf);
    nbuf = NULL;

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
    free(nbuf);

    (void)daos_fini();
    (void)MPI_Finalize();
    return 1;
}

