#include "h5rados_example.h"
#include <time.h>

int main(int argc, char *argv[]) {
    rados_t cluster;
    char *pool = "mypool";
    hid_t file = -1, dset = -1, fapl = -1;
    int buf[4][6];
    int i, j;

    (void)MPI_Init(&argc, &argv);

    /* Seed random number generator */
    srand(time(NULL));

    if(argc != 3)
        PRINTF_ERROR("argc != 3\n");

    if(rados_create(&cluster, NULL) < 0)
        ERROR;
    if(rados_conf_read_file(cluster, "ceph.conf") < 0)
        ERROR;

    /* Initialize VOL */
    if(H5VLrados_init(cluster, pool) < 0)
        ERROR;

    /* Set up FAPL */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        ERROR;
    if(H5Pset_fapl_rados(fapl, MPI_COMM_WORLD, MPI_INFO_NULL) < 0)
        ERROR;
    if(H5Pset_all_coll_metadata_ops(fapl, true) < 0)
        ERROR;

    /* Open file */
    if((file = H5Fopen(argv[1], H5F_ACC_RDONLY, fapl)) < 0)
        ERROR;

    /* Open dataset */
    if((dset = H5Dopen2(file, argv[2], H5P_DEFAULT)) < 0)
        ERROR;

    printf("Reading dataset\n");

    /* Initialize buffer */
    for(i = 0; i < 4; i++)
        for(j = 0; j < 6; j++)
            buf[i][j] = -1;

    /* Read data */
    if(H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        ERROR;

    /* Print buffer */
    printf("Successfully read data. Buffer is:\n");
    for(i = 0; i < 4; i++) {
        for(j = 0; j < 6; j++)
            printf("%d ", buf[i][j]);
        printf("\n");
    }

    /* Close */
    if(H5Dclose(dset) < 0)
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
        H5Dclose(dset);
        H5Fclose(file);
        H5Pclose(fapl);
    } H5E_END_TRY;

    (void)MPI_Finalize();
    return 1;
}

