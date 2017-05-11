#include "h5dsm_example.h"

#define VL_INCREMENT 4
#define NUM_KEYS 345

int main(int argc, char *argv[]) {
    uuid_t pool_uuid;
    char *pool_grp = NULL;
    hid_t file = -1, map1 = -1, map2 = -1, map3 = -1, fapl = -1;
    hid_t dtid1, dtid2;
    const char *str_wdata[5]= {
        "Four score and seven years ago our forefathers brought forth on this continent a new nation,",
        "conceived in liberty and dedicated to the proposition that all men are created equal.",
        "Now we are engaged in a great civil war,",
        "testing whether that nation or any nation so conceived and so dedicated can long endure.",
        "President Abraham Lincoln"
    };   /* Information to write */
    char *str_rdata[5];   /* Information read in */
    hvl_t wdata[5];
    hvl_t rdata[5];
    int i, value, ret;
    hsize_t count;
    hbool_t exists;

    (void)MPI_Init(&argc, &argv);

    if(argc != 3)
        PRINTF_ERROR("argc must be 3\n");

    /* Set write buffer for VL data*/
    {
        int n = 0;
        int j;

        /* Allocate and initialize VL data to write */
        for(i = 0; i < 5; i++) {
            int temp = i * VL_INCREMENT + VL_INCREMENT;

            wdata[i].p = malloc(temp * sizeof(unsigned int));
            wdata[i].len = temp;
            for(j = 0; j < temp; j++)
                ((unsigned int *)wdata[i].p)[j] = n++;
        } /* end for */
    } /* end block */

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

    /* Open file */
    if((file = H5Fopen(argv[2], H5F_ACC_RDWR, fapl)) < 0)
        ERROR;

    printf("Creating Maps\n");

    dtid1 = H5Tvlen_create (H5T_NATIVE_UINT);

    /* Create an HDF5 VL string datatype */
    dtid2 = H5Tcopy(H5T_C_S1);
    H5Tset_size(dtid2, H5T_VARIABLE);

    if((map1 = H5Mcreate(file, "MAP_VL_T", H5T_NATIVE_INT, dtid1, 
                H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;
    if((map2 = H5Mcreate(file, "MAP_VL_STR", H5T_NATIVE_INT, dtid2, 
                H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;
    if((map3 = H5Mcreate(file, "MAP_FIXED", H5T_NATIVE_INT, H5T_NATIVE_INT, 
                H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    for(i = 0 ; i < 5 ; i++) {
                ret = H5Mset(map1, H5T_NATIVE_INT, &i, dtid1, &wdata[i],
                 H5P_DEFAULT);
        if (ret != 0)
            ERROR;
    } /* end for */

    for(i = 0 ; i < 5 ; i++) {
                ret = H5Mset(map2, H5T_NATIVE_INT, &i, dtid2, str_wdata[i],
                 H5P_DEFAULT);
        if (ret != 0)
            ERROR;
    } /* end for */

    for(i = 0 ; i < NUM_KEYS ; i++) {
                value = 1000 + i;
                ret = H5Mset(map3, H5T_NATIVE_INT, &i, H5T_NATIVE_INT, &value,
                 H5P_DEFAULT);
        if (ret != 0)
            ERROR;
    } /* end for */

    if(H5Mclose(map1) < 0)
        ERROR;
    if(H5Mclose(map2) < 0)
        ERROR;
    if(H5Mclose(map3) < 0)
        ERROR;

    printf("Opening and reading maps\n");

    if((map1 = H5Mopen(file, "MAP_VL_T", H5P_DEFAULT)) < 0)
        ERROR;
    if((map2 = H5Mopen(file, "MAP_VL_STR", H5P_DEFAULT)) < 0)
        ERROR;
    if((map3 = H5Mopen(file, "MAP_FIXED", H5P_DEFAULT)) < 0)
        ERROR;

    ret = H5Mget_count(map1, &count);
    if (ret != 0)
        ERROR;
    printf("KEY count %s = %llu\n", "MAP_VL_T", count);
    if(count != 5)
        ERROR;
    ret = H5Mget_count(map2, &count);
    if (ret != 0)
        ERROR;
    printf("KEY count %s = %llu\n", "MAP_VL_STR", count);
    if(count != 5)
        ERROR;
    ret = H5Mget_count(map3, &count);
    if (ret != 0)
        ERROR;
    printf("KEY count %s = %llu\n", "MAP_FIXED", count);
    if(count != NUM_KEYS)
        ERROR;

    i = 2;
    ret = H5Mexists(map1, H5T_NATIVE_INT, &i, &exists);
    if (ret != 0)
        ERROR;
    if(!exists) {
        printf("Key %d should exist\n", i);
        ERROR;
    } /* end if */

    i = 6;
    ret = H5Mexists(map1, H5T_NATIVE_INT, &i, &exists);
    if (ret != 0)
        ERROR;
    if(exists) {
        printf("Key %d should NOT exist\n", i);
        ERROR;
    } /* end if */

        printf("Reading VL DATA: \n");
        for(i=0 ; i<5 ; i++) {
            int increment=4, j=0;
            int temp = i*increment + increment;

            ret = H5Mget(map1, H5T_NATIVE_INT, &i, dtid1, &rdata[i],
                     H5P_DEFAULT);
            if (ret != 0)
                ERROR;
            printf("Key %d  size %zu: ", i, rdata[i].len);
            for(j = 0; j < temp; j++)
                printf("%d ",((unsigned int *)rdata[i].p)[j]);
            printf("\n");
        } /* end for */

        printf("Reading VL Strings: \n");
    for(i=0 ; i<5 ; i++) {
        str_rdata[i] = NULL;
        ret = H5Mget(map2, H5T_NATIVE_INT, &i, dtid2, &str_rdata[i],
                 H5P_DEFAULT);
        if (ret != 0)
            ERROR;
        printf("Key %d:  %s\n", i, str_rdata[i]);
        free(str_rdata[i]);
        }

    printf("Checking Fixed length Data ... \n");
        for(i=0 ; i<NUM_KEYS ; i++) {
        value = -1;
        ret = H5Mget(map3, H5T_NATIVE_INT, &i, H5T_NATIVE_INT, &value,
                 H5P_DEFAULT);

        if(value != 1000+i) {
            printf("Key %d: Value recieved = %d\n", i, value);
            ERROR;
        }
        }
    printf("All good\n");


    /* Close */
    if(H5Tclose(dtid2) < 0)
        ERROR;
    if(H5Mclose(map1) < 0)
        ERROR;
    if(H5Mclose(map2) < 0)
        ERROR;
    if(H5Mclose(map3) < 0)
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
        H5Mclose(map1);
        H5Mclose(map2);
        H5Mclose(map3);
        H5Fclose(file);
        H5Tclose(dtid2);
        H5Pclose(fapl);
    } H5E_END_TRY;

    (void)MPI_Finalize();
    return 1;
}

