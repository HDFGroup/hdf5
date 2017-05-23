#include "h5dsm_example.h"
#include <time.h>

#define FILE_NAME "tvlen.h5"
#define NITER 10

hbool_t verbose_g = 1;

static void print_vla(const hvl_t *vla) {
    int i, j, k;

    for(i = 0; i < 4; i++) {
        for(j = 0; j < 2; j++) {
            if(j > 0)
                printf(", ");
            printf("{");
            for(k = 0; k < (int)vla[2 * i + j].len; k++) {
                if(k > 0)
                    printf(", ");
                printf("%d", ((int *)vla[2 * i + j].p)[k]);
            } /* end for */
            printf("}");
        } /* end for */
        printf("\n");
    } /* end for */

    return;
} /* end print_vla() */

static int check_vla(const hvl_t *vla1, const hvl_t *vla2) {
    int i, j, k;

    for(i = 0; i < 4; i++)
        for(j = 0; j < 2; j++) {
            if(vla1[2 * i + j].len != vla2[2 * i + j].len)
                PRINTF_ERROR("vlen array length at location %d, %d does not match", i, j);
            for(k = 0; k < (int)vla1[2 * i + j].len; k++)
                if(((int *)vla1[2 * i + j].p)[k] != ((int *)vla2[2 * i + j].p)[k])
                    PRINTF_ERROR("vlen array data at location %d, %d, %d does not match", i, j, k);
        } /* end for */

    return 0;

error:
    return -1;
} /* end check_vla() */

static void print_vls(char **vls) {
    int i, j;

    for(i = 0; i < 4; i++) {
        for(j = 0; j < 2; j++) {
            if(j > 0)
                printf(", ");
            if(vls[2 * i + j])
                printf("\"%s\"", vls[2 * i + j]);
            else
                printf("NULL");
        } /* end for */
        printf("\n");
    } /* end for */

    return;
} /* end print_vls() */

static int check_vls(char **vls1, char **vls2) {
    int i, j;

    for(i = 0; i < 4; i++)
        for(j = 0; j < 2; j++) {
            if((vls1[2 * i + j] == NULL) != (vls2[2 * i + j] == NULL))
                PRINTF_ERROR("one vlen string at location %d, %d is NULL and the other is not", i, j);
            if(vls1[2 * i + j] && strcmp(vls1[2 * i + j], vls2[2 * i + j]))
                PRINTF_ERROR("vlen strings at location %d, %d do not match", i, j);
        } /* end for */

    return 0;

error:
    return -1;
} /* end check_vls() */

#define UPDATE_FBUF_VLA(FBUF, UBUF) \
do { \
    if((FBUF).p) \
        free((FBUF).p); \
    if((UBUF).len > 0) { \
        if(NULL == ((FBUF).p = malloc((UBUF).len * sizeof(int)))) \
            ERROR; \
        memcpy((FBUF).p, (UBUF).p, (UBUF).len * sizeof(int)); \
    } /* end if */ \
    else \
        (FBUF).p = NULL; \
    (FBUF).len = (UBUF).len; \
} while(0)

#define UPDATE_FBUF_VLS(FBUF, UBUF) \
do { \
    if(FBUF) \
        free(FBUF); \
    if(UBUF) { \
        if(NULL == ((FBUF) = strdup(UBUF))) \
            ERROR; \
    } /* end if */ \
    else \
        (FBUF) = NULL; \
} while(0)

int main(int argc, char *argv[]) {
    uuid_t pool_uuid;
    char *pool_grp = NULL;
    hid_t file = -1, dset_vla = -1, dset_vls = -1, attr_vla = -1, attr_vls = -1, space = -1, fapl = -1;
    hid_t type_vla = -1, type_vls = -1;
    hid_t space_contig = -1, space_ncontig = -1;
    hsize_t dims[2] = {4, 2};
    hsize_t start[2], count[2];
    int static_buf_vla[4][2][8];
    char static_buf_vls[4][2][8];
    hvl_t rbuf_vla[4][2];
    char *rbuf_vls[4][2];
    hvl_t wbuf_vla[4][2];
    char *wbuf_vls[4][2];
    hvl_t ebuf_vla[4][2];
    char *ebuf_vls[4][2];
    hvl_t fbuf_vla[4][2];
    char *fbuf_vls[4][2];
    int bogus_int = -1;
    char bogus_str[3] = {'-', '1', '\0'};
    const hvl_t rbuf_vla_init[4][2] = {{{1, &bogus_int}, {1, &bogus_int}},
            {{1, &bogus_int}, {1, &bogus_int}},
            {{1, &bogus_int}, {1, &bogus_int}},
            {{1, &bogus_int}, {1, &bogus_int}}};
    char * const rbuf_vls_init[4][2] = {{bogus_str, bogus_str},
            {bogus_str, bogus_str},
            {bogus_str, bogus_str},
            {bogus_str, bogus_str}};
    const hvl_t wbuf_vla_init[4][2] = {{{0, &static_buf_vla[0][0][0]}, {0, &static_buf_vla[0][1][0]}},
            {{0, &static_buf_vla[1][0][0]}, {0, &static_buf_vla[1][1][0]}},
            {{0, &static_buf_vla[2][0][0]}, {0, &static_buf_vla[2][1][0]}},
            {{0, &static_buf_vla[3][0][0]}, {1, &static_buf_vla[3][1][0]}}};
    char * const wbuf_vls_init[4][2] = {{&static_buf_vls[0][0][0], &static_buf_vls[0][1][0]},
            {&static_buf_vls[1][0][0], &static_buf_vls[1][1][0]},
            {&static_buf_vls[2][0][0], &static_buf_vls[2][1][0]},
            {&static_buf_vls[3][0][0], &static_buf_vls[3][1][0]}};
    int i, j, k, l;

    memset(fbuf_vla[0], 0, sizeof(fbuf_vla));
    memset(fbuf_vls[0], 0, sizeof(fbuf_vls));

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

    /* Set up contiguous dataspace selection */
    if((space_contig = H5Screate_simple(2, dims, NULL)) < 0)
        ERROR;
    start[0] = 1;
    start[1] = 0;
    count[0] = 2;
    count[1] = 2;
    if(H5Sselect_hyperslab(space_contig, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        ERROR;

    /* Set up con-contiguous dataspace selection */
    if((space_ncontig = H5Screate_simple(2, dims, NULL)) < 0)
        ERROR;
    start[0] = 0;
    start[1] = 1;
    count[0] = 4;
    count[1] = 1;
    if(H5Sselect_hyperslab(space_ncontig, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        ERROR;

    /*
     * Set up types
     */
    /* Create variable length array */
    /* Set up file "all" type */
    if((type_vla = H5Tvlen_create(H5T_NATIVE_INT)) < 0)
        ERROR;

    /* Set up variable length string type */
    if((type_vls = H5Tcopy(H5T_C_S1)) < 0)
        ERROR;
    if(H5Tset_size(type_vls, H5T_VARIABLE) < 0)
        ERROR;

    /*
     * Create objects
     */
    /* Create vla dataset */
    if((dset_vla = H5Dcreate2(file, "vla_dset", type_vla, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /* Create vls dataset */
    if((dset_vls = H5Dcreate2(file, "vls_dset", type_vls, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /* Create vla attribute */
    if((attr_vla = H5Acreate2(dset_vla, "vla_attr", type_vla, space, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /* Create vls attribute */
    if((attr_vls = H5Acreate2(dset_vla, "vls_attr", type_vls, space, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /*
     * Test attribute
     */
    /* Loop over iterations */
    for(i = 0; i < NITER; i++) {
        /*
         * Variable length array attribute test
         */
        /* Fill write data buffer */
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++)
                for(l = 0; l < (sizeof(static_buf_vla[0][0]) / sizeof(static_buf_vla[0][0][0])); l++)
                    static_buf_vla[j][k][l] = rand() % 100;

        /* Fill write buffer */
        memcpy(wbuf_vla, wbuf_vla_init, sizeof(wbuf_vla));
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++) {
                /* 10% chance of having a NULL sequence */
                if(rand() % 10)
                    wbuf_vla[j][k].len = rand() % 9;
                else {
                    wbuf_vla[j][k].len = 0;
                    wbuf_vla[j][k].p = NULL;
                } /* end if */
            } /* end for */

        /* Print message */
        if(verbose_g) {
            printf("Writing attribute with variable length arrays.  buf =\n");
            print_vla(wbuf_vla[0]);
        } /* end if */

        /* Write data */
        if(H5Awrite(attr_vla, type_vla, wbuf_vla) < 0)
            ERROR;

        /* Read data */
        memcpy(rbuf_vla, rbuf_vla_init, sizeof(rbuf_vla));
        if(H5Aread(attr_vla, type_vla, rbuf_vla) < 0)
            ERROR;

        /* Print message */
        if(verbose_g) {
            printf("Read attribute with variable length arrays.  buf =\n");
            print_vla(rbuf_vla[0]);
        } /* end if */

        /* Check buffer */
        if(check_vla(wbuf_vla[0], rbuf_vla[0]) < 0) {
            (void)H5Dvlen_reclaim(type_vla, space, H5P_DEFAULT, rbuf_vla);
            ERROR;
        } /* end if */

        /* Reclaim read buffer */
        if(H5Dvlen_reclaim(type_vla, space, H5P_DEFAULT, rbuf_vla) < 0)
            ERROR;

        if(verbose_g)
            printf("\n");

        /*
         * Variable length string attribute test
         */
        /* Fill write data buffer */
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++)
                for(l = 0; l < (sizeof(static_buf_vls[0][0]) / sizeof(static_buf_vls[0][0][0])); l++)
                    static_buf_vls[j][k][l] = 'a' + rand() % 26;

        /* Fill write buffer */
        memcpy(wbuf_vls, wbuf_vls_init, sizeof(wbuf_vls));
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++) {
                /* 10% chance of having a NULL sequence */
                if(rand() % 10)
                    static_buf_vls[j][k][rand() % 8] = '\0';
                else
                    wbuf_vls[j][k] = NULL;
            } /* end for */

        /* Print message */
        if(verbose_g) {
            printf("Writing attribute with variable length strings.  buf =\n");
            print_vls(wbuf_vls[0]);
        } /* end if */

        /* Write data */
        if(H5Awrite(attr_vls, type_vls, wbuf_vls) < 0)
            ERROR;

        /* Read data */
        memcpy(rbuf_vls, rbuf_vls_init, sizeof(rbuf_vls));
        if(H5Aread(attr_vls, type_vls, rbuf_vls) < 0)
            ERROR;

        /* Print message */
        if(verbose_g) {
            printf("Read attribute with variable length strings.  buf =\n");
            print_vls(rbuf_vls[0]);
        } /* end if */

        /* Check buffer */
        if(check_vls(wbuf_vls[0], rbuf_vls[0]) < 0) {
            (void)H5Dvlen_reclaim(type_vls, space, H5P_DEFAULT, rbuf_vls);
            ERROR;
        } /* end if */

        /* Reclaim read buffer */
        if(H5Dvlen_reclaim(type_vls, space, H5P_DEFAULT, rbuf_vls) < 0)
            ERROR;

        if(verbose_g)
            printf("\n");

        /*
         * Variable length array dataset test
         */
        /* Fill write data buffer */
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++)
                for(l = 0; l < (sizeof(static_buf_vla[0][0]) / sizeof(static_buf_vla[0][0][0])); l++)
                    static_buf_vla[j][k][l] = rand() % 100;

        /* Fill write buffer */
        memcpy(wbuf_vla, wbuf_vla_init, sizeof(wbuf_vla));
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++) {
                /* 10% chance of having a NULL sequence */
                if(rand() % 10)
                    wbuf_vla[j][k].len = rand() % 9;
                else {
                    wbuf_vla[j][k].len = 0;
                    wbuf_vla[j][k].p = NULL;
                } /* end if */
            } /* end for */

        /* Print message */
        if(verbose_g) {
            printf("Writing dataset with variable length arrays.  buf =\n");
            print_vla(wbuf_vla[0]);
        } /* end if */

        /* Write data */
        if(H5Dwrite(dset_vla, type_vla, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf_vla) < 0)
            ERROR;

        /* Read data */
        memcpy(rbuf_vla, rbuf_vla_init, sizeof(rbuf_vla));
        if(H5Dread(dset_vla, type_vla, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf_vla) < 0)
            ERROR;

        /* Print message */
        if(verbose_g) {
            printf("Read dataset with variable length arrays.  buf =\n");
            print_vla(rbuf_vla[0]);
        } /* end if */

        /* Check buffer */
        if(check_vla(wbuf_vla[0], rbuf_vla[0]) < 0) {
            (void)H5Dvlen_reclaim(type_vla, space, H5P_DEFAULT, rbuf_vla);
            ERROR;
        } /* end if */

        /* Reclaim read buffer */
        if(H5Dvlen_reclaim(type_vla, space, H5P_DEFAULT, rbuf_vla) < 0)
            ERROR;

        if(verbose_g)
            printf("\n");

        /*
         * Variable length string dataset test
         */
        /* Fill write data buffer */
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++)
                for(l = 0; l < (sizeof(static_buf_vls[0][0]) / sizeof(static_buf_vls[0][0][0])); l++)
                    static_buf_vls[j][k][l] = 'a' + rand() % 26;

        /* Fill write buffer */
        memcpy(wbuf_vls, wbuf_vls_init, sizeof(wbuf_vls));
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++) {
                /* 10% chance of having a NULL sequence */
                if(rand() % 10)
                    static_buf_vls[j][k][rand() % 8] = '\0';
                else
                    wbuf_vls[j][k] = NULL;
            } /* end for */

        /* Print message */
        if(verbose_g) {
            printf("Writing dataset with variable length strings.  buf =\n");
            print_vls(wbuf_vls[0]);
        } /* end if */

        /* Write data */
        if(H5Dwrite(dset_vls, type_vls, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf_vls) < 0)
            ERROR;

        /* Read data */
        memcpy(rbuf_vls, rbuf_vls_init, sizeof(rbuf_vls));
        if(H5Dread(dset_vls, type_vls, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf_vls) < 0)
            ERROR;

        /* Print message */
        if(verbose_g) {
            printf("Read dataset with variable length strings.  buf =\n");
            print_vls(rbuf_vls[0]);
        } /* end if */

        /* Check buffer */
        if(check_vls(wbuf_vls[0], rbuf_vls[0]) < 0) {
            (void)H5Dvlen_reclaim(type_vls, space, H5P_DEFAULT, rbuf_vls);
            ERROR;
        } /* end if */

        /* Reclaim read buffer */
        if(H5Dvlen_reclaim(type_vls, space, H5P_DEFAULT, rbuf_vls) < 0)
            ERROR;

        if(verbose_g)
            printf("\n");

        /*
         * Variable length array dataset test with selections
         */
        /* Fill write data buffer */
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++)
                for(l = 0; l < (sizeof(static_buf_vla[0][0]) / sizeof(static_buf_vla[0][0][0])); l++)
                    static_buf_vla[j][k][l] = rand() % 100;

        /* Fill write buffer */
        memcpy(wbuf_vla, wbuf_vla_init, sizeof(wbuf_vla));
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++) {
                /* 10% chance of having a NULL sequence */
                if(rand() % 10)
                    wbuf_vla[j][k].len = rand() % 9;
                else {
                    wbuf_vla[j][k].len = 0;
                    wbuf_vla[j][k].p = NULL;
                } /* end if */
            } /* end for */

        /* Print message */
        if(verbose_g) {
            printf("Writing dataset with variable length arrays.  buf =\n");
            print_vla(wbuf_vla[0]);
        } /* end if */

        /* Write data to whole dataset */
        if(H5Dwrite(dset_vla, type_vla, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf_vla) < 0)
            ERROR;

        /* Update fbuf */
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++)
                UPDATE_FBUF_VLA(fbuf_vla[j][k], wbuf_vla[j][k]);

        /* Read data from non-contiguous to contiguous */
        memcpy(rbuf_vla, rbuf_vla_init, sizeof(rbuf_vla));
        memcpy(ebuf_vla, rbuf_vla_init, sizeof(rbuf_vla));
        if(H5Dread(dset_vla, type_vla, space_contig, space_ncontig, H5P_DEFAULT, rbuf_vla) < 0)
            ERROR;

        /* Print message */
        if(verbose_g) {
            printf("Read dataset with variable length arrays with selection.  buf =\n");
            print_vla(rbuf_vla[0]);
        } /* end if */

        /* Update ebuf */
        ebuf_vla[1][0] = fbuf_vla[0][1];
        ebuf_vla[1][1] = fbuf_vla[1][1];
        ebuf_vla[2][0] = fbuf_vla[2][1];
        ebuf_vla[2][1] = fbuf_vla[3][1];

        /* Check buffer */
        if(check_vla(ebuf_vla[0], rbuf_vla[0]) < 0) {
            (void)H5Dvlen_reclaim(type_vla, space_contig, H5P_DEFAULT, rbuf_vla);
            ERROR;
        } /* end if */

        /* Reclaim read buffer */
        if(H5Dvlen_reclaim(type_vla, space_contig, H5P_DEFAULT, rbuf_vla) < 0)
            ERROR;

        if(verbose_g)
            printf("\n");

        /* Fill write data buffer */
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++)
                for(l = 0; l < (sizeof(static_buf_vla[0][0]) / sizeof(static_buf_vla[0][0][0])); l++)
                    static_buf_vla[j][k][l] = rand() % 100;

        /* Fill write buffer */
        memcpy(wbuf_vla, wbuf_vla_init, sizeof(wbuf_vla));
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++) {
                /* 10% chance of having a NULL sequence */
                if(rand() % 10)
                    wbuf_vla[j][k].len = rand() % 9;
                else {
                    wbuf_vla[j][k].len = 0;
                    wbuf_vla[j][k].p = NULL;
                } /* end if */
            } /* end for */

        /* Print message */
        if(verbose_g) {
            printf("Writing dataset with variable length arrays with selection.  buf =\n");
            print_vla(wbuf_vla[0]);
        } /* end if */

        /* Write data to dataset from non-contiguous to contiguous */
        if(H5Dwrite(dset_vla, type_vla, space_ncontig, space_contig, H5P_DEFAULT, wbuf_vla) < 0)
            ERROR;

        /* Update fbuf */
        UPDATE_FBUF_VLA(fbuf_vla[1][0], wbuf_vla[0][1]);
        UPDATE_FBUF_VLA(fbuf_vla[1][1], wbuf_vla[1][1]);
        UPDATE_FBUF_VLA(fbuf_vla[2][0], wbuf_vla[2][1]);
        UPDATE_FBUF_VLA(fbuf_vla[2][1], wbuf_vla[3][1]);

        /* Read entire dataset */
        memcpy(rbuf_vla, rbuf_vla_init, sizeof(rbuf_vla));
        memcpy(ebuf_vla, rbuf_vla_init, sizeof(rbuf_vla));
        if(H5Dread(dset_vla, type_vla, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf_vla) < 0)
            ERROR;

        /* Print message */
        if(verbose_g) {
            printf("Read dataset with variable length arrays.  buf =\n");
            print_vla(rbuf_vla[0]);
        } /* end if */

        /* Check buffer */
        if(check_vla(fbuf_vla[0], rbuf_vla[0]) < 0) {
            (void)H5Dvlen_reclaim(type_vla, space, H5P_DEFAULT, rbuf_vla);
            ERROR;
        } /* end if */

        /* Reclaim read buffer */
        if(H5Dvlen_reclaim(type_vla, space, H5P_DEFAULT, rbuf_vla) < 0)
            ERROR;

        if(verbose_g)
            printf("\n");

        /*
         * Variable length string dataset test with selections
         */
        /* Fill write data buffer */
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++)
                for(l = 0; l < (sizeof(static_buf_vls[0][0]) / sizeof(static_buf_vls[0][0][0])); l++)
                    static_buf_vls[j][k][l] = 'a' + rand() % 26;

        /* Fill write buffer */
        memcpy(wbuf_vls, wbuf_vls_init, sizeof(wbuf_vls));
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++) {
                /* 10% chance of having a NULL sequence */
                if(rand() % 10)
                    static_buf_vls[j][k][rand() % 8] = '\0';
                else
                    wbuf_vls[j][k] = NULL;
            } /* end for */

        /* Print message */
        if(verbose_g) {
            printf("Writing dataset with variable length strings.  buf =\n");
            print_vls(wbuf_vls[0]);
        } /* end if */

        /* Write data to whole dataset */
        if(H5Dwrite(dset_vls, type_vls, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf_vls) < 0)
            ERROR;

        /* Update fbuf */
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++)
                UPDATE_FBUF_VLS(fbuf_vls[j][k], wbuf_vls[j][k]);

        /* Read data from contiguous to non-contiguous */
        memcpy(rbuf_vls, rbuf_vls_init, sizeof(rbuf_vls));
        memcpy(ebuf_vls, rbuf_vls_init, sizeof(rbuf_vls));
        if(H5Dread(dset_vls, type_vls, space_ncontig, space_contig, H5P_DEFAULT, rbuf_vls) < 0)
            ERROR;

        /* Print message */
        if(verbose_g) {
            printf("Read dataset with variable length strings with selection.  buf =\n");
            print_vls(rbuf_vls[0]);
        } /* end if */

        /* Update ebuf */
        ebuf_vls[0][1] = fbuf_vls[1][0];
        ebuf_vls[1][1] = fbuf_vls[1][1];
        ebuf_vls[2][1] = fbuf_vls[2][0];
        ebuf_vls[3][1] = fbuf_vls[2][1];

        /* Check buffer */
        if(check_vls(ebuf_vls[0], rbuf_vls[0]) < 0) {
            (void)H5Dvlen_reclaim(type_vls, space_ncontig, H5P_DEFAULT, rbuf_vls);
            ERROR;
        } /* end if */

        /* Reclaim read buffer */
        if(H5Dvlen_reclaim(type_vls, space_ncontig, H5P_DEFAULT, rbuf_vls) < 0)
            ERROR;

        if(verbose_g)
            printf("\n");

        /* Fill write data buffer */
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++)
                for(l = 0; l < (sizeof(static_buf_vls[0][0]) / sizeof(static_buf_vls[0][0][0])); l++)
                    static_buf_vls[j][k][l] = 'a' + rand() % 26;

        /* Fill write buffer */
        memcpy(wbuf_vls, wbuf_vls_init, sizeof(wbuf_vls));
        for(j = 0; j < dims[0]; j++)
            for(k = 0; k < dims[1]; k++) {
                /* 10% chance of having a NULL sequence */
                if(rand() % 10)
                    static_buf_vls[j][k][rand() % 8] = '\0';
                else
                    wbuf_vls[j][k] = NULL;
            } /* end for */

        /* Print message */
        if(verbose_g) {
            printf("Writing dataset with variable length strings with selection.  buf =\n");
            print_vls(wbuf_vls[0]);
        } /* end if */

        /* Write data to dataset from non-contiguous to contiguous */
        if(H5Dwrite(dset_vls, type_vls, space_contig, space_ncontig, H5P_DEFAULT, wbuf_vls) < 0)
            ERROR;

        /* Update fbuf */
        UPDATE_FBUF_VLS(fbuf_vls[0][1], wbuf_vls[1][0]);
        UPDATE_FBUF_VLS(fbuf_vls[1][1], wbuf_vls[1][1]);
        UPDATE_FBUF_VLS(fbuf_vls[2][1], wbuf_vls[2][0]);
        UPDATE_FBUF_VLS(fbuf_vls[3][1], wbuf_vls[2][1]);

        /* Read entire dataset */
        memcpy(rbuf_vls, rbuf_vls_init, sizeof(rbuf_vls));
        memcpy(ebuf_vls, rbuf_vls_init, sizeof(rbuf_vls));
        if(H5Dread(dset_vls, type_vls, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf_vls) < 0)
            ERROR;

        /* Print message */
        if(verbose_g) {
            printf("Read dataset with variable length strings.  buf =\n");
            print_vls(rbuf_vls[0]);
        } /* end if */

        /* Check buffer */
        if(check_vls(fbuf_vls[0], rbuf_vls[0]) < 0) {
            (void)H5Dvlen_reclaim(type_vls, space, H5P_DEFAULT, rbuf_vls);
            ERROR;
        } /* end if */

        /* Reclaim read buffer */
        if(H5Dvlen_reclaim(type_vls, space, H5P_DEFAULT, rbuf_vls) < 0)
            ERROR;

        if(verbose_g)
            printf("\n");
    } /* end for */

    /*
     * Close
     */
    if(H5Aclose(attr_vla) < 0)
        ERROR;
    if(H5Aclose(attr_vls) < 0)
        ERROR;
    if(H5Dclose(dset_vla) < 0)
        ERROR;
    if(H5Dclose(dset_vls) < 0)
        ERROR;
    if(H5Fclose(file) < 0)
        ERROR;
    if(H5Tclose(type_vla) < 0)
        ERROR;
    if(H5Tclose(type_vls) < 0)
        ERROR;
    if(H5Sclose(space) < 0)
        ERROR;
    if(H5Sclose(space_contig) < 0)
        ERROR;
    if(H5Sclose(space_ncontig) < 0)
        ERROR;
    if(H5Pclose(fapl) < 0)
        ERROR;

    for(i = 0; i < dims[0]; i++)
        for(j = 0; j < dims[1]; j++) {
            if(fbuf_vla[i][j].p)
                free(fbuf_vla[i][j].p);
            if(fbuf_vls[i][j])
                free(fbuf_vls[i][j]);
        } /* end for */

    printf("Success\n");

    (void)MPI_Finalize();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Aclose(attr_vla);
        H5Aclose(attr_vls);
        H5Dclose(dset_vla);
        H5Dclose(dset_vls);
        H5Fclose(file);
        H5Tclose(type_vla);
        H5Tclose(type_vls);
        H5Sclose(space);
        H5Sclose(space_contig);
        H5Sclose(space_ncontig);
        H5Pclose(fapl);
    } H5E_END_TRY;

    for(i = 0; i < dims[0]; i++)
        for(j = 0; j < dims[1]; j++) {
            if(fbuf_vla[i][j].p)
                free(fbuf_vla[i][j].p);
            if(fbuf_vls[i][j])
                free(fbuf_vls[i][j]);
        } /* end for */

    (void)MPI_Finalize();
    return 1;
}

