#include "h5dsm_example.h"
#include <time.h>

#define FILE_NAME "ttconv.h5"

typedef struct {
    int a;
    char b;
    double c;
} type_all;

typedef struct {
    long long a;
    char b;
    float c;
} type_convall;

hbool_t verbose_g = 1;

int main(int argc, char *argv[]) {
    uuid_t pool_uuid;
    char *pool_grp = NULL;
    hid_t file = -1, dset = -1, dset_a = -1, dset_b = -1, dset_c = -1, dset2 = -1, attr = -1, attr_a = -1, attr_b = -1 , attr_c = -1, space = -1, space2 = -1, space2_contig = -1, fapl = -1;
    hid_t file_type = -1, file_type_a = -1, file_type_b = -1, file_type_c = -1;
    hid_t mem_type = -1, mem_type_conv = -1, mem_type_a = -1, mem_type_b = -1, mem_type_c = -1;
    hsize_t dims[2] = {4, 2};
    hsize_t start[2], count[2];
    type_all buf[4];
    type_all file_buf[4];
    type_all file_buf2[4][2];
    type_convall buf_conv[4];
    type_convall buf2[4][2];
    const type_all buf_init[4] = {{-1, 'a', (double)-1.}, {-2, 'b', (double)-2.}, {-3, 'c', (double)-3.}, {-4, 'd', (double)-4.}};
    const type_convall buf_conv_init[4] = {{(long long)-5, 'e', (float)-5.}, {(long long)-6, 'f', (float)-6.}, {(long long)-7, 'g', (float)-7.}, {(long long)-8, 'h', (float)-8.}};
    const type_convall buf2_init[4][2] = {{{(long long)-1, 'a', (float)-1.}, {(long long)-2, 'b', (float)-2.}}, {{(long long)-3, 'c', (float)-3.}, {(long long)-4, 'd', (float)-4.}}, {{(long long)-5, 'e', (float)-5.}, {(long long)-6, 'f', (float)-6.}}, {{(long long)-7, 'g', (float)-7.}, {(long long)-8, 'h', (float)-8.}}};
    int i, j, i2;

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
    if((space = H5Screate_simple(1, dims, NULL)) < 0)
        ERROR;

    /* Set up 2-D dataspace */
    if((space2 = H5Screate_simple(2, dims, NULL)) < 0)
        ERROR;

    /* Set up second 2-D dataspace */
    if((space2_contig = H5Screate_simple(2, dims, NULL)) < 0)
        ERROR;

    /*
     * Set up types
     */
    /* Set up file "all" type */
    if((file_type = H5Tcreate(H5T_COMPOUND, sizeof(int) + 1 + sizeof(double))) < 0)
        ERROR;
    if(H5Tinsert(file_type, "a", 0, H5T_NATIVE_INT) < 0)
        ERROR;
    if(H5Tinsert(file_type, "b", sizeof(int), H5T_NATIVE_CHAR) < 0)
        ERROR;
    if(H5Tinsert(file_type, "c", sizeof(int) + 1, H5T_NATIVE_DOUBLE) < 0)
        ERROR;

    /* Set up file "a" type" */
    if((file_type_a = H5Tcreate(H5T_COMPOUND, sizeof(int))) < 0)
        ERROR;
    if(H5Tinsert(file_type_a, "a", 0, H5T_NATIVE_INT) < 0)
        ERROR;

    /* Set up file "b" type" */
    if((file_type_b = H5Tcreate(H5T_COMPOUND, 1)) < 0)
        ERROR;
    if(H5Tinsert(file_type_b, "b", 0, H5T_NATIVE_CHAR) < 0)
        ERROR;

    /* Set up file "c" type" */
    if((file_type_c = H5Tcreate(H5T_COMPOUND, sizeof(double))) < 0)
        ERROR;
    if(H5Tinsert(file_type_c, "c", 0, H5T_NATIVE_DOUBLE) < 0)
        ERROR;

    /* Set up memory "all" type */
    if((mem_type = H5Tcreate(H5T_COMPOUND, sizeof(type_all))) < 0)
        ERROR;
    if(H5Tinsert(mem_type, "a", HOFFSET(type_all, a), H5T_NATIVE_INT) < 0)
        ERROR;
    if(H5Tinsert(mem_type, "b", HOFFSET(type_all, b), H5T_NATIVE_CHAR) < 0)
        ERROR;
    if(H5Tinsert(mem_type, "c", HOFFSET(type_all, c), H5T_NATIVE_DOUBLE) < 0)
        ERROR;

    /* Set up memory "convall" type */
    if((mem_type_conv = H5Tcreate(H5T_COMPOUND, sizeof(type_convall))) < 0)
        ERROR;
    if(H5Tinsert(mem_type_conv, "a", HOFFSET(type_convall, a), H5T_NATIVE_LLONG) < 0)
        ERROR;
    if(H5Tinsert(mem_type_conv, "b", HOFFSET(type_convall, b), H5T_NATIVE_CHAR) < 0)
        ERROR;
    if(H5Tinsert(mem_type_conv, "c", HOFFSET(type_convall, c), H5T_NATIVE_FLOAT) < 0)
        ERROR;

    /* Set up memory "a" type" */
    if((mem_type_a = H5Tcreate(H5T_COMPOUND, sizeof(type_convall))) < 0)
        ERROR;
    if(H5Tinsert(mem_type_a, "a", HOFFSET(type_convall, a), H5T_NATIVE_LLONG) < 0)
        ERROR;

    /* Set up memory "b" type" */
    if((mem_type_b = H5Tcreate(H5T_COMPOUND, sizeof(type_convall))) < 0)
        ERROR;
    if(H5Tinsert(mem_type_b, "b", HOFFSET(type_convall, b), H5T_NATIVE_CHAR) < 0)
        ERROR;

    /* Set up memory "c" type" */
    if((mem_type_c = H5Tcreate(H5T_COMPOUND, sizeof(type_convall))) < 0)
        ERROR;
    if(H5Tinsert(mem_type_c, "c", HOFFSET(type_convall, c), H5T_NATIVE_FLOAT) < 0)
        ERROR;

    /*
     * Create objects
     */
    /* Create dataset */
    if((dset = H5Dcreate2(file, "dset", file_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /* Create dataset a */
    if((dset_a = H5Dcreate2(file, "dset_a", file_type_a, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /* Create dataset b */
    if((dset_b = H5Dcreate2(file, "dset_b", file_type_b, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /* Create dataset c */
    if((dset_c = H5Dcreate2(file, "dset_c", file_type_c, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /* Create 2-D dataset */
    if((dset2 = H5Dcreate2(file, "dset2", file_type, space2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /* Create attribute */
    if((attr = H5Acreate2(dset, "attr", file_type, space, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /* Create attribute a */
    if((attr_a = H5Acreate2(dset_a, "attr_a", file_type_a, space, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /* Create attribute b */
    if((attr_b = H5Acreate2(dset_b, "attr_b", file_type_b, space, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /* Create attribute c */
    if((attr_c = H5Acreate2(dset_c, "attr_c", file_type_c, space, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;

    /*
     * Create selections
     */
    /* Create non-contiguous 2-D selection */
    start[0] = 0;
    start[1] = 1;
    count[0] = 4;
    count[1] = 1;
    if(H5Sselect_hyperslab(space2, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        ERROR;

    /* Create contiguous 2-D selection */
    start[0] = 1;
    start[1] = 0;
    count[0] = 2;
    count[1] = 2;
    if(H5Sselect_hyperslab(space2_contig, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        ERROR;

    /*
     * Test attribute
     */
    /*
     * Full write/read, no member conversion
     */
    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf[i].a = rand() % 10;
        buf[i].b = 'A' + (char)(rand() % 26);
        buf[i].c = (double)(rand() % 100) / (double)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing attribute with no member conversion\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", buf[i].a, buf[i].b, buf[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Awrite(attr, mem_type, buf) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++) {
        file_buf[i].a = buf[i].a;
        file_buf[i].b = buf[i].b;
        file_buf[i].c = buf[i].c;
    } /* end for */

    /* Read data */
    memcpy(buf, buf_init, sizeof(buf));
    if(H5Aread(attr, mem_type, buf) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read attribute with no member conversion\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, file_buf[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", buf[i].a, buf[i].b, buf[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(file_buf[i].a != buf[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - buf[i].c) > (double)0.01)
                || ((file_buf[i].c - buf[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /*
     * Full write/read, with member conversion
     */
    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf_conv[i].a = (long long)(rand() % 10);
        buf_conv[i].b = 'A' + (char)(rand() % 26);
        buf_conv[i].c = (float)(rand() % 100) / (float)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing attribute with member conversion\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Awrite(attr, mem_type_conv, buf_conv) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++) {
        file_buf[i].a = (int)buf_conv[i].a;
        file_buf[i].b = buf_conv[i].b;
        file_buf[i].c = (double)buf_conv[i].c;
    } /* end for */

    /* Read data */
    memcpy(buf, buf_init, sizeof(buf));
    if(H5Aread(attr, mem_type, buf) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read attribute with no member conversion\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, file_buf[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", buf[i].a, buf[i].b, buf[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(file_buf[i].a != buf[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - buf[i].c) > (double)0.01)
                || ((file_buf[i].c - buf[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    /* Read data */
    memcpy(buf_conv, buf_conv_init, sizeof(buf_conv));
    if(H5Aread(attr, mem_type_conv, buf_conv) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read attribute with member conversion\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, file_buf[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if((long long)file_buf[i].a != buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - (double)buf_conv[i].c) > (double)0.01)
                || ((file_buf[i].c - (double)buf_conv[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /*
     * Write by parts
     */
    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf_conv[i].a = (long long)(rand() % 10);
        buf_conv[i].b = 'A' + (char)(rand() % 26);
        buf_conv[i].c = (float)(rand() % 100) / (float)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing attribute compound member \'a\'\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Awrite(attr, mem_type_a, buf_conv) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++)
        file_buf[i].a = (int)buf_conv[i].a;

    /* Read data */
    memcpy(buf, buf_init, sizeof(buf));
    if(H5Aread(attr, mem_type, buf) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read attribute with no member conversion\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, file_buf[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", buf[i].a, buf[i].b, buf[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(file_buf[i].a != buf[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - buf[i].c) > (double)0.01)
                || ((file_buf[i].c - buf[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf_conv[i].a = (long long)(rand() % 10);
        buf_conv[i].b = 'A' + (char)(rand() % 26);
        buf_conv[i].c = (float)(rand() % 100) / (float)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing attribute compound member \'b\'\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Awrite(attr, mem_type_b, buf_conv) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++)
        file_buf[i].b = buf_conv[i].b;

    /* Read data */
    memcpy(buf, buf_init, sizeof(buf));
    if(H5Aread(attr, mem_type, buf) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read attribute with no member conversion\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, file_buf[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", buf[i].a, buf[i].b, buf[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(file_buf[i].a != buf[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - buf[i].c) > (double)0.01)
                || ((file_buf[i].c - buf[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf_conv[i].a = (long long)(rand() % 10);
        buf_conv[i].b = 'A' + (char)(rand() % 26);
        buf_conv[i].c = (float)(rand() % 100) / (float)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing attribute compound member \'c\'\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Awrite(attr, mem_type_c, buf_conv) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++)
        file_buf[i].c = (double)buf_conv[i].c;

    /* Read data */
    memcpy(buf, buf_init, sizeof(buf));
    if(H5Aread(attr, mem_type, buf) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read attribute with no member conversion\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, file_buf[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", buf[i].a, buf[i].b, buf[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(file_buf[i].a != buf[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - buf[i].c) > (double)0.01)
                || ((file_buf[i].c - buf[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /*
     * Read by parts
     */
    /* Read member a */
    memcpy(buf_conv, buf_conv_init, sizeof(buf_conv));
    if(H5Aread(attr, mem_type_a, buf_conv) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read full attribute compound member \'a\'\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, buf_conv_init[i].b, (double)buf_conv_init[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if((long long)file_buf[i].a != buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(buf_conv_init[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((buf_conv_init[i].c - buf_conv[i].c) > 0.01f)
                || ((buf_conv_init[i].c - buf_conv[i].c) < -0.01f))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /* Read member b */
    memcpy(buf_conv, buf_conv_init, sizeof(buf_conv));
    if(H5Aread(attr, mem_type_b, buf_conv) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read full attribute compound member \'b\'\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv_init[i].a, file_buf[i].b, (double)buf_conv_init[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(buf_conv_init[i].a != buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((buf_conv_init[i].c - buf_conv[i].c) > 0.01f)
                || ((buf_conv_init[i].c - buf_conv[i].c) < -0.01f))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /* Read member c */
    memcpy(buf_conv, buf_conv_init, sizeof(buf_conv));
    if(H5Aread(attr, mem_type_c, buf_conv) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read full attribute compound member \'c\'\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv_init[i].a, buf_conv_init[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(buf_conv_init[i].a != buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(buf_conv_init[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - (double)buf_conv[i].c) > (double)0.01)
                || ((file_buf[i].c - (double)buf_conv[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /*
     * Write/read partial attributes
     */
    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf_conv[i].a = (long long)(rand() % 10);
        buf_conv[i].b = 'A' + (char)(rand() % 26);
        buf_conv[i].c = (float)(rand() % 100) / (float)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing full memory type to attribute containing only member \'a\'\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Awrite(attr_a, mem_type_conv, buf_conv) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++)
        file_buf[i].a = (int)buf_conv[i].a;

    /* Read data */
    memcpy(buf_conv, buf_conv_init, sizeof(buf_conv));
    if(H5Aread(attr_a, mem_type_conv, buf_conv) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read attribute containing only member \'a\'\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, buf_conv_init[i].b, (double)buf_conv_init[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if((long long)file_buf[i].a != buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(buf_conv_init[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((buf_conv_init[i].c - buf_conv[i].c) > 0.01f)
                || ((buf_conv_init[i].c - buf_conv[i].c) < -0.01f))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf_conv[i].a = (long long)(rand() % 10);
        buf_conv[i].b = 'A' + (char)(rand() % 26);
        buf_conv[i].c = (float)(rand() % 100) / (float)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing full memory type to attribute containing only member \'b\'\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Awrite(attr_b, mem_type_conv, buf_conv) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++)
        file_buf[i].b = buf_conv[i].b;

    /* Read data */
    memcpy(buf_conv, buf_conv_init, sizeof(buf_conv));
    if(H5Aread(attr_b, mem_type_conv, buf_conv) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read attribute containing only member \'b\'\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv_init[i].a, file_buf[i].b, (double)buf_conv_init[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(buf_conv_init[i].a != buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((buf_conv_init[i].c - buf_conv[i].c) > 0.01f)
                || ((buf_conv_init[i].c - buf_conv[i].c) < -0.01f))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf_conv[i].a = (long long)(rand() % 10);
        buf_conv[i].b = 'A' + (char)(rand() % 26);
        buf_conv[i].c = (float)(rand() % 100) / (float)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing full memory type to attribute containing only member \'c\'\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Awrite(attr_c, mem_type_conv, buf_conv) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++)
        file_buf[i].c = (double)buf_conv[i].c;

    /* Read data */
    memcpy(buf_conv, buf_conv_init, sizeof(buf_conv));
    if(H5Aread(attr_c, mem_type_conv, buf_conv) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read attribute containing only member \'c\'\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv_init[i].a, buf_conv_init[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(buf_conv_init[i].a != buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(buf_conv_init[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - (double)buf_conv[i].c) > (double)0.01)
                || ((file_buf[i].c - (double)buf_conv[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /*
     * Test dataset
     */
    /*
     * Full write/read, no member conversion
     */
    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf[i].a = rand() % 10;
        buf[i].b = 'A' + (char)(rand() % 26);
        buf[i].c = (double)(rand() % 100) / (double)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing dataset with no member conversion\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", buf[i].a, buf[i].b, buf[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Dwrite(dset, mem_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++) {
        file_buf[i].a = buf[i].a;
        file_buf[i].b = buf[i].b;
        file_buf[i].c = buf[i].c;
    } /* end for */

    /* Read data */
    memcpy(buf, buf_init, sizeof(buf));
    if(H5Dread(dset, mem_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read dataset with no member conversion\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, file_buf[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", buf[i].a, buf[i].b, buf[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(file_buf[i].a != buf[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - buf[i].c) > (double)0.01)
                || ((file_buf[i].c - buf[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /*
     * Full write/read, with member conversion
     */
    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf_conv[i].a = (long long)(rand() % 10);
        buf_conv[i].b = 'A' + (char)(rand() % 26);
        buf_conv[i].c = (float)(rand() % 100) / (float)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing dataset with member conversion\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Dwrite(dset, mem_type_conv, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_conv) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++) {
        file_buf[i].a = (int)buf_conv[i].a;
        file_buf[i].b = buf_conv[i].b;
        file_buf[i].c = (double)buf_conv[i].c;
    } /* end for */

    /* Read data */
    memcpy(buf, buf_init, sizeof(buf));
    if(H5Dread(dset, mem_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read dataset with no member conversion\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, file_buf[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", buf[i].a, buf[i].b, buf[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(file_buf[i].a != buf[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - buf[i].c) > (double)0.01)
                || ((file_buf[i].c - buf[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    /* Read data */
    memcpy(buf_conv, buf_conv_init, sizeof(buf_conv));
    if(H5Dread(dset, mem_type_conv, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_conv) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read dataset with member conversion\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, file_buf[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if((long long)file_buf[i].a != buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - (double)buf_conv[i].c) > (double)0.01)
                || ((file_buf[i].c - (double)buf_conv[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /*
     * Write by parts
     */
    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf_conv[i].a = (long long)(rand() % 10);
        buf_conv[i].b = 'A' + (char)(rand() % 26);
        buf_conv[i].c = (float)(rand() % 100) / (float)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing dataset compound member \'a\'\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Dwrite(dset, mem_type_a, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_conv) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++)
        file_buf[i].a = (int)buf_conv[i].a;

    /* Read data */
    memcpy(buf, buf_init, sizeof(buf));
    if(H5Dread(dset, mem_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read dataset with no member conversion\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, file_buf[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", buf[i].a, buf[i].b, buf[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(file_buf[i].a != buf[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - buf[i].c) > (double)0.01)
                || ((file_buf[i].c - buf[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf_conv[i].a = (long long)(rand() % 10);
        buf_conv[i].b = 'A' + (char)(rand() % 26);
        buf_conv[i].c = (float)(rand() % 100) / (float)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing dataset compound member \'b\'\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Dwrite(dset, mem_type_b, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_conv) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++)
        file_buf[i].b = buf_conv[i].b;

    /* Read data */
    memcpy(buf, buf_init, sizeof(buf));
    if(H5Dread(dset, mem_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read dataset with no member conversion\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, file_buf[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", buf[i].a, buf[i].b, buf[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(file_buf[i].a != buf[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - buf[i].c) > (double)0.01)
                || ((file_buf[i].c - buf[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf_conv[i].a = (long long)(rand() % 10);
        buf_conv[i].b = 'A' + (char)(rand() % 26);
        buf_conv[i].c = (float)(rand() % 100) / (float)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing dataset compound member \'c\'\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Dwrite(dset, mem_type_c, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_conv) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++)
        file_buf[i].c = (double)buf_conv[i].c;

    /* Read data */
    memcpy(buf, buf_init, sizeof(buf));
    if(H5Dread(dset, mem_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read dataset with no member conversion\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, file_buf[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", buf[i].a, buf[i].b, buf[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(file_buf[i].a != buf[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - buf[i].c) > (double)0.01)
                || ((file_buf[i].c - buf[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /*
     * Read by parts
     */
    /* Read member a */
    memcpy(buf_conv, buf_conv_init, sizeof(buf_conv));
    if(H5Dread(dset, mem_type_a, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_conv) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read full dataset compound member \'a\'\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, buf_conv_init[i].b, (double)buf_conv_init[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if((long long)file_buf[i].a != buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(buf_conv_init[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((buf_conv_init[i].c - buf_conv[i].c) > 0.01f)
                || ((buf_conv_init[i].c - buf_conv[i].c) < -0.01f))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /* Read member b */
    memcpy(buf_conv, buf_conv_init, sizeof(buf_conv));
    if(H5Dread(dset, mem_type_b, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_conv) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read full dataset compound member \'b\'\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv_init[i].a, file_buf[i].b, (double)buf_conv_init[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(buf_conv_init[i].a != buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((buf_conv_init[i].c - buf_conv[i].c) > 0.01f)
                || ((buf_conv_init[i].c - buf_conv[i].c) < -0.01f))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /* Read member c */
    memcpy(buf_conv, buf_conv_init, sizeof(buf_conv));
    if(H5Dread(dset, mem_type_c, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_conv) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read full dataset compound member \'c\'\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv_init[i].a, buf_conv_init[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(buf_conv_init[i].a != buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(buf_conv_init[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - (double)buf_conv[i].c) > (double)0.01)
                || ((file_buf[i].c - (double)buf_conv[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /*
     * Write/read partial datasets
     */
    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf_conv[i].a = (long long)(rand() % 10);
        buf_conv[i].b = 'A' + (char)(rand() % 26);
        buf_conv[i].c = (float)(rand() % 100) / (float)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing full memory type to dataset containing only member \'a\'\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Dwrite(dset_a, mem_type_conv, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_conv) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++)
        file_buf[i].a = (int)buf_conv[i].a;

    /* Read data */
    memcpy(buf_conv, buf_conv_init, sizeof(buf_conv));
    if(H5Dread(dset_a, mem_type_conv, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_conv) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read dataset containing only member \'a\'\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%d, %c, %.1f}", file_buf[i].a, buf_conv_init[i].b, (double)buf_conv_init[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if((long long)file_buf[i].a != buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(buf_conv_init[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((buf_conv_init[i].c - buf_conv[i].c) > 0.01f)
                || ((buf_conv_init[i].c - buf_conv[i].c) < -0.01f))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf_conv[i].a = (long long)(rand() % 10);
        buf_conv[i].b = 'A' + (char)(rand() % 26);
        buf_conv[i].c = (float)(rand() % 100) / (float)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing full memory type to dataset containing only member \'b\'\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Dwrite(dset_b, mem_type_conv, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_conv) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++)
        file_buf[i].b = buf_conv[i].b;

    /* Read data */
    memcpy(buf_conv, buf_conv_init, sizeof(buf_conv));
    if(H5Dread(dset_b, mem_type_conv, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_conv) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read dataset containing only member \'b\'\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv_init[i].a, file_buf[i].b, (double)buf_conv_init[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(buf_conv_init[i].a != buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((buf_conv_init[i].c - buf_conv[i].c) > 0.01f)
                || ((buf_conv_init[i].c - buf_conv[i].c) < -0.01f))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /* Fill buffer */
    for(i = 0; i < dims[0]; i++) {
        buf_conv[i].a = (long long)(rand() % 10);
        buf_conv[i].b = 'A' + (char)(rand() % 26);
        buf_conv[i].c = (float)(rand() % 100) / (float)10;
    } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing full memory type to dataset containing only member \'c\'\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Dwrite(dset_c, mem_type_conv, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_conv) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++)
        file_buf[i].c = (double)buf_conv[i].c;

    /* Read data */
    memcpy(buf_conv, buf_conv_init, sizeof(buf_conv));
    if(H5Dread(dset_c, mem_type_conv, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_conv) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read dataset containing only member \'c\'\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv_init[i].a, buf_conv_init[i].b, file_buf[i].c);
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf(", ");
            printf("{%lld, %c, %.1f}", buf_conv[i].a, buf_conv[i].b, (double)buf_conv[i].c);
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++) {
        if(buf_conv_init[i].a != buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(buf_conv_init[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - (double)buf_conv[i].c) > (double)0.01)
                || ((file_buf[i].c - (double)buf_conv[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

    if(verbose_g)
        printf("\n");

    /*
     * Test dataset with selections
     */
    /*
     * Full write/read, member conversion
     */
    /* Fill buffer */
    for(i = 0; i < dims[0]; i++)
        for(j = 0; j < dims[1]; j++) {
            buf2[i][j].a = rand() % 10;
            buf2[i][j].b = 'A' + (char)(rand() % 26);
            buf2[i][j].c = (double)(rand() % 100) / (double)10;
        } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing full 2-D dataset with member conversion\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf("\n       ");
            for(j = 0; j < dims[1]; j++) {
                if(j > 0)
                    printf(", ");
                printf("{%lld, %c, %.1f}", buf2[i][j].a, buf2[i][j].b, (double)buf2[i][j].c);
            } /* end for */
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Dwrite(dset2, mem_type_conv, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2) < 0)
        ERROR;

    /* Update file_buf */
    for(i = 0; i < dims[0]; i++)
        for(j = 0; j < dims[1]; j++) {
            file_buf2[i][j].a = (int)buf2[i][j].a;
            file_buf2[i][j].b = buf2[i][j].b;
            file_buf2[i][j].c = (double)buf2[i][j].c;
        } /* end for */

    /* Read data */
    memcpy(buf2, buf2_init, sizeof(buf2));
    if(H5Dread(dset2, mem_type_conv, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read full 2-D dataset with member conversion\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf("\n       ");
            for(j = 0; j < dims[1]; j++) {
                if(j > 0)
                    printf(", ");
                printf("{%d, %c, %.1f}", file_buf2[i][j].a, file_buf2[i][j].b, file_buf2[i][j].c);
            } /* end for */
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf("\n       ");
            for(j = 0; j < dims[1]; j++) {
                if(j > 0)
                    printf(", ");
                printf("{%lld, %c, %.1f}", buf2[i][j].a, buf2[i][j].b, (double)buf2[i][j].c);
            } /* end for */
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++)
        for(j = 0; j < dims[1]; j++) {
            if((long long)file_buf2[i][j].a != buf2[i][j].a)
                PRINTF_ERROR("Member a at location %d does not match", i);
            if(file_buf2[i][j].b != buf2[i][j].b)
                PRINTF_ERROR("Member b at location %d does not match", i);
            if(((file_buf2[i][j].c - (double)buf2[i][j].c) > (double)0.01)
                    || ((file_buf2[i][j].c - (double)buf2[i][j].c) < (double)-0.01))
                PRINTF_ERROR("Member c at location %d does not match", i);
        } /* end for */

    if(verbose_g)
        printf("\n");

    /*! ----------------------------------------------*/
    /*! Temporary hack until overwrites are supported */
    memset(file_buf2, 0, sizeof(file_buf2));
    if(H5Dclose(dset2) < 0)
        ERROR;
    if((dset2 = H5Dcreate2(file, "dset2b", file_type, space2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;
    /*! ----------------------------------------------*/

    /*
     * Partial write with full type/full read to full type 
     */
    /* Fill buffer */
    for(i = 0; i < dims[0]; i++)
        for(j = 0; j < dims[1]; j++) {
            buf2[i][j].a = rand() % 10;
            buf2[i][j].b = 'A' + (char)(rand() % 26);
            buf2[i][j].c = (double)(rand() % 100) / (double)10;
        } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing partial 2-D dataset with member conversion\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf("\n       ");
            for(j = 0; j < dims[1]; j++) {
                if(j > 0)
                    printf(", ");
                printf("{%lld, %c, %.1f}", buf2[i][j].a, buf2[i][j].b, (double)buf2[i][j].c);
            } /* end for */
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Dwrite(dset2, mem_type_conv, space2_contig, space2, H5P_DEFAULT, buf2) < 0)
        ERROR;

    /* Update file_buf */
    i2 = 0;
    for(i = 1; i < 3; i++)
        for(j = 0; j < 2; j++) {
            file_buf2[i2][1].a = (int)buf2[i][j].a;
            file_buf2[i2][1].b = buf2[i][j].b;
            file_buf2[i2][1].c = (double)buf2[i][j].c;
            i2++;
        } /* end for */

    /* Read data */
    memcpy(buf2, buf2_init, sizeof(buf2));
    if(H5Dread(dset2, mem_type_conv, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read full 2-D dataset with member conversion\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf("\n       ");
            for(j = 0; j < dims[1]; j++) {
                if(j > 0)
                    printf(", ");
                printf("{%d, %c, %.1f}", file_buf2[i][j].a, file_buf2[i][j].b, file_buf2[i][j].c);
            } /* end for */
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf("\n       ");
            for(j = 0; j < dims[1]; j++) {
                if(j > 0)
                    printf(", ");
                printf("{%lld, %c, %.1f}", buf2[i][j].a, buf2[i][j].b, (double)buf2[i][j].c);
            } /* end for */
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++)
        /*! ----------------------------------------------*/
        /*! Temporary hack until overwrites are supported */
        {
            j = 1;
        /*for(j = 0; j < dims[1]; j++) {*/
        /*! ----------------------------------------------*/
            if((long long)file_buf2[i][j].a != buf2[i][j].a)
                PRINTF_ERROR("Member a at location %d, %d does not match", i, j);
            if(file_buf2[i][j].b != buf2[i][j].b)
                PRINTF_ERROR("Member b at location %d, %d does not match", i, j);
            if(((file_buf2[i][j].c - (double)buf2[i][j].c) > (double)0.01)
                    || ((file_buf2[i][j].c - (double)buf2[i][j].c) < (double)-0.01))
                PRINTF_ERROR("Member c at location %d, %d does not match", i, j);
        } /* end for */

    if(verbose_g)
        printf("\n");

    /*! ----------------------------------------------*/
    /*! Temporary hack until overwrites are supported */
    memset(file_buf2, 0, sizeof(file_buf2));
    if(H5Dclose(dset2) < 0)
        ERROR;
    if((dset2 = H5Dcreate2(file, "dset2c", file_type, space2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        ERROR;
    /*! ----------------------------------------------*/

    /*
     * Partial write with member a/full read to full type 
     */
    /* Fill buffer */
    for(i = 0; i < dims[0]; i++)
        for(j = 0; j < dims[1]; j++) {
            buf2[i][j].a = rand() % 10;
            buf2[i][j].b = 'A' + (char)(rand() % 26);
            buf2[i][j].c = (double)(rand() % 100) / (double)10;
        } /* end for */

    /* Print message */
    if(verbose_g) {
        printf("Writing partial 2-D dataset with member conversion to member \'a\'\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf("\n       ");
            for(j = 0; j < dims[1]; j++) {
                if(j > 0)
                    printf(", ");
                printf("{%lld, %c, %.1f}", buf2[i][j].a, buf2[i][j].b, (double)buf2[i][j].c);
            } /* end for */
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Write data */
    if(H5Dwrite(dset2, mem_type_a, space2_contig, space2, H5P_DEFAULT, buf2) < 0)
        ERROR;

    /* Update file_buf */
    i2 = 0;
    for(i = 1; i < 3; i++)
        for(j = 0; j < 2; j++) {
            file_buf2[i2][1].a = (int)buf2[i][j].a;
            i2++;
        } /* end for */

    /* Read data */
    memcpy(buf2, buf2_init, sizeof(buf2));
    if(H5Dread(dset2, mem_type_conv, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read full 2-D dataset with member conversion\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf("\n       ");
            for(j = 0; j < dims[1]; j++) {
                if(j > 0)
                    printf(", ");
                printf("{%d, %c, %.1f}", file_buf2[i][j].a, file_buf2[i][j].b, file_buf2[i][j].c);
            } /* end for */
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf("\n       ");
            for(j = 0; j < dims[1]; j++) {
                if(j > 0)
                    printf(", ");
                printf("{%lld, %c, %.1f}", buf2[i][j].a, buf2[i][j].b, (double)buf2[i][j].c);
            } /* end for */
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++)
        /*! ----------------------------------------------*/
        /*! Temporary hack until overwrites are supported */
        {
            j = 1;
        /*for(j = 0; j < dims[1]; j++) {*/
        /*! ----------------------------------------------*/
            if((long long)file_buf2[i][j].a != buf2[i][j].a)
                PRINTF_ERROR("Member a at location %d, %d does not match", i, j);
            if(file_buf2[i][j].b != buf2[i][j].b)
                PRINTF_ERROR("Member b at location %d, %d does not match", i, j);
            if(((file_buf2[i][j].c - (double)buf2[i][j].c) > (double)0.01)
                    || ((file_buf2[i][j].c - (double)buf2[i][j].c) < (double)-0.01))
                PRINTF_ERROR("Member c at location %d, %d does not match", i, j);
        } /* end for */

    if(verbose_g)
        printf("\n");

    /*
     * Partial read to contiguous buffer, member conversion to type "a"
     */
    /* Read data */
    memcpy(buf2, buf2_init, sizeof(buf2));
    if(H5Dread(dset2, mem_type_a, H5S_ALL, space2_contig, H5P_DEFAULT, buf2) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read partial 2-D dataset to contiguous buffer with member conversion to member \'a\'\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf("\n       ");
            for(j = 0; j < dims[1]; j++) {
                if(j > 0)
                    printf(", ");
                printf("{%lld, %c, %.1f}", (i == 1 || i == 2) ? (long long)file_buf2[i][j].a : buf2_init[i][j].a, buf2_init[i][j].b, (double)buf2_init[i][j].c);
            } /* end for */
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf("\n       ");
            for(j = 0; j < dims[1]; j++) {
                if(j > 0)
                    printf(", ");
                printf("{%lld, %c, %.1f}", buf2[i][j].a, buf2[i][j].b, (double)buf2[i][j].c);
            } /* end for */
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++)
        for(j = 0; j < dims[1]; j++) {
            /*! ----------------------------------------------*/
            /*! Temporary hack until overwrites are supported */
            if((j == 1) || (i == 0) || (i == 3))
            /*! ----------------------------------------------*/
            if(((i == 1 || i == 2)
                    ? (long long)file_buf2[i][j].a : buf2_init[i][j].a)
                    != buf2[i][j].a)
                PRINTF_ERROR("Member a at location %d, %d does not match", i, j);
            if(buf2_init[i][j].b != buf2[i][j].b)
                PRINTF_ERROR("Member b at location %d, %d does not match", i, j);
            if(((buf2_init[i][j].c - buf2[i][j].c) > 0.01f)
                    || ((buf2_init[i][j].c - buf2[i][j].c) < -0.01f))
                PRINTF_ERROR("Member c at location %d, %d does not match", i, j);
        } /* end for */

    if(verbose_g)
        printf("\n");

    /*
     * Partial read to non-contiguous buffer, member conversion to type "a"
     */
    /* Read data */
    memcpy(buf2, buf2_init, sizeof(buf2));
    if(H5Dread(dset2, mem_type_a, space2, space2_contig, H5P_DEFAULT, buf2) < 0)
        ERROR;

    /* Print message */
    if(verbose_g) {
        printf("Read partial 2-D dataset to non-contiguous buffer with member conversion to member \'a\'\n");
        printf("exp = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf("\n       ");
            for(j = 0; j < dims[1]; j++) {
                if(j > 0)
                    printf(", ");
                printf("{%lld, %c, %.1f}", (j == 1) ? (long long)file_buf2[i / 2 + 1][i % 2].a : buf2_init[i][j].a, buf2_init[i][j].b, (double)buf2_init[i][j].c);
            } /* end for */
        } /* end for */
        printf("}\n");
        printf("buf = {");
        for(i = 0; i < dims[0]; i++) {
            if(i > 0)
                printf("\n       ");
            for(j = 0; j < dims[1]; j++) {
                if(j > 0)
                    printf(", ");
                printf("{%lld, %c, %.1f}", buf2[i][j].a, buf2[i][j].b, (double)buf2[i][j].c);
            } /* end for */
        } /* end for */
        printf("}\n");
    } /* end if */

    /* Check buffer */
    for(i = 0; i< dims[0]; i++)
        for(j = 0; j < dims[1]; j++) {
            /*! ----------------------------------------------*/
            /*! Temporary hack until overwrites are supported */
            if((j == 0) || (i == 1) || (i == 3))
            /*! ----------------------------------------------*/
            if(((j == 1) ? (long long)file_buf2[i / 2 + 1][i % 2].a
                    : buf2_init[i][j].a)
                    != buf2[i][j].a)
                PRINTF_ERROR("Member a at location %d, %d does not match", i, j);
            if(buf2_init[i][j].b != buf2[i][j].b)
                PRINTF_ERROR("Member b at location %d, %d does not match", i, j);
            if(((buf2_init[i][j].c - buf2[i][j].c) > 0.01f)
                    || ((buf2_init[i][j].c - buf2[i][j].c) < -0.01f))
                PRINTF_ERROR("Member c at location %d, %d does not match", i, j);
        } /* end for */

    if(verbose_g)
        printf("\n");

    /*
     * Close
     */
    if(H5Aclose(attr) < 0)
        ERROR;
    if(H5Aclose(attr_a) < 0)
        ERROR;
    if(H5Aclose(attr_b) < 0)
        ERROR;
    if(H5Aclose(attr_c) < 0)
        ERROR;
    if(H5Dclose(dset) < 0)
        ERROR;
    if(H5Dclose(dset_a) < 0)
        ERROR;
    if(H5Dclose(dset_b) < 0)
        ERROR;
    if(H5Dclose(dset_c) < 0)
        ERROR;
    if(H5Dclose(dset2) < 0)
        ERROR;
    if(H5Fclose(file) < 0)
        ERROR;
    if(H5Tclose(file_type) < 0)
        ERROR;
    if(H5Tclose(file_type_a) < 0)
        ERROR;
    if(H5Tclose(file_type_b) < 0)
        ERROR;
    if(H5Tclose(file_type_c) < 0)
        ERROR;
    if(H5Tclose(mem_type) < 0)
        ERROR;
    if(H5Tclose(mem_type_conv) < 0)
        ERROR;
    if(H5Tclose(mem_type_a) < 0)
        ERROR;
    if(H5Tclose(mem_type_b) < 0)
        ERROR;
    if(H5Tclose(mem_type_c) < 0)
        ERROR;
    if(H5Sclose(space) < 0)
        ERROR;
    if(H5Sclose(space2) < 0)
        ERROR;
    if(H5Sclose(space2_contig) < 0)
        ERROR;
    if(H5Pclose(fapl) < 0)
        ERROR;

    printf("Success\n");

    (void)MPI_Finalize();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Aclose(attr);
        H5Aclose(attr_a);
        H5Aclose(attr_b);
        H5Aclose(attr_c);
        H5Dclose(dset);
        H5Dclose(dset_a);
        H5Dclose(dset_b);
        H5Dclose(dset_c);
        H5Dclose(dset2);
        H5Fclose(file);
        H5Tclose(file_type);
        H5Tclose(file_type_a);
        H5Tclose(file_type_b);
        H5Tclose(file_type_c);
        H5Tclose(mem_type);
        H5Tclose(mem_type_conv);
        H5Tclose(mem_type_a);
        H5Tclose(mem_type_b);
        H5Tclose(mem_type_c);
        H5Sclose(space);
        H5Sclose(space2);
        H5Sclose(space2_contig);
        H5Pclose(fapl);
    } H5E_END_TRY;

    (void)MPI_Finalize();
    return 1;
}

