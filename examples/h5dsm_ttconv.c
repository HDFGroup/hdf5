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
    hid_t file = -1, dset = -1, dset_a = -1, dset_b = -1, dset_c = -1, attr = -1, attr_a = -1, attr_b = -1 , attr_c = -1, space = -1, fapl = -1;
    hid_t file_type = -1, file_type_a = -1, file_type_b = -1, file_type_c = -1;
    hid_t mem_type = -1, mem_type_conv = -1, mem_type_a = -1, mem_type_b = -1, mem_type_c = -1;
    hsize_t dims[1] = {4};
    type_all buf[4];
    type_all file_buf[4];
    type_convall buf_conv[4];
    const type_all buf_init[4] = {{-1, 'a', (double)-1.}, {-2, 'b', (double)-2.}, {-3, 'c', (double)-3.}, {-4, 'd', (double)-4.}};
    const type_convall buf_conv_init[4] = {{(long long)-5, 'e', (float)-5.}, {(long long)-6, 'f', (float)-6.}, {(long long)-7, 'g', (float)-7.}, {(long long)-8, 'h', (float)-8.}};
    int i;

    (void)MPI_Init(&argc, &argv);

    /* Seed random number generator */
    srand(time(NULL));

    if(argc != 2)
        PRINTF_ERROR("argc must be 2\n");

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

    /* Create file */
    if((file = H5Fcreate(FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        ERROR;

    /* Set up dataspace */
    if((space = H5Screate_simple(1, dims, NULL)) < 0)
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
        if(file_buf[i].a != (int)buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(file_buf[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((file_buf[i].c - (double)buf_conv[i].c) > (double)0.01)
                || ((file_buf[i].c - (double)buf_conv[i].c) < (double)-0.01))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

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
        if(file_buf[i].a != (int)buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(buf_conv_init[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((buf_conv_init[i].c - buf_conv[i].c) > 0.01f)
                || ((buf_conv_init[i].c - buf_conv[i].c) < -0.01f))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

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
        if(file_buf[i].a != (int)buf_conv[i].a)
            PRINTF_ERROR("Member a at location %d does not match", i);
        if(buf_conv_init[i].b != buf_conv[i].b)
            PRINTF_ERROR("Member b at location %d does not match", i);
        if(((buf_conv_init[i].c - buf_conv[i].c) > 0.01f)
                || ((buf_conv_init[i].c - buf_conv[i].c) < -0.01f))
            PRINTF_ERROR("Member c at location %d does not match", i);
    } /* end for */

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
    if(H5Pclose(fapl) < 0)
        ERROR;

    if(H5VLdaosm_term() < 0)
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
        H5Pclose(fapl);
        H5VLdaosm_term();
    } H5E_END_TRY;

    (void)MPI_Finalize();
    return 1;
}

