/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, December  9, 1997
 *
 * Purpose:     Tests the dataset interface (H5D)
 */
#include <assert.h>
#include <hdf5.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#ifndef HAVE_FUNCTION
#undef __FUNCTION__
#define __FUNCTION__ ""
#endif
#define AT() printf ("   at %s:%d in %s()...\n",                            \
                     __FILE__, __LINE__, __FUNCTION__);

#define DSET_DEFAULT_NAME       "default"
#define DSET_CHUNKED_NAME       "chunked"
#define DSET_SIMPLE_IO_NAME     "simple_io"
#define DSET_TCONV_NAME         "tconv"

/*-------------------------------------------------------------------------
 * Function:    test_create
 *
 * Purpose:     Attempts to create a dataset.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_create(hid_t file)
{
    hid_t                   dataset, space, create_parms;
    size_t                  dims[2];
    herr_t                  status;
    size_t                  csize[2];

    printf("%-70s", "Testing create/open/close");

    /* Create the data space */
    dims[0] = 256;
    dims[1] = 512;
    space = H5Screate_simple(2, dims, NULL);
    assert(space != FAIL);

    /*
     * Create a dataset using the default dataset creation properties.  We're
     * not sure what they are, so we won't check.
     */
    dataset = H5Dcreate(file, DSET_DEFAULT_NAME, H5T_NATIVE_DOUBLE, space,
                        H5P_DEFAULT);
    if (dataset < 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Cannot create initial dataset.\n");
        }
        goto error;
    }
    /* Close the dataset */
    if (H5Dclose(dataset) < 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Cannot close initial dataset.\n");
        }
        goto error;
    }

    /*
     * Try creating a dataset that already exists.  This should fail since a
     * dataset can only be created once.  Temporarily turn off error
     * reporting.
     */
    H5Eset_auto (NULL, NULL);
    dataset = H5Dcreate(file, DSET_DEFAULT_NAME, H5T_NATIVE_DOUBLE, space,
                        H5P_DEFAULT);
    H5Eset_auto ((herr_t(*)(void*))H5Eprint, stdout);
    if (dataset >= 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Library allowed overwrite of existing dataset.\n");
        }
        goto error;
    }
    
    /*
     * Open the dataset we created above and then close it.  This is how
     * existing datasets are accessed.
     */
    dataset = H5Dopen(file, DSET_DEFAULT_NAME);
    if (dataset < 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Cannot open dataset `%s'.\n", DSET_DEFAULT_NAME);
        }
        goto error;
    }
    if (H5Dclose(dataset) < 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Cannot close dataset.\n");
        }
        goto error;
    }
    
    /*
     * Try opening a non-existent dataset. This should fail since new datasets
     * cannot be created with this function.  Temporarily turn off error
     * reporting.
     */
    H5Eset_auto (NULL, NULL);
    dataset = H5Dopen(file, "does_not_exist");
    H5Eset_auto ((herr_t(*)(void*))H5Eprint, stdout);
    if (dataset >= 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Opened a non-existent dataset.\n");
        }
        goto error;
    }
    /*
     * Create a new dataset that uses chunked storage instead of the default
     * layout.
     */
    create_parms = H5Pcreate(H5P_DATASET_CREATE);
    assert(create_parms >= 0);
    csize[0] = 5;
    csize[1] = 100;
    status = H5Pset_chunk(create_parms, 2, csize);
    assert(status >= 0);

    dataset = H5Dcreate(file, DSET_CHUNKED_NAME, H5T_NATIVE_DOUBLE, space,
                        create_parms);
    if (dataset < 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Could not create a chunked dataset.\n");
        }
        goto error;
    }
    /*
     * Close the chunked dataset.
     */
    if (H5Dclose(dataset) < 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Cannot close chunked dataset.\n");
        }
        goto error;
    }
    puts(" PASSED");
    return SUCCEED;

  error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_simple_io
 *
 * Purpose:     Tests simple I/O.  That is, reading and writing a complete
 *              multi-dimensional array without data type or data space
 *              conversions, without compression, and stored contiguously.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, December 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_simple_io(hid_t file)
{
    hid_t                   dataset, space;
    herr_t                  status;
    int                     points[100][200], check[100][200];
    int                     i, j, n;
    size_t                  dims[2];

    printf("%-70s", "Testing simple I/O");

    /* Initialize the dataset */
    for (i = n = 0; i < 100; i++) {
        for (j = 0; j < 100; j++) {
            points[i][j] = n++;
        }
    }

    /* Create the data space */
    dims[0] = 100;
    dims[1] = 200;
    space = H5Screate_simple(2, dims, NULL);
    assert(space != FAIL);

    /* Create the dataset */
    dataset = H5Dcreate(file, DSET_SIMPLE_IO_NAME, H5T_NATIVE_INT, space,
                        H5P_DEFAULT);
    assert(dataset >= 0);

    /* Write the data to the dataset */
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
                      H5P_DEFAULT, points);
    if (status < 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   H5Dwrite() failed\n");
        }
        goto error;
    }
    /* Read the dataset back */
    status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
                     H5P_DEFAULT, check);
    if (status < 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   H5Dread() failed\n");
        }
        goto error;
    }
    /* Check that the values read are the same as the values written */
    for (i = 0; i < 100; i++) {
        for (j = 0; j < 200; j++) {
            if (points[i][j] != check[i][j]) {
                puts("*FAILED*");
                if (!isatty(1)) {
                    AT();
                    printf("   Read different values than written.\n");
                    printf("   At index %d,%d\n", i, j);
                }
                goto error;
            }
        }
    }

    H5Dclose(dataset);

    puts(" PASSED");
    return SUCCEED;

  error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_tconv
 *
 * Purpose:     Test some simple data type conversion stuff.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, January 14, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_tconv(hid_t file)
{
    uint8	*out=NULL, *in=NULL;
    intn	i;
    size_t	dims[1];
    hid_t	space, dataset, type;
    herr_t	status;

    out = malloc (4*1000000);
    assert (out);
    in = malloc (4*1000000);
    assert (in);

    printf("%-70s", "Testing data type conversion");

    /* Initialize the dataset */
    for (i = 0; i < 1000000; i++)
        ((int32 *) out)[i] = 0x11223344;

    /* Create the data space */
    dims[0] = 1000000;
    space = H5Screate_simple (1, dims, NULL);
    assert(space != FAIL);

    /* Create the data set */
    dataset = H5Dcreate(file, DSET_TCONV_NAME, H5T_NATIVE_INT32, space,
                        H5P_DEFAULT);
    assert(dataset >= 0);

    /* Write the data to the dataset */
    status = H5Dwrite(dataset, H5T_NATIVE_INT32, H5S_ALL, H5S_ALL,
                      H5P_DEFAULT, out);
    assert(status >= 0);

    /* Create a new type with the opposite byte order */
    type = H5Tcopy(H5T_NATIVE_INT32);
    switch (H5Tget_order(type)) {
    case H5T_ORDER_BE:
        H5Tset_order(type, H5T_ORDER_LE);
        break;
    case H5T_ORDER_LE:
        H5Tset_order(type, H5T_ORDER_BE);
        break;
    default:
        assert("funny byte order" && 0);
        break;
    }

    /* Read data with byte order conversion */
    status = H5Dread(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, in);
    assert(status >= 0);

    /* Check */
    for (i = 0; i < 1000000; i++) {
        assert(in[4 * i + 0] == out[4 * i + 3]);
        assert(in[4 * i + 1] == out[4 * i + 2]);
        assert(in[4 * i + 2] == out[4 * i + 1]);
        assert(in[4 * i + 3] == out[4 * i + 0]);
    }

    H5Dclose(dataset);
    H5Tclose(type);

    puts(" PASSED");
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests the dataset interface (H5D)
 *
 * Return:      Success:        exit(0)
 *
 *              Failure:        exit(1)
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t                   file;
    herr_t                  status;
    intn                    nerrors = 0;

    status = H5open ();
    assert (status>=0);

    /* Automatic error reporting to standard output */
    H5Eset_auto ((herr_t(*)(void*))H5Eprint, stdout);

    unlink("dataset.h5");
    file = H5Fcreate("dataset.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    assert(file >= 0);

    status = test_create(file);
    nerrors += status < 0 ? 1 : 0;

    status = test_simple_io(file);
    nerrors += status < 0 ? 1 : 0;

    status = test_tconv(file);
    nerrors += status < 0 ? 1 : 0;

    status = H5Fclose(file);

    if (nerrors) {
        printf("***** %d DATASET TEST%s FAILED! *****\n",
               nerrors, 1 == nerrors ? "" : "S");
        if (isatty(1)) {
            printf("(Redirect output to a pager or a file to see debug "
                   "output)\n");
        }
        exit(1);
    }
    printf("All dataset tests passed.\n");

    status = H5close ();
    assert (status>=0);
    
    return 0;
}
