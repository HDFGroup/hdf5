/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, December  9, 1997
 *
 * Purpose:     Tests the data type interface (H5T)
 */
#include <hdf5.h>
#include <stdio.h>
#include <unistd.h>

#include <H5Aprivate.h>
#include <H5Tprivate.h>

#ifndef HAVE_FUNCTION
#undef __FUNCTION__
#define __FUNCTION__ ""
#endif
#define AT() printf ("   at %s:%d in %s()...\n",                            \
                     __FILE__, __LINE__, __FUNCTION__);

typedef struct complex_t {
    double                  re;
    double                  im;
} complex_t;

/*-------------------------------------------------------------------------
 * Function:    test_classes
 *
 * Purpose:     Test type classes
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
test_classes(void)
{
    printf("%-70s", "Testing H5Tget_class()");

    if (H5T_INTEGER != H5Tget_class(H5T_NATIVE_INT)) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Invalid type class for H5T_NATIVE_INT\n");
        }
        goto error;
    }
    if (H5T_FLOAT != H5Tget_class(H5T_NATIVE_DOUBLE)) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Invalid type class for H5T_NATIVE_DOUBLE\n");
        }
        goto error;
    }
    puts(" PASSED");
    return SUCCEED;

  error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_copy
 *
 * Purpose:     Are we able to copy a data type?
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
test_copy(void)
{
    hid_t                   a_copy;

    printf("%-70s", "Testing H5Tcopy()");

    if ((a_copy = H5Tcopy(H5T_NATIVE_SHORT)) < 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Cannot copy a builtin type.\n");
        }
        goto error;
    }
    if (H5Tclose(a_copy) < 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Cannot close the copied type.\n");
        }
        goto error;
    }
    if (H5Tclose(H5T_NATIVE_CHAR) >= 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Was able to free a built-in type.\n");
        }
        goto error;
    }
    puts(" PASSED");
    return SUCCEED;

  error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_compound
 *
 * Purpose:     Tests various things about compound data types.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compound(void)
{
    complex_t               tmp;
    hid_t                   complex_id;
    herr_t                  status;

    printf("%-70s", "Testing compound data types");

    /* Create the empty type */
    complex_id = H5Tcreate(H5T_COMPOUND, sizeof tmp);
    if (complex_id < 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Cannot create empty compound data type.\n");
        }
        goto error;
    }
    /* Add a coupld fields */
    status = H5Tinsert(complex_id, "real", HOFFSET(tmp, re),
                       H5T_NATIVE_DOUBLE);
    if (status < 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Cannot insert real component.\n");
        }
        goto error;
    }
    status = H5Tinsert(complex_id, "imaginary", HOFFSET(tmp, im),
                       H5T_NATIVE_DOUBLE);
    if (status < 0) {
        puts("*FAILED*");
        if (!isatty(1)) {
            AT();
            printf("   Cannot insert imaginary component.\n");
        }
        goto error;
    }
    puts(" PASSED");

    /* Just for debugging... */
    H5T_debug(H5A_object(complex_id), stdout);
    printf("\n");

    return SUCCEED;

  error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Test the data type interface.
 *
 * Return:      Success:        
 *
 *              Failure:        
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
    herr_t                  status;
    intn                    nerrors = 0;

    status = test_classes();
    nerrors += status < 0 ? 1 : 0;

    status = test_copy();
    nerrors += status < 0 ? 1 : 0;

    status = test_compound();
    nerrors += status < 0 ? 1 : 0;

    if (nerrors) {
        printf("***** %d DATA TYPE TEST%s FAILED! *****\n",
               nerrors, 1 == nerrors ? "" : "S");
        if (isatty(1)) {
            printf("(Redirect output to a pager or a file to see debug "
                   "output)\n");
        }
        exit(1);
    }
    printf("All data type tests passed.\n");
    return 0;
}
