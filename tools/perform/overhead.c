/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Robb Matzke
 *              Monday, September 28, 1998
 *
 * Purpose:  Creates a chunked dataset and measures the storage overhead.
 */

/* See H5private.h for how to include headers */
#undef NDEBUG
#include "hdf5.h"
#include "H5private.h"

#ifdef H5_STDC_HEADERS
#include <ctype.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <string.h>
#endif

#ifdef H5_HAVE_IO_H
#include <io.h>
#endif

#ifdef H5_HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

/* Solaris Studio defines attribute, but for the attributes we need */
#if !defined(H5_HAVE_ATTRIBUTE) || defined __cplusplus || defined(__SUNPRO_C)
#undef __attribute__
#define __attribute__(X) /*void*/
#define H5_ATTR_UNUSED   /*void*/
#else
#define H5_ATTR_UNUSED __attribute__((unused))
#endif

#define FILE_NAME_1 "overhead.h5"
#ifndef FALSE
#define FALSE 0
#endif /* FALSE */
#ifndef TRUE
#define TRUE 1
#endif /* TRUE */

typedef enum fill_t { FILL_ALL, FILL_FORWARD, FILL_REVERSE, FILL_INWARD, FILL_OUTWARD, FILL_RANDOM } fill_t;

/*-------------------------------------------------------------------------
 * Function:  usage
 *
 * Purpose:  Prints a usage message and exits.
 *
 * Return:  never returns
 *
 * Programmer:  Robb Matzke
 *              Wednesday, September 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage(const char *prog)
{
    HDfprintf(stderr, "usage: %s [STYLE|cache] [LEFT [MIDDLE [RIGHT]]]\n", prog);
    HDfprintf(stderr, "\
    STYLE is the order that the dataset is filled and should be one of:\n\
        forward   --  Fill the dataset from lowest address to highest\n\
                      address. This style tests the right split ratio.\n\
        reverse   --  Fill the dataset from highest address to lowest\n\
                      address.  This is the reverse order of `forward' and\n\
                      tests the left split ratio.\n\
        inward    --  Fill beginning at both the lowest and highest\n\
                      addresses and work in toward the center of the\n\
                      dataset.  This tests the middle split ratio.\n\
        outward   --  Start at the center of the dataset and work outward\n\
                      toward the lowest and highest addresses.  This tests\n\
                      both left and right split ratios.\n\
        random    --  Write the chunks of the dataset in random order.  This\n\
                      tests all split ratios.\n\
    If no fill style is specified then all fill styles are tried and a\n\
    single value is printed for each one.\n\
\n\
    If the word `cache' is used instead of a fill style then the raw data\n\
    cache is enabled.  It is not possible to enable the raw data cache when\n\
    a specific fill style is used because H5Fflush() is called after each\n\
    chunk is written in order to calculate overhead during the test.  If\n\
    the cache is enabled then chunks are written to disk in different orders\n\
    than the actual H5Dwrite() calls in the test due to collisions and the\n\
    resulting B-tree will be split differently.\n\
\n\
    LEFT, MIDDLE, and RIGHT are the ratios to use for splitting and should\n\
    be values between zero and one, inclusive.\n");
    exit(1);
}

/*-------------------------------------------------------------------------
 * Function:  cleanup
 *
 * Purpose:  Removes test files
 *
 * Return:  void
 *
 * Programmer:  Robb Matzke
 *              Thursday, June  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
cleanup(void)
{
    if (!getenv("HDF5_NOCLEANUP")) {
        remove(FILE_NAME_1);
    }
}

/*-------------------------------------------------------------------------
 * Function:  display_error_cb
 *
 * Purpose:  Displays the error stack after printing "*FAILED*".
 *
 * Return:  Success:  0
 *
 *    Failure:  -1
 *
 * Programmer:  Robb Matzke
 *    Wednesday, March  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
display_error_cb(hid_t estack, void H5_ATTR_UNUSED *client_data)
{
    puts("*FAILED*");
    H5Eprint2(estack, stdout);

    return 0;
}

/*-------------------------------------------------------------------------
 * Function:  test
 *
 * Purpose:  The guts of the test
 *
 * Return:  Success:  0
 *
 *    Failure:  number of errors
 *
 * Programmer:  Robb Matzke
 *              Wednesday, September 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test(fill_t fill_style, const double splits[], hbool_t verbose, hbool_t use_rdcc)
{
    hid_t       file = (-1), fapl = (-1), dcpl = (-1), xfer = (-1), mspace = (-1), fspace = (-1), dset = (-1);
    hsize_t     ch_size[1]  = {1};             /*chunk size    */
    hsize_t     cur_size[1] = {1000};          /*current dataset size  */
    hsize_t     max_size[1] = {H5S_UNLIMITED}; /*maximum dataset size  */
    hsize_t     hs_start[1];                   /*hyperslab start offset*/
    hsize_t     hs_count[1] = {1};             /*hyperslab nelmts  */
    int         fd          = (-1);            /*h5 file direct  */
    int *       had         = NULL;            /*for random filling  */
    const char *sname       = NULL;            /*fill style nam  */
    int         mdc_nelmts;                    /*num meta objs to cache*/
    hsize_t     i, k;
    int         j;
    h5_stat_t   sb;

    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if (!use_rdcc) {
        if (H5Pget_cache(fapl, &mdc_nelmts, NULL, NULL, NULL) < 0)
            goto error;
        if (H5Pset_cache(fapl, mdc_nelmts, 0, 0, 0.0F) < 0)
            goto error;
    }
    if ((file = H5Fcreate(FILE_NAME_1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(dcpl, 1, ch_size) < 0)
        goto error;
    if ((xfer = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pset_btree_ratios(xfer, splits[0], splits[1], splits[2]) < 0)
        goto error;
    if ((fspace = H5Screate_simple(1, cur_size, max_size)) < 0)
        goto error;
    if ((mspace = H5Screate_simple(1, ch_size, ch_size)) < 0)
        goto error;
    if ((dset = H5Dcreate2(file, "chunked", H5T_NATIVE_INT, fspace, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;
    if ((fd = HDopen(FILE_NAME_1, O_RDONLY, 0666)) < 0)
        goto error;

    if (FILL_RANDOM == fill_style)
        had = (int *)calloc((size_t)cur_size[0], sizeof(int));

    for (i = 1; i <= cur_size[0]; i++) {

        /* Decide which chunk to write to */
        switch (fill_style) {
            case FILL_FORWARD:
                hs_start[0] = i - 1;
                break;
            case FILL_REVERSE:
                hs_start[0] = cur_size[0] - i;
                break;
            case FILL_INWARD:
                hs_start[0] = i % 2 ? i / 2 : cur_size[0] - i / 2;
                break;
            case FILL_OUTWARD:
                k           = (cur_size[0] - i) + 1;
                hs_start[0] = k % 2 ? (k / 2) : (hsize_t)((hssize_t)cur_size[0] - (hssize_t)(k / 2));
                break;
            case FILL_RANDOM:
                for (j = HDrand() % (int)cur_size[0]; had[j]; j = (j + 1) % (int)cur_size[0])
                    /*void*/;
                hs_start[0] = (hsize_t)j;
                had[j]      = 1;
                break;
            case FILL_ALL:
                abort();
            default:
                /* unknown request */
                HDfprintf(stderr, "Unknown fill style\n");
                goto error;
                break;
        }

        /* Write the chunk */
        if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_start, NULL, hs_count, NULL) < 0)
            goto error;
        if (H5Dwrite(dset, H5T_NATIVE_INT, mspace, fspace, xfer, &i) < 0) {
            goto error;
        }

        /* Determine overhead */
        if (verbose) {
            if (H5Fflush(file, H5F_SCOPE_LOCAL) < 0)
                goto error;
            if (HDfstat(fd, &sb) < 0)
                goto error;
            printf("%4lu %8.3f ***\n", (unsigned long)i,
                   (double)(sb.st_size - (HDoff_t)(i * sizeof(int))) / (double)i);
        }
    }

    if (had) {
        free(had);
        had = NULL;
    } /* end if */

    H5Dclose(dset);
    H5Sclose(mspace);
    H5Sclose(fspace);
    H5Pclose(dcpl);
    H5Pclose(xfer);
    H5Fclose(file);

    if (!verbose) {
        switch (fill_style) {
            case FILL_FORWARD:
                sname = "forward";
                break;
            case FILL_REVERSE:
                sname = "reverse";
                break;
            case FILL_INWARD:
                sname = "inward";
                break;
            case FILL_OUTWARD:
                sname = "outward";
                break;
            case FILL_RANDOM:
                sname = "random";
                break;
            case FILL_ALL:
                abort();
            default:
                /* unknown request */
                HDfprintf(stderr, "Unknown fill style\n");
                goto error;
                break;
        }

        if (HDfstat(fd, &sb) < 0)
            goto error;
        printf("%-7s %8.3f\n", sname,
               (double)(sb.st_size - (HDoff_t)(cur_size[0] * sizeof(int))) / (double)cur_size[0]);
    }
    HDclose(fd);

    return 0;

error:
    H5Dclose(dset);
    H5Sclose(mspace);
    H5Sclose(fspace);
    H5Pclose(dcpl);
    H5Pclose(xfer);
    H5Fclose(file);
    if (had)
        free(had);
    HDclose(fd);
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:  main
 *
 * Purpose:
 *
 * Return:  Success:        zero
 *
 *    Failure:  non-zero
 *
 * Programmer:  Robb Matzke
 *              Monday, September 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t   xfer;
    fill_t  fill_style = FILL_ALL;
    hbool_t use_cache  = FALSE;
    double  splits[3];
    int     i, j, nerrors = 0;

    /* Default split ratios */
    H5Eset_auto2(H5E_DEFAULT, display_error_cb, NULL);

    if ((xfer = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if (H5Pget_btree_ratios(xfer, splits + 0, splits + 1, splits + 2) < 0)
        goto error;
    if (H5Pclose(xfer) < 0)
        goto error;

    /* Parse command-line options */
    for (i = 1, j = 0; i < argc; i++) {
        if (!strcmp(argv[i], "forward")) {
            fill_style = FILL_FORWARD;
        }
        else if (!strcmp(argv[i], "reverse")) {
            fill_style = FILL_REVERSE;
        }
        else if (!strcmp(argv[i], "inward")) {
            fill_style = FILL_INWARD;
        }
        else if (!strcmp(argv[i], "outward")) {
            fill_style = FILL_OUTWARD;
        }
        else if (!strcmp(argv[i], "random")) {
            fill_style = FILL_RANDOM;
        }
        else if (!strcmp(argv[i], "cache")) {
            use_cache = TRUE;
        }
        else if (j < 3 && (isdigit(argv[i][0]) || '.' == argv[i][0])) {
            splits[j++] = strtod(argv[i], NULL);
        }
        else {
            usage(argv[0]);
        }
    }

    if (FILL_ALL == fill_style) {
        printf("%-7s %8s\n", "Style", "Bytes/Chunk");
        printf("%-7s %8s\n", "-----", "-----------");
        nerrors += test(FILL_FORWARD, splits, FALSE, use_cache);
        nerrors += test(FILL_REVERSE, splits, FALSE, use_cache);
        nerrors += test(FILL_INWARD, splits, FALSE, use_cache);
        nerrors += test(FILL_OUTWARD, splits, FALSE, use_cache);
        nerrors += test(FILL_RANDOM, splits, FALSE, use_cache);
    }
    else {
        if (use_cache)
            usage(argv[0]);
        nerrors += test(fill_style, splits, TRUE, FALSE);
    }
    if (nerrors > 0)
        goto error;
    cleanup();
    return 0;

error:
    HDfprintf(stderr, "*** ERRORS DETECTED ***\n");
    return 1;
}
