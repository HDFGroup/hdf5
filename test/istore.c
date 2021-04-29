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

/* Programmer:    Robb Matzke
 *        Wednesday, October 15, 1997
 *
 * Purpose:    Tests various aspects of indexed raw data storage.
 */

#define H5F_FRIEND /*suppress error about including H5Fpkg      */

#include "h5test.h"

#include "H5Dprivate.h"
#include "H5Iprivate.h"
#include "H5Pprivate.h"
#include "H5Fpkg.h"
#include "H5Gprivate.h"
#include "H5Oprivate.h"
#include "H5VMprivate.h"

const char *FILENAME[] = {"istore", NULL};

#define TEST_SMALL  0x0001
#define TEST_MEDIUM 0x0002
#define TEST_LARGE  0x0004

/* The datatype of the dataset operated on by this test */
#define TEST_DATATYPE H5T_NATIVE_UCHAR

#define TEST_CHUNK_SIZE  50
#define TEST_SPARSE_SIZE 1000000

hsize_t chunk_dims[H5O_LAYOUT_NDIMS];
hsize_t zero[H5O_LAYOUT_NDIMS];

/*-------------------------------------------------------------------------
 * Function:    is_sparse
 *
 * Purpose:    Determines if the file system of the current working
 *        directory supports holes.
 *
 * Return:    Success:    Non-zero if holes are supported; zero
 *                otherwise.
 *
 *        Failure:    zero
 *
 * Programmer:    Robb Matzke
 *              Wednesday, July 15, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
is_sparse(void)
{
    int       fd;
    h5_stat_t sb;

    if ((fd = HDopen("x.h5", O_RDWR | O_TRUNC | O_CREAT, H5_POSIX_CREATE_MODE_RW)) < 0)
        return 0;
    if (HDlseek(fd, (off_t)(1024 * 1024), SEEK_SET) != 1024 * 1024)
        return 0;
    if (5 != HDwrite(fd, "hello", (size_t)5))
        return 0;
    if (HDclose(fd) < 0)
        return 0;
    if (HDstat("x.h5", &sb) < 0)
        return 0;
    if (HDremove("x.h5") < 0)
        return 0;
#ifdef H5_HAVE_STAT_ST_BLOCKS
    return ((unsigned long)sb.st_blocks * 512 < (unsigned long)sb.st_size);
#else
    return (0);
#endif
}

/*-------------------------------------------------------------------------
 * Function:    print_array
 *
 * Purpose:    Prints the values in an array
 *
 * Return:    void
 *
 * Programmer:    Robb Matzke
 *        Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
print_array(uint8_t *array, size_t nx, size_t ny, size_t nz)
{
    size_t i, j, k;

    for (i = 0; i < nx; i++) {
        if (nz > 1) {
            HDfprintf(stderr, "i=%lu:\n", (unsigned long)i);
        }
        else {
            HDfprintf(stderr, "%03lu:", (unsigned long)i);
        }

        for (j = 0; j < ny; j++) {
            if (nz > 1)
                HDfprintf(stderr, "%03lu:", (unsigned long)j);
            for (k = 0; k < nz; k++) {
                HDfprintf(stderr, " %3d", *array++);
            }
            if (nz > 1)
                HDfprintf(stderr, "\n");
        }
        HDfprintf(stderr, "\n");
    }
}

/*-------------------------------------------------------------------------
 * Function:    new_object
 *
 * Purpose:    Creates a new object that refers to a indexed storage of raw
 *        data.  No raw data is stored.
 *
 * Return:    Success:    ID of dataset
 *
 *        Failure:    -1
 *
 * Programmer:    Robb Matzke
 *        Wednesday, October 15, 1997
 *
 * Modifications:
 *              Converted to use datasets instead of directly messing with
 *              the istore routines, etc. since the new raw data architecture
 *              performs hyperslab operations at a higher level than the
 *              istore routines did and the new istore routines can't handle
 *              I/O on more than one chunk at a time. QAK - 2003/04/16
 *
 *-------------------------------------------------------------------------
 */
static hid_t
new_object(hid_t f, const char *name, int ndims, hsize_t dims[], hsize_t cdims[])
{
    hid_t dataset; /* Dataset ID */
    hid_t space;   /* Dataspace ID */
    hid_t dcpl;    /* Dataset creation property list ID */

    /* Create the dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* Set the chunk dimensions */
    if (H5Pset_chunk(dcpl, ndims, cdims) < 0)
        TEST_ERROR;

    /* Create the dataspace */
    if ((space = H5Screate_simple(ndims, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create the dataset */
    if ((dataset = H5Dcreate2(f, name, TEST_DATATYPE, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Clean up */

    /* Close property lists */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    /* Close dataspace */
    if (H5Sclose(space) < 0)
        TEST_ERROR;

    return dataset;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    test_create
 *
 * Purpose:    Creates a named object that refers to indexed storage of raw
 *        data.  No raw data is stored.
 *
 * Return:    Success:    SUCCEED
 *
 *        Failure:    FAIL
 *
 * Programmer:    Robb Matzke
 *        Wednesday, October 15, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_create(hid_t f, const char *prefix)
{
    hid_t    dataset;                             /* Dataset ID */
    hsize_t  dims[H5O_LAYOUT_NDIMS + 1];          /* Dimensions of dataset */
    hsize_t  my_chunk_dims[H5O_LAYOUT_NDIMS + 1]; /* Dimensions of chunks */
    char     name[256];                           /* Dataset name */
    unsigned u;                                   /* Local index variable */

    TESTING("istore create");

    dims[0] = my_chunk_dims[0] = 1;
    for (u = 1; u <= H5S_MAX_RANK; u++) {
        /* Initialize the dimension size in this new dimension */
        dims[u] = my_chunk_dims[u] = 2;

        /* Create chunked dataset of this dimensionality */
        HDsnprintf(name, sizeof name, "%s_%02u", prefix, u);
        if ((dataset = new_object(f, name, (int)u, dims, my_chunk_dims)) < 0)
            return FAIL;

        /* Close dataset created */
        if (H5Dclose(dataset) < 0)
            return FAIL;
    }

    PASSED();
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_extend
 *
 * Purpose:    Creates an empty object and then writes to it in such a way
 *        as to always extend the object's domain without creating
 *        holes and without causing the object to become concave.
 *
 * Return:    Success:    SUCCEED
 *
 *        Failure:    FAIL
 *
 * Programmer:    Robb Matzke
 *        Wednesday, October 15, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_extend(hid_t f, const char *prefix, size_t nx, size_t ny, size_t nz)
{
    hid_t    dataset; /* Dataset ID */
    hid_t    fspace;  /* Dataset's file dataspace */
    hid_t    mspace;  /* Dataset's memory dataspace */
    size_t   i, j, k, ctr;
    int      ndims;
    uint8_t *buf = NULL, *check = NULL, *whole = NULL;
    char     dims[64], s[256], name[256];
    hsize_t  offset[3];
    hsize_t  max_corner[3];
    hsize_t  size[3];
    hsize_t  whole_size[3];
    hsize_t  nelmts;

    if (!nz) {
        if (!ny) {
            ndims = 1;
            ny = nz = 1;
            HDsprintf(dims, "%lu", (unsigned long)nx);
        }
        else {
            ndims = 2;
            nz    = 1;
            HDsprintf(dims, "%lux%lu", (unsigned long)nx, (unsigned long)ny);
        }
    }
    else {
        ndims = 3;
        HDsprintf(dims, "%lux%lux%lu", (unsigned long)nx, (unsigned long)ny, (unsigned long)nz);
    }

    HDsprintf(s, "istore extend: %s", dims);
    TESTING(s);
    buf   = (uint8_t *)HDmalloc(nx * ny * nz);
    check = (uint8_t *)HDmalloc(nx * ny * nz);
    whole = (uint8_t *)HDcalloc((size_t)1, nx * ny * nz);

    whole_size[0] = nx;
    whole_size[1] = ny;
    whole_size[2] = nz;
    max_corner[0] = 0;
    max_corner[1] = 0;
    max_corner[2] = 0;

    /* Build the new empty object */
    HDsprintf(name, "%s_%s", prefix, dims);
    if ((dataset = new_object(f, name, ndims, whole_size, whole_size)) < 0) {
        HDfprintf(stderr, "    Cannot create %u-d object `%s'\n", ndims, name);
        goto error;
    }

    /* Get dataset's dataspace */
    if ((fspace = H5Dget_space(dataset)) < 0)
        TEST_ERROR;

    for (ctr = 0; H5VM_vector_lt_u((unsigned)ndims, max_corner, whole_size); ctr++) {

        /* Size and location */
        if (0 == ctr) {
            offset[0] = offset[1] = offset[2] = 0;
            size[0] = size[1] = size[2] = 1;
            nelmts                      = 1;
        }
        else {
            for (i = 0, nelmts = 1; i < (size_t)ndims; i++) {
                if (ctr % (size_t)ndims == i) {
                    offset[i] = max_corner[i];
                    size[i]   = MIN(1, whole_size[i] - offset[i]);
                }
                else {
                    offset[i] = 0;
                    size[i]   = max_corner[i];
                }
                nelmts *= size[i];
            }
        }

#if 0
    if (0 == ctr)
        HDfprintf(stderr,"\n");
    HDfprintf(stderr,"    Insert: ctr=%lu, corner=(%ld", (unsigned long)ctr, (long)offset[0]);
    if (ndims > 1)
        HDfprintf(stderr,",%ld", (long)offset[1]);
    if (ndims > 2)
        HDfprintf(stderr,",%ld", (long)offset[2]);
    HDfprintf(stderr,"), size=(%lu", (unsigned long)size[0]);
    if (ndims > 1)
        HDfprintf(stderr,",%lu", (unsigned long)size[1]);
    if (ndims > 2)
        HDfprintf(stderr,",%lu", (unsigned long)size[2]);
    HDfprintf(stderr,"), %lu element%s", (unsigned long)nelmts, 1 == nelmts ? "" : "s");
    if (0 == nelmts)
        HDfprintf(stderr," *SKIPPED*");
    HDfprintf(stderr,"\n");
#endif

        /* Fill the source array */
        if (0 == nelmts)
            continue;
        HDmemset(buf, (signed)(128 + ctr), (size_t)nelmts);

        /* Create dataspace for selection in memory */
        if ((mspace = H5Screate_simple(1, &nelmts, NULL)) < 0)
            TEST_ERROR;

        /* Select region in file dataspace */
        if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, offset, NULL, size, NULL) < 0)
            TEST_ERROR;

        /* Write to disk */
        if (H5Dwrite(dataset, TEST_DATATYPE, mspace, fspace, H5P_DEFAULT, buf) < 0) {
            H5_FAILED();
            HDfprintf(stderr, "    Write failed: ctr=%lu\n", (unsigned long)ctr);
            goto error;
        }

        /* Read from disk */
        HDmemset(check, 0xff, (size_t)nelmts);
        if (H5Dread(dataset, TEST_DATATYPE, mspace, fspace, H5P_DEFAULT, check) < 0) {
            H5_FAILED();
            HDfprintf(stderr, "    Read failed: ctr=%lu\n", (unsigned long)ctr);
            goto error;
        }
        if (HDmemcmp(buf, check, (size_t)nelmts) != 0) {
            H5_FAILED();
            HDfprintf(stderr, "    Read check failed: ctr=%lu\n", (unsigned long)ctr);
            HDfprintf(stderr, "    Wrote:\n");
            print_array(buf, (size_t)size[0], (size_t)size[1], (size_t)size[2]);
            HDfprintf(stderr, "    Read:\n");
            print_array(check, (size_t)size[0], (size_t)size[1], (size_t)size[2]);
            goto error;
        }

        /* Close memory dataspace */
        if (H5Sclose(mspace) < 0)
            TEST_ERROR;

        /* Write to `whole' buffer for later checking */
        H5VM_hyper_copy((unsigned)ndims, size, whole_size, offset, whole, /*dst*/
                        size, H5VM_ZERO, buf);                            /*src*/

        /* Update max corner */
        for (i = 0; i < (size_t)ndims; i++)
            max_corner[i] = MAX(max_corner[i], offset[i] + size[i]);
    }

    /* Now read the entire array back out and check it */
    HDmemset(buf, 0xff, nx * ny * nz);
    if (H5Dread(dataset, TEST_DATATYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) {
        H5_FAILED();
        HDfprintf(stderr, "    Read failed for whole array.\n");
        goto error;
    }
    for (i = 0; i < nx; i++) {
        for (j = 0; j < ny; j++) {
            for (k = 0; k < nz; k++) {
                if (whole[i * ny * nz + j * nz + k] != buf[i * ny * nz + j * nz + k]) {
                    H5_FAILED();
                    HDfprintf(stderr, "    Check failed at i=%lu", (unsigned long)i);
                    if (ndims > 1) {
                        HDfprintf(stderr, ", j=%lu", (unsigned long)j);
                    }
                    if (ndims > 2) {
                        HDfprintf(stderr, ", k=%lu", (unsigned long)k);
                    }
                    HDfprintf(stderr, "\n    Check array is:\n");
                    print_array(whole, nx, ny, nz);
                    HDfprintf(stderr, "    Value read is:\n");
                    print_array(buf, nx, ny, nz);
                    goto error;
                }
            }
        }
    }

    /* Close dataset's dataspace */
    if (H5Sclose(fspace) < 0)
        TEST_ERROR;

    /* Close dataset */
    if (H5Dclose(dataset) < 0)
        TEST_ERROR;

    /* Free memory used */
    HDfree(buf);
    HDfree(check);
    HDfree(whole);

    PASSED();
    return SUCCEED;

error:
    HDfree(buf);
    HDfree(check);
    HDfree(whole);
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_sparse
 *
 * Purpose:    Creates a sparse matrix consisting of NBLOCKS randomly placed
 *        blocks each of size NX,NY,NZ.
 *
 * Return:    Success:    SUCCEED
 *
 *        Failure:    FAIL
 *
 * Programmer:    Robb Matzke
 *        Wednesday, October 22, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_sparse(hid_t f, const char *prefix, size_t nblocks, size_t nx, size_t ny, size_t nz, int skip_test)
{
    hid_t    dataset; /* Dataset ID */
    hid_t    fspace;  /* Dataset's file dataspace */
    hid_t    mspace;  /* Dataset's memory dataspace */
    int      ndims;
    hsize_t  ctr;
    char     dims[64], s[256], name[256];
    hsize_t  offset[3];
    hsize_t  size[3], total = 0;
    uint8_t *buf = NULL;
    hsize_t  whole_size[3]; /* Size of dataset's dataspace */
    size_t   u;             /* Local index variable */

    if (!nz) {
        if (!ny) {
            ndims = 1;
            ny = nz = 1;
            HDsprintf(dims, "%lu", (unsigned long)nx);
        }
        else {
            ndims = 2;
            nz    = 1;
            HDsprintf(dims, "%lux%lu", (unsigned long)nx, (unsigned long)ny);
        }
    }
    else {
        ndims = 3;
        HDsprintf(dims, "%lux%lux%lu", (unsigned long)nx, (unsigned long)ny, (unsigned long)nz);
    }

    HDsprintf(s, "istore sparse: %s", dims);
    TESTING(s);
    if (skip_test) {
        SKIPPED()
        return SUCCEED;
    }
    buf = (uint8_t *)HDmalloc(nx * ny * nz);
    HDmemset(buf, 128, nx * ny * nz);

    /* Set dimensions of dataset */
    for (u = 0; u < (size_t)ndims; u++)
        whole_size[u] = TEST_SPARSE_SIZE;

    /* Set dimensions of selection */
    size[0] = nx;
    size[1] = ny;
    size[2] = nz;

    /* Build the new empty object */
    HDsprintf(name, "%s_%s", prefix, dims);
    if ((dataset = new_object(f, name, ndims, whole_size, chunk_dims)) < 0) {
        HDprintf("    Cannot create %u-d object `%s'\n", ndims, name);
        goto error;
    }

    /* Get dataset's dataspace */
    if ((fspace = H5Dget_space(dataset)) < 0)
        TEST_ERROR;

    /* Create dataspace for memory buffer */
    if ((mspace = H5Screate_simple(ndims, size, NULL)) < 0)
        TEST_ERROR;

    for (ctr = 0; ctr < nblocks; ctr++) {
        offset[0] = (hsize_t)(HDrandom() % (int)(TEST_SPARSE_SIZE - nx));
        offset[1] = (hsize_t)(HDrandom() % (int)(TEST_SPARSE_SIZE - ny));
        offset[2] = (hsize_t)(HDrandom() % (int)(TEST_SPARSE_SIZE - nz));

        /* Select region in file dataspace */
        if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, offset, NULL, size, NULL) < 0)
            TEST_ERROR;

        /* write to disk */
        if (H5Dwrite(dataset, TEST_DATATYPE, mspace, fspace, H5P_DEFAULT, buf) < 0) {
            H5_FAILED();
            HDprintf("    Write failed: ctr=%lu\n", (unsigned long)ctr);
            HDprintf("    offset=(%lu", (unsigned long)(offset[0]));
            if (ndims > 1)
                HDprintf(",%lu", (unsigned long)(offset[1]));
            if (ndims > 2)
                HDprintf(",%lu", (unsigned long)(offset[2]));
            HDprintf("), size=(%lu", (unsigned long)(size[0]));
            if (ndims > 1)
                HDprintf(",%lu", (unsigned long)(size[1]));
            if (ndims > 2)
                HDprintf(",%lu", (unsigned long)(size[2]));
            HDprintf(")\n");
            goto error;
        }
        total += nx * ny * nz;
#if 0
    HDfprintf(stderr,"ctr: ctr=%Zu, total=%Zu\n", ctr, total);
#endif

        /* We don't test reading yet.... */
    }

    /* Close memory dataspace */
    if (H5Sclose(mspace) < 0)
        TEST_ERROR;

    /* Close dataset's dataspace */
    if (H5Sclose(fspace) < 0)
        TEST_ERROR;

    /* Close dataset */
    if (H5Dclose(dataset) < 0)
        TEST_ERROR;

    HDfree(buf);
    PASSED();
    return SUCCEED;

error:
    HDfree(buf);
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests indexed storage
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 * Programmer:    Robb Matzke
 *        Wednesday, October 15, 1997
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t    fapl = -1, file = -1, fcpl = -1;
    herr_t   status;
    int      nerrors = 0;
    unsigned size_of_test;
    unsigned u; /* Local index variable */
    char     filename[1024];
    int      skip_test          = 0;
    int      has_sparse_support = 0;

    /* Parse arguments or assume these tests (`small', `medium' ) */
    if (1 == argc) {
        size_of_test = TEST_SMALL | TEST_MEDIUM | TEST_LARGE;
    }
    else {
        int i;
        for (i = 1, size_of_test = 0; i < argc; i++) {
            if (!HDstrcmp(argv[i], "small")) {
                size_of_test |= TEST_SMALL;
            }
            else if (!HDstrcmp(argv[i], "medium")) {
                size_of_test |= TEST_MEDIUM;
            }
            else if (!HDstrcmp(argv[i], "large")) {
                size_of_test |= TEST_LARGE;
            }
            else {
                HDprintf("unrecognized argument: %s\n", argv[i]);
            }
        }
    }
    HDprintf("Test sizes: ");
    if (size_of_test & TEST_SMALL)
        HDprintf(" SMALL");
    if (size_of_test & TEST_MEDIUM)
        HDprintf(" MEDIUM");
    if (size_of_test & TEST_LARGE)
        HDprintf(" LARGE");
    HDprintf("\n");

    /* Set the random # seed */
    HDsrandom((unsigned)HDtime(NULL));

    /* Check to see if the file system supports POSIX-style sparse files.
     * Windows NTFS does not, so we want to avoid tests which create
     * very large files.
     */
    has_sparse_support = is_sparse();

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();

    /* Use larger file addresses... */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    H5Pset_sizes(fcpl, (size_t)8, (size_t)0);

    /* Create the test file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0) {
        HDprintf("Cannot create file %s; test aborted\n", filename);
        HDexit(EXIT_FAILURE);
    }

    /* Initialize chunk dimensions */
    for (u = 0; u < H5O_LAYOUT_NDIMS; u++)
        chunk_dims[u] = TEST_CHUNK_SIZE;

    /*
     * Creation test: Creates empty objects with various raw data sizes
     * and alignments.
     */
    status = test_create(file, "create");
    nerrors += status < 0 ? 1 : 0;

    if (size_of_test & TEST_SMALL) {
        status = test_extend(file, "extend", (size_t)10, (size_t)0, (size_t)0);
        nerrors += status < 0 ? 1 : 0;
        status = test_extend(file, "extend", (size_t)10, (size_t)10, (size_t)0);
        nerrors += status < 0 ? 1 : 0;
        status = test_extend(file, "extend", (size_t)10, (size_t)10, (size_t)10);
        nerrors += status < 0 ? 1 : 0;
    }
    if (size_of_test & TEST_MEDIUM) {
        status = test_extend(file, "extend", (size_t)10000, (size_t)0, (size_t)0);
        nerrors += status < 0 ? 1 : 0;
        status = test_extend(file, "extend", (size_t)2500, (size_t)10, (size_t)0);
        nerrors += status < 0 ? 1 : 0;
        status = test_extend(file, "extend", (size_t)10, (size_t)400, (size_t)10);
        nerrors += status < 0 ? 1 : 0;
    }
    skip_test = 0;
    if (size_of_test & TEST_SMALL) {
        status = test_sparse(file, "sparse", (size_t)100, (size_t)5, (size_t)0, (size_t)0, skip_test);
        nerrors += status < 0 ? 1 : 0;
        status = test_sparse(file, "sparse", (size_t)100, (size_t)3, (size_t)4, (size_t)0, skip_test);
        nerrors += status < 0 ? 1 : 0;
        status = test_sparse(file, "sparse", (size_t)100, (size_t)2, (size_t)3, (size_t)4, skip_test);
        nerrors += status < 0 ? 1 : 0;
    }
    if (size_of_test & TEST_MEDIUM) {
        status = test_sparse(file, "sparse", (size_t)1000, (size_t)30, (size_t)0, (size_t)0, skip_test);
        nerrors += status < 0 ? 1 : 0;
        status = test_sparse(file, "sparse", (size_t)2000, (size_t)7, (size_t)3, (size_t)0, skip_test);
        nerrors += status < 0 ? 1 : 0;
        status = test_sparse(file, "sparse", (size_t)2000, (size_t)4, (size_t)2, (size_t)3, skip_test);
        nerrors += status < 0 ? 1 : 0;
    }
    skip_test = !has_sparse_support;
    if (size_of_test & TEST_LARGE) {
        /* This test is skipped if there is no POSIX-style sparse file support
         * e.g.: Windows NTFS filesystems
         */
        status = test_sparse(file, "sparse", (size_t)800, (size_t)50, (size_t)50, (size_t)50, skip_test);
        if (skip_test)
            HDprintf("    The current VFD does not support sparse files on this platform.\n");
        nerrors += status < 0 ? 1 : 0;
    }

    /* Close the test file and exit */
    H5Pclose(fcpl);
    H5Fclose(file);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if (nerrors) {
        HDprintf("***** %d I-STORE TEST%s FAILED! *****\n", nerrors, 1 == nerrors ? "" : "S");
        HDexit(EXIT_FAILURE);
    }

    HDprintf("All i-store tests passed.\n");

    h5_cleanup(FILENAME, fapl);

    HDexit(EXIT_SUCCESS);
}
