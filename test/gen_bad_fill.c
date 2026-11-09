/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Generate an HDF5 file with a deliberately malformed fill value message, used
 * by the test_bad_fill_value() regression test in fillval.c.
 *
 * The file contains a single contiguous integer dataset written with an
 * explicit fill value using a version-2 (old-style) fill value message.  After
 * the file is written, the message's 4-byte signed size field is patched to a
 * negative value (0xFF000000 == -16777216).
 *
 * On read, H5O_fill_old_decode() decodes that size with INT32DECODE and, since
 * it is not > 0, leaves fill.buf == NULL without normalizing the size to the
 * "undefined" sentinel (-1).  The dataset's create property list therefore ends
 * up with a fill value whose size is neither 0, -1, nor positive and whose
 * datatype is NULL.
 *
 */

#include "h5test.h"

#define BAD_FILL_FILE  "bad_fill_value.h5"
#define BAD_FILL_DSET  "dset"
#define BAD_FILL_VALUE 0x1a2b3c4d

/* Read an entire file into a newly allocated buffer. */
static unsigned char *
slurp(const char *name, size_t *len_out)
{
    FILE          *f = fopen(name, "rb");
    long           len;
    unsigned char *buf;

    if (!f)
        return NULL;
    if (fseek(f, 0, SEEK_END) != 0 || (len = ftell(f)) < 0 || fseek(f, 0, SEEK_SET) != 0) {
        fclose(f);
        return NULL;
    }
    if (NULL == (buf = malloc((size_t)len))) {
        fclose(f);
        return NULL;
    }
    if (fread(buf, 1, (size_t)len, f) != (size_t)len) {
        free(buf);
        fclose(f);
        return NULL;
    }
    fclose(f);
    *len_out = (size_t)len;
    return buf;
}

/* Write a buffer back out to a file. */
static int
spew(const char *name, const unsigned char *buf, size_t len)
{
    FILE *f = fopen(name, "wb");
    if (!f)
        return -1;
    if (fwrite(buf, 1, len, f) != len) {
        fclose(f);
        return -1;
    }
    return fclose(f) == 0 ? 0 : -1;
}

/* Find the unique occurrence of pattern in buf; return offset or (size_t)-1. */
static size_t
find_once(const unsigned char *buf, size_t len, const unsigned char *pat, size_t patlen)
{
    size_t i, found = (size_t)-1;

    if (patlen == 0 || len < patlen)
        return (size_t)-1;
    for (i = 0; i <= len - patlen; i++) {
        if (memcmp(buf + i, pat, patlen) == 0) {
            if (found != (size_t)-1)
                return (size_t)-1; /* not unique */
            found = i;
        }
    }
    return found;
}

int
main(void)
{
    hid_t          fapl = H5I_INVALID_HID, file = H5I_INVALID_HID, sid = H5I_INVALID_HID;
    hid_t          dcpl = H5I_INVALID_HID, dset = H5I_INVALID_HID;
    hsize_t        dims[1] = {4};
    int            data[4] = {10, 20, 30, 40};
    int            fillval = BAD_FILL_VALUE;
    unsigned char *buf     = NULL;
    size_t         len, off;

    /* Anchor for the fill value message's size field: the "fill defined" byte
     * (0x01), the 4-byte size (4), and the distinctive fill value that follows.
     * This uniquely locates the size field regardless of its file offset. */
    static const unsigned char anchor[] = {0x01, 0x04, 0x00, 0x00, 0x00, 0x4d, 0x3c, 0x2b, 0x1a};

    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;
    if ((file = H5Fcreate(BAD_FILL_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_layout(dcpl, H5D_CONTIGUOUS) < 0)
        TEST_ERROR;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0)
        TEST_ERROR;

    if ((dset = H5Dcreate2(file, BAD_FILL_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0 || H5Pclose(dcpl) < 0 || H5Sclose(sid) < 0 || H5Fclose(file) < 0 ||
        H5Pclose(fapl) < 0)
        TEST_ERROR;

    /* Patch the fill value message's size field to a negative value. */
    if (NULL == (buf = slurp(BAD_FILL_FILE, &len)))
        TEST_ERROR;
    if ((off = find_once(buf, len, anchor, sizeof(anchor))) == (size_t)-1)
        TEST_ERROR;
    /* size field starts one byte past the "fill defined" byte: 0x00000000 ->
     * 0xFF000000, which INT32DECODE reads as -16777216. */
    buf[off + 1] = 0x00;
    buf[off + 2] = 0x00;
    buf[off + 3] = 0x00;
    buf[off + 4] = 0xff;
    if (spew(BAD_FILL_FILE, buf, len) < 0)
        TEST_ERROR;

    free(buf);
    printf("Generated %s\n", BAD_FILL_FILE);
    return EXIT_SUCCESS;

error:
    free(buf);
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Fclose(file);
        H5Pclose(fapl);
    }
    H5E_END_TRY
    fprintf(stderr, "failed to generate %s\n", BAD_FILL_FILE);
    return EXIT_FAILURE;
}
