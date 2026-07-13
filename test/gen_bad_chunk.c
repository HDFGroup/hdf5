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
 * Generate an HDF5 file with a chunked dataset whose stored chunk layout
 * dimensionality does not match the dataset's dataspace rank.
 *
 * A valid 3-D chunked dataset of native int is written, then the version-3
 * chunk layout message's "dimensionality" byte is patched from 4 down to 3.
 * The stored chunk rank includes an extra element-size dimension, so a valid
 * 3-D dataset stores 4. Patching it to 3 makes the layout describe a 2-D
 * chunk over a 3-D dataspace.
 *
 */

#include "h5test.h"

#define BAD_CHUNK_FILE "bad_chunk_ndims.h5"
#define BAD_CHUNK_DSET "dset"

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
    hsize_t        dims[3]  = {3, 4, 5};
    hsize_t        chunk[3] = {2, 2, 4}; /* chunk edge lengths, in elements */
    int            data[3 * 4 * 5];
    unsigned char *buf = NULL;
    size_t         len, off;
    int            i;

    /* The version-3 chunk layout stores, after the 3-byte header (version,
     * class, dimensionality) and the 8-byte b-tree address, one little-endian
     * uint32 per stored dimension: the three chunk edge lengths (in elements)
     * followed by the element size in bytes.  For this dataset those on-disk
     * values are literally {2, 2, 4, 4} -- the three chunk edge lengths and a
     * trailing 4-byte element size; this byte pattern uniquely locates the
     * layout message. */
    static const unsigned char layout_dims[] = {2, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0};

    for (i = 0; i < 3 * 4 * 5; i++)
        data[i] = i;

    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;
    if ((file = H5Fcreate(BAD_CHUNK_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((sid = H5Screate_simple(3, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 3, chunk) < 0)
        TEST_ERROR;

    if ((dset = H5Dcreate2(file, BAD_CHUNK_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0 || H5Pclose(dcpl) < 0 || H5Sclose(sid) < 0 || H5Fclose(file) < 0 ||
        H5Pclose(fapl) < 0)
        TEST_ERROR;

    /* Patch the chunk layout's dimensionality byte from 4 to 3. */
    if (NULL == (buf = slurp(BAD_CHUNK_FILE, &len)))
        TEST_ERROR;
    if ((off = find_once(buf, len, layout_dims, sizeof(layout_dims))) == (size_t)-1)
        TEST_ERROR;
    /* Layout header is 3 bytes (version, class, ndims) + 8-byte address before
     * the chunk sizes, so the ndims byte is 11 bytes before the sizes.
     *
     * Patching ndims from 4 to 3 makes the layout describe a 2-D chunk over the
     * 3-D dataspace.  The decoder then reads only the first three stored sizes,
     * {2, 2, 4}, and treats the last of those (4) as the element-size
     * dimension.  Because the dataset's third chunk edge was chosen to be 4,
     * that reinterpreted element size still matches the stored 4-byte element
     * size, so the layout decodes consistently and the dataset opens instead
     * of being rejected by the element-size check -- which is what let the
     * original bug reach the I/O path and crash. */
    if (off < 11 || buf[off - 11] != 3 /* version */ || buf[off - 10] != 2 /* chunked */ ||
        buf[off - 9] != 4 /* ndims */)
        TEST_ERROR;
    buf[off - 9] = 3;
    if (spew(BAD_CHUNK_FILE, buf, len) < 0)
        TEST_ERROR;

    free(buf);
    printf("Generated %s\n", BAD_CHUNK_FILE);
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
    fprintf(stderr, "failed to generate %s\n", BAD_CHUNK_FILE);
    return EXIT_FAILURE;
}
