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
 * Generate HDF5 files with deliberately malformed filter metadata, used by
 * the test_filter_bad_params() regression test in dsets.c.
 *
 * Each file contains a single chunked, filtered dataset whose on-disk filter
 * metadata has been corrupted (by patching a few bytes after the file was
 * written with the public API) so that reading the dataset exercises a filter
 * decode path that used to crash:
 *
 *   bad_nbit_params.h5      The N-Bit filter's stored client-data parameter
 *                           count is set to 0, leaving cd_values == NULL at
 *                           filter time (GitHub issue #6489 "Null_Pointer").
 *
 *   bad_nbit_decompress.h5  The N-Bit filter's stored element count (cd[2]) is
 *                           inflated so decompression walks past the end of the
 *                           small compressed chunk (GitHub issue #6489
 *                           "Heap_Corruption_1").
 *
 *   bad_fletcher32.h5       A Fletcher32-filtered chunk's stored size is set to
 *                           2 (smaller than the 4-byte trailing checksum), so
 *                           the filter computes nbytes - FLETCHER_LEN and
 *                           underflows (GitHub issue #6490 "Heap_Corruption_2").
 *                           A value of 2 (rather than 0) is used so the file is
 *                           also exercisable in builds with assertions enabled,
 *                           which otherwise trip the chunk B-tree's
 *                           "nbytes > 0" sanity assertion before the filter
 *                           runs.
 *
 * The files use the earliest library version bounds so the filter pipeline
 * message lives in a version-1 (un-checksummed) object header; this lets us
 * patch the pipeline parameters without having to recompute an object-header
 * checksum.
 */

#include "h5test.h"

#define NBIT_PARAMS_FILE     "bad_nbit_params.h5"
#define NBIT_DECOMPRESS_FILE "bad_nbit_decompress.h5"
#define FLETCHER32_FILE      "bad_fletcher32.h5"

#define NBIT_DATASET      "Nbit_float_data_le"
#define FLETCHER32_DATASET "Fletcher_float_data_be"

#define NX   7
#define NY   6
#define RANK 2

/*-------------------------------------------------------------------------
 * Read an entire file into a newly allocated buffer.
 *-------------------------------------------------------------------------
 */
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

/* Store a little-endian unsigned value of the given width. */
static void
put_le(unsigned char *p, unsigned long long val, unsigned width)
{
    unsigned i;
    for (i = 0; i < width; i++)
        p[i] = (unsigned char)((val >> (8 * i)) & 0xff);
}

/*-------------------------------------------------------------------------
 * Create a single N-Bit-filtered float dataset matching the one produced by
 * gen_cross.c, then corrupt it two different ways.
 *-------------------------------------------------------------------------
 */
static int
create_nbit_files(void)
{
    hid_t          fapl = H5I_INVALID_HID, file = H5I_INVALID_HID, sid = H5I_INVALID_HID;
    hid_t          dcpl = H5I_INVALID_HID, dtype = H5I_INVALID_HID, dset = H5I_INVALID_HID;
    hsize_t        dims[RANK]  = {NX, NY};
    hsize_t        chunk[RANK] = {2, 3};
    float          data[NX][NY];
    float          fillvalue = -2.2f;
    unsigned char *buf       = NULL;
    size_t         len, anchor, cd_off;
    int            i, j;

    /* The N-Bit cd_values stored for this dataset:
     *   [0]=nparms(8) [1]=need_not_compress(0) [2]=nelmts(6) [3]=class(atomic)
     *   [4]=size(4)   [5]=order(LE)            [6]=precision(20) [7]=offset(7)
     */
    static const unsigned char cd_pattern[] = {8,  0, 0, 0, 0, 0, 0, 0, 6,  0, 0, 0, 1, 0, 0, 0,
                                               4,  0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 0, 7, 0, 0, 0};
    /* Version-1 pipeline filter record header for the N-Bit filter:
     *   filter id = 5, name length = 8, flags = 1, cd_nelmts = 8, name "nbit"
     */
    static const unsigned char filter_anchor[] = {5, 0, 8, 0, 1, 0, 8, 0, 'n', 'b', 'i', 't'};

    for (j = 0; j < NX; j++)
        for (i = 0; i < NY; i++)
            data[j][i] = ((float)(i + j + 1)) / 3;

    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;
    if ((file = H5Fcreate(NBIT_PARAMS_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((sid = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_nbit(dcpl) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, RANK, chunk) < 0)
        TEST_ERROR;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_FLOAT, &fillvalue) < 0)
        TEST_ERROR;

    /* 20-bit little-endian floating-point type (same as gen_cross.c) */
    if ((dtype = H5Tcopy(H5T_IEEE_F32LE)) < 0)
        TEST_ERROR;
    if (H5Tset_fields(dtype, (size_t)26, (size_t)20, (size_t)6, (size_t)7, (size_t)13) < 0)
        TEST_ERROR;
    if (H5Tset_offset(dtype, (size_t)7) < 0)
        TEST_ERROR;
    if (H5Tset_precision(dtype, (size_t)20) < 0)
        TEST_ERROR;
    if (H5Tset_size(dtype, (size_t)4) < 0)
        TEST_ERROR;
    if (H5Tset_ebias(dtype, (size_t)31) < 0)
        TEST_ERROR;

    if ((dset = H5Dcreate2(file, NBIT_DATASET, dtype, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0 || H5Tclose(dtype) < 0 || H5Pclose(dcpl) < 0 || H5Sclose(sid) < 0 ||
        H5Fclose(file) < 0 || H5Pclose(fapl) < 0)
        TEST_ERROR;

    /* Slurp the freshly written (clean) file once and locate the filter
     * record and its cd_values; both bad files are derived from this buffer. */
    if (NULL == (buf = slurp(NBIT_PARAMS_FILE, &len)))
        TEST_ERROR;
    if ((cd_off = find_once(buf, len, cd_pattern, sizeof(cd_pattern))) == (size_t)-1)
        TEST_ERROR;
    if ((anchor = find_once(buf, len, filter_anchor, sizeof(filter_anchor))) == (size_t)-1)
        TEST_ERROR;

    /* File 2 (decompress over-read): inflate cd[2] (element count, the third
     * 4-byte value of cd_values) on a copy of the clean buffer so the chunk is
     * too small for the claimed element count.  Done first, before the clean
     * buffer is mutated for file 1. */
    {
        unsigned char *copy = malloc(len);
        if (NULL == copy)
            TEST_ERROR;
        memcpy(copy, buf, len);
        put_le(copy + cd_off + 8, 100000, 4);
        if (spew(NBIT_DECOMPRESS_FILE, copy, len) < 0) {
            free(copy);
            TEST_ERROR;
        }
        free(copy);
    }

    /* File 1 (NULL parameters): zero the stored cd_nelmts (2 bytes at
     * anchor+6) so the filter is invoked with cd_nelmts == 0 / cd_values ==
     * NULL.  Overwrites the clean base file in place. */
    put_le(buf + anchor + 6, 0, 2);
    if (spew(NBIT_PARAMS_FILE, buf, len) < 0)
        TEST_ERROR;

    free(buf);
    return 0;

error:
    free(buf);
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Tclose(dtype);
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Fclose(file);
        H5Pclose(fapl);
    }
    H5E_END_TRY
    return -1;
}

/*-------------------------------------------------------------------------
 * Create a single Fletcher32-filtered float dataset, then set its one chunk's
 * stored size to 0 to trigger the checksum-length underflow.
 *-------------------------------------------------------------------------
 */
static int
create_fletcher32_file(void)
{
    hid_t          fapl = H5I_INVALID_HID, file = H5I_INVALID_HID, sid = H5I_INVALID_HID;
    hid_t          dcpl = H5I_INVALID_HID, dtype = H5I_INVALID_HID, dset = H5I_INVALID_HID;
    hsize_t        dims[RANK]  = {NX, NY};
    hsize_t        chunk[RANK] = {NX, NY}; /* single chunk holds the whole dataset */
    float          data[NX][NY];
    float          fillvalue = -2.2f;
    unsigned char *buf       = NULL;
    size_t         len, i;
    int            r, c;

    for (r = 0; r < NX; r++)
        for (c = 0; c < NY; c++)
            data[r][c] = ((float)(c + r + 1)) / 3;

    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;
    if ((file = H5Fcreate(FLETCHER32_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((sid = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_fletcher32(dcpl) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, RANK, chunk) < 0)
        TEST_ERROR;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_FLOAT, &fillvalue) < 0)
        TEST_ERROR;

    if ((dtype = H5Tcopy(H5T_IEEE_F32BE)) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, FLETCHER32_DATASET, dtype, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0 || H5Tclose(dtype) < 0 || H5Pclose(dcpl) < 0 || H5Sclose(sid) < 0 ||
        H5Fclose(file) < 0 || H5Pclose(fapl) < 0)
        TEST_ERROR;

    /* Find the raw-data chunk's version-1 B-tree node ("TREE" with a node type
     * byte of 1) and shrink the first key's chunk size to 2 (4 bytes at
     * node + 24: sig(4) + type(1) + level(1) + entries(2) + left(8) + right(8)).
     * 2 is smaller than the 4-byte Fletcher32 checksum, so the read underflows,
     * but it is still > 0 so assertion-enabled builds reach the filter. */
    if (NULL == (buf = slurp(FLETCHER32_FILE, &len)))
        TEST_ERROR;
    for (i = 0; i + 24 + 4 <= len; i++) {
        if (memcmp(buf + i, "TREE", 4) == 0 && buf[i + 4] == 1) {
            put_le(buf + i + 24, 2, 4);
            if (spew(FLETCHER32_FILE, buf, len) < 0)
                TEST_ERROR;
            free(buf);
            return 0;
        }
    }
    TEST_ERROR; /* no raw-data B-tree found */

error:
    free(buf);
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Tclose(dtype);
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Fclose(file);
        H5Pclose(fapl);
    }
    H5E_END_TRY
    return -1;
}

int
main(void)
{
    if (create_nbit_files() < 0) {
        fprintf(stderr, "failed to generate N-Bit bad-filter files\n");
        return EXIT_FAILURE;
    }
    if (create_fletcher32_file() < 0) {
        fprintf(stderr, "failed to generate Fletcher32 bad-filter file\n");
        return EXIT_FAILURE;
    }
    printf("Generated %s, %s, %s\n", NBIT_PARAMS_FILE, NBIT_DECOMPRESS_FILE, FLETCHER32_FILE);
    return EXIT_SUCCESS;
}
