/* Benchmark: measure per-chunk read time over many iterations for a
 * deflate-compressed chunked dataset.  Intended to catch the Windows
 * heap-fragmentation regression where read times grow steadily across
 * iterations (issue #4481).
 *
 * Usage:
 *   chunk_deflate_perf [nchunks [chunk_dim [passes]]]
 *     nchunks   – number of 1-D chunks to write (default 500)
 *     chunk_dim – elements per chunk edge, chunk is chunk_dim^3 int32s
 *                 (default 64 → 64*64*64*4 = 1 MiB per chunk)
 *     passes    – how many full read passes to run (default 5)
 *
 * Build (standalone, outside CMake):
 *   cc -O2 -o chunk_deflate_perf chunk_deflate_perf.c \
 *       $(pkg-config --cflags --libs hdf5) -lm
 *
 * Interpretation:
 *   With the fix, per-chunk time should be roughly constant across passes.
 *   Without the fix (or on a regressed build), times trend upward.
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "hdf5.h"

#define FILENAME "chunk_deflate_perf.h5"
#define DSET     "data"

static double
now_sec(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + (double)ts.tv_nsec * 1e-9;
}

static int
create_dataset(hid_t file_id, hsize_t nchunks, hsize_t cdim)
{
    hsize_t dims[4]       = {nchunks, cdim, cdim, cdim};
    hsize_t chunk_dims[4] = {1, cdim, cdim, cdim};
    hid_t   space_id      = H5I_INVALID_HID;
    hid_t   dcpl_id       = H5I_INVALID_HID;
    hid_t   dset_id       = H5I_INVALID_HID;
    int    *buf           = NULL;
    hsize_t chunk_elems   = cdim * cdim * cdim;
    int     ret           = -1;
    hsize_t i;

    buf = (int *)malloc(chunk_elems * sizeof(int));
    if (!buf) {
        fprintf(stderr, "malloc failed\n");
        return -1;
    }
    /* Patterned data that compresses well (ratio ~4:1) */
    for (hsize_t j = 0; j < chunk_elems; j++)
        buf[j] = (int)(j % 256);

    space_id = H5Screate_simple(4, dims, NULL);
    dcpl_id  = H5Pcreate(H5P_DATASET_CREATE);
    H5Pset_chunk(dcpl_id, 4, chunk_dims);
    H5Pset_deflate(dcpl_id, 6);

    dset_id = H5Dcreate2(file_id, DSET, H5T_NATIVE_INT, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    if (dset_id < 0) {
        fprintf(stderr, "H5Dcreate2 failed\n");
        goto done;
    }

    /* Write one chunk at a time */
    for (i = 0; i < nchunks; i++) {
        hsize_t start[4]  = {i, 0, 0, 0};
        hsize_t count[4]  = {1, cdim, cdim, cdim};
        hid_t   fspace_id = H5Dget_space(dset_id);
        hid_t   mspace_id = H5Screate_simple(4, count, NULL);

        H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL);
        /* Vary data per chunk so each chunk has a different compressed size */
        for (hsize_t j = 0; j < chunk_elems; j++)
            buf[j] = (int)((j + i * 13) % 256);
        H5Dwrite(dset_id, H5T_NATIVE_INT, mspace_id, fspace_id, H5P_DEFAULT, buf);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
    }
    ret = 0;

done:
    if (dset_id >= 0)
        H5Dclose(dset_id);
    H5Pclose(dcpl_id);
    H5Sclose(space_id);
    free(buf);
    return ret;
}

static int
run_read_pass(hid_t file_id, hsize_t nchunks, hsize_t cdim, double *times_out)
{
    hid_t   dset_id     = H5I_INVALID_HID;
    int    *buf         = NULL;
    hsize_t chunk_elems = cdim * cdim * cdim;
    int     ret         = -1;

    buf = (int *)malloc(chunk_elems * sizeof(int));
    if (!buf) {
        fprintf(stderr, "malloc failed\n");
        return -1;
    }

    dset_id = H5Dopen2(file_id, DSET, H5P_DEFAULT);
    if (dset_id < 0) {
        fprintf(stderr, "H5Dopen2 failed\n");
        goto done;
    }

    for (hsize_t i = 0; i < nchunks; i++) {
        hsize_t start[4]  = {i, 0, 0, 0};
        hsize_t count[4]  = {1, cdim, cdim, cdim};
        hid_t   fspace_id = H5Dget_space(dset_id);
        hid_t   mspace_id = H5Screate_simple(4, count, NULL);
        double  t0, t1;

        H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL);

        t0 = now_sec();
        H5Dread(dset_id, H5T_NATIVE_INT, mspace_id, fspace_id, H5P_DEFAULT, buf);
        t1 = now_sec();

        times_out[i] = (t1 - t0) * 1e6; /* µs */

        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
    }
    ret = 0;

done:
    if (dset_id >= 0)
        H5Dclose(dset_id);
    free(buf);
    return ret;
}

static int
cmp_double(const void *a, const void *b)
{
    double da = *(const double *)a;
    double db = *(const double *)b;
    return (da > db) - (da < db);
}

static void
print_stats(const char *label, const double *t, hsize_t n)
{
    double *sorted = (double *)malloc(n * sizeof(double));
    double  sum    = 0.0;
    double  med;

    memcpy(sorted, t, n * sizeof(double));
    qsort(sorted, n, sizeof(double), cmp_double);
    for (hsize_t i = 0; i < n; i++)
        sum += t[i];
    med = (n % 2 == 0) ? (sorted[n / 2 - 1] + sorted[n / 2]) / 2.0 : sorted[n / 2];

    printf("  %-20s  min=%7.1f µs  median=%7.1f µs  mean=%7.1f µs  max=%7.1f µs\n", label, sorted[0], med,
           sum / (double)n, sorted[n - 1]);
    free(sorted);
}

int
main(int argc, char **argv)
{
    hsize_t nchunks = 500;
    hsize_t cdim    = 64;
    int     passes  = 5;
    hid_t   file_id = H5I_INVALID_HID;
    double *times   = NULL;
    char    label[32];
    int     ret = EXIT_FAILURE;

    if (argc > 1)
        nchunks = (hsize_t)atol(argv[1]);
    if (argc > 2)
        cdim = (hsize_t)atol(argv[2]);
    if (argc > 3)
        passes = atoi(argv[3]);

    printf("chunk_deflate_perf: nchunks=%llu  chunk_dim=%llu (%llu MiB/chunk)  passes=%d\n",
           (unsigned long long)nchunks, (unsigned long long)cdim,
           (unsigned long long)(cdim * cdim * cdim * sizeof(int)) / (1024 * 1024), passes);

    times = (double *)malloc(nchunks * sizeof(double));
    if (!times) {
        fprintf(stderr, "malloc failed\n");
        return EXIT_FAILURE;
    }

    /* Create file and write dataset */
    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (file_id < 0) {
        fprintf(stderr, "H5Fcreate failed\n");
        goto done;
    }
    printf("Writing %llu chunks ...\n", (unsigned long long)nchunks);
    if (create_dataset(file_id, nchunks, cdim) < 0)
        goto done;
    H5Fclose(file_id);

    /* Re-open read-only so the chunk cache is empty each time */
    for (int p = 0; p < passes; p++) {
        file_id = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
        if (file_id < 0) {
            fprintf(stderr, "H5Fopen failed (pass %d)\n", p);
            goto done;
        }
        if (run_read_pass(file_id, nchunks, cdim, times) < 0)
            goto done;
        H5Fclose(file_id);
        file_id = H5I_INVALID_HID;

        snprintf(label, sizeof(label), "pass %d", p + 1);
        print_stats(label, times, nchunks);
    }

    /* Also print first-quarter vs last-quarter medians to highlight drift */
    {
        hsize_t q = nchunks / 4;
        if (q > 1) {
            printf("\n  (Re-running pass %d split into quarters to show drift)\n", passes);
            file_id = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
            if (file_id < 0) {
                fprintf(stderr, "H5Fopen failed\n");
                goto done;
            }
            if (run_read_pass(file_id, nchunks, cdim, times) < 0)
                goto done;
            H5Fclose(file_id);
            file_id = H5I_INVALID_HID;

            print_stats("Q1 (chunks 0..q-1)", times, q);
            print_stats("Q4 (chunks 3q..n-1)", times + 3 * q, nchunks - 3 * q);
        }
    }

    ret = EXIT_SUCCESS;
    printf("\nDone. File: %s\n", FILENAME);

done:
    if (file_id >= 0)
        H5Fclose(file_id);
    free(times);
    return ret;
}
