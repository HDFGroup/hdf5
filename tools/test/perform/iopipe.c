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
 *              Thursday, March 12, 1998
 */

/* See H5private.h for how to include headers */
#include "hdf5.h"

#include "H5private.h"

#define RAW_FILE_NAME  "iopipe.raw"
#define HDF5_FILE_NAME "iopipe.h5"
#define HEADING        "%-16s"
#define PROGRESS       '='

#if 0
/* Normal testing */
#define REQUEST_SIZE_X  4579
#define REQUEST_SIZE_Y  4579
#define NREAD_REQUESTS  45
#define NWRITE_REQUESTS 45
#else
/* Speedy testing */
#define REQUEST_SIZE_X  1000
#define REQUEST_SIZE_Y  1000
#define NREAD_REQUESTS  45
#define NWRITE_REQUESTS 45
#endif

/*-------------------------------------------------------------------------
 * Function:  print_stats
 *
 * Purpose:  Prints statistics
 *
 * Return:  void
 *
 * Programmer:  Robb Matzke
 *              Thursday, March 12, 1998
 *
 *-------------------------------------------------------------------------
 */
static void
print_stats(const char *prefix,
#ifdef H5_HAVE_GETRUSAGE
            struct rusage *r_start, struct rusage *r_stop,
#endif /* H5_HAVE_GETRUSAGE */
            double t_start, double t_stop, size_t nbytes)
{
    double e_time;
    char   bw[16];
#ifdef H5_HAVE_GETRUSAGE
    double u_time, s_time;

    u_time = ((double)(r_stop->ru_utime.tv_sec) + (double)(r_stop->ru_utime.tv_usec) / (double)1000000.0F) -
             ((double)(r_start->ru_utime.tv_sec) + (double)(r_start->ru_utime.tv_usec) / (double)1000000.0F);

    s_time = ((double)(r_stop->ru_stime.tv_sec) + (double)(r_stop->ru_stime.tv_usec) / (double)1000000.0F) -
             ((double)(r_start->ru_stime.tv_sec) + (double)(r_start->ru_stime.tv_usec) / (double)1000000.0F);
#endif
    e_time = t_stop - t_start;
    H5_bandwidth(bw, (double)nbytes, e_time);

#ifdef H5_HAVE_GETRUSAGE
    HDprintf(HEADING "%1.2fuser %1.2fsystem %1.2felapsed %s\n", prefix, u_time, s_time, e_time, bw);
#else
    HDprintf(HEADING "%1.2felapsed %s\n", prefix, e_time, bw);
#endif
}

/*-------------------------------------------------------------------------
 * Function:  synchronize
 *
 * Purpose:
 *
 * Return:  void
 *
 * Programmer:  Robb Matzke
 *              Thursday, March 12, 1998
 *
 *-------------------------------------------------------------------------
 */
static void
synchronize(void)
{
#ifdef H5_HAVE_SYSTEM
#if defined(H5_HAVE_WIN32_API) && !defined(__CYGWIN__)
    _flushall();
#else
    int H5_ATTR_NDEBUG_UNUSED status;

    status = HDsystem("sync");
    HDassert(status >= 0);

    status = HDsystem("df >/dev/null");
    HDassert(status >= 0);
#endif
#endif
}

/*-------------------------------------------------------------------------
 * Function:  main
 *
 * Purpose:
 *
 * Return:  Success:
 *
 *    Failure:
 *
 * Programmer:  Robb Matzke
 *              Thursday, March 12, 1998
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hsize_t  size[2] = {REQUEST_SIZE_X, REQUEST_SIZE_Y};
    unsigned nread = NREAD_REQUESTS, nwrite = NWRITE_REQUESTS;

    unsigned char *the_data = NULL;
    hid_t          file, dset, file_space = H5I_INVALID_HID;
#ifdef H5_HAVE_GETRUSAGE
    struct rusage r_start, r_stop;
#endif
    double                         t_start, t_stop;
    int                            fd;
    unsigned                       u;
    herr_t H5_ATTR_NDEBUG_UNUSED   status;
    hssize_t H5_ATTR_NDEBUG_UNUSED n;
    off_t H5_ATTR_NDEBUG_UNUSED    offset;
    hsize_t                        start[2];
    hsize_t                        count[2];

    /*
     * The extra cast in the following statement is a bug workaround for the
     * Win32 version 5.0 compiler.
     * 1998-11-06 ptl
     */
    HDprintf("I/O request size is %1.1fMB\n",
             (double)(hssize_t)(size[0] * size[1]) / (double)1024.0F * (double)1024);

    /* Open the files */
    file = H5Fcreate(HDF5_FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(file >= 0);
    fd = HDopen(RAW_FILE_NAME, O_RDWR | O_CREAT | O_TRUNC, 0666);
    HDassert(fd >= 0);

    /* Create the dataset */
    file_space = H5Screate_simple(2, size, size);
    HDassert(file_space >= 0);
    dset = H5Dcreate2(file, "dset", H5T_NATIVE_UCHAR, file_space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(dset >= 0);
    the_data = (unsigned char *)HDmalloc((size_t)(size[0] * size[1]));

    /* initial fill for lazy malloc */
    HDmemset(the_data, 0xAA, (size_t)(size[0] * size[1]));

    /* Fill raw */
    synchronize();
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_start);
#endif
    t_start = H5_get_time();
    HDfprintf(stderr, HEADING, "fill raw");
    for (u = 0; u < nwrite; u++) {
        HDputc(PROGRESS, stderr);
        HDfflush(stderr);
        HDmemset(the_data, 0xAA, (size_t)(size[0] * size[1]));
    }
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_stop);
#endif
    t_stop = H5_get_time();
    HDputc('\n', stderr);
    print_stats("fill raw",
#ifdef H5_HAVE_GETRUSAGE
                &r_start, &r_stop,
#endif /* H5_HAVE_GETRUSAGE */
                t_start, t_stop, (size_t)(nread * size[0] * size[1]));

    /* Fill hdf5 */
    synchronize();
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_start);
#endif
    t_start = H5_get_time();
    HDfprintf(stderr, HEADING, "fill hdf5");
    for (u = 0; u < nread; u++) {
        HDputc(PROGRESS, stderr);
        HDfflush(stderr);
        status = H5Dread(dset, H5T_NATIVE_UCHAR, file_space, file_space, H5P_DEFAULT, the_data);
        HDassert(status >= 0);
    }
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_stop);
#endif
    t_stop = H5_get_time();
    HDputc('\n', stderr);
    print_stats("fill hdf5",
#ifdef H5_HAVE_GETRUSAGE
                &r_start, &r_stop,
#endif /* H5_HAVE_GETRUSAGE */
                t_start, t_stop, (size_t)(nread * size[0] * size[1]));

    /* Write the raw dataset */
    synchronize();
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_start);
#endif
    t_start = H5_get_time();
    HDfprintf(stderr, HEADING, "out raw");
    for (u = 0; u < nwrite; u++) {
        HDputc(PROGRESS, stderr);
        HDfflush(stderr);
        offset = HDlseek(fd, (off_t)0, SEEK_SET);
        HDassert(0 == offset);
        n = HDwrite(fd, the_data, (size_t)(size[0] * size[1]));
        HDassert(n >= 0 && (size_t)n == (size[0] * size[1]));
    }
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_stop);
#endif
    t_stop = H5_get_time();
    HDputc('\n', stderr);
    print_stats("out raw",
#ifdef H5_HAVE_GETRUSAGE
                &r_start, &r_stop,
#endif /* H5_HAVE_GETRUSAGE */
                t_start, t_stop, (size_t)(nread * size[0] * size[1]));

    /* Write the hdf5 dataset */
    synchronize();
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_start);
#endif
    t_start = H5_get_time();
    HDfprintf(stderr, HEADING, "out hdf5");
    for (u = 0; u < nwrite; u++) {
        HDputc(PROGRESS, stderr);
        HDfflush(stderr);
        status = H5Dwrite(dset, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, the_data);
        HDassert(status >= 0);
    }
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_stop);
#endif
    t_stop = H5_get_time();
    HDputc('\n', stderr);
    print_stats("out hdf5",
#ifdef H5_HAVE_GETRUSAGE
                &r_start, &r_stop,
#endif /* H5_HAVE_GETRUSAGE */
                t_start, t_stop, (size_t)(nread * size[0] * size[1]));

    /* Read the raw dataset */
    synchronize();
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_start);
#endif
    t_start = H5_get_time();
    HDfprintf(stderr, HEADING, "in raw");
    for (u = 0; u < nread; u++) {
        HDputc(PROGRESS, stderr);
        HDfflush(stderr);
        offset = HDlseek(fd, (off_t)0, SEEK_SET);
        HDassert(0 == offset);
        n = HDread(fd, the_data, (size_t)(size[0] * size[1]));
        HDassert(n >= 0 && (size_t)n == (size[0] * size[1]));
    }
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_stop);
#endif
    t_stop = H5_get_time();
    HDputc('\n', stderr);
    print_stats("in raw",
#ifdef H5_HAVE_GETRUSAGE
                &r_start, &r_stop,
#endif /* H5_HAVE_GETRUSAGE */
                t_start, t_stop, (size_t)(nread * size[0] * size[1]));

    /* Read the hdf5 dataset */
    synchronize();
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_start);
#endif
    t_start = H5_get_time();
    HDfprintf(stderr, HEADING, "in hdf5");
    for (u = 0; u < nread; u++) {
        HDputc(PROGRESS, stderr);
        HDfflush(stderr);
        status = H5Dread(dset, H5T_NATIVE_UCHAR, file_space, file_space, H5P_DEFAULT, the_data);
        HDassert(status >= 0);
    }
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_stop);
#endif
    t_stop = H5_get_time();
    HDputc('\n', stderr);
    print_stats("in hdf5",
#ifdef H5_HAVE_GETRUSAGE
                &r_start, &r_stop,
#endif /* H5_HAVE_GETRUSAGE */
                t_start, t_stop, (size_t)(nread * size[0] * size[1]));

    /* Read hyperslab */
    HDassert(size[0] > 20 && size[1] > 20);
    start[0] = start[1] = 10;
    count[0] = count[1] = size[0] - 20;
    status              = H5Sselect_hyperslab(file_space, H5S_SELECT_SET, start, NULL, count, NULL);
    HDassert(status >= 0);
    synchronize();
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_start);
#endif
    t_start = H5_get_time();
    HDfprintf(stderr, HEADING, "in hdf5 partial");
    for (u = 0; u < nread; u++) {
        HDputc(PROGRESS, stderr);
        HDfflush(stderr);
        status = H5Dread(dset, H5T_NATIVE_UCHAR, file_space, file_space, H5P_DEFAULT, the_data);
        HDassert(status >= 0);
    }
#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage(RUSAGE_SELF, &r_stop);
#endif
    t_stop = H5_get_time();
    HDputc('\n', stderr);
    print_stats("in hdf5 partial",
#ifdef H5_HAVE_GETRUSAGE
                &r_start, &r_stop,
#endif /* H5_HAVE_GETRUSAGE */
                t_start, t_stop, (size_t)(nread * size[0] * size[1]));

    /* Close everything */
    HDclose(fd);

    H5Dclose(dset);
    H5Sclose(file_space);
    H5Fclose(file);

    HDfree(the_data);

    return 0;
}
