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
 * Author: Albert Cheng of NCSA, May 1, 2001.
 * This is derived from code given to me by Robert Ross.
 *
 * NOTE: This code assumes that all command line arguments make it out to all
 * the processes that make up the parallel job, which isn't always the case.
 * So if it doesn't work on some platform, that might be why.
 */

#include "hdf5.h"
#include "H5private.h"

#ifdef H5_HAVE_PARALLEL

#ifdef H5_STDC_HEADERS
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#endif

#ifdef H5_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if defined(H5_TIME_WITH_SYS_TIME)
#include <sys/time.h>
#include <time.h>
#elif defined(H5_HAVE_SYS_TIME_H)
#include <sys/time.h>
#else
#include <time.h>
#endif

#ifdef H5_HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef H5_HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <mpi.h>
#ifndef MPI_FILE_NULL /*MPIO may be defined in mpi.h already       */
#include <mpio.h>
#endif

/* Macro definitions */
/* Verify:
 * if val is false (0), print mesg and if fatal is true (non-zero), die.
 */
#define H5FATAL 1
#define VRFY(val, mesg, fatal)                                                                               \
    do {                                                                                                     \
        if (!val) {                                                                                          \
            printf("Proc %d: ", mynod);                                                                      \
            printf("*** Assertion failed (%s) at line %4d in %s\n", mesg, (int)__LINE__, __FILE__);          \
            if (fatal) {                                                                                     \
                fflush(stdout);                                                                              \
                goto die_jar_jar_die;                                                                        \
            }                                                                                                \
        }                                                                                                    \
    } while (0)
#define RANK     1
#define MAX_PATH 1024

hsize_t dims[RANK]; /* dataset dim sizes */
hsize_t block[RANK], stride[RANK], count[RANK];
hsize_t start[RANK];
hid_t   fid;            /* HDF5 file ID */
hid_t   acc_tpl;        /* File access templates */
hid_t   sid;            /* Dataspace ID */
hid_t   file_dataspace; /* File dataspace ID */
hid_t   mem_dataspace;  /* memory dataspace ID */
hid_t   dataset;        /* Dataset ID */
hsize_t opt_alignment = 1;
hsize_t opt_threshold = 1;
int     opt_split_vfd = 0;
char *  meta_ext, *raw_ext; /* holds the meta and raw file extension if */
                            /* opt_split_vfd is set */

/* DEFAULT VALUES FOR OPTIONS */
int64_t opt_block        = 1048576 * 16;
int     opt_iter         = 1;
int     opt_stripe       = -1;
int     opt_correct      = 0;
int     amode            = O_RDWR | O_CREAT;
char    opt_file[256]    = "perftest.out";
char    opt_pvfstab[256] = "notset";
int     opt_pvfstab_set  = 0;

const char *FILENAME[] = {opt_file, NULL};

/* function prototypes */
static int parse_args(int argc, char **argv);

#ifndef H5_HAVE_UNISTD_H
/* globals needed for getopt */
extern char *optarg;
#endif

#ifndef HDF5_PARAPREFIX
#define HDF5_PARAPREFIX ""
#endif
char *   paraprefix   = NULL;          /* for command line option para-prefix */
MPI_Info h5_io_info_g = MPI_INFO_NULL; /* MPI INFO object for IO */

static char *h5_fixname_real(const char *base_name, hid_t fapl, const char *_suffix, char *fullname,
                             size_t size, hbool_t nest_printf, hbool_t subst_for_superblock);

int
main(int argc, char **argv)
{
    char *  buf, *tmp, *buf2 = NULL, *tmp2 = NULL, *check;
    int     i, j, mynod = 0, nprocs = 1, my_correct = 1, correct, myerrno;
    double  stim, etim;
    double  write_tim = 0;
    double  read_tim  = 0;
    double  read_bw, write_bw;
    double  max_read_tim, max_write_tim;
    double  min_read_tim, min_write_tim;
    double  ave_read_tim, ave_write_tim;
    int64_t iter_jump = 0;
    char    filename[MAX_PATH];
    herr_t  ret; /* Generic return value */

    /* startup MPI and determine the rank of this process */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &mynod);

    /* parse the command line arguments */
    parse_args(argc, argv);

    if (mynod == 0)
        printf("# Using hdf5-io calls.\n");

#ifdef H5_HAVE_UNISTD_H
    /* Kind of a weird hack- if the location of the pvfstab file was
     * specified on the command line, then spit out this location into
     * the appropriate environment variable.
     */
    if (opt_pvfstab_set) {
        if ((setenv("PVFSTAB_FILE", opt_pvfstab, 1)) < 0) {
            perror("setenv");
            goto die_jar_jar_die;
        }
    }
#endif

    /* this is how much of the file data is covered on each iteration of
     * the test.  used to help determine the seek offset on each
     * iteration */
    iter_jump = nprocs * opt_block;

    /* setup a buffer of data to write */
    if (!(tmp = (char *)malloc((size_t)opt_block + 256))) {
        perror("malloc");
        goto die_jar_jar_die;
    }
    buf = tmp + 128 - (((long)tmp) % 128); /* align buffer */

    if (opt_correct) {
        /* do the same buffer setup for verifiable data */
        if (!(tmp2 = (char *)malloc((size_t)opt_block + 256))) {
            perror("malloc2");
            goto die_jar_jar_die;
        }
        buf2 = tmp + 128 - (((long)tmp) % 128);
    }

    /* setup file access template with parallel IO access. */
    if (opt_split_vfd) {
        hid_t mpio_pl;

        mpio_pl = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((acc_tpl >= 0), "", H5FATAL);
        ret = H5Pset_fapl_mpio(mpio_pl, MPI_COMM_WORLD, MPI_INFO_NULL);
        VRFY((ret >= 0), "", H5FATAL);

        /* set optional allocation alignment */
        if (opt_alignment * opt_threshold != 1) {
            ret = H5Pset_alignment(acc_tpl, opt_threshold, opt_alignment);
            VRFY((ret >= 0), "H5Pset_alignment succeeded", !H5FATAL);
        }

        /* setup file access template */
        acc_tpl = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((acc_tpl >= 0), "", H5FATAL);
        ret = H5Pset_fapl_split(acc_tpl, meta_ext, mpio_pl, raw_ext, mpio_pl);
        VRFY((ret >= 0), "H5Pset_fapl_split succeeded", H5FATAL);
        ret = H5Pclose(mpio_pl);
        VRFY((ret >= 0), "H5Pclose mpio_pl succeeded", H5FATAL);
    }
    else {
        /* setup file access template */
        acc_tpl = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((acc_tpl >= 0), "", H5FATAL);
        ret = H5Pset_fapl_mpio(acc_tpl, MPI_COMM_WORLD, MPI_INFO_NULL);
        VRFY((ret >= 0), "", H5FATAL);

        /* set optional allocation alignment */
        if (opt_alignment * opt_threshold != 1) {
            ret = H5Pset_alignment(acc_tpl, opt_threshold, opt_alignment);
            VRFY((ret >= 0), "H5Pset_alignment succeeded", !H5FATAL);
        }
    }

    h5_fixname_real(FILENAME[0], acc_tpl, NULL, filename, sizeof filename, FALSE, FALSE);

    /* create the parallel file */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded", H5FATAL);

    /* define a contiquous dataset of opt_iter*nprocs*opt_block chars */
    dims[0] = (hsize_t)opt_iter * (hsize_t)nprocs * (hsize_t)opt_block;
    sid     = H5Screate_simple(RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple succeeded", H5FATAL);
    dataset = H5Dcreate2(fid, "Dataset1", H5T_NATIVE_CHAR, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2 succeeded", H5FATAL);

    /* create the memory dataspace and the file dataspace */
    dims[0]       = (hsize_t)opt_block;
    mem_dataspace = H5Screate_simple(RANK, dims, NULL);
    VRFY((mem_dataspace >= 0), "", H5FATAL);
    file_dataspace = H5Dget_space(dataset);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded", H5FATAL);

    /* now each process writes a block of opt_block chars in round robbin
     * fashion until the whole dataset is covered.
     */
    for (j = 0; j < opt_iter; j++) {
        /* setup a file dataspace selection */
        start[0]  = (hsize_t)((j * iter_jump) + (mynod * opt_block));
        stride[0] = block[0] = (hsize_t)opt_block;
        count[0]             = 1;
        ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
        VRFY((ret >= 0), "H5Sset_hyperslab succeeded", H5FATAL);

        if (opt_correct) /* fill in buffer for iteration */ {
            for (i = mynod + j, check = buf; i < opt_block; i++, check++)
                *check = (char)i;
        }

        /* discover the starting time of the operation */
        MPI_Barrier(MPI_COMM_WORLD);
        stim = MPI_Wtime();

        /* write data */
        ret = H5Dwrite(dataset, H5T_NATIVE_CHAR, mem_dataspace, file_dataspace, H5P_DEFAULT, buf);
        VRFY((ret >= 0), "H5Dwrite dataset1 succeeded", !H5FATAL);

        /* discover the ending time of the operation */
        etim = MPI_Wtime();

        write_tim += (etim - stim);

        /* we are done with this "write" iteration */
    }

    /* close dataset and file */
    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose succeeded", H5FATAL);
    ret = H5Fclose(fid);
    VRFY((ret >= 0), "H5Fclose succeeded", H5FATAL);

    /* wait for everyone to synchronize at this point */
    MPI_Barrier(MPI_COMM_WORLD);

    /* reopen the file for reading */
    fid = H5Fopen(filename, H5F_ACC_RDONLY, acc_tpl);
    VRFY((fid >= 0), "", H5FATAL);

    /* open the dataset */
    dataset = H5Dopen2(fid, "Dataset1", H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dopen succeeded", H5FATAL);

    /* we can re-use the same mem_dataspace and file_dataspace
     * the H5Dwrite used since the dimension size is the same.
     */

    /* we are going to repeat the read the same pattern the write used */
    for (j = 0; j < opt_iter; j++) {
        /* setup a file dataspace selection */
        start[0]  = (hsize_t)((j * iter_jump) + (mynod * opt_block));
        stride[0] = block[0] = (hsize_t)opt_block;
        count[0]             = 1;
        ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
        VRFY((ret >= 0), "H5Sset_hyperslab succeeded", H5FATAL);
        /* seek to the appropriate spot give the current iteration and
         * rank within the MPI processes */

        /* discover the start time */
        MPI_Barrier(MPI_COMM_WORLD);
        stim = MPI_Wtime();

        /* read in the file data */
        if (!opt_correct) {
            ret = H5Dread(dataset, H5T_NATIVE_CHAR, mem_dataspace, file_dataspace, H5P_DEFAULT, buf);
        }
        else {
            ret = H5Dread(dataset, H5T_NATIVE_CHAR, mem_dataspace, file_dataspace, H5P_DEFAULT, buf2);
        }
        myerrno = errno;

        /* discover the end time */
        etim = MPI_Wtime();
        read_tim += (etim - stim);
        VRFY((ret >= 0), "H5Dwrite dataset1 succeeded", !H5FATAL);

        if (ret < 0)
            HDfprintf(stderr, "node %d, read error, loc = %" PRId64 ": %s\n", mynod, mynod * opt_block,
                      strerror(myerrno));

        /* if the user wanted to check correctness, compare the write
         * buffer to the read buffer */
        if (opt_correct && memcmp(buf, buf2, (size_t)opt_block)) {
            HDfprintf(stderr, "node %d, correctness test failed\n", mynod);
            my_correct = 0;
            MPI_Allreduce(&my_correct, &correct, 1, MPI_INT, MPI_MIN, MPI_COMM_WORLD);
        }

        /* we are done with this read iteration */
    }

    /* close dataset and file */
    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose succeeded", H5FATAL);
    ret = H5Fclose(fid);
    VRFY((ret >= 0), "H5Fclose succeeded", H5FATAL);
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "H5Pclose succeeded", H5FATAL);

    /* compute the read and write times */
    MPI_Allreduce(&read_tim, &max_read_tim, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);
    MPI_Allreduce(&read_tim, &min_read_tim, 1, MPI_DOUBLE, MPI_MIN, MPI_COMM_WORLD);
    MPI_Allreduce(&read_tim, &ave_read_tim, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);

    /* calculate the average from the sum */
    ave_read_tim = ave_read_tim / nprocs;

    MPI_Allreduce(&write_tim, &max_write_tim, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);
    MPI_Allreduce(&write_tim, &min_write_tim, 1, MPI_DOUBLE, MPI_MIN, MPI_COMM_WORLD);
    MPI_Allreduce(&write_tim, &ave_write_tim, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);

    /* calculate the average from the sum */
    ave_write_tim = ave_write_tim / nprocs;

    /* print out the results on one node */
    if (mynod == 0) {
        read_bw  = (double)((int64_t)(opt_block * nprocs * opt_iter)) / (max_read_tim * 1000000.0);
        write_bw = (double)((int64_t)(opt_block * nprocs * opt_iter)) / (max_write_tim * 1000000.0);

        printf("nr_procs = %d, nr_iter = %d, blk_sz = %ld\n", nprocs, opt_iter, (long)opt_block);

        printf("# total_size = %ld\n", (long)(opt_block * nprocs * opt_iter));

        printf("# Write:  min_time = %f, max_time = %f, mean_time = %f\n", min_write_tim, max_write_tim,
               ave_write_tim);
        printf("# Read:  min_time = %f, max_time = %f, mean_time = %f\n", min_read_tim, max_read_tim,
               ave_read_tim);

        printf("Write bandwidth = %f Mbytes/sec\n", write_bw);
        printf("Read bandwidth = %f Mbytes/sec\n", read_bw);

        if (opt_correct) {
            printf("Correctness test %s.\n", correct ? "passed" : "failed");
        }
    }

die_jar_jar_die:

#ifdef H5_HAVE_UNISTD
    /* Clear the environment variable if it was set earlier */
    if (opt_pvfstab_set) {
        unsetenv("PVFSTAB_FILE");
    }
#endif

    free(tmp);
    if (opt_correct)
        free(tmp2);

    MPI_Finalize();

    return (0);
}

static int
parse_args(int argc, char **argv)
{
    int c;

    while ((c = getopt(argc, argv, "s:b:i:f:p:a:2:c")) != EOF) {
        switch (c) {
            case 's': /* stripe */
                opt_stripe = atoi(optarg);
                break;
            case 'b': /* block size */
                opt_block = atoi(optarg);
                break;
            case 'i': /* iterations */
                opt_iter = atoi(optarg);
                break;
            case 'f': /* filename */
                strncpy(opt_file, optarg, 255);
                FILENAME[0] = opt_file;
                break;
            case 'p': /* pvfstab file */
                strncpy(opt_pvfstab, optarg, 255);
                opt_pvfstab_set = 1;
                break;
            case 'a': /* aligned allocation.
                       * syntax: -a<alignment>/<threshold>
                       * e.g., -a4096/512  allocate at 4096 bytes
                       * boundary if request size >= 512.
                       */
            {
                char *p;

                opt_alignment = (hsize_t)HDatoi(optarg);
                if (NULL != (p = (char *)HDstrchr(optarg, '/')))
                    opt_threshold = (hsize_t)HDatoi(p + 1);
            }
                HDfprintf(stdout, "alignment/threshold=%" PRIuHSIZE "/%" PRIuHSIZE "\n", opt_alignment,
                          opt_threshold);
                break;
            case '2': /* use 2-files, i.e., split file driver */
                opt_split_vfd = 1;
                /* get meta and raw file extension. */
                /* syntax is <raw_ext>,<meta_ext> */
                meta_ext = raw_ext = optarg;
                while (*raw_ext != '\0') {
                    if (*raw_ext == ',') {
                        *raw_ext = '\0';
                        raw_ext++;
                        break;
                    }
                    raw_ext++;
                }
                printf("split-file-vfd used: %s,%s\n", meta_ext, raw_ext);
                break;
            case 'c': /* correctness */
                opt_correct = 1;
                break;
            case '?': /* unknown */
            default:
                break;
        }
    }

    return (0);
}
/*-------------------------------------------------------------------------
 * Function:  getenv_all
 *
 * Purpose:  Used to get the environment that the root MPI task has.
 *     name specifies which environment variable to look for
 *     val is the string to which the value of that environment
 *     variable will be copied.
 *
 *     NOTE: The pointer returned by this function is only
 *     valid until the next call to getenv_all and the data
 *     stored there must be copied somewhere else before any
 *     further calls to getenv_all take place.
 *
 * Return:  pointer to a string containing the value of the environment variable
 *     NULL if the varialbe doesn't exist in task 'root's environment.
 *
 * Programmer:  Leon Arber
 *              4/4/05
 *
 * Modifications:
 *    Use original getenv if MPI is not initialized. This happens
 *    one uses the PHDF5 library to build a serial nature code.
 *    Albert 2006/04/07
 *
 *-------------------------------------------------------------------------
 */
char *
getenv_all(MPI_Comm comm, int root, const char *name)
{
    int          mpi_size, mpi_rank, mpi_initialized, mpi_finalized;
    int          len;
    static char *env = NULL;

    HDassert(name);

    MPI_Initialized(&mpi_initialized);
    MPI_Finalized(&mpi_finalized);

    if (mpi_initialized && !mpi_finalized) {
        MPI_Comm_rank(comm, &mpi_rank);
        MPI_Comm_size(comm, &mpi_size);
        HDassert(root < mpi_size);

        /* The root task does the getenv call
         * and sends the result to the other tasks */
        if (mpi_rank == root) {
            env = HDgetenv(name);
            if (env) {
                len = (int)HDstrlen(env);
                MPI_Bcast(&len, 1, MPI_INT, root, comm);
                MPI_Bcast(env, len, MPI_CHAR, root, comm);
            }
            else {
                /* len -1 indicates that the variable was not in the environment */
                len = -1;
                MPI_Bcast(&len, 1, MPI_INT, root, comm);
            }
        }
        else {
            MPI_Bcast(&len, 1, MPI_INT, root, comm);
            if (len >= 0) {
                if (env == NULL)
                    env = (char *)HDmalloc((size_t)len + 1);
                else if (HDstrlen(env) < (size_t)len)
                    env = (char *)HDrealloc(env, (size_t)len + 1);

                MPI_Bcast(env, len, MPI_CHAR, root, comm);
                env[len] = '\0';
            }
            else {
                if (env)
                    HDfree(env);
                env = NULL;
            }
        }
#ifndef NDEBUG
        MPI_Barrier(comm);
#endif
    }
    else {
        /* use original getenv */
        if (env)
            HDfree(env);
        env = HDgetenv(name);
    } /* end if */

    return env;
}

/*-------------------------------------------------------------------------
 * Function:  h5_fixname_real
 *
 * Purpose:  Create a file name from a file base name like `test' and
 *    return it through the FULLNAME (at most SIZE characters
 *    counting the null terminator). The full name is created by
 *    prepending the contents of HDF5_PREFIX (separated from the
 *    base name by a slash) and appending a file extension based on
 *    the driver supplied, resulting in something like
 *    `ufs:/u/matzke/test.h5'.
 *
 * Return:  Success:  The FULLNAME pointer.
 *
 *    Failure:  NULL if BASENAME or FULLNAME is the null
 *        pointer or if FULLNAME isn't large enough for
 *        the result.
 *
 * Programmer:  Robb Matzke
 *              Thursday, November 19, 1998
 *
 *-------------------------------------------------------------------------
 */
static char *
h5_fixname_real(const char *base_name, hid_t fapl, const char *_suffix, char *fullname, size_t size,
                hbool_t nest_printf, hbool_t subst_for_superblock)
{
    const char *prefix = NULL;
    const char *env    = NULL; /* HDF5_DRIVER environment variable     */
    char *      ptr, last = '\0';
    const char *suffix = _suffix;
    size_t      i, j;
    hid_t       driver     = -1;
    int         isppdriver = 0; /* if the driver is MPI parallel */

    if (!base_name || !fullname || size < 1)
        return NULL;

    HDmemset(fullname, 0, size);

    /* figure out the suffix */
    if (H5P_DEFAULT != fapl) {
        if ((driver = H5Pget_driver(fapl)) < 0)
            return NULL;

        if (suffix) {
            if (H5FD_FAMILY == driver) {
                if (subst_for_superblock)
                    suffix = "00000.h5";
                else
                    suffix = nest_printf ? "%%05d.h5" : "%05d.h5";
            }
            else if (H5FD_MULTI == driver) {

                /* Get the environment variable, if it exists, in case
                 * we are using the split driver since both of those
                 * use the multi VFD under the hood.
                 */
                env = HDgetenv("HDF5_DRIVER");
#ifdef HDF5_DRIVER
                /* Use the environment variable, then the compile-time constant */
                if (!env)
                    env = HDF5_DRIVER;
#endif
                if (env && !HDstrcmp(env, "split")) {
                    /* split VFD */
                    if (subst_for_superblock)
                        suffix = "-m.h5";
                    else
                        suffix = NULL;
                }
                else {
                    /* multi VFD */
                    if (subst_for_superblock)
                        suffix = "-s.h5";
                    else
                        suffix = NULL;
                }
            }
        }
    }

    /* Must first check fapl is not H5P_DEFAULT (-1) because H5FD_XXX
     * could be of value -1 if it is not defined.
     */
    isppdriver = H5P_DEFAULT != fapl && (H5FD_MPIO == driver);

    /* Check what prefix to use for test files. Process HDF5_PARAPREFIX and
     * HDF5_PREFIX.
     * Use different ones depending on parallel or serial driver used.
     * (The #ifdef is needed to prevent compile failure in case MPI is not
     * configured.)
     */
    if (isppdriver) {
        /*
         * For parallel:
         *      First use command line option, then the environment
         *      variable, then try the constant
         */
        static int explained = 0;

        prefix = (paraprefix ? paraprefix : getenv_all(MPI_COMM_WORLD, 0, "HDF5_PARAPREFIX"));

        if (!prefix && !explained) {
            /* print hint by process 0 once. */
            int mpi_rank;

            MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

            if (mpi_rank == 0)
                HDprintf("*** Hint ***\n"
                         "You can use environment variable HDF5_PARAPREFIX to "
                         "run parallel test files in a\n"
                         "different directory or to add file type prefix. e.g.,\n"
                         "   HDF5_PARAPREFIX=pfs:/PFS/user/me\n"
                         "   export HDF5_PARAPREFIX\n"
                         "*** End of Hint ***\n");

            explained = TRUE;
#ifdef HDF5_PARAPREFIX
            prefix = HDF5_PARAPREFIX;
#endif /* HDF5_PARAPREFIX */
        }
    }
    else {
        /*
         * For serial:
         *      First use the environment variable, then try the constant
         */
        prefix = HDgetenv("HDF5_PREFIX");

#ifdef HDF5_PREFIX
        if (!prefix)
            prefix = HDF5_PREFIX;
#endif /* HDF5_PREFIX */
    }

    /* Prepend the prefix value to the base name */
    if (prefix && *prefix) {
        if (isppdriver) {
            /* This is a parallel system */
            char *subdir;

            if (!HDstrcmp(prefix, HDF5_PARAPREFIX)) {
                /*
                 * If the prefix specifies the HDF5_PARAPREFIX directory, then
                 * default to using the "/tmp/$USER" or "/tmp/$LOGIN"
                 * directory instead.
                 */
                char *user, *login;

                user   = HDgetenv("USER");
                login  = HDgetenv("LOGIN");
                subdir = (user ? user : login);

                if (subdir) {
                    for (i = 0; i < size && prefix[i]; i++)
                        fullname[i] = prefix[i];

                    fullname[i++] = '/';

                    for (j = 0; i < size && subdir[j]; ++i, ++j)
                        fullname[i] = subdir[j];
                }
            }

            if (!fullname[0]) {
                /* We didn't append the prefix yet */
                HDstrncpy(fullname, prefix, size);
                fullname[size - 1] = '\0';
            }

            if (HDstrlen(fullname) + HDstrlen(base_name) + 1 < size) {
                /*
                 * Append the base_name with a slash first. Multiple
                 * slashes are handled below.
                 */
                h5_stat_t buf;

                if (HDstat(fullname, &buf) < 0)
                    /* The directory doesn't exist just yet */
                    if (HDmkdir(fullname, (mode_t)0755) < 0 && errno != EEXIST)
                        /*
                         * We couldn't make the "/tmp/${USER,LOGIN}"
                         * subdirectory.  Default to PREFIX's original
                         * prefix value.
                         */
                        HDstrcpy(fullname, prefix);

                HDstrcat(fullname, "/");
                HDstrcat(fullname, base_name);
            }
            else {
                /* Buffer is too small */
                return NULL;
            }
        }
        else {
            if (HDsnprintf(fullname, size, "%s/%s", prefix, base_name) == (int)size)
                /* Buffer is too small */
                return NULL;
        }
    }
    else if (HDstrlen(base_name) >= size) {
        /* Buffer is too small */
        return NULL;
    }
    else {
        HDstrcpy(fullname, base_name);
    }

    /* Append a suffix */
    if (suffix) {
        if (HDstrlen(fullname) + HDstrlen(suffix) >= size)
            return NULL;

        HDstrcat(fullname, suffix);
    }

    /* Remove any double slashes in the filename */
    for (ptr = fullname, i = j = 0; ptr && i < size; i++, ptr++) {
        if (*ptr != '/' || last != '/')
            fullname[j++] = *ptr;

        last = *ptr;
    }

    return fullname;
}

/*
 * Local variables:
 *  c-indent-level: 3
 *  c-basic-offset: 3
 *  tab-width: 3
 * End:
 */

#else  /* H5_HAVE_PARALLEL */
/* dummy program since H5_HAVE_PARALLEL is not configured in */
int
main(int H5_ATTR_UNUSED argc, char H5_ATTR_UNUSED **argv)
{
    printf("No parallel performance because parallel is not configured in\n");
    return (0);
}
#endif /* H5_HAVE_PARALLEL */
