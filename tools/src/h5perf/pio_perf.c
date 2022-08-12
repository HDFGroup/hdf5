/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Parallel HDF5 Performance Testing Code
 * --------------------------------------
 *
 * Portable code to test performance on the different platforms we support.
 * This is what the report should look like:
 *
 *  nprocs = Max#Procs
 *      IO API = POSIXIO
 *          # Files = 1, # of dsets = 1000, Elements per dset = 37000
 *              Write Results = x MB/s
 *              Read Results = x MB/s
 *          # Files = 1, # of dsets = 3000, Elements per dset = 37000
 *              Write Results = x MB/s
 *              Read Results = x MB/s
 *
 *          . . .
 *
 *      IO API = MPIO
 *          # Files = 1, # of dsets = 1000, Elements per dset = 37000
 *              Write Results = x MB/s
 *              Read Results = x MB/s
 *          # Files = 1, # of dsets = 3000, Elements per dset = 37000
 *              Write Results = x MB/s
 *              Read Results = x MB/s
 *
 *          . . .
 *
 *      IO API = PHDF5
 *          # Files = 1, # of dsets = 1000, Elements per dset = 37000
 *              Write Results = x MB/s
 *              Read Results = x MB/s
 *          # Files = 1, # of dsets = 3000, Elements per dset = 37000
 *              Write Results = x MB/s
 *              Read Results = x MB/s
 *
 *          . . .
 *
 *  nprocs = Max#Procs / 2
 *
 *      . . .
 *
 */

/* system header files */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "hdf5.h"

#ifdef H5_HAVE_PARALLEL

/* library header files */
#include <mpi.h>

/* our header files */
#include "pio_perf.h"

/* useful macros */
#define TAB_SPACE 4

#define ONE_KB 1024
#define ONE_MB (ONE_KB * ONE_KB)
#define ONE_GB (ONE_MB * ONE_KB)

#define PIO_POSIX 0x1
#define PIO_MPI   0x2
#define PIO_HDF5  0x4

/* report 0.0 in case t is zero too */
#define MB_PER_SEC(bytes, t) (H5_DBL_ABS_EQUAL((t), 0.0) ? 0.0 : ((((double)bytes) / ONE_MB) / (t)))

#ifndef TRUE
#define TRUE 1
#endif /* TRUE */
#ifndef FALSE
#define FALSE (!TRUE)
#endif /* FALSE */

/* global variables */
FILE    *output;              /* output file                          */
int      comm_world_rank_g;   /* my rank in MPI_COMM_RANK             */
int      comm_world_nprocs_g; /* num. of processes of MPI_COMM_WORLD  */
MPI_Comm pio_comm_g;          /* Communicator to run the PIO          */
int      pio_mpi_rank_g;      /* MPI rank of pio_comm_g               */
int      pio_mpi_nprocs_g;    /* Number of processes of pio_comm_g    */
int      pio_debug_level = 0; /* The debug level:
                               *   0 - Off
                               *   1 - Minimal
                               *   2 - Some more
                               *   3 - Maximal
                               *   4 - Maximal & then some
                               */

/* local variables */
static const char *progname = "h5perf";

#ifndef HDF5_PARAPREFIX
#define HDF5_PARAPREFIX ""
#endif
char    *paraprefix   = NULL;          /* for command line option para-prefix */
MPI_Info h5_io_info_g = MPI_INFO_NULL; /* MPI INFO object for IO */

/*
 * Command-line options: The user can specify short or long-named
 * parameters. The long-named ones can be partially spelled. When
 * adding more, make sure that they don't clash with each other.
 */
#if 1
static const char *s_opts = "a:A:B:cCd:D:e:F:ghi:Imno:p:P:stT:wx:X:";
#else
static const char *s_opts = "a:A:bB:cCd:D:e:F:ghi:Imno:p:P:stT:wx:X:";
#endif /* 1 */
static struct h5_long_options l_opts[] = {{"align", require_arg, 'a'},
                                          {"api", require_arg, 'A'},
#if 0
    /* a sighting of the elusive binary option */
    { "binary", no_arg, 'b' },
#endif /* 0 */
                                          {"block-size", require_arg, 'B'},
                                          {"chunk", no_arg, 'c'},
                                          {"collective", no_arg, 'C'},
                                          {"debug", require_arg, 'D'},
                                          {"geometry", no_arg, 'g'},
                                          {"help", no_arg, 'h'},
                                          {"interleaved", require_arg, 'I'},
                                          {"max-num-processes", require_arg, 'P'},
                                          {"min-num-processes", require_arg, 'p'},
                                          {"max-xfer-size", require_arg, 'X'},
                                          {"min-xfer-size", require_arg, 'x'},
                                          {"num-bytes", require_arg, 'e'},
                                          {"num-dsets", require_arg, 'd'},
                                          {"num-files", require_arg, 'F'},
                                          {"num-iterations", require_arg, 'i'},
                                          {"output", require_arg, 'o'},
                                          {"threshold", require_arg, 'T'},
                                          {"write-only", require_arg, 'w'},
                                          {NULL, 0, '\0'}};

struct options {
    long        io_types;      /* bitmask of which I/O types to test   */
    const char *output_file;   /* file to print report to              */
    long        num_dsets;     /* number of datasets                   */
    long        num_files;     /* number of files                      */
    off_t       num_bpp;       /* number of bytes per proc per dset    */
    int         num_iters;     /* number of iterations                 */
    int         max_num_procs; /* maximum number of processes to use   */
    int         min_num_procs; /* minimum number of processes to use   */
    size_t      max_xfer_size; /* maximum transfer buffer size         */
    size_t      min_xfer_size; /* minimum transfer buffer size         */
    size_t      blk_size;      /* Block size                           */
    unsigned    interleaved;   /* Interleaved vs. contiguous blocks    */
    unsigned    collective;    /* Collective vs. independent I/O       */
    unsigned    dim2d;         /* 1D vs. 2D geometry                   */
    int         print_times;   /* print times as well as throughputs   */
    int         print_raw;     /* print raw data throughput info       */
    off_t       h5_alignment;  /* alignment in HDF5 file               */
    off_t       h5_threshold;  /* threshold for alignment in HDF5 file */
    int         h5_use_chunks; /* Make HDF5 dataset chunked            */
    int         h5_write_only; /* Perform the write tests only         */
    int         verify;        /* Verify data correctness              */
};

typedef struct _minmax {
    double min;
    double max;
    double sum;
    int    num;
} minmax;

/* local functions */
static off_t           parse_size_directive(const char *size);
static struct options *parse_command_line(int argc, const char *const *argv);
static void            run_test_loop(struct options *options);
static int             run_test(iotype iot, parameters parms, struct options *opts);
static void            output_all_info(minmax *mm, int count, int indent_level);
static void            get_minmax(minmax *mm, double val);
static minmax          accumulate_minmax_stuff(minmax *mm, int count);
static int             create_comm_world(int num_procs, int *doing_pio);
static int             destroy_comm_world(void);
static void  output_results(const struct options *options, const char *name, minmax *table, int table_size,
                            off_t data_size);
static void  output_times(const struct options *options, const char *name, minmax *table, int table_size);
static void  output_report(const char *fmt, ...) H5_ATTR_FORMAT(printf, 1, 2);
static void  print_indent(int indent);
static void  usage(const char *prog);
static void  report_parameters(struct options *opts);
static off_t squareo(off_t);

/*
 * Function:    main
 * Purpose:     Start things up. Initialize MPI and then call the test looping
 *              function.
 * Return:      EXIT_SUCCESS or EXIT_FAILURE
 * Programmer:  Bill Wendling, 30. October 2001
 * Modifications:
 */
int
main(int argc, char *argv[])
{
    int             ret;
    int             exit_value = EXIT_SUCCESS;
    struct options *opts       = NULL;

    /* Initialize h5tools lib */
    h5tools_init();

    output = stdout;

    /* initialize MPI and get the maximum num of processors we started with */
    MPI_Init(&argc, &argv);
    ret = MPI_Comm_size(MPI_COMM_WORLD, &comm_world_nprocs_g);

    if (ret != MPI_SUCCESS) {
        HDfprintf(stderr, "%s: MPI_Comm_size call failed\n", progname);

        if (ret == MPI_ERR_COMM)
            HDfprintf(stderr, "invalid MPI communicator\n");
        else
            HDfprintf(stderr, "invalid argument\n");

        exit_value = EXIT_FAILURE;
        goto finish;
    }

    ret = MPI_Comm_rank(MPI_COMM_WORLD, &comm_world_rank_g);

    if (ret != MPI_SUCCESS) {
        HDfprintf(stderr, "%s: MPI_Comm_rank call failed\n", progname);

        if (ret == MPI_ERR_COMM)
            HDfprintf(stderr, "invalid MPI communicator\n");
        else
            HDfprintf(stderr, "invalid argument\n");

        exit_value = EXIT_FAILURE;
        goto finish;
    }

    pio_comm_g = MPI_COMM_WORLD;

    h5_set_info_object();
    opts = parse_command_line(argc, (const char *const *)argv);

    if (!opts) {
        exit_value = EXIT_FAILURE;
        goto finish;
    }

    if (opts->output_file) {
        if ((output = HDfopen(opts->output_file, "w")) == NULL) {
            HDfprintf(stderr, "%s: cannot open output file\n", progname);
            perror(opts->output_file);
            goto finish;
        }
    }

    if ((pio_debug_level == 0 && comm_world_rank_g == 0) || pio_debug_level > 0)
        report_parameters(opts);

    run_test_loop(opts);

finish:
    MPI_Finalize();
    free(opts);
    return exit_value;
}

off_t
squareo(off_t x)
{
    return x * x;
}

/*
 * Function:    run_test_loop
 * Purpose:     Run the I/O tests. Write the results to OUTPUT.
 *
 *            - The slowest changing part of the test is the number of
 *              processors to use. For each loop iteration, we divide that
 *              number by 2 and rerun the test.
 *
 *            - The second slowest is what type of IO API to perform. We have
 *              three choices: POSIXIO, MPI-IO, and PHDF5.
 *
 *            - Then we change the size of the buffer. This information is
 *              inferred from the number of datasets to create and the number
 *              of integers to put into each dataset. The backend code figures
 *              this out.
 *
 * Return:      Nothing
 * Programmer:  Bill Wendling, 30. October 2001
 * Modifications:
 *    Added 2D testing (Christian Chilan, 10. August 2005)
 */
static void
run_test_loop(struct options *opts)
{
    parameters parms;
    int        num_procs;
    int        doing_pio; /* if this process is doing PIO */

    parms.num_files     = opts->num_files;
    parms.num_dsets     = opts->num_dsets;
    parms.num_iters     = opts->num_iters;
    parms.blk_size      = opts->blk_size;
    parms.interleaved   = opts->interleaved;
    parms.collective    = opts->collective;
    parms.dim2d         = opts->dim2d;
    parms.h5_align      = (hsize_t)opts->h5_alignment;
    parms.h5_thresh     = (hsize_t)opts->h5_threshold;
    parms.h5_use_chunks = opts->h5_use_chunks;
    parms.h5_write_only = opts->h5_write_only;
    parms.verify        = opts->verify;

    /* start with max_num_procs and decrement it by half for each loop. */
    /* if performance needs restart, fewer processes may be needed. */
    for (num_procs = opts->max_num_procs; num_procs >= opts->min_num_procs; num_procs >>= 1) {
        size_t buf_size;

        parms.num_procs = num_procs;

        if (create_comm_world(parms.num_procs, &doing_pio) != SUCCESS) {
            /* do something harsh */
        }

        /* only processes doing PIO will run the tests */
        if (doing_pio) {
            output_report("Number of processors = %d\n", parms.num_procs);

            /* multiply the xfer buffer size by 2 for each loop iteration */
            for (buf_size = opts->min_xfer_size; buf_size <= opts->max_xfer_size; buf_size <<= 1) {
                parms.buf_size = buf_size;

                if (parms.dim2d) {
                    parms.num_bytes = squareo(opts->num_bpp * parms.num_procs);
                    if (parms.interleaved)
                        output_report("Transfer Buffer Size: %ldx%ld bytes, File size: %.2f MB\n", buf_size,
                                      opts->blk_size,
                                      ((double)parms.num_dsets * (double)parms.num_bytes) / ONE_MB);
                    else
                        output_report("Transfer Buffer Size: %ldx%ld bytes, File size: %.2f MB\n",
                                      opts->blk_size, buf_size,
                                      ((double)parms.num_dsets * (double)parms.num_bytes) / ONE_MB);

                    print_indent(1);
                    output_report("  # of files: %ld, # of datasets: %ld, dataset size: %.2fx%.2f KB\n",
                                  parms.num_files, parms.num_dsets,
                                  (double)(opts->num_bpp * parms.num_procs) / ONE_KB,
                                  (double)(opts->num_bpp * parms.num_procs) / ONE_KB);
                }
                else {
                    parms.num_bytes = (off_t)opts->num_bpp * parms.num_procs;
                    output_report("Transfer Buffer Size: %ld bytes, File size: %.2f MB\n", buf_size,
                                  ((double)parms.num_dsets * (double)parms.num_bytes) / ONE_MB);

                    print_indent(1);
                    output_report("  # of files: %ld, # of datasets: %ld, dataset size: %.2f MB\n",
                                  parms.num_files, parms.num_dsets,
                                  (double)(opts->num_bpp * parms.num_procs) / ONE_MB);
                }

                if (opts->io_types & PIO_POSIX)
                    run_test(POSIXIO, parms, opts);

                if (opts->io_types & PIO_MPI)
                    run_test(MPIO, parms, opts);

                if (opts->io_types & PIO_HDF5)
                    run_test(PHDF5, parms, opts);

                /* Run the tests once if buf_size==0, but then break out */
                if (buf_size == 0)
                    break;
            }

            if (destroy_comm_world() != SUCCESS) {
                /* do something harsh */
            }
        }
    }
}

/*
 * Function:    run_test
 * Purpose:     Inner loop call to actually run the I/O test.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 18. December 2001
 * Modifications:
 */
static int
run_test(iotype iot, parameters parms, struct options *opts)
{
    results res;
    int     i, ret_value = SUCCESS;
    int     comm_size;
    off_t   raw_size;
    minmax *write_mpi_mm_table   = NULL;
    minmax *write_mm_table       = NULL;
    minmax *write_gross_mm_table = NULL;
    minmax *write_raw_mm_table   = NULL;
    minmax *read_mpi_mm_table    = NULL;
    minmax *read_mm_table        = NULL;
    minmax *read_gross_mm_table  = NULL;
    minmax *read_raw_mm_table    = NULL;
    minmax *read_open_mm_table   = NULL;
    minmax *read_close_mm_table  = NULL;
    minmax *write_open_mm_table  = NULL;
    minmax *write_close_mm_table = NULL;
    minmax  write_mpi_mm         = {0.0, 0.0, 0.0, 0};
    minmax  write_mm             = {0.0, 0.0, 0.0, 0};
    minmax  write_gross_mm       = {0.0, 0.0, 0.0, 0};
    minmax  write_raw_mm         = {0.0, 0.0, 0.0, 0};
    minmax  read_mpi_mm          = {0.0, 0.0, 0.0, 0};
    minmax  read_mm              = {0.0, 0.0, 0.0, 0};
    minmax  read_gross_mm        = {0.0, 0.0, 0.0, 0};
    minmax  read_raw_mm          = {0.0, 0.0, 0.0, 0};
    minmax  read_open_mm         = {0.0, 0.0, 0.0, 0};
    minmax  read_close_mm        = {0.0, 0.0, 0.0, 0};
    minmax  write_open_mm        = {0.0, 0.0, 0.0, 0};
    minmax  write_close_mm       = {0.0, 0.0, 0.0, 0};

    raw_size      = parms.num_files * (off_t)parms.num_dsets * (off_t)parms.num_bytes;
    parms.io_type = iot;
    print_indent(2);
    output_report("IO API = ");

    switch (iot) {
        case POSIXIO:
            output_report("POSIX\n");
            break;
        case MPIO:
            output_report("MPIO\n");
            break;
        case PHDF5:
            output_report("PHDF5 (w/MPI-IO driver)\n");
            break;
        default:
            break;
    }

    MPI_Comm_size(pio_comm_g, &comm_size);

    /* allocate space for tables minmax and that it is sufficient */
    /* to initialize all elements to zeros by calloc.             */
    write_mpi_mm_table   = calloc((size_t)parms.num_iters, sizeof(minmax));
    write_mm_table       = calloc((size_t)parms.num_iters, sizeof(minmax));
    write_gross_mm_table = calloc((size_t)parms.num_iters, sizeof(minmax));
    write_raw_mm_table   = calloc((size_t)parms.num_iters, sizeof(minmax));
    write_open_mm_table  = calloc((size_t)parms.num_iters, sizeof(minmax));
    write_close_mm_table = calloc((size_t)parms.num_iters, sizeof(minmax));
    if (!parms.h5_write_only) {
        read_mpi_mm_table   = calloc((size_t)parms.num_iters, sizeof(minmax));
        read_mm_table       = calloc((size_t)parms.num_iters, sizeof(minmax));
        read_gross_mm_table = calloc((size_t)parms.num_iters, sizeof(minmax));
        read_raw_mm_table   = calloc((size_t)parms.num_iters, sizeof(minmax));
        read_open_mm_table  = calloc((size_t)parms.num_iters, sizeof(minmax));
        read_close_mm_table = calloc((size_t)parms.num_iters, sizeof(minmax));
    }

    /* Do IO iteration times, collecting statistics each time */
    for (i = 0; i < parms.num_iters; ++i) {
        double t;

        MPI_Barrier(pio_comm_g);
        res = do_pio(parms);

        /* gather all of the "mpi write" times */
        t = io_time_get(res.timers, HDF5_MPI_WRITE);
        get_minmax(&write_mpi_mm, t);

        write_mpi_mm_table[i] = write_mpi_mm;

        /* gather all of the "write" times */
        t = io_time_get(res.timers, HDF5_FINE_WRITE_FIXED_DIMS);
        get_minmax(&write_mm, t);

        write_mm_table[i] = write_mm;

        /* gather all of the "write" times from open to close */
        t = io_time_get(res.timers, HDF5_GROSS_WRITE_FIXED_DIMS);
        get_minmax(&write_gross_mm, t);

        write_gross_mm_table[i] = write_gross_mm;

        /* gather all of the raw "write" times */
        t = io_time_get(res.timers, HDF5_RAW_WRITE_FIXED_DIMS);
        get_minmax(&write_raw_mm, t);

        write_raw_mm_table[i] = write_raw_mm;

        /* gather all of the file open times (time from open to first write) */
        t = io_time_get(res.timers, HDF5_FILE_WRITE_OPEN);
        get_minmax(&write_open_mm, t);

        write_open_mm_table[i] = write_open_mm;

        /* gather all of the file close times (time from last write to close) */
        t = io_time_get(res.timers, HDF5_FILE_WRITE_CLOSE);
        get_minmax(&write_close_mm, t);

        write_close_mm_table[i] = write_close_mm;

        if (!parms.h5_write_only) {
            /* gather all of the "mpi read" times */
            t = io_time_get(res.timers, HDF5_MPI_READ);
            get_minmax(&read_mpi_mm, t);

            read_mpi_mm_table[i] = read_mpi_mm;

            /* gather all of the "read" times */
            t = io_time_get(res.timers, HDF5_FINE_READ_FIXED_DIMS);
            get_minmax(&read_mm, t);

            read_mm_table[i] = read_mm;

            /* gather all of the "read" times from open to close */
            t = io_time_get(res.timers, HDF5_GROSS_READ_FIXED_DIMS);
            get_minmax(&read_gross_mm, t);

            read_gross_mm_table[i] = read_gross_mm;

            /* gather all of the raw "read" times */
            t = io_time_get(res.timers, HDF5_RAW_READ_FIXED_DIMS);
            get_minmax(&read_raw_mm, t);

            read_raw_mm_table[i] = read_raw_mm;

            /* gather all of the file open times (time from open to first read) */
            t = io_time_get(res.timers, HDF5_FILE_READ_OPEN);
            get_minmax(&read_open_mm, t);

            read_open_mm_table[i] = read_open_mm;

            /* gather all of the file close times (time from last read to close) */
            t = io_time_get(res.timers, HDF5_FILE_READ_CLOSE);
            get_minmax(&read_close_mm, t);

            read_close_mm_table[i] = read_close_mm;
        }

        io_time_destroy(res.timers);
    }

    /*
     * Show various statistics
     */
    /* Write statistics    */
    /* Print the raw data throughput if desired */
    if (opts->print_raw) {
        /* accumulate and output the max, min, and average "raw write" times */
        if (pio_debug_level >= 3) {
            /* output all of the times for all iterations */
            print_indent(3);
            output_report("Raw Data Write details:\n");
            output_all_info(write_raw_mm_table, parms.num_iters, 4);
        }

        output_results(opts, "Raw Data Write", write_raw_mm_table, parms.num_iters, raw_size);
    } /* end if */

    /* show mpi write statistics */
    if (pio_debug_level >= 3) {
        /* output all of the times for all iterations */
        print_indent(3);
        output_report("MPI Write details:\n");
        output_all_info(write_mpi_mm_table, parms.num_iters, 4);
    }

    /* We don't currently output the MPI write results */

    /* accumulate and output the max, min, and average "write" times */
    if (pio_debug_level >= 3) {
        /* output all of the times for all iterations */
        print_indent(3);
        output_report("Write details:\n");
        output_all_info(write_mm_table, parms.num_iters, 4);
    }

    output_results(opts, "Write", write_mm_table, parms.num_iters, raw_size);

    /* accumulate and output the max, min, and average "gross write" times */
    if (pio_debug_level >= 3) {
        /* output all of the times for all iterations */
        print_indent(3);
        output_report("Write Open-Close details:\n");
        output_all_info(write_gross_mm_table, parms.num_iters, 4);
    }

    output_results(opts, "Write Open-Close", write_gross_mm_table, parms.num_iters, raw_size);

    if (opts->print_times) {
        output_times(opts, "Write File Open", write_open_mm_table, parms.num_iters);
        output_times(opts, "Write File Close", write_close_mm_table, parms.num_iters);
    }

    /* Print out time from open to first write */
    if (pio_debug_level >= 3) {
        /* output all of the times for all iterations */
        print_indent(3);
        output_report("Write file open details:\n");
        output_all_info(write_open_mm_table, parms.num_iters, 4);
    }

    /* Print out time from last write to close */
    if (pio_debug_level >= 3) {
        /* output all of the times for all iterations */
        print_indent(3);
        output_report("Write file close details:\n");
        output_all_info(write_close_mm_table, parms.num_iters, 4);
    }

    if (!parms.h5_write_only) {
        /* Read statistics    */
        /* Print the raw data throughput if desired */
        if (opts->print_raw) {
            /* accumulate and output the max, min, and average "raw read" times */
            if (pio_debug_level >= 3) {
                /* output all of the times for all iterations */
                print_indent(3);
                output_report("Raw Data Read details:\n");
                output_all_info(read_raw_mm_table, parms.num_iters, 4);
            }

            output_results(opts, "Raw Data Read", read_raw_mm_table, parms.num_iters, raw_size);
        } /* end if */

        /* show mpi read statistics */
        if (pio_debug_level >= 3) {
            /* output all of the times for all iterations */
            print_indent(3);
            output_report("MPI Read details:\n");
            output_all_info(read_mpi_mm_table, parms.num_iters, 4);
        }

        /* We don't currently output the MPI read results */

        /* accumulate and output the max, min, and average "read" times */
        if (pio_debug_level >= 3) {
            /* output all of the times for all iterations */
            print_indent(3);
            output_report("Read details:\n");
            output_all_info(read_mm_table, parms.num_iters, 4);
        }

        output_results(opts, "Read", read_mm_table, parms.num_iters, raw_size);

        /* accumulate and output the max, min, and average "gross read" times */
        if (pio_debug_level >= 3) {
            /* output all of the times for all iterations */
            print_indent(3);
            output_report("Read Open-Close details:\n");
            output_all_info(read_gross_mm_table, parms.num_iters, 4);
        }

        output_results(opts, "Read Open-Close", read_gross_mm_table, parms.num_iters, raw_size);

        if (opts->print_times) {
            output_times(opts, "Read File Open", read_open_mm_table, parms.num_iters);
            output_times(opts, "Read File Close", read_close_mm_table, parms.num_iters);
        }

        /* Print out time from open to first read */
        if (pio_debug_level >= 3) {
            /* output all of the times for all iterations */
            print_indent(3);
            output_report("Read file open details:\n");
            output_all_info(read_open_mm_table, parms.num_iters, 4);
        }

        /* Print out time from last read to close */
        if (pio_debug_level >= 3) {
            /* output all of the times for all iterations */
            print_indent(3);
            output_report("Read file close details:\n");
            output_all_info(read_close_mm_table, parms.num_iters, 4);
        }
    }

    /* clean up our mess */
    free(write_mpi_mm_table);
    free(write_mm_table);
    free(write_gross_mm_table);
    free(write_raw_mm_table);
    free(write_open_mm_table);
    free(write_close_mm_table);

    if (!parms.h5_write_only) {
        free(read_mpi_mm_table);
        free(read_mm_table);
        free(read_gross_mm_table);
        free(read_raw_mm_table);
        free(read_open_mm_table);
        free(read_close_mm_table);
    }

    return ret_value;
}

/*
 * Function:    output_all_info
 * Purpose:
 * Return:      Nothing
 * Programmer:  Bill Wendling, 29. January 2002
 * Modifications:
 */
static void
output_all_info(minmax *mm, int count, int indent_level)
{
    int i;

    for (i = 0; i < count; ++i) {
        print_indent(indent_level);
        output_report("Iteration %d:\n", i + 1);
        print_indent(indent_level + 1);
        output_report("Minimum Time: %.2fs\n", mm[i].min);
        print_indent(indent_level + 1);
        output_report("Maximum Time: %.2fs\n", mm[i].max);
    }
}

/*
 * Function:    h5_set_info_object
 * Purpose:     Process environment variables setting to set up MPI Info
 *              object.
 * Return:      0 if all is fine; otherwise non-zero.
 * Programmer:  Albert Cheng, 2002/05/21.
 * Modifications:
 *          Bill Wendling, 2002/05/31
 *          Modified so that the HDF5_MPI_INFO environment variable can
 *          be a semicolon separated list of "key=value" pairings. Most
 *          of the code is to remove any whitespaces which might be
 *          surrounding the "key=value" pairs.
 */
int
h5_set_info_object(void)
{
    char *envp; /* environment pointer */
    int   ret_value = 0;

    /* handle any MPI INFO hints via $HDF5_MPI_INFO */
    if ((envp = HDgetenv("HDF5_MPI_INFO")) != NULL) {
        char *next, *valp;

        valp = envp = next = HDstrdup(envp);

        if (!valp)
            return 0;

        /* create an INFO object if not created yet */
        if (h5_io_info_g == MPI_INFO_NULL)
            MPI_Info_create(&h5_io_info_g);

        do {
            size_t len;
            char  *key_val, *endp, *namep;

            if (*valp == ';')
                valp++;

            /* copy key/value pair into temporary buffer */
            len     = strcspn(valp, ";");
            next    = &valp[len];
            key_val = (char *)HDcalloc(1, len + 1);

            /* increment the next pointer past the terminating semicolon */
            if (*next == ';')
                ++next;

            namep = HDstrncpy(key_val, valp, len);

            /* pass up any beginning whitespaces */
            while (*namep && (*namep == ' ' || *namep == '\t'))
                namep++;

            if (!*namep)
                continue; /* was all white space, so move to next k/v pair */

            /* eat up any ending white spaces */
            endp = &namep[HDstrlen(namep) - 1];

            while (endp && (*endp == ' ' || *endp == '\t'))
                *endp-- = '\0';

            /* find the '=' */
            valp = HDstrchr(namep, '=');

            if (valp != NULL) { /* it's a valid key/value pairing */
                char *tmp_val = valp + 1;

                /* change '=' to \0, move valp down one */
                *valp-- = '\0';

                /* eat up ending whitespace on the "key" part */
                while (*valp == ' ' || *valp == '\t')
                    *valp-- = '\0';

                valp = tmp_val;

                /* eat up beginning whitespace on the "value" part */
                while (*valp == ' ' || *valp == '\t')
                    *valp++ = '\0';

                /* actually set the darned thing */
                if (MPI_SUCCESS != MPI_Info_set(h5_io_info_g, namep, valp)) {
                    HDprintf("MPI_Info_set failed\n");
                    ret_value = -1;
                }
            }

            valp = next;
            HDfree(key_val);
        } while (next && *next);

        HDfree(envp);
    }

    return ret_value;
}

/*
 * Function:    h5_dump_info_object
 * Purpose:     Display content of an MPI Info object
 * Return:      void
 * Programmer:  Albert Cheng 2002/05/21
 * Modifications:
 */
void
h5_dump_info_object(MPI_Info info)
{
    char key[MPI_MAX_INFO_KEY + 1];
    char value[MPI_MAX_INFO_VAL + 1];
    int  flag;
    int  i, nkeys;

    HDprintf("Dumping MPI Info Object (up to %d bytes per item):\n", MPI_MAX_INFO_VAL);
    if (info == MPI_INFO_NULL) {
        HDprintf("object is MPI_INFO_NULL\n");
    }
    else {
        MPI_Info_get_nkeys(info, &nkeys);
        HDprintf("object has %d items\n", nkeys);
        for (i = 0; i < nkeys; i++) {
            MPI_Info_get_nthkey(info, i, key);
            MPI_Info_get(info, key, MPI_MAX_INFO_VAL, value, &flag);
            HDprintf("%s=%s\n", key, value);
        }
    }
}

/*
 * Function:    get_minmax
 * Purpose:     Gather all the min, max and total of val.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 21. December 2001
 * Modifications:
 *    Use MPI_Allreduce to do it. -akc, 2002/01/11
 */
static void
get_minmax(minmax *mm, double val)
{
    int myrank;

    MPI_Comm_rank(pio_comm_g, &myrank);
    MPI_Comm_size(pio_comm_g, &mm->num);

    MPI_Allreduce(&val, &mm->max, 1, MPI_DOUBLE, MPI_MAX, pio_comm_g);
    MPI_Allreduce(&val, &mm->min, 1, MPI_DOUBLE, MPI_MIN, pio_comm_g);
    MPI_Allreduce(&val, &mm->sum, 1, MPI_DOUBLE, MPI_SUM, pio_comm_g);
}

/*
 * Function:    accumulate_minmax_stuff
 * Purpose:     Accumulate the minimum, maximum, and average of the times
 *              across all processes.
 * Return:      TOTAL_MM - the total of all of these.
 * Programmer:  Bill Wendling, 21. December 2001
 * Modifications:
 *              Changed to use seconds instead of MB/s - QAK, 5/9/02
 */
static minmax
accumulate_minmax_stuff(minmax *mm, int count)
{
    int    i;
    minmax total_mm;

    total_mm.sum = 0.0;
    total_mm.max = -DBL_MAX;
    total_mm.min = DBL_MAX;
    total_mm.num = count;

    for (i = 0; i < count; ++i) {
        double m = mm[i].max;

        total_mm.sum += m;

        if (m < total_mm.min)
            total_mm.min = m;

        if (m > total_mm.max)
            total_mm.max = m;
    }

    return total_mm;
}

/*
 * Function:    create_comm_world
 * Purpose:     Create an MPI Comm world and store it in pio_comm_g, which
 *              is a global variable.
 * Return:      SUCCESS on success.
 *              FAIL otherwise.
 * Programmer:  Bill Wendling, 19. December 2001
 * Modifications:
 */
static int
create_comm_world(int num_procs, int *doing_pio)
{
    /* MPI variables */
    int mrc;   /* return values                */
    int color; /* for communicator creation    */
    int myrank, nprocs;

    pio_comm_g = MPI_COMM_NULL;

    /*
     * Create a sub communicator for this PIO run. Easier to use the first N
     * processes.
     */
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

    if (num_procs > nprocs) {
        HDfprintf(stderr, "number of process(%d) must be <= number of processes in MPI_COMM_WORLD(%d)\n",
                  num_procs, nprocs);
        goto error_done;
    }

    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
    color = (myrank < num_procs);
    mrc   = MPI_Comm_split(MPI_COMM_WORLD, color, myrank, &pio_comm_g);

    if (mrc != MPI_SUCCESS) {
        HDfprintf(stderr, "MPI_Comm_split failed\n");
        goto error_done;
    }

    if (!color) {
        /* not involved in this run */
        mrc = destroy_comm_world();
        goto done;
    }

    /* determine the MPI rank in the PIO communicator */
    MPI_Comm_size(pio_comm_g, &pio_mpi_nprocs_g);
    MPI_Comm_rank(pio_comm_g, &pio_mpi_rank_g);

done:
    *doing_pio = color;
    return SUCCESS;

error_done:
    destroy_comm_world();
    return FAIL;
}

/*
 * Function:    destroy_comm_world
 * Purpose:     Destroy the created MPI Comm world which is stored in the
 *              pio_comm_g global variable.
 * Return:      SUCCESS on success.
 *              FAIL otherwise.
 * Programmer:  Bill Wendling, 19. December 2001
 * Modifications:
 */
static int
destroy_comm_world(void)
{
    int mrc = SUCCESS; /* return code      */

    /* release MPI resources */
    if (pio_comm_g != MPI_COMM_NULL)
        mrc = (MPI_Comm_free(&pio_comm_g) == MPI_SUCCESS ? SUCCESS : FAIL);

    return mrc;
}

/*
 * Function:    output_results
 * Purpose:     Print information about the time & bandwidth for a given
 *                  minmax & # of iterations.
 * Return:      Nothing
 * Programmer:  Quincey Koziol, 9. May 2002
 * Modifications:
 */
static void
output_results(const struct options *opts, const char *name, minmax *table, int table_size, off_t data_size)
{
    minmax total_mm;

    total_mm = accumulate_minmax_stuff(table, table_size);

    print_indent(3);
    output_report("%s (%d iteration(s)):\n", name, table_size);

    /* Note: The maximum throughput uses the minimum amount of time & vice versa */

    print_indent(4);
    output_report("Maximum Throughput: %6.2f MB/s", MB_PER_SEC(data_size, total_mm.min));
    if (opts->print_times)
        output_report(" (%7.3f s)\n", total_mm.min);
    else
        output_report("\n");

    print_indent(4);
    output_report("Average Throughput: %6.2f MB/s", MB_PER_SEC(data_size, total_mm.sum / total_mm.num));
    if (opts->print_times)
        output_report(" (%7.3f s)\n", (total_mm.sum / total_mm.num));
    else
        output_report("\n");

    print_indent(4);
    output_report("Minimum Throughput: %6.2f MB/s", MB_PER_SEC(data_size, total_mm.max));
    if (opts->print_times)
        output_report(" (%7.3f s)\n", total_mm.max);
    else
        output_report("\n");
}

static void
output_times(const struct options *opts, const char *name, minmax *table, int table_size)
{
    minmax total_mm;

    total_mm = accumulate_minmax_stuff(table, table_size);

    print_indent(3);
    output_report("%s (%d iteration(s)):\n", name, table_size);

    /* Note: The maximum throughput uses the minimum amount of time & vice versa */

    print_indent(4);
    output_report("Minimum Accumulated Time using %ld file(s): %7.5f s\n", opts->num_files, (total_mm.min));

    print_indent(4);
    output_report("Average Accumulated Time using %ld file(s): %7.5f s\n", opts->num_files,
                  (total_mm.sum / total_mm.num));

    print_indent(4);
    output_report("Maximum Accumulated Time using %ld file(s): %7.5f s\n", opts->num_files, (total_mm.max));
}

/*
 * Function:    output_report
 * Purpose:     Print a line of the report. Only do so if I'm the 0 process.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 19. December 2001
 * Modifications:
 */
static void
output_report(const char *fmt, ...)
{
    int myrank;

    MPI_Comm_rank(pio_comm_g, &myrank);

    if (myrank == 0) {
        va_list ap;

        HDva_start(ap, fmt);
        H5_GCC_CLANG_DIAG_OFF("format-nonliteral")
        HDvfprintf(output, fmt, ap);
        H5_GCC_CLANG_DIAG_ON("format-nonliteral")
        HDva_end(ap);
    }
}

/*
 * Function:    print_indent
 * Purpose:     Print spaces to indent a new line of text for pretty printing
 *              things.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 29. October 2001
 */
static void
print_indent(int indent)
{
    int myrank;

    MPI_Comm_rank(pio_comm_g, &myrank);

    if (myrank == 0) {
        indent *= TAB_SPACE;

        for (; indent > 0; --indent)
            HDfputc(' ', output);
    }
}

static void
recover_size_and_print(long long val, const char *end)
{
    if (val >= ONE_KB && (val % ONE_KB) == 0) {
        if (val >= ONE_MB && (val % ONE_MB) == 0) {
            if (val >= ONE_GB && (val % ONE_GB) == 0)
                HDfprintf(output,
                          "%lld"
                          "GB%s",
                          val / ONE_GB, end);
            else
                HDfprintf(output,
                          "%lld"
                          "MB%s",
                          val / ONE_MB, end);
        }
        else {
            HDfprintf(output,
                      "%lld"
                      "KB%s",
                      val / ONE_KB, end);
        }
    }
    else {
        HDfprintf(output,
                  "%lld"
                  "%s",
                  val, end);
    }
}

static void
print_io_api(long io_types)
{
    if (io_types & PIO_POSIX)
        HDfprintf(output, "posix ");
    if (io_types & PIO_MPI)
        HDfprintf(output, "mpiio ");
    if (io_types & PIO_HDF5)
        HDfprintf(output, "phdf5 ");
    HDfprintf(output, "\n");
}

static void
report_parameters(struct options *opts)
{
    int rank = comm_world_rank_g;

    print_version("HDF5 Library"); /* print library version */
    HDfprintf(output, "rank %d: ==== Parameters ====\n", rank);

    HDfprintf(output, "rank %d: IO API=", rank);
    print_io_api(opts->io_types);

    HDfprintf(output, "rank %d: Number of files=%ld\n", rank, opts->num_files);
    HDfprintf(output, "rank %d: Number of datasets=%ld\n", rank, opts->num_dsets);
    HDfprintf(output, "rank %d: Number of iterations=%d\n", rank, opts->num_iters);
    HDfprintf(output, "rank %d: Number of processes=%d:%d\n", rank, opts->min_num_procs, opts->max_num_procs);

    if (opts->dim2d) {
        HDfprintf(output, "rank %d: Number of bytes per process per dataset=", rank);
        recover_size_and_print((long long)(opts->num_bpp * opts->num_bpp * opts->min_num_procs), ":");
        recover_size_and_print((long long)(opts->num_bpp * opts->num_bpp * opts->max_num_procs), "\n");

        HDfprintf(output, "rank %d: Size of dataset(s)=", rank);
        recover_size_and_print((long long)(opts->num_bpp * opts->min_num_procs), "x");
        recover_size_and_print((long long)(opts->num_bpp * opts->min_num_procs), ":");
        recover_size_and_print((long long)(opts->num_bpp * opts->max_num_procs), "x");
        recover_size_and_print((long long)(opts->num_bpp * opts->max_num_procs), "\n");

        HDfprintf(output, "rank %d: File size=", rank);
        recover_size_and_print((long long)(squareo(opts->num_bpp * opts->min_num_procs) * opts->num_dsets),
                               ":");
        recover_size_and_print((long long)(squareo(opts->num_bpp * opts->max_num_procs) * opts->num_dsets),
                               "\n");

        HDfprintf(output, "rank %d: Transfer buffer size=", rank);
        if (opts->interleaved) {
            recover_size_and_print((long long)opts->min_xfer_size, "x");
            recover_size_and_print((long long)opts->blk_size, ":");
            recover_size_and_print((long long)opts->max_xfer_size, "x");
            recover_size_and_print((long long)opts->blk_size, "\n");
        }
        else {
            recover_size_and_print((long long)opts->blk_size, "x");
            recover_size_and_print((long long)opts->min_xfer_size, ":");
            recover_size_and_print((long long)opts->blk_size, "x");
            recover_size_and_print((long long)opts->max_xfer_size, "\n");
        }
        HDfprintf(output, "rank %d: Block size=", rank);
        recover_size_and_print((long long)opts->blk_size, "x");
        recover_size_and_print((long long)opts->blk_size, "\n");
    }
    else {
        HDfprintf(output, "rank %d: Number of bytes per process per dataset=", rank);
        recover_size_and_print((long long)opts->num_bpp, "\n");

        HDfprintf(output, "rank %d: Size of dataset(s)=", rank);
        recover_size_and_print((long long)(opts->num_bpp * opts->min_num_procs), ":");
        recover_size_and_print((long long)(opts->num_bpp * opts->max_num_procs), "\n");

        HDfprintf(output, "rank %d: File size=", rank);
        recover_size_and_print((long long)(opts->num_bpp * opts->min_num_procs * opts->num_dsets), ":");
        recover_size_and_print((long long)(opts->num_bpp * opts->max_num_procs * opts->num_dsets), "\n");

        HDfprintf(output, "rank %d: Transfer buffer size=", rank);
        recover_size_and_print((long long)opts->min_xfer_size, ":");
        recover_size_and_print((long long)opts->max_xfer_size, "\n");
        HDfprintf(output, "rank %d: Block size=", rank);
        recover_size_and_print((long long)opts->blk_size, "\n");
    }

    HDfprintf(output, "rank %d: Block Pattern in Dataset=", rank);
    if (opts->interleaved)
        HDfprintf(output, "Interleaved\n");
    else
        HDfprintf(output, "Contiguous\n");

    HDfprintf(output, "rank %d: I/O Method for MPI and HDF5=", rank);
    if (opts->collective)
        HDfprintf(output, "Collective\n");
    else
        HDfprintf(output, "Independent\n");

    HDfprintf(output, "rank %d: Geometry=", rank);
    if (opts->dim2d)
        HDfprintf(output, "2D\n");
    else
        HDfprintf(output, "1D\n");

    HDfprintf(output, "rank %d: VFL used for HDF5 I/O=%s\n", rank, "MPI-IO driver");

    HDfprintf(output, "rank %d: Data storage method in HDF5=", rank);
    if (opts->h5_use_chunks)
        HDfprintf(output, "Chunked\n");
    else
        HDfprintf(output, "Contiguous\n");

    {
        char *prefix = HDgetenv("HDF5_PARAPREFIX");

        HDfprintf(output, "rank %d: Env HDF5_PARAPREFIX=%s\n", rank, (prefix ? prefix : "not set"));
    }

    HDfprintf(output, "rank %d: ", rank);
    h5_dump_info_object(h5_io_info_g);

    HDfprintf(output, "rank %d: ==== End of Parameters ====\n", rank);
    HDfprintf(output, "\n");
}

/*
 * Function:    parse_command_line
 * Purpose:     Parse the command line options and return a STRUCT OPTIONS
 *              structure which will need to be freed by the calling function.
 * Return:      Pointer to an OPTIONS structure
 * Programmer:  Bill Wendling, 31. October 2001
 * Modifications:
 *    Added 2D testing (Christian Chilan, 10. August 2005)
 */
static struct options *
parse_command_line(int argc, const char *const *argv)
{
    int             opt;
    struct options *cl_opts;

    cl_opts = (struct options *)malloc(sizeof(struct options));

    cl_opts->output_file   = NULL;
    cl_opts->io_types      = 0; /* will set default after parsing options */
    cl_opts->num_dsets     = 1;
    cl_opts->num_files     = 1;
    cl_opts->num_bpp       = 0;
    cl_opts->num_iters     = 1;
    cl_opts->max_num_procs = comm_world_nprocs_g;
    cl_opts->min_num_procs = 1;
    cl_opts->max_xfer_size = 0;
    cl_opts->min_xfer_size = 0;
    cl_opts->blk_size      = 0;
    cl_opts->interleaved   = 0;     /* Default to contiguous blocks in dataset */
    cl_opts->collective    = 0;     /* Default to independent I/O access */
    cl_opts->dim2d         = 0;     /* Default to 1D */
    cl_opts->print_times   = FALSE; /* Printing times is off by default */
    cl_opts->print_raw     = FALSE; /* Printing raw data throughput is off by default */
    cl_opts->h5_alignment  = 1;     /* No alignment for HDF5 objects by default */
    cl_opts->h5_threshold  = 1;     /* No threshold for aligning HDF5 objects by default */
    cl_opts->h5_use_chunks = FALSE; /* Don't chunk the HDF5 dataset by default */
    cl_opts->h5_write_only = FALSE; /* Do both read and write by default */
    cl_opts->verify        = FALSE; /* No Verify data correctness by default */

    while ((opt = H5_get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
            case 'a':
                cl_opts->h5_alignment = parse_size_directive(H5_optarg);
                break;
            case 'A': {
                const char *end = H5_optarg;

                while (end && *end != '\0') {
                    char buf[10];
                    int  i;

                    HDmemset(buf, '\0', sizeof(buf));

                    for (i = 0; *end != '\0' && *end != ','; ++end)
                        if (isalnum(*end) && i < 10)
                            buf[i++] = *end;

                    if (!HDstrcasecmp(buf, "phdf5")) {
                        cl_opts->io_types |= PIO_HDF5;
                    }
                    else if (!HDstrcasecmp(buf, "mpiio")) {
                        cl_opts->io_types |= PIO_MPI;
                    }
                    else if (!HDstrcasecmp(buf, "posix")) {
                        cl_opts->io_types |= PIO_POSIX;
                    }
                    else {
                        HDfprintf(stderr, "pio_perf: invalid --api option %s\n", buf);
                        HDexit(EXIT_FAILURE);
                    }

                    if (*end == '\0')
                        break;

                    end++;
                }
            }

            break;
#if 0
        case 'b':
            /* the future "binary" option */
            break;
#endif /* 0 */
            case 'B':
                cl_opts->blk_size = (size_t)parse_size_directive(H5_optarg);
                break;
            case 'c':
                /* Turn on chunked HDF5 dataset creation */
                cl_opts->h5_use_chunks = TRUE;
                break;
            case 'C':
                cl_opts->collective = 1;
                break;
            case 'd':
                cl_opts->num_dsets = atoi(H5_optarg);
                break;
            case 'D': {
                const char *end = H5_optarg;

                while (end && *end != '\0') {
                    char buf[10];
                    int  i;

                    HDmemset(buf, '\0', sizeof(buf));

                    for (i = 0; *end != '\0' && *end != ','; ++end)
                        if (HDisalnum(*end) && i < 10)
                            buf[i++] = *end;

                    if (HDstrlen(buf) > 1 || HDisdigit(buf[0])) {
                        size_t j;

                        for (j = 0; j < 10 && buf[j] != '\0'; ++j)
                            if (!isdigit(buf[j])) {
                                HDfprintf(stderr, "pio_perf: invalid --debug option %s\n", buf);
                                HDexit(EXIT_FAILURE);
                            }

                        pio_debug_level = atoi(buf);

                        if (pio_debug_level > 4)
                            pio_debug_level = 4;
                        else if (pio_debug_level < 0)
                            pio_debug_level = 0;
                    }
                    else {
                        switch (*buf) {
                            case 'r':
                                /* Turn on raw data throughput info */
                                cl_opts->print_raw = TRUE;
                                break;
                            case 't':
                                /* Turn on time printing */
                                cl_opts->print_times = TRUE;
                                break;
                            case 'v':
                                /* Turn on verify data correctness*/
                                cl_opts->verify = TRUE;
                                break;
                            default:
                                HDfprintf(stderr, "pio_perf: invalid --debug option %s\n", buf);
                                HDexit(EXIT_FAILURE);
                        }
                    }

                    if (*end == '\0')
                        break;

                    end++;
                }
            }

            break;
            case 'e':
                cl_opts->num_bpp = parse_size_directive(H5_optarg);
                break;
            case 'F':
                cl_opts->num_files = HDatoi(H5_optarg);
                break;
            case 'g':
                cl_opts->dim2d = 1;
                break;
            case 'i':
                cl_opts->num_iters = HDatoi(H5_optarg);
                break;
            case 'I':
                cl_opts->interleaved = 1;
                break;
            case 'o':
                cl_opts->output_file = H5_optarg;
                break;
            case 'p':
                cl_opts->min_num_procs = HDatoi(H5_optarg);
                break;
            case 'P':
                cl_opts->max_num_procs = HDatoi(H5_optarg);
                break;
            case 'T':
                cl_opts->h5_threshold = parse_size_directive(H5_optarg);
                break;
            case 'w':
                cl_opts->h5_write_only = TRUE;
                break;
            case 'x':
                cl_opts->min_xfer_size = (size_t)parse_size_directive(H5_optarg);
                break;
            case 'X':
                cl_opts->max_xfer_size = (size_t)parse_size_directive(H5_optarg);
                break;
            case 'h':
            case '?':
            default:
                usage(progname);
                HDfree(cl_opts);
                return NULL;
        }
    }

    if (cl_opts->num_bpp == 0) {
        if (cl_opts->dim2d == 0)
            cl_opts->num_bpp = 256 * ONE_KB;
        else
            cl_opts->num_bpp = 8 * ONE_KB;
    }

    if (cl_opts->max_xfer_size == 0)
        cl_opts->max_xfer_size = (size_t)cl_opts->num_bpp;

    if (cl_opts->min_xfer_size == 0)
        cl_opts->min_xfer_size = (size_t)(cl_opts->num_bpp) / 2;

    if (cl_opts->blk_size == 0)
        cl_opts->blk_size = (size_t)(cl_opts->num_bpp) / 2;

    /* set default if none specified yet */
    if (!cl_opts->io_types)
        cl_opts->io_types = PIO_HDF5 | PIO_MPI | PIO_POSIX; /* run all API */

    /* verify parameters sanity.  Adjust if needed. */
    /* cap xfer_size with bytes per process */
    if (!cl_opts->dim2d) {
        if (cl_opts->min_xfer_size > (size_t)cl_opts->num_bpp)
            cl_opts->min_xfer_size = (size_t)cl_opts->num_bpp;
        if (cl_opts->max_xfer_size > (size_t)cl_opts->num_bpp)
            cl_opts->max_xfer_size = (size_t)cl_opts->num_bpp;
    }
    if (cl_opts->min_xfer_size > cl_opts->max_xfer_size)
        cl_opts->min_xfer_size = cl_opts->max_xfer_size;
    if (cl_opts->blk_size > (size_t)cl_opts->num_bpp)
        cl_opts->blk_size = (size_t)cl_opts->num_bpp;
    /* check range of number of processes */
    if (cl_opts->min_num_procs <= 0)
        cl_opts->min_num_procs = 1;
    if (cl_opts->max_num_procs <= 0)
        cl_opts->max_num_procs = 1;
    if (cl_opts->min_num_procs > cl_opts->max_num_procs)
        cl_opts->min_num_procs = cl_opts->max_num_procs;
    /* check iteration */
    if (cl_opts->num_iters <= 0)
        cl_opts->num_iters = 1;

    return cl_opts;
}

/*
 * Function:    parse_size_directive
 * Purpose:     Parse the size directive passed on the commandline. The size
 *              directive is an integer followed by a size indicator:
 *
 *                  K, k - Kilobyte
 *                  M, m - Megabyte
 *                  G, g - Gigabyte
 *
 * Return:      The size as a off_t because this is related to file size.
 *              If an unknown size indicator is used, then the program will
 *              exit with EXIT_FAILURE as the return value.
 * Programmer:  Bill Wendling, 18. December 2001
 * Modifications:
 */
static off_t
parse_size_directive(const char *size)
{
    off_t s;
    char *endptr;

    s = HDstrtol(size, &endptr, 10);

    if (endptr && *endptr) {
        while (*endptr != '\0' && (*endptr == ' ' || *endptr == '\t'))
            ++endptr;

        switch (*endptr) {
            case 'K':
            case 'k':
                s *= ONE_KB;
                break;
            case 'M':
            case 'm':
                s *= ONE_MB;
                break;
            case 'G':
            case 'g':
                s *= ONE_GB;
                break;
            default:
                HDfprintf(stderr, "Illegal size specifier '%c'\n", *endptr);
                HDexit(EXIT_FAILURE);
        }
    }

    return s;
}

/*
 * Function:    usage
 * Purpose:     Print a usage message and then exit.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 31. October 2001
 * Modifications:
 *     Added 2D testing (Christian Chilan, 10. August 2005)
 */
static void
usage(const char *prog)
{
    int myrank;

    MPI_Comm_rank(pio_comm_g, &myrank);

    if (myrank == 0) {
        print_version(prog);
        HDprintf("usage: %s [OPTIONS]\n", prog);
        HDprintf("  OPTIONS\n");
        HDprintf("     -h, --help                  Print a usage message and exit\n");
        HDprintf("     -a S, --align=S             Alignment of objects in HDF5 file [default: 1]\n");
        HDprintf("     -A AL, --api=AL             Which APIs to test [default: all of them]\n");
#if 0
        HDprintf("     -b, --binary                The elusive binary option\n");
#endif /* 0 */
        HDprintf("     -B S, --block-size=S        Block size within transfer buffer\n");
        HDprintf("                                 (see below for description)\n");
        HDprintf("                                 [default: half the number of bytes per process\n");
        HDprintf("                                           per dataset]\n");
        HDprintf("     -c, --chunk                 Create HDF5 datasets using chunked storage\n");
        HDprintf("                                 [default: contiguous storage]\n");
        HDprintf("     -C, --collective            Use collective I/O for MPI and HDF5 APIs\n");
        HDprintf("                                 [default: independent I/O)\n");
        HDprintf("     -d N, --num-dsets=N         Number of datasets per file [default: 1]\n");
        HDprintf("     -D DL, --debug=DL           Indicate the debugging level\n");
        HDprintf("                                 [default: no debugging]\n");
        HDprintf("     -e S, --num-bytes=S         Number of bytes per process per dataset\n");
        HDprintf("                                 (see below for description)\n");
        HDprintf("                                 [default: 256K for 1D, 8K for 2D]\n");
        HDprintf("     -F N, --num-files=N         Number of files [default: 1]\n");
        HDprintf("     -g, --geometry              Use 2D geometry [default: 1D geometry]\n");
        HDprintf("     -i N, --num-iterations=N    Number of iterations to perform [default: 1]\n");
        HDprintf("     -I, --interleaved           Interleaved access pattern\n");
        HDprintf("                                 (see below for example)\n");
        HDprintf("                                 [default: Contiguous access pattern]\n");
        HDprintf("     -o F, --output=F            Output raw data into file F [default: none]\n");
        HDprintf("     -p N, --min-num-processes=N Minimum number of processes to use [default: 1]\n");
        HDprintf("     -P N, --max-num-processes=N Maximum number of processes to use\n");
        HDprintf("                                 [default: all MPI_COMM_WORLD processes ]\n");
        HDprintf("     -T S, --threshold=S         Threshold for alignment of objects in HDF5 file\n");
        HDprintf("                                 [default: 1]\n");
        HDprintf("     -w, --write-only            Perform write tests not the read tests\n");
        HDprintf("     -x S, --min-xfer-size=S     Minimum transfer buffer size\n");
        HDprintf("                                 (see below for description)\n");
        HDprintf("                                 [default: half the number of bytes per process\n");
        HDprintf("                                           per dataset]\n");
        HDprintf("     -X S, --max-xfer-size=S     Maximum transfer buffer size\n");
        HDprintf("                                 [default: the number of bytes per process per\n");
        HDprintf("                                           dataset]\n");
        HDprintf("\n");
        HDprintf("  F  - is a filename.\n");
        HDprintf("  N  - is an integer >=0.\n");
        HDprintf("  S  - is a size specifier, an integer >=0 followed by a size indicator:\n");
        HDprintf("          K - Kilobyte (%d)\n", ONE_KB);
        HDprintf("          M - Megabyte (%d)\n", ONE_MB);
        HDprintf("          G - Gigabyte (%d)\n", ONE_GB);
        HDprintf("\n");
        HDprintf("      Example: '37M' is 37 megabytes or %d bytes\n", 37 * ONE_MB);
        HDprintf("\n");
        HDprintf("  AL - is an API list. Valid values are:\n");
        HDprintf("          phdf5 - Parallel HDF5\n");
        HDprintf("          mpiio - MPI-I/O\n");
        HDprintf("          posix - POSIX\n");
        HDprintf("\n");
        HDprintf("      Example: --api=mpiio,phdf5\n");
        HDprintf("\n");
        HDprintf("  Dataset size:\n");
        HDprintf("      Depending on the selected geometry, each test dataset is either a linear\n");
        HDprintf("      array of size bytes-per-process * num-processes, or a square array of size\n");
        HDprintf("      (bytes-per-process * num-processes) x (bytes-per-process * num-processes).\n");
        HDprintf("\n");
        HDprintf("  Block size vs. Transfer buffer size:\n");
        HDprintf("      buffer-size controls the size of the memory buffer, which is broken into\n");
        HDprintf("      blocks and written to the file. Depending on the selected geometry, each\n");
        HDprintf("      block can be a linear array of size block-size or a square array of size\n");
        HDprintf("      block-size x block-size. The arrangement in which blocks are written is\n");
        HDprintf("      determined by the access pattern.\n");
        HDprintf("\n");
        HDprintf("      In 1D geometry, the transfer buffer is a linear array of size buffer-size.\n");
        HDprintf("      In 2D geometry, it is a rectangular array of size block-size x buffer-size\n");
        HDprintf("      or buffer-size x block-size if interleaved pattern is selected.\n");
        HDprintf("\n");
        HDprintf("  Interleaved and Contiguous patterns in 1D geometry:\n");
        HDprintf("      When contiguous access pattern is chosen, the dataset is evenly divided\n");
        HDprintf("      into num-processes regions and each process writes data to its own region.\n");
        HDprintf("      When interleaved blocks are written to a dataset, space for the first\n");
        HDprintf("      block of the first process is allocated in the dataset, then space is\n");
        HDprintf("      allocated for the first block of the second process, etc. until space is\n");
        HDprintf("      allocated for the first block of each process, then space is allocated for\n");
        HDprintf("      the second block of the first process, the second block of the second\n");
        HDprintf("      process, etc.\n");
        HDprintf("\n");
        HDprintf("      For example, with a 3 process run, 512KB bytes-per-process, 256KB transfer\n");
        HDprintf("        buffer size, and 64KB block size, each process must issue 2 transfer\n");
        HDprintf("        requests to complete access to the dataset.\n");
        HDprintf("          Contiguous blocks of the first transfer request are written like so:\n");
        HDprintf("              1111----2222----3333----\n");
        HDprintf("          Interleaved blocks of the first transfer request are written like so:\n");
        HDprintf("              123123123123------------\n");
        HDprintf("        The actual number of I/O operations involved in a transfer request\n");
        HDprintf("        depends on the access pattern and communication mode.\n");
        HDprintf("        When using independent I/O with interleaved pattern, each process\n");
        HDprintf("        performs 4 small non-contiguous I/O operations per transfer request.\n");
        HDprintf("        If collective I/O is turned on, the combined content of the buffers of\n");
        HDprintf("        the 3 processes will be written using one collective I/O operation\n");
        HDprintf("        per transfer request.\n");
        HDprintf("\n");
        HDprintf("      For information about access patterns in 2D geometry, please refer to the\n");
        HDprintf("      HDF5 Reference Manual.\n");
        HDprintf("\n");
        HDprintf("  DL - is a list of debugging flags. Valid values are:\n");
        HDprintf("          1 - Minimal\n");
        HDprintf("          2 - Not quite everything\n");
        HDprintf("          3 - Everything\n");
        HDprintf("          4 - The kitchen sink\n");
        HDprintf("          r - Raw data I/O throughput information\n");
        HDprintf("          t - Times as well as throughputs\n");
        HDprintf("          v - Verify data correctness\n");
        HDprintf("\n");
        HDprintf("      Example: --debug=2,r,t\n");
        HDprintf("\n");
        HDprintf("  Environment variables:\n");
        HDprintf("  HDF5_NOCLEANUP   Do not remove data files if set [default remove]\n");
        HDprintf("  HDF5_MPI_INFO    MPI INFO object key=value separated by ;\n");
        HDprintf("  HDF5_PARAPREFIX  Parallel data files prefix\n");
        fflush(stdout);
    } /* end if */
} /* end usage() */

#else /* H5_HAVE_PARALLEL */

/*
 * Function:    main
 * Purpose:     Dummy main() function for if HDF5 was configured without
 *              parallel stuff.
 * Return:      EXIT_SUCCESS
 * Programmer:  Bill Wendling, 14. November 2001
 */
int
main(void)
{
    HDprintf("No parallel IO performance because parallel is not configured\n");
    return EXIT_SUCCESS;
} /* end main */

#endif /* !H5_HAVE_PARALLEL */
