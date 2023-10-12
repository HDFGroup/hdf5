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
 * Serial HDF5 Performance Testing Code
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
 *
 *      IO API = HDF5
 *          # Files = 1, # of dsets = 1000, Elements per dset = 37000
 *              Write Results = x MB/s
 *              Read Results = x MB/s
 *          # Files = 1, # of dsets = 3000, Elements per dset = 37000
 *              Write Results = x MB/s
 *              Read Results = x MB/s
 *
 *          . . .
 *
 *
 *      . . .
 *
 */

/* system header files */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "hdf5.h"

/* our header files */
#include "sio_perf.h"

/* useful macros */
#define TAB_SPACE 4

#define ONE_KB 1024
#define ONE_MB (ONE_KB * ONE_KB)
#define ONE_GB (ONE_MB * ONE_KB)

#define SIO_POSIX 0x1
#define SIO_HDF5  0x4

/* report 0.0 in case t is zero too */
#define MB_PER_SEC(bytes, t) (H5_DBL_ABS_EQUAL(t, 0.0) ? 0.0 : ((((double)(bytes)) / (double)ONE_MB) / (t)))

#ifndef true
#define true 1
#endif /* true */
#ifndef false
#define false (!true)
#endif /* false */

/* global variables */
FILE *output;              /* output file                          */
int   sio_debug_level = 0; /* The debug level:
                            *   0 - Off
                            *   1 - Minimal
                            *   2 - Some more
                            *   3 - Maximal
                            *   4 - Maximal & then some
                            */

/* local variables */
static const char *progname = "h5perf_serial";

/*
 * Command-line options: The user can specify short or long-named
 * parameters. The long-named ones can be partially spelled. When
 * adding more, make sure that they don't clash with each other.
 */

/*
 * It seems that only the options that accept additional information
 * such as dataset size (-e) require the colon next to it.
 */
static const char            *s_opts   = "a:A:B:c:Cd:D:e:F:ghi:Imno:p:P:r:stT:v:wx:X:";
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
                                          {"file-driver", require_arg, 'v'},
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
                                          {"order", require_arg, 'r'},
                                          {"output", require_arg, 'o'},
                                          {"extendable", no_arg, 't'},
                                          {"threshold", require_arg, 'T'},
                                          {"write-only", require_arg, 'w'},
                                          {NULL, 0, '\0'}};

struct options {
    long        io_types;            /* bitmask of which I/O types to test   */
    const char *output_file;         /* file to print report to              */
    long        num_dsets;           /* number of datasets                   */
    long        num_files;           /* number of files                      */
    off_t       num_bpp;             /* number of bytes per proc per dset    */
    int         num_iters;           /* number of iterations                 */
    hsize_t     dset_size[MAX_DIMS]; /* Dataset size                           */
    size_t      buf_size[MAX_DIMS];  /* Buffer size                           */
    size_t      chk_size[MAX_DIMS];  /* Chunk size                           */
    int         order[MAX_DIMS];     /* Dimension access order                           */
    int         dset_rank;           /* Rank                   */
    int         buf_rank;            /* Rank                   */
    int         order_rank;          /* Rank                   */
    int         chk_rank;            /* Rank                   */
    int         print_times;         /* print times as well as throughputs   */
    int         print_raw;           /* print raw data throughput info       */
    hsize_t     h5_alignment;        /* alignment in HDF5 file               */
    hsize_t     h5_threshold;        /* threshold for alignment in HDF5 file */
    int         h5_use_chunks;       /* Make HDF5 dataset chunked            */
    int         h5_write_only;       /* Perform the write tests only         */
    int         h5_extendable;       /* Perform the write tests only         */
    int         verify;              /* Verify data correctness              */
    vfdtype     vfd;                 /* File driver */
    size_t      page_buffer_size;
    size_t      page_size;
};

typedef struct {
    double min;
    double max;
    double sum;
    int    num;
} minmax;

/* local functions */
static hsize_t         parse_size_directive(const char *size);
static struct options *parse_command_line(int argc, const char *const *argv);
static void            run_test_loop(struct options *options);
static int             run_test(iotype iot, parameters parms, struct options *opts);
static void            output_all_info(minmax *mm, int count, int indent_level);
static void            get_minmax(minmax *mm, double val);
static void            accumulate_minmax_stuff(const minmax *mm, int count, minmax *total_mm);
static void output_results(const struct options *options, const char *name, minmax *table, int table_size,
                           off_t data_size);
static void output_report(const char *fmt, ...) H5_ATTR_FORMAT(printf, 1, 2);
static void print_indent(int indent);
static void usage(const char *prog);
static void report_parameters(struct options *opts);

/*
 * Function:    main
 * Purpose:     Start things up.
 * Return:      EXIT_SUCCESS or EXIT_FAILURE
 */
int
main(int argc, char *argv[])
{
    int             exit_value = EXIT_SUCCESS;
    struct options *opts       = NULL;

    /* Initialize h5tools lib */
    h5tools_init();

    output = stdout;

    opts = parse_command_line(argc, (const char *const *)argv);

    if (!opts) {
        exit_value = EXIT_FAILURE;
        goto finish;
    }

    if (opts->output_file) {
        if ((output = fopen(opts->output_file, "w")) == NULL) {
            fprintf(stderr, "%s: cannot open output file\n", progname);
            perror(opts->output_file);
            goto finish;
        }
    }

    report_parameters(opts);

    run_test_loop(opts);

finish:
    free(opts);
    return exit_value;
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
 *              three choices: POSIXIO, and HDF5.
 *
 *            - Then we change the size of the buffer. This information is
 *              inferred from the number of datasets to create and the number
 *              of integers to put into each dataset. The backend code figures
 *              this out.
 *
 * Return:      Nothing
 */
static void
run_test_loop(struct options *opts)
{
    parameters parms;
    int        i;
    size_t     buf_bytes;

    /* load options into parameter structure */
    parms.num_files        = opts->num_files;
    parms.num_dsets        = opts->num_dsets;
    parms.num_iters        = opts->num_iters;
    parms.rank             = opts->dset_rank;
    parms.h5_align         = opts->h5_alignment;
    parms.h5_thresh        = opts->h5_threshold;
    parms.h5_use_chunks    = opts->h5_use_chunks;
    parms.h5_extendable    = opts->h5_extendable;
    parms.h5_write_only    = opts->h5_write_only;
    parms.verify           = opts->verify;
    parms.vfd              = opts->vfd;
    parms.page_buffer_size = opts->page_buffer_size;
    parms.page_size        = opts->page_size;

    /* load multidimensional options */
    parms.num_bytes = 1;
    buf_bytes       = 1;
    for (i = 0; i < parms.rank; i++) {
        parms.buf_size[i]  = opts->buf_size[i];
        parms.dset_size[i] = opts->dset_size[i];
        parms.chk_size[i]  = opts->chk_size[i];
        parms.order[i]     = opts->order[i];
        parms.num_bytes *= opts->dset_size[i];
        buf_bytes *= opts->buf_size[i];
    }

    /* print size information */
    output_report("Transfer Buffer Size (bytes): %zu\n", buf_bytes);
    output_report("File Size(MB): %.2f\n", ((double)parms.num_bytes) / ONE_MB);

    print_indent(0);
    if (opts->io_types & SIO_POSIX)
        run_test(POSIXIO, parms, opts);

    print_indent(0);
    if (opts->io_types & SIO_HDF5)
        run_test(HDF5, parms, opts);
}

/*
 * Function:    run_test
 * Purpose:     Inner loop call to actually run the I/O test.
 * Return:      Nothing
 */
static int
run_test(iotype iot, parameters parms, struct options *opts)
{
    results res;
    int     i, ret_value = SUCCESS;
    off_t   raw_size;
    minmax *write_sys_mm_table   = NULL;
    minmax *write_mm_table       = NULL;
    minmax *write_gross_mm_table = NULL;
    minmax *write_raw_mm_table   = NULL;
    minmax *read_sys_mm_table    = NULL;
    minmax *read_mm_table        = NULL;
    minmax *read_gross_mm_table  = NULL;
    minmax *read_raw_mm_table    = NULL;
    minmax  write_sys_mm         = {0.0, 0.0, 0.0, 0};
    minmax  write_mm             = {0.0, 0.0, 0.0, 0};
    minmax  write_gross_mm       = {0.0, 0.0, 0.0, 0};
    minmax  write_raw_mm         = {0.0, 0.0, 0.0, 0};
    minmax  read_sys_mm          = {0.0, 0.0, 0.0, 0};
    minmax  read_mm              = {0.0, 0.0, 0.0, 0};
    minmax  read_gross_mm        = {0.0, 0.0, 0.0, 0};
    minmax  read_raw_mm          = {0.0, 0.0, 0.0, 0};

    raw_size      = (off_t)parms.num_bytes;
    parms.io_type = iot;
    print_indent(2);
    output_report("IO API = ");

    switch (iot) {
        case POSIXIO:
            output_report("POSIX\n");
            break;
        case HDF5:
            output_report("HDF5\n");
            break;
        default:
            /* unknown request */
            fprintf(stderr, "Unknown IO type request (%d)\n", (int)iot);
            assert(0 && "Unknown IO tpe");
            break;
    }

    /* allocate space for tables minmax and that it is sufficient */
    /* to initialize all elements to zeros by calloc.             */
    write_sys_mm_table   = (minmax *)calloc((size_t)parms.num_iters, sizeof(minmax));
    write_mm_table       = (minmax *)calloc((size_t)parms.num_iters, sizeof(minmax));
    write_gross_mm_table = (minmax *)calloc((size_t)parms.num_iters, sizeof(minmax));
    write_raw_mm_table   = (minmax *)calloc((size_t)parms.num_iters, sizeof(minmax));

    if (!parms.h5_write_only) {
        read_sys_mm_table   = (minmax *)calloc((size_t)parms.num_iters, sizeof(minmax));
        read_mm_table       = (minmax *)calloc((size_t)parms.num_iters, sizeof(minmax));
        read_gross_mm_table = (minmax *)calloc((size_t)parms.num_iters, sizeof(minmax));
        read_raw_mm_table   = (minmax *)calloc((size_t)parms.num_iters, sizeof(minmax));
    }

    /* Do IO iteration times, collecting statistics each time */
    for (i = 0; i < parms.num_iters; ++i) {
        double t;

        do_sio(parms, &res);

        /* gather all of the "sys write" times */
        t = io_time_get(res.timers, HDF5_MPI_WRITE);
        get_minmax(&write_sys_mm, t);

        write_sys_mm_table[i] = write_sys_mm;

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

        if (!parms.h5_write_only) {
            /* gather all of the "mpi read" times */
            t = io_time_get(res.timers, HDF5_MPI_READ);
            get_minmax(&read_sys_mm, t);

            read_sys_mm_table[i] = read_sys_mm;

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

            read_raw_mm_table[i] = read_gross_mm;
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
        if (sio_debug_level >= 3) {
            /* output all of the times for all iterations */
            print_indent(3);
            output_report("Raw Data Write details:\n");
            output_all_info(write_raw_mm_table, parms.num_iters, 4);
        }

        output_results(opts, "Raw Data Write", write_raw_mm_table, parms.num_iters, raw_size);
    } /* end if */

    /* show sys write statistics */
#if 0
    if (sio_debug_level >= 3) {
        /* output all of the times for all iterations */
        print_indent(3);
        output_report("MPI Write details:\n");
        output_all_info(write_sys_mm_table, parms.num_iters, 4);
    }
#endif
    /* We don't currently output the MPI write results */

    /* accumulate and output the max, min, and average "write" times */
    if (sio_debug_level >= 3) {
        /* output all of the times for all iterations */
        print_indent(3);
        output_report("Write details:\n");
        output_all_info(write_mm_table, parms.num_iters, 4);
    }

    output_results(opts, "Write", write_mm_table, parms.num_iters, raw_size);

    /* accumulate and output the max, min, and average "gross write" times */
    if (sio_debug_level >= 3) {
        /* output all of the times for all iterations */
        print_indent(3);
        output_report("Write Open-Close details:\n");
        output_all_info(write_gross_mm_table, parms.num_iters, 4);
    }

    output_results(opts, "Write Open-Close", write_gross_mm_table, parms.num_iters, raw_size);

    if (!parms.h5_write_only) {
        /* Read statistics    */
        /* Print the raw data throughput if desired */
        if (opts->print_raw) {
            /* accumulate and output the max, min, and average "raw read" times */
            if (sio_debug_level >= 3) {
                /* output all of the times for all iterations */
                print_indent(3);
                output_report("Raw Data Read details:\n");
                output_all_info(read_raw_mm_table, parms.num_iters, 4);
            }

            output_results(opts, "Raw Data Read", read_raw_mm_table, parms.num_iters, raw_size);
        } /* end if */

        /* show mpi read statistics */
#if 0
        if (sio_debug_level >= 3) {
            /* output all of the times for all iterations */
            print_indent(3);
            output_report("MPI Read details:\n");
            output_all_info(read_sys_mm_table, parms.num_iters, 4);
        }
#endif
        /* We don't currently output the MPI read results */

        /* accumulate and output the max, min, and average "read" times */
        if (sio_debug_level >= 3) {
            /* output all of the times for all iterations */
            print_indent(3);
            output_report("Read details:\n");
            output_all_info(read_mm_table, parms.num_iters, 4);
        }

        output_results(opts, "Read", read_mm_table, parms.num_iters, raw_size);

        /* accumulate and output the max, min, and average "gross read" times */
        if (sio_debug_level >= 3) {
            /* output all of the times for all iterations */
            print_indent(3);
            output_report("Read Open-Close details:\n");
            output_all_info(read_gross_mm_table, parms.num_iters, 4);
        }

        output_results(opts, "Read Open-Close", read_gross_mm_table, parms.num_iters, raw_size);
    }

    /* clean up our mess */
    free(write_sys_mm_table);
    free(write_mm_table);
    free(write_gross_mm_table);
    free(write_raw_mm_table);

    if (!parms.h5_write_only) {
        free(read_sys_mm_table);
        free(read_mm_table);
        free(read_gross_mm_table);
        free(read_raw_mm_table);
    }

    return ret_value;
}

/*
 * Function:    output_all_info
 * Purpose:
 * Return:      Nothing
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
 * Function:    get_minmax
 * Purpose:     Gather all the min, max and total of val.
 * Return:      Nothing
 */

static void
get_minmax(minmax *mm, double val)
{
    mm->max = val;
    mm->min = val;
    mm->sum = val;
}

/*
 * Function:    accumulate_minmax_stuff
 * Purpose:     Accumulate the minimum, maximum, and average of the times
 *              across all processes.
 * Return:      TOTAL_MM - the total of all of these.
 */
static void
accumulate_minmax_stuff(const minmax *mm, int count, minmax *total_mm)
{
    int i;

    total_mm->sum = 0.0;
    total_mm->max = -DBL_MAX;
    total_mm->min = DBL_MAX;
    total_mm->num = count;

    for (i = 0; i < count; ++i) {
        double m = mm[i].max;

        total_mm->sum += m;

        if (m < total_mm->min)
            total_mm->min = m;

        if (m > total_mm->max)
            total_mm->max = m;
    }
}

/*
 * Function:    output_results
 * Purpose:     Print information about the time & bandwidth for a given
 *                  minmax & # of iterations.
 * Return:      Nothing
 */
static void
output_results(const struct options *opts, const char *name, minmax *table, int table_size, off_t data_size)
{
    minmax total_mm;

    accumulate_minmax_stuff(table, table_size, &total_mm);

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

/*
 * Function:    output_report
 * Purpose:     Print a line of the report. Only do so if I'm the 0 process.
 * Return:      Nothing
 */
static void
output_report(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    H5_GCC_CLANG_DIAG_OFF("format-nonliteral")
    vfprintf(output, fmt, ap);
    H5_GCC_CLANG_DIAG_ON("format-nonliteral")
    va_end(ap);
}

/*
 * Function:    print_indent
 * Purpose:     Print spaces to indent a new line of text for pretty printing
 *              things.
 * Return:      Nothing
 */
static void
print_indent(int indent)
{
    indent *= TAB_SPACE;

    for (; indent > 0; --indent)
        fputc(' ', output);
}

static void
recover_size_and_print(long long val, const char *end)
{
    if (val >= ONE_KB && (val % ONE_KB) == 0) {
        if (val >= ONE_MB && (val % ONE_MB) == 0) {
            if (val >= ONE_GB && (val % ONE_GB) == 0)
                fprintf(output,
                        "%lld"
                        "GB%s",
                        val / ONE_GB, end);
            else
                fprintf(output,
                        "%lld"
                        "MB%s",
                        val / ONE_MB, end);
        }
        else {
            fprintf(output,
                    "%lld"
                    "KB%s",
                    val / ONE_KB, end);
        }
    }
    else {
        fprintf(output,
                "%lld"
                "%s",
                val, end);
    }
}

static void
print_io_api(long io_types)
{
    if (io_types & SIO_POSIX)
        fprintf(output, "posix ");
    if (io_types & SIO_HDF5)
        fprintf(output, "hdf5 ");
    fprintf(output, "\n");
}

static void
report_parameters(struct options *opts)
{
    int i, rank;
    rank = opts->dset_rank;

    print_version("HDF5 Library"); /* print library version */
    fprintf(output, "==== Parameters ====\n");

    fprintf(output, "IO API=");
    print_io_api(opts->io_types);

    fprintf(output, "Number of iterations=%d\n", opts->num_iters);

    fprintf(output, "Dataset size=");

    for (i = 0; i < rank; i++)
        recover_size_and_print((long long)opts->dset_size[i], " ");
    fprintf(output, "\n");

    fprintf(output, "Transfer buffer size=");
    for (i = 0; i < rank; i++)
        recover_size_and_print((long long)opts->buf_size[i], " ");
    fprintf(output, "\n");

    if (opts->page_size) {
        fprintf(output, "Page Aggregation Enabled. Page size = %zu\n", opts->page_size);
        if (opts->page_buffer_size)
            fprintf(output, "Page Buffering Enabled. Page Buffer size = %zu\n", opts->page_buffer_size);
        else
            fprintf(output, "Page Buffering Disabled\n");
    }
    else
        fprintf(output, "Page Aggregation Disabled\n");

    fprintf(output, "Dimension access order=");
    for (i = 0; i < rank; i++)
        recover_size_and_print((long long)opts->order[i], " ");
    fprintf(output, "\n");

    if (opts->io_types & SIO_HDF5) {

        fprintf(output, "HDF5 data storage method=");

        if (opts->h5_use_chunks) {

            fprintf(output, "Chunked\n");
            fprintf(output, "HDF5 chunk size=");
            for (i = 0; i < rank; i++)
                recover_size_and_print((long long)opts->chk_size[i], " ");
            fprintf(output, "\n");

            fprintf(output, "HDF5 dataset dimensions=");
            if (opts->h5_extendable) {
                fprintf(output, "Extendable\n");
            }
            else {
                fprintf(output, "Fixed\n");
            }
        }
        else {
            fprintf(output, "Contiguous\n");
        }

        fprintf(output, "HDF5 file driver=");
        if (opts->vfd == sec2) {
            fprintf(output, "sec2\n");
        }
        else if (opts->vfd == stdio) {
            fprintf(output, "stdio\n");
        }
        else if (opts->vfd == core) {
            fprintf(output, "core\n");
        }
        else if (opts->vfd == split) {
            fprintf(output, "split\n");
        }
        else if (opts->vfd == multi) {
            fprintf(output, "multi\n");
        }
        else if (opts->vfd == family) {
            fprintf(output, "family\n");
        }
        else if (opts->vfd == direct) {
            fprintf(output, "direct\n");
        }
    }

    {
        char *prefix = getenv("HDF5_PREFIX");

        fprintf(output, "Env HDF5_PREFIX=%s\n", (prefix ? prefix : "not set"));
    }

    fprintf(output, "==== End of Parameters ====\n");
    fprintf(output, "\n");
}

/*
 * Function:    parse_command_line
 * Purpose:     Parse the command line options and return a STRUCT OPTIONS
 *              structure which will need to be freed by the calling function.
 * Return:      Pointer to an OPTIONS structure
 */
static struct options *
parse_command_line(int argc, const char *const *argv)
{
    int             opt;
    struct options *cl_opts;
    int             i, default_rank, actual_rank, ranks[4];

    cl_opts = (struct options *)malloc(sizeof(struct options));

    cl_opts->page_buffer_size = 0;
    cl_opts->page_size        = 0;

    cl_opts->output_file = NULL;
    cl_opts->io_types    = 0; /* will set default after parsing options */
    cl_opts->num_iters   = 1;

    default_rank = 2;

    cl_opts->dset_rank  = 0;
    cl_opts->buf_rank   = 0;
    cl_opts->chk_rank   = 0;
    cl_opts->order_rank = 0;

    for (i = 0; i < MAX_DIMS; i++) {
        cl_opts->buf_size[i]  = (size_t)((i + 1) * 10);
        cl_opts->dset_size[i] = (hsize_t)((i + 1) * 100);
        cl_opts->chk_size[i]  = (size_t)((i + 1) * 10);
        cl_opts->order[i]     = i + 1;
    }

    cl_opts->vfd = sec2;

    cl_opts->print_times   = false; /* Printing times is off by default */
    cl_opts->print_raw     = false; /* Printing raw data throughput is off by default */
    cl_opts->h5_alignment  = 1;     /* No alignment for HDF5 objects by default */
    cl_opts->h5_threshold  = 1;     /* No threshold for aligning HDF5 objects by default */
    cl_opts->h5_use_chunks = false; /* Don't chunk the HDF5 dataset by default */
    cl_opts->h5_write_only = false; /* Do both read and write by default */
    cl_opts->h5_extendable = false; /* Use extendable dataset */
    cl_opts->verify        = false; /* No Verify data correctness by default */

    while ((opt = H5_get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
            case 'a':
                cl_opts->h5_alignment = parse_size_directive(H5_optarg);
                break;
            case 'G':
                cl_opts->page_size = parse_size_directive(H5_optarg);
                break;
            case 'b':
                cl_opts->page_buffer_size = parse_size_directive(H5_optarg);
                break;
            case 'A': {
                const char *end = H5_optarg;
                while (end && *end != '\0') {
                    char buf[10];

                    memset(buf, '\0', sizeof(buf));

                    for (i = 0; *end != '\0' && *end != ','; ++end)
                        if (isalnum(*end) && i < 10)
                            buf[i++] = *end;

                    if (!HDstrcasecmp(buf, "hdf5")) {
                        cl_opts->io_types |= SIO_HDF5;
                    }
                    else if (!HDstrcasecmp(buf, "posix")) {
                        cl_opts->io_types |= SIO_POSIX;
                    }
                    else {
                        fprintf(stderr, "sio_perf: invalid --api option %s\n", buf);
                        exit(EXIT_FAILURE);
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
            case 'c':
                /* Turn on chunked HDF5 dataset creation */
                cl_opts->h5_use_chunks = 1;
                {
                    const char *end = H5_optarg;
                    int         j   = 0;

                    while (end && *end != '\0') {
                        char buf[10];

                        memset(buf, '\0', sizeof(buf));

                        for (i = 0; *end != '\0' && *end != ','; ++end)
                            if (isalnum(*end) && i < 10)
                                buf[i++] = *end;

                        cl_opts->chk_size[j] = parse_size_directive(buf);

                        j++;

                        if (*end == '\0')
                            break;

                        end++;
                    }
                    cl_opts->chk_rank = j;
                }

                break;

            case 'D': {
                const char *end = H5_optarg;

                while (end && *end != '\0') {
                    char buf[10];

                    memset(buf, '\0', sizeof(buf));

                    for (i = 0; *end != '\0' && *end != ','; ++end)
                        if (isalnum(*end) && i < 10)
                            buf[i++] = *end;

                    if (strlen(buf) > 1 || isdigit(buf[0])) {
                        size_t j;

                        for (j = 0; j < 10 && buf[j] != '\0'; ++j)
                            if (!isdigit(buf[j])) {
                                fprintf(stderr, "sio_perf: invalid --debug option %s\n", buf);
                                exit(EXIT_FAILURE);
                            }

                        sio_debug_level = atoi(buf);

                        if (sio_debug_level > 4)
                            sio_debug_level = 4;
                        else if (sio_debug_level < 0)
                            sio_debug_level = 0;
                    }
                    else {
                        switch (*buf) {
                            case 'r':
                                /* Turn on raw data throughput info */
                                cl_opts->print_raw = true;
                                break;
                            case 't':
                                /* Turn on time printing */
                                cl_opts->print_times = true;
                                break;
                            case 'v':
                                /* Turn on verify data correctness*/
                                cl_opts->verify = true;
                                break;
                            default:
                                fprintf(stderr, "sio_perf: invalid --debug option %s\n", buf);
                                exit(EXIT_FAILURE);
                        }
                    }

                    if (*end == '\0')
                        break;

                    end++;
                }
            }

            break;
            case 'e': {
                const char *end = H5_optarg;
                int         j   = 0;

                while (end && *end != '\0') {
                    char buf[10];

                    memset(buf, '\0', sizeof(buf));

                    for (i = 0; *end != '\0' && *end != ','; ++end)
                        if (isalnum(*end) && i < 10)
                            buf[i++] = *end;

                    cl_opts->dset_size[j] = parse_size_directive(buf);

                    j++;

                    if (*end == '\0')
                        break;

                    end++;
                }
                cl_opts->dset_rank = j;
            }

            break;

            case 'i':
                cl_opts->num_iters = atoi(H5_optarg);
                break;
            case 'o':
                cl_opts->output_file = H5_optarg;
                break;
            case 'T':
                cl_opts->h5_threshold = parse_size_directive(H5_optarg);
                break;
            case 'v':
                if (!HDstrcasecmp(H5_optarg, "sec2")) {
                    cl_opts->vfd = sec2;
                }
                else if (!HDstrcasecmp(H5_optarg, "stdio")) {
                    cl_opts->vfd = stdio;
                }
                else if (!HDstrcasecmp(H5_optarg, "core")) {
                    cl_opts->vfd = core;
                }
                else if (!HDstrcasecmp(H5_optarg, "split")) {
                    cl_opts->vfd = split;
                }
                else if (!HDstrcasecmp(H5_optarg, "multi")) {
                    cl_opts->vfd = multi;
                }
                else if (!HDstrcasecmp(H5_optarg, "family")) {
                    cl_opts->vfd = family;
                }
                else if (!HDstrcasecmp(H5_optarg, "direct")) {
                    cl_opts->vfd = direct;
                }
                else {
                    fprintf(stderr, "sio_perf: invalid --api option %s\n", H5_optarg);
                    exit(EXIT_FAILURE);
                }
                break;
            case 'w':
                cl_opts->h5_write_only = true;
                break;
            case 't':
                cl_opts->h5_extendable = true;
                break;
            case 'x': {
                const char *end = H5_optarg;
                int         j   = 0;

                while (end && *end != '\0') {
                    char buf[10];

                    memset(buf, '\0', sizeof(buf));

                    for (i = 0; *end != '\0' && *end != ','; ++end)
                        if (isalnum(*end) && i < 10)
                            buf[i++] = *end;

                    cl_opts->buf_size[j] = parse_size_directive(buf);

                    j++;

                    if (*end == '\0')
                        break;

                    end++;
                }
                cl_opts->buf_rank = j;
            }

            break;

            case 'r': {
                const char *end = H5_optarg;
                int         j   = 0;

                while (end && *end != '\0') {
                    char buf[10];

                    memset(buf, '\0', sizeof(buf));

                    for (i = 0; *end != '\0' && *end != ','; ++end)
                        if (isalnum(*end) && i < 10)
                            buf[i++] = *end;

                    cl_opts->order[j] = (int)parse_size_directive(buf);

                    j++;

                    if (*end == '\0')
                        break;

                    end++;
                }

                cl_opts->order_rank = j;
            }

            break;

            case 'h':
            case '?':
            default:
                usage(progname);
                free(cl_opts);
                return NULL;
        }
    }

    /* perform rank consistency analysis */
    actual_rank = 0;

    ranks[0] = cl_opts->dset_rank;
    ranks[1] = cl_opts->buf_rank;
    ranks[2] = cl_opts->order_rank;
    ranks[3] = cl_opts->chk_rank;

    for (i = 0; i < 4; i++) {
        if (ranks[i] > 0) {
            if (!actual_rank) {
                actual_rank = ranks[i];
            }
            else {
                if (actual_rank != ranks[i])
                    exit(EXIT_FAILURE);
            }
        }
    }

    if (!actual_rank)
        actual_rank = default_rank;

    cl_opts->dset_rank  = actual_rank;
    cl_opts->buf_rank   = actual_rank;
    cl_opts->order_rank = actual_rank;
    cl_opts->chk_rank   = actual_rank;

    for (i = 0; i < actual_rank; i++) {
        if (cl_opts->order[i] > actual_rank) {
            exit(EXIT_FAILURE);
        }
    }

    /* set default if none specified yet */
    if (!cl_opts->io_types)
        cl_opts->io_types = SIO_HDF5 | SIO_POSIX; /* run all API */

    /* verify parameters sanity.  Adjust if needed. */
    /* cap xfer_size with bytes per process */
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
 */

static hsize_t
parse_size_directive(const char *size)
{
    hsize_t s;
    char   *endptr;

    s = strtoull(size, &endptr, 10);

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
                fprintf(stderr, "Illegal size specifier '%c'\n", *endptr);
                exit(EXIT_FAILURE);
        }
    }

    return s;
}

/*
 * Function:    usage
 * Purpose:     Print a usage message and then exit.
 * Return:      Nothing
 */
static void
usage(const char *prog)
{
    print_version(prog);
    printf("usage: %s [OPTIONS]\n", prog);
    printf("  OPTIONS\n");
    printf("     -h                Print an usage message and exit\n");
    printf("     -A AL             Which APIs to test\n");
    printf("                       [default: all of them]\n");
    printf("     -c SL             Selects chunked storage and defines chunks dimensions\n");
    printf("                       and sizes\n");
    printf("                       [default: Off]\n");
    printf("     -e SL             Dimensions and sizes of dataset\n");
    printf("                       [default: 100,200]\n");
    printf("     -i N              Number of iterations to perform\n");
    printf("                       [default: 1]\n");
    printf("     -r NL             Dimension access order (see below for description)\n");
    printf("                       [default: 1,2]\n");
    printf("     -t                Selects extendable dimensions for HDF5 dataset\n");
    printf("                       [default: Off]\n");
    printf("     -v VFD            Selects file driver for HDF5 access\n");
    printf("                       [default: sec2]\n");
    printf("     -w                Perform write tests, not the read tests\n");
    printf("                       [default: Off]\n");
    printf("     -x SL             Dimensions and sizes of the transfer buffer\n");
    printf("                       [default: 10,20]\n");
    printf("\n");
    printf("  N  - is an integer > 0.\n");
    printf("\n");
    printf("  S  - is a size specifier, an integer > 0 followed by a size indicator:\n");
    printf("          K - Kilobyte (%d)\n", ONE_KB);
    printf("          M - Megabyte (%d)\n", ONE_MB);
    printf("          G - Gigabyte (%d)\n", ONE_GB);
    printf("\n");
    printf("      Example: '37M' is 37 megabytes or %d bytes\n", 37 * ONE_MB);
    printf("\n");
    printf("  AL - is an API list. Valid values are:\n");
    printf("          hdf5 - HDF5\n");
    printf("          posix - POSIX\n");
    printf("\n");
    printf("      Example: -A posix,hdf5\n");
    printf("\n");
    printf("  NL - is list of integers (N) separated by commas.\n");
    printf("\n");
    printf("      Example: 1,2,3\n");
    printf("\n");
    printf("  SL - is list of size specifiers (S) separated by commas.\n");
    printf("\n");
    printf("      Example: 2K,2K,3K\n");
    printf("\n");
    printf("      The example defines an object (dataset, transfer buffer) with three\n");
    printf("      dimensions. Be aware that as the number of dimensions increases, the\n");
    printf("      the total size of the object increases exponentially.\n");
    printf("\n");
    printf("  VFD  - is an HDF5 file driver specifier. Valid values are:\n");
    printf("          sec2, stdio, core, split, multi, family, direct\n");
    printf("\n");
    printf("  Dimension access order:\n");
    printf("      Data access starts at the cardinal origin of the dataset using the\n");
    printf("      transfer buffer. The next access occurs on a dataset region next to\n");
    printf("      the previous one. For a multidimensional dataset, there are several\n");
    printf("      directions as to where to proceed. This can be specified in the dimension\n");
    printf("      access order. For example, -r 1,2 states that the tool should traverse\n");
    printf("      dimension 1 first, and then dimension 2.\n");
    printf("\n");
    printf("  Environment variables:\n");
    printf("      HDF5_NOCLEANUP   Do not remove data files if set [default remove]\n");
    printf("      HDF5_PREFIX      Data file prefix\n");
    printf("\n");
    fflush(stdout);
} /* end usage() */
