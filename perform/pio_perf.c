/*
 * Copyright (C) 2001
 *     National Center for Supercomputing Applications
 *     All rights reserved.
 *
 */

/*
 * Parallel HDF5 Performance Testing Code
 * --------------------------------------
 *
 * Portable code to test performance on the different platforms we support.
 * This is what the report should look like:
 *
 *  nprocs = Max#Procs
 *      IO Type = Raw
 *          # Files = 1, # of dsets = 1000, Elements per dset = 37000
 *              Write Results = x MB/s
 *              Read Results = x MB/s
 *          # Files = 1, # of dsets = 3000, Elements per dset = 37000
 *              Write Results = x MB/s
 *              Read Results = x MB/s
 *
 *          . . .
 *
 *      IO Type = MPIO
 *          # Files = 1, # of dsets = 1000, Elements per dset = 37000
 *              Write Results = x MB/s
 *              Read Results = x MB/s
 *          # Files = 1, # of dsets = 3000, Elements per dset = 37000
 *              Write Results = x MB/s
 *              Read Results = x MB/s
 *
 *          . . .
 *
 *      IO Type = PHDF5
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
#include <stdio.h>
#include <stdlib.h>

#include "hdf5.h"

#ifdef H5_HAVE_PARALLEL

/* library header files */
#include <mpi.h>

/* our header files */
#include "h5tools_utils.h"
#include "pio_perf.h"

/* useful macros */
#define TAB_SPACE           4

#define ONE_KB              1024
#define ONE_MB              (ONE_KB * ONE_KB)
#define ONE_GB              (ONE_MB * ONE_KB)

#define PIO_RAW             020
#define PIO_MPI             040
#define PIO_HDF5            060

#define MB_PER_SEC(bytes,t) (((bytes) / ONE_MB) / t)

#define MIN_HDF5_BUF_SIZE   (ONE_MB >> 1)
#define MAX_HDF5_BUF_SIZE   (ONE_GB / 2)

/* local variables */
static const char  *progname = "pio_perf";

/*
 * Command-line options: The user can specify short or long-named
 * parameters. The long-named ones can be partially spelled. When
 * adding more, make sure that they don't clash with each other.
 */
#if 1
static const char *s_opts = "hf:HP:p:X:x:md:F:i:o:r";
#else
static const char *s_opts = "hbf:HP:p:X:x:md:F:i:o:r";
#endif  /* 1 */
static struct long_options l_opts[] = {
    { "help", no_arg, 'h' },
    { "hel", no_arg, 'h' },
    { "he", no_arg, 'h' },
#if 0
    /* a siting of the elusive binary option */
    { "binary", no_arg, 'b' },
    { "binar", no_arg, 'b' },
    { "bina", no_arg, 'b' },
    { "bin", no_arg, 'b' },
    { "bi", no_arg, 'b' },
#endif  /* 0 */
    { "file-size", require_arg, 'f' },
    { "file-siz", require_arg, 'f' },
    { "file-si", require_arg, 'f' },
    { "file-s", require_arg, 'f' },
    { "file", require_arg, 'f' },
    { "fil", require_arg, 'f' },
    { "fi", require_arg, 'f' },
    { "hdf5", no_arg, 'H' },
    { "hdf", no_arg, 'H' },
    { "hd", no_arg, 'H' },
    { "max-num-processes", require_arg, 'P' },
    { "max-num-processe", require_arg, 'P' },
    { "max-num-process", require_arg, 'P' },
    { "max-num-proces", require_arg, 'P' },
    { "max-num-proce", require_arg, 'P' },
    { "max-num-proc", require_arg, 'P' },
    { "max-num-pro", require_arg, 'P' },
    { "max-num-pr", require_arg, 'P' },
    { "max-num-p", require_arg, 'P' },
    { "min-num-processes", require_arg, 'p' },
    { "min-num-processe", require_arg, 'p' },
    { "min-num-process", require_arg, 'p' },
    { "min-num-proces", require_arg, 'p' },
    { "min-num-proce", require_arg, 'p' },
    { "min-num-proc", require_arg, 'p' },
    { "min-num-pro", require_arg, 'p' },
    { "min-num-pr", require_arg, 'p' },
    { "min-num-p", require_arg, 'p' },
    { "max-xfer-size", require_arg, 'X' },
    { "max-xfer-siz", require_arg, 'X' },
    { "max-xfer-si", require_arg, 'X' },
    { "max-xfer-s", require_arg, 'X' },
    { "max-xfer", require_arg, 'X' },
    { "max-xfe", require_arg, 'X' },
    { "max-xf", require_arg, 'X' },
    { "max-x", require_arg, 'X' },
    { "min-xfer-size", require_arg, 'x' },
    { "min-xfer-siz", require_arg, 'x' },
    { "min-xfer-si", require_arg, 'x' },
    { "min-xfer-s", require_arg, 'x' },
    { "min-xfer", require_arg, 'x' },
    { "min-xfe", require_arg, 'x' },
    { "min-xf", require_arg, 'x' },
    { "min-x", require_arg, 'x' },
    { "mpiio", no_arg, 'm' },
    { "mpii", no_arg, 'm' },
    { "mpi", no_arg, 'm' },
    { "mp", no_arg, 'm' },
    { "num-dsets", require_arg, 'd' },
    { "num-dset", require_arg, 'd' },
    { "num-dse", require_arg, 'd' },
    { "num-ds", require_arg, 'd' },
    { "num-d", require_arg, 'd' },
    { "num-files", require_arg, 'F' },
    { "num-file", require_arg, 'F' },
    { "num-fil", require_arg, 'F' },
    { "num-fi", require_arg, 'F' },
    { "num-f", require_arg, 'F' },
    { "num-iterations", require_arg, 'i' },
    { "num-iteration", require_arg, 'i' },
    { "num-iteratio", require_arg, 'i' },
    { "num-iterati", require_arg, 'i' },
    { "num-iterat", require_arg, 'i' },
    { "num-itera", require_arg, 'i' },
    { "num-iter", require_arg, 'i' },
    { "num-ite", require_arg, 'i' },
    { "num-it", require_arg, 'i' },
    { "num-i", require_arg, 'i' },
    { "output", require_arg, 'o' },
    { "outpu", require_arg, 'o' },
    { "outp", require_arg, 'o' },
    { "out", require_arg, 'o' },
    { "ou", require_arg, 'o' },
    { "raw", no_arg, 'r' },
    { "ra", no_arg, 'r' },
    { NULL, 0, '\0' }
};

struct options {
    long io_types;              /* bitmask of which I/O types to test   */
    const char *output_file;    /* file to print report to              */
    long file_size;             /* size of file                         */
    long num_dsets;             /* number of datasets                   */
    long num_files;             /* number of files                      */
    long num_iters;             /* number of iterations                 */
    long max_num_procs;         /* maximum number of processes to use   */
    long min_num_procs;         /* minimum number of processes to use   */
    long max_xfer_size;         /* maximum transfer buffer size         */
    long min_xfer_size;         /* minimum transfer buffer size         */
};

/* local functions */
static long parse_size_directive(const char *size);
static struct options *parse_command_line(int argc, char *argv[]);
static void run_test_loop(FILE *output, struct options *options);
static void run_test(FILE *output, iotype iot, parameters parms);
static void print_indent(register FILE *output, register int indent);
static void usage(const char *prog);

/*
 * Function:    main
 * Purpose:     Start things up. Initialize MPI and then call the test looping
 *              function.
 * Return:      EXIT_SUCCESS or EXIT_FAILURE
 * Programmer:  Bill Wendling, 30. October 2001
 * Modifications:
 */
int
main(int argc, char **argv)
{
    int world_size, ret;
    int exit_value = EXIT_SUCCESS;
    FILE *output = stdout;
    struct options *opts;

    opts = parse_command_line(argc, argv);

    if (opts->output_file) {
        if ((output = fopen(opts->output_file, "w")) == NULL) {
            fprintf(stderr, "%s: cannot open output file\n", progname);
            perror(opts->output_file);
            goto onions;
        }
    }

    /* initialize MPI and get the maximum num of processors we started with */
    MPI_Init(&argc, &argv);
    ret = MPI_Comm_size(MPI_COMM_WORLD, &world_size);

    if (ret != MPI_SUCCESS) {
        fprintf(stderr, "%s: MPI_Comm_size call failed\n", progname);

        if (ret == MPI_ERR_COMM)
            fprintf(stderr, "invalid MPI communicator\n");
        else
            fprintf(stderr, "invalid argument\n");

        exit_value = EXIT_FAILURE;
        goto cheese_and;
    }

    run_test_loop(output, opts);

cheese_and:
    MPI_Finalize();

onions:
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
 *            - The second slowest is what type of IO to perform. We have
 *              three choices: RAW, MPI-IO, and PHDF5.
 *
 *            - Then we change the size of the buffer. This information is
 *              inferred from the number of datasets to create and the number
 *              of integers to put into each dataset. The backend code figures
 *              this out.
 *
 * Return:      Nothing
 * Programmer:  Bill Wendling, 30. October 2001
 * Modifications:
 */
static void
run_test_loop(FILE *output, struct options *opts)
{
    parameters parms;
    long num_procs;
    int io_runs = PIO_HDF5 | PIO_MPI | PIO_RAW; /* default to run all tests */

    if (opts->io_types & ~07) {
        /* we want to run only a select subset of these tests */
        opts->io_types = 0;

        if (opts->io_types | PIO_HDF5)
            io_runs |= PIO_HDF5;

        if (opts->io_types | PIO_MPI)
            io_runs |= PIO_MPI;

        if (opts->io_types | PIO_RAW)
            io_runs |= PIO_RAW;
    }

    parms.num_files = opts->num_files;
    parms.num_dsets = opts->num_dsets;
    parms.num_iters = opts->num_iters;

    /* divide the maximum number of processors by 2 for each loop iter */
    for (num_procs = opts->min_num_procs;
            num_procs <= opts->max_num_procs; num_procs <<= 1) {
        register long j;

        parms.num_procs = num_procs;
        fprintf(output, "Number of processors = %u\n", parms.num_procs);

        for (j = opts->min_xfer_size; j <= opts->max_xfer_size; j <<= 1) {
            parms.buf_size = j;
            parms.num_elmts = opts->file_size / (parms.num_dsets * sizeof(int));

            print_indent(output, TAB_SPACE * 1);
            fprintf(output,
                    "# of files: %u, # of dsets: %lu, Elements per dset: %lu\n",
                    parms.num_files, parms.num_dsets, parms.num_elmts);

            if (io_runs | PIO_RAW)
                run_test(output, RAW, parms);

            if (io_runs | PIO_MPI)
                run_test(output, MPIO, parms);

            if (io_runs | PIO_HDF5)
                run_test(output, PHDF5, parms);
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
static void
run_test(FILE *output, iotype iot, parameters parms)
{
    results res;

    parms.io_type = iot;
    print_indent(output, TAB_SPACE * 2);
    fprintf(output, "Type of IO = ");

    switch (iot) {
    case RAW:
        fprintf(output, "Raw\n");
        break;
    case MPIO:
        fprintf(output, "MPIO\n");
        break;
    case PHDF5:
        fprintf(output, "PHDF5\n");
        break;
    }

    /* call Albert's testing here */
    res = do_pio(parms); 

    print_indent(output, TAB_SPACE * 3);
    fprintf(output, "Write Results = %.2f MB/s\n",
            MB_PER_SEC(parms.num_dsets * parms.num_elmts * sizeof(int),
                       get_time(res.timers, HDF5_WRITE_FIXED_DIMS)));

    print_indent(output, TAB_SPACE * 3);
    fprintf(output, "Read Results = %.2f MB/s\n",
            MB_PER_SEC(parms.num_dsets * parms.num_elmts * sizeof(int),
                       get_time(res.timers, HDF5_READ_FIXED_DIMS)));

    pio_time_destroy(res.timers);
}

/*
 * Function:    print_indent
 * Purpose:     Print spaces to indent a new line of text for pretty printing
 *              things.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 29. October 2001
 * Modifications:
 */
static void
print_indent(register FILE *output, register int indent)
{
    for (; indent > 0; --indent)
        fputc(' ', output);
}

/*
 * Function:    parse_command_line
 * Purpose:     Parse the command line options and return a STRUCT OPTIONS
 *              structure which will need to be freed by the calling function.
 * Return:      Pointer to an OPTIONS structure
 * Programmer:  Bill Wendling, 31. October 2001
 * Modifications:
 */
static struct options *
parse_command_line(int argc, char *argv[])
{
    int opt;
    struct options *cl_opts;

    cl_opts = (struct options *)malloc(sizeof(struct options));

    cl_opts->output_file = NULL;
    cl_opts->file_size = 64 * ONE_MB;
    cl_opts->io_types = 07;     /* bottom bits indicate default type to run */
    cl_opts->num_dsets = 1;
    cl_opts->num_files = 1;
    cl_opts->num_iters = 1;
    cl_opts->max_num_procs = 1;
    cl_opts->min_num_procs = 1;
    cl_opts->max_xfer_size = 1 * ONE_MB;
    cl_opts->min_xfer_size = 1 * ONE_KB;

    while ((opt = get_option(argc, (const char **)argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
#if 0
        case 'b':
            /* the future "binary" option */
            break;
#endif  /* 0 */
        case 'd':
            cl_opts->num_dsets = strtol(opt_arg, NULL, 10);
            break;
        case 'f':
            cl_opts->file_size = parse_size_directive(opt_arg);
            break;
        case 'F':
            cl_opts->num_files = strtol(opt_arg, NULL, 10);
            break;
        case 'H':
            cl_opts->io_types &= ~07;
            cl_opts->io_types |= PIO_HDF5;
            break;
        case 'i':
            cl_opts->num_iters = strtol(opt_arg, NULL, 10);
            break;
        case 'm':
            cl_opts->io_types &= ~07;
            cl_opts->io_types |= PIO_MPI;
            break;
        case 'o':
            cl_opts->output_file = opt_arg;
            break;
        case 'p':
            cl_opts->min_num_procs = strtol(opt_arg, NULL, 10);
            break;
        case 'P':
            cl_opts->max_num_procs = strtol(opt_arg, NULL, 10);
            break;
        case 'r':
            cl_opts->io_types &= ~07;
            cl_opts->io_types |= PIO_RAW;
            break;
        case 'x':
            cl_opts->min_xfer_size = parse_size_directive(opt_arg);
            break;
        case 'X':
            cl_opts->max_xfer_size = parse_size_directive(opt_arg);
            break;
        case 'h':
            usage(progname);
            exit(EXIT_SUCCESS);
        case '?':
        default:
            /* there could be other command line options, such as MPI stuff 
             * that gets passed to our program, for some reason */
            break;
        }
    }

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
 * Return:      The size as a LONG. If an unknown size indicator is used, then
 *              the program will exit with EXIT_FAILURE as the return value.
 * Programmer:  Bill Wendling, 18. December 2001
 * Modifications:
 */
static long
parse_size_directive(const char *size)
{
    long s;
    char *endptr;

    s = strtol(size, &endptr, 10);

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
 * Programmer:  Bill Wendling, 31. October 2001
 * Modifications:
 */
static void
usage(const char *prog)
{
    fflush(stdout);
    fprintf(stdout, "usage: %s [OPTIONS]\n", prog);
    fprintf(stdout, "  OPTIONS\n");
    fprintf(stdout, "     -h, --help                  Print a usage message and exit\n");
    fprintf(stdout, "     -d N, --num-dsets=N         Number of datasets per file [default:1]\n");
    fprintf(stdout, "     -f S, --file-size=S         Size of a single file [default: 64M]\n");
    fprintf(stdout, "     -F N, --num-files=N         Number of files [default: 1]\n");
    fprintf(stdout, "     -H, --hdf5                  Run HDF5 performance test\n");
    fprintf(stdout, "     -i, --num-iterations        Number of iterations to perform [default: 1]\n");
    fprintf(stdout, "     -m, --mpiio                 Run MPI/IO performance test\n");
    fprintf(stdout, "     -o F, --output=F            Output raw data into file F [default: none]\n");
    fprintf(stdout, "     -P N, --max-num-processes=N Maximum number of processes to use [default: 1]\n");
    fprintf(stdout, "     -p N, --min-num-processes=N Minimum number of processes to use [default: 1]\n");
    fprintf(stdout, "     -r, --raw                   Run raw (UNIX) performance test\n");
    fprintf(stdout, "     -X S, --max-xfer-size=S     Maximum transfer buffer size [default: 1M]\n");
    fprintf(stdout, "     -x S, --min-xfer-size=S     Minimum transfer buffer size [default: 1K]\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "  F - is a filename.\n");
    fprintf(stdout, "  N - is an integer >=0.\n");
    fprintf(stdout, "  S - is a size specifier, an integer >=0 followed by a size indicator:\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "          K - Kilobyte\n");
    fprintf(stdout, "          M - Megabyte\n");
    fprintf(stdout, "          G - Gigabyte\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "      Example: 37M = 37 Megabytes\n");
    fprintf(stdout, "\n");
    fflush(stdout);
}

#else /* H5_HAVE_PARALLEL */

/*
 * Function:    main
 * Purpose:     Dummy main() function for if HDF5 was configured without
 *              parallel stuff.
 * Return:      EXIT_SUCCESS
 * Programmer:  Bill Wendling, 14. November 2001
 * Modifications:
 */
int
main(void)
{
    printf("No parallel IO performance because parallel is not configured\n");
    return EXIT_SUCCESS;
}

#endif /* !H5_HAVE_PARALLEL */
