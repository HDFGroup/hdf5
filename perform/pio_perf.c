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

#define ONE_GB              1073741824UL

#if 0
#define MIN_HDF5_BUF_SIZE   1024
#define MAX_HDF5_BUF_SIZE   (ONE_GB / 2)
#else
#define MIN_HDF5_BUF_SIZE   1024*1024*8
#define MAX_HDF5_BUF_SIZE   MIN_HDF5_BUF_SIZE*4
#endif

/* local variables */
static const char  *progname = "pio_perf";

/*
 * Command-line options: The user can specify short or long-named
 * parameters. The long-named ones can be partially spelled. When
 * adding more, make sure that they don't clash with each other.
 */
#if 1
static const char *s_opts = "ho:";
#else
static const char *s_opts = "hbo:";
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
    { "max-size", require_arg, 'm' },
    { "max-siz", require_arg, 'm' },
    { "max-si", require_arg, 'm' },
    { "max-s", require_arg, 'm' },
    { "max", require_arg, 'm' },
    { "ma", require_arg, 'm' },
    { "output", require_arg, 'o' },
    { "outpu", require_arg, 'o' },
    { "outp", require_arg, 'o' },
    { "out", require_arg, 'o' },
    { "ou", require_arg, 'o' },
    { NULL, 0, '\0' }
};

struct options {
    const char *output_file;    /* file to print report to              */
    long max_size;              /* maximum size of file in gigabytes    */
};

/* local functions */
static struct options *parse_command_line(int argc, char *argv[]);
static void run_test_loop(FILE *output, int max_num_procs, long max_size);
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

    run_test_loop(output, world_size, opts->max_size);

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
run_test_loop(FILE *output, int max_num_procs, long max_size)
{
    parameters parms;

    /* num_files stays ``1'' for now but may change later */
    parms.num_files = 1;
    parms.num_iters = 1;

    /* divide the maximum number of processors by 2 for each loop iter */
    for (; max_num_procs > 0; max_num_procs /= 2) {
        register iotype i;

        parms.max_num_procs = max_num_procs;
        fprintf(output, "Number of processors = %u\n", parms.max_num_procs);

        for (i = RAW; i <= PHDF5; ++i) {
            register unsigned long j;

            parms.io_type = i;
            print_indent(output, TAB_SPACE * 1);
            fprintf(output, "Type of IO = ");

            if (i == RAW)
                fprintf(output, "Raw\n");
            else if (i == MPIO)
                fprintf(output, "MPIO\n");
            else
                fprintf(output, "PHDF5\n");

            for (j = MIN_HDF5_BUF_SIZE; j <= MAX_HDF5_BUF_SIZE; j <<= 1) {
                results res;

                parms.num_dsets = ONE_GB / j;
                parms.num_elmts = (max_size * j) / sizeof(int);

                print_indent(output, TAB_SPACE * 2);
                fprintf(output,
                        "# of files: %u, # of dsets: %lu, Elements per dset: %lu\n",
                        parms.num_files, parms.num_dsets, parms.num_elmts);

                /* call Albert's testing here */
                res = do_pio(parms); 

                print_indent(output, TAB_SPACE * 3);
                fprintf(output, "Write Results = %f MB/s\n",
                        (parms.num_dsets * parms.num_elmts * sizeof(int)) /
                        get_time(res.timers, HDF5_WRITE_FIXED_DIMS));

                /* get back ``result'' object and report */
                /* (res.ret_code == SUCCESS); */
                /* (res.timers); */
                pio_time_destroy(res.timers);
            }
        }
    }
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
    fprintf(stdout, "     -h, --help           Print a usage message and exit\n");
    fprintf(stdout, "     -m #, --max-size=#   Maximum size of file in gigabytes [default: 2]\n");
    fprintf(stdout, "     -o F, --output=F     Output raw data into file F\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "  F - is a filename.\n");
    fprintf(stdout, "\n");
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

    cl_opts = (struct options *)calloc(1, sizeof(struct options));
    cl_opts->max_size = 2;

    while ((opt = get_option(argc, (const char **)argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
#if 0
        case 'b':
            /* the future "binary" option */
            break;
#endif  /* 0 */
        case 'm':
            cl_opts->max_size = atol(opt_arg);
            break;
        case 'o':
            cl_opts->output_file = opt_arg;
            break;
        case 'h':
            usage(progname);
            exit(EXIT_SUCCESS);
        case '?':
        default:
            usage(progname);
            exit(EXIT_FAILURE);
        }
    }

    return cl_opts;
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
