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
#include <stdarg.h>
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

#define PIO_RAW             0x10
#define PIO_MPI             0x20
#define PIO_HDF5            0x40

#define MB_PER_SEC(bytes,t) (((bytes) / ONE_MB) / t)

#define MIN_HDF5_BUF_SIZE   (ONE_MB >> 1)
#define MAX_HDF5_BUF_SIZE   (ONE_GB / 2)

/* global variables */
MPI_Comm    pio_comm_g;         /* Communicator to run the PIO          */
int         pio_mpi_rank_g;     /* MPI rank of pio_comm_g               */
int	        pio_mpi_nprocs_g;   /* number of processes of pio_comm_g    */


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

typedef struct _minmax {
    double min;
    double max;
    double sum;
    int num;
} minmax;

/* local functions */
static long parse_size_directive(const char *size);
static struct options *parse_command_line(int argc, char *argv[]);
static void run_test_loop(FILE *output, struct options *options);
static int run_test(FILE *output, iotype iot, parameters parms);
static void get_minmax(minmax *mm);
static minmax accumulate_minmax_stuff(minmax *mm, int count);
static int create_comm_world(int num_procs, int *doing_pio);
static int destroy_comm_world(void);
static void output_report(FILE *output, const char *fmt, ...);
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
    struct options *opts = NULL;

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
        goto finish;
    }

    pio_comm_g = MPI_COMM_WORLD;

    opts = parse_command_line(argc, argv);

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

    run_test_loop(output, opts);

finish:
    MPI_Finalize();
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
    int		doing_pio;		/* if this process is doing PIO */
    int io_runs = PIO_HDF5 | PIO_MPI | PIO_RAW; /* default to run all tests */

    if (opts->io_types & ~0x7) {
        /* we want to run only a select subset of these tests */
        io_runs = 0;

        if (opts->io_types & PIO_HDF5)
            io_runs |= PIO_HDF5;

        if (opts->io_types & PIO_MPI)
            io_runs |= PIO_MPI;

        if (opts->io_types & PIO_RAW)
            io_runs |= PIO_RAW;
    }

    parms.num_files = opts->num_files;
    parms.num_dsets = opts->num_dsets;
    parms.num_iters = opts->num_iters;

    /* multiply the maximum number of processors by 2 for each loop iter */
    for (num_procs = opts->min_num_procs;
            num_procs <= opts->max_num_procs; num_procs <<= 1) {
        register long buf_size;

        parms.num_procs = num_procs;

        if (create_comm_world(parms.num_procs, &doing_pio) != SUCCESS) {
            /* do something harsh */
        }

	/* only processes doing PIO will run the tests */
	if (doing_pio){
        output_report(output, "Number of processors = %ld\n", parms.num_procs);

        /* multiply the xfer buffer size by 2 for each loop iteration */
        for (buf_size = opts->min_xfer_size;
                buf_size <= opts->max_xfer_size; buf_size <<= 1) {
            parms.buf_size = buf_size;
            parms.num_elmts = opts->file_size / (parms.num_dsets * sizeof(int));

            print_indent(output, TAB_SPACE * 1);
            output_report(output, "Transfer Buffer Size: %ld\n", buf_size);
            print_indent(output, TAB_SPACE * 1);
            output_report(output,
                          "  # of files: %ld, # of dsets: %ld, # of elmts per dset: %ld\n",
                          parms.num_files, parms.num_dsets, parms.num_elmts);

            if (io_runs & PIO_RAW)
                run_test(output, RAW, parms);

            if (io_runs & PIO_MPI)
                run_test(output, MPIO, parms);

            if (io_runs & PIO_HDF5)
                run_test(output, PHDF5, parms);
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
run_test(FILE *output, iotype iot, parameters parms)
{
    results         res;
    register int    i, ret_value = SUCCESS;
    int             comm_size;
    minmax          total_mm;
    minmax         *write_mm_table;
    minmax         *read_mm_table;
    minmax          write_mm = {0.0, 0.0, 0.0, 0};
    minmax          read_mm = {0.0, 0.0, 0.0, 0};

    parms.io_type = iot;
    print_indent(output, TAB_SPACE * 2);
    output_report(output, "Type of IO = ");

    switch (iot) {
    case RAW:
        output_report(output, "Raw\n");
        break;
    case MPIO:
        output_report(output, "MPIO\n");
        break;
    case PHDF5:
        output_report(output, "PHDF5\n");
        break;
    }

    MPI_Comm_size(pio_comm_g, &comm_size);

    write_mm_table = malloc(parms.num_iters * sizeof(minmax));
    read_mm_table = malloc(parms.num_iters * sizeof(minmax));

    for (i = 0; i < parms.num_iters; ++i) {
        write_mm_table[i].min = 0.0;
        write_mm_table[i].max = 0.0;
        write_mm_table[i].sum = 0.0;
        write_mm_table[i].num = 0;

        read_mm_table[i].min = 0.0;
        read_mm_table[i].max = 0.0;
        read_mm_table[i].sum = 0.0;
        read_mm_table[i].num = 0;
    }

    /* call Albert's testing here */
    for (i = 0; i < parms.num_iters; ++i) {
        register int j;
        double t;

        MPI_Barrier(pio_comm_g);
        res = do_pio(parms);

        t = get_time(res.timers, HDF5_GROSS_WRITE_FIXED_DIMS);
        MPI_Send((void *)&t, 1, MPI_DOUBLE, 0, 0, pio_comm_g);

        for (j = 0; j < comm_size; ++j)
            get_minmax(&write_mm);

        write_mm_table[i] = write_mm;

        t = get_time(res.timers, HDF5_GROSS_READ_FIXED_DIMS);
        MPI_Send((void *)&t, 1, MPI_DOUBLE, 0, 0, pio_comm_g);

        for (j = 0; j < comm_size; ++j)
            get_minmax(&read_mm);

        read_mm_table[i] = read_mm;
    }

    total_mm = accumulate_minmax_stuff(write_mm_table, parms.num_iters);

printf("write metrics: min: %f, max: %f, avg: %f\n", total_mm.min,
       total_mm.max, total_mm.sum / total_mm.num);

    total_mm = accumulate_minmax_stuff(read_mm_table, parms.num_iters);

printf("read metrics: min: %f, max: %f, avg: %f\n", total_mm.min,
       total_mm.max, total_mm.sum / total_mm.num);

    print_indent(output, TAB_SPACE * 3);
    output_report(output, "Write Results = %.2f MB/s\n",
                  MB_PER_SEC(parms.num_dsets * parms.num_elmts * sizeof(int),
                             get_time(res.timers, HDF5_GROSS_WRITE_FIXED_DIMS)));

    print_indent(output, TAB_SPACE * 3);
    output_report(output, "Read Results = %.2f MB/s\n",
                  MB_PER_SEC(parms.num_dsets * parms.num_elmts * sizeof(int),
                             get_time(res.timers, HDF5_GROSS_READ_FIXED_DIMS)));

    free(write_mm_table);
    free(read_mm_table);
    pio_time_destroy(res.timers);
    return ret_value;
}

static void
get_minmax(minmax *mm)
{
    int myrank;

    MPI_Comm_rank(pio_comm_g, &myrank);

    if (myrank == 0) {
        MPI_Status status = {0};
        double t;

        MPI_Recv((void *)&t, 1, MPI_DOUBLE, MPI_ANY_SOURCE,
                 MPI_ANY_TAG, pio_comm_g, &status);

        ++mm->num;
        mm->sum += t;

        if (t > mm->max)
            mm->max = t;

        if (t < mm->min || mm->min <= 0.0)
            mm->min = t;
    }
}

static minmax
accumulate_minmax_stuff(minmax *mm, int count)
{
    register int i;
    minmax total_mm = mm[0];

    for (i = 1; i < count; ++i) {
        total_mm.sum += mm[i].sum;
        total_mm.num += mm[i].num;

        if (mm[i].min < total_mm.min)
            total_mm.min = mm[i].min;

        if (mm[i].max > total_mm.max)
            total_mm.max = mm[i].max;
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
    int     mrc, ret_value;     /* return values                */
    int     color;              /* for communicator creation    */
    int     myrank, nprocs;

    pio_comm_g = MPI_COMM_NULL;

    /*
     * Create a sub communicator for this PIO run. Easier to use the first N
     * processes.
     */
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

    if (num_procs > nprocs) {
        fprintf(stderr,
                "number of process(%d) must be <= number of processes in MPI_COMM_WORLD(%d)\n",
                num_procs, nprocs);
        goto error_done;
    }

    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
    color = (myrank < num_procs);
    mrc = MPI_Comm_split(MPI_COMM_WORLD, color, myrank, &pio_comm_g);

    if (mrc != MPI_SUCCESS) {
        fprintf(stderr, "MPI_Comm_split failed\n");
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
    return ret_value;

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
    int     mrc = SUCCESS;      /* return code      */

    /* release MPI resources */
    if (pio_comm_g != MPI_COMM_NULL)
        mrc = (MPI_Comm_free(&pio_comm_g) == MPI_SUCCESS ? SUCCESS : FAIL);

    return mrc;
}

/*
 * Function:    output_report
 * Purpose:     Print a line of the report. Only do so if I'm the 0 process.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 19. December 2001
 * Modifications:
 */
static void
output_report(FILE *output, const char *fmt, ...)
{
    int myrank;

    MPI_Comm_rank(pio_comm_g, &myrank);

    if (myrank == 0) {
        va_list ap;

        va_start(ap, fmt);
        vfprintf(output, fmt, ap);
        va_end(ap);
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
    register int opt;
    struct options *cl_opts;

    cl_opts = (struct options *)malloc(sizeof(struct options));

    cl_opts->output_file = NULL;
    cl_opts->file_size = 64 * ONE_MB;
    cl_opts->io_types = 0x7;    /* bottom bits indicate default type to run */
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
            cl_opts->io_types &= ~0x7;
            cl_opts->io_types |= PIO_HDF5;
            break;
        case 'i':
            cl_opts->num_iters = strtol(opt_arg, NULL, 10);
            break;
        case 'm':
            cl_opts->io_types &= ~0x7;
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
            cl_opts->io_types &= ~0x7;
            cl_opts->io_types |= PIO_RAW;
            break;
        case 'x':
            cl_opts->min_xfer_size = parse_size_directive(opt_arg);
            break;
        case 'X':
            cl_opts->max_xfer_size = parse_size_directive(opt_arg);
            break;
        case 'h':
        case '?':
        default:
            usage(progname);
            free(cl_opts);
            return NULL;
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
    int myrank;

    MPI_Comm_rank(pio_comm_g, &myrank);

    if (myrank == 0) {
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
