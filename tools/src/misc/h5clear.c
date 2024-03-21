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
 * Purpose: A tool used to do the following:
 *      (1) -s, --status:   clear the status_flags field from the file's superblock
 *      (2) -m, --image:    remove the metadata cache image from the file
 *      (3) --increment=C:  set the file's EOA to the maximum of (EOA, EOF) + C
 *      (4) --filesize:     print the file's EOA and EOF
 */
#include "hdf5.h"
#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/* Name of tool */
#define PROGRAMNAME "h5clear"

/* Make these private properties (defined in H5Fprivate.h) available to h5clear. */
#define H5F_ACS_CLEAR_STATUS_FLAGS_NAME "clear_status_flags"
#define H5F_ACS_NULL_FSM_ADDR_NAME      "null_fsm_addr"
#define H5F_ACS_SKIP_EOF_CHECK_NAME     "skip_eof_check"

/* Default increment is 1 megabytes for the --increment option */
#define DEFAULT_INCREMENT (1024 * 1024)

static char   *fname_g            = NULL;
static bool    clear_status_flags = false;
static bool    remove_cache_image = false;
static bool    print_filesize     = false;
static bool    increment_eoa_eof  = false;
static hsize_t increment          = DEFAULT_INCREMENT;

/*
 * Command-line options: only publicize long options
 */
static const char            *s_opts   = "hVsmzi*";
static struct h5_long_options l_opts[] = {
    {"help", no_arg, 'h'},  {"version", no_arg, 'V'},  {"status", no_arg, 's'},
    {"image", no_arg, 'm'}, {"filesize", no_arg, 'z'}, {"increment", optional_arg, 'i'},
    {NULL, 0, '\0'}};

/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Prints a usage message
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
usage(const char *prog)
{
    fprintf(stdout, "h5clear clears superblock status flag field, removes metadata cache image, prints\n");
    fprintf(stdout, "EOA and EOF, or sets EOA of a file.  It is not a general repair tool and should not\n");
    fprintf(stdout, "be used to fix file corruption.  If a process doesn't shut down cleanly, the\n");
    fprintf(stdout, "superblock mark can be left that prevents opening a file without SWMR.  Then,\n");
    fprintf(stdout, "h5clear can be used to remove this superblock mark so that the file can be inspected\n");
    fprintf(stdout, "and appropriate actions can be taken.\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "usage: %s [OPTIONS] file_name\n", prog);
    fprintf(stdout, "  OPTIONS\n");
    fprintf(stdout, "   -h, --help                Print a usage message and exit\n");
    fprintf(stdout, "   -V, --version             Print version number and exit\n");
    fprintf(stdout, "   -s, --status              Clear the status_flags field in the file's superblock\n");
    fprintf(stdout, "   -m, --image               Remove the metadata cache image from the file\n");
    fprintf(stdout, "   --filesize                Print the file's EOA and EOF\n");
    fprintf(stdout, "   --increment=C             Set the file's EOA to the maximum of (EOA, EOF) + C for\n");
    fprintf(stdout, "                             the file <file_name>.\n");
    fprintf(stdout,
            "                             C is >= 0; C is optional and will default to 1M when not set.\n");
    fprintf(stdout,
            "                             This option helps to repair a crashed SWMR file when the stored\n");
    fprintf(stdout, "                             EOA in the superblock is different from the actual EOF.\n");
    fprintf(stdout, "                             The file's EOA and EOF will be the same after applying\n");
    fprintf(stdout, "                             this option to the file.\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "Examples of use:\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "h5clear -s file_name\n");
    fprintf(stdout, "  Clear the status_flags field in the superblock of the HDF5 file <file_name>.\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "h5clear -m file_name\n");
    fprintf(stdout, "  Remove the metadata cache image from the HDF5 file <file_name>.\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "h5clear --increment file_name\n");
    fprintf(stdout, "  Set the EOA to the maximum of (EOA, EOF) + 1M for the file <file_name>.\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "h5clear --increment=512 file_name\n");
    fprintf(stdout, "  Set the EOA to the maximum of (EOA, EOF) + 512 for the file <file_name>.\n");
} /* usage() */

/*-------------------------------------------------------------------------
 * Function: parse_command_line
 *
 * Purpose: Parses command line and sets up global variable to control output
 *
 * Return:  Success: 0
 *
 *          Failure: -1
 *
 *-------------------------------------------------------------------------
 */
static int
parse_command_line(int argc, const char *const *argv)
{
    int opt;

    /* no arguments */
    if (argc == 1) {
        usage(h5tools_getprogname());
        h5tools_setstatus(EXIT_FAILURE);
        goto error;
    }

    /* parse command line options */
    while ((opt = H5_get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
            case 'h':
                usage(h5tools_getprogname());
                h5tools_setstatus(EXIT_SUCCESS);
                goto done;

            case 'V':
                print_version(h5tools_getprogname());
                h5tools_setstatus(EXIT_SUCCESS);
                goto done;

            case 's':
                clear_status_flags = true;
                break;

            case 'm':
                remove_cache_image = true;
                break;

            case 'z':
                print_filesize = true;
                break;

            case 'i':
                increment_eoa_eof = true;
                if (H5_optarg != NULL) {
                    if (atoi(H5_optarg) < 0) {
                        usage(h5tools_getprogname());
                        goto done;
                    }
                    increment = (hsize_t)atoi(H5_optarg);
                }
                break;

            default:
                usage(h5tools_getprogname());
                h5tools_setstatus(EXIT_FAILURE);
                goto error;
        } /* end switch */
    }     /* end while */

    /* check for file name to be processed */
    if (argc <= H5_optind) {
        error_msg("missing file name\n");
        usage(h5tools_getprogname());
        h5tools_setstatus(EXIT_FAILURE);
        goto error;
    } /* end if */

    fname_g = strdup(argv[H5_optind]);

done:
    return (0);

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    leave
 *
 * Purpose:     Close the tools library and exit
 *
 * Return:      Does not return
 *
 *-------------------------------------------------------------------------
 */
static void
leave(int ret)
{
    h5tools_close();
    exit(ret);
} /* leave() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose: The options are:
 *          (1) -s, --status:   clear the status_flags field from the file's superblock
 *          (2) -m, --image:    remove the metadata cache image from the file
 *          (3) --increment=C:  set the file's EOA to the maximum of (EOA, EOF) + C
 *          (4) --filesize:     print the file's EOA and EOF
 *
 *          The three options: -s, -m, and --increment will modify the file
 *          so the file is opened with write access.
 *          The --filesize option just prints the EOA and EOF, so the file
 *          is opened with read access.
 *
 *          The -s option will activate the private property:
 *              --H5F_ACS_CLEAR_STATUS_FLAGS_NAME
 *          The --increment option will active these two private properties:
 *              --H5F_ACS_NULL_FSM_ADDR_NAME
 *              --H5F_ACS_SKIP_EOF_CHECK_NAME
 *          The --filesize will activate the private property:
 *              --H5F_ACS_SKIP_EOF_CHECK_NAME
 *
 * Return:      Success: 0
 *              Failure: 1
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    char    *fname = NULL;            /* File name */
    hid_t    fapl  = H5I_INVALID_HID; /* File access property list */
    hid_t    fid   = H5I_INVALID_HID; /* File ID */
    haddr_t  image_addr;
    hsize_t  image_len;
    unsigned flags = H5F_ACC_RDWR; /* file access flags */

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* initialize h5tools lib */
    h5tools_init();

    /* Parse command line options */
    if (parse_command_line(argc, (const char *const *)argv) < 0)
        goto done;

    if (fname_g == NULL)
        goto done;

    /* enable error reporting if command line option */
    h5tools_error_report();

    /* Print usage/exit if not using at least one of the options */
    if (!clear_status_flags && !remove_cache_image && !increment_eoa_eof && !print_filesize) {
        usage(h5tools_getprogname());
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* Cannot combine the --filesize option with other options */
    if (print_filesize && (clear_status_flags || remove_cache_image || increment_eoa_eof)) {
        error_msg("Cannot combine --filesize with other options\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* Duplicate the file name */
    fname = strdup(fname_g);

    /* Get a copy of the file access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
        error_msg("H5Pcreate\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* -s option */
    if (clear_status_flags) {
        /* Set to clear the status_flags in the file's superblock */
        /* Activate this private property */
        if (H5Pset(fapl, H5F_ACS_CLEAR_STATUS_FLAGS_NAME, &clear_status_flags) < 0) {
            error_msg("H5Pset\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
    }

    /* --increment option */
    if (increment_eoa_eof) {
        /* Activate this private property */
        if (H5Pset(fapl, H5F_ACS_SKIP_EOF_CHECK_NAME, &increment_eoa_eof) < 0) {
            error_msg("H5Pset\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        /* Activate this private property */
        if (H5Pset(fapl, H5F_ACS_NULL_FSM_ADDR_NAME, &increment_eoa_eof) < 0) {
            error_msg("H5Pset\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
    }

    /* --filesize option; open the file read-only */
    if (print_filesize) {
        /* Activate this private property */
        if (H5Pset(fapl, H5F_ACS_SKIP_EOF_CHECK_NAME, &print_filesize) < 0) {
            error_msg("H5Pset\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        flags = H5F_ACC_RDONLY;
    }

    /* Open the file */
    if ((fid = h5tools_fopen(fname, flags, fapl, false, NULL, (size_t)0)) < 0) {
        error_msg("h5tools_fopen\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* --filesize option */
    if (print_filesize) {
        h5_stat_t st;  /* Stat info call */
        haddr_t   eoa; /* The EOA value */

        /* Get the file's EOA and EOF */
        memset(&st, 0, sizeof(h5_stat_t));
        if (H5Fget_eoa(fid, &eoa) < 0 || HDstat(fname, &st) < 0) {
            error_msg("H5Fget_eoa or HDstat\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        fprintf(stdout, "EOA is %" PRIuHADDR "; EOF is %" PRIuHADDR " \n", eoa, (haddr_t)st.st_size);
    }

    /* --increment option */
    if (increment_eoa_eof) {
        /* Set the file's EOA to the maximum of (EOA, EOF) + increment */
        if (H5Fincrement_filesize(fid, increment) < 0) {
            error_msg("H5Fset_eoa\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
    }

    /* -m option */
    if (remove_cache_image) {
        if (H5Fget_mdc_image_info(fid, &image_addr, &image_len) < 0) {
            error_msg("H5Fget_mdc_image_info\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        if (image_addr == HADDR_UNDEF && image_len == 0)
            warn_msg("No cache image in the file\n");
    }

    h5tools_setstatus(EXIT_SUCCESS);

done:
    if (fname)
        free(fname);
    if (fname_g)
        free(fname_g);

    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    leave(h5tools_getstatus());
} /* main() */
