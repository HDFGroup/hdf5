/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
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
#define PROGRAMNAME     "h5clear"

/* Make these private properties (defined in H5Fprivate.h) available to h5clear. */
#define H5F_ACS_CLEAR_STATUS_FLAGS_NAME         "clear_status_flags"
#define H5F_ACS_NULL_FSM_ADDR_NAME              "null_fsm_addr"
#define H5F_ACS_SKIP_EOF_CHECK_NAME             "skip_eof_check"

/* Default increment is 1 megabytes for the --increment option */
#define DEFAULT_INCREMENT   1024*1024

static char *fname_g = NULL;
static hbool_t clear_status_flags = FALSE;
static hbool_t remove_cache_image = FALSE;
static hbool_t print_filesize = FALSE;
static hbool_t increment_eoa_eof = FALSE;
static hsize_t increment = DEFAULT_INCREMENT;

/*
 * Command-line options: only publicize long options
 */
static const char *s_opts = "hVsmzi*";
static struct long_options l_opts[] = {
        { "help", no_arg, 'h' },
        { "hel", no_arg, 'h'},
        { "he", no_arg, 'h'},
        { "version", no_arg, 'V' },
        { "version", no_arg, 'V' },
        { "versio", no_arg, 'V' },
        { "versi", no_arg, 'V' },
        { "vers", no_arg, 'V' },
        { "status", no_arg, 's' },
        { "statu", no_arg, 's' },
        { "stat", no_arg, 's' },
        { "sta", no_arg, 's' },
        { "st", no_arg, 's' },
        { "image", no_arg, 'm' },
        { "imag", no_arg, 'm' },
        { "ima", no_arg, 'm' },
        { "im", no_arg, 'm' },
        { "filesize", no_arg, 'z' },
        { "filesiz", no_arg, 'z' },
        { "filesi", no_arg, 'z' },
        { "files", no_arg, 'z' },
        { "file", no_arg, 'z' },
        { "fil", no_arg, 'z' },
        { "fi", no_arg, 'z' },
        { "increment", optional_arg, 'i' },
        { "incremen", optional_arg, 'i' },
        { "increme", optional_arg, 'i' },
        { "increm", optional_arg, 'i' },
        { "incre", optional_arg, 'i' },
        { "incr", optional_arg, 'i' },
        { "inc", optional_arg, 'i' },
        { "in", optional_arg, 'i' },
        { NULL, 0, '\0' }
};



/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Prints a usage message
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void usage(const char *prog)
{
    HDfprintf(stdout, "usage: %s [OPTIONS] file_name\n", prog);
    HDfprintf(stdout, "  OPTIONS\n");
    HDfprintf(stdout, "   -h, --help                Print a usage message and exit\n");
    HDfprintf(stdout, "   -V, --version             Print version number and exit\n");
    HDfprintf(stdout, "   -s, --status              Clear the status_flags field in the file's superblock\n");
    HDfprintf(stdout, "   -m, --image               Remove the metadata cache image from the file\n");
    HDfprintf(stdout, "   --filesize                Print the file's EOA and EOF\n");
    HDfprintf(stdout, "   --increment=C             Set the file's EOA to the maximum of (EOA, EOF) + C for the file <file_name>\n");
    HDfprintf(stdout, "                             C is >= 0; C is optional and will default to 1M when not set");
    HDfprintf(stdout, "\n");
    HDfprintf(stdout, "Examples of use:\n");
    HDfprintf(stdout, "\n");
    HDfprintf(stdout, "h5clear -s file_name\n");
    HDfprintf(stdout, "  Clear the status_flags field in the superblock of the HDF5 file <file_name>.\n");
    HDfprintf(stdout, "\n");
    HDfprintf(stdout, "h5clear -m file_name\n");
    HDfprintf(stdout, "  Remove the metadata cache image from the HDF5 file <file_name>.\n");
    HDfprintf(stdout, "\n");
    HDfprintf(stdout, "h5clear --increment file_name\n");
    HDfprintf(stdout, "  Set the EOA to the maximum of (EOA, EOF) + 1M for the file <file_name>.\n");
    HDfprintf(stdout, "\n");
    HDfprintf(stdout, "h5clear --increment=512 file_name\n");
    HDfprintf(stdout, "  Set the EOA to the maximum of (EOA, EOF) + 512 for the file <file_name>.\n");
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
parse_command_line(int argc, const char **argv)
{
    int opt;

     /* no arguments */
    if (argc == 1) {
        usage(h5tools_getprogname());
        h5tools_setstatus(EXIT_FAILURE);
        goto error;
    }

    /* parse command line options */
    while((opt = get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch((char)opt) {
            case 'h':
                usage(h5tools_getprogname());
                h5tools_setstatus(EXIT_SUCCESS);
                goto done;

            case 'V':
                print_version(h5tools_getprogname());
                h5tools_setstatus(EXIT_SUCCESS);
                goto done;

            case 's':
                clear_status_flags = TRUE;
                break;

            case 'm':
                remove_cache_image = TRUE;
                break;

            case 'z':
                print_filesize = TRUE;
                break;

            case 'i':
                increment_eoa_eof = TRUE;
                if(opt_arg != NULL) {
                    if (HDatoi(opt_arg) < 0) {
                        usage(h5tools_getprogname());
                        goto done;
                    }
                    increment = HDatoi(opt_arg);
                }
                break;

            default:
                usage(h5tools_getprogname());
                h5tools_setstatus(EXIT_FAILURE);
                goto error;
        } /* end switch */
    } /* end while */

    /* check for file name to be processed */
    if(argc <= opt_ind) {
        error_msg("missing file name\n");
        usage(h5tools_getprogname());
        h5tools_setstatus(EXIT_FAILURE);
        goto error;
    } /* end if */

    fname_g = HDstrdup(argv[opt_ind]);

done:
    return(0);

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
    HDexit(ret);
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
main (int argc, const char *argv[])
{
    char *fname = NULL;             /* File name */
    hid_t fapl = -1;                /* File access property list */
    hid_t fid = -1;                 /* File ID */
    haddr_t image_addr;
    hsize_t image_len;
    unsigned flags = H5F_ACC_RDWR;    /* file access flags */

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Disable the HDF5 library's error reporting */
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    /* initialize h5tools lib */
    h5tools_init();

    /* Parse command line options */
    if(parse_command_line(argc, argv) < 0)
        goto done;

    if(fname_g == NULL)
        goto done;

    /* Print usage/exit if not using at least one of the options */
    if(!clear_status_flags && !remove_cache_image &&
       !increment_eoa_eof && !print_filesize) {
        usage(h5tools_getprogname());
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* Cannot combine the --filesize option with other options */
    if(print_filesize &&
       (clear_status_flags || remove_cache_image || increment_eoa_eof)) {
        error_msg("Cannot combine --filesize with other options\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* Duplicate the file name */
    fname = HDstrdup(fname_g);

    /* Get a copy of the file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
        error_msg("H5Pcreate\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
     }

    /* -s option */
    if(clear_status_flags) {
        /* Set to clear the status_flags in the file's superblock */
        /* Activate this private property */
        if(H5Pset(fapl, H5F_ACS_CLEAR_STATUS_FLAGS_NAME, &clear_status_flags) < 0) {
            error_msg("H5Pset\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
    }

    /* --increment option */
    if(increment_eoa_eof) {
        /* Activate this private property */
        if(H5Pset(fapl, H5F_ACS_SKIP_EOF_CHECK_NAME, &increment_eoa_eof) < 0) {
            error_msg("H5Pset\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        /* Activate this private property */
        if(H5Pset(fapl, H5F_ACS_NULL_FSM_ADDR_NAME, &increment_eoa_eof) < 0) {
            error_msg("H5Pset\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
    }

    /* --filesize option; open the file read-only */
    if(print_filesize) {
        /* Activate this private property */
        if(H5Pset(fapl, H5F_ACS_SKIP_EOF_CHECK_NAME, &print_filesize) < 0) {
            error_msg("H5Pset\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        flags = H5F_ACC_RDONLY;
    }

    /* Open the file */
    if((fid = h5tools_fopen(fname, flags, fapl, NULL, NULL, (size_t)0)) < 0) {
        error_msg("h5tools_fopen\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* --filesize option */
    if(print_filesize) {
        h5_stat_t st;   /* Stat info call */
        haddr_t eoa;    /* The EOA value */

        /* Get the file's EOA and EOF */
        if(H5Fget_eoa(fid, &eoa) < 0 || HDstat(fname, &st) < 0) {
            error_msg("H5Fget_eoa or HDstat\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        HDfprintf(stdout, "EOA is %a; EOF is %a \n", eoa, st.st_size);
    }

    /* --increment option */
    if(increment_eoa_eof) {
        /* Set the file's EOA to the maximum of (EOA, EOF) + increment */
        if(H5Fincrement_filesize(fid, increment) < 0) {
            error_msg("H5Fset_eoa\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
    }

    /* -m option */
    if(remove_cache_image) {
        if(H5Fget_mdc_image_info(fid, &image_addr, &image_len) < 0) {
            error_msg("H5Fget_mdc_image_info\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        if(image_addr == HADDR_UNDEF && image_len == 0)
            warn_msg("No cache image in the file\n");
    }


    h5tools_setstatus(EXIT_SUCCESS);

done:
    if(fname)
        HDfree(fname);
    if(fname_g)
        HDfree(fname_g);

    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Fclose(fid);
    } H5E_END_TRY

    leave(h5tools_getstatus());
} /* main() */

