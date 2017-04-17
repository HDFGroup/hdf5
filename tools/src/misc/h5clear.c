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
 * Purpose: A tool to clear the status_flags field from the file's superblock via -s option.
 *          A tool to remove cache image from the file via -m option.
 *          
 */
#include "hdf5.h"
#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/* Name of tool */
#define PROGRAMNAME     "h5clear"

/* Make this private property (defined in H5Fprivate.h) available to h5clear. */
#define H5F_ACS_CLEAR_STATUS_FLAGS_NAME            "clear_status_flags"

static char *fname_g = NULL;
static hbool_t clear_status_flags = FALSE;
static hbool_t remove_cache_image = FALSE;

/*
 * Command-line options: The user can specify short or long-named
 * parameters.
 */
static const char *s_opts = "hVsm";
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
    HDfprintf(stdout, "\n");
    HDfprintf(stdout, "Examples of use:\n");
    HDfprintf(stdout, "\n");
    HDfprintf(stdout, "h5clear -s file_name\n");
    HDfprintf(stdout, "  Clear the status_flags field in the superblock of the HDF5 file <file_name>.\n");
    HDfprintf(stdout, "\n");
    HDfprintf(stdout, "h5clear -m file_name\n");
    HDfprintf(stdout, "  Remove the metadata cache image from the HDF5 file <file_name>.\n");
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
 * Purpose:     To clear the status_flags field in the file's superblock (-s option).
 *              To remove the cache image from the file (-m option).
 *
 * Return:      Success: 0
 *              Failure: 1
 *
 *-------------------------------------------------------------------------
 */
int
main (int argc, const char *argv[])
{
    char *fname = NULL;    /* File name */
    hid_t fapl = -1;       /* File access property list */
    hid_t fid = -1;        /* File ID */
    haddr_t image_addr;
    hsize_t image_len;

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

    if(!clear_status_flags && !remove_cache_image) {
        usage(h5tools_getprogname());
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
        /* This is a private property used by h5clear only */
        if(H5Pset(fapl, H5F_ACS_CLEAR_STATUS_FLAGS_NAME, &clear_status_flags) < 0) {
            error_msg("H5Pset\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
    }

    if((fid = h5tools_fopen(fname, H5F_ACC_RDWR, fapl, NULL, NULL, (size_t)0)) < 0) {
        error_msg("h5tools_fopen\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
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

