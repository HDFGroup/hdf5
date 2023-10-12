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

#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/* Name of tool */
#define PROGRAMNAME "h5mkgrp"

/* Exit status for tools library routines */
int d_status = EXIT_SUCCESS;

/* command-line options: short and long-named parameters */
static const char            *s_opts   = "hlpvV";
static struct h5_long_options l_opts[] = {
    {"help", no_arg, 'h'},          {"latest", no_arg, 'l'},        {"parents", no_arg, 'p'},
    {"verbose", no_arg, 'v'},       {"version", no_arg, 'V'},       {"vol-value", require_arg, '1'},
    {"vol-name", require_arg, '2'}, {"vol-info", require_arg, '3'}, {"vfd-value", require_arg, '4'},
    {"vfd-name", require_arg, '5'}, {"vfd-info", require_arg, '6'}, {NULL, 0, '\0'}};

/* Command line parameter settings */
typedef struct mkgrp_opt_t {
    char  *fname;   /* File name to operate on */
    bool   latest;  /* Whether file should use latest format versions */
    bool   verbose; /* Whether output should be verbose */
    bool   parents; /* Whether to create intermediate groups */
    size_t ngroups; /* Number of groups to create */
    char **groups;  /* Pointer to array of group names */
    hid_t  fapl_id; /* fapl to use when opening the file */
} mkgrp_opt_t;

static mkgrp_opt_t params_g; /* Command line parameter settings */

/*-------------------------------------------------------------------------
 * Function:    leave
 *
 * Purpose:     Shutdown MPI and/or HDF5 and call exit()
 *
 * Return:      Does not return
 *
 *-------------------------------------------------------------------------
 */
static void
leave(int ret)
{
    size_t curr_group;

    if (params_g.fname)
        free(params_g.fname);
    if (params_g.ngroups) {
        for (curr_group = 0; curr_group < params_g.ngroups; curr_group++)
            free(params_g.groups[curr_group]);
        free(params_g.groups);
    }
    if (H5I_INVALID_HID != params_g.fapl_id && H5P_DEFAULT != params_g.fapl_id)
        if (H5Pclose(params_g.fapl_id) < 0)
            error_msg("Could not close file access property list\n");

    h5tools_close();
    exit(ret);
} /* end leave() */

/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: Prints a usage message on stderr and then returns.
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */
static void
usage(const char *prog)
{
    FLUSHSTREAM(rawoutstream);
    PRINTSTREAM(rawoutstream, "usage: %s [OPTIONS] FILE GROUP...\n", prog);
    PRINTVALSTREAM(rawoutstream, "   OPTIONS\n");
    PRINTVALSTREAM(rawoutstream, "      -h, --help         Print a usage message and exit\n");
    PRINTVALSTREAM(rawoutstream,
                   "      -l, --latest       Use latest version of file format to create groups\n");
    PRINTVALSTREAM(rawoutstream,
                   "      -p, --parents      No error if existing, make parent groups as needed\n");
    PRINTVALSTREAM(rawoutstream, "      -v, --verbose      Print information about OBJECTS and OPTIONS\n");
    PRINTVALSTREAM(rawoutstream, "      -V, --version      Print version number and exit\n");
    PRINTVALSTREAM(rawoutstream,
                   "      --vol-value        Value (ID) of the VOL connector to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                         HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "      --vol-name         Name of the VOL connector to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                         HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "      --vol-info         VOL-specific info to pass to the VOL connector used for\n");
    PRINTVALSTREAM(rawoutstream, "                         opening the HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "                         If none of the above options are used to specify a VOL, then\n");
    PRINTVALSTREAM(
        rawoutstream,
        "                         the VOL named by HDF5_VOL_CONNECTOR (or the native VOL connector,\n");
    PRINTVALSTREAM(rawoutstream,
                   "                         if that environment variable is unset) will be used\n");
    PRINTVALSTREAM(rawoutstream,
                   "      --vfd-value        Value (ID) of the VFL driver to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                         HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream, "      --vfd-name         Name of the VFL driver to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                         HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "      --vfd-info         VFD-specific info to pass to the VFL driver used for\n");
    PRINTVALSTREAM(rawoutstream, "                         opening the HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream, "\n");
} /* end usage() */

/*-------------------------------------------------------------------------
 * Function:    parse_command_line
 *
 * Purpose:     Parses command line and sets up global variable to control
 *              output
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 *-------------------------------------------------------------------------
 */
static int
parse_command_line(int argc, const char *const *argv, mkgrp_opt_t *options)
{
    int                opt;        /* Option from command line */
    size_t             curr_group; /* Current group name to copy */
    bool               custom_vol = false;
    bool               custom_vfd = false;
    h5tools_vol_info_t vol_info;
    h5tools_vfd_info_t vfd_info;
    hid_t              tmp_fapl_id = H5I_INVALID_HID;

    /* Check for empty command line */
    if (argc == 1) {
        usage(h5tools_getprogname());
        leave(EXIT_SUCCESS);
    }

    /* Initialize fapl info structs */
    memset(&vol_info, 0, sizeof(h5tools_vol_info_t));
    memset(&vfd_info, 0, sizeof(h5tools_vfd_info_t));

    /* Parse command line options */
    while ((opt = H5_get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
            /* Display 'help' */
            case 'h':
                usage(h5tools_getprogname());
                leave(EXIT_SUCCESS);
                break;

            /* Create objects with the latest version of the format */
            case 'l':
                options->latest = true;
                break;

            /* Create parent groups */
            case 'p':
                options->parents = true;
                break;

            /* Verbose output */
            case 'v':
                options->verbose = true;
                break;

            /* Display version */
            case 'V':
                print_version(h5tools_getprogname());
                leave(EXIT_SUCCESS);
                break;

            case '1':
                vol_info.type    = VOL_BY_VALUE;
                vol_info.u.value = (H5VL_class_value_t)atoi(H5_optarg);
                custom_vol       = true;
                break;

            case '2':
                vol_info.type   = VOL_BY_NAME;
                vol_info.u.name = H5_optarg;
                custom_vol      = true;
                break;

            case '3':
                vol_info.info_string = H5_optarg;
                break;

            case '4':
                vfd_info.type    = VFD_BY_VALUE;
                vfd_info.u.value = (H5FD_class_value_t)atoi(H5_optarg);
                custom_vfd       = true;
                break;

            case '5':
                vfd_info.type   = VFD_BY_NAME;
                vfd_info.u.name = H5_optarg;
                custom_vfd      = true;
                break;

            case '6':
                vfd_info.info = (const void *)H5_optarg;
                break;

            /* Bad command line argument */
            default:
                usage(h5tools_getprogname());
                leave(EXIT_FAILURE);
        } /* end switch */
    }     /* end while */

    /* Check for file name to be processed */
    if (argc <= H5_optind) {
        error_msg("missing file name\n");
        usage(h5tools_getprogname());
        leave(EXIT_FAILURE);
    }

    /* Retrieve file name */
    options->fname = strdup(argv[H5_optind]);
    H5_optind++;

    /* Check for group(s) to be created */
    if (argc <= H5_optind) {
        error_msg("missing group name(s)\n");
        usage(h5tools_getprogname());
        leave(EXIT_FAILURE);
    }

    /* Allocate space for the group name pointers */
    options->ngroups = (size_t)(argc - H5_optind);
    options->groups  = (char **)malloc(options->ngroups * sizeof(char *));

    /* Retrieve the group names */
    curr_group = 0;
    while (H5_optind < argc) {
        options->groups[curr_group] = strdup(argv[H5_optind]);
        curr_group++;
        H5_optind++;
    }

    /* Setup a custom fapl for file accesses */
    if (custom_vol || custom_vfd) {
        if ((tmp_fapl_id = h5tools_get_fapl(options->fapl_id, custom_vol ? &vol_info : NULL,
                                            custom_vfd ? &vfd_info : NULL)) < 0) {
            error_msg("failed to setup file access property list (fapl) for file\n");
            leave(EXIT_FAILURE);
        }

        /* Close the old fapl */
        if (options->fapl_id != H5P_DEFAULT)
            if (H5Pclose(options->fapl_id) < 0) {
                error_msg("failed to close file access property list (fapl)\n");
                leave(EXIT_FAILURE);
            }

        options->fapl_id = tmp_fapl_id;
    }

    return 0;
} /* parse_command_line() */

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: Create group(s) in an HDF5 file
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t  fid     = H5I_INVALID_HID; /* HDF5 file ID */
    hid_t  lcpl_id = H5I_INVALID_HID; /* Link creation property list ID */
    size_t curr_group;                /* Current group to create */

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Initialize h5tools lib */
    h5tools_init();

    /* Initialize the parameters */
    memset(&params_g, 0, sizeof(params_g));

    /* Create file access property list */
    if ((params_g.fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
        error_msg("Could not create file access property list\n");
        leave(EXIT_FAILURE);
    }

    /* Parse command line */
    if (parse_command_line(argc, (const char *const *)argv, &params_g) < 0) {
        error_msg("unable to parse command line arguments\n");
        leave(EXIT_FAILURE);
    }

    /* enable error reporting if command line option */
    h5tools_error_report();

    /* Check for creating groups with new format version */
    if (params_g.latest) {
        /* Set the "use the latest version of the format" bounds */
        if (H5Pset_libver_bounds(params_g.fapl_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) {
            error_msg("Could not set property for using latest version of the format\n");
            leave(EXIT_FAILURE);
        }

        /* Display some output if requested */
        if (params_g.verbose)
            printf("%s: Creating groups with latest version of the format\n", h5tools_getprogname());
    }

    /* Attempt to open an existing HDF5 file first */
    fid = h5tools_fopen(params_g.fname, H5F_ACC_RDWR, params_g.fapl_id, (params_g.fapl_id != H5P_DEFAULT),
                        NULL, 0);

    /* If we couldn't open an existing file, try creating file */
    /* (use "EXCL" instead of "TRUNC", so we don't blow away existing non-HDF5 file) */
    if (fid < 0)
        fid = H5Fcreate(params_g.fname, H5F_ACC_EXCL, H5P_DEFAULT, params_g.fapl_id);

    /* Test for error in opening file */
    if (fid < 0) {
        error_msg("Could not open output file '%s'\n", params_g.fname);
        leave(EXIT_FAILURE);
    }

    /* Create link creation property list */
    if ((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0) {
        error_msg("Could not create link creation property list\n");
        leave(EXIT_FAILURE);
    }

    /* Check for creating intermediate groups */
    if (params_g.parents) {
        /* Set the intermediate group creation property */
        if (H5Pset_create_intermediate_group(lcpl_id, true) < 0) {
            error_msg("Could not set property for creating parent groups\n");
            leave(EXIT_FAILURE);
        }

        /* Display some output if requested */
        if (params_g.verbose)
            printf("%s: Creating parent groups\n", h5tools_getprogname());
    }

    /* Loop over creating requested groups */
    for (curr_group = 0; curr_group < params_g.ngroups; curr_group++) {
        hid_t gid; /* Group ID */

        /* Attempt to create a group */
        if ((gid = H5Gcreate2(fid, params_g.groups[curr_group], lcpl_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            error_msg("Could not create group '%s'\n", params_g.groups[curr_group]);
            leave(EXIT_FAILURE);
        }

        /* Close the group */
        if (H5Gclose(gid) < 0) {
            error_msg("Could not close group '%s'??\n", params_g.groups[curr_group]);
            leave(EXIT_FAILURE);
        }

        /* Display some output if requested */
        if (params_g.verbose)
            printf("%s: created group '%s'\n", h5tools_getprogname(), params_g.groups[curr_group]);
    } /* end for */

    /* Close link creation property list */
    if (H5Pclose(lcpl_id) < 0) {
        error_msg("Could not close link creation property list\n");
        leave(EXIT_FAILURE);
    }

    /* Close file */
    if (H5Fclose(fid) < 0) {
        error_msg("Could not close output file '%s'??\n", params_g.fname);
        leave(EXIT_FAILURE);
    }

    leave(EXIT_SUCCESS);
} /* end main() */
