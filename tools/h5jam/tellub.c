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

#include "hdf5.h"
#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/* Name of tool */
#define PROGRAMNAME "tellub"

/*
 * Command-line options: The user can specify short or long-named
 * parameters. The long-named ones can be partially spelled. When
 * adding more, make sure that they don't clash with each other.
 */
static const char *s_opts = "h";
static struct long_options l_opts[] = {
    {"help", no_arg, 'h'},
    {"hel", no_arg, 'h'},
    {NULL, 0, '\0'}
};

/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Print the usage message
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
usage (const char *prog)
{
    HDfflush(stdout);
    HDfprintf(stdout, "usage: %s h5_file\n", prog);
    HDfprintf(stdout,
        "           Check that h5_fil is HDF5 file and print size of user block \n");
    HDfprintf(stdout, "       %s -h\n", prog);
    HDfprintf(stdout, "           Print a usage message and exit\n");
} /* end usage() */

/*-------------------------------------------------------------------------
 * Function:    parse_command_line
 *
 * Purpose:     Parse the command line.
 *
 * Return:      Success:    void
 *              Failure:    Exits program with EXIT_FAILURE value.
 *-------------------------------------------------------------------------
 */

static void
parse_command_line (int argc, const char *argv[])
{
    int opt;

    /* parse command line options */
    while ((opt = get_option (argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char) opt) {
            case 'h':
                usage (h5tools_getprogname());
                HDexit(EXIT_SUCCESS);
            case '?':
            default:
                usage (h5tools_getprogname());
                HDexit(EXIT_FAILURE);
        }
    }

    /* check for file name to be processed */
    if (argc <= opt_ind) {
        error_msg("missing file name\n");
        usage (h5tools_getprogname());
        HDexit(EXIT_FAILURE);
    }
} /* end parse_command_line() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     HDF5 user block tell size
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *-------------------------------------------------------------------------
 */
int
main (int argc, const char *argv[])
{
    char *ifname;
    void *edata;
    H5E_auto2_t func;
    hid_t ifile;
    hsize_t usize;
    htri_t testval;
    herr_t status;
    hid_t plist = H5I_INVALID_HID;

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Initialize h5tools lib */
    h5tools_init();

    /* Disable error reporting */
    H5Eget_auto2(H5E_DEFAULT, &func, &edata);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    parse_command_line(argc, argv);

    if(argc <= (opt_ind)) {
        error_msg("missing file name\n");
        usage (h5tools_getprogname());
        return EXIT_FAILURE;
    }

    ifname = HDstrdup(argv[opt_ind]);

    testval = H5Fis_hdf5 (ifname);

    if(testval <= 0) {
        error_msg("Input HDF5 file is not HDF \"%s\"\n", ifname);
        return EXIT_FAILURE;
    }

    ifile = H5Fopen(ifname, H5F_ACC_RDONLY, H5P_DEFAULT);

    if(ifile < 0) {
        error_msg("Can't open input HDF5 file \"%s\"\n", ifname);
        return EXIT_FAILURE;
    }

    plist = H5Fget_create_plist(ifile);
    if(plist < 0) {
        error_msg("Can't get file creation plist for file \"%s\"\n", ifname);
        return EXIT_FAILURE;
    }

    status = H5Pget_userblock(plist, &usize);
    if(status < 0) {
        error_msg("Can't get user block for file \"%s\"\n", ifname);
        return EXIT_FAILURE;
    }

    HDprintf("%ld\n", (long) usize);

    H5Pclose (plist);
    H5Fclose (ifile);

    return EXIT_SUCCESS;
} /* end main() */

