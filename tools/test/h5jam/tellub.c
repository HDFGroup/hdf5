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
static const char            *s_opts   = "h";
static struct h5_long_options l_opts[] = {{"help", no_arg, 'h'}, {"hel", no_arg, 'h'}, {NULL, 0, '\0'}};

/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Print the usage message
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
usage(const char *prog)
{
    fflush(stdout);
    fprintf(stdout, "usage: %s h5_file\n", prog);
    fprintf(stdout, "           Check that h5_fil is HDF5 file and print size of user block \n");
    fprintf(stdout, "       %s -h\n", prog);
    fprintf(stdout, "           Print a usage message and exit\n");
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
parse_command_line(int argc, const char *const *argv)
{
    int opt;

    /* parse command line options */
    while ((opt = H5_get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
            case 'h':
                usage(h5tools_getprogname());
                h5tools_setstatus(EXIT_SUCCESS);
                break;
            case '?':
            default:
                usage(h5tools_getprogname());
                h5tools_setstatus(EXIT_FAILURE);
        }
    }

    /* check for file name to be processed */
    if (argc <= H5_optind) {
        error_msg("missing file name\n");
        usage(h5tools_getprogname());
        h5tools_setstatus(EXIT_FAILURE);
    }
} /* end parse_command_line() */

static void
leave(int ret)
{
    h5tools_close();
    exit(ret);
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     HDF5 user block tell size
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    char   *ifname;
    hid_t   ifile = H5I_INVALID_HID;
    hsize_t usize;
    htri_t  testval;
    herr_t  status;
    hid_t   plist = H5I_INVALID_HID;

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Initialize h5tools lib */
    h5tools_init();

    parse_command_line(argc, (const char *const *)argv);

    /* enable error reporting if command line option */
    h5tools_error_report();

    if (argc <= (H5_optind)) {
        error_msg("missing file name\n");
        usage(h5tools_getprogname());
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    ifname = strdup(argv[H5_optind]);

    testval = H5Fis_accessible(ifname, H5P_DEFAULT);

    if (testval <= 0) {
        error_msg("Input HDF5 file is not HDF \"%s\"\n", ifname);
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    ifile = H5Fopen(ifname, H5F_ACC_RDONLY, H5P_DEFAULT);

    if (ifile < 0) {
        error_msg("Can't open input HDF5 file \"%s\"\n", ifname);
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    plist = H5Fget_create_plist(ifile);
    if (plist < 0) {
        error_msg("Can't get file creation plist for file \"%s\"\n", ifname);
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    status = H5Pget_userblock(plist, &usize);
    if (status < 0) {
        error_msg("Can't get user block for file \"%s\"\n", ifname);
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    printf("%ld\n", (long)usize);

done:
    H5Pclose(plist);
    if (ifile >= 0)
        H5Fclose(ifile);

    leave(h5tools_getstatus());
} /* end main() */
