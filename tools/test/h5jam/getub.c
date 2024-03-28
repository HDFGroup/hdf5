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

void parse_command_line(int argc, const char *const *argv);

/* Name of tool */
#define PROGRAM_NAME "getub"
static char *nbytes = NULL;

static const char            *s_opts   = "c:";                     /* add more later ? */
static struct h5_long_options l_opts[] = {{"c", require_arg, 'c'}, /* input file */
                                          {NULL, 0, '\0'}};

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
    fprintf(stdout, "usage: %s -c nb file] \n", prog);
    fprintf(stdout, "           print first 'nb' byts of file to stdoug.\n");
}

/*-------------------------------------------------------------------------
 * Function:    parse_command_line
 *
 * Purpose:     Parse the command line for the h5dumper.
 *
 * Return:      Success:
 *
 *              Failure:    Exits program with EXIT_FAILURE value.
 *-------------------------------------------------------------------------
 */
void
parse_command_line(int argc, const char *const *argv)
{
    int opt;

    /* parse command line options */
    while ((opt = H5_get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
            case 'c':
                nbytes = strdup(H5_optarg);
                break;
            case '?':
            default:
                usage(h5tools_getprogname());
                exit(EXIT_FAILURE);
        } /* end switch */
    }     /* end while */

    if (argc <= H5_optind) {
        error_msg("missing file name\n");
        usage(h5tools_getprogname());
        exit(EXIT_FAILURE);
    } /* end if */
} /* end parse_command_line() */

int
main(int argc, char *argv[])
{
    int      fd = H5I_INVALID_HID;
    unsigned size;
    char    *filename = NULL;
    long     res;
    char    *buf = NULL;

    h5tools_setprogname(PROGRAM_NAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Initialize h5tools lib */
    h5tools_init();

    parse_command_line(argc, (const char *const *)argv);

    if (NULL == nbytes) {
        /* missing arg */
        error_msg("missing size\n");
        usage(h5tools_getprogname());
        goto error;
    } /* end if */

    if (argc <= (H5_optind)) {
        error_msg("missing file name\n");
        usage(h5tools_getprogname());
        goto error;
    } /* end if */

    filename = strdup(argv[H5_optind]);

    size = 0;
    if (EOF == (res = sscanf(nbytes, "%u", &size))) {
        /* fail */
        error_msg("missing file name\n");
        usage(h5tools_getprogname());
        goto error;
    } /* end if */

    if ((fd = HDopen(filename, O_RDONLY, 0)) < 0) {
        error_msg("can't open file %s\n", filename);
        goto error;
    } /* end if */

    if (NULL == (buf = (char *)malloc((unsigned)(size + 1)))) {
        error_msg("can't allocate buffer \n");
        goto error;
    } /* end if */

    res = HDread(fd, buf, (unsigned)size);
    if (res < (long)size) {
        error_msg("Bad read \n");
        goto error;
    } /* end if */

    if (HDwrite(1, buf, (unsigned)size) < 0) {
        error_msg("Bad write \n");
        goto error;
    } /* end if */

    /* close things and exit */
    free(filename);
    free(buf);
    HDclose(fd);

    return EXIT_SUCCESS;

error:
    free(filename);
    free(buf);
    if (fd >= 0)
        HDclose(fd);
    return EXIT_FAILURE;
} /* end main() */
