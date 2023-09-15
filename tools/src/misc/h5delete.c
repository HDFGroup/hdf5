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

/* h5delete tool
 *
 * Deletes storage via H5Fdelete() using the VOL connector specified in the
 * environment variable.
 */

#include "H5private.h"
#include "H5Eprivate.h"
#include "H5Pprivate.h"

static void usage(void);

static void
usage(void)
{
    fprintf(stderr, "usage: h5delete [-f] <filename>\n");
}

int
main(int argc, char *argv[])
{
    bool        quiet = false;
    const char *name  = NULL;
    int         ret   = 0;

    switch (argc) {
        case 3:
            if (strcmp(argv[1], "-f") != 0) {
                usage();
                return EXIT_FAILURE;
            }
            quiet = true;
            name  = argv[2];
            break;
        case 2:
            name = argv[1];
            break;
        default:
            usage();
            return EXIT_FAILURE;
    }

    H5E_BEGIN_TRY
    {
        /* Only uses the environment variable at this time */
        ret = (int)H5Fdelete(name, H5P_DEFAULT);
    }
    H5E_END_TRY

    if (ret < 0 && !quiet)
        fprintf(stderr, "Unable to delete storage at: %s\n", name);

    return ret < 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}
