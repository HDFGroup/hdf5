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

/* This file contains the definition of functions required to build h5perf in
 * STANDALONE mode.
 * Created: Christian Chilan, 2005/5/18.
 */

#include "pio_perf.h"

/** From h5tools_utils.c **/

/* global variables */
int nCols = 80;

int
get_option(int argc, const char **argv, const char *opts, const struct h5_long_options *l_opts)
{
    static int sp      = 1;   /* character index in current token */
    int        opt_opt = '?'; /* option character passed back to user */

    if (sp == 1) {
        /* check for more flag-like tokens */
        if (H5_optind >= argc || argv[H5_optind][0] != '-' || argv[H5_optind][1] == '\0') {
            return EOF;
        }
        else if (HDstrcmp(argv[H5_optind], "--") == 0) {
            H5_optind++;
            return EOF;
        }
    }

    if (sp == 1 && argv[H5_optind][0] == '-' && argv[H5_optind][1] == '-') {
        /* long command line option */
        const char *arg = &argv[H5_optind][2];
        int         i;

        for (i = 0; l_opts && l_opts[i].name; i++) {
            size_t len = HDstrlen(l_opts[i].name);

            if (HDstrncmp(arg, l_opts[i].name, len) == 0) {
                /* we've found a matching long command line flag */
                opt_opt = l_opts[i].shortval;

                if (l_opts[i].has_arg != no_arg) {
                    if (arg[len] == '=') {
                        H5_optarg = &arg[len + 1];
                    }
                    else if (H5_optind < (argc - 1) && argv[H5_optind + 1][0] != '-') {
                        H5_optarg = argv[++H5_optind];
                    }
                    else if (l_opts[i].has_arg == require_arg) {
                        if (H5_opterr)
                            HDfprintf(stderr, "%s: option required for \"--%s\" flag\n", argv[0], arg);

                        opt_opt = '?';
                    }
                }
                else {
                    if (arg[len] == '=') {
                        if (H5_opterr)
                            HDfprintf(stderr, "%s: no option required for \"%s\" flag\n", argv[0], arg);

                        opt_opt = '?';
                    }

                    H5_optarg = NULL;
                }

                break;
            }
        }

        if (l_opts[i].name == NULL) {
            /* exhausted all of the l_opts we have and still didn't match */
            if (H5_opterr)
                HDfprintf(stderr, "%s: unknown option \"%s\"\n", argv[0], arg);

            opt_opt = '?';
        }

        H5_optind++;
        sp = 1;
    }
    else {
        register char *cp; /* pointer into current token */

        /* short command line option */
        opt_opt = argv[H5_optind][sp];

        if (opt_opt == ':' || (cp = strchr(opts, opt_opt)) == 0) {

            if (H5_opterr)
                HDfprintf(stderr, "%s: unknown option \"%c\"\n", argv[0], opt_opt);

            /* if no chars left in this token, move to next token */
            if (argv[H5_optind][++sp] == '\0') {
                H5_optind++;
                sp = 1;
            }

            return '?';
        }

        if (*++cp == ':') {
            /* if a value is expected, get it */
            if (argv[H5_optind][sp + 1] != '\0') {
                /* flag value is rest of current token */
                H5_optarg = &argv[H5_optind++][sp + 1];
            }
            else if (++H5_optind >= argc) {
                if (H5_opterr)
                    HDfprintf(stderr, "%s: value expected for option \"%c\"\n", argv[0], opt_opt);

                opt_opt = '?';
            }
            else {
                /* flag value is next token */
                H5_optarg = argv[H5_optind++];
            }

            sp = 1;
        }
        else {
            /* set up to look at next char in token, next time */
            if (argv[H5_optind][++sp] == '\0') {
                /* no more in current token, so setup next token */
                H5_optind++;
                sp = 1;
            }

            H5_optarg = NULL;
        }
    }

    /* return the current flag character found */
    return opt_opt;
}

void
print_version(const char *progname)
{
    printf("%s: Version %u.%u.%u%s%s\n", progname, H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE,
           H5_VERS_SUBRELEASE[0] ? "-" : "", H5_VERS_SUBRELEASE);
}

