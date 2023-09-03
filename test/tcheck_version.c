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
 * This tests the h5check_version() function.
 *
 * The default is to call the h5check_version() with the version information
 * in the header file and should incur no warnings or abort.
 * Options provided to call it with incorrect versions to test
 * if it will indeed issue the warning message and aborts.  With environment
 * variable $HDF5_DISABLE_VERSION_CHECK sets to 1, it should issue warnings
 * but no abort.  If it is 2, no warning or abort.
 */

#include "h5test.h"

#ifdef H5_HAVE_WIN32_API
#include <crtdbg.h>
#endif

#define progname "tcheck_version"

/* prototypes */
void showhelp(void);
void parse(int ac, char **av);
void abort_intercept(int H5_ATTR_UNUSED sig);

/* global variables */
static unsigned major   = H5_VERS_MAJOR;
static unsigned minor   = H5_VERS_MINOR;
static unsigned release = H5_VERS_RELEASE;

void
showhelp(void)
{
    printf("Usage: " progname " [-h] [-t<vers>]\n");
    printf("\t-h\tShow this page and version information\n");
    printf("\t-t<vers>: Test by changing (adding 1 to) the <vers> to trigger\n");
    printf("\t\t  the warning. <vers> can be:\n");
    printf("\t\t\tM for Major version number (%d)\n", H5_VERS_MAJOR);
    printf("\t\t\tm for Minor version number (%d)\n", H5_VERS_MINOR);
    printf("\t\t\tr for Release number (%d)\n", H5_VERS_RELEASE);
}

void
parse(int ac, char **av)
{
    char *pt;

    while (--ac > 0) {
        pt = *(++av);
        if (*pt != '-') {
            fprintf(stderr, "Unknown option(%s). Aborted.\n", *av);
            exit(EXIT_FAILURE);
        }
        else {
            switch (*(++pt)) {
                case 't': /* option -t */
                    switch (*(++pt)) {
                        case 'M':
                            major++;
                            break;
                        case 'm':
                            minor++;
                            break;
                        case 'r':
                            release++;
                            break;
                        default:
                            fprintf(stderr, "Unknown -v parameter (%s). Aborted.\n", *av);
                            exit(EXIT_FAILURE);
                    }
                    break;
                case 'h': /* help page */
                    showhelp();
                    exit(EXIT_SUCCESS);
                default:
                    fprintf(stderr, "Unknown option(%s). Aborted.\n", *av);
                    exit(EXIT_FAILURE);
            }
        }
    }
}

/* Handler for SIGABRT - catch the abort signal supposedly from check_version()
 * and exit(6).  Would have used 134 is the return code in Unix systems
 * but some systems (e.g., poe in AIX interprets exit(134) the same as
 * if the process has really been interrupted by the abort signal and prints
 * extra messages that confuse test script that is looking for matching output.
 * This handles the abort signal instead letting it interrupt the OS because
 * some systems may produce extra messages and/or produce core dump.
 * This tries to eliminate those side effects.
 */
H5_ATTR_NORETURN void
abort_intercept(int H5_ATTR_UNUSED sig)
{
    exit(6);
}

#ifdef H5_HAVE_WIN32_API
/* Turns off the modal dialog that is raised when the Windows CRT calls abort().
 *
 * Returning true here lets Windows know that we've handled the abort() and that there
 * is no need to alert the user with a modal dialog box.
 */
int
handle_crt_abort(int reportType, char *message, int *returnValue)
{
    return true;
}
#endif

int
main(int ac, char **av)
{
#ifdef H5_HAVE_WIN32_API
    (void)_CrtSetReportHook2(_CRT_RPTHOOK_INSTALL, handle_crt_abort);
#endif
    parse(ac, av);
    HDsignal(SIGABRT, &abort_intercept);
    H5check_version(major, minor, release);
    HDsignal(SIGABRT, SIG_DFL);
#ifdef H5_HAVE_WIN32_API
    (void)_CrtSetReportHook2(_CRT_RPTHOOK_REMOVE, handle_crt_abort);
#endif
    return 0;
}
