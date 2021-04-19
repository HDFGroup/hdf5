/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
#include "h5diff.h"
#include "h5diff_common.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: h5diff main program
 *
 * Return: An  exit status of 0 means no differences were found, 1 means some
 *   differences were found.
 *
 * Programmer: Pedro Vicente
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 * Modifications: July 2004
 *  Introduced the four modes:
 *   Normal mode: print the number of differences found and where they occured
 *   Report mode: print the above plus the differences
 *   Verbose mode: print the above plus a list of objects and warnings
 *   Quiet mode: do not print output
 *
 * November 2004: Leon Arber (larber@uiuc.edu)
 *     Additions that allow h5diff to be run in parallel
 *
 * February 2005: Leon Arber (larber@uiuc.edu)
 *   h5diff and ph5diff split into two files, one that is used
 *   to build a serial h5diff and one used to build a parallel h5diff
 *   Common functions have been moved to h5diff_common.c
 *
 * October 2005
 *  Introduced a new field 'not_cmp' to 'diff_opt_t' that detects
 *  if some objects are not comparable and prints the message
 *  "Some objects are not comparable"
 *
 * February 2007
 *  Added comparison for dataset regions.
 *  Added support for reading and comparing by hyperslabs for large files.
 *  Inclusion of a relative error formula to compare floating
 *   point numbers in order to deal with floating point uncertainty.
 *  Printing of dataset dimensions along with dataset name
 *
 *  November 19, 2007
 *    adopted the syntax h5diff  [OPTIONS]  file1 file2  [obj1[obj2]]
 *
 *-------------------------------------------------------------------------
 */

int
main(int argc, const char *argv[])
{
    int         ret;
    int         i;
    const char *fname1   = NULL;
    const char *fname2   = NULL;
    const char *objname1 = NULL;
    const char *objname2 = NULL;
    hsize_t     nfound   = 0;
    diff_opt_t  opts;

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Initialize h5tools lib */
    h5tools_init();

    /*-------------------------------------------------------------------------
     * process the command-line
     *-------------------------------------------------------------------------
     */
    parse_command_line(argc, argv, &fname1, &fname2, &objname1, &objname2, &opts);

    /* enable error reporting if command line option */
    h5tools_error_report();

    /*-------------------------------------------------------------------------
     * do the diff
     *-------------------------------------------------------------------------
     */

    nfound = h5diff(fname1, fname2, objname1, objname2, &opts);

    print_info(&opts);

    /*-------------------------------------------------------------------------
     * exit code
     *   1 if differences, 0 if no differences, 2 if error
     *-------------------------------------------------------------------------
     */

    ret = (nfound == 0 ? 0 : 1);

    /* if graph difference return 1 for differences  */
    if (opts.contents == 0)
        ret = 1;

    /* and return 2 for error */
    if (opts.err_stat)
        ret = 2;

    /* free any buffers */
    for (i = 0; i < 2; i++) {
        if (opts.sset[i]) {
            if (opts.sset[i]->start.data)
                HDfree(opts.sset[i]->start.data);
            if (opts.sset[i]->stride.data)
                HDfree(opts.sset[i]->stride.data);
            if (opts.sset[i]->count.data)
                HDfree(opts.sset[i]->count.data);
            if (opts.sset[i]->block.data)
                HDfree(opts.sset[i]->block.data);

            HDfree(opts.sset[i]);
            opts.sset[i] = NULL;
        }
    }

    h5diff_exit(ret);
}

/*-------------------------------------------------------------------------
 * Function: h5diff_exit
 *
 * Purpose: dismiss phdiff worker processes and exit
 *
 * Return: none
 *
 * Programmer: Albert Cheng
 * Date: Feb 6, 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5_ATTR_NORETURN void
h5diff_exit(int status)
{
    h5tools_close();

    HDexit(status);
}
