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
 *-------------------------------------------------------------------------
 */

int
main(int argc, char *argv[])
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
    parse_command_line(argc, (const char *const *)argv, &fname1, &fname2, &objname1, &objname2, &opts);

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
                free(opts.sset[i]->start.data);
            if (opts.sset[i]->stride.data)
                free(opts.sset[i]->stride.data);
            if (opts.sset[i]->count.data)
                free(opts.sset[i]->count.data);
            if (opts.sset[i]->block.data)
                free(opts.sset[i]->block.data);

            free(opts.sset[i]);
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
 *-------------------------------------------------------------------------
 */
H5_ATTR_NORETURN void
h5diff_exit(int status)
{
    h5tools_close();

    exit(status);
}
