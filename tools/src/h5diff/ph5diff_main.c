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
#include "ph5diff.h"
#include "h5diff_common.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/* Name of tool */
#if defined(PROGRAMNAME)
#undef PROGRAMNAME
#endif

#define PROGRAMNAME "ph5diff"

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: ph5diff main program
 *
 * Return: An exit status of 0 means no differences were found, 1 means some
 *   differences were found.
 *
 * Programmer: Pedro Vicente
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 * This function drives the diff process and will do a serial or parallel diff depending
 * on the value of the global variable g_Parallel (default is 0), set to 1 when the program
 * is run as "ph5diff"
 *-------------------------------------------------------------------------
 */

int
main(int argc, char *argv[])
{
    const char *fname1   = NULL;
    const char *fname2   = NULL;
    const char *objname1 = NULL;
    const char *objname2 = NULL;
    diff_opt_t  opts;

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Initialize h5tools lib */
    h5tools_init();

    outBuffOffset = 0;
    g_Parallel    = 1;

    MPI_Init(&argc, (char ***)&argv);

    MPI_Comm_rank(MPI_COMM_WORLD, (int *)&g_nID);
    MPI_Comm_size(MPI_COMM_WORLD, &g_nTasks);

    parse_command_line(argc, argv, &fname1, &fname2, &objname1, &objname2, &opts);

    if (g_nTasks == 1) {
        HDprintf("Only 1 task available...doing serial diff\n");

        g_Parallel = 0;

        h5diff(fname1, fname2, objname1, objname2, &opts);

        print_info(&opts);
    }
    /* Parallel h5diff */
    else {
        ph5diff(fname1, fname2, objname1, objname2, &opts);

        print_info(&opts);
    } /* end else */

    MPI_Finalize();

    return 0;
}

/*-------------------------------------------------------------------------
 * Function: h5diff_exit
 *
 * Purpose: Tool exit.  Finalize MPI and close the h5tools library.
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
    /* if in parallel mode, dismiss workers, close down MPI, then exit */
    if (g_Parallel) {
        MPI_Finalize();
        status = EXIT_SUCCESS; /* Reset exit status, since some
                                  mpiexec commands generate output on
                                  failure status */
    }

    h5tools_close();

    /* Always exit(0), since MPI implementations do weird stuff when they
     *  receive a non-zero exit value. - QAK
     */
    HDexit(status);
}
