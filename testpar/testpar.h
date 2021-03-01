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

/* common definitions used by all parallel test programs. */

#ifndef TESTPAR_H
#define TESTPAR_H

/* Indicate that these are parallel tests, for the testing framework */
#define H5_PARALLEL_TEST

#include "h5test.h"

/* Constants definitions */
#define MAX_ERR_REPORT 10 /* Maximum number of errors reported */

/* Define some handy debugging shorthands, routines, ... */
/* debugging tools */

/* Print message mesg if verbose level is at least medium and
 * mesg is not an empty string.
 */
#define MESG(mesg)                                                                                           \
    if (VERBOSE_MED && *mesg != '\0')                                                                        \
    HDprintf("%s\n", mesg)

/*
 * VRFY: Verify if the condition val is true.
 * If it is true, then call MESG to print mesg, depending on the verbose
 * level.
 * If val is not true, it prints error messages and if the verbose
 * level is lower than medium, it calls MPI_Abort to abort the program.
 * If verbose level is at least medium, it will not abort.
 * This will allow program to continue and can be used for debugging.
 * (The "do {...} while(0)" is to group all the statements as one unit.)
 */
#define VRFY_IMPL(val, mesg, rankvar)                                                                        \
    do {                                                                                                     \
        if (val) {                                                                                           \
            MESG(mesg);                                                                                      \
        }                                                                                                    \
        else {                                                                                               \
            HDprintf("Proc %d: ", rankvar);                                                                  \
            HDprintf("*** Parallel ERROR ***\n");                                                            \
            HDprintf("    VRFY (%s) failed at line %4d in %s\n", mesg, (int)__LINE__, __FILE__);             \
            ++nerrors;                                                                                       \
            fflush(stdout);                                                                                  \
            if (!VERBOSE_MED) {                                                                              \
                HDprintf("aborting MPI processes\n");                                                        \
                MPI_Abort(MPI_COMM_WORLD, 1);                                                                \
            }                                                                                                \
        }                                                                                                    \
    } while (0)

#define VRFY_G(val, mesg) VRFY_IMPL(val, mesg, mpi_rank_g)
#define VRFY(val, mesg)   VRFY_IMPL(val, mesg, mpi_rank)

/*
 * Checking for information purpose.
 * If val is false, print mesg; else nothing.
 * Either case, no error setting.
 */
#define INFO(val, mesg)                                                                                      \
    do {                                                                                                     \
        if (val) {                                                                                           \
            MESG(mesg);                                                                                      \
        }                                                                                                    \
        else {                                                                                               \
            HDprintf("Proc %d: ", mpi_rank);                                                                 \
            HDprintf("*** PHDF5 REMARK (not an error) ***\n");                                               \
            HDprintf("        Condition (%s) failed at line %4d in %s\n", mesg, (int)__LINE__, __FILE__);    \
            fflush(stdout);                                                                                  \
        }                                                                                                    \
    } while (0)

#define MPI_BANNER(mesg)                                                                                     \
    do {                                                                                                     \
        if (VERBOSE_MED || MAINPROCESS) {                                                                    \
            HDprintf("--------------------------------\n");                                                  \
            HDprintf("Proc %d: ", mpi_rank);                                                                 \
            HDprintf("*** %s\n", mesg);                                                                      \
            HDprintf("--------------------------------\n");                                                  \
        }                                                                                                    \
    } while (0)

#define MAINPROCESS (!mpi_rank) /* define process 0 as main process */

#define SYNC(comm)                                                                                           \
    do {                                                                                                     \
        MPI_BANNER("doing a SYNC");                                                                          \
        MPI_Barrier(comm);                                                                                   \
        MPI_BANNER("SYNC DONE");                                                                             \
    } while (0)

/* End of Define some handy debugging shorthands, routines, ... */

#endif /* TESTPAR_H */
