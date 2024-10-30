/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
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

/* For now, include testing framework functionality since the MESG, VRFY,
 * etc. macros depend on the test verbosity level
 */
#include "testframe.h"

/* File_Access_type bits */
#define FACC_DEFAULT 0x0 /* default */
#define FACC_MPIO    0x1 /* MPIO */
#define FACC_SPLIT   0x2 /* Split File */

/* Constants definitions */
#define DXFER_COLLECTIVE_IO  0x1 /* Collective IO */
#define DXFER_INDEPENDENT_IO 0x2 /* Independent IO collectively */

/* Hyperslab layout styles */
#define BYROW 1 /* divide into slabs of rows */
#define BYCOL 2 /* divide into blocks of columns */
#define ZROW  3 /* same as BYCOL except process 0 gets 0 rows */
#define ZCOL  4 /* same as BYCOL except process 0 gets 0 columns */

/* point selection order */
#define IN_ORDER     1
#define OUT_OF_ORDER 2

#define MAX_ERR_REPORT 10 /* Maximum number of errors reported */

/* Define some handy debugging shorthands, routines, ... */
/* debugging tools */

/* Print message mesg if verbose level is at least medium and
 * mesg is not an empty string.
 */
#define MESG(mesg)                                                                                           \
    do {                                                                                                     \
        if (VERBOSE_MED && *mesg != '\0') {                                                                  \
            printf("%s\n", mesg);                                                                            \
        }                                                                                                    \
    } while (0)

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
            printf("Proc %d: ", rankvar);                                                                    \
            printf("*** Parallel ERROR ***\n");                                                              \
            printf("    VRFY (%s) failed at line %4d in %s\n", mesg, (int)__LINE__, __FILE__);               \
            ++nerrors;                                                                                       \
            fflush(stdout);                                                                                  \
            if (!VERBOSE_MED) {                                                                              \
                printf("aborting MPI processes\n");                                                          \
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
            printf("Proc %d: ", mpi_rank);                                                                   \
            printf("*** PHDF5 REMARK (not an error) ***\n");                                                 \
            printf("        Condition (%s) failed at line %4d in %s\n", mesg, (int)__LINE__, __FILE__);      \
            fflush(stdout);                                                                                  \
        }                                                                                                    \
    } while (0)

#define MPI_BANNER(mesg)                                                                                     \
    do {                                                                                                     \
        if (VERBOSE_MED || MAINPROCESS) {                                                                    \
            printf("--------------------------------\n");                                                    \
            printf("Proc %d: ", mpi_rank);                                                                   \
            printf("*** %s\n", mesg);                                                                        \
            printf("--------------------------------\n");                                                    \
        }                                                                                                    \
    } while (0)

#define MAINPROCESS (!mpi_rank) /* define process 0 as main process */

#define SYNC(comm)                                                                                           \
    do {                                                                                                     \
        MPI_BANNER("doing a SYNC");                                                                          \
        MPI_Barrier(comm);                                                                                   \
        MPI_BANNER("SYNC DONE");                                                                             \
    } while (0)

/* Shared enum for some parallel tests that
 * contains values to determine how parallel
 * I/O is performed
 */
enum H5TEST_COLL_CHUNK_API {
    API_NONE = 0,
    API_LINK_HARD,
    API_MULTI_HARD,
    API_LINK_TRUE,
    API_LINK_FALSE,
    API_MULTI_COLL,
    API_MULTI_IND
};

/* Shape Same Tests Definitions */
typedef enum {
    IND_CONTIG,  /* Independent IO on contiguous datasets */
    COL_CONTIG,  /* Collective IO on contiguous datasets */
    IND_CHUNKED, /* Independent IO on chunked datasets */
    COL_CHUNKED  /* Collective IO on chunked datasets */
} ShapeSameTestMethods;

/* End of Define some handy debugging shorthands, routines, ... */

#ifdef __cplusplus
extern "C" {
#endif

H5TESTPAR_DLL hid_t create_faccess_plist(MPI_Comm comm, MPI_Info info, int l_facc_type);

H5TESTPAR_DLL void point_set(hsize_t start[], hsize_t count[], hsize_t stride[], hsize_t block[],
                             size_t num_points, hsize_t coords[], int order);

#ifdef __cplusplus
}
#endif
#endif /* TESTPAR_H */
