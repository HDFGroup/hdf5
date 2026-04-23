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

#ifndef PH5DIFF_H
#define PH5DIFF_H

/* Send from manager to workers */
#define MPI_TAG_ARGS      1
#define MPI_TAG_PRINT_TOK 2

/*Sent from workers to manager */
#define MPI_TAG_TOK_REQUEST 3
#define MPI_TAG_DONE        4
#define MPI_TAG_TOK_RETURN  5
#define MPI_TAG_PRINT_DATA  6

/* Operational tags used to init and complete diff */
#define MPI_TAG_END      7
#define MPI_TAG_PARALLEL 8

/* Abort immediately on any MPI Pack/Unpack failure. */
#define MPI_CHECK(call)                                                                                      \
    do {                                                                                                     \
        int _err = (call);                                                                                   \
        if (H5_UNLIKELY(_err != MPI_SUCCESS)) {                                                              \
            printf("ph5diff: MPI error %d at %s:%d\n", _err, __FILE__, __LINE__);                            \
            MPI_Abort(MPI_COMM_WORLD, _err);                                                                 \
        }                                                                                                    \
    } while (0)

struct diff_mpi_args {
    char *name1; /* on manager: points into caller's path buffer (not owned);
                  * on worker: heap-allocated during unpack (must be freed) */
    char       *name2;
    diff_opt_t  opts;
    diff_args_t argdata; /* rest args */
};

struct diffs_found {
    hsize_t nfound;
    int     not_cmp;
};

#endif /* PH5DIFF_H */
