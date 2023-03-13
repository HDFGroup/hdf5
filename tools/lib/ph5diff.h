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

struct diff_mpi_args {
    char        name1[256];
    char        name2[256];
    diff_opt_t  opts;
    diff_args_t argdata; /* rest args */
};

struct diffs_found {
    hsize_t nfound;
    int     not_cmp;
};

#endif /* PH5DIFF_H */
