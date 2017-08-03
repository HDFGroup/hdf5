/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer: Jordan Henderson
 *             01/31/2017
 *
 * This file contains #defines for tests of the use
 * of filters in parallel HDF5, implemented in
 * H5Dmpio.c
 */

#ifndef TEST_PARALLEL_FILTERS_H_
#define TEST_PARALLEL_FILTERS_H_

#include <string.h>

#include "stdlib.h"
#include "h5test.h"

/* Used to load other filters than GZIP */
/* #define DYNAMIC_FILTER */ /* Uncomment and define the fields below to use a dynamically loaded filter */
#define FILTER_NUM_CDVALUES  1
const unsigned int cd_values[FILTER_NUM_CDVALUES];
H5Z_filter_t       filter_id;
unsigned int       flags = 0;
size_t             cd_nelmts = FILTER_NUM_CDVALUES;

/* Utility Macros */
#define STRINGIFY(type) #type

/* Common defines for all tests */
#define FILENAME             "tfilters_parallel.h5"
#define NUM_MPI_RANKS        4
#define C_DATATYPE           long
#define C_DATATYPE_STR(type) STRINGIFY(type)
#define HDF5_DATATYPE_NAME   H5T_NATIVE_LONG
#define GEN_DATA(i)          RANK_DATA(i) /* Given an index value i, generates test data based upon selected mode */
#define RANK_DATA(i)         (mpi_rank)   /* Generates test data to visibly show which rank wrote to which parts of the dataset */
#define INCREMENTAL_DATA(i)  i            /* Generates incremental test data */
#ifdef DYNAMIC_FILTER
#define SET_FILTER(dcpl)     H5Pset_filter(dcpl, filter_id, flags, FILTER_NUM_CDVALUES, cd_values) /* Test other filter in parallel */
#else
#define SET_FILTER(dcpl)     H5Pset_deflate(dcpl, 6) /* Test GZIP filter in parallel */
#endif

/* Defines for the one-chunk filtered dataset test */
#define ONE_CHUNK_FILTERED_DATASET_NAME       "one_chunk_filtered_dataset"
#define ONE_CHUNK_FILTERED_DATASET_DIMS       2
#define ONE_CHUNK_FILTERED_DATASET_NROWS      16 /* Must be an even multiple of the number of ranks to avoid issues */
#define ONE_CHUNK_FILTERED_DATASET_NCOLS      8  /* Must be an even multiple of the number of ranks to avoid issues */
#define ONE_CHUNK_FILTERED_DATASET_CH_NROWS   ONE_CHUNK_FILTERED_DATASET_NROWS
#define ONE_CHUNK_FILTERED_DATASET_CH_NCOLS   ONE_CHUNK_FILTERED_DATASET_NCOLS

/* Defines for the unshared filtered chunks write test */
#define UNSHARED_FILTERED_CHUNKS_DATASET_NAME "unshared_filtered_chunks"
#define UNSHARED_FILTERED_CHUNKS_DATASET_DIMS 2
#define UNSHARED_FILTERED_CHUNKS_NROWS        16
#define UNSHARED_FILTERED_CHUNKS_NCOLS        8
#define UNSHARED_FILTERED_CHUNKS_CH_NROWS     (UNSHARED_FILTERED_CHUNKS_NROWS / NUM_MPI_RANKS)
#define UNSHARED_FILTERED_CHUNKS_CH_NCOLS     (UNSHARED_FILTERED_CHUNKS_NCOLS / NUM_MPI_RANKS)

/* Defines for the shared filtered chunks write test */
#define SHARED_FILTERED_CHUNKS_DATASET_NAME "shared_filtered_chunks"
#define SHARED_FILTERED_CHUNKS_DATASET_DIMS 2
#define SHARED_FILTERED_CHUNKS_NROWS        16
#define SHARED_FILTERED_CHUNKS_NCOLS        8
#define SHARED_FILTERED_CHUNKS_CH_NROWS     (SHARED_FILTERED_CHUNKS_NROWS / NUM_MPI_RANKS)
#define SHARED_FILTERED_CHUNKS_CH_NCOLS     (SHARED_FILTERED_CHUNKS_NCOLS / NUM_MPI_RANKS)

/* Defines for the filtered chunks write test where a process has no selection */
#define SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME   "single_no_selection_filtered_chunks"
#define SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS   2
#define SINGLE_NO_SELECTION_FILTERED_CHUNKS_NROWS          16
#define SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS          8
#define SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS       (SINGLE_NO_SELECTION_FILTERED_CHUNKS_NROWS / NUM_MPI_RANKS)
#define SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS       (SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS / NUM_MPI_RANKS)
#define SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC 3

/* Defines for the filtered chunks write test where no process has a selection */
#define ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME   "all_no_selection_filtered_chunks"
#define ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS   2
#define ALL_NO_SELECTION_FILTERED_CHUNKS_NROWS          16
#define ALL_NO_SELECTION_FILTERED_CHUNKS_NCOLS          8
#define ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS       (ALL_NO_SELECTION_FILTERED_CHUNKS_NROWS / NUM_MPI_RANKS)
#define ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS       (ALL_NO_SELECTION_FILTERED_CHUNKS_NCOLS / NUM_MPI_RANKS)
#define ALL_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC 3

/* Defines for the filtered chunks write test with a point selection */
#define POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME "point_selection_filtered_chunks"
#define POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS 2
#define POINT_SELECTION_FILTERED_CHUNKS_NROWS        16
#define POINT_SELECTION_FILTERED_CHUNKS_NCOLS        8
#define POINT_SELECTION_FILTERED_CHUNKS_CH_NROWS     (POINT_SELECTION_FILTERED_CHUNKS_NROWS / NUM_MPI_RANKS)
#define POINT_SELECTION_FILTERED_CHUNKS_CH_NCOLS     (POINT_SELECTION_FILTERED_CHUNKS_NCOLS / NUM_MPI_RANKS)
#define POINT_SELECTION_FILTERED_CHUNKS_NUM_CHUNKS   ((POINT_SELECTION_FILTERED_CHUNKS_NROWS / POINT_SELECTION_FILTERED_CHUNKS_CH_NROWS) * (POINT_SELECTION_FILTERED_CHUNKS_NCOLS / POINT_SELECTION_FILTERED_CHUNKS_CH_NCOLS))

/* Defines for the filtered dataset interleaved write test */
#define INTERLEAVED_WRITE_FILTERED_DATASET_NAME        "interleaved_write_filtered_dataset"
#define INTERLEAVED_WRITE_FILTERED_DATASET_DIMS        2
#define INTERLEAVED_WRITE_FILTERED_DATASET_NROWS       16 /* Must be an even multiple of the number of ranks to avoid issues */
#define INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS       8  /* Must be an even multiple of the number of ranks to avoid issues */
#define INTERLEAVED_WRITE_FILTERED_DATASET_CH_NROWS    (INTERLEAVED_WRITE_FILTERED_DATASET_NROWS / NUM_MPI_RANKS)
#define INTERLEAVED_WRITE_FILTERED_DATASET_CH_NCOLS    (INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS / NUM_MPI_RANKS)
#define INTERLEAVED_WRITE_FILTERED_DATASET_SEL_NPOINTS (INTERLEAVED_WRITE_FILTERED_DATASET_CH_NROWS * INTERLEAVED_WRITE_FILTERED_DATASET_CH_NCOLS / INTERLEAVED_WRITE_FILTERED_DATASET_NUM_RANKS)

/* Defines for the 3D unshared filtered dataset separate page write test */
#define UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME "3d_unshared_filtered_chunks_separate_pages"
#define UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS 3
#define UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS        16
#define UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS        8
#define UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DEPTH        NUM_MPI_RANKS
#define UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS     (UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS / NUM_MPI_RANKS)
#define UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS     (UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS / NUM_MPI_RANKS)

/* Defines for the 3D unshared filtered dataset same page write test */
#define UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME "3d_unshared_filtered_chunks_same_pages"
#define UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS 3
#define UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NROWS        16
#define UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS        8
#define UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH        NUM_MPI_RANKS
#define UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS     (UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NROWS / NUM_MPI_RANKS)
#define UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS     (UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS / NUM_MPI_RANKS)

/* Defines for the 3d shared filtered dataset write test */
#define SHARED_FILTERED_CHUNKS_3D_DATASET_NAME "3d_shared_filtered_chunks"
#define SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS 3
#define SHARED_FILTERED_CHUNKS_3D_NROWS        16
#define SHARED_FILTERED_CHUNKS_3D_NCOLS        8
#define SHARED_FILTERED_CHUNKS_3D_DEPTH        NUM_MPI_RANKS
#define SHARED_FILTERED_CHUNKS_3D_CH_NROWS     (SHARED_FILTERED_CHUNKS_3D_NROWS / NUM_MPI_RANKS)
#define SHARED_FILTERED_CHUNKS_3D_CH_NCOLS     (SHARED_FILTERED_CHUNKS_3D_NCOLS / NUM_MPI_RANKS)

/* Struct type for the compound datatype filtered dataset tests */
typedef struct {
    short  field1;
    int    field2;
    long   field3;
    double field4;
} cmpd_filtered_t;

/* Defines for the compound datatype filtered dataset no conversion write test with unshared chunks */
#define COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME      "compound_unshared_filtered_chunks_no_conversion"
#define COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS      2
#define COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NROWS             1
#define COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NCOLS             NUM_MPI_RANKS
#define COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS          1
#define COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS          1
#define COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC  COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NCOLS / NUM_MPI_RANKS

/* Defines for the compound datatype filtered dataset no conversion write test with shared chunks */
#define COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME      "compound_shared_filtered_chunks_no_conversion"
#define COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS      2
#define COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NROWS             NUM_MPI_RANKS
#define COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NCOLS             NUM_MPI_RANKS
#define COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS          NUM_MPI_RANKS
#define COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS          1
#define COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC  COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NCOLS

/* Defines for the compound datatype filtered dataset type conversion write test with unshared chunks */
#define COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME      "compound_unshared_filtered_chunks_type_conversion"
#define COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS      2
#define COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NROWS             1
#define COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NCOLS             NUM_MPI_RANKS
#define COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS          1
#define COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS          1
#define COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC  COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NCOLS / NUM_MPI_RANKS

/* Defines for the compound datatype filtered dataset type conversion write test with shared chunks */
#define COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME      "compound_shared_filtered_chunks_type_conversion"
#define COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS      2
#define COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NROWS             NUM_MPI_RANKS
#define COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NCOLS             NUM_MPI_RANKS
#define COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS          NUM_MPI_RANKS
#define COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS          1
#define COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC  COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NCOLS

#endif /* TEST_PARALLEL_FILTERS_H_ */
