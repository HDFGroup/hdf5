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
#include "testpar.h"

/* Used to load other filters than GZIP */
/* #define DYNAMIC_FILTER */ /* Uncomment and define the fields below to use a dynamically loaded filter */
#define FILTER_NUM_CDVALUES  1
const unsigned int cd_values[FILTER_NUM_CDVALUES] = { 0 };
H5Z_filter_t       filter_id;
unsigned int       flags = 0;
size_t             cd_nelmts = FILTER_NUM_CDVALUES;

/* Utility Macros */
#define STRINGIFY(type) #type

/* Common defines for all tests */
#define C_DATATYPE           long
#define C_DATATYPE_MPI       MPI_LONG
#define COMPOUND_C_DATATYPE  cmpd_filtered_t
#define C_DATATYPE_STR(type) STRINGIFY(type)
#define HDF5_DATATYPE_NAME   H5T_NATIVE_LONG

/* Macro used to generate data for datasets for later verification */
#define GEN_DATA(i)          INCREMENTAL_DATA(i)

/* For experimental purposes only, will cause tests to fail data verification phase - JTH */
/* #define GEN_DATA(i)          RANK_DATA(i) */  /* Given an index value i, generates test data based upon selected mode */

#define INCREMENTAL_DATA(i)  ((size_t) mpi_rank + i)  /* Generates incremental test data */
#define RANK_DATA(i)         (mpi_rank)               /* Generates test data to visibly show which rank wrote to which parts of the dataset */

#define DEFAULT_DEFLATE_LEVEL 6

#define DIM0_SCALE_FACTOR 4
#define DIM1_SCALE_FACTOR 2

/* Struct type for the compound datatype filtered dataset tests */
typedef struct {
    short  field1;
    int    field2;
    long   field3;
} COMPOUND_C_DATATYPE;

/* Defines for the one-chunk filtered dataset write test */
#define WRITE_ONE_CHUNK_FILTERED_DATASET_NAME       "one_chunk_filtered_dataset_write"
#define WRITE_ONE_CHUNK_FILTERED_DATASET_DIMS       2
#define WRITE_ONE_CHUNK_FILTERED_DATASET_NROWS      (mpi_size * DIM0_SCALE_FACTOR) /* Must be an even multiple of the number of ranks to avoid issues */
#define WRITE_ONE_CHUNK_FILTERED_DATASET_NCOLS      (mpi_size * DIM1_SCALE_FACTOR) /* Must be an even multiple of the number of ranks to avoid issues */
#define WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NROWS   WRITE_ONE_CHUNK_FILTERED_DATASET_NROWS
#define WRITE_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS   WRITE_ONE_CHUNK_FILTERED_DATASET_NCOLS

/* Defines for the unshared filtered chunks write test */
#define WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_NAME "unshared_filtered_chunks_write"
#define WRITE_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS 2
#define WRITE_UNSHARED_FILTERED_CHUNKS_NROWS        (mpi_size * DIM0_SCALE_FACTOR)
#define WRITE_UNSHARED_FILTERED_CHUNKS_NCOLS        (mpi_size * DIM1_SCALE_FACTOR)
#define WRITE_UNSHARED_FILTERED_CHUNKS_CH_NROWS     (WRITE_UNSHARED_FILTERED_CHUNKS_NROWS / mpi_size)
#define WRITE_UNSHARED_FILTERED_CHUNKS_CH_NCOLS     (WRITE_UNSHARED_FILTERED_CHUNKS_NCOLS / mpi_size)

/* Defines for the shared filtered chunks write test */
#define WRITE_SHARED_FILTERED_CHUNKS_DATASET_NAME "shared_filtered_chunks_write"
#define WRITE_SHARED_FILTERED_CHUNKS_DATASET_DIMS 2
#define WRITE_SHARED_FILTERED_CHUNKS_CH_NROWS     (mpi_size)
#define WRITE_SHARED_FILTERED_CHUNKS_CH_NCOLS     (mpi_size)
#define WRITE_SHARED_FILTERED_CHUNKS_NROWS        (WRITE_SHARED_FILTERED_CHUNKS_CH_NROWS * DIM0_SCALE_FACTOR)
#define WRITE_SHARED_FILTERED_CHUNKS_NCOLS        (WRITE_SHARED_FILTERED_CHUNKS_CH_NCOLS * DIM1_SCALE_FACTOR)

/* Defines for the filtered chunks write test where a process has no selection */
#define WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME   "single_no_selection_filtered_chunks_write"
#define WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS   2
#define WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS       (DIM0_SCALE_FACTOR)
#define WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS       (DIM1_SCALE_FACTOR)
#define WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NROWS          (WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS * mpi_size)
#define WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS          (WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS * mpi_size)
#define WRITE_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC (mpi_size - 1)

/* Defines for the filtered chunks write test where no process has a selection */
#define WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME   "all_no_selection_filtered_chunks_write"
#define WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS   2
#define WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS       (DIM0_SCALE_FACTOR)
#define WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS       (DIM1_SCALE_FACTOR)
#define WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_NROWS          (WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS * mpi_size)
#define WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_NCOLS          (WRITE_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS * mpi_size)

/* Defines for the filtered chunks write test with a point selection */
#define WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME "point_selection_filtered_chunks_write"
#define WRITE_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS 2
#define WRITE_POINT_SELECTION_FILTERED_CHUNKS_CH_NROWS     (DIM0_SCALE_FACTOR)
#define WRITE_POINT_SELECTION_FILTERED_CHUNKS_CH_NCOLS     (DIM1_SCALE_FACTOR)
#define WRITE_POINT_SELECTION_FILTERED_CHUNKS_NROWS        (WRITE_POINT_SELECTION_FILTERED_CHUNKS_CH_NROWS * mpi_size)
#define WRITE_POINT_SELECTION_FILTERED_CHUNKS_NCOLS        (WRITE_POINT_SELECTION_FILTERED_CHUNKS_CH_NCOLS * mpi_size)

/* Defines for the filtered dataset interleaved write test */
#define INTERLEAVED_WRITE_FILTERED_DATASET_NAME        "filtered_dataset_interleaved_write"
#define INTERLEAVED_WRITE_FILTERED_DATASET_DIMS        2
#define INTERLEAVED_WRITE_FILTERED_DATASET_CH_NROWS    (mpi_size)
#define INTERLEAVED_WRITE_FILTERED_DATASET_CH_NCOLS    (DIM1_SCALE_FACTOR)
#define INTERLEAVED_WRITE_FILTERED_DATASET_NROWS       (INTERLEAVED_WRITE_FILTERED_DATASET_CH_NROWS * DIM0_SCALE_FACTOR)
#define INTERLEAVED_WRITE_FILTERED_DATASET_NCOLS       (INTERLEAVED_WRITE_FILTERED_DATASET_CH_NCOLS * DIM1_SCALE_FACTOR)

/* Defines for the 3D unshared filtered dataset separate page write test */
#define WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME "3D_unshared_filtered_chunks_separate_pages_write"
#define WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS 3
#define WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS        (mpi_size * DIM0_SCALE_FACTOR)
#define WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS        (mpi_size * DIM1_SCALE_FACTOR)
#define WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DEPTH        (mpi_size)
#define WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS     (WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS / mpi_size)
#define WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS     (WRITE_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS / mpi_size)

/* Defines for the 3D unshared filtered dataset same page write test */
#define WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME "3D_unshared_filtered_chunks_same_pages_write"
#define WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS 3
#define WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NROWS        (mpi_size * DIM0_SCALE_FACTOR)
#define WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS        (mpi_size * DIM1_SCALE_FACTOR)
#define WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH        (mpi_size)
#define WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS     (WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NROWS / mpi_size)
#define WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS     (WRITE_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS / mpi_size)

/* Defines for the 3d shared filtered dataset write test */
#define WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_NAME "3D_shared_filtered_chunks_write"
#define WRITE_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS 3
#define WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NROWS     (mpi_size)
#define WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS     (DIM1_SCALE_FACTOR)
#define WRITE_SHARED_FILTERED_CHUNKS_3D_NROWS        (WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NROWS * DIM0_SCALE_FACTOR)
#define WRITE_SHARED_FILTERED_CHUNKS_3D_NCOLS        (WRITE_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS * DIM1_SCALE_FACTOR)
#define WRITE_SHARED_FILTERED_CHUNKS_3D_DEPTH        (mpi_size)

/* Defines for the compound datatype filtered dataset no conversion write test with unshared chunks */
#define WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME      "compound_unshared_filtered_chunks_no_conversion_write"
#define WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS      2
#define WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NROWS             1
#define WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NCOLS             mpi_size
#define WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS          1
#define WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS          1
#define WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC  (WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NCOLS / mpi_size)

/* Defines for the compound datatype filtered dataset no conversion write test with shared chunks */
#define WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME      "compound_shared_filtered_chunks_no_conversion_write"
#define WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS      2
#define WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NROWS             mpi_size
#define WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NCOLS             mpi_size
#define WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS          mpi_size
#define WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS          1
#define WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC  WRITE_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NCOLS

/* Defines for the compound datatype filtered dataset type conversion write test with unshared chunks */
#define WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME      "compound_unshared_filtered_chunks_type_conversion_write"
#define WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS      2
#define WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NROWS             1
#define WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NCOLS             mpi_size
#define WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS          1
#define WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS          1
#define WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC  (WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NCOLS / mpi_size)

/* Defines for the compound datatype filtered dataset type conversion write test with shared chunks */
#define WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME      "compound_shared_filtered_chunks_type_conversion_write"
#define WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS      2
#define WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NROWS             mpi_size
#define WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NCOLS             mpi_size
#define WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS          mpi_size
#define WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS          1
#define WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC  WRITE_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NCOLS

/* Defines for the one-chunk filtered dataset read test */
#define READ_ONE_CHUNK_FILTERED_DATASET_NAME       "one_chunk_filtered_dataset_read"
#define READ_ONE_CHUNK_FILTERED_DATASET_DIMS       2
#define READ_ONE_CHUNK_FILTERED_DATASET_NROWS      (mpi_size * DIM0_SCALE_FACTOR) /* Must be an even multiple of the number of ranks to avoid issues */
#define READ_ONE_CHUNK_FILTERED_DATASET_NCOLS      (mpi_size * DIM1_SCALE_FACTOR) /* Must be an even multiple of the number of ranks to avoid issues */
#define READ_ONE_CHUNK_FILTERED_DATASET_CH_NROWS   READ_ONE_CHUNK_FILTERED_DATASET_NROWS
#define READ_ONE_CHUNK_FILTERED_DATASET_CH_NCOLS   READ_ONE_CHUNK_FILTERED_DATASET_NCOLS

/* Defines for the unshared filtered chunks read test */
#define READ_UNSHARED_FILTERED_CHUNKS_DATASET_NAME "unshared_filtered_chunks_read"
#define READ_UNSHARED_FILTERED_CHUNKS_DATASET_DIMS 2
#define READ_UNSHARED_FILTERED_CHUNKS_NROWS        (mpi_size * DIM0_SCALE_FACTOR)
#define READ_UNSHARED_FILTERED_CHUNKS_NCOLS        (mpi_size * DIM1_SCALE_FACTOR)
#define READ_UNSHARED_FILTERED_CHUNKS_CH_NROWS     (READ_UNSHARED_FILTERED_CHUNKS_NROWS / mpi_size)
#define READ_UNSHARED_FILTERED_CHUNKS_CH_NCOLS     (READ_UNSHARED_FILTERED_CHUNKS_NCOLS / mpi_size)

/* Defines for the shared filtered chunks read test */
#define READ_SHARED_FILTERED_CHUNKS_DATASET_NAME "shared_filtered_chunks_read"
#define READ_SHARED_FILTERED_CHUNKS_DATASET_DIMS 2
#define READ_SHARED_FILTERED_CHUNKS_CH_NROWS     (mpi_size)
#define READ_SHARED_FILTERED_CHUNKS_CH_NCOLS     (mpi_size)
#define READ_SHARED_FILTERED_CHUNKS_NROWS        (READ_SHARED_FILTERED_CHUNKS_CH_NROWS * DIM0_SCALE_FACTOR)
#define READ_SHARED_FILTERED_CHUNKS_NCOLS        (READ_SHARED_FILTERED_CHUNKS_CH_NCOLS * DIM1_SCALE_FACTOR)

/* Defines for the filtered chunks read test where a process has no selection */
#define READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME   "single_no_selection_filtered_chunks_read"
#define READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS   2
#define READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS       (DIM0_SCALE_FACTOR)
#define READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS       (DIM1_SCALE_FACTOR)
#define READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NROWS          (READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS * mpi_size)
#define READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NCOLS          (READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS * mpi_size)
#define READ_SINGLE_NO_SELECTION_FILTERED_CHUNKS_NO_SELECT_PROC (mpi_size - 1)

/* Defines for the filtered chunks read test where no process has a selection */
#define READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_NAME   "all_no_selection_filtered_chunks_read"
#define READ_ALL_NO_SELECTION_FILTERED_CHUNKS_DATASET_DIMS   2
#define READ_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS       (DIM0_SCALE_FACTOR)
#define READ_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS       (DIM1_SCALE_FACTOR)
#define READ_ALL_NO_SELECTION_FILTERED_CHUNKS_NROWS          (READ_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NROWS * mpi_size)
#define READ_ALL_NO_SELECTION_FILTERED_CHUNKS_NCOLS          (READ_ALL_NO_SELECTION_FILTERED_CHUNKS_CH_NCOLS * mpi_size)

/* Defines for the filtered chunks read test with a point selection */
#define READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_NAME "point_selection_filtered_chunks_read"
#define READ_POINT_SELECTION_FILTERED_CHUNKS_DATASET_DIMS 2
#define READ_POINT_SELECTION_FILTERED_CHUNKS_CH_NROWS     (DIM0_SCALE_FACTOR)
#define READ_POINT_SELECTION_FILTERED_CHUNKS_CH_NCOLS     (DIM1_SCALE_FACTOR)
#define READ_POINT_SELECTION_FILTERED_CHUNKS_NROWS        (READ_POINT_SELECTION_FILTERED_CHUNKS_CH_NROWS * mpi_size)
#define READ_POINT_SELECTION_FILTERED_CHUNKS_NCOLS        (READ_POINT_SELECTION_FILTERED_CHUNKS_CH_NCOLS * mpi_size)

/* Defines for the filtered dataset interleaved read test */
#define INTERLEAVED_READ_FILTERED_DATASET_NAME        "filtered_dataset_interleaved_read"
#define INTERLEAVED_READ_FILTERED_DATASET_DIMS        2
#define INTERLEAVED_READ_FILTERED_DATASET_CH_NROWS    (mpi_size)
#define INTERLEAVED_READ_FILTERED_DATASET_CH_NCOLS    (DIM1_SCALE_FACTOR)
#define INTERLEAVED_READ_FILTERED_DATASET_NROWS       (INTERLEAVED_READ_FILTERED_DATASET_CH_NROWS * DIM0_SCALE_FACTOR)
#define INTERLEAVED_READ_FILTERED_DATASET_NCOLS       (INTERLEAVED_READ_FILTERED_DATASET_CH_NCOLS * DIM1_SCALE_FACTOR)

/* Defines for the 3D unshared filtered dataset separate page read test */
#define READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_NAME "3D_unshared_filtered_chunks_separate_pages_read"
#define READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DATASET_DIMS 3
#define READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS        (mpi_size * DIM0_SCALE_FACTOR)
#define READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS        (mpi_size * DIM1_SCALE_FACTOR)
#define READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_DEPTH        (mpi_size)
#define READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NROWS     (READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NROWS / mpi_size)
#define READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_CH_NCOLS     (READ_UNSHARED_FILTERED_CHUNKS_3D_SEP_PAGE_NCOLS / mpi_size)

/* Defines for the 3D unshared filtered dataset same page read test */
#define READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_NAME "3D_unshared_filtered_chunks_same_pages_read"
#define READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DATASET_DIMS 3
#define READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NROWS        (mpi_size * DIM0_SCALE_FACTOR)
#define READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS        (mpi_size * DIM1_SCALE_FACTOR)
#define READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_DEPTH        (mpi_size)
#define READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NROWS     (READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NROWS / mpi_size)
#define READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_CH_NCOLS     (READ_UNSHARED_FILTERED_CHUNKS_3D_SAME_PAGE_NCOLS / mpi_size)

/* Defines for the 3d shared filtered dataset read test */
#define READ_SHARED_FILTERED_CHUNKS_3D_DATASET_NAME "3D_shared_filtered_chunks_read"
#define READ_SHARED_FILTERED_CHUNKS_3D_DATASET_DIMS 3
#define READ_SHARED_FILTERED_CHUNKS_3D_CH_NROWS     (mpi_size)
#define READ_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS     (DIM1_SCALE_FACTOR)
#define READ_SHARED_FILTERED_CHUNKS_3D_NROWS        (READ_SHARED_FILTERED_CHUNKS_3D_CH_NROWS * DIM0_SCALE_FACTOR)
#define READ_SHARED_FILTERED_CHUNKS_3D_NCOLS        (READ_SHARED_FILTERED_CHUNKS_3D_CH_NCOLS * DIM1_SCALE_FACTOR)
#define READ_SHARED_FILTERED_CHUNKS_3D_DEPTH        (mpi_size)

/* Defines for the compound datatype filtered dataset no conversion read test with unshared chunks */
#define READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_NAME      "compound_unshared_filtered_chunks_no_conversion_read"
#define READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_DATASET_DIMS      2
#define READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NROWS             1
#define READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NCOLS             mpi_size
#define READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NROWS          1
#define READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_CH_NCOLS          1
#define READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_ENTRIES_PER_PROC  (READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_UNSHARED_NCOLS / mpi_size)

/* Defines for the compound datatype filtered dataset no conversion read test with shared chunks */
#define READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_NAME      "compound_shared_filtered_chunks_no_conversion_read"
#define READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_DATASET_DIMS      2
#define READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NROWS             mpi_size
#define READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NCOLS             mpi_size
#define READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NROWS          mpi_size
#define READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_CH_NCOLS          1
#define READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_ENTRIES_PER_PROC  READ_COMPOUND_FILTERED_CHUNKS_NO_CONVERSION_SHARED_NCOLS

/* Defines for the compound datatype filtered dataset type conversion read test with unshared chunks */
#define READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_NAME      "compound_unshared_filtered_chunks_type_conversion_read"
#define READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_DATASET_DIMS      2
#define READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NROWS             1
#define READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NCOLS             mpi_size
#define READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NROWS          1
#define READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_CH_NCOLS          1
#define READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_ENTRIES_PER_PROC  (READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_UNSHARED_NCOLS / mpi_size)

/* Defines for the compound datatype filtered dataset type conversion read test with shared chunks */
#define READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_NAME      "compound_shared_filtered_chunks_type_conversion_read"
#define READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_DATASET_DIMS      2
#define READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NROWS             mpi_size
#define READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NCOLS             mpi_size
#define READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NROWS          mpi_size
#define READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_CH_NCOLS          1
#define READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_ENTRIES_PER_PROC  READ_COMPOUND_FILTERED_CHUNKS_TYPE_CONVERSION_SHARED_NCOLS

/* Defines for the write file serially/read in parallel test */
#define WRITE_SERIAL_READ_PARALLEL_DATASET_NAME "write_serial_read_parallel"
#define WRITE_SERIAL_READ_PARALLEL_DATASET_DIMS 3
#define WRITE_SERIAL_READ_PARALLEL_NROWS        (mpi_size * DIM0_SCALE_FACTOR)
#define WRITE_SERIAL_READ_PARALLEL_NCOLS        (mpi_size * DIM1_SCALE_FACTOR)
#define WRITE_SERIAL_READ_PARALLEL_DEPTH        (mpi_size)
#define WRITE_SERIAL_READ_PARALLEL_CH_NROWS     (WRITE_SERIAL_READ_PARALLEL_NROWS / mpi_size)
#define WRITE_SERIAL_READ_PARALLEL_CH_NCOLS     (WRITE_SERIAL_READ_PARALLEL_NCOLS / mpi_size)

/* Defines for the write file in parallel/read serially test */
#define WRITE_PARALLEL_READ_SERIAL_DATASET_NAME "write_parallel_read_serial"
#define WRITE_PARALLEL_READ_SERIAL_DATASET_DIMS 3
#define WRITE_PARALLEL_READ_SERIAL_NROWS        (mpi_size * DIM0_SCALE_FACTOR)
#define WRITE_PARALLEL_READ_SERIAL_NCOLS        (mpi_size * DIM1_SCALE_FACTOR)
#define WRITE_PARALLEL_READ_SERIAL_DEPTH        (mpi_size)
#define WRITE_PARALLEL_READ_SERIAL_CH_NROWS     (WRITE_PARALLEL_READ_SERIAL_NROWS / mpi_size)
#define WRITE_PARALLEL_READ_SERIAL_CH_NCOLS     (WRITE_PARALLEL_READ_SERIAL_NCOLS / mpi_size)

/* Defines for the shrinking/growing chunks test */
#define SHRINKING_GROWING_CHUNKS_DATASET_NAME "shrink_grow_chunks_test"
#define SHRINKING_GROWING_CHUNKS_DATASET_DIMS 2
#define SHRINKING_GROWING_CHUNKS_NROWS        (mpi_size * DIM0_SCALE_FACTOR)
#define SHRINKING_GROWING_CHUNKS_NCOLS        (mpi_size * DIM1_SCALE_FACTOR)
#define SHRINKING_GROWING_CHUNKS_CH_NROWS     (SHRINKING_GROWING_CHUNKS_NROWS / mpi_size)
#define SHRINKING_GROWING_CHUNKS_CH_NCOLS     (SHRINKING_GROWING_CHUNKS_NCOLS / mpi_size)
#define SHRINKING_GROWING_CHUNKS_NLOOPS       20

#endif /* TEST_PARALLEL_FILTERS_H_ */
