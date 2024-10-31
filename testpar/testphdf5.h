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

/* common definitions used by all parallel hdf5 test programs. */

#ifndef PHDF5TEST_H
#define PHDF5TEST_H

/* Include testing framework functionality */
#include "testframe.h"

#include "testpar.h"

#ifndef false
#define false 0
#endif

#ifndef true
#define true 1
#endif

/* Constants definitions */
#define DIM0         600  /* Default dataset sizes. */
#define DIM1         1200 /* Values are from a monitor pixel sizes */
#define ROW_FACTOR   8    /* Nominal row factor for dataset size */
#define COL_FACTOR   16   /* Nominal column factor for dataset size */
#define RANK         2
#define DATASETNAME1 "Data1"
#define DATASETNAME2 "Data2"
#define DATASETNAME3 "Data3"
#define DATASETNAME4 "Data4"
#define DATASETNAME5 "Data5"
#define DATASETNAME6 "Data6"
#define DATASETNAME7 "Data7"
#define DATASETNAME8 "Data8"
#define DATASETNAME9 "Data9"

/*Constants for collective chunk definitions */
#define SPACE_DIM1            24
#define SPACE_DIM2            4
#define BYROW_CONT            1
#define BYROW_DISCONT         2
#define BYROW_SELECTNONE      3
#define BYROW_SELECTUNBALANCE 4
#define BYROW_SELECTINCHUNK   5

#define DIMO_NUM_CHUNK        4
#define DIM1_NUM_CHUNK        2
#define LINK_TRUE_NUM_CHUNK   2
#define LINK_FALSE_NUM_CHUNK  6
#define MULTI_TRUE_PERCENT    50
#define LINK_TRUE_CHUNK_NAME  "h5_link_chunk_TRUE"
#define LINK_FALSE_CHUNK_NAME "h5_link_chunk_FALSE"
#define LINK_HARD_CHUNK_NAME  "h5_link_chunk_hard"
#define MULTI_HARD_CHUNK_NAME "h5_multi_chunk_hard"
#define MULTI_COLL_CHUNK_NAME "h5_multi_chunk_coll"
#define MULTI_INDP_CHUNK_NAME "h5_multi_chunk_indp"

#define DSET_COLLECTIVE_CHUNK_NAME "coll_chunk_name"

/*Constants for MPI derived data type generated from span tree */

#define MSPACE1_RANK 1     /* Rank of the first dataset in memory */
#define MSPACE1_DIM  27000 /* Dataset size in memory */
#define FSPACE_RANK  2     /* Dataset rank as it is stored in the file */
#define FSPACE_DIM1  9     /* Dimension sizes of the dataset as it is stored in the file */
#define FSPACE_DIM2  3600
/* We will read dataset back from the file to the dataset in memory with these dataspace parameters. */
#define MSPACE_RANK 2
#define MSPACE_DIM1 9
#define MSPACE_DIM2 3600
#define FHCOUNT0    1   /* Count of the first dimension of the first hyperslab selection*/
#define FHCOUNT1    768 /* Count of the second dimension of the first hyperslab selection*/
#define FHSTRIDE0   4   /* Stride of the first dimension of the first hyperslab selection*/
#define FHSTRIDE1   3   /* Stride of the second dimension of the first hyperslab selection*/
#define FHBLOCK0    3   /* Block of the first dimension of the first hyperslab selection*/
#define FHBLOCK1    2   /* Block of the second dimension of the first hyperslab selection*/
#define FHSTART0    0   /* start of the first dimension of the first hyperslab selection*/
#define FHSTART1    1   /* start of the second dimension of the first hyperslab selection*/

#define SHCOUNT0  1   /* Count of the first dimension of the first hyperslab selection*/
#define SHCOUNT1  1   /* Count of the second dimension of the first hyperslab selection*/
#define SHSTRIDE0 1   /* Stride of the first dimension of the first hyperslab selection*/
#define SHSTRIDE1 1   /* Stride of the second dimension of the first hyperslab selection*/
#define SHBLOCK0  3   /* Block of the first dimension of the first hyperslab selection*/
#define SHBLOCK1  768 /* Block of the second dimension of the first hyperslab selection*/
#define SHSTART0  4   /* start of the first dimension of the first hyperslab selection*/
#define SHSTART1  0   /* start of the second dimension of the first hyperslab selection*/

#define MHCOUNT0  6912 /* Count of the first dimension of the first hyperslab selection*/
#define MHSTRIDE0 1    /* Stride of the first dimension of the first hyperslab selection*/
#define MHBLOCK0  1    /* Block of the first dimension of the first hyperslab selection*/
#define MHSTART0  1    /* start of the first dimension of the first hyperslab selection*/

#define RFFHCOUNT0  3   /* Count of the first dimension of the first hyperslab selection*/
#define RFFHCOUNT1  768 /* Count of the second dimension of the first hyperslab selection*/
#define RFFHSTRIDE0 1   /* Stride of the first dimension of the first hyperslab selection*/
#define RFFHSTRIDE1 1   /* Stride of the second dimension of the first hyperslab selection*/
#define RFFHBLOCK0  1   /* Block of the first dimension of the first hyperslab selection*/
#define RFFHBLOCK1  1   /* Block of the second dimension of the first hyperslab selection*/
#define RFFHSTART0  1   /* start of the first dimension of the first hyperslab selection*/
#define RFFHSTART1  2   /* start of the second dimension of the first hyperslab selection*/

#define RFSHCOUNT0  3    /* Count of the first dimension of the first hyperslab selection*/
#define RFSHCOUNT1  1536 /* Count of the second dimension of the first hyperslab selection*/
#define RFSHSTRIDE0 1    /* Stride of the first dimension of the first hyperslab selection*/
#define RFSHSTRIDE1 1    /* Stride of the second dimension of the first hyperslab selection*/
#define RFSHBLOCK0  1    /* Block of the first dimension of the first hyperslab selection*/
#define RFSHBLOCK1  1    /* Block of the second dimension of the first hyperslab selection*/
#define RFSHSTART0  2    /* start of the first dimension of the first hyperslab selection*/
#define RFSHSTART1  4    /* start of the second dimension of the first hyperslab selection*/

#define RMFHCOUNT0  3   /* Count of the first dimension of the first hyperslab selection*/
#define RMFHCOUNT1  768 /* Count of the second dimension of the first hyperslab selection*/
#define RMFHSTRIDE0 1   /* Stride of the first dimension of the first hyperslab selection*/
#define RMFHSTRIDE1 1   /* Stride of the second dimension of the first hyperslab selection*/
#define RMFHBLOCK0  1   /* Block of the first dimension of the first hyperslab selection*/
#define RMFHBLOCK1  1   /* Block of the second dimension of the first hyperslab selection*/
#define RMFHSTART0  0   /* start of the first dimension of the first hyperslab selection*/
#define RMFHSTART1  0   /* start of the second dimension of the first hyperslab selection*/

#define RMSHCOUNT0  3    /* Count of the first dimension of the first hyperslab selection*/
#define RMSHCOUNT1  1536 /* Count of the second dimension of the first hyperslab selection*/
#define RMSHSTRIDE0 1    /* Stride of the first dimension of the first hyperslab selection*/
#define RMSHSTRIDE1 1    /* Stride of the second dimension of the first hyperslab selection*/
#define RMSHBLOCK0  1    /* Block of the first dimension of the first hyperslab selection*/
#define RMSHBLOCK1  1    /* Block of the second dimension of the first hyperslab selection*/
#define RMSHSTART0  1    /* start of the first dimension of the first hyperslab selection*/
#define RMSHSTART1  2    /* start of the second dimension of the first hyperslab selection*/

#define NPOINTS                                                                                              \
    4 /* Number of points that will be selected                                                              \
                                  and overwritten */

/* Definitions of the selection mode for the test_actual_io_function. */
#define TEST_ACTUAL_IO_NO_COLLECTIVE            0
#define TEST_ACTUAL_IO_RESET                    1
#define TEST_ACTUAL_IO_MULTI_CHUNK_IND          2
#define TEST_ACTUAL_IO_MULTI_CHUNK_COL          3
#define TEST_ACTUAL_IO_MULTI_CHUNK_MIX          4
#define TEST_ACTUAL_IO_MULTI_CHUNK_MIX_DISAGREE 5
#define TEST_ACTUAL_IO_DIRECT_MULTI_CHUNK_IND   6
#define TEST_ACTUAL_IO_DIRECT_MULTI_CHUNK_COL   7
#define TEST_ACTUAL_IO_LINK_CHUNK               8
#define TEST_ACTUAL_IO_CONTIGUOUS               9

/* Definitions of the selection mode for the no_collective_cause_tests function. */
#define TEST_COLLECTIVE                                 0x001
#define TEST_SET_INDEPENDENT                            0x002
#define TEST_DATATYPE_CONVERSION                        0x004
#define TEST_DATA_TRANSFORMS                            0x008
#define TEST_NOT_SIMPLE_OR_SCALAR_DATASPACES            0x010
#define TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET_COMPACT  0x020
#define TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET_EXTERNAL 0x040

/* Don't erase these lines, they are put here for debugging purposes */
/*
#define MSPACE1_RANK     1
#define MSPACE1_DIM      50
#define MSPACE2_RANK     1
#define MSPACE2_DIM      4
#define FSPACE_RANK      2
#define FSPACE_DIM1      8
#define FSPACE_DIM2      12
#define MSPACE_RANK      2
#define MSPACE_DIM1      8
#define MSPACE_DIM2      9
#define NPOINTS          4
*/ /* end of debugging macro */

/* type definitions */
typedef struct H5Ptest_param_t /* holds extra test parameters */
{
    char *name;
    int   count;
} H5Ptest_param_t;

/* Dataset data type.  Int's can be easily octo dumped. */
typedef int DATATYPE;

/* Shared global variables */
extern int dim0, dim1;           /*Dataset dimensions */
extern int chunkdim0, chunkdim1; /*Chunk dimensions */
extern int nerrors;              /*errors count */
extern int facc_type;            /*Test file access type */
extern int dxfer_coll_type;

/* Test program prototypes */
void test_plist_ed(const void *params);
void external_links(const void *params);
void zero_dim_dset(const void *params);
void test_file_properties(const void *params);
void test_delete(const void *params);
void test_invalid_libver_bounds_file_close_assert(const void *params);
void test_evict_on_close_parallel_unsupp(const void *params);
void test_fapl_preserve_hints(const void *params);
void multiple_dset_write(const void *params);
void multiple_group_write(const void *params);
void multiple_group_read(const void *params);
void collective_group_write_independent_group_read(const void *params);
void collective_group_write(const void *params);
void independent_group_read(const void *params);
void test_fapl_mpio_dup(const void *params);
void test_get_dxpl_mpio(const void *params);
void test_split_comm_access(const void *params);
void test_page_buffer_access(const void *params);
void dataset_atomicity(const void *params);
void dataset_writeInd(const void *params);
void dataset_writeAll(const void *params);
void extend_writeInd(const void *params);
void extend_writeInd2(const void *params);
void extend_writeAll(const void *params);
void dataset_readInd(const void *params);
void dataset_readAll(const void *params);
void extend_readInd(const void *params);
void extend_readAll(const void *params);
void none_selection_chunk(const void *params);
void actual_io_mode_tests(const void *params);
void no_collective_cause_tests(const void *params);
void test_chunk_alloc(const void *params);
void test_chunk_alloc_incr_ser_to_par(const void *params);
void test_filter_read(const void *params);
void compact_dataset(const void *params);
void null_dataset(const void *params);
void big_dataset(const void *params);
void dataset_fillvalue(const void *params);
void coll_chunk1(const void *params);
void coll_chunk2(const void *params);
void coll_chunk3(const void *params);
void coll_chunk4(const void *params);
void coll_chunk5(const void *params);
void coll_chunk6(const void *params);
void coll_chunk7(const void *params);
void coll_chunk8(const void *params);
void coll_chunk9(const void *params);
void coll_chunk10(const void *params);
void coll_irregular_cont_read(const void *params);
void coll_irregular_cont_write(const void *params);
void coll_irregular_simple_chunk_read(const void *params);
void coll_irregular_simple_chunk_write(const void *params);
void coll_irregular_complex_chunk_read(const void *params);
void coll_irregular_complex_chunk_write(const void *params);
void io_mode_confusion(const void *params);
void rr_obj_hdr_flush_confusion(const void *params);
void chunk_align_bug_1(const void *params);
void lower_dim_size_comp_test(const void *params);
void link_chunk_collective_io_test(const void *params);
void contig_hyperslab_dr_pio_test(ShapeSameTestMethods sstest_type);
void checker_board_hyperslab_dr_pio_test(ShapeSameTestMethods sstest_type);
void file_image_daisy_chain_test(const void *params);
#ifdef H5_HAVE_FILTER_DEFLATE
void compress_readAll(const void *params);
#endif /* H5_HAVE_FILTER_DEFLATE */
void test_dense_attr(const void *params);
void test_partial_no_selection_coll_md_read(const void *params);
void test_multi_chunk_io_addrmap_issue(const void *params);
void test_link_chunk_io_sort_chunk_issue(const void *params);
void test_collective_global_heap_write(const void *params);
void test_coll_io_ind_md_write(const void *params);
void test_oflush(const void *params);

/* commonly used prototypes */
MPI_Offset h5_mpi_get_file_size(const char *filename, MPI_Comm comm, MPI_Info info);
int dataset_vrfy(hsize_t start[], hsize_t count[], hsize_t stride[], hsize_t block[], DATATYPE *dataset,
                 DATATYPE *original);
#endif /* PHDF5TEST_H */
