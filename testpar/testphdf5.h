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

/* common definitions used by all parallel hdf5 test programs. */

#ifndef PHDF5TEST_H
#define PHDF5TEST_H

#include "testpar.h"

enum H5TEST_COLL_CHUNK_API {
    API_NONE = 0,
    API_LINK_HARD,
    API_MULTI_HARD,
    API_LINK_TRUE,
    API_LINK_FALSE,
    API_MULTI_COLL,
    API_MULTI_IND
};

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
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

/* point selection order */
#define IN_ORDER     1
#define OUT_OF_ORDER 2

/* Hyperslab layout styles */
#define BYROW 1 /* divide into slabs of rows */
#define BYCOL 2 /* divide into blocks of columns */
#define ZROW  3 /* same as BYCOL except process 0 gets 0 rows */
#define ZCOL  4 /* same as BYCOL except process 0 gets 0 columns */

/* File_Access_type bits */
#define FACC_DEFAULT 0x0 /* default */
#define FACC_MPIO    0x1 /* MPIO */
#define FACC_SPLIT   0x2 /* Split File */

#define DXFER_COLLECTIVE_IO  0x1 /* Collective IO*/
#define DXFER_INDEPENDENT_IO 0x2 /* Independent IO collectively */
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
#define LINK_TRUE_CHUNK_NAME  "h5_link_chunk_true"
#define LINK_FALSE_CHUNK_NAME "h5_link_chunk_false"
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
#define TEST_FILTERS                                    0x080
/* TEST_FILTERS will take place of this after supporting mpio + filter for
 * H5Dcreate and H5Dwrite */
#define TEST_FILTERS_READ 0x100

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

/* Shape Same Tests Definitions */
typedef enum {
    IND_CONTIG,  /* Independent IO on contigous datasets */
    COL_CONTIG,  /* Collective IO on contigous datasets */
    IND_CHUNKED, /* Independent IO on chunked datasets */
    COL_CHUNKED  /* Collective IO on chunked datasets */
} ShapeSameTestMethods;

/* Shared global variables */
extern int dim0, dim1;           /*Dataset dimensions */
extern int chunkdim0, chunkdim1; /*Chunk dimensions */
extern int nerrors;              /*errors count */
extern int facc_type;            /*Test file access type */
extern int dxfer_coll_type;

/* Test program prototypes */
void test_plist_ed(void);
void external_links(void);
void zero_dim_dset(void);
void test_file_properties(void);
void test_delete(void);
void multiple_dset_write(void);
void multiple_group_write(void);
void multiple_group_read(void);
void collective_group_write_independent_group_read(void);
void collective_group_write(void);
void independent_group_read(void);
void test_fapl_mpio_dup(void);
void test_split_comm_access(void);
void test_page_buffer_access(void);
void dataset_atomicity(void);
void dataset_writeInd(void);
void dataset_writeAll(void);
void extend_writeInd(void);
void extend_writeInd2(void);
void extend_writeAll(void);
void dataset_readInd(void);
void dataset_readAll(void);
void extend_readInd(void);
void extend_readAll(void);
void none_selection_chunk(void);
void actual_io_mode_tests(void);
void no_collective_cause_tests(void);
void test_chunk_alloc(void);
void test_filter_read(void);
void compact_dataset(void);
void null_dataset(void);
void big_dataset(void);
void dataset_fillvalue(void);
void coll_chunk1(void);
void coll_chunk2(void);
void coll_chunk3(void);
void coll_chunk4(void);
void coll_chunk5(void);
void coll_chunk6(void);
void coll_chunk7(void);
void coll_chunk8(void);
void coll_chunk9(void);
void coll_chunk10(void);
void coll_irregular_cont_read(void);
void coll_irregular_cont_write(void);
void coll_irregular_simple_chunk_read(void);
void coll_irregular_simple_chunk_write(void);
void coll_irregular_complex_chunk_read(void);
void coll_irregular_complex_chunk_write(void);
void io_mode_confusion(void);
void rr_obj_hdr_flush_confusion(void);
void rr_obj_hdr_flush_confusion_reader(MPI_Comm comm);
void rr_obj_hdr_flush_confusion_writer(MPI_Comm comm);
void chunk_align_bug_1(void);
void lower_dim_size_comp_test(void);
void link_chunk_collective_io_test(void);
void contig_hyperslab_dr_pio_test(ShapeSameTestMethods sstest_type);
void checker_board_hyperslab_dr_pio_test(ShapeSameTestMethods sstest_type);
void file_image_daisy_chain_test(void);
#ifdef H5_HAVE_FILTER_DEFLATE
void compress_readAll(void);
#endif /* H5_HAVE_FILTER_DEFLATE */
void test_dense_attr(void);
void test_partial_no_selection_coll_md_read(void);
void test_multi_chunk_io_addrmap_issue(void);
void test_link_chunk_io_sort_chunk_issue(void);

/* commonly used prototypes */
hid_t      create_faccess_plist(MPI_Comm comm, MPI_Info info, int l_facc_type);
MPI_Offset h5_mpi_get_file_size(const char *filename, MPI_Comm comm, MPI_Info info);
int  dataset_vrfy(hsize_t start[], hsize_t count[], hsize_t stride[], hsize_t block[], DATATYPE *dataset,
                  DATATYPE *original);
void point_set(hsize_t start[], hsize_t count[], hsize_t stride[], hsize_t block[], size_t num_points,
               hsize_t coords[], int order);
#endif /* PHDF5TEST_H */
