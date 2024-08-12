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

/*
 * Purpose:    Tests sparse storage based on the concept of structured chunk
 */
#define H5D_FRIEND /*suppress error about including H5Dpkg      */
#define H5D_TESTING
#define H5FD_FRIEND /*suppress error about including H5FDpkg      */
#define H5FD_TESTING

#define H5Z_FRIEND /*suppress error about including H5Zpkg      */

#include "testhdf5.h"
#include "H5srcdir.h"

#include "H5CXprivate.h" /* API Contexts                         */
#include "H5Iprivate.h"
#include "H5Pprivate.h"

#define H5F_FRIEND /*suppress error about including H5Fpkg */
#define H5F_TESTING
#include "H5Fpkg.h" /* File access                          */

#define H5S_FRIEND  /*suppress error about including H5Spkg */
#include "H5Spkg.h" /* Dataspace                            */

#define H5T_FRIEND  /*suppress error about including H5Tpkg */
#include "H5Tpkg.h" /* Datatype                             */

#define H5A_FRIEND  /*suppress error about including H5Apkg     */
#include "H5Apkg.h" /* Attributes                   */

/* Use in version bound test */
#define H5O_FRIEND  /*suppress error about including H5Opkg */
#include "H5Opkg.h" /* Object headers                       */

#include "H5Dpkg.h"
#include "H5FDpkg.h"
#include "H5VMprivate.h"
#include "H5Zpkg.h"

static const char *FILENAME[] = {"sparse",                    /* 0 */
                                 "sparse_data",               /* 1 */
                                 "sparse_direct_chunk",       /* 2 */
                                 "sparse_query_direct_chunk", /* 3 */
                                 "sparse_filter",             /* 4 */
                                 "sparse_dense_api",          /* 5 */
                                 NULL};
#define FILENAME_BUF_SIZE 1024

#define SPARSE_DSET "sparse_dset"

#define RANK     2
#define NX       10
#define NY       10
#define CHUNK_NX 5
#define CHUNK_NY 5

/* Size of a chunk */
#define CHK_SIZE (CHUNK_NX * CHUNK_NY * sizeof(int))

/*-------------------------------------------------------------------------
 * Function:    test_sparse_data
 *
 * Purpose:     Verify APIs for handling sparse data:
 *              --H5Dget_defined()
 *              --H5Derase()
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_sparse_data(hid_t fapl)
{
    char    filename[FILENAME_BUF_SIZE]; /* File name */
    hid_t   fid          = H5I_INVALID_HID;
    hid_t   sid          = H5I_INVALID_HID;
    hid_t   sid1         = H5I_INVALID_HID;
    hid_t   sid2         = H5I_INVALID_HID;
    hid_t   dcpl         = H5I_INVALID_HID;
    hid_t   did          = H5I_INVALID_HID;
    hsize_t dim[1]       = {50}; /* 1-d dataspace */
    hsize_t chunk_dim[1] = {5};  /* Chunk size */
    int     wbuf[50];            /* Write buffer */

    TESTING("APIs for handling sparse data");

    /* Create a file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(1, dim, NULL)) < 0)
        FAIL_STACK_ERROR;

    /* Create property list for compact dataset creation */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;

    /* TBD: need to set to H5D_SPARSE_CHUNK */
    if (H5Pset_layout(dcpl, H5D_STRUCT_CHUNK) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_struct_chunk(dcpl, 1, chunk_dim, H5D_SPARSE_CHUNK) < 0)
        FAIL_STACK_ERROR;

    if ((did = H5Dcreate2(fid, SPARSE_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Write sparse data to the dataset */
    memset(wbuf, 0, sizeof(wbuf));

    /* Initialize and write sparse data to the dataset */
    wbuf[1]  = 1;
    wbuf[12] = 12;
    wbuf[13] = 13;
    wbuf[14] = 14;
    wbuf[22] = 22;
    wbuf[23] = 23;
    wbuf[24] = 24;
    wbuf[48] = 48;
    wbuf[49] = 49;
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    /* Get defined elements */
    /* TBD: Verify that dataset with H5D_SPARSE_CHUNK layout will succeed; otherwise fail */
    if ((sid1 = H5Dget_defined(did, H5S_ALL, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* TBD: Verify defined elements in sid1 are as expected */

    /* Erase all defined elements */
    /* TBD: Verify that dataset with H5D_SPARSE_CHUNK layout will succeed; otherwise fail */
    if (H5Derase(did, sid1, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;

    /* Call H5Dget_defined() again after H5Derase() */
    if ((sid2 = H5Dget_defined(did, H5S_ALL, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* TBD: Verify nothing is defined in sid2 */

    if (H5Sclose(sid1) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid2) < 0)
        FAIL_STACK_ERROR;

    /* Closing */
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;

    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;

    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;

    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Sclose(sid1);
        H5Sclose(sid2);
        H5Pclose(dcpl);
        H5Dclose(did);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return FAIL;
} /* end test_sparse_data() */

/*-------------------------------------------------------------------------
 * Function:    test_sparse_direct_chunk
 *
 * Purpose:     Verify APIs for direct chunk I/O on structured chunk:
 *                  --H5Dwrite_struct_chunk()
 *                  --H5Dread_struct_chunk()
 *
 * Return:      # of errors
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_sparse_direct_chunk(hid_t fapl)
{
    char  filename[FILENAME_BUF_SIZE]; /* File name */
    hid_t fid  = H5I_INVALID_HID;
    hid_t did  = H5I_INVALID_HID;
    hid_t sid  = H5I_INVALID_HID;
    hid_t dcpl = H5I_INVALID_HID;

    hsize_t dims[2]       = {NX, NY};
    hsize_t maxdims[2]    = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t chunk_dims[2] = {CHUNK_NX, CHUNK_NY};

    int     buf[NX][NY];
    size_t  encode_size;
    hsize_t start[2], block[2], count[2];

    hsize_t                 wr_offset[2] = {0, 0};
    H5D_struct_chunk_info_t wr_chk_info;
    uint16_t                wr_filter_mask[2] = {0, 0};
    size_t                  wr_section_size[2];
    void                   *wr_buf[2];
    unsigned char          *wr_buf0;
    int                    *wr_buf1;

    hsize_t                 rd_offset[2] = {5, 5};
    H5D_struct_chunk_info_t rd_chk_info;
    uint16_t                rd_filter_mask[2] = {0, 0};
    size_t                  rd_section_size[2];
    void                   *rd_buf[2];
    unsigned char          *rd_buf0;
    int                    *rd_buf1;

    TESTING("APIs for direct chunk I/O on structured chunks");

    /* Create a file */
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /*
     * Create the data space with unlimited dimensions.
     */
    if ((sid = H5Screate_simple(RANK, dims, maxdims)) < 0)
        FAIL_STACK_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;

    /* TBD: need to set to H5D_SPARSE_CHUNK */
    if (H5Pset_layout(dcpl, H5D_CHUNKED) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_chunk(dcpl, RANK, chunk_dims) < 0)
        FAIL_STACK_ERROR;

    /*
     * Create a new dataset within the file using dcpl
     */
    if ((did = H5Dcreate2(fid, SPARSE_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    start[0] = 3;
    start[1] = 2;
    block[0] = 2;
    block[1] = 3;
    count[0] = count[1] = 1;
    /* Select the 2x3 block in chunk index 0 for writing */
    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL, count, block) < 0)
        FAIL_STACK_ERROR;

    /* Get the encoded size for the selection */
    if (H5Sencode2(sid, NULL, &encode_size, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;

    /* Set up section size for section 0 and section 1 */
    wr_section_size[0] = encode_size;
    wr_section_size[1] = block[0] * block[1] * sizeof(int);

    /* Allocate buffers for section 0 (encoded selection) and section 1 (data) */
    if ((wr_buf0 = (unsigned char *)calloc((size_t)1, encode_size)) == NULL)
        FAIL_STACK_ERROR;
    if ((wr_buf1 = (int *)calloc((size_t)1, wr_section_size[1])) == NULL)
        FAIL_STACK_ERROR;

    /* Encode selection into the buffer for section 0 */
    if (H5Sencode2(sid, wr_buf0, &encode_size, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;

    /* Set up data into the bufer for section 1 */
    wr_buf1[0] = 32;
    wr_buf1[1] = 33;
    wr_buf1[2] = 34;
    wr_buf1[3] = 42;
    wr_buf1[4] = 43;
    wr_buf1[5] = 44;

    /* Set up the buffer for H5D_write_struct_chunk() */
    wr_buf[0] = wr_buf0;
    wr_buf[1] = wr_buf1;

    wr_chk_info.type              = 4; /* should be H5D_SPARSE_CHUNK */
    wr_chk_info.num_sections      = 2;
    wr_chk_info.filter_mask       = wr_filter_mask;
    wr_chk_info.section_size      = wr_section_size;
    wr_chk_info.section_orig_size = wr_section_size;

    /* Write the structured chunk at offset [0,0]: chunk index 0 */
    if (H5Dwrite_struct_chunk(did, H5P_DEFAULT, wr_offset, &wr_chk_info, wr_buf) < 0)
        FAIL_STACK_ERROR;

    /* Read the whole dataset */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        FAIL_STACK_ERROR;
    /* TBD: Verify buf read has data as in wr_buf1[] at location wr_buf0[] */

    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;

    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;

    if ((did = H5Dopen2(fid, SPARSE_DSET, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;

    if ((sid = H5Dget_space(did)) == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    /* Select the 2x1 block in chunk index 3 for reading */
    start[0] = 5;
    start[1] = 5;
    block[0] = 2;
    block[1] = 1;
    count[0] = count[1] = 1;
    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL, count, block) < 0)
        FAIL_STACK_ERROR;

    if (H5Sencode2(sid, NULL, &encode_size, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;

    rd_section_size[0] = encode_size;
    rd_section_size[1] = block[0] * block[1] * sizeof(int);

    /* Allocate buffers for section 0 (encoded selection) and section 1 (data) */
    if ((rd_buf0 = (unsigned char *)calloc((size_t)1, encode_size)) == NULL)
        FAIL_STACK_ERROR;
    if ((rd_buf1 = (int *)calloc((size_t)1, rd_section_size[1])) == NULL)
        FAIL_STACK_ERROR;

    rd_buf[0] = rd_buf0;
    rd_buf[1] = rd_buf1;

    rd_chk_info.type              = 4; /* should be H5D_SPARSE_CHUNK */
    rd_chk_info.num_sections      = 2;
    rd_chk_info.filter_mask       = rd_filter_mask;
    rd_chk_info.section_size      = rd_section_size;
    rd_chk_info.section_orig_size = rd_section_size;

    /* Read the structured chunk at offset [5,5] */
    if (H5Dread_struct_chunk(did, H5P_DEFAULT, rd_offset, &rd_chk_info, rd_buf) < 0)
        FAIL_STACK_ERROR;
    /* Verify rd_chk_info and rd_buf are the same as wr_chk_info and wr_buf */

    /*
     * Close/release resources.
     */
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    H5_FAILED();
    return 1;

} /* test_sparse_direct_chunk() */

/*-------------------------------------------------------------------------
 * Function:    verify_get_struct_chunk_info (helper function)
 *
 * Purpose:     Verifies that H5Dget_struct_chunk_info returns correct
 *              values for a chunk.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
verify_get_struct_chunk_info(hid_t did, hid_t sid, hsize_t chk_index,
                             const hsize_t H5_ATTR_UNUSED          *exp_offset,
                             H5D_struct_chunk_info_t H5_ATTR_UNUSED exp_chunk_info[],
                             hsize_t H5_ATTR_UNUSED                 exp_chk_size)
{
    hsize_t                 out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t                 out_chk_size  = 0;      /* Size of an allocated/written chunk */
    haddr_t                 out_addr      = 0;      /* Address of an allocated/written chunk */
    H5D_struct_chunk_info_t out_chunk_info[50];

    /* Get info of the chunk specified by chk_index */
    if (H5Dget_struct_chunk_info(did, sid, chk_index, out_offset, out_chunk_info, &out_addr, &out_chk_size) <
        0)
        TEST_ERROR;

#ifdef TBD

    /* Verify info from H5Dget_struct_chunk_info() with expected chunk info */

    if (out_offset[0] != exp_offset[0])
        FAIL_PUTS_ERROR("unexpected offset[0]");
    if (out_offset[1] != exp_offset[1])
        FAIL_PUTS_ERROR("unexpected offset[1]");

    Compare out_chunk_info with exp_chunk_info

        if (HADDR_UNDEF == out_addr) FAIL_PUTS_ERROR("address cannot be HADDR_UNDEF");

    if (out_chk_size != exp_chk_size)
        FAIL_PUTS_ERROR("unexpected chunk size");

#endif

    /* For now, just return SUCCEED */

    return SUCCEED;

error:
    return FAIL;
} /* verify_get_struct_chunk_info() */

/*-------------------------------------------------------------------------
 *
 * Function:    verify_get_struct_chunk_info_by_coord (helper function)
 *
 * Purpose:     Verifies that H5Dget_struct_chunk_info_by_coord returns correct
 *              values for a chunk.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
verify_get_struct_chunk_info_by_coord(hid_t did, hsize_t *offset,
                                      H5D_struct_chunk_info_t H5_ATTR_UNUSED exp_chunk_info[],
                                      hsize_t H5_ATTR_UNUSED                 exp_chk_size)
{
    hsize_t                 out_chk_size = 0; /* Size of an allocated/written chunk */
    haddr_t                 out_addr     = 0; /* Address of an allocated/written chunk */
    H5D_struct_chunk_info_t out_chunk_info[50];

    /* Get info of the chunk at logical coordinates specified by offset */
    if (H5Dget_struct_chunk_info_by_coord(did, offset, out_chunk_info, &out_addr, &out_chk_size) < 0)
        TEST_ERROR;

#ifdef TBD
    {
        /* Verify info from H5Dget_struct_chunk_info_by_coord() with expected chunk info */

        if (HADDR_UNDEF == out_addr)
            FAIL_PUTS_ERROR("address cannot be HADDR_UNDEF");

        Compare out_chunk_info with exp_chunk_info

            if (out_chk_size != exp_chk_size) FAIL_PUTS_ERROR("unexpected chunk size");
    }
#endif

    /* For now, just return SUCCEED */

    return SUCCEED;

error:
    return FAIL;
} /* verify_get_struct_chunk_info_by_coord() */

typedef struct struct_chunk_iter_info_t {
    hsize_t                  offset[2];
    H5D_struct_chunk_info_t *chunk_info;
    haddr_t                  addr;
    hsize_t                  chunk_size;
} struct_chunk_iter_info_t;

typedef struct struct_chunk_iter_udata_t {
    struct_chunk_iter_info_t *struct_chunk_info;
    int                       last_index;
} struct_chunk_iter_udata_t;

static int
iter_cb_struct(const hsize_t *offset, H5D_struct_chunk_info_t *chunk_info, haddr_t *addr, hsize_t *chunk_size,
               void *op_data)
{
    struct_chunk_iter_udata_t *cidata = (struct_chunk_iter_udata_t *)op_data;
    int                        idx    = cidata->last_index + 1;

    cidata->struct_chunk_info[idx].offset[0]  = offset[0];
    cidata->struct_chunk_info[idx].offset[1]  = offset[1];
    cidata->struct_chunk_info[idx].chunk_info = chunk_info;
    cidata->struct_chunk_info[idx].addr       = *addr;
    cidata->struct_chunk_info[idx].chunk_size = *chunk_size;

    cidata->last_index++;

    return H5_ITER_CONT;
} /* iter_cb_struct() */

/*-------------------------------------------------------------------------
 * Function:    test_sparse_direct_chunk_query
 *
 * Purpose:     Verify APIs for direct chunk I/O query on structured chunk:
 *                  --H5Dget_struct_chunk_info()
 *                  --H5Dget_struct_chunk_info_by_coord()
 *                  --H5Dstruct_chunk_iter()
 *
 * Return:      # of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_sparse_direct_chunk_query(hid_t fapl)
{
    char    filename[FILENAME_BUF_SIZE];          /* File name */
    hid_t   fid           = H5I_INVALID_HID;      /* File ID */
    hid_t   sid           = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t   did           = H5I_INVALID_HID;      /* Dataset ID */
    hid_t   dcpl          = H5I_INVALID_HID;      /* Creation plist */
    hsize_t dims[2]       = {NX, NY};             /* Dataset dimensions */
    hsize_t chunk_dims[2] = {CHUNK_NX, CHUNK_NY}; /* Chunk dimensions */

    struct_chunk_iter_info_t  chunk_infos[2]; /* Chunk infos filled up by iterator */
    struct_chunk_iter_udata_t udata;          /* udata for iteration */
    H5D_struct_chunk_info_t   chk_info;

    uint16_t filter_mask[2]  = {0, 0};
    hsize_t  offset[2]       = {0, 0};
    size_t   section_size[2] = {32, 48};
    void    *write_buf[2];
    hsize_t  in0[4] = {3, 2, 4, 4};              /* Encoded coordinates: [3,2] - [4,4] */
    hsize_t  in1[6] = {66, 69, 72, 96, 99, 102}; /* Data: 66,69,72,96,99,102 */

    TESTING("APIs for direct chunk I/O query on structured chunk");

    /* Create the file */
    h5_fixname(FILENAME[3], fapl, filename, sizeof filename);

    /* Create a new file. */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR;

    /* Enable chunking */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* TBD: need to set to H5D_SPARSE_CHUNK */
    if (H5Pset_layout(dcpl, H5D_CHUNKED) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_chunk(dcpl, RANK, chunk_dims) < 0)
        TEST_ERROR;

    /* Create a new dataset using dcpl creation properties */
    did = H5Dcreate2(fid, SPARSE_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (did < 0)
        TEST_ERROR;

    write_buf[0] = in0;
    write_buf[1] = in1;

    chk_info.type              = 4; /* should be H5D_SPARSE_CHUNK */
    chk_info.num_sections      = 2;
    chk_info.filter_mask       = filter_mask;
    chk_info.section_size      = section_size;
    chk_info.section_orig_size = section_size;

    /* Write the structured chunk at offset */
    if (H5Dwrite_struct_chunk(did, H5P_DEFAULT, offset, &chk_info, write_buf) < 0)
        FAIL_STACK_ERROR;

    /* Verify info of the first and only chunk via H5Dget_struct_chunk_info() */
    if (verify_get_struct_chunk_info(did, H5S_ALL, 0, offset, &chk_info, CHK_SIZE) == FAIL)
        FAIL_PUTS_ERROR("Verification H5Dget_struct_chunk_info failed\n");

    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY;

    /* Write the structured chunk at offset */
    if (H5Dwrite_struct_chunk(did, H5P_DEFAULT, offset, &chk_info, write_buf) < 0)
        FAIL_STACK_ERROR;

    /* Verify info of the chunk at offset [CHUNK_NX,CHUNK_NY] via H5Dget_struct_chunk_info_by_coord() */
    if (verify_get_struct_chunk_info_by_coord(did, offset, &chk_info, CHK_SIZE) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_struct_chunk_info_by_coord failed\n");

    /* For now, H5Dstruct_chunk_iter() just returns SUCCEED without actual iteration */
    udata.struct_chunk_info = chunk_infos;
    udata.last_index        = -1;
    if (H5Dstruct_chunk_iter(did, H5P_DEFAULT, &iter_cb_struct, &udata) < 0)
        TEST_ERROR;

    /* Release resource */
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /* Remove the test file */
    HDremove(filename);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return 1;
} /* test_sparse_direct_chunk_query() */

/*-------------------------------------------------------------------------
 *
 * Function:    test_sparse_filter()
 *
 * Purpose: Verify the following APIs for structured chunk filtering:
 *              --H5Pset_filter2()
 *              --H5Pget_nfilters2()
 *              --H5Pget_filters3()
 *              --H5Pget_filter_by_id3()
 *              --H5Premove_filter2()
 *              --H5Pmodify_filter2()
 *
 * Return:      # of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_sparse_filter(hid_t fapl)
{
    char         filename[FILENAME_BUF_SIZE];          /* File name */
    hid_t        fid           = H5I_INVALID_HID;      /* File ID */
    hid_t        sid           = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t        did           = H5I_INVALID_HID;      /* Dataset ID */
    hid_t        dcpl          = H5I_INVALID_HID;      /* Creation plist */
    hsize_t      dims[2]       = {NX, NY};             /* Dataset dimensions */
    hsize_t      chunk_dims[2] = {CHUNK_NX, CHUNK_NY}; /* Chunk dimensions */
    int          nfilters;
    H5Z_filter_t filtn;
    uint64_t     flags;

    TESTING("APIs for structured chunk filtering");

    /* Create the file */
    h5_fixname(FILENAME[4], fapl, filename, sizeof filename);

    /* Create a new file. */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR;

    /* Enable chunking */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* TBD: need to set to H5D_SPARSE_CHUNK */
    if (H5Pset_layout(dcpl, H5D_CHUNKED) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_chunk(dcpl, RANK, chunk_dims) < 0)
        TEST_ERROR;

    if (H5Pset_filter2(dcpl, H5Z_FLAG_SPARSE_SELECTION, H5Z_FILTER_DEFLATE, H5Z_FLAG_MANDATORY, 0, NULL) < 0)
        TEST_ERROR;

    /* Create a new dataset using dcpl creation properties */
    did = H5Dcreate2(fid, SPARSE_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);

    /* TBD: verify nfilters is correct; for now it is 0 */
    nfilters = H5Pget_nfilters2(dcpl, H5Z_FLAG_SPARSE_SELECTION);
    if (nfilters)
        TEST_ERROR;

    if (did < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if ((did = H5Dopen2(fid, SPARSE_DSET, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    if ((dcpl = H5Dget_create_plist(did)) < 0)
        FAIL_STACK_ERROR;

    /* TBD: verify that filtn is H5Z_FILTER_DEFLATE and flags is H5Z_FLAG_MANDATORY */
    filtn = H5Pget_filter3(dcpl, H5Z_FLAG_SPARSE_SELECTION, 0, &flags, NULL, NULL, (size_t)0, NULL, NULL);

    /* For now: just verify it's 0 */
    if (filtn != H5Z_FILTER_NONE)
        TEST_ERROR;

    /* TBD: Modify the filter's flags to optional */
    if (H5Pmodify_filter2(dcpl, H5Z_FLAG_SPARSE_SELECTION, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, 0, NULL) <
        0)
        FAIL_STACK_ERROR;

    /* TBD: verify that flags is H5Z_FLAG_OPTIONAL */
    if (H5Pget_filter_by_id3(dcpl, H5Z_FLAG_SPARSE_SELECTION, H5Z_FILTER_DEFLATE, &flags, NULL, NULL, 0, NULL,
                             NULL) < 0)
        FAIL_STACK_ERROR;

    /* TBD: Remove the filter */
    if (H5Premove_filter2(dcpl, H5Z_FLAG_SPARSE_SELECTION, H5Z_FILTER_DEFLATE) < 0)
        FAIL_STACK_ERROR;

    /* TBD: verify that filtn is H5Z_FILTER_NONE */
    filtn = H5Pget_filter3(dcpl, H5Z_FLAG_SPARSE_SELECTION, 0, NULL, NULL, NULL, (size_t)0, NULL, NULL);

    /* For now: just verify it's 0 */
    if (filtn != H5Z_FILTER_NONE)
        TEST_ERROR;

    /* Release resource */
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /* Remove the test file */
    HDremove(filename);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return 1;
} /* test_sparse_filter() */

typedef struct chunk_iter_info_t {
    hsize_t  offset[2];
    unsigned filter_mask;
    haddr_t  addr;
    hsize_t  size;
} chunk_iter_info_t;

typedef struct chunk_iter_udata_t {
    chunk_iter_info_t *chunk_info;
    int                last_index;
} chunk_iter_udata_t;

static int
iter_cb(const hsize_t *offset, unsigned filter_mask, haddr_t addr, hsize_t size, void *op_data)
{
    chunk_iter_udata_t *cidata = (chunk_iter_udata_t *)op_data;
    int                 idx    = cidata->last_index + 1;

    cidata->chunk_info[idx].offset[0]   = offset[0];
    cidata->chunk_info[idx].offset[1]   = offset[1];
    cidata->chunk_info[idx].filter_mask = filter_mask;
    cidata->chunk_info[idx].addr        = addr;
    cidata->chunk_info[idx].size        = size;

    cidata->last_index++;

    return H5_ITER_CONT;
} /* iter_cb() */

/*-------------------------------------------------------------------------
 * Function:    test_dense_chunk_api_on_sparse()
 *
 * Purpose: Verify the following dense chunk APIs will fail for
 *          H5D_SPARSE_CHUNK layout:
 *             --H5Dwrite_chunk()
 *             --H5Dget_chunk_info()
 *             --H5Dget_chunk_info_by_coord()
 *             --H5Dchunk_iter()
 *          Verify the following dense chunk APIs will succeed for
 *          H5D_SPARSE_CHUNK layout:
 *              --H5Dread_chunk()
 *              --H5Dget_chunk_storage_size()
 *              --H5Dget_num_chunks()
 *
 * Return:      # of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_dense_chunk_api_on_sparse(hid_t fapl)
{
    char               filename[FILENAME_BUF_SIZE];          /* File name */
    hid_t              fid           = H5I_INVALID_HID;      /* File ID */
    hid_t              sid           = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t              did           = H5I_INVALID_HID;      /* Dataset ID */
    hid_t              dcpl          = H5I_INVALID_HID;      /* Creation plist */
    hsize_t            dims[2]       = {NX, NY};             /* Dataset dimensions */
    hsize_t            chunk_dims[2] = {CHUNK_NX, CHUNK_NY}; /* Chunk dimensions */
    chunk_iter_info_t  chunk_infos[2];
    chunk_iter_udata_t udata;
    hsize_t            nchunks = 0;
    hsize_t            chunk_nbytes;
    hsize_t            offset[2] = {0, 0};
    int                direct_buf[CHUNK_NX][CHUNK_NY];
    haddr_t            addr    = 0;
    uint32_t           filters = 0;

    TESTING("APIs for direct chunk I/O: dense chunk functions on sparse layout");

    /* Create the file */
    h5_fixname(FILENAME[3], fapl, filename, sizeof filename);

    /* Create a new file. */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR;

    /* Enable chunking */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* TBD: need to set to H5D_SPARSE_CHUNK */
    if (H5Pset_layout(dcpl, H5D_CHUNKED) < 0)
        FAIL_STACK_ERROR;

    /* The layout is set to H5D_CHUNKED as a side-effect */
    if (H5Pset_chunk(dcpl, RANK, chunk_dims) < 0)
        TEST_ERROR;

    /* Create a new dataset using dcpl creation properties */
    did = H5Dcreate2(fid, SPARSE_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (did < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY
    {
        H5Dwrite_chunk(did, H5P_DEFAULT, 0, offset, CHK_SIZE, direct_buf);
    }
    H5E_END_TRY
    /* TBD: set return status and verify that it should fail */

    H5E_BEGIN_TRY
    {
        H5Dget_chunk_info(did, H5S_ALL, 0, NULL, NULL, &addr, NULL);
    }
    H5E_END_TRY
    /* TBD: set return status and verify that it should fail */

    H5E_BEGIN_TRY
    {
        H5Dget_chunk_info_by_coord(did, offset, NULL, &addr, NULL);
    }
    H5E_END_TRY
    /* TBD: set return status and verify that it should fail */

    H5E_BEGIN_TRY
    {
        udata.chunk_info = chunk_infos;
        udata.last_index = -1;
        H5Dchunk_iter(did, H5P_DEFAULT, &iter_cb, &udata);
    }
    H5E_END_TRY
    /* TBD: set return status and verify that it should fail */

    H5Dread_chunk(did, H5P_DEFAULT, offset, &filters, direct_buf);
    /* TBD: should succeed */

    H5Dget_num_chunks(did, sid, &nchunks);
    /* TBD: should succeed */

    H5Dget_chunk_storage_size(did, offset, &chunk_nbytes);
    /* TBD: should succeed */

    /* Release resource */
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /* Remove the test file */
    HDremove(filename);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return 1;
} /* test_dense_chunk_api_on_sparse() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests for sparse data
 *              Tests are copied and modified from:
 *               --test/dsets.c
 *               --test/direct_chunk.c
 *               --test/chunk_info.c
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       file, grp, fapl, fapl2;
    hid_t       fcpl = H5I_INVALID_HID, fcpl2 = H5I_INVALID_HID;
    unsigned    new_format;
    unsigned    paged;
    unsigned    minimized_ohdr;
    int         mdc_nelmts;
    size_t      rdcc_nelmts;
    size_t      rdcc_nbytes;
    double      rdcc_w0;
    int         nerrors = 0;
    const char *driver_name;
    bool        contig_addr_vfd; /* Whether VFD used has a contiguous address space */
    bool        driver_is_default_compatible;

    /* Don't run this test using certain file drivers */
    driver_name = h5_get_test_driver_name();

    /* Current VFD that does not support contiguous address space */
    contig_addr_vfd = (bool)(strcmp(driver_name, "split") != 0 && strcmp(driver_name, "multi") != 0);

    /* Set the random # seed */
    srand((unsigned)time(NULL));

    /* Testing setup */
    h5_test_init();
    fapl = h5_fileaccess();

    if (h5_driver_is_default_vfd_compatible(fapl, &driver_is_default_compatible) < 0)
        TEST_ERROR;

    /* Turn off the chunk cache, so all the chunks are immediately written to disk */
    if (H5Pget_cache(fapl, &mdc_nelmts, &rdcc_nelmts, &rdcc_nbytes, &rdcc_w0) < 0)
        goto error;
    rdcc_nbytes = 0;
    if (H5Pset_cache(fapl, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0) < 0)
        goto error;

    /* Copy the file access property list */
    if ((fapl2 = H5Pcopy(fapl)) < 0)
        TEST_ERROR;

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    if (H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    /* create a file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        TEST_ERROR;
    if ((fcpl2 = H5Pcopy(fcpl)) < 0)
        TEST_ERROR;

    /* Set file space strategy to paged aggregation and persisting free-space */
    if (H5Pset_file_space_strategy(fcpl2, H5F_FSPACE_STRATEGY_PAGE, true, (hsize_t)1) < 0)
        TEST_ERROR;

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Test with paged aggregation enabled or not */
    for (paged = false; paged <= true; paged++) {

        /* Temporary: skip testing for multi/split drivers:
             fail file create when persisting free-space or using paged aggregation strategy */
        if (!contig_addr_vfd && paged)
            continue;

        for (minimized_ohdr = false; minimized_ohdr <= true; minimized_ohdr++) {

            /* Test with old & new format groups */
            /* Use new_format for NOW */
            for (new_format = true; new_format <= true; new_format++) {
                hid_t my_fapl, my_fcpl;

                /* Set the FAPL for the type of format */
                if (new_format) {
                    my_fapl = fapl2;
                    if (paged) {
                        my_fcpl = fcpl2;
                        puts("\nTesting with new file format and paged aggregation");
                    }
                    else {
                        my_fcpl = fcpl;
                        puts("\nTesting with new file format and non-paged aggregation");
                    }
                } /* end if */
                else {
                    my_fapl = fapl;
                    if (paged) {
                        my_fcpl = fcpl2;
                        puts("Testing with old file format and paged aggregation:");
                    }
                    else {
                        my_fcpl = fcpl;
                        puts("Testing with old file format and non-paged aggregation:");
                    }
                } /* end else */

                /* Create the file for this test */
                if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, my_fcpl, my_fapl)) < 0)
                    goto error;

                if (true == minimized_ohdr) {
                    if (0 > H5Fset_dset_no_attrs_hint(file, true))
                        goto error;
                    puts("(minimized dataset object headers with file setting)");
                }

                /* Cause the library to emit initial messages */
                if ((grp = H5Gcreate2(file, "emit diagnostics", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                    goto error;
                if (H5Oset_comment(grp, "Causes diagnostic messages to be emitted") < 0)
                    goto error;
                if (H5Gclose(grp) < 0)
                    goto error;

                /* Create its own testfile */
                nerrors += (test_sparse_data(my_fapl) < 0 ? 1 : 0);
                nerrors += (test_sparse_direct_chunk(my_fapl) < 0 ? 1 : 0);
                nerrors += (test_sparse_direct_chunk_query(my_fapl) < 0 ? 1 : 0);
                nerrors += (test_sparse_filter(my_fapl) < 0 ? 1 : 0);
                nerrors += (test_dense_chunk_api_on_sparse(my_fapl) < 0 ? 1 : 0);

                if (H5Fclose(file) < 0)
                    goto error;
            } /* end for new_format */
        }     /* end for minimized_ohdr */
    }         /* end for paged */

    /* Close property lists */
    if (H5Pclose(fapl2) < 0)
        TEST_ERROR;
    if (H5Pclose(fcpl) < 0)
        TEST_ERROR;
    if (H5Pclose(fcpl2) < 0)
        TEST_ERROR;

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if (nerrors)
        goto error;
    printf("All sparse storage tests passed.\n");

    h5_cleanup(FILENAME, fapl);

    exit(EXIT_SUCCESS);

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d SPARSE STORAGE TEST%s FAILED! *****\n", nerrors, 1 == nerrors ? "" : "S");
    exit(EXIT_FAILURE);
} /* end main() */
