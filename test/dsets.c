/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:    Robb Matzke <matzke@llnl.gov>
 *        Tuesday, December  9, 1997
 *
 * Purpose:    Tests the dataset interface (H5D)
 */
#define H5D_FRIEND        /*suppress error about including H5Dpkg      */
#define H5D_TESTING
#define H5FD_FRIEND       /*suppress error about including H5FDpkg      */
#define H5FD_TESTING

#define H5Z_FRIEND        /*suppress error about including H5Zpkg      */

#include "testhdf5.h"
#include "H5srcdir.h"

#include "H5Bprivate.h"
#include "H5CXprivate.h"        /* API Contexts                         */
#include "H5Iprivate.h"
#include "H5Pprivate.h"

#define H5F_FRIEND      /*suppress error about including H5Fpkg */
#define H5F_TESTING
#include "H5Fpkg.h"     /* File access                          */

#define H5S_FRIEND      /*suppress error about including H5Spkg */
#include "H5Spkg.h"     /* Dataspace                            */

#define H5T_FRIEND      /*suppress error about including H5Tpkg */
#include "H5Tpkg.h"     /* Datatype                             */

#define H5A_FRIEND      /*suppress error about including H5Apkg     */
#include "H5Apkg.h"     /* Attributes                   */

/* Use in version bound test */
#define H5O_FRIEND      /*suppress error about including H5Opkg */
#include "H5Opkg.h"     /* Object headers                       */

#include "H5Dpkg.h"
#include "H5FDpkg.h"
#include "H5VMprivate.h"
#include "H5Zpkg.h"
#ifdef H5_HAVE_SZLIB_H
#   include "szlib.h"
#endif

const char *FILENAME[] = {
    "dataset",          /* 0 */
    "compact_dataset",  /* 1 */
    "dset_offset",      /* 2 */
    "max_compact_dataset",    /* 3 */
    "simple",           /* 4 */
    "set_local",        /* 5 */
    "random_chunks",    /* 6 */
    "huge_chunks",      /* 7 */
    "chunk_cache",      /* 8 */
    "big_chunk",        /* 9 */
    "chunk_fast",       /* 10 */
    "chunk_expand",     /* 11 */
    "chunk_fixed",      /* 12 */
    "copy_dcpl_newfile",/* 13 */
    "partial_chunks",   /* 14 */
    "layout_extend",    /* 15 */
    "zero_chunk",       /* 16 */
    "chunk_single",     /* 17 */
    "swmr_non_latest",  /* 18 */
    "earray_hdr_fd",    /* 19 */
    "farray_hdr_fd",    /* 20 */
    "bt2_hdr_fd",       /* 21 */
    "storage_size",     /* 22 */
    "dls_01_strings",   /* 23 */
    "power2up",         /* 24 */
    "version_bounds",   /* 25 */
    NULL
};

#define OHMIN_FILENAME_A "ohdr_min_a"

#define FILENAME_BUF_SIZE       1024
#define KB                      1024

#define FILE_DEFLATE_NAME       "deflate.h5"

/* Dataset names for testing filters */
#define DSET_DEFAULT_NAME           "default"
#define DSET_CHUNKED_NAME           "chunked"
#define DSET_COMPACT_NAME           "compact"
#define DSET_SIMPLE_IO_NAME         "simple_io"
#define DSET_USERBLOCK_IO_NAME      "userblock_io"
#define DSET_COMPACT_IO_NAME        "compact_io"
#define DSET_COMPACT_MAX_NAME       "max_compact"
#define DSET_COMPACT_MAX2_NAME      "max_compact_2"
#define DSET_CONV_BUF_NAME          "conv_buf"
#define DSET_TCONV_NAME             "tconv"
#define DSET_DEFLATE_NAME           "deflate"
#define DSET_SHUFFLE_NAME           "shuffle"
#define DSET_FLETCHER32_NAME        "fletcher32"
#define DSET_FLETCHER32_NAME_2      "fletcher32_2"
#define DSET_FLETCHER32_NAME_3      "fletcher32_3"
#define DSET_SHUF_DEF_FLET_NAME     "shuffle+deflate+fletcher32"
#define DSET_SHUF_DEF_FLET_NAME_2   "shuffle+deflate+fletcher32_2"
#ifdef H5_HAVE_FILTER_SZIP
#define DSET_SZIP_NAME              "szip"
#define DSET_SHUF_SZIP_FLET_NAME    "shuffle+szip+fletcher32"
#define DSET_SHUF_SZIP_FLET_NAME_2  "shuffle+szip+fletcher32_2"
#endif /* H5_HAVE_FILTER_SZIP */

#define DSET_BOGUS_NAME             "bogus"
#define DSET_MISSING_NAME           "missing"
#define DSET_CAN_APPLY_NAME         "can_apply"
#define DSET_CAN_APPLY_NAME2        "can_apply2"
#ifdef H5_HAVE_FILTER_SZIP
#define DSET_CAN_APPLY_SZIP_NAME    "can_apply_szip"
#endif /* H5_HAVE_FILTER_SZIP */
#define DSET_SET_LOCAL_NAME         "set_local"
#define DSET_SET_LOCAL_NAME_2       "set_local_2"
#define DSET_ONEBYTE_SHUF_NAME      "onebyte_shuffle"
#define DSET_NBIT_INT_NAME             "nbit_int"
#define DSET_NBIT_FLOAT_NAME           "nbit_float"
#define DSET_NBIT_DOUBLE_NAME          "nbit_double"
#define DSET_NBIT_ARRAY_NAME           "nbit_array"
#define DSET_NBIT_COMPOUND_NAME        "nbit_compound"
#define DSET_NBIT_COMPOUND_NAME_2      "nbit_compound_2"
#define DSET_NBIT_COMPOUND_NAME_3      "nbit_compound_3"
#define DSET_NBIT_INT_SIZE_NAME        "nbit_int_size"
#define DSET_NBIT_FLT_SIZE_NAME        "nbit_flt_size"
#define DSET_SCALEOFFSET_INT_NAME      "scaleoffset_int"
#define DSET_SCALEOFFSET_INT_NAME_2    "scaleoffset_int_2"
#define DSET_SCALEOFFSET_FLOAT_NAME    "scaleoffset_float"
#define DSET_SCALEOFFSET_FLOAT_NAME_2  "scaleoffset_float_2"
#define DSET_SCALEOFFSET_DOUBLE_NAME   "scaleoffset_double"
#define DSET_SCALEOFFSET_DOUBLE_NAME_2 "scaleoffset_double_2"
#define DSET_COMPARE_DCPL_NAME         "compare_dcpl"
#define DSET_COMPARE_DCPL_NAME_2       "compare_dcpl_2"
#define DSET_COPY_DCPL_NAME_1          "copy_dcpl_1"
#define DSET_COPY_DCPL_NAME_2          "copy_dcpl_2"
#define COPY_DCPL_EXTFILE_NAME         "ext_file"
#define DSET_DEPREC_NAME               "deprecated"
#define DSET_DEPREC_NAME_CHUNKED       "deprecated_chunked"
#define DSET_DEPREC_NAME_COMPACT       "deprecated_compact"
#define DSET_DEPREC_NAME_FILTER        "deprecated_filter"

/* Dataset names for testing Fixed Array Indexing */
#define DSET_FIXED_MAX     "DSET_FIXED_MAX"
#define DSET_FIXED_NOMAX   "DSET_FIXED_NOMAX"
#define DSET_FIXED_BIG     "DSET_FIXED_BIG"
#define POINTS             72
#define POINTS_BIG         2500

/* Dataset names used for testing header flush dependencies */
#define DSET_EARRAY_HDR_FD  "earray_hdr_fd"
#define DSET_FARRAY_HDR_FD  "farray_hdr_fd"
#define DSET_BT2_HDR_FD     "bt2_hdr_fd"

/* Dataset names for testing Implicit Indexing */
#define DSET_SINGLE_MAX         "DSET_SINGLE_MAX"
#define DSET_SINGLE_NOMAX       "DSET_SINGLE_NOMAX"

#define USER_BLOCK              1024
#define SIXTY_FOUR_KB           65536

/* Temporary filter IDs used for testing */
#define H5Z_FILTER_BOGUS             305
#define H5Z_FILTER_CORRUPT           306
#define H5Z_FILTER_CAN_APPLY_TEST    307
#define H5Z_FILTER_SET_LOCAL_TEST    308
#define H5Z_FILTER_DEPREC            309
#define H5Z_FILTER_EXPAND            310
#define H5Z_FILTER_CAN_APPLY_TEST2   311
#define H5Z_FILTER_COUNT             312

/* Flags for testing filters */
#define DISABLE_FLETCHER32      0
#define ENABLE_FLETCHER32       1
#define DATA_CORRUPTED          1
#define DATA_NOT_CORRUPTED      0

/* Parameters for the "set local" test */
#define BOGUS2_PERM_NPARMS      2       /* Number of "permanent" parameters */
#define BOGUS2_PARAM_1          13      /* (No particular meaning, just for checking value) */
#define BOGUS2_PARAM_2          35      /* (No particular meaning, just for checking value) */
#define BOGUS2_ALL_NPARMS       4       /* Total number of parameter = permanent + "local" parameters */

/* Dimensionality for conversion buffer test */
#define DIM1          100  /* Dim. Size of data member # 1 */
#define DIM2         5000  /* Dim. Size of data member # 2 */
#define DIM3           10  /* Dim. Size of data member # 3 */

/* Parameters for internal filter test */
#define FILTER_CHUNK_DIM1       2
#define FILTER_CHUNK_DIM2       25
#define FILTER_HS_OFFSET1       7
#define FILTER_HS_OFFSET2       30
#define FILTER_HS_SIZE1         4
#define FILTER_HS_SIZE2         50

/* Names for noencoder test */
#ifdef H5_HAVE_FILTER_SZIP
#define NOENCODER_FILENAME      "noencoder.h5"
#define NOENCODER_COPY_FILENAME "noencoder.h5.copy"
#define NOENCODER_TEST_DATASET  "noencoder_tdset.h5"
#define NOENCODER_SZIP_DATASET  "noencoder_szip_dset.h5"
#define NOENCODER_SZIP_SHUFF_FLETCH_DATASET "noencoder_szip_shuffle_fletcher_dset.h5"
#endif /* H5_HAVE_FILTER_SZIP */

/* Names for zero-dim test */
#define ZERODIM_DATASET  "zerodim"
#define ZERODIM_DATASET2 "zerodim2"

/* Parameters for zero-dim test */
#define MISSING_CHUNK_DATASET   "missing_chunk"
#define MISSING_CHUNK_DATASET2  "missing_chunk2"
#define MISSING_CHUNK_DIM       100

/* Names for random chunks test */
#define NPOINTS         50

/* Parameters for huge chunks test */
#define HUGE_DATASET            "Dataset"
#define HUGE_DIM                ((hsize_t)16 * 1024 * 1024 * 1024)
#define HUGE_CHUNK_DIM          ((hsize_t)2 * 1024 * 1024 * 1024)
#define TOO_HUGE_CHUNK_DIM      ((hsize_t)4 * 1024 * 1024 * 1024)
#define HUGE_DATASET2           "Dataset2"
#define HUGE_DIM2_0             ((hsize_t)16 * 1024)
#define HUGE_DIM2_1             ((hsize_t)16 * 1024)
#define HUGE_DIM2_2             ((hsize_t)16 * 1024)
#define HUGE_CHUNK_DIM2_0       ((hsize_t)2 * 1024)
#define HUGE_CHUNK_DIM2_1       ((hsize_t)1024)
#define HUGE_CHUNK_DIM2_2       ((hsize_t)1024)
#define TOO_HUGE_CHUNK_DIM2_0   ((hsize_t)4 * 1024)
#define TOO_HUGE_CHUNK_DIM2_1   ((hsize_t)1024)
#define TOO_HUGE_CHUNK_DIM2_2   ((hsize_t)1024)

/* Parameters for testing bypassing chunk cache */
#define BYPASS_DATASET1          "Dset1"
#define BYPASS_DATASET2          "Dset2"

#define T_BYPASS_DATASET1        "T_Dset1"
#define T_BYPASS_DATASET2        "T_Dset2"

#define BYPASS_DIM               1000
#define BYPASS_CHUNK_DIM         500
#define BYPASS_FILL_VALUE        7

/* Parameters for testing extensible array chunk indices */
#define EARRAY_MAX_RANK         3
#define EARRAY_DSET_DIM         15
#define EARRAY_CHUNK_DIM        3
#define EARRAY_EXTEND_INCR      15
#define EARRAY_MAX_EXTEND       75

/* Parameters for datasets in query storage size tests */
#define STORAGE_SIZE_DIM1       12
#define STORAGE_SIZE_DIM2       6
#define STORAGE_SIZE_MAX_DIM1   100
#define STORAGE_SIZE_MAX_DIM2   80
#define STORAGE_SIZE_CHUNK_DIM1 5
#define STORAGE_SIZE_CHUNK_DIM2 5

/* Parameters for testing version bounds */
#define VDS_FNAME1  "virtual_file1"
#define VDS_FNAME2  "virtual_file2"
#define SRC_FNAME   "source_file"
#define SRC_DSET    "src_dset"
#define V_DSET      "v_dset"

/* Shared global arrays */
#define DSET_DIM1       100
#define DSET_DIM2       200
int     points[DSET_DIM1][DSET_DIM2], check[DSET_DIM1][DSET_DIM2];
double  points_dbl[DSET_DIM1][DSET_DIM2], check_dbl[DSET_DIM1][DSET_DIM2];
size_t  count_nbytes_read = 0;
size_t  count_nbytes_written = 0;

/* Temporary buffer dimensions */
#define DSET_TMP_DIM1   50
#define DSET_TMP_DIM2   100

/* Declarations for test_idx_compatible() */
#define DSET            "dset"
#define DSET_FILTER     "dset_filter"
const char *OLD_FILENAME[] = {  /* Files created under 1.6 branch and 1.8 branch */
    "btree_idx_1_6.h5", /* 1.6 HDF5 file */
    "btree_idx_1_8.h5"  /* 1.8 HDF5 file */
};


/* Local prototypes for filter functions */
static size_t filter_bogus(unsigned int flags, size_t cd_nelmts,
        const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);
static htri_t can_apply_bogus(hid_t dcpl_id, hid_t type_id, hid_t space_id);
static herr_t set_local_bogus2(hid_t dcpl_id, hid_t type_id, hid_t space_id);
static size_t filter_bogus2(unsigned int flags, size_t cd_nelmts,
        const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);
static size_t filter_bogus3(unsigned int flags, size_t cd_nelmts,
        const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);
static size_t filter_corrupt(unsigned int flags, size_t cd_nelmts,
        const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);
static size_t filter_expand(unsigned int flags, size_t cd_nelmts,
        const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);
static size_t filter_count(unsigned int flags, size_t cd_nelmts,
        const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);

/* This message derives from H5Z */
const H5Z_class2_t H5Z_COUNT[1] = {{
    H5Z_CLASS_T_VERS,           /* H5Z_class_t version */
    H5Z_FILTER_COUNT,           /* Filter id number */
    1, 1,                       /* Encoding and decoding enabled */
    "count",                    /* Filter name for debugging */
    NULL,                       /* The "can apply" callback */
    NULL,                       /* The "set local" callback */
    filter_count,               /* The actual filter function */
}};


/*-------------------------------------------------------------------------
 * Function:    filter_count
 *
 * Purpose:     This filter counts the number of bytes read and written,
 *              incrementing count_nbytes_read or count_nbytes_written as
 *              appropriate.
 *
 * Return:      Success:        Data chunk size
 *              Failure:        0
 *-------------------------------------------------------------------------
 */
static size_t
filter_count(unsigned int flags, size_t H5_ATTR_UNUSED cd_nelmts,
      const unsigned int H5_ATTR_UNUSED *cd_values, size_t nbytes,
      size_t H5_ATTR_UNUSED *buf_size, void H5_ATTR_UNUSED **buf)
{
    if(flags & H5Z_FLAG_REVERSE)
        count_nbytes_read += nbytes;
    else
        count_nbytes_written += nbytes;

    return nbytes;
} /* end filter_count() */


/*-------------------------------------------------------------------------
 * Function:  test_create
 *
 * Purpose:   Attempts to create a dataset.
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_create(hid_t file)
{
    hid_t      dataset, space, small_space, create_parms;
    hsize_t    dims[2], small_dims[2];
    herr_t     status;
    hsize_t    csize[2];

    TESTING("create, open, close");

    /* Create the data space */
    dims[0] = 256;
    dims[1] = 512;
    space = H5Screate_simple(2, dims, NULL);
    assert(space>=0);

    /* Create a small data space for compact dataset */
    small_dims[0] = 16;
    small_dims[1] = 8;
    small_space = H5Screate_simple(2, small_dims, NULL);
    assert(space>=0);

    /*
     * Create a dataset using the default dataset creation properties.    We're
     * not sure what they are, so we won't check.
     */
    dataset = H5Dcreate2(file, DSET_DEFAULT_NAME, H5T_NATIVE_DOUBLE, space,
            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dataset < 0) goto error;

    /* Close the dataset */
    if(H5Dclose(dataset) < 0) goto error;

    /* Add a comment to the dataset */
    status = H5Oset_comment_by_name(file, DSET_DEFAULT_NAME, "This is a dataset", H5P_DEFAULT);
    if(status < 0) goto error;

    /*
     * Try creating a dataset that already exists.  This should fail since a
     * dataset can only be created once.  Temporarily turn off error
     * reporting.
     */
    H5E_BEGIN_TRY {
    dataset = H5Dcreate2(file, DSET_DEFAULT_NAME, H5T_NATIVE_DOUBLE, space,
                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dataset >= 0) {
        H5_FAILED();
        HDputs("    Library allowed overwrite of existing dataset.");
        goto error;
    }

    /*
     * Open the dataset we created above and then close it.  This is how
     * existing datasets are accessed.
     */
    if(H5Fflush(file, H5F_SCOPE_GLOBAL) < 0) goto error;
    if((dataset = H5Dopen2(file, DSET_DEFAULT_NAME, H5P_DEFAULT)) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    /*
     * Try opening a non-existent dataset. This should fail since new datasets
     * cannot be created with this function.  Temporarily turn off error
     * reporting.
     */
    H5E_BEGIN_TRY {
        dataset = H5Dopen2(file, "does_not_exist", H5P_DEFAULT);
    } H5E_END_TRY;
    if(dataset >= 0) {
        H5_FAILED();
        HDputs("    Opened a non-existent dataset.");
        goto error;
    }

    /*
     * Create a new dataset that uses chunked storage instead of the default
     * layout.
     */
    create_parms = H5Pcreate(H5P_DATASET_CREATE);
    assert(create_parms >= 0);

    /* Attempt to create a dataset with invalid chunk sizes */
    csize[0] = dims[0]*2;
    csize[1] = dims[1]*2;
    status = H5Pset_chunk(create_parms, 2, csize);
    assert(status >= 0);
    H5E_BEGIN_TRY {
        dataset = H5Dcreate2(file, DSET_CHUNKED_NAME, H5T_NATIVE_DOUBLE, space,
            H5P_DEFAULT, create_parms, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dataset >= 0) {
        H5_FAILED();
        HDputs("    Opened a dataset with incorrect chunking parameters.");
        goto error;
    }

    csize[0] = 5;
    csize[1] = 100;
    status = H5Pset_chunk(create_parms, 2, csize);
    assert(status >= 0);

    dataset = H5Dcreate2(file, DSET_CHUNKED_NAME, H5T_NATIVE_DOUBLE, space,
            H5P_DEFAULT, create_parms, H5P_DEFAULT);
    if(dataset < 0) goto error;
    H5Pclose(create_parms);

    /* Test dataset address.  Should be undefined. */
    if(H5Dget_offset(dataset)!=HADDR_UNDEF) goto error;

    /*
     * Close the chunked dataset.
     */
    if(H5Dclose(dataset) < 0) goto error;

    /*
     * Create a compact dataset, then close it.
     */
    create_parms = H5Pcreate(H5P_DATASET_CREATE);
    assert(create_parms >= 0);
    status = H5Pset_layout(create_parms, H5D_COMPACT);
    assert(status >= 0);
    status = H5Pset_alloc_time(create_parms, H5D_ALLOC_TIME_EARLY);
    assert(status >= 0);

    dataset = H5Dcreate2(file, DSET_COMPACT_NAME, H5T_NATIVE_DOUBLE,
                        small_space, H5P_DEFAULT, create_parms, H5P_DEFAULT);
    if(dataset < 0) goto error;
    H5Pclose(create_parms);
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
    return SUCCEED;

 error:
    return FAIL;
} /* end test_create() */


/*-------------------------------------------------------------------------
 * Function:   test_simple_io
 *
 * Purpose:    Tests simple I/O.  That is, reading and writing a complete
 *        multi-dimensional array without data type or data space
 *        conversions, without compression, and stored contiguously.
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_simple_io(const char *env_h5_drvr, hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       file = -1, dataset = -1, space = -1, xfer = -1;
    int         i, j, n;
    hsize_t     dims[2];
    void        *tconv_buf = NULL;
    int         f = -1;
    haddr_t     offset;
    int         rdata[DSET_DIM1][DSET_DIM2];

    TESTING("simple I/O");

    /* Can't run this test with multi-file VFDs because of HDopen/read/seek the file directly */
    if(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi") && HDstrcmp(env_h5_drvr, "family")) {
        h5_fixname(FILENAME[4], fapl, filename, sizeof filename);

        /* Initialize the dataset */
        for(i = n = 0; i < DSET_DIM1; i++)
            for(j = 0; j < DSET_DIM2; j++)
                points[i][j] = n++;

        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            goto error;

        /* Create the data space */
        dims[0] = DSET_DIM1;
        dims[1] = DSET_DIM2;
        if((space = H5Screate_simple(2, dims, NULL)) < 0) goto error;

        /* Create a small conversion buffer to test strip mining */
        tconv_buf = HDmalloc((size_t)1000);
        xfer = H5Pcreate(H5P_DATASET_XFER);
        assert(xfer>=0);
        if(H5Pset_buffer (xfer, (size_t)1000, tconv_buf, NULL) < 0) goto error;

        /* Create the dataset */
        if((dataset = H5Dcreate2(file, DSET_SIMPLE_IO_NAME, H5T_NATIVE_INT, space,
                                 H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
        if(H5Sclose(space) < 0) TEST_ERROR
        space = -1;

        /* Test dataset address.  Should be undefined. */
        if(H5Dget_offset(dataset) != HADDR_UNDEF) goto error;

        /* Write the data to the dataset */
        if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, points) < 0)
            goto error;

        /* Test dataset address in file. Open the same file as a C file, seek
         * the data position as H5Dget_offset points to, read the dataset, and
         * compare it with the data written in.*/
        if((offset=H5Dget_offset(dataset))==HADDR_UNDEF) goto error;

        /* Read the dataset back */
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, check) < 0)
            goto error;

        /* Check that the values read are the same as the values written */
        for(i = 0; i < DSET_DIM1; i++) {
            for(j = 0; j < DSET_DIM2; j++) {
                if(points[i][j] != check[i][j]) {
                    H5_FAILED();
                    HDprintf("    Read different values than written.\n");
                    HDprintf("    At index %d,%d\n", i, j);
                    goto error;
                }
            }
        }

        if(H5Pclose (xfer) < 0) goto error;
            xfer = -1;
        if(H5Dclose(dataset) < 0) goto error;
            dataset = -1;
        if(H5Fclose(file) < 0) goto error;
            file = -1;

        f = HDopen(filename, O_RDONLY);
        HDlseek(f, (off_t)offset, SEEK_SET);
        if(HDread(f, rdata, sizeof(int)*DSET_DIM1*DSET_DIM2) < 0)
            goto error;

        /* Check that the values read are the same as the values written */
        for(i = 0; i < DSET_DIM1; i++) {
            for(j = 0; j < DSET_DIM2; j++) {
                if(points[i][j] != rdata[i][j]) {
                    H5_FAILED();
                    HDprintf("    Read different values than written.\n");
                    HDprintf("    At index %d,%d\n", i, j);
                    goto error;
                }
            }
        }

        HDclose(f);
        f = -1;

        HDfree(tconv_buf);
        PASSED();
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support continuous address space");
    } /* end else */

    return SUCCEED;

error:
    if(space > 0)
        if(H5Sclose(space) < 0) TEST_ERROR
    if(xfer > 0)
        if(H5Pclose(xfer) < 0) TEST_ERROR
    if(dataset > 0)
        if(H5Dclose(dataset) < 0) TEST_ERROR
    if(file > 0)
        if(H5Fclose(file) < 0) TEST_ERROR
    if(f > 0)
        HDclose(f);
    if(tconv_buf)
        HDfree(tconv_buf);
    return FAIL;
} /* end test_simple_io() */


/*-------------------------------------------------------------------------
 * Function:  test_userblock_offset
 *
 * Purpose:   Tests H5Dget_offset when user block exists.
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_userblock_offset(const char *env_h5_drvr, hid_t fapl, hbool_t new_format)
{
    char                filename[FILENAME_BUF_SIZE];
    hid_t               file = -1, fcpl = -1, dataset = -1, space = -1;
    int                 i, j;
    hsize_t             dims[2];
    int                 f = -1;
    haddr_t             offset;
    int                 rdata[DSET_DIM1][DSET_DIM2];

    TESTING("dataset offset with user block");

    /* Can't run this test with multi-file VFDs because of HDopen/read/seek the file directly */
    if(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi") && HDstrcmp(env_h5_drvr, "family")) {
        h5_fixname(FILENAME[2], fapl, filename, sizeof filename);

        if((fcpl=H5Pcreate(H5P_FILE_CREATE)) < 0) goto error;
        if(H5Pset_userblock(fcpl, (hsize_t)USER_BLOCK) < 0) goto error;
        if(new_format)
            if(H5Pset_file_space_page_size(fcpl, (hsize_t)USER_BLOCK) < 0)
                goto error;

        if((file=H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
            goto error;
        if(H5Pclose(fcpl) < 0) TEST_ERROR
        fcpl = -1;

        /* Create the data space */
        dims[0] = DSET_DIM1;
        dims[1] = DSET_DIM2;
        if((space = H5Screate_simple(2, dims, NULL)) < 0) goto error;

        /* Create the dataset */
        if((dataset = H5Dcreate2(file, DSET_USERBLOCK_IO_NAME, H5T_NATIVE_INT, space,
                                 H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
        if(H5Sclose(space) < 0) TEST_ERROR
        space = -1;

        /* Write the data to the dataset */
        if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0)
            goto error;

        /* Test dataset address in file. Open the same file as a C file, seek
         * the data position as H5Dget_offset points to, read the dataset, and
         * compare it with the data written in.*/
        if((offset = H5Dget_offset(dataset)) == HADDR_UNDEF) goto error;

        if(H5Dclose(dataset) < 0) goto error;
        dataset = -1;
        if(H5Fclose(file) < 0) goto error;
        file = -1;

        f = HDopen(filename, O_RDONLY);
        HDlseek(f, (off_t)offset, SEEK_SET);
        if(HDread(f, rdata, sizeof(int)*DSET_DIM1*DSET_DIM2) < 0)
            goto error;

        /* Check that the values read are the same as the values written */
        for(i = 0; i < DSET_DIM1; i++) {
            for(j = 0; j < DSET_DIM2; j++) {
                if(points[i][j] != rdata[i][j]) {
                    H5_FAILED();
                    HDprintf("    Read different values than written.\n");
                    HDprintf("    At index %d,%d\n", i, j);
                    goto error;
                }
            }
        }

        HDclose(f);
        f = -1;

        PASSED();
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support continuous address space");
    } /* end else */

    return SUCCEED;

error:
    if(space > 0)
        if(H5Sclose(space) < 0) TEST_ERROR
    if(fcpl > 0)
        if(H5Pclose(fcpl) < 0) TEST_ERROR
    if(dataset > 0)
        if(H5Dclose(dataset) < 0) TEST_ERROR
    if(file > 0)
        if(H5Fclose(file) < 0) TEST_ERROR
    if(f > 0)
        HDclose(f);
    return FAIL;
} /* end test_userblock_offset() */


/*-------------------------------------------------------------------------
 * Function:    test_compact_io
 *
 * Purpose:     Tests compact dataset I/O.  That is, reading and writing a
 *              complete multi-dimensional array without data type or data
 *              space conversions, without compression, and store in
 *              compact dataset.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_compact_io(hid_t fapl)
{
    hid_t       file, dataset, space, plist;
    hid_t verfile = -1, new_fapl = -1;
    hsize_t     dims[2];
    int         wbuf[16][8], rbuf[16][8];
    char        filename[FILENAME_BUF_SIZE];
    H5F_libver_t low, high;     /* File format bounds */
    H5F_t       *fp;            /* Internal file pointer */
    H5D_t       *dsetp;         /* Internal dataset pointer */
    int         i, j, n;        /* Indices */
    herr_t      ret;            /* Generic return value */

    TESTING("compact dataset I/O");

    /* Initialize data */
    n = 0;
    for(i = 0; i < 16; i++)
        for(j = 0; j < 8; j++)
            wbuf[i][j] = n++;

    /* Create a small data space for compact dataset */
    dims[0] = 16;
    dims[1] = 8;
    if((space = H5Screate_simple(2, dims, NULL)) < 0) TEST_ERROR

    /* Create a file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create property list for compact dataset creation */
    if((plist = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_layout(plist, H5D_COMPACT) < 0) TEST_ERROR
    if(H5Pset_alloc_time(plist, H5D_ALLOC_TIME_EARLY) < 0) TEST_ERROR

    /* Create and write to a compact dataset */
    if((dataset = H5Dcreate2(file, DSET_COMPACT_IO_NAME, H5T_NATIVE_INT, space, H5P_DEFAULT, plist, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Test dataset address.  Should be undefined. */
    if(H5Dget_offset(dataset) != HADDR_UNDEF) TEST_ERROR

    if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR

    /* Test dataset address.  Should be undefined. */
    if(H5Dget_offset(dataset)!=HADDR_UNDEF) TEST_ERROR

    /* Close file */
    if(H5Dclose(dataset) < 0) TEST_ERROR
    if(H5Fclose(file) < 0) TEST_ERROR

    /*
     * Open the file and check data
     */
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR
    if((dataset = H5Dopen2(file, DSET_COMPACT_IO_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR

     /* Check that the values read are the same as the values written */
     for(i = 0; i < 16; i++)
         for(j = 0; j < 8; j++)
             if(rbuf[i][j] != wbuf[i][j]) {
                 H5_FAILED();
                 HDprintf("    Read different values than written.\n");
                 HDprintf("    At index %d,%d\n", i, j);
                 HDprintf("    wbuf[%d][%d]=%d\n", i, j, wbuf[i][j]);
                 HDprintf("    rbuf[%d][%d]=%d\n", i, j, rbuf[i][j]);
                 goto error;
             } /* end  */

     if(H5Dclose(dataset) < 0) TEST_ERROR
     if(H5Fclose(file) < 0) TEST_ERROR

    /**************************************
     * Additional test for version bounds *
     **************************************/

    /* Create a copy of file access property list */
    if((new_fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) TEST_ERROR

    /* Loop through all the combinations of low/high library format bounds,
       skipping invalid combinations.
       - Create a file, create and write a compact dataset, and verify its data
       - Verify the dataset's layout and fill message versions */
    for(low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for(high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {

            /* Set version bounds */
            H5E_BEGIN_TRY {
                ret = H5Pset_libver_bounds(new_fapl, low, high);
            } H5E_END_TRY;

            if (ret < 0) /* Invalid low/high combinations */
                continue;

            /* Create a file */
            h5_fixname(FILENAME[25], new_fapl, filename, sizeof filename);
            if((verfile = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, new_fapl)) < 0)
                TEST_ERROR

            /* Create the compact dataset */
            dataset = H5Dcreate2(verfile, DSET_DEFAULT_NAME, H5T_NATIVE_INT, space, H5P_DEFAULT, plist, H5P_DEFAULT);
            if(dataset < 0) TEST_ERROR

            /* Write the same data as of DSET_COMPACT_IO_NAME */
            if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
                TEST_ERROR

            /* Close DSET_DEFAULT_NAME, then reopen it to read and verify
               the data */
            if(H5Dclose(dataset) < 0) TEST_ERROR
            if((dataset = H5Dopen2(verfile, DSET_DEFAULT_NAME, H5P_DEFAULT)) < 0)
                TEST_ERROR
            if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
                TEST_ERROR

            /* Check that the values read are the same as the values written */
            for(i = 0; i < 16; i++)
                for(j = 0; j < 8; j++)
                    if(rbuf[i][j] != wbuf[i][j]) {
                        H5_FAILED();
                        HDprintf("    Read different values than written.\n");
                        HDprintf("    At index %d,%d\n", i, j);
                        HDprintf("    wbuf[%d][%d]=%d\n", i, j, wbuf[i][j]);
                        HDprintf("    rbuf[%d][%d]=%d\n", i, j, rbuf[i][j]);
                        goto error;
                    } /* end  */

            /* Get the internal file pointer */
            if((fp = (H5F_t *)H5I_object(verfile)) == NULL) TEST_ERROR

            /* Get the internal dataset pointer */
            if((dsetp = (H5D_t *)H5I_object(dataset)) == NULL) TEST_ERROR

            /* Verify the dataset's layout and fill message versions */
            if(fp->shared->low_bound == H5F_LIBVER_EARLIEST) {
                VERIFY(dsetp->shared->layout.version, H5O_LAYOUT_VERSION_DEFAULT, "layout_ver_bounds");
                VERIFY(dsetp->shared->dcpl_cache.fill.version, H5O_FILL_VERSION_2, "fill_ver_bounds");
            } else {
                VERIFY(dsetp->shared->layout.version, H5O_layout_ver_bounds[fp->shared->low_bound], "layout_ver_bounds");
                VERIFY(dsetp->shared->dcpl_cache.fill.version, H5O_fill_ver_bounds[fp->shared->low_bound], "fill_ver_bounds");
            }

            /* Close the dataset and delete from the file */
            if(H5Dclose(dataset) < 0) TEST_ERROR
            if(H5Ldelete(verfile, DSET_DEFAULT_NAME, H5P_DEFAULT) < 0)
                TEST_ERROR

            /* Close the file */
            if(H5Fclose(verfile) < 0) TEST_ERROR

        } /* end for high */
    } /* end for low */

    if(H5Pclose(new_fapl) < 0) TEST_ERROR
    if(H5Sclose(space) < 0) TEST_ERROR
    if(H5Pclose(plist) < 0) TEST_ERROR

    PASSED();
    return SUCCEED;

 error:
    H5E_BEGIN_TRY {
        H5Sclose(space);
        H5Pclose(plist);
        H5Pclose(new_fapl);
        H5Dclose(dataset);
        H5Fclose(file);
        H5Fclose(verfile);
    } H5E_END_TRY;

    return FAIL;
} /* end test_compact_io() */


/*-------------------------------------------------------------------------
 * Function:    test_max_compact
 *
 * Purpose:     Tests compact dataset of maximal size.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_max_compact(hid_t fapl)
{
    hid_t       file = -1;
    hid_t       dataset = -1;
    hid_t       space = -1;
    hid_t       plist = -1;
    hsize_t     dims[1];
    size_t      compact_size;
    int        *wbuf = NULL;
    int        *rbuf = NULL;
    char        filename[FILENAME_BUF_SIZE];
    int         n;
    size_t      u;

    TESTING("compact dataset of maximal size");

    /* Test compact dataset of size 64KB-64 */

    /* Initialize data */
    compact_size = (SIXTY_FOUR_KB - 64) / sizeof(int);

    if(NULL == (wbuf = (int *)HDmalloc(sizeof(int) * compact_size)))
        TEST_ERROR
    if(NULL == (rbuf = (int *)HDmalloc(sizeof(int) * compact_size)))
        TEST_ERROR

    n = 0;
    for(u = 0; u < compact_size; u++)
        wbuf[u] = n++;

    /* Create a small data space for compact dataset */
    dims[0] = (hsize_t)compact_size;
    if((space = H5Screate_simple(1, dims, NULL)) < 0)
        FAIL_STACK_ERROR

    /* Create a file */
    h5_fixname(FILENAME[3], fapl, filename, sizeof filename);
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Create property list for compact dataset creation */
    if((plist = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_layout(plist, H5D_COMPACT) < 0)
        FAIL_STACK_ERROR

    /* Create and write to a compact dataset */
    if((dataset = H5Dcreate2(file, DSET_COMPACT_MAX_NAME, H5T_NATIVE_INT, space, H5P_DEFAULT, plist, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Sclose(space) < 0)
        FAIL_STACK_ERROR
    if(H5Pclose(plist) < 0)
        FAIL_STACK_ERROR
    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /*
     * Open the file and check data
     */
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR
    if((dataset = H5Dopen2(file, DSET_COMPACT_MAX_NAME, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        FAIL_STACK_ERROR

    /* Check that the values read are the same as the values written */
    for(u = 0; u < compact_size; u++)
        if(rbuf[u] != wbuf[u]) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %u\n", (unsigned)u);
            goto error;
        } /* end if */

     if(H5Dclose(dataset) < 0)
         FAIL_STACK_ERROR
     if(H5Fclose(file) < 0)
         FAIL_STACK_ERROR
     HDfree(wbuf);
     wbuf = NULL;
     HDfree(rbuf);
     rbuf = NULL;

     /* Test compact dataset of size 64KB */

     /* Create a data space for compact dataset */
     compact_size = SIXTY_FOUR_KB / sizeof(int);
     dims[0] = (hsize_t)compact_size;
     if((space = H5Screate_simple(1, dims, NULL)) < 0)
         FAIL_STACK_ERROR

     /* Open file */
     if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
         goto error;

     /* Create property list for compact dataset creation */
     if((plist = H5Pcreate(H5P_DATASET_CREATE)) < 0)
         FAIL_STACK_ERROR
     if(H5Pset_layout(plist, H5D_COMPACT) < 0)
         FAIL_STACK_ERROR

     /* Create and write to a compact dataset */
     H5E_BEGIN_TRY {
         H5Dcreate2(file, DSET_COMPACT_MAX2_NAME, H5T_NATIVE_INT, space, H5P_DEFAULT, plist, H5P_DEFAULT);
     } H5E_END_TRY;

     /* Close file */
     if(H5Sclose(space) < 0)
         FAIL_STACK_ERROR
     if(H5Pclose(plist) < 0)
         FAIL_STACK_ERROR
     if(H5Fclose(file) < 0)
         FAIL_STACK_ERROR

     PASSED();
     return SUCCEED;

error:
    if(wbuf)
        HDfree(wbuf);
    if(rbuf)
        HDfree(rbuf);

    H5E_BEGIN_TRY {
        /* Close file */
        H5Sclose(space);
        H5Pclose(plist);
        H5Dclose(dataset);
        H5Fclose(file);
    } H5E_END_TRY;

     return FAIL;
} /* end test_max_compact() */


/*-------------------------------------------------------------------------
 * Function:    test_layout_extend
 *
 * Purpose:     Verify that the creation of extendible dataset with dataspace:
 *        cur_dims < max_dims (max_dims can be fixed size or H5S_UNLIMITED)
 *        will behave as follows:
 *            H5D_COMPACT layout: fail
 *            H5D_CONTIGUOUS layout: fail
 *            H5D_CHUNKED layout: succeed
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_layout_extend(hid_t fapl)
{
    char filename[FILENAME_BUF_SIZE];    /* File name */
    hid_t fid = -1;                 /* File id */
    hid_t sid_fix = -1, sid_unlim = -1;     /* Dataspace id */
    hid_t dcpl_compact = -1, dcpl_contig = -1, dcpl_chunked = -1; /* Dataset creation property list id */
    hid_t did_fixed = -1, did_unlim = -1;    /* Dataset id */
    hsize_t cur_size[1] = {10};        /* Current size of dataspace */
    hsize_t max_unlim[1] = {H5S_UNLIMITED};        /* Maximum size of dataspace (unlimited) */
    hsize_t max_fix[1] = {100};                /* Maximum size of dataspace (fixed) */
    hsize_t chunk_dim[1] = {10};            /* Chunk size */

    TESTING("extendible dataset with various layout");

    /* Create a file */
    h5_fixname(FILENAME[15], fapl, filename, sizeof filename);
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Create dataspace */
    if((sid_fix = H5Screate_simple(1, cur_size, max_fix)) < 0)
        FAIL_STACK_ERROR
    if((sid_unlim = H5Screate_simple(1, cur_size, max_unlim)) < 0)
        FAIL_STACK_ERROR

    /* Create property list for compact dataset creation */
    if((dcpl_compact = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_layout(dcpl_compact, H5D_COMPACT) < 0)
        FAIL_STACK_ERROR

    /* Create dataset with extendible dataspace (fixed max_dims) should fail */
    H5E_BEGIN_TRY {
    if(H5Dcreate2(fid, "compact", H5T_NATIVE_INT, sid_fix, H5P_DEFAULT, dcpl_compact, H5P_DEFAULT) != FAIL)
        TEST_ERROR
    } H5E_END_TRY;

    /* Create dataset with extendible dataspace (unlimited max_dims) should fail */
    H5E_BEGIN_TRY {
    if(H5Dcreate2(fid, "compact", H5T_NATIVE_INT, sid_unlim, H5P_DEFAULT, dcpl_compact, H5P_DEFAULT) != FAIL)
        TEST_ERROR
    } H5E_END_TRY;

    /* Create property list for contiguous dataset creation */
    if((dcpl_contig = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if((H5Pset_layout(dcpl_contig, H5D_CONTIGUOUS)) < 0)
        FAIL_STACK_ERROR

    /* Create dataset with extendible dataspace (fixed max_dims) should fail */
    H5E_BEGIN_TRY {
    if(H5Dcreate2(fid, "contig", H5T_NATIVE_INT, sid_fix, H5P_DEFAULT, dcpl_contig, H5P_DEFAULT) != FAIL)
        TEST_ERROR
    } H5E_END_TRY;

    /* Create dataset with extendible dataspace (unlimited max_dims) should fail*/
    H5E_BEGIN_TRY {
    if(H5Dcreate2(fid, "contig", H5T_NATIVE_INT, sid_unlim, H5P_DEFAULT, dcpl_contig, H5P_DEFAULT) != FAIL)
        TEST_ERROR
    } H5E_END_TRY;

    /* Create property list for chunked dataset creation */
    if((dcpl_chunked = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_layout(dcpl_chunked, H5D_CHUNKED) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_chunk(dcpl_chunked, 1, chunk_dim) < 0) FAIL_STACK_ERROR

    /* Create dataset with extendible dataspace (fixed max_dims) should succeed */
    if((did_fixed = H5Dcreate2(fid, "chunked_fixed", H5T_NATIVE_INT, sid_fix, H5P_DEFAULT, dcpl_chunked, H5P_DEFAULT)) < 0)
    FAIL_STACK_ERROR

    /* Create dataset with extendible dataspace (unlimited max_dims) should succeed */
    if((did_unlim = H5Dcreate2(fid, "chunked_unlim", H5T_NATIVE_INT, sid_unlim, H5P_DEFAULT, dcpl_chunked, H5P_DEFAULT)) < 0)
    FAIL_STACK_ERROR

    /* Closing */
    if(H5Sclose(sid_fix) < 0) FAIL_STACK_ERROR
    if(H5Sclose(sid_unlim) < 0) FAIL_STACK_ERROR

    if(H5Pclose(dcpl_compact) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl_contig) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl_chunked) < 0) FAIL_STACK_ERROR

    if(H5Dclose(did_fixed) < 0) FAIL_STACK_ERROR
    if(H5Dclose(did_unlim) < 0) FAIL_STACK_ERROR

    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid_fix);
        H5Sclose(sid_unlim);
        H5Pclose(dcpl_compact);
        H5Pclose(dcpl_contig);
        H5Pclose(dcpl_chunked);
        H5Dclose(did_fixed);
        H5Dclose(did_unlim);
        H5Fclose(fid);
    } H5E_END_TRY;

     return FAIL;
} /* end test_layout_extend() */


/*-------------------------------------------------------------------------
 * Function:  test_conv_buffer
 *
 * Purpose:   Test size of data type conversion buffer.
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_conv_buffer(hid_t fid)
{
    typedef struct
    {
        int      a[DIM1][DIM2][DIM3];
        float    b[DIM2];
        double   c[DIM3];
    } CmpField;

    typedef struct
    {
        float    b[DIM2];
        double   c[DIM3];
    } CmpFieldR;

    herr_t       status = -1;
    int          j, k, l;

    CmpField     *cf = NULL;
    CmpFieldR    *cfrR = NULL;

    hid_t       dataset = H5I_INVALID_HID; /* dataset ID             */
    hid_t       space   = H5I_INVALID_HID; /* data space ID          */
    hid_t       ctype1 = H5I_INVALID_HID,
                ctype2 = H5I_INVALID_HID; /* data type ID           */
    hid_t       arr_type1 = H5I_INVALID_HID,
                arr_type2 = H5I_INVALID_HID,
                arr_type3 = H5I_INVALID_HID,
                arr_type4 = H5I_INVALID_HID,
                arr_type5 = H5I_INVALID_HID;
    hsize_t     dimsa[3];
    hsize_t     dimsb[1];
    hsize_t     dimsc[1];
    hid_t       xfer_list = H5I_INVALID_HID;
    size_t      size;

    TESTING("data type conversion buffer size");

    if ((cf = (CmpField *)HDcalloc((size_t)1, sizeof(CmpField))) == 0) goto error;

    /* Populate the data members */
    for(j = 0; j < DIM1; j++)
        for(k = 0; k < DIM2; k++)
            for(l = 0; l < DIM3; l++)
                cf->a[j][k][l] = 10*(j+1) + l + k;

    for(j = 0; j < DIM2; j++)
        cf->b[j] = 100.0f * (float)(j+1) + 0.01f * (float)j;

    for(j = 0; j < DIM3; j++)
        cf->c[j] = 100.0f * (float)(j+1) + 0.02f * (float)j;


  /* Create data space */
  if((space=H5Screate(H5S_SCALAR)) < 0) goto error;

  /* Add  members to the compound data type */
  dimsa[0] = DIM1;
  dimsa[1] = DIM2;
  dimsa[2] = DIM3;
  dimsb[0] = DIM2;
  dimsc[0] = DIM3;

  /* Create the memory data type */
  if((ctype1 = H5Tcreate(H5T_COMPOUND, sizeof (CmpField))) < 0) goto error;

  if((arr_type1 = H5Tarray_create2(H5T_NATIVE_INT, 3, dimsa)) < 0) goto error;
  if((arr_type2 = H5Tarray_create2(H5T_NATIVE_FLOAT, 1, dimsb)) < 0) goto error;
  if((arr_type3 = H5Tarray_create2(H5T_NATIVE_DOUBLE, 1, dimsc)) < 0) goto error;

  if(H5Tinsert(ctype1, "A", HOFFSET(CmpField, a), arr_type1) < 0) goto error;
  if(H5Tinsert(ctype1, "B", HOFFSET(CmpField, b), arr_type2) < 0) goto error;
  if(H5Tinsert(ctype1, "C", HOFFSET(CmpField, c), arr_type3) < 0) goto error;

  /* Create the dataset */
  if((dataset = H5Dcreate2(fid, DSET_CONV_BUF_NAME, ctype1, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
  if(H5Dwrite(dataset, ctype1, H5S_ALL, H5S_ALL, H5P_DEFAULT, cf) < 0) goto error;

  if((ctype2 = H5Tcreate(H5T_COMPOUND, sizeof (CmpFieldR))) < 0) goto error;

  if((arr_type4 = H5Tarray_create2(H5T_NATIVE_FLOAT, 1, dimsb)) < 0) goto error;
  if((arr_type5 = H5Tarray_create2(H5T_NATIVE_DOUBLE, 1, dimsc)) < 0) goto error;

  if(H5Tinsert(ctype2, "B", HOFFSET(CmpFieldR, b), arr_type4) < 0) goto error;
  if(H5Tinsert(ctype2, "C", HOFFSET(CmpFieldR, c), arr_type5) < 0) goto error;

  /* Read should succeed since library will set conversion buffer big enough */
  if ((cfrR = (CmpFieldR *)HDcalloc((size_t)1, sizeof(CmpFieldR))) == 0) goto error;
  if(H5Dread(dataset, ctype2, H5S_ALL, H5S_ALL, H5P_DEFAULT, cfrR) < 0) goto error;

  /* Read should fail since conversion buffer isn't big enough */
  xfer_list = H5Pcreate(H5P_DATASET_XFER);
  size = (DIM2 * DIM3 * (sizeof(int))+ DIM2 * (sizeof(float))+
         DIM3 * (sizeof(double)));
  if(H5Pset_buffer(xfer_list, size, NULL, NULL) < 0) goto error;

  H5E_BEGIN_TRY {
    status = H5Dread(dataset, ctype2, H5S_ALL, H5S_ALL, xfer_list, cfrR);
  } H5E_END_TRY;
  if(status >= 0) {
      H5_FAILED();
      HDputs("    Library shouldn't allow conversion buffer too small");
      goto error;
  }

  /* Read will succeed since conversion buffer is big enough */
  size = (DIM1 * DIM2 * DIM3 * (sizeof(int))+ DIM2 * (sizeof(float))+
         DIM3 * (sizeof(double)));
  if(H5Pset_buffer(xfer_list, size, NULL, NULL) < 0) goto error;

  if(H5Dread(dataset, ctype2, H5S_ALL, H5S_ALL, xfer_list, cfrR) < 0) goto error;


  if(H5Pclose(xfer_list) < 0) goto error;
  if(H5Sclose(space) < 0) goto error;
  if(H5Tclose(arr_type1) < 0) goto error;
  if(H5Tclose(arr_type2) < 0) goto error;
  if(H5Tclose(arr_type3) < 0) goto error;
  if(H5Tclose(ctype1) < 0) goto error;
  if(H5Tclose(ctype2) < 0) goto error;
  if(H5Tclose(arr_type4) < 0) goto error;
  if(H5Tclose(arr_type5) < 0) goto error;
  if(H5Dclose(dataset) < 0) goto error;

  HDfree(cf);
  HDfree(cfrR);
  HDputs(" PASSED");
  return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(xfer_list);
        H5Sclose(space);
        H5Tclose(arr_type1);
        H5Tclose(arr_type2);
        H5Tclose(arr_type3);
        H5Tclose(ctype1);
        H5Tclose(ctype2);
        H5Tclose(arr_type4);
        H5Tclose(arr_type5);
        H5Dclose(dataset);
    } H5E_END_TRY;

  return FAIL;
} /* end test_conv_buffer() */


/*-------------------------------------------------------------------------
 * Function:  test_tconv
 *
 * Purpose:   Test some simple data type conversion stuff.
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_tconv(hid_t file)
{
    char    *out = NULL, *in = NULL;
    hsize_t    dims[1];
    hid_t    space = -1, dataset = -1;
    int        i;

    if ((out = (char *)HDmalloc((size_t)(4 * 1000 * 1000))) == NULL)
        goto error;
    if ((in = (char *)HDmalloc((size_t)(4 * 1000 * 1000))) == NULL)
        goto error;

    TESTING("data type conversion");

    /* Initialize the dataset */
    for(i = 0; i < 1000000; i++) {
        out[i * 4 + 0] = 0x11;
        out[i * 4 + 1] = 0x22;
        out[i * 4 + 2] = 0x33;
        out[i * 4 + 3] = 0x44;
    } /* end for */

    /* Create the data space */
    dims[0] = 1000000;
    if((space = H5Screate_simple (1, dims, NULL)) < 0) goto error;

    /* Create the data set */
    if((dataset = H5Dcreate2(file, DSET_TCONV_NAME, H5T_STD_I32LE, space,
                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if(H5Dwrite(dataset, H5T_STD_I32LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out) < 0)
        goto error;

    /* Read data with byte order conversion */
    if(H5Dread(dataset, H5T_STD_I32BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, in) < 0)
        goto error;

    /* Check */
    for(i = 0; i < 1000000; i++) {
        if(in[4 * i + 0] != out[4 * i + 3] ||
                in[4 * i + 1] != out[4 * i + 2] ||
                in[4 * i + 2] != out[4 * i + 1] ||
                in[4 * i + 3] != out[4 * i + 0]) {
            H5_FAILED();
            HDputs("    Read with byte order conversion failed.");
            goto error;
        }
    }

    if(H5Dclose(dataset) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    HDfree(out);
    HDfree(in);

    HDputs(" PASSED");
    return SUCCEED;

error:
    if(out)
        HDfree(out);
    if(in)
        HDfree(in);

    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(space);
    } H5E_END_TRY;

    return FAIL;
} /* end test_tconv() */

/* This message derives from H5Z */
const H5Z_class2_t H5Z_BOGUS[1] = {{
    H5Z_CLASS_T_VERS,       /* H5Z_class_t version */
    H5Z_FILTER_BOGUS,        /* Filter id number        */
    1, 1,               /* Encoding and decoding enabled */
    "bogus",            /* Filter name for debugging    */
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_bogus,        /* The actual filter function    */
}};


/*-------------------------------------------------------------------------
 * Function:  can_apply_bogus
 *
 * Purpose:   A bogus 'can apply' callback that returns 0 for H5T_NATIVE_DOUBLE
 *            dataype, but returns 1 for all other datatypes
 *
 * Return:    Success:    Described above
 *            Failure:    0
 *-------------------------------------------------------------------------
 */
static htri_t
can_apply_bogus(hid_t H5_ATTR_UNUSED dcpl_id, hid_t type_id, hid_t H5_ATTR_UNUSED space_id)
{
    if(H5Tequal(type_id,H5T_NATIVE_DOUBLE))
        return 0;
    else if(H5Tequal(type_id,H5T_NATIVE_INT))
        return 1;
    else
        return -1;
} /* end can_apply_bogus() */


/*-------------------------------------------------------------------------
 * Function:  filter_bogus
 *
 * Purpose:   A bogus compression method that doesn't do anything.
 *
 * Return:    Success:    Data chunk size
 *            Failure:    0
 *-------------------------------------------------------------------------
 */
static size_t
filter_bogus(unsigned int H5_ATTR_UNUSED flags, size_t H5_ATTR_UNUSED cd_nelmts,
      const unsigned int H5_ATTR_UNUSED *cd_values, size_t nbytes,
      size_t H5_ATTR_UNUSED *buf_size, void H5_ATTR_UNUSED **buf)
{
    return nbytes;
} /* end filter_bogus() */


/*-------------------------------------------------------------------------
 * Function:  set_local_bogus2
 *
 * Purpose:   A 'set local' callback that stores the size of the datatype
 *            and adds it to all the H5T_NATIVE_INT values during
 *            filter operation.
 *
 * Return:    Success:    non-negative
 *            Failure:    negative
 *-------------------------------------------------------------------------
 */
static herr_t
set_local_bogus2(hid_t dcpl_id, hid_t type_id, hid_t H5_ATTR_UNUSED space_id)
{
    unsigned add_on=0;      /* Value to add to data going through */
    unsigned flags;         /* Filter flags */
    size_t cd_nelmts=BOGUS2_PERM_NPARMS;        /* Number of filter parameters */
    unsigned cd_values[4];  /* Filter parameters */

    /* Check for native integer datatype and set private property */
    if(H5Tequal(type_id,H5T_NATIVE_INT)>0)
        add_on=(unsigned)H5Tget_size(type_id);

    /* Get the filter's current parameters */
    if(H5Pget_filter_by_id2(dcpl_id, H5Z_FILTER_SET_LOCAL_TEST, &flags, &cd_nelmts, cd_values, (size_t)0, NULL, NULL) < 0)
        return(FAIL);

    /* Check that the parameter values were passed along correctly */
    if(cd_values[0]!=BOGUS2_PARAM_1)
        return(FAIL);
    if(cd_values[1]!=BOGUS2_PARAM_2)
        return(FAIL);

    /* Set "local" parameters for this dataset */
    cd_values[2]=(unsigned)(add_on>0);    /* Flag to indicate data is modified */
    cd_values[3]=add_on;        /* Amount the data was modified by */

    /* Modify the filter's parameters for this dataset */
    if(H5Pmodify_filter(dcpl_id, H5Z_FILTER_SET_LOCAL_TEST, flags, (size_t)BOGUS2_ALL_NPARMS,
            cd_values) < 0)
        return(FAIL);

    return(SUCCEED);
} /* end set_local_bogus2() */


/*-------------------------------------------------------------------------
 * Function:  filter_bogus2
 *
 * Purpose:   A filter method that adds a value to data values on writing
 *            (if the parameter is set), but does not modify data values on
 *            reading (so that correct operation of the filter can be
 *            checked).
 *
 * Return:    Success:    Data chunk size
 *            Failure:    0
 *-------------------------------------------------------------------------
 */
static size_t
filter_bogus2(unsigned int flags, size_t cd_nelmts,
      const unsigned int *cd_values, size_t nbytes,
      size_t *buf_size, void **buf)
{
    /* Check for the correct number of parameters */
    if(cd_nelmts!=BOGUS2_ALL_NPARMS)
        return(0);

    /* Check that permanent parameters are set correctly */
    if(cd_values[0]!=BOGUS2_PARAM_1)
        return(0);
    if(cd_values[1]!=BOGUS2_PARAM_2)
        return(0);

    /* Check if this filter is supposed to do something */
    if(cd_values[2]>0) {
        /* Check whether we are "uncompressing" */
        if(flags & H5Z_FLAG_REVERSE) {
            /* Do nothing */
        } /* end if */
        /* "Compressing" */
        else {
            unsigned add_on=cd_values[3];   /* Get "add on" value */
            int *int_ptr=(int *)*buf;          /* Pointer to the data values */
            size_t buf_left=*buf_size;  /* Amount of data buffer left to process */

            /* Add the "add on" value to all the data values */
            while(buf_left>0) {
                *int_ptr++ += (int)add_on;
                buf_left -= sizeof(int);
            } /* end while */
        } /* end else */

        return(nbytes);
    } /* end if */
    /* Filter is "no op" */
    else
        return(nbytes);
} /* end filter_bogus2() */


/*-------------------------------------------------------------------------
 * Function:  filter_bogus3
 *
 * Purpose:   A bogus compression method that returns a failure.
 *
 * Return:    Success:    Data chunk size
 *            Failure:    0
 *-------------------------------------------------------------------------
 */
static size_t
filter_bogus3(unsigned int H5_ATTR_UNUSED flags, size_t H5_ATTR_UNUSED cd_nelmts,
      const unsigned int H5_ATTR_UNUSED *cd_values, size_t H5_ATTR_UNUSED nbytes,
      size_t H5_ATTR_UNUSED *buf_size, void H5_ATTR_UNUSED **buf)
{
    return 0;
} /* end filter_bogus3() */

/* This message derives from H5Z */
const H5Z_class2_t H5Z_CORRUPT[1] = {{
    H5Z_CLASS_T_VERS,            /* H5Z_class_t version */
    H5Z_FILTER_CORRUPT,        /* Filter id number        */
    1, 1,               /* Encoding and decoding enabled */
    "corrupt",            /* Filter name for debugging    */
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_corrupt,        /* The actual filter function    */
}};


/*-------------------------------------------------------------------------
 * Function:  filter_corrupt
 *
 * Purpose:   For testing Fletcher32 checksum.  modify data slightly during
 *            writing so that when data is read back, the checksum should
 *            fail.
 *
 * Return:    Success:    Data chunk size
  *           Failure:    0
 *-------------------------------------------------------------------------
 */
static size_t
filter_corrupt(unsigned int flags, size_t cd_nelmts,
      const unsigned int *cd_values, size_t nbytes,
      size_t *buf_size, void **buf)
{
    void  *data = NULL;
    unsigned char  *dst = (unsigned char*)(*buf);
    unsigned int   offset;
    unsigned int   length;
    unsigned int   value;
    size_t         ret_value = 0;

    if(cd_nelmts != 3 || !cd_values)
        TEST_ERROR
    offset = cd_values[0];
    length = cd_values[1];
    value  = cd_values[2];
    if(offset > nbytes || (offset + length) > nbytes || length < sizeof(unsigned int))
        TEST_ERROR

    if(NULL == (data = HDmalloc((size_t)length)))
        TEST_ERROR
    HDmemset(data, (int)value, (size_t)length);

    if(flags & H5Z_FLAG_REVERSE) { /* Varify data is actually corrupted during read */
        dst += offset;
        if(HDmemcmp(data, dst, (size_t)length) != 0)
            TEST_ERROR
        else {
            *buf_size = nbytes;
            ret_value = nbytes;
        } /* end else */
    }  /* end if */
    else { /* Write corrupted data */
        dst += offset;
        HDmemcpy(dst, data, (size_t)length);
        *buf_size = nbytes;
        ret_value = *buf_size;
    } /* end else */

error:
    if(data)
        HDfree(data);

    return ret_value;
} /* end filter_corrupt() */


/*-------------------------------------------------------------------------
 * Function:    filter_cb_cont
 *
 * Purpose:     Callback function to handle checksum failure.  Let it continue.
 *
 * Return:      continue
 *-------------------------------------------------------------------------
 */
static H5Z_cb_return_t
filter_cb_cont(H5Z_filter_t filter, void H5_ATTR_UNUSED *buf, size_t H5_ATTR_UNUSED buf_size,
           void H5_ATTR_UNUSED *op_data)
{
    if(H5Z_FILTER_FLETCHER32==filter)
       return H5Z_CB_CONT;
    else
        return H5Z_CB_FAIL;
} /* end filter_cb_cont() */


/*-------------------------------------------------------------------------
 * Function:    filter_cb_fail
 *
 * Purpose:     Callback function to handle checksum failure.  Let it fail.
 *
 * Return:      fail
 *-------------------------------------------------------------------------
 */
static H5Z_cb_return_t
filter_cb_fail(H5Z_filter_t filter, void H5_ATTR_UNUSED *buf, size_t H5_ATTR_UNUSED buf_size,
           void H5_ATTR_UNUSED *op_data)
{
    if(H5Z_FILTER_FLETCHER32==filter)
       return H5Z_CB_FAIL;
    else
       return H5Z_CB_CONT;
} /* end filter_cb_fail() */


/*-------------------------------------------------------------------------
 * Function:  test_filter_internal
 *
 * Purpose:   Tests dataset compression. If compression is requested when
 *            it hasn't been compiled into the library (such as when
 *            updating an existing compressed dataset) then data is sent to
 *            the file uncompressed but no errors are returned.
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_filter_internal(hid_t fid, const char *name, hid_t dcpl, int if_fletcher32,
                     int corrupted, hsize_t *dset_size)
{
    hid_t            dataset;        /* Dataset ID */
    hid_t            dxpl;           /* Dataset xfer property list ID */
    hid_t            write_dxpl;     /* Dataset xfer property list ID for writing */
    hid_t            sid;            /* Dataspace ID */
    const hsize_t    size[2] = {DSET_DIM1, DSET_DIM2};           /* Dataspace dimensions */
    const hsize_t    hs_offset[2] = {FILTER_HS_OFFSET1, FILTER_HS_OFFSET2}; /* Hyperslab offset */
    const hsize_t    hs_size[2] = {FILTER_HS_SIZE1, FILTER_HS_SIZE2};   /* Hyperslab size */
    void            *tconv_buf = NULL;      /* Temporary conversion buffer */
    size_t           i, j, n;        /* Local index variables */
    herr_t           status;         /* Error status */

    /* Create the data space */
    if((sid = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /*
     * Create a small conversion buffer to test strip mining. We
     * might as well test all we can!
     */
    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0) goto error;
    tconv_buf = HDmalloc((size_t)1000);
    if(H5Pset_buffer(dxpl, (size_t)1000, tconv_buf, NULL) < 0) goto error;
    if((write_dxpl = H5Pcopy(dxpl)) < 0) TEST_ERROR;

    if(if_fletcher32==DISABLE_FLETCHER32) {
        if(H5Pset_edc_check(dxpl, H5Z_DISABLE_EDC) < 0)
            goto error;
        if(H5Z_DISABLE_EDC != H5Pget_edc_check(dxpl))
            goto error;
    }

    TESTING("    filters (setup)");

    /* Check if all the filters are available */
    if(H5Pall_filters_avail(dcpl)!=TRUE) {
        H5_FAILED();
        HDprintf("    Line %d: Incorrect filter availability\n",__LINE__);
        goto error;
    } /* end if */

    /* Create the dataset */
    if((dataset = H5Dcreate2(fid, name, H5T_NATIVE_INT, sid, H5P_DEFAULT,
                dcpl, H5P_DEFAULT)) < 0) goto error;

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Read uninitialized data.  It should be zero.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (uninitialized read)");

    if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
    TEST_ERROR;

    for(i=0; i<(size_t)size[0]; i++) {
    for(j=0; j<(size_t)size[1]; j++) {
        if(0!=check[i][j]) {
        H5_FAILED();
        HDprintf("    Read a non-zero value.\n");
        HDprintf("    At index %lu,%lu\n",
            (unsigned long)i, (unsigned long)j);
        goto error;
        }
    }
    }
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Test filters by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (write)");

    for(i=n=0; i<size[0]; i++) {
        for(j=0; j<size[1]; j++) {
            points[i][j] = (int)(n++);
        }
    }

    if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, points) < 0)
        TEST_ERROR;

    if((*dset_size=H5Dget_storage_size(dataset))==0) TEST_ERROR;

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 3: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (read)");

    /* Read the dataset back */
    if(corrupted) {
        /* Default behavior is failure when data is corrupted. */
        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status=H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status>=0) TEST_ERROR;

        /* Callback decides to continue inspite data is corrupted. */
        if(H5Pset_filter_callback(dxpl, filter_cb_cont, NULL) < 0) TEST_ERROR;
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
            TEST_ERROR;

        /* Callback decides to fail when data is corrupted. */
        if(H5Pset_filter_callback(write_dxpl, filter_cb_fail, NULL) < 0) TEST_ERROR;
        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status=H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status>=0) TEST_ERROR;
    }
    else {
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
            TEST_ERROR;

        /* Check that the values read are the same as the values written */
        for(i=0; i<size[0]; i++) {
            for(j=0; j<size[1]; j++) {
                if(points[i][j] != check[i][j]) {
                    H5_FAILED();
                    HDfprintf(stderr,"    Read different values than written.\n");
                    HDfprintf(stderr,"    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                    HDfprintf(stderr,"    At original: %d\n", (int)points[i][j]);
                    HDfprintf(stderr,"    At returned: %d\n", (int)check[i][j]);
                    goto error;
                }
            }
        }
    }

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 4: Write new data over the top of the old data.  The new data is
     * random thus not very compressible, and will cause the chunks to move
     * around as they grow.  We only change values for the left half of the
     * dataset although we rewrite the whole thing.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (modify)");

    for(i=0; i<size[0]; i++) {
        for(j=0; j<size[1]/2; j++) {
            points[i][j] = (int)HDrandom ();
        }
    }
    if(H5Dwrite (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, points) < 0)
        TEST_ERROR;

    if(corrupted) {
        /* Default behavior is failure when data is corrupted. */
        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status=H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status>=0) TEST_ERROR;

        /* Callback decides to continue inspite data is corrupted. */
        if(H5Pset_filter_callback(dxpl, filter_cb_cont, NULL) < 0) TEST_ERROR;
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
            TEST_ERROR;

        /* Callback decides to fail when data is corrupted. */
        if(H5Pset_filter_callback(write_dxpl, filter_cb_fail, NULL) < 0) TEST_ERROR;
        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status=H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status>=0) TEST_ERROR;
    }
    else {
        /* Read the dataset back and check it */
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
            TEST_ERROR;

        /* Check that the values read are the same as the values written */
        for(i=0; i<size[0]; i++) {
            for(j=0; j<size[1]; j++) {
                if(points[i][j] != check[i][j]) {
                    H5_FAILED();
                    HDprintf("    Read different values than written.\n");
                    HDprintf("    At index %lu,%lu\n",
                            (unsigned long)i, (unsigned long)j);
                    goto error;
                }
            }
        }
    }

    if((*dset_size=H5Dget_storage_size(dataset))==0) TEST_ERROR;
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 5: Close the dataset and then open it and read it again.  This
     * insures that the filters message is picked up properly from the
     * object header.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (re-open)");

    if(H5Dclose(dataset) < 0) TEST_ERROR;
    if((dataset = H5Dopen2(fid, name, H5P_DEFAULT)) < 0) TEST_ERROR;

    if(corrupted) {
        /* Default behavior is failure when data is corrupted. */
        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status >= 0) TEST_ERROR;

        /* Callback decides to continue inspite data is corrupted. */
        if(H5Pset_filter_callback(dxpl, filter_cb_cont, NULL) < 0) TEST_ERROR;
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
            TEST_ERROR;

        /* Callback decides to fail when data is corrupted. */
        if(H5Pset_filter_callback(write_dxpl, filter_cb_fail, NULL) < 0) TEST_ERROR;

        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status >= 0) TEST_ERROR;
    } /* end if */
    else {
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
            TEST_ERROR;

        /* Check that the values read are the same as the values written */
        for(i = 0; i < size[0]; i++)
            for(j = 0; j < size[1]; j++)
                if(points[i][j] != check[i][j]) {
                    H5_FAILED();
                    HDprintf("    Read different values than written.\n");
                    HDprintf("    At index %lu,%lu\n",
                            (unsigned long)i, (unsigned long)j);
                    goto error;
                } /* end if */
    } /* end else */

    PASSED();


    /*----------------------------------------------------------------------
     * STEP 6: Test partial I/O by writing to and then reading from a
     * hyperslab of the dataset.  The hyperslab does not line up on chunk
     * boundaries (we know that case already works from above tests).
     *----------------------------------------------------------------------
     */
    TESTING("    filters (partial I/O)");

    for(i=0; i<(size_t)hs_size[0]; i++) {
        for(j=0; j<(size_t)hs_size[1]; j++) {
            points[(size_t)hs_offset[0]+i][(size_t)hs_offset[1]+j] = (int)HDrandom();
        }
    }
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, hs_offset, NULL, hs_size,
                NULL) < 0) TEST_ERROR;
    /* (Use the "read" DXPL because partial I/O on corrupted data test needs to ignore errors during writing) */
    if(H5Dwrite (dataset, H5T_NATIVE_INT, sid, sid, dxpl, points) < 0)
    TEST_ERROR;

    if(corrupted) {
        /* Default behavior is failure when data is corrupted. */
        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status=H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status>=0) TEST_ERROR;

        /* Callback decides to continue inspite data is corrupted. */
        if(H5Pset_filter_callback(dxpl, filter_cb_cont, NULL) < 0) TEST_ERROR;
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
            TEST_ERROR;

        /* Callback decides to fail when data is corrupted. */
        if(H5Pset_filter_callback(write_dxpl, filter_cb_fail, NULL) < 0) TEST_ERROR;
        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status=H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status>=0) TEST_ERROR;
    }
    else {
        if(H5Dread (dataset, H5T_NATIVE_INT, sid, sid, dxpl, check) < 0)
            TEST_ERROR;

        /* Check that the values read are the same as the values written */
        for(i=0; i<(size_t)hs_size[0]; i++) {
            for(j=0; j<(size_t)hs_size[1]; j++) {
                if(points[(size_t)hs_offset[0]+i][(size_t)hs_offset[1]+j] !=
                            check[(size_t)hs_offset[0]+i][(size_t)hs_offset[1]+j]) {
                    H5_FAILED();
                    HDfprintf(stderr,"    Read different values than written.\n");
                    HDfprintf(stderr,"    At index %lu,%lu\n",
                            (unsigned long)((size_t)hs_offset[0]+i),
                            (unsigned long)((size_t)hs_offset[1]+j));
                    HDfprintf(stderr,"    At original: %d\n",
                            (int)points[(size_t)hs_offset[0]+i][(size_t)hs_offset[1]+j]);
                    HDfprintf(stderr,"    At returned: %d\n",
                            (int)check[(size_t)hs_offset[0]+i][(size_t)hs_offset[1]+j]);
                    goto error;
                }
            }
        }
    }

    /* Get the storage size of the dataset */
    if((*dset_size=H5Dget_storage_size(dataset))==0) goto error;

    PASSED();

    /* Clean up objects used for this test */
    if(H5Dclose (dataset) < 0) goto error;
    if(H5Sclose (sid) < 0) goto error;
    if(H5Pclose (dxpl) < 0) goto error;
    if(H5Pclose (write_dxpl) < 0) goto error;
    HDfree (tconv_buf);

    return SUCCEED;

error:
    if(tconv_buf)
        HDfree (tconv_buf);
    return FAIL;
} /* end test_filter_internal() */

/*-------------------------------------------------------------------------
 * Function:  test_filter_noencoder
 *
 * Purpose:   Tests filters with no encoder present.  Ensures that data
 *            can still be decoded correctly and that errors are thrown
 *            when the application tries to write.
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_FILTER_SZIP
static herr_t
test_filter_noencoder(const char *dset_name)
{
    hid_t file_id = -1;
    hid_t dset_id = -1;
    hid_t test_dset_id = -1;
    hid_t dcpl_id = -1;
    hid_t space_id = -1;
    hsize_t dims = 10;
    herr_t err;
    int test_ints[10] = { 12 };
    int read_buf[10];
    int i;

    /* Make a local copy of the file since this test writes to the data file
       from svn. */
    if (h5_make_local_copy(NOENCODER_FILENAME, NOENCODER_COPY_FILENAME) < 0)
        goto error;

    /* Open file */
    file_id = H5Fopen(NOENCODER_COPY_FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    if(file_id < 0) goto error;

    dset_id = H5Dopen2(file_id, dset_name, H5P_DEFAULT);
    if(dset_id < 0) goto error;

    space_id = H5Screate_simple(1, &dims, NULL);
    if(space_id < 0) goto error;

    TESTING("    decoding without encoder");

    /* Read the dataset and make sure the decoder is working correctly */
    err = H5Dread(dset_id, H5T_NATIVE_INT, space_id, space_id, H5P_DEFAULT, read_buf);
    if(err < 0) goto error;

    for(i = 0; i < 10; i++)
        if(read_buf[i] != i)
            goto error;

    H5Sclose(space_id);

    PASSED();

    /* Attempt to copy the DCPL and use it to create a new dataset.
     * Since the filter does not have an encoder, the creation
     * should fail.
     */
    TESTING("    trying to write without encoder");

    dcpl_id = H5Dget_create_plist(dset_id);
    if(dcpl_id < 0) goto error;

    space_id = H5Screate_simple(1, &dims, NULL);
    if(space_id < 0) goto error;

    H5E_BEGIN_TRY{
    test_dset_id = H5Dcreate2(file_id, NOENCODER_TEST_DATASET, H5T_NATIVE_INT, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    }H5E_END_TRY

    if(test_dset_id >= 0) goto error;

    /* Attempt to extend the dataset.  This should fail because
     * the dataset has a fill value and is instructed to fill on
     * allocation.
     */
    dims = 20; /* Dataset is originally of size 10 */
    H5E_BEGIN_TRY{
        err = H5Dset_extent(dset_id, &dims);
    }H5E_END_TRY

    if(err >= 0) goto error;

    /* Attempt to write to the dataset.  This should fail because
     * the filter does not have an encoder.
     */
    H5E_BEGIN_TRY{
        err = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, test_ints);
    }H5E_END_TRY

    if(err >= 0) goto error;

    H5Fclose(file_id);
    H5Dclose(dset_id);
    H5Sclose(space_id);
    H5Pclose(dcpl_id);

    PASSED();

    return SUCCEED;

error:
    H5_FAILED();
    if(dset_id != -1)
        H5Dclose(dset_id);
    if(test_dset_id != -1)
        H5Dclose(test_dset_id);
    if(space_id != -1)
        H5Sclose(space_id);
    if(dcpl_id != -1)
        H5Pclose(dcpl_id);
    if(file_id != -1)
        H5Fclose(file_id);

    return FAIL;
} /* end test_filter_noencoder() */
#endif /* H5_HAVE_FILTER_SZIP */

/*-------------------------------------------------------------------------
 * Function:    test_get_filter_info
 *
 * Purpose:     Tests the H5Zget_filter_info function.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_get_filter_info(void)
{
  unsigned int flags;  /* flags returned from H5Zget_filter_info */
  herr_t err;

  TESTING("H5Zget_filter_info");

  /* Verify that each filter is reported as having the right combination
   * of encoder and decoder.
   */
  if(H5Zget_filter_info(H5Z_FILTER_FLETCHER32, &flags) < 0) TEST_ERROR

  if(((flags & H5Z_FILTER_CONFIG_ENCODE_ENABLED) == 0) ||
     ((flags & H5Z_FILTER_CONFIG_DECODE_ENABLED) == 0))
      TEST_ERROR

  if(H5Zget_filter_info(H5Z_FILTER_SHUFFLE, &flags) < 0) TEST_ERROR

  if(((flags & H5Z_FILTER_CONFIG_ENCODE_ENABLED) == 0) ||
     ((flags & H5Z_FILTER_CONFIG_DECODE_ENABLED) == 0))
      TEST_ERROR

#ifdef H5_HAVE_FILTER_DEFLATE
  if(H5Zget_filter_info(H5Z_FILTER_DEFLATE, &flags) < 0) TEST_ERROR

  if(((flags & H5Z_FILTER_CONFIG_ENCODE_ENABLED) == 0) ||
     ((flags & H5Z_FILTER_CONFIG_DECODE_ENABLED) == 0))
      TEST_ERROR
#endif

#ifdef H5_HAVE_FILTER_SZIP
    if(H5Zget_filter_info(H5Z_FILTER_SZIP, &flags) < 0) TEST_ERROR

    if(H5Z_SZIP->encoder_present) {
        if(((flags & H5Z_FILTER_CONFIG_ENCODE_ENABLED) == 0) ||
                ((flags & H5Z_FILTER_CONFIG_DECODE_ENABLED) == 0))
            TEST_ERROR
    } /* end if */
    else {
        if(((flags & H5Z_FILTER_CONFIG_ENCODE_ENABLED) != 0) ||
                ((flags & H5Z_FILTER_CONFIG_DECODE_ENABLED) == 0))
            TEST_ERROR
    } /* end else */
#endif /* H5_HAVE_FILTER_SZIP */

  /* Verify that get_filter_info throws an error when given a bad filter */
  /* (Depends on 1.6 compatibility flag) */
  H5E_BEGIN_TRY {
    err = H5Zget_filter_info(-1, &flags);
  } H5E_END_TRY;
  if(err >= 0) TEST_ERROR

  PASSED();
  return SUCCEED;

error:
  return FAIL;
} /* end test_get_filter_info() */

/*-------------------------------------------------------------------------
 * Function:  test_filters
 *
 * Purpose:   Tests dataset filter.
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_filters(hid_t file, hid_t
#ifndef H5_HAVE_FILTER_SZIP
H5_ATTR_UNUSED
#endif /* H5_HAVE_FILTER_SZIP */
    fapl)
{
    hid_t       dc;                  /* Dataset creation property list ID */
    const hsize_t chunk_size[2] = {FILTER_CHUNK_DIM1, FILTER_CHUNK_DIM2};  /* Chunk dimensions */
    hsize_t     null_size;           /* Size of dataset with null filter */

    hsize_t     fletcher32_size;     /* Size of dataset with Fletcher32 checksum */
    unsigned    data_corrupt[3];     /* position and length of data to be corrupted */

#ifdef H5_HAVE_FILTER_DEFLATE
    hsize_t     deflate_size;        /* Size of dataset with deflate filter */
#endif /* H5_HAVE_FILTER_DEFLATE */

#ifdef H5_HAVE_FILTER_SZIP
    hsize_t     szip_size;           /* Size of dataset with szip filter */
    unsigned    szip_options_mask = H5_SZIP_NN_OPTION_MASK;
    unsigned    szip_pixels_per_block = 4;
#endif /* H5_HAVE_FILTER_SZIP */

    hsize_t     shuffle_size;       /* Size of dataset with shuffle filter */

#if(defined H5_HAVE_FILTER_DEFLATE | defined H5_HAVE_FILTER_SZIP)
    hsize_t     combo_size;         /* Size of dataset with multiple filters */
#endif /* defined H5_HAVE_FILTER_DEFLATE | defined H5_HAVE_FILTER_SZIP */

    /* test the H5Zget_filter_info function */
    if(test_get_filter_info() < 0) goto error;

    /*----------------------------------------------------------
     * STEP 0: Test null I/O filter by itself.
     *----------------------------------------------------------
     */
    HDputs("Testing 'null' filter");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Zregister (H5Z_BOGUS) < 0) goto error;
    if(H5Pset_filter(dc, H5Z_FILTER_BOGUS, 0, (size_t)0, NULL) < 0) goto error;

    if(test_filter_internal(file,DSET_BOGUS_NAME,dc,DISABLE_FLETCHER32,DATA_NOT_CORRUPTED,&null_size) < 0) goto error;

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;

    /*----------------------------------------------------------
     * STEP 1: Test Fletcher32 Checksum by itself.
     *----------------------------------------------------------
     */
    HDputs("Testing Fletcher32 checksum(enabled for read)");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_filter(dc, H5Z_FILTER_FLETCHER32, 0, (size_t)0, NULL) < 0) goto error;

    /* Enable checksum during read */
    if(test_filter_internal(file,DSET_FLETCHER32_NAME,dc,ENABLE_FLETCHER32,DATA_NOT_CORRUPTED,&fletcher32_size) < 0) goto error;
    if(fletcher32_size<=null_size) {
        H5_FAILED();
        HDputs("    Size after checksumming is incorrect.");
        goto error;
    } /* end if */

    /* Disable checksum during read */
    HDputs("Testing Fletcher32 checksum(disabled for read)");
    if(test_filter_internal(file,DSET_FLETCHER32_NAME_2,dc,DISABLE_FLETCHER32,DATA_NOT_CORRUPTED,&fletcher32_size) < 0) goto error;
    if(fletcher32_size<=null_size) {
        H5_FAILED();
        HDputs("    Size after checksumming is incorrect.");
        goto error;
    } /* end if */

    /* Try to corrupt data and see if checksum fails */
    HDputs("Testing Fletcher32 checksum(when data is corrupted)");
    data_corrupt[0] = 52;
    data_corrupt[1] = 33;
    data_corrupt[2] = 27;

    if(H5Zregister (H5Z_CORRUPT) < 0) goto error;
    if(H5Pset_filter(dc, H5Z_FILTER_CORRUPT, 0, (size_t)3, data_corrupt) < 0) goto error;
    if(test_filter_internal(file,DSET_FLETCHER32_NAME_3,dc,DISABLE_FLETCHER32,DATA_CORRUPTED,&fletcher32_size) < 0) goto error;
    if(fletcher32_size<=null_size) {
        H5_FAILED();
        HDputs("    Size after checksumming is incorrect.");
        goto error;
    } /* end if */

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;


    /*----------------------------------------------------------
     * STEP 2: Test deflation by itself.
     *----------------------------------------------------------
     */
#ifdef H5_HAVE_FILTER_DEFLATE
    HDputs("Testing deflate filter");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_deflate (dc, 6) < 0) goto error;

    if(test_filter_internal(file,DSET_DEFLATE_NAME,dc,DISABLE_FLETCHER32,DATA_NOT_CORRUPTED,&deflate_size) < 0) goto error;
    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;
#else /* H5_HAVE_FILTER_DEFLATE */
    TESTING("deflate filter");
    SKIPPED();
    HDputs("    Deflate filter not enabled");
#endif /* H5_HAVE_FILTER_DEFLATE */

    /*----------------------------------------------------------
     * STEP 3: Test szip compression by itself.
     *----------------------------------------------------------
     */
#ifdef H5_HAVE_FILTER_SZIP
    TESTING("szip filter (with encoder)");
    if( h5_szip_can_encode() == 1) {
        if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
        if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;

    HDputs("");
    if(H5Pset_szip(dc, szip_options_mask, szip_pixels_per_block) < 0) goto error;
    if(test_filter_internal(file,DSET_SZIP_NAME,dc,DISABLE_FLETCHER32,DATA_NOT_CORRUPTED,&szip_size) < 0) goto error;
        if(H5Pclose (dc) < 0) goto error;
    }
    else {
        SKIPPED();
    }

    TESTING("szip filter (without encoder)");

    if( h5_szip_can_encode() != 1) {
        HDputs("");
        if(test_filter_noencoder(NOENCODER_SZIP_DATASET) < 0) goto error;
    }
    else {
        SKIPPED();
    }

#else /* H5_HAVE_FILTER_SZIP */
    TESTING("szip filter");
    SKIPPED();
    HDputs("    Szip filter not enabled");
#endif /* H5_HAVE_FILTER_SZIP */

    /*----------------------------------------------------------
     * STEP 4: Test shuffling by itself.
     *----------------------------------------------------------
     */
    HDputs("Testing shuffle filter");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_shuffle (dc) < 0) goto error;

    if(test_filter_internal(file,DSET_SHUFFLE_NAME,dc,DISABLE_FLETCHER32,DATA_NOT_CORRUPTED,&shuffle_size) < 0) goto error;
    if(shuffle_size!=null_size) {
        H5_FAILED();
        HDputs("    Shuffled size not the same as uncompressed size.");
        goto error;
    } /* end if */

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;

    /*----------------------------------------------------------
     * STEP 5: Test shuffle + deflate + checksum in any order.
     *----------------------------------------------------------
     */
#ifdef H5_HAVE_FILTER_DEFLATE
    HDputs("Testing shuffle+deflate+checksum filters(checksum first)");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_fletcher32 (dc) < 0) goto error;
    if(H5Pset_shuffle (dc) < 0) goto error;
    if(H5Pset_deflate (dc, 6) < 0) goto error;

    if(test_filter_internal(file,DSET_SHUF_DEF_FLET_NAME,dc,ENABLE_FLETCHER32,DATA_NOT_CORRUPTED,&combo_size) < 0) goto error;

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;

    HDputs("Testing shuffle+deflate+checksum filters(checksum last)");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_shuffle (dc) < 0) goto error;
    if(H5Pset_deflate (dc, 6) < 0) goto error;
    if(H5Pset_fletcher32 (dc) < 0) goto error;

    if(test_filter_internal(file,DSET_SHUF_DEF_FLET_NAME_2,dc,ENABLE_FLETCHER32,DATA_NOT_CORRUPTED,&combo_size) < 0) goto error;

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;
#else /* H5_HAVE_FILTER_DEFLATE */
    TESTING("shuffle+deflate+fletcher32 filters");
    SKIPPED();
    HDputs("    Deflate filter not enabled");
#endif /* H5_HAVE_FILTER_DEFLATE */

    /*----------------------------------------------------------
     * STEP 6: Test shuffle + szip + checksum in any order.
     *----------------------------------------------------------
     */
#ifdef H5_HAVE_FILTER_SZIP

    TESTING("shuffle+szip+checksum filters(checksum first, with encoder)");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_fletcher32 (dc) < 0) goto error;
    if(H5Pset_shuffle (dc) < 0) goto error;

    /* Make sure encoding is enabled */
    if( h5_szip_can_encode() == 1) {
        HDputs("");
        if(H5Pset_szip(dc, szip_options_mask, szip_pixels_per_block) < 0) goto error;
        if(test_filter_internal(file,DSET_SHUF_SZIP_FLET_NAME,dc,ENABLE_FLETCHER32,DATA_NOT_CORRUPTED,&combo_size) < 0) goto error;
    }
    else {
        SKIPPED();
    }

    TESTING("shuffle+szip+checksum filters(checksum first, without encoder)");

    if( h5_szip_can_encode() != 1) {
        HDputs("");
        if(test_filter_noencoder(NOENCODER_SZIP_SHUFF_FLETCH_DATASET) < 0) goto error;
    }
    else {
        SKIPPED();
    }

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;

    TESTING("shuffle+szip+checksum filters(checksum last, with encoder)");

    /* Make sure encoding is enabled */
    if( h5_szip_can_encode() == 1) {
        HDputs("");
        if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
        if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
        if(H5Pset_shuffle (dc) < 0) goto error;
        if(H5Pset_szip(dc, szip_options_mask, szip_pixels_per_block) < 0) goto error;
        if(H5Pset_fletcher32 (dc) < 0) goto error;

        if(test_filter_internal(file,DSET_SHUF_SZIP_FLET_NAME_2,dc,ENABLE_FLETCHER32,DATA_NOT_CORRUPTED,&combo_size) < 0) goto error;

        /* Clean up objects used for this test */
        if(H5Pclose (dc) < 0) goto error;
    }
    else {
        SKIPPED();
    }

#else /* H5_HAVE_FILTER_SZIP */
    TESTING("shuffle+szip+fletcher32 filters");
    SKIPPED();
    HDputs("    szip filter not enabled");
#endif /* H5_HAVE_FILTER_SZIP */
    return SUCCEED;

error:
    return FAIL;
} /* end test_filters() */


/*-------------------------------------------------------------------------
 * Function:  test_missing_filter
 *
 * Purpose:   Tests library behavior when filter is missing
 *
 * Return:    SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
test_missing_filter(hid_t file)
{
    hid_t       fid;            /* File ID */
    hid_t       dsid;           /* Dataset ID */
    hid_t       sid;            /* Dataspace ID */
    hid_t       dcpl;           /* Dataspace creation property list ID */
    const hsize_t dims[2] = {DSET_DIM1, DSET_DIM2};         /* Dataspace dimensions */
    const hsize_t chunk_dims[2] = {2, 25};      /* Chunk dimensions */
    hsize_t     dset_size;      /* Dataset size */
    size_t      i,j;            /* Local index variables */
    herr_t      ret;            /* Generic return value */
    const char *testfile = H5_get_srcdir_filename(FILE_DEFLATE_NAME); /* Corrected test file name */
    hbool_t     api_ctx_pushed = FALSE;             /* Whether API context pushed */

    TESTING("dataset access with missing filter");

    /* Unregister the deflate filter */
#ifdef H5_HAVE_FILTER_DEFLATE
        /* Verify deflate filter is registered currently */
        if(H5Zfilter_avail(H5Z_FILTER_DEFLATE)!=TRUE) {
            H5_FAILED();
            HDprintf("    Line %d: Deflate filter not available\n",__LINE__);
            goto error;
        } /* end if */

        /* Push API context */
        if(H5CX_push() < 0) FAIL_STACK_ERROR
        api_ctx_pushed = TRUE;

        /* Unregister deflate filter */
        /* (Use private routine, to avoid range checking on filter ID) */
        if(H5Z__unregister(H5Z_FILTER_DEFLATE) < 0) {
            H5_FAILED();
            HDprintf("    Line %d: Can't unregister deflate filter\n",__LINE__);
            goto error;
        } /* end if */
#endif /* H5_HAVE_FILTER_DEFLATE */
        /* Verify deflate filter is not registered currently */
        if(H5Zfilter_avail(H5Z_FILTER_DEFLATE)!=FALSE) {
            H5_FAILED();
            HDprintf("    Line %d: Deflate filter available\n",__LINE__);
            goto error;
        } /* end if */

    /* Create dcpl with deflate filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't create dcpl\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't set chunk sizes\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_deflate(dcpl, 9) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't set deflate filter\n",__LINE__);
        goto error;
    } /* end if */

    /* Check if all the filters are available */
    ret=H5Pall_filters_avail(dcpl);
    if(ret<0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't check filter availability\n",__LINE__);
        goto error;
    } /* end if */
    if(ret!=FALSE) {
        H5_FAILED();
        HDprintf("    Line %d: Filter shouldn't be available\n",__LINE__);
        goto error;
    } /* end if */

    /* Create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't open dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Create new dataset */
    if((dsid = H5Dcreate2(file, DSET_MISSING_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't create dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Write data */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Error writing dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Flush the file (to clear the cache) */
    if(H5Fflush(file, H5F_SCOPE_GLOBAL) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Error flushing file\n",__LINE__);
        goto error;
    } /* end if */

    /* Query the dataset's size on disk */
    if(0 == (dset_size = H5Dget_storage_size(dsid))) {
        H5_FAILED();
        HDprintf("    Line %d: Error querying dataset size, dset_size=%lu\n",__LINE__,(unsigned long)dset_size);
        goto error;
    } /* end if */

    /* Verify that the size indicates data is uncompressed */
    /* (i.e. the deflation filter we asked for was silently ignored) */
    if((H5Tget_size(H5T_NATIVE_INT) * DSET_DIM1 * DSET_DIM2) != dset_size) {
        H5_FAILED();
        HDprintf("    Line %d: Incorrect dataset size: %lu\n",__LINE__,(unsigned long)dset_size);
        goto error;
    } /* end if */

    /* Read data */
    if(H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Error reading dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Compare data */
    /* Check that the values read are the same as the values written */
    for(i=0; i<(size_t)dims[0]; i++) {
        for(j=0; j<(size_t)dims[1]; j++) {
            if(points[i][j] != check[i][j]) {
                H5_FAILED();
                HDprintf("    Line %d: Read different values than written.\n",__LINE__);
                HDprintf("    At index %lu,%lu\n", (unsigned long)(i), (unsigned long)(j));
                HDprintf("    At original: %d\n",points[i][j]);
                HDprintf("    At returned: %d\n",check[i][j]);
                goto error;
            } /* end if */
        } /* end for */
    } /* end for */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataspace */
    if(H5Sclose(sid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset creation property list */
    if(H5Pclose(dcpl) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dcpl\n",__LINE__);
        goto error;
    } /* end if */


    /* Try reading existing dataset with deflate filter */

    /* Open existing file */
    if((fid = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't open existing deflated file\n", __LINE__);
        goto error;
    } /* end if */

    /* Open dataset */
    if((dsid = H5Dopen2(fid, "Dataset1", H5P_DEFAULT)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't open dataset\n", __LINE__);
        goto error;
    } /* end if */

    /* Read data (should fail, since deflate filter is missing) */
    H5E_BEGIN_TRY {
        ret = H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check);
    } H5E_END_TRY;
    if(ret>=0) {
        H5_FAILED();
        HDprintf("    Line %d: Should not be able to read dataset data\n", __LINE__);
        goto error;
    } /* end if */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dataset\n", __LINE__);
        goto error;
    } /* end if */

    /* Close existing file */
    if(H5Fclose(fid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close file\n", __LINE__);
        goto error;
    } /* end if */

    /* Re-register the deflate filter */
        /* Verify deflate filter is not registered currently */
        if(H5Zfilter_avail(H5Z_FILTER_DEFLATE)!=FALSE) {
            H5_FAILED();
            HDprintf("    Line %d: Deflate filter available\n",__LINE__);
            goto error;
        } /* end if */
#ifdef H5_HAVE_FILTER_DEFLATE
        /* Register deflate filter (use internal function to avoid range checks) */
        if(H5Z_register(H5Z_DEFLATE) < 0) {
            H5_FAILED();
            HDprintf("    Line %d: Can't unregister deflate filter\n",__LINE__);
            goto error;
        } /* end if */

        /* Verify deflate filter is registered currently */
        if(H5Zfilter_avail(H5Z_FILTER_DEFLATE)!=TRUE) {
            H5_FAILED();
            HDprintf("    Line %d: Deflate filter not available\n",__LINE__);
            goto error;
        } /* end if */
#endif /* H5_HAVE_FILTER_DEFLATE */

    /* Pop API context */
    if(api_ctx_pushed && H5CX_pop() < 0) FAIL_STACK_ERROR
    api_ctx_pushed = FALSE;

    PASSED();
    return SUCCEED;

error:
    if(api_ctx_pushed) H5CX_pop();

    return FAIL;
} /* end test_missing_filter() */


/*-------------------------------------------------------------------------
 * Function:  test_onebyte_shuffle
 *
 * Purpose:   Tests the 8-bit array with shuffling algorithm.
 *            The shuffled array  should be the same result as
 *            that before the shuffling.
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_onebyte_shuffle(hid_t file)
{
    hid_t            dataset, space,dc;
    const hsize_t    size[2] = {10, 20};
    const hsize_t    chunk_size[2] = {10, 20};
    unsigned char    orig_data[10][20];
    unsigned char    new_data[10][20];
    size_t           i, j;

    TESTING("8-bit shuffling (setup)");

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Use shuffling algorithm with 8-bit  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_shuffle (dc) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_ONEBYTE_SHUF_NAME, H5T_NATIVE_UCHAR,
                space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    for(i= 0;i< 10; i++)
        for(j = 0; j < 20; j++)
            orig_data[i][j] = (unsigned char)HDrandom();

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Test shuffling by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("8-bit shuffling (write)");

    if(H5Dwrite(dataset, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig_data) < 0)
        goto error;

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("8-bit shuffling (read)");

    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, new_data) < 0)
        goto error;

    /* Check that the values read are the same as the values written */
    for(i=0; i<(size_t)size[0]; i++) {
        for(j=0; j<(size_t)size[1]; j++) {
            if(new_data[i][j] != orig_data[i][j]) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %lu,%lu\n",
                    (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Pclose (dc) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();

    return SUCCEED;

error:
    return FAIL;
} /* end test_onebyte_shuffle() */


/*-------------------------------------------------------------------------
 * Function:    test_nbit_int
 *
 * Purpose:     Tests the integer datatype for nbit filter
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_int(hid_t file)
{
    hid_t               dataset, datatype, mem_datatype, space, dc;
    hsize_t             size[2] = {2, 5};
    hsize_t             chunk_size[2] = {2,5};
    int                 orig_data[2][5];
    int                 new_data[2][5];
    unsigned int        mask;
    size_t              precision, offset;
    double              power;
    size_t              i, j;

    HDputs("Testing nbit filter");
    TESTING("    nbit int (setup)");

    /* Define dataset datatype (integer), and set precision, offset */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    precision = 17; /* precision includes sign bit */
    if(H5Tset_precision(datatype,precision) < 0) goto error;
    offset = 4;
    if(H5Tset_offset(datatype,offset) < 0) goto error;

    /* Copy to memory datatype before setting order */
    mem_datatype = H5Tcopy(datatype);

    /* Set order of dataset datatype */
    if(H5Tset_order(datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Use nbit filter  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_nbit(dc) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_NBIT_INT_NAME, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Initialize data, assuming size of long long >= size of int */
    for(i= 0;i< (size_t)size[0]; i++)
        for(j = 0; j < (size_t)size[1]; j++) {
            power = HDpow(2.0f, (double)(precision - 1));
            orig_data[i][j] = (int)(((long long)HDrandom() % (long long)power) << offset);

            /* even-numbered values are negtive */
            if((i*size[1]+j+1)%2 == 0)
                orig_data[i][j] = -orig_data[i][j];
        }

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Test nbit by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit int (write)");

    if(H5Dwrite(dataset, mem_datatype, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig_data) < 0)
        goto error;
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit int (read)");

    /* Read the dataset back */
    if(H5Dread(dataset, mem_datatype, H5S_ALL, H5S_ALL, H5P_DEFAULT, new_data) < 0)
        goto error;

    /* Check that the values read are the same as the values written
     * Use mask for checking the significant bits, ignoring the padding bits
     */
    mask = ~((unsigned)~0 << (precision + offset)) & ((unsigned)~0 << offset);
    for(i=0; i<(size_t)size[0]; i++) {
        for(j=0; j<(size_t)size[1]; j++) {
            if(((unsigned)new_data[i][j] & mask) != ((unsigned)orig_data[i][j] & mask)) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Tclose(mem_datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();

    return SUCCEED;

error:
    return FAIL;
} /* end test_nbit_int() */


/*-------------------------------------------------------------------------
 * Function:    test_nbit_float
 *
 * Purpose:     Tests the float datatype of nbit filter
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_float(hid_t file)
{
    hid_t               dataset, datatype, space, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2, 5};
    /* orig_data[] are initialized to be within the range that can be represented by
     * dataset datatype (no precision loss during datatype conversion)
     */
    float               orig_data[2][5] = {{188384.0f, 19.103516f, -1.0831790e9f, -84.242188f, 5.2045898f},
                                           {-49140.0f, 2350.25f, -3.2110596e-1f, 6.4998865e-5f, -0.0f}};
    float               new_data[2][5];
    size_t              precision, offset;
    size_t              i, j;

    TESTING("    nbit float (setup)");

    /* Define user-defined single-precision floating-point type for dataset */
    datatype = H5Tcopy(H5T_IEEE_F32BE);
    if(H5Tset_fields(datatype, (size_t)26, (size_t)20, (size_t)6, (size_t)7, (size_t)13) < 0)
        FAIL_STACK_ERROR
    offset = 7;
    if(H5Tset_offset(datatype,offset) < 0)
        FAIL_STACK_ERROR
    precision = 20;
    if(H5Tset_precision(datatype,precision) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_size(datatype, (size_t)4) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_ebias(datatype, (size_t)31) < 0)
        FAIL_STACK_ERROR

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0)
        FAIL_STACK_ERROR

    /* Use nbit filter  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_chunk(dc, 2, chunk_size) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_nbit(dc) < 0)
        FAIL_STACK_ERROR

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_NBIT_FLOAT_NAME, datatype, space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Test nbit by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit float (write)");

    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig_data) < 0)
        FAIL_STACK_ERROR

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit float (read)");

    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, new_data) < 0)
        FAIL_STACK_ERROR

    /* Check that the values read are the same as the values written
     * Assume size of int = size of float
     */
    for(i = 0; i < (size_t)size[0]; i++) {
        for(j = 0; j < (size_t)size[1]; j++) {
            if(!(orig_data[i][j] == orig_data[i][j]))
                continue;  /* skip if value is NaN */
            if(!H5_FLT_ABS_EQUAL(new_data[i][j], orig_data[i][j])) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0)
        FAIL_STACK_ERROR
    if(H5Pclose(dc) < 0)
        FAIL_STACK_ERROR
    if(H5Sclose(space) < 0)
        FAIL_STACK_ERROR
    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return SUCCEED;

error:
    return FAIL;
} /* end test_nbit_float() */


/*-------------------------------------------------------------------------
 * Function:    test_nbit_double
 *
 * Purpose:     Tests the double datatype of nbit filter
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_double(hid_t file)
{
/* assume unsigned int and float has the same number of bytes */
    hid_t               dataset, datatype, space, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2, 5};
    /* orig_data[] are initialized to be within the range that can be represented by
     * dataset datatype (no precision loss during datatype conversion)
     */
    double              orig_data[2][5] = {
        {
            H5_DOUBLE(1.6081706885101836e+60),
            H5_DOUBLE(-255.32099170994480),
            H5_DOUBLE(1.2677579992621376e-61),
            H5_DOUBLE(64568.289448797700),
            H5_DOUBLE(-1.0619721778839084e-75)
        },
        {
            H5_DOUBLE(2.1499497833454840e+56),
            H5_DOUBLE(6.6562295504670740e-3),
            H5_DOUBLE(-1.5747263393432150),
            H5_DOUBLE(1.0711093225222612),
            H5_DOUBLE(-9.8971679387636870e-1)
        }};
    double              new_data[2][5];
    size_t              precision, offset;
    size_t             i, j;

    TESTING("    nbit double (setup)");

    /* Define user-defined doule-precision floating-point type for dataset */
    datatype = H5Tcopy(H5T_IEEE_F64BE);
    if(H5Tset_fields(datatype, (size_t)55, (size_t)46, (size_t)9, (size_t)5, (size_t)41) < 0)
        FAIL_STACK_ERROR
    offset = 5;
    if(H5Tset_offset(datatype,offset) < 0)
        FAIL_STACK_ERROR
    precision = 51;
    if(H5Tset_precision(datatype,precision) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_size(datatype, (size_t)8) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_ebias(datatype, (size_t)255) < 0)
        FAIL_STACK_ERROR

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0)
        FAIL_STACK_ERROR

    /* Use nbit filter  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_chunk(dc, 2, chunk_size) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_nbit(dc) < 0)
        FAIL_STACK_ERROR

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_NBIT_DOUBLE_NAME, datatype, space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Test nbit by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit double (write)");

    if(H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig_data) < 0)
        FAIL_STACK_ERROR
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit double (read)");

    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, new_data) < 0)
        FAIL_STACK_ERROR

    /* Check that the values read are the same as the values written
     * Assume size of long long = size of double
     */
    for(i = 0; i < (size_t)size[0]; i++) {
        for(j = 0; j < (size_t)size[1]; j++) {
            if(!(orig_data[i][j] == orig_data[i][j]))
                continue;  /* skip if value is NaN */
            if(!H5_DBL_ABS_EQUAL(new_data[i][j], orig_data[i][j])) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0)
        FAIL_STACK_ERROR
    if(H5Pclose(dc) < 0)
        FAIL_STACK_ERROR
    if(H5Sclose(space) < 0)
        FAIL_STACK_ERROR
    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return SUCCEED;

error:
    return FAIL;
} /* end test_nbit_double() */


/*-------------------------------------------------------------------------
 * Function:    test_nbit_array
 *
 * Purpose:     Tests the simple version array datatype for nbit filter
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_array(hid_t file)
{
    hid_t               dataset, base_datatype, array_datatype, space, dc;
    hid_t               mem_base_datatype, mem_array_datatype;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       adims[2] = {3, 2};
    const hsize_t       chunk_size[2] = {2,5};
    unsigned int        orig_data[2][5][3][2];
    unsigned int        new_data[2][5][3][2];
    size_t              precision, offset;
    double              power;
    size_t              i, j, m, n;

    TESTING("    nbit array (setup)");

    /* Define dataset array datatype's base datatype and set precision, offset */
    base_datatype = H5Tcopy(H5T_NATIVE_UINT);
    precision = 22;
    if(H5Tset_precision(base_datatype,precision) < 0) goto error;
    offset = 7;
    if(H5Tset_offset(base_datatype,offset) < 0) goto error;

    /* Copy to memory array datatype's base datatype before setting order */
    mem_base_datatype = H5Tcopy(base_datatype);

    /* Set order of dataset array datatype's base datatype */
    if(H5Tset_order(base_datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create dataset array datatype */
    array_datatype = H5Tarray_create2(base_datatype, 2, adims);

    /* Create memory array datatype */
    mem_array_datatype = H5Tarray_create2(mem_base_datatype, 2, adims);

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Use nbit filter  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_nbit(dc) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_NBIT_ARRAY_NAME, array_datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Initialize data, assuming size of long long >= size of unsigned int */
    for(i= 0;i< (size_t)size[0]; i++)
      for(j = 0; j < (size_t)size[1]; j++)
        for(m = 0; m < (size_t)adims[0]; m++)
          for(n = 0; n < (size_t)adims[1]; n++) {
            power = HDpow(2.0F, (double)precision);
            orig_data[i][j][m][n] = (unsigned int)(((long long)HDrandom() %
                                     (long long)power) << offset);
          } /* end for */
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Test nbit by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit array (write)");

    if(H5Dwrite(dataset, mem_array_datatype, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 orig_data) < 0)
        goto error;

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit array (read)");

    /* Read the dataset back */
    if(H5Dread(dataset, mem_array_datatype, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                new_data) < 0)
        goto error;

    /* Check that the values read are the same as the values written
     */
    for(i=0; i<(size_t)size[0]; i++)
      for(j=0; j<(size_t)size[1]; j++)
        for(m = 0; m < (size_t)adims[0]; m++)
          for(n = 0; n < (size_t)adims[1]; n++) {
            if(new_data[i][j][m][n]!= orig_data[i][j][m][n]) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %lu,%lu,%lu,%lu\n",
                (unsigned long)i, (unsigned long)j, (unsigned long)m, (unsigned long)n);
                goto error;
            }
          }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(array_datatype) < 0) goto error;
    if(H5Tclose(base_datatype) < 0) goto error;
    if(H5Tclose(mem_array_datatype) < 0) goto error;
    if(H5Tclose(mem_base_datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();

    return SUCCEED;

error:
    return FAIL;
} /* end test_nbit_array() */


/*-------------------------------------------------------------------------
 * Function:    test_nbit_compound
 *
 * Purpose:     Tests a simple version of compound datatype of nbit filter
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Tuesday, Jan. 18th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_compound(hid_t file)
{
    typedef struct {     /* Struct with atomic fields */
        int i;
        char c;
        short s;
        float f;
    } atomic;
    hid_t               i_tid, c_tid, s_tid, f_tid;
    hid_t               cmpd_tid; /* atomic compound datatype */
    hid_t               mem_cmpd_tid; /* memory atomic compound datatype */
    size_t              precision[3] = {15, 7, 10};
    size_t              offset[3] = {9, 0, 3};
    hid_t               dataset, space, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2, 5};
    const float         float_val[2][5] = {{188384.0F, 19.103516F, -1.0831790e9F, -84.242188F, 5.2045898F},
                                           {-49140.0F, 2350.25F, -3.2110596e-1F, 6.4998865e-5F, -0.0F}};
    atomic              orig_data[2][5];
    atomic              new_data[2][5];
    unsigned int        i_mask, s_mask, c_mask;
    double              power;
    size_t              i, j;


    TESTING("    nbit compound (setup)");

    /* Define datatypes of members of compound datatype */
    if((i_tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
        FAIL_STACK_ERROR
    if((c_tid = H5Tcopy(H5T_NATIVE_CHAR)) < 0)
        FAIL_STACK_ERROR
    if((s_tid = H5Tcopy(H5T_NATIVE_SHORT)) < 0)
        FAIL_STACK_ERROR
    if((f_tid = H5Tcopy(H5T_IEEE_F32BE)) < 0)
        FAIL_STACK_ERROR

    /* Set precision and offset etc. */
    if(H5Tset_precision(i_tid,precision[0]) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_offset(i_tid,offset[0]) < 0)
        FAIL_STACK_ERROR

    if(H5Tset_precision(c_tid,precision[1]) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_offset(c_tid,offset[1]) < 0)
        FAIL_STACK_ERROR

    if(H5Tset_precision(s_tid,precision[2]) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_offset(s_tid,offset[2]) < 0)
        FAIL_STACK_ERROR

    if(H5Tset_fields(f_tid, (size_t)26, (size_t)20, (size_t)6, (size_t)7, (size_t)13) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_offset(f_tid, (size_t)7) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_precision(f_tid, (size_t)20) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_size(f_tid, (size_t)4) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_ebias(f_tid, (size_t)31) < 0)
        FAIL_STACK_ERROR

    /* Create a memory compound datatype before setting the order */
    if((mem_cmpd_tid = H5Tcreate(H5T_COMPOUND, sizeof(atomic))) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(mem_cmpd_tid, "i", HOFFSET(atomic, i), i_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(mem_cmpd_tid, "c", HOFFSET(atomic, c), c_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(mem_cmpd_tid, "s", HOFFSET(atomic, s), s_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(mem_cmpd_tid, "f", HOFFSET(atomic, f), H5T_NATIVE_FLOAT) < 0)
        FAIL_STACK_ERROR

    /* Create a dataset compound datatype and insert some atomic types */
    if((cmpd_tid = H5Tcreate(H5T_COMPOUND, sizeof(atomic))) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(cmpd_tid, "i", HOFFSET(atomic, i), i_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(cmpd_tid, "c", HOFFSET(atomic, c), c_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(cmpd_tid, "s", HOFFSET(atomic, s), s_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(cmpd_tid, "f", HOFFSET(atomic, f), f_tid) < 0)
        FAIL_STACK_ERROR

    /* Set order of dataset compound datatype */
    if(H5Tset_order(cmpd_tid, H5T_ORDER_BE) < 0)
        FAIL_STACK_ERROR

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0)
        FAIL_STACK_ERROR

    /* Use nbit filter  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_chunk(dc, 2, chunk_size) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_nbit(dc) < 0)
        FAIL_STACK_ERROR

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_NBIT_COMPOUND_NAME, cmpd_tid, space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Initialize data, assuming size of long long >= size of member datatypes */
    for(i = 0; i < (size_t)size[0]; i++)
        for(j = 0; j < (size_t)size[1]; j++) {
            power = HDpow(2.0F, (double)(precision[0]-1));
            orig_data[i][j].i = (int)(((long long)HDrandom() % (long long)power) << offset[0]);
            power = HDpow(2.0F, (double)(precision[1]-1));
            orig_data[i][j].c = (char)(((long long)HDrandom() % (long long)power) << offset[1]);
            power = HDpow(2.0F, (double)(precision[2]-1));
            orig_data[i][j].s = (short)(((long long)HDrandom() % (long long)power) << offset[2]);
            orig_data[i][j].f = float_val[i][j];

            /* some even-numbered integer values are negtive */
            if((i * size[1] + j + 1) % 2 == 0) {
                orig_data[i][j].i = -orig_data[i][j].i;
                orig_data[i][j].s = (short)-orig_data[i][j].s;
            }
        }

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Test nbit by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit compound (write)");

    if(H5Dwrite(dataset, mem_cmpd_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig_data) < 0)
        FAIL_STACK_ERROR
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit compound (read)");

    /* Read the dataset back */
    if(H5Dread(dataset, mem_cmpd_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, new_data) < 0)
        FAIL_STACK_ERROR

    /* Check that the values read are the same as the values written
     * Use mask for checking the significant bits, ignoring the padding bits
     */
    i_mask = ~((unsigned)~0 << (precision[0] + offset[0])) & ((unsigned)~0 << offset[0]);
    c_mask = ~((unsigned)~0 << (precision[1] + offset[1])) & ((unsigned)~0 << offset[1]);
    s_mask = ~((unsigned)~0 << (precision[2] + offset[2])) & ((unsigned)~0 << offset[2]);
    for(i = 0; i < size[0]; i++) {
        for(j = 0; j < size[1]; j++) {
            if(((unsigned)new_data[i][j].i & i_mask) != ((unsigned)orig_data[i][j].i & i_mask) ||
                ((unsigned)new_data[i][j].c & c_mask) != ((unsigned)orig_data[i][j].c & c_mask) ||
                ((unsigned)new_data[i][j].s & s_mask) != ((unsigned)orig_data[i][j].s & s_mask) ||
                (orig_data[i][j].f == orig_data[i][j].f && !H5_FLT_ABS_EQUAL(new_data[i][j].f, orig_data[i][j].f)))
            {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(i_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(c_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(s_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(f_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(cmpd_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(mem_cmpd_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Pclose(dc) < 0)
        FAIL_STACK_ERROR
    if(H5Sclose(space) < 0)
        FAIL_STACK_ERROR
    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return SUCCEED;

error:
    return FAIL;
} /* end test_nbit_compound() */


/*-------------------------------------------------------------------------
 * Function:    test_nbit_compound_2
 *
 * Purpose:     Tests a complex version of compound datatype of nbit filter
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Tuesday, Jan. 18th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_compound_2(hid_t file)
{
    typedef struct {     /* Struct with atomic fields */
        int i;
        char c;
        short s;
        float f;
    } atomic;

    typedef struct {     /* Struct with complex fields */
        atomic a;
        unsigned int v;
        char   b[2][2];
        atomic d[2][2];
    } complex;

    hid_t               i_tid, c_tid, s_tid, f_tid, v_tid;
    hid_t               cmpd_tid1; /* atomic compound datatype */
    hid_t               cmpd_tid2; /* complex compound datatype */
    hid_t               mem_cmpd_tid1; /* memory atomic compound datatype */
    hid_t               mem_cmpd_tid2; /* memory complex compound datatype */
    hid_t               base_tid;      /* simple array datatype's base datatype */
    hid_t               array_tid;     /* simple array datatype */
    hid_t               array_cmplx_tid;     /* complex array datatype */
    hid_t               mem_array_cmplx_tid; /* memory complex array datatype */
    const hsize_t       array_dims[2] = {2, 2};
    size_t              precision[5] = {31, 8, 10, 23, 8};
    size_t              offset[5] = {1, 0, 3, 5, 0};
    hid_t               dataset, space, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2, 5};
    const float         float_val[2][5] = {{188384.0F, 19.103516F, -1.0831790e9F, -84.242188F, 5.2045898F},
                                           {-49140.0F, 2350.25F, -3.2110596e-1F, 6.4998865e-5F, -0.0F}};
    complex             orig_data[2][5];
    complex             new_data[2][5];
    unsigned int        i_mask, s_mask, c_mask, b_mask;
    double              power;
    size_t              i, j, m, n, b_failed, d_failed;


    TESTING("    nbit compound complex (setup)");

    /* Define datatypes of members of compound datatype */
    if((i_tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
        FAIL_STACK_ERROR
    if((c_tid = H5Tcopy(H5T_NATIVE_CHAR)) < 0)
        FAIL_STACK_ERROR
    if((s_tid = H5Tcopy(H5T_NATIVE_SHORT)) < 0)
        FAIL_STACK_ERROR
    if((v_tid = H5Tcopy(H5T_NATIVE_UINT)) < 0)
        FAIL_STACK_ERROR
    if((f_tid = H5Tcopy(H5T_IEEE_F32BE)) < 0)
        FAIL_STACK_ERROR

    /* Set precision and offset etc. of atomic compound datatype members */
    if(H5Tset_precision(i_tid,precision[0]) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_offset(i_tid,offset[0]) < 0)
        FAIL_STACK_ERROR

    if(H5Tset_precision(c_tid,precision[1]) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_offset(c_tid,offset[1]) < 0)
        FAIL_STACK_ERROR

    if(H5Tset_precision(s_tid,precision[2]) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_offset(s_tid,offset[2]) < 0)
        FAIL_STACK_ERROR

    if(H5Tset_fields(f_tid, (size_t)26, (size_t)20, (size_t)6, (size_t)7, (size_t)13) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_offset(f_tid, (size_t)7) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_precision(f_tid, (size_t)20) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_size(f_tid, (size_t)4) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_ebias(f_tid, (size_t)31) < 0)
        FAIL_STACK_ERROR

    /* Create a memory atomic compound datatype before setting the order */
    if((mem_cmpd_tid1 = H5Tcreate(H5T_COMPOUND, sizeof(atomic))) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(mem_cmpd_tid1, "i", HOFFSET(atomic, i), i_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(mem_cmpd_tid1, "c", HOFFSET(atomic, c), c_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(mem_cmpd_tid1, "s", HOFFSET(atomic, s), s_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(mem_cmpd_tid1, "f", HOFFSET(atomic, f), H5T_NATIVE_FLOAT) < 0)
        FAIL_STACK_ERROR

    /* Create a dataset atomic compound datatype and insert some atomic types */
    if((cmpd_tid1 = H5Tcreate(H5T_COMPOUND, sizeof(atomic))) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(cmpd_tid1, "i", HOFFSET(atomic, i), i_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(cmpd_tid1, "c", HOFFSET(atomic, c), c_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(cmpd_tid1, "s", HOFFSET(atomic, s), s_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(cmpd_tid1, "f", HOFFSET(atomic, f), f_tid) < 0)
        FAIL_STACK_ERROR

    /* Set order of dataset compound datatype */
    if(H5Tset_order(cmpd_tid1, H5T_ORDER_BE) < 0)
        FAIL_STACK_ERROR

    /* Set precision and offset of the other data member */
    if(H5Tset_precision(v_tid,precision[3]) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_offset(v_tid,offset[3]) < 0)
        FAIL_STACK_ERROR

    /* Create the simple array datatype */
    if((base_tid = H5Tcopy(H5T_NATIVE_CHAR)) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_precision(base_tid,precision[4]) < 0)
        FAIL_STACK_ERROR
    if(H5Tset_offset(base_tid,offset[4]) < 0)
        FAIL_STACK_ERROR
    if((array_tid = H5Tarray_create2(base_tid, 2, array_dims)) < 0)
        FAIL_STACK_ERROR

    /* Create the complex memory and dataset array datatype */
    if((array_cmplx_tid = H5Tarray_create2(cmpd_tid1, 2, array_dims)) < 0)
        FAIL_STACK_ERROR
    if((mem_array_cmplx_tid = H5Tarray_create2(mem_cmpd_tid1, 2, array_dims)) < 0)
        FAIL_STACK_ERROR

    /* Create a memory complex compound datatype before setting the order */
    if((mem_cmpd_tid2 = H5Tcreate(H5T_COMPOUND, sizeof(complex))) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(mem_cmpd_tid2, "a", HOFFSET(complex, a), mem_cmpd_tid1) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(mem_cmpd_tid2, "v", HOFFSET(complex, v), v_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(mem_cmpd_tid2, "b", HOFFSET(complex, b), array_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(mem_cmpd_tid2, "d", HOFFSET(complex, d), mem_array_cmplx_tid) < 0)
        FAIL_STACK_ERROR

    /* Set order of dataset other complex compound member datatype */
    if(H5Tset_order(v_tid, H5T_ORDER_BE) < 0)
        FAIL_STACK_ERROR

    /* Create a dataset complex compound datatype and insert members */
    if((cmpd_tid2 = H5Tcreate(H5T_COMPOUND, sizeof(complex))) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(cmpd_tid2, "a", HOFFSET(complex, a), cmpd_tid1) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(cmpd_tid2, "v", HOFFSET(complex, v), v_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(cmpd_tid2, "b", HOFFSET(complex, b), array_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tinsert(cmpd_tid2, "d", HOFFSET(complex, d), array_cmplx_tid) < 0)
        FAIL_STACK_ERROR

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0)
        FAIL_STACK_ERROR

    /* Use nbit filter  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_chunk(dc, 2, chunk_size) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_nbit(dc) < 0)
        FAIL_STACK_ERROR

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_NBIT_COMPOUND_NAME_2, cmpd_tid2, space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Initialize data, assuming size of long long >= size of member datatypes */
    for(i= 0;i< (size_t)size[0]; i++)
        for(j = 0; j < (size_t)size[1]; j++) {
            power = HDpow(2.0F, (double)(precision[0]-1));
            orig_data[i][j].a.i = (int)(((long long)HDrandom() % (long long)power) << offset[0]);
            power = HDpow(2.0F, (double)(precision[1]-1));
            orig_data[i][j].a.c = (char)(((long long)HDrandom() % (long long)power) << offset[1]);
            power = HDpow(2.0F, (double)(precision[2]-1));
            orig_data[i][j].a.s = (short)(-((long long)HDrandom() % (long long)power) << offset[2]);
            orig_data[i][j].a.f = float_val[i][j];

            power = HDpow(2.0F, (double)precision[3]);
            orig_data[i][j].v = (unsigned int)(((long long)HDrandom() % (long long)power) << offset[3]);

            for(m = 0; m < (size_t)array_dims[0]; m++)
                for(n = 0; n < (size_t)array_dims[1]; n++) {
                    power = HDpow(2.0F, (double)(precision[4]-1));
                    orig_data[i][j].b[m][n] = (char)(((long long)HDrandom() % (long long)power) << offset[4]);
                } /* end for */

            for(m = 0; m < (size_t)array_dims[0]; m++)
                for(n = 0; n < (size_t)array_dims[1]; n++) {
                    power = HDpow(2.0F, (double)(precision[0]-1));
                    orig_data[i][j].d[m][n].i = (int)(-((long long)HDrandom() % (long long)power) << offset[0]);
                    power = HDpow(2.0F, (double)(precision[1]-1));
                    orig_data[i][j].d[m][n].c = (char)(((long long)HDrandom() % (long long)power) << offset[1]);
                    power = HDpow(2.0F, (double)(precision[2]-1));
                    orig_data[i][j].d[m][n].s = (short)(((long long)HDrandom() % (long long)power) << offset[2]);
                    orig_data[i][j].d[m][n].f = float_val[i][j];
                } /* end for */
        } /* end for */

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Test nbit by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit compound complex (write)");

    if(H5Dwrite(dataset, mem_cmpd_tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig_data) < 0)
        FAIL_STACK_ERROR
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit compound complex (read)");

    /* Read the dataset back */
    if(H5Dread(dataset, mem_cmpd_tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, new_data) < 0)
        FAIL_STACK_ERROR

    /* Check that the values read are the same as the values written
     * Use mask for checking the significant bits, ignoring the padding bits
     */
    /* The original code
     *   i_mask = ~((unsigned)~0 << (precision[0] + offset[0])) & ((unsigned)~0 << offset[0]);
     * left shift a 32-bit integer for 32-bit.  The result is undefined by C language.  A user
     * discovered it using clang compiler with -fcatch-undefined-behavior option (see Issue 7674
     * in Jira).  So I changed it in a funny way as below to avoid it. SLU - 2011/8/11
     */
    if(sizeof(unsigned) > 4)
        i_mask = ~((unsigned)~0 << (precision[0] + offset[0])) & ((unsigned)~0 << offset[0]);
    else {
        i_mask = 0xffffffff;
        i_mask = i_mask & ((unsigned)~0 << offset[0]);
    }
    c_mask = ~((unsigned)~0 << (precision[1] + offset[1])) & ((unsigned)~0 << offset[1]);
    s_mask = ~((unsigned)~0 << (precision[2] + offset[2])) & ((unsigned)~0 << offset[2]);
    b_mask = ~((unsigned)~0 << (precision[4] + offset[4])) & ((unsigned)~0 << offset[4]);
    for(i=0; i<(size_t)size[0]; i++) {
        for(j=0; j<(size_t)size[1]; j++) {
            b_failed = 0;
            d_failed = 0;

            for(m = 0; m < (size_t)array_dims[0]; m++)
            for(n = 0; n < (size_t)array_dims[1]; n++)
            if(((unsigned)new_data[i][j].b[m][n] & b_mask)!=((unsigned)orig_data[i][j].b[m][n] & b_mask)) {
                b_failed = 1;
                goto out;
            }

            for(m = 0; m < (size_t)array_dims[0]; m++)
                for(n = 0; n < (size_t)array_dims[1]; n++)
                    if(((unsigned)new_data[i][j].d[m][n].i & i_mask) != ((unsigned)orig_data[i][j].d[m][n].i & i_mask)||
                            ((unsigned)new_data[i][j].d[m][n].c & c_mask) != ((unsigned)orig_data[i][j].d[m][n].c & c_mask)||
                            ((unsigned)new_data[i][j].d[m][n].s & s_mask) != ((unsigned)orig_data[i][j].d[m][n].s & s_mask)||
                            (new_data[i][j].d[m][n].f == new_data[i][j].d[m][n].f && !H5_FLT_ABS_EQUAL(new_data[i][j].d[m][n].f, new_data[i][j].d[m][n].f))) {
                        d_failed = 1;
                        goto out;
                    }

out:
            if(((unsigned)new_data[i][j].a.i & i_mask) != ((unsigned)orig_data[i][j].a.i & i_mask)||
                    ((unsigned)new_data[i][j].a.c & c_mask) != ((unsigned)orig_data[i][j].a.c & c_mask)||
                    ((unsigned)new_data[i][j].a.s & s_mask) != ((unsigned)orig_data[i][j].a.s & s_mask)||
                    (new_data[i][j].a.f == new_data[i][j].a.f && !H5_FLT_ABS_EQUAL(new_data[i][j].a.f, new_data[i][j].a.f)) ||
                    new_data[i][j].v != orig_data[i][j].v || b_failed || d_failed) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(i_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(c_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(s_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(f_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(v_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(cmpd_tid2) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(cmpd_tid1) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(mem_cmpd_tid2) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(mem_cmpd_tid1) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(array_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(base_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(array_cmplx_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Tclose(mem_array_cmplx_tid) < 0)
        FAIL_STACK_ERROR
    if(H5Pclose(dc) < 0)
        FAIL_STACK_ERROR
    if(H5Sclose(space) < 0)
        FAIL_STACK_ERROR
    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return SUCCEED;

error:
    return FAIL;
} /* end test_nbit_compound_2() */


/*-------------------------------------------------------------------------
 * Function:    test_nbit_compound_3
 *
 * Purpose:     Tests no-op datatypes in compound datatype for nbit filter
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Thursday, Mar. 31th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_compound_3(hid_t file)
{
    typedef struct {     /* Struct with some no-op type fields */
        int i;              /* integer field, NOT a no-op type */
        char str[30];       /* fixed-length string, no-op type */
        char *vl_str;       /* varible-length string, no-op type */
        hvl_t v;            /* VL datatype field, no-op type */
        hobj_ref_t r;       /* Object reference field, no-op type */
        unsigned char o[5]; /* Opaque field, no-op type */
    } atomic;
    hid_t               i_tid, str_tid, vl_str_tid, v_tid, o_tid;
    hid_t               cmpd_tid; /* atomic compound datatype */
    hid_t               dataset, space, dc, obj_ref_dataset = -1;
    const hsize_t       size[1] = {5};
    const hsize_t       chunk_size[1] = {5};
    atomic              orig_data[5];
    atomic              new_data[5];
    double              power;
    size_t              i, k, j;


    TESTING("    nbit compound with no-op type (setup)");

    /* Define datatypes of members of compound datatype */
    i_tid=H5Tcopy(H5T_NATIVE_INT);
    if(H5Tset_precision(i_tid, (size_t)17) < 0) goto error;

    str_tid=H5Tcopy(H5T_C_S1);
    if(H5Tset_size(str_tid, (size_t)30) < 0) goto error;

    vl_str_tid = H5Tcopy(H5T_C_S1);
    if(H5Tset_size(vl_str_tid,H5T_VARIABLE) < 0) goto error;

    if((v_tid = H5Tvlen_create(H5T_NATIVE_UINT)) < 0) goto error;

    if((o_tid = H5Tcreate(H5T_OPAQUE, (size_t)5)) < 0) goto error;
    if(H5Tset_tag(o_tid, "testing opaque field") < 0) goto error;

    /* Create a dataset compound datatype and insert some atomic types */
    cmpd_tid = H5Tcreate(H5T_COMPOUND, sizeof(atomic));
    if(H5Tinsert(cmpd_tid, "i", HOFFSET(atomic, i), i_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid, "str", HOFFSET(atomic, str), str_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid, "vl_str", HOFFSET(atomic, vl_str), vl_str_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid, "v", HOFFSET(atomic, v), v_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid, "r", HOFFSET(atomic, r), H5T_STD_REF_OBJ) < 0) goto error;
    if(H5Tinsert(cmpd_tid, "o", HOFFSET(atomic, o), o_tid) < 0) goto error;

    /* Create the data space */
    if((space = H5Screate_simple(1, size, NULL)) < 0) goto error;

    /* Use nbit filter  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dc, 1, chunk_size) < 0) goto error;
    if(H5Pset_nbit(dc) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_NBIT_COMPOUND_NAME_3, cmpd_tid,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Create the dataset object reference points to */
    if((obj_ref_dataset = H5Dcreate2(file, "nbit_obj_ref", H5T_NATIVE_INT,
                                     space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;

    /* Initialize data */
    for(i = 0; i < (size_t)size[0]; i++) {
        power = HDpow(2.0F, 17.0F - 1.0F);
        HDmemset(&orig_data[i], 0, sizeof(orig_data[i]));
        orig_data[i].i = (int)(HDrandom() % (long)power);
        HDstrcpy(orig_data[i].str, "fixed-length C string");
        orig_data[i].vl_str = HDstrdup("variable-length C string");

        orig_data[i].v.p = HDmalloc((size_t)(i+1)*sizeof(unsigned int));
        orig_data[i].v.len = (size_t)i+1;
        for(k = 0; k < (i+1); k++) ((unsigned int *)orig_data[i].v.p)[k] = (unsigned int)(i*100 + k);

        /* Create reference to the dataset "nbit_obj_ref" */
        if(H5Rcreate(&orig_data[i].r, file, "nbit_obj_ref", H5R_OBJECT, (hid_t)-1) < 0) goto error;

        for(j = 0; j < 5; j++) orig_data[i].o[j] = (unsigned char)(i + j);
    }

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Test nbit by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit compound with no-op type (write)");

    if(H5Dwrite(dataset, cmpd_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 orig_data) < 0)
        goto error;
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit compound with no-op type (read)");

    /* Read the dataset back */
    if(H5Dread(dataset, cmpd_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                new_data) < 0)
        goto error;

    /* Check that the values read are the same as the values written */
    for(i = 0; i < (size_t)size[0]; i++) {
        if(new_data[i].i != orig_data[i].i ||
           strcmp(new_data[i].str, orig_data[i].str) !=0 ||
           strcmp(new_data[i].vl_str, orig_data[i].vl_str) !=0 ||
           new_data[i].v.len != orig_data[i].v.len ||
           new_data[i].r != orig_data[i].r)
        {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %lu\n", (unsigned long)i);
            goto error;
        }

        for(k=0; k<i+1; k++)
            if(((unsigned int *)orig_data[i].v.p)[k] !=((unsigned int *)new_data[i].v.p)[k])
            {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %lu\n", (unsigned long)i);
                goto error;
            }

        for(j=0; j<5; j++)
            if(orig_data[i].o[j] != new_data[i].o[j])
            {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %lu\n", (unsigned long)i);
                goto error;
            }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Dvlen_reclaim(cmpd_tid, space, H5P_DEFAULT, new_data) < 0) goto error;
    if(H5Dvlen_reclaim(cmpd_tid, space, H5P_DEFAULT, orig_data) < 0) goto error;
    if(H5Tclose(i_tid) < 0) goto error;
    if(H5Tclose(str_tid) < 0) goto error;
    if(H5Tclose(vl_str_tid) < 0) goto error;
    if(H5Tclose(v_tid) < 0) goto error;
    if(H5Tclose(o_tid) < 0) goto error;
    if(H5Tclose(cmpd_tid) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(obj_ref_dataset) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();

    return SUCCEED;

error:
    return FAIL;
} /* end test_nbit_compound_3() */


/*-------------------------------------------------------------------------
 * Function:    test_nbit_int_size
 *
 * Purpose:     Tests the correct size of the integer datatype for nbit filter
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              19 November 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_int_size(hid_t file)
{
    hid_t   dataspace, dataset, datatype, mem_datatype, dset_create_props;
    hsize_t dims[2], chunk_size[2];
    hsize_t dset_size = 0;
    int     orig_data[DSET_DIM1][DSET_DIM2];
    double  power;
    int     i, j;
    size_t  precision, offset;

    TESTING("    nbit integer dataset size");

   /* Define dataset datatype (integer), and set precision, offset */
   if((datatype = H5Tcopy(H5T_NATIVE_INT)) < 0) {
       H5_FAILED();
       HDprintf("    line %d: H5Tcopy failed\n",__LINE__);
       goto error;
   } /* end if */

   precision = 16; /* precision includes sign bit */
   if(H5Tset_precision(datatype,precision)<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Pset_precision failed\n",__LINE__);
       goto error;
   } /* end if */

   offset = 8;
   if(H5Tset_offset(datatype,offset)<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Tset_offset failed\n",__LINE__);
       goto error;
   } /* end if */

   /* Copy to memory datatype */
   if((mem_datatype = H5Tcopy(datatype)) < 0) {
       H5_FAILED();
       HDprintf("    line %d: H5Tcopy failed\n",__LINE__);
       goto error;
   } /* end if */

   /* Set order of dataset datatype */
   if(H5Tset_order(datatype, H5T_ORDER_BE)<0) {
        H5_FAILED();
       HDprintf("    line %d: H5Pset_order failed\n",__LINE__);
       goto error;
   } /* end if */

   if(H5Tset_size(datatype, 4)<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Pset_size failed\n",__LINE__);
       goto error;
   } /* end if */

  /* Initiliaze data buffer with random data within correct range
   * corresponding to the memory datatype's precision and offset.
   */
   for (i=0; i < DSET_DIM1; i++)
       for (j=0; j < DSET_DIM2; j++) {
           power = HDpow(2.0F, (double)(precision-1));
           orig_data[i][j] = HDrandom() % (int)power << offset;
       } /* end for */


   /* Describe the dataspace. */
   dims[0] = DSET_DIM1;
   dims[1] = DSET_DIM2;
   if((dataspace = H5Screate_simple (2, dims, NULL))<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Pcreate failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Set the dataset creation property list to specify the chunks
   */
   chunk_size[0] = DSET_DIM1/10;
   chunk_size[1] = DSET_DIM2/10;
   if((dset_create_props = H5Pcreate (H5P_DATASET_CREATE))<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Pcreate failed\n",__LINE__);
       goto error;
   } /* end if */

   if(H5Pset_chunk (dset_create_props, 2, chunk_size)<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Pset_chunk failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Set for n-bit compression
   */
   if(H5Pset_nbit (dset_create_props)<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Pset_nbit failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Create a new dataset within the file.
   */
   if((dataset = H5Dcreate2 (file, DSET_NBIT_INT_SIZE_NAME, datatype,
                            dataspace, H5P_DEFAULT,
                            dset_create_props, H5P_DEFAULT))<0) {
       H5_FAILED();
       HDprintf("    line %d: H5dwrite failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Write the array to the file.
   */
   if(H5Dwrite (dataset, mem_datatype, H5S_ALL, H5S_ALL,
                H5P_DEFAULT, orig_data)<0) {
       H5_FAILED();
       HDprintf("    Line %d: H5Dwrite failed\n",__LINE__);
       goto error;
   } /* end if */

   /*
    * Get the precision of the data type
    */
   if((precision = H5Tget_precision(datatype)) == 0) {
       H5_FAILED();
       HDprintf("    Line %d: wrong precision size: %zu\n",__LINE__, precision);
       goto error;
   } /* end if */

   /*
    * The size of the dataset after compression should around 2 * DSET_DIM1 * DSET_DIM2
    */
   if((dset_size = H5Dget_storage_size(dataset)) < DSET_DIM1*DSET_DIM2*(precision/8) ||
       dset_size > DSET_DIM1*DSET_DIM2*(precision/8) + 1*KB) {
       H5_FAILED();
       HDfprintf(stdout, "    Line %d: wrong dataset size: %Hu\n",__LINE__, dset_size);
       goto error;
   } /* end if */

   H5Tclose (datatype);
   H5Tclose (mem_datatype);
   H5Dclose (dataset);
   H5Sclose (dataspace);
   H5Pclose (dset_create_props);

    PASSED();

   return SUCCEED;
error:
    return FAIL;
} /* end test_nbit_int_size() */


/*-------------------------------------------------------------------------
 * Function:    test_nbit_flt_size
 *
 * Purpose:     Tests the correct size of the floating-number datatype for
 *              nbit filter
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              19 November 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_flt_size(hid_t file)
{
    hid_t   dataspace, dataset, datatype, dset_create_props;
    hsize_t dims[2], chunk_size[2];
    hsize_t dset_size = 0;
    float   orig_data[DSET_DIM1][DSET_DIM2];
    int     i, j;
    size_t  precision, offset;
    size_t  spos, epos, esize, mpos, msize;

    TESTING("    nbit floating-number dataset size");

  /* Define floating-point type for dataset
   *-------------------------------------------------------------------
   * size=4 byte, precision=16 bits, offset=8 bits,
   * mantissa size=9 bits, mantissa position=8,
   * exponent size=6 bits, exponent position=17,
   * exponent bias=31.
   * It can be illustrated in little-endian order as:
   * (S - sign bit, E - exponent bit, M - mantissa bit,
   *  ? - padding bit)
   *
   *           3        2        1        0
   *       ???????? SEEEEEEM MMMMMMMM ????????
   *
   * To create a new floating-point type, the following
   * properties must be set in the order of
   *     set fields -> set offset -> set precision -> set size.
   * All these properties must be set before the type can function.
   * Other properties can be set anytime. Derived type size cannot
   * be expanded bigger than original size but can be decreased.
   * There should be no holes among the significant bits. Exponent
   * bias usually is set 2^(n-1)-1, where n is the exponent size.
   *-------------------------------------------------------------------*/
   if((datatype = H5Tcopy(H5T_IEEE_F32LE)) < 0) {
       H5_FAILED();
       HDprintf("    line %d: H5Tcopy failed\n",__LINE__);
       goto error;
   } /* end if */

   msize = 9;
   spos = 23;
   epos = 17;
   esize = 6;
   mpos = 8;
   offset = 8;
   precision = 16;

   if(H5Tset_fields(datatype, spos, epos, esize, mpos, msize)<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Tset_fields failed\n",__LINE__);
       goto error;
   } /* end if */

   if(H5Tset_offset(datatype,offset)<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Tset_offset failed\n",__LINE__);
       goto error;
   } /* end if */

   if(H5Tset_precision(datatype,precision)<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Tset_precision failed\n",__LINE__);
       goto error;
   } /* end if */

   if(H5Tset_size(datatype, 4)<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Pset_size failed\n",__LINE__);
       goto error;
   } /* end if */

   /* Set order of dataset datatype */
   if(H5Tset_order(datatype, H5T_ORDER_BE)<0) {
        H5_FAILED();
       HDprintf("    line %d: H5Pset_order failed\n",__LINE__);
       goto error;
   } /* end if */

   if(H5Tset_ebias(datatype, 31)<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Pset_size failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Initiliaze data buffer with random data
   */
   for (i=0; i < DSET_DIM1; i++)
       for (j=0; j < DSET_DIM2; j++)
           orig_data[i][j] = (float)(HDrandom() % 1234567) / 2;


   /* Describe the dataspace. */
   dims[0] = DSET_DIM1;
   dims[1] = DSET_DIM2;
   if((dataspace = H5Screate_simple (2, dims, NULL))<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Pcreate failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Set the dataset creation property list to specify the chunks
   */
   chunk_size[0] = DSET_DIM1/10;
   chunk_size[1] = DSET_DIM2/10;
   if((dset_create_props = H5Pcreate (H5P_DATASET_CREATE))<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Pcreate failed\n",__LINE__);
       goto error;
   } /* end if */

   if(H5Pset_chunk (dset_create_props, 2, chunk_size)<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Pset_chunk failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Set for n-bit compression
   */
   if(H5Pset_nbit (dset_create_props)<0) {
       H5_FAILED();
       HDprintf("    line %d: H5Pset_nbit failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Create a new dataset within the file.
   */
   if((dataset = H5Dcreate2 (file, DSET_NBIT_FLT_SIZE_NAME, datatype,
                            dataspace, H5P_DEFAULT,
                            dset_create_props, H5P_DEFAULT))<0) {
       H5_FAILED();
       HDprintf("    line %d: H5dwrite failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Write the array to the file.
   */
   if(H5Dwrite (dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL,
                H5P_DEFAULT, orig_data)<0) {
       H5_FAILED();
       HDprintf("    Line %d: H5Dwrite failed\n",__LINE__);
       goto error;
   } /* end if */

   /*
    * Get the precision of the data type
    */
   if((precision = H5Tget_precision(datatype)) == 0) {
       H5_FAILED();
       HDprintf("    Line %d: wrong precision size: %zu\n",__LINE__, precision);
       goto error;
   } /* end if */

   /*
    * The size of the dataset after compression should around 2 * DSET_DIM1 * DSET_DIM2
    */
   if((dset_size = H5Dget_storage_size(dataset)) < DSET_DIM1*DSET_DIM2*(precision/8) ||
       dset_size > DSET_DIM1*DSET_DIM2*(precision/8) + 1*KB) {
       H5_FAILED();
       HDfprintf(stdout, "    Line %d: wrong dataset size: %Hu\n",__LINE__, dset_size);
       goto error;
   } /* end if */

   H5Tclose (datatype);
   H5Dclose (dataset);
   H5Sclose (dataspace);
   H5Pclose (dset_create_props);

    PASSED();

   return SUCCEED;
error:
    return FAIL;
} /* end test_nbit_flt_size() */

/*-------------------------------------------------------------------------
 * Function:    test_scaleoffset_int
 *
 * Purpose:     Tests the integer datatype for scaleoffset filter
 *              with fill value not defined
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Monday, Feb. 14th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_scaleoffset_int(hid_t file)
{
    hid_t               dataset, datatype, space, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2,5};
    int                 orig_data[2][5];
    int                 new_data[2][5];
    size_t             i, j;

    HDputs("Testing scaleoffset filter");
    TESTING("    scaleoffset int without fill value (setup)");

    datatype = H5Tcopy(H5T_NATIVE_INT);

    /* Set order of dataset datatype */
    if(H5Tset_order(datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Create the dataset property list  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* Fill value undefined */
    if(H5Pset_fill_value(dc, datatype, NULL) < 0) goto error;

    /* Set up to use scaleoffset filter, let library calculate minbits */
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_scaleoffset(dc, H5Z_SO_INT,H5Z_SO_INT_MINBITS_DEFAULT) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_SCALEOFFSET_INT_NAME, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Initialize data */
    for(i= 0;i< (size_t)size[0]; i++)
      for(j = 0; j < (size_t)size[1]; j++) {
        orig_data[i][j] = HDrandom() % 10000;

        /* even-numbered values are negtive */
        if((i*size[1]+j+1)%2 == 0)
            orig_data[i][j] = -orig_data[i][j];
      }

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Test scaleoffset by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset int without fill value (write)");

    if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 orig_data) < 0) goto error;
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset int without fill value (read)");

    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                new_data) < 0) goto error;

    /* Check that the values read are the same as the values written */
    for(i=0; i<(size_t)size[0]; i++) {
        for(j=0; j<(size_t)size[1]; j++) {
            if(new_data[i][j] != orig_data[i][j]) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();

    return SUCCEED;
error:
    return FAIL;
} /* end test_scaleoffset_int() */


/*-------------------------------------------------------------------------
 * Function:    test_scaleoffset_int_2
 *
 * Purpose:     Tests the integer datatype for scaleoffset filter
 *              with fill value set
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Tuesday, March 15th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_scaleoffset_int_2(hid_t file)
{
    hid_t               dataset, datatype, space, mspace, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2,5};
    int                 orig_data[2][5];
    int                 new_data[2][5];
    hsize_t             start[2]; /* Start of hyperslab */
    hsize_t             stride[2]; /* Stride of hyperslab */
    hsize_t             count[2];  /* Block count */
    hsize_t             block[2];  /* Block sizes */
    int                 fillval;
    size_t              j;

    TESTING("    scaleoffset int with fill value (setup)");

    datatype = H5Tcopy(H5T_NATIVE_INT);

    /* Set order of dataset datatype */
    if(H5Tset_order(datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create the data space for the dataset */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Create the dataset property list  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* Set fill value */
    fillval = 10000;
    if(H5Pset_fill_value(dc, H5T_NATIVE_INT, &fillval) < 0) goto error;

    /* Set up to use scaleoffset filter, let library calculate minbits */
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_scaleoffset(dc, H5Z_SO_INT,H5Z_SO_INT_MINBITS_DEFAULT) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_SCALEOFFSET_INT_NAME_2, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Create the memory data space */
    if((mspace = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Select hyperslab for data to write, using 1x5 blocks,
     * (1,1) stride and (1,1) count starting at the position (0,0).
     */
    start[0]  = 0; start[1]  = 0;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 1; count[1]  = 1;
    block[0]  = 1; block[1]  = 5;
    if(H5Sselect_hyperslab(mspace, H5S_SELECT_SET, start,
                           stride, count, block) < 0) goto error;

    /* Initialize data of hyperslab */
    for(j = 0; j < (size_t)size[1]; j++) {
        orig_data[0][j] = (int)HDrandom() % 10000;

        /* even-numbered values are negtive */
        if((j+1)%2 == 0)
            orig_data[0][j] = -orig_data[0][j];
    }

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Test scaleoffset by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset int with fill value (write)");

    /* only data in the hyperslab will be written, other value should be fill value */
    if(H5Dwrite(dataset, H5T_NATIVE_INT, mspace, mspace, H5P_DEFAULT,
                 orig_data) < 0) goto error;
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset int with fill value (read)");

    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_INT, mspace, mspace, H5P_DEFAULT,
                new_data) < 0) goto error;

    /* Check that the values read are the same as the values written */
    for(j=0; j<(size_t)size[1]; j++) {
        if(new_data[0][j] != orig_data[0][j]) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %lu,%lu\n", (unsigned long)0, (unsigned long)j);
            goto error;
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();

    return SUCCEED;
error:
    return FAIL;
} /* end test_scaleoffset_int_2() */


/*-------------------------------------------------------------------------
 * Function:    test_scaleoffset_float
 *
 * Purpose:     Tests the float datatype for scaleoffset filter, with fill
 *              value undefined, using variable-minimum-bits method
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Wednesday, Apr. 20th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_scaleoffset_float(hid_t file)
{
    hid_t               dataset, datatype, space, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2,5};
    float               orig_data[2][5];
    float               new_data[2][5];
    size_t              i, j;

    TESTING("    scaleoffset float without fill value, D-scaling (setup)");

    datatype = H5Tcopy(H5T_NATIVE_FLOAT);

    /* Set order of dataset datatype */
    if(H5Tset_order(datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Create the dataset property list  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* Fill value undefined */
    if(H5Pset_fill_value(dc, datatype, NULL) < 0) goto error;

    /* Set up to use scaleoffset filter, decimal scale factor is 3,
     * use variable-minimum-bits method
     */
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_scaleoffset(dc, H5Z_SO_FLOAT_DSCALE,3) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_SCALEOFFSET_FLOAT_NAME, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Initialize data */
    for(i= 0;i< (size_t)size[0]; i++)
      for(j = 0; j < (size_t)size[1]; j++) {
        orig_data[i][j] = (float)(HDrandom() % 100000) / 1000.0F;

        /* even-numbered values are negtive */
        if((i*size[1]+j+1)%2 == 0)
            orig_data[i][j] = -orig_data[i][j];
      }

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Test scaleoffset by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset float without fill value, D-scaling (write)");

    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 orig_data) < 0) goto error;
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset float without fill value, D-scaling (read)");

    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                new_data) < 0) goto error;

    /* Check that the values read are the same as the values written */
    for(i=0; i<(size_t)size[0]; i++) {
        for(j=0; j<(size_t)size[1]; j++) {
            if(HDfabs(new_data[i][j]-orig_data[i][j]) > HDpow(10.0F, -3.0F)) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();

    return SUCCEED;
error:
    return FAIL;
} /* end test_scaleoffset_float() */


/*-------------------------------------------------------------------------
 * Function:    test_scaleoffset_float_2
 *
 * Purpose:     Tests the float datatype for scaleoffset filter, with fill
 *              value set, using variable-minimum-bits method
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Wednesday, Apr. 20th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_scaleoffset_float_2(hid_t file)
{
    hid_t               dataset, datatype, space, mspace, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2,5};
    float               orig_data[2][5];
    float               new_data[2][5];
    float               fillval;
    hsize_t             start[2];  /* Start of hyperslab */
    hsize_t             stride[2]; /* Stride of hyperslab */
    hsize_t             count[2];  /* Block count */
    hsize_t             block[2];  /* Block sizes */
    size_t              j;

    TESTING("    scaleoffset float with fill value, D-scaling (setup)");

    datatype = H5Tcopy(H5T_NATIVE_FLOAT);

    /* Set order of dataset datatype */
    if(H5Tset_order(datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create the data space for the dataset */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Create the dataset property list  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* Set fill value */
    fillval = 10000.0F;
    if(H5Pset_fill_value(dc, H5T_NATIVE_FLOAT, &fillval) < 0) goto error;

    /* Set up to use scaleoffset filter, decimal scale factor is 3,
     * use variable-minimum-bits method
     */
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_scaleoffset(dc, H5Z_SO_FLOAT_DSCALE,3) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_SCALEOFFSET_FLOAT_NAME_2, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Create the memory data space */
    if((mspace = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Select hyperslab for data to write, using 1x5 blocks,
     * (1,1) stride and (1,1) count starting at the position (0,0).
     */
    start[0]  = 0; start[1]  = 0;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 1; count[1]  = 1;
    block[0]  = 1; block[1]  = 5;
    if(H5Sselect_hyperslab(mspace, H5S_SELECT_SET, start,
                           stride, count, block) < 0) goto error;

    /* Initialize data of hyperslab */
    for(j = 0; j < (size_t)size[1]; j++) {
        orig_data[0][j] = (float)(HDrandom() % 100000) / 1000.0F;

        /* even-numbered values are negtive */
        if((j + 1) % 2 == 0)
            orig_data[0][j] = -orig_data[0][j];
    }

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Test scaleoffset by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset float with fill value, D-scaling (write)");

    /* only data in the hyperslab will be written, other value should be fill value */
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, mspace, mspace, H5P_DEFAULT,
                 orig_data) < 0) goto error;
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset float with fill value, D-scaling (read)");

    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_FLOAT, mspace, mspace, H5P_DEFAULT,
                new_data) < 0) goto error;

    /* Check that the values read are the same as the values written */
    for(j=0; j<(size_t)size[1]; j++) {
        if(HDfabs(new_data[0][j]-orig_data[0][j]) > HDpow(10.0F, -3.0F)) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %lu,%lu\n", (unsigned long)0, (unsigned long)j);
            goto error;
        }
    }
    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();

    return SUCCEED;
error:
    return FAIL;
} /* end test_scaleoffset_float_2() */


/*-------------------------------------------------------------------------
 * Function:    test_scaleoffset_double
 *
 * Purpose:     Tests the double datatype for scaleoffset filter, with fill
 *              value undefined, using variable-minimum-bits method
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Monday, Apr. 25th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_scaleoffset_double(hid_t file)
{
    hid_t               dataset, datatype, space, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2,5};
    double              orig_data[2][5];
    double              new_data[2][5];
    size_t              i, j;

    TESTING("    scaleoffset double without fill value, D-scaling (setup)");

    datatype = H5Tcopy(H5T_NATIVE_DOUBLE);

    /* Set order of dataset datatype */
    if(H5Tset_order(datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Create the dataset property list  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* Fill value undefined */
    if(H5Pset_fill_value(dc, datatype, NULL) < 0) goto error;

    /* Set up to use scaleoffset filter, decimal scale factor is 7,
     * use variable-minimum-bits method
     */
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_scaleoffset(dc, H5Z_SO_FLOAT_DSCALE,7) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_SCALEOFFSET_DOUBLE_NAME, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Initialize data */
    for(i= 0;i< (size_t)size[0]; i++)
      for(j = 0; j < (size_t)size[1]; j++) {
        orig_data[i][j] = (float)(HDrandom() % 10000000) / 10000000.0F;

        /* even-numbered values are negtive */
        if((i* size[1] + j + 1) % 2 == 0)
            orig_data[i][j] = -orig_data[i][j];
      }

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Test scaleoffset by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset double without fill value, D-scaling (write)");

    if(H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 orig_data) < 0) goto error;
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset double without fill value, D-scaling (read)");

    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                new_data) < 0) goto error;

    /* Check that the values read are the same as the values written */
    for(i=0; i<(size_t)size[0]; i++) {
        for(j=0; j<(size_t)size[1]; j++) {
            if(HDfabs(new_data[i][j]-orig_data[i][j]) > HDpow(10.0F, -7.0F)) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();

    return SUCCEED;
error:
    return FAIL;
} /* end test_scaleoffset_double() */


/*-------------------------------------------------------------------------
 * Function:    test_scaleoffset_double_2
 *
 * Purpose:     Tests the double datatype for scaleoffset filter, with fill
 *              value set, using variable-minimum-bits method
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Monday, Apr. 25th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_scaleoffset_double_2(hid_t file)
{
    hid_t               dataset, datatype, space, mspace, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2,5};
    double              orig_data[2][5];
    double              new_data[2][5];
    double              fillval;
    hsize_t             start[2];  /* Start of hyperslab */
    hsize_t             stride[2]; /* Stride of hyperslab */
    hsize_t             count[2];  /* Block count */
    hsize_t             block[2];  /* Block sizes */
    size_t              j;

    TESTING("    scaleoffset double with fill value, D-scaling (setup)");

    datatype = H5Tcopy(H5T_NATIVE_DOUBLE);

    /* Set order of dataset datatype */
    if(H5Tset_order(datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create the data space for the dataset */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Create the dataset property list  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* Set fill value */
    fillval = 10000.0F;
    if(H5Pset_fill_value(dc, H5T_NATIVE_DOUBLE, &fillval) < 0) goto error;

    /* Set up to use scaleoffset filter, decimal scale factor is 7,
     * use variable-minimum-bits method
     */
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_scaleoffset(dc, H5Z_SO_FLOAT_DSCALE,7) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_SCALEOFFSET_DOUBLE_NAME_2, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Create the memory data space */
    if((mspace = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Select hyperslab for data to write, using 1x5 blocks,
     * (1,1) stride and (1,1) count starting at the position (0,0).
     */
    start[0]  = 0; start[1]  = 0;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 1; count[1]  = 1;
    block[0]  = 1; block[1]  = 5;
    if(H5Sselect_hyperslab(mspace, H5S_SELECT_SET, start,
                           stride, count, block) < 0) goto error;

    /* Initialize data of hyperslab */
    for(j = 0; j < (size_t)size[1]; j++) {
        orig_data[0][j] = (float)(HDrandom() % 10000000) / 10000000.0F;

        /* even-numbered values are negtive */
        if((j + 1) % 2 == 0)
            orig_data[0][j] = -orig_data[0][j];
    }

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Test scaleoffset by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset double with fill value, D-scaling (write)");

    /* only data in the hyperslab will be written, other value should be fill value */
    if(H5Dwrite(dataset, H5T_NATIVE_DOUBLE, mspace, mspace, H5P_DEFAULT,
                 orig_data) < 0) goto error;
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset double with fill value, D-scaling (read)");

    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_DOUBLE, mspace, mspace, H5P_DEFAULT,
                new_data) < 0) goto error;

    /* Check that the values read are the same as the values written */
    for(j=0; j<(size_t)size[1]; j++) {
        if(HDfabs(new_data[0][j]-orig_data[0][j]) > HDpow(10.0F, -7.0F)) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %lu,%lu\n", (unsigned long)0, (unsigned long)j);
            goto error;
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();

    return SUCCEED;
error:
    return FAIL;
} /* end test_scaleoffset_double_2() */


/*-------------------------------------------------------------------------
 * Function:    test_multiopen
 *
 * Purpose:     Tests that a bug no longer exists.  If a dataset is opened
 *              twice and one of the handles is used to extend the dataset,
 *              then the other handle should return the new size when
 *              queried.
 *
 * Return:      Success:    0
 *
 *              Failure:    -1
 *
 * Programmer:  Robb Matzke
 *              Tuesday, June  9, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_multiopen (hid_t file)
{
    hid_t   dcpl = -1, space = -1, dset1 = -1, dset2 = -1;
    hsize_t cur_size[1] = {10};
    hsize_t tmp_size[1];
    static hsize_t max_size[1] = {H5S_UNLIMITED};

    TESTING("multi-open with extending");

    /* Create the dataset and open it twice */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dcpl, 1, cur_size) < 0) goto error;
    if((space = H5Screate_simple(1, cur_size, max_size)) < 0) goto error;
    if((dset1 = H5Dcreate2(file, "multiopen", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) goto error;
    if((dset2 = H5Dopen2(dset1, ".", H5P_DEFAULT)) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;

    /* Extend with the first handle */
    cur_size[0] = 20;
    if(H5Dset_extent(dset1, cur_size) < 0) goto error;

    /* Get the size from the second handle */
    if((space = H5Dget_space(dset2)) < 0) goto error;
    if(H5Sget_simple_extent_dims(space, tmp_size, NULL) < 0) goto error;
    if(cur_size[0] != tmp_size[0]) {
        H5_FAILED();
        HDprintf("    Got %d instead of %d!\n", (int)tmp_size[0], (int)cur_size[0]);
        goto error;
    } /* end if */

    if(H5Dclose(dset1) < 0) goto error;
    if(H5Dclose(dset2) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Pclose(dcpl) < 0) goto error;

    PASSED();
    return SUCCEED;

 error:
    H5E_BEGIN_TRY {
        H5Dclose(dset1);
        H5Dclose(dset2);
        H5Sclose(space);
        H5Pclose(dcpl);
    } H5E_END_TRY;
    return FAIL;
} /* end test_multiopen() */


/*-------------------------------------------------------------------------
 * Function:    test_types
 *
 * Purpose:    Make some datasets with various types so we can test h5ls.
 *
 * Return:    Success:    0
 *
 *        Failure:    -1
 *
 * Programmer:    Robb Matzke
 *              Monday, June  7, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_types(hid_t file)
{
    hid_t        grp=-1, type=-1, space=-1, dset=-1;
    size_t        i;
    hsize_t        nelmts;
    unsigned char    buf[32];

    TESTING("various datatypes");
    if((grp = H5Gcreate2(file, "typetests", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;

    /* bitfield_1 */
    nelmts = sizeof(buf);
    if((type=H5Tcopy(H5T_STD_B8LE)) < 0 ||
    (space=H5Screate_simple(1, &nelmts, NULL)) < 0 ||
    (dset=H5Dcreate2(grp, "bitfield_1", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
    goto error;
    for(i=0; i<sizeof buf; i++) buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if(H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
    goto error;

    if(H5Sclose(space) < 0) goto error;
    if(H5Tclose(type) < 0) goto error;
    if(H5Dclose(dset) < 0) goto error;

    /* bitfield_2 */
    nelmts = sizeof(buf)/2;
    if((type=H5Tcopy(H5T_STD_B16LE)) < 0 ||
    (space=H5Screate_simple(1, &nelmts, NULL)) < 0 ||
    (dset=H5Dcreate2(grp, "bitfield_2", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
    goto error;
    for(i=0; i<sizeof buf; i++) buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if(H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
    goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Tclose(type) < 0) goto error;
    if(H5Dclose(dset) < 0) goto error;

    /* opaque_1 */
    nelmts = sizeof(buf);
    if((type = H5Tcreate(H5T_OPAQUE, (size_t)1)) < 0 ||
            H5Tset_tag(type, "testing 1-byte opaque type") < 0 ||
            (space = H5Screate_simple(1, &nelmts, NULL)) < 0 ||
            (dset = H5Dcreate2(grp, "opaque_1", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
    goto error;
    for(i = 0; i < sizeof buf; i++)
        buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if(H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Tclose(type) < 0) goto error;
    if(H5Dclose(dset) < 0) goto error;

    /* opaque_2 */
    nelmts = sizeof(buf)/4;
    if((type = H5Tcreate(H5T_OPAQUE, (size_t)4)) < 0 ||
            H5Tset_tag(type, "testing 4-byte opaque type") < 0 ||
            (space = H5Screate_simple(1, &nelmts, NULL)) < 0 ||
            (dset = H5Dcreate2(grp, "opaque_2", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
    goto error;
    for(i = 0; i < sizeof buf; i++)
        buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if(H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Tclose(type) < 0) goto error;
    if(H5Dclose(dset) < 0) goto error;

    /* Cleanup */
    if(H5Gclose(grp) < 0) goto error;
    PASSED();
    return SUCCEED;

 error:
    H5E_BEGIN_TRY {
    H5Gclose(grp);
    H5Tclose(type);
    H5Sclose(space);
    H5Dclose(dset);
    } H5E_END_TRY;
    return FAIL;
} /* end test_types() */

/* This message derives from H5Z */
const H5Z_class2_t H5Z_CAN_APPLY_TEST[1] = {{
    H5Z_CLASS_T_VERS,
    H5Z_FILTER_CAN_APPLY_TEST,    /* Filter id number        */
    1, 1,
    "can_apply_test",        /* Filter name for debugging    */
    can_apply_bogus,            /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_bogus,        /* The actual filter function    */
}};


/*-------------------------------------------------------------------------
 * Function:    test_can_apply
 *
 * Purpose:    Tests library behavior when filter indicates it can't
 *              apply to certain combinations of creation parameters.
 *              The filter is mandate.  If the CAN_APPLY callback function
 *              indicates wrong datatype, the dataset creation should fail.
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Friday, April  5, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_can_apply(hid_t file)
{
    hid_t       dsid;           /* Dataset ID */
    hid_t       sid;            /* Dataspace ID */
    hid_t       dcpl;           /* Dataspace creation property list ID */
    const hsize_t dims[2] = {DSET_DIM1, DSET_DIM2};         /* Dataspace dimensions */
    const hsize_t chunk_dims[2] = {2, 25};      /* Chunk dimensions */
    hsize_t     dset_size;      /* Dataset size */
    size_t      i,j;            /* Local index variables */

    TESTING("dataset filter 'can apply' callback");

    /* Create dcpl with special filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't create dcpl\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't set chunk sizes\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Zregister (H5Z_CAN_APPLY_TEST) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't register 'can apply' filter\n",__LINE__);
        goto error;
    }
    /* The filter is mandate. */
    if(H5Pset_filter(dcpl, H5Z_FILTER_CAN_APPLY_TEST, 0, (size_t)0, NULL) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't set bogus filter\n",__LINE__);
        goto error;
    }

    /* Create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't open dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Create new dataset */
    /* (Should fail because the 'can apply' function should indicate inappropriate
     * combination.  And the filter is mandate.) */
    H5E_BEGIN_TRY {
        dsid = H5Dcreate2(file, DSET_CAN_APPLY_NAME, H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dsid >=0) {
        H5_FAILED();
        HDprintf("    Line %d: Shouldn't have created dataset!\n",__LINE__);
        H5Dclose(dsid);
        goto error;
    } /* end if */

    /* (Should fail because the 'can apply' function should fail) */
    H5E_BEGIN_TRY {
        dsid = H5Dcreate2(file, DSET_CAN_APPLY_NAME, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dsid >=0) {
        H5_FAILED();
        HDprintf("    Line %d: Shouldn't have created dataset!\n",__LINE__);
        H5Dclose(dsid);
        goto error;
    } /* end if */

    /* Create new dataset */
    if((dsid = H5Dcreate2(file, DSET_CAN_APPLY_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't create dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Write data */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Error writing dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Flush the file (to clear the cache) */
    if(H5Fflush(file, H5F_SCOPE_GLOBAL) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Error flushing file\n",__LINE__);
        goto error;
    } /* end if */

    /* Query the dataset's size on disk */
    if((dset_size=H5Dget_storage_size(dsid))==0) {
        H5_FAILED();
        HDprintf("    Line %d: Error querying dataset size\n",__LINE__);
        goto error;
    } /* end if */

    /* Verify that the size indicates data is uncompressed */
    if((H5Tget_size(H5T_NATIVE_INT)*dims[0]*dims[1])!=dset_size) {
        H5_FAILED();
        HDprintf("    Line %d: Incorrect dataset size: %lu\n",__LINE__,(unsigned long)dset_size);
        goto error;
    } /* end if */

    /* Read data */
    if(H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Error reading dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Compare data */
    /* Check that the values read are the same as the values written */
    for(i=0; i<(size_t)dims[0]; i++) {
    for(j=0; j<(size_t)dims[1]; j++) {
        if(points[i][j] != check[i][j]) {
        H5_FAILED();
        HDprintf("    Line %d: Read different values than written.\n",__LINE__);
        HDprintf("    At index %lu,%lu\n", (unsigned long)(i), (unsigned long)(j));
        HDprintf("    At original: %d\n",points[i][j]);
        HDprintf("    At returned: %d\n",check[i][j]);
        goto error;
        } /* end if */
    } /* end for */
    } /* end for */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataspace */
    if(H5Sclose(sid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset creation property list */
    if(H5Pclose(dcpl) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dcpl\n",__LINE__);
        goto error;
    } /* end if */


    PASSED();
    return SUCCEED;

error:
    return FAIL;
} /* end test_can_apply() */

/* This message derives from H5Z */
const H5Z_class2_t H5Z_CAN_APPLY_TEST2[1] = {{
    H5Z_CLASS_T_VERS,
    H5Z_FILTER_CAN_APPLY_TEST2,    /* Filter id number        */
    1, 1,
    "can_apply_test",        /* Filter name for debugging    */
    can_apply_bogus,            /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_bogus3,        /* The actual filter function    */
}};


/*-------------------------------------------------------------------------
 * Function:    test_can_apply2
 *
 * Purpose:    Tests library behavior when an optional filter indicates
 *              it can't apply to certain combinations of creation
 *              parameters.  The filter function FILTER_BOGUS3 does nothing
 *              than returning a failure.  Because the filter is optional,
 *              the library skips the filter even though the CAN_APPLY_BOGUS
 *              indicates the datatype DOUBLE can't apply to the dataset.
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Raymond Lu
 *              4 August 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_can_apply2(hid_t file)
{
    hid_t       dsid;           /* Dataset ID */
    hid_t       sid;            /* Dataspace ID */
    hid_t       dcpl;           /* Dataspace creation property list ID */
    const hsize_t dims[2] = {DSET_DIM1, DSET_DIM2};         /* Dataspace dimensions */
    const hsize_t chunk_dims[2] = {2, 25};      /* Chunk dimensions */
    hsize_t     dset_size;      /* Dataset size */
    size_t      i,j;            /* Local index variables */

    TESTING("dataset filter 'can apply' callback second");

    /* Create dcpl with special filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't create dcpl\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't set chunk sizes\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Zregister (H5Z_CAN_APPLY_TEST2) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't register 'can apply' filter\n",__LINE__);
        goto error;
    }
    /* The filter is optional. */
    if(H5Pset_filter(dcpl, H5Z_FILTER_CAN_APPLY_TEST2, H5Z_FLAG_OPTIONAL, (size_t)0, NULL) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't set bogus filter\n",__LINE__);
        goto error;
    }

    /* Create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't open dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Create new dataset */
    if((dsid = H5Dcreate2(file, DSET_CAN_APPLY_NAME2, H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't create dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Write data */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Error writing dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Flush the file (to clear the cache) */
    if(H5Fflush(file, H5F_SCOPE_GLOBAL) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Error flushing file\n",__LINE__);
        goto error;
    } /* end if */

    /* Query the dataset's size on disk */
    if((dset_size=H5Dget_storage_size(dsid))==0) {
        H5_FAILED();
        HDprintf("    Line %d: Error querying dataset size\n",__LINE__);
        goto error;
    } /* end if */

    /* Verify that the size indicates data is uncompressed */
    if((H5Tget_size(H5T_NATIVE_DOUBLE)*dims[0]*dims[1])!=dset_size) {
        H5_FAILED();
        HDprintf("    Line %d: Incorrect dataset size: %lu\n",__LINE__,(unsigned long)dset_size);
        goto error;
    } /* end if */

    /* Read data */
    if(H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Error reading dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Compare data */
    /* Check that the values read are the same as the values written */
    for(i=0; i<(size_t)dims[0]; i++) {
    for(j=0; j<(size_t)dims[1]; j++) {
        if(points[i][j] != check[i][j]) {
        H5_FAILED();
        HDprintf("    Line %d: Read different values than written.\n",__LINE__);
        HDprintf("    At index %lu,%lu\n", (unsigned long)(i), (unsigned long)(j));
        HDprintf("    At original: %d\n",points[i][j]);
        HDprintf("    At returned: %d\n",check[i][j]);
        goto error;
        } /* end if */
    } /* end for */
    } /* end for */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataspace */
    if(H5Sclose(sid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset creation property list */
    if(H5Pclose(dcpl) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dcpl\n",__LINE__);
        goto error;
    } /* end if */


    PASSED();
    return SUCCEED;

error:
    return FAIL;
} /* end test_can_apply2() */



/*-------------------------------------------------------------------------
 * Function:    test_can_apply_szip
 *
 * Purpose:    Tests library behavior when szip filter indicates it can't
 *              apply to certain combinations of creation parameters
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Monday, April  7, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_can_apply_szip(hid_t
#ifndef H5_HAVE_FILTER_SZIP
H5_ATTR_UNUSED
#endif /* H5_HAVE_FILTER_SZIP */
file)
{
#ifdef H5_HAVE_FILTER_SZIP
    hid_t       dsid;           /* Dataset ID */
    hid_t       sid;            /* Dataspace ID */
    hid_t       dcpl;           /* Dataspace creation property list ID */
    unsigned szip_options_mask=H5_SZIP_NN_OPTION_MASK;
    unsigned szip_pixels_per_block;
    const hsize_t dims[2] = {500, 4096};        /* Dataspace dimensions */
    const hsize_t dims2[2] = {4, 2};            /* Dataspace dimensions */
    const hsize_t chunk_dims[2] = {250, 2048};  /* Chunk dimensions */
    const hsize_t chunk_dims2[2] = {2, 1};      /* Chunk dimensions */
    herr_t      ret;            /* Status value */
#endif /* H5_HAVE_FILTER_SZIP */

    TESTING("dataset szip filter 'can apply' callback");

#ifdef H5_HAVE_FILTER_SZIP

    if(h5_szip_can_encode() == 1) {
    /* Create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't open dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Create dcpl with special filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't create dcpl\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't set chunk sizes\n",__LINE__);
        goto error;
    } /* end if */

    /* Set (invalid at property set time) szip parameters */
    szip_pixels_per_block=3;
    H5E_BEGIN_TRY {
        ret=H5Pset_szip (dcpl, szip_options_mask, szip_pixels_per_block);
    } H5E_END_TRY;
    if(ret>=0) {
        H5_FAILED();
        HDprintf("    Line %d: Shouldn't be able to set szip filter\n",__LINE__);
        goto error;
    }

    /* Set (invalid at property set time) szip parameters */
    szip_pixels_per_block=512;
    H5E_BEGIN_TRY {
        ret=H5Pset_szip (dcpl, szip_options_mask, szip_pixels_per_block);
    } H5E_END_TRY;
    if(ret>=0) {
        H5_FAILED();
        HDprintf("    Line %d: Shouldn't be able to set szip filter\n",__LINE__);
        goto error;
    }

    /* Set (invalid at dataset creation time) szip parameters */
    szip_pixels_per_block=2;
    if(H5Pset_szip (dcpl, szip_options_mask, szip_pixels_per_block) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't set szip filter\n",__LINE__);
        goto error;
    }

    /* Create new dataset */
    /* (Should succeed; according to the new algorithm, scanline should be reset
        to 2*128 satisfying 'maximum blocks per scanline' condition) */
    H5E_BEGIN_TRY {
        dsid = H5Dcreate2(file, DSET_CAN_APPLY_SZIP_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dsid <=0) {
        H5_FAILED();
        HDprintf("    Line %d: Should have created dataset!\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataspace */
    if(H5Sclose(sid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset creation property list */
    if(H5Pclose(dcpl) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dcpl\n",__LINE__);
        goto error;
    } /* end if */

    /* Create another data space */
    if((sid = H5Screate_simple(2, dims2, NULL)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't open dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Create dcpl with special filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't create dcpl\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_chunk(dcpl, 2, chunk_dims2) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't set chunk sizes\n",__LINE__);
        goto error;
    } /* end if */

    /* Set (invalid at dataset creation time) szip parameters */
    szip_pixels_per_block=32;
    if(H5Pset_szip (dcpl, szip_options_mask, szip_pixels_per_block) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't set szip filter\n",__LINE__);
        goto error;
    }

    /* Create new dataset */
    /* (Should fail because the 'can apply' filter should indicate inappropriate combination) */
    H5E_BEGIN_TRY {
        dsid = H5Dcreate2(file, DSET_CAN_APPLY_SZIP_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dsid >=0) {
        H5_FAILED();
        HDprintf("    Line %d: Shouldn't have created dataset!\n",__LINE__);
        H5Dclose(dsid);
        goto error;
    } /* end if */

    /* Close dataspace */
    if(H5Sclose(sid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset creation property list */
    if(H5Pclose(dcpl) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dcpl\n",__LINE__);
        goto error;
    } /* end if */


    PASSED();
} else {
    SKIPPED();
    HDputs("    Szip encoding is not enabled.");
}
#else /* H5_HAVE_FILTER_SZIP */
    SKIPPED();
    HDputs("    Szip filter is not enabled.");
#endif /* H5_HAVE_FILTER_SZIP */
    return SUCCEED;

#ifdef H5_HAVE_FILTER_SZIP
error:
    return FAIL;
#endif /* H5_HAVE_FILTER_SZIP */
} /* end test_can_apply_szip() */


/* This message derives from H5Z */
const H5Z_class2_t H5Z_SET_LOCAL_TEST[1] = {{
    H5Z_CLASS_T_VERS,
    H5Z_FILTER_SET_LOCAL_TEST,    /* Filter id number        */
    1, 1,
    "set_local_test",        /* Filter name for debugging    */
    NULL,                       /* The "can apply" callback     */
    set_local_bogus2,           /* The "set local" callback     */
    filter_bogus2,        /* The actual filter function    */
}};


/*-------------------------------------------------------------------------
 * Function:    test_set_local
 *
 * Purpose:    Tests library behavior for "set local" filter callback
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Monday, April  7, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_set_local(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       file;           /* File ID */
    hid_t       dsid;           /* Dataset ID */
    hid_t       sid;            /* Dataspace ID */
    hid_t       dcpl;           /* Dataspace creation property list ID */
    const hsize_t dims[2] = {DSET_DIM1, DSET_DIM2};         /* Dataspace dimensions */
    const hsize_t chunk_dims[2] = {2, 25};      /* Chunk dimensions */
    hsize_t     dset_size;      /* Dataset size */
    unsigned    cd_values[2]={BOGUS2_PARAM_1, BOGUS2_PARAM_2};   /* Parameters for Bogus2 filter */
    size_t      i,j;          /* Local index variables */
    double      n;          /* Local index variables */

    TESTING("dataset filter 'set local' callback");

    h5_fixname(FILENAME[5], fapl, filename, sizeof filename);

    /* Initialize the integer & floating-point dataset */
    n=1.0F;
    for(i = 0; i < DSET_DIM1; i++)
    for(j = 0; j < DSET_DIM2; j++) {
        points[i][j] = (int)n++;
        points_dbl[i][j] = (double)1.5F*n++;
    }

    /* Open file */
    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't open file\n",__LINE__);
    goto error;
    }

    /* Create dcpl with special filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't create dcpl\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't set chunk sizes\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Zregister (H5Z_SET_LOCAL_TEST) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't register 'set local' filter\n",__LINE__);
        goto error;
    }
    if(H5Pset_filter(dcpl, H5Z_FILTER_SET_LOCAL_TEST, 0, (size_t)BOGUS2_PERM_NPARMS, cd_values) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't set bogus2 filter\n",__LINE__);
        goto error;
    }

    /* Create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't open dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Create new dataset */
    if((dsid = H5Dcreate2(file, DSET_SET_LOCAL_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't create dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Write data */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Error writing dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Create new dataset */
    /* (Shouldn't get modified by output filter) */
    if((dsid = H5Dcreate2(file, DSET_SET_LOCAL_NAME_2, H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't create dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Write data */
    if(H5Dwrite(dsid, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, points_dbl) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Error writing dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataspace */
    if(H5Sclose(sid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dataspace\n", __LINE__);
        goto error;
    } /* end if */

    /* Close dataset creation property list */
    if(H5Pclose(dcpl) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dcpl\n", __LINE__);
        goto error;
    } /* end if */

    /* Close file (flushes & empties cache) */
    if(H5Fclose(file) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close file\n", __LINE__);
        goto error;
    } /* end if */

    /* Open file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't open file\n", __LINE__);
        goto error;
    }

    /* Re-open dataset */
    if((dsid = H5Dopen2(file, DSET_SET_LOCAL_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't open dataset\n", __LINE__);
        goto error;
    } /* end if */

    /* Query the dataset's size on disk */
    if((dset_size = H5Dget_storage_size(dsid)) == 0) {
        H5_FAILED();
        HDprintf("    Line %d: Error querying dataset size\n", __LINE__);
        goto error;
    } /* end if */

    /* Verify that the size indicates data is uncompressed */
    if((H5Tget_size(H5T_NATIVE_INT) * dims[0] * dims[1]) != dset_size) {
        H5_FAILED();
        HDprintf("    Line %d: Incorrect dataset size: %lu\n", __LINE__, (unsigned long)dset_size);
        goto error;
    } /* end if */

    /* Read data */
    if(H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Error reading dataset data\n", __LINE__);
        goto error;
    } /* end if */

    /* Compare data */
    /* Check that the values read are the modified version of what was written */
    for(i=0; i<dims[0]; i++) {
    for(j=0; j<dims[1]; j++) {
        if((points[i][j]+(int)sizeof(int)) != check[i][j]) {
        H5_FAILED();
        HDprintf("    Line %d: Read different values than written.\n",__LINE__);
        HDprintf("    At index %lu,%lu\n", (unsigned long)(i), (unsigned long)(j));
        HDprintf("    At original: %d\n",points[i][j]);
        HDprintf("    At returned: %d\n",check[i][j]);
        goto error;
        } /* end if */
    } /* end for */
    } /* end for */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dataset\n", __LINE__);
        goto error;
    } /* end if */

    /* Re-open second dataset */
    if((dsid = H5Dopen2(file, DSET_SET_LOCAL_NAME_2, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't open dataset\n", __LINE__);
        goto error;
    } /* end if */

    /* Query the dataset's size on disk */
    if((dset_size = H5Dget_storage_size(dsid)) == 0) {
        H5_FAILED();
        HDprintf("    Line %d: Error querying dataset size\n", __LINE__);
        goto error;
    } /* end if */

    /* Verify that the size indicates data is uncompressed */
    if((H5Tget_size(H5T_NATIVE_DOUBLE) * dims[0] * dims[1]) != dset_size) {
        H5_FAILED();
        HDprintf("    Line %d: Incorrect dataset size: %lu\n", __LINE__, (unsigned long)dset_size);
        goto error;
    } /* end if */

    /* Read data */
    if(H5Dread(dsid, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, check_dbl) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Error reading dataset data\n", __LINE__);
        goto error;
    } /* end if */

    /* Compare data */
    /* Check that the values read are the modified version of what was written */
    for(i=0; i<dims[0]; i++) {
    for(j=0; j<dims[1]; j++) {
        /* If the difference between two values is greater than 0.001%, they're
             * considered not equal. */
            if(!H5_DBL_REL_EQUAL(points_dbl[i][j], check_dbl[i][j], (double)0.00001F)) {
        H5_FAILED();
        HDprintf("    Line %d: Read different values than written.\n",__LINE__);
        HDprintf("    At index %lu,%lu\n", (unsigned long)(i), (unsigned long)(j));
        HDprintf("    At original: %f\n",points_dbl[i][j]);
        HDprintf("    At returned: %f\n",check_dbl[i][j]);
        goto error;
        } /* end if */
    } /* end for */
    } /* end for */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Close file */
    if(H5Fclose(file) < 0) {
        H5_FAILED();
        HDprintf("    Line %d: Can't close file\n",__LINE__);
        goto error;
    } /* end if */


    PASSED();
    return SUCCEED;

error:
    return FAIL;
} /* end test_set_local() */


/*-------------------------------------------------------------------------
 * Function:    test_compare_dcpl
 *
 * Purpose:    Verifies that if the same DCPL was used to create two
 *              datasets, the DCPLs retrieved from each dataset should
 *              compare equal.
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Wednesday, January  7, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compare_dcpl(hid_t file)
{
    hid_t       dsid=(-1);      /* Dataset ID */
    hid_t       sid=(-1);       /* Dataspace ID */
    hid_t       dcpl=(-1);      /* Dataspace creation property list ID */
    hid_t       dcpl1=(-1),dcpl2=(-1);          /* Dataspace creation property list IDs from datasets */
    const hsize_t dims[2] = {500, 4096};        /* Dataspace dimensions */
    const hsize_t chunk_dims[2] = {250, 2048};  /* Chunk dimensions */

    TESTING("comparing dataset creation property lists");

    /* Create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) TEST_ERROR

    /* Create dcpl with special filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) TEST_ERROR

    /* Set gzip parameter (if available) */
#ifdef H5_HAVE_FILTER_DEFLATE
    if(H5Pset_deflate (dcpl, 9) < 0) TEST_ERROR
#endif /* H5_HAVE_FILTER_DEFLATE */

    /* Create first dataset */
    if((dsid = H5Dcreate2(file, DSET_COMPARE_DCPL_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Get copy of dataset's dataset creation property list */
    if((dcpl1=H5Dget_create_plist(dsid)) < 0) TEST_ERROR

    /* Close dataset */
    if(H5Dclose (dsid) < 0) TEST_ERROR

    /* Create second dataset */
    if((dsid = H5Dcreate2(file, DSET_COMPARE_DCPL_NAME_2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Get copy of dataset's dataset creation property list */
    if((dcpl2=H5Dget_create_plist(dsid)) < 0) TEST_ERROR

    /* Close dataset */
    if(H5Dclose (dsid) < 0) TEST_ERROR

    /* Close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* Compare dataset creation property lists */
    if(H5Pequal(dcpl1,dcpl2)<=0) TEST_ERROR

    /* Close dataset creation property lists */
    if(H5Pclose(dcpl) < 0) TEST_ERROR
    if(H5Pclose(dcpl1) < 0) TEST_ERROR
    if(H5Pclose(dcpl2) < 0) TEST_ERROR


    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Pclose(dcpl1);
        H5Pclose(dcpl2);
    } H5E_END_TRY;
    return FAIL;
} /* end test_compare_dcpl() */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dcpl
 *
 * Purpose:    Verifies whether the copy of dataset creation property
 *              list works.  It tests the DCPL for chunked layout with
 *              filter and for contiguous layout with external storage.
 *              (Please see #1608 in Bugzilla)
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Raymond Lu
 *              28 January 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_copy_dcpl(hid_t file, hid_t fapl)
{
    hid_t       dsid1=(-1), dsid2=(-1);         /* Dataset ID */
    hid_t       new_dsid1=(-1), new_dsid2=(-1); /* Dataset ID */
    hid_t       sid=(-1);                       /* Dataspace ID */
    hid_t       dcpl=(-1);                      /* Dataset creation property list ID */
    hid_t       dcpl1=(-1),dcpl2=(-1);          /* Copies of creation property list IDs */
    hid_t       dcpl1_copy=(-1),dcpl2_copy=(-1);/* Copies of creation property list IDs */
    const hsize_t dims[2] = {500, 4096};        /* Dataspace dimensions */
    const hsize_t chunk_dims[2] = {250, 2048};  /* Chunk dimensions */
    char    filename[FILENAME_BUF_SIZE];
    hid_t       new_file=(-1);

    TESTING("copying dataset creation property lists");

    /* Create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) TEST_ERROR

    /* Create dcpl with special filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) TEST_ERROR
    if(H5Pset_fletcher32(dcpl) < 0) TEST_ERROR

    /* Create first dataset of chunking with filter */
    if((dsid1 = H5Dcreate2(file, DSET_COPY_DCPL_NAME_1, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl,
        H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close dataset */
    if(H5Dclose (dsid1) < 0) TEST_ERROR

    /* Reopen the first dataset */
    if((dsid1 = H5Dopen2(file, DSET_COPY_DCPL_NAME_1, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Get the copy of dataset's creation property list */
    if((dcpl1=H5Dget_create_plist(dsid1)) < 0) TEST_ERROR
    if((dcpl1_copy = H5Pcopy(dcpl1)) < 0) TEST_ERROR

    /* Close dataset */
    if(H5Dclose (dsid1) < 0) TEST_ERROR

    /* Change the DCPL for contiguous layout with external storage.  The size of the reserved
     * space in the external file is the size of the dataset - 500*4096*sizeof(int).
     * There's no need to clean up the external file since the library doesn't create it
     * until the data is written to it. */
    if(H5Pset_layout(dcpl, H5D_CONTIGUOUS) < 0) TEST_ERROR
    if(H5Premove_filter(dcpl, H5Z_FILTER_FLETCHER32) < 0) TEST_ERROR
    if(H5Pset_external(dcpl, COPY_DCPL_EXTFILE_NAME, (off_t)0, (hsize_t)(500 * 4096 * sizeof(int))) < 0) TEST_ERROR

    /* Create second dataset of contiguous layout with external storage */
    if((dsid2 = H5Dcreate2(file, DSET_COPY_DCPL_NAME_2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl,
        H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close dataset */
    if(H5Dclose (dsid2) < 0) TEST_ERROR

    /* Reopen the second dataset */
    if((dsid2 = H5Dopen2(file, DSET_COPY_DCPL_NAME_2, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Get copy of dataset's dataset creation property list */
    if((dcpl2=H5Dget_create_plist(dsid2)) < 0) TEST_ERROR
    if((dcpl2_copy = H5Pcopy(dcpl2)) < 0) TEST_ERROR

    /* Close dataset */
    if(H5Dclose (dsid2) < 0) TEST_ERROR

    /* Create a second file and create 2 datasets with the copies of the DCPLs in the first
     * file.  Test whether the copies of DCPLs work. */
    h5_fixname(FILENAME[13], fapl, filename, sizeof filename);
    if((new_file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((new_dsid1 = H5Dcreate2(new_file, DSET_COPY_DCPL_NAME_1, H5T_NATIVE_INT, sid,
            H5P_DEFAULT, dcpl1_copy, H5P_DEFAULT)) < 0) TEST_ERROR

    if((new_dsid2 = H5Dcreate2(new_file, DSET_COPY_DCPL_NAME_2, H5T_NATIVE_INT, sid,
            H5P_DEFAULT, dcpl2_copy, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* Close datasets */
    if(H5Dclose (new_dsid1) < 0) TEST_ERROR
    if(H5Dclose (new_dsid2) < 0) TEST_ERROR

    /* Close the second file */
    if(H5Fclose (new_file) < 0) TEST_ERROR

    /* Close dataset creation property lists */
    if(H5Pclose(dcpl) < 0) TEST_ERROR
    if(H5Pclose(dcpl1) < 0) TEST_ERROR
    if(H5Pclose(dcpl2) < 0) TEST_ERROR
    if(H5Pclose(dcpl1_copy) < 0) TEST_ERROR
    if(H5Pclose(dcpl2_copy) < 0) TEST_ERROR

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dsid1);
        H5Dclose(dsid2);
        H5Dclose(new_dsid1);
        H5Dclose(new_dsid2);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Pclose(dcpl1);
        H5Pclose(dcpl2);
        H5Pclose(dcpl1_copy);
        H5Pclose(dcpl2_copy);
    } H5E_END_TRY;
    return FAIL;
} /* end test_copy_dcpl() */


/*-------------------------------------------------------------------------
 * Function: test_filter_delete
 *
 * Purpose: Tests deletion of filters from a dataset creation property list
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: Pedro Vicente
 *              Monday, January 26, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filter_delete(hid_t file)
{
    H5Z_filter_t filtn;                 /* filter identification number */
    hid_t        dsid=-1;                  /* dataset ID */
    hid_t        sid=-1;                   /* dataspace ID */
    hid_t        dcpl=-1;                  /* dataset creation property list ID */
    hid_t        dcpl1=-1;                 /* dataset creation property list ID */
    hsize_t      dims[2]={20,20};       /* dataspace dimensions */
    hsize_t      chunk_dims[2]={10,10}; /* chunk dimensions */
    int          nfilters;              /* number of filters in DCPL */
    unsigned     flags;                 /* flags for filter */
    herr_t       ret;                   /* generic return value */
    int          i;

    TESTING("filter deletion");

#ifdef H5_HAVE_FILTER_DEFLATE
    /* create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) goto error;

    /* create dcpl  */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) goto error;

    if(H5Pset_fletcher32 (dcpl) < 0) goto error;
    if(H5Pset_deflate (dcpl, 6) < 0) goto error;
    if(H5Pset_shuffle (dcpl) < 0) goto error;

    /* create a dataset */
    if((dsid = H5Dcreate2(file,"dsetdel", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) goto error;

    /* get copy of dataset's dataset creation property list */
    if((dcpl1=H5Dget_create_plist(dsid)) < 0) goto error;

   /*----------------------------------------------------------------------
    * delete the deflate filter
    *----------------------------------------------------------------------
    */
    /* delete the deflate filter */
    if(H5Premove_filter(dcpl1,H5Z_FILTER_DEFLATE) < 0) goto error;

    /* get information about filters */
    if((nfilters = H5Pget_nfilters(dcpl1)) < 0) goto error;

    /* check if filter was deleted */
    for(i=0; i<nfilters; i++) {
        filtn = H5Pget_filter2(dcpl1, (unsigned)i, NULL, NULL, NULL, (size_t)0, NULL, NULL);
        if(H5Z_FILTER_DEFLATE==filtn)
            goto error;
    }

    /* try to get the info for the deflate filter */
    H5E_BEGIN_TRY {
        ret = H5Pget_filter_by_id2(dcpl1, H5Z_FILTER_DEFLATE, &flags, NULL, NULL, (size_t)0, NULL, NULL);
    } H5E_END_TRY;
    if(ret >=0) {
        H5_FAILED();
        HDprintf("    Line %d: Shouldn't have deleted filter!\n",__LINE__);
        goto error;
    } /* end if */

    /* try to delete the deflate filter again */
    H5E_BEGIN_TRY {
        ret=H5Premove_filter(dcpl1,H5Z_FILTER_DEFLATE);
    } H5E_END_TRY;
    if(ret >=0) {
        H5_FAILED();
        HDprintf("    Line %d: Shouldn't have deleted filter!\n",__LINE__);
        goto error;
    } /* end if */

   /*----------------------------------------------------------------------
    * delete all filters
    *----------------------------------------------------------------------
    */
    /* delete all filters */
    if(H5Premove_filter(dcpl1,H5Z_FILTER_ALL) < 0) goto error;

    /* get information about filters */
    if((nfilters = H5Pget_nfilters(dcpl1)) < 0) goto error;

    /* check if filters were deleted */
    if(nfilters)goto error;

   /*----------------------------------------------------------------------
    * close
    *----------------------------------------------------------------------
    */

    /* clean up objects used for this test */
    if(H5Pclose (dcpl) < 0) goto error;
    if(H5Pclose (dcpl1) < 0) goto error;
    if(H5Dclose (dsid) < 0) goto error;
    if(H5Sclose (sid) < 0) goto error;

    PASSED();
#else
    SKIPPED();
#endif
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Pclose(dcpl1);
        H5Dclose(dsid);
        H5Sclose(sid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_filter_delete() */



/*-------------------------------------------------------------------------
 * Function: auxread_fdata
 *
 * Purpose: reads a dataset "NAME" from FID
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: Pedro Vicente
 *              Monday, March 8, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
auxread_fdata(hid_t fid, const char *name)
{
    hid_t     dset_id=-1;           /* dataset ID */
    hid_t     dcpl_id=-1;           /* dataset creation property list ID */
    hid_t     space_id=-1;          /* space ID */
    hid_t     ftype_id=-1;          /* file data type ID */
    hid_t     mtype_id=-1;          /* memory data type ID */
    size_t    msize;             /* memory size of memory type */
    void      *buf=NULL;         /* data buffer */
    hsize_t   nelmts;            /* number of elements in dataset */
    int       rank;              /* rank of dataset */
    hsize_t   dims[H5S_MAX_RANK];/* dimensions of dataset */
    int       i;

    if((dset_id = H5Dopen2(fid, name, H5P_DEFAULT)) < 0)
        goto error;
    if((space_id = H5Dget_space(dset_id)) < 0)
        goto error;
    if((ftype_id = H5Dget_type(dset_id)) < 0)
        goto error;
    if((dcpl_id = H5Dget_create_plist(dset_id)) < 0)
        goto error;
    if((rank = H5Sget_simple_extent_ndims(space_id)) < 0)
        goto error;
    HDmemset(dims, 0, sizeof dims);
    if(H5Sget_simple_extent_dims(space_id, dims, NULL) < 0)
        goto error;
    nelmts = 1;
    for(i = 0; i < rank; i++)
        nelmts *= dims[i];
    if((mtype_id = H5Tget_native_type(ftype_id, H5T_DIR_DEFAULT)) < 0)
        goto error;
    if((msize = H5Tget_size(mtype_id)) == 0)
        goto error;

    if(nelmts) {
        buf = (void *)HDmalloc((size_t)(nelmts * msize));
        if(buf == NULL) {
            HDprintf( "cannot read into memory\n" );
            goto error;
        }
        if(H5Dread(dset_id, mtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
            goto error;
    }

    if(H5Pclose(dcpl_id) < 0)
        goto error;
    if(H5Sclose(space_id) < 0)
        goto error;
    if(H5Dclose(dset_id) < 0)
        goto error;
    if(buf)
        HDfree(buf);

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl_id);
        H5Sclose(space_id);
        H5Dclose(dset_id);
        H5Tclose(ftype_id);
        H5Tclose(mtype_id);
        if(buf)
            HDfree(buf);
    } H5E_END_TRY;
    return FAIL;
} /* end auxread_fdata() */


/*-------------------------------------------------------------------------
 * Function: test_filters_endianess
 *
 * Purpose: Reads/writes data with filters (big-endian/little-endian data)
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: Pedro Vicente
 *              Monday, March 8, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filters_endianess(void)
{
    hid_t     fid=-1;                   /* file ID */
    hid_t     dsid=-1;                  /* dataset ID */
    hid_t     sid=-1;                   /* dataspace ID */
    hid_t     dcpl=-1;                  /* dataset creation property list ID */
    const char *data_file = H5_get_srcdir_filename("test_filters_le.h5"); /* Corrected test file name */

    TESTING("filters with big-endian/little-endian data");

   /*-------------------------------------------------------------------------
    * step 1: open a file written on a little-endian machine
    *-------------------------------------------------------------------------
    */

    /* open */
    if((fid = H5Fopen(data_file, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* read */
    if(auxread_fdata(fid,"dset") < 0) TEST_ERROR

    /* close */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

   /*-------------------------------------------------------------------------
    * step 2: open a file written on a big-endian machine
    *-------------------------------------------------------------------------
    */

    /* compose the name of the file to open, using the srcdir, if appropriate */
    data_file = H5_get_srcdir_filename("test_filters_be.h5"); /* Corrected test file name */

    /* open */
    if((fid = H5Fopen(data_file, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* read */
    if(auxread_fdata(fid,"dset") < 0) TEST_ERROR

    /* close */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_filters_endianess() */


/*-------------------------------------------------------------------------
 * Function: test_zero_dims
 *
 * Purpose: Tests read/writes to zero-sized extendible datasets
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: Quincey Koziol
 *              Tuesday, July 27, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_zero_dims(hid_t file)
{
    hid_t       s = -1, d = -1, dcpl = -1;
    hid_t       s2 = -1, d2 = -1, dcpl2 = -1;
    hsize_t     dzero = 0, dmax = H5S_UNLIMITED, csize = 5;
    hsize_t     dzero2[2] = {0, 0};
    hsize_t     dmax2[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t    csize2[2] = {5, 5};
    hid_t    fapl;        /* File access property list */
    H5D_chunk_index_t idx_type; /* Dataset chunk index type */
    H5F_libver_t low;           /* File format low bound */
    herr_t      ret;

    TESTING("I/O on datasets with zero-sized dims");

    /* Get the file's file access property list */
    if((fapl = H5Fget_access_plist(file)) < 0) FAIL_STACK_ERROR

    /* Get library format */
    if(H5Pget_libver_bounds(fapl, &low, NULL) < 0) FAIL_STACK_ERROR

    /* Close FAPL */
    if(H5Pclose(fapl) < 0) TEST_ERROR

    /*
     * One-dimensional dataset
     */
    if((s = H5Screate_simple(1, &dzero, &dmax)) < 0) FAIL_STACK_ERROR

    /* Try creating chunked dataset with undefined chunk dimensions */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR
    if(H5Pset_layout(dcpl, H5D_CHUNKED) < 0) FAIL_STACK_ERROR

    H5E_BEGIN_TRY {
        d = H5Dcreate2(file, ZERODIM_DATASET, H5T_NATIVE_INT, s, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(d > 0) {
        H5Dclose(d);
        FAIL_PUTS_ERROR("created dataset with undefined chunk dimensions")
    } /* end if */

    /* Try creating chunked dataset with zero-sized chunk dimensions */
    H5E_BEGIN_TRY {
        ret = H5Pset_chunk(dcpl, 1, &dzero);
    } H5E_END_TRY;
    if(ret > 0)
        FAIL_PUTS_ERROR("set zero-sized chunk dimensions")

    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR

    /* Create the zero-sized extendible dataset */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR
    if(H5Pset_chunk(dcpl, 1, &csize) < 0) FAIL_STACK_ERROR
    if((d = H5Dcreate2(file, ZERODIM_DATASET, H5T_NATIVE_INT, s, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Get the chunk index type */
    if(H5D__layout_idx_type_test(d, &idx_type) < 0) FAIL_STACK_ERROR

    /* Verify index type */
    if(low == H5F_LIBVER_LATEST) {
    if(idx_type != H5D_CHUNK_IDX_EARRAY)
        FAIL_PUTS_ERROR("should be using extensible array as index");
    } else if(idx_type != H5D_CHUNK_IDX_BTREE)
    FAIL_PUTS_ERROR("should be using v1 B-tree as index");

    /* Various no-op writes */
    if(H5Dwrite(d, H5T_NATIVE_INT, s, s, H5P_DEFAULT, (void*)911) < 0) FAIL_STACK_ERROR
    if(H5Dwrite(d, H5T_NATIVE_INT, s, s, H5P_DEFAULT, NULL) < 0) FAIL_STACK_ERROR
    if(H5Dwrite(d, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void*)911) < 0) FAIL_STACK_ERROR
    if(H5Dwrite(d, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, NULL) < 0) FAIL_STACK_ERROR

    /* Various no-op reads */
    if(H5Dread(d, H5T_NATIVE_INT, s, s, H5P_DEFAULT, (void*)911) < 0) FAIL_STACK_ERROR
    if(H5Dread(d, H5T_NATIVE_INT, s, s, H5P_DEFAULT, NULL) < 0) FAIL_STACK_ERROR
    if(H5Dread(d, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void*)911) < 0) FAIL_STACK_ERROR
    if(H5Dread(d, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, NULL) < 0) FAIL_STACK_ERROR

    if(H5Dclose(d) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Sclose(s) < 0) FAIL_STACK_ERROR

    /*
     * Two-dimensional dataset
     */
    if((s2 = H5Screate_simple(2, dzero2, dmax2)) < 0) FAIL_STACK_ERROR

    /* Try creating chunked dataset with undefined chunk dimensions */
    if((dcpl2 = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR
    if(H5Pset_layout(dcpl2, H5D_CHUNKED) < 0) FAIL_STACK_ERROR

    H5E_BEGIN_TRY {
        d2 = H5Dcreate2(file, ZERODIM_DATASET2, H5T_NATIVE_INT, s2, H5P_DEFAULT, dcpl2, H5P_DEFAULT);
    } H5E_END_TRY;
    if(d2 > 0) {
        H5Dclose(d2);
        FAIL_PUTS_ERROR("created dataset with undefined chunk dimensions")
    } /* end if */

    /* Try creating chunked dataset with zero-sized chunk dimensions */
    H5E_BEGIN_TRY {
        ret = H5Pset_chunk(dcpl2, 2, dzero2);
    } H5E_END_TRY;
    if(ret > 0)
        FAIL_PUTS_ERROR("set zero-sized chunk dimensions")

    if(H5Pclose(dcpl2) < 0) FAIL_STACK_ERROR

    /* Write to the zero-sized extendible dataset */
    if((dcpl2 = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR
    if(H5Pset_chunk(dcpl2, 2, csize2) < 0) FAIL_STACK_ERROR

    /* Create the dataset */
    if((d2 = H5Dcreate2(file, ZERODIM_DATASET2, H5T_NATIVE_INT, s2, H5P_DEFAULT, dcpl2, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Get the chunk index type */
    if(H5D__layout_idx_type_test(d2, &idx_type) < 0) FAIL_STACK_ERROR

    /* Verify index type */
    if(low == H5F_LIBVER_LATEST) {
    if(idx_type != H5D_CHUNK_IDX_BT2)
        FAIL_PUTS_ERROR("should be using v2 B-tree as index");
    } else if(idx_type != H5D_CHUNK_IDX_BTREE)
    FAIL_PUTS_ERROR("should be using v1 B-tree as index");

    /* Just a no-op */
    if(H5Dwrite(d2, H5T_NATIVE_INT, s2, s2, H5P_DEFAULT, (void*)911) < 0) FAIL_STACK_ERROR

    if(H5Dclose(d2) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl2) < 0) FAIL_STACK_ERROR
    if(H5Sclose(s2) < 0) FAIL_STACK_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);

        H5Pclose(dcpl);
        H5Dclose(d);
        H5Sclose(s);

        H5Pclose(dcpl2);
        H5Dclose(d2);
        H5Sclose(s2);
    } H5E_END_TRY;
    return FAIL;
} /* end test_zero_dims() */


/*-------------------------------------------------------------------------
 * Function: test_missing_chunk
 *
 * Purpose: Tests that reads from chunked dataset with undefined fill value and
 *              not all chunks written don't overwrite data in user's buffer
 *              for missing chunks.
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: Quincey Koziol
 *              Tuesday, August 25, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_missing_chunk(hid_t file)
{
    hid_t       d = -1, did2 = -1;        /* Dataset IDs */
    hid_t    dcpl = -1, dcpl2 = -1;        /* Dataset creation property IDs */
    hid_t       s = -1, sid2 = -1;         /* Dataspace ID */
    hsize_t    hs_start[1], hs_stride[1], hs_count[1], hs_block[1];    /* Hyperslab setting */
    hsize_t    hs_start2[2], hs_stride2[2], hs_count2[2], hs_block2[2];/* Hyperslab setting */

    /* Buffers for reading/writing dataset */
    int         wdata[MISSING_CHUNK_DIM],
                rdata[MISSING_CHUNK_DIM];
    int         wdata2[MISSING_CHUNK_DIM][MISSING_CHUNK_DIM],
                rdata2[MISSING_CHUNK_DIM][MISSING_CHUNK_DIM];

    /* Setting for 1-D dataset */
    hsize_t     dsize=100, dmax=H5S_UNLIMITED;
    hsize_t    csize=5;

    /* Setting for 2-D dataset */
    hsize_t     dsize2[2] = {100, 100}, dmax2[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t    csize2[2] = {5, 5};
    size_t      u, i, j;    /* Local Index variable */

    hid_t    fapl;        /* File access property list */
    H5F_libver_t low;              /* File format low bound */
    H5D_chunk_index_t idx_type, idx_type2;     /* Dataset chunk index types */

    TESTING("Read dataset with unwritten chunk & undefined fill value");

    /* Get the file's file access property list */
    if((fapl = H5Fget_access_plist(file)) < 0) TEST_ERROR;

    /* Get library format */
    if(H5Pget_libver_bounds(fapl, &low, NULL) < 0) TEST_ERROR;

    /* Close FAPL */
    if(H5Pclose(fapl) < 0) TEST_ERROR

    /* Initialize data for 1-D dataset */
    for(u = 0; u < MISSING_CHUNK_DIM; u++) {
        wdata[u] = (int)u;
        rdata[u] = 911;
    } /* end for */

    /* Initialize data for 2-D dataset */
    for(i = 0; i < MISSING_CHUNK_DIM; i++) {
    for(j = 0; j < MISSING_CHUNK_DIM; j++) {
        wdata2[i][j] = (int)(j + (i * MISSING_CHUNK_DIM));
        rdata2[i][j] = 911;
    }
    } /* end for */

    /* Create dataspace */
    if((s = H5Screate_simple(1, &dsize, &dmax)) < 0) TEST_ERROR;
    if((sid2 = H5Screate_simple(2, dsize2, dmax2)) < 0) TEST_ERROR;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;
    if((dcpl2 = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;

    /* Set to chunked */
    if(H5Pset_chunk(dcpl, 1, &csize) < 0) TEST_ERROR;
    if(H5Pset_chunk(dcpl2, 2, csize2) < 0) TEST_ERROR;

    /* Undefine fill value */
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_INT, NULL) < 0) TEST_ERROR;
    if(H5Pset_fill_value(dcpl2, H5T_NATIVE_INT, NULL) < 0) TEST_ERROR;

    /* Create the 1-D & 2-D datasets */
    if((d = H5Dcreate2(file, MISSING_CHUNK_DATASET, H5T_NATIVE_INT, s, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) TEST_ERROR;
    if((did2 = H5Dcreate2(file, MISSING_CHUNK_DATASET2, H5T_NATIVE_INT, sid2, H5P_DEFAULT, dcpl2, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Get the chunk index types */
    if(H5D__layout_idx_type_test(d, &idx_type) < 0) TEST_ERROR;
    if(H5D__layout_idx_type_test(did2, &idx_type2) < 0) TEST_ERROR;

    /* Verify index type */
    if(low == H5F_LIBVER_LATEST) {
        if(idx_type != H5D_CHUNK_IDX_EARRAY)
            FAIL_PUTS_ERROR("should be using Extensible Array as index");
        if(idx_type2 != H5D_CHUNK_IDX_BT2)
            FAIL_PUTS_ERROR("should be using v2 B-tree as index");
    } else {
    if(idx_type != H5D_CHUNK_IDX_BTREE)
        FAIL_PUTS_ERROR("should be using v1 B-tree as index");
    if(idx_type2 != H5D_CHUNK_IDX_BTREE)
        FAIL_PUTS_ERROR("should be using v1 B-tree as index");
    }

    /* Select elements in every other chunk for 1-D dataset */
    hs_start[0]=0;
    hs_stride[0]=10;
    hs_count[0]=10;
    hs_block[0]=5;
    if(H5Sselect_hyperslab(s, H5S_SELECT_SET, hs_start, hs_stride, hs_count,
                hs_block) < 0) TEST_ERROR;

    /* Select elements in every other chunk for 2-D dataset */
    hs_start2[0] = hs_start2[1] = 0;
    hs_stride2[0] = hs_stride2[1] = 10;
    hs_count2[0] = hs_count2[1] = 10;
    hs_block2[0] = hs_block2[1] = 5;
    if(H5Sselect_hyperslab(sid2, H5S_SELECT_SET, hs_start2, hs_stride2, hs_count2,
                hs_block2) < 0) TEST_ERROR;

    /* Write selected data to the datasets */
    if(H5Dwrite(d, H5T_NATIVE_INT, s, s, H5P_DEFAULT, wdata) < 0) TEST_ERROR;
    if(H5Dwrite(did2, H5T_NATIVE_INT, sid2, sid2, H5P_DEFAULT, wdata2) < 0) TEST_ERROR;

    /* Read all data from the datasets */
    if(H5Dread(d, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0) TEST_ERROR;
    if(H5Dread(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata2) < 0) TEST_ERROR;

    /* Validata values read for the 1-D dataset */
    for(u=0; u<MISSING_CHUNK_DIM; u++) {
        if((u%10)>=5) {
            if(rdata[u]!=911) {
                HDprintf("    Line %d: Incorrect value, rdata[%u]=%d\n",__LINE__,(unsigned)u,rdata[u]);
                TEST_ERROR;
            } /* end if */
        } /* end if */
        else {
            if(rdata[u]!=wdata[u]) {
                HDprintf("    Line %d: Incorrect value, wdata[%u]=%d, rdata[%u]=%d\n",__LINE__,(unsigned)u,wdata[u],(unsigned)u,rdata[u]);
                TEST_ERROR;
            } /* end if */
        } /* end else */
    } /* end for */

    /* Validata values read for the 2-D dataset */
    for(i = 0; i < MISSING_CHUNK_DIM; i++) {
    for(j = 0; j < MISSING_CHUNK_DIM; j++) {

        if((i % 10) >= 5 || (j % 10) >= 5) {
        if(rdata2[i][j] != 911) {
            HDprintf("    Line %d: Incorrect value, rdata2[%u][%u] = %d\n",
            __LINE__,(unsigned)i, (unsigned)j, rdata2[i][j]);
            TEST_ERROR;
        } /* end if */
        } /* end if */
        else {
        if(rdata2[i][j] != wdata2[i][j]) {
            HDprintf("    Line %d: Incorrect value, wdata2[%u][%u] = %d, rdata2[%u][%u] = %d\n",
            __LINE__,(unsigned)i, (unsigned)j, wdata2[i][j],(unsigned)i, (unsigned)j, rdata2[i][j]);
            TEST_ERROR;
        } /* end if */
        } /* end else */
    } /* end for */
    } /* end for */

    /* Close everything */
    if(H5Pclose(dcpl) < 0) TEST_ERROR;
    if(H5Pclose(dcpl2) < 0) TEST_ERROR;
    if(H5Sclose(s) < 0) TEST_ERROR;
    if(H5Sclose(sid2) < 0) TEST_ERROR;
    if(H5Dclose(d) < 0) TEST_ERROR;
    if(H5Dclose(did2) < 0) TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);

        H5Pclose(dcpl);
        H5Pclose(dcpl2);
        H5Dclose(d);
        H5Dclose(did2);
        H5Sclose(s);
        H5Sclose(sid2);
    } H5E_END_TRY;
    return FAIL;
} /* end test_missing_chunk() */


/*-------------------------------------------------------------------------
 * Function: test_random_chunks_real
 *
 * Purpose: Tests that write/read on randomly selected chunks
 *
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: Christian Chilan
 *             Monday, March 26, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_random_chunks_real(const char *testname, hbool_t early_alloc, hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       s=-1, m=-1, d=-1, dcpl=-1, file=-1;
    int         wbuf[NPOINTS],
                rbuf[NPOINTS],
                check2[20][20];
    hsize_t     coord[NPOINTS][2];
    hsize_t     dsize[2]={100,100}, dmax[2]={H5S_UNLIMITED, H5S_UNLIMITED}, csize[2]={10,10}, nsize[2]={200,200};
    hsize_t    fixed_dmax[2] = {1000, 1000};
    hsize_t     msize[1]={NPOINTS};
    const char  dname[]="dataset";
    int         chunk_row, chunk_col;
    size_t      i, j;
    H5D_chunk_index_t idx_type; /* Dataset chunk index type */
    H5F_libver_t low;           /* File format low bound */


    TESTING(testname);

    assert(NPOINTS < 100);

    h5_fixname(FILENAME[6], fapl, filename, sizeof filename);

    if(H5Pget_libver_bounds(fapl, &low, NULL) < 0) TEST_ERROR;

    /* Create file for first test */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Create dataspace */
    if((s = H5Screate_simple(2, dsize, NULL)) < 0) TEST_ERROR;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;

    /* Set chunked layout */
    if(H5Pset_chunk(dcpl, 2, csize) < 0) TEST_ERROR;

    /* Set early allocation time for one dataset; the other dataset is using default alloc time */
    if(early_alloc)
        if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0) TEST_ERROR;

    /* Create dataset */
    if((d = H5Dcreate2(file, dname, H5T_NATIVE_INT, s, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Initialization of check array for repeated coordinates */
    for(i=0; i<dsize[0]/csize[0]; i++)
        for(j=0; j<dsize[1]/csize[1]; j++)
            check2[i][j] = 0;

    /* Generate random point coordinates. Only one point is selected per chunk */
    for(i=0; i<NPOINTS; i++){
        do {
            chunk_row = (int)HDrandom () % (int)(dsize[0]/csize[0]);
            chunk_col = (int)HDrandom () % (int)(dsize[1]/csize[1]);
        } while (check2[chunk_row][chunk_col]);

        wbuf[i] = check2[chunk_row][chunk_col] = chunk_row+chunk_col+1;
        coord[i][0] = (hsize_t)chunk_row * csize[0];
        coord[i][1] = (hsize_t)chunk_col * csize[1];
    }

    /* Create dataspace for write buffer */
    if((m = H5Screate_simple(1, msize, NULL)) < 0) TEST_ERROR;

    /* Select the random points for writing */
    if(H5Sselect_elements(s, H5S_SELECT_SET, (size_t)NPOINTS, (const hsize_t *)coord) < 0) TEST_ERROR;

    /* Write into dataset */
    if(H5Dwrite(d, H5T_NATIVE_INT, m, s, H5P_DEFAULT, wbuf) < 0) TEST_ERROR;

    /* Close resources*/
    if(H5Sclose(s) < 0) TEST_ERROR;
    if(H5Sclose(m) < 0) TEST_ERROR;
    if(H5Pclose(dcpl) < 0) TEST_ERROR;
    if(H5Dclose(d) < 0) TEST_ERROR;
    if(H5Fclose(file) < 0) TEST_ERROR;

    /* Open first file again */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR;

    /* Open dataset */
    if((d = H5Dopen2(file, dname, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Get the chunk index type */
    if(H5D__layout_idx_type_test(d, &idx_type) < 0) TEST_ERROR;

    /* Verify index type */
    if(low == H5F_LIBVER_LATEST) {
        if(early_alloc) {
            if(idx_type != H5D_CHUNK_IDX_NONE)
                FAIL_PUTS_ERROR("should be using Non-Index as index");
        } /* end if */
        else {
            if(idx_type != H5D_CHUNK_IDX_FARRAY)
                FAIL_PUTS_ERROR("should be using Fixed Array as index");
        } /* end else */
    } else if(idx_type != H5D_CHUNK_IDX_BTREE)
    FAIL_PUTS_ERROR("should be using v1 B-tree as index");

    /* Get dataset dataspace */
    if((s = H5Dget_space(d)) < 0) TEST_ERROR;

    /* Create dataspace for read buffer */
    if((m = H5Screate_simple(1, msize, NULL)) < 0) TEST_ERROR;

    /* Select the random points for reading */
    if(H5Sselect_elements (s, H5S_SELECT_SET, (size_t)NPOINTS, (const hsize_t *)coord) < 0) TEST_ERROR;

    /* Read from dataset */
    if(H5Dread(d, H5T_NATIVE_INT, m, s, H5P_DEFAULT, rbuf) < 0) TEST_ERROR;

    /* Verify that written and read data are the same */
    for(i = 0; i < NPOINTS; i++)
        if(rbuf[i] != wbuf[i]){
            HDprintf("    Line %d: Incorrect value, wbuf[%u]=%d, rbuf[%u]=%d\n",__LINE__,(unsigned)i,wbuf[i],(unsigned)i,rbuf[i]);
            HDprintf("             coord[%u] = {%lu, %lu}\n", (unsigned)i, (unsigned long)coord[i][0], (unsigned long)coord[i][1]);
            TEST_ERROR;
        } /* end if */

    /* Close resources */
    if(H5Sclose(s) < 0) TEST_ERROR;
    if(H5Sclose(m) < 0) TEST_ERROR;
    if(H5Dclose(d) < 0) TEST_ERROR;
    if(H5Fclose(file) < 0) TEST_ERROR;


    /* Create second file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Create dataspace with unlimited maximum dimensions */
    if(early_alloc) {
    if((s = H5Screate_simple(2, dsize, fixed_dmax)) < 0) TEST_ERROR;
    } else
    if((s = H5Screate_simple(2, dsize, dmax)) < 0) TEST_ERROR;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;

    /* Set chunked layout */
    if(H5Pset_chunk(dcpl, 2, csize) < 0) TEST_ERROR;

    /* Set early allocation time for one dataset; the other dataset is using default alloc time */
    if(early_alloc)
        if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0) TEST_ERROR;

    /* Create dataset */
    if((d = H5Dcreate2(file, dname, H5T_NATIVE_INT, s, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Get the chunk index type */
    if(H5D__layout_idx_type_test(d, &idx_type) < 0) TEST_ERROR;

    /* Verify index type */
    if(low == H5F_LIBVER_LATEST) {
    if(early_alloc) {
        if(idx_type != H5D_CHUNK_IDX_NONE)
        FAIL_PUTS_ERROR("should be using implicit indexing");
    } else if(idx_type != H5D_CHUNK_IDX_BT2)
        FAIL_PUTS_ERROR("should be using v2 B-tree as index");
    } else if(idx_type != H5D_CHUNK_IDX_BTREE)
    FAIL_PUTS_ERROR("should be using v1 B-tree as index");

    /* Extend both dimensions of the dataset */
    if(H5Dset_extent(d, nsize) < 0) TEST_ERROR;

    /* Reset the dataset dataspace to new dimensions */
    if(H5Sset_extent_simple(s, 2, nsize, dmax) < 0) TEST_ERROR;

    /* Initialize check buffer for repeated coordinates */
    for(i = 0; i < nsize[0]/csize[0]; i++)
        for(j = 0; j < nsize[1] / csize[1]; j++)
            check2[i][j] = 0;

    /* Generate random point coordinates. Only one point is selected per chunk */
    for(i = 0; i < NPOINTS; i++){
        do {
            chunk_row = (int)HDrandom() % (int)(nsize[0] / csize[0]);
            chunk_col = (int)HDrandom() % (int)(nsize[1] / csize[1]);
        } while (check2[chunk_row][chunk_col]);

        wbuf[i] = check2[chunk_row][chunk_col] = chunk_row + chunk_col + 1;
        coord[i][0] = (hsize_t)chunk_row * csize[0];
        coord[i][1] = (hsize_t)chunk_col * csize[1];
    }

    /* Create dataspace for write buffer */
    if((m = H5Screate_simple(1, msize, NULL)) < 0) TEST_ERROR;

    /* Select the random points for writing */
    if(H5Sselect_elements(s, H5S_SELECT_SET, (size_t)NPOINTS, (const hsize_t *)coord) < 0) TEST_ERROR;

    /* Write into dataset */
    if(H5Dwrite(d, H5T_NATIVE_INT, m, s, H5P_DEFAULT, wbuf) < 0) TEST_ERROR;

    /* Close resources */
    if(H5Sclose(s) < 0) TEST_ERROR;
    if(H5Sclose(m) < 0) TEST_ERROR;
    if(H5Pclose(dcpl) < 0) TEST_ERROR;
    if(H5Dclose(d) < 0) TEST_ERROR;
    if(H5Fclose(file) < 0) TEST_ERROR;

    /* Open second file again */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR;

    /* Open dataset */
    if((d = H5Dopen2(file, dname, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Get dataset dataspace */
    if((s = H5Dget_space(d)) < 0) TEST_ERROR;

    /* Create dataspace for read buffer */
    if((m = H5Screate_simple(1, msize, NULL)) < 0) TEST_ERROR;

    /* Select the random points for reading */
    if(H5Sselect_elements (s, H5S_SELECT_SET, (size_t)NPOINTS, (const hsize_t *)coord) < 0) TEST_ERROR;

    /* Read from dataset */
    if(H5Dread(d, H5T_NATIVE_INT, m, s, H5P_DEFAULT, rbuf) < 0) TEST_ERROR;

    /* Verify that written and read data are the same */
    for(i = 0; i < NPOINTS; i++)
        if(rbuf[i] != wbuf[i]){
            HDprintf("    Line %d: Incorrect value, wbuf[%u]=%d, rbuf[%u]=%d\n",__LINE__,(unsigned)i,wbuf[i],(unsigned)i,rbuf[i]);
                TEST_ERROR;
        } /* end if */

    /* Close resources */
    if(H5Sclose(s) < 0) TEST_ERROR;
    if(H5Sclose(m) < 0) TEST_ERROR;
    if(H5Dclose(d) < 0) TEST_ERROR;
    if(H5Fclose(file) < 0) TEST_ERROR;


    /* Create third file */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Create dataspace with fixed maximum dimensions */
    if((s = H5Screate_simple(2, dsize, fixed_dmax)) < 0) TEST_ERROR;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;

    /* Set chunked layout */
    if(H5Pset_chunk(dcpl, 2, csize) < 0) TEST_ERROR;

    /* Set early allocation time for one dataset; the other dataset is using default alloc time */
    if(early_alloc)
        if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0) TEST_ERROR;

    /* Create dataset */
    if((d = H5Dcreate2(file, dname, H5T_NATIVE_INT, s, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Get the chunk index type */
    if(H5D__layout_idx_type_test(d, &idx_type) < 0) TEST_ERROR;

    /* Verify index type */
    if(low == H5F_LIBVER_LATEST) {
        if(early_alloc) {
            if(idx_type != H5D_CHUNK_IDX_NONE)
                FAIL_PUTS_ERROR("should be using Non-Index as index");
        } /* end if */
        else {
            if(idx_type != H5D_CHUNK_IDX_FARRAY)
                FAIL_PUTS_ERROR("should be using Fixed Array as index");
        } /* end else */
    } else if(idx_type != H5D_CHUNK_IDX_BTREE)
    FAIL_PUTS_ERROR("should be using v1 B-tree as index");

    /* Extend both dimensions of the dataset */
    if(H5Dset_extent(d, nsize) < 0) TEST_ERROR;

    /* Reset the dataset dataspace to new dimensions */
    if(H5Sset_extent_simple(s, 2, nsize, dmax) < 0) TEST_ERROR;

    /* Initialize check buffer for repeated coordinates */
    for(i = 0; i < nsize[0]/csize[0]; i++)
        for(j = 0; j < nsize[1] / csize[1]; j++)
            check2[i][j] = 0;

    /* Generate random point coordinates. Only one point is selected per chunk */
    for(i = 0; i < NPOINTS; i++){
        do {
            chunk_row = (int)HDrandom() % (int)(nsize[0] / csize[0]);
            chunk_col = (int)HDrandom() % (int)(nsize[1] / csize[1]);
        } while (check2[chunk_row][chunk_col]);

        wbuf[i] = check2[chunk_row][chunk_col] = chunk_row + chunk_col + 1;
        coord[i][0] = (hsize_t)chunk_row * csize[0];
        coord[i][1] = (hsize_t)chunk_col * csize[1];
    }

    /* Create dataspace for write buffer */
    if((m = H5Screate_simple(1, msize, NULL)) < 0) TEST_ERROR;

    /* Select the random points for writing */
    if(H5Sselect_elements(s, H5S_SELECT_SET, (size_t)NPOINTS, (const hsize_t *)coord) < 0) TEST_ERROR;

    /* Write into dataset */
    if(H5Dwrite(d, H5T_NATIVE_INT, m, s, H5P_DEFAULT, wbuf) < 0) TEST_ERROR;

    /* Close resources */
    if(H5Sclose(s) < 0) TEST_ERROR;
    if(H5Sclose(m) < 0) TEST_ERROR;
    if(H5Pclose(dcpl) < 0) TEST_ERROR;
    if(H5Dclose(d) < 0) TEST_ERROR;
    if(H5Fclose(file) < 0) TEST_ERROR;

    /* Open third file again */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR;

    /* Open dataset */
    if((d = H5Dopen2(file, dname, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Get dataset dataspace */
    if((s = H5Dget_space(d)) < 0) TEST_ERROR;

    /* Create dataspace for read buffer */
    if((m = H5Screate_simple(1, msize, NULL)) < 0) TEST_ERROR;

    /* Select the random points for reading */
    if(H5Sselect_elements (s, H5S_SELECT_SET, (size_t)NPOINTS, (const hsize_t *)coord) < 0) TEST_ERROR;

    /* Read from dataset */
    if(H5Dread(d, H5T_NATIVE_INT, m, s, H5P_DEFAULT, rbuf) < 0) TEST_ERROR;

    /* Verify that written and read data are the same */
    for(i = 0; i < NPOINTS; i++)
        if(rbuf[i] != wbuf[i]){
            HDprintf("    Line %d: Incorrect value, wbuf[%u]=%d, rbuf[%u]=%d\n",__LINE__,(unsigned)i,wbuf[i],(unsigned)i,rbuf[i]);
                TEST_ERROR;
        } /* end if */

    /* Close resources */
    if(H5Sclose(s) < 0) TEST_ERROR;
    if(H5Sclose(m) < 0) TEST_ERROR;
    if(H5Dclose(d) < 0) TEST_ERROR;
    if(H5Fclose(file) < 0) TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(s);
        H5Sclose(m);
        H5Dclose(d);
        H5Fclose(file);
    } H5E_END_TRY;
    return FAIL;
} /* end test_random_chunks_real() */


/*-------------------------------------------------------------------------
 * Function: test_random_chunks
 *
 * Purpose: Tests that write/read on randomly selected chunks
 *          First file:
 *              One dataset has fixed dimensions without max. dims & H5D_ALLOC_TIME_EARLY
 *              One dataset has fixed dimensions without max. dims & default alloc time
 *         Second file:
 *        One extendible dataset with unlimited max. dims & H5D_ALLOC_TIME_EARLY
 *        One extendible dataset with unlimited max. dims & default alloc time
 *        third file:
 *        one extendible dataset with fixed max. dims & H5D_ALLOC_TIME_EARLY
 *        one extendible dataset with fixed max. dims & default alloc time
 *
 *          All the datasets in second & third files are extended before write/read operations
 *
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: Christian Chilan
 *             Monday, March 26, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_random_chunks(hid_t fapl)
{
    int         nerrors = 0;    /* Errors in sub-tests */

    nerrors += test_random_chunks_real("Write/read on randomly selected chunks w/non-implicit index", FALSE, fapl);
    nerrors += test_random_chunks_real("Write/read on randomly selected chunks w/implicit index", TRUE, fapl);

    return nerrors;;
} /* end test_random_chunks() */

#ifndef H5_NO_DEPRECATED_SYMBOLS
/* Empty can_apply and set_local callbacks */
static htri_t
can_apply_deprec(hid_t H5_ATTR_UNUSED dcpl_id, hid_t H5_ATTR_UNUSED type_id, hid_t H5_ATTR_UNUSED space_id)
{
    return 1;
}

static herr_t
set_local_deprec(hid_t H5_ATTR_UNUSED dcpl_id, hid_t H5_ATTR_UNUSED type_id, hid_t H5_ATTR_UNUSED space_id)
{
    return(SUCCEED);
}

/* Old style H5Z_class_t, essentially a copy of the "bogus" filter */
const H5Z_class1_t H5Z_DEPREC[1] = {{
    H5Z_FILTER_DEPREC,        /* Filter id number        */
    "deprec",            /* Filter name for debugging    */
    can_apply_deprec,           /* The "can apply" callback     */
    set_local_deprec,           /* The "set local" callback     */
    filter_bogus,        /* The actual filter function    */
}};


/*-------------------------------------------------------------------------
 * Function: test_deprec
 *
 * Purpose: Tests deprecated API symbols
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: Quincey Koziol
 *             Monday, October 8, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_deprec(hid_t file)
{
    hid_t    dataset, space, small_space, create_parms, dcpl;
    hsize_t    dims[2], small_dims[2];
    hsize_t     deprec_size;
    herr_t    status;
    hsize_t    csize[2];

    TESTING("deprecated API routines");

    /* Create the data space */
    dims[0] = 256;
    dims[1] = 512;
    space = H5Screate_simple(2, dims, NULL);
    assert(space>=0);

    /* Create a small data space for compact dataset */
    small_dims[0] = 16;
    small_dims[1] = 8;
    small_space = H5Screate_simple(2, small_dims, NULL);
    assert(space>=0);

    /*
     * Create a dataset using the default dataset creation properties.    We're
     * not sure what they are, so we won't check.
     */
    if((dataset = H5Dcreate1(file, DSET_DEPREC_NAME, H5T_NATIVE_DOUBLE, space, H5P_DEFAULT)) < 0) goto error;

    /* Close the dataset */
    if(H5Dclose(dataset) < 0) goto error;

    /*
     * Try creating a dataset that already exists.  This should fail since a
     * dataset can only be created once.  Temporarily turn off error
     * reporting.
     */
    H5E_BEGIN_TRY {
    dataset = H5Dcreate1(file, DSET_DEFAULT_NAME, H5T_NATIVE_DOUBLE, space,
                H5P_DEFAULT);
    } H5E_END_TRY;
    if(dataset >= 0) {
    H5_FAILED();
    HDputs("    Library allowed overwrite of existing dataset.");
    goto error;
    }

    /*
     * Open the dataset we created above and then close it.  This is how
     * existing datasets are accessed.
     */
    if((dataset = H5Dopen1(file, DSET_DEPREC_NAME)) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    /*
     * Try opening a non-existent dataset. This should fail since new datasets
     * cannot be created with this function.  Temporarily turn off error
     * reporting.
     */
    H5E_BEGIN_TRY {
    dataset = H5Dopen1(file, "does_not_exist");
    } H5E_END_TRY;
    if(dataset >= 0) {
    H5_FAILED();
    HDputs("    Opened a non-existent dataset.");
    goto error;
    }

    /*
     * Create a new dataset that uses chunked storage instead of the default
     * layout.
     */
    create_parms = H5Pcreate(H5P_DATASET_CREATE);
    assert(create_parms >= 0);

    /* Add the deflate filter, if available */
#if defined H5_HAVE_FILTER_DEFLATE
{
    H5Z_filter_t filtn;                 /* filter identification number */
    size_t cd_nelmts = 1;               /* Number of filter parameters */
    unsigned cd_value;                  /* Filter parameter */

    if(H5Pset_deflate(create_parms, 6) < 0) goto error;

    /* Check for the deflate filter */
    filtn = H5Pget_filter1(create_parms, (unsigned)0, NULL, &cd_nelmts, &cd_value, (size_t)0, NULL);
    if(H5Z_FILTER_DEFLATE != filtn)
        goto error;
    if(1 != cd_nelmts)
        goto error;
    if(6 != cd_value)
        goto error;

    /* Check for the deflate filter */
    if(H5Pget_filter_by_id1(create_parms, H5Z_FILTER_DEFLATE, NULL, &cd_nelmts, &cd_value, (size_t)0, NULL) < 0) goto error;
    if(1 != cd_nelmts)
        goto error;
    if(6 != cd_value)
        goto error;
}
#endif /* H5_HAVE_FILTER_DEFLATE */

    /* Attempt to create a dataset with invalid chunk sizes */
    csize[0] = dims[0]*2;
    csize[1] = dims[1]*2;
    status = H5Pset_chunk(create_parms, 2, csize);
    assert(status >= 0);
    H5E_BEGIN_TRY {
        dataset = H5Dcreate1(file, DSET_DEPREC_NAME_CHUNKED, H5T_NATIVE_DOUBLE, space,
            create_parms);
    } H5E_END_TRY;
    if(dataset >= 0) {
    H5_FAILED();
    HDputs("    Opened a dataset with incorrect chunking parameters.");
    goto error;
    }

    csize[0] = 5;
    csize[1] = 100;
    status = H5Pset_chunk(create_parms, 2, csize);
    assert(status >= 0);

    if((dataset = H5Dcreate1(file, DSET_DEPREC_NAME_CHUNKED, H5T_NATIVE_DOUBLE, space, create_parms)) < 0) goto error;
    H5Pclose(create_parms);

    /*
     * Close the chunked dataset.
     */
    if(H5Dclose(dataset) < 0) goto error;


    /*
     * Open the dataset we created above and then close it.  This is how
     * existing datasets are accessed.
     */
    if((dataset = H5Dopen1(file, DSET_DEPREC_NAME_CHUNKED)) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    /*
     * Create a compact dataset, then close it.
     */
    create_parms = H5Pcreate(H5P_DATASET_CREATE);
    assert(create_parms >= 0);
    status = H5Pset_layout(create_parms, H5D_COMPACT);
    assert(status >= 0);
    status = H5Pset_alloc_time(create_parms, H5D_ALLOC_TIME_EARLY);
    assert(status >= 0);

    if((dataset = H5Dcreate1(file, DSET_DEPREC_NAME_COMPACT, H5T_NATIVE_DOUBLE, small_space, create_parms)) < 0) goto error;
    H5Pclose(create_parms);
    if(H5Dclose(dataset) < 0) goto error;

    /*
     * Open the dataset we created above and then close it.  This is how
     * existing datasets are accessed.
     */
    if((dataset = H5Dopen1(file, DSET_DEPREC_NAME_COMPACT)) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    /* Test H5Zregister with deprecated H5Z_class1_t */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dcpl, 2, csize) < 0) goto error;
    if(H5Zregister(H5Z_DEPREC) < 0) goto error;
    if(H5Pset_filter(dcpl, H5Z_FILTER_DEPREC, 0, (size_t)0, NULL) < 0) goto error;

    HDputs("");
    if(test_filter_internal(file,DSET_DEPREC_NAME_FILTER,dcpl,DISABLE_FLETCHER32,DATA_NOT_CORRUPTED,&deprec_size) < 0) goto error;

    if(H5Pclose(dcpl) < 0) goto error;

    return SUCCEED;

 error:
    return FAIL;
} /* end test_deprec() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */


/*-------------------------------------------------------------------------
 * Function: test_huge_chunks
 *
 * Purpose: Tests that datasets with chunks >4GB can't be created.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Quincey Koziol
 *              Thursday, May  1, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_huge_chunks(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;       /* File ID */
    hid_t       dcpl = -1;      /* Dataset creation property list ID */
    hid_t       sid = -1;       /* Dataspace ID */
    hid_t       dsid = -1;      /* Dataset ID */
    hsize_t     dim, chunk_dim; /* Dataset and chunk dimensions */
    hsize_t     dim2[3], chunk_dim2[3];  /* Dataset and chunk dimensions */
    herr_t      ret;            /* Generic return value */

    TESTING("creating dataset with >4GB chunks");

    h5_fixname(FILENAME[7], fapl, filename, sizeof filename);

    /* Create file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

    /* Try to set too large of a chunk for 1-D dataset (# of elements) */
    chunk_dim = TOO_HUGE_CHUNK_DIM;
    H5E_BEGIN_TRY {
        ret = H5Pset_chunk(dcpl, 1, &chunk_dim);
    } H5E_END_TRY;
    if(ret >= 0)
    FAIL_PUTS_ERROR("    Set chunk size with too large of chunk dimensions.")

    /* Try to set too large of a chunk for n-D dataset (# of elements) */
    chunk_dim2[0] = TOO_HUGE_CHUNK_DIM2_0;
    chunk_dim2[1] = TOO_HUGE_CHUNK_DIM2_1;
    chunk_dim2[2] = TOO_HUGE_CHUNK_DIM2_2;
    H5E_BEGIN_TRY {
        ret = H5Pset_chunk(dcpl, 3, chunk_dim2);
    } H5E_END_TRY;
    if(ret >= 0)
    FAIL_PUTS_ERROR("    Set chunk size with too large of chunk dimensions.")

    /* Set 1-D chunk size */
    chunk_dim = HUGE_CHUNK_DIM;
    if(H5Pset_chunk(dcpl, 1, &chunk_dim) < 0) FAIL_STACK_ERROR

    /* Create 1-D dataspace */
    dim = HUGE_DIM;
    if((sid = H5Screate_simple(1, &dim, NULL)) < 0) FAIL_STACK_ERROR

    /* Try to create dataset */
    H5E_BEGIN_TRY {
        dsid = H5Dcreate2(fid, HUGE_DATASET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dsid >= 0)
    FAIL_PUTS_ERROR("    1-D Dataset with too large of chunk dimensions created.")

    /* Close 1-D dataspace */
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR


    /* Set n-D chunk size */
    chunk_dim2[0] = HUGE_CHUNK_DIM2_0;
    chunk_dim2[1] = HUGE_CHUNK_DIM2_1;
    chunk_dim2[2] = HUGE_CHUNK_DIM2_2;
    if(H5Pset_chunk(dcpl, 3, chunk_dim2) < 0) FAIL_STACK_ERROR

    /* Create n-D dataspace */
    dim2[0] = HUGE_DIM2_0;
    dim2[1] = HUGE_DIM2_1;
    dim2[2] = HUGE_DIM2_2;
    if((sid = H5Screate_simple(3, dim2, NULL)) < 0) FAIL_STACK_ERROR

    /* Try to create dataset */
    H5E_BEGIN_TRY {
        dsid = H5Dcreate2(fid, HUGE_DATASET2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dsid >= 0)
    FAIL_PUTS_ERROR("    n-D Dataset with too large of chunk dimensions created.")

    /* Close n-D dataspace */
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR

    /* Close everything else */
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_huge_chunks() */


/*-------------------------------------------------------------------------
 * Function: test_chunk_cache
 *
 * Purpose: Tests API for setting rdcc info on a DAPL, and interaction
 *          with the corresponding properties in the file structure.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Neil Fortner
 *              Wednesday, October 29, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_chunk_cache(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;       /* File ID */
    hid_t       fapl_local = -1; /* Local fapl */
    hid_t       fapl_def = -1;  /* Default fapl */
    hid_t       dcpl = -1;      /* Dataset creation property list ID */
    hid_t       dapl1 = -1;     /* Dataset access property list ID */
    hid_t       dapl2 = -1;     /* Dataset access property list ID */
    hid_t       sid = -1;       /* Dataspace ID */
    hid_t       dsid = -1;      /* Dataset ID */
    hsize_t     dim, chunk_dim; /* Dataset and chunk dimensions */
    size_t      nslots_1, nslots_2, nslots_3, nslots_4; /* rdcc number of elements */
    size_t      nbytes_1, nbytes_2, nbytes_3, nbytes_4; /* rdcc number of bytes */
    size_t      nlinks;         /* Number of link traversals */
    double      w0_1, w0_2, w0_3, w0_4; /* rdcc preemption policy */

    TESTING("dataset chunk cache configuration");

    /* Create a default fapl and dapl */
    if ((fapl_def = H5Pcreate(H5P_FILE_ACCESS)) < 0) FAIL_STACK_ERROR
    if ((dapl1 = H5Pcreate(H5P_DATASET_ACCESS)) < 0) FAIL_STACK_ERROR

    /* Verify that H5Pget_chunk_cache(dapl) returns the same values as are in
     * the default fapl.
     */
    if (H5Pget_cache(fapl_def, NULL, &nslots_1, &nbytes_1, &w0_1) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl1, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_1 != nslots_4) || (nbytes_1 != nbytes_4) || !H5_DBL_ABS_EQUAL(w0_1, w0_4))
        FAIL_PUTS_ERROR("    Cache values from default dapl do not match those from fapl.")

    /* Set a lapl property on dapl1 (to verify inheritance) */
    if (H5Pset_nlinks(dapl1, (size_t)134) < 0) FAIL_STACK_ERROR
    if (H5Pget_nlinks(dapl1, &nlinks) < 0) FAIL_STACK_ERROR
    if (nlinks != 134)
        FAIL_PUTS_ERROR("    nlinks parameter not set properly on dapl.")

    /* Copy fapl passed to this function (as we will be modifying it) */
    if ((fapl_local = H5Pcopy(fapl)) < 0) FAIL_STACK_ERROR

    /* Set new rdcc settings on fapl */
    nslots_2 = nslots_1 * 2;
    nbytes_2 = nbytes_1 * 2;
    w0_2 = w0_1 / (double)2.0F;
    if (H5Pset_cache(fapl_local, 0, nslots_2, nbytes_2, w0_2) < 0) FAIL_STACK_ERROR

    h5_fixname(FILENAME[8], fapl, filename, sizeof filename);

    /* Create file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_local)) < 0) FAIL_STACK_ERROR

    /* Create dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

    /* Set chunking */
    chunk_dim = 10;
    if (H5Pset_chunk(dcpl, 1, &chunk_dim) < 0) FAIL_STACK_ERROR

    /* Create 1-D dataspace */
    dim = 100;
    if ((sid = H5Screate_simple(1, &dim, NULL)) < 0) FAIL_STACK_ERROR

    /* Create dataset with default dapl */
    if ((dsid = H5Dcreate2(fid, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl1)) < 0)
        FAIL_STACK_ERROR

    /* Retrieve dapl from dataset, verfiy cache values are the same as on fapl_local */
    if ((dapl2 = H5Dget_access_plist(dsid)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_2 != nslots_4) || (nbytes_2 != nbytes_4) || !H5_DBL_ABS_EQUAL(w0_2, w0_4))
        FAIL_PUTS_ERROR("    Cache values from retrieved dapl do not match those from fapl.")
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR

    /* Set new values on dapl1.  nbytes will be set to default, so the file
     * property will override this setting */
    nslots_3 = nslots_2 * 2;
    nbytes_3 = H5D_CHUNK_CACHE_NBYTES_DEFAULT;
    w0_3 = w0_2 / 2;
    if (H5Pset_chunk_cache(dapl1, nslots_3, nbytes_3, w0_3) < 0) FAIL_STACK_ERROR

    /* Close dataset, reopen with dapl1.  Note the use of a dapl with H5Oopen */
    if (H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if ((dsid = H5Oopen(fid, "dset", dapl1)) < 0) FAIL_STACK_ERROR

    /* Retrieve dapl from dataset, verfiy cache values are the same as on dapl1 */
    /* Note we rely on the knowledge that H5Pget_chunk_cache retrieves these
     * values directly from the dataset structure, and not from a copy of the
     * dapl used to open the dataset (which is not preserved).
     */
    if ((dapl2 = H5Dget_access_plist(dsid)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_3 != nslots_4) || (nbytes_2 != nbytes_4) || !H5_DBL_ABS_EQUAL(w0_3, w0_4))
        FAIL_PUTS_ERROR("    Cache values from retrieved dapl do not match those from dapl1.")
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR

    /* Close dataset, reopen with H5P_DEFAULT as dapl */
    if (H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if ((dsid = H5Dopen2(fid, "dset", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Retrieve dapl from dataset, verfiy cache values are the same on fapl_local */
    if ((dapl2 = H5Dget_access_plist(dsid)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_2 != nslots_4) || (nbytes_2 != nbytes_4) || !H5_DBL_ABS_EQUAL(w0_2, w0_4))
        FAIL_PUTS_ERROR("    Cache values from retrieved dapl do not match those from fapl.")
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR

    /* Similary, test use of H5Dcreate2 with H5P_DEFAULT */
    if (H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if ((dsid = H5Dcreate2(fid, "dset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if ((dapl2 = H5Dget_access_plist(dsid)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_2 != nslots_4) || (nbytes_2 != nbytes_4) || !H5_DBL_ABS_EQUAL(w0_2, w0_4))
        FAIL_PUTS_ERROR("    Cache values from retrieved dapl do not match those from fapl.")
    /* Don't close dapl2, we will use it in the next section */

    /* Modify cache values on fapl_local */
    nbytes_3 = nbytes_2 * 2;
    if (H5Pset_cache(fapl_local, 0, nslots_3, nbytes_3, w0_3) < 0) FAIL_STACK_ERROR

    /* Close and reopen file with new fapl_local */
    if (H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if (H5Fclose(fid) < 0) FAIL_STACK_ERROR
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl_local)) < 0) FAIL_STACK_ERROR

    /* Verify that dapl2 retrieved earlier (using values from the old fapl)
     * sets its values in the new file (test use of H5Dopen2 with a dapl)
     */
    if ((dsid = H5Dopen2(fid, "dset", dapl2)) < 0) FAIL_STACK_ERROR
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR /* Close dapl2, to avoid id leak */
    if ((dapl2 = H5Dget_access_plist(dsid)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_2 != nslots_4) || (nbytes_2 != nbytes_4) || !H5_DBL_ABS_EQUAL(w0_2, w0_4))
        FAIL_PUTS_ERROR("    Cache values from retrieved dapl do not match those from dapl2.")

    /* Test H5D_CHUNK_CACHE_NSLOTS_DEFAULT and H5D_CHUNK_CACHE_W0_DEFAULT */
    nslots_2 = H5D_CHUNK_CACHE_NSLOTS_DEFAULT;
    w0_2 = H5D_CHUNK_CACHE_W0_DEFAULT;
    if (H5Pset_chunk_cache(dapl2, nslots_2, nbytes_2, w0_2) < 0) FAIL_STACK_ERROR

    if (H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if ((dsid = H5Dopen2(fid, "dset", dapl2)) < 0) FAIL_STACK_ERROR
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR /* Close dapl2, to avoid id leak */
    if ((dapl2 = H5Dget_access_plist(dsid)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_3 != nslots_4) || (nbytes_2 != nbytes_4) || !H5_DBL_ABS_EQUAL(w0_3, w0_4))
        FAIL_PUTS_ERROR("    Cache values from retrieved dapl do not match those expected.")
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR

    /* Verify that the file has indeed started using the new cache values (test
     * use of H5Oopen with H5P_DEFAULT) */
    if (H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if ((dsid = H5Oopen(fid, "dset", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if ((dapl2 = H5Dget_access_plist(dsid)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_3 != nslots_4) || (nbytes_3 != nbytes_4) || !H5_DBL_ABS_EQUAL(w0_3, w0_4))
        FAIL_PUTS_ERROR("    Cache values from retrieved dapl do not match those from fapl.")
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR

    /* Verify functionality of H5Pcopy with a dapl */
    if ((dapl2 = H5Pcopy(dapl1)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_3 != nslots_4) || (nbytes_1 != nbytes_4) || !H5_DBL_ABS_EQUAL(w0_3, w0_4))
        FAIL_PUTS_ERROR("    Cache values from dapl2 do not match those from dapl1.")

    /* Close */
    if (H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if (H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if (H5Pclose(fapl_local) < 0) FAIL_STACK_ERROR
    if (H5Pclose(fapl_def) < 0) FAIL_STACK_ERROR
    if (H5Pclose(dapl1) < 0) FAIL_STACK_ERROR
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR
    if (H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if (H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_local);
        H5Pclose(fapl_def);
        H5Pclose(dapl1);
        H5Pclose(dapl2);
        H5Pclose(dcpl);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_chunk_cache() */


/*-------------------------------------------------------------------------
 * Function:    test_big_chunks_bypass_cache
 *
 * Purpose:     When the chunk size is bigger than the cache size and the
 *              chunk isn't on disk, this test verifies that the library
 *              bypasses the cache.
 *
 * Note:        This test is not very conclusive - it doesn't actually check
 *              if the chunks bypass the cache... :-(  -QAK
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Raymond Lu
 *              11 Feb 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_big_chunks_bypass_cache(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;           /* File ID */
    hid_t       fapl_local = -1;     /* File access property list ID */
    hid_t       dcpl = -1, t_dcpl = -1;    /* Dataset creation property list ID */
    hid_t       sid = -1, t_sid = -1;     /* Dataspace ID */
    hid_t    mid;             /* Memory space ID */
    hid_t       dsid = -1, t_dsid = -1;    /* Dataset ID */
    hsize_t     dim, chunk_dim;     /* Dataset and chunk dimensions */
    hsize_t     t_dim[2], t_max[2], t_chunk_dim[2]; /* Dataset and chunk dimensions */
    size_t      rdcc_nelmts, rdcc_nbytes;    /* Chunk cache parameters */
    int         fvalue = BYPASS_FILL_VALUE;    /* Fill value */
    hsize_t     count, stride, offset, block;    /* Setting for hyperslab (1-D) */
    hsize_t     t_count[2], t_stride[2], t_offset[2], t_block[2];  /* Setting for hyperslab (2-D) */
        /* Buffers for reading and writing data (1-D) */
    int         *wdata = NULL, *rdata1 = NULL, *rdata2 = NULL;
        /* Buffer for reading and writing data (2-D) */
    static int  t_wdata[BYPASS_CHUNK_DIM/2][BYPASS_CHUNK_DIM/2], t_rdata1[BYPASS_DIM][BYPASS_DIM],
                t_rdata2[BYPASS_CHUNK_DIM/2][BYPASS_CHUNK_DIM/2];
    int         i, j;        /* Local index variables */
    H5F_libver_t low;           /* File format low bound */
    H5D_chunk_index_t idx_type, t_idx_type;      /* Dataset chunk index types */


    TESTING("big chunks bypassing the cache");

    h5_fixname(FILENAME[9], fapl, filename, sizeof filename);

     /* Check if we are using the latest version of the format */
    if(H5Pget_libver_bounds(fapl, &low, NULL) < 0) FAIL_STACK_ERROR

    /* Copy fapl passed to this function (as we will be modifying it) */
    if((fapl_local = H5Pcopy(fapl)) < 0) FAIL_STACK_ERROR

    /* Define cache size to be smaller than chunk size */
    rdcc_nelmts = BYPASS_CHUNK_DIM/5;
    rdcc_nbytes = sizeof(int)*BYPASS_CHUNK_DIM/5;
    if(H5Pset_cache(fapl_local, 0, rdcc_nelmts, rdcc_nbytes, 0.0F) < 0) FAIL_STACK_ERROR

    /* Create file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_local)) < 0) FAIL_STACK_ERROR

    /* Create 1-D & 2-D dataspace */
    dim = t_dim[0] = t_dim[1] = BYPASS_DIM;
    t_max[0] = t_max[1] = H5S_UNLIMITED;
    if((sid = H5Screate_simple(1, &dim, NULL)) < 0) FAIL_STACK_ERROR
    if((t_sid = H5Screate_simple(2, t_dim, t_max)) < 0) FAIL_STACK_ERROR

    /* Create 1-D & 2-D dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR
    if((t_dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

    /* Define chunk size. */
    /* There will be 2 chunks in 1-D dataset & 4 chunks in the 2-D dataset */
    chunk_dim = t_chunk_dim[0] = t_chunk_dim[1] = BYPASS_CHUNK_DIM;
    if(H5Pset_chunk(dcpl, 1, &chunk_dim) < 0) FAIL_STACK_ERROR
    if(H5Pset_chunk(t_dcpl, 2, t_chunk_dim) < 0) FAIL_STACK_ERROR

    /* Define fill value, fill time, and chunk allocation time */
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fvalue) < 0) FAIL_STACK_ERROR
    if(H5Pset_fill_value(t_dcpl, H5T_NATIVE_INT, &fvalue) < 0) FAIL_STACK_ERROR

    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_IFSET) < 0) FAIL_STACK_ERROR
    if(H5Pset_fill_time(t_dcpl, H5D_FILL_TIME_IFSET) < 0) FAIL_STACK_ERROR

    if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_INCR) < 0) FAIL_STACK_ERROR
    if(H5Pset_alloc_time(t_dcpl, H5D_ALLOC_TIME_INCR) < 0) FAIL_STACK_ERROR

    /* Create the first 1-D dataset */
    if((dsid = H5Dcreate2(fid, BYPASS_DATASET1, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Create the first 2-D dataset */
    if((t_dsid = H5Dcreate2(fid, T_BYPASS_DATASET1, H5T_NATIVE_INT, t_sid, H5P_DEFAULT, t_dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Get the chunk index types for 1-D and 2-d datasets */
    if(H5D__layout_idx_type_test(dsid, &idx_type) < 0) FAIL_STACK_ERROR
    if(H5D__layout_idx_type_test(t_dsid, &t_idx_type) < 0) FAIL_STACK_ERROR

    /* Chunk index type expected depends on whether we are using the latest version of the format */
    if(low == H5F_LIBVER_LATEST) {
    /* Verify index type */
    if(idx_type != H5D_CHUNK_IDX_FARRAY) FAIL_PUTS_ERROR("should be using Fixed Array as index");
    if(t_idx_type != H5D_CHUNK_IDX_BT2) FAIL_PUTS_ERROR("should be using v2 B-tree as index");
    } else {
    /* Verify index type */
    if(idx_type != H5D_CHUNK_IDX_BTREE) FAIL_PUTS_ERROR("should be using v1 B-tree as index");
    if(t_idx_type != H5D_CHUNK_IDX_BTREE) FAIL_PUTS_ERROR("should be using v1 B-tree as index");
    } /* end else */

    /* Select first chunk to write the data */
    offset = t_offset[0] = t_offset[1] = 0;
    count = t_count[0] = t_count[1] = 1;
    stride = t_stride[0] = t_stride[1] = 1;
    block = t_block[0] = t_block[1] = BYPASS_CHUNK_DIM / 2;
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, &offset, &stride, &count, &block) < 0)
        FAIL_STACK_ERROR

    if(H5Sselect_hyperslab(t_sid, H5S_SELECT_SET, t_offset, t_stride, t_count, t_block) < 0)
        FAIL_STACK_ERROR

    /* Allocate buffers */
    if(NULL == (wdata = (int *)HDmalloc(sizeof(int) * (BYPASS_CHUNK_DIM / 2))))
        TEST_ERROR
    if(NULL == (rdata1 = (int *)HDmalloc(sizeof(int) * BYPASS_DIM)))
        TEST_ERROR
    if(NULL == (rdata2 = (int *)HDmalloc(sizeof(int) * (BYPASS_CHUNK_DIM / 2))))
        TEST_ERROR

    /* Initialize data to write for 1-D dataset */
    for(i = 0; i < BYPASS_CHUNK_DIM / 2; i++)
        wdata[i] = i;

    /* Initialize data to write for 2-D dataset */
    for(i = 0; i < BYPASS_CHUNK_DIM / 2; i++)
    for(j = 0; j < BYPASS_CHUNK_DIM / 2; j++)
        t_wdata[i][j] = j;

    /* Set up memory space for the 2-D dataset */
    mid = H5Screate_simple(2, t_block, NULL);

    /* Write to the first 1-D & 2-D datasets */
    /* This write should go through the cache because fill value is used. */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, sid, H5P_DEFAULT, wdata) < 0)
        FAIL_STACK_ERROR
    if(H5Dwrite(t_dsid, H5T_NATIVE_INT, mid, t_sid, H5P_DEFAULT, t_wdata) < 0)
        FAIL_STACK_ERROR

    /* Close the first 1-D & 2-D datasets */
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(t_dsid) < 0) FAIL_STACK_ERROR

    /* Reopen the first 1-D & 2-D datasets */
    if((dsid = H5Dopen2(fid, BYPASS_DATASET1, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((t_dsid = H5Dopen2(fid, T_BYPASS_DATASET1, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Reads both 2 chunks.  Reading the second chunk should bypass the cache because the
     * chunk is bigger than the cache size and it isn't allocated on disk. */
    if(H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata1) < 0)
        FAIL_STACK_ERROR
    if(H5Dread(t_dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, t_rdata1) < 0)
        FAIL_STACK_ERROR

    /* Verify data for the first 1-D dataset */
    for(i = 0; i < BYPASS_CHUNK_DIM / 2; i++)
        if(rdata1[i] != i) {
            HDprintf("    Read different values than written in the 1st chunk.\n");
            HDprintf("    At line %d and index %d, rdata1 = %d. It should be %d.\n", __LINE__, i, rdata1[i], i);
            TEST_ERROR
        } /* end if */

    for(j = BYPASS_CHUNK_DIM / 2; j < BYPASS_DIM; j++)
        if(rdata1[j] != fvalue) {
            HDprintf("    Read different values than written in the 2nd chunk.\n");
            HDprintf("    At line %d and index %d, rdata1 = %d. It should be %d.\n", __LINE__, i, rdata1[i], fvalue);
            TEST_ERROR
        } /* end if */

    /* Verify data for the first 2-D dataset */
    for(i = 0; i < BYPASS_CHUNK_DIM / 2; i++)
    for(j = 0; j < BYPASS_CHUNK_DIM / 2; j++)
        if(t_rdata1[i][j] != j) {
        HDprintf("    Read different values than written in the 1st chunk.\n");
        HDprintf("    At line %d and index (%d, %d), t_rdata1 = %d. It should be %d.\n",
            __LINE__, i, j, t_rdata1[i][j], j);
        TEST_ERROR
        } /* end if */

    for(i = BYPASS_CHUNK_DIM / 2; i < BYPASS_DIM; i++)
    for(j = BYPASS_CHUNK_DIM / 2; j < BYPASS_DIM; j++)
        if(t_rdata1[i][j] != fvalue) {
        HDprintf("    Read different values than written in the 2nd chunk.\n");
        HDprintf("    At line %d and index (%d, %d), t_rdata1 = %d. It should be %d.\n",
            __LINE__, i, j, t_rdata1[i][j], fvalue);
            TEST_ERROR
        } /* end if */

    /* Close the first 1-D & 2-D datasets */
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(t_dsid) < 0) FAIL_STACK_ERROR

    /* Create a second dataset without fill value.  This time, both write
     * and read should bypass the cache because the chunk is bigger than the
     * cache size and it's not allocated on disk. */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) FAIL_STACK_ERROR
    if(H5Pset_fill_time(t_dcpl, H5D_FILL_TIME_NEVER) < 0) FAIL_STACK_ERROR

    /* Create a second 1-D & 2-D dataset */
    if((dsid = H5Dcreate2(fid, BYPASS_DATASET2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if((t_dsid = H5Dcreate2(fid, T_BYPASS_DATASET2, H5T_NATIVE_INT, t_sid, H5P_DEFAULT, t_dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Write to the second 1-D & 2-D dataset */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, sid, H5P_DEFAULT, wdata) < 0)
        FAIL_STACK_ERROR
    if(H5Dwrite(t_dsid, H5T_NATIVE_INT, mid, t_sid, H5P_DEFAULT, t_wdata) < 0)
        FAIL_STACK_ERROR

    /* Close the second 1-D & 2-D dataset */
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(t_dsid) < 0) FAIL_STACK_ERROR

    /* Reopen the second 1-d dataset and 2-d dataset */
    if((dsid = H5Dopen2(fid, BYPASS_DATASET2, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((t_dsid = H5Dopen2(fid, T_BYPASS_DATASET2, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Read back only the part that was written to the file.  Reading the
     * half chunk should bypass the cache because the chunk is bigger than
     * the cache size. */
    if(H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, sid, H5P_DEFAULT, rdata2) < 0)
    FAIL_STACK_ERROR
    if(H5Dread(t_dsid, H5T_NATIVE_INT, mid, t_sid, H5P_DEFAULT, t_rdata2) < 0)
    FAIL_STACK_ERROR

    /* Verify data for the second 1-D dataset */
    for(i = 0; i < BYPASS_CHUNK_DIM / 2; i++)
    if(rdata2[i] != i) {
            HDprintf("    Read different values than written in the chunk.\n");
            HDprintf("    At line %d and index %d, rdata2 = %d. It should be %d.\n", __LINE__, i, rdata2[i], i);
            TEST_ERROR
        } /* end if */

    /* Verify data for the second 2-D dataset */
    for(i = 0; i < BYPASS_CHUNK_DIM / 2; i++)
    for(j = 0; j < BYPASS_CHUNK_DIM / 2; j++)
        if(t_rdata2[i][j] != j) {
        HDprintf("    Read different values than written in the chunk.\n");
        HDprintf("    At line %d and index (%d, %d), t_rdata2 = %d. It should be %d.\n",
            __LINE__, i, j, t_rdata2[i][j], j);
        TEST_ERROR
        } /* end if */

    /* Close IDs */
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if(H5Sclose(t_sid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(t_dsid) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Pclose(t_dcpl) < 0) FAIL_STACK_ERROR
    if(H5Pclose(fapl_local) < 0) FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Release buffers */
    HDfree(wdata);
    HDfree(rdata1);
    HDfree(rdata2);

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Pclose(t_dcpl);
        H5Pclose(fapl_local);
        H5Dclose(dsid);
        H5Dclose(t_dsid);
        H5Sclose(sid);
        H5Sclose(t_sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    if(wdata)
        HDfree(wdata);
    if(rdata1)
        HDfree(rdata1);
    if(rdata2)
        HDfree(rdata2);
    return FAIL;
} /* end test_big_chunks_bypass_cache() */


/*-------------------------------------------------------------------------
 * Function: test_chunk_fast
 *
 * Purpose: Tests support for extensible arrays as chunk index.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, February  3, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_chunk_fast(const char *env_h5_driver, hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;       /* File ID */
    hid_t       my_fapl = -1;   /* File access property list ID */
    hid_t       dcpl = -1;      /* Dataset creation property list ID */
    hid_t       sid = -1;       /* Dataspace ID */
    hid_t       scalar_sid = -1;/* Scalar dataspace ID */
    hid_t       dsid = -1;      /* Dataset ID */
    hsize_t     fill;           /* Temporary value, for filling arrays */
    hsize_t     hs_size[EARRAY_MAX_RANK];   /* Hyperslab size */
    hsize_t     chunk_dim[EARRAY_MAX_RANK]; /* Chunk dimensions */
    H5F_libver_t low;           /* File format low bound */
    unsigned    swmr;           /* Whether file should be written with SWMR access enabled */

    TESTING("datasets w/extensible array as chunk index");

    h5_fixname(FILENAME[10], fapl, filename, sizeof filename);

    /* Copy the file access property list */
    if((my_fapl = H5Pcopy(fapl)) < 0) FAIL_STACK_ERROR

    /* Turn on the chunk cache again */
    {
        int mdc_nelmts;             /* # of elements in metadata cache */
        size_t rdcc_nelmts;         /* # of chunks in chunk cache */
        size_t rdcc_nbytes;         /* # of bytes in chunk cache */
        double rdcc_w0;             /* write-ratio for chunk cache */

        if(H5Pget_cache(my_fapl, &mdc_nelmts, &rdcc_nelmts, &rdcc_nbytes, &rdcc_w0) < 0) FAIL_STACK_ERROR
        rdcc_nbytes = 1048576;
        if(H5Pset_cache(my_fapl, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0) < 0) FAIL_STACK_ERROR
    } /* end block */

    /* Check if we are using the latest version of the format */
    if(H5Pget_libver_bounds(my_fapl, &low, NULL) < 0) FAIL_STACK_ERROR

    /* Create scalar dataspace */
    if((scalar_sid = H5Screate(H5S_SCALAR)) < 0) FAIL_STACK_ERROR

    /* Initialize chunk dimensions */
    fill = EARRAY_CHUNK_DIM;
    H5VM_array_fill(chunk_dim, &fill, sizeof(fill), EARRAY_MAX_RANK);

    /* Initialize hyperslab size */
    fill = 1;
    H5VM_array_fill(hs_size, &fill, sizeof(fill), EARRAY_MAX_RANK);

    /* Loop over using SWMR access to write */
    for(swmr = 0; swmr <= 1; swmr++) {
        int     compress;       /* Whether chunks should be compressed */

        /* SWMR is now supported with/without latest format:  */
        /* (1) write+latest-format (2) SWMR-write+non-latest-format */

        /* Skip this iteration if SWMR I/O is not supported for the VFD specified
         * by the environment variable.
         */
        if(swmr && !H5FD_supports_swmr_test(env_h5_driver))
            continue;

#ifdef H5_HAVE_FILTER_DEFLATE
        /* Loop over compressing chunks */
        for(compress = 0; compress <= 1; compress++)
#else
        /* Loop over without compression */
        for(compress = 0; compress <= 0; compress++)
#endif /* H5_HAVE_FILTER_DEFLATE */
        {
            H5D_alloc_time_t alloc_time;        /* Storage allocation time */

            /* Loop over storage allocation time */
            for(alloc_time = H5D_ALLOC_TIME_EARLY; alloc_time <= H5D_ALLOC_TIME_INCR; H5_INC_ENUM(H5D_alloc_time_t, alloc_time)) {
                unsigned ndims;          /* Current # of dims to test */

                /* Loop over dataspace ranks to test */
                for(ndims = 1; ndims < (EARRAY_MAX_RANK + 1); ndims++) {
                    unsigned unlim_dim;

                    /* Create dataset creation property list */
                    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

                    /* Set chunking & chunk dims */
                    if(H5Pset_chunk(dcpl, (int)ndims, chunk_dim) < 0) FAIL_STACK_ERROR

#ifdef H5_HAVE_FILTER_DEFLATE
                    /* Check if we should compress the chunks */
                    if(compress)
                        if(H5Pset_deflate(dcpl, 9) < 0) FAIL_STACK_ERROR
#endif /* H5_HAVE_FILTER_DEFLATE */

                    /* Set fill time */
                    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) FAIL_STACK_ERROR

                    /* Set allocation time */
                    if(H5Pset_alloc_time(dcpl, alloc_time) < 0) FAIL_STACK_ERROR

                    /* Loop over which dimension is unlimited */
                    for(unlim_dim = 0; unlim_dim < ndims; unlim_dim++) {
                        H5D_chunk_index_t idx_type; /* Dataset chunk index type */
                        hsize_t dim[EARRAY_MAX_RANK], max_dim[EARRAY_MAX_RANK]; /* Dataset dimensions */
                        hsize_t swizzled_dim[EARRAY_MAX_RANK];      /* Dimensions, with unlimited dimension moved to rank 0 */
                        hsize_t down[EARRAY_MAX_RANK];      /* 'down' sizes, for computing array index */
                        hsize_t hs_offset[EARRAY_MAX_RANK];      /* Hyperslab offset */
                        hssize_t snpoints;      /* # of points in dataspace extent (signed) */
                        hsize_t npoints;        /* # of points in dataspace extent */
                        unsigned write_elem, read_elem;  /* Element written/read */
                        hsize_t u;             /* Local index variable */

                        /* Create file */
                        if((fid = H5Fcreate(filename, H5F_ACC_TRUNC | (swmr ? H5F_ACC_SWMR_WRITE : 0), H5P_DEFAULT, my_fapl)) < 0) FAIL_STACK_ERROR

                        /* Create n-D dataspace */
                        fill = EARRAY_DSET_DIM;
                        H5VM_array_fill(dim, &fill, sizeof(fill), EARRAY_MAX_RANK);
                        fill = EARRAY_DSET_DIM;
                        H5VM_array_fill(max_dim, &fill, sizeof(fill), EARRAY_MAX_RANK);
                        max_dim[unlim_dim] = H5S_UNLIMITED;
                        fill = EARRAY_DSET_DIM;
                        H5VM_array_fill(swizzled_dim, &fill, sizeof(fill), EARRAY_MAX_RANK);
                        if((sid = H5Screate_simple((int)ndims, dim, max_dim)) < 0) FAIL_STACK_ERROR

                        /* Get the number of points in the dataspace */
                        if((snpoints = H5Sget_simple_extent_npoints(sid)) < 0) FAIL_STACK_ERROR
                        npoints = (hsize_t)snpoints;

                        /* Compute the "down" dimension values */
                        if(H5VM_array_down(ndims, dim, down) < 0) FAIL_STACK_ERROR

                        /* Create chunked dataset */
                        if((dsid = H5Dcreate2(fid, "dset", H5T_NATIVE_UINT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
                            FAIL_STACK_ERROR

                        /* Get the chunk index type */
                        if(H5D__layout_idx_type_test(dsid, &idx_type) < 0) FAIL_STACK_ERROR

                        /* Chunk index type expected depends on whether we are using the latest version of the format */
                        if(low == H5F_LIBVER_LATEST || swmr) {
                            /* Verify index type */
                            if(idx_type != H5D_CHUNK_IDX_EARRAY) FAIL_PUTS_ERROR("should be using extensible array as index");
                        } /* end if */
                        else {
                            /* Verify index type */
                            if(idx_type != H5D_CHUNK_IDX_BTREE) FAIL_PUTS_ERROR("should be using v1 B-tree as index");
                        } /* end else */

                        /* Fill existing elements */
                        for(u = 0; u < npoints; u++) {
                            /* Compute the coordinate from the linear offset */
                            if(H5VM_array_calc_pre(u, ndims, down, hs_offset) < 0) FAIL_STACK_ERROR

                            /* Un-swizzle hyperslab offset in same way as swizzled dimensions */
                            H5VM_unswizzle_coords(hsize_t, hs_offset, unlim_dim);

                            /* Select a single element in the dataset */
                            if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, hs_offset, NULL, hs_size, NULL) < 0) FAIL_STACK_ERROR

                            /* Read (unwritten) element from dataset */
                            read_elem = 1;
                            if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR

                            /* Verify unwritten element is fill value (0) */
                            if(read_elem != 0) FAIL_PUTS_ERROR("invalid unwritten element read");

                            /* Write element to dataset */
                            write_elem = (unsigned)u;
                            if(H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem) < 0) FAIL_STACK_ERROR

                            /* Read element from dataset */
                            read_elem = write_elem + 1;
                            if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR

                            /* Verify written element is read in */
                            if(read_elem != write_elem) FAIL_PUTS_ERROR("invalid written element read");
                        } /* end for */

                        /* Incrementally extend dataset and verify write/reads */
                        while(dim[unlim_dim] < EARRAY_MAX_EXTEND) {
                            hssize_t snew_npoints;      /* # of points in dataspace extent (signed) */
                            hsize_t new_npoints;        /* # of points in dataspace extent */

                            /* Extend dataset */
                            dim[unlim_dim] += EARRAY_EXTEND_INCR;
                            swizzled_dim[0] += EARRAY_EXTEND_INCR;
                            if(H5Dset_extent(dsid, dim) < 0) FAIL_STACK_ERROR

                            /* Close old dataspace */
                            if(H5Sclose(sid) < 0) FAIL_STACK_ERROR

                            /* Get dataspace for dataset now */
                            if((sid = H5Dget_space(dsid)) < 0) FAIL_STACK_ERROR

                            /* Get the new number of points in the dataspace */
                            if((snew_npoints = H5Sget_simple_extent_npoints(sid)) < 0) FAIL_STACK_ERROR
                            new_npoints = (hsize_t)snew_npoints;

                            /* Fill new elements */
                            for(u = npoints; u < new_npoints; u++) {
                                /* Compute the coordinate from the linear offset */
                                if(H5VM_array_calc(u, ndims, swizzled_dim, hs_offset) < 0) FAIL_STACK_ERROR

                                /* Un-swizzle hyperslab offset in same way as swizzled dimensions */
                                H5VM_unswizzle_coords(hsize_t, hs_offset, unlim_dim);

                                /* Select a single element in the dataset */
                                if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, hs_offset, NULL, hs_size, NULL) < 0) FAIL_STACK_ERROR

                                /* Read (unwritten) element from dataset */
                                read_elem = 1;
                                if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR

                                /* Verify unwritten element is fill value (0) */
                                if(read_elem != 0) FAIL_PUTS_ERROR("invalid unwritten element read");

                                /* Write element to dataset */
                                write_elem = (unsigned)u;
                                if(H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem) < 0) FAIL_STACK_ERROR

                                /* Read element from dataset */
                                read_elem = write_elem + 1;
                                if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR

                                /* Verify written element is read in */
                                if(read_elem != write_elem) FAIL_PUTS_ERROR("invalid written element read");
                            } /* end for */

                            /* Update the number of points in the dataspace */
                            npoints = new_npoints;
                        } /* end while */

                        /* Close everything */
                        if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
                        if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
                        if(H5Fclose(fid) < 0) FAIL_STACK_ERROR


                        /* Re-open file & dataset */
                        if((fid = H5Fopen(filename, H5F_ACC_RDONLY | (swmr ? H5F_ACC_SWMR_READ : 0), my_fapl)) < 0) FAIL_STACK_ERROR

                        /* Open dataset */
                        if((dsid = H5Dopen2(fid, "dset", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

                        /* Get the chunk index type */
                        if(H5D__layout_idx_type_test(dsid, &idx_type) < 0) FAIL_STACK_ERROR

                        /* Chunk index tyepe expected depends on whether we are using the latest version of the format */
                        if(low == H5F_LIBVER_LATEST || swmr) {
                            /* Verify index type */
                            if(idx_type != H5D_CHUNK_IDX_EARRAY) FAIL_PUTS_ERROR("should be using extensible array as index");
                        } /* end if */
                        else {
                            /* Verify index type */
                            if(idx_type != H5D_CHUNK_IDX_BTREE) FAIL_PUTS_ERROR("should be using v1 B-tree as index");
                        } /* end else */

                        /* Get dataspace for dataset now */
                        if((sid = H5Dget_space(dsid)) < 0) FAIL_STACK_ERROR

                        /* Get the number of points in the dataspace */
                        if((snpoints = H5Sget_simple_extent_npoints(sid)) < 0) FAIL_STACK_ERROR
                        npoints = (hsize_t)snpoints;

                        /* Get the current dimensions into swizzled_dim array */
                        if(H5Sget_simple_extent_dims(sid, swizzled_dim, NULL) < 0) FAIL_STACK_ERROR

                        /* Generate the swizzled dimensions */
                        H5VM_swizzle_coords(hsize_t, swizzled_dim, unlim_dim);

                        /* Compute the "down" dimension values */
                        if(H5VM_array_down(ndims, swizzled_dim, down) < 0) FAIL_STACK_ERROR

                        /* Read elements */
                        for(u = 0; u < npoints; u++) {
                            /* Compute the coordinate from the linear offset */
                            if(H5VM_array_calc_pre(u, ndims, down, hs_offset) < 0) FAIL_STACK_ERROR

                            /* Unswizzle hyperslab offset in same way as swizzled dimensions */
                            H5VM_unswizzle_coords(hsize_t, hs_offset, unlim_dim);

                            /* Select a single element in the dataset */
                            if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, hs_offset, NULL, hs_size, NULL) < 0) FAIL_STACK_ERROR

                            /* Read written element from dataset */
                            read_elem = (unsigned)(u + 1);
                            if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR

                            /* Verify written element is correct */
                            if(read_elem != u) FAIL_PUTS_ERROR("invalid element read");
                        } /* end for */

                        /* Close everything */
                        if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
                        if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
                        if(H5Fclose(fid) < 0) FAIL_STACK_ERROR


                        /* Re-open file */
                        if((fid = H5Fopen(filename, H5F_ACC_RDWR, my_fapl)) < 0) FAIL_STACK_ERROR

                        /* Delete dataset */
                        if(H5Ldelete(fid, "dset", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

                        /* Close everything */
                        if(H5Fclose(fid) < 0) FAIL_STACK_ERROR
                    } /* end for */

                    /* Close everything */
                    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
                } /* end for */
            } /* end for */
        } /* end for */
    } /* end for */

    /* Close everything */
    if(H5Sclose(scalar_sid) < 0) FAIL_STACK_ERROR
    if(H5Pclose(my_fapl) < 0) FAIL_STACK_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Sclose(scalar_sid);
        H5Fclose(fid);
        H5Pclose(my_fapl);
    } H5E_END_TRY;
    return FAIL;
} /* end test_chunk_fast() */


/*-------------------------------------------------------------------------
 * Function: test_reopen_chunk_fast
 *
 * Purpose:  To verify a bug in extensible arrays as chunk index.
 *        When the dataset is closed in H5D_close(), the pointer
 *        to the extensible array struct in the layout message
 *        is copied via H5D_flush_real() before H5D_chunk_dest().
 *        This causes an abort from "Assertion `ea->hdr' failed."
 *        later when the dataset is re-opened and read.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Vailin Choi
 *              April 13, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_reopen_chunk_fast(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;       /* File ID */
    hid_t       dcpl = -1;      /* Dataset creation property list ID */
    hid_t       sid = -1;       /* Dataspace ID */
    hid_t       scalar_sid = -1;/* Scalar dataspace ID */
    hid_t       dsid = -1;      /* Dataset ID */
    hsize_t     dim, max_dim, chunk_dim; /* Dataset and chunk dimensions */
    hsize_t    hs_offset;      /* Hyperslab offset */
    hsize_t    hs_size;        /* Hyperslab size */
    H5D_alloc_time_t alloc_time;        /* Storage allocation time */
    unsigned    write_elem, read_elem;  /* Element written/read */
    unsigned    u;              /* Local index variable */

    TESTING("datasets w/extensible array open/reopen with read/write");

    h5_fixname(FILENAME[10], fapl, filename, sizeof filename);

    /* Loop over storage allocation time */
    for(alloc_time = H5D_ALLOC_TIME_EARLY; alloc_time <= H5D_ALLOC_TIME_INCR; H5_INC_ENUM(H5D_alloc_time_t, alloc_time)) {
    /* Create file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

    /* Set chunking */
    chunk_dim = 10;
    if(H5Pset_chunk(dcpl, 1, &chunk_dim) < 0) FAIL_STACK_ERROR

    /* Set fill time */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) FAIL_STACK_ERROR

    /* Set allocation time */
    if(H5Pset_alloc_time(dcpl, alloc_time) < 0) FAIL_STACK_ERROR

    /* Create scalar dataspace */
    if((scalar_sid = H5Screate(H5S_SCALAR)) < 0) FAIL_STACK_ERROR

    /* Create 1-D dataspace */
    dim = 100;
    max_dim = H5S_UNLIMITED;
    if((sid = H5Screate_simple(1, &dim, &max_dim)) < 0) FAIL_STACK_ERROR

    /* Create chunked dataset */
    if((dsid = H5Dcreate2(fid, "dset", H5T_NATIVE_UINT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Fill existing elements */
    hs_size = 1;
    for(u = 0; u < 100; u++) {
        /* Select a single element in the dataset */
        hs_offset = u;
        if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, &hs_offset, NULL, &hs_size, NULL) < 0)
        FAIL_STACK_ERROR
        /* Write element to dataset */
        write_elem = u;
        if(H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem) < 0)
        FAIL_STACK_ERROR
    } /* end for */

    /* Close everything */
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR

    /* Reopen the dataset */
    if((dsid = H5Dopen2(fid, "dset", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
        hs_size = 1;

    /* Read from dataset */
    for(u = 0; u < 100; u++) {
        /* Select a single element in the dataset */
        hs_offset = u;
        if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, &hs_offset, NULL, &hs_size, NULL) < 0)
        FAIL_STACK_ERROR

        /* Read element from dataset */
        if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0)
        FAIL_STACK_ERROR
    } /* end for */

    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if(H5Sclose(scalar_sid) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    } /* end for */

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Sclose(scalar_sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_reopen_chunk_fast() */


/*-------------------------------------------------------------------------
 * Function: test_chunk_fast_bug1
 *
 * Purpose:     Test extensible arrays where the first dimension in the
 *              chunk size is the same as the second dimension in the
 *              dataset size.  This helps to confirm that all dimensions
 *              are being "swizzled" correctly in the earray chunk index
 *              code.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Neil Fortner
 *              March 22, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_chunk_fast_bug1(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;       /* File ID */
    hid_t       dcpl = -1;      /* Dataset creation property list ID */
    hid_t       sid = -1;       /* Dataspace ID */
    hid_t       dsid = -1;      /* Dataset ID */
    hsize_t     dim[2], max_dim[2], chunk_dim[2]; /* Dataset and chunk dimensions */
    H5D_alloc_time_t alloc_time;        /* Storage allocation time */
    static unsigned wbuf[40][20], rbuf[40][20];  /* Element written/read */
    unsigned    i, j;            /* Local index variables */

    TESTING("datasets w/extensible array chunk indexing bug");

    h5_fixname(FILENAME[10], fapl, filename, sizeof filename);

    /* Initialize write buffer */
    for(i=0; i<40; i++)
        for(j=0; j<20; j++)
            wbuf[i][j] = (i * 20) + j;

    /* Create 2-D dataspace */
    dim[0] = 40;
    dim[1] = 20;
    max_dim[0] = 40;
    max_dim[1] = H5S_UNLIMITED;
    if((sid = H5Screate_simple(2, dim, max_dim)) < 0) FAIL_STACK_ERROR

    /* Loop over storage allocation time */
    for(alloc_time = H5D_ALLOC_TIME_EARLY; alloc_time <= H5D_ALLOC_TIME_INCR; H5_INC_ENUM(H5D_alloc_time_t, alloc_time)) {
        /* Create file */
        if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

        /* Create dataset creation property list */
        if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

        /* Set chunking */
        chunk_dim[0] = 20;
        chunk_dim[1] = 10;
        if(H5Pset_chunk(dcpl, 2, chunk_dim) < 0) FAIL_STACK_ERROR

        /* Set allocation time */
        if(H5Pset_alloc_time(dcpl, alloc_time) < 0) FAIL_STACK_ERROR

        /* Create chunked dataset */
        if((dsid = H5Dcreate2(fid, "dset", H5T_NATIVE_UINT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR

        /* Write buffer to dataset */
        if(H5Dwrite(dsid, H5T_NATIVE_UINT, sid, sid, H5P_DEFAULT, &wbuf) < 0)
            FAIL_STACK_ERROR

        /* Close everything */
        if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR

        /* Reopen the dataset */
        if((dsid = H5Dopen2(fid, "dset", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

        /* Read from dataset */
        if(H5Dread(dsid, H5T_NATIVE_UINT, sid, sid, H5P_DEFAULT, &rbuf) < 0)
            FAIL_STACK_ERROR

        /* Verify read data */
        for(i=0; i<40; i++)
            for(j=0; j<20; j++)
                if(wbuf[i][j] != rbuf[i][j])
                    FAIL_PUTS_ERROR("invalid element read");

        if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
        if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
        if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    } /* end for */

    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_chunk_fast_bug1() */

/* This message derives from H5Z */
const H5Z_class2_t H5Z_EXPAND[1] = {{
    H5Z_CLASS_T_VERS,           /* H5Z_class_t version */
    H5Z_FILTER_EXPAND,        /* Filter id number        */
    1, 1,                       /* Encoding and decoding enabled */
    "expand",            /* Filter name for debugging    */
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_expand,        /* The actual filter function    */
}};

/* Global "expansion factor" for filter_expand() routine */
static size_t filter_expand_factor_g = 0;


/*-------------------------------------------------------------------------
 * Function:    filter_expand
 *
 * Purpose:     For testing library's behavior when a filter expands a chunk
 *              too much.
 *
 * Note:    This filter doesn't actually re-allocate the buffer to be
 *        larger, it just changes the buffer size to a value that's too
 *        large.  The library should throw an error before using the
 *        incorrect buffer information.
 *
 * Return:    Success:    Data chunk size
 *        Failure:    0
 *
 * Programmer:    Quincey Koziol
 *              Mar 31, 2009
 *
 *-------------------------------------------------------------------------
 */
static size_t
filter_expand(unsigned int flags, size_t H5_ATTR_UNUSED cd_nelmts,
      const unsigned int H5_ATTR_UNUSED *cd_values, size_t nbytes,
      size_t *buf_size, void H5_ATTR_UNUSED **buf)
{
    size_t         ret_value = 0;

    if(flags & H5Z_FLAG_REVERSE) {
        /* Don't do anything when filter is applied in reverse */
        *buf_size = nbytes;
        ret_value = nbytes;
    } /* end if */
    else {
        /* Check for expanding the chunk */
        if(filter_expand_factor_g > 0) {
            /* Expand the buffer size beyond what can be encoded */
            *buf_size = nbytes * 256 * 256 * 256 * filter_expand_factor_g;
            ret_value = *buf_size;
        } /* end if */
        else {
            /* Don't expand the chunk's size */
            *buf_size = nbytes;
            ret_value = nbytes;
        } /* end else */
    } /* end else */

    return ret_value;
} /* end filter_expand() */


/*-------------------------------------------------------------------------
 * Function: test_chunk_expand
 *
 * Purpose: Tests support for proper error handling when a chunk expands
 *              too much after a filter is applied
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March 31, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_chunk_expand(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;               /* File ID */
    hid_t       dcpl = -1, dcpl2 = -1;          /* Dataset creation property list ID */
    hid_t       sid = -1, sid2 = -1;           /* Dataspace ID */
    hid_t       scalar_sid = -1;        /* Scalar dataspace ID */
    hid_t       dsid = -1, dsid2 = -1;          /* Dataset ID */
    hsize_t     dim, max_dim, chunk_dim;     /* Dataset and chunk dimensions */
    hsize_t     dim2[2], max_dim2[2], chunk_dim2[2];     /* Dataset and chunk dimensions */
    H5D_chunk_index_t idx_type, idx_type2;     /* Dataset chunk index type */
    H5F_libver_t low, high;             /* File format bounds */
    hsize_t    hs_offset, hs_offset2[2];          /* Hyperslab offset */
    hsize_t    hs_size, hs_size2[2];            /* Hyperslab size */
    H5D_alloc_time_t alloc_time;            /* Storage allocation time */
    unsigned    write_elem, read_elem;      /* Element written/read */
    unsigned    write_elem2, read_elem2;      /* Element written/read */
    unsigned    u;                      /* Local index variable */
    herr_t      status;                 /* Generic return value */

    TESTING("filter expanding chunks too much");

    h5_fixname(FILENAME[11], fapl, filename, sizeof filename);

    /* Check if we are using the latest version of the format */
    if(H5Pget_libver_bounds(fapl, &low, &high) < 0) FAIL_STACK_ERROR

    if(sizeof(size_t) <= 4 && low != H5F_LIBVER_LATEST) {
    SKIPPED();
    HDputs("    Current machine can't test for error w/old file format");
    } /* end if */
    else {
        /* Register "expansion" filter */
        if(H5Zregister(H5Z_EXPAND) < 0) FAIL_STACK_ERROR

        /* Check that the filter was registered */
        if(TRUE != H5Zfilter_avail(H5Z_FILTER_EXPAND)) FAIL_STACK_ERROR

        /* Loop over storage allocation time */
        for(alloc_time = H5D_ALLOC_TIME_EARLY; alloc_time <= H5D_ALLOC_TIME_INCR; H5_INC_ENUM(H5D_alloc_time_t, alloc_time)) {

            /* Create file */
            if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

            /* Create dataset creation property list */
            if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR
            if((dcpl2 = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

            /* Set chunking */
            chunk_dim = chunk_dim2[0] = chunk_dim2[1] = 10;
            if(H5Pset_chunk(dcpl, 1, &chunk_dim) < 0) FAIL_STACK_ERROR
            if(H5Pset_chunk(dcpl2, 2, chunk_dim2) < 0) FAIL_STACK_ERROR

            /* Set fill time */
            if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) FAIL_STACK_ERROR
            if(H5Pset_fill_time(dcpl2, H5D_FILL_TIME_ALLOC) < 0) FAIL_STACK_ERROR

            /* Set allocation time */
            if(H5Pset_alloc_time(dcpl, alloc_time) < 0) FAIL_STACK_ERROR
            if(H5Pset_alloc_time(dcpl2, alloc_time) < 0) FAIL_STACK_ERROR

            /* Set "expand" filter */
            if(H5Pset_filter(dcpl, H5Z_FILTER_EXPAND, 0, (size_t)0, NULL) < 0) FAIL_STACK_ERROR
            if(H5Pset_filter(dcpl2, H5Z_FILTER_EXPAND, 0, (size_t)0, NULL) < 0) FAIL_STACK_ERROR

            /* Create scalar dataspace */
            if((scalar_sid = H5Screate(H5S_SCALAR)) < 0) FAIL_STACK_ERROR

            /* Create 1-D and 2-D dataspace */
            dim = dim2[0] = dim2[1] = 100;
            max_dim = max_dim2[0] = max_dim2[1]  = H5S_UNLIMITED;
            if((sid = H5Screate_simple(1, &dim, &max_dim)) < 0) FAIL_STACK_ERROR
            if((sid2 = H5Screate_simple(2, dim2, max_dim2)) < 0) FAIL_STACK_ERROR

            /* Create 1-D & 2-D chunked datasets */
            if(H5D_ALLOC_TIME_EARLY == alloc_time) {
                /* Make the expansion factor large enough to cause failure right away */
                filter_expand_factor_g = 8;

                H5E_BEGIN_TRY {
                    dsid = H5Dcreate2(fid, "dset", H5T_NATIVE_UINT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                } H5E_END_TRY;
                if(dsid >= 0) FAIL_PUTS_ERROR("should fail to create dataset when allocation time is early");

                H5E_BEGIN_TRY {
                    dsid2 = H5Dcreate2(fid, "dset2", H5T_NATIVE_UINT, sid2, H5P_DEFAULT, dcpl2, H5P_DEFAULT);
                } H5E_END_TRY;
                if(dsid2 >= 0) FAIL_PUTS_ERROR("should fail to create dataset when allocation time is early");

            } /* end if */
            else {
                if((dsid = H5Dcreate2(fid, "dset", H5T_NATIVE_UINT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
                    FAIL_STACK_ERROR

                if((dsid2 = H5Dcreate2(fid, "dset2", H5T_NATIVE_UINT, sid2, H5P_DEFAULT, dcpl2, H5P_DEFAULT)) < 0)
                    FAIL_STACK_ERROR

                /* Get the chunk index type */
                if(H5D__layout_idx_type_test(dsid, &idx_type) < 0) FAIL_STACK_ERROR
                if(H5D__layout_idx_type_test(dsid2, &idx_type2) < 0) FAIL_STACK_ERROR

                /* Chunk index type expected depends on whether we are using the latest version of the format */
                if(low == H5F_LIBVER_LATEST) {
                    /* Verify index type */
                    if(idx_type != H5D_CHUNK_IDX_EARRAY) FAIL_PUTS_ERROR("should be using extensible array as index");
                    if(idx_type2 != H5D_CHUNK_IDX_BT2) FAIL_PUTS_ERROR("should be using v2 B-tree as index");
                } /* end if */
                else {
                    /* Verify index type */
                    if(idx_type != H5D_CHUNK_IDX_BTREE) FAIL_PUTS_ERROR("should be using v1 B-tree as index");
                    if(idx_type2 != H5D_CHUNK_IDX_BTREE) FAIL_PUTS_ERROR("should be using v1 B-tree as index");
                } /* end else */

                /* Fill elements */
                hs_size = hs_size2[0] = hs_size2[1] = 1;
                for(u = 0; u < 100; u++) {

                    hs_offset = hs_offset2[0] = hs_offset2[1] = u;

                    /* Select a single element in the 1-D dataset */
                    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, &hs_offset, NULL, &hs_size, NULL) < 0) FAIL_STACK_ERROR

                    /* Select a single element in the 2-D dataset; NOT every element is selected */
                    if(H5Sselect_hyperslab(sid2, H5S_SELECT_SET, hs_offset2, NULL, hs_size2, NULL) < 0) FAIL_STACK_ERROR

                    /* Read (unwritten) element from dataset */
                    read_elem = read_elem2 = 1;
                    if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR
                    if(H5Dread(dsid2, H5T_NATIVE_UINT, scalar_sid, sid2, H5P_DEFAULT, &read_elem2) < 0) FAIL_STACK_ERROR

                    /* Verify unwritten element is fill value (0) */
                    if(read_elem != 0) FAIL_PUTS_ERROR("invalid unwritten element read");
                    if(read_elem2 != 0) FAIL_PUTS_ERROR("invalid unwritten element read");

                    /* Don't expand chunks yet */
                    filter_expand_factor_g = 0;

                    /* Write element to the datasets */
                    write_elem = write_elem2 = u;
                    if(H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem) < 0) FAIL_STACK_ERROR
                    if(H5Dwrite(dsid2, H5T_NATIVE_UINT, scalar_sid, sid2, H5P_DEFAULT, &write_elem2) < 0) FAIL_STACK_ERROR

                    /* Read element from the datasets */
                    read_elem = write_elem + 1;
                    read_elem2 = write_elem2 + 1;
                    if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR
                    if(H5Dread(dsid2, H5T_NATIVE_UINT, scalar_sid, sid2, H5P_DEFAULT, &read_elem2) < 0) FAIL_STACK_ERROR

                    /* Verify written element is read in */
                    if(read_elem != write_elem) FAIL_PUTS_ERROR("invalid written element read");
                    if(read_elem2 != write_elem2) FAIL_PUTS_ERROR("invalid written element read");

                    /* Expand chunks now */
                    filter_expand_factor_g = 8;

                    /* Write element to the datasets */
                    write_elem = write_elem2 = u;
                    H5E_BEGIN_TRY {
                        status = H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem);
                    } H5E_END_TRY;
                    if(status >= 0) FAIL_PUTS_ERROR("should fail to write to dataset when allocation time is not early");

                    H5E_BEGIN_TRY {
                        status = H5Dwrite(dsid2, H5T_NATIVE_UINT, scalar_sid, sid2, H5P_DEFAULT, &write_elem2);
                    } H5E_END_TRY;
                    if(status >= 0) FAIL_PUTS_ERROR("should fail to write to dataset when allocation time is not early");
                } /* end for */

                /* Incrementally extend dataset and verify write/reads */
                while(dim < 1000) {
                    /* Extend the datasets */
                    dim += 100;
                    dim2[0] += 100;
                    dim2[1] += 100;
                    if(H5Dset_extent(dsid, &dim) < 0) FAIL_STACK_ERROR
                    if(H5Dset_extent(dsid2, dim2) < 0) FAIL_STACK_ERROR

                    /* Close old dataspace */
                    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
                    if(H5Sclose(sid2) < 0) FAIL_STACK_ERROR

                    /* Get dataspace for the datasets now */
                    if((sid = H5Dget_space(dsid)) < 0) FAIL_STACK_ERROR
                    if((sid2 = H5Dget_space(dsid2)) < 0) FAIL_STACK_ERROR

                    /* Fill new elements */
                    hs_size = hs_size2[0] = hs_size2[1] = 1;
                    for(u = 0; u < 100; u++) {
                        /* Select a single element in the datasets */
                        hs_offset = (dim + u) - 100;
                        hs_offset2[0] = (dim2[0] + u) - 100;
                        hs_offset2[1] = (dim2[1] + u) - 100;
                        if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, &hs_offset, NULL, &hs_size, NULL) < 0) FAIL_STACK_ERROR
                        if(H5Sselect_hyperslab(sid2, H5S_SELECT_SET, hs_offset2, NULL, hs_size2, NULL) < 0) FAIL_STACK_ERROR

                        /* Read (unwritten) element from the datasets */
                        read_elem = read_elem2 = 1;
                        if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR
                        if(H5Dread(dsid2, H5T_NATIVE_UINT, scalar_sid, sid2, H5P_DEFAULT, &read_elem2) < 0) FAIL_STACK_ERROR

                        /* Verify unwritten element is fill value (0) */
                        if(read_elem != 0) FAIL_PUTS_ERROR("invalid unwritten element read");
                        if(read_elem2 != 0) FAIL_PUTS_ERROR("invalid unwritten element read");

                        /* Don't expand chunks yet */
                        filter_expand_factor_g = 0;

                        /* Write element to the datasets */
                        write_elem = write_elem2 = u;
                        if(H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem) < 0) FAIL_STACK_ERROR
                        if(H5Dwrite(dsid2, H5T_NATIVE_UINT, scalar_sid, sid2, H5P_DEFAULT, &write_elem2) < 0) FAIL_STACK_ERROR

                        /* Read element from the datasets */
                        read_elem = write_elem + 1;
                        read_elem2 = write_elem2 + 1;
                        if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR
                        if(H5Dread(dsid2, H5T_NATIVE_UINT, scalar_sid, sid2, H5P_DEFAULT, &read_elem2) < 0) FAIL_STACK_ERROR

                        /* Verify written element is read in */
                        if(read_elem != write_elem) FAIL_PUTS_ERROR("invalid written element read");
                        if(read_elem2 != write_elem2) FAIL_PUTS_ERROR("invalid written element read");

                        /* Expand chunks now */
                        filter_expand_factor_g = 8;

                        /* Write element to the datasets */
                        write_elem = write_elem2 = u;
                        H5E_BEGIN_TRY {
                            status = H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem);
                        } H5E_END_TRY;
                        if(status >= 0) FAIL_PUTS_ERROR("should fail to write to dataset when allocation time is not early");

                        H5E_BEGIN_TRY {
                            status = H5Dwrite(dsid2, H5T_NATIVE_UINT, scalar_sid, sid2, H5P_DEFAULT, &write_elem2);
                        } H5E_END_TRY;
                        if(status >= 0) FAIL_PUTS_ERROR("should fail to write to dataset when allocation time is not early");
                    } /* end for */
                } /* end while */

                /* Close the datasets */
                if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
                if(H5Dclose(dsid2) < 0) FAIL_STACK_ERROR
            } /* end else */

            /* Close everything */
            if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
            if(H5Sclose(sid2) < 0) FAIL_STACK_ERROR
            if(H5Sclose(scalar_sid) < 0) FAIL_STACK_ERROR
            if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
            if(H5Pclose(dcpl2) < 0) FAIL_STACK_ERROR
            if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

            /* If the dataset was created, do some extra testing */
            if(H5D_ALLOC_TIME_EARLY != alloc_time) {
                /* Re-open file & datasets */
                if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

                /* Open the datasets */
                if((dsid = H5Dopen2(fid, "dset", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
                if((dsid2 = H5Dopen2(fid, "dset2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

                /* Get the chunk index type for the two datasets */
                if(H5D__layout_idx_type_test(dsid, &idx_type) < 0) FAIL_STACK_ERROR
                if(H5D__layout_idx_type_test(dsid2, &idx_type2) < 0) FAIL_STACK_ERROR

                /* Chunk index type expected depends on whether we are using the latest version of the format */
                if(low == H5F_LIBVER_LATEST) {
                    /* Verify index type */
                    if(idx_type != H5D_CHUNK_IDX_EARRAY) FAIL_PUTS_ERROR("should be using extensible array as index");
                    if(idx_type2 != H5D_CHUNK_IDX_BT2) FAIL_PUTS_ERROR("should be using v2 B-tree as index");
                } /* end if */
                else {
                    /* Verify index type */
                    if(idx_type != H5D_CHUNK_IDX_BTREE) FAIL_PUTS_ERROR("should be using v1 B-tree as index");
                    if(idx_type2 != H5D_CHUNK_IDX_BTREE) FAIL_PUTS_ERROR("should be using v1 B-tree as index");
                } /* end else */

                /* Create scalar dataspace */
                if((scalar_sid = H5Screate(H5S_SCALAR)) < 0) FAIL_STACK_ERROR

                /* Get dataspace for the datasets now */
                if((sid = H5Dget_space(dsid)) < 0) FAIL_STACK_ERROR
                if((sid2 = H5Dget_space(dsid2)) < 0) FAIL_STACK_ERROR

                /* Read elements */
                hs_size = hs_size2[0] = hs_size2[1] = 1;
                for(u = 0; u < 1000; u++) {
                    /* Select a single element in the datasets */
                    hs_offset = hs_offset2[0] = hs_offset2[1] = u;
                    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, &hs_offset, NULL, &hs_size, NULL) < 0) FAIL_STACK_ERROR
                    if(H5Sselect_hyperslab(sid2, H5S_SELECT_SET, hs_offset2, NULL, hs_size2, NULL) < 0) FAIL_STACK_ERROR

                    /* Read element from the datasets */
                    read_elem = read_elem2 = u + 1;
                    if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR
                    if(H5Dread(dsid2, H5T_NATIVE_UINT, scalar_sid, sid2, H5P_DEFAULT, &read_elem2) < 0) FAIL_STACK_ERROR

                    /* Verify unwritten element is proper value */
                    if(read_elem != (u % 100)) FAIL_PUTS_ERROR("invalid element read");
                    if(read_elem2 != (u % 100)) FAIL_PUTS_ERROR("invalid element read");

                    /* Don't expand chunks yet */
                    filter_expand_factor_g = 0;

                    /* Write element to the datasets */
                    write_elem = write_elem2 = u % 100;
                    if(H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem) < 0) FAIL_STACK_ERROR
                    if(H5Dwrite(dsid2, H5T_NATIVE_UINT, scalar_sid, sid2, H5P_DEFAULT, &write_elem2) < 0) FAIL_STACK_ERROR

                    /* Read element from the datasets */
                    read_elem = write_elem + 1;
                    read_elem2 = write_elem2 + 1;
                    if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR
                    if(H5Dread(dsid2, H5T_NATIVE_UINT, scalar_sid, sid2, H5P_DEFAULT, &read_elem2) < 0) FAIL_STACK_ERROR

                    /* Verify written element is read in */
                    if(read_elem != write_elem) FAIL_PUTS_ERROR("invalid written element read");
                    if(read_elem2 != write_elem2) FAIL_PUTS_ERROR("invalid written element read");

                    /* Expand chunks now */
                    filter_expand_factor_g = 8;

                    /* Write element to the datasets */
                    write_elem = write_elem2 = u % 100;
                    H5E_BEGIN_TRY {
                        status = H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem);
                    } H5E_END_TRY;
                    if(status >= 0) FAIL_PUTS_ERROR("should fail to write to dataset when allocation time is not early");

                    H5E_BEGIN_TRY {
                        status = H5Dwrite(dsid2, H5T_NATIVE_UINT, scalar_sid, sid2, H5P_DEFAULT, &write_elem2);
                    } H5E_END_TRY;
                    if(status >= 0) FAIL_PUTS_ERROR("should fail to write to dataset when allocation time is not early");
                } /* end for */

                /* Close everything */
                if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
                if(H5Sclose(sid2) < 0) FAIL_STACK_ERROR
                if(H5Sclose(scalar_sid) < 0) FAIL_STACK_ERROR
                if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
                if(H5Dclose(dsid2) < 0) FAIL_STACK_ERROR
                if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

                /* Re-open file */
                if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

                /* Delete the datasets */
                if(H5Ldelete(fid, "dset", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
                if(H5Ldelete(fid, "dset2", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

                /* Close everything */
                if(H5Fclose(fid) < 0) FAIL_STACK_ERROR
            } /* end if */
        } /* end for */

        /* Unregister "expansion" filter */
        if(H5Zunregister(H5Z_FILTER_EXPAND) < 0) FAIL_STACK_ERROR

        /* Check that the filter was unregistered */
        if(FALSE != H5Zfilter_avail(H5Z_FILTER_EXPAND)) FAIL_STACK_ERROR

        PASSED();
    } /* end else */

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Pclose(dcpl2);
        H5Dclose(dsid);
        H5Dclose(dsid2);
        H5Sclose(sid);
        H5Sclose(sid2);
        H5Sclose(scalar_sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_chunk_expand() */


/*-------------------------------------------------------------------------
 * Function: test_fixed_array
 *
 * Purpose: Tests support for Fixed Array and Implicit Indexing
 *
 *    Create the following 3 datasets:
 *    1) extendible chunked dataset with fixed max. dims
 *    2) extendible chunked dataset with NULL max. dims
 *    3) extendible chunked dataset with same max. dims
 *         (Note that the third dataset is created with bigger size for curr & max. dims
 *        so that Fixed Array Indexing with paging is exercised)
 *
 *      Repeat the following test with/without compression filter
 *              Repeat the following test with H5D_ALLOC_TIME_EARLY/H5D_ALLOC_TIME_LATE/H5D_ALLOC_TIME_INCR
 *                      For the old format,
 *                verify that v1 btree indexing type is used for
 *                    all 3 datasets with all settings
 *                      For the new format:
 *                Verify that Implicit Index type is used for
 *                    #1, #2, #3 datasets when ALLOC_TIME_EARLY and compression are true
 *                Verify Fixed Array indexing type is used for
 *                    #1, #2, #3 datasets with all other settings
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Vailin Choi; 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_fixed_array(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];    /* File name */
    hid_t       fid = -1;       /* File ID */
    hid_t       dcpl = -1;      /* Dataset creation property list ID */

    hid_t       sid = -1;       /* Dataspace ID for dataset with fixed dimensions */
    hid_t       sid_big = -1;   /* Dataspate ID for big dataset */
    hid_t       sid_max = -1;   /* Dataspace ID for dataset with maximum dimensions set */

    hid_t       dsid = -1;      /* Dataset ID for dataset with fixed dimensions */
    hid_t    dsid_big = -1;    /* Dataset ID for big dataset with fixed dimensions */
    hid_t    dsid_max = -1;    /* Dataset ID for dataset with maximum dimensions set */

    hsize_t     dim2[2] = {48, 18};           /* Dataset dimensions */
    hsize_t     dim2_big[2] = {500, 60};      /* Big dataset dimensions */
    hsize_t     dim2_max[2] = {120, 50};      /* Maximum dataset dimensions */

    hid_t       mem_id;          /* Memory space ID */
    hid_t       big_mem_id;       /* Memory space ID for big dataset */

    hsize_t     msize[1] = {POINTS};        /* Size of memory space */
    hsize_t     msize_big[1] = {POINTS_BIG};    /* Size of memory space for big dataset */

    int         wbuf[POINTS];           /* write buffer */
    int         *wbuf_big = NULL;      /* write buffer for big dataset */
    int         rbuf[POINTS];              /* read buffer */
    int         *rbuf_big = NULL;      /* read buffer for big dataset */

    hsize_t     chunk_dim2[2] = {4, 3}; /* Chunk dimensions */
    int         chunks[12][6];          /* # of chunks for dataset dimensions */
    int         chunks_big[125][20];    /* # of chunks for big dataset dimensions */
    int         chunk_row;              /* chunk row index */
    int         chunk_col;              /* chunk column index */

    hsize_t     coord[POINTS][2];           /* datdaset coordinates */
    hsize_t     coord_big[POINTS_BIG][2];       /* big dataset coordinates */

    H5D_chunk_index_t idx_type;     /* Dataset chunk index type */
    H5F_libver_t low, high;         /* File format bounds */
    H5D_alloc_time_t alloc_time;        /* Storage allocation time */

#ifdef H5_HAVE_FILTER_DEFLATE
    unsigned    compress;           /* Whether chunks should be compressed */
#endif /* H5_HAVE_FILTER_DEFLATE */

    h5_stat_size_t       empty_size;      /* Size of an empty file */
    h5_stat_size_t       file_size;      /* Size of each file created */

    size_t      i, j;               /* local index variables */
    herr_t      ret;                /* Generic return value */

    TESTING("datasets w/fixed array as chunk index");

    h5_fixname(FILENAME[12], fapl, filename, sizeof filename);

    /* Check if we are using the latest version of the format */
    if(H5Pget_libver_bounds(fapl, &low, &high) < 0) FAIL_STACK_ERROR

    /* Create and close the file to get the file size */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        STACK_ERROR
    if(H5Fclose(fid) < 0)
        STACK_ERROR

    /* Get the size of the empty file */
    if((empty_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Allocate the "big" buffers */
    if(NULL == (wbuf_big = (int *)HDmalloc(sizeof(int) * POINTS_BIG)))
        TEST_ERROR
    if(NULL == (rbuf_big = (int *)HDmalloc(sizeof(int) * POINTS_BIG)))
        TEST_ERROR

#ifdef H5_HAVE_FILTER_DEFLATE
    /* Loop over compressing chunks */
    for(compress = FALSE; compress <= TRUE; compress++) {
#endif /* H5_HAVE_FILTER_DEFLATE */

        /* Loop over storage allocation time */
        for(alloc_time = H5D_ALLOC_TIME_EARLY; alloc_time <= H5D_ALLOC_TIME_INCR; H5_INC_ENUM(H5D_alloc_time_t, alloc_time)) {
            /* Create file */
            if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

            /* Create dataset creation property list */
            if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

            /* Set chunking */
        if((ret = H5Pset_chunk(dcpl, 2, chunk_dim2)) < 0)
        FAIL_PUTS_ERROR("    Problem with setting chunk.")

#ifdef H5_HAVE_FILTER_DEFLATE
            /* Check if we should compress the chunks */
            if(compress)
                if(H5Pset_deflate(dcpl, 9) < 0) FAIL_STACK_ERROR
#endif /* H5_HAVE_FILTER_DEFLATE */

            /* Set fill time */
            if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) FAIL_STACK_ERROR

            /* Set allocation time */
            if(H5Pset_alloc_time(dcpl, alloc_time) < 0) FAIL_STACK_ERROR

        /* Initialization of chunk array for repeated coordinates */
        for(i = 0; i < dim2[0]/chunk_dim2[0]; i++)
        for(j = 0; j < dim2[1]/chunk_dim2[1]; j++)
            chunks[i][j] = 0;

        /* Generate random point coordinates. Only one point is selected per chunk */
        for(i = 0; i < POINTS; i++){
        do {
            chunk_row = (int)HDrandom () % (int)(dim2[0]/chunk_dim2[0]);
            chunk_col = (int)HDrandom () % (int)(dim2[1]/chunk_dim2[1]);
        } while (chunks[chunk_row][chunk_col]);

        wbuf[i] = chunks[chunk_row][chunk_col] = chunk_row+chunk_col+1;
        coord[i][0] = (hsize_t)chunk_row * chunk_dim2[0];
        coord[i][1] = (hsize_t)chunk_col * chunk_dim2[1];
        } /* end for */

        /* Create first dataset with cur and max dimensions */
        if((sid_max = H5Screate_simple(2, dim2, dim2_max)) < 0) FAIL_STACK_ERROR
        dsid_max = H5Dcreate2(fid, DSET_FIXED_MAX, H5T_NATIVE_INT, sid_max, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        if(dsid_max < 0)
        FAIL_PUTS_ERROR("    Creating Chunked Dataset with maximum dimensions.")

        /* Get the chunk index type */
        if(H5D__layout_idx_type_test(dsid_max, &idx_type) < 0) FAIL_STACK_ERROR

        /* Chunk index type depends on whether we are using the latest version of the format */
        if(low == H5F_LIBVER_LATEST) {
        if(alloc_time == H5D_ALLOC_TIME_EARLY
#ifdef H5_HAVE_FILTER_DEFLATE
            && !compress
#endif /* H5_HAVE_FILTER_DEFLATE */
            ) {
            if(idx_type != H5D_CHUNK_IDX_NONE)
            FAIL_PUTS_ERROR("should be using Non Index as index");
        } else if (idx_type != H5D_CHUNK_IDX_FARRAY)
            FAIL_PUTS_ERROR("should be using Fixed Array as index");
        } /* end if */
        else {
        if(idx_type != H5D_CHUNK_IDX_BTREE)
            FAIL_PUTS_ERROR("should be using v1 B-tree as index");
        } /* end else */

        /* Create dataspace for write buffer */
        if((mem_id = H5Screate_simple(1, msize, NULL)) < 0) TEST_ERROR;

        /* Select the random points for writing */
        if(H5Sselect_elements(sid_max, H5S_SELECT_SET, POINTS, (const hsize_t *)coord) < 0)
        TEST_ERROR;

        /* Write into dataset */
        if(H5Dwrite(dsid_max, H5T_NATIVE_INT, mem_id, sid_max, H5P_DEFAULT, wbuf) < 0) TEST_ERROR;

        /* Closing */
        if(H5Dclose(dsid_max) < 0) FAIL_STACK_ERROR
        if(H5Sclose(sid_max) < 0) FAIL_STACK_ERROR
            if(H5Sclose(mem_id) < 0) FAIL_STACK_ERROR


        /* Create second dataset with curr dim but NULL max dim */
        if((sid = H5Screate_simple(2, dim2, NULL)) < 0) FAIL_STACK_ERROR
        dsid = H5Dcreate2(fid, DSET_FIXED_NOMAX, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        if(dsid < 0)
        FAIL_PUTS_ERROR("    Creating Chunked Dataset.")

        /* Get the chunk index type */
        if(H5D__layout_idx_type_test(dsid, &idx_type) < 0) FAIL_STACK_ERROR

        /* Chunk index type depends on whether we are using the latest version of the format */
        if(low == H5F_LIBVER_LATEST) {
        if(alloc_time == H5D_ALLOC_TIME_EARLY
#ifdef H5_HAVE_FILTER_DEFLATE
            && !compress
#endif /* H5_HAVE_FILTER_DEFLATE */
            ) {
            if(idx_type != H5D_CHUNK_IDX_NONE)
            FAIL_PUTS_ERROR("should be using Non Index as index");
        } else if(idx_type != H5D_CHUNK_IDX_FARRAY)
            FAIL_PUTS_ERROR("should be using Fixed Array as index");
        } else {
        if(idx_type != H5D_CHUNK_IDX_BTREE)
            FAIL_PUTS_ERROR("should be using v1 B-tree as index");
        } /* end else */

        /* Create dataspace for write buffer */
        if((mem_id = H5Screate_simple(1, msize, NULL)) < 0) TEST_ERROR;

        /* Select the random points for writing */
        if(H5Sselect_elements(sid, H5S_SELECT_SET, POINTS, (const hsize_t *)coord) < 0)
        TEST_ERROR;

        /* Write into dataset */
        if(H5Dwrite(dsid, H5T_NATIVE_INT, mem_id, sid, H5P_DEFAULT, wbuf) < 0) TEST_ERROR;

        /* Closing */
        if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
        if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
            if(H5Sclose(mem_id) < 0) FAIL_STACK_ERROR

        /* Create the third dataset with bigger size and both curr & max dimensions are the same */
        if((sid_big = H5Screate_simple(2, dim2_big, dim2_big)) < 0) FAIL_STACK_ERROR
        dsid_big = H5Dcreate2(fid, DSET_FIXED_BIG, H5T_NATIVE_INT, sid_big, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        if(dsid_big < 0)
        FAIL_PUTS_ERROR("    Creating Big Chunked Dataset.")

        /* Get the chunk index type */
        if(H5D__layout_idx_type_test(dsid_big, &idx_type) < 0) FAIL_STACK_ERROR

        /* Chunk index type depends on whether we are using the latest version of the format */
        if(low == H5F_LIBVER_LATEST) {
        if(alloc_time == H5D_ALLOC_TIME_EARLY
#ifdef H5_HAVE_FILTER_DEFLATE
            && !compress
#endif /* H5_HAVE_FILTER_DEFLATE */
            ) {
            if(idx_type != H5D_CHUNK_IDX_NONE)
            FAIL_PUTS_ERROR("should be using Non Index as index");
        } else if(idx_type != H5D_CHUNK_IDX_FARRAY)
            FAIL_PUTS_ERROR("should be using Fixed Array as index");
        } /* end if */
        else {
        if(idx_type != H5D_CHUNK_IDX_BTREE)
            FAIL_PUTS_ERROR("should be using v1 B-tree as index");
        } /* end else */

        /* Initialization of chunk array for repeated coordinates */
        for(i = 0; i < dim2_big[0]/chunk_dim2[0]; i++)
        for(j = 0; j < dim2_big[1]/chunk_dim2[1]; j++)
            chunks_big[i][j] = 0;

        /* Generate random point coordinates. Only one point is selected per chunk */
        for(i = 0; i < POINTS_BIG; i++){
        do {
            chunk_row = (int)HDrandom () % (int)(dim2_big[0]/chunk_dim2[0]);
            chunk_col = (int)HDrandom () % (int)(dim2_big[1]/chunk_dim2[1]);
        } while (chunks_big[chunk_row][chunk_col]);

        wbuf_big[i] = chunks_big[chunk_row][chunk_col] = chunk_row+chunk_col+1;
        coord_big[i][0] = (hsize_t)chunk_row * chunk_dim2[0];
        coord_big[i][1] = (hsize_t)chunk_col * chunk_dim2[1];
        } /* end for */

        /* Create dataspace for write buffer */
        if((big_mem_id = H5Screate_simple(1, msize_big, NULL)) < 0) TEST_ERROR;

        /* Select the random points for writing */
        if(H5Sselect_elements(sid_big, H5S_SELECT_SET, POINTS_BIG, (const hsize_t *)coord_big) < 0)
        TEST_ERROR;

        /* Write into dataset */
        if(H5Dwrite(dsid_big, H5T_NATIVE_INT, big_mem_id, sid_big, H5P_DEFAULT, wbuf_big) < 0) TEST_ERROR;

        /* Closing */
        if(H5Dclose(dsid_big) < 0) FAIL_STACK_ERROR
        if(H5Sclose(sid_big) < 0) FAIL_STACK_ERROR
            if(H5Sclose(big_mem_id) < 0) FAIL_STACK_ERROR
            if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR

        /* Open the first dataset */
        if((dsid = H5Dopen2(fid, DSET_FIXED_MAX, H5P_DEFAULT)) < 0) TEST_ERROR;

        /* Get dataset dataspace */
        if((sid = H5Dget_space(dsid)) < 0) TEST_ERROR;

        /* Create dataspace for read buffer */
        if((mem_id = H5Screate_simple(1, msize, NULL)) < 0) TEST_ERROR;

        /* Select the random points for reading */
        if(H5Sselect_elements (sid, H5S_SELECT_SET, POINTS, (const hsize_t *)coord) < 0) TEST_ERROR;

        /* Read from dataset */
        if(H5Dread(dsid, H5T_NATIVE_INT, mem_id, sid, H5P_DEFAULT, rbuf) < 0) TEST_ERROR;

        /* Verify that written and read data are the same */
        for(i = 0; i < POINTS; i++)
        if(rbuf[i] != wbuf[i]){
            HDprintf("    Line %d: Incorrect value, wbuf[%u]=%d, rbuf[%u]=%d\n",
            __LINE__,(unsigned)i,wbuf[i],(unsigned)i,rbuf[i]);
            TEST_ERROR;
        } /* end if */

        /* Closing */
        if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
            if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
            if(H5Sclose(mem_id) < 0) FAIL_STACK_ERROR

        /* Open the second dataset */
        if((dsid = H5Dopen2(fid, DSET_FIXED_NOMAX, H5P_DEFAULT)) < 0) TEST_ERROR;

        /* Get dataset dataspace */
        if((sid = H5Dget_space(dsid)) < 0) TEST_ERROR;

        /* Create dataspace for read buffer */
        if((mem_id = H5Screate_simple(1, msize, NULL)) < 0) TEST_ERROR;

        /* Select the random points for reading */
        if(H5Sselect_elements (sid, H5S_SELECT_SET, POINTS, (const hsize_t *)coord) < 0) TEST_ERROR;

        /* Read from dataset */
        if(H5Dread(dsid, H5T_NATIVE_INT, mem_id, sid, H5P_DEFAULT, rbuf) < 0) TEST_ERROR;

        /* Verify that written and read data are the same */
        for(i = 0; i < POINTS; i++)
        if(rbuf[i] != wbuf[i]){
            HDprintf("    Line %d: Incorrect value, wbuf[%u]=%d, rbuf[%u]=%d\n",
            __LINE__,(unsigned)i,wbuf[i],(unsigned)i,rbuf[i]);
            TEST_ERROR;
        } /* end if */

        /* Closing */
        if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
            if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
            if(H5Sclose(mem_id) < 0) FAIL_STACK_ERROR

        /* Open the third dataset */
        if((dsid_big = H5Dopen2(fid, DSET_FIXED_BIG, H5P_DEFAULT)) < 0) TEST_ERROR;
        /* Get dataset dataspace */
        if((sid_big = H5Dget_space(dsid_big)) < 0) TEST_ERROR;

        /* Create dataspace for read buffer */
        if((big_mem_id = H5Screate_simple(1, msize_big, NULL)) < 0) TEST_ERROR;

        /* Select the random points for reading */
        if(H5Sselect_elements (sid_big, H5S_SELECT_SET, POINTS_BIG, (const hsize_t *)coord_big) < 0) TEST_ERROR;
        /* Read from dataset */
        if(H5Dread(dsid_big, H5T_NATIVE_INT, big_mem_id, sid_big, H5P_DEFAULT, rbuf_big) < 0) TEST_ERROR;

        /* Verify that written and read data are the same */
        for(i = 0; i < POINTS_BIG; i++)
        if(rbuf_big[i] != wbuf_big[i]) {
            HDprintf("    Line %d: Incorrect value, wbuf_bif[%u]=%d, rbuf_big[%u]=%d\n",
            __LINE__,(unsigned)i,wbuf_big[i],(unsigned)i,rbuf_big[i]);
            TEST_ERROR;
        } /* end if */

        /* Closing */
        if(H5Dclose(dsid_big) < 0) FAIL_STACK_ERROR
            if(H5Sclose(sid_big) < 0) FAIL_STACK_ERROR
            if(H5Sclose(big_mem_id) < 0) FAIL_STACK_ERROR

            /* Delete datasets */
            if(H5Ldelete(fid, DSET_FIXED_BIG, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
            if(H5Ldelete(fid, DSET_FIXED_NOMAX, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
            if(H5Ldelete(fid, DSET_FIXED_MAX, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

            /* Close everything */
            if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

        /* Verify the file is correct size */
        if(file_size != empty_size)
        TEST_ERROR

        } /* end for */
#ifdef H5_HAVE_FILTER_DEFLATE
    } /* end for */
#endif /* H5_HAVE_FILTER_DEFLATE */

    /* Release buffers */
    HDfree(wbuf_big);
    HDfree(rbuf_big);

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Sclose(mem_id);
        H5Fclose(fid);
    } H5E_END_TRY;
    if(wbuf_big)
        HDfree(wbuf_big);
    if(rbuf_big)
        HDfree(rbuf_big);
    return FAIL;
} /* end test_fixed_array() */


/*-------------------------------------------------------------------------
 * Function: test_single_chunk
 *
 * Purpose: Tests support for Single Chunk indexing type
 *
 *    Create the following 2 datasets:
 *    1) chunked dataset with NULL max dims and cur_dims = chunk_dims
 *    2) chunked dataset with cur_dims = max_dims = chunk_dims
 *
 *      Repeat the following test with/without compression filter
 *              Repeat the following test with H5D_ALLOC_TIME_EARLY/H5D_ALLOC_TIME_LATE/H5D_ALLOC_TIME_INCR
 *                      For the old format,
 *              verify that v1 btree indexing type is used for
 *                all datasets with all settings
 *                      For the new format:
 *              Verify that Single Chunk indexing type is used for
 *                all datasets with all settings
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Vailin Choi; July 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_single_chunk(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];    /* File name */
    hid_t       fid = -1;       /* File ID */
    hid_t       dcpl = -1;      /* Dataset creation property list ID */
    hid_t       t_dcpl = -1;      /* Dataset creation property list ID */

    hid_t       sid = -1, sid_max = -1;           /* Dataspace ID for dataset with fixed dimensions */
    hid_t       did = -1, did_max = -1;          /* Dataset ID for dataset with fixed dimensions */
    hsize_t     dim2[2] = {DSET_DIM1, DSET_DIM2};       /* Dataset dimensions */
    hsize_t     t_dim2[2] = {DSET_TMP_DIM1, DSET_TMP_DIM2};   /* Dataset dimensions */
    int         *wbuf = NULL;            /* write buffer */
    int         *t_wbuf = NULL;          /* write buffer */
    int         *rbuf = NULL;               /* read buffer */
    int         *t_rbuf = NULL;          /* read buffer */

    H5D_chunk_index_t idx_type;     /* Dataset chunk index type */
    H5F_libver_t low, high;         /* File format bounds */
    H5D_alloc_time_t alloc_time;        /* Storage allocation time */

#ifdef H5_HAVE_FILTER_DEFLATE
    unsigned    compress;           /* Whether chunks should be compressed */
#endif /* H5_HAVE_FILTER_DEFLATE */

    size_t      n, i;               /* local index variables */
    herr_t      ret;                /* Generic return value */
    h5_stat_size_t       empty_size;    /* Size of an empty file */
    h5_stat_size_t       file_size;     /* Size of each file created */

    TESTING("datasets w/Single Chunk indexing");

    h5_fixname(FILENAME[17], fapl, filename, sizeof filename);

    /* Check if we are using the latest version of the format */
    if(H5Pget_libver_bounds(fapl, &low, &high) < 0) FAIL_STACK_ERROR

    /* Create and close the file to get the file size */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        STACK_ERROR
    if(H5Fclose(fid) < 0)
        STACK_ERROR

    /* Get the size of the empty file */
    if((empty_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Allocate the buffers */
    if(NULL == (wbuf = (int *)HDmalloc(sizeof(int) * (DSET_DIM1 * DSET_DIM2))))
        TEST_ERROR
    if(NULL == (rbuf = (int *)HDmalloc(sizeof(int) * (DSET_DIM1 * DSET_DIM2))))
        TEST_ERROR
    if(NULL == (t_wbuf = (int *)HDmalloc(sizeof(int) * (DSET_TMP_DIM1 * DSET_TMP_DIM2))))
        TEST_ERROR
    if(NULL == (t_rbuf = (int *)HDmalloc(sizeof(int) * (DSET_TMP_DIM1 * DSET_TMP_DIM2))))
        TEST_ERROR

    for(i = n = 0; i < (DSET_DIM1 * DSET_DIM2); i++)
    wbuf[i] = (int)n++;

    for(i = n = 0; i < (DSET_TMP_DIM1* DSET_TMP_DIM2); i++)
    t_wbuf[i] = (int)n++;

#ifdef H5_HAVE_FILTER_DEFLATE
    /* Loop over compressing chunks */
    for(compress = FALSE; compress <= TRUE; compress++) {
#endif /* H5_HAVE_FILTER_DEFLATE */

        /* Loop over storage allocation time */
        for(alloc_time = H5D_ALLOC_TIME_EARLY; alloc_time <= H5D_ALLOC_TIME_INCR; H5_INC_ENUM(H5D_alloc_time_t, alloc_time)) {
            /* Create file */
            if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

            /* Create dataset creation property list */
            if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR
            if((t_dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

            /* Set chunking */
        if((ret = H5Pset_chunk(dcpl, 2, dim2)) < 0)
        FAIL_PUTS_ERROR("    Problem with setting chunk.")

        if((ret = H5Pset_chunk(t_dcpl, 2, t_dim2)) < 0)
        FAIL_PUTS_ERROR("    Problem with setting chunk.")

#ifdef H5_HAVE_FILTER_DEFLATE
            /* Check if we should compress the chunks */
            if(compress) {
                if(H5Pset_deflate(dcpl, 9) < 0) FAIL_STACK_ERROR
                if(H5Pset_deflate(t_dcpl, 9) < 0) FAIL_STACK_ERROR
        }
#endif /* H5_HAVE_FILTER_DEFLATE */

            /* Set fill time */
            if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) FAIL_STACK_ERROR
            if(H5Pset_fill_time(t_dcpl, H5D_FILL_TIME_ALLOC) < 0) FAIL_STACK_ERROR

            /* Set allocation time */
            if(H5Pset_alloc_time(dcpl, alloc_time) < 0) FAIL_STACK_ERROR
            if(H5Pset_alloc_time(t_dcpl, alloc_time) < 0) FAIL_STACK_ERROR

        /* Create first dataset with cur and max dimensions */
        if((sid_max = H5Screate_simple(2, dim2, dim2)) < 0) FAIL_STACK_ERROR
        did_max = H5Dcreate2(fid, DSET_SINGLE_MAX, H5T_NATIVE_INT, sid_max, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        if(did_max < 0)
        FAIL_PUTS_ERROR("    Creating Chunked Dataset with maximum dimensions.")

        /* Get the chunk index type */
        if(H5D__layout_idx_type_test(did_max, &idx_type) < 0) FAIL_STACK_ERROR

        /* Chunk index type depends on whether we are using the latest version of the format */
        if(low == H5F_LIBVER_LATEST) {
        if(idx_type != H5D_CHUNK_IDX_SINGLE)
            FAIL_PUTS_ERROR("should be using Single Chunk indexing");
        } /* end if */
        else {
        if(idx_type != H5D_CHUNK_IDX_BTREE)
            FAIL_PUTS_ERROR("should be using v1 B-tree as index");
        } /* end else */

        /* Write into dataset */
        if(H5Dwrite(did_max, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0) TEST_ERROR;

        /* Closing */
        if(H5Dclose(did_max) < 0) FAIL_STACK_ERROR
        if(H5Sclose(sid_max) < 0) FAIL_STACK_ERROR

        /* Create second dataset with curr dim but NULL max dim */
        if((sid = H5Screate_simple(2, t_dim2, NULL)) < 0) FAIL_STACK_ERROR
        did = H5Dcreate2(fid, DSET_SINGLE_NOMAX, H5T_NATIVE_INT, sid, H5P_DEFAULT, t_dcpl, H5P_DEFAULT);
        if(did < 0)
        FAIL_PUTS_ERROR("    Creating Chunked Dataset.")

        /* Get the chunk index type */
        if(H5D__layout_idx_type_test(did, &idx_type) < 0) FAIL_STACK_ERROR

        /* Chunk index type depends on whether we are using the latest version of the format */
        if(low == H5F_LIBVER_LATEST) {
        if(idx_type != H5D_CHUNK_IDX_SINGLE)
            FAIL_PUTS_ERROR("should be using Single Chunk indexing");
        } else {
        if(idx_type != H5D_CHUNK_IDX_BTREE)
            FAIL_PUTS_ERROR("should be using v1 B-tree as index");
        } /* end else */

        /* Write into dataset */
        if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, sid, H5P_DEFAULT, t_wbuf) < 0) TEST_ERROR;

        /* Closing */
        if(H5Dclose(did) < 0) FAIL_STACK_ERROR
        if(H5Sclose(sid) < 0) FAIL_STACK_ERROR

        /* Open the first dataset */
        if((did_max = H5Dopen2(fid, DSET_SINGLE_MAX, H5P_DEFAULT)) < 0) TEST_ERROR;

        /* Read from dataset */
        if(H5Dread(did_max, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0) TEST_ERROR;

        /* Verify that written and read data are the same */
        for(i = 0; i < (DSET_DIM1 * DSET_DIM2); i++)
        if(rbuf[i] != wbuf[i]){
            HDprintf("    Line %d: Incorrect value, wbuf[%u]=%d, rbuf[%u]=%d\n",
            __LINE__,(unsigned)i,wbuf[i],(unsigned)i,rbuf[i]);
            TEST_ERROR;
        } /* end if */

        /* Closing */
        if(H5Dclose(did_max) < 0) FAIL_STACK_ERROR

        /* Open the second dataset */
        if((did = H5Dopen2(fid, DSET_SINGLE_NOMAX, H5P_DEFAULT)) < 0) TEST_ERROR;

        HDmemset(rbuf, 0, sizeof(int) * (DSET_DIM1 * DSET_DIM2));

        /* Read from dataset */
        if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, t_rbuf) < 0) TEST_ERROR;

        /* Verify that written and read data are the same */
        for(i = 0; i < (DSET_TMP_DIM1* DSET_TMP_DIM2); i++)
        if(t_rbuf[i] != t_wbuf[i]) {
            HDprintf("    Line %d: Incorrect value, t_wbuf[%u]=%d, t_rbuf[%u]=%d\n",
            __LINE__,(unsigned)i,t_wbuf[i],(unsigned)i,t_rbuf[i]);
            TEST_ERROR;
        } /* end if */

        /* Closing */
        if(H5Dclose(did) < 0) FAIL_STACK_ERROR

        /* Delete datasets */
            if(H5Ldelete(fid, DSET_SINGLE_NOMAX, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
            if(H5Ldelete(fid, DSET_SINGLE_MAX, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

            /* Close everything */
            if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

             /* Get the size of the file */
            if((file_size = h5_get_file_size(filename, fapl)) < 0)
                TEST_ERROR

            /* Verify the file is correct size */
            if(file_size != empty_size)
                TEST_ERROR

        } /* end for */
#ifdef H5_HAVE_FILTER_DEFLATE
    } /* end for */
#endif /* H5_HAVE_FILTER_DEFLATE */


    /* Release buffers */
    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(t_wbuf);
    HDfree(t_rbuf);

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Pclose(t_dcpl);
        H5Dclose(did);
        H5Dclose(did_max);
        H5Sclose(sid);
        H5Sclose(sid_max);
        H5Fclose(fid);
    } H5E_END_TRY;
    if(wbuf)
        HDfree(wbuf);
    if(rbuf)
        HDfree(rbuf);
    if(t_wbuf)
        HDfree(t_wbuf);
    if(t_rbuf)
        HDfree(t_rbuf);
    return FAIL;
} /* end test_single_chunk() */


/*-------------------------------------------------------------------------
 *
 *  test_idx_compatible():
 *    Verify that the library can read datasets created with
 *    1.6/1.8 library that use the B-tree indexing method.
 *
 *  Programmer: Vailin Choi; 26th August, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_idx_compatible(void)
{
    hid_t    fid = -1;        /* File id */
    hid_t       did = -1;        /* Dataset id */
    const char  *filename = NULL;  /* old test file name */
    unsigned    j;              /* Local index variable */
    H5D_chunk_index_t idx_type; /* Chunked dataset index type */

    /* Output message about test being performed */
    TESTING("compatibility for 1.6/1.8 datasets that use B-tree indexing");

    for(j = 0; j < NELMTS(OLD_FILENAME); j++) {

    /* Generate correct name for test file by prepending the source path */
    filename = H5_get_srcdir_filename(OLD_FILENAME[j]);

    /* Open the file */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Should be able to read the dataset w/o filter created under 1.8/1.6 */
    if((did = H5Dopen2(fid, DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get the chunk index type */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0) FAIL_STACK_ERROR

    /* Verify index type */
    if(idx_type != H5D_CHUNK_IDX_BTREE)
        FAIL_PUTS_ERROR("should be using v1 B-tree as index")

    if(H5Dclose(did) < 0) FAIL_STACK_ERROR

    /* Should be able to read the dataset w/ filter created under 1.8/1.6 */
    if((did = H5Dopen2(fid, DSET_FILTER, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get the chunk index type */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0) FAIL_STACK_ERROR

    /* Verify index type */
    if(idx_type != H5D_CHUNK_IDX_BTREE)
        FAIL_PUTS_ERROR("should be using v1 B-tree as index")

    if(H5Dclose(did) < 0) FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR
    }

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(did);
    H5Fclose(fid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_idx_compatible() */

/*-------------------------------------------------------------------------
 *
 *  test_unfiltered_edge_chunks():
 *      Tests that partial edge chunks aren't filtered when the
 *      H5D_CHUNK_FILTER_PARTIAL_CHUNKS option is set.
 *
 *  Programmer: Neil Fortner; 17th March, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_unfiltered_edge_chunks(hid_t fapl)
{
    hid_t       fid = -1;            /* File id */
    hid_t       did = -1;            /* Dataset id */
    hid_t       sid = -1;            /* Dataspace id */
    hid_t       dcpl = -1;           /* DCPL id */
    hsize_t     dim[2] = {4, 3}; /* Dataset dimensions */
    hsize_t     cdim[2] = {2, 2}; /* Chunk dimension */
    char        wbuf[4][3];     /* Write buffer */
    char        rbuf[4][3];     /* Read buffer */
    char        filename[FILENAME_BUF_SIZE] = "";  /* old test file name */
    unsigned    opts;           /* Chunk options */
    unsigned    i, j;           /* Local index variables */

    /* Output message about test being performed */
    TESTING("disabled partial chunk filters");

    h5_fixname(FILENAME[14], fapl, filename, sizeof filename);

    /* Create the file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Register byte-counting filter */
    if(H5Zregister(H5Z_COUNT) < 0)
        TEST_ERROR

    /* Create dataspace */
    if((sid = H5Screate_simple(2, dim, NULL)) < 0)
        TEST_ERROR

    /* Create DCPL */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    /* Set chunk dimensions */
    if(H5Pset_chunk(dcpl, 2, cdim) < 0)
        TEST_ERROR

    /* Add "count" filter */
    if(H5Pset_filter(dcpl, H5Z_FILTER_COUNT, 0u, (size_t)0, NULL) < 0)
        TEST_ERROR

    /* Disable filters on partial chunks */
    if(H5Pget_chunk_opts(dcpl, &opts) < 0)
        TEST_ERROR
    opts |= H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS;
    if(H5Pset_chunk_opts(dcpl, opts) < 0)
        TEST_ERROR

    /* Initialize write buffer */
    for(i=0; i<dim[0]; i++)
        for(j=0; j<dim[1]; j++)
            wbuf[i][j] = (char)((2 * i) - j);

    /* Reset byte counts */
    count_nbytes_read = (size_t)0;
    count_nbytes_written = (size_t)0;

    /* Create dataset */
    if((did = H5Dcreate2(fid, DSET_CHUNKED_NAME, H5T_NATIVE_CHAR, sid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Nothing should have been written, as we are not using early allocation */
    if(count_nbytes_read != (size_t)0)
        TEST_ERROR
    if(count_nbytes_written != (size_t)0)
        TEST_ERROR

    /* Write data */
    if(H5Dwrite(did, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR

    /* Close dataset */
    if(H5Dclose(did) < 0)
        TEST_ERROR

    /* Make sure only 2 of the 4 chunks were written through the filter (4 bytes
     * each) */
    if(count_nbytes_read != (size_t)0)
        TEST_ERROR
    if(count_nbytes_written != (size_t)(2 * cdim[0] * cdim[1]))
        TEST_ERROR

    /* Reopen the dataset */
    if((did = H5Dopen2(fid, DSET_CHUNKED_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Read the dataset */
    if(H5Dread(did, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR

    /* Verify that data read == data written */
    for(i=0; i<dim[0]; i++)
        for(j=0; j<dim[1]; j++)
            if(rbuf[i][j] != wbuf[i][j])
                TEST_ERROR

    /* Make sure only 2 of the 4 chunks were read through the filter (4 bytes
     * each) */
    if(count_nbytes_read != (size_t)(2 * cdim[0] * cdim[1]))
        TEST_ERROR
    if(count_nbytes_written != (size_t)(2 * cdim[0] * cdim[1]))
        TEST_ERROR

    /* Close IDs */
    if(H5Dclose(did) < 0)
        TEST_ERROR
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR
    if(H5Sclose(sid) < 0)
        TEST_ERROR
    if(H5Fclose(fid) < 0)
        TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(did);
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_unfiltered_edge_chunks() */


/*-------------------------------------------------------------------------
 * Function: test_large_chunk_shrink
 *
 * Purpose: Tests support for shrinking a chunk larger than 1 MB by a
 *              size greater than 1 MB.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Neil Fortner
 *              Monday, November 31, 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_large_chunk_shrink(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;       /* File ID */
    hid_t       dcpl = -1;      /* Dataset creation property list ID */
    hid_t       sid = -1;       /* Dataspace ID */
    hid_t       scalar_sid = -1;/* Scalar dataspace ID */
    hid_t       dsid = -1;      /* Dataset ID */
    hsize_t     dim, max_dim, chunk_dim; /* Dataset and chunk dimensions */
    hsize_t     hs_offset;      /* Hyperslab offset */
    hsize_t     hs_size;        /* Hyperslab size */
    unsigned    write_elem, read_elem;  /* Element written/read */

    TESTING("shrinking large chunk");

    h5_fixname(FILENAME[10], fapl, filename, sizeof filename);

    /* Create file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

    /* Set 2 MB chunk size */
    chunk_dim = 2 * 1024 * 1024 / sizeof(unsigned);
    if(H5Pset_chunk(dcpl, 1, &chunk_dim) < 0) FAIL_STACK_ERROR

    /* Create scalar dataspace */
    if((scalar_sid = H5Screate(H5S_SCALAR)) < 0) FAIL_STACK_ERROR

    /* Create 1-D dataspace */
    dim = 2 * 1024 * 1024 / sizeof(unsigned);
    max_dim = H5S_UNLIMITED;
    if((sid = H5Screate_simple(1, &dim, &max_dim)) < 0) FAIL_STACK_ERROR

    /* Create 2 MB chunked dataset */
    if((dsid = H5Dcreate2(fid, "dset", H5T_NATIVE_UINT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Select last element in the dataset */
    hs_offset = dim - 1;
    hs_size = 1;
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, &hs_offset, NULL, &hs_size, NULL) < 0) FAIL_STACK_ERROR

    /* Read (unwritten) element from dataset */
    read_elem = 1;
    if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR

    /* Verify unwritten element is fill value (0) */
    if(read_elem != 0) FAIL_PUTS_ERROR("invalid unwritten element read");

    /* Write element to dataset */
    write_elem = 2;
    if(H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem) < 0) FAIL_STACK_ERROR

    /* Read element from dataset */
    read_elem = write_elem + 1;
    if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR

    /* Verify written element is read in */
    if(read_elem != write_elem) FAIL_PUTS_ERROR("invalid written element read");

    /* Shrink dataset to 512 KB */
    dim = 512 * 1024 / sizeof(unsigned);
    if(H5Dset_extent(dsid, &dim) < 0) FAIL_STACK_ERROR

    /* Expand dataset back to 2MB */
    dim = 2 * 1024 * 1024 / sizeof(unsigned);
    if(H5Dset_extent(dsid, &dim) < 0) FAIL_STACK_ERROR

    /* Read element from dataset */
    read_elem = 1;
    if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR

    /* Verify element is now 0 */
    if(read_elem != 0) FAIL_PUTS_ERROR("invalid element read");

    /* Close everything */
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if(H5Sclose(scalar_sid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Sclose(scalar_sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_large_chunk_shrink() */


/*-------------------------------------------------------------------------
 * Function: test_zero_dim_dset
 *
 * Purpose:     Tests support for reading a 1D chunked dataset with
 *              dimension size = 0.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Mohamad Chaarawi
 *              Wednesdat, July 9, 2014
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_zero_dim_dset(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;       /* File ID */
    hid_t       dcpl = -1;      /* Dataset creation property list ID */
    hid_t       sid = -1;       /* Dataspace ID */
    hid_t       dsid = -1;      /* Dataset ID */
    hsize_t     dim, chunk_dim; /* Dataset and chunk dimensions */
    int         data[1];
    H5F_libver_t low, high; /* File format bounds */
    herr_t      ret;                /* Generic return value */

    TESTING("chunked dataset with zero dimension");

    /* Loop through all the combinations of low/high library format bounds,
       skipping invalid combination, and verify support for reading a 1D
       chunked dataset with dimension size = 0 */
    for(low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for(high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {

            /* Set version bounds before opening the file */
            H5E_BEGIN_TRY {
                ret = H5Pset_libver_bounds(fapl, low, high);
            } H5E_END_TRY;

            if (ret < 0) /* Invalid low/high combinations */
                continue;

            h5_fixname(FILENAME[16], fapl, filename, sizeof filename);

            /* Create file */
            if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

            /* Create dataset creation property list */
            if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

            /* Set 1 chunk size */
            chunk_dim = 1;
            if(H5Pset_chunk(dcpl, 1, &chunk_dim) < 0) FAIL_STACK_ERROR

            /* Create 1D dataspace with 0 dim size */
            dim = 0;
            if((sid = H5Screate_simple(1, &dim, NULL)) < 0) FAIL_STACK_ERROR

            /* Create chunked dataset */
            if((dsid = H5Dcreate2(fid, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR

            /* write 0 elements from dataset */
            if(H5Dwrite(dsid, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, data) < 0) FAIL_STACK_ERROR

            /* Read 0 elements from dataset */
            if(H5Dread(dsid, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, data) < 0) FAIL_STACK_ERROR

            /* Close everything */
            if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
            if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
            if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
            if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

        } /* end for high */
    } /* end for low */

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_zero_dim_dset() */


/*-------------------------------------------------------------------------
 * Function: test_swmr_non_latest
 *
 * Purpose: Checks that a file created with either:
 *        (a) SWMR-write + non-latest-format
 *        (b) write + latest format
 *        will generate datset with latest chunk indexing type.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_swmr_non_latest(const char *env_h5_driver, hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;               /* File ID */
    hid_t       gid = -1;               /* Group ID */
    hid_t       dcpl = -1;              /* Dataset creation property list ID */
    hid_t       sid = -1;               /* Dataspace ID */
    hid_t       did = -1;               /* Dataset ID */
    hsize_t     dim[1], dims2[2];            /* Size of dataset */
    hsize_t     max_dim[1], max_dims2[2];        /* Maximum size of dataset */
    hsize_t     chunk_dim[1], chunk_dims2[2];      /* Chunk dimensions */
    H5D_chunk_index_t idx_type;         /* Chunk index type */
    int         data;                  /* Data to be written to the dataset */
    H5F_libver_t low;                   /* File format low bound */

    TESTING("File created with write+latest-format/SWMR-write+non-latest-format: dataset with latest chunk index");

    /* Skip this test if SWMR I/O is not supported for the VFD specified
     * by the environment variable.
     */
    if(!H5FD_supports_swmr_test(env_h5_driver)) {
        SKIPPED();
        HDputs("    Test skipped due to VFD not supporting SWMR I/O.");
        return SUCCEED;
    }

    /* Check if we are using the latest version of the format */
    if(H5Pget_libver_bounds(fapl, &low, NULL) < 0)
        FAIL_STACK_ERROR

    h5_fixname(FILENAME[18], fapl, filename, sizeof filename);

    if(low == H5F_LIBVER_LATEST) {
        /* Create file with write+latest-format */
        if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR
    }
    else {
        /* Create file with SWMR-write+non-latest-format */
        if((fid = H5Fcreate(filename, H5F_ACC_TRUNC|H5F_ACC_SWMR_WRITE, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR
    } /* end else */

    /* Create a chunked dataset: this will use extensible array chunk indexing */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR

    chunk_dim[0] = 6;
    if(H5Pset_chunk(dcpl, 1, chunk_dim) < 0)
        FAIL_STACK_ERROR

    dim[0] = 1;
    max_dim[0] = H5S_UNLIMITED;
    if((sid = H5Screate_simple(1, dim, max_dim)) < 0)
        FAIL_STACK_ERROR

    if((did = H5Dcreate2(fid, DSET_CHUNKED_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Write to the dataset */
    data = 100;
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &data) < 0)
        FAIL_STACK_ERROR

    /* Verify the dataset's indexing type */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0)
        FAIL_STACK_ERROR
    if(idx_type != H5D_CHUNK_IDX_EARRAY)
        FAIL_PUTS_ERROR("created dataset not indexed by extensible array")

    /* Closing */
    if(H5Dclose(did) < 0) FAIL_STACK_ERROR
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Open the file again */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Open the dataset in the file */
    if((did = H5Dopen2(fid, DSET_CHUNKED_NAME, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Verify the dataset's indexing type */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0)
        FAIL_STACK_ERROR
    if(idx_type != H5D_CHUNK_IDX_EARRAY)
        FAIL_PUTS_ERROR("created dataset not indexed by extensible array")

    /* Read from the dataset and verify data read is correct */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &data) < 0)
        FAIL_STACK_ERROR
    if(data != 100)
        TEST_ERROR

    /* Close the dataset */
    if(H5Dclose(did) < 0) FAIL_STACK_ERROR

    /* Create a group in the file */
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Create a chunked dataset in the group: this will use v2 B-tree chunk indexing */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR

    chunk_dims2[0] = chunk_dims2[1] = 10;
    if(H5Pset_chunk(dcpl, 2, chunk_dims2) < 0)
        FAIL_STACK_ERROR

    dims2[0] = dims2[1] = 1;
    max_dims2[0] = max_dims2[1] = H5S_UNLIMITED;
    if((sid = H5Screate_simple(2, dims2, max_dims2)) < 0)
        FAIL_STACK_ERROR

    if((did = H5Dcreate2(gid, DSET_CHUNKED_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Verify the dataset's indexing type */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0)
        FAIL_STACK_ERROR
    if(idx_type != H5D_CHUNK_IDX_BT2)
        FAIL_PUTS_ERROR("created dataset not indexed by v2 B-tree")

    /* Closing */
    if(H5Dclose(did) < 0) FAIL_STACK_ERROR
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Open the file again */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Open the group */
    if((gid = H5Gopen2(fid, "group", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Open the dataset in the group */
    if((did = H5Dopen2(gid, DSET_CHUNKED_NAME, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Verify the dataset's indexing type */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0)
        FAIL_STACK_ERROR
    if(idx_type != H5D_CHUNK_IDX_BT2)
        FAIL_PUTS_ERROR("created dataset not indexed by v2 B-tree")

    /* Closing */
    if(H5Dclose(did) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR


    /* Reopen the file with SWMR-write */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Open the dataset in the file */
    if((did = H5Dopen2(fid, DSET_CHUNKED_NAME, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Verify the dataset's indexing type */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0)
        FAIL_STACK_ERROR
    if(idx_type != H5D_CHUNK_IDX_EARRAY)
        FAIL_PUTS_ERROR("created dataset not indexed by extensible array")

    /* Close the dataset */
    if(H5Dclose(did) < 0) FAIL_STACK_ERROR

    /* Open the group */
    if((gid = H5Gopen2(fid, "group", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Open the dataset in the group */
    if((did = H5Dopen2(gid, DSET_CHUNKED_NAME, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Verify the dataset's indexing type */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0)
        FAIL_STACK_ERROR
    if(idx_type != H5D_CHUNK_IDX_BT2)
        FAIL_PUTS_ERROR("created dataset not indexed by v2 B-tree")

    /* Write to the dataset in the group */
    data = 99;
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &data) < 0)
        FAIL_STACK_ERROR

    /* Closing */
    if(H5Dclose(did) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Open the file again with SWMR read access */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR

    if((gid = H5Gopen2(fid, "group", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Open the dataset */
    if((did = H5Dopen2(gid, DSET_CHUNKED_NAME, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Read from the dataset and verify data read is correct */
    data = 0;
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &data) < 0)
        FAIL_STACK_ERROR
    if(data != 99)
        TEST_ERROR

    /* Closing */
    if(H5Dclose(did) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(did);
        H5Sclose(sid);
        H5Gclose(gid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return FAIL;
} /* test_swmr_non_latest() */


/*-------------------------------------------------------------------------
 * Function: test_earray_hdr_fd
 *
 * Purpose: Tests that extensible array header flush dependencies
 *          are created and torn down correctly when used as a
 *          chunk index.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_earray_hdr_fd(const char *env_h5_driver, hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t fid = -1;
    hid_t sid = -1;
    hid_t did = -1;
    hid_t tid = -1;
    hid_t dcpl = -1;
    hid_t msid = -1;
    H5D_chunk_index_t idx_type;
    const hsize_t shape[1] = { 8 };
    const hsize_t maxshape[1] = { H5S_UNLIMITED };
    const hsize_t chunk[1] = { 8 };
    const int buffer[8] = {0, 1, 2, 3, 4, 5, 6, 7};
    H5O_info_t info;

    TESTING("Extensible array chunk index header flush dependencies handled correctly");

    /* Skip this test if SWMR I/O is not supported for the VFD specified
     * by the environment variable.
     */
    if(!H5FD_supports_swmr_test(env_h5_driver)) {
        SKIPPED();
        HDputs("    Test skipped due to VFD not supporting SWMR I/O.");
        return SUCCEED;
    }

    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        FAIL_STACK_ERROR;
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    h5_fixname(FILENAME[19], fapl, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Create a dataset with one unlimited dimension */
    if((sid = H5Screate_simple(1, shape, maxshape)) < 0)
        FAIL_STACK_ERROR;
    if((tid = H5Tcopy(H5T_NATIVE_INT32)) < 0)
        FAIL_STACK_ERROR;
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    if(H5Pset_chunk(dcpl, 1, chunk) < 0)
        FAIL_STACK_ERROR;
    if((did = H5Dcreate2(fid, DSET_EARRAY_HDR_FD, tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT )) < 0)
        FAIL_STACK_ERROR;

    /* Verify the chunk index type */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0)
        FAIL_STACK_ERROR;
    if(idx_type != H5D_CHUNK_IDX_EARRAY)
        FAIL_PUTS_ERROR("should be using extensible array as index");

    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;
    if(H5Tclose(tid) < 0)
        FAIL_STACK_ERROR;
    if(H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;

    if(H5Fstart_swmr_write(fid) < 0)
        FAIL_STACK_ERROR;

    /* Write data to the dataset */
    if((did = H5Dopen2(fid, DSET_EARRAY_HDR_FD, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if((tid = H5Dget_type(did)) < 0)
        FAIL_STACK_ERROR;
    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer) < 0)
        FAIL_STACK_ERROR;
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if(H5Tclose(tid) < 0)
        FAIL_STACK_ERROR;

    /* The second call triggered a bug in the library (JIRA issue: SWMR-95) */
    if(H5Oget_info_by_name2(fid, DSET_EARRAY_HDR_FD, &info, H5O_INFO_BASIC, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;
    if(H5Oget_info_by_name2(fid, DSET_EARRAY_HDR_FD, &info, H5O_INFO_BASIC, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;

    if(H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    if(H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Fclose(fid);
        H5Dclose(did);
        H5Pclose(dcpl);
        H5Tclose(tid);
        H5Sclose(sid);
        H5Sclose(msid);
    } H5E_END_TRY;
    return FAIL;
} /* test_earray_hdr_fd() */


/*-------------------------------------------------------------------------
 * Function: test_farray_hdr_fd
 *
 * Purpose: Tests that fixed array header flush dependencies
 *          are created and torn down correctly when used as a
 *          chunk index.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_farray_hdr_fd(const char *env_h5_driver, hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t fid = -1;
    hid_t sid = -1;
    hid_t did = -1;
    hid_t tid = -1;
    hid_t dcpl = -1;
    hid_t msid = -1;
    H5D_chunk_index_t idx_type;
    const hsize_t shape[1] = { 8 };
    const hsize_t maxshape[1] = { 64 };
    const hsize_t chunk[1] = { 8 };
    const int buffer[8] = {0, 1, 2, 3, 4, 5, 6, 7};
    H5O_info_t info;

    TESTING("Fixed array chunk index header flush dependencies handled correctly");

    /* Skip this test if SWMR I/O is not supported for the VFD specified
     * by the environment variable.
     */
    if(!H5FD_supports_swmr_test(env_h5_driver)) {
        SKIPPED();
        HDputs("    Test skipped due to VFD not supporting SWMR I/O.");
        return SUCCEED;
    }

    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        FAIL_STACK_ERROR;
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    h5_fixname(FILENAME[20], fapl, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Create a chunked dataset with fixed dimensions */
    if((sid = H5Screate_simple(1, shape, maxshape)) < 0)
        FAIL_STACK_ERROR;
    if((tid = H5Tcopy(H5T_NATIVE_INT32)) < 0)
        FAIL_STACK_ERROR;
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    if(H5Pset_chunk(dcpl, 1, chunk) < 0)
        FAIL_STACK_ERROR;
    if((did = H5Dcreate2(fid, DSET_FARRAY_HDR_FD, tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT )) < 0)
        FAIL_STACK_ERROR;

    /* Verify the chunk index type */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0)
        FAIL_STACK_ERROR;
    if(idx_type != H5D_CHUNK_IDX_FARRAY)
        FAIL_PUTS_ERROR("should be using fixed array as index");

    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;
    if(H5Tclose(tid) < 0)
        FAIL_STACK_ERROR;
    if(H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;

    if(H5Fstart_swmr_write(fid) < 0)
        FAIL_STACK_ERROR;

    /* Write data to the dataset */
    if((did = H5Dopen2(fid, DSET_FARRAY_HDR_FD, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if((tid = H5Dget_type(did)) < 0)
        FAIL_STACK_ERROR;
    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer) < 0)
        FAIL_STACK_ERROR;
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if(H5Tclose(tid) < 0)
        FAIL_STACK_ERROR;

    /* The second call triggered a bug in the library (JIRA issue: SWMR-95) */
    if(H5Oget_info_by_name2(fid, DSET_FARRAY_HDR_FD, &info, H5O_INFO_BASIC, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;
    if(H5Oget_info_by_name2(fid, DSET_FARRAY_HDR_FD, &info, H5O_INFO_BASIC, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;

    if(H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    if(H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Fclose(fid);
        H5Dclose(did);
        H5Pclose(dcpl);
        H5Tclose(tid);
        H5Sclose(sid);
        H5Sclose(msid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_farray_hdr_fd() */


/*-------------------------------------------------------------------------
 * Function: test_bt2_hdr_fd
 *
 * Purpose: Tests that version 2 B-tree header flush dependencies
 *          are created and torn down correctly when used as a
 *          chunk index.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_bt2_hdr_fd(const char *env_h5_driver, hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t fid = -1;
    hid_t sid = -1;
    hid_t did = -1;
    hid_t tid = -1;
    hid_t dcpl = -1;
    hid_t msid = -1;
    H5D_chunk_index_t idx_type;
    const hsize_t shape[2] = { 8, 8 };
    const hsize_t maxshape[2] = { H5S_UNLIMITED, H5S_UNLIMITED };
    const hsize_t chunk[2] = { 8, 8 };
    const int buffer[8] = {0, 1, 2, 3, 4, 5, 6, 7};
    H5O_info_t info;

    TESTING("Version 2 B-tree chunk index header flush dependencies handled correctly");

    /* Skip this test if SWMR I/O is not supported for the VFD specified
     * by the environment variable.
     */
    if(!H5FD_supports_swmr_test(env_h5_driver)) {
        SKIPPED();
        HDputs("    Test skipped due to VFD not supporting SWMR I/O.");
        return SUCCEED;
    }

    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        FAIL_STACK_ERROR;
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    h5_fixname(FILENAME[21], fapl, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Create a chunked dataset with fixed dimensions */
    if((sid = H5Screate_simple(2, shape, maxshape)) < 0)
        FAIL_STACK_ERROR;
    if((tid = H5Tcopy(H5T_NATIVE_INT32)) < 0)
        FAIL_STACK_ERROR;
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    if(H5Pset_chunk(dcpl, 2, chunk) < 0)
        FAIL_STACK_ERROR;
    if((did = H5Dcreate2(fid, DSET_BT2_HDR_FD, tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT )) < 0)
        FAIL_STACK_ERROR;

    /* Verify the chunk index type */
    if(H5D__layout_idx_type_test(did, &idx_type) < 0)
        FAIL_STACK_ERROR;
    if(idx_type != H5D_CHUNK_IDX_BT2)
        FAIL_PUTS_ERROR("should be using fixed array as index");

    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;
    if(H5Tclose(tid) < 0)
        FAIL_STACK_ERROR;
    if(H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;

    if(H5Fstart_swmr_write(fid) < 0)
        FAIL_STACK_ERROR;

    /* Write data to the dataset */
    if((did = H5Dopen2(fid, DSET_BT2_HDR_FD, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if((tid = H5Dget_type(did)) < 0)
        FAIL_STACK_ERROR;
    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer) < 0)
        FAIL_STACK_ERROR;
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if(H5Tclose(tid) < 0)
        FAIL_STACK_ERROR;

    /* The second call triggered a bug in the library (JIRA issue: SWMR-95) */
    if(H5Oget_info_by_name2(fid, DSET_BT2_HDR_FD, &info, H5O_INFO_BASIC, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;
    if(H5Oget_info_by_name2(fid, DSET_BT2_HDR_FD, &info, H5O_INFO_BASIC, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;

    if(H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    if(H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Fclose(fid);
        H5Dclose(did);
        H5Pclose(dcpl);
        H5Tclose(tid);
        H5Sclose(sid);
        H5Sclose(msid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_bt2_hdr_fd() */


/*-------------------------------------------------------------------------
 * Function: test_storage_size
 *
 * Purpose:     Tests results from querying the storage size of a dataset,
 *              before/after extending the dimensions.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, April 11, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_storage_size(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;       /* File ID */
    hid_t       dcpl = -1, dcpl2 = -1;  /* Dataset creation property list IDs */
    hid_t       sid = -1;       /* Dataspace ID */
    hid_t       dsid = -1;      /* Dataset ID */
    hsize_t     dims[2], max_dims[2]; /* Dataset dimensions */
    hsize_t     new_dims[2];    /* New dataset dimensions */
    hsize_t     chunk_dims[2];  /* Chunk dimensions */
    int         wdata[STORAGE_SIZE_DIM1][STORAGE_SIZE_DIM2];
    hsize_t     ssize;          /* Dataset storage size */

    TESTING("querying storage size");

    h5_fixname(FILENAME[22], fapl, filename, sizeof filename);

    /* Create file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

    /* Set chunk size */
    chunk_dims[0] = STORAGE_SIZE_CHUNK_DIM1;
    chunk_dims[1] = STORAGE_SIZE_CHUNK_DIM2;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) FAIL_STACK_ERROR

    /* Copy the DCPL, and set it to early allocation */
    if((dcpl2 = H5Pcopy(dcpl)) < 0) FAIL_STACK_ERROR
    if(H5Pset_alloc_time(dcpl2, H5D_ALLOC_TIME_EARLY) < 0) FAIL_STACK_ERROR

    /* Create 2D dataspace, with max dims same as current dimensions */
    dims[0] = STORAGE_SIZE_DIM1;
    dims[1] = STORAGE_SIZE_DIM2;
    max_dims[0] = STORAGE_SIZE_DIM1;
    max_dims[1] = STORAGE_SIZE_DIM2;
    if((sid = H5Screate_simple(2, dims, max_dims)) < 0) FAIL_STACK_ERROR

    /* Create chunked dataset */
    if((dsid = H5Dcreate2(fid, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Initialize buffer to zeroes */
    HDmemset(wdata, 0, sizeof(wdata));

    /* write elements to dataset */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wdata) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 6 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Close dataset & dataspace */
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR

    /* Copy the dataset */
    if(H5Ocopy(fid, "dset", fid, "dset_copy", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open the copied dataset */
    if((dsid = H5Dopen2(fid, "dset_copy", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 6 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Close copied dataset */
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR


    /* Create 2D dataspace with max dims > current dims (but not unlimited) */
    dims[0] = STORAGE_SIZE_DIM1;
    dims[1] = STORAGE_SIZE_DIM2;
    max_dims[0] = STORAGE_SIZE_MAX_DIM1;
    max_dims[1] = STORAGE_SIZE_MAX_DIM2;
    if((sid = H5Screate_simple(2, dims, max_dims)) < 0) FAIL_STACK_ERROR

    /* Create chunked dataset */
    if((dsid = H5Dcreate2(fid, "dset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Initialize buffer to zeroes */
    HDmemset(wdata, 0, sizeof(wdata));

    /* write elements to dataset */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wdata) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 6 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Extend dataset's dimensions */
    new_dims[0] = STORAGE_SIZE_DIM1 * 2;
    new_dims[1] = STORAGE_SIZE_DIM2 * 2;
    if(H5Dset_extent(dsid, new_dims) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 6 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Close dataset & dataspace */
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR

    /* Copy the dataset */
    if(H5Ocopy(fid, "dset2", fid, "dset2_copy", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open the copied dataset */
    if((dsid = H5Dopen2(fid, "dset2_copy", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 6 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Close copied dataset */
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR


    /* Create 2D dataspace with max dims > current dims (but not unlimited) */
    dims[0] = STORAGE_SIZE_DIM1;
    dims[1] = STORAGE_SIZE_DIM2;
    max_dims[0] = STORAGE_SIZE_MAX_DIM1;
    max_dims[1] = STORAGE_SIZE_MAX_DIM2;
    if((sid = H5Screate_simple(2, dims, max_dims)) < 0) FAIL_STACK_ERROR

    /* Create chunked dataset, w/early allocation */
    if((dsid = H5Dcreate2(fid, "dset2a", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl2, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Initialize buffer to zeroes */
    HDmemset(wdata, 0, sizeof(wdata));

    /* write elements to dataset */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wdata) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 6 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Extend dataset's dimensions */
    new_dims[0] = STORAGE_SIZE_DIM1 * 2;
    new_dims[1] = STORAGE_SIZE_DIM2 * 2;
    if(H5Dset_extent(dsid, new_dims) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 15 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Close dataset & dataspace */
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR

    /* Copy the dataset */
    if(H5Ocopy(fid, "dset2a", fid, "dset2a_copy", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open the copied dataset */
    if((dsid = H5Dopen2(fid, "dset2a_copy", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 15 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Close copied dataset */
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR


    /* Create 2D dataspace with max dims > current dims (and 1 unlimited dim) */
    dims[0] = STORAGE_SIZE_DIM1;
    dims[1] = STORAGE_SIZE_DIM2;
    max_dims[0] = H5S_UNLIMITED;
    max_dims[1] = STORAGE_SIZE_MAX_DIM2;
    if((sid = H5Screate_simple(2, dims, max_dims)) < 0) FAIL_STACK_ERROR

    /* Create chunked dataset */
    if((dsid = H5Dcreate2(fid, "dset3", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Initialize buffer to zeroes */
    HDmemset(wdata, 0, sizeof(wdata));

    /* write elements to dataset */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wdata) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 6 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Extend dataset's dimensions */
    new_dims[0] = STORAGE_SIZE_DIM1 * 2;
    new_dims[1] = STORAGE_SIZE_DIM2 * 2;
    if(H5Dset_extent(dsid, new_dims) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 6 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Close dataset & dataspace */
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR

    /* Copy the dataset */
    if(H5Ocopy(fid, "dset3", fid, "dset3_copy", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open the copied dataset */
    if((dsid = H5Dopen2(fid, "dset3_copy", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 6 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Close copied dataset */
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR


    /* Create 2D dataspace with max dims > current dims (and 1 unlimited dim) */
    dims[0] = STORAGE_SIZE_DIM1;
    dims[1] = STORAGE_SIZE_DIM2;
    max_dims[0] = H5S_UNLIMITED;
    max_dims[1] = STORAGE_SIZE_MAX_DIM2;
    if((sid = H5Screate_simple(2, dims, max_dims)) < 0) FAIL_STACK_ERROR

    /* Create chunked dataset, w/early allocation */
    if((dsid = H5Dcreate2(fid, "dset3a", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl2, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Initialize buffer to zeroes */
    HDmemset(wdata, 0, sizeof(wdata));

    /* write elements to dataset */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wdata) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 6 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Extend dataset's dimensions */
    new_dims[0] = STORAGE_SIZE_DIM1 * 2;
    new_dims[1] = STORAGE_SIZE_DIM2 * 2;
    if(H5Dset_extent(dsid, new_dims) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 15 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Close dataset & dataspace */
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR

    /* Copy the dataset */
    if(H5Ocopy(fid, "dset3a", fid, "dset3a_copy", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open the copied dataset */
    if((dsid = H5Dopen2(fid, "dset3a_copy", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 15 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Close copied dataset */
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR


    /* Create 2D dataspace with max dims > current dims (and 2 unlimited dims) */
    dims[0] = STORAGE_SIZE_DIM1;
    dims[1] = STORAGE_SIZE_DIM2;
    max_dims[0] = H5S_UNLIMITED;
    max_dims[1] = H5S_UNLIMITED;
    if((sid = H5Screate_simple(2, dims, max_dims)) < 0) FAIL_STACK_ERROR

    /* Create chunked dataset */
    if((dsid = H5Dcreate2(fid, "dset4", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Initialize buffer to zeroes */
    HDmemset(wdata, 0, sizeof(wdata));

    /* write elements to dataset */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wdata) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 6 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Extend dataset's dimensions */
    new_dims[0] = STORAGE_SIZE_DIM1 * 2;
    new_dims[1] = STORAGE_SIZE_DIM2 * 2;
    if(H5Dset_extent(dsid, new_dims) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 6 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Close dataset & dataspace */
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR

    /* Copy the dataset */
    if(H5Ocopy(fid, "dset4", fid, "dset4_copy", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open the copied dataset */
    if((dsid = H5Dopen2(fid, "dset4_copy", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 6 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Close copied dataset */
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR


    /* Create 2D dataspace with max dims > current dims (and 2 unlimited dims) */
    dims[0] = STORAGE_SIZE_DIM1;
    dims[1] = STORAGE_SIZE_DIM2;
    max_dims[0] = H5S_UNLIMITED;
    max_dims[1] = H5S_UNLIMITED;
    if((sid = H5Screate_simple(2, dims, max_dims)) < 0) FAIL_STACK_ERROR

    /* Create chunked dataset, w/early allocation */
    if((dsid = H5Dcreate2(fid, "dset4a", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl2, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Initialize buffer to zeroes */
    HDmemset(wdata, 0, sizeof(wdata));

    /* write elements to dataset */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wdata) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 6 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Extend dataset's dimensions */
    new_dims[0] = STORAGE_SIZE_DIM1 * 2;
    new_dims[1] = STORAGE_SIZE_DIM2 * 2;
    if(H5Dset_extent(dsid, new_dims) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 15 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Close dataset & dataspace */
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR

    /* Copy the dataset */
    if(H5Ocopy(fid, "dset4a", fid, "dset4a_copy", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open the copied dataset */
    if((dsid = H5Dopen2(fid, "dset4a_copy", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Get the storage size */
    if(0 == (ssize = H5Dget_storage_size(dsid))) FAIL_STACK_ERROR
    if((sizeof(int) * 15 * STORAGE_SIZE_CHUNK_DIM1 * STORAGE_SIZE_CHUNK_DIM2) != ssize) TEST_ERROR

    /* Close copied dataset */
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR


    /* Close rest */
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Pclose(dcpl2);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_storage_size() */


/*-------------------------------------------------------------------------
 * Function:    test_power2up
 *
 * Purpose:     Tests that the H5VM_power2up(n) function does not result in an
 *              infinite loop when input n exceeds 2^63. (HDFFV-10217)
 *              H5VM_power2up() is used to calculate the next power of 2 for
 *              a dataset's scaled dimension sizes.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Vailin Choi; June 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_power2up(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;       /* File ID */
    hid_t       dcpl = -1;      /* Dataset creation property list */
    hid_t       sid = -1;       /* Dataspace ID */
    hid_t       did = -1;       /* Dataset ID */
    hsize_t     dims[2];        /* Dataset dimension sizes */
    hsize_t     max_dims[2];    /* Maximum dimension sizes */
    hsize_t     chunk_dims[2];  /* Chunk dimensions */
    hsize_t     ext_dims[2];    /* Extended dimension sizes */
    herr_t      status;         /* Error status */

    TESTING("the next power of 2");

    h5_fixname(FILENAME[24], fapl, filename, sizeof filename);

    /* Create file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Set dims[1] to ((2^63) -1) */
    dims[0] = 0;
    dims[1] = ((hsize_t)1 << ((sizeof(hsize_t) * CHAR_BIT) -1)) - 1;
    max_dims[0] = max_dims[1] = H5S_UNLIMITED;
    sid =  H5Screate_simple(2, dims, max_dims);

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
       TEST_ERROR

    /* Set chunk size */
    chunk_dims[0] = chunk_dims[1] = 1;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
       TEST_ERROR

    /* Create chunked dataset */
    if((did = H5Dcreate2(fid, "dset", H5T_NATIVE_INT64, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
       TEST_ERROR

    ext_dims[0] = 1;
    ext_dims[1] = dims[1] + 5;

    /* Extend to (2^63)+ */
    H5E_BEGIN_TRY {
        status = H5Dset_extent(did, ext_dims);
    } H5E_END_TRY;
    if(status >= 0)
       TEST_ERROR

    /* Closing */
    if(H5Dclose(did) < 0)
       TEST_ERROR
    if(H5Sclose(sid) < 0)
       TEST_ERROR
    if(H5Pclose(dcpl) < 0)
       TEST_ERROR
    if(H5Fclose(fid) < 0)
       TEST_ERROR

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_power2up() */


/*-------------------------------------------------------------------------
 * Function:    test_scatter
 *
 * Purpose:     Tests H5Dscatter with a variety of different selections
 *              and source buffer sizes.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Neil Fortner
 *              Wednesday, January 16, 2013
 *
 *-------------------------------------------------------------------------
 */
typedef struct scatter_info_t {
    int *src_buf;   /* Source data buffer */
    size_t block;   /* Maximum number of elements to return to H5Dscatter() */
    size_t size;    /* Remaining number of elements to return */
} scatter_info_t;

#define TEST_SCATTER_CHECK_ARR(ARR, EXP) \
    for(i=0; i<(int)(sizeof(ARR)/sizeof(ARR[0])); i++) \
        for(j=0; j<(int)(sizeof(ARR[0])/sizeof(ARR[0][0])); j++) \
            for(k=0; k<(int)(sizeof(ARR[0][0])/sizeof(ARR[0][0][0])); k++) \
                if(ARR[i][j][k] != EXP[i][j][k]) { \
                    H5_FAILED(); AT(); \
                    HDprintf("  " #ARR "[%d][%d][%d] == %d, " #EXP "[%d][%d][%d] == %d\n", i, j, k, ARR[i][j][k], i, j, k, EXP[i][j][k]); \
                    goto error; \
                }

static herr_t
scatter_cb(void **src_buf/*out*/, size_t *src_buf_bytes_used/*out*/,
    void *_scatter_info)
{
    scatter_info_t *scatter_info = (scatter_info_t *)_scatter_info;
    size_t nelmts;      /* Number of elements to return in src_buf */

    /* Calculate number of elements */
    nelmts = MIN(scatter_info->block, scatter_info->size);
    HDassert(nelmts > 0);

    /* Set output variables */
    *src_buf = (void *)scatter_info->src_buf;
    *src_buf_bytes_used = nelmts * sizeof(scatter_info->src_buf[0]);

    /* Update scatter_info */
    scatter_info->src_buf += nelmts;
    scatter_info->size -= nelmts;

    return SUCCEED;
}

static herr_t
test_scatter(void)
{
    hid_t       sid = -1;       /* Dataspace ID */
    hsize_t     dim[3] = {8, 5, 8}; /* Dataspace dimensions */
    hsize_t     start[3] = {0, 0, 0};
    hsize_t     stride[3] = {0, 0, 0};
    hsize_t     count[3] = {0, 0, 0};
    hsize_t     block[3] = {0, 0, 0};
    hsize_t     start2[3] = {0, 0, 0};
    hsize_t     count2[3] = {0, 0, 0};
    hsize_t     point[4][3] = {{2, 3, 2}, {3, 0, 2}, {7, 2, 0}, {0, 1, 5}};
    size_t      src_buf_size;
    int         src_buf[36];    /* Source data buffer */
    int         dst_buf[8][5][8]; /* Destination data buffer */
    int         expect_dst_buf[8][5][8]; /* Expected destination data buffer */
    scatter_info_t scatter_info; /* Operator data for callback */
    int         i, j, k, src_i; /* Local index variables */

    TESTING("H5Dscatter()");

    /* Create dataspace */
    if((sid = H5Screate_simple(3, dim, NULL)) < 0) TEST_ERROR

    /* Initialize src_buf */
    for(i=0; i<(int)(sizeof(src_buf)/sizeof(src_buf[0])); i++)
        src_buf[i] = i + 1;


    /*
     * Test 1: Simple case
     */
    /* Select hyperslab */
    count[0] = 1;
    count[1] = 1;
    count[2] = 8;
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR

    /* Initialize dst_buf and expect_dst_buf */
    (void)HDmemset(expect_dst_buf, 0, sizeof(expect_dst_buf));
    for(i=0; i<8; i++)
        expect_dst_buf[0][0][i] = src_buf[i];

    /* Loop over buffer sizes */
    for(src_buf_size=1; src_buf_size<=9; src_buf_size++) {
        /* Reset dst_buf */
        (void)HDmemset(dst_buf, 0, sizeof(dst_buf));

        /* Set up scatter info */
        scatter_info.src_buf = src_buf;
        scatter_info.block = src_buf_size;
        scatter_info.size = 8;

        /* Scatter data */
        if(H5Dscatter((H5D_scatter_func_t)scatter_cb, &scatter_info, H5T_NATIVE_INT, sid, dst_buf) < 0)
            TEST_ERROR

        /* Verify data */
        TEST_SCATTER_CHECK_ARR(dst_buf, expect_dst_buf)
    } /* end for */


    /*
     * Test 2: Single block in dataset
     */
    /* Select hyperslab */
    start[0] = 3;
    start[1] = 2;
    start[2] = 4;
    count[0] = 2;
    count[1] = 3;
    count[2] = 2;
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR

    /* Initialize expect_dst_buf */
    (void)HDmemset(expect_dst_buf, 0, sizeof(expect_dst_buf));
    src_i = 0;
    for(i=3; i<5; i++)
        for(j=2; j<5; j++)
            for(k=4; k<6; k++)
                expect_dst_buf[i][j][k] = src_buf[src_i++];

    /* Loop over buffer sizes */
    for(src_buf_size=1; src_buf_size<=13; src_buf_size++) {
        /* Reset dst_buf */
        (void)HDmemset(dst_buf, 0, sizeof(dst_buf));

        /* Set up scatter info */
        scatter_info.src_buf = src_buf;
        scatter_info.block = src_buf_size;
        scatter_info.size = 12;

        /* Scatter data */
        if(H5Dscatter((H5D_scatter_func_t)scatter_cb, &scatter_info, H5T_NATIVE_INT, sid, dst_buf) < 0)
            TEST_ERROR

        /* Verify data */
        TEST_SCATTER_CHECK_ARR(dst_buf, expect_dst_buf)
    } /* end for */


    /*
     * Test 3: Multiple blocks
     */
    /* Select hyperslab */
    start[0] = 1;
    start[1] = 1;
    start[2] = 1;
    stride[0] = 3;
    stride[1] = 4;
    stride[2] = 5;
    count[0] = 3;
    count[1] = 1;
    count[2] = 2;
    block[0] = 1;
    block[1] = 3;
    block[2] = 2;
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride ,count, block) < 0)
        TEST_ERROR

    /* Initialize expect_dst_buf */
    /* Iterate over block containing selection, checking if each element is in
     * selection.  Note that the algorithm used here (if statement) would not
     * work for overlapping hyperslabs. */
    (void)HDmemset(expect_dst_buf, 0, sizeof(expect_dst_buf));
    src_i = 0;
    for(i=1; i<8; i++)
        for(j=1; j<4; j++)
            for(k=1; k<8; k++)
                if((hsize_t)i >= start[0]
                        && ((hsize_t)i - start[0]) % stride[0] < block[0]
                        && ((hsize_t)i - start[0]) / stride[0] < count[0]
                        && (hsize_t)j >= start[1]
                        && ((hsize_t)j - start[1]) % stride[1] < block[1]
                        && ((hsize_t)j - start[1]) / stride[1] < count[1]
                        && (hsize_t)k >= start[2]
                        && ((hsize_t)k - start[2]) % stride[2] < block[2]
                        && ((hsize_t)k - start[2]) / stride[2] < count[2])
                    expect_dst_buf[i][j][k] = src_buf[src_i++];

    /* Loop over buffer sizes */
    for(src_buf_size=1; src_buf_size<=37; src_buf_size++) {
        /* Reset dst_buf */
        (void)HDmemset(dst_buf, 0, sizeof(dst_buf));

        /* Set up scatter info */
        scatter_info.src_buf = src_buf;
        scatter_info.block = src_buf_size;
        scatter_info.size = 36;

        /* Scatter data */
        if(H5Dscatter((H5D_scatter_func_t)scatter_cb, &scatter_info, H5T_NATIVE_INT, sid, dst_buf) < 0)
            TEST_ERROR

        /* Verify data */
        TEST_SCATTER_CHECK_ARR(dst_buf, expect_dst_buf)
    } /* end for */


    /*
     * Test 4: Compound selection
     */
    /* Select hyperslabs */
    start[0] = 2;
    start[1] = 1;
    start[2] = 1;
    count[0] = 2;
    count[1] = 3;
    count[2] = 2;
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR
    start2[0] = 1;
    start2[1] = 2;
    start2[2] = 2;
    count2[0] = 3;
    count2[1] = 2;
    count2[2] = 2;
    if(H5Sselect_hyperslab(sid, H5S_SELECT_XOR, start2, NULL ,count2, NULL) < 0)
        TEST_ERROR

    /* Initialize expect_dst_buf */
    /* Iterate over block containing selection, checking if each element is in
     * selection. */
    (void)HDmemset(expect_dst_buf, 0, sizeof(expect_dst_buf));
    src_i = 0;
    for(i=1; i<4; i++)
        for(j=1; j<4; j++)
            for(k=1; k<4; k++)
                if(!(((hsize_t)i >= start[0] && (hsize_t)i < start[0] + count[0])
                        && ((hsize_t)j >= start[1] && (hsize_t)j < start[1] + count[1])
                        && ((hsize_t)k >= start[2] && (hsize_t)k < start[2] + count[2]))
                        != !(((hsize_t)i >= start2[0] && (hsize_t)i < start2[0] + count2[0])
                        && ((hsize_t)j >= start2[1] && (hsize_t)j < start2[1] + count2[1])
                        && ((hsize_t)k >= start2[2] && (hsize_t)k < start2[2] + count2[2])))
                    expect_dst_buf[i][j][k] = src_buf[src_i++];

    /* Loop over buffer sizes */
    for(src_buf_size=1; src_buf_size<=17; src_buf_size++) {
        /* Reset dst_buf */
        (void)HDmemset(dst_buf, 0, sizeof(dst_buf));

        /* Set up scatter info */
        scatter_info.src_buf = src_buf;
        scatter_info.block = src_buf_size;
        scatter_info.size = 16;

        /* Scatter data */
        if(H5Dscatter((H5D_scatter_func_t)scatter_cb, &scatter_info, H5T_NATIVE_INT, sid, dst_buf) < 0)
            TEST_ERROR

        /* Verify data */
        TEST_SCATTER_CHECK_ARR(dst_buf, expect_dst_buf)
    } /* end for */


    /*
     * Test 5: Point selection
     */
    /* Select hyperslabs */
    if(H5Sselect_elements(sid, H5S_SELECT_SET, sizeof(point) / sizeof(point[0]), (hsize_t *)point) < 0)
        TEST_ERROR

    /* Initialize expect_dst_buf */
    /* Iterate over block containing selection, checking if each element is in
     * selection. */
    (void)HDmemset(expect_dst_buf, 0, sizeof(expect_dst_buf));
    for(i=0; i<(int)(sizeof(point) / sizeof(point[0])); i++)
        expect_dst_buf[point[i][0]][point[i][1]][point[i][2]]
                = src_buf[i];

    /* Loop over buffer sizes */
    for(src_buf_size=1; src_buf_size<=5; src_buf_size++) {
        /* Reset dst_buf */
        (void)HDmemset(dst_buf, 0, sizeof(dst_buf));

        /* Set up scatter info */
        scatter_info.src_buf = src_buf;
        scatter_info.block = src_buf_size;
        scatter_info.size = 4;

        /* Scatter data */
        if(H5Dscatter((H5D_scatter_func_t)scatter_cb, &scatter_info, H5T_NATIVE_INT, sid, dst_buf) < 0)
            TEST_ERROR

        /* Verify data */
        TEST_SCATTER_CHECK_ARR(dst_buf, expect_dst_buf)
    } /* end for */


    /* Close everything */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_scatter() */


/*-------------------------------------------------------------------------
 * Function:    test_gather
 *
 * Purpose:     Tests H5Dgather with a variety of different selections and
 *              destination buffer sizes.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Neil Fortner
 *              Wednesday, January 16, 2013
 *
 *-------------------------------------------------------------------------
 */
typedef struct gather_info_t {
    int *expect_dst_buf; /* Expected destination data buffer */
    size_t max_nelmts;  /* Maximum number of elements passed to callback */
    hbool_t last_call;  /* Whether this should be the last time the callback is called */
} gather_info_t;

static herr_t
gather_cb(const void *dst_buf, size_t dst_buf_bytes_used,
    void *_gather_info)
{
    gather_info_t *gather_info = (gather_info_t *)_gather_info;
    size_t nelmts;      /* Number of elements in src_buf */
    int i;              /* Local index variable */

    HDassert(dst_buf_bytes_used > 0);

    /* Calculate number of elements */
    nelmts = dst_buf_bytes_used / sizeof(gather_info->expect_dst_buf[0]);

    /* Make sure the number of bytes is a multiple of the number of elements */
    if(nelmts * sizeof(gather_info->expect_dst_buf[0]) != dst_buf_bytes_used)
        TEST_ERROR

    /* Make sure we weren't passed more data than we requested to be passed at
     * once */
    if(nelmts > gather_info->max_nelmts)
        TEST_ERROR

    /* If we were passed less data than requested, make sure this is the last
     * time the callback was called */
    if(gather_info->last_call)
        TEST_ERROR
    if(nelmts < gather_info->max_nelmts)
        gather_info->last_call = TRUE;

    /* Compare data and expected data */
    for(i=0; i<(int)nelmts; i++)
        if(((const int *)dst_buf)[i] != *((gather_info->expect_dst_buf)++))
            TEST_ERROR

    return SUCCEED;

error:
    return FAIL;
} /* end gather_cb() */

static herr_t
test_gather(void)
{
    hid_t       sid = -1;       /* Dataspace ID */
    hsize_t     dim[3] = {8, 5, 8}; /* Dataspace dimensions */
    hsize_t     start[3] = {0, 0, 0};
    hsize_t     stride[3] = {0, 0, 0};
    hsize_t     count[3] = {0, 0, 0};
    hsize_t     block[3] = {0, 0, 0};
    hsize_t     start2[3] = {0, 0, 0};
    hsize_t     count2[3] = {0, 0, 0};
    hsize_t     point[4][3] = {{2, 3, 2}, {3, 0, 2}, {7, 2, 0}, {0, 1, 5}};
    size_t      dst_buf_size;
    int         src_buf[8][5][8]; /* Source data buffer */
    int         dst_buf[36];    /* Destination data buffer */
    int         expect_dst_buf[36]; /* Expected destination data buffer */
    gather_info_t gather_info;  /* Operator data for callback */
    int         i, j, k, dst_i; /* Local index variables */

    TESTING("H5Dgather()");

    /* Create dataspace */
    if((sid = H5Screate_simple(3, dim, NULL)) < 0) TEST_ERROR

    /* Initialize src_buf */
    for(i=0; i<(int)(sizeof(src_buf)/sizeof(src_buf[0])); i++)
        for(j=0; j<(int)(sizeof(src_buf[0])/sizeof(src_buf[0][0])); j++)
            for(k=0; k<(int)(sizeof(src_buf[0][0])/sizeof(src_buf[0][0][0])); k++)
                src_buf[i][j][k] = 1 + k
                        + (int)(sizeof(src_buf[0][0]) / sizeof(src_buf[0][0][0])) * j
                        + (int)(sizeof(src_buf[0]) / sizeof(src_buf[0][0][0])) * i;


    /*
     * Test 1: Simple case
     */
    /* Select hyperslab */
    count[0] = 1;
    count[1] = 1;
    count[2] = 8;
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR

    /* Initialize expect_dst_buf */
    (void)HDmemset(expect_dst_buf, 0, sizeof(expect_dst_buf));
    for(i=0; i<8; i++)
        expect_dst_buf[i] = src_buf[0][0][i];

    /* Loop over buffer sizes */
    for(dst_buf_size=1; dst_buf_size<=9; dst_buf_size++) {
        /* Reset dst_buf */
        (void)HDmemset(dst_buf, 0, sizeof(dst_buf));

        /* Initialize gather_info */
        gather_info.expect_dst_buf = expect_dst_buf;
        gather_info.max_nelmts = dst_buf_size;
        gather_info.last_call = FALSE;

        /* Gather data */
        if(H5Dgather(sid, src_buf, H5T_NATIVE_INT, dst_buf_size * sizeof(dst_buf[0]), dst_buf, gather_cb, &gather_info) < 0)
            TEST_ERROR

        /* Verify that all data has been gathered (and verified) */
        if(gather_info.expect_dst_buf - expect_dst_buf != 8) TEST_ERROR
    } /* end for */

    /* Test without a callback */
    /* Loop over buffer sizes */
    for(dst_buf_size=8; dst_buf_size<=9; dst_buf_size++) {
        /* Reset dst_buf */
        (void)HDmemset(dst_buf, 0, sizeof(dst_buf));

        /* Gather data */
        if(H5Dgather(sid, src_buf, H5T_NATIVE_INT, dst_buf_size * sizeof(dst_buf[0]), dst_buf, NULL, NULL) < 0)
            TEST_ERROR

        /* Verify data */
        for(i=0; i<(int)(sizeof(dst_buf)/sizeof(dst_buf[0])); i++)
            if(dst_buf[i] != expect_dst_buf[i])
                TEST_ERROR
    } /* end for */

    /* Test with a dst_buf_size that is not a multiple of the datatype size */
    /* Reset dst_buf */
    dst_buf_size = 7;
    (void)HDmemset(dst_buf, 0, sizeof(dst_buf));

    /* Initialize gather_info */
    gather_info.expect_dst_buf = expect_dst_buf;
    gather_info.max_nelmts = dst_buf_size - 1;
    gather_info.last_call = FALSE;

    /* Gather data */
    if(H5Dgather(sid, src_buf, H5T_NATIVE_INT, dst_buf_size * sizeof(dst_buf[0]) - 1, dst_buf, gather_cb, &gather_info) < 0)
        TEST_ERROR

    /* Verify that all data has been gathered (and verified) */
    if(gather_info.expect_dst_buf - expect_dst_buf != 8) TEST_ERROR


    /*
     * Test 2: Single block in dataset
     */
    /* Select hyperslab */
    start[0] = 3;
    start[1] = 2;
    start[2] = 4;
    count[0] = 2;
    count[1] = 3;
    count[2] = 2;
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR

    /* Initialize expect_dst_buf */
    (void)HDmemset(expect_dst_buf, 0, sizeof(expect_dst_buf));
    dst_i = 0;
    for(i=3; i<5; i++)
        for(j=2; j<5; j++)
            for(k=4; k<6; k++)
                expect_dst_buf[dst_i++] = src_buf[i][j][k];

    /* Loop over buffer sizes */
    for(dst_buf_size=1; dst_buf_size<=13; dst_buf_size++) {
        /* Reset dst_buf */
        (void)HDmemset(dst_buf, 0, sizeof(dst_buf));

        /* Initialize gather_info */
        gather_info.expect_dst_buf = expect_dst_buf;
        gather_info.max_nelmts = dst_buf_size;
        gather_info.last_call = FALSE;

        /* Gather data */
        if(H5Dgather(sid, src_buf, H5T_NATIVE_INT, dst_buf_size * sizeof(dst_buf[0]), dst_buf, gather_cb, &gather_info) < 0)
            TEST_ERROR

        /* Verify that all data has been gathered (and verified) */
        if(gather_info.expect_dst_buf - expect_dst_buf != 12) TEST_ERROR
    } /* end for */


    /*
     * Test 3: Multiple blocks
     */
    /* Select hyperslab */
    start[0] = 1;
    start[1] = 1;
    start[2] = 1;
    stride[0] = 3;
    stride[1] = 4;
    stride[2] = 5;
    count[0] = 3;
    count[1] = 1;
    count[2] = 2;
    block[0] = 1;
    block[1] = 3;
    block[2] = 2;
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride ,count, block) < 0)
        TEST_ERROR

    /* Initialize expect_dst_buf */
    /* Iterate over block containing selection, checking if each element is in
     * selection.  Note that the algorithm used here (if statement) would not
     * work for overlapping hyperslabs. */
    (void)HDmemset(expect_dst_buf, 0, sizeof(expect_dst_buf));
    dst_i = 0;
    for(i=1; i<8; i++)
        for(j=1; j<4; j++)
            for(k=1; k<8; k++)
                if((hsize_t)i >= start[0]
                        && ((hsize_t)i - start[0]) % stride[0] < block[0]
                        && ((hsize_t)i - start[0]) / stride[0] < count[0]
                        && (hsize_t)j >= start[1]
                        && ((hsize_t)j - start[1]) % stride[1] < block[1]
                        && ((hsize_t)j - start[1]) / stride[1] < count[1]
                        && (hsize_t)k >= start[2]
                        && ((hsize_t)k - start[2]) % stride[2] < block[2]
                        && ((hsize_t)k - start[2]) / stride[2] < count[2])
                    expect_dst_buf[dst_i++] = src_buf[i][j][k];

    /* Loop over buffer sizes */
    for(dst_buf_size=1; dst_buf_size<=37; dst_buf_size++) {
        /* Reset dst_buf */
        (void)HDmemset(dst_buf, 0, sizeof(dst_buf));

        /* Initialize gather_info */
        gather_info.expect_dst_buf = expect_dst_buf;
        gather_info.max_nelmts = dst_buf_size;
        gather_info.last_call = FALSE;

        /* Gather data */
        if(H5Dgather(sid, src_buf, H5T_NATIVE_INT, dst_buf_size * sizeof(dst_buf[0]), dst_buf, gather_cb, &gather_info) < 0)
            TEST_ERROR

        /* Verify that all data has been gathered (and verified) */
        if(gather_info.expect_dst_buf - expect_dst_buf != 36) TEST_ERROR
    } /* end for */


    /*
     * Test 4: Compound selection
     */
    /* Select hyperslabs */
    start[0] = 2;
    start[1] = 1;
    start[2] = 1;
    count[0] = 2;
    count[1] = 3;
    count[2] = 2;
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR
    start2[0] = 1;
    start2[1] = 2;
    start2[2] = 2;
    count2[0] = 3;
    count2[1] = 2;
    count2[2] = 2;
    if(H5Sselect_hyperslab(sid, H5S_SELECT_XOR, start2, NULL ,count2, NULL) < 0)
        TEST_ERROR

    /* Initialize expect_dst_buf */
    /* Iterate over block containing selection, checking if each element is in
     * selection. */
    (void)HDmemset(expect_dst_buf, 0, sizeof(expect_dst_buf));
    dst_i = 0;
    for(i=1; i<4; i++)
        for(j=1; j<4; j++)
            for(k=1; k<4; k++)
                if(!(((hsize_t)i >= start[0] && (hsize_t)i < start[0] + count[0])
                        && ((hsize_t)j >= start[1] && (hsize_t)j < start[1] + count[1])
                        && ((hsize_t)k >= start[2] && (hsize_t)k < start[2] + count[2]))
                        != !(((hsize_t)i >= start2[0] && (hsize_t)i < start2[0] + count2[0])
                        && ((hsize_t)j >= start2[1] && (hsize_t)j < start2[1] + count2[1])
                        && ((hsize_t)k >= start2[2] && (hsize_t)k < start2[2] + count2[2])))
                    expect_dst_buf[dst_i++] = src_buf[i][j][k];

    /* Loop over buffer sizes */
    for(dst_buf_size=1; dst_buf_size<=17; dst_buf_size++) {
        /* Reset dst_buf */
        (void)HDmemset(dst_buf, 0, sizeof(dst_buf));

        /* Initialize gather_info */
        gather_info.expect_dst_buf = expect_dst_buf;
        gather_info.max_nelmts = dst_buf_size;
        gather_info.last_call = FALSE;

        /* Gather data */
        if(H5Dgather(sid, src_buf, H5T_NATIVE_INT, dst_buf_size * sizeof(dst_buf[0]), dst_buf, gather_cb, &gather_info) < 0)
            TEST_ERROR

        /* Verify that all data has been gathered (and verified) */
        if(gather_info.expect_dst_buf - expect_dst_buf != 16) TEST_ERROR
    } /* end for */


    /*
     * Test 5: Point selection
     */
    /* Select hyperslabs */
    if(H5Sselect_elements(sid, H5S_SELECT_SET, sizeof(point) / sizeof(point[0]), (hsize_t *)point) < 0)
        TEST_ERROR

    /* Initialize expect_dst_buf */
    /* Iterate over block containing selection, checking if each element is in
     * selection. */
    (void)HDmemset(expect_dst_buf, 0, sizeof(expect_dst_buf));
    for(i=0; i<(int)(sizeof(point) / sizeof(point[0])); i++)
        expect_dst_buf[i] = src_buf[point[i][0]][point[i][1]][point[i][2]];

    /* Loop over buffer sizes */
    for(dst_buf_size=1; dst_buf_size<=5; dst_buf_size++) {
        /* Reset dst_buf */
        (void)HDmemset(dst_buf, 0, sizeof(dst_buf));

        /* Initialize gather_info */
        gather_info.expect_dst_buf = expect_dst_buf;
        gather_info.max_nelmts = dst_buf_size;
        gather_info.last_call = FALSE;

        /* Gather data */
        if(H5Dgather(sid, src_buf, H5T_NATIVE_INT, dst_buf_size * sizeof(dst_buf[0]), dst_buf, gather_cb, &gather_info) < 0)
            TEST_ERROR

        /* Verify that all data has been gathered (and verified) */
        if(gather_info.expect_dst_buf - expect_dst_buf != 4) TEST_ERROR
    } /* end for */


    /* Close everything */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_gather() */


/*-------------------------------------------------------------------------
 * Function:    test_scatter_error
 *
 * Purpose:     Tests H5Dscatter with a variety of different conditions
 *              that should cause errors.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Neil Fortner
 *              Monday, February 4, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
scatter_error_cb_fail(void **src_buf/*out*/, size_t *src_buf_bytes_used/*out*/,
    void *_scatter_info)
{
    scatter_info_t *scatter_info = (scatter_info_t *)_scatter_info;
    size_t nelmts;      /* Number of elements to return in src_buf */

    /* Calculate number of elements */
    nelmts = MIN(scatter_info->block, scatter_info->size);
    HDassert(nelmts > 0);

    /* Set output variables */
    *src_buf = (void *)scatter_info->src_buf;
    *src_buf_bytes_used = nelmts * sizeof(scatter_info->src_buf[0]);

    return FAIL;
} /* end scatter_error_cb_fail() */

static herr_t
scatter_error_cb_null(void **src_buf/*out*/, size_t *src_buf_bytes_used/*out*/,
    void *_scatter_info)
{
    scatter_info_t *scatter_info = (scatter_info_t *)_scatter_info;
    size_t nelmts;      /* Number of elements to return in src_buf */

    /* Calculate number of elements */
    nelmts = MIN(scatter_info->block, scatter_info->size);
    HDassert(nelmts > 0);

    /* Set output variables */
    *src_buf = NULL;
    *src_buf_bytes_used = nelmts * sizeof(scatter_info->src_buf[0]);

    return SUCCEED;
} /* end scatter_error_cb_null() */

static herr_t
scatter_error_cb_unalign(void **src_buf/*out*/, size_t *src_buf_bytes_used/*out*/,
    void *_src_buf_bytes_used)
{
    /* Set output variables */
    *src_buf = _src_buf_bytes_used;
    *src_buf_bytes_used = *(size_t *)_src_buf_bytes_used;

    return SUCCEED;
} /* endscatter_error_cb_unalign() */

static herr_t
test_scatter_error(void)
{
    hid_t       sid = -1;       /* Dataspace ID */
    hsize_t     dim[1] = {10};  /* Dataspace dimensions */
    hsize_t     start[3] = {2};
    hsize_t     count[3] = {6};
    int         src_buf[7];     /* Source data buffer */
    int         dst_buf[10];    /* Destination data buffer */
    scatter_info_t scatter_info; /* Operator data for callback */
    size_t      cb_unalign_nbytes; /* Number of bytes to return for unaligned test */
    herr_t      ret;            /* Return value */
    int         i;              /* Local index variable */

    TESTING("H5Dscatter() error conditions");

    /* Create dataspace */
    if((sid = H5Screate_simple(1, dim, NULL)) < 0) TEST_ERROR

    /* Initialize src_buf */
    for(i=0; i<(int)(sizeof(src_buf)/sizeof(src_buf[0])); i++)
        src_buf[i] = i + 1;

    /* Select hyperslab */
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR

    /* Verify that base configuration passes */
    scatter_info.src_buf = src_buf;
    scatter_info.block = sizeof(src_buf)/sizeof(src_buf[0]);
    scatter_info.size = 6;
    if(H5Dscatter((H5D_scatter_func_t)scatter_cb, &scatter_info, H5T_NATIVE_INT, sid, dst_buf) < 0)
        TEST_ERROR


    /*
     * Test invalid parameters
     */
    scatter_info.src_buf = src_buf;
    scatter_info.size = 6;
    H5E_BEGIN_TRY {
        ret = H5Dscatter(NULL, NULL, H5T_NATIVE_INT, sid, dst_buf);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR

    scatter_info.src_buf = src_buf;
    scatter_info.size = 6;
    H5E_BEGIN_TRY {
        ret = H5Dscatter((H5D_scatter_func_t)scatter_cb, &scatter_info, sid, sid, dst_buf);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR

    scatter_info.src_buf = src_buf;
    scatter_info.size = 6;
    H5E_BEGIN_TRY {
        ret = H5Dscatter((H5D_scatter_func_t)scatter_cb, &scatter_info, H5T_NATIVE_INT, H5T_NATIVE_INT, dst_buf);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR

    scatter_info.src_buf = src_buf;
    scatter_info.size = 6;
    H5E_BEGIN_TRY {
        ret = H5Dscatter((H5D_scatter_func_t)scatter_cb, &scatter_info, H5T_NATIVE_INT, sid, NULL);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR


    /*
     * Test returning too many elements in callback
     */
    scatter_info.src_buf = src_buf;
    scatter_info.size = 7;
    H5E_BEGIN_TRY {
        ret = H5Dscatter((H5D_scatter_func_t)scatter_cb, &scatter_info, H5T_NATIVE_INT, sid, dst_buf);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR


    /*
     * Test callback returns failure
     */
    scatter_info.src_buf = src_buf;
    scatter_info.size = 6;
    H5E_BEGIN_TRY {
        ret = H5Dscatter((H5D_scatter_func_t)scatter_error_cb_fail, &scatter_info, H5T_NATIVE_INT, sid, dst_buf);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR


    /*
     * Test callback returns NULL buffer
     */
    scatter_info.src_buf = src_buf;
    scatter_info.size = 6;
    H5E_BEGIN_TRY {
        ret = H5Dscatter((H5D_scatter_func_t)scatter_error_cb_null, &scatter_info, H5T_NATIVE_INT, sid, dst_buf);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR


    /*
     * Test callback returns 0 for src_buf_bytes_used
     */
    cb_unalign_nbytes = 0;
    H5E_BEGIN_TRY {
        ret = H5Dscatter((H5D_scatter_func_t)scatter_error_cb_unalign, &cb_unalign_nbytes, H5T_NATIVE_INT, sid, dst_buf);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR


    /*
     * Test callback returns src_buf_bytes_used that is not a multiple of
     * datatype size
     */
    cb_unalign_nbytes = sizeof(src_buf[0]) - 1;
    H5E_BEGIN_TRY {
        ret = H5Dscatter((H5D_scatter_func_t)scatter_error_cb_unalign, &cb_unalign_nbytes, H5T_NATIVE_INT, sid, dst_buf);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR

    cb_unalign_nbytes = sizeof(src_buf[0]) + 1;
    H5E_BEGIN_TRY {
        ret = H5Dscatter((H5D_scatter_func_t)scatter_error_cb_unalign, &cb_unalign_nbytes, H5T_NATIVE_INT, sid, dst_buf);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR


    /* Close everything */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_scatter_error() */


/*-------------------------------------------------------------------------
 * Function:    test_gather_error
 *
 * Purpose:     Tests H5Dgather with a variety of different conditions
 *              that should cause errors.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Neil Fortner
 *              Monday, February 4, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
gather_error_cb_fail(const void H5_ATTR_UNUSED *dst_buf,
    size_t H5_ATTR_UNUSED dst_buf_bytes_used, void H5_ATTR_UNUSED *op_data)
{
    return FAIL;
} /* end gather_error_cb_fail() */

static herr_t
test_gather_error(void)
{
    hid_t       sid = -1;       /* Dataspace ID */
    hsize_t     dim[1] = {10};  /* Dataspace dimensions */
    hsize_t     start[1] = {2};
    hsize_t     count[1] = {6};
    int         src_buf[10];    /* Source data buffer */
    int         dst_buf[6];     /* Destination data buffer */
    int         expect_dst_buf[6]; /* Expected destination data buffer */
    gather_info_t gather_info;  /* Operator data for callback */
    herr_t      ret;            /* Return value */
    int         i;              /* Local index variable */

    TESTING("H5Dgather() error conditions");

    /* Create dataspace */
    if((sid = H5Screate_simple(1, dim, NULL)) < 0) TEST_ERROR

    /* Initialize src_buf */
    for(i=0; i<(int)(sizeof(src_buf)/sizeof(src_buf[0])); i++)
        src_buf[i] = 1 + i;

    /* Select hyperslab */
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR

    /* Initialize expect_dst_buf */
    (void)HDmemset(expect_dst_buf, 0, sizeof(expect_dst_buf));
    for(i=0; i<6; i++)
        expect_dst_buf[i] = src_buf[i + 2];

    /* Verify that base configuration passes */
    gather_info.expect_dst_buf = expect_dst_buf;
    gather_info.max_nelmts = 6;
    gather_info.last_call = FALSE;
    if(H5Dgather(sid, src_buf, H5T_NATIVE_INT, 6 * sizeof(dst_buf[0]), dst_buf, gather_cb, &gather_info) < 0)
        TEST_ERROR

    /*
     * Test invalid parameters
     */
    gather_info.expect_dst_buf = expect_dst_buf;
    gather_info.last_call = FALSE;
    H5E_BEGIN_TRY {
        ret = H5Dgather(H5T_NATIVE_INT, src_buf, H5T_NATIVE_INT, 6 * sizeof(dst_buf[0]), dst_buf, gather_cb, &gather_info);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR

    gather_info.expect_dst_buf = expect_dst_buf;
    gather_info.last_call = FALSE;
    H5E_BEGIN_TRY {
        ret = H5Dgather(sid, NULL, H5T_NATIVE_INT, 6 * sizeof(dst_buf[0]), dst_buf, gather_cb, &gather_info);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR

    gather_info.expect_dst_buf = expect_dst_buf;
    gather_info.last_call = FALSE;
    H5E_BEGIN_TRY {
        ret = H5Dgather(sid, src_buf, sid, 6 * sizeof(dst_buf[0]), dst_buf, gather_cb, &gather_info);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR

    gather_info.expect_dst_buf = expect_dst_buf;
    gather_info.last_call = FALSE;
    H5E_BEGIN_TRY {
        ret = H5Dgather(sid, src_buf, H5T_NATIVE_INT, 0, dst_buf, gather_cb, &gather_info);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR

    gather_info.expect_dst_buf = expect_dst_buf;
    gather_info.last_call = FALSE;
    H5E_BEGIN_TRY {
        ret = H5Dgather(sid, src_buf, H5T_NATIVE_INT, 1, dst_buf, gather_cb, &gather_info);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR

    gather_info.expect_dst_buf = expect_dst_buf;
    gather_info.last_call = FALSE;
    H5E_BEGIN_TRY {
        ret = H5Dgather(sid, src_buf, H5T_NATIVE_INT, 6 * sizeof(dst_buf[0]), NULL, gather_cb, &gather_info);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR

    gather_info.expect_dst_buf = expect_dst_buf;
    gather_info.last_call = FALSE;
    H5E_BEGIN_TRY {
        ret = H5Dgather(sid, src_buf, H5T_NATIVE_INT, 5 * sizeof(dst_buf[0]), dst_buf, NULL, &gather_info);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR


    /*
     * Test callback returns failure
     */
    gather_info.expect_dst_buf = expect_dst_buf;
    gather_info.last_call = FALSE;
    H5E_BEGIN_TRY {
        ret = H5Dgather(sid, src_buf, H5T_NATIVE_INT, 6 * sizeof(dst_buf[0]), dst_buf, gather_error_cb_fail, NULL);
    } H5E_END_TRY
    if(ret >= 0) TEST_ERROR


    /* Close everything */
    if(H5Sclose(sid) < 0) TEST_ERROR

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_gather_error() */

/*-------------------------------------------------------------------------
 * DLS bug -- HDFFV-9672
 *
 * The following functions replicate the test code provided by DLS to
 * expose bug hdffv-9672.  All functions associated with this test
 * have the prefix DLS_01_
 *
 * The note documenting the bug is reproduced below:
 *
 * ------------------------------------------------------
 *
 * Hi,
 * We've found an issue regarding fixed length strings.
 *
 * If we create a chunked dataset of large fixed length strings
 * (up to 1kb per string) with small chunk sizes (~8 elements per
 * chunk) then the resulting dataset may not be read later.
 * This only happens if the file is created with LIBVER_LATEST
 * for the version bounds.
 *
 * Calling H5Oget_info(...) on the dataset results in the following:
 *
 * H5Dearray.c:250: H5D__earray_crt_context: Assertion
 * `udata->chunk_size > 0' failed.
 *
 * Example:
 * void create_data(...)
 * {
 *    ...
 *
 *    hsize_t chunks[1] = {8} ;
 *
 *    err = H5Tset_size( tid, 256 );
 *
 *    err = H5Pset_chunk( dcpl, 1, chunks );
 *
 *    H5Dcreate2( fid, "data", tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT );
 *
 *    // write data
 * }
 *
 * void read_data(...)
 * {
 *    ...
 *
 *    H5O_into_t info; status = H5Oget_info( did, &info ) // crash
 *    ...
 * }
 *
 * If the size of the chunk is increased (usually matching the
 * string length) then this problem disappears.
 *
 * A full program that produces such a file (and crashes trying to
 * read it) is attached.
 *
 * Tested with 1.10.0-alpha1.
 *
 * Regards,
 *
 * Charles Mita
 * Software Engineer
 * Diamond Light Source Ltd.
 * +44 1235 778029
 *
 * ------------------------------------------------------
 *
 * The bug in question turned out to be caused by a failure to update
 * the enc_bytes_per_dim field in the layout if the size of the
 * underlying type required more bytes to encode than any of the
 * chunk dimensions.
 *
 * At least in debug builds, the following test code exposes the
 * failure via an assertion failure.
 *
 * Note that the test code make no attempt to run with different
 * file drivers, as the bug is in the actual on disk encoding of
 * the chunk layout.
 *
 *                                           JRM -- 2/5/16
 *
 *-------------------------------------------------------------------------
 */

#define DLS_01_DATASET        "data"
#define DLS_01_STR_SIZE     256
#define DLS_01_CHUNK_SIZE     8
#define DLS_01_DIMS         4

static herr_t dls_01_setup_file( hid_t fid );
static herr_t dls_01_write_data( hid_t fid, char* buffer );
static herr_t dls_01_read_stuff( hid_t fid );
static herr_t dls_01_main( void );

static herr_t
dls_01_setup_file( hid_t fid )
{
    int status = 0;
    hid_t sid = 0, did = 0, tid = 0, dcpl = 0;
    int ndims = 1;
    hsize_t max_shape[1] = {H5S_UNLIMITED};
    hsize_t initial_shape[1] = {0};
    hsize_t chunks[1] = {DLS_01_CHUNK_SIZE};

    sid = H5Screate_simple( ndims, initial_shape, max_shape );
    if ( sid <= 0 ) TEST_ERROR

    tid = H5Tcopy( H5T_C_S1 );
    if ( tid <= 0 ) TEST_ERROR

    status = H5Tset_size( tid, DLS_01_STR_SIZE );
    if ( status != 0 ) TEST_ERROR

    dcpl = H5Pcreate( H5P_DATASET_CREATE );
    if ( dcpl <= 0 ) TEST_ERROR

    status = H5Pset_chunk( dcpl, ndims, chunks );
    if ( status != 0 ) TEST_ERROR

    did = H5Dcreate2( fid, DLS_01_DATASET, tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT );
    if ( did <= 0 ) TEST_ERROR

    status = H5Dclose( did );
    if ( status != 0 ) TEST_ERROR

    status = H5Pclose( dcpl );
    if ( status != 0 ) TEST_ERROR

    status = H5Tclose( tid );
    if ( status != 0 ) TEST_ERROR

    status = H5Sclose( sid );
    if ( status != 0 ) TEST_ERROR

    return SUCCEED;

error:
    return FAIL;
} /* end dls_01_setup_file() */

static herr_t
dls_01_write_data( hid_t fid, char* buffer )
{
    int status = 0;
    hid_t did = 0, tid = 0;
    hsize_t extent[1] = {4};

    did = H5Dopen2( fid, DLS_01_DATASET, H5P_DEFAULT );
    if ( did <= 0 ) TEST_ERROR

    tid = H5Dget_type( did );
    if ( tid <= 0 ) TEST_ERROR

    status = H5Dset_extent( did, extent );
    if ( status != 0 ) TEST_ERROR

    status = H5Dwrite( did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer );
    if ( status != 0 ) TEST_ERROR

    status = H5Fflush( fid, H5F_SCOPE_LOCAL );
    if ( status != 0 ) TEST_ERROR

    status = H5Tclose( tid );
    if ( status != 0 ) TEST_ERROR

    status = H5Dclose( did );
    if ( status != 0 ) TEST_ERROR

    return SUCCEED;

error:
    return FAIL;
} /* end dls_01_write_data() */

static herr_t
dls_01_read_stuff( hid_t fid )
{
    int status = 0;
    hid_t did = 0;
    H5O_info_t info;

    did = H5Dopen2( fid, DLS_01_DATASET, H5P_DEFAULT );
    if ( did <= 0 ) TEST_ERROR

    status = H5Oget_info2( did, &info, H5O_INFO_BASIC );
    if ( status != 0 ) TEST_ERROR

    status = H5Dclose( did );
    if ( status != 0 ) TEST_ERROR

    return SUCCEED;

error:
    return FAIL;
} /* end dls_01_read_stuff() */

static herr_t
dls_01_main( void )
{
    char filename[512];
    int status = 0;
    hid_t fapl = 0, fid = 0;
    const char* strings[DLS_01_DIMS] =
    { "String 1", "Test string 2", "Another string", "Final String" };
    char* buffer = NULL;

    TESTING("Testing DLS bugfix 1");

    if ( NULL == h5_fixname(FILENAME[23], H5P_DEFAULT, filename,
                            sizeof(filename)) )
    TEST_ERROR

    buffer = (char *)HDcalloc( DLS_01_DIMS, DLS_01_STR_SIZE );
    if ( NULL == buffer )
        TEST_ERROR

    HDstrcpy( buffer, strings[0] );
    HDstrcpy( buffer + DLS_01_STR_SIZE, strings[1] );
    HDstrcpy( buffer + DLS_01_STR_SIZE * 2, strings[2] );
    HDstrcpy( buffer + DLS_01_STR_SIZE * 3, strings[3] );

    fapl = H5Pcreate( H5P_FILE_ACCESS );
    if ( fapl <= 0 ) TEST_ERROR

    status = H5Pset_libver_bounds( fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST );
    if ( status != 0 ) TEST_ERROR

    fid = H5Fcreate( filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl );
    if ( fid <= 0 ) TEST_ERROR

    if ( 0 != dls_01_setup_file( fid ) )
    goto error;

    if ( 0 != dls_01_write_data( fid, buffer ) )
    goto error;

    status = H5Fclose( fid );
    if ( status != 0 ) TEST_ERROR

    fid = H5Fopen( filename, H5F_ACC_RDONLY, fapl );
    if ( fid <= 0 ) TEST_ERROR

    if ( 0 != dls_01_read_stuff( fid ) )
    goto error;

    status = H5Fclose( fid );
    if ( status != 0 ) TEST_ERROR

    status = H5Pclose( fapl );
    if ( status != 0 ) TEST_ERROR

    HDfree(buffer);

    PASSED();

    return SUCCEED;

error:
    if ( buffer ) HDfree(buffer);
    return FAIL;
} /* end dls_01_main() */

/*-------------------------------------------------------------------------
 * Function:    test_compact_open_close_dirty
 *
 * Purpose:     Verify that the two issues reported in HDFFV-10051 are fixed:
 *              (1) Repeated open/close of a compact dataset fails due to the
 *                  increment of ndims in the dataset structure for every open.
 *              (2) layout "dirty" flag for a compact dataset is not reset
 *                  properly after flushing the data at dataset close.
 *              The test for issue #1 is based on compactoc.c attached
 *              to the jira issue HDFFV-10051
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi; April 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compact_open_close_dirty(hid_t fapl)
{
    hid_t       fid = -1;       /* File ID */
    hid_t       did = -1;       /* Dataset ID */
    hid_t       sid = -1;       /* Dataspace ID */
    hid_t       dcpl = -1;      /* Dataset creation property list */
    hsize_t     dims[1] = {10}; /* Dimension */
    int         wbuf[10];       /* Data buffer */
    char        filename[FILENAME_BUF_SIZE];    /* Filename */
    int         i;              /* Local index variable */
    hbool_t     dirty;          /* The dirty flag */

    TESTING("compact dataset repeated open/close and dirty flag");

    /* Create a file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Initialize data */
    for(i = 0; i < 10; i++)
        wbuf[i] = i;

    /* Create dataspace */
    if((sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR

    /* Set compact layout */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if(H5Pset_layout(dcpl, H5D_COMPACT) < 0)
        TEST_ERROR
    if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR

    /* Create a compact dataset */
    if((did = H5Dcreate2(fid, DSET_COMPACT_MAX_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Write to the dataset */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR

    /* Close the dataset */
    if(H5Dclose(did) < 0)
        TEST_ERROR

    /* Verify the repeated open/close of the dataset will not fail */
    for(i = 0; i < 20;i++) {
        H5E_BEGIN_TRY {
            did = H5Dopen2 (fid, DSET_COMPACT_MAX_NAME, H5P_DEFAULT);
        } H5E_END_TRY;
        if(did < 0)
            TEST_ERROR
        if(H5Dclose(did) < 0)
            TEST_ERROR
    }

    /* Open the dataset */
    if((did = H5Dopen2(fid, DSET_COMPACT_MAX_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Retrieve the "dirty" flag from the compact dataset layout */
    if(H5D__layout_compact_dirty_test(did, &dirty) < 0)
        TEST_ERROR

    /* Verify that the "dirty" flag is false */
    if(dirty)
        TEST_ERROR

    /* Close the dataset */
    if(H5Dclose(did) < 0)
        TEST_ERROR

    /* Close the dataspace */
    if(H5Sclose(sid) < 0)
        TEST_ERROR

    /* Close the dataset creation property list */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
        TEST_ERROR

     PASSED();
     return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;
    return FAIL;
} /* end test_compact_open_close_dirty() */


/*-------------------------------------------------------------------------
 * Function:    test_versionbounds
 *
 * Purpose:     Tests various format versions.
 *              (Currently, only virtual dataset feature)
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 * Description:
 *              This function attempts to create a virtual dataset in all
 *              valid combinations of low/high library format bounds.  Creation
 *              of virtual dataset should only succeed in H5F_LIBVER_V110.
 *              -BMR, January 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_versionbounds(void)
{
    hid_t fapl = -1;
    hid_t srcfile = -1;   /* Files with source dsets */
    hid_t vfile = -1;     /* File with virtual dset */
    hid_t dcpl = -1;      /* Dataset creation property list */
    hid_t srcspace = -1;  /* Source dataspaces */
    hid_t vspace = -1;    /* Virtual dset dataspaces */
    hid_t srcdset = -1;   /* Source datset */
    hid_t vdset = -1;     /* Virtual dataset */
    hid_t null_dspace = -1;     /* Data space of H5S_NULL */
    hsize_t dims[1] = {3};      /* Data space current size */
    char  srcfilename[FILENAME_BUF_SIZE];
    char  vfilename1[FILENAME_BUF_SIZE];
    char  vfilename2[FILENAME_BUF_SIZE];
    H5F_libver_t low, high;     /* File format bounds */
    herr_t ret;                 /* Generic return value */

    TESTING("version bounds of datasets");

    /* Create a copy of file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) TEST_ERROR

    h5_fixname(VDS_FNAME1, fapl, vfilename1, sizeof vfilename1);
    h5_fixname(VDS_FNAME2, fapl, vfilename2, sizeof vfilename2);
    h5_fixname(SRC_FNAME, fapl, srcfilename, sizeof srcfilename);

    /* Create DCPL */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    /* Clear virtual layout in DCPL */
    if(H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR

    /* Create source dataspace */
    if((srcspace = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR

    /* Create virtual dataspace */
    if((vspace = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR

    /* Add virtual layout mapping */
    if(H5Pset_virtual(dcpl, vspace, srcfilename, SRC_DSET, srcspace) < 0)
        TEST_ERROR

    /* Loop through all the combinations of low/high library format bounds */
    /* Create a source file and a dataset in it.  Create a virtual file and
       virtual dataset.  Creation of virtual dataset should only succeed in
       H5F_LIBVER_V110 */
    for(low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for(high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {

            /* Set version bounds, skip for invalid low/high combination */
            H5E_BEGIN_TRY {
                ret = H5Pset_libver_bounds(fapl, low, high);
            } H5E_END_TRY;

            if (ret < 0) /* Invalid low/high combinations */
                continue;

            /* Create a source file and dataset */
            if((srcfile = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
                TEST_ERROR
            if((srcdset = H5Dcreate2(srcfile, SRC_DSET, H5T_NATIVE_INT, srcspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                TEST_ERROR

            /* Create a virtual file */
            if((vfile = H5Fcreate(vfilename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
                TEST_ERROR

            /* Create the virtual dataset */
            H5E_BEGIN_TRY {
                vdset = H5Dcreate2(vfile, V_DSET, H5T_NATIVE_INT, vspace, H5P_DEFAULT, dcpl, H5P_DEFAULT);
            } H5E_END_TRY;

            if (vdset > 0) /* dataset created successfully */
            {
                /* Virtual dataset is only available starting in V110 */
                VERIFY(high, H5F_LIBVER_V110, "virtual dataset");

                if(H5Dclose(vdset) < 0) TEST_ERROR
                vdset = -1;
            }

            /* Close virtual file */
            if(H5Fclose(vfile) < 0) TEST_ERROR
            vfile = -1;

            /* Close srcdset and srcfile */
            if(H5Dclose(srcdset) < 0) TEST_ERROR
            srcdset = -1;

            if(H5Fclose(srcfile) < 0) TEST_ERROR
            srcfile = -1;

        } /* for high */
    } /* for low */

    /* Close dataspaces and properties */
    if(H5Sclose(srcspace) < 0)
        TEST_ERROR
    srcspace = -1;
    if(H5Sclose(vspace) < 0)
        TEST_ERROR
    vspace = -1;
    if(H5Pclose(fapl) < 0)
        TEST_ERROR
    fapl = -1;
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR
    dcpl = -1;
    PASSED();
    return SUCCEED;

 error:
    H5E_BEGIN_TRY {
        H5Sclose(srcspace);
        H5Sclose(vspace);
        H5Pclose(dcpl);
        H5Pclose(fapl);
        H5Dclose(srcdset);
        H5Dclose(vdset);
        H5Fclose(srcfile);
        H5Fclose(vfile);
    } H5E_END_TRY;
    return FAIL;
} /* end test_versionbounds() */


/*-----------------------------------------------------------------------------
 * Function:   test_object_header_minimization_dcpl
 *
 * Purpose:    Test the "datset object header minimization" property as part of
 *             the DCPL.
 *
 * Return:     Success/pass:   0
 *             Failure/error: -1
 *
 * Programmer: Jacob Smith
 *             2018 August 15
 *
 * Changes:    None.
 *-----------------------------------------------------------------------------
 */
static herr_t
test_object_header_minimization_dcpl(hid_t fapl_id)
{
    hid_t    dcpl_id  = -1;
    hid_t    file_id  = -1;
    char     filename[FILENAME_BUF_SIZE] = "";
    hbool_t  minimize = FALSE;
    herr_t   ret;

    TESTING("dcpl flags to minimize dataset object header");

    /*********/
    /* SETUP */
    /*********/

    if(NULL == h5_fixname(OHMIN_FILENAME_A, fapl_id, filename, sizeof(filename)))
        TEST_ERROR

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    if (file_id == H5I_INVALID_HID)
        TEST_ERROR

    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    if (dcpl_id == H5I_INVALID_HID)
        TEST_ERROR

    /*********/
    /* TESTS */
    /*********/

    /* default value (not set explicitly)
     */
    if (H5Pget_dset_no_attrs_hint(dcpl_id, &minimize) == FAIL)
        TEST_ERROR
    if (FALSE != minimize)
        TEST_ERROR

    /* FALSE-set value
     */
    if (H5Pset_dset_no_attrs_hint(dcpl_id, FALSE) == FAIL)
        TEST_ERROR
    if (H5Pget_dset_no_attrs_hint(dcpl_id, &minimize) == FAIL)
        TEST_ERROR
    if (FALSE != minimize)
        TEST_ERROR

    /* TRUE-set value
     */
    if (H5Pset_dset_no_attrs_hint(dcpl_id, TRUE) == FAIL)
        TEST_ERROR
    if (H5Pget_dset_no_attrs_hint(dcpl_id, &minimize) == FAIL)
        TEST_ERROR
    if (TRUE != minimize)
        TEST_ERROR

    /* error cases
     */
    H5E_BEGIN_TRY {
        ret = H5Pget_dset_no_attrs_hint(-1, &minimize);
    } H5E_END_TRY;
    if (ret == SUCCEED)
        TEST_ERROR /* Invalid DCPL ID should fail */

    H5E_BEGIN_TRY {
        ret = H5Pset_dset_no_attrs_hint(-1, FALSE);
    } H5E_END_TRY;
    if (ret == SUCCEED)
        TEST_ERROR /* Invalid DCPL ID should fail */

    H5E_BEGIN_TRY {
        ret = H5Pset_dset_no_attrs_hint(-1, TRUE);
    } H5E_END_TRY;
    if (ret == SUCCEED)
        TEST_ERROR /* Invalid DCPL ID should fail */

    H5E_BEGIN_TRY {
        ret = H5Pget_dset_no_attrs_hint(dcpl_id, NULL);
    } H5E_END_TRY;
    if (ret == SUCCEED)
        TEST_ERROR /* NULL out pointer should fail */

    /************/
    /* TEARDOWN */
    /************/

    if (H5Fclose(file_id) == FAIL)
        TEST_ERROR

    if (H5Pclose(dcpl_id) == FAIL)
        TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return FAIL;
} /* end test_object_header_minimization_dcpl() */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests the dataset interface (H5D)
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    char     filename[FILENAME_BUF_SIZE];
    hid_t    file, grp, fapl, fapl2;
    hid_t    fcpl = -1, fcpl2 = -1;
    unsigned new_format;
    unsigned paged;
    unsigned minimized_ohdr;
    int      mdc_nelmts;
    size_t   rdcc_nelmts;
    size_t   rdcc_nbytes;
    double   rdcc_w0;
    int      nerrors = 0;
    const char *envval;
    hbool_t  contig_addr_vfd;    /* Whether VFD used has a contigous address space */

    /* Don't run this test using certain file drivers */
    envval = HDgetenv("HDF5_DRIVER");
    if(envval == NULL)
        envval = "nomatch";

    /* Current VFD that does not support contigous address space */
    contig_addr_vfd = (hbool_t)(HDstrcmp(envval, "split") && HDstrcmp(envval, "multi"));

    /* Set the random # seed */
    HDsrandom((unsigned)HDtime(NULL));

    /* Testing setup */
    h5_reset();
    fapl = h5_fileaccess();

    /* Turn off the chunk cache, so all the chunks are immediately written to disk */
    if(H5Pget_cache(fapl, &mdc_nelmts, &rdcc_nelmts, &rdcc_nbytes, &rdcc_w0) < 0)
        goto error;
    rdcc_nbytes = 0;
    if(H5Pset_cache(fapl, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0) < 0)
        goto error;

    /* Copy the file access property list */
    if((fapl2 = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    if(H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) TEST_ERROR

    /* create a file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        TEST_ERROR
    if((fcpl2 = H5Pcopy(fcpl)) < 0) TEST_ERROR

    /* Set file space strategy to paged aggregation and persisting free-space */
    if(H5Pset_file_space_strategy(fcpl2, H5F_FSPACE_STRATEGY_PAGE, TRUE, (hsize_t)1) < 0)
        TEST_ERROR

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Test with paged aggregation enabled or not */
    for(paged = FALSE; paged <= TRUE; paged++) {

        /* Temporary: skip testing for multi/split drivers:
             fail file create when persisting free-space or using paged aggregation strategy */
        if(!contig_addr_vfd && paged)
            continue;

        for(minimized_ohdr = FALSE; minimized_ohdr <= TRUE; minimized_ohdr++) {

            /* Test with old & new format groups */
            for(new_format = FALSE; new_format <= TRUE; new_format++) {
                hid_t my_fapl, my_fcpl;

                /* Set the FAPL for the type of format */
                if(new_format) {
                    my_fapl = fapl2;
                    if(paged) {
                        my_fcpl = fcpl2;
                        HDputs("\nTesting with new file format and paged aggregation");
                    } else {
                        my_fcpl = fcpl;
                        HDputs("\nTesting with new file format and non-paged aggregation");
                    }
                } /* end if */
                else {
                    my_fapl = fapl;
                    if(paged) {
                        my_fcpl = fcpl2;
                        HDputs("Testing with old file format and paged aggregation:");
                    } else {
                        my_fcpl = fcpl;
                        HDputs("Testing with old file format and non-paged aggregation:");
                    }
                } /* end else */

                /* Create the file for this test */
                if((file = H5Fcreate(filename, H5F_ACC_TRUNC, my_fcpl, my_fapl)) < 0)
                    goto error;

                if (TRUE == minimized_ohdr) {
                    if (0 > H5Fset_dset_no_attrs_hint(file, TRUE))
                        goto error;
                    HDputs("(minimized dataset object headers with file setting)");
                }

                /* Cause the library to emit initial messages */
                if((grp = H5Gcreate2(file, "emit diagnostics", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                    goto error;
                if(H5Oset_comment(grp, "Causes diagnostic messages to be emitted") < 0)
                    goto error;
                if(H5Gclose(grp) < 0)
                    goto error;

                nerrors += (test_create(file) < 0             ? 1 : 0);
                nerrors += (test_simple_io(envval, my_fapl) < 0        ? 1 : 0);
                nerrors += (test_compact_io(my_fapl) < 0          ? 1 : 0);
                nerrors += (test_max_compact(my_fapl) < 0        ? 1 : 0);
                nerrors += (test_compact_open_close_dirty(my_fapl) < 0     ? 1 : 0);
                nerrors += (test_conv_buffer(file) < 0            ? 1 : 0);
                nerrors += (test_tconv(file) < 0            ? 1 : 0);
                nerrors += (test_filters(file, my_fapl) < 0        ? 1 : 0);
                nerrors += (test_onebyte_shuffle(file) < 0         ? 1 : 0);
                nerrors += (test_nbit_int(file) < 0                 ? 1 : 0);
                nerrors += (test_nbit_float(file) < 0                     ? 1 : 0);
                nerrors += (test_nbit_double(file) < 0                     ? 1 : 0);
                nerrors += (test_nbit_array(file) < 0                 ? 1 : 0);
                nerrors += (test_nbit_compound(file) < 0         ? 1 : 0);
                nerrors += (test_nbit_compound_2(file) < 0         ? 1 : 0);
                nerrors += (test_nbit_compound_3(file) < 0         ? 1 : 0);
                nerrors += (test_nbit_int_size(file) < 0         ? 1 : 0);
                nerrors += (test_nbit_flt_size(file) < 0         ? 1 : 0);
                nerrors += (test_scaleoffset_int(file) < 0         ? 1 : 0);
                nerrors += (test_scaleoffset_int_2(file) < 0             ? 1 : 0);
                nerrors += (test_scaleoffset_float(file) < 0             ? 1 : 0);
                nerrors += (test_scaleoffset_float_2(file) < 0             ? 1 : 0);
                nerrors += (test_scaleoffset_double(file) < 0             ? 1 : 0);
                nerrors += (test_scaleoffset_double_2(file) < 0     ? 1 : 0);
                nerrors += (test_multiopen (file) < 0                ? 1 : 0);
                nerrors += (test_types(file) < 0                       ? 1 : 0);
                nerrors += (test_userblock_offset(envval, my_fapl, new_format) < 0  ? 1 : 0);
                nerrors += (test_missing_filter(file) < 0        ? 1 : 0);
                nerrors += (test_can_apply(file) < 0                ? 1 : 0);
                nerrors += (test_can_apply2(file) < 0                ? 1 : 0);
                nerrors += (test_set_local(my_fapl) < 0                ? 1 : 0);
                nerrors += (test_can_apply_szip(file) < 0        ? 1 : 0);
                nerrors += (test_compare_dcpl(file) < 0                ? 1 : 0);
                nerrors += (test_copy_dcpl(file, my_fapl) < 0            ? 1 : 0);
                nerrors += (test_filter_delete(file) < 0        ? 1 : 0);
                nerrors += (test_filters_endianess() < 0            ? 1 : 0);
                nerrors += (test_zero_dims(file) < 0                ? 1 : 0);
                nerrors += (test_missing_chunk(file) < 0        ? 1 : 0);
                nerrors += (test_random_chunks(my_fapl) < 0        ? 1 : 0);

#ifndef H5_NO_DEPRECATED_SYMBOLS
                nerrors += (test_deprec(file) < 0            ? 1 : 0);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

                nerrors += (test_huge_chunks(my_fapl) < 0        ? 1 : 0);
                nerrors += (test_chunk_cache(my_fapl) < 0        ? 1 : 0);
                nerrors += (test_big_chunks_bypass_cache(my_fapl) < 0   ? 1 : 0);
                nerrors += (test_chunk_fast(envval, my_fapl) < 0    ? 1 : 0);
                nerrors += (test_reopen_chunk_fast(my_fapl) < 0        ? 1 : 0);
                nerrors += (test_chunk_fast_bug1(my_fapl) < 0           ? 1 : 0);
                nerrors += (test_chunk_expand(my_fapl) < 0        ? 1 : 0);
                nerrors += (test_layout_extend(my_fapl) < 0        ? 1 : 0);
                nerrors += (test_fixed_array(my_fapl) < 0        ? 1 : 0);
                nerrors += (test_idx_compatible() < 0            ? 1 : 0);
                nerrors += (test_unfiltered_edge_chunks(my_fapl) < 0    ? 1 : 0);
                nerrors += (test_single_chunk(my_fapl) < 0              ? 1 : 0);
                nerrors += (test_large_chunk_shrink(my_fapl) < 0        ? 1 : 0);
                nerrors += (test_zero_dim_dset(my_fapl) < 0             ? 1 : 0);
                nerrors += (test_storage_size(my_fapl) < 0              ? 1 : 0);
                nerrors += (test_power2up(my_fapl) < 0                  ? 1 : 0);

                nerrors += (test_swmr_non_latest(envval, my_fapl) < 0   ? 1 : 0);
                nerrors += (test_earray_hdr_fd(envval, my_fapl) < 0     ? 1 : 0);
                nerrors += (test_farray_hdr_fd(envval, my_fapl) < 0     ? 1 : 0);
                nerrors += (test_bt2_hdr_fd(envval, my_fapl) < 0        ? 1 : 0);

                if(H5Fclose(file) < 0)
                    goto error;
            } /* end for new_format */
        } /* end for minimized_ohdr */
    } /* end for paged */

    /* Close property lists */
    if(H5Pclose(fapl2) < 0) TEST_ERROR
    if(H5Pclose(fcpl) < 0) TEST_ERROR
    if(H5Pclose(fcpl2) < 0) TEST_ERROR

    /* Tests that do not use files */
    nerrors += (test_scatter() < 0                          ? 1 : 0);
    nerrors += (test_gather() < 0                           ? 1 : 0);
    nerrors += (test_scatter_error() < 0                    ? 1 : 0);
    nerrors += (test_gather_error() < 0                     ? 1 : 0);

    /* Tests version bounds using its own file */
    nerrors += (test_versionbounds() < 0                    ? 1 : 0);

    nerrors += (test_object_header_minimization_dcpl(fapl) < 0 ? 1 : 0);

    /* Run misc tests */
    nerrors += dls_01_main();

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if(nerrors)
        goto error;
    HDprintf("All dataset tests passed.\n");
#ifdef H5_HAVE_FILTER_SZIP
    if (GetTestCleanup())
        HDremove(NOENCODER_COPY_FILENAME);
#endif /* H5_HAVE_FILTER_SZIP */
    h5_cleanup(FILENAME, fapl);

    HDexit(EXIT_SUCCESS);

error:
    nerrors = MAX(1, nerrors);
    HDprintf("***** %d DATASET TEST%s FAILED! *****\n",
            nerrors, 1 == nerrors ? "" : "S");
    HDexit(EXIT_FAILURE);
} /* end main() */

