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

/***********************************************************
*
* Test program:     tfile
*
* Test the low-level file I/O features.
*
*************************************************************/

#include "testhdf5.h"
#include "H5srcdir.h"

#include "H5Bprivate.h"
#include "H5Iprivate.h"
#include "H5Pprivate.h"

/*
 * This file needs to access private information from the H5F package.
 * This file also needs to access the file testing code.
 */
#define H5F_FRIEND        /*suppress error about including H5Fpkg */
#define H5F_TESTING
#include "H5Fpkg.h"        /* File access                             */

#define H5D_FRIEND      /*suppress error about including H5Dpkg */
#include "H5Dpkg.h"     /* Dataset access                       */

#define H5S_FRIEND      /*suppress error about including H5Spkg */
#include "H5Spkg.h"     /* Dataspace                            */

#define H5T_FRIEND      /*suppress error about including H5Tpkg */
#include "H5Tpkg.h"     /* Datatype                             */

#define H5A_FRIEND      /*suppress error about including H5Apkg */
#include "H5Apkg.h"     /* Attributes                           */

#define H5O_FRIEND      /*suppress error about including H5Opkg */
#include "H5Opkg.h"     /* Object headers                       */

#define BAD_USERBLOCK_SIZE1  (hsize_t)1
#define BAD_USERBLOCK_SIZE2  (hsize_t)2
#define BAD_USERBLOCK_SIZE3  (hsize_t)3
#define BAD_USERBLOCK_SIZE4  (hsize_t)64
#define BAD_USERBLOCK_SIZE5  (hsize_t)511
#define BAD_USERBLOCK_SIZE6  (hsize_t)513
#define BAD_USERBLOCK_SIZE7  (hsize_t)6144

#define F1_USERBLOCK_SIZE  (hsize_t)0
#define F1_OFFSET_SIZE       sizeof(haddr_t)
#define F1_LENGTH_SIZE       sizeof(hsize_t)
#define F1_SYM_LEAF_K       4
#define F1_SYM_INTERN_K       16
#define FILE1    "tfile1.h5"
#define SFILE1    "sys_file1"

#define REOPEN_FILE "tfile_reopen.h5"
#define REOPEN_DSET "dset"

#define F2_USERBLOCK_SIZE  (hsize_t)512
#define F2_OFFSET_SIZE       8
#define F2_LENGTH_SIZE       8
#define F2_SYM_LEAF_K       8
#define F2_SYM_INTERN_K       32
#define F2_RANK            2
#define F2_DIM0            4
#define F2_DIM1            6
#define F2_DSET            "dset"
#define FILE2    "tfile2.h5"

#define F3_USERBLOCK_SIZE  (hsize_t)0
#define F3_OFFSET_SIZE       F2_OFFSET_SIZE
#define F3_LENGTH_SIZE       F2_LENGTH_SIZE
#define F3_SYM_LEAF_K       F2_SYM_LEAF_K
#define F3_SYM_INTERN_K       F2_SYM_INTERN_K
#define FILE3    "tfile3.h5"

#define GRP_NAME         "/group"
#define DSET_NAME         "dataset"
#define ATTR_NAME          "attr"
#define TYPE_NAME          "type"
#define FILE4               "tfile4.h5"

#define OBJ_ID_COUNT_0     0
#define OBJ_ID_COUNT_1     1
#define OBJ_ID_COUNT_2     2
#define OBJ_ID_COUNT_3     3
#define OBJ_ID_COUNT_4     4
#define OBJ_ID_COUNT_6       6
#define OBJ_ID_COUNT_8     8

#define GROUP1  "Group1"
#define DSET1   "Dataset1"
#define DSET2   "/Group1/Dataset2"

#define TESTA_GROUPNAME "group"
#define TESTA_DSETNAME "dataset"
#define TESTA_ATTRNAME "attribute"
#define TESTA_DTYPENAME "compound"
#define TESTA_NAME_BUF_SIZE     64
#define TESTA_RANK 2
#define TESTA_NX 4
#define TESTA_NY 5

#define USERBLOCK_SIZE      ((hsize_t) 512)

/* Declarations for test_filespace_*() */
#define FILENAME_LEN        1024                        /* length of file name */
#define DSETNAME            "dset"                      /* Name of dataset */
#define NELMTS(X)           (sizeof(X)/sizeof(X[0]))    /* # of elements */
#define READ_OLD_BUFSIZE    1024                        /* Buffer for holding file data */
#define FILE5               "tfile5.h5"                 /* Test file */
#define TEST_THRESHOLD10    10                          /* Free space section threshold */
#define FSP_SIZE_DEF        4096                        /* File space page size default */
#define FSP_SIZE512         512                         /* File space page size */
#define FSP_SIZE1G          1024*1024*1024              /* File space page size */

/* Declaration for test_libver_macros2() */
#define FILE6            "tfile6.h5"    /* Test file */

/* Declaration for test_get_obj_ids() */
#define FILE7            "tfile7.h5"    /* Test file */
#define NGROUPS            2
#define NDSETS            4

/* Declaration for test_incr_filesize() */
#define FILE8            "tfile8.h5"    /* Test file */

/* Files created under 1.6 branch and 1.8 branch--used in test_filespace_compatible() */
const char *OLD_FILENAME[] = {
    "filespace_1_6.h5",    /* 1.6 HDF5 file */
    "filespace_1_8.h5"    /* 1.8 HDF5 file */
};

/* Files created in 1.10.0 release --used in test_filespace_1.10.0_compatible() */
/* These files are copied from release 1.10.0 tools/h5format_convert/testfiles */
const char *OLD_1_10_0_FILENAME[] = {
    "h5fc_ext1_i.h5",   /* 0 */
    "h5fc_ext1_f.h5",   /* 1 */
    "h5fc_ext2_if.h5",  /* 2 */
    "h5fc_ext2_sf.h5",  /* 3 */
    "h5fc_ext3_isf.h5", /* 4 */
    "h5fc_ext_none.h5"  /* 5 */
};

/* Files used in test_filespace_round_compatible() */
const char *FSPACE_FILENAMES[] = {
    "fsm_aggr_nopersist.h5",    /* H5F_FILE_SPACE_AGGR, not persisting free-space */
    "fsm_aggr_persist.h5",      /* H5F_FILE_SPACE_AGGR, persisting free-space */
    "paged_nopersist.h5",       /* H5F_FILE_SPACE_PAGE, not persisting free-space */
    "paged_persist.h5",         /* H5F_FILE_SPACE_PAGE, persisting free-space */
    "aggr.h5",                  /* H5F_FILE_SPACE_AGGR */
    "none.h5"                   /* H5F_FILE_SPACE_NONE */
};

const char *FILESPACE_NAME[] = {
    "tfilespace",
    NULL
};


/* Declarations for test_libver_bounds_copy():                          */
/* SRC_FILE: source file created under 1.8 branch with latest format    */
/* DST_FILE: destination file for copying the dataset in SRC_FILE       */
/* DSET_DS1: the dataset created in SRC_FILE to be copied to DST_FILE   */
#define SRC_FILE        "fill18.h5"
#define DST_FILE        "fill18_copy.h5"
#define DSET_DS1        "DS1"

/* Local test function declarations for version bounds */
static void test_libver_bounds_low_high(void);
static void test_libver_bounds_super(hid_t fapl);
static void test_libver_bounds_super_create(hid_t fapl, hid_t fcpl, htri_t is_swmr, htri_t non_def_fsm);
static void test_libver_bounds_super_open(hid_t fapl, hid_t fcpl, htri_t is_swmr, htri_t non_def_fsm);
static void test_libver_bounds_obj(hid_t fapl);
static void test_libver_bounds_dataset(hid_t fapl);
static void test_libver_bounds_dataspace(hid_t fapl);
static void test_libver_bounds_datatype(hid_t fapl);
static void test_libver_bounds_datatype_check(hid_t fapl, hid_t tid);
static void test_libver_bounds_attributes(hid_t fapl);

#define DSET_NULL    "DSET_NULL"
#define DSET        "DSET"
#define DSETA        "DSETA"
#define DSETB        "DSETB"
#define DSETC        "DSETC"

static void
create_objects(hid_t, hid_t, hid_t *, hid_t *, hid_t *, hid_t *);
static void
test_obj_count_and_id(hid_t, hid_t, hid_t, hid_t, hid_t, hid_t);
static void
check_file_id(hid_t, hid_t);

/* Helper routine used by test_rw_noupdate() */
static int cal_chksum(const char *file, uint32_t *chksum);

static void test_rw_noupdate(void);

/****************************************************************
**
**  test_file_create(): Low-level file creation I/O test routine.
**
****************************************************************/
static void
test_file_create(void)
{
    hid_t        fid1, fid2, fid3; /* HDF5 File IDs        */
    hid_t        tmpl1, tmpl2;    /*file creation templates    */
    hsize_t        ublock;        /*sizeof userblock        */
    size_t        parm;        /*file-creation parameters    */
    size_t        parm2;        /*file-creation parameters    */
    unsigned        iparm;
    unsigned        iparm2;
    herr_t        ret;        /*generic return value        */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Low-Level File Creation I/O\n"));

    /* First ensure the file does not exist */
    HDremove(FILE1);

    /* Try opening a non-existant file */
    fid1 = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
    VERIFY(fid1, FAIL, "H5Fopen");

    /* Test create with various sequences of H5F_ACC_EXCL and */
    /* H5F_ACC_TRUNC flags */

    /* Create with H5F_ACC_EXCL */
    fid1 = H5Fcreate(FILE1, H5F_ACC_EXCL, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /*
     * try to create the same file with H5F_ACC_TRUNC. This should fail
     * because fid1 is the same file and is currently open.
     */
    fid2 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(fid2, FAIL, "H5Fcreate");

    /* Close all files */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Fclose(fid2);
    VERIFY(ret, FAIL, "H5Fclose"); /*file should not have been open */

    /*
     * Try again with H5F_ACC_EXCL. This should fail because the file already
     * exists from the previous steps.
     */
    fid1 = H5Fcreate(FILE1, H5F_ACC_EXCL, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(fid1, FAIL, "H5Fcreate");

    /* Test create with H5F_ACC_TRUNC. This will truncate the existing file. */
    fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /*
     * Try to truncate first file again. This should fail because fid1 is the
     * same file and is currently open.
     */
    fid2 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(fid2, FAIL, "H5Fcreate");

    /*
     * Try with H5F_ACC_EXCL. This should fail too because the file already
     * exists.
     */
    fid2 = H5Fcreate(FILE1, H5F_ACC_EXCL, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(fid2, FAIL, "H5Fcreate");

    /* Get the file-creation template */
    tmpl1 = H5Fget_create_plist(fid1);
    CHECK(tmpl1, FAIL, "H5Fget_create_plist");

    /* Get the file-creation parameters */
    ret = H5Pget_userblock(tmpl1, &ublock);
    CHECK(ret, FAIL, "H5Pget_userblock");
    VERIFY(ublock, F1_USERBLOCK_SIZE, "H5Pget_userblock");

    ret = H5Pget_sizes(tmpl1, &parm, &parm2);
    CHECK(ret, FAIL, "H5Pget_sizes");
    VERIFY(parm, F1_OFFSET_SIZE, "H5Pget_sizes");
    VERIFY(parm2, F1_LENGTH_SIZE, "H5Pget_sizes");

    ret = H5Pget_sym_k(tmpl1, &iparm, &iparm2);
    CHECK(ret, FAIL, "H5Pget_sym_k");
    VERIFY(iparm, F1_SYM_INTERN_K, "H5Pget_sym_k");
    VERIFY(iparm2, F1_SYM_LEAF_K, "H5Pget_sym_k");

    /* Release file-creation template */
    ret = H5Pclose(tmpl1);
    CHECK(ret, FAIL, "H5Pclose");

#ifdef LATER
    /* Double-check that the atom has been vaporized */
    ret = H5Pclose(tmpl1);
    VERIFY(ret, FAIL, "H5Pclose");
#endif

    /* Create a new file with a non-standard file-creation template */
    tmpl1 = H5Pcreate(H5P_FILE_CREATE);
    CHECK(tmpl1, FAIL, "H5Pcreate");

    /* Try setting some bad userblock sizes */
    H5E_BEGIN_TRY {
        ret = H5Pset_userblock(tmpl1, BAD_USERBLOCK_SIZE1);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Pset_userblock");
    H5E_BEGIN_TRY {
        ret = H5Pset_userblock(tmpl1, BAD_USERBLOCK_SIZE2);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Pset_userblock");
    H5E_BEGIN_TRY {
        ret = H5Pset_userblock(tmpl1, BAD_USERBLOCK_SIZE3);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Pset_userblock");
    H5E_BEGIN_TRY {
        ret = H5Pset_userblock(tmpl1, BAD_USERBLOCK_SIZE4);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Pset_userblock");
    H5E_BEGIN_TRY {
        ret = H5Pset_userblock(tmpl1, BAD_USERBLOCK_SIZE5);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Pset_userblock");
    H5E_BEGIN_TRY {
        ret = H5Pset_userblock(tmpl1, BAD_USERBLOCK_SIZE6);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Pset_userblock");
    H5E_BEGIN_TRY {
        ret = H5Pset_userblock(tmpl1, BAD_USERBLOCK_SIZE7);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Pset_userblock");

    /* Set the new file-creation parameters */
    ret = H5Pset_userblock(tmpl1, F2_USERBLOCK_SIZE);
    CHECK(ret, FAIL, "H5Pset_userblock");

    ret = H5Pset_sizes(tmpl1, (size_t)F2_OFFSET_SIZE, (size_t)F2_LENGTH_SIZE);
    CHECK(ret, FAIL, "H5Pset_sizes");

    ret = H5Pset_sym_k(tmpl1, F2_SYM_INTERN_K, F2_SYM_LEAF_K);
    CHECK(ret, FAIL, "H5Pset_sym_k");

    /*
     * Try to create second file, with non-standard file-creation template
     * params.
     */
    fid2 = H5Fcreate(FILE2, H5F_ACC_TRUNC, tmpl1, H5P_DEFAULT);
    CHECK(fid2, FAIL, "H5Fcreate");

    /* Release file-creation template */
    ret = H5Pclose(tmpl1);
    CHECK(ret, FAIL, "H5Pclose");

    /* Make certain we can create a dataset properly in the file with the userblock */
    {
       hid_t       dataset_id, dataspace_id;  /* identifiers */
       hsize_t     dims[F2_RANK];
       unsigned    data[F2_DIM0][F2_DIM1];
       unsigned i,j;

       /* Create the data space for the dataset. */
       dims[0] = F2_DIM0;
       dims[1] = F2_DIM1;
       dataspace_id = H5Screate_simple(F2_RANK, dims, NULL);
       CHECK(dataspace_id, FAIL, "H5Screate_simple");

       /* Create the dataset. */
       dataset_id = H5Dcreate2(fid2, F2_DSET, H5T_NATIVE_UINT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
       CHECK(dataset_id, FAIL, "H5Dcreate2");

       for(i = 0; i < F2_DIM0; i++)
           for(j = 0; j < F2_DIM1; j++)
               data[i][j] = i * 10 + j;

       /* Write data to the new dataset */
       ret = H5Dwrite(dataset_id, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
       CHECK(ret, FAIL, "H5Dwrite");

       /* End access to the dataset and release resources used by it. */
       ret = H5Dclose(dataset_id);
       CHECK(ret, FAIL, "H5Dclose");

       /* Terminate access to the data space. */
       ret = H5Sclose(dataspace_id);
       CHECK(ret, FAIL, "H5Sclose");
    }

    /* Get the file-creation template */
    tmpl1 = H5Fget_create_plist(fid2);
    CHECK(tmpl1, FAIL, "H5Fget_create_plist");

    /* Get the file-creation parameters */
    ret = H5Pget_userblock(tmpl1, &ublock);
    CHECK(ret, FAIL, "H5Pget_userblock");
    VERIFY(ublock, F2_USERBLOCK_SIZE, "H5Pget_userblock");

    ret = H5Pget_sizes(tmpl1, &parm, &parm2);
    CHECK(ret, FAIL, "H5Pget_sizes");
    VERIFY(parm, F2_OFFSET_SIZE, "H5Pget_sizes");
    VERIFY(parm2, F2_LENGTH_SIZE, "H5Pget_sizes");

    ret = H5Pget_sym_k(tmpl1, &iparm, &iparm2);
    CHECK(ret, FAIL, "H5Pget_sym_k");
    VERIFY(iparm, F2_SYM_INTERN_K, "H5Pget_sym_k");
    VERIFY(iparm2, F2_SYM_LEAF_K, "H5Pget_sym_k");

    /* Clone the file-creation template */
    tmpl2 = H5Pcopy(tmpl1);
    CHECK(tmpl2, FAIL, "H5Pcopy");

    /* Release file-creation template */
    ret = H5Pclose(tmpl1);
    CHECK(ret, FAIL, "H5Pclose");

    /* Set the new file-creation parameter */
    ret = H5Pset_userblock(tmpl2, F3_USERBLOCK_SIZE);
    CHECK(ret, FAIL, "H5Pset_userblock");

    /*
     * Try to create second file, with non-standard file-creation template
     * params
     */
    fid3 = H5Fcreate(FILE3, H5F_ACC_TRUNC, tmpl2, H5P_DEFAULT);
    CHECK(fid3, FAIL, "H5Fcreate");

    /* Release file-creation template */
    ret = H5Pclose(tmpl2);
    CHECK(ret, FAIL, "H5Pclose");

    /* Get the file-creation template */
    tmpl1 = H5Fget_create_plist(fid3);
    CHECK(tmpl1, FAIL, "H5Fget_create_plist");

    /* Get the file-creation parameters */
    ret = H5Pget_userblock(tmpl1, &ublock);
    CHECK(ret, FAIL, "H5Pget_userblock");
    VERIFY(ublock, F3_USERBLOCK_SIZE, "H5Pget_userblock");

    ret = H5Pget_sizes(tmpl1, &parm, &parm2);
    CHECK(ret, FAIL, "H5Pget_sizes");
    VERIFY(parm, F3_OFFSET_SIZE, "H5Pget_sizes");
    VERIFY(parm2, F3_LENGTH_SIZE, "H5Pget_sizes");

    ret = H5Pget_sym_k(tmpl1, &iparm, &iparm2);
    CHECK(ret, FAIL, "H5Pget_sym_k");
    VERIFY(iparm, F3_SYM_INTERN_K, "H5Pget_sym_k");
    VERIFY(iparm2, F3_SYM_LEAF_K, "H5Pget_sym_k");

    /* Release file-creation template */
    ret = H5Pclose(tmpl1);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close first file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close second file */
    ret = H5Fclose(fid2);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close third file */
    ret = H5Fclose(fid3);
    CHECK(ret, FAIL, "H5Fclose");
}                /* test_file_create() */

/****************************************************************
**
**  test_file_open(): Low-level file open I/O test routine.
**
****************************************************************/
static void
test_file_open(void)
{
    hid_t        fid1, fid2;     /*HDF5 File IDs            */
    hid_t               did;            /*dataset ID                    */
    hid_t               fapl_id;        /*file access property list ID  */
    hid_t        tmpl1;        /*file creation templates    */
    hsize_t        ublock;        /*sizeof user block        */
    size_t        parm;        /*file-creation parameters    */
    size_t        parm2;        /*file-creation parameters    */
    unsigned        iparm;
    unsigned        iparm2;
    unsigned        intent;
    herr_t        ret;        /*generic return value        */

    /*
     * Test single file open
     */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Low-Level File Opening I/O\n"));

    /* Open first file */
    fid1 = H5Fopen(FILE2, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Get the intent */
    ret = H5Fget_intent(fid1, &intent);
    CHECK(ret, FAIL, "H5Fget_intent");
    VERIFY(intent, H5F_ACC_RDWR, "H5Fget_intent");

    /* Get the file-creation template */
    tmpl1 = H5Fget_create_plist(fid1);
    CHECK(tmpl1, FAIL, "H5Fget_create_plist");

    /* Get the file-creation parameters */
    ret = H5Pget_userblock(tmpl1, &ublock);
    CHECK(ret, FAIL, "H5Pget_userblock");
    VERIFY(ublock, F2_USERBLOCK_SIZE, "H5Pget_userblock");

    ret = H5Pget_sizes(tmpl1, &parm, &parm2);
    CHECK(ret, FAIL, "H5Pget_sizes");
    VERIFY(parm, F2_OFFSET_SIZE, "H5Pget_sizes");
    VERIFY(parm2, F2_LENGTH_SIZE, "H5Pget_sizes");

    ret = H5Pget_sym_k(tmpl1, &iparm, &iparm2);
    CHECK(ret, FAIL, "H5Pget_sym_k");
    VERIFY(iparm, F2_SYM_INTERN_K, "H5Pget_sym_k");
    VERIFY(iparm2, F2_SYM_LEAF_K, "H5Pget_sym_k");

    /* Release file-creation template */
    ret = H5Pclose(tmpl1);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close first file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");


    /*
     * Test two file opens: one is opened H5F_ACC_RDONLY and H5F_CLOSE_WEAK.
     * It's closed with an object left open.  Then another is opened
     * H5F_ACC_RDWR, which should fail.
     */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing 2 File Openings\n"));

    /* Create file access property list */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl_id, FAIL, "H5Pcreate");

    /* Set file close mode to H5F_CLOSE_WEAK */
    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_WEAK);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    /* Open file for first time */
    fid1 = H5Fopen(FILE2, H5F_ACC_RDONLY, fapl_id);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Check the intent */
    ret = H5Fget_intent(fid1, &intent);
    CHECK(ret, FAIL, "H5Fget_intent");
    VERIFY(intent, H5F_ACC_RDONLY, "H5Fget_intent");

    /* Open dataset */
    did = H5Dopen2(fid1, F2_DSET, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen2");

    /* Check that the intent works even if NULL is passed in */
    ret = H5Fget_intent(fid1, NULL);
    CHECK(ret, FAIL, "H5Fget_intent");

    /* Close first open */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Open file for second time, which should fail. */
    fid2 = H5Fopen(FILE2, H5F_ACC_RDWR, fapl_id);
    VERIFY(fid2, FAIL, "H5Fopen");

    /* Check that the intent fails for an invalid ID */
    ret = H5Fget_intent(fid1, &intent);
    VERIFY(ret, FAIL, "H5Fget_intent");

    /* Close dataset from first open */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Pclose(fapl_id);
    CHECK(ret, FAIL, "H5Pclose");
}   /* test_file_open() */

/****************************************************************
**
**  test_file_reopen(): File reopen test routine.
**
****************************************************************/
static void
test_file_reopen(void)
{
    hid_t   fid = -1;           /* file ID from initial open            */
    hid_t   rfid = -1;          /* file ID from reopen                  */
    hid_t   did = -1;           /* dataset ID (both opens)              */
    hid_t   sid = -1;           /* dataspace ID for dataset creation    */
    hsize_t dims = 6;           /* dataspace size                       */
    herr_t  ret;                /* Generic return value                 */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing File Re-opening\n"));

    /* Create file via first ID */
    fid = H5Fcreate(REOPEN_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(fid, "H5Fcreate");

    /* Create a dataset in the file */
    sid = H5Screate_simple(1, &dims, &dims);
    CHECK_I(sid, "H5Screate_simple")
    did = H5Dcreate2(fid, REOPEN_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(did, "H5Dcreate2");

    /* Close dataset and dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Reopen the file with a different file ID */
    rfid = H5Freopen(fid);
    CHECK_I(rfid, "H5Freopen");

    /* Reopen the dataset through the reopen file ID */
    did = H5Dopen2(rfid, REOPEN_DSET, H5P_DEFAULT);
    CHECK_I(did, "H5Dopen2");

    /* Close and clean up */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Fclose(rfid);
    CHECK(ret, FAIL, "H5Fclose");
    HDremove(REOPEN_FILE);

}   /* test_file_reopen() */

/****************************************************************
**
**  test_file_close():  low-level file close test routine.
**                      It mainly tests behavior with close degree.
**
*****************************************************************/
static void
test_file_close(void)
{
    hid_t               fid1, fid2;
    hid_t               fapl_id, access_id;
    hid_t        dataset_id, group_id1, group_id2, group_id3;
    H5F_close_degree_t  fc_degree;
    herr_t              ret;

    /* Test behavior while opening file multiple times with different
     * file close degree value
     */
    fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl_id, FAIL, "H5Pcreate");

    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_STRONG);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    ret = H5Pget_fclose_degree(fapl_id, &fc_degree);
    VERIFY(fc_degree, H5F_CLOSE_STRONG, "H5Pget_fclose_degree");

    /* should fail */
    fid2 = H5Fopen(FILE1, H5F_ACC_RDWR, fapl_id);
    VERIFY(fid2, FAIL, "H5Fopen");

    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_DEFAULT);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    /* should succeed */
    fid2 = H5Fopen(FILE1, H5F_ACC_RDWR, fapl_id);
    CHECK(fid2, FAIL, "H5Fopen");

    /* Close first open */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close second open */
    ret = H5Fclose(fid2);
    CHECK(ret, FAIL, "H5Fclose");


    /* Test behavior while opening file multiple times with different file
     * close degree
     */
    fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_WEAK);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    ret = H5Pget_fclose_degree(fapl_id, &fc_degree);
    VERIFY(fc_degree, H5F_CLOSE_WEAK, "H5Pget_fclose_degree");

    /* should succeed */
    fid2 = H5Fopen(FILE1, H5F_ACC_RDWR, fapl_id);
    CHECK(fid2, FAIL, "H5Fopen");

    /* Close first open */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close second open */
    ret = H5Fclose(fid2);
    CHECK(ret, FAIL, "H5Fclose");


    /* Test behavior while opening file multiple times with file close
     * degree STRONG */
    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_STRONG);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    CHECK(fid1, FAIL, "H5Fcreate");

    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_WEAK);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    /* should fail */
    fid2 = H5Fopen(FILE1, H5F_ACC_RDWR, fapl_id);
    VERIFY(fid2, FAIL, "H5Fopen");

    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_STRONG);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    /* should succeed */
    fid2 = H5Fopen(FILE1, H5F_ACC_RDWR, fapl_id);
    CHECK(fid2, FAIL, "H5Fopen");

    /* Create a dataset and a group in each file open respectively */
    create_objects(fid1, fid2, NULL, NULL, NULL, NULL);

    /* Close first open */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close second open */
    ret = H5Fclose(fid2);
    CHECK(ret, FAIL, "H5Fclose");


    /* Test behavior while opening file multiple times with file close
     * degree SEMI */
    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_SEMI);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    CHECK(fid1, FAIL, "H5Fcreate");

    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_DEFAULT);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    /* should fail */
    fid2 = H5Fopen(FILE1, H5F_ACC_RDWR, fapl_id);
    VERIFY(fid2, FAIL, "H5Fopen");

    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_SEMI);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    /* should succeed */
    fid2 = H5Fopen(FILE1, H5F_ACC_RDWR, fapl_id);
    CHECK(fid2, FAIL, "H5Fopen");

    /* Create a dataset and a group in each file open respectively */
    create_objects(fid1, fid2, &dataset_id, &group_id1, &group_id2, &group_id3);

    /* Close first open, should fail since it is SEMI and objects are
     * still open. */
    ret = H5Fclose(fid1);
    VERIFY(ret, FAIL, "H5Fclose");

    /* Close second open, should fail since it is SEMI and objects are
     * still open. */
    ret = H5Fclose(fid2);
    VERIFY(ret, FAIL, "H5Fclose");

    ret = H5Dclose(dataset_id);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close first open */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Gclose(group_id1);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Gclose(group_id2);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close second open, should fail since it is SEMI and one group ID is
     * still open. */
    ret = H5Fclose(fid2);
    VERIFY(ret, FAIL, "H5Fclose");

    /* Same check with H5Idec_ref() (should fail also) */
    ret = H5Idec_ref(fid2);
    VERIFY(ret, FAIL, "H5Idec_ref");

    ret = H5Gclose(group_id3);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close second open again.  Should succeed. */
    ret = H5Fclose(fid2);
    CHECK(ret, FAIL, "H5Fclose");


    /* Test behavior while opening file multiple times with file close
     * degree WEAK */
    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_WEAK);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    CHECK(fid1, FAIL, "H5Fcreate");

    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_SEMI);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    /* should fail */
    fid2 = H5Fopen(FILE1, H5F_ACC_RDWR, fapl_id);
    VERIFY(fid2, FAIL, "H5Fopen");

    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_DEFAULT);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    /* should succeed */
    fid2 = H5Fopen(FILE1, H5F_ACC_RDWR, fapl_id);
    CHECK(fid2, FAIL, "H5Fopen");

    /* Create a dataset and a group in each file open respectively */
    create_objects(fid1, fid2, &dataset_id, &group_id1, &group_id2, &group_id3);

    /* Create more new files and test object count and ID list functions */
    test_obj_count_and_id(fid1, fid2, dataset_id, group_id1,
                group_id2, group_id3);

    /* Close first open */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close second open.  File will be finally closed after all objects
     * are closed. */
    ret = H5Fclose(fid2);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Dclose(dataset_id);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Gclose(group_id1);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Gclose(group_id2);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Gclose(group_id3);
    CHECK(ret, FAIL, "H5Gclose");


    /* Test behavior while opening file multiple times with file close
     * degree DEFAULT */
    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_DEFAULT);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    CHECK(fid1, FAIL, "H5Fcreate");

    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_SEMI);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    /* should fail */
    fid2 = H5Fopen(FILE1, H5F_ACC_RDWR, fapl_id);
    VERIFY(fid2, FAIL, "H5Fopen");

    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_DEFAULT);
    CHECK(ret, FAIL, "H5Pset_fclose_degree");

    /* should succeed */
    fid2 = H5Fopen(FILE1, H5F_ACC_RDWR, fapl_id);
    CHECK(fid2, FAIL, "H5Fopen");

    /* Create a dataset and a group in each file open respectively */
    create_objects(fid1, fid2, &dataset_id, &group_id1, &group_id2, &group_id3);

    access_id = H5Fget_access_plist(fid1);
    CHECK(access_id, FAIL, "H5Fget_access_plist");

    ret= H5Pget_fclose_degree(access_id, &fc_degree);
    CHECK(ret, FAIL, "H5Pget_fclose_degree");

    switch(fc_degree) {
        case H5F_CLOSE_STRONG:
            /* Close first open */
            ret = H5Fclose(fid1);
            CHECK(ret, FAIL, "H5Fclose");
            /* Close second open */
            ret = H5Fclose(fid2);
            CHECK(ret, FAIL, "H5Fclose");
            break;
        case H5F_CLOSE_SEMI:
            /* Close first open */
            ret = H5Fclose(fid1);
            CHECK(ret, FAIL, "H5Fclose");
            ret = H5Dclose(dataset_id);
            CHECK(ret, FAIL, "H5Dclose");
            ret = H5Gclose(group_id1);
            CHECK(ret, FAIL, "H5Gclose");
            ret = H5Gclose(group_id2);
            CHECK(ret, FAIL, "H5Gclose");
            ret = H5Gclose(group_id3);
            CHECK(ret, FAIL, "H5Gclose");
            /* Close second open */
            ret = H5Fclose(fid2);
            CHECK(ret, FAIL, "H5Fclose");
            break;
        case H5F_CLOSE_WEAK:
            /* Close first open */
            ret = H5Fclose(fid1);
            CHECK(ret, FAIL, "H5Fclose");
            /* Close second open */
            ret = H5Fclose(fid2);
            CHECK(ret, FAIL, "H5Fclose");
            ret = H5Dclose(dataset_id);
            CHECK(ret, FAIL, "H5Dclose");
            ret = H5Gclose(group_id1);
            CHECK(ret, FAIL, "H5Gclose");
            ret = H5Gclose(group_id2);
            CHECK(ret, FAIL, "H5Gclose");
            ret = H5Gclose(group_id3);
            CHECK(ret, FAIL, "H5Gclose");
            break;
        case H5F_CLOSE_DEFAULT:
        default:
            CHECK(fc_degree, H5F_CLOSE_DEFAULT, "H5Pget_fclose_degree");
            break;
    }

    /* Close file access property list */
    ret = H5Pclose(fapl_id);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(access_id);
    CHECK(ret, FAIL, "H5Pclose");
}

/****************************************************************
**
**  create_objects(): routine called by test_file_close to create
**                    a dataset and a group in file.
**
****************************************************************/
static void
create_objects(hid_t fid1, hid_t fid2, hid_t *ret_did, hid_t *ret_gid1,
        hid_t *ret_gid2, hid_t *ret_gid3)
{
    ssize_t    oid_count;
    herr_t    ret;

    /* Check reference counts of file IDs and opened object IDs.
     * The verification is hard-coded.  If in any case, this testing
     * is changed, remember to check this part and update the macros.
     */
    {
       oid_count = H5Fget_obj_count(fid1, H5F_OBJ_ALL);
       CHECK(oid_count, FAIL, "H5Fget_obj_count");
       VERIFY(oid_count, OBJ_ID_COUNT_2, "H5Fget_obj_count");

       oid_count = H5Fget_obj_count(fid1, H5F_OBJ_DATASET|H5F_OBJ_GROUP|H5F_OBJ_DATATYPE|H5F_OBJ_ATTR);
       CHECK(oid_count, FAIL, "H5Fget_obj_count");
       VERIFY(oid_count, OBJ_ID_COUNT_0, "H5Fget_obj_count");

       oid_count = H5Fget_obj_count(fid2, H5F_OBJ_ALL);
       CHECK(oid_count, FAIL, "H5Fget_obj_count");
       VERIFY(oid_count, OBJ_ID_COUNT_2, "H5Fget_obj_count");

       oid_count = H5Fget_obj_count(fid2, H5F_OBJ_DATASET|H5F_OBJ_GROUP|H5F_OBJ_DATATYPE|H5F_OBJ_ATTR);
       CHECK(oid_count, FAIL, "H5Fget_obj_count");
       VERIFY(oid_count, OBJ_ID_COUNT_0, "H5Fget_obj_count");
    }

    /* create a dataset in the first file open */
    {
       hid_t       dataset_id, dataspace_id;  /* identifiers */
       hsize_t     dims[F2_RANK];
       unsigned    data[F2_DIM0][F2_DIM1];
       unsigned    i,j;

       /* Create the data space for the dataset. */
       dims[0] = F2_DIM0;
       dims[1] = F2_DIM1;
       dataspace_id = H5Screate_simple(F2_RANK, dims, NULL);
       CHECK(dataspace_id, FAIL, "H5Screate_simple");

       /* Create the dataset. */
       dataset_id = H5Dcreate2(fid1, "/dset", H5T_NATIVE_UINT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
       CHECK(dataset_id, FAIL, "H5Dcreate2");

       for(i = 0; i < F2_DIM0; i++)
           for(j = 0; j < F2_DIM1; j++)
               data[i][j] = i * 10 + j;

       /* Write data to the new dataset */
       ret = H5Dwrite(dataset_id, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
       CHECK(ret, FAIL, "H5Dwrite");

       if(ret_did != NULL)
           *ret_did = dataset_id;

       /* Terminate access to the data space. */
       ret = H5Sclose(dataspace_id);
       CHECK(ret, FAIL, "H5Sclose");
    }

    /* Create a group in the second file open */
    {
        hid_t   gid1, gid2, gid3;
        gid1 = H5Gcreate2(fid2, "/group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(gid1, FAIL, "H5Gcreate2");
        if(ret_gid1 != NULL)
            *ret_gid1 = gid1;

        gid2 = H5Gopen2(fid2, "/group", H5P_DEFAULT);
        CHECK(gid2, FAIL, "H5Gopen2");
        if(ret_gid2 != NULL)
            *ret_gid2 = gid2;

        gid3 = H5Gopen2(fid2, "/group", H5P_DEFAULT);
        CHECK(gid3, FAIL, "H5Gopen2");
        if(ret_gid3 != NULL)
            *ret_gid3 = gid3;
    }

    /* Check reference counts of file IDs and opened object IDs.
     * The verification is hard-coded.  If in any case, this testing
     * is changed, remember to check this part and update the macros.
     */
    {
       oid_count = H5Fget_obj_count(fid1, H5F_OBJ_ALL);
       CHECK(oid_count, FAIL, "H5Fget_obj_count");
       VERIFY(oid_count, OBJ_ID_COUNT_6, "H5Fget_obj_count");

       oid_count = H5Fget_obj_count(fid1, H5F_OBJ_DATASET|H5F_OBJ_GROUP|H5F_OBJ_DATATYPE|H5F_OBJ_ATTR);
       CHECK(oid_count, FAIL, "H5Fget_obj_count");
       VERIFY(oid_count, OBJ_ID_COUNT_4, "H5Fget_obj_count");

       oid_count = H5Fget_obj_count(fid2, H5F_OBJ_ALL);
       CHECK(oid_count, FAIL, "H5Fget_obj_count");
       VERIFY(oid_count, OBJ_ID_COUNT_6, "H5Fget_obj_count");

       oid_count = H5Fget_obj_count(fid2, H5F_OBJ_DATASET|H5F_OBJ_GROUP|H5F_OBJ_DATATYPE|H5F_OBJ_ATTR);
       CHECK(oid_count, FAIL, "H5Fget_obj_count");
       VERIFY(oid_count, OBJ_ID_COUNT_4, "H5Fget_obj_count");
    }
}

/****************************************************************
**
**  test_get_obj_ids(): Test the bug and the fix for Jira 8528.
**                      H5Fget_obj_ids overfilled the list of
**                      object IDs by one.  This is an enhancement
**                      for test_obj_count_and_id().
**
****************************************************************/
static void
test_get_obj_ids(void)
{
    hid_t    fid, gid[NGROUPS], dset[NDSETS];
    hid_t    filespace;
    hsize_t  file_dims[F2_RANK] = {F2_DIM0, F2_DIM1};
    ssize_t  oid_count, ret_count;
    hid_t *oid_list = NULL;
    herr_t   ret;
    int i, m, n;
    ssize_t oid_list_size = NDSETS;
    char gname[64], dname[64];

    /* Create a new file */
    fid = H5Fcreate(FILE7, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    filespace = H5Screate_simple(F2_RANK, file_dims,  NULL);
    CHECK(filespace, FAIL, "H5Screate_simple");

    /* creates NGROUPS groups under the root group */
    for(m = 0; m < NGROUPS; m++) {
        HDsprintf(gname, "group%d", m);
        gid[m] = H5Gcreate2(fid, gname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(gid[m], FAIL, "H5Gcreate2");
    }

    /* create NDSETS datasets under the root group */
    for(n = 0; n < NDSETS; n++) {
         HDsprintf(dname, "dataset%d", n);
         dset[n] = H5Dcreate2(fid, dname, H5T_NATIVE_INT, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
         CHECK(dset[n], FAIL, "H5Dcreate2");
    }

    /* The number of opened objects should be NGROUPS + NDSETS + 1.  One is opened file. */
    oid_count = H5Fget_obj_count(fid, H5F_OBJ_ALL);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, (NGROUPS + NDSETS + 1), "H5Fget_obj_count");

    oid_list = (hid_t *)HDcalloc((size_t)oid_list_size, sizeof(hid_t));
    CHECK_PTR(oid_list, "HDcalloc");

    /* Call the public function H5F_get_obj_ids to use H5F_get_objects.  User reported having problem here.
     * that the returned size (ret_count) from H5Fget_obj_ids is one greater than the size passed in
     * (oid_list_size) */
    ret_count = H5Fget_obj_ids(fid, H5F_OBJ_ALL, (size_t)oid_list_size, oid_list);
    CHECK(ret_count, FAIL, "H5Fget_obj_ids");
    VERIFY(ret_count, oid_list_size, "H5Fget_obj_count");

    /* Close all object IDs on the list except the file ID. The first ID is supposed to be file ID according
     * to the library design */
    for(i = 0; i< ret_count; i++) {
        if(fid != oid_list[i]) {
            ret = H5Oclose(oid_list[i]);
            CHECK(ret, FAIL, "H5Oclose");
        }
    }

    /* The number of opened objects should be NGROUPS + 1 + 1.  The first one is opened file. The second one
     * is the dataset ID left open from the previous around of H5Fget_obj_ids */
    oid_count = H5Fget_obj_count(fid, H5F_OBJ_ALL);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, NGROUPS + 2, "H5Fget_obj_count");

    /* Get the IDs of the left opend objects */
    ret_count = H5Fget_obj_ids(fid, H5F_OBJ_ALL, (size_t)oid_list_size, oid_list);
    CHECK(ret_count, FAIL, "H5Fget_obj_ids");
    VERIFY(ret_count, oid_list_size, "H5Fget_obj_count");

    /* Close all object IDs on the list except the file ID. The first ID is still the file ID */
    for(i = 0; i< ret_count; i++) {
        if(fid != oid_list[i]) {
            ret = H5Oclose(oid_list[i]);
            CHECK(ret, FAIL, "H5Oclose");
        }
    }

    H5Sclose(filespace);
    H5Fclose(fid);

    HDfree(oid_list);

    /* Reopen the file to check whether H5Fget_obj_count and H5Fget_obj_ids still works
     * when the file is closed first */
    fid = H5Fopen(FILE7, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open NDSETS datasets under the root group */
    for(n = 0; n < NDSETS; n++) {
         HDsprintf(dname, "dataset%d", n);
         dset[n] = H5Dopen2(fid, dname, H5P_DEFAULT);
         CHECK(dset[n], FAIL, "H5Dcreate2");
    }

    /* Close the file first */
    H5Fclose(fid);

    /* Get the number of all opened objects */
    oid_count = H5Fget_obj_count((hid_t)H5F_OBJ_ALL, H5F_OBJ_ALL);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, NDSETS, "H5Fget_obj_count");

    oid_list = (hid_t *)HDcalloc((size_t)oid_count, sizeof(hid_t));
    CHECK_PTR(oid_list, "HDcalloc");

    /* Get the list of all opened objects */
    ret_count = H5Fget_obj_ids((hid_t)H5F_OBJ_ALL, H5F_OBJ_ALL, (size_t)oid_count, oid_list);
    CHECK(ret_count, FAIL, "H5Fget_obj_ids");
    VERIFY(ret_count, NDSETS, "H5Fget_obj_count");

    /* Close all open objects with H5Oclose */
    for(n = 0; n < oid_count; n++)
         H5Oclose(oid_list[n]);

    HDfree(oid_list);
}

/****************************************************************
**
**  test_get_file_id(): Test H5Iget_file_id()
**
*****************************************************************/
static void
test_get_file_id(void)
{
    hid_t               fid, fid2, fid3;
    hid_t        datatype_id, dataset_id, dataspace_id, group_id, attr_id;
    hid_t               plist;
    hsize_t             dims[F2_RANK];
    unsigned            intent;
    herr_t              ret;

    /* Create a file */
    fid = H5Fcreate(FILE4, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Check the intent */
    ret = H5Fget_intent(fid, &intent);
    CHECK(ret, FAIL, "H5Fget_intent");
    VERIFY(intent, H5F_ACC_RDWR, "H5Fget_intent");

    /* Test H5Iget_file_id() */
    check_file_id(fid, fid);

    /* Create a group in the file.  Make a duplicated file ID from the group.
     * And close this duplicated ID
     */
    group_id = H5Gcreate2(fid, GRP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group_id, FAIL, "H5Gcreate2");

    /* Test H5Iget_file_id() */
    check_file_id(fid, group_id);

    /* Close the file and get file ID from the group ID */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Test H5Iget_file_id() */
    check_file_id((hid_t)-1, group_id);

    ret = H5Gclose(group_id);
    CHECK(ret, FAIL, "H5Gclose");

    /* Open the file again.  Test H5Iget_file_id() */
    fid = H5Fopen(FILE4, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    group_id = H5Gopen2(fid, GRP_NAME, H5P_DEFAULT);
    CHECK(group_id, FAIL, "H5Gopen2");

    /* Test H5Iget_file_id() */
    check_file_id(fid, group_id);

    /* Open the file for second time.  Test H5Iget_file_id() */
    fid3 = H5Freopen(fid);
    CHECK(fid3, FAIL, "H5Freopen");

    /* Test H5Iget_file_id() */
    check_file_id(fid3, fid3);

    ret = H5Fclose(fid3);
    CHECK(ret, FAIL, "H5Fclose");

    /* Create a dataset in the group.  Make a duplicated file ID from the
     * dataset.  And close this duplicated ID.
     */
    dims[0] = F2_DIM0;
    dims[1] = F2_DIM1;
    dataspace_id = H5Screate_simple(F2_RANK, dims, NULL);
    CHECK(dataspace_id, FAIL, "H5Screate_simple");

    dataset_id = H5Dcreate2(group_id, DSET_NAME, H5T_NATIVE_INT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset_id, FAIL, "H5Dcreate2");

    /* Test H5Iget_file_id() */
    check_file_id(fid, dataset_id);

    /* Create an attribute for the dataset.  Make a duplicated file ID from
     * this attribute.  And close it.
     */
    attr_id = H5Acreate2(dataset_id, ATTR_NAME, H5T_NATIVE_INT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Acreate2");

    /* Test H5Iget_file_id() */
    check_file_id(fid, attr_id);

    /* Create a named datatype.  Make a duplicated file ID from
     * this attribute.  And close it.
     */
    datatype_id = H5Tcopy(H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tcopy");

    ret = H5Tcommit2(fid, TYPE_NAME, datatype_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Test H5Iget_file_id() */
    check_file_id(fid, datatype_id);

    /* Create a property list and try to get file ID from it.
     * Supposed to fail.
     */
    plist = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(plist, FAIL, "H5Pcreate");

    H5E_BEGIN_TRY {
        fid2 = H5Iget_file_id(plist);
    } H5E_END_TRY;
    VERIFY(fid2, FAIL, "H5Iget_file_id");

    /* Close objects */
    ret = H5Pclose(plist);
    CHECK(ret, FAIL, "H5Pclose");

    ret = H5Tclose(datatype_id);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Aclose(attr_id);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Sclose(dataspace_id);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Dclose(dataset_id);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Gclose(group_id);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}

/****************************************************************
**
**  check_file_id(): Internal function of test_get_file_id()
**
*****************************************************************/
static void
check_file_id(hid_t fid, hid_t object_id)
{
    hid_t               new_fid;
    herr_t              ret;

    /* Return a duplicated file ID even not expecting user to do it.
     * And close this duplicated ID
     */
    new_fid = H5Iget_file_id(object_id);

    if(fid >=0)
        VERIFY(new_fid, fid, "H5Iget_file_id");
    else
        CHECK(new_fid, FAIL, "H5Iget_file_id");

    ret = H5Fclose(new_fid);
    CHECK(ret, FAIL, "H5Fclose");
}

/****************************************************************
**
**  test_obj_count_and_id(): test object count and ID list functions.
**
****************************************************************/
static void
test_obj_count_and_id(hid_t fid1, hid_t fid2, hid_t did, hid_t gid1,
            hid_t gid2, hid_t gid3)
{
    hid_t    fid3, fid4;
    ssize_t  oid_count, ret_count;
    herr_t   ret;

    /* Create two new files */
    fid3 = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid3, FAIL, "H5Fcreate");
    fid4 = H5Fcreate(FILE3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid4, FAIL, "H5Fcreate");

    /* test object count of all files IDs open */
    oid_count = H5Fget_obj_count((hid_t)H5F_OBJ_ALL, H5F_OBJ_FILE);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_4, "H5Fget_obj_count");

    /* test object count of all datasets open */
    oid_count = H5Fget_obj_count((hid_t)H5F_OBJ_ALL, H5F_OBJ_DATASET);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_1, "H5Fget_obj_count");

    /* test object count of all groups open */
    oid_count = H5Fget_obj_count((hid_t)H5F_OBJ_ALL, H5F_OBJ_GROUP);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_3, "H5Fget_obj_count");

    /* test object count of all named datatypes open */
    oid_count = H5Fget_obj_count((hid_t)H5F_OBJ_ALL, H5F_OBJ_DATATYPE);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_0, "H5Fget_obj_count");

    /* test object count of all attributes open */
    oid_count = H5Fget_obj_count((hid_t)H5F_OBJ_ALL, H5F_OBJ_ATTR);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_0, "H5Fget_obj_count");

    /* test object count of all objects currently open */
    oid_count = H5Fget_obj_count((hid_t)H5F_OBJ_ALL, H5F_OBJ_ALL);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_8, "H5Fget_obj_count");

    if(oid_count > 0) {
        hid_t *oid_list;

        oid_list = (hid_t *)HDcalloc((size_t)oid_count, sizeof(hid_t));
        if(oid_list != NULL) {
            int   i;

        ret_count = H5Fget_obj_ids((hid_t)H5F_OBJ_ALL, H5F_OBJ_ALL, (size_t)oid_count, oid_list);
        CHECK(ret_count, FAIL, "H5Fget_obj_ids");

            for(i = 0; i < oid_count; i++) {
                H5I_type_t id_type;

                id_type = H5Iget_type(oid_list[i]);
                switch(id_type) {
                    case H5I_FILE:
                        if(oid_list[i] != fid1 && oid_list[i] != fid2
                                && oid_list[i] != fid3 && oid_list[i] != fid4)
                            ERROR("H5Fget_obj_ids");
                        break;

                    case H5I_GROUP:
                        if(oid_list[i] != gid1 && oid_list[i] != gid2
                                && oid_list[i] != gid3)
                            ERROR("H5Fget_obj_ids");
                        break;

                    case H5I_DATASET:
                        VERIFY(oid_list[i], did, "H5Fget_obj_ids");
                        break;

                    case H5I_UNINIT:
                    case H5I_BADID:
                    case H5I_DATATYPE:
                    case H5I_DATASPACE:
                    case H5I_ATTR:
                    case H5I_REFERENCE:
                    case H5I_VFL:
                    case H5I_GENPROP_CLS:
                    case H5I_GENPROP_LST:
                    case H5I_ERROR_CLASS:
                    case H5I_ERROR_MSG:
                    case H5I_ERROR_STACK:
                    case H5I_NTYPES:
                    default:
                        ERROR("H5Fget_obj_ids");
                } /* end switch */
            } /* end for */

            HDfree(oid_list);
        } /* end if */
    } /* end if */

    /* close the two new files */
    ret = H5Fclose(fid3);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Fclose(fid4);
    CHECK(ret, FAIL, "H5Fclose");
}

/****************************************************************
**
**  test_file_perm(): low-level file test routine.
**      This test verifies that a file can be opened for both
**      read-only and read-write access and things will be handled
**      appropriately.
**
*****************************************************************/
static void
test_file_perm(void)
{
    hid_t    file;      /* File opened with read-write permission */
    hid_t    filero;    /* Same file opened with read-only permission */
    hid_t    dspace;    /* Dataspace ID */
    hid_t    dset;      /* Dataset ID */
    herr_t   ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Low-Level File Permissions\n"));

    dspace = H5Screate(H5S_SCALAR);
    CHECK(dspace, FAIL, "H5Screate");

    /* Create the file (with read-write permission) */
    file = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Create a dataset with the read-write file handle */
    dset = H5Dcreate2(file, F2_DSET, H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate2");

    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Open the file (with read-only permission) */
    filero = H5Fopen(FILE2, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(filero, FAIL, "H5Fopen");

    /* Create a dataset with the read-only file handle (should fail) */
    H5E_BEGIN_TRY {
        dset = H5Dcreate2(filero, F2_DSET, H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(dset, FAIL, "H5Dcreate2");
    if(dset!=FAIL) {
        ret = H5Dclose(dset);
        CHECK(ret, FAIL, "H5Dclose");
    } /* end if */

    ret = H5Fclose(filero);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Sclose(dspace);
    CHECK(ret, FAIL, "H5Sclose");

} /* end test_file_perm() */

/****************************************************************
**
**  test_file_perm2(): low-level file test routine.
**      This test verifies that no object can be created in a
**      file that is opened for read-only.
**
*****************************************************************/
static void
test_file_perm2(void)
{
    hid_t    file;      /* File opened with read-write permission */
    hid_t    filero;    /* Same file opened with read-only permission */
    hid_t    dspace;    /* Dataspace ID */
    hid_t    group;     /* Group ID */
    hid_t    dset;      /* Dataset ID */
    hid_t    type;      /* Datatype ID */
    hid_t    attr;      /* Attribute ID */
    herr_t   ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Low-Level File Permissions again\n"));

    dspace = H5Screate(H5S_SCALAR);
    CHECK(dspace, FAIL, "H5Screate");

    /* Create the file (with read-write permission) */
    file = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Open the file (with read-only permission) */
    filero = H5Fopen(FILE2, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(filero, FAIL, "H5Fopen");

    /* Create a group with the read-only file handle (should fail) */
    H5E_BEGIN_TRY {
        group = H5Gcreate2(filero, "MY_GROUP", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(group, FAIL, "H5Gcreate2");

    /* Create a dataset with the read-only file handle (should fail) */
    H5E_BEGIN_TRY {
        dset = H5Dcreate2(filero, F2_DSET, H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(dset, FAIL, "H5Dcreate2");

    /* Create an attribute with the read-only file handle (should fail) */
    H5E_BEGIN_TRY {
        attr = H5Acreate2(filero, "MY_ATTR", H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(attr, FAIL, "H5Acreate2");

    type = H5Tcopy(H5T_NATIVE_SHORT);
    CHECK(type, FAIL, "H5Tcopy");

    /* Commit a datatype with the read-only file handle (should fail) */
    H5E_BEGIN_TRY {
        ret = H5Tcommit2(filero, "MY_DTYPE", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Tcommit2");

    ret = H5Tclose(type);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Fclose(filero);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Sclose(dspace);
    CHECK(ret, FAIL, "H5Sclose");
} /* end test_file_perm2() */


/****************************************************************
**
**  test_file_ishdf5(): low-level file test routine.
**      This test checks whether the H5Fis_hdf5() routine is working
**      correctly in variuous situations.
**
*****************************************************************/
static void
test_file_ishdf5(void)
{
    hid_t    file;      /* File opened with read-write permission */
    hid_t    fcpl;      /* File creation property list */
    int      fd;        /* File Descriptor */
    ssize_t  nbytes;    /* Number of bytes written */
    unsigned u;         /* Local index variable */
    unsigned char buf[1024];    /* Buffer of data to write */
    htri_t   status;    /* Whether a file is an HDF5 file */
    herr_t   ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Detection of HDF5 Files\n"));

    /* Create a file */
    file = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Verify that the file is an HDF5 file */
    status = H5Fis_hdf5(FILE1);
    VERIFY(status, TRUE, "H5Fis_hdf5");


    /* Create a file creation property list with a non-default user block size */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");

    ret = H5Pset_userblock(fcpl, (hsize_t)2048);
    CHECK(ret, FAIL, "H5Pset_userblock");

    /* Create file with non-default user block */
    file = H5Fcreate(FILE1, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Release file-creation property list */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Verify that the file is an HDF5 file */
    status = H5Fis_hdf5(FILE1);
    VERIFY(status, TRUE, "H5Fis_hdf5");


    /* Create non-HDF5 file and check it */
    fd = HDopen(FILE1, O_RDWR|O_CREAT|O_TRUNC, H5_POSIX_CREATE_MODE_RW);
    CHECK(fd, FAIL, "HDopen");

    /* Initialize information to write */
    for(u = 0; u < 1024; u++)
        buf[u]=(unsigned char)u;

    /* Write some information */
    nbytes = HDwrite(fd, buf, (size_t)1024);
    VERIFY(nbytes, 1024, "HDwrite");

    /* Close the file */
    ret = HDclose(fd);
    CHECK(ret, FAIL, "HDclose");

    /* Verify that the file is not an HDF5 file */
    status = H5Fis_hdf5(FILE1);
    VERIFY(status, FALSE, "H5Fis_hdf5");

} /* end test_file_ishdf5() */

/****************************************************************
**
**  test_file_open_dot(): low-level file test routine.
**      This test checks whether opening objects with "." for a name
**      works correctly in variuous situations.
**
*****************************************************************/
static void
test_file_open_dot(void)
{
    hid_t fid;          /* File ID */
    hid_t gid, gid2;    /* Group IDs */
    hid_t did;          /* Dataset ID */
    hid_t sid;          /* Dataspace ID */
    hid_t tid, tid2;    /* Datatype IDs */
    herr_t   ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing opening objects with \".\" for a name\n"));

    /* Create a new HDF5 file to work with */
    fid = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create a group in the HDF5 file */
    gid = H5Gcreate2(fid, GRP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    /* Create a dataspace for creating datasets */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create a dataset with no name using the file ID */
    H5E_BEGIN_TRY {
        did = H5Dcreate2(fid, ".", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(did, FAIL, "H5Dcreate2");

    /* Create a dataset with no name using the group ID */
    H5E_BEGIN_TRY {
        did = H5Dcreate2(gid, ".", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(did, FAIL, "H5Dcreate2");

    /* Open a dataset with no name using the file ID */
    H5E_BEGIN_TRY {
        did = H5Dopen2(fid, ".", H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(did, FAIL, "H5Dopen2");

    /* Open a dataset with no name using the group ID */
    H5E_BEGIN_TRY {
        did = H5Dopen2(gid, ".", H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(did, FAIL, "H5Dopen2");

    /* Make a copy of a datatype to use for creating a named datatype */
    tid = H5Tcopy(H5T_NATIVE_INT);
    CHECK(tid, FAIL, "H5Tcopy");

    /* Create a named datatype with no name using the file ID */
    H5E_BEGIN_TRY {
        ret = H5Tcommit2(fid, ".", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Tcommit2");

    /* Create a named datatype with no name using the group ID */
    H5E_BEGIN_TRY {
        ret = H5Tcommit2(gid, ".", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Tcommit2");

    /* Open a named datatype with no name using the file ID */
    H5E_BEGIN_TRY {
        tid2 = H5Topen2(fid, ".", H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tid2, FAIL, "H5Topen2");

    /* Open a named datatype with no name using the group ID */
    H5E_BEGIN_TRY {
        tid2 = H5Topen2(gid, ".", H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(tid2, FAIL, "H5Topen2");

    /* Create a group with no name using the file ID */
    H5E_BEGIN_TRY {
        gid2 = H5Gcreate2(fid, ".", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(gid2, FAIL, "H5Gcreate2");

    /* Create a group with no name using the group ID */
    H5E_BEGIN_TRY {
        gid2 = H5Gcreate2(gid, ".", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(gid2, FAIL, "H5Gcreate2");

    /* Open a group with no name using the file ID (should open the root group) */
    gid2 = H5Gopen2(fid, ".", H5P_DEFAULT);
    CHECK(gid2, FAIL, "H5Gopen2");

    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");

    /* Open a group with no name using the group ID (should open the group again) */
    gid2 = H5Gopen2(gid, ".", H5P_DEFAULT);
    CHECK(gid2, FAIL, "H5Gopen2");

    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");


    /* Close everything */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_file_open_dot() */

/****************************************************************
**
**  test_file_open_overlap(): low-level file test routine.
**      This test checks whether opening files in an overlapping way
**      (as opposed to a nested manner) works correctly.
**
*****************************************************************/
static void
test_file_open_overlap(void)
{
    hid_t fid1, fid2;
    hid_t did1, did2;
    hid_t gid;
    hid_t sid;
    ssize_t nobjs;      /* # of open objects */
    unsigned intent;
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing opening overlapping file opens\n"));

    /* Create file */
    fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Open file also */
    fid2 = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid2, FAIL, "H5Fopen");

    /* Check the intent */
    ret = H5Fget_intent(fid1, &intent);
    CHECK(ret, FAIL, "H5Fget_intent");
    VERIFY(intent, H5F_ACC_RDWR, "H5Fget_intent");

    /* Create a group in file */
    gid = H5Gcreate2(fid1, GROUP1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create dataset in group w/first file ID */
    did1 = H5Dcreate2(gid, DSET1, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did1, FAIL, "H5Dcreate2");

    /* Check number of objects opened in first file */
    nobjs = H5Fget_obj_count(fid1, H5F_OBJ_LOCAL|H5F_OBJ_ALL);
    VERIFY(nobjs, 3, "H5Fget_obj_count");       /* 3 == file, dataset & group */

    /* Close dataset */
    ret = H5Dclose(did1);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close first file ID */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");


    /* Create dataset with second file ID */
    did2 = H5Dcreate2(fid2, DSET2, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did2, FAIL, "H5Dcreate2");

    /* Check number of objects opened in first file */
    nobjs = H5Fget_obj_count(fid2, H5F_OBJ_ALL);
    VERIFY(nobjs, 2, "H5Fget_obj_count");       /* 3 == file & dataset */

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close second dataset */
    ret = H5Dclose(did2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close second file */
    ret = H5Fclose(fid2);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_file_open_overlap() */

/****************************************************************
**
**  test_file_getname(): low-level file test routine.
**      This test checks whether H5Fget_name works correctly.
**
*****************************************************************/
static void
test_file_getname(void)
{
    /* Compound datatype */
    typedef struct s1_t {
        unsigned int a;
        float        b;
    } s1_t;

    hid_t   file_id;
    hid_t   group_id;
    hid_t   dataset_id;
    hid_t   space_id;
    hid_t   type_id;
    hid_t   attr_id;
    hsize_t dims[TESTA_RANK] = {TESTA_NX, TESTA_NY};
    char    name[TESTA_NAME_BUF_SIZE];
    ssize_t name_len;
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing H5Fget_name() functionality\n"));

    /* Create a new file_id using default properties. */
    file_id = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );
    CHECK(file_id, FAIL, "H5Fcreate");

    /* Get and verify file name */
    name_len = H5Fget_name(file_id, name, (size_t)TESTA_NAME_BUF_SIZE);
    CHECK(name_len, FAIL, "H5Fget_name");
    VERIFY_STR(name, FILE1, "H5Fget_name");

    /* Create a group in the root group */
    group_id = H5Gcreate2(file_id, TESTA_GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group_id, FAIL, "H5Gcreate2");

    /* Get and verify file name */
    name_len = H5Fget_name(group_id, name, (size_t)TESTA_NAME_BUF_SIZE);
    CHECK(name_len, FAIL, "H5Fget_name");
    VERIFY_STR(name, FILE1, "H5Fget_name");

    /* Create the data space  */
    space_id = H5Screate_simple(TESTA_RANK, dims, NULL);
    CHECK(space_id, FAIL, "H5Screate_simple");

    /* Try get file name from data space.  Supposed to fail because
     * it's illegal operation. */
    H5E_BEGIN_TRY {
        name_len = H5Fget_name(space_id, name, (size_t)TESTA_NAME_BUF_SIZE);
    } H5E_END_TRY;
    VERIFY(name_len, FAIL, "H5Fget_name");

    /* Create a new dataset */
    dataset_id = H5Dcreate2(file_id, TESTA_DSETNAME, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset_id, FAIL, "H5Dcreate2");

    /* Get and verify file name */
    name_len = H5Fget_name(dataset_id, name, (size_t)TESTA_NAME_BUF_SIZE);
    CHECK(name_len, FAIL, "H5Fget_name");
    VERIFY_STR(name, FILE1, "H5Fget_name");

    /* Create an attribute for the dataset */
    attr_id = H5Acreate2(dataset_id, TESTA_ATTRNAME, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr_id, FAIL, "H5Acreate2");

    /* Get and verify file name */
    name_len = H5Fget_name(attr_id, name, (size_t)TESTA_NAME_BUF_SIZE);
    CHECK(name_len, FAIL, "H5Fget_name");
    VERIFY_STR(name, FILE1, "H5Fget_name");

    /* Create a compound datatype */
    type_id = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(type_id, FAIL, "H5Tcreate");

    /* Insert fields */
    ret = H5Tinsert (type_id, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert (type_id, "b", HOFFSET(s1_t,b), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Save it on file */
    ret = H5Tcommit2(file_id, TESTA_DTYPENAME, type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Get and verify file name */
    name_len = H5Fget_name(type_id, name, (size_t)TESTA_NAME_BUF_SIZE);
    CHECK(name_len, FAIL, "H5Fget_name");
    VERIFY_STR(name, FILE1, "H5Fget_name");

    /* Close things down */
    ret = H5Tclose(type_id);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Aclose(attr_id);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Dclose(dataset_id);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(space_id);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Gclose(group_id);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(file_id);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_file_getname() */

/****************************************************************
**
**  test_file_double_root_open(): low-level file test routine.
**      This test checks whether opening the root group from two
**      different files works correctly.
**
*****************************************************************/
static void
test_file_double_root_open(void)
{
    hid_t file1_id, file2_id;
    hid_t grp1_id, grp2_id;
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing double root group open\n"));

    file1_id = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file1_id, FAIL, "H5Fcreate");
    file2_id = H5Fopen (FILE1, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file2_id, FAIL, "H5Fopen");

    grp1_id  = H5Gopen2(file1_id, "/", H5P_DEFAULT);
    CHECK(grp1_id, FAIL, "H5Gopen2");
    grp2_id  = H5Gopen2(file2_id, "/", H5P_DEFAULT);
    CHECK(grp2_id, FAIL, "H5Gopen2");

    /* Note "assymetric" close order */
    ret = H5Gclose(grp1_id);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Gclose(grp2_id);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(file1_id);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Fclose(file2_id);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_file_double_root_open() */

/****************************************************************
**
**  test_file_double_group_open(): low-level file test routine.
**      This test checks whether opening the same group from two
**      different files works correctly.
**
*****************************************************************/
static void
test_file_double_group_open(void)
{
    hid_t file1_id, file2_id;
    hid_t grp1_id, grp2_id;
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing double non-root group open\n"));

    file1_id = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file1_id, FAIL, "H5Fcreate");
    file2_id = H5Fopen (FILE1, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file2_id, FAIL, "H5Fopen");

    grp1_id  = H5Gcreate2(file1_id, GRP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(grp1_id, FAIL, "H5Gcreate2");
    grp2_id  = H5Gopen2(file2_id, GRP_NAME, H5P_DEFAULT);
    CHECK(grp2_id, FAIL, "H5Gopen2");

    /* Note "assymetric" close order */
    ret = H5Gclose(grp1_id);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Gclose(grp2_id);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(file1_id);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Fclose(file2_id);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_file_double_group_open() */

/****************************************************************
**
**  test_file_double_dataset_open(): low-level file test routine.
**      This test checks whether opening the same dataset from two
**      different files works correctly.
**
*****************************************************************/
static void
test_file_double_dataset_open(void)
{
    hid_t file1_id, file2_id;
    hid_t dset1_id, dset2_id;
    hid_t space_id;
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing double dataset open\n"));

    file1_id = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file1_id, FAIL, "H5Fcreate");
    file2_id = H5Fopen (FILE1, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file2_id, FAIL, "H5Fopen");

    /* Create dataspace for dataset */
    space_id = H5Screate(H5S_SCALAR);
    CHECK(space_id, FAIL, "H5Screate");

    dset1_id  = H5Dcreate2(file1_id, DSET_NAME, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset1_id, FAIL, "H5Dcreate2");
    dset2_id  = H5Dopen2(file2_id, DSET_NAME, H5P_DEFAULT);
    CHECK(dset2_id, FAIL, "H5Dopen2");

    /* Close "supporting" dataspace */
    ret = H5Sclose(space_id);
    CHECK(ret, FAIL, "H5Sclose");

    /* Note "assymetric" close order */
    ret = H5Dclose(dset1_id);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2_id);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Fclose(file1_id);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Fclose(file2_id);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_file_double_dataset_open() */

/****************************************************************
**
**  test_file_double_file_dataset_open():
**      This test checks multi-opens of files & datasets:
**    It simulates the multi-thread test program from DLS
**    which exposes the file pointer segmentation fault failure.
**    NOTE: The order on when the files and datasets are open/close
**    is important.
**
*****************************************************************/
static void
test_file_double_file_dataset_open(hbool_t new_format)
{
    hid_t fapl = -1;                /* File access property list */
    hid_t dcpl = -1;                /* Dataset creation property list */
    hid_t fid1 = -1, fid2 = -1;            /* File IDs */
    hid_t did1 = -1, did2 = -1;            /* Dataset IDs */
    hid_t sid1 = -1, sid2 = -1;            /* Dataspace IDs */
    hid_t tid1 = -1, tid2 = -1;            /* Datatype IDs */
    hsize_t dims[1] = {5}, dims2[2] = {1, 4};    /* Dimension sizes */
    hsize_t e_ext_dims[1] = {7};        /* Expanded dimension sizes */
    hsize_t s_ext_dims[1] = {3};        /* Shrunk dimension sizes */
    hsize_t max_dims0[1] = {8};         /* Maximum dimension sizes */
    hsize_t max_dims1[1] = {H5S_UNLIMITED};     /* Maximum dimesion sizes for extensible array index */
    hsize_t max_dims2[2] = {H5S_UNLIMITED, H5S_UNLIMITED};    /* Maximum dimension sizes for v2 B-tree index */
    hsize_t chunks[1] = {2}, chunks2[2] = {4, 5};        /* Chunk dimension sizes */
    hsize_t size;                               /* File size */
    char filename[FILENAME_LEN];    /* Filename to use */
    const char* data[] = {"String 1", "String 2", "String 3", "String 4", "String 5"};    /* Input Data */
    const char* e_data[] = {"String 1", "String 2", "String 3", "String 4", "String 5", "String 6", "String 7"};    /* Input Data */
    char* buffer[5];                /* Output buffer */
    int wbuf[4] = {1, 2, 3, 4};            /* Input data */
    herr_t ret;                     /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing double file and dataset open/close\n"));

    /* Setting up test file */
    fapl = h5_fileaccess();
    CHECK(fapl, FAIL, "H5Pcreate");
    if(new_format) {
        ret = H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
        CHECK(ret, FAIL, "H5Pset_libver_bounds");
    } /* end if */
    h5_fixname(FILE1, fapl, filename, sizeof filename);

    /* Create the test file */
    fid1 = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create a chunked dataset with fixed array indexing */
    sid1 = H5Screate_simple(1, dims, max_dims0);
    CHECK(sid1, FAIL, "H5Screate_simple");
    tid1 = H5Tcopy(H5T_C_S1);
    CHECK(tid1, FAIL, "H5Tcopy");
    ret = H5Tset_size(tid1, H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");
    ret = H5Pset_chunk(dcpl, 1, chunks);
    CHECK(ret, FAIL, "H5Pset_chunk");

    did1 = H5Dcreate2(fid1, "dset_fa", tid1, sid1, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did1, FAIL, "H5Dcreate2");

    /* Closing */
    ret = H5Dclose(did1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create a chunked dataset with extensible array indexing */
    sid1 = H5Screate_simple(1, dims, max_dims1);
    CHECK(sid1, FAIL, "H5Screate_simple");
    tid1 = H5Tcopy(H5T_C_S1);
    CHECK(tid1, FAIL, "H5Tcopy");
    ret = H5Tset_size(tid1, H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");
    ret = H5Pset_chunk(dcpl, 1, chunks);
    CHECK(ret, FAIL, "H5Pset_chunk");

    did1 = H5Dcreate2(fid1, "dset_ea", tid1, sid1, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did1, FAIL, "H5Dcreate2");

    /* Write to the dataset */
    ret = H5Dwrite(did1, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Closing */
    /* (Leave sid1 open for later use) */
    ret = H5Dclose(did1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create a chunked dataset with v2 btree indexing */
    sid2 = H5Screate_simple(2, dims2, max_dims2);
    CHECK(sid2, FAIL, "H5Screate_simple");

    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");
    ret = H5Pset_chunk(dcpl, 2, chunks2);
    CHECK(ret, FAIL, "H5Pset_chunk");

    did2 = H5Dcreate2(fid1, "dset_bt2", H5T_NATIVE_INT, sid2, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(did2, FAIL, "H5Dcreate2");

    /* Write to the dataset */
    ret = H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Closing */
    ret = H5Dclose(did2);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /*
     * Scenario 1
     */

    /* First file open */
    fid1 = H5Fopen(filename, H5F_ACC_RDWR, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* First file's dataset open */
    did1 = H5Dopen2(fid1, "/dset_fa", H5P_DEFAULT);
    CHECK(did1, FAIL, "H5Dopen2");

    tid1 = H5Tcopy(did1);
    CHECK(tid1, FAIL, "H5Tcopy");

    /* First file's dataset write */
    ret = H5Dwrite(did1, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Second file open */
    fid2 = H5Fopen(filename, H5F_ACC_RDWR, fapl);
    CHECK(fid2, FAIL, "H5Fopen");

    /* Second file's dataset open */
    did2 = H5Dopen2(fid2, "/dset_fa", H5P_DEFAULT );
    CHECK(did2, FAIL, "H5Dopen2");

    tid2 = H5Tcopy(did2);
    CHECK(tid2, FAIL, "H5Tcopy");

    /* First file's dataset close */
    ret = H5Dclose(did1);
    CHECK(ret, FAIL, "H5Dclose");

    /* First file close */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Second file's dataset write */
    ret = H5Dwrite(did2, tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Second file's dataset close */
    ret = H5Dclose(did2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Second file close */
    ret = H5Fclose(fid2);
    CHECK(ret, FAIL, "H5Fclose");

    /* Closing */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /*
     * Scenario 2
     */

    /* First file open */
    fid1 = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Second file open */
    fid2 = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
    CHECK(fid2, FAIL, "H5Fopen");

    /* Second file's dataset open */
    did2 = H5Dopen2(fid2, "/dset_ea", H5P_DEFAULT );
    CHECK(did2, FAIL, "H5Dopen2");

    tid2 = H5Tcopy(did2);
    CHECK(tid2, FAIL, "H5Tcopy");

    /* First file's dataset open */
    did1 = H5Dopen2(fid1, "/dset_ea", H5P_DEFAULT);
    CHECK(did1, FAIL, "H5Dopen2");

    tid1 = H5Tcopy(did1);
    CHECK(tid1, FAIL, "H5Tcopy");

    /* Second file's dataset read */
    HDmemset(buffer, 0, sizeof(char*) * 5);
    ret = H5Dread(did2, tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer);
    CHECK(ret, FAIL, "H5Dread");
    ret = H5Dvlen_reclaim(tid2, sid1, H5P_DEFAULT, buffer);
    CHECK(ret, FAIL, "H5Dvlen_reclaim");

    /* Second file's dataset close */
    ret = H5Dclose(did2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Second file close */
    ret = H5Fclose(fid2);
    CHECK(ret, FAIL, "H5Fclose");

    /* First file's dataset read */
    HDmemset(buffer, 0, sizeof(char*) * 5);
    ret = H5Dread(did1, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer);
    CHECK(ret, FAIL, "H5Dread");
    ret = H5Dvlen_reclaim(tid2, sid1, H5P_DEFAULT, buffer);
    CHECK(ret, FAIL, "H5Dvlen_reclaim");

    /* First file's dataset close */
    ret = H5Dclose(did1);
    CHECK(ret, FAIL, "H5Dclose");

    /* First file close */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Closing */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /*
     * Scenario 3
     */

    /* First file open */
    fid1 = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* First file's dataset open */
    did1 = H5Dopen2(fid1, "/dset_bt2", H5P_DEFAULT);
    CHECK(did1, FAIL, "H5Dopen2");

    /* First file's get storage size */
    size = H5Dget_storage_size(did1);
    CHECK(size, 0, "H5Dget_storage_size");

    /* Second file open */
    fid2 = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
    CHECK(fid2, FAIL, "H5Fopen");

    /* Second file's dataset open */
    did2 = H5Dopen2(fid2, "/dset_bt2", H5P_DEFAULT );
    CHECK(did2, FAIL, "H5Dopen2");

    /* First file's dataset close */
    ret = H5Dclose(did1);
    CHECK(ret, FAIL, "H5Dclose");

    /* First file close */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Second file's get storage size */
    size = H5Dget_storage_size(did2);
    CHECK(size, 0, "H5Dget_storage_size");

    /* Second file's dataset close */
    ret = H5Dclose(did2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Second file close */
    ret = H5Fclose(fid2);
    CHECK(ret, FAIL, "H5Fclose");

    /*
     * Scenario 4
     * --trigger H5AC_protect: Assertion `f->shared' failed
     *     from second call to H5Dset_extent->...H5D__earray_idx_remove->H5EA_get...H5EA__iblock_protect...H5AC_protect
     */
    /* First file open */
    fid1 = H5Fopen(filename, H5F_ACC_RDWR, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* First file's dataset open */
    did1 = H5Dopen2(fid1, "/dset_ea", H5P_DEFAULT);
    CHECK(did1, FAIL, "H5Dopen2");

    tid1 = H5Tcopy(did1);
    CHECK(tid1, FAIL, "H5Tcopy");

    /* Extend the dataset */
    ret = H5Dset_extent(did1, e_ext_dims);
    CHECK(ret, FAIL, "H5Dset_extent");

    /* Write to the dataset */
    ret = H5Dwrite(did1, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, e_data);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Second file open */
    fid2 = H5Fopen(filename, H5F_ACC_RDWR, fapl);
    CHECK(fid2, FAIL, "H5Fopen");

    /* Second file's dataset open */
    did2 = H5Dopen2(fid2, "/dset_ea", H5P_DEFAULT );
    CHECK(did2, FAIL, "H5Dopen2");

    /* First file's dataset close */
    ret = H5Dclose(did1);
    CHECK(ret, FAIL, "H5Dclose");

    /* First file close */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Shrink the dataset */
    ret = H5Dset_extent(did2, s_ext_dims);
    CHECK(ret, FAIL, "H5Dset_extent");

    /* Second file's dataset close */
    ret = H5Dclose(did2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Second file close */
    ret = H5Fclose(fid2);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close the data type */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close FAPL */
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");
} /* end test_file_double_dataset_open() */

/****************************************************************
**
**  test_file_double_datatype_open(): low-level file test routine.
**      This test checks whether opening the same named datatype from two
**      different files works correctly.
**
*****************************************************************/
static void
test_file_double_datatype_open(void)
{
    hid_t file1_id, file2_id;
    hid_t type1_id, type2_id;
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing double dataset open\n"));

    file1_id = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file1_id, FAIL, "H5Fcreate");
    file2_id = H5Fopen (FILE1, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file2_id, FAIL, "H5Fopen");

    type1_id  = H5Tcopy(H5T_NATIVE_INT);
    CHECK(type1_id, FAIL, "H5Tcopy");
    ret  = H5Tcommit2(file1_id, TYPE_NAME, type1_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");
    type2_id  = H5Topen2(file2_id, TYPE_NAME, H5P_DEFAULT);
    CHECK(type2_id, FAIL, "H5Topen2");

    /* Note "assymetric" close order */
    ret = H5Tclose(type1_id);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Tclose(type2_id);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Fclose(file1_id);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Fclose(file2_id);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_file_double_dataset_open() */

/****************************************************************
**
**  test_userblock_file_size(): low-level file test routine.
**      This test checks that the presence of a userblock
**      affects the file size in the expected manner, and that
**      the filesize is not changed by reopening the file.  It
**      creates two files which are identical except that one
**      contains a userblock, and verifies that their file sizes
**      differ exactly by the userblock size.
**
*****************************************************************/
static void
test_userblock_file_size(void)
{
    hid_t file1_id, file2_id;
    hid_t group1_id, group2_id;
    hid_t dset1_id, dset2_id;
    hid_t space_id;
    hid_t fcpl2_id;
    hsize_t dims[2] = {3, 4};
    hsize_t filesize1, filesize2, filesize;
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing file size with user block\n"));

    /* Create property list with userblock size set */
    fcpl2_id = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl2_id, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl2_id, USERBLOCK_SIZE);
    CHECK(ret, FAIL, "H5Pset_userblock");

    /* Create files.  Onyl file2 with have a userblock. */
    file1_id = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file1_id, FAIL, "H5Fcreate");
    file2_id = H5Fcreate(FILE2, H5F_ACC_TRUNC, fcpl2_id, H5P_DEFAULT);
    CHECK(file2_id, FAIL, "H5Fcreate");

    /* Create groups */
    group1_id = H5Gcreate2(file1_id, GROUP1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group1_id, FAIL, "H5Gcreate2");
    group2_id = H5Gcreate2(file2_id, GROUP1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group2_id, FAIL, "H5Gcreate2");

    /* Create dataspace */
    space_id = H5Screate_simple(2, dims, NULL);
    CHECK(space_id, FAIL, "H5Screate_simple");

    /* Create datasets */
    dset1_id = H5Dcreate2(file1_id, DSET2, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset1_id, FAIL, "H5Dcreate2");
    dset2_id = H5Dcreate2(file2_id, DSET2, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset2_id, FAIL, "H5Dcreate2");

    /* Close IDs */
    ret = H5Dclose(dset1_id);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2_id);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Sclose(space_id);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Gclose(group1_id);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Gclose(group2_id);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Pclose(fcpl2_id);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close files */
    ret = H5Fclose(file1_id);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Fclose(file2_id);
    CHECK(ret, FAIL, "H5Fclose");

    /* Reopen files */
    file1_id = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(file1_id, FAIL, "H5Fopen");
    file2_id = H5Fopen(FILE2, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(file2_id, FAIL, "H5Fopen");

    /* Check file sizes */
    ret = H5Fget_filesize(file1_id, &filesize1);
    CHECK(ret, FAIL, "H5Fget_filesize");
    ret = H5Fget_filesize(file2_id, &filesize2);
    CHECK(ret, FAIL, "H5Fget_filesize");

    /* Verify that the file sizes differ exactly by the userblock size */
    VERIFY_TYPE((unsigned long long)filesize2, (unsigned long long)(filesize1 + USERBLOCK_SIZE), unsigned long long, "%llu", "H5Fget_filesize");

    /* Close files */
    ret = H5Fclose(file1_id);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Fclose(file2_id);
    CHECK(ret, FAIL, "H5Fclose");

    /* Reopen files */
    file1_id = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(file1_id, FAIL, "H5Fopen");
    file2_id = H5Fopen(FILE2, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(file2_id, FAIL, "H5Fopen");

    /* Verify file sizes did not change */
    ret = H5Fget_filesize(file1_id, &filesize);
    CHECK(ret, FAIL, "H5Fget_filesize");
    VERIFY(filesize, filesize1, "H5Fget_filesize");
    ret = H5Fget_filesize(file2_id, &filesize);
    CHECK(ret, FAIL, "H5Fget_filesize");
    VERIFY(filesize, filesize2, "H5Fget_filesize");

    /* Close files */
    ret = H5Fclose(file1_id);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Fclose(file2_id);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_userblock_file_size() */

/****************************************************************
**
**  test_cached_stab_info(): low-level file test routine.
**      This test checks that new files are created with cached
**      symbol table information in the superblock (when using
**      the old format).  This is necessary to ensure backwards
**      compatibility with versions from 1.3.0 to 1.6.3.
**
*****************************************************************/
static void
test_cached_stab_info(void)
{
    hid_t file_id;
    hid_t group_id;
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing cached symbol table information\n"));

    /* Create file */
    file_id = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file_id, FAIL, "H5Fcreate");

    /* Create group */
    group_id = H5Gcreate2(file_id, GROUP1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group_id, FAIL, "H5Gcreate2");

    /* Close file and group */
    ret = H5Gclose(group_id);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Fclose(file_id);
    CHECK(ret, FAIL, "H5Fclose");

    /* Reopen file */
    file_id = H5Fopen(FILE1, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file_id, FAIL, "H5Fopen");

    /* Verify the cached symbol table information */
    ret = H5F_check_cached_stab_test(file_id);
    CHECK(ret, FAIL, "H5F_check_cached_stab_test");

    /* Close file */
    ret = H5Fclose(file_id);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_cached_stab_info() */

/*
 * To calculate the checksum for a file.
 * This is a helper routine for test_rw_noupdate().
 */
static int
cal_chksum(const char *file, uint32_t *chksum)
{
    int curr_num_errs = GetTestNumErrs();       /* Retrieve the current # of errors */
    int fdes = -1;                              /* File descriptor */
    void *file_data = NULL;                     /* Copy of file data */
    ssize_t bytes_read;                         /* # of bytes read */
    h5_stat_t sb;                               /* Stat buffer for file */
    herr_t ret;                                 /* Generic return value */

    /* Open the file */
    fdes = HDopen(file, O_RDONLY);
    CHECK(fdes, FAIL, "HDopen");

    /* Retrieve the file's size */
    ret = HDfstat(fdes, &sb);
    CHECK(fdes, FAIL, "HDfstat");

    /* Allocate space for the file data */
    file_data = HDmalloc((size_t)sb.st_size);
    CHECK_PTR(file_data, "HDmalloc");

    if(file_data) {
        /* Read file's data into memory */
        bytes_read = HDread(fdes, file_data, (size_t)sb.st_size);
        CHECK(bytes_read == sb.st_size, FALSE, "HDmalloc");

        /* Calculate checksum */
        *chksum = H5_checksum_lookup3(file_data, sizeof(file_data), 0);

        /* Free memory */
        HDfree(file_data);
    }

    /* Close the file */
    ret = HDclose(fdes);
    CHECK(ret, FAIL, "HDclose");

    return((GetTestNumErrs() == curr_num_errs) ? 0 : -1);
} /* cal_chksum() */

/****************************************************************
**
**  test_rw_noupdate(): low-level file test routine.
**      This test checks to ensure that opening and closing a file
**      with read/write permissions does not write anything to the
**      file if the file does not change.
**    Due to the implementation of file locking (status_flags in
**    the superblock is used), this test is changed to use checksum
**    instead of timestamp to verify the file is not changed.
**
**  Programmer: Vailin Choi; July 2013
**
*****************************************************************/
static void
test_rw_noupdate(void)
{
    herr_t ret;         /* Generic return value */
    hid_t fid;            /* File ID */
    uint32_t chksum1, chksum2;     /* Checksum value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing to verify that nothing is written if nothing is changed.\n"));

    /* Create and Close a HDF5 File */
    fid = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Calculate checksum for the file */
    ret = cal_chksum(FILE1, &chksum1);
    CHECK(ret, FAIL, "cal_chksum");

    /* Open and close File With Read/Write Permission */
    fid = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Calculate checksum for the file */
    ret = cal_chksum(FILE1, &chksum2);
    CHECK(ret, FAIL, "cal_chksum");

    /* The two checksums are the same, i.e. the file is not changed */
    VERIFY(chksum1, chksum2, "Checksum");

} /* end test_rw_noupdate() */

/****************************************************************
**
**  test_userblock_alignment_helper1(): helper routine for
**      test_userblock_alignment() test, to handle common testing
**
**  Programmer: Quincey Koziol
**              koziol@hdfgroup.org
**              Septmber 10, 2009
**
*****************************************************************/
static int
test_userblock_alignment_helper1(hid_t fcpl, hid_t fapl)
{
    hid_t fid;          /* File ID */
    int curr_num_errs = GetTestNumErrs();       /* Retrieve the current # of errors */
    herr_t ret;         /* Generic return value */

    /* Create a file with FAPL & FCPL */
    fid = H5Fcreate(FILE1, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Only proceed further if file ID is OK */
    if(fid > 0) {
        hid_t gid;      /* Group ID */
        hid_t sid;      /* Dataspace ID */
        hid_t did;      /* Dataset ID */
        int val = 2;    /* Dataset value */

        /* Create a group */
        gid = H5Gcreate2(fid, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(gid, FAIL, "H5Gcreate2");

        /* Create a dataset */
        sid = H5Screate(H5S_SCALAR);
        CHECK(sid, FAIL, "H5Screate");
        did = H5Dcreate2(gid, "dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(did, FAIL, "H5Dcreate2");

        /* Close dataspace */
        ret = H5Sclose(sid);
        CHECK(ret, FAIL, "H5Sclose");

        /* Write value to dataset */
        ret = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &val);
        CHECK(ret, FAIL, "H5Dwrite");

        /* Close dataset */
        ret = H5Dclose(did);
        CHECK(ret, FAIL, "H5Dclose");

        /* Close group */
        ret = H5Gclose(gid);
        CHECK(ret, FAIL, "H5Gclose");

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");
    } /* end if */

    return((GetTestNumErrs() == curr_num_errs) ? 0 : -1);
} /* end test_userblock_alignment_helper1() */

/****************************************************************
**
**  test_userblock_alignment_helper2(): helper routine for
**      test_userblock_alignment() test, to handle common testing
**
**  Programmer: Quincey Koziol
**              koziol@hdfgroup.org
**              Septmber 10, 2009
**
*****************************************************************/
static int
test_userblock_alignment_helper2(hid_t fapl, hbool_t open_rw)
{
    hid_t fid;          /* File ID */
    int curr_num_errs = GetTestNumErrs();       /* Retrieve the current # of errors */
    herr_t ret;         /* Generic return value */

    /* Re-open file */
    fid = H5Fopen(FILE1, (open_rw ? H5F_ACC_RDWR : H5F_ACC_RDONLY), fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Only proceed further if file ID is OK */
    if(fid > 0) {
        hid_t gid;      /* Group ID */
        hid_t did;      /* Dataset ID */
        int val = -1;   /* Dataset value */

        /* Open group */
        gid = H5Gopen2(fid, "group1", H5P_DEFAULT);
        CHECK(gid, FAIL, "H5Gopen2");

        /* Open dataset */
        did = H5Dopen2(gid, "dataset", H5P_DEFAULT);
        CHECK(did, FAIL, "H5Dopen2");

        /* Read value from dataset */
        ret = H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &val);
        CHECK(ret, FAIL, "H5Dread");
        VERIFY(val, 2, "H5Dread");

        /* Close dataset */
        ret = H5Dclose(did);
        CHECK(ret, FAIL, "H5Dclose");

        /* Only create new objects if file is open R/W */
        if(open_rw) {
            hid_t gid2;        /* Group ID */

            /* Create a new group */
            gid2 = H5Gcreate2(gid, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            CHECK(gid, FAIL, "H5Gcreate2");

            /* Close new group */
            ret = H5Gclose(gid2);
            CHECK(ret, FAIL, "H5Gclose");
        } /* end if */

        /* Close group */
        ret = H5Gclose(gid);
        CHECK(ret, FAIL, "H5Gclose");

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");
    } /* end if */

    return((GetTestNumErrs() == curr_num_errs) ? 0 : -1);
} /* end test_userblock_alignment_helper2() */

/****************************************************************
**
**  test_userblock_alignment(): low-level file test routine.
**      This test checks to ensure that files with both a userblock and a
**      object [allocation] alignment size set interact properly.
**
**  Programmer: Quincey Koziol
**              koziol@hdfgroup.org
**              Septmber 8, 2009
**
*****************************************************************/
static void
test_userblock_alignment(void)
{
    hid_t fid;          /* File ID */
    hid_t fcpl;         /* File creation property list ID */
    hid_t fapl;         /* File access property list ID */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing that non-zero userblocks and object alignment interact correctly.\n"));

    /* Case 1:
     *  Userblock size = 0, alignment != 0
     * Outcome:
     *  Should succeed
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)0);
    CHECK(ret, FAIL, "H5Pset_userblock");

    /* Create file access property list with alignment */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)3);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Call helper routines to perform file manipulations */
    ret = test_userblock_alignment_helper1(fcpl, fapl);
    CHECK(ret, FAIL, "test_userblock_alignment_helper1");
    ret = test_userblock_alignment_helper2(fapl, TRUE);
    CHECK(ret, FAIL, "test_userblock_alignment_helper2");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");


    /* Case 2:
     *  Userblock size = 512, alignment = 16
     *  (userblock is integral mult. of alignment)
     * Outcome:
     *  Should succeed
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_userblock");

    /* Create file access property list with alignment */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)16);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Call helper routines to perform file manipulations */
    ret = test_userblock_alignment_helper1(fcpl, fapl);
    CHECK(ret, FAIL, "test_userblock_alignment_helper1");
    ret = test_userblock_alignment_helper2(fapl, TRUE);
    CHECK(ret, FAIL, "test_userblock_alignment_helper2");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");


    /* Case 3:
     *  Userblock size = 512, alignment = 512
     *  (userblock is equal to alignment)
     * Outcome:
     *  Should succeed
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_userblock");

    /* Create file access property list with alignment */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Call helper routines to perform file manipulations */
    ret = test_userblock_alignment_helper1(fcpl, fapl);
    CHECK(ret, FAIL, "test_userblock_alignment_helper1");
    ret = test_userblock_alignment_helper2(fapl, TRUE);
    CHECK(ret, FAIL, "test_userblock_alignment_helper2");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");


    /* Case 4:
     *  Userblock size = 512, alignment = 3
     *  (userblock & alignment each individually valid, but userblock is
     *          non-integral multiple of alignment)
     * Outcome:
     *  Should fail at file creation
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_userblock");

    /* Create file access property list with alignment */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)3);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Create a file with FAPL & FCPL */
    H5E_BEGIN_TRY {
        fid = H5Fcreate(FILE1, H5F_ACC_TRUNC, fcpl, fapl);
    } H5E_END_TRY;
    VERIFY(fid, FAIL, "H5Fcreate");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");


    /* Case 5:
     *  Userblock size = 512, alignment = 1024
     *  (userblock & alignment each individually valid, but userblock is
     *          less than alignment)
     * Outcome:
     *  Should fail at file creation
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_userblock");

    /* Create file access property list with alignment */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)1024);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Create a file with FAPL & FCPL */
    H5E_BEGIN_TRY {
        fid = H5Fcreate(FILE1, H5F_ACC_TRUNC, fcpl, fapl);
    } H5E_END_TRY;
    VERIFY(fid, FAIL, "H5Fcreate");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");


    /* Case 6:
     *  File created with:
     *          Userblock size = 512, alignment = 512
     *  File re-opened for read-only & read-write access with:
     *          Userblock size = 512, alignment = 1024
     * Outcome:
     *  Should succeed
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_userblock");

    /* Create file access property list with alignment */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Call helper routines to perform file manipulations */
    ret = test_userblock_alignment_helper1(fcpl, fapl);
    CHECK(ret, FAIL, "test_userblock_alignment_helper1");

    /* Change alignment in FAPL */
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)1024);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Call helper routines to perform file manipulations */
    ret = test_userblock_alignment_helper2(fapl, FALSE);
    CHECK(ret, FAIL, "test_userblock_alignment_helper2");
    ret = test_userblock_alignment_helper2(fapl, TRUE);
    CHECK(ret, FAIL, "test_userblock_alignment_helper2");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");
} /* end test_userblock_alignment() */

/****************************************************************
**
**  test_userblock_alignment_paged(): low-level file test routine.
**      This test checks to ensure that files with both a userblock and
**      alignment interact properly:
**        -- alignment via H5Pset_alignment
**        -- alignment via paged aggregation
**
**  Programmer: Vailin Choi; March 2013
**
*****************************************************************/
static void
test_userblock_alignment_paged(void)
{
    hid_t fid;          /* File ID */
    hid_t fcpl;         /* File creation property list ID */
    hid_t fapl;         /* File access property list ID */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing interaction between userblock and alignment (via paged aggregation and H5Pset_alignment)\n"));

    /*
     * Case 1:
     *  Userblock size = 0
     *  Alignment in use = 4096
     *    Strategy = H5F_FILE_SPACE_PAGE; fsp_size = alignment = 4096
     * Outcome:
     *  Should succeed:
     *      userblock is 0 and alignment != 0
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)0);
    CHECK(ret, FAIL, "H5Pset_userblock");

    /* Create file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    /* Set the "use the latest version of the format" bounds */
    ret = H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, FALSE, (hsize_t)1);
    CHECK(ret, FAIL, "H5Pset_file_space_strategy");

    /* Call helper routines to perform file manipulations */
    ret = test_userblock_alignment_helper1(fcpl, fapl);
    CHECK(ret, FAIL, "test_userblock_alignment_helper1");
    ret = test_userblock_alignment_helper2(fapl, TRUE);
    CHECK(ret, FAIL, "test_userblock_alignment_helper2");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /*
     * Case 2a:
     *  Userblock size =  1024
     *  Alignment in use = 512
     *    Strategy = H5F_FILE_SPACE_PAGE; fsp_size = alignment = 512
     *    H5Pset_alignment() is 3
     * Outcome:
     *  Should succeed:
     *    userblock (1024) is integral mult. of alignment (512)
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)1024);
    CHECK(ret, FAIL, "H5Pset_userblock");
    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, FALSE, (hsize_t)1);
    ret = H5Pset_file_space_page_size(fcpl, (hsize_t)512);

    /* Create file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)3);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Call helper routines to perform file manipulations */
    ret = test_userblock_alignment_helper1(fcpl, fapl);
    CHECK(ret, FAIL, "test_userblock_alignment_helper1");
    ret = test_userblock_alignment_helper2(fapl, TRUE);
    CHECK(ret, FAIL, "test_userblock_alignment_helper2");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /*
     * Case 2b:
     *  Userblock size =  1024
     *  Alignment in use = 3
     *    Strategy = H5F_FILE_SPACE_AGGR; fsp_size = 512
     *      (via default file creation property)
     *    H5Pset_alignment() is 3
     * Outcome:
     *  Should fail at file creation:
     *      userblock (1024) is non-integral mult. of alignment (3)
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)1024);
    CHECK(ret, FAIL, "H5Pset_userblock");
    ret = H5Pset_file_space_page_size(fcpl, (hsize_t)512);

    /* Create file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)3);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Create a file with FAPL & FCPL */
    H5E_BEGIN_TRY {
        fid = H5Fcreate(FILE1, H5F_ACC_TRUNC, fcpl, fapl);
    } H5E_END_TRY;
    VERIFY(fid, FAIL, "H5Fcreate");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /*
     * Case 3a:
     *  Userblock size =  512
     *  Alignment in use = 512
     *    Strategy is H5F_FILE_SPACE_PAGE; fsp_size = alignment = 512
     *    H5Pset_alignment() is 3
     * Outcome:
     *  Should succeed:
     *      userblock (512) is equal to alignment (512)
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_userblock");
    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, TRUE, (hsize_t)1);
    CHECK(ret, FAIL, "H5Pset_file_space_strategy");
    ret = H5Pset_file_space_page_size(fcpl, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_file_space_page_size");

    /* Create file access property list with alignment */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)3);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Call helper routines to perform file manipulations */
    ret = test_userblock_alignment_helper1(fcpl, fapl);
    CHECK(ret, FAIL, "test_userblock_alignment_helper1");
    ret = test_userblock_alignment_helper2(fapl, TRUE);
    CHECK(ret, FAIL, "test_userblock_alignment_helper2");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /*
     * Case 3b:
     *  Userblock size =  512
     *  Alignment in use = 3
     *    Strategy is H5F_FILE_SPACE_NONE; fsp_size = 512
     *    H5Pset_alignment() is 3
     * Outcome:
     *  Should fail at file creation:
     *      userblock (512) is non-integral mult. of alignment (3)
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_userblock");
    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_NONE, FALSE, (hsize_t)1);
    CHECK(ret, FAIL, "H5Pset_file_space_strategy");
    ret = H5Pset_file_space_page_size(fcpl, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_file_space_page_size");

    /* Create file access property list with alignment */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)3);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Create a file with FAPL & FCPL */
    H5E_BEGIN_TRY {
        fid = H5Fcreate(FILE1, H5F_ACC_TRUNC, fcpl, fapl);
    } H5E_END_TRY;
    VERIFY(fid, FAIL, "H5Fcreate");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /*
     * Case 4a:
     *  Userblock size =  1024
     *  Alignment in use = 1023
     *    Strategy is H5F_FILE_SPACE_PAGE; fsp_size = alignment = 1023
     *    H5Pset_alignment() is 16
     * Outcome:
     *  Should fail at file creation:
     *      userblock (1024) is non-integral multiple of alignment (1023)
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)1024);
    CHECK(ret, FAIL, "H5Pset_userblock");
    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, TRUE, (hsize_t)1);
    CHECK(ret, FAIL, "H5Pset_file_space_strategy");
    ret = H5Pset_file_space_page_size(fcpl, (hsize_t)1023);
    CHECK(ret, FAIL, "H5Pset_file_space_page_size");

    /* Create file access property list with alignment */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)16);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Create a file with FAPL & FCPL */
    H5E_BEGIN_TRY {
        fid = H5Fcreate(FILE1, H5F_ACC_TRUNC, fcpl, fapl);
    } H5E_END_TRY;
    VERIFY(fid, FAIL, "H5Fcreate");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /*
     * Case 4b:
     *  Userblock size =  1024
     *  Alignment in use = 16
     *    Strategy is H5F_FILE_SPACE_FSM_AGGR; fsp_size = 1023
     *    H5Pset_alignment() is 16
     * Outcome:
     *  Should succeed:
     *      userblock (512) is integral multiple of alignment (16)
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)1024);
    CHECK(ret, FAIL, "H5Pset_userblock");
    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_FSM_AGGR, FALSE, (hsize_t)1);
    CHECK(ret, FAIL, "H5Pset_file_space_strategy");
    ret = H5Pset_file_space_page_size(fcpl, (hsize_t)1023);
    CHECK(ret, FAIL, "H5Pset_file_space_page_size");

    /* Create file access property list with alignment */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)16);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Call helper routines to perform file manipulations */
    ret = test_userblock_alignment_helper1(fcpl, fapl);
    CHECK(ret, FAIL, "test_userblock_alignment_helper1");
    ret = test_userblock_alignment_helper2(fapl, TRUE);
    CHECK(ret, FAIL, "test_userblock_alignment_helper2");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /*
     * Case 5a:
     *  Userblock size = 512
     *  Alignment in use = 1024
     *    Strategy is H5F_FILE_SPACE_PAGE; fsp_size = alignment = 1024
     *    H5Pset_alignment() is 16
     * Outcome:
     *  Should fail at file creation:
     *      userblock (512) is less than alignment (1024)
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_userblock");
    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, FALSE, (hsize_t)1);
    CHECK(ret, FAIL, "H5Pset_file_space_strategy");
    ret = H5Pset_file_space_page_size(fcpl, (hsize_t)1024);
    CHECK(ret, FAIL, "H5Pset_file_space_page_size");

    /* Create file access property list with alignment */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)16);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Create a file with FAPL & FCPL */
    H5E_BEGIN_TRY {
        fid = H5Fcreate(FILE1, H5F_ACC_TRUNC, fcpl, fapl);
    } H5E_END_TRY;
    VERIFY(fid, FAIL, "H5Fcreate");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /*
     * Case 5b:
     *  Userblock size = 512
     *  Alignment in use = 16
     *    Strategy is H5F_FILE_SPACE_NONE; fsp_size = 1024
     *    H5Pset_alignment() is 16
     * Outcome:
     *  Should succed:
     *      userblock (512) is integral multiple of alignment (16)
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_userblock");
    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_NONE, FALSE, (hsize_t)1);
    CHECK(ret, FAIL, "H5Pset_file_space_strategy");
    ret = H5Pset_file_space_page_size(fcpl, (hsize_t)1024);
    CHECK(ret, FAIL, "H5Pset_file_space_page_size");

    /* Create file access property list with alignment */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)16);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Call helper routines to perform file manipulations */
    ret = test_userblock_alignment_helper1(fcpl, fapl);
    CHECK(ret, FAIL, "test_userblock_alignment_helper1");
    ret = test_userblock_alignment_helper2(fapl, TRUE);
    CHECK(ret, FAIL, "test_userblock_alignment_helper2");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /*
     * Case 6:
     *  Userblock size = 512
     *  Alignment in use = 512
     *    Strategy is H5F_FILE_SPACE_PAGE; fsp_size = alignment = 512
     *    H5Pset_alignment() is 3
     *      Reopen the file; H5Pset_alignment() is 1024
     * Outcome:
     *  Should succed:
     *      Userblock (512) is the same as alignment (512);
     *    The H5Pset_alignment() calls have no effect
     */
    /* Create file creation property list with user block */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");
    ret = H5Pset_userblock(fcpl, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_userblock");
    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, FALSE, (hsize_t)1);
    CHECK(ret, FAIL, "H5Pset_file_space_strategy");
    ret = H5Pset_file_space_page_size(fcpl, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_file_space_page_size");

    /* Create file access property list with alignment */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)3);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Call helper routines to perform file manipulations */
    ret = test_userblock_alignment_helper1(fcpl, fapl);
    CHECK(ret, FAIL, "test_userblock_alignment_helper1");

    /* Change alignment in FAPL */
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)1024);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Call helper routines to perform file manipulations */
    ret = test_userblock_alignment_helper2(fapl, FALSE);
    CHECK(ret, FAIL, "test_userblock_alignment_helper2");
    ret = test_userblock_alignment_helper2(fapl, TRUE);
    CHECK(ret, FAIL, "test_userblock_alignment_helper2");

    /* Release property lists */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

} /* end test_userblock_alignment_paged() */

/****************************************************************
**
**  test_filespace_info():
**    Verify the following public routines retrieve and set file space
**    information correctly:
**      (1) H5Pget/set_file_space_strategy():
**          Retrieve and set file space strategy, persisting free-space,
**          and free-space section threshold as specified
**      (2) H5Pget/set_file_space_page_size():
**          Retrieve and set the page size for paged aggregation
**
****************************************************************/
static void
test_filespace_info(const char *env_h5_drvr)
{
    hid_t fid;                          /* File IDs    */
    hid_t fapl, new_fapl;               /* File access property lists */
    hid_t fcpl, fcpl1, fcpl2;           /* File creation property lists */
    H5F_fspace_strategy_t strategy;     /* File space strategy */
    hbool_t persist;                    /* Persist free-space or not */
    hsize_t threshold;                  /* Free-space section threshold */
    unsigned new_format;                /* New or old format */
    H5F_fspace_strategy_t fs_strategy;  /* File space strategy--iteration variable */
    unsigned fs_persist;                /* Persist free-space or not--iteration variable */
    hsize_t fs_threshold;               /* Free-space section threshold--iteration variable */
    hsize_t fsp_size;                   /* File space page size */
    char filename[FILENAME_LEN];        /* Filename to use */
    hbool_t contig_addr_vfd;            /* Whether VFD used has a contigous address space */
    herr_t ret;                         /* Return value    */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing file creation public routines: H5Pget/set_file_space_strategy & H5Pget/set_file_space_page_size\n"));

    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));

    fapl = h5_fileaccess();
    h5_fixname(FILESPACE_NAME[0], fapl, filename, sizeof filename);

    /* Get a copy of the file access property list */
    new_fapl = H5Pcopy(fapl);
    CHECK(new_fapl, FAIL, "H5Pcopy");

    /* Set the "use the latest version of the format" bounds */
    ret = H5Pset_libver_bounds(new_fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /*
     * Case (1)
     *  Check file space information from a default file creation property list.
     *  Values expected:
     *      strategy--H5F_FILE_SPACE_AGGR
     *      persist--FALSE
     *      threshold--1
     *      file space page size--4096
     */
    /* Create file creation property list template */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");

    /* Retrieve file space information */
    ret = H5Pget_file_space_strategy(fcpl, &strategy, &persist, &threshold);
    CHECK(ret, FAIL, "H5Pget_file_space_strategy");

    /* Verify file space information */
    VERIFY(strategy, H5F_FSPACE_STRATEGY_FSM_AGGR, "H5Pget_file_space_strategy");
    VERIFY(persist, FALSE, "H5Pget_file_space_strategy");
    VERIFY(threshold, 1, "H5Pget_file_space_strategy");

    /* Retrieve file space page size */
    ret = H5Pget_file_space_page_size(fcpl, &fsp_size);
    CHECK(ret, FAIL, "H5Pget_file_space_page_size");
    VERIFY(fsp_size, FSP_SIZE_DEF, "H5Pget_file_space_page_size");

    /* Close property list */
    H5Pclose(fcpl);

    /*
     * Case (2)
     *  File space page size has a minimum size of 512.
     *  Setting value less than 512 will return an error;
     *      --setting file space page size to 0
     *      --setting file space page size to 511
     *
     *  File space page size has a maximum size of 1 gigabyte.
     *  Setting value greater than 1 gigabyte will return an error.
     */
    /* Create file creation property list template */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");

    /* Setting to 0: should fail */
    H5E_BEGIN_TRY {
        ret = H5Pset_file_space_page_size(fcpl, 0);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Pset_file_space_page_size");

    /* Setting to 511: should fail */
    H5E_BEGIN_TRY {
        ret = H5Pset_file_space_page_size(fcpl, 511);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Pset_file_space_page_size");

    /* Setting to 1GB+1: should fail */
    H5E_BEGIN_TRY {
        ret = H5Pset_file_space_page_size(fcpl, FSP_SIZE1G+1);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Pset_file_space_page_size");

    /* Setting to 512: should succeed */
    ret = H5Pset_file_space_page_size(fcpl, FSP_SIZE512);
    CHECK(ret, FAIL, "H5Pset_file_space_page_size");
    ret = H5Pget_file_space_page_size(fcpl, &fsp_size);
    CHECK(ret, FAIL, "H5Pget_file_space_page_size");
    VERIFY(fsp_size, FSP_SIZE512, "H5Pget_file_space_page_size");

    /* Setting to 1GB: should succeed */
    ret = H5Pset_file_space_page_size(fcpl, FSP_SIZE1G);
    CHECK(ret, FAIL, "H5Pset_file_space_page_size");
    ret = H5Pget_file_space_page_size(fcpl, &fsp_size);
    CHECK(ret, FAIL, "H5Pget_file_space_page_size");
    VERIFY(fsp_size, FSP_SIZE1G, "H5Pget_file_space_page_size");

    /* Close property list */
    H5Pclose(fcpl);

    /*
     * Case (3)
     *  Check file space information when creating a file with default properties.
     *  Values expected:
     *      strategy--H5F_FILE_SPACE_AGGR
     *      persist--FALSE
     *      threshold--1
     *      file space page size--4096
     */
    /* Create a file with default file creation and access property lists */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Get the file's creation property list */
    fcpl1 = H5Fget_create_plist(fid);
    CHECK(fcpl1, FAIL, "H5Fget_create_plist");

    /* Retrieve file space information */
    ret = H5Pget_file_space_strategy(fcpl1, &strategy, &persist, &threshold);
    CHECK(ret, FAIL, "H5Pget_file_space_strategy");

    /* Verify file space information */
    VERIFY(strategy, H5F_FSPACE_STRATEGY_FSM_AGGR, "H5Pget_file_space_strategy");
    VERIFY(persist, FALSE, "H5Pget_file_space_strategy");
    VERIFY(threshold, 1, "H5Pget_file_space_strategy");

    /* Retrieve file space page size */
    ret = H5Pget_file_space_page_size(fcpl1, &fsp_size);
    CHECK(ret, FAIL, "H5Pget_file_space_page_size");
    VERIFY(fsp_size, FSP_SIZE_DEF, "H5Pget_file_space_page_size");

    /* Close property lists */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Pclose(fcpl1);
    CHECK(ret, FAIL, "H5Pclose");

    /*
     * Case (4)
     *  Check file space information when creating a file with the
     *  latest library format and default properties.
     *  Values expected:
     *      strategy--H5F_FILE_SPACE_AGGR
     *      persist--FALSE
     *      threshold--1
     *      file space page size--4096
     */
    /* Create a file with the latest library format */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, new_fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Get the file's creation property */
    fcpl1 = H5Fget_create_plist(fid);
    CHECK(fcpl1, FAIL, "H5Fget_create_plist");

    /* Retrieve file space information */
    ret = H5Pget_file_space_strategy(fcpl1, &strategy, &persist, &threshold);
    CHECK(ret, FAIL, "H5Pget_file_space_strategy");

    /* Verify file space information */
    VERIFY(strategy, H5F_FSPACE_STRATEGY_FSM_AGGR, "H5Pget_file_space_strategy");
    VERIFY(persist, FALSE, "H5Pget_file_space_strategy");
    VERIFY(threshold, 1, "H5Pget_file_space_strategy");

    /* Retrieve file space page size */
    ret = H5Pget_file_space_page_size(fcpl1, &fsp_size);
    CHECK(ret, FAIL, "H5Pget_file_space_page_size");
    VERIFY(fsp_size, FSP_SIZE_DEF, "H5Pget_file_space_page_size");

    /* Close property lists */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Pclose(fcpl1);
    CHECK(ret, FAIL, "H5Pclose");

    /*
     * Case (5)
     *  Check file space information with the following combinations:
     *    Create file with --
     *        New or old format
     *        Persist or not persist free-space
     *        Different sizes for free-space section threshold (0 to 10)
     *        The four file space strategies:
     *          H5F_FSPACE_STRATEGY_FSM_AGGR, H5F_FSPACE_STRATEGY_PAGE,
     *        H5F_FSPACE_STRATEGY_AGGR, H5F_FSPACE_STRATEGY_NONE
     *        File space page size: set to 512
     *
     */
    for(new_format = FALSE; new_format <= TRUE; new_format++) {
        hid_t my_fapl;

        /* Set the FAPL for the type of format */
        if(new_format) {
            MESSAGE(5, ("Testing with new group format\n"));
            my_fapl = new_fapl;
        } /* end if */
        else {
            MESSAGE(5, ("Testing with old group format\n"));
            my_fapl = fapl;
        } /* end else */

        /* Test with TRUE or FALSE for persisting free-space */
        for(fs_persist = FALSE; fs_persist <= TRUE; fs_persist++) {

            /* Test with free-space section threshold size: 0 to 10 */
            for(fs_threshold = 0; fs_threshold <= TEST_THRESHOLD10; fs_threshold++) {

                /* Test with 4 file space strategies */
                for(fs_strategy = H5F_FSPACE_STRATEGY_FSM_AGGR; fs_strategy < H5F_FSPACE_STRATEGY_NTYPES; H5_INC_ENUM(H5F_fspace_strategy_t, fs_strategy)) {

                    if(!contig_addr_vfd && (fs_strategy == H5F_FSPACE_STRATEGY_PAGE || fs_persist))
                        continue;

                    /* Create file creation property list template */
                    fcpl = H5Pcreate(H5P_FILE_CREATE);
                    CHECK(fcpl, FAIL, "H5Pcreate");

                    /* Set file space information */
                    ret = H5Pset_file_space_strategy(fcpl, fs_strategy, (hbool_t)fs_persist, fs_threshold);
                    CHECK(ret, FAIL, "H5Pset_file_space_strategy");

                    ret = H5Pset_file_space_page_size(fcpl, FSP_SIZE512);
                    CHECK(ret, FAIL, "H5Pset_file_space_strategy");

                    /* Retrieve file space information */
                    ret = H5Pget_file_space_strategy(fcpl, &strategy, &persist, &threshold);
                    CHECK(ret, FAIL, "H5Pget_file_space_strategy");

                    /* Verify file space information */
                    VERIFY(strategy, fs_strategy, "H5Pget_file_space_strategy");

                    if(fs_strategy < H5F_FSPACE_STRATEGY_AGGR) {
                        VERIFY(persist, (hbool_t)fs_persist, "H5Pget_file_space_strategy");
                        VERIFY(threshold, fs_threshold, "H5Pget_file_space_strategy");
                    } else {
                        VERIFY(persist, FALSE, "H5Pget_file_space_strategy");
                        VERIFY(threshold, 1, "H5Pget_file_space_strategy");
                    }

                    /* Retrieve and verify file space page size */
                    ret = H5Pget_file_space_page_size(fcpl, &fsp_size);
                    CHECK(ret, FAIL, "H5Pget_file_space_page_size");
                    VERIFY(fsp_size, FSP_SIZE512, "H5Pget_file_space_page_size");

                    /* Create the file with the specified file space info */
                    fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, my_fapl);
                    CHECK(fid, FAIL, "H5Fcreate");

                    /* Get the file's creation property */
                    fcpl1 = H5Fget_create_plist(fid);
                    CHECK(fcpl1, FAIL, "H5Fget_create_plist");

                    /* Retrieve file space information */
                    ret = H5Pget_file_space_strategy(fcpl1, &strategy, &persist, &threshold);
                    CHECK(ret, FAIL, "H5Pget_file_space_strategy");

                    /* Verify file space information */
                    VERIFY(strategy, fs_strategy, "H5Pget_file_space_strategy");

                    if(fs_strategy < H5F_FSPACE_STRATEGY_AGGR) {
                        VERIFY(persist, fs_persist, "H5Pget_file_space_strategy");
                        VERIFY(threshold, fs_threshold, "H5Pget_file_space_strategy");
                    } else {
                        VERIFY(persist, FALSE, "H5Pget_file_space_strategy");
                        VERIFY(threshold, 1, "H5Pget_file_space_strategy");
                    }

                    /* Retrieve and verify file space page size */
                    ret = H5Pget_file_space_page_size(fcpl1, &fsp_size);
                    CHECK(ret, FAIL, "H5Pget_file_space_page_size");
                    VERIFY(fsp_size, FSP_SIZE512, "H5Pget_file_space_page_size");

                    /* Close the file */
                    ret = H5Fclose(fid);
                    CHECK(ret, FAIL, "H5Fclose");

                    /* Re-open the file */
                    fid = H5Fopen(filename, H5F_ACC_RDWR, my_fapl);
                    CHECK(ret, FAIL, "H5Fopen");

                    /* Get the file's creation property */
                    fcpl2 = H5Fget_create_plist(fid);
                    CHECK(fcpl2, FAIL, "H5Fget_create_plist");

                    /* Retrieve file space information */
                    ret = H5Pget_file_space_strategy(fcpl2, &strategy, &persist, &threshold);
                    CHECK(ret, FAIL, "H5Pget_file_space_strategy");

                    /* Verify file space information */
                    VERIFY(strategy, fs_strategy, "H5Pget_file_space_strategy");
                    if(fs_strategy < H5F_FSPACE_STRATEGY_AGGR) {
                        VERIFY(persist, fs_persist, "H5Pget_file_space_strategy");
                        VERIFY(threshold, fs_threshold, "H5Pget_file_space_strategy");
                    } else {
                        VERIFY(persist, FALSE, "H5Pget_file_space_strategy");
                        VERIFY(threshold, 1, "H5Pget_file_space_strategy");
                    }

                    /* Retrieve and verify file space page size */
                    ret = H5Pget_file_space_page_size(fcpl2, &fsp_size);
                    CHECK(ret, FAIL, "H5Pget_file_space_page_size");
                    VERIFY(fsp_size, FSP_SIZE512, "H5Pget_file_space_page_size");

                    /* Close the file */
                    ret = H5Fclose(fid);
                    CHECK(ret, FAIL, "H5Fclose");

                    /* Release file creation property lists */
                    ret = H5Pclose(fcpl);
                    CHECK(ret, FAIL, "H5Pclose");
                    ret = H5Pclose(fcpl1);
                    CHECK(ret, FAIL, "H5Pclose");
                    ret = H5Pclose(fcpl2);
                    CHECK(ret, FAIL, "H5Pclose");
                } /* end for file space strategy type */
            } /* end for free-space section threshold */
        } /* end for fs_persist */

        /* close fapl_ and remove the file */
        h5_clean_files(FILESPACE_NAME, my_fapl);
    } /* end for new_format */

}  /* test_filespace_info() */

/****************************************************************
**
** set_multi_split():
**  Internal routine to set up page-aligned address space for multi/split driver
**  when testing paged aggregation.
**  This is used by test_file_freespace() and test_sects_freespace().
**
*****************************************************************/
static int
set_multi_split(hid_t fapl, hsize_t pagesize, hbool_t multi, hbool_t split)
{
    H5FD_mem_t memb_map[H5FD_MEM_NTYPES];
    hid_t memb_fapl_arr[H5FD_MEM_NTYPES];
    char *memb_name[H5FD_MEM_NTYPES];
    haddr_t memb_addr[H5FD_MEM_NTYPES];
    hbool_t relax;
    H5FD_mem_t  mt;

    HDassert(split || multi);

    HDmemset(memb_name, 0, sizeof memb_name);

    /* Get current split settings */
    if(H5Pget_fapl_multi(fapl, memb_map, memb_fapl_arr, memb_name, memb_addr, &relax) < 0)
        TEST_ERROR

    if(split) {
        /* Set memb_addr aligned */
        memb_addr[H5FD_MEM_SUPER] = ((memb_addr[H5FD_MEM_SUPER] + pagesize - 1) / pagesize) * pagesize;
        memb_addr[H5FD_MEM_DRAW] = ((memb_addr[H5FD_MEM_DRAW] + pagesize - 1) / pagesize) * pagesize;
    } else {
        /* Set memb_addr aligned */
        for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, mt))
            memb_addr[mt] = ((memb_addr[mt] + pagesize - 1) / pagesize) * pagesize;
    } /* end else */

    /* Set multi driver with new FAPLs */
    if(H5Pset_fapl_multi(fapl, memb_map, memb_fapl_arr, (const char * const *)memb_name, memb_addr, relax) < 0)
        TEST_ERROR

    /* Free memb_name */
    for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, mt))
        free(memb_name[mt]);

    return 0;

error:
    return(-1);

} /* set_multi_split() */

/****************************************************************
**
**  test_file_freespace():
**      This routine checks the free space available in a file as
**      returned by the public routine H5Fget_freespace().
**
**
*****************************************************************/
static void
test_file_freespace(const char *env_h5_drvr)
{
    hid_t    file;                      /* File opened with read-write permission */
    h5_stat_size_t empty_filesize;      /* Size of file when empty */
    h5_stat_size_t mod_filesize;        /* Size of file after being modified */
    hssize_t free_space;                /* Amount of free space in file */
    hid_t    fcpl;                      /* File creation property list */
    hid_t    fapl, new_fapl;            /* File access property list IDs */
    hid_t    dspace;                    /* Dataspace ID */
    hid_t    dset;                      /* Dataset ID */
    hid_t    dcpl;                      /* Dataset creation property list */
    int k;                              /* Local index variable */
    unsigned u;                         /* Local index variable */
    char     filename[FILENAME_LEN];    /* Filename to use */
    char     name[32];                  /* Dataset name */
    unsigned new_format;                /* To use old or new format */
    hbool_t split_vfd, multi_vfd;       /* Indicate multi/split driver */
    hsize_t expected_freespace;         /* Freespace expected */
    hsize_t expected_fs_del;            /* Freespace expected after delete */
    herr_t   ret;                       /* Return value */

    split_vfd = !HDstrcmp(env_h5_drvr, "split");
    multi_vfd = !HDstrcmp(env_h5_drvr, "multi");

    if(!split_vfd && !multi_vfd) {
        fapl = h5_fileaccess();
        h5_fixname(FILESPACE_NAME[0], fapl, filename, sizeof filename);

        new_fapl = H5Pcopy(fapl);
        CHECK(new_fapl, FAIL, "H5Pcopy");

        /* Set the "use the latest version of the format" bounds */
        ret = H5Pset_libver_bounds(new_fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
        CHECK(ret, FAIL, "H5Pset_libver_bounds");

        fcpl = H5Pcreate(H5P_FILE_CREATE);
        CHECK(fcpl, FAIL, "H5Pcreate");

        /* Test with old & new format */
        for(new_format = FALSE; new_format <= TRUE; new_format++) {
            hid_t my_fapl;

            /* Set the FAPL for the type of format */
            if(new_format) {
                MESSAGE(5, ("Testing with new group format\n"));

                my_fapl = new_fapl;

                if(multi_vfd || split_vfd) {
                    ret = set_multi_split(new_fapl, FSP_SIZE_DEF, multi_vfd, split_vfd);
                    CHECK(ret, FAIL, "set_multi_split");
                }

                ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, FALSE, (hsize_t)1);
                CHECK(ret, FAIL, "H5P_set_file_space_strategy");

                expected_freespace = 4534;
                if(split_vfd) expected_freespace = 427;
                if(multi_vfd) expected_freespace = 248;
                expected_fs_del = 0;
            } /* end if */
            else {
                MESSAGE(5, ("Testing with old group format\n"));
                /* Default: non-paged aggregation, non-persistent free-space */
                my_fapl = fapl;
                expected_freespace = 2464;
                if(split_vfd) expected_freespace = 264;
                if(multi_vfd) expected_freespace = 0;
                expected_fs_del = 4096;

            } /* end else */

            /* Create an "empty" file */
            file = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, my_fapl);
            CHECK(file, FAIL, "H5Fcreate");

            ret = H5Fclose(file);
            CHECK_I(ret, "H5Fclose");

            /* Get the "empty" file size */
            empty_filesize = h5_get_file_size(filename, H5P_DEFAULT);

            /* Re-open the file (with read-write permission) */
            file = H5Fopen(filename, H5F_ACC_RDWR, my_fapl);
            CHECK_I(file, "H5Fopen");

            /* Check that the free space is 0 */
            free_space = H5Fget_freespace(file);
            CHECK(free_space, FAIL, "H5Fget_freespace");
            VERIFY(free_space, 0, "H5Fget_freespace");

            /* Create dataspace for datasets */
            dspace = H5Screate(H5S_SCALAR);
            CHECK(dspace, FAIL, "H5Screate");

            /* Create a dataset creation property list */
            dcpl = H5Pcreate(H5P_DATASET_CREATE);
            CHECK(dcpl, FAIL, "H5Pcreate");

            /* Set the space allocation time to early */
            ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY);
            CHECK(ret, FAIL, "H5Pset_alloc_time");

            /* Create datasets in file */
            for(u = 0; u < 10; u++) {
                HDsprintf(name, "Dataset %u", u);
                dset = H5Dcreate2(file, name, H5T_STD_U32LE, dspace, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                CHECK(dset, FAIL, "H5Dcreate2");

                ret = H5Dclose(dset);
                CHECK(ret, FAIL, "H5Dclose");
            } /* end for */

            /* Close dataspace */
            ret = H5Sclose(dspace);
            CHECK(ret, FAIL, "H5Sclose");

            /* Close dataset creation property list */
            ret = H5Pclose(dcpl);
            CHECK(ret, FAIL, "H5Pclose");

            /* Check that there is the right amount of free space in the file */
            free_space = H5Fget_freespace(file);
            CHECK(free_space, FAIL, "H5Fget_freespace");
            VERIFY(free_space, expected_freespace, "H5Fget_freespace");

            /* Delete datasets in file */
            for(k = 9; k >= 0; k--) {
                HDsprintf(name, "Dataset %u", (unsigned)k);
                ret = H5Ldelete(file, name, H5P_DEFAULT);
                CHECK(ret, FAIL, "H5Ldelete");
            } /* end for */

            /* Check that there is the right amount of free space in the file */
            free_space = H5Fget_freespace(file);
            CHECK(free_space, FAIL, "H5Fget_freespace");
            VERIFY(free_space, expected_fs_del, "H5Fget_freespace");

            /* Close file */
            ret = H5Fclose(file);
            CHECK(ret, FAIL, "H5Fclose");

            /* Get the file size after modifications*/
            mod_filesize = h5_get_file_size(filename, H5P_DEFAULT);

            /* Check that the file reverted to empty size */
            VERIFY(mod_filesize, empty_filesize, "H5Fget_freespace");

            h5_clean_files(FILESPACE_NAME, my_fapl);

        } /* end for */
    }

} /* end test_file_freespace() */

/****************************************************************
**
**  test_sects_freespace():
**      This routine checks free-space section information for the
**    file as returned by the public routine H5Fget_free_sections().
**
*****************************************************************/
static void
test_sects_freespace(const char *env_h5_drvr, hbool_t new_format)
{
    char     filename[FILENAME_LEN];    /* Filename to use */
    hid_t    file;                      /* File ID */
    hid_t    fcpl;                      /* File creation property list template */
    hid_t    fapl;                      /* File access property list template */
    hssize_t free_space;                /* Amount of free-space in the file */
    hid_t    dspace;                    /* Dataspace ID */
    hid_t    dset;                      /* Dataset ID */
    hid_t    dcpl;                      /* Dataset creation property list */
    char     name[32];                  /* Dataset name */
    hssize_t nsects = 0;                /* # of free-space sections */
    hssize_t nall;                      /* # of free-space sections for all types of data */
    hssize_t nmeta = 0, nraw = 0;       /* # of free-space sections for meta/raw/generic data */
    H5F_sect_info_t sect_info[15];      /* Array to hold free-space information */
    H5F_sect_info_t all_sect_info[15];  /* Array to hold free-space information for all types of data */
    H5F_sect_info_t meta_sect_info[15]; /* Array to hold free-space information for metadata */
    H5F_sect_info_t raw_sect_info[15];  /* Array to hold free-space information for raw data */
    hsize_t  total = 0;                    /* sum of the free-space section sizes */
    hsize_t  tmp_tot = 0;               /* Sum of the free-space section sizes */
    hsize_t  last_size;                    /* Size of last free-space section */
    hsize_t  dims[1];                   /* Dimension sizes */
    unsigned u;                         /* Local index variable */
    H5FD_mem_t type;
    hbool_t split_vfd = FALSE, multi_vfd = FALSE;
    herr_t   ret;                   /* Return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing H5Fget_free_sections()--free-space section info in the file\n"));

    split_vfd = !HDstrcmp(env_h5_drvr, "split");
    multi_vfd = !HDstrcmp(env_h5_drvr, "multi");

    if(!split_vfd && !multi_vfd) {

        fapl = h5_fileaccess();
        h5_fixname(FILESPACE_NAME[0], fapl, filename, sizeof filename);

        /* Create file-creation template */
        fcpl = H5Pcreate(H5P_FILE_CREATE);
        CHECK(fcpl, FAIL, "H5Pcreate");

        if(new_format) {
            ret = H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
            CHECK(ret, FAIL, "H5Pset_libver_bounds");

            /* Set to paged aggregation and persistent free-space */
            ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, TRUE, (hsize_t)1);
            CHECK(ret, FAIL, "H5Pget_file_space_strategy");

            /* Set up paged aligned address space for multi/split driver */
            if(multi_vfd || split_vfd) {
                ret = set_multi_split(fapl, FSP_SIZE_DEF, multi_vfd, split_vfd);
                CHECK(ret, FAIL, "set_multi_split");
            }

        } else {
            ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_FSM_AGGR, TRUE, (hsize_t)1);
            CHECK(ret, FAIL, "H5Pget_file_space_strategy");
        }

        /* Create the file */
        file = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl);
        CHECK(file, FAIL, "H5Fcreate");

        /* Create a dataset creation property list */
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(dcpl, FAIL, "H5Pcreate");

        /* Set the space allocation time to early */
        ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY);
        CHECK(ret, FAIL, "H5Pset_alloc_time");

        /* Create 1 large dataset */
        dims[0] = 1200;
        dspace = H5Screate_simple(1, dims, NULL);
        dset = H5Dcreate2(file, "Dataset_large", H5T_STD_U32LE, dspace, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        CHECK(dset, FAIL, "H5Dcreate2");

        /* Close dataset */
        ret = H5Dclose(dset);
        CHECK(ret, FAIL, "H5Dclose");

        /* Close dataspace */
        ret = H5Sclose(dspace);
        CHECK(ret, FAIL, "H5Sclose");

        /* Create dataspace for datasets */
        dspace = H5Screate(H5S_SCALAR);
        CHECK(dspace, FAIL, "H5Screate");

        /* Create datasets in file */
        for(u = 0; u < 10; u++) {
            HDsprintf(name, "Dataset %u", u);
            dset = H5Dcreate2(file, name, H5T_STD_U32LE, dspace, H5P_DEFAULT, dcpl, H5P_DEFAULT);
            CHECK(dset, FAIL, "H5Dcreate2");

            ret = H5Dclose(dset);
            CHECK(ret, FAIL, "H5Dclose");
        } /* end for */

        /* Close dataspace */
        ret = H5Sclose(dspace);
        CHECK(ret, FAIL, "H5Sclose");

        /* Close dataset creation property list */
        ret = H5Pclose(dcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Delete odd-numbered datasets in file */
        for(u = 0; u < 10; u++) {
            HDsprintf(name, "Dataset %u", u);
            if(u % 2) {
                ret = H5Ldelete(file, name, H5P_DEFAULT);
                CHECK(ret, FAIL, "H5Ldelete");
            } /* end if */
        } /* end for */

        /* Close file */
        ret = H5Fclose(file);
        CHECK(ret, FAIL, "H5Fclose");

        /* Re-open the file with read-only permission */
        file = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
        CHECK_I(file, "H5Fopen");

        /* Get the amount of free space in the file */
        free_space = H5Fget_freespace(file);
        CHECK(free_space, FAIL, "H5Fget_freespace");

        /* Get the total # of free-space sections in the file */
        nall = H5Fget_free_sections(file, H5FD_MEM_DEFAULT, (size_t)0, NULL);
        CHECK(nall, FAIL, "H5Fget_free_sections");

        /* Should return failure when nsects is 0 with a nonnull sect_info */
        nsects = H5Fget_free_sections(file, H5FD_MEM_DEFAULT, (size_t)0, all_sect_info);
        VERIFY(nsects, FAIL, "H5Fget_free_sections");

        /* Retrieve and verify free space info for all the sections */
        HDmemset(all_sect_info, 0,  sizeof(all_sect_info));
        nsects = H5Fget_free_sections(file, H5FD_MEM_DEFAULT, (size_t)nall, all_sect_info);
        VERIFY(nsects, nall, "H5Fget_free_sections");

        /* Verify the amount of free-space is correct */
        for(u = 0; u < nall; u++)
            total += all_sect_info[u].size;
        VERIFY(free_space, total, "H5Fget_free_sections");

        /* Save the last section's size */
        last_size = all_sect_info[nall-1].size;

        /* Retrieve and verify free space info for -1 sections */
        HDmemset(sect_info, 0,  sizeof(sect_info));
        nsects = H5Fget_free_sections(file, H5FD_MEM_DEFAULT, (size_t)(nall - 1), sect_info);
        VERIFY(nsects, nall, "H5Fget_free_sections");

        /* Verify the amount of free-space is correct */
        total = 0;
        for(u = 0; u < (nall - 1); u++) {
            VERIFY(sect_info[u].addr, all_sect_info[u].addr, "H5Fget_free_sections");
            VERIFY(sect_info[u].size, all_sect_info[u].size, "H5Fget_free_sections");
            total += sect_info[u].size;
        }
        VERIFY(((hsize_t)free_space - last_size), total, "H5Fget_free_sections");

        /* Retrieve and verify free-space info for +1 sections */
        HDmemset(sect_info, 0,  sizeof(sect_info));
        nsects = H5Fget_free_sections(file, H5FD_MEM_DEFAULT, (size_t)(nall + 1), sect_info);
        VERIFY(nsects, nall, "H5Fget_free_sections");

        /* Verify amount of free-space is correct */
        total = 0;
        for(u = 0; u < nall; u++) {
            VERIFY(sect_info[u].addr, all_sect_info[u].addr, "H5Fget_free_sections");
            VERIFY(sect_info[u].size, all_sect_info[u].size, "H5Fget_free_sections");
            total += sect_info[u].size;
        }
        VERIFY(sect_info[nall].addr, 0, "H5Fget_free_sections");
        VERIFY(sect_info[nall].size, 0, "H5Fget_free_sections");
        VERIFY(free_space, total, "H5Fget_free_sections");

        HDmemset(meta_sect_info, 0,  sizeof(meta_sect_info));
        if(multi_vfd) {
            hssize_t ntmp;

            for(type = H5FD_MEM_SUPER; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type)) {
                if(type == H5FD_MEM_DRAW || type == H5FD_MEM_GHEAP)
                    continue;
                /* Get the # of free-space sections in the file for metadata */
                ntmp = H5Fget_free_sections(file, type, (size_t)0, NULL);
                CHECK(ntmp, FAIL, "H5Fget_free_sections");

                if(ntmp > 0) {
                    nsects = H5Fget_free_sections(file, type, (size_t)ntmp, &meta_sect_info[nmeta]);
                    VERIFY(nsects, ntmp, "H5Fget_free_sections");
                    nmeta += ntmp;
                }
            }
        } else {
            /* Get the # of free-space sections in the file for metadata */
            nmeta = H5Fget_free_sections(file, H5FD_MEM_SUPER, (size_t)0, NULL);
            CHECK(nmeta, FAIL, "H5Fget_free_sections");

            /* Retrieve and verify free-space sections for metadata */
            nsects = H5Fget_free_sections(file, H5FD_MEM_SUPER, (size_t)nmeta, meta_sect_info);
            VERIFY(nsects, nmeta, "H5Fget_free_sections");
        }

        /* Get the # of free-space sections in the file for raw data */
        nraw = H5Fget_free_sections(file, H5FD_MEM_DRAW, (size_t)0, NULL);
        CHECK(nraw, FAIL, "H5Fget_free_sections");

        /* Retrieve and verify free-space sections for raw data */
        HDmemset(raw_sect_info, 0,  sizeof(raw_sect_info));
        nsects = H5Fget_free_sections(file, H5FD_MEM_DRAW, (size_t)nraw, raw_sect_info);
        VERIFY(nsects, nraw, "H5Fget_free_sections");

        /* Sum all the free-space sections */
        for(u = 0; u < nmeta; u++)
            tmp_tot += meta_sect_info[u].size;

        for(u = 0; u < nraw; u++)
            tmp_tot += raw_sect_info[u].size;

        /* Verify free-space info */
        VERIFY(nmeta+nraw, nall, "H5Fget_free_sections");
        VERIFY(tmp_tot, total, "H5Fget_free_sections");

        /* Closing */
        ret = H5Fclose(file);
        CHECK(ret, FAIL, "H5Fclose");
        ret = H5Pclose(fcpl);
        CHECK(fcpl, FAIL, "H5Pclose");

        h5_clean_files(FILESPACE_NAME, fapl);
    }

} /* end test_sects_freespace() */


/****************************************************************
**
**  test_filespace_compatible():
**    Verify that the trunk with the latest file space management
**    can open, read and modify 1.6 HDF5 file and 1.8 HDF5 file.
**    Also verify the correct file space handling information
**    and the amount of free space.
**
****************************************************************/
static void
test_filespace_compatible(void)
{
    int fd_old = (-1), fd_new = (-1);   /* File descriptors for copying data */
    hid_t    fid = -1;        /* File id */
    hid_t       did = -1;        /* Dataset id */
    hid_t    fcpl;            /* File creation property list template */
    int         check[100];         /* Temporary buffer for verifying dataset data */
    int         rdbuf[100];        /* Temporary buffer for reading in dataset data */
    uint8_t     buf[READ_OLD_BUFSIZE];    /* temporary buffer for reading */
    ssize_t     nread;          /* Number of bytes read in */
    unsigned    i, j;            /* Local index variable */
    hssize_t    free_space;        /* Amount of free-space in the file */
    hbool_t    persist;        /* Persist free-space or not */
    hsize_t    threshold;        /* Free-space section threshold */
    H5F_fspace_strategy_t strategy;        /* File space handling strategy */
    herr_t    ret;            /* Return value */

    /* Output message about test being performed */
    MESSAGE(5, ("File space compatibility testing for 1.6 and 1.8 files\n"));

    for(j = 0; j < NELMTS(OLD_FILENAME); j++) {
        const char *filename = H5_get_srcdir_filename(OLD_FILENAME[j]); /* Corrected test file name */

        /* Open and copy the test file into a temporary file */
    fd_old = HDopen(filename, O_RDONLY);
    CHECK(fd_old, FAIL, "HDopen");
    fd_new = HDopen(FILE5, O_RDWR|O_CREAT|O_TRUNC, H5_POSIX_CREATE_MODE_RW);
    CHECK(fd_new, FAIL, "HDopen");

    /* Copy data */
        while((nread = HDread(fd_old, buf, (size_t)READ_OLD_BUFSIZE)) > 0) {
            ssize_t write_err = HDwrite(fd_new, buf, (size_t)nread);
            CHECK(write_err, -1, "HDwrite");
        } /* end while */

    /* Close the files */
    ret = HDclose(fd_old);
    CHECK(ret, FAIL, "HDclose");
    ret = HDclose(fd_new);
    CHECK(ret, FAIL, "HDclose");

        /* Open the temporary test file */
    fid = H5Fopen(FILE5, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* There should not be any free space in the file */
    free_space = H5Fget_freespace(fid);
    CHECK(free_space, FAIL, "H5Fget_freespace");
    VERIFY(free_space, (hssize_t)0, "H5Fget_freespace");

        /* Get the file's file creation property list */
        fcpl = H5Fget_create_plist(fid);
        CHECK(fcpl, FAIL, "H5Fget_create_plist");

        /* Retrieve the file space info */
        ret = H5Pget_file_space_strategy(fcpl, &strategy, &persist, &threshold);
        CHECK(ret, FAIL, "H5Pget_file_space_strategy");

        /* File space handling strategy should be H5F_FSPACE_STRATEGY_FSM_AGGR */
        /* Persisting free-space should be FALSE */
        /* Free-space section threshold should be 1 */
        VERIFY(strategy, H5F_FSPACE_STRATEGY_FSM_AGGR, "H5Pget_file_space_strategy");
        VERIFY(persist, FALSE, "H5Pget_file_space_strategy");
        VERIFY(threshold, 1, "H5Pget_file_space_strategy");

    /* Generate raw data */
    for(i = 0; i < 100; i++)
        check[i] = (int)i;

    /* Open and read the dataset */
    did = H5Dopen2(fid, DSETNAME, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen");
    ret = H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Verify the data read is correct */
    for(i = 0; i < 100; i++)
        VERIFY(rdbuf[i], check[i], "test_compatible");

    /* Close the dataset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Remove the dataset */
    ret = H5Ldelete(fid, DSETNAME, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

        /* Close the plist */
        ret = H5Pclose(fcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Close the file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

    /* Re-Open the file */
    fid = H5Fopen(FILE5, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* The dataset should not be there */
    did = H5Dopen2(fid, DSETNAME, H5P_DEFAULT);
    VERIFY(did, FAIL, "H5Dopen");

    /* There should not be any free space in the file */
    free_space = H5Fget_freespace(fid);
    CHECK(free_space, FAIL, "H5Fget_freespace");
    VERIFY(free_space, (hssize_t)0, "H5Fget_freespace");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
    } /* end for */
} /* test_filespace_compatible */

/****************************************************************
**
**  test_filespace_1.10.0_compatible():
**    Verify that the latest file space management can open, read and
**    modify 1.10.0 HDF5 files :
**    h5fc_ext1_i.h5:   H5F_FILE_SPACE_ALL, default threshold; has superblock extension but no fsinfo message
**    h5fc_ext1_f.h5:   H5F_FILE_SPACE_ALL_PERSIST, default threshold; has superblock extension with fsinfo message
**    h5fc_ext2_if.h5:  H5F_FILE_SPACE_ALL, non-default threshold; has superblock extension with fsinfo message
**    h5fc_ext2_sf.h5:  H5F_FILE_SPACE_VFD, default threshold; has superblock extension with fsinfo message
**    h5fc_ext3_isf.h5: H5F_FILE_SPACE_AGGR_VFD, default threshold; has superblock extension with fsinfo message
**    h5fc_ext_none.h5: H5F_FILE_SPACE_ALL, default threshold; without superblock extension
**  The above files are copied from release 1.10.0 tools/h5format_convert/testfiles.
**
****************************************************************/
static void
test_filespace_1_10_0_compatible(void)
{
    hid_t       fid = -1;               /* File id */
    hid_t       did = -1;        /* Dataset id */
    hid_t       fcpl;                   /* File creation property list */
    hbool_t    persist;                /* Persist free-space or not */
    hsize_t    threshold;              /* Free-space section threshold */
    H5F_fspace_strategy_t strategy;     /* File space handling strategy */
    int         wbuf[24];         /* Buffer for dataset data */
    int         rdbuf[24];        /* Buffer for dataset data */
    int         status;                 /* Status from copying the existing file */
    unsigned    i, j;                   /* Local index variable */
    herr_t    ret;            /* Return value */

    /* Output message about test being performed */
    MESSAGE(5, ("File space compatibility testing for 1.10.0 files\n"));

    for(j = 0; j < NELMTS(OLD_1_10_0_FILENAME); j++) {
        /* Make a copy of the test file */
        status = h5_make_local_copy(OLD_1_10_0_FILENAME[j], FILE5);
        CHECK(status, FAIL, "h5_make_local_copy");

        /* Open the temporary test file */
        fid = H5Fopen(FILE5, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK(fid, FAIL, "H5Fopen");

        /* Get the file's file creation property list */
        fcpl = H5Fget_create_plist(fid);
        CHECK(fcpl, FAIL, "H5Fget_create_plist");

        /* Retrieve the file space info */
        ret = H5Pget_file_space_strategy(fcpl, &strategy, &persist, &threshold);
        CHECK(ret, FAIL, "H5Pget_file_space_strategy");

        switch(j) {
            case 0:
                VERIFY(strategy, H5F_FILE_SPACE_STRATEGY_DEF, "H5Pget_file_space_strategy");
                VERIFY(persist, H5F_FREE_SPACE_PERSIST_DEF, "H5Pget_file_space_strategy");
                VERIFY(threshold, H5F_FREE_SPACE_THRESHOLD_DEF, "H5Pget_file_space_strategy");

                /* Open the dataset */
                did = H5Dopen2(fid, "/DSET_EA", H5P_DEFAULT);
                CHECK(did, FAIL, "H5Dopen");

                for(i = 0; i < 24; i++)
                    wbuf[i] = (int)j+1;

                /* Write to the dataset */
                ret = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);
                CHECK(ret, FAIL, "H5Dwrite");

                /* Close the dataset */
                ret = H5Dclose(did);
                CHECK(ret, FAIL, "H5Dclose");
                break;

            case 1:
                VERIFY(strategy, H5F_FSPACE_STRATEGY_FSM_AGGR, "H5Pget_file_space_strategy");
                VERIFY(persist, TRUE, "H5Pget_file_space_strategy");
                VERIFY(threshold, H5F_FREE_SPACE_THRESHOLD_DEF, "H5Pget_file_space_strategy");

                /* Open the dataset */
                did = H5Dopen2(fid, "/DSET_NDATA_BT2", H5P_DEFAULT);
                CHECK(did, FAIL, "H5Dopen");

                for(i = 0; i < 24; i++)
                    wbuf[i] = (int)j+1;

                /* Write to the dataset */
                ret = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);
                CHECK(ret, FAIL, "H5Dwrite");

                /* Close the dataset */
                ret = H5Dclose(did);
                CHECK(ret, FAIL, "H5Dclose");
                break;

            case 2:
                VERIFY(strategy, H5F_FSPACE_STRATEGY_FSM_AGGR, "H5Pget_file_space_strategy");
                VERIFY(persist, H5F_FREE_SPACE_PERSIST_DEF, "H5Pget_file_space_strategy");
                VERIFY(threshold, 2, "H5Pget_file_space_strategy");

                /* Open the dataset */
                did = H5Dopen2(fid, "/DSET_NONE", H5P_DEFAULT);
                CHECK(did, FAIL, "H5Dopen");

                for(i = 0; i < 24; i++)
                    wbuf[i] = (int)j+1;

                /* Write to the dataset */
                ret = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);
                CHECK(ret, FAIL, "H5Dwrite");

                /* Close the dataset */
                ret = H5Dclose(did);
                CHECK(ret, FAIL, "H5Dclose");
                break;

            case 3:
                VERIFY(strategy, H5F_FSPACE_STRATEGY_NONE, "H5Pget_file_space_strategy");
                VERIFY(persist, H5F_FREE_SPACE_PERSIST_DEF, "H5Pget_file_space_strategy");
                VERIFY(threshold, H5F_FREE_SPACE_THRESHOLD_DEF, "H5Pget_file_space_strategy");

                /* Open the dataset */
                did = H5Dopen2(fid, "/GROUP/DSET_NDATA_EA", H5P_DEFAULT);
                CHECK(did, FAIL, "H5Dopen");

                for(i = 0; i < 24; i++)
                    wbuf[i] = (int)j+1;

                /* Write to the dataset */
                ret = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);
                CHECK(ret, FAIL, "H5Dwrite");

                /* Close the dataset */
                ret = H5Dclose(did);
                CHECK(ret, FAIL, "H5Dclose");
                break;

            case 4:
                VERIFY(strategy, H5F_FSPACE_STRATEGY_AGGR, "H5Pget_file_space_strategy");
                VERIFY(persist, H5F_FREE_SPACE_PERSIST_DEF, "H5Pget_file_space_strategy");
                VERIFY(threshold, H5F_FREE_SPACE_THRESHOLD_DEF, "H5Pget_file_space_strategy");

                /* Open the dataset */
                did = H5Dopen2(fid, "/GROUP/DSET_NDATA_FA", H5P_DEFAULT);
                CHECK(did, FAIL, "H5Dopen");

                for(i = 0; i < 24; i++)
                    wbuf[i] = (int)j+1;

                /* Write to the dataset */
                ret = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);
                CHECK(ret, FAIL, "H5Dwrite");

                /* Close the dataset */
                ret = H5Dclose(did);
                CHECK(ret, FAIL, "H5Dclose");
                break;
            case 5:
                VERIFY(strategy, H5F_FSPACE_STRATEGY_FSM_AGGR, "H5Pget_file_space_strategy");
                VERIFY(persist, H5F_FREE_SPACE_PERSIST_DEF, "H5Pget_file_space_strategy");
                VERIFY(threshold, H5F_FREE_SPACE_THRESHOLD_DEF, "H5Pget_file_space_strategy");

                /* Open the dataset */
                did = H5Dopen2(fid, "/GROUP/DSET_NDATA_NONE", H5P_DEFAULT);
                CHECK(did, FAIL, "H5Dopen");

                for(i = 0; i < 24; i++)
                    wbuf[i] = (int)j+1;

                /* Write to the dataset */
                ret = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);
                CHECK(ret, FAIL, "H5Dwrite");

                /* Close the dataset */
                ret = H5Dclose(did);
                CHECK(ret, FAIL, "H5Dclose");
                break;

            default:
               break;
        }

        /* Close the plist */
        ret = H5Pclose(fcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Close the file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Re-Open the file */
        fid = H5Fopen(FILE5, H5F_ACC_RDONLY, H5P_DEFAULT);
        CHECK(fid, FAIL, "H5Fopen");

        switch(j) {
            case 0:
                /* Open and read the dataset */
                did = H5Dopen2(fid, "/DSET_EA", H5P_DEFAULT);
                CHECK(did, FAIL, "H5Dopen");

                ret = H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdbuf);
                CHECK(ret, FAIL, "H5Dread");

                /* Verify the data read is correct */
                for(i = 0; i < 24; i++)
                    VERIFY(rdbuf[i], j+1, "test_compatible");

                /* Close the dataset */
                ret = H5Dclose(did);
                CHECK(ret, FAIL, "H5Dclose");
                break;

            case 1:
                /* Open and read the dataset */
                did = H5Dopen2(fid, "/DSET_NDATA_BT2", H5P_DEFAULT);
                CHECK(did, FAIL, "H5Dopen");

                ret = H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdbuf);
                CHECK(ret, FAIL, "H5Dread");

                /* Verify the data read is correct */
                for(i = 0; i < 24; i++)
                    VERIFY(rdbuf[i], j+1, "test_compatible");

                /* Close the dataset */
                ret = H5Dclose(did);
                CHECK(ret, FAIL, "H5Dclose");
                break;

            case 2:
                /* Open and read the dataset */
                did = H5Dopen2(fid, "/DSET_NONE", H5P_DEFAULT);
                CHECK(did, FAIL, "H5Dopen");

                ret = H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdbuf);
                CHECK(ret, FAIL, "H5Dread");

                /* Verify the data read is correct */
                for(i = 0; i < 24; i++)
                    VERIFY(rdbuf[i], j+1, "test_compatible");

                /* Close the dataset */
                ret = H5Dclose(did);
                CHECK(ret, FAIL, "H5Dclose");
                break;

            case 3:
                /* Open and read the dataset */
                did = H5Dopen2(fid, "/GROUP/DSET_NDATA_EA", H5P_DEFAULT);
                CHECK(did, FAIL, "H5Dopen");

                ret = H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdbuf);
                CHECK(ret, FAIL, "H5Dread");

                /* Verify the data read is correct */
                for(i = 0; i < 24; i++)
                    VERIFY(rdbuf[i], j+1, "test_compatible");

                /* Close the dataset */
                ret = H5Dclose(did);
                CHECK(ret, FAIL, "H5Dclose");
                break;

            case 4:

                /* Open and read the dataset */
                did = H5Dopen2(fid, "/GROUP/DSET_NDATA_FA", H5P_DEFAULT);
                CHECK(did, FAIL, "H5Dopen");

                ret = H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdbuf);
                CHECK(ret, FAIL, "H5Dread");

                /* Verify the data read is correct */
                for(i = 0; i < 24; i++)
                    VERIFY(rdbuf[i], j+1, "test_compatible");

                /* Close the dataset */
                ret = H5Dclose(did);
                CHECK(ret, FAIL, "H5Dclose");
                break;

            case 5:

                /* Open and read the dataset */
                did = H5Dopen2(fid, "/GROUP/DSET_NDATA_NONE", H5P_DEFAULT);
                CHECK(did, FAIL, "H5Dopen");

                ret = H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdbuf);
                CHECK(ret, FAIL, "H5Dread");

                /* Verify the data read is correct */
                for(i = 0; i < 24; i++)
                    VERIFY(rdbuf[i], j+1, "test_compatible");

                /* Close the dataset */
                ret = H5Dclose(did);
                CHECK(ret, FAIL, "H5Dclose");
                break;

            default:
               break;
        }

        /* Close the file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");
    } /* end for */

} /* test_filespace_1_10_0_compatible */

/****************************************************************
**
**  test_filespace_round_compatible():
**    Verify that the trunk can open, read and modify these files--
**      1) They are initially created (via gen_filespace.c) in the trunk
**         with combinations of file space strategies, default/non-default
**         threshold, and file spacing paging enabled/disbled.
**         The library creates the file space info message with
**         "mark if unknown" in these files.
**      2) They are copied to the 1.8 branch, and are opened/read/modified
**         there via test_filespace_compatible() in test/tfile.c.
**         The 1.8 library marks the file space info message as "unknown"
**         in these files.
**      3) They are then copied back from the 1.8 branch to the trunk for
**         compatibility testing via this routine.
**      4) Upon encountering the file space info message which is marked
**         as "unknown", the library will use the default file space management
**         from then on: non-persistent free-space managers, default threshold,
**         and non-paging file space.
**
****************************************************************/
static void
test_filespace_round_compatible(void)
{
    hid_t    fid = -1;        /* File id */
    hid_t    fcpl = -1;        /* File creation property list ID */
    unsigned    j;            /* Local index variable */
    H5F_fspace_strategy_t strategy;    /* File space strategy */
    hbool_t     persist;        /* Persist free-space or not */
    hsize_t     threshold;        /* Free-space section threshold */
    hssize_t    free_space;        /* Amount of free space in the file */
    int         status;                 /* Status from copying the existing file */
    herr_t    ret;            /* Return value */

    /* Output message about test being performed */
    MESSAGE(5, ("File space compatibility testing for files from trunk to 1_8 to trunk\n"));

    for(j = 0; j < NELMTS(FSPACE_FILENAMES); j++) {
        /* Make a copy of the test file */
        status = h5_make_local_copy(FSPACE_FILENAMES[j], FILE5);
    CHECK(status, FAIL, "h5_make_local_copy");

    /* Open the temporary test file */
    fid = H5Fopen(FILE5, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Get the file's creation property list */
    fcpl = H5Fget_create_plist(fid);
    CHECK(fcpl, FAIL, "H5Fget_create_plist");

    ret = H5Pget_file_space_strategy(fcpl, &strategy, &persist, &threshold);
    CHECK(ret, FAIL, "H5Pget_file_space_strategy");
    VERIFY(strategy, H5F_FSPACE_STRATEGY_FSM_AGGR, "H5Pget_file_space_strategy");
    VERIFY(persist, FALSE, "H5Pget_file_space_strategy");
    VERIFY(threshold, 1, "H5Pget_file_space_strategy");

    /* There should not be any free space in the file */
    free_space = H5Fget_freespace(fid);
    CHECK(free_space, FAIL, "H5Fget_freespace");
    VERIFY(free_space, (hssize_t)0, "H5Fget_freespace");

    /* Closing */
    ret = H5Fclose(fid);
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Fclose");
    } /* end for */

} /* test_filespace_round_compatible */


/****************************************************************
**
**  test_libver_bounds_real():
**      Verify that a file created and modified with the
**      specified libver bounds has the specified object header
**      versions for the right objects.
**
****************************************************************/
static void
test_libver_bounds_real(H5F_libver_t libver_create, unsigned oh_vers_create,
    H5F_libver_t libver_mod, unsigned oh_vers_mod)
{
    hid_t       file, group;            /* Handles */
    hid_t       fapl;                   /* File access property list */
    H5O_info_t  oinfo;                  /* Object info */
    herr_t      ret;                    /* Return value */

    /*
     * Create a new file using the creation properties.
     */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    ret = H5Pset_libver_bounds(fapl, libver_create, H5F_LIBVER_LATEST);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    file = H5Fcreate("tfile5.h5", H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(file, FAIL, "H5Fcreate");

    /*
     * Make sure the root group has the correct object header version
     */
    ret = H5Oget_info_by_name2(file, "/", &oinfo, H5O_INFO_HDR, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.version, oh_vers_create, "H5Oget_info_by_name");

    /*
     * Reopen the file and make sure the root group still has the correct version
     */
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Pset_libver_bounds(fapl, libver_mod, H5F_LIBVER_LATEST);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    file = H5Fopen("tfile5.h5", H5F_ACC_RDWR, fapl);
    CHECK(file, FAIL, "H5Fopen");

    ret = H5Oget_info_by_name2(file, "/", &oinfo, H5O_INFO_HDR, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.version, oh_vers_create, "H5Oget_info_by_name");

    /*
     * Create a group named "G1" in the file, and make sure it has the correct
     * object header version
     */
    group = H5Gcreate2(file, "/G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, FAIL, "H5Gcreate");

    ret = H5Oget_info2(group, &oinfo, H5O_INFO_HDR);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.hdr.version, oh_vers_mod, "H5Oget_info");

    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /*
     * Create a group named "/G1/G3" in the file, and make sure it has the
     * correct object header version
     */
    group = H5Gcreate2(file, "/G1/G3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, FAIL, "H5Gcreate");

    ret = H5Oget_info2(group, &oinfo, H5O_INFO_HDR);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.version, oh_vers_mod, "H5Oget_info_by_name");

    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /*
     * Make sure the root group still has the correct object header version
     */
    ret = H5Oget_info_by_name2(file, "/", &oinfo, H5O_INFO_HDR, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.version, oh_vers_create, "H5Oget_info_by_name");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");
} /* end test_libver_bounds_real() */


/*-------------------------------------------------------------------------
 * Function:    test_libver_bounds_open
 *
 * Purpose:     Tests opening latest file with various low/high bounds.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
#define VERBFNAME        "tverbounds_dspace.h5"
#define VERBDSNAME       "dataset 1"
#define SPACE1_DIM1     3
static void
test_libver_bounds_open(void)
{
    hid_t file = -1;    /* File ID */
    hid_t space = -1;   /* Dataspace ID */
    hid_t dset = -1;    /* Dataset ID */
    hid_t fapl = -1;    /* File access property list ID */
    hid_t new_fapl = -1;/* File access property list ID for reopened file */
    hid_t dcpl = -1;    /* Dataset creation property list ID */
    hsize_t dim[1] = {SPACE1_DIM1}; /* Dataset dimensions */
    H5F_libver_t low, high;         /* File format bounds */
    hsize_t chunk_dim[1] = {SPACE1_DIM1}; /* Chunk dimensions */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Opening File in Various Version Bounds\n"));

    /* Create a file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    /* Create dataspace */
    space = H5Screate_simple(1, dim, NULL);
    CHECK(space, FAIL, "H5Screate_simple");

    /* Create a dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Create and set chunk plist */
    ret = H5Pset_chunk(dcpl, 1, chunk_dim);
    CHECK(ret, FAIL, "H5Pset_chunk");
    ret = H5Pset_deflate(dcpl, 9);
    CHECK(ret, FAIL, "H5Pset_deflate");
    ret = H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS);
    CHECK(ret, FAIL, "H5Pset_chunk_opts");

    /* Create a file with (LATEST, LATEST) bounds, create a layout version 4
       dataset, then close the file */

    /* Set version bounds to (LATEST, LATEST) */
    low = H5F_LIBVER_LATEST;
    high = H5F_LIBVER_LATEST;
    ret = H5Pset_libver_bounds(fapl, low, high);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /* Create the file */
    file = H5Fcreate(VERBFNAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(file, FAIL, "H5Fcreate");

    /* Create dataset */
    dset = H5Dcreate2(file, VERBDSNAME, H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate2");

    /* Close dataset and file */
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Attempt to open latest file with (earliest, v18), should fail */
    ret = H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_V18);
    H5E_BEGIN_TRY {
        file = H5Fopen(VERBFNAME, H5F_ACC_RDONLY, fapl);
    } H5E_END_TRY;
    VERIFY(file, FAIL, "Attempted to open latest file with earliest version");

    /* Attempt to open latest file with (v18, v18), should fail */
    ret = H5Pset_libver_bounds(fapl, H5F_LIBVER_V18, H5F_LIBVER_V18);
    H5E_BEGIN_TRY {
        file = H5Fopen(VERBFNAME, H5F_ACC_RDONLY, fapl);
    } H5E_END_TRY;
    VERIFY(file, FAIL, "Attempted to open latest file with v18 bounds");

    /* Opening VERBFNAME in these combination should succeed.
       For each low bound, verify that it is upgraded properly */
    high = H5F_LIBVER_LATEST;
    for (low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++)
    {
        H5F_libver_t new_low = H5F_LIBVER_EARLIEST;

        /* Set version bounds for opening file */
        ret = H5Pset_libver_bounds(fapl, low, high);
        CHECK(ret, FAIL, "H5Pset_libver_bounds");

        /* Open the file */
        file = H5Fopen(VERBFNAME, H5F_ACC_RDONLY, fapl);
        CHECK(file, FAIL, "H5Fopen");

        /* Get the new file access property */
        new_fapl = H5Fget_access_plist(file);
        CHECK(new_fapl, FAIL, "H5Fget_access_plist");

        /* Get new low bound and verify that it has been upgraded properly */
        ret = H5Pget_libver_bounds(new_fapl, &new_low, NULL);
        CHECK(ret, FAIL, "H5Pget_libver_bounds");
        VERIFY(new_low, H5F_LIBVER_LATEST, "Low bound should be upgraded to H5F_LIBVER_LATEST");

        ret = H5Pclose(new_fapl);
        CHECK(ret, FAIL, "H5Pclose");
        ret = H5Fclose(file);
        CHECK(ret, FAIL, "H5Fclose");
    } /* for low */

    /* Close dataspace and property lists */
    ret = H5Sclose(space);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");
} /* end test_libver_bounds_open() */


/*-------------------------------------------------------------------------
 * Function:    test_libver_bounds_copy
 *
 * Purpose:     Test to verify HDFFV-10800 is fixed:
 *              This test is copied from the user test program: copy10.c.
 *              (See attached programs in the jira issue.)
 *
 *              The source file used in the test is generated by the user test
 *              program "fill18.c" with the 1.8 library.  The file is created
 *              with the latest format and the dataset created in the file
 *              has version 3 fill value message (latest).
 *
 *              The test creates the destination file with (v18, v18) version bounds.
 *              H5Ocopy() should succeed in copying the dataset in the source file
 *              to the destination file.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static void
test_libver_bounds_copy(void)
{
    hid_t src_fid = -1;     /* File ID */
    hid_t dst_fid = -1;     /* File ID */
    hid_t fapl = -1;        /* File access property list ID */
    const char *src_fname;  /* Source file name */
    herr_t ret;             /* Generic return value */

    /* Output message about the test being performed */
    MESSAGE(5, ("Testing H5Ocopy a dataset in a 1.8 library file to a 1.10 library file\n"));

    /* Get the test file name */
    src_fname = H5_get_srcdir_filename(SRC_FILE);

    /* Open the source test file */
    src_fid = H5Fopen(src_fname, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(src_fid, FAIL, "H5Fopen");

    /* Create file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    /* Set library version bounds to (v18, v18) */
    ret = H5Pset_libver_bounds(fapl, H5F_LIBVER_V18, H5F_LIBVER_V18);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /* Create the destination file with the fapl */
    dst_fid = H5Fcreate(DST_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(dst_fid, FAIL, "H5Pcreate");

    /* Close the fapl */
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Copy the dataset in the source file to the destination file */
    ret = H5Ocopy(src_fid, DSET_DS1, dst_fid, DSET_DS1, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(ret, SUCCEED, "H5Ocopy");

    /* Close the source file */
    ret = H5Fclose(src_fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close the destination file */
    ret = H5Fclose(dst_fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Remove the destination file */
    HDremove(DST_FILE);

} /* end test_libver_bounds_copy() */

/****************************************************************
**
**  test_libver_bounds():
**      Verify that a file created and modified with various
**      libver bounds is handled correctly.  (Further testing
**      welcome)
**
****************************************************************/
static void
test_libver_bounds(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing setting library version bounds\n"));

    /* Run the tests */
    test_libver_bounds_real(H5F_LIBVER_EARLIEST, 1, H5F_LIBVER_LATEST, 2);
    test_libver_bounds_real(H5F_LIBVER_LATEST, 2, H5F_LIBVER_EARLIEST, 2);
    test_libver_bounds_open();
    test_libver_bounds_copy();
} /* end test_libver_bounds() */

/**************************************************************************************
**
**  test_libver_bounds_low_high():
**      Tests to verify that format versions are correct with the following five
**      pairs of low/high version bounds set in fapl via H5Pset_libver_bounds():
**          (1) (earliest, v18)
**          (2) (earliest, v110)
**          (3) (v18, v18)
**          (4) (v18, v110)
**          (5) (v110, v110)
**
**      For each pair of setting in fapl, verify format versions with the following
**      six tests:
**          (1) test_libver_bounds_super(fapl): superblock versions
**          (2) test_libver_bounds_obj(fapl): object header versions
**          (3) test_libver_bounds_dataset(fapl): message versions associated with dataset
**          (4) test_libver_bounds_dataspace(fapl): dataspace message versions
**          (5) test_libver_bounds_datatype(fapl): datatype message versions
**          (6) test_libver_bounds_attributes(fapl): attribute message versions
**
**************************************************************************************/
static void
test_libver_bounds_low_high(void)
{
    hid_t fapl = H5I_INVALID_HID;  /* File access property list */
    H5F_libver_t low, high;         /* Low and high bounds */
    herr_t ret;                     /* The return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing setting (low, high) format version bounds\n"));

    /* Create a file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, H5I_INVALID_HID, "H5Pcreate");

    /* Loop through all the combinations of low/high version bounds */
    for(low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++)
        for(high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {

            H5E_BEGIN_TRY {
                /* Set the low/high version bounds */
                ret = H5Pset_libver_bounds(fapl, low, high);
            } H5E_END_TRY;

            /* Should fail: invalid combinations */
            if(high == H5F_LIBVER_EARLIEST) {
                VERIFY(ret, FAIL, "H5Pset_libver_bounds");
                continue;
            }

            /* Should fail: invalid combinations */
            if(high < low) {
                VERIFY(ret, FAIL, "H5Pset_libver_bounds");
                continue;
            }

            /* All other combinations are valid and should succeed */
            VERIFY(ret, SUCCEED, "H5Pset_libver_bounds");

            /* Tests to verify version bounds */
            test_libver_bounds_super(fapl);
            test_libver_bounds_obj(fapl);
            test_libver_bounds_dataset(fapl);
            test_libver_bounds_dataspace(fapl);
            test_libver_bounds_datatype(fapl);
            test_libver_bounds_attributes(fapl);
        }

    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

} /* end test_libver_bounds_low_high() */


/***********************************************************************
**
**  test_libver_bounds_super():
**      Verify superblock version with the following two tests:
**            (1) test_libver_bounds_super_create():
**              --when creating a file with the input fapl and the fcpl
**                that has the following feature enabled:
**                (A) default fcpl
**                (B) fcpl with v1-btee K value enabled
**                (C) fcpl with shared messages enabled
**                (D) fcpl with persistent free-space manager enabled
**
**            (2) test_libver_bounds_super_open():
**              --when opening a file which is created with the input fapl
**                and the fcpl setting as #A to #D above.
**
**     These two tests are run with or without SWMR file access.
**
*************************************************************************/
static void
test_libver_bounds_super(hid_t fapl)
{
    hid_t fcpl = H5I_INVALID_HID;   /* File creation property list */
    herr_t ret;                     /* The return value */

    /* Create a default fcpl: #A */
    /* This will result in superblock version 0 */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, H5I_INVALID_HID, "H5Pcreate");

    /* Verify superblock version when creating a file with input fapl,
       fcpl #A and with/without SWMR access */
    test_libver_bounds_super_create(fapl, fcpl, TRUE, FALSE);
    test_libver_bounds_super_create(fapl, fcpl, FALSE, FALSE);

    /* Verify superblock version when opening a file which is created
       with input fapl, fcpl #A and with/without SWMR access */
    test_libver_bounds_super_open(fapl, fcpl, TRUE, FALSE);
    test_libver_bounds_super_open(fapl, fcpl, FALSE, FALSE);

    /* Close the fcpl */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Create a fcpl with v1-btree K value enabled: #B */
    /* This will result in superblock version 1 */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, H5I_INVALID_HID, "H5Pcreate");
    ret = H5Pset_istore_k(fcpl, 64);
    CHECK(ret, FAIL, "H5Pset_istore_k");

    /* Verify superblock version when creating a file with input fapl,
       fcpl #B and with/without SWMR access */
    test_libver_bounds_super_create(fapl, fcpl, TRUE, FALSE);
    test_libver_bounds_super_create(fapl, fcpl, FALSE, FALSE);

    /* Verify superblock version when opening a file which is created
       with input fapl, fcpl #B and with/without SWMR access */
    test_libver_bounds_super_open(fapl, fcpl, TRUE, FALSE);
    test_libver_bounds_super_open(fapl, fcpl, FALSE, FALSE);

    /* Close the fcpl */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Create a fcpl with shared messages enabled: #C */
    /* This will result in superblock version 2 */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, H5I_INVALID_HID, "H5Pcreate");
    ret = H5Pset_shared_mesg_nindexes(fcpl, 1);
    CHECK(ret, FAIL, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl, 0, H5O_SHMESG_ATTR_FLAG, 2);
    CHECK(ret, FAIL, "H5Pset_shared_mesg_index");

    /* Verify superblock version when creating a file with input fapl,
       fcpl #C and with/without SWMR access */
    test_libver_bounds_super_create(fapl, fcpl, TRUE, FALSE);
    test_libver_bounds_super_create(fapl, fcpl, FALSE, FALSE);

    /* Verify superblock version when opening a file which is created
       with input fapl, fcpl #C and with/without SWMR access */
    test_libver_bounds_super_open(fapl, fcpl, TRUE, FALSE);
    test_libver_bounds_super_open(fapl, fcpl, FALSE, FALSE);

    /* Close the fcpl */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Create a fcpl with persistent free-space manager enabled: #D */
    /* This will result in superblock version 2 */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, H5I_INVALID_HID, "H5Pcreate");
    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_FSM_AGGR, 1, (hsize_t)1);
    CHECK(ret, FAIL, "H5Pset_file_space");

    /* Verify superblock version when creating a file with input fapl,
       fcpl #D and with/without SWMR access */
    test_libver_bounds_super_create(fapl, fcpl, TRUE, TRUE);
    test_libver_bounds_super_create(fapl, fcpl, FALSE, TRUE);

    /* Verify superblock version when opening a file which is created
       with input fapl, fcpl #D and with/without SWMR access */
    test_libver_bounds_super_open(fapl, fcpl, TRUE, TRUE);
    test_libver_bounds_super_open(fapl, fcpl, FALSE, TRUE);

    /* Close the fcpl */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

} /* end test_libver_bounds_super() */


/**************************************************************************************************
**
**  test_libver_bounds_super_create():
**      Verify the following when the file is created with the input fapl, fcpl,
**      and with/without SWMR access:
**          (a) the superblock version #
**          (b) the file's low bound setting
**          (c) fail or succeed in creating the file
**
**      For file creation, the bounds setting in fapl, the feature enabled in fcpl,
**      and with/without SWMR file access will determine the results for #a to #c.
**
**      The first row for the following two tables is the 5 pairs of low/high bounds setting
**      in the input fapl. The next three rows list the expected results for #a to #c.
**      "-->" indicates "upgrade to"
**
**      The last table lists the expected results in creating the file when non-default
**      free-space info (fsinfo) is enabled in fcpl.
**
**                                  Creating a file with write access
**                    --------------------------------------------------------------------------------
**                    | (earliest, v18) | (earliest, v110) | (v18, v18) | (v18, v110) | (v110, v110) |
**                    |______________________________________________________________________________|
** Superblock version | vers 0, 1, 2    | vers 0, 1, 2     | vers 2     | vers 2      | vers 3       |
**                    |------------------------------------------------------------------------------|
** File's low bound   |                            no change                                         |
**                    |------------------------------------------------------------------------------|
** File creation      |                             succeed                                          |
**                    |______________________________________________________________________________|
**
**                                  Creating a file with SWMR-write access
**                    --------------------------------------------------------------------------------
**                    | (earliest, v18) | (earliest, v110) | (v18, v18) | (v18, v110) | (v110, v110) |
**                    |______________________________________________________________________________|
** Superblock version | --              | vers 3           | --         | vers  3     | vers 3       |
**                    |------------------------------------------------------------------------------|
** File's low bound   | --              | ->v110           | --         | ->v110      | no change    |
**                    |------------------------------------------------------------------------------|
** File creation      | fail            | succeed          | fail       | succeed     | succeed      |
**                    |______________________________________________________________________________|
**
**                                  Creating a file with write/SWMR-write access + non-default fsinfo
**                    --------------------------------------------------------------------------------
**                    | (earliest, v18) | (earliest, v110) | (v18, v18) | (v18, v110) | (v110, v110) |
**                    |______________________________________________________________________________|
** File creation      |  fail            | succeed         | fail       | succeed      | succeed     |
**                    |______________________________________________________________________________|
**
******************************************************************************************************/
static void
test_libver_bounds_super_create(hid_t fapl, hid_t fcpl, htri_t is_swmr, htri_t non_def_fsm)
{
    hid_t fid = H5I_INVALID_HID;    /* File ID */
    H5F_t *f = NULL;                /* Internal file pointer */
    H5F_libver_t low, high;         /* Low and high bounds */
    hbool_t ok;                     /* The result is ok or not */
    herr_t ret;                 /* The return value */

    /* Try to create the file */
    H5E_BEGIN_TRY {
        fid = H5Fcreate(FILE8, H5F_ACC_TRUNC | (is_swmr ? H5F_ACC_SWMR_WRITE : 0), fcpl, fapl);
    } H5E_END_TRY;

    /* Get the internal file pointer if the create succeeds */
    if(fid >= 0) {
        f = (H5F_t *)H5I_object(fid);
        CHECK(f, NULL, "H5I_object");
    }

    /* Retrieve the low/high bounds */
    ret = H5Pget_libver_bounds(fapl, &low, &high);
    CHECK(ret, FAIL, "H5Pget_libver_bounds");

    if(non_def_fsm && high < H5F_LIBVER_LATEST)
        VERIFY(fid, H5I_INVALID_HID, "H5Fcreate");

    else if(is_swmr) { /* SWMR is enabled */
        if(high == H5F_LIBVER_LATEST) { /* Should succeed */
            VERIFY(fid >= 0, TRUE, "H5Fcreate");
            VERIFY(HDF5_SUPERBLOCK_VERSION_3, f->shared->sblock->super_vers, "HDF5_superblock_ver_bounds");
            VERIFY(H5F_LIBVER_V110, f->shared->low_bound, "HDF5_superblock_ver_bounds");

        } else /* Should fail */
            VERIFY(fid >= 0, FALSE, "H5Fcreate");

    } else { /* Should succeed */
        VERIFY(fid >= 0, TRUE, "H5Fcreate");
        VERIFY(low, f->shared->low_bound, "HDF5_superblock_ver_bounds");

        switch(low) {
            case H5F_LIBVER_EARLIEST:
               ok = (f->shared->sblock->super_vers == HDF5_SUPERBLOCK_VERSION_DEF  ||
                     f->shared->sblock->super_vers == HDF5_SUPERBLOCK_VERSION_1 ||
                     f->shared->sblock->super_vers == HDF5_SUPERBLOCK_VERSION_2);
               VERIFY(ok, TRUE, "HDF5_superblock_ver_bounds");
               break;

            case H5F_LIBVER_V18:
               ok = (f->shared->sblock->super_vers == HDF5_SUPERBLOCK_VERSION_2);
               VERIFY(ok, TRUE, "HDF5_superblock_ver_bounds");
               break;

            case H5F_LIBVER_V110:
                ok = (f->shared->sblock->super_vers == HDF5_SUPERBLOCK_VERSION_3);
                VERIFY(ok, TRUE, "HDF5_superblock_ver_bounds");
                break;

            case H5F_LIBVER_ERROR:
            case H5F_LIBVER_NBOUNDS:
            default:
                ERROR("H5Pget_libver_bounds");

        } /* end switch */

    } /* end else */

    if(fid >= 0) { /* Close the file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");
    }

} /* end test_libver_bounds_super_create() */

/**************************************************************************************************
**
**  test_libver_bounds_super_open():
**      Verify the following when opening a file which is created with the input fapl, fcpl,
**      and with/without SWMR access:
**          (a) the file's low bound setting
**          (b) fail or succeed in opening the file
**
**      (1) Create a file with the input fapl, fcpl and with/without SWMR access
**      (2) Close the file
**      (3) Reopen the file with a new fapl that is set to the 5 pairs of low/high bounds
**          in a for loop. For each pair of setting in the new fapl:
**          --Verify the expected results for #a and #b above.
**          --Close the file.
**
**      For file open, the file's superblock version, the low/high bounds setting in fapl,
**      and with/without SWMR file access will determine the results for #a and #b.
**
**      The first row for the following tables (#A - #B) is the 5 pairs of low/high bounds setting
**      in the input fapl. The next two rows list the expected results for #a and #b.
**      "-->" indicates "upgrade to"
**
**      The last table (#C) lists the expected results in opening the file when non-default
**      free-space info (fsinfo) is enabled in fcpl.
**
**                      (A) Opening a file with write access
**
**                                              Superblock version 0, 1
**                  --------------------------------------------------------------------------------
**                  | (earliest, v18) | (earliest, v110) | (v18, v18) | (v18, v110) | (v110, v110) |
**                  |______________________________________________________________________________|
** File's low bound |                               no change                                      |
**                  |------------------------------------------------------------------------------|
** File open        |                               succeed                                        |
**                  |______________________________________________________________________________|
**
**
**                                              Superblock version 2
**                  --------------------------------------------------------------------------------
**                  | (earliest, v18) | (earliest, v110) | (v18, v18) | (v18, v110) | (v110, v110) |
**                  |______________________________________________________________________________|
** File's low bound |               -->v18               |               no change                 |
**                  |------------------------------------------------------------------------------|
** File open        |                               succeed                                        |
**                  |______________________________________________________________________________|
**
**                                              Superblock version 3
**                  --------------------------------------------------------------------------------
**                  | (earliest, v18) | (earliest, v110) | (v18, v18) | (v18, v110) | (v110, v110) |
**                  |______________________________________________________________________________|
** File's low bound | --              | -->v110          | --         | -->v110     | no change    |
**                  |------------------------------------------------------------------------------|
** File open        | fail            | succeed          | fail       | succeed     | succeed      |
**                  |______________________________________________________________________________|
**
**
**
**                     (B) Opening a file with SWMR-write access
**
**                                              Superblock version 0, 1, 2
**                  -------------------------------------------------------------------------------
**                  | (earliest, v18) | (earliest, v10) | (v18, v18) | (v18, v110) | (v110, v110) |
**                  |_____________________________________________________________________________|
** File's low bound |                                   ----
**                  |-----------------------------------------------------------------------------|
** File open        |                                   fail
**                  |_____________________________________________________________________________|
**
**
**                                              Superblock version 3
**                  -------------------------------------------------------------------------------
**                  | (earliest, v18) | (earliest, v10) | (v18, v18) | (v18, v110) | (v110, v110) |
**                  |_____________________________________________________________________________|
** File's low bound | --              | -->v110         | --         | -->v110     | no change    |
**                  |-----------------------------------------------------------------------------|
** File open        | fail            | succeed         | fail       | succeed     | succeed      |
**                  |_____________________________________________________________________________|
**
**
**                      (C) Opening a file with write/SWMR-write access + non-default fsinfo
**                  -------------------------------------------------------------------------------
**                  | (earliest, v18) | (earliest, v10) | (v18, v18) | (v18, v110) | (v110, v110) |
**                  |_____________________________________________________________________________|
** File open        | fail            | succeed         | fail       | succeed     | succeed      |
**                  |_____________________________________________________________________________|
**
**
******************************************************************************************************/
static void
test_libver_bounds_super_open(hid_t fapl, hid_t fcpl, htri_t is_swmr, htri_t non_def_fsm)
{
    hid_t fid = H5I_INVALID_HID;        /* File ID */
    H5F_t *f = NULL;                    /* Internal file pointer */
    hid_t new_fapl = H5I_INVALID_HID;   /* File access property list */
    unsigned super_vers;    /* Superblock version */
    H5F_libver_t low, high; /* Low and high bounds */
    herr_t ret;                /* Return value */

    /* Create the file with the input fcpl and fapl */
    H5E_BEGIN_TRY {
        fid = H5Fcreate(FILE8, H5F_ACC_TRUNC, fcpl, fapl);
    } H5E_END_TRY;

    /* Retrieve the low/high bounds */
    ret = H5Pget_libver_bounds(fapl, &low, &high);
    CHECK(ret, FAIL, "H5Pget_libver_bounds");

    if(non_def_fsm && high < H5F_LIBVER_LATEST) {
        VERIFY(fid, H5I_INVALID_HID, "H5Fcreate");

    } else {
        VERIFY(fid >= 0, TRUE, "H5Fcreate");

        /* Get the internal file pointer */
        f = (H5F_t *)H5I_object(fid);
        CHECK(f, NULL, "H5I_object");

        /* The file's superblock version */
        super_vers = f->shared->sblock->super_vers;

        /* Close the file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Create a default file access property list */
        new_fapl = H5Pcreate(H5P_FILE_ACCESS);
        CHECK(new_fapl, FAIL, "H5Pcreate");

        /* Loop through all the combinations of low/high bounds in new_fapl */
        for(low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; H5_INC_ENUM(H5F_libver_t, low)) {
            for(high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; H5_INC_ENUM(H5F_libver_t, high)) {
                H5E_BEGIN_TRY {
                    ret = H5Pset_libver_bounds(new_fapl, low, high);
                } H5E_END_TRY;

                /* Invalid combinations */
                if (ret < 0)
                    continue;

                /* Open the file with or without SWMR access */
                H5E_BEGIN_TRY {
                    fid = H5Fopen(FILE8, H5F_ACC_RDWR | (is_swmr ? H5F_ACC_SWMR_WRITE : 0), new_fapl);
                } H5E_END_TRY;

                if(non_def_fsm && high < H5F_LIBVER_LATEST) {
                    VERIFY(fid, H5I_INVALID_HID, "H5Fopen");
                    continue;
                }

                /* Get the internal file pointer if the open succeeds */
                if(fid >= 0) {
                    f = (H5F_t *)H5I_object(fid);
                    CHECK(f, NULL, "H5I_object");
                }

                /* Verify the file open succeeds or fails */
                switch(super_vers) {
                    case 3:
                        if(high == H5F_LIBVER_LATEST) {
                            /* Should succeed */
                            VERIFY(fid >= 0, TRUE, "H5Fopen");
                            VERIFY(H5F_LIBVER_V110, f->shared->low_bound, "HDF5_superblock_ver_bounds");

                            /* Close the file */
                            ret = H5Fclose(fid);
                            CHECK(ret, FAIL, "H5Fclose");
                        } else /* Should fail */
                            VERIFY(fid >= 0, FALSE, "H5Fopen");
                        break;

                    case 2:
                        if(is_swmr)  /* Should fail */
                            VERIFY(fid >= 0, FALSE, "H5Fopen");
                        else { /* Should succeed */
                            VERIFY(fid >= 0, TRUE, "H5Fopen");
                            VERIFY(f->shared->low_bound >= H5F_LIBVER_V18, TRUE, "HDF5_superblock_ver_bounds");

                            /* Close the file */
                            ret = H5Fclose(fid);
                            CHECK(ret, FAIL, "H5Fclose");
                        }
                        break;

                    case 1:
                    case 0:
                        if(is_swmr)  /* Should fail */
                            VERIFY(fid >= 0, FALSE, "H5Fopen");
                        else { /* Should succeed */
                            VERIFY(fid >= 0, TRUE, "H5Fopen");
                            VERIFY(f->shared->low_bound, low, "HDF5_superblock_ver_bounds");

                            ret = H5Fclose(fid);
                            CHECK(ret, FAIL, "H5Fclose");
                        }
                        break;

                    default:
                        break;
                } /* end switch */
            } /* end for */
        } /* end for */

        /* Close the file access property list */
        ret = H5Pclose(new_fapl);
        CHECK(ret, FAIL, "H5Pclose");
    } /* end else */

} /* end test_libver_bounds_super_open() */

/****************************************************************
**
**  test_libver_bounds_obj():
**      Verify object header versions:
**
**      (a) Create a file with:
**          --the input fapl
**          --a fcpl that has shared message enabled
**          Verify the root group's object header version.
**          Close the file.
**
**      (b) Create another file with:
**          --the input fapl
**          --a default fcpl
**          Verify the root group's object header version.
**          Close the file.
**
**      (c) Reopen the same file in (b) with a new fapl.
**          The new fapl is set to the 5 pairs of low/high
**          bounds in a "for" loop.  For each setting in fapl:
**          --Create a group in the file
**          --Verify the group's object header version
**          --Close and delete the group
**          --Close the file
**
****************************************************************/
static void
test_libver_bounds_obj(hid_t fapl)
{
    hid_t fid = H5I_INVALID_HID;        /* File ID */
    hid_t gid = H5I_INVALID_HID;        /* Group ID */
    hid_t fcpl = H5I_INVALID_HID;       /* File creation property list */
    hid_t new_fapl = H5I_INVALID_HID;   /* File access property list */
    H5F_t *f = NULL;        /* Internal file pointer */
    H5F_libver_t low, high; /* Low and high bounds */
    H5O_info_t  oinfo;      /* Object info */
    H5G_info_t ginfo;       /* Group info */
    herr_t ret;             /* Return value */

    /* Retrieve the low/high bounds from the input fapl */
    ret = H5Pget_libver_bounds(fapl, &low, &high);
    CHECK(ret, FAIL, "H5Pget_libver_bounds");

    /* Create a default file creation property list */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, H5I_INVALID_HID, "H5Pcreate");

    /* Enable shared message in the fcpl */
    /* This will result in a version 2 object header */
    ret = H5Pset_shared_mesg_nindexes(fcpl, 1);
    CHECK(ret, FAIL, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl, 0, H5O_SHMESG_ATTR_FLAG, 2);
    CHECK(ret, FAIL, "H5Pset_shared_mesg_index");

    /* Create the file with the fcpl and the input fapl */
    fid = H5Fcreate(FILE8, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, H5I_INVALID_HID, "H5Fcreate");

    /* Get root group's object info */
    ret = H5Oget_info_by_name2(fid, "/", &oinfo, H5O_INFO_HDR, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");

    /* Verify object header version is 2 because shared message is enabled */
    VERIFY(oinfo.hdr.version, H5O_VERSION_2, "H5O_obj_ver_bounds");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close the file creation property list */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Create a file with the default fcpl and input fapl */
    fid = H5Fcreate(FILE8, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid, H5I_INVALID_HID, "H5Fcreate");

    /* Get root group's object info */
    ret = H5Oget_info_by_name2(fid, "/", &oinfo, H5O_INFO_HDR, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");

    /* Verify object header version is as indicated by low_bound */
    VERIFY(oinfo.hdr.version, H5O_obj_ver_bounds[low], "H5O_obj_ver_bounds");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Create a new default file access property list which
       is used to open the file in the "for" loop */
    new_fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(new_fapl, H5I_INVALID_HID, "H5Pcreate");

    /* Loop through all the combinations of low/high bounds in new_fapl */
    /* Open the file with the fapl; create a group and verify the
       object header version, then delete the group and close the file.*/
    for(low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for(high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {
            H5E_BEGIN_TRY {
                ret = H5Pset_libver_bounds(new_fapl, low, high);
            } H5E_END_TRY;

            if (ret < 0) /* Invalid combinations */
                continue;

            /* Open the file */
            H5E_BEGIN_TRY {
                fid = H5Fopen(FILE8, H5F_ACC_RDWR, new_fapl);
            } H5E_END_TRY;

            if(fid >=0 ) { /* The file open succeeds */

                /* Get the internal file pointer */
                f = (H5F_t *)H5I_object(fid);
                CHECK(f, NULL, "H5I_object");

                /* Create a group in the file */
                gid = H5Gcreate2(fid, GRP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(gid, FAIL, "H5Gcreate2");

                /* Get group information */
                ret = H5Gget_info(gid, &ginfo);
                CHECK(ret, FAIL, "H5Gget_info");

                /* Verify group storage type */
                if(f->shared->low_bound >= H5F_LIBVER_V18)
                    /* Links in group are stored in object header */
                    VERIFY(ginfo.storage_type, H5G_STORAGE_TYPE_COMPACT, "H5Gget_info");
                else
                    /* Links in group are stored with a "symbol table" */
                    VERIFY(ginfo.storage_type, H5G_STORAGE_TYPE_SYMBOL_TABLE, "H5Gget_info");

                /* Get object header information */
                ret = H5Oget_info_by_name2(gid, GRP_NAME, &oinfo, H5O_INFO_HDR, H5P_DEFAULT);
                CHECK(ret, FAIL, "H5Oget_info_by_name");

                /* Verify object header version as indicated by low_bound */
                VERIFY(oinfo.hdr.version, H5O_obj_ver_bounds[f->shared->low_bound], "H5O_obj_ver_bounds");

                /* Close the group */
                ret = H5Gclose(gid);
                CHECK(ret, FAIL, "H5Gclose");

                /* Delete the group */
                ret = H5Ldelete(fid, GRP_NAME, H5P_DEFAULT);
                CHECK(ret, FAIL, "H5Ldelete");

                /* Close the file */
                ret = H5Fclose(fid);
                CHECK(ret, FAIL, "H5Fclose");

            } /* end if */
        } /* end for */
    } /* end for */

    /* Close the file access property list */
    ret = H5Pclose(new_fapl);
    CHECK(ret, FAIL, "H5Pclose");

} /* end test_libver_bounds_obj() */

/****************************************************************
**
**  test_libver_bounds_dataset():
**      Verify message versions associated with datasets:
**
**      (a) Create a file with default fcpl and the input fapl.
**          Create the following two datasets:
**              --A contiguous dataset
**              --A chunked dataset with "no filter edge chunks"
**          For both datasets, verify the versions for the layout,
**          fill value and filter pipeline messages.
**          Close the file.
**
**      (b) Create a new fapl that is set to the 5 pairs of low/high
**          bounds in a "for" loop.  For each pair of setting in the
**          new fapl:
**              --Open the same file in (a) with the fapl
**              --Create a chunked dataset with 2 unlimited
**                dimensions
**              --Verify the versions for the layout, fill value
**                and filter pipeline messages
**              --Close and delete the dataset
**              --Close the file
**
****************************************************************/
static void
test_libver_bounds_dataset(hid_t fapl)
{
    hid_t fid = H5I_INVALID_HID;        /* File ID */
    hid_t new_fapl = H5I_INVALID_HID;   /* File access property list */
    hid_t did = H5I_INVALID_HID;         /* Dataset ID */
    hid_t sid = H5I_INVALID_HID;         /* Dataspace ID */
    hid_t dcpl = H5I_INVALID_HID;        /* Dataset creation property list */
    H5D_t *dset = NULL;     /* Internal dataset pointer */
    H5F_t *f = NULL;        /* Internal file pointer */
    H5F_libver_t low, high;             /* Low and high bounds */
    herr_t ret;                         /* Return value */
    hsize_t fix_dims2[2] = {10, 4};     /* Dimension sizes */
    hsize_t fix_chunks2[2] = {4, 3};    /* Chunk dimension sizes */
    hsize_t dims2[2] = {1, 4};          /* Dimension sizes */
    hsize_t max_dims2[2] = {H5S_UNLIMITED, H5S_UNLIMITED};  /* Maximum dimension sizes */
    hsize_t chunks2[2] = {4, 5};        /* Chunk dimension sizes */

    /* Retrieve the low/high bounds from the input fapl */
    ret = H5Pget_libver_bounds(fapl, &low, &high);
    CHECK(ret, FAIL, "H5Pget_libver_bounds");

    /* Create the file with the input fapl */
    fid = H5Fcreate(FILE8, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid, H5I_INVALID_HID, "H5Fcreate");

    /* Create the dataspace */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, H5I_INVALID_HID, "H5Screate");

    /* Create a contiguous dataset */
    did = H5Dcreate2(fid, DSETA, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did, H5I_INVALID_HID, "H5Dcreate");

    /* Get the internal dataset pointer */
    dset = (H5D_t *)H5I_object(did);
    CHECK(dset, NULL, "H5I_object");

    /* Verify version for layout and fill value messages */
    if(low == H5F_LIBVER_EARLIEST) {
        /* For layout message: the earliest version the library will set is 3 */
        /* For fill value message: the earliest version the library will set is 2 */
        VERIFY(dset->shared->layout.version, H5O_LAYOUT_VERSION_DEFAULT, "H5O_layout_ver_bounds");
        VERIFY(dset->shared->dcpl_cache.fill.version, H5O_FILL_VERSION_2, "H5O_fill_ver_bounds");
    } else {
        VERIFY(dset->shared->layout.version, H5O_layout_ver_bounds[low], "H5O_layout_ver_bounds");
        VERIFY(dset->shared->dcpl_cache.fill.version, H5O_fill_ver_bounds[low], "H5O_fill_ver_bounds");
    }

    /* Verify filter pipleline message version */
    VERIFY(dset->shared->dcpl_cache.pline.version, H5O_pline_ver_bounds[low], "H5O_pline_ver_bounds");

    /* Close the dataset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close the dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Set up dataspace and dcpl for creating a chunked dataset
       with "no filter edge chunks" enabled.
       This will result in a version 4 layout message */
    sid = H5Screate_simple(2, fix_dims2, NULL);
    CHECK(sid, H5I_INVALID_HID, "H5Screate_simple");
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, H5I_INVALID_HID, "H5Pcreate");
    ret = H5Pset_chunk(dcpl, 2, fix_chunks2);
    CHECK(ret, FAIL, "H5Pset_chunk");
    ret = H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS);
    CHECK(ret, FAIL, "H5Pset_chunk_opts");

    /* Create the chunked dataset */
    H5E_BEGIN_TRY {
        did = H5Dcreate2(fid, DSETB, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;

    if(did >= 0) {

        /* Get the internal dataset pointer */
        dset = (H5D_t *)H5I_object(did);
        CHECK(dset, NULL, "H5I_object");

        /* Verify layout message version and chunk indexing type */
        VERIFY(dset->shared->layout.version, H5O_LAYOUT_VERSION_4, "H5O_layout_ver_bounds");
        VERIFY(dset->shared->layout.u.chunk.idx_type, H5D_CHUNK_IDX_FARRAY, "chunk_index_type");

        /* Close the dataset */
        ret = H5Dclose(did);
        CHECK(ret, FAIL, "H5Dclose");
    }

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close the dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close the datset creation property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Create a default file access property list which is used
       to open the file in the 'for' loop */
    new_fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(new_fapl, H5I_INVALID_HID, "H5Pcreate");

    /* Set up dataspace and dcpl for creating a chunked dataset with
       2 unlimited dimensions in the 'for' loop */
    sid = H5Screate_simple(2, dims2, max_dims2);
    CHECK(sid, H5I_INVALID_HID, "H5Screate_simple");
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, H5I_INVALID_HID, "H5Pcreate");
    ret = H5Pset_chunk(dcpl, 2, chunks2);
    CHECK(ret, FAIL, "H5Pset_chunk");

    /* Loop through all the combinations of low/high bounds in new_fapl */
    /* Open the file with the fapl and create the chunked dataset */
    /* Verify the dataset's layout, fill value and filter pipleline message versions */
    for(low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for(high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {
            H5E_BEGIN_TRY {
                ret = H5Pset_libver_bounds(new_fapl, low, high);
            } H5E_END_TRY;

            if (ret < 0) /* Invalid low/high combinations */
                continue;

            /* Open the file */
            H5E_BEGIN_TRY {
                fid = H5Fopen(FILE8, H5F_ACC_RDWR, new_fapl);
            } H5E_END_TRY;

            if(fid >=0 ) { /* The file open succeeds */

                /* Get the internal file pointer */
                f = (H5F_t *)H5I_object(fid);
                CHECK(f, NULL, "H5I_object");

                /* Create the chunked dataset */
                did = H5Dcreate2(fid, DSETC, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                CHECK(did, H5I_INVALID_HID, "H5Dcreate2");

                /* Get the internal file pointer */
                dset = (H5D_t *)H5I_object(did);
                CHECK(dset, NULL, "H5I_object");

                /* Verify the dataset's layout, fill value and filter pipeline message versions */
                /* Also verify the chunk indexing type */
                if(f->shared->low_bound == H5F_LIBVER_EARLIEST) {
                    /* For layout message: the earliest version the library will set is 3 */
                    /* For fill value message: the earliest version the library will set is 2 */
                    VERIFY(dset->shared->layout.version, H5O_LAYOUT_VERSION_DEFAULT, "H5O_layout_ver_bounds");
                    VERIFY(dset->shared->dcpl_cache.fill.version, H5O_FILL_VERSION_2, "H5O_fill_ver_bounds");
                } else {
                    VERIFY(dset->shared->layout.version, H5O_layout_ver_bounds[f->shared->low_bound], "H5O_layout_ver_bounds");
                    VERIFY(dset->shared->dcpl_cache.fill.version, H5O_fill_ver_bounds[f->shared->low_bound], "H5O_fill_ver_bounds");
                }

                /* Verify the filter pipeline message version */
                VERIFY(dset->shared->dcpl_cache.pline.version, H5O_pline_ver_bounds[f->shared->low_bound], "H5O_pline_ver_bounds");

                /* Verify the dataset's chunk indexing type */
                if(dset->shared->layout.version == H5O_LAYOUT_VERSION_LATEST)
                    VERIFY(dset->shared->layout.u.chunk.idx_type, H5D_CHUNK_IDX_BT2, "chunk_index_type");
                else
                    VERIFY(dset->shared->layout.u.chunk.idx_type, H5D_CHUNK_IDX_BTREE, "chunk_index_type");

                /* Close the dataset */
                ret = H5Dclose(did);
                CHECK(ret, FAIL, "H5Dclose");

                /* Delete the dataset */
                ret = H5Ldelete(fid, DSETC, H5P_DEFAULT);
                CHECK(ret, FAIL, "H5Ldelete");

                /* Close the file */
                ret = H5Fclose(fid);
                CHECK(ret, FAIL, "H5Fclose");

            } /* end if */
        } /* end for */
    } /* end for */

    /* Close the file access property list */
    ret = H5Pclose(new_fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close the dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close the dataset creation property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

} /* end test_libver_bounds_dataset() */

/****************************************************************
**
**  test_libver_bounds_dataspace():
**      Verify dataspace message versions:
**
**      (a) Create a file with default fcpl and the input fapl.
**          Create the following two datasets:
**              --A dataset with scalar dataspace
**              --A dataset with null dataspace
**          For both datasets, verify the dataspace message versions.
**          Close the file.
**
**      (b) Create a new fapl that is set to the 5 pairs of low/high
**          bounds in a "for" loop.  For each pair of setting in the
**          new fapl:
**              --Open the same file in (a) with the fapl
**              --Create a chunked dataset, a compact dataset and
**                a contigous dataset
**              --Verify the dataspace message version for these
**                three datasets
**              --Delete the three datasets and the dataspaces
**              --Close the file
**
****************************************************************/
static void
test_libver_bounds_dataspace(hid_t fapl)
{
    hid_t fid = H5I_INVALID_HID;        /* File ID */
    hid_t new_fapl = H5I_INVALID_HID;   /* File access property list */
    hid_t did = H5I_INVALID_HID, did_null = H5I_INVALID_HID;            /* Dataset IDs */
    hid_t did_compact = H5I_INVALID_HID, did_contig = H5I_INVALID_HID;  /* Dataset IDs */
    hid_t sid = H5I_INVALID_HID, sid_null = H5I_INVALID_HID;            /* Dataspace IDs */
    hid_t sid_compact = H5I_INVALID_HID, sid_contig = H5I_INVALID_HID;  /* Dataspace IDs */
    hid_t dcpl = H5I_INVALID_HID;      /* Dataset creation property list */
    hid_t dcpl_compact = H5I_INVALID_HID, dcpl_contig = H5I_INVALID_HID;  /* Dataset creation property lists */
    H5S_t *space = NULL, *space_null = NULL;    /* Internal dataspace pointers */
    H5F_t *f = NULL;                            /* Internal file pointer */
    H5F_libver_t low, high;                     /* Low and high bounds */
    hsize_t dims[1] = {1};                      /* Dimension sizes */
    hsize_t dims2[2] = {5, 4};                  /* Dimension sizes */
    hsize_t max_dims[1] = {H5S_UNLIMITED};      /* Maximum dimension sizes */
    hsize_t chunks[1] = {4};                    /* Chunk dimension sizes */
    herr_t ret;                                 /* Return value */

    /* Retrieve the low/high bounds from the input fapl */
    ret = H5Pget_libver_bounds(fapl, &low, &high);
    CHECK(ret, FAIL, "H5Pget_libver_bounds");

    /* Create the file with the input fapl */
    fid = H5Fcreate(FILE8, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid, H5I_INVALID_HID, "H5Fcreate");

    /* Create scalar dataspace */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, H5I_INVALID_HID, "H5Screate");

    /* Create a dataset with the scalar dataspace */
    did = H5Dcreate2(fid, DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did, H5I_INVALID_HID, "H5Dcreate");

    /* Get the internal dataspace pointer */
    sid = H5Dget_space(did);
    CHECK(sid, H5I_INVALID_HID, "H5Dget_space");
    space = (H5S_t *)H5I_object(sid);
    CHECK(space, NULL, "H5I_object");

    /* Verify the dataspace version */
    VERIFY(space->extent.version, H5O_sdspace_ver_bounds[low], "H5O_sdspace_ver_bounds");

    /* Create null dataspace */
    sid_null = H5Screate(H5S_NULL);
    CHECK(sid_null, H5I_INVALID_HID, "H5Screate");

    /* Create a dataset with the null dataspace */
    did_null = H5Dcreate2(fid, DSET_NULL, H5T_NATIVE_INT, sid_null, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did_null, H5I_INVALID_HID, "H5Dcreate");

    /* Get the internal dataspace pointer */
    sid_null = H5Dget_space(did_null);
    CHECK(sid_null, H5I_INVALID_HID, "H5Dget_space");
    space_null = (H5S_t *)H5I_object(sid_null);
    CHECK(space_null, NULL, "H5I_object");

    /* Verify the dataspace version */
    VERIFY(space_null->extent.version, H5O_SDSPACE_VERSION_2, "H5O_sdspace_ver_bounds");

    /* Close the datasets */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(did_null);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close the dataspaces */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(sid_null);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Create a default file access property list which is used
       to open the file in the 'for' loop */
    new_fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(new_fapl, H5I_INVALID_HID, "H5Pcreate");

    /* Set up dataspace and dcpl for creating a chunked dataset */
    sid = H5Screate_simple(1, dims, max_dims);
    CHECK(sid, H5I_INVALID_HID, "H5Screate_simple");
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, H5I_INVALID_HID, "H5Pcreate");
    ret = H5Pset_chunk(dcpl, 1, chunks);
    CHECK(ret, FAIL, "H5Pset_chunk");

    /* Set up dataspace and dcpl for creating a compact dataset */
    sid_compact = H5Screate_simple(1, dims, NULL);
    CHECK(sid_compact, H5I_INVALID_HID, "H5Screate_simple");
    dcpl_compact = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl_compact, H5I_INVALID_HID, "H5Pcreate");
    ret = H5Pset_layout(dcpl_compact, H5D_COMPACT);
    CHECK(ret, FAIL, "H5Pset_layout");

    /* Set up dataspace and dcpl for creating a contiguous dataset */
    sid_contig = H5Screate_simple(2, dims2, NULL);
    CHECK(sid_contig, H5I_INVALID_HID, "H5Screate_simple");
    dcpl_contig = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl_contig, H5I_INVALID_HID, "H5Pcreate");
    ret = H5Pset_layout(dcpl_contig, H5D_CONTIGUOUS);
    CHECK(ret, FAIL, "H5Pset_layout");

    /* Loop through all the combinations of low/high bounds in new_fapl */
    /* Open the file and create the chunked/compact/contiguous datasets */
    /* Verify the dataspace message version for the three datasets */
    for(low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for(high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {
            hid_t tmp_sid, tmp_sid_compact, tmp_sid_contig;             /* Dataspace IDs */
            H5S_t *tmp_space, *tmp_space_compact, *tmp_space_contig;    /* Internal dataspace pointers */

            H5E_BEGIN_TRY {
                ret = H5Pset_libver_bounds(new_fapl, low, high);
            } H5E_END_TRY;

            if (ret < 0) /* Invalid low/high combinations */
                continue;

            /* Open the file */
            H5E_BEGIN_TRY {
                fid = H5Fopen(FILE8, H5F_ACC_RDWR, new_fapl);
            } H5E_END_TRY;

            if(fid >=0 ) { /* The file open succeeds */

                /* Get the internal file pointer */
                f = (H5F_t *)H5I_object(fid);
                CHECK(f, NULL, "H5I_object");

                /* Create the chunked dataset */
                did = H5Dcreate2(fid, DSETA, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                CHECK(did, H5I_INVALID_HID, "H5Dcreate2");

                /* Get the internal dataspace pointer for the chunked dataset */
                tmp_sid = H5Dget_space(did);
                CHECK(tmp_sid, H5I_INVALID_HID, "H5Dget_space");
                tmp_space = (H5S_t *)H5I_object(tmp_sid);
                CHECK(tmp_space, NULL, "H5I_object");

                /* Create the compact dataset */
                did_compact = H5Dcreate2(fid, DSETB, H5T_NATIVE_INT, sid_compact, H5P_DEFAULT, dcpl_compact, H5P_DEFAULT);
                CHECK(did_compact, H5I_INVALID_HID, "H5Dcreate2");

                /* Get the internal dataspace pointer for the compact dataset */
                tmp_sid_compact = H5Dget_space(did_compact);
                CHECK(tmp_sid_compact, H5I_INVALID_HID, "H5Dget_space");
                tmp_space_compact = (H5S_t *)H5I_object(tmp_sid_compact);
                CHECK(tmp_space_compact, NULL, "H5I_object");

                /* Create the contiguous dataset */
                did_contig = H5Dcreate2(fid, DSETC, H5T_NATIVE_INT, sid_contig, H5P_DEFAULT, dcpl_contig, H5P_DEFAULT);
                CHECK(did_contig, H5I_INVALID_HID, "H5Dcreate2");

                /* Get the internal dataspace pointer for the contiguous dataset */
                tmp_sid_contig = H5Dget_space(did_contig);
                CHECK(tmp_sid_contig, H5I_INVALID_HID, "H5Dget_space");
                tmp_space_contig = (H5S_t *)H5I_object(tmp_sid_contig);
                CHECK(tmp_space_contig, NULL, "H5I_object");

                /* Verify versions for the three dataspaces */
                VERIFY(tmp_space->extent.version, H5O_sdspace_ver_bounds[f->shared->low_bound], "H5O_sdspace_ver_bounds");
                VERIFY(tmp_space_compact->extent.version, H5O_sdspace_ver_bounds[f->shared->low_bound], "H5O_sdspace_ver_bounds");
                VERIFY(tmp_space_contig->extent.version, H5O_sdspace_ver_bounds[f->shared->low_bound], "H5O_sdspace_ver_bounds");

                /* Close the three datasets */
                ret = H5Dclose(did);
                CHECK(ret, FAIL, "H5Dclose");
                ret = H5Dclose(did_compact);
                CHECK(ret, FAIL, "H5Dclose");
                ret = H5Dclose(did_contig);
                CHECK(ret, FAIL, "H5Dclose");

                /* Close the three dataspaces */
                ret = H5Sclose(tmp_sid);
                CHECK(ret, FAIL, "H5Sclose");
                ret = H5Sclose(tmp_sid_compact);
                CHECK(ret, FAIL, "H5Sclose");
                ret = H5Sclose(tmp_sid_contig);
                CHECK(ret, FAIL, "H5Sclose");

                /* Delete the three datasets */
                ret = H5Ldelete(fid, DSETA, H5P_DEFAULT);
                CHECK(ret, FAIL, "H5Ldelete");
                ret = H5Ldelete(fid, DSETB, H5P_DEFAULT);
                CHECK(ret, FAIL, "H5Ldelete");
                ret = H5Ldelete(fid, DSETC, H5P_DEFAULT);
                CHECK(ret, FAIL, "H5Ldelete");

                /* Close the file */
                ret = H5Fclose(fid);
                CHECK(ret, FAIL, "H5Fclose");

            } /* end if */
        } /* end for */
    } /* end for */

    /* Close the file access property list */
    ret = H5Pclose(new_fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close the three dataspaces */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(sid_compact);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(sid_contig);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close the three dataset creation property lists */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(dcpl_compact);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(dcpl_contig);
    CHECK(ret, FAIL, "H5Pclose");

} /* end test_libver_bounds_dataspace() */


/****************************************************************
**
**  test_libver_bounds_datatype():
**      Verify the datatype message version:
**
**      (a) Create the following datatypes:
**          1) integer
**          2) enum
**          3) array
**          4) compound
**          5) vlen
**      (b) Call test_libver_bounds_datatype_check() for each
**          datatype in (a) to verify the datatype message version.
**
****************************************************************/
static void
test_libver_bounds_datatype(hid_t fapl)
{
    hid_t tid = H5I_INVALID_HID, tid_enum = H5I_INVALID_HID, tid_array = H5I_INVALID_HID;  /* Datatype IDs */
    hid_t tid_compound = H5I_INVALID_HID, tid_vlen = H5I_INVALID_HID;                       /* Datatype IDs */
    int enum_value;                                 /* Value for enum datatype */
    typedef struct s1 { /* Data structure for compound datatype */
        char c;
        int  i;
    } s1;
    hsize_t dims[1] = {1};  /* Dimension sizes */
    herr_t ret;             /* Return value */

    /* Create integer datatype */
    tid = H5Tcopy(H5T_NATIVE_INT);

    /* Verify datatype message version */
    test_libver_bounds_datatype_check(fapl, tid);

    /* Create enum datatype */
    tid_enum = H5Tenum_create(tid);
    enum_value = 0;
    H5Tenum_insert(tid_enum, "val1", &enum_value);
    enum_value = 1;
    H5Tenum_insert(tid_enum, "val2", &enum_value);

    /* Verify datatype message version */
    test_libver_bounds_datatype_check(fapl, tid_enum);

    /* Create array datatype */
    tid_array = H5Tarray_create2(tid, 1, dims);

    /* Verify datatype message version */
    test_libver_bounds_datatype_check(fapl, tid_array);

    /* Create compound datatype */
    tid_compound = H5Tcreate(H5T_COMPOUND, sizeof(s1));
    H5Tinsert(tid_compound, "c", HOFFSET(s1, c), H5T_STD_U8LE);
    H5Tinsert(tid_compound, "i", HOFFSET(s1, i), H5T_NATIVE_INT);

    /* Verify datatype message version */
    test_libver_bounds_datatype_check(fapl, tid_compound);

    /* Create vlen datatype */
    tid_vlen = H5Tvlen_create(tid);

    /* Verify datatype message version */
    test_libver_bounds_datatype_check(fapl, tid_vlen);

    /* Close the datatypes */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Tclose(tid_enum);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Tclose(tid_array);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Tclose(tid_compound);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Tclose(tid_vlen);
    CHECK(ret, FAIL, "H5Tclose");

} /* end test_libver_bounds_datatype() */

/****************************************************************
**
**  test_libver_bounds_datatype_check():
**      Helper routine called by test_libver_bounds_datatype()
**      to verify the datatype message version for the input tid:
**
**      (a) Create a file with default fcpl and the input fapl.
**          Create a contiguous dataset with the input tid.
**          Verify the datatype message version.
**          Create a committed datatype of string to be
**          used later.
**          Close the file.
**
**      (b) Create a new fapl that is set to the 5 pairs of low/high
**          bounds in a "for" loop.  For each pair of setting in
**          the new fapl:
**              --Open the same file in (a) with the fapl
**              --Verify the message version for the committed
**                datatype created earlier
**              --Create a chunked dataset with the input tid
**              --Verify the datatype message version
**              --Close and delete the dataset
**              --Close the file
**
****************************************************************/
static void
test_libver_bounds_datatype_check(hid_t fapl, hid_t tid)
{
    hid_t fid = H5I_INVALID_HID;        /* File ID */
    hid_t new_fapl = H5I_INVALID_HID;   /* File acess property list */
    hid_t dcpl = H5I_INVALID_HID;       /* Dataset creation property list */
    hid_t dtid = H5I_INVALID_HID;       /* Datatype ID for the dataset */
    hid_t str_tid = H5I_INVALID_HID;    /* String datatype ID */
    hid_t did = H5I_INVALID_HID;        /* Dataset ID */
    hid_t sid = H5I_INVALID_HID;        /* Dataspace ID */
    hsize_t dims[1] = {1};      /* Dimension sizes */
    hsize_t dims2[2] = {5, 4};  /* Dimension sizes */
    hsize_t max_dims2[2] = {H5S_UNLIMITED, H5S_UNLIMITED};  /* Maximum dimension sizes */
    hsize_t chunks[2] = {2, 3}; /* Chunk dimension sizes */
    H5T_t *dtype = NULL;        /* Internal datatype pointer */
    H5T_t *str_dtype = NULL;    /* Internal datatype pointer for the string datatype */
    H5F_t *f = NULL;            /* Internal file pointer */
    H5F_libver_t low, high;     /* Low and high bounds */
    herr_t ret;                 /* Return value */

    /* Retrieve the low/high version bounds from the input fapl */
    ret = H5Pget_libver_bounds(fapl, &low, &high);
    CHECK(ret, FAIL, "H5Pget_libver_bounds");

    /* Create the file with the input fapl */
    fid = H5Fcreate(FILE8, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid, H5I_INVALID_HID, "H5Fcreate");

    /* Create a committed datatype of string which will be used
       later inside the 'for' loop */
    str_tid = H5Tcopy(H5T_C_S1);
    CHECK(str_tid, H5I_INVALID_HID, "H5Tcopy");
    ret = H5Tset_size(str_tid, (size_t)10);
    CHECK(ret, FAIL, "H5Tset_size");
    ret = H5Tcommit2(fid, "datatype", str_tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");
    ret = H5Tclose(str_tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Create dataspace */
    sid = H5Screate_simple(1, dims, NULL);
    CHECK(sid, H5I_INVALID_HID, "H5Screate_simple");

    /* Create a dataset with the input tid */
    did = H5Dcreate2(fid, DSET1, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did, H5I_INVALID_HID, "H5Dcreate2");

    /* Get the dataset's datatype */
    dtid = H5Dget_type(did);
    CHECK(dtid, H5I_INVALID_HID, "H5Dget_type");

    /* Get the internal datatype pointer */
    dtype = (H5T_t *)H5I_object(dtid);
    CHECK(dtype, NULL, "H5I_object");

    /* Verify the datatype message version */
    /* H5T_COMPOUND, H5T_ENUM, H5T_ARRAY:
     *  --the library will set version according to low_bound
     *  --H5T_ARRAY: the earliest version the library will set is 2
     * H5T_INTEGER, H5T_FLOAT, H5T_TIME, H5T_STRING, H5T_BITFIELD, H5T_OPAQUE, H5T_REFERENCE:
     *  --the library will only use basic version
     */

    if(dtype->shared->type == H5T_COMPOUND ||
       dtype->shared->type == H5T_ENUM ||
       dtype->shared->type == H5T_ARRAY) {
        if(dtype->shared->type == H5T_ARRAY && low == H5F_LIBVER_EARLIEST)
            VERIFY(dtype->shared->version, H5O_DTYPE_VERSION_2, "H5O_dtype_ver_bounds");
        else
            VERIFY(dtype->shared->version, H5O_dtype_ver_bounds[low], "H5O_dtype_ver_bounds");
    } else
        VERIFY(dtype->shared->version, H5O_dtype_ver_bounds[H5F_LIBVER_EARLIEST], "H5O_dtype_ver_bounds");

    /* Close the dataset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close the dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close the datatype */
    ret = H5Tclose(dtid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Create a default file access property list */
    new_fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(new_fapl, H5I_INVALID_HID, "H5Pcreate");

    /* Set up dataspace and dcpl for creating a chunked dataset */
    sid = H5Screate_simple(2, dims2, max_dims2);
    CHECK(sid, H5I_INVALID_HID, "H5Screate_simple");
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, H5I_INVALID_HID, "H5Pcreate");
    ret = H5Pset_chunk(dcpl, 2, chunks);
    CHECK(ret, FAIL, "H5Pset_chunk");

    /* Loop through all the combinations of low/high bounds */
    /* Open the file and create the chunked dataset with the input tid */
    /* Verify the dataset's datatype message version */
    /* Also verify the committed atatype message version */
    for(low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for(high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {
            H5E_BEGIN_TRY {
                ret = H5Pset_libver_bounds(new_fapl, low, high);
            } H5E_END_TRY;

            if (ret < 0) /* Invalid low/high combinations */
                continue;

            /* Open the file */
            H5E_BEGIN_TRY {
                fid = H5Fopen(FILE8, H5F_ACC_RDWR, new_fapl);
            } H5E_END_TRY;

            if(fid >= 0 ) {  /* The file open succeeds */

                /* Get the internal file pointer */
                f = (H5F_t *)H5I_object(fid);
                CHECK(f, NULL, "H5I_object");

                /* Open the committed datatype */
                str_tid = H5Topen2(fid, "datatype", H5P_DEFAULT);
                CHECK(str_tid, FAIL, "H5Topen2");
                str_dtype = (H5T_t *)H5I_object(str_tid);
                CHECK(str_dtype, NULL, "H5I_object");

                /* Verify the committed datatype message version */
                VERIFY(str_dtype->shared->version, H5O_dtype_ver_bounds[H5F_LIBVER_EARLIEST], "H5O_dtype_ver_bounds");

                /* Close the committed datatype */
                ret = H5Tclose(str_tid);
                CHECK(ret, FAIL, "H5Tclose");


                /* Create the chunked dataset */
                did = H5Dcreate2(fid, DSETNAME, tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                CHECK(did, H5I_INVALID_HID, "H5Dcreate2");

                /* Get the dataset's datatype */
                dtid = H5Dget_type(did);
                CHECK(dtid, H5I_INVALID_HID, "H5Dget_type");

                /* Get the internal datatype pointer */
                dtype = (H5T_t *)H5I_object(dtid);
                CHECK(dtype, NULL, "H5I_object");

                /* Verify the dataset's datatype message version */
                /* H5T_COMPOUND, H5T_ENUM, H5T_ARRAY:
                 *  --the library will set version according to low_bound
                 *  --H5T_ARRAY: the earliest version the library will set is 2
                 * H5T_INTEGER, H5T_FLOAT, H5T_TIME, H5T_STRING, H5T_BITFIELD, H5T_OPAQUE, H5T_REFERENCE:
                 *  --the library will only use basic version
                 */
                if(dtype->shared->type == H5T_COMPOUND ||
                   dtype->shared->type == H5T_ENUM ||
                   dtype->shared->type == H5T_ARRAY) {
                    if(dtype->shared->type == H5T_ARRAY &&
                       f->shared->low_bound == H5F_LIBVER_EARLIEST)
                        VERIFY(dtype->shared->version, H5O_DTYPE_VERSION_2, "H5O_dtype_ver_bounds");
                    else
                        VERIFY(dtype->shared->version, H5O_dtype_ver_bounds[f->shared->low_bound], "H5O_dtype_ver_bounds");
                } else
                    VERIFY(dtype->shared->version, H5O_dtype_ver_bounds[H5F_LIBVER_EARLIEST], "H5O_dtype_ver_bounds");

                /* Close the dataset */
                ret = H5Dclose(did);
                CHECK(ret, FAIL, "H5Dclose");

                /* Close the dataset's datatype */
                ret = H5Tclose(dtid);
                CHECK(ret, FAIL, "H5Tclose");

                /* Delete the dataset */
                ret = H5Ldelete(fid, DSETNAME, H5P_DEFAULT);
                CHECK(ret, FAIL, "H5Ldelete");

                /* Close the file */
                ret = H5Fclose(fid);
                CHECK(ret, FAIL, "H5Fclose");

            } /* end if */
        } /* end for */
    } /* end for */

    /* Close the file access property list */
    ret = H5Pclose(new_fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close the dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close the dataset creation property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

} /* end test_libver_bounds_datatype_check() */

/****************************************************************
**
**  test_libver_bounds_attributes():
**      Verify the attribute message versions:
**
**      (a) Create a file with default fcpl and the input fapl.
**          Create a group and attach the following three attributes
**          to the group:
**          (1) Attribute with a committed datatype
**          (2) Attribute with integer type
**          (3) Attribute with character encoding set
**          Verify the three attributes' message versions.
**          Close the file.
**
**      (b) Create a fcpl that has shared datatype message enabled.
**          Create a file with the fcpl and the input fapl.
**          Create a group and attach an attribute with shared
**          integer type to the group.
**          Verify the attribute message version.
**          Close the file
**
**      (b) Create a new fapl that is set to the 5 pairs of low/high
**          bounds in a "for" loop.  For each pair of setting in
**          the new fapl:
**              --Open the same file in (b) with the fapl
**              --Open the group and attach an attribute with integer
**                type to the group
**              --Verify the attribute message version
**              --Delete the attribute
**              --Close the group and the file
**
****************************************************************/
static void
test_libver_bounds_attributes(hid_t fapl)
{
    hid_t fid = H5I_INVALID_HID;         /* File ID */
    hid_t fcpl = H5I_INVALID_HID;        /* File creation property list */
    hid_t new_fapl = H5I_INVALID_HID;    /* File access property list */
    hid_t tid = H5I_INVALID_HID;         /* Datatype ID */
    hid_t gid = H5I_INVALID_HID;         /* Group ID */
    hid_t sid = H5I_INVALID_HID;         /* Dataspace ID */
    hid_t aid = H5I_INVALID_HID;         /* Attribute ID */
    hid_t attr_cpl = H5I_INVALID_HID;    /* Attribute creation property list */
    H5A_t *attr = NULL;     /* Internal attribute pointer */
    H5F_t *f = NULL;        /* Internal file pointer */
    H5F_libver_t low, high; /* Low and high bounds */
    herr_t ret;             /* Return value */

    /* Retrieve the low/high bounds from the input fapl */
    ret = H5Pget_libver_bounds(fapl, &low, &high);
    CHECK(ret, FAIL, "H5Pget_libver_bounds");

    /* Create the file */
    fid = H5Fcreate(FILE8, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid, H5I_INVALID_HID, "H5Fcreate");

    /* Integer datatpye */
    tid = H5Tcopy(H5T_NATIVE_INT);
    CHECK(tid, H5I_INVALID_HID, "H5Tcopy");

    /* Create a committed datatype */
    ret = H5Tcommit2(fid, "datatype", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Create dataspace */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, H5I_INVALID_HID, "H5Screate");

    /* Create a group */
    gid = H5Gcreate2(fid, GRP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, H5I_INVALID_HID, "H5Gcreate2");

    /* Attach an attribute to the group with the committed datatype */
    aid = H5Acreate2(gid, "attr1", tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, H5I_INVALID_HID, "H5Acreate2");

    /* Get the internal attribute pointer */
    attr = (H5A_t *)H5I_object(aid);
    CHECK(attr, NULL, "H5I_object");

    /* Verify the attribute version */
    if(low == H5F_LIBVER_EARLIEST)
        /* The earliest version the library can set for an attribute with committed datatype is 2 */
        VERIFY(attr->shared->version, H5O_ATTR_VERSION_2, "H5O_attr_ver_bounds");
    else
        VERIFY(attr->shared->version, H5O_attr_ver_bounds[low], "H5O_attr_ver_bounds");

    /* Close the attribute */
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    /* Create an attribute to the group with integer type */
    aid = H5Acreate2(gid, "attr2", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, FAIL, "H5Acreate2");

    /* Get the internal attribute pointer */
    attr = (H5A_t *)H5I_object(aid);
    CHECK(attr, NULL, "H5I_object");

    /* Verify attribute version */
    VERIFY(attr->shared->version, H5O_attr_ver_bounds[low], "H5O_attr_ver_bounds");

    /* Close the attribute */
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    /* Enable character encoding in attribute creation property list */
    attr_cpl = H5Pcreate(H5P_ATTRIBUTE_CREATE);
    CHECK(attr_cpl, H5I_INVALID_HID, "H5Pcreate");
    ret = H5Pset_char_encoding(attr_cpl, H5T_CSET_UTF8);
    CHECK(ret, FAIL, "H5Pset_char_encoding");

    /* Attach an attribute to the group with character encoding set */
    aid = H5Acreate2(gid, "attr3", H5T_NATIVE_INT, sid, attr_cpl, H5P_DEFAULT);
    CHECK(aid, H5I_INVALID_HID, "H5Acreate2");

    /* Get internal attribute pointer */
    attr = (H5A_t *)H5I_object(aid);
    CHECK(attr, NULL, "H5I_object");

    /* Verify attribute version */
    if(low == H5F_LIBVER_EARLIEST)
        /* The earliest version the library can set for an attribute with character encoding is 3 */
        VERIFY(attr->shared->version, H5O_ATTR_VERSION_3, "H5O_attr_ver_bounds");
    else
        VERIFY(attr->shared->version, H5O_attr_ver_bounds[low], "H5O_attr_ver_bounds");

    /* Close the attribute */
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close the attribute creation property list */
    ret = H5Pclose(attr_cpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close the group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close the dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close the datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Create a copy of the file creation property list */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, H5I_INVALID_HID, "H5Pcreate");

    /* Enable shared datatype message */
    ret = H5Pset_shared_mesg_nindexes(fcpl, 1);
    CHECK(ret, FAIL, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl, 0, H5O_SHMESG_DTYPE_FLAG, 2);
    CHECK(ret, FAIL, "H5Pset_shared_mesg_index");

    /* Create the file with shared datatype message enabled */
    fid = H5Fcreate(FILE8, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, H5I_INVALID_HID, "H5Fcreate");

    /* Create an integer datatye */
    tid = H5Tcopy(H5T_NATIVE_INT);
    CHECK(tid, H5I_INVALID_HID, "H5Tcopy");

    /* Create dataspace */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, H5I_INVALID_HID, "H5Screate");

    /* Create a group */
    gid = H5Gcreate2(fid, GRP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, H5I_INVALID_HID, "H5Gcreate2");

    /* Attach an attribute to the group with shared integer datatype */
    aid = H5Acreate2(gid, ATTR_NAME, tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, H5I_INVALID_HID, "H5Acreate2");

    /* Get the internal attribute pointer */
    attr = (H5A_t *)H5I_object(aid);
    CHECK(attr, NULL, "H5I_object");

    /* Verify the attribute version */
    if(low == H5F_LIBVER_EARLIEST)
        /* The earliest version the library can set for an attribute with shared datatype is 2 */
        VERIFY(attr->shared->version, H5O_ATTR_VERSION_2, "H5O_attr_ver_bounds");
    else
        VERIFY(attr->shared->version, H5O_attr_ver_bounds[low], "H5O_attr_ver_bounds");

    /* Close the attribute */
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close the group */
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close the dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close the datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Create a default file access property list */
    new_fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(new_fapl, FAIL, "H5Pcreate");

    /* Create a scalar dataspace to be used later for the attribute */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, H5I_INVALID_HID, "H5Screate");

    /* Loop through all the combinations of low/high bounds */
    /* Open the file and group and attach an attribute to the group */
    /* Verify the attribute version */
    for(low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for(high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {
            H5E_BEGIN_TRY {
                ret = H5Pset_libver_bounds(new_fapl, low, high);
            } H5E_END_TRY;

            if(ret < 0) /* Invalid low/high combinations */
                continue;

            /* Open the file */
            H5E_BEGIN_TRY {
                fid = H5Fopen(FILE8, H5F_ACC_RDWR, new_fapl);
            } H5E_END_TRY;

            if(fid >=0 ) { /* The file open succeeds */

                /* Get the internal file pointer */
                f = (H5F_t *)H5I_object(fid);
                CHECK(f, NULL, "H5I_object");

                /* Open the group */
                gid = H5Gopen2(fid, GRP_NAME, H5P_DEFAULT);
                CHECK(gid, FAIL, "H5Gopen2");

                /* Attach an attribute to the group */
                aid = H5Acreate2(gid, "attr1", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT);
                CHECK(aid, FAIL, "H5Acreate2");

                /* Get the internal attribute pointer */
                attr = (H5A_t *)H5I_object(aid);
                CHECK(attr, NULL, "H5I_object");

                /* Verify the attribute message version */
                VERIFY(attr->shared->version, H5O_attr_ver_bounds[f->shared->low_bound], "H5O_attr_ver_bounds");

                /* Close the attribute */
                ret = H5Aclose(aid);
                CHECK(ret, FAIL, "H5Aclose");

                /* Delete the attribute */
                ret = H5Adelete(gid, "attr1");
                CHECK(ret, FAIL, "H5Adelete");

                /* Close the group */
                ret = H5Gclose(gid);
                CHECK(ret, FAIL, "H5Gclose");

                /* Close the file */
                ret = H5Fclose(fid);
                CHECK(ret, FAIL, "H5Fclose");

            } /* end if */
        } /* end for */
    } /* end for */

    /* Close the file access property list */
    ret = H5Pclose(new_fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close the dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

} /* end test_libver_bounds_attributes() */

/****************************************************************
**
**  test_libver_macros():
**    Verify that H5_VERSION_GE and H5_VERSION_LE work correactly.
**
****************************************************************/
static void
test_libver_macros(void)
{
    int     major = H5_VERS_MAJOR;
    int     minor = H5_VERS_MINOR;
    int     release = H5_VERS_RELEASE;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing macros for library version comparison\n"));

    VERIFY(H5_VERSION_GE(major,minor,release), TRUE, "H5_VERSION_GE");
    VERIFY(H5_VERSION_GE(major-1,minor,release), TRUE, "H5_VERSION_GE");
    VERIFY(H5_VERSION_GE(major-1,minor+1,release), TRUE, "H5_VERSION_GE");
    VERIFY(H5_VERSION_GE(major-1,minor,release+1), TRUE, "H5_VERSION_GE");
    VERIFY(H5_VERSION_GE(major,minor-1,release), TRUE, "H5_VERSION_GE");
    VERIFY(H5_VERSION_GE(major,minor-1,release+1), TRUE, "H5_VERSION_GE");
    if(H5_VERS_RELEASE > 0)
        VERIFY(H5_VERSION_GE(major,minor,release-1), TRUE, "H5_VERSION_GE");

    VERIFY(H5_VERSION_GE(major+1,minor,release), FALSE, "H5_VERSION_GE");
    VERIFY(H5_VERSION_GE(major+1,minor-1,release), FALSE, "H5_VERSION_GE");
    VERIFY(H5_VERSION_GE(major+1,minor-1,release-1), FALSE, "H5_VERSION_GE");
    VERIFY(H5_VERSION_GE(major,minor+1,release), FALSE, "H5_VERSION_GE");
    VERIFY(H5_VERSION_GE(major,minor+1,release-1), FALSE, "H5_VERSION_GE");
    VERIFY(H5_VERSION_GE(major,minor,release+1), FALSE, "H5_VERSION_GE");

    VERIFY(H5_VERSION_LE(major,minor,release), TRUE, "H5_VERSION_LE");
    VERIFY(H5_VERSION_LE(major+1,minor,release), TRUE, "H5_VERSION_LE");
    VERIFY(H5_VERSION_LE(major+1,minor-1,release), TRUE, "H5_VERSION_LE");
    VERIFY(H5_VERSION_LE(major+1,minor-1,release-1), TRUE, "H5_VERSION_LE");
    VERIFY(H5_VERSION_LE(major,minor+1,release), TRUE, "H5_VERSION_LE");
    VERIFY(H5_VERSION_LE(major,minor+1,release-1), TRUE, "H5_VERSION_LE");
    VERIFY(H5_VERSION_LE(major,minor,release+1), TRUE, "H5_VERSION_LE");

    VERIFY(H5_VERSION_LE(major-1,minor,release), FALSE, "H5_VERSION_LE");
    VERIFY(H5_VERSION_LE(major-1,minor+1,release), FALSE, "H5_VERSION_LE");
    VERIFY(H5_VERSION_LE(major-1,minor+1,release+1), FALSE, "H5_VERSION_LE");
    VERIFY(H5_VERSION_LE(major,minor-1,release), FALSE, "H5_VERSION_LE");
    VERIFY(H5_VERSION_LE(major,minor-1,release+1), FALSE, "H5_VERSION_LE");
    if(H5_VERS_RELEASE > 0)
        VERIFY(H5_VERSION_LE(major,minor,release-1), FALSE, "H5_VERSION_LE");
} /* test_libver_macros() */

/****************************************************************
**
**  test_libver_macros2():
**    Verify that H5_VERSION_GE works correactly and show how
**      to use it.
**
****************************************************************/
static void
test_libver_macros2(void)
{
    hid_t    file;
    hid_t    grp;
    htri_t   status;
    herr_t   ret;                    /* Return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing macros for library version comparison with a file\n"));

    /*
     * Create a file.
     */
    file = H5Fcreate(FILE6, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /*
     * Create a group in the file.
     */
    grp = H5Gcreate2(file, "Group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Gcreate");

    /*
     * Close the group
     */
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");

    /*
     * Delete the group using different function based on the library version.
     *  And verify the action.
     */
#if H5_VERSION_GE(1,8,0)
    ret = H5Ldelete(file, "Group", H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lunlink");

    status = H5Lexists(file, "Group", H5P_DEFAULT);
    VERIFY(status, FALSE, "H5Lexists");
#else
    ret = H5Gunlink(file, "Group");
    CHECK(ret, FAIL, "H5Gunlink");

    H5E_BEGIN_TRY {
        grp = H5Gopen(file, "Group");
    } H5E_END_TRY;
    VERIFY(grp, FAIL, "H5Gopen");
#endif

    /*
     * Close the file.
     */
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

} /* test_libver_macros2() */

/****************************************************************
**
**  test_filesize():
**    Verify H5Fincrement_filesize() and H5Fget_eoa() works as
**    indicated in the "RFC: Enhancement to the tool h5clear".
**
****************************************************************/
static void
test_incr_filesize(void)
{
    hid_t    fid;                       /* File opened with read-write permission */
    h5_stat_size_t filesize;            /* Size of file when empty */
    hid_t    fcpl;                      /* File creation property list */
    hid_t    fapl;                      /* File access property list */
    hid_t    dspace;                    /* Dataspace ID */
    hid_t    dset;                      /* Dataset ID */
    hid_t    dcpl;                      /* Dataset creation property list */
    unsigned u;                         /* Local index variable */
    char     filename[FILENAME_LEN];    /* Filename to use */
    char     name[32];                  /* Dataset name */
    haddr_t  stored_eoa;                /* The stored EOA value */
    hid_t    driver_id = -1;            /* ID for this VFD */
    unsigned long driver_flags = 0;     /* VFD feature flags */
    herr_t   ret;                       /* Return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing H5Fincrement_filesize() and H5Fget_eoa())\n"));

    fapl = h5_fileaccess();
    h5_fixname(FILE8, fapl, filename, sizeof filename);

    /* Get the VFD feature flags */
    driver_id = H5Pget_driver(fapl);
    CHECK(driver_id, FAIL, "H5Pget_driver");

    ret = H5FDdriver_query(driver_id, &driver_flags);
    CHECK(ret, FAIL, "H5PDdriver_query");

    /* Check whether the VFD feature flag supports these two public routines */
    if(driver_flags & H5FD_FEAT_SUPPORTS_SWMR_IO) {

        fcpl = H5Pcreate(H5P_FILE_CREATE);
        CHECK(fcpl, FAIL, "H5Pcreate");

        /* Set file space strategy */
        ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_FSM_AGGR, FALSE, (hsize_t)1);
        CHECK(ret, FAIL, "H5P_set_file_space_strategy");

        /* Create the test file */
        fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl);
        CHECK(fid, FAIL, "H5Fcreate");

        /* Create dataspace for datasets */
        dspace = H5Screate(H5S_SCALAR);
        CHECK(dspace, FAIL, "H5Screate");

        /* Create a dataset creation property list */
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(dcpl, FAIL, "H5Pcreate");

        /* Set the space allocation time to early */
        ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY);
        CHECK(ret, FAIL, "H5Pset_alloc_time");

        /* Create datasets in file */
        for(u = 0; u < 10; u++) {
            HDsprintf(name, "Dataset %u", u);
            dset = H5Dcreate2(fid, name, H5T_STD_U32LE, dspace, H5P_DEFAULT, dcpl, H5P_DEFAULT);
            CHECK(dset, FAIL, "H5Dcreate2");

            ret = H5Dclose(dset);
            CHECK(ret, FAIL, "H5Dclose");
        } /* end for */

        /* Close dataspace */
        ret = H5Sclose(dspace);
        CHECK(ret, FAIL, "H5Sclose");

        /* Close dataset creation property list */
        ret = H5Pclose(dcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Get the file size */
        filesize = h5_get_file_size(filename, fapl);

        /* Open the file */
        fid = H5Fopen(filename, H5F_ACC_RDWR, fapl);
        CHECK(fid, FAIL, "H5Fopen");

        /* Get the stored EOA */
        ret = H5Fget_eoa(fid, &stored_eoa);
        CHECK(ret, FAIL, "H5Fget_eoa");

        /* Verify the stored EOA is the same as filesize */
        VERIFY(filesize, stored_eoa, "file size");

        /* Set the EOA to the MAX(EOA, EOF) + 512 */
        ret = H5Fincrement_filesize(fid, 512);
        CHECK(ret, FAIL, "H5Fincrement_filesize");

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Get the file size */
        filesize = h5_get_file_size(filename, fapl);

        /* Verify the filesize is the previous stored_eoa + 512 */
        VERIFY(filesize, stored_eoa+512, "file size");

        /* Close the file access property list */
        ret = H5Pclose(fapl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Close the file creation property list */
        ret = H5Pclose(fcpl);
        CHECK(ret, FAIL, "H5Pclose");
    }
} /* end test_incr_filesize() */

/****************************************************************
**
**  test_min_dset_ohdr():
**    Test API calls to toggle dataset object header minimization.
**
**  TODO (as separate function?):
**    + setting persists between close and (re)open?
**    + dataset header sizes created while changing value of toggle
**
****************************************************************/
static void
test_min_dset_ohdr(void)
{
    const char basename[]       = "min_dset_ohdr_testfile";
    char filename[FILENAME_LEN] = "";
    hid_t      file_id          = -1;
    hid_t      file2_id         = -1;
    hbool_t    minimize;
    herr_t     ret;

    MESSAGE(5, ("Testing dataset object header minimization\n"));

    /*********/
    /* SETUP */
    /*********/

    h5_fixname(basename, H5P_DEFAULT, filename, sizeof(filename));

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK_I(file_id, "H5Fcreate");

    /*********/
    /* TESTS */
    /*********/

    /*----------------------------------------
     * TEST default value
     */
    ret = H5Fget_dset_no_attrs_hint(file_id, &minimize);
    CHECK(ret, FAIL, "H5Fget_dset_no_attrs_hint");
    VERIFY(minimize, FALSE, "minimize flag");

    /*----------------------------------------
     * TEST set to TRUE
     */
    ret = H5Fset_dset_no_attrs_hint(file_id, TRUE);
    CHECK(ret, FAIL, "H5Fset_dset_no_attrs_hint");

    ret = H5Fget_dset_no_attrs_hint(file_id, &minimize);
    CHECK(ret, FAIL, "H5Fget_dset_no_attrs_hint");
    VERIFY(minimize, TRUE, "minimize flag");

    /*----------------------------------------
     * TEST second file open on same filename
     */
    file2_id = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK_I(file2_id, "H5Fopen");

    /* verify TRUE setting on second open
     */
    ret = H5Fget_dset_no_attrs_hint(file_id, &minimize);
    CHECK(ret, FAIL, "H5Fget_dset_no_attrs_hint");
    VERIFY(minimize, TRUE, "minimize flag");

    /* re-set to FALSE on first open
     */
    ret = H5Fset_dset_no_attrs_hint(file_id, FALSE);
    CHECK(ret, FAIL, "H5Fset_dset_no_attrs_hint");

    /* verify FALSE set on both opens
     */
    ret = H5Fget_dset_no_attrs_hint(file_id, &minimize);
    CHECK(ret, FAIL, "H5Fget_dset_no_attrs_hint");
    VERIFY(minimize, FALSE, "minimize flag");

    ret = H5Fget_dset_no_attrs_hint(file2_id, &minimize);
    CHECK(ret, FAIL, "H5Fget_dset_no_attrs_hint");
    VERIFY(minimize, FALSE, "minimize flag");

    /* re-set to TRUE on second open
     */
    ret = H5Fset_dset_no_attrs_hint(file2_id, TRUE);
    CHECK(ret, FAIL, "H5Fset_dset_no_attrs_hint");

    /* verify TRUE set on both opens
     */
    ret = H5Fget_dset_no_attrs_hint(file_id, &minimize);
    CHECK(ret, FAIL, "H5Fget_dset_no_attrs_hint");
    VERIFY(minimize, TRUE, "minimize flag");

    ret = H5Fget_dset_no_attrs_hint(file2_id, &minimize);
    CHECK(ret, FAIL, "H5Fget_dset_no_attrs_hint");
    VERIFY(minimize, TRUE, "minimize flag");

    /*----------------------------------------
     * TEST error cases
     */

    /* trying to set with invalid file ID */
    H5E_BEGIN_TRY {
        ret = H5Fset_dset_no_attrs_hint(-1, TRUE);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Fset_dset_no_attrs_hint");

    /* trying to get with invalid file ID */
    H5E_BEGIN_TRY {
        ret = H5Fget_dset_no_attrs_hint(-1, &minimize);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Fget_dset_no_attrs_hint");

    /* trying to get with invalid pointer */
    H5E_BEGIN_TRY {
        ret = H5Fget_dset_no_attrs_hint(file_id, NULL);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Fget_dset_no_attrs_hint");

    /************/
    /* TEARDOWN */
    /************/

    ret = H5Fclose(file_id);
    CHECK(ret, FAIL, "H5Fclose");
    ret = H5Fclose(file2_id);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_min_dset_ohdr() */

/****************************************************************
**
**  test_deprec():
**    Test deprecated functionality.
**
****************************************************************/
#ifndef H5_NO_DEPRECATED_SYMBOLS
static void
test_deprec(void)
{
    hid_t       file;           /* File IDs for old & new files */
    hid_t       fcpl;           /* File creation property list */
    hid_t       fapl;           /* File creation property list */
    hid_t new_fapl;
    hsize_t align;
    unsigned    super;          /* Superblock version # */
    unsigned    freelist;       /* Free list version # */
    unsigned    stab;           /* Symbol table entry version # */
    unsigned    shhdr;          /* Shared object header version # */
    H5F_info1_t    finfo;        /* global information about file */
    herr_t      ret;            /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing deprecated routines\n"));

    /* Creating a file with the default file creation property list should
     * create a version 0 superblock
     */

    /* Create file with default file creation property list */
    file= H5Fcreate(FILE1, H5F_ACC_TRUNC , H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Get the file's version information */
    ret = H5Fget_info1(file, &finfo);
    CHECK(ret, FAIL, "H5Fget_info1");
    VERIFY(finfo.super_ext_size, 0,"H5Fget_info1");
    VERIFY(finfo.sohm.hdr_size, 0,"H5Fget_info1");
    VERIFY(finfo.sohm.msgs_info.index_size, 0,"H5Fget_info1");
    VERIFY(finfo.sohm.msgs_info.heap_size, 0,"H5Fget_info1");

    /* Get the file's dataset creation property list */
    fcpl =  H5Fget_create_plist(file);
    CHECK(fcpl, FAIL, "H5Fget_create_plist");

    /* Get the file's version information */
    ret=H5Pget_version(fcpl, &super, &freelist, &stab, &shhdr);
    CHECK(ret, FAIL, "H5Pget_version");
    VERIFY(super,0,"H5Pget_version");
    VERIFY(freelist,0,"H5Pget_version");
    VERIFY(stab,0,"H5Pget_version");
    VERIFY(shhdr,0,"H5Pget_version");

    /* Close FCPL */
    ret=H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");


    /* Create a file creation property list */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");

    /* Set a property in the FCPL that will push the superblock version up */
    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 1, (hsize_t)0);
    ret = H5Pset_file_space_page_size(fcpl, (hsize_t)512);
    CHECK(ret, FAIL, "H5Pset_file_space_strategy");

    fapl = H5Pcreate(H5P_FILE_ACCESS);
    ret = H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)1024);
    CHECK(ret, FAIL, "H5Pset_alignment");

    /* Creating a file with the non-default file creation property list should
     * create a version 2 superblock
     */

    /* Create file with custom file creation property list */
    file= H5Fcreate(FILE1, H5F_ACC_TRUNC , fcpl, fapl);
    CHECK(file, FAIL, "H5Fcreate");

    new_fapl = H5Fget_access_plist(file);
    H5Pget_alignment(new_fapl, NULL, &align);

    /* Close FCPL */
    ret=H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Get the file's version information */
    ret = H5Fget_info1(file, &finfo);
    CHECK(ret, FAIL, "H5Fget_info1");
    VERIFY(finfo.super_ext_size, 152,"H5Fget_info1");
    VERIFY(finfo.sohm.hdr_size, 0,"H5Fget_info1");
    VERIFY(finfo.sohm.msgs_info.index_size, 0,"H5Fget_info1");
    VERIFY(finfo.sohm.msgs_info.heap_size, 0,"H5Fget_info1");

    /* Get the file's dataset creation property list */
    fcpl =  H5Fget_create_plist(file);
    CHECK(fcpl, FAIL, "H5Fget_create_plist");

    /* Get the file's version information */
    ret=H5Pget_version(fcpl, &super, &freelist, &stab, &shhdr);
    CHECK(ret, FAIL, "H5Pget_version");
    VERIFY(super,2,"H5Pget_version");
    VERIFY(freelist,0,"H5Pget_version");
    VERIFY(stab,0,"H5Pget_version");
    VERIFY(shhdr,0,"H5Pget_version");

    /* Close FCPL */
    ret=H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    file = H5Fopen(FILE1, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Get the file's version information */
    ret = H5Fget_info1(file, &finfo);
    CHECK(ret, FAIL, "H5Fget_info1");
    VERIFY(finfo.super_ext_size, 152,"H5Fget_info1");
    VERIFY(finfo.sohm.hdr_size, 0,"H5Fget_info1");
    VERIFY(finfo.sohm.msgs_info.index_size, 0,"H5Fget_info1");
    VERIFY(finfo.sohm.msgs_info.heap_size, 0,"H5Fget_info1");

    /* Get the file's creation property list */
    fcpl =  H5Fget_create_plist(file);
    CHECK(fcpl, FAIL, "H5Fget_create_plist");

    /* Get the file's version information */
    ret=H5Pget_version(fcpl, &super, &freelist, &stab, &shhdr);
    CHECK(ret, FAIL, "H5Pget_version");
    VERIFY(super,2,"H5Pget_version");
    VERIFY(freelist,0,"H5Pget_version");
    VERIFY(stab,0,"H5Pget_version");
    VERIFY(shhdr,0,"H5Pget_version");

    /* Close FCPL */
    ret=H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    { /* Test deprecated H5Pget/set_file_space() */

        H5F_file_space_type_t old_strategy;
        hsize_t old_threshold;
        hid_t fid;
        hid_t ffcpl;

        fcpl = H5Pcreate(H5P_FILE_CREATE);
        CHECK(fcpl, FAIL, "H5Pcreate");

        ret = H5Pget_file_space(fcpl, &old_strategy, &old_threshold);
        CHECK(ret, FAIL, "H5Pget_file_space");
        VERIFY(old_strategy, H5F_FILE_SPACE_ALL, "H5Pget_file_space");
        VERIFY(old_threshold, H5F_FREE_SPACE_THRESHOLD_DEF, "H5Pget_file_space");

        /* Set file space strategy and free space section threshold */
        ret = H5Pset_file_space(fcpl, H5F_FILE_SPACE_ALL_PERSIST, (hsize_t)0);
        CHECK(ret, FAIL, "H5Pget_file_space");

        /* Get the file space info from the creation property */
        ret = H5Pget_file_space(fcpl, &old_strategy, &old_threshold);
        CHECK(ret, FAIL, "H5Pget_file_space");
        VERIFY(old_strategy, H5F_FILE_SPACE_ALL_PERSIST, "H5Pget_file_space");
        VERIFY(old_threshold, H5F_FREE_SPACE_THRESHOLD_DEF, "H5Pget_file_space");

        ret = H5Pset_file_space(fcpl, H5F_FILE_SPACE_DEFAULT, (hsize_t)3);
        CHECK(ret, FAIL, "H5Pget_file_space");

        ret = H5Pget_file_space(fcpl, &old_strategy, &old_threshold);
        CHECK(ret, FAIL, "H5Pget_file_space");
        VERIFY(old_strategy, H5F_FILE_SPACE_ALL_PERSIST, "H5Pget_file_space");
        VERIFY(old_threshold, 3, "H5Pget_file_space");

        /* Create a file */
        fid = H5Fcreate(FILE1, H5F_ACC_TRUNC , fcpl, H5P_DEFAULT);
        CHECK(file, FAIL, "H5Fcreate");

        old_strategy = H5F_FILE_SPACE_DEFAULT;
        old_threshold = 0;
        ffcpl = H5Fget_create_plist(fid);
        ret = H5Pget_file_space(ffcpl, &old_strategy, &old_threshold);
        CHECK(ret, FAIL, "H5Pget_file_space");
        VERIFY(old_strategy, H5F_FILE_SPACE_ALL_PERSIST, "H5Pget_file_space");
        VERIFY(old_threshold, 3, "H5Pget_file_space");

        /* Close file */
        ret=H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        ret = H5Pclose(ffcpl);
        CHECK(ret, FAIL, "H5Pclose");

        ret = H5Pclose(fcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Reopen the file */
        fid = H5Fopen(FILE1, H5F_ACC_RDONLY, H5P_DEFAULT);
        CHECK(fid, FAIL, "H5Fcreate");

        old_strategy = H5F_FILE_SPACE_DEFAULT;
        old_threshold = 0;
        ffcpl = H5Fget_create_plist(fid);
        ret = H5Pget_file_space(ffcpl, &old_strategy, &old_threshold);
        CHECK(ret, FAIL, "H5Pget_file_space");
        VERIFY(old_strategy, H5F_FILE_SPACE_ALL_PERSIST, "H5Pget_file_space");
        VERIFY(old_threshold, 3, "H5Pget_file_space");

        ret = H5Pclose(ffcpl);
        CHECK(ret, FAIL, "H5Pclose");

        ret=H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");
    }

} /* test_deprec */
#endif /* H5_NO_DEPRECATED_SYMBOLS */

/****************************************************************
**
**  test_file(): Main low-level file I/O test routine.
**
****************************************************************/
void
test_file(void)
{
    const char  *env_h5_drvr;         /* File Driver value from environment */
    hid_t fapl_id = H5I_INVALID_HID;    /* VFD-dependent fapl ID */
    herr_t   ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Low-Level File I/O\n"));

    /* Get the VFD to use */
    env_h5_drvr = HDgetenv("HDF5_DRIVER");
    if(env_h5_drvr == NULL)
        env_h5_drvr = "nomatch";

    /* Improved version of VFD-dependent checks */
    fapl_id = h5_fileaccess();
    CHECK(fapl_id, H5I_INVALID_HID, "h5_fileaccess");

    test_file_create();                         /* Test file creation(also creation templates)*/
    test_file_open();                           /* Test file opening */
    test_file_reopen();                         /* Test file reopening */
    test_file_close();                          /* Test file close behavior */
    test_get_file_id();                         /* Test H5Iget_file_id */
    test_get_obj_ids();                         /* Test H5Fget_obj_ids for Jira Issue 8528 */
    test_file_perm();                           /* Test file access permissions */
    test_file_perm2();                          /* Test file access permission again */
    test_file_ishdf5();                         /* Test detecting HDF5 files correctly */
    test_file_open_dot();                       /* Test opening objects with "." for a name */
    test_file_open_overlap();                   /* Test opening files in an overlapping manner */
    test_file_getname();                        /* Test basic H5Fget_name() functionality */
    test_file_double_root_open();               /* Test opening root group from two files works properly */
    test_file_double_group_open();              /* Test opening same group from two files works properly */
    test_file_double_dataset_open();            /* Test opening same dataset from two files works properly */
    test_file_double_datatype_open();           /* Test opening same named datatype from two files works properly */
    test_file_double_file_dataset_open(TRUE);
    test_file_double_file_dataset_open(FALSE);
    test_userblock_file_size();                 /* Tests that files created with a userblock have the correct size */
    test_cached_stab_info();                    /* Tests that files are created with cached stab info in the superblock */
    test_rw_noupdate();                         /* Test to ensure that RW permissions don't write the file unless dirtied */
    test_userblock_alignment();                 /* Tests that files created with a userblock and alignment interact properly */
    test_userblock_alignment_paged();           /* Tests files created with a userblock and alignment (via paged aggregation) interact properly */
    test_filespace_info(env_h5_drvr);           /* Test file creation public routines: */
                                                /* H5Pget/set_file_space_strategy() & H5Pget/set_file_space_page_size() */
                                                /* Skipped testing for multi/split drivers */
    test_file_freespace(env_h5_drvr);           /* Test file public routine H5Fget_freespace() */
                                                /* Skipped testing for multi/split drivers */
                                                /* Setup for multi/split drivers are there already */
    test_sects_freespace(env_h5_drvr, TRUE);    /* Test file public routine H5Fget_free_sections() for new format */
                                                /* Skipped testing for multi/split drivers */
                                                /* Setup for multi/split drivers are there already */
    test_sects_freespace(env_h5_drvr, FALSE);   /* Test file public routine H5Fget_free_sections() */
                                                /* Skipped testing for multi/split drivers */
    test_filespace_compatible();                /* Test compatibility for file space management */
    test_filespace_round_compatible();          /* Testing file space compatibility for files from trunk to 1_8 to trunk */
    test_filespace_1_10_0_compatible();          /* Testing file space compatibility for files from release 1.10.0 */
    test_libver_bounds();                       /* Test compatibility for file space management */
    test_libver_bounds_low_high();
    test_libver_macros();                       /* Test the macros for library version comparison */
    test_libver_macros2();                      /* Test the macros for library version comparison */
    test_incr_filesize();                       /* Test H5Fincrement_filesize() and H5Fget_eoa() */
    test_min_dset_ohdr();                       /* Test datset object header minimization */
#ifndef H5_NO_DEPRECATED_SYMBOLS
    test_deprec();                              /* Test deprecated routines */
#endif /* H5_NO_DEPRECATED_SYMBOLS */

    ret = H5Pclose(fapl_id);
    CHECK(ret, FAIL, "H5Pclose");

} /* test_file() */


/*-------------------------------------------------------------------------
 * Function:    cleanup_file
 *
 * Purpose:    Cleanup temporary test files
 *
 * Return:    none
 *
 * Programmer:    Albert Cheng
 *              July 2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_file(void)
{
    HDremove(SFILE1);
    HDremove(FILE1);
    HDremove(FILE2);
    HDremove(FILE3);
    HDremove(FILE4);
    HDremove(FILE5);
    HDremove(FILE6);
    HDremove(FILE7);
    HDremove(DST_FILE);
}

