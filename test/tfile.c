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

/***********************************************************
*
* Test program:	 tfile
*
* Test the low-level file I/O features.
*
*************************************************************/

#include "hdf5.h"
#include "testhdf5.h"
#include "H5srcdir.h"

#include "H5Bprivate.h"
#include "H5Pprivate.h"

/*
 * This file needs to access private information from the H5F package.
 * This file also needs to access the file testing code.
 */
#define H5F_FRIEND		/*suppress error about including H5Fpkg	  */
#define H5F_TESTING
#include "H5Fpkg.h"		/* File access	 			*/

#define BAD_USERBLOCK_SIZE1  (hsize_t)1
#define BAD_USERBLOCK_SIZE2  (hsize_t)2
#define BAD_USERBLOCK_SIZE3  (hsize_t)3
#define BAD_USERBLOCK_SIZE4  (hsize_t)64
#define BAD_USERBLOCK_SIZE5  (hsize_t)511
#define BAD_USERBLOCK_SIZE6  (hsize_t)513
#define BAD_USERBLOCK_SIZE7  (hsize_t)6144

#define F1_USERBLOCK_SIZE  (hsize_t)0
#define F1_OFFSET_SIZE	   sizeof(haddr_t)
#define F1_LENGTH_SIZE	   sizeof(hsize_t)
#define F1_SYM_LEAF_K	   4
#define F1_SYM_INTERN_K	   16
#define FILE1	"tfile1.h5"
#define SFILE1	"sys_file1"

#define REOPEN_FILE "tfile_reopen.h5"
#define REOPEN_DSET "dset"

#define F2_USERBLOCK_SIZE  (hsize_t)512
#define F2_OFFSET_SIZE	   8
#define F2_LENGTH_SIZE	   8
#define F2_SYM_LEAF_K	   8
#define F2_SYM_INTERN_K	   32
#define F2_RANK            2
#define F2_DIM0            4
#define F2_DIM1            6
#define F2_DSET            "dset"
#define FILE2	"tfile2.h5"

#define F3_USERBLOCK_SIZE  (hsize_t)0
#define F3_OFFSET_SIZE	   F2_OFFSET_SIZE
#define F3_LENGTH_SIZE	   F2_LENGTH_SIZE
#define F3_SYM_LEAF_K	   F2_SYM_LEAF_K
#define F3_SYM_INTERN_K	   F2_SYM_INTERN_K
#define FILE3	"tfile3.h5"

#define GRP_NAME         "/group"
#define DSET_NAME         "dataset"
#define ATTR_NAME          "attr"
#define TYPE_NAME          "type"
#define FILE4	           "tfile4.h5"

#define OBJ_ID_COUNT_0     0
#define OBJ_ID_COUNT_1     1
#define OBJ_ID_COUNT_2     2
#define OBJ_ID_COUNT_3     3
#define OBJ_ID_COUNT_4     4
#define OBJ_ID_COUNT_6	   6
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
#define FILENAME_LEN    	1024		/* length of file name */
#define CORE_INCREMENT  	1024		/* core file */
#define FAMILY_SIZE     	1024		/* family file */
#define DSETNAME 		"dset"		/* Name of dataset */
#define NELMTS(X)               (sizeof(X)/sizeof(X[0]))	/* # of elements */
#define READ_OLD_BUFSIZE	1024		/* Buffer for holding file data */
#define FILE5			"tfile5.h5"	/* Test file */
#define TEST_THRESHOLD10        10		/* Free space section threshold */

/* Declaration for test_libver_macros2() */
#define FILE6			"tfile6.h5"	/* Test file */

/* Declaration for test_get_obj_ids() */
#define FILE7			"tfile7.h5"	/* Test file */
#define NGROUPS			2
#define NDSETS			4

const char *OLD_FILENAME[] = {  /* Files created under 1.6 branch and 1.8 branch */
    "filespace_1_6.h5",	/* 1.6 HDF5 file */
    "filespace_1_8.h5"	/* 1.8 HDF5 file */
};
const char *FILESPACE_NAME[] = {
    "tfilespace",
    NULL
};

const char *FILENAME[] = {
    "sec2_tfile",
    "split_tfile",
    "stdio_tfile",
    "core_tfile",
    "family_tfile",
    NULL
};


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
    hid_t		fid1, fid2, fid3; /* HDF5 File IDs		*/
    hid_t		tmpl1, tmpl2;	/*file creation templates	*/
    hsize_t		ublock;		/*sizeof userblock		*/
    size_t		parm;		/*file-creation parameters	*/
    size_t		parm2;		/*file-creation parameters	*/
    unsigned		iparm;
    unsigned		iparm2;
    herr_t		ret;		/*generic return value		*/

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
}				/* test_file_create() */

/****************************************************************
**
**  test_file_open(): Low-level file open I/O test routine.
**
****************************************************************/
static void
test_file_open(void)
{
    hid_t		fid1, fid2;     /*HDF5 File IDs			*/
    hid_t               did;            /*dataset ID                    */
    hid_t               fapl_id;        /*file access property list ID  */
    hid_t		tmpl1;		/*file creation templates	*/
    hsize_t		ublock;		/*sizeof user block		*/
    size_t		parm;		/*file-creation parameters	*/
    size_t		parm2;		/*file-creation parameters	*/
    unsigned		iparm;
    unsigned		iparm2;
    unsigned		intent;
    herr_t		ret;		/*generic return value		*/

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
    hid_t		dataset_id, group_id1, group_id2, group_id3;
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
    ssize_t	oid_count;
    herr_t	ret;

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
        sprintf(gname, "group%d", m);
        gid[m] = H5Gcreate2(fid, gname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(gid[m], FAIL, "H5Gcreate2");
    }

    /* create NDSETS datasets under the root group */
    for(n = 0; n < NDSETS; n++) {
         sprintf(dname, "dataset%d", n);
         dset[n] = H5Dcreate2(fid, dname, H5T_NATIVE_INT, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
         CHECK(dset[n], FAIL, "H5Dcreate2");
    }

    /* The number of opened objects should be NGROUPS + NDSETS + 1.  One is opened file. */
    oid_count = H5Fget_obj_count(fid, H5F_OBJ_ALL);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, (NGROUPS + NDSETS + 1), "H5Fget_obj_count");

    oid_list = (hid_t *)HDcalloc((size_t)oid_list_size, sizeof(hid_t));
    CHECK(oid_list, NULL, "HDcalloc");

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
         sprintf(dname, "dataset%d", n);
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
    CHECK(oid_list, NULL, "HDcalloc");

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
    hid_t		datatype_id, dataset_id, dataspace_id, group_id, attr_id;
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
**  test_file_freespace(): low-level file test routine.
**      This test checks the free space available in a file in various
**      situations.
**
**  Modifications:
**	Vailin Choi; July 2012
**	Remove datasets in reverse order so that all file spaces are shrunk.
**	(A change due to H5FD_FLMAP_DICHOTOMY.)
**
*****************************************************************/
static void
test_file_freespace(void)
{
    hid_t    file;      /* File opened with read-write permission */
    h5_stat_size_t empty_filesize;      /* Size of file when empty */
    h5_stat_size_t mod_filesize;        /* Size of file after being modified */
    hssize_t free_space;        /* Amount of free space in file */
    hid_t    dspace;    /* Dataspace ID */
    hid_t    dset;      /* Dataset ID */
    hid_t    dcpl;      /* Dataset creation property list */
    int k;		/* Local index variable */
    unsigned u;         /* Local index variable */
    char     name[32];  /* Dataset name */
    herr_t   ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Low-Level File Free Space\n"));

    /* Create an "empty" file */
    file = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    ret = H5Fclose(file);
    CHECK_I(ret, "H5Fclose");

    /* Get the "empty" file size */
    empty_filesize = h5_get_file_size(FILE1, H5P_DEFAULT);

    /* Re-open the file (with read-write permission) */
    file = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
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
        sprintf(name, "Dataset %u", u);
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
    VERIFY(free_space, 2360, "H5Fget_freespace");

    /* Delete datasets in file */
    for(k = 9; k >= 0; k--) {
        sprintf(name, "Dataset %u", (unsigned)k);
        ret = H5Ldelete(file, name, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Ldelete");
    } /* end for */

    /* Check that there is the right amount of free space in the file */
    free_space = H5Fget_freespace(file);
    CHECK(free_space, FAIL, "H5Fget_freespace");
    VERIFY(free_space, 0, "H5Fget_freespace");

    /* Close file */
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get the file size after modifications*/
    mod_filesize = h5_get_file_size(FILE1, H5P_DEFAULT);

    /* Check that the file reverted to empty size */
    VERIFY(mod_filesize, empty_filesize, "H5Fget_freespace");
} /* end test_file_freespace() */

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
    fd=HDopen(FILE1, O_RDWR|O_CREAT|O_TRUNC, 0666);
    CHECK(fd, FAIL, "HDopen");

    /* Initialize information to write */
    for(u=0; u<1024; u++)
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
**	It simulates the multi-thread test program from DLS 
**	which exposes the file pointer segmentation fault failure.
**	NOTE: The order on when the files and datasets are open/close
**	is important.
**
*****************************************************************/
static void
test_file_double_file_dataset_open(hbool_t new_format)
{
    hid_t fapl = -1;				/* File access property list */
    hid_t dcpl = -1;				/* Dataset creation property list */
    hid_t fid1 = -1, fid2 = -1;			/* File IDs */
    hid_t did1 = -1, did2 = -1;			/* Dataset IDs */
    hid_t sid1 = -1, sid2 = -1;			/* Dataspace IDs */
    hid_t tid1 = -1, tid2 = -1;			/* Datatype IDs */
    hsize_t dims[1] = {5}, dims2[2] = {1, 4};	/* Dimension sizes */
    hsize_t e_ext_dims[1] = {7};		/* Expanded dimension sizes */
    hsize_t s_ext_dims[1] = {3};		/* Shrunk dimension sizes */
    hsize_t max_dims0[1] = {8}; 		/* Maximum dimension sizes */
    hsize_t max_dims1[1] = {H5S_UNLIMITED}; 	/* Maximum dimesion sizes for extensible array index */
    hsize_t max_dims2[2] = {H5S_UNLIMITED, H5S_UNLIMITED};	/* Maximum dimension sizes for v2 B-tree index */
    hsize_t chunks[1] = {2}, chunks2[2] = {4, 5};		/* Chunk dimension sizes */
    hsize_t size;                               /* File size */
    const char* data[] = {"String 1", "String 2", "String 3", "String 4", "String 5"};	/* Input Data */
    const char* e_data[] = {"String 1", "String 2", "String 3", "String 4", "String 5", "String 6", "String 7"};	/* Input Data */
    char* buffer[5];				/* Output buffer */
    int wbuf[4] = {1, 2, 3, 4};			/* Input data */
    herr_t ret;         			/* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing double file and dataset open/close\n"));

    /* Setting up test file */

    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    if(new_format) {
        ret = H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
        CHECK(ret, FAIL, "H5Pset_libver_bounds");
    } /* end if */

    /* Create the test file */
    fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create a chunked dataset with fixed array indexing */
    sid1 = H5Screate_simple(1, dims, max_dims0);
    CHECK(sid1, FAIL, "H5Screate");
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
    CHECK(sid1, FAIL, "H5Screate");
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
    CHECK(sid2, FAIL, "H5Screate");

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
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* 
     * Scenario 1 
     */

    /* First file open */
    fid1 = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
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
    fid2 = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
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
    fid1 = H5Fopen(FILE1, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Second file open */
    fid2 = H5Fopen(FILE1, H5F_ACC_RDONLY, H5P_DEFAULT);
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
    fid1 = H5Fopen(FILE1, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* First file's dataset open */
    did1 = H5Dopen2(fid1, "/dset_bt2", H5P_DEFAULT);
    CHECK(did1, FAIL, "H5Dopen2");

    /* First file's get storage size */
    size = H5Dget_storage_size(did1);
    CHECK(size, 0, "H5Dget_storage_size");

    /* Second file open */
    fid2 = H5Fopen(FILE1, H5F_ACC_RDONLY, H5P_DEFAULT);
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
     *	 from second call to H5Dset_extent->...H5D__earray_idx_remove->H5EA_get...H5EA__iblock_protect...H5AC_protect
     */
    /* First file open */
    fid1 = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
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
    fid2 = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
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
    fdes = HDopen(file, O_RDONLY, 0);
    CHECK(fdes, FAIL, "HDopen");

    /* Retrieve the file's size */
    ret = HDfstat(fdes, &sb);
    CHECK(fdes, FAIL, "HDfstat");

    /* Allocate space for the file data */
    file_data = HDmalloc((size_t)sb.st_size);
    CHECK(file_data, NULL, "HDmalloc");

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
**	Due to the implementation of file locking (status_flags in 
**	the superblock is used), this test is changed to use checksum 
**	instead of timestamp to verify the file is not changed.
**
**  Programmer: Vailin Choi; July 2013
**
*****************************************************************/
static void
test_rw_noupdate(void)
{
    herr_t ret;         /* Generic return value */
    hid_t fid;			/* File ID */
    uint32_t chksum1, chksum2; 	/* Checksum value */

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
    CHECK(ret, FAIL, "HDopen");

    /* Open and close File With Read/Write Permission */
    fid = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Calculate checksum for the file */
    ret = cal_chksum(FILE1, &chksum2);
    CHECK(ret, FAIL, "HDopen");

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
**  test_free_sections():
**      This routine does the actual work of checking information for
**	free space sections available in a file in various situations.
**
*****************************************************************/
static void
test_free_sections(hid_t fapl, char *fname)
{
    hid_t    file;      	/* File ID */
    hid_t    fcpl;		/* File creation property list template */
    hssize_t free_space;        /* Amount of free space in file */
    hid_t    dspace;    	/* Dataspace ID */
    hid_t    dset;      	/* Dataset ID */
    hid_t    dcpl;      	/* Dataset creation property list */
    unsigned u;         	/* Local index variable */
    char     name[32];  	/* Dataset name */
    hssize_t nsects;        	/* # of free-space sections */
    hssize_t saved_nsects;   	/* saved copy for the # of free-space sections */
    int	     i;			/* local index variable */
    hsize_t  total;        	/* sum of the free-space section sizes */
    hsize_t  last_size;        	/* size of last free-space section */
    H5F_sect_info_t *sect_info;	/* array to hold the free-space information */
    H5F_sect_info_t *saved_sect_info;	/* array to hold the free-space information */
    herr_t   ret;		/* return value */

    /* Create file-creation template */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");

    /* Set file space strategy and free space section threshold */
    ret = H5Pset_file_space(fcpl, H5F_FILE_SPACE_ALL_PERSIST, (hsize_t)0);
    CHECK(ret, FAIL, "H5Pget_file_space");

    /* Create the file */
    file = H5Fcreate(fname, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(file, FAIL, "H5Fcreate");

    /* Close the FCPL */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

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
        sprintf(name, "Dataset %u", u);
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
        sprintf(name, "Dataset %u", u);
	if(u % 2) {
	    ret = H5Ldelete(file, name, H5P_DEFAULT);
	    CHECK(ret, FAIL, "H5Ldelete");
	} /* end if */
    } /* end for */

    /* Close file */
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file with read-only permission */
    file = H5Fopen(fname, H5F_ACC_RDONLY, fapl);
    CHECK_I(file, "H5Fopen");

    /* Get the amount of free space in the file */
    free_space = H5Fget_freespace(file);
    CHECK(free_space, FAIL, "H5Fget_freespace");

    /* Get the # of free-space sections in the file */
    saved_nsects = H5Fget_free_sections(file, H5FD_MEM_DEFAULT, (size_t)0, NULL);
    CHECK(saved_nsects, FAIL, "H5Fget_free_sections");

    /* Allocate storage for the free space section information */
    saved_sect_info = (H5F_sect_info_t *)HDcalloc((size_t)saved_nsects, sizeof(H5F_sect_info_t));
    CHECK(saved_sect_info, NULL, "HDcalloc");

    /* Should return failure when nsects is 0 with a nonnull sect_info */
    nsects = H5Fget_free_sections(file, H5FD_MEM_DEFAULT, (size_t)0, saved_sect_info);
    VERIFY(nsects, FAIL, "H5Fget_free_sections");

    /* Verify the correct # of free-space sections */
    nsects = H5Fget_free_sections(file, H5FD_MEM_DEFAULT, (size_t)saved_nsects, saved_sect_info);
    VERIFY(nsects, saved_nsects, "H5Fget_free_sections");

    /* Verify the amount of free-space is correct */
    total = 0;
    for(i = 0; i < nsects; i++)
	total += saved_sect_info[i].size;
    VERIFY(free_space, total, "H5Fget_free_sections");

    /* save the last section's size */
    last_size = saved_sect_info[nsects-1].size;

    /* Allocate storage for -1 free space section information */
    sect_info = (H5F_sect_info_t *)HDcalloc((size_t)(saved_nsects - 1), sizeof(H5F_sect_info_t));
    CHECK(sect_info, NULL, "HDcalloc");

    /* Retrieve free space info for -1 sections */
    nsects = H5Fget_free_sections(file, H5FD_MEM_DEFAULT, (size_t)(saved_nsects - 1), sect_info);
    VERIFY(nsects, saved_nsects, "H5Fget_free_sections");

    /* Verify the amount of free-space is correct */
    total = 0;
    for(i = 0; i < (saved_nsects - 1); i++) {
	VERIFY(sect_info[i].addr, saved_sect_info[i].addr, "H5Fget_free_sections");
	VERIFY(sect_info[i].size, saved_sect_info[i].size, "H5Fget_free_sections");
	total += sect_info[i].size;
    }

    VERIFY(((hsize_t)free_space - last_size), total, "H5Fget_free_sections");
    HDfree(sect_info);

    /* Allocate storage for +1 free space section information */
    sect_info = (H5F_sect_info_t *)HDcalloc((size_t)(saved_nsects + 1), sizeof(H5F_sect_info_t));
    CHECK(sect_info, NULL, "HDcalloc");

    /* Retrieve free-space info for +1 sections */
    nsects = H5Fget_free_sections(file, H5FD_MEM_DEFAULT, (size_t)(saved_nsects + 1), sect_info);
    VERIFY(nsects, saved_nsects, "H5Fget_free_sections");

    /* Verify free-space info is correct */
    total = 0;
    for(i = 0; i < nsects; i++) {
	VERIFY(sect_info[i].addr, saved_sect_info[i].addr, "H5Fget_free_sections");
	VERIFY(sect_info[i].size, saved_sect_info[i].size, "H5Fget_free_sections");
	total += sect_info[i].size;
    }

    VERIFY(sect_info[nsects].addr, 0, "H5Fget_free_sections");
    VERIFY(sect_info[nsects].size, 0, "H5Fget_free_sections");
    VERIFY(free_space, total, "H5Fget_free_sections");
    HDfree(sect_info);

    /* Verify that there is no free-space section for this type */
    nsects = H5Fget_free_sections(file, H5FD_MEM_BTREE, (size_t)0, NULL);
    VERIFY(nsects, 0, "H5Fget_free_sections");

    /* Close file */
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    HDfree(saved_sect_info);

} /* end test_free_sections() */

/****************************************************************
**
**  test_filespace_sects():
**      This test checks free space section info for
**	files created with sec2 and split drivers.
**
*****************************************************************/
static void
test_filespace_sects(void)
{
    hid_t	fapl_sec2;	/* File access property id with sec2 driver */
    hid_t	fapl_split;	/* File access property id with split driver */
    hid_t	fapl_core;	/* File access property id with core driver */
    hid_t	fapl_stdio;	/* File access property id with stdio driver */
    hid_t	fapl_family;	/* File access property id with family driver */
    char        filename[FILENAME_LEN];	/* Filename to use */
    herr_t   	ret;		/* Return value */

    /* SEC2 */
    MESSAGE(5, ("Testing File free space information for a sec2 file\n"));

    fapl_sec2 = H5Pcreate(H5P_FILE_ACCESS);

    ret = H5Pset_fapl_sec2(fapl_sec2);
    CHECK(ret, FAIL, "H5Pset_fapl_sec2");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl_sec2, filename, sizeof(filename));

    /* perform free space information test for file with sec2 driver */
    test_free_sections(fapl_sec2, filename);

    /* close fapl_sec2 and remove the file */
    h5_clean_files(FILENAME, fapl_sec2);


    /* SPLIT */
    MESSAGE(5, ("Testing File free space information for a split file\n"));

    fapl_split = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl_split, FAIL, "h5_fileaccess");

    ret = H5Pset_fapl_split(fapl_split, "-m.h5", H5P_DEFAULT, "-r.h5", H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Pset_fapl_split");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[1], fapl_split, filename, sizeof(filename));

    /* perform free space information test for file with split driver */
    test_free_sections(fapl_split, filename);

    /* close fapl and remove the file */
    h5_clean_files(FILENAME, fapl_split);


    /* STDIO */
    MESSAGE(5, ("Testing File free space information for a stdio file\n"));

    fapl_stdio = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl_stdio, FAIL, "h5_fileaccess");

    ret = H5Pset_fapl_stdio(fapl_stdio);
    CHECK(ret, FAIL, "H5Pset_fapl_split");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[2], fapl_stdio, filename, sizeof(filename));

    /* perform free space information test for file with stdio driver */
    test_free_sections(fapl_stdio, filename);

    /* close fapl and remove the file */
    h5_clean_files(FILENAME, fapl_stdio);


    /* CORE */
    MESSAGE(5, ("Testing File free space information for a core file\n"));

    fapl_core = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl_core, FAIL, "h5_fileaccess");

    ret = H5Pset_fapl_core(fapl_core, (size_t)CORE_INCREMENT, TRUE);
    CHECK(ret, FAIL, "H5Pset_fapl_core");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[3], fapl_core, filename, sizeof(filename));

    /* perform free space information test for file with core driver */
    test_free_sections(fapl_core, filename);

    /* close fapl_ and remove the file */
    h5_clean_files(FILENAME, fapl_core);


    /* FAMILY */
    MESSAGE(5, ("Testing File free space information for a family file\n"));

    fapl_family = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl_family, FAIL, "h5_fileaccess");

    ret = H5Pset_fapl_family(fapl_family, (hsize_t)FAMILY_SIZE, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Pset_fapl_family");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[4], fapl_family, filename, sizeof(filename));

    /* perform free space information test for file with family driver */
    test_free_sections(fapl_family, filename);

    /* close fapl and remove the file */
    h5_clean_files(FILENAME, fapl_family);

} /* end test_filespace_sects() */

/****************************************************************
**
**  test_filespace_info():
**	Verify that the public routines H5Pget/set_file_space()
**	retrieve and set the file space strategy and free space
**	section threshold as specified.
**
****************************************************************/
static void
test_filespace_info(void)
{
    hid_t	fid1, fid2; 		/* HDF5 File IDs	*/
    hid_t	fapl, new_fapl;		/* File access property */
    hid_t	fcpl, fcpl1, fcpl2;	/* File creation property */
    char        filename[FILENAME_LEN];	/* Filename to use */
    H5F_file_space_type_t	strategy, fs_type, def_type;	/* File space handling strategy */
    hsize_t	threshold, fs_size, def_size;			/* Free space section threshold */
    unsigned 	new_format;	/* new format or old format */
    herr_t	ret;			/* return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing File Space Management public routines: H5Pget/set_file_space()\n"));

    fapl = h5_fileaccess();
    h5_fixname(FILESPACE_NAME[0], fapl, filename, sizeof filename);

    new_fapl = H5Pcopy(fapl);
    CHECK(new_fapl, FAIL, "H5Pcopy");

    /* Set the "use the latest version of the format" bounds */
    ret = H5Pset_libver_bounds(new_fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /* Create file-creation template */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");

    /* Get default file space information */
    ret = H5Pget_file_space(fcpl, &def_type, &def_size);
    CHECK(ret, FAIL, "H5Pget_file_space");

    /* Test with old & new format groups */
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

        /* Test with different sized free space section threshold */
        for(fs_size = 0; fs_size <= TEST_THRESHOLD10; fs_size++) {

            /* Test with different file space handling strategies */
            for(fs_type = H5F_FILE_SPACE_DEFAULT; fs_type < H5F_FILE_SPACE_NTYPES; H5_INC_ENUM(H5F_file_space_type_t, fs_type)) {

                /* Get a copy of the default file creation property */
                fcpl1 = H5Pcopy(fcpl);
                CHECK(fcpl1, FAIL, "H5Pcopy");

                /* Set file space strategy and free space section threshold */
                ret = H5Pset_file_space(fcpl1, fs_type, fs_size);
                CHECK(ret, FAIL, "H5Pget_file_space");

                /* Get the file space info from the creation property */
                ret = H5Pget_file_space(fcpl1, &strategy, &threshold);
                CHECK(ret, FAIL, "H5Pget_file_space");

                /* A 0 value for strategy retains existing strategy in use */
                VERIFY(strategy, (H5F_file_space_type_t)(fs_type ? fs_type : def_type), "H5Pget_file_space");
                /* A 0 value for threshold retains existing threshold in use */
                VERIFY(threshold, (hsize_t)(fs_size ? fs_size : def_size), "H5Pget_file_space");

                /* Create the file with the specified file space info */
                fid1 = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl1, my_fapl);
                CHECK(fid1, FAIL, "H5Fcreate");

                /* Close the file */
                ret = H5Fclose(fid1);
                CHECK(ret, FAIL, "H5Fclose");

                /* Re-open the file */
                fid2 = H5Fopen(filename, H5F_ACC_RDWR, my_fapl);
                CHECK(fid2, FAIL, "H5Fopen");

                /* Get the file's creation property */
                fcpl2 = H5Fget_create_plist(fid2);
                CHECK(fcpl2, FAIL, "H5Fget_create_plist");

                strategy = H5F_FILE_SPACE_DEFAULT;
                threshold = 0;

                /* Get the file space info from the creation property list */
                ret = H5Pget_file_space(fcpl2, &strategy, &threshold);
                CHECK(ret, FAIL, "H5Pget_file_space");

                VERIFY(strategy, (H5F_file_space_type_t)(fs_type ? fs_type : def_type), "H5Pget_file_space");
                VERIFY(threshold, (hsize_t)(fs_size ? fs_size : def_size), "H5Pget_file_space");

                /* Close the file */
                ret = H5Fclose(fid2);
                CHECK(ret, FAIL, "H5Fclose");

                /* Release file-creation template */
                ret = H5Pclose(fcpl1);
                CHECK(ret, FAIL, "H5Pclose");
                ret = H5Pclose(fcpl2);
                CHECK(ret, FAIL, "H5Pclose");
            } /* end for file space strategy type */
        } /* end for free space threshold */

        h5_clean_files(FILESPACE_NAME, my_fapl);

    } /* end for new/old format */

    /* Close the file creation property list */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
}  /* test_filespace_info() */

/****************************************************************
**
**  test_filespace_compatible():
**	Verify that the branch with file space management enhancement
**	can open, read and modify 1.6 HDF5 file and 1.8 HDF5 file.
**	Also verify the correct file space strategy/threshold in use
**	and the amount of free space.
**
****************************************************************/
static void
test_filespace_compatible(void)
{
    int fd_old = (-1), fd_new = (-1);   /* File descriptors for copying data */
    hid_t	fid;	/* File id */
    hid_t	fcpl;	/* File creation property list template */
    hid_t       did;	/* Dataset id */
    int         check[100]; 	/* Temporary buffer for verifying dataset data */
    int         rdbuf[100];	/* Temporary buffer for reading in dataset data */
    uint8_t     buf[READ_OLD_BUFSIZE];	/* temporary buffer for reading */
    ssize_t 	nread;  	/* Number of bytes read in */
    unsigned    i, j;		    /* Local index variable */
    hssize_t	free_space;	    /* Amount of free space in the file */
    hsize_t	threshold;	    /* Free space section threshold */
    H5F_file_space_type_t strategy; /* File space handling strategy */
    herr_t	ret;		    /* Return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing File space compatibility for 1.6 and 1.8 files\n"));

    for(j = 0; j < NELMTS(OLD_FILENAME); j++) {
        const char *filename = H5_get_srcdir_filename(OLD_FILENAME[j]); /* Corrected test file name */

	/* Copy old file into test file */
	fd_old = HDopen(filename, O_RDONLY, 0666);
	CHECK(fd_old, FAIL, "HDopen");
	fd_new = HDopen(FILE5, O_RDWR|O_CREAT|O_TRUNC, 0666);
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

	/* Open the test file */
	fid = H5Fopen(FILE5, H5F_ACC_RDWR, H5P_DEFAULT);
	CHECK(fid, FAIL, "H5Fopen");

	/* There should not be any free space in the file */
	free_space = H5Fget_freespace(fid);
	CHECK(free_space, FAIL, "H5Fget_freespace");
	VERIFY(free_space, (hssize_t)0, "H5Fget_freespace");

	/* Get the file's file creation property list */
	/* Retrieve the file space handling strategy and threshold */
	fcpl = H5Fget_create_plist(fid);
	CHECK(fcpl, FAIL, "H5Fget_create_plist");
	ret = H5Pget_file_space(fcpl, &strategy, &threshold);
	CHECK(ret, FAIL, "H5Pget_file_space");

	/* File space handling strategy should be H5F_FILE_SPACE_ALL = 2 */
	/* Free space section threshold should be 1 */
	VERIFY(strategy, 2, "H5Pget_file_space");
	VERIFY(threshold, 1, "H5Pget_file_space");

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
    ret = H5Oget_info_by_name(file, "/", &oinfo, H5P_DEFAULT);
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

    ret = H5Oget_info_by_name(file, "/", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.version, oh_vers_create, "H5Oget_info_by_name");

    /*
     * Create a group named "G1" in the file, and make sure it has the correct
     * object header version
     */
    group = H5Gcreate2(file, "/G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, FAIL, "H5Gcreate");

    ret = H5Oget_info(group, &oinfo);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.version, oh_vers_mod, "H5Oget_info_by_name");

    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /*
     * Create a group named "/G1/G3" in the file, and make sure it has the
     * correct object header version
     */
    group = H5Gcreate2(file, "/G1/G3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, FAIL, "H5Gcreate");

    ret = H5Oget_info(group, &oinfo);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.version, oh_vers_mod, "H5Oget_info_by_name");

    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /*
     * Make sure the root group still has the correct object header version
     */
    ret = H5Oget_info_by_name(file, "/", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");
    VERIFY(oinfo.hdr.version, oh_vers_create, "H5Oget_info_by_name");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");
} /* end test_libver_bounds_real() */

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
} /* end test_libver_bounds() */

/****************************************************************
**
**  test_libver_macros():
**	Verify that H5_VERSION_GE and H5_VERSION_LE work correactly.
**
****************************************************************/
static void
test_libver_macros(void)
{
    unsigned	major = H5_VERS_MAJOR;
    unsigned	minor = H5_VERS_MINOR;
    unsigned	release = H5_VERS_RELEASE;

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
**	Verify that H5_VERSION_GE works correactly and show how
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
**  test_deprec():
**	Test deprecated functionality.
**
****************************************************************/
#ifndef H5_NO_DEPRECATED_SYMBOLS
static void
test_deprec(void)
{
    hid_t       file;           /* File IDs for old & new files */
    hid_t       fcpl;           /* File creation property list */
    unsigned    super;          /* Superblock version # */
    unsigned    freelist;       /* Free list version # */
    unsigned    stab;           /* Symbol table entry version # */
    unsigned    shhdr;          /* Shared object header version # */
    H5F_info1_t	finfo;		/* global information about file */
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
    ret = H5Pset_file_space(fcpl, H5F_FILE_SPACE_VFD, (hsize_t)0);
    CHECK(ret, FAIL, "H5Pset_file_space");

    /* Creating a file with the non-default file creation property list should
     * create a version 2 superblock
     */

    /* Create file with custom file creation property list */
    file= H5Fcreate(FILE1, H5F_ACC_TRUNC , fcpl, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Close FCPL */
    ret=H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Get the file's version information */
    ret = H5Fget_info1(file, &finfo);
    CHECK(ret, FAIL, "H5Fget_info1");
    VERIFY(finfo.super_ext_size, 40,"H5Fget_info1");
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
    VERIFY(finfo.super_ext_size, 40,"H5Fget_info1");
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
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Low-Level File I/O\n"));

    test_file_create();         /* Test file creation(also creation templates)*/
    test_file_open();           /* Test file opening */
    test_file_reopen();         /* Test file reopening */
    test_file_close();          /* Test file close behavior */
    test_get_file_id();         /* Test H5Iget_file_id */
    test_get_obj_ids();         /* Test H5Fget_obj_ids for Jira Issue 8528 */
    test_file_perm();           /* Test file access permissions */
    test_file_perm2();          /* Test file access permission again */
    test_file_freespace();      /* Test file free space information */
    test_file_ishdf5();         /* Test detecting HDF5 files correctly */
    test_file_open_dot();       /* Test opening objects with "." for a name */
    test_file_open_overlap();   /* Test opening files in an overlapping manner */
    test_file_getname();        /* Test basic H5Fget_name() functionality */
    test_file_double_root_open();       /* Test opening root group from two files works properly */
    test_file_double_group_open();      /* Test opening same group from two files works properly */
    test_file_double_dataset_open();    /* Test opening same dataset from two files works properly */
    test_file_double_datatype_open();   /* Test opening same named datatype from two files works properly */
    test_file_double_file_dataset_open(TRUE);
    test_file_double_file_dataset_open(FALSE);
    test_userblock_file_size(); /* Tests that files created with a userblock have the correct size */
    test_cached_stab_info();    /* Tests that files are created with cached stab info in the superblock */
    test_rw_noupdate();         /* Test to ensure that RW permissions don't write the file unless dirtied */
    test_userblock_alignment(); /* Tests that files created with a userblock and alignment interact properly */
    test_filespace_sects();     /* Test file free space section information */
    test_filespace_info();	/* Test file creation public routines:H5Pget/set_file_space */
    test_filespace_compatible();/* Test compatibility for file space management */
    test_libver_bounds();       /* Test compatibility for file space management */
    test_libver_macros();       /* Test the macros for library version comparison */
    test_libver_macros2();      /* Test the macros for library version comparison */
#ifndef H5_NO_DEPRECATED_SYMBOLS
    test_deprec();              /* Test deprecated routines */
#endif /* H5_NO_DEPRECATED_SYMBOLS */
} /* test_file() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_file
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
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
}

