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

#include "H5Bprivate.h"
#include "H5Pprivate.h"

#define F1_USERBLOCK_SIZE  (hsize_t)0
#define F1_OFFSET_SIZE	   sizeof(haddr_t)
#define F1_LENGTH_SIZE	   sizeof(hsize_t)
#define F1_SYM_LEAF_K	   4
#define F1_SYM_INTERN_K	   16
#define FILE1	"tfile1.h5"

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

static void
create_objects(hid_t, hid_t, hid_t *, hid_t *, hid_t *, hid_t *);
static void
test_obj_count_and_id(hid_t, hid_t, hid_t, hid_t, hid_t, hid_t);
static void
check_file_id(hid_t, hid_t);

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
#ifdef H5_WANT_H5_V1_4_COMPAT
    int		iparm2;
#else /* H5_WANT_H5_V1_4_COMPAT */
    unsigned		iparm2;
#endif /* H5_WANT_H5_V1_4_COMPAT */
    herr_t		ret;		/*generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Low-Level File Creation I/O\n"));

    /* First ensure the file does not exist */
    remove(FILE1);

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
       int         data[F2_DIM0][F2_DIM1];
       unsigned i,j;

       /* Create the data space for the dataset. */
       dims[0] = F2_DIM0;
       dims[1] = F2_DIM1;
       dataspace_id = H5Screate_simple(F2_RANK, dims, NULL);
       CHECK(dataspace_id, FAIL, "H5Screate_simple");

       /* Create the dataset. */
       dataset_id = H5Dcreate(fid2, F2_DSET, H5T_NATIVE_INT, dataspace_id, H5P_DEFAULT);
       CHECK(dataset_id, FAIL, "H5Dcreate");

       for(i=0; i<F2_DIM0; i++)
           for(j=0; j<F2_DIM1; j++)
               data[i][j]=i*10+j;

       /* Write data to the new dataset */
       ret = H5Dwrite(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
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
#ifdef H5_WANT_H5_V1_4_COMPAT
    int		iparm2;
#else /* H5_WANT_H5_V1_4_COMPAT */
    unsigned		iparm2;
#endif /* H5_WANT_H5_V1_4_COMPAT */
    herr_t		ret;		/*generic return value		*/

    /*
     * Test single file open
     */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Low-Level File Opening I/O\n"));

    /* Open first file */
    fid1 = H5Fopen(FILE2, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

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

    /* Open dataset */
    did = H5Dopen(fid1, F2_DSET);
    CHECK(did, FAIL, "H5Dopen");

    /* Close first open */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Open file for second time, which should fail. */
    fid2 = H5Fopen(FILE2, H5F_ACC_RDWR, fapl_id);
    VERIFY(fid2, FAIL, "H5Fopen");

    /* Close dataset from first open */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");
}   /* test_file_open() */

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
       int         data[F2_DIM0][F2_DIM1];
       unsigned    i,j;

       /* Create the data space for the dataset. */
       dims[0] = F2_DIM0;
       dims[1] = F2_DIM1;
       dataspace_id = H5Screate_simple(F2_RANK, dims, NULL);
       CHECK(dataspace_id, FAIL, "H5Screate_simple");

       /* Create the dataset. */
       dataset_id = H5Dcreate(fid1, "/dset", H5T_NATIVE_INT, dataspace_id,
                        H5P_DEFAULT);
       CHECK(dataset_id, FAIL, "H5Dcreate");

       for(i=0; i<F2_DIM0; i++)
           for(j=0; j<F2_DIM1; j++)
               data[i][j]=i*10+j;

       /* Write data to the new dataset */
       ret = H5Dwrite(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
                H5P_DEFAULT, data);
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
        gid1 = H5Gcreate(fid2, "/group", (size_t)0);
        CHECK(gid1, FAIL, "H5Gcreate");
        if(ret_gid1 != NULL)
            *ret_gid1 = gid1;

        gid2 = H5Gopen(fid2, "/group");
        CHECK(gid2, FAIL, "H5Gopen");
        if(ret_gid2 != NULL)
            *ret_gid2 = gid2;

        gid3 = H5Gopen(fid2, "/group");
        CHECK(gid3, FAIL, "H5Gopen");
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
    herr_t              ret;

    /* Create a file */
    fid = H5Fcreate(FILE4, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Test H5Iget_file_id() */
    check_file_id(fid, fid);

    /* Create a group in the file.  Make a duplicated file ID from the group.
     * And close this duplicated ID
     */
    group_id = H5Gcreate(fid, GRP_NAME, (size_t)0);
    CHECK(group_id, FAIL, "H5Gcreate");

    /* Test H5Iget_file_id() */
    check_file_id(fid, group_id);

    /* Close the file and get file ID from the group ID */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Test H5Iget_file_id() */
    check_file_id(-1, group_id);

    ret = H5Gclose(group_id);
    CHECK(ret, FAIL, "H5Gclose");

    /* Open the file again.  Test H5Iget_file_id() */
    fid = H5Fopen(FILE4, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    group_id = H5Gopen(fid, GRP_NAME);
    CHECK(group_id, FAIL, "H5Gcreate");

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

    dataset_id = H5Dcreate(group_id, DSET_NAME, H5T_NATIVE_INT, dataspace_id,
                        H5P_DEFAULT);
    CHECK(dataset_id, FAIL, "H5Dcreate");

    /* Test H5Iget_file_id() */
    check_file_id(fid, dataset_id);

    /* Create an attribute for the dataset.  Make a duplicated file ID from
     * this attribute.  And close it.
     */
    attr_id=H5Acreate(dataset_id,ATTR_NAME,H5T_NATIVE_INT,dataspace_id,H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Acreate");

    /* Test H5Iget_file_id() */
    check_file_id(fid, attr_id);

    /* Create a named datatype.  Make a duplicated file ID from
     * this attribute.  And close it.
     */
    datatype_id=H5Tcopy(H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tcopy");

    ret = H5Tcommit(fid, TYPE_NAME, datatype_id);
    CHECK(ret, FAIL, "H5Tcommit");

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
    oid_count = H5Fget_obj_count(H5F_OBJ_ALL, H5F_OBJ_FILE);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_4, "H5Fget_obj_count");

    /* test object count of all datasets open */
    oid_count = H5Fget_obj_count(H5F_OBJ_ALL, H5F_OBJ_DATASET);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_1, "H5Fget_obj_count");

    /* test object count of all groups open */
    oid_count = H5Fget_obj_count(H5F_OBJ_ALL, H5F_OBJ_GROUP);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_3, "H5Fget_obj_count");

    /* test object count of all named datatypes open */
    oid_count = H5Fget_obj_count(H5F_OBJ_ALL, H5F_OBJ_DATATYPE);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_0, "H5Fget_obj_count");

    /* test object count of all attributes open */
    oid_count = H5Fget_obj_count(H5F_OBJ_ALL, H5F_OBJ_ATTR);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_0, "H5Fget_obj_count");

    /* test object count of all objects currently open */
    oid_count = H5Fget_obj_count(H5F_OBJ_ALL, H5F_OBJ_ALL);
    CHECK(oid_count, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_8, "H5Fget_obj_count");

    {
        hid_t      *oid_list;
        int   i;
        H5I_type_t id_type;

        oid_list = (hid_t*)calloc((size_t)oid_count, sizeof(hid_t));
        if(oid_list != NULL) {
	    ret_count = H5Fget_obj_ids(H5F_OBJ_ALL, H5F_OBJ_ALL, (size_t)oid_count, oid_list);
	    CHECK(ret_count, FAIL, "H5Fget_obj_ids");
        }

        for(i=0; i<oid_count; i++) {
	    id_type = H5Iget_type(oid_list[i]);
	    switch(id_type) {
	        case H5I_FILE:
		    if(oid_list[i]!=fid1 && oid_list[i]!=fid2 &&
			oid_list[i]!=fid3 && oid_list[i]!=fid4) {
			ret = FAIL;
			CHECK(ret, FAIL, "H5Fget_obj_ids");
		    }
		    break;
	        case H5I_GROUP:
                    if(oid_list[i]!=gid1 && oid_list[i]!=gid2 &&
                        oid_list[i]!=gid3) {
			ret = FAIL;
                        CHECK(ret, FAIL, "H5Fget_obj_ids");
                    }
		    break;
	        case H5I_DATASET:
	 	    VERIFY(oid_list[i], did, "H5Fget_obj_ids");
		    break;
		default:
		    ret = FAIL;
                    CHECK(ret, FAIL, "H5Fget_obj_ids");
	    }
        }

        free(oid_list);
    }

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
    dset = H5Dcreate(file, F2_DSET, H5T_NATIVE_INT, dspace, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate");

    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Open the file (with read-only permission) */
    filero = H5Fopen(FILE2, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(filero, FAIL, "H5Fopen");

    /* Create a dataset with the read-only file handle (should fail) */
    H5E_BEGIN_TRY {
        dset = H5Dcreate(filero, F2_DSET, H5T_NATIVE_INT, dspace, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(dset, FAIL, "H5Dcreate");
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
**  test_file_freespace(): low-level file test routine.
**      This test checks the free space available in a file in various
**      situations.
**
*****************************************************************/
static void
test_file_freespace(void)
{
    hid_t    file;      /* File opened with read-write permission */
    hssize_t free_space;        /* Amount of free space in file */
    hid_t    dspace;    /* Dataspace ID */
    hid_t    dset;      /* Dataset ID */
    hid_t    dcpl;      /* Dataset creation property list */
    unsigned u;         /* Local index variable */
    char     name[32];  /* Dataset name */
    herr_t   ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Low-Level File Free Space\n"));

    /* Create the file (with read-write permission) */
    file = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

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
    ret = H5Pset_alloc_time(dcpl,H5D_ALLOC_TIME_EARLY);
    CHECK(ret, FAIL, "H5Pset_alloc_time");

    /* Create datasets in file */
    for(u=0; u<10; u++) {
        sprintf(name,"Dataset %u",u);
        dset = H5Dcreate(file, name, H5T_STD_U32LE, dspace, dcpl);
        CHECK(dset, FAIL, "H5Dcreate");

        ret = H5Dclose(dset);
        CHECK(ret, FAIL, "H5Dclose");
    } /* end for */

    /* Close dataspace */
    ret = H5Sclose(dspace);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close dataset creation property list */
    ret=H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Check that there is the right amount of free space in the file */
    free_space = H5Fget_freespace(file);
    CHECK(free_space, FAIL, "H5Fget_freespace");
    VERIFY(free_space, 168, "H5Fget_freespace");

    /* Delete datasets in file */
    for(u=0; u<10; u++) {
        sprintf(name,"Dataset %u",u);
        ret = H5Gunlink(file, name);
        CHECK(ret, FAIL, "H5Gunlink");
    } /* end for */

    /* Check that there is the right amount of free space in the file */
    free_space = H5Fget_freespace(file);
    CHECK(free_space, FAIL, "H5Fget_freespace");
    VERIFY(free_space, 3584, "H5Fget_freespace");

    /* Close file */
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

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
    CHECK(ret, FAIL, "HDopen");

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
    gid = H5Gcreate(fid, GRP_NAME, (size_t)0);
    CHECK(gid, FAIL, "H5Gcreate");

    /* Create a dataspace for creating datasets */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create a dataset with no name using the file ID */
    H5E_BEGIN_TRY {
        did = H5Dcreate(fid, ".", H5T_NATIVE_INT, sid, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(did, FAIL, "H5Dcreate");

    /* Create a dataset with no name using the group ID */
    H5E_BEGIN_TRY {
        did = H5Dcreate(gid, ".", H5T_NATIVE_INT, sid, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(did, FAIL, "H5Dcreate");

    /* Open a dataset with no name using the file ID */
    H5E_BEGIN_TRY {
        did = H5Dopen(fid, ".");
    } H5E_END_TRY;
    VERIFY(did, FAIL, "H5Dopen");

    /* Open a dataset with no name using the group ID */
    H5E_BEGIN_TRY {
        did = H5Dopen(gid, ".");
    } H5E_END_TRY;
    VERIFY(did, FAIL, "H5Dopen");

    /* Make a copy of a datatype to use for creating a named datatype */
    tid = H5Tcopy(H5T_NATIVE_INT);
    CHECK(tid, FAIL, "H5Tcopy");

    /* Create a named datatype with no name using the file ID */
    H5E_BEGIN_TRY {
        ret = H5Tcommit(fid, ".", tid);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Tcommit");

    /* Create a named datatype with no name using the group ID */
    H5E_BEGIN_TRY {
        ret = H5Tcommit(gid, ".", tid);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Tcommit");

    /* Open a named datatype with no name using the file ID */
    H5E_BEGIN_TRY {
        tid2 = H5Topen(fid, ".");
    } H5E_END_TRY;
    VERIFY(tid2, FAIL, "H5Topen");

    /* Open a named datatype with no name using the group ID */
    H5E_BEGIN_TRY {
        tid2 = H5Topen(gid, ".");
    } H5E_END_TRY;
    VERIFY(tid2, FAIL, "H5Topen");

    /* Create a group with no name using the file ID */
    H5E_BEGIN_TRY {
        gid2 = H5Gcreate(fid, ".", (size_t)0);
    } H5E_END_TRY;
    VERIFY(gid2, FAIL, "H5Gcreate");

    /* Create a group with no name using the group ID */
    H5E_BEGIN_TRY {
        gid2 = H5Gcreate(gid, ".", (size_t)0);
    } H5E_END_TRY;
    VERIFY(gid2, FAIL, "H5Gcreate");

    /* Open a group with no name using the file ID (should open the root group) */
    gid2 = H5Gopen(fid, ".");
    CHECK(gid2, FAIL, "H5Gopen");

    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");

    /* Open a group with no name using the group ID (should open the group again) */
    gid2 = H5Gopen(gid, ".");
    CHECK(gid2, FAIL, "H5Gopen");

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
    int nobjs;          /* # of open objects */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing opening overlapping file opens\n"));

    /* Create file */
    fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Open file also */
    fid2 = H5Fopen(FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid2, FAIL, "H5Fopen");

    /* Create a group in file */
    gid = H5Gcreate(fid1, GROUP1, (size_t)0);
    CHECK(gid, FAIL, "H5Gcreate");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create dataset in group w/first file ID */
    did1 = H5Dcreate(gid, DSET1, H5T_NATIVE_INT, sid, H5P_DEFAULT);
    CHECK(did1, FAIL, "H5Dcreate");

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
    did2 = H5Dcreate(fid2, DSET2, H5T_NATIVE_INT, sid, H5P_DEFAULT);
    CHECK(did2, FAIL, "H5Dcreate");

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
    name_len = H5Fget_name(file_id, name, TESTA_NAME_BUF_SIZE);
    CHECK(name_len, FAIL, "H5Fget_name");
    VERIFY_STR(name, FILE1, "H5Fget_name");

    /* Create a group in the root group */
    group_id = H5Gcreate(file_id, TESTA_GROUPNAME, 0);
    CHECK(group_id, FAIL, "H5Gcreate");

    /* Get and verify file name */
    name_len = H5Fget_name(group_id, name, TESTA_NAME_BUF_SIZE);
    CHECK(name_len, FAIL, "H5Fget_name");
    VERIFY_STR(name, FILE1, "H5Fget_name");

    /* Create the data space  */
    space_id = H5Screate_simple(TESTA_RANK, dims, NULL);
    CHECK(space_id, FAIL, "H5Screate_simple");

    /* Try get file name from data space.  Supposed to fail because
     * it's illegal operation. */
    H5E_BEGIN_TRY {
        name_len = H5Fget_name(space_id, name, TESTA_NAME_BUF_SIZE);
    } H5E_END_TRY;
    VERIFY(name_len, FAIL, "H5Fget_name");

    /* Create a new dataset */
    dataset_id = H5Dcreate(file_id, TESTA_DSETNAME, H5T_NATIVE_INT, space_id, H5P_DEFAULT);
    CHECK(dataset_id, FAIL, "H5Dcreate");

    /* Get and verify file name */
    name_len = H5Fget_name(dataset_id, name, TESTA_NAME_BUF_SIZE);
    CHECK(name_len, FAIL, "H5Fget_name");
    VERIFY_STR(name, FILE1, "H5Fget_name");

    /* Create an attribute for the dataset */
    attr_id = H5Acreate(dataset_id,TESTA_ATTRNAME,H5T_NATIVE_INT,space_id,H5P_DEFAULT);
    CHECK(attr_id, FAIL, "H5Acreate");

    /* Get and verify file name */
    name_len = H5Fget_name(attr_id, name, TESTA_NAME_BUF_SIZE);
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
    ret = H5Tcommit(file_id, TESTA_DTYPENAME, type_id);
    CHECK(ret, FAIL, "H5Tcommit");

    /* Get and verify file name */
    name_len = H5Fget_name(type_id, name, TESTA_NAME_BUF_SIZE);
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

    grp1_id  = H5Gopen(file1_id, "/");
    CHECK(grp1_id, FAIL, "H5Gopen");
    grp2_id  = H5Gopen(file2_id, "/");
    CHECK(grp2_id, FAIL, "H5Gopen");

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

    grp1_id  = H5Gcreate(file1_id, GRP_NAME, (size_t)0);
    CHECK(grp1_id, FAIL, "H5Gcreate");
    grp2_id  = H5Gopen(file2_id, GRP_NAME);
    CHECK(grp2_id, FAIL, "H5Gopen");

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

    dset1_id  = H5Dcreate(file1_id, DSET_NAME, H5T_NATIVE_INT, space_id, H5P_DEFAULT);
    CHECK(dset1_id, FAIL, "H5Dcreate");
    dset2_id  = H5Dopen(file2_id, DSET_NAME);
    CHECK(dset2_id, FAIL, "H5Dopen");

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
    ret  = H5Tcommit(file1_id, TYPE_NAME, type1_id);
    CHECK(ret, FAIL, "H5Tcommit");
    type2_id  = H5Topen(file2_id, TYPE_NAME);
    CHECK(type2_id, FAIL, "H5Topen");

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
**  test_file(): Main low-level file I/O test routine.
**
****************************************************************/
void
test_file(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Low-Level File I/O\n"));

    test_file_create();		/* Test file creation(also creation templates)*/
    test_file_open();		/* Test file opening */
#ifndef H5_NO_SHARED_WRITING
    test_file_close();          /* Test file close behavior */
#endif /* H5_NO_SHARED_WRITING */
    test_get_file_id();         /* Test H5Iget_file_id */
    test_file_perm();           /* Test file access permissions */
    test_file_freespace();      /* Test file free space information */
    test_file_ishdf5();         /* Test detecting HDF5 files correctly */
    test_file_open_dot();       /* Test opening objects with "." for a name */
    test_file_open_overlap();   /* Test opening files in an overlapping manner */
    test_file_getname();        /* Test basic H5Fget_name() functionality */
    test_file_double_root_open();       /* Test opening root group from two files works properly */
    test_file_double_group_open();      /* Test opening same group from two files works properly */
    test_file_double_dataset_open();    /* Test opening same dataset from two files works properly */
    test_file_double_datatype_open();   /* Test opening same named datatype from two files works properly */
}				/* test_file() */


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
    remove(FILE1);
    remove(FILE2);
    remove(FILE3);
    remove(FILE4);
}
