/****************************************************************************
 * NCSA HDF								    *
 * Software Development Group						    *
 * National Center for Supercomputing Applications			    *
 * University of Illinois at Urbana-Champaign				    *
 * 605 E. Springfield, Champaign IL 61820				    *
 *									    *
 * For conditions of distribution and use, see the accompanying		    *
 * hdf/COPYING file.							    *
 *									    *
 ****************************************************************************/

/* $Id$ */

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
#define FILE2	"tfile2.h5"

#define F3_USERBLOCK_SIZE  (hsize_t)0
#define F3_OFFSET_SIZE	   F2_OFFSET_SIZE
#define F3_LENGTH_SIZE	   F2_LENGTH_SIZE
#define F3_SYM_LEAF_K	   F2_SYM_LEAF_K
#define F3_SYM_INTERN_K	   F2_SYM_INTERN_K
#define FILE3	"tfile3.h5"

#define OBJ_ID_COUNT_0     0
#define OBJ_ID_COUNT_1     1 
#define OBJ_ID_COUNT_2     2
#define OBJ_ID_COUNT_3     3 
#define OBJ_ID_COUNT_4     4 
#define OBJ_ID_COUNT_6	   6    
#define OBJ_ID_COUNT_8     8 

static void
create_objects(hid_t, hid_t, hid_t *, hid_t *, hid_t *, hid_t *);
static void
test_obj_count_and_id(hid_t, hid_t, hid_t, hid_t, hid_t, hid_t);

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
    int			iparm;
#ifdef H5_WANT_H5_V1_4_COMPAT
    int		iparm2;
#else /* H5_WANT_H5_V1_4_COMPAT */
    unsigned		iparm2;
#endif /* H5_WANT_H5_V1_4_COMPAT */
    herr_t		ret;		/*generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Low-Level File Creation I/O\n"));

    /* Test create with various sequences of H5F_ACC_EXCL and */
    /* H5F_ACC_TRUNC flags */

    /* Create with H5F_ACC_EXCL */
    /* First ensure the file does not exist */
    remove(FILE1);
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
    CHECK(tmpl1, FAIL, "H5Pnew");

    /* Set the new file-creation parameters */
    ret = H5Pset_userblock(tmpl1, F2_USERBLOCK_SIZE);
    CHECK(ret, FAIL, "H5Pset_userblock");

    ret = H5Pset_sizes(tmpl1, F2_OFFSET_SIZE, F2_LENGTH_SIZE);
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
       dataset_id = H5Dcreate(fid2, "/dset", H5T_NATIVE_INT, dataspace_id, H5P_DEFAULT);
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
    hid_t		fid1;		/*HDF5 File IDs			*/
    hid_t		tmpl1;		/*file creation templates	*/
    hsize_t		ublock;		/*sizeof user block		*/
    size_t		parm;		/*file-creation parameters	*/
    size_t		parm2;		/*file-creation parameters	*/
    int			iparm;
#ifdef H5_WANT_H5_V1_4_COMPAT
    int		iparm2;
#else /* H5_WANT_H5_V1_4_COMPAT */
    unsigned		iparm2;
#endif /* H5_WANT_H5_V1_4_COMPAT */
    herr_t		ret;		/*generic return value		*/

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
}				/* test_file_open() */

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

    /* Close first open */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close second open, should fail since it is SEMI and objects are 
     * still open. */
    ret = H5Fclose(fid2);
    VERIFY(ret, FAIL, "H5Fclose");

    ret = H5Dclose(dataset_id);
    CHECK(ret, FAIL, "H5Dclose");
  
    ret = H5Gclose(group_id1);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Gclose(group_id2);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close second open, should fail since it is SEMI and one group ID is 
     * still open. */
    ret = H5Fclose(fid2);
    VERIFY(ret, FAIL, "H5Fclose");

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

    ret = H5Pset_fclose_degree(fapl_id, H5F_CLOSE_WEAK);
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
    unsigned	oid_count;
    herr_t	ret;

    /* Check reference counts of file IDs and opened object IDs.
     * The verification is hard-coded.  If in any case, this testing
     * is changed, remember to check this part and update the macros.
     */
    {
       ret = H5Fget_obj_count(fid1, H5F_OBJ_ALL, &oid_count);
       CHECK(ret, FAIL, "H5Fget_obj_count");
       VERIFY(oid_count, OBJ_ID_COUNT_2, "H5Fget_obj_count");

       ret = H5Fget_obj_count(fid1, H5F_OBJ_DATASET|H5F_OBJ_GROUP|
				H5F_OBJ_DATATYPE, &oid_count);
       CHECK(ret, FAIL, "H5Fget_obj_count");
       VERIFY(oid_count, OBJ_ID_COUNT_0, "H5Fget_obj_count");

       ret = H5Fget_obj_count(fid2, H5F_OBJ_ALL, &oid_count);
       CHECK(ret, FAIL, "H5Fget_obj_count");
       VERIFY(oid_count, OBJ_ID_COUNT_2, "H5Fget_obj_count");

       ret = H5Fget_obj_count(fid2, H5F_OBJ_DATASET|H5F_OBJ_GROUP|
                                H5F_OBJ_DATATYPE, &oid_count);
       CHECK(ret, FAIL, "H5Fget_obj_count");
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
        gid1 = H5Gcreate(fid2, "/group", 0);
        if(ret_gid1 != NULL)
            *ret_gid1 = gid1;

        gid2 = H5Gopen(fid2, "/group");
        if(ret_gid2 != NULL)
            *ret_gid2 = gid2;

        gid3 = H5Gopen(fid2, "/group");
        if(ret_gid3 != NULL)
            *ret_gid3 = gid3;
    }

    /* Check reference counts of file IDs and opened object IDs.
     * The verification is hard-coded.  If in any case, this testing
     * is changed, remember to check this part and update the macros.
     */
    {
       ret = H5Fget_obj_count(fid1, H5F_OBJ_ALL, &oid_count);
       CHECK(ret, FAIL, "H5Fget_obj_count");
       VERIFY(oid_count, OBJ_ID_COUNT_6, "H5Fget_obj_count");

       ret = H5Fget_obj_count(fid1, H5F_OBJ_DATASET|H5F_OBJ_GROUP|
                                H5F_OBJ_DATATYPE, &oid_count);
       CHECK(ret, FAIL, "H5Fget_obj_count");
       VERIFY(oid_count, OBJ_ID_COUNT_4, "H5Fget_obj_count");

       ret = H5Fget_obj_count(fid2, H5F_OBJ_ALL, &oid_count);
       CHECK(ret, FAIL, "H5Fget_obj_count");
       VERIFY(oid_count, OBJ_ID_COUNT_6, "H5Fget_obj_count");

       ret = H5Fget_obj_count(fid2, H5F_OBJ_DATASET|H5F_OBJ_GROUP|
                                H5F_OBJ_DATATYPE, &oid_count);
       CHECK(ret, FAIL, "H5Fget_obj_count");
       VERIFY(oid_count, OBJ_ID_COUNT_4, "H5Fget_obj_count");
    }
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
    unsigned oid_count;
    herr_t   ret;

    /* Create two new files */
    fid3 = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid3, FAIL, "H5Fcreate");
    fid4 = H5Fcreate(FILE3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid4, FAIL, "H5Fcreate");

    /* test object count of all files IDs open */
    ret = H5Fget_obj_count(H5F_OBJ_ALL, H5F_OBJ_FILE, &oid_count);
    CHECK(ret, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_4, "H5Fget_obj_count");

    /* test object count of all dataset open */
    ret = H5Fget_obj_count(H5F_OBJ_ALL, H5F_OBJ_DATASET, &oid_count);
    CHECK(ret, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_1, "H5Fget_obj_count");

    /* test object count of all group open */
    ret = H5Fget_obj_count(H5F_OBJ_ALL, H5F_OBJ_GROUP, &oid_count);
    CHECK(ret, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_3, "H5Fget_obj_count");

    /* test object count of all datatype open */
    ret = H5Fget_obj_count(H5F_OBJ_ALL, H5F_OBJ_DATATYPE, &oid_count);
    CHECK(ret, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_0, "H5Fget_obj_count");

    /* test object count of all objects currently open */
    ret = H5Fget_obj_count(H5F_OBJ_ALL, H5F_OBJ_ALL, &oid_count);
    CHECK(ret, FAIL, "H5Fget_obj_count");
    VERIFY(oid_count, OBJ_ID_COUNT_8, "H5Fget_obj_count");

    {
        hid_t      *oid_list;
        unsigned   i;
        H5I_type_t id_type;

        oid_list = (hid_t*)calloc(oid_count, sizeof(hid_t));
        if(oid_list != NULL) {
	    ret = H5Fget_obj_ids(H5F_OBJ_ALL, H5F_OBJ_ALL, oid_list);
	    CHECK(ret, FAIL, "H5Fget_obj_ids");
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
    CHECK(fid3, FAIL, "H5Fclose");
    ret = H5Fclose(fid4);
    CHECK(fid4, FAIL, "H5Fclose");
}

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
    test_file_close();          /* Test file close behavior */
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
}
