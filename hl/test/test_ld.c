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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include "h5hltest.h"
#include "H5srcdir.h"
#include "H5LDpublic.h"

/* File name */
#define FILE "test_ld.h5"
/* Copied file name */
#define COPY_FILENAME "COPY_test_ld.h5"

/* Dataset names */
#define DSET_ONE        "DSET_ONE"
#define DSET_ALLOC_LATE "DSET_ALLOC_LATE"
#define DSET_ALLOC_EARLY "DSET_ALLOC_EARLY"
#define DSET_TWO 	"DSET_TWO"
#define TWO_DIM_1	4
#define TWO_DIM_2	10
#define DSET_CMPD 	"DSET_CMPD"
#define DSET_CMPD_ESC 	"DSET_CMPD_ESC"
#define DSET_CMPD_TWO 	"DSET_CMPD_TWO"
#define DSET_NULL       "DSET_NULL"
#define DSET_SCALAR     "DSET_SCALAR"

/* Selected compound field members for testing */
#define VALID_FIELDS1 "field1,field2.a,field3,field4" /* TEMPORORAY */
#define VALID_FIELDS2 "field2.b.a,field2.c,field4.b"

#define INVALID_FIELDS1 "field2.k.a,field2.c,field4.k"
#define INVALID_FIELDS2 "field2.b.a,field2.c,field4.b."
#define INVALID_FIELDS3 "field2.b.a,,field2.c,field4.b"

#define VALID_ESC_FIELDS1 "field\\,1,field2\\..\\.a,field\\\\3,field4\\,"
#define VALID_ESC_FIELDS2 "field2\\..\\,b.a,field2\\..\\\\c,field4\\,.b\\,"

#define INVALID_ESC_FIELDS1 "field2\\..\\,k.a,field2\\..\\\\c,field4\\,.k\\,"
#define INVALID_ESC_FIELDS2 "field2\\..\\,b.a,field2\\..\\\\c,field4\\,.b\\,."
#define INVALID_ESC_FIELDS3 "field2\\..\\,,b.a,field2\\..\\\\c,field4\\,.b\\,"

/* 
 * Test variations (retained original) for one-dimensional dataset:
 *	Varies from 10->13; 10->9, 10->10, 10->1, 10->11
 */
#define ONE_NTESTS      5
int one_tests[ONE_NTESTS] = {3, -1, 0, -9, 1};

/* 
 * Test variations (retained original) for two-dimensional dataset:
 * 	Varies from {4,10}->{6,12}; {4,10}->{6,9}; {4,10}->{6,10};
 *		    {4,10}->{3,12}; {4,10}->{3,9}; {4,10}->{3,10};
 *		    {4,10}->{4,12}; {4,10}->{4,9}; {4,10}->{4,10}
 */
#define TWO_NTESTS 	9
int two_tests[TWO_NTESTS][2] = { {2,2},  {2,-1},  {2,0},
			    	  {-1,2}, {-1,-1}, {-1,0},
			      	  {0,2},  {0,-1},  {0,0} };


/* Verify that the two input values are the same */
#define VERIFY_EQUAL(_x, _y)						\
{									\
	long __x = (long)_x, __y = (long)_y;				\
   	if(__x != __y) TEST_ERROR					\
}

/* Temporary buffer for reading in the test file */
#define TMP_BUF_SIZE		2048
char  g_tmp_buf[TMP_BUF_SIZE];

/* Macros for verifying compound fields */
/* Verify all fields */
#define VERIFY_ELMTS_ALL(ent1, ent2)  {			\
    VERIFY_EQUAL(ent1.field1, ent2.field1);		\
    VERIFY_EQUAL(ent1.field2.a, ent2.field2.a);		\
    VERIFY_EQUAL(ent1.field2.b.a, ent2.field2.b.a);	\
    VERIFY_EQUAL(ent1.field2.b.b, ent2.field2.b.b);	\
    VERIFY_EQUAL(ent1.field2.b.c, ent2.field2.b.c);	\
    VERIFY_EQUAL(ent1.field2.c, ent2.field2.c);		\
    VERIFY_EQUAL(ent1.field3, ent2.field3);		\
    VERIFY_EQUAL(ent1.field4.a, ent2.field4.a);		\
}

/* Verify fields selected in VALID_FIELDS1 */
#define VERIFY_ELMTS_VALID1(ent1, ent2)  {		\
    VERIFY_EQUAL(ent1.field1, ent2.field1);		\
    VERIFY_EQUAL(ent1.field2_a, ent2.field2.a);		\
    VERIFY_EQUAL(ent1.field3, ent2.field3);		\
    VERIFY_EQUAL(ent1.field4.a, ent2.field4.a);		\
    VERIFY_EQUAL(ent1.field4.b, ent2.field4.b);		\
}

/* Verify fields selected in VALID_FIELDS2 */
#define VERIFY_ELMTS_VALID2(ent1, ent2)  {		\
    VERIFY_EQUAL(ent1.field2_b_a, ent2.field2.b.a);	\
    VERIFY_EQUAL(ent1.field2_c, ent2.field2.c);		\
    VERIFY_EQUAL(ent1.field4_b, ent2.field4.b);		\
}

/* The types of 2-dimensional dataset: DSET_TWO or DSET_CMPD_TWO */
#define TWO_NONE                0       /* DSET_TWO */
#define TWO_CMPD_NULL           1       /* DSET_CMPD_TWO with NULL fields */
#define TWO_CMPD_VALID1         2       /* DSET_CMPD_TWO with VALID_FIELDS1 or VALID_ESC_FIELDS1 */
#define TWO_CMPD_VALID2         3       /* DSET_CMPD_TWO with VALID_FIELDS2 or VALID_ESC_FIELDS2 */

#define VERIFY_ELMTS(type, k, ind, _ldbuf, _buf) {			\
    if(type == TWO_NONE) {						\
	int *iib = (int *)_ldbuf;					\
	int *ib = (int *)_buf;						\
									\
	VERIFY_EQUAL(iib[k], ib[ind + n])				\
    } else if(type == TWO_CMPD_NULL) {					\
	set_t *ccb = (set_t *)_ldbuf;					\
	set_t *cb = (set_t *)_buf;					\
									\
	VERIFY_ELMTS_ALL(ccb[k], cb[ind + n])				\
    } else if(type == TWO_CMPD_VALID1) {				\
	test_valid_fields1 *vb1 = (test_valid_fields1 *)_ldbuf;		\
	set_t *cb = (set_t *)_buf;					\
									\
	VERIFY_ELMTS_VALID1(vb1[k], cb[ind + n])			\
    } else if(type == TWO_CMPD_VALID2) {				\
	test_valid_fields2 *vb2 = (test_valid_fields2 *)_ldbuf;		\
	set_t *cb = (set_t *)_buf;					\
									\
	VERIFY_ELMTS_VALID2(vb2[k], cb[ind + n])			\
    }									\
}

/* Tests for test_LD_elmts_pipe() */
#define ONE_TESTS	3
int  onetests[ONE_TESTS] = {3, 9, 1};
#define TWO_TESTS	5
int twotests[TWO_TESTS][2] = { {2,2}, {2,-1}, {2,0}, {-1,2}, {0,2}  };


static herr_t test_LD_dims_params(const char *file);
static herr_t test_LD_dims(const char *file);

static herr_t test_LD_size(const char *file);

static herr_t test_LD_elmts_invalid(const char *file);
static herr_t test_LD_elmts_one(const char *file, const char *dname, const char *fields);
static herr_t test_LD_elmts_two(const char *file, const char *dname, const char *fields);

static herr_t verify_elmts_two(int type, hsize_t *ext_dims, hsize_t *prev_dims,  void *_ldbuf, void *_buf);

/* data structures for compound data type */
typedef struct sub22_t {
    int a;
    int b;
    int c;
} sub22_t;

typedef struct sub2_t {
    int a;
    sub22_t b;
    int c;
} sub2_t;

typedef struct sub4_t {
    int a;
    int b;
} sub4_t;

typedef struct set_t {
    int field1;
    sub2_t field2;
    double field3;
    sub4_t field4;
} set_t;

/* NOTE: 
 * This will fail on heiwa and amani when VALID_FIELDS1 is "field1,field3,field4" 
 * because of alignment problems: 
 *    amani and heiwa - 8 byte alignment
 *    jam - 4 byte alignemnt
 * This will need to be fixed in the library for H5Tget_native_type().
 */
/* VALID_FIELDS1 "field1,field2.a,field3,field4" */
/* VALID_ESC_FIELDS1 "field\\,1,field2\\..\\.a,field\\\\3,field4\\," */
typedef struct test_valid_fields1 {
    int field1;
    int field2_a;
    double field3;
    sub4_t field4;
} test_valid_fields1;

/* VALID_FIELDS2 "field2.b.a,field2.c,field4.b" */
/* VALID_ESC_FIELDS2 "field2\\..\\,b.a,field2\\..\\\\c,field4\\,.b\\," */
typedef struct test_valid_fields2 {
    int field2_b_a;
    int field2_c;
    int field4_b;
} test_valid_fields2;


/* Temporary buffers for tests: test_LD_elmts_one() & test_LD_elmts_two() */
#define TEST_BUF_SIZE 		100
int *iibuf;			/* buffer for storing retrieved elements */
int *ibuf;			/* buffer for storing retrieved elements (integer) */
set_t *cbuf;			/* buffer for storing retrieved elemnets (compound) */
set_t *ccbuf;			/* buffer for storing retrieved elemnets (compound) */
test_valid_fields1 *vbuf1;	/* buffer for storing retrieved elements (FIELDS1) */
test_valid_fields2 *vbuf2;	/* buffer for storing retrieved elements (FIELDS2) */


/* 
 *********************************************************************************
 *
 * Testing for the High Level public routine: H5LDget_dset_dims()
 * 	1) An invalid dataset id
 *	2) "DSET_ALLOC_EARLY": NULL cur_dims
 *	3) "DSET_ALLOC_LATE": nonNULL cur_dims
 *	4) "DSET_CMPD_TWO": nonNULL cur_dims
 *	5) "DSET_NULL": nonNULL cur_dims
 *	6) "DSET_SCALAR": nonNULL cur_dims
 *
 *********************************************************************************
 */
static herr_t
test_LD_dims_params(const char *file)
{
    hid_t fid=-1;		/* file identifier */
    hid_t did=-1;		/* dataset identifier */
    hsize_t one_cur_dims[1];	/* current dimension sizes for 1-dimensonal dataset */
    hsize_t two_cur_dims[2];	/* current dimension sizes for 2-dimensional dataset */
    hid_t invalid_id = -1;
    herr_t ret;			/* return value */

    const char *filename = H5_get_srcdir_filename(file);

    TESTING("H5LDget_dset_dims");

    /* Open the copied file */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* 
     * 1. Verify failure with negative dataset id
     */
    H5E_BEGIN_TRY {
	ret = H5LDget_dset_dims(invalid_id, one_cur_dims);
    } H5E_END_TRY;
    VERIFY_EQUAL(ret, FAIL)

    /* 
     * 2. Verify failure for NULL cur_dims
     */
    if((did = H5Dopen2(fid, DSET_ALLOC_EARLY, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    H5E_BEGIN_TRY {
	ret = H5LDget_dset_dims(did, NULL);
    } H5E_END_TRY;
    VERIFY_EQUAL(ret, FAIL)
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR

    /* 
     * 3. Verify for nonNULL cur_dims
     */
    if((did = H5Dopen2(fid, DSET_ALLOC_LATE, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5LDget_dset_dims(did, one_cur_dims) < 0)
	FAIL_STACK_ERROR
    VERIFY_EQUAL(one_cur_dims[0], 10)
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR

    /* 
     * 4. Verify nonNULL cur_dims for a 2-dimensional dataset
     */
    if((did = H5Dopen2(fid, DSET_CMPD_TWO, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5LDget_dset_dims(did, two_cur_dims) < 0)
	FAIL_STACK_ERROR
    VERIFY_EQUAL(two_cur_dims[0], TWO_DIM_1)
    VERIFY_EQUAL(two_cur_dims[1], TWO_DIM_2)
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR

    /* 
     * 5. Verify nonNULL cur_dims for dataset with H5S_NULL dataspace
     */
    one_cur_dims[0] = 0;

    if((did = H5Dopen2(fid, DSET_NULL, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    if(H5LDget_dset_dims(did, one_cur_dims) < 0)
	FAIL_STACK_ERROR
    VERIFY_EQUAL(one_cur_dims[0], 0)
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR

    /* 
     * 6. Verify nonNULL cur_dims for dataset with H5S_SCALAR dataspace
     */
    one_cur_dims[0] = 0;

    if((did = H5Dopen2(fid, DSET_SCALAR, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    if(H5LDget_dset_dims(did, one_cur_dims) < 0)
	FAIL_STACK_ERROR
    VERIFY_EQUAL(one_cur_dims[0], 0)
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Dclose(did);
	H5Fclose(fid);
    } H5E_END_TRY;

    return(-1);
} /* test_LD_dims_params() */


/* 
 *********************************************************************************
 *
 * Testing for the High Level public routine: H5LDget_dset_dims()
 * Verify that the dimension sizes retrieved via H5LDget_dset_dims() are correct
 * for the following cases:
 *
 *	DSET_ONE: one-dimensional dataset
 *	  1. Increase dims[0]
 *	  2. Decrease dims[0]
 *	  3. same dims[0]
 *	  4. Decrease dims[0]
 *	  5. Increase dims[0]
 *
 *		one_tests[ONE_NTESTS] = {3, -1, 0, -9, 1}
 *		Varies from 10->3; 10->9, 10->10, 10->1, 10->11
 *
 *	DSET_TWO: two-dimensional dataset
 *	  1. Increase dims[0], increase dims[1]
 *	  2. Increase dims[0], decrease dims[1]
 *	  3. Increase dims[0], same dims[1]
 *	  4. Decrease dims[0], increase dims[1]
 *	  5. Decrease dims[0], decrease dims[1]
 *	  6. Decrease dims[0], same dims[1]
 *	  7. same dims[0], increase dims[1]
 *	  8. same dims[0], decrease dims[1]
 *	  9. same dims[0], same dims[1]
 *
 *		two_tests[TWO_NTESTS][2] = { {2,2},  {2,-1},  {2,0},
 *					     {-1,2}, {-1,-1}, {-1,0},
 * 					     {0,2},  {0,-1},  {0,0} }
 *		Varies from {4,10}->{6,12}; {4,10}->{6,9}; {4,10}->{6,10};
 *                  	    {4,10}->{3,12}; {4,10}->{3,9}; {4,10}->{3,10};
 *                  	    {4,10}->{4,12}; {4,10}->{4,9}; {4,10}->{4,10}
 *
 *********************************************************************************
 */
static herr_t
test_LD_dims(const char *file)
{
    hid_t fid=-1;		/* file identifier */
    hid_t did=-1;		/* dataset identifier */
    hsize_t one_prev_dims[1];	/* original dimension sizes for 1-dimensonal dataset */
    hsize_t one_cur_dims[1];	/* current dimension sizes for 1-dimensonal dataset */
    hsize_t one_ext_dims[1];	/* extended dimension sizes for 1-dimensonal dataset */
    hsize_t two_prev_dims[2];	/* original dimension sizes for 2-dimensional dataset */
    hsize_t two_cur_dims[2];	/* current dimension sizes for 2-dimensional dataset */
    hsize_t two_ext_dims[2];	/* extended dimension sizes for 2-dimensional dataset*/
    int i;			/* local index variable */

    TESTING("H5LDget_dset_dims with H5Dset_extent");

    /* Make a copy of the test file */
    if(h5_make_local_copy(file, COPY_FILENAME) < 0)
        TEST_ERROR

    /* Open the copied file */
    if((fid = H5Fopen(COPY_FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* 
     * Testing with one-dimensional dataset: DSET_ONE
     */
    if((did = H5Dopen2(fid, DSET_ONE, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Retrieve dimension sizes */
    if(H5LDget_dset_dims(did, one_prev_dims) < 0)
	FAIL_STACK_ERROR

    for(i = 0; i < ONE_NTESTS; i++) {

	/* Set up the extended dimension sizes */
	one_ext_dims[0] = (hsize_t)((int)one_prev_dims[0] + one_tests[i]);

	/* Change the dimension size */
	if(H5Dset_extent(did, one_ext_dims) < 0)
	    FAIL_STACK_ERROR

	/* Retrieve the dimension size */
	if(H5LDget_dset_dims(did, one_cur_dims) < 0)
	    FAIL_STACK_ERROR

	/* Verify that the retrieved dimension size is correct as expected */
	VERIFY_EQUAL(one_cur_dims[0], one_ext_dims[0])
    }

    /* Close the dataset */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR

    /* 
     * Testing with two-dimensional dataset: DSET_TWO
     */
    if((did = H5Dopen2(fid, DSET_TWO, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Retrieve the dimension sizes */
    if(H5LDget_dset_dims(did, two_prev_dims) < 0)
	FAIL_STACK_ERROR

    for(i = 0; i < TWO_NTESTS; i++) {

	/* Set up the extended dimension sizes */
	two_ext_dims[0] = (hsize_t)((int)two_prev_dims[0] + two_tests[i][0]);
	two_ext_dims[1] = (hsize_t) ((int)two_prev_dims[1] + two_tests[i][1]);

	/* Change the dimension sizes */
	if(H5Dset_extent(did, two_ext_dims) < 0)
	    FAIL_STACK_ERROR

	/* Retrieve the dimension sizes */
	if(H5LDget_dset_dims(did, two_cur_dims) < 0)
	    FAIL_STACK_ERROR

	/* Verify that the retrieved dimension sizes are correct as expected */
	VERIFY_EQUAL(two_cur_dims[0], two_ext_dims[0])
	VERIFY_EQUAL(two_cur_dims[1], two_ext_dims[1])
    } /* end TWO_NTESTS */

    /* Close the dataset */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR

    /* Remove the copied file */
    HDremove(COPY_FILENAME);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Dclose(did);
	H5Fclose(fid);
    } H5E_END_TRY;
    return(-1);

} /* test_LD_dims() */


/* 
 **********************************************************************************
 *
 * Testing for the High Level public routine: H5LDget_dset_type_size()
 * Verify that the data type size returned via H5LDget_dset_type_size()
 * are correct for the following cases:
 *
 *	Verify failure for an invalid dataset id
 *
 *	DSET_CMPD: one-dimensional dataset with compound type
 *	  1. The whole element 
 *	  2. VALID_FIELDS1: "field1,field2.a,field3,field4"
 *	  3. VALID_FIELDS2: "field2.b.a,field2.c,field4.b"
 *	  4. INVALID_FIELDS1: "field2.k.a,field2.c,field4.k"
 *	  5. INVALID_FIELDS2: "field2.b.a,field2.c,field4.b."
 *	  6. INVALID_FIELDS3: "field2.b.a,,field2.c,field4.b"
 *
 *	DSET_CMPD_ESC: one-dimensional dataset with compound type and
 *		       member names with escape/separator characters
 *	  1. The whole element
 * 	  2. VALID_ESC_FIELDS1: "field\\,1,field2\\..\\.a,field\\\\3,field4\\,"
 *	  3. VALID_ESC_FIELDS2: "field2\\..\\,b.a,field2\\..\\\\c,field4\\,.b\\,"
 *	  4. INVALID_ESC_FIELDS1: "field2\\..\\,k.a,field2\\..\\\\c,field4\\,.k\\,"
 *	  5. INVALID_ESC_FIELDS2: "field2\\..\\,b.a,field2\\..\\\\c,field4\\,.b\\,."
 * 	  6. INVALID_ESC_FIELDS3: "field2\\..\\,,b.a,field2\\..\\\\c,field4\\,.b\\,"
 *
 **********************************************************************************
 */
static int
test_LD_size(const char *file)
{
    hid_t fid=-1;	/* file identifier */
    hid_t did=-1;	/* dataset identifier */
    hid_t dtid=-1;	/* dataset's datatype identifier */
    hid_t invalid_id=-1;
    hid_t memb0_tid=-1;	/* type identifier for a member in the compound type */
    hid_t memb1_tid=-1;	/* type identifier for a member in the compound type */
    hid_t memb2_tid=-1;	/* type identifier for a member in the compound type */
    hid_t memb3_tid=-1;	/* type identifier for a member in the compound type */
    hid_t memb_tid=-1;	/* type identifier for a member in the compound type */
    hid_t memb_tid2=-1;	/* type identifier for a member in the compound type */
    size_t dsize;	/* size of the dataset's datatype */
    size_t ck_dsize;	/* size of the dataset's datatype to be checked against */

    const char *filename = H5_get_srcdir_filename(file);

    TESTING("H5LDget_dset_type_size");

    /* Open the file */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* 
     * Verify failure with an invalid dataset id
     */
    H5E_BEGIN_TRY {
	dsize = H5LDget_dset_type_size(invalid_id, NULL);
    } H5E_END_TRY;
    VERIFY_EQUAL(dsize, 0)

    /* 
     * Testing one-dimensional dataset with compound datatype:
     *		DSET_CMPD
     */

    /* Open dataset DSET_CMPD */
    if((did = H5Dopen2(fid, DSET_CMPD, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Return size of the whole element */
    if((dsize = H5LDget_dset_type_size(did, NULL)) == 0)
	FAIL_STACK_ERROR

    /* Get the dataset's datatype and then its datatype size */
    if((dtid = H5Tget_native_type(H5Dget_type(did), H5T_DIR_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    if((ck_dsize = H5Tget_size(dtid)) == 0)
	FAIL_STACK_ERROR

    /* Verify case #1 */
    VERIFY_EQUAL(dsize, ck_dsize)

    /* Get datatype id for each member */
    if((memb0_tid = H5Tget_member_type(dtid, 0)) < 0) /* "field1" */
	FAIL_STACK_ERROR
    if((memb1_tid = H5Tget_member_type(dtid, 1)) < 0) /* "field2" */
	FAIL_STACK_ERROR
    if((memb2_tid = H5Tget_member_type(dtid, 2)) < 0) /* "field3" */
	FAIL_STACK_ERROR
    if((memb3_tid = H5Tget_member_type(dtid, 3)) < 0) /* "field4" */
	FAIL_STACK_ERROR

    /* Obtain size for VALID_FIELDS1: "field1,field2.a,field3,field4" */
    if((dsize = H5LDget_dset_type_size(did, VALID_FIELDS1)) == 0)
	FAIL_STACK_ERROR

    /* Get the datatype size for "field1" */
    if((ck_dsize = H5Tget_size(memb0_tid)) == 0)
	FAIL_STACK_ERROR

    /* Add the datatype size for "field2.a" */
    if((memb_tid = H5Tget_member_type(memb1_tid, 0)) < 0)
	FAIL_STACK_ERROR
    if((ck_dsize += H5Tget_size(memb_tid)) == 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb_tid) < 0)
	FAIL_STACK_ERROR

    /* Add the datatype size for "field3" */
    if((ck_dsize += H5Tget_size(memb2_tid)) == 0)
	FAIL_STACK_ERROR

    /* Add the datatype size for "field4" */
    if((ck_dsize += H5Tget_size(memb3_tid)) == 0)
	FAIL_STACK_ERROR

    /* Verify case #2 */
    VERIFY_EQUAL(dsize, ck_dsize)

    /* Obtain datatype size for VALID_FIELDS2: "field2.b.a,field2.c,field4.b" */
    if((dsize = H5LDget_dset_type_size(did, VALID_FIELDS2)) == 0)
	FAIL_STACK_ERROR

    /* Get the datatype size for "field2.b.a" */
    if((memb_tid = H5Tget_member_type(memb1_tid, 1)) < 0)
	FAIL_STACK_ERROR
    if((memb_tid2 = H5Tget_member_type(memb_tid, 0)) < 0)
	FAIL_STACK_ERROR
    if((ck_dsize = H5Tget_size(memb_tid2)) == 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb_tid) < 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb_tid2) < 0)
	FAIL_STACK_ERROR

    /* Add the datatype size for "field2.c" */
    if((memb_tid = H5Tget_member_type(memb1_tid, 2)) < 0)
	FAIL_STACK_ERROR
    if((ck_dsize += H5Tget_size(memb_tid)) == 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb_tid) < 0)
	FAIL_STACK_ERROR

    /* Add the datatype size for "field4.b" */
    if((memb_tid = H5Tget_member_type(memb3_tid, 1)) < 0)
	FAIL_STACK_ERROR
    if((ck_dsize += H5Tget_size(memb_tid)) == 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb_tid) < 0)
	FAIL_STACK_ERROR

    /* Verify case #3 */
    VERIFY_EQUAL(dsize, ck_dsize)

    /*
     * Verify failure for the following invalid nested fields: 
     *	INVALID_FIELDS1: "field2.k.a,field2.c,field4.k"
     *  INVALID_FIELDS2: "field2.b.a,field2.c,field4.b."
     *  INVALID_FIELDS3: "field2.b.a,,field2.c,field4.b"
     */
    /* Verify failure for case #4 */
    dsize = H5LDget_dset_type_size(did, INVALID_FIELDS1);
    VERIFY_EQUAL(dsize, 0)

    /* Verify failure for case #5 */
    dsize = H5LDget_dset_type_size(did, INVALID_FIELDS2);
    VERIFY_EQUAL(dsize, 0)

    /* Verify failure for case #6 */
    dsize = H5LDget_dset_type_size(did, INVALID_FIELDS3);
    VERIFY_EQUAL(dsize, 0)
    
    /* Closing */
    if(H5Tclose(memb0_tid) < 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb1_tid) < 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb2_tid) < 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb3_tid) < 0)
	FAIL_STACK_ERROR
    if(H5Tclose(dtid) < 0)
	FAIL_STACK_ERROR
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR

    /* 
     * Testing one-dimensional dataset with compound datatype and
     *	 member names consisting of escape/separator characters:
     *		DSET_CMPD_ESC
     */

    /* Open dataset DSET_CMPD_ESC */
    if((did = H5Dopen2(fid, DSET_CMPD_ESC, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Return size of the whole element */
    if((dsize = H5LDget_dset_type_size(did, NULL)) == 0)
	FAIL_STACK_ERROR

    /* Get the dataset's datatype and then its datatype size */
    if((dtid = H5Tget_native_type(H5Dget_type(did), H5T_DIR_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if((ck_dsize = H5Tget_size(dtid)) == 0)
	FAIL_STACK_ERROR

    /* Verify case #1 */
    VERIFY_EQUAL(dsize, ck_dsize)

    /* Get datatype id for each member */
    if((memb0_tid = H5Tget_member_type(dtid, 0)) < 0) /* "field,1" */
	FAIL_STACK_ERROR
    if((memb1_tid = H5Tget_member_type(dtid, 1)) < 0) /* "field2." */
	FAIL_STACK_ERROR
    if((memb2_tid = H5Tget_member_type(dtid, 2)) < 0) /* "field\3" */
	FAIL_STACK_ERROR
    if((memb3_tid = H5Tget_member_type(dtid, 3)) < 0) /* "field4," */
	FAIL_STACK_ERROR

    /* Obtain size for VALID_ESC_FIELDS1: "field\\,1,field2\\..\\.a,field\\\\3,field4\\," */
    if((dsize = H5LDget_dset_type_size(did, VALID_ESC_FIELDS1)) == 0)
	FAIL_STACK_ERROR

    /* Get the datatype size for "field\\,1" */
    if((ck_dsize = H5Tget_size(memb0_tid)) == 0)
	FAIL_STACK_ERROR

    /* Add the datatype size for "field2\\..\\.a" */
    if((memb_tid = H5Tget_member_type(memb1_tid, 0)) < 0)
	FAIL_STACK_ERROR
    if((ck_dsize += H5Tget_size(memb_tid)) == 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb_tid) < 0)
	FAIL_STACK_ERROR

    /* Add the datatype size for "field\\\\3" */
    if((ck_dsize += H5Tget_size(memb2_tid)) == 0)
	FAIL_STACK_ERROR

    /* Add the datatype size for "field4\\," */
    if((ck_dsize += H5Tget_size(memb3_tid)) == 0)
	FAIL_STACK_ERROR

    /* Verify case #2 */
    VERIFY_EQUAL(dsize, ck_dsize)

    /* Obtain datatype size for VALID_ESC_FIELDS2: 
	    "field2\\..\\,b.a,field2\\..\\\\c,field4\\,.b\\," */
    if((dsize = H5LDget_dset_type_size(did, VALID_ESC_FIELDS2)) == 0)
	FAIL_STACK_ERROR

    /* Get the datatype size for "field2\..,b.a" */
    if((memb_tid = H5Tget_member_type(memb1_tid, 1)) < 0)
	FAIL_STACK_ERROR
    if((memb_tid2 = H5Tget_member_type(memb_tid, 0)) < 0)
	FAIL_STACK_ERROR
    if((ck_dsize = H5Tget_size(memb_tid2)) == 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb_tid) < 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb_tid2) < 0)
	FAIL_STACK_ERROR

    /* Add the datatype size for "field2\..\\c" */
    if((memb_tid = H5Tget_member_type(memb1_tid, 2)) < 0)
	FAIL_STACK_ERROR
    if((ck_dsize += H5Tget_size(memb_tid)) == 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb_tid) < 0)
	FAIL_STACK_ERROR

    /* Add the datatype size for "field4\,.b\," */
    if((memb_tid = H5Tget_member_type(memb3_tid, 1)) < 0)
	FAIL_STACK_ERROR
    if((ck_dsize += H5Tget_size(memb_tid)) == 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb_tid) < 0)
	FAIL_STACK_ERROR

    /* Verify case #3 */
    VERIFY_EQUAL(dsize, ck_dsize)

    /*
     * Verify failure for the following invalid nested fields: 
     *   INVALID_ESC_FIELDS1: "field2\..\,k.a,field2\..\\c,field4\,.k\,"
     *   INVALID_ESC_FIELDS2: "field2\..\,b.a,field2\..\\c,field4\,.b\,."
     *   INVALID_ESC_FIELDS3: "field2\..\,,b.a,field2\..\\c,field4\,.b\,"
     */
    /* Verify failure for case #4 */
    dsize = H5LDget_dset_type_size(did, INVALID_ESC_FIELDS1);
    VERIFY_EQUAL(dsize, 0)

    /* Verify failure for case #5 */
    dsize = H5LDget_dset_type_size(did, INVALID_ESC_FIELDS2);
    VERIFY_EQUAL(dsize, 0)

    /* Verify failure for case #6 */
    dsize = H5LDget_dset_type_size(did, INVALID_ESC_FIELDS3);
    VERIFY_EQUAL(dsize, 0)

    /* Closing */
    if(H5Tclose(memb0_tid) < 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb1_tid) < 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb2_tid) < 0)
	FAIL_STACK_ERROR
    if(H5Tclose(memb3_tid) < 0)
	FAIL_STACK_ERROR
    if(H5Tclose(dtid) < 0)
	FAIL_STACK_ERROR
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR

    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Tclose(memb0_tid);
	H5Tclose(memb1_tid);
	H5Tclose(memb2_tid);
	H5Tclose(memb3_tid);
	H5Tclose(memb_tid);
	H5Tclose(memb_tid2);
	H5Tclose(dtid);
	H5Dclose(did);
	H5Fclose(fid);
    } H5E_END_TRY;
    return(-1);

} /* test_LD_size() */


/* 
 **************************************************************************************
 * Testing for the High Level public routine: H5LDget_dset_elmts()
 * 	Verify failures when calling H5LDget_dset_elmts() with the following
 *   	invalid conditions: 
 *
 *	A. DSET_TWO: two-dimensional dataset
 *		1. CUR_DIMS and PREV_DIMS are NULL
 *		2. PREV_DIMS is NULL
 *		3. CUR_DIMS is NULL
 *		4. FIELDS is nonnull but the dataset is not compound datatype
 *		5. BUF is NULL
 *		6. CUR_DIMS is not greater than PREV_DIMS
 *
 *	B. DSET_CMPD: one-dimensional dataset with compound type
 *		1. Invalid dataset id
 *		2. FIELDS are not valid members in the compound type
 *
 **************************************************************************************
 */
static int
test_LD_elmts_invalid(const char *file)
{
    hid_t fid=-1;		/* file identifier */
    hid_t did=-1;		/* dataset identifier */
    hid_t sid=-1;		/* dataspace identifier */
    hid_t invalid_id=-1;
    int ret;			/* return value */
    hsize_t cur_dims[2];	/* current dimension sizes of the dataset */
    hsize_t prev_dims[2];	/* previous dimension sizes of the dataset */
    char tbuf[2];	/* temporary buffer for testing */
    int ndims;		/* # of dimension sizes */
    int i;		/* local index variable */

    const char *filename = H5_get_srcdir_filename(file);

    TESTING("H5LDget_dset_elmts on invalid conditions");

    /* Open the copied file */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* 
     * Testing two-dimensional dataset: DSET_TWO
     */

    /* Open dataset: DSET_TWO */
    if((did = H5Dopen2(fid, DSET_TWO, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Verify failure from case #1: cur_dims and prev_dims are NULL */
    ret = H5LDget_dset_elmts(did, NULL, NULL, NULL, NULL);
    VERIFY_EQUAL(ret, FAIL)

    /* Verify failure from case #2: prev_dims is NULL */
    ret = H5LDget_dset_elmts(did, cur_dims, NULL, NULL, NULL);
    VERIFY_EQUAL(ret, FAIL)

    /* Verify failure from case #3: cur_dims is NULL */
    ret = H5LDget_dset_elmts(did, NULL, prev_dims, NULL, NULL);
    VERIFY_EQUAL(ret, FAIL)

    if((sid = H5Dget_space(did)) < 0)
	FAIL_STACK_ERROR

    /* Get the # of dimensions and current dimension sizes */
    if((ndims = H5Sget_simple_extent_dims(sid, cur_dims, NULL)) < 0)
	FAIL_STACK_ERROR

    /* Set up valid cur_dims and prev_dims */
    for(i = 0; i < ndims; i++)
	prev_dims[i] = cur_dims[i] - 1;

    /* Verify failure from case #4: FIELDS is nonNULL but the dataset is not compound datatype */
    ret = H5LDget_dset_elmts(did, prev_dims, cur_dims, "field1", tbuf);
    VERIFY_EQUAL(ret, FAIL)

    /* Verify failure from case #5: BUF is NULL */
    ret = H5LDget_dset_elmts(did, prev_dims, cur_dims, NULL, NULL);
    VERIFY_EQUAL(ret, FAIL)

    /* Verify failure from case #6: cur_dims is not > than prev_dims */
    cur_dims[0] = prev_dims[0] - 1;
    cur_dims[1] = prev_dims[1] - 1;
    ret = H5LDget_dset_elmts(did, prev_dims, cur_dims, NULL, tbuf);
    VERIFY_EQUAL(ret, FAIL)

    /* Close DSET_TWO */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR

    /* 
     * Testing one-dimensional dataset with compound datatype:
     *		DSET_CMPD
     */

    /* Verify failure from case #1: an invalid dataset id */
    H5E_BEGIN_TRY {
	ret = H5LDget_dset_elmts(invalid_id, prev_dims, cur_dims, NULL, tbuf);
    } H5E_END_TRY;
    VERIFY_EQUAL(ret, FAIL)

    /* Open dataset: DSET_CMPD */
    if((did = H5Dopen2(fid, DSET_CMPD, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Retrieve the current dimension sizes */
    if(H5LDget_dset_dims(did, cur_dims) < 0)
	FAIL_STACK_ERROR

    /* Set up valid cur_dims, prev_dims */
    prev_dims[0] = cur_dims[0] - 1;

    /* Verify failure from case #2: invalid FIELDS */
    ret = H5LDget_dset_elmts(did, prev_dims, cur_dims, "field2.k.a,field2.c,field4.k", tbuf);
    VERIFY_EQUAL(ret, FAIL)

    /* Close DSET_CMPD */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Sclose(sid);
	H5Dclose(did);
	H5Fclose(fid);
    } H5E_END_TRY;
    return(-1);

} /* test_LD_elmts_invalid() */


/* 
 **************************************************************************************
 * Testing for the High Level public routine: H5LDget_dset_elmts()
 * 	Verify elements retrieved via H5LDget_dset_elmts() are correct as expected
 *	when the dataset's dimension sizes are changed according to one_tests[]:
 *
 *	one-dimensional dataset : 
 *		DSET_ONE with NULL fields
 *		DSET_CMPD with fields: NULL, VALID_FIELDS1, VALID_FIELDS2
 *		DSET_CMPD_ESC with fields: NULL, VALID_ESC_FIELDS1, VALID_ESC_FIELDS2
 *	
 *		case #1. increase dims[0]
 *        	case #2. decrease dims[0]	(failure)
 *        	case #3. same dims[0]		(failure)
 *        	case #4. decrease dims[0]	(failure)
 *        	case #5. increase dims[0]
 *
 **************************************************************************************
 */
static herr_t
test_LD_elmts_one(const char *file, const char *dname, const char *fields)
{
    hid_t fid=-1;		/* file identifier */
    hid_t did=-1;		/* dataset identifier */
    hid_t dtype=-1;		/* dataset's data type */
    hsize_t ext_dims[1];	/* extended dimension sizes of the dataset */
    hsize_t prev_dims[1];	/* previous dimension sizes of the dataset */
    int i, j;			/* local index variable */
    int ret = 0;		/* return value */

    TESTING("H5LDget_dset_elmts: one-dimensional dataset");

    /* Copy the test file */
    if(h5_make_local_copy(file, COPY_FILENAME) < 0)
        TEST_ERROR

    for(i = 0; i < TEST_BUF_SIZE; i++) {
	cbuf[i].field1 = i;
	cbuf[i].field2.a = i;
	cbuf[i].field2.b.a = i;
	cbuf[i].field2.b.b = i;
	cbuf[i].field2.b.c = i;
	cbuf[i].field2.c = i;
	cbuf[i].field3 = (double)i;
	cbuf[i].field4.a = i;
	cbuf[i].field4.b = i;
	ibuf[i] = i;
    } /* end for */

    /* Open the copied file */
    if((fid = H5Fopen(COPY_FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Open the dataset */
    if((did = H5Dopen2(fid, dname, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Get the dataset's data type */
    if((dtype = H5Tget_native_type(H5Dget_type(did), H5T_DIR_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Get current dimension sizes before extending the dataset's dimension sizes */
    if(H5LDget_dset_dims(did, prev_dims) < 0)
	FAIL_STACK_ERROR

    /* Loop through different variations of extending the dataset */
    for(i = 0; i < ONE_NTESTS; i++) {
	HDmemset(vbuf1, 0, TEST_BUF_SIZE * sizeof(test_valid_fields1));
	HDmemset(vbuf2, 0, TEST_BUF_SIZE * sizeof(test_valid_fields2));
	HDmemset(ccbuf, 0, TEST_BUF_SIZE * sizeof(set_t));
	HDmemset(iibuf, 0, TEST_BUF_SIZE * sizeof(int));

	ext_dims[0] = (hsize_t)((int)prev_dims[0] + one_tests[i]);

	/* Change the dimension sizes of the dataset */
	if(H5Dset_extent(did, ext_dims) < 0)
	    FAIL_STACK_ERROR

	/* Initialize data */
	if(!HDstrcmp(dname, DSET_CMPD) || !HDstrcmp(dname, DSET_CMPD_ESC)) {
	    if(H5Dwrite(did, dtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, cbuf) < 0)
		FAIL_STACK_ERROR
	} /* end if */
        else if(!HDstrcmp(dname, DSET_ONE)) {
	    if(H5Dwrite(did, dtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, ibuf) < 0)
		FAIL_STACK_ERROR
	} /* end if */

	/* There are changes in dimension sizes */
	if(one_tests[i] > 0) {
	    if(!HDstrcmp(dname, DSET_CMPD) || !HDstrcmp(dname, DSET_CMPD_ESC)) {
		if(fields) {
                    if(!HDstrcmp(fields, VALID_FIELDS1) || !HDstrcmp(fields, VALID_ESC_FIELDS1)) {
                        /* Retrieve the elmemts in BUF */
                        if(H5LDget_dset_elmts(did, prev_dims, ext_dims, fields, vbuf1) < 0)
                            TEST_ERROR
                        for(j = 0; j < one_tests[i]; j++)
                            VERIFY_ELMTS_VALID1(vbuf1[j], cbuf[prev_dims[0] + (hsize_t)j])
                    } /* end if */
                    else if(!HDstrcmp(fields, VALID_FIELDS2) || !HDstrcmp(fields, VALID_ESC_FIELDS2)) {
                        /* Retrieve the elmemts in BUF */
                        if(H5LDget_dset_elmts(did, prev_dims, ext_dims, fields, vbuf2) < 0)
                            TEST_ERROR
                        for(j = 0; j < one_tests[i]; j++)
                            VERIFY_ELMTS_VALID2(vbuf2[j], cbuf[prev_dims[0] + (hsize_t)j])
                    } /* end else-if */
                    else
			TEST_ERROR
		} /* end if */
                else {
		    /* Retrieve the elmemts in BUF */
		    if(H5LDget_dset_elmts(did, prev_dims, ext_dims, fields, ccbuf) < 0)
			TEST_ERROR
		    for(j = 0; j < one_tests[i]; j++)
			VERIFY_ELMTS_ALL(ccbuf[j], cbuf[prev_dims[0] + (hsize_t)j])
		} /* end else-if */
	    } /* end if */
            else {
		/* Retrieve the elmemts in BUF */
		if(H5LDget_dset_elmts(did, prev_dims, ext_dims, fields, iibuf) < 0)
		    TEST_ERROR
		for(j = 0; j < one_tests[i]; j++)
		    VERIFY_EQUAL(iibuf[j], ibuf[prev_dims[0] + (hsize_t)j])
	    } /* end else */
	} /* end if */
        else {
            /* Verify failure when changes between prev_dims and ext_dims are same/decrease */
            ret = H5LDget_dset_elmts(did, prev_dims, ext_dims, fields, iibuf);
            VERIFY_EQUAL(ret, FAIL)
        } /* end else */
    } /* end for */

    /* Closing */
    if(H5Tclose(dtype) < 0)
        FAIL_STACK_ERROR
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR

    /* Remove the copied file */
    HDremove(COPY_FILENAME);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Tclose(dtype);
	H5Dclose(did);
	H5Fclose(fid);
    } H5E_END_TRY;
    return(-1);
} /* test_LD_elmts_one() */


/*
 **************************************************************************************
 *
 * Helper routine to verify elements of a 2-dimensional dataset 
 *	_ldbuf contains the elements retrieved via H5LDget_dset_elmts()
 *	_buf contains the data written to the dataset
 *
 *	e.g. prev_dims[2] = {4, 6}; ext_dims[2] = {6, 10}
 *	     elements marked in 'v' in _buf are compared to elements in _ldbuf
 *		0 1 2 3 4 5 | 6 7 8 9
 *             0	    | v v v v
 *	       1	    | v v v v
 *	       2	    | v v v v
 *	       3	    | v v v v
 *	        ---------------------
 *	       4 v v v v v v  v v v v	    
 *	       5 v v v v v v  v v v v
 *
 **************************************************************************************
 */
static herr_t
verify_elmts_two(int type, hsize_t *ext_dims, hsize_t *prev_dims,  void *_ldbuf, void *_buf)
{
    int k, m;		/* Local index variable */

    k = 0;							
    for(m = 0; m < (int)ext_dims[0]; m++) {
        int n, ind;	/* Local index variable */

	ind = m * (int)ext_dims[1];	
	if(m < (int)prev_dims[0]) {
	    for(n = (int)prev_dims[1]; n < (int)ext_dims[1]; n++) {
		VERIFY_ELMTS(type, k, ind, _ldbuf, _buf)
		++k;
	    }  /* end for */
	} /* end if */
        else {					
	    for(n = 0; n < (int)ext_dims[1]; n++) {
		VERIFY_ELMTS(type, k, ind, _ldbuf, _buf)
		++k;
	    } /* end for */
	} /* end else */
    } /* end for */

    return(0);

error:
    return(-1);
} /* verify_elmts_two() */


/* 
 **************************************************************************************
 * Testing for the High Level public routine: H5LDget_dset_elmts()
 * 	Verify elements retrieved via H5LDget_dset_elmts() are correct as expected when
 *	the datset's dimension sizes are changed accordingly to two_tests[]:
 *
 *	two-dimensional dataset: DSET_TWO with NULL fields
 *				 DSET_CMPD_TWO with fields: NULL, VALID_FIELDS1, VALID_FIELDS2
 *
 *			dims[0]		dims[1]
 *			-------		-------
 *	case #1: 	increase	increase
 *      case #2:	increase	decrease
 *      case #3:	increase	same
 *      case #4:	decrease	increase
 *      case #5:	decrease	decrease	(failure)
 *      case #6:	decrease	same		(failure)
 *      case #7:	same		increase
 *      case #8:	same		decrease	(failure)
 *      case #9:	same		same		(failure)
 *		
 **************************************************************************************
 */
static herr_t
test_LD_elmts_two(const char *file, const char *dname, const char *fields)
{
    hid_t fid=-1;		/* file identifier */
    hid_t did=-1;		/* dataset identifier */
    hid_t dtype=-1;		/* dataset's data type */
    hsize_t ext_dims[2];	/* extended dimension sizes of the dataset */
    hsize_t prev_dims[2];	/* previous dimension sizes of the dataset */
    int i;			/* local index variable */
    int ret = 0;		/* return value */

    TESTING("H5LDget_dset_elmts: two-dimensional dataset");

    /* Copy the test file */
    if(h5_make_local_copy(file, COPY_FILENAME) < 0)
        TEST_ERROR

    for(i = 0; i < TEST_BUF_SIZE; i++) {
	cbuf[i].field1 = i;
	cbuf[i].field2.a = i;
	cbuf[i].field2.b.a = i;
	cbuf[i].field2.b.b = i;
	cbuf[i].field2.b.c = i;
	cbuf[i].field2.c = i;
	cbuf[i].field3 = (double)i;
	cbuf[i].field4.a = i;
	cbuf[i].field4.b = i;
	ibuf[i] = i;
    } /* end for */

    /* Open the copied file */
    if((fid = H5Fopen(COPY_FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Open the dataset */
    if((did = H5Dopen2(fid, dname, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Get the dataset's data type */
    if((dtype = H5Tget_native_type(H5Dget_type(did), H5T_DIR_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Get current dimension sizes before extending the dataset's dimension sizes */
    if(H5LDget_dset_dims(did, prev_dims) < 0)
	FAIL_STACK_ERROR

    /* Loop through different variations of extending the dataset */
    for(i = 0; i < TWO_NTESTS; i++) {
	HDmemset(vbuf1, 0, TEST_BUF_SIZE * sizeof(test_valid_fields1));
	HDmemset(vbuf2, 0, TEST_BUF_SIZE * sizeof(test_valid_fields2));
	HDmemset(ccbuf, 0, TEST_BUF_SIZE * sizeof(set_t));
	HDmemset(iibuf, 0, TEST_BUF_SIZE * sizeof(int));

	ext_dims[0] = (hsize_t)((int)prev_dims[0] + two_tests[i][0]);
	ext_dims[1] = (hsize_t)((int)prev_dims[1] + two_tests[i][1]);

	/* Change the dimension sizes of the dataset */
	if(H5Dset_extent(did, ext_dims) < 0)
	    FAIL_STACK_ERROR

	/* Initialize data */
	if(!HDstrcmp(dname, DSET_CMPD_TWO)) {
	    if(H5Dwrite(did, dtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, cbuf) < 0)
		FAIL_STACK_ERROR
	} /* end if */
        else if(!HDstrcmp(dname, DSET_TWO)) {
	    if(H5Dwrite(did, dtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, ibuf) < 0)
		FAIL_STACK_ERROR
	} /* end else-if */
        else
            TEST_ERROR

	/* There are changes in dimension sizes */
	if(two_tests[i][0] > 0 || two_tests[i][1] > 0) {
	    if(!HDstrcmp(dname, DSET_CMPD_TWO)) {
		if(fields) {
                     if(!HDstrcmp(fields, VALID_FIELDS1) || !HDstrcmp(fields, VALID_ESC_FIELDS1)) {
                        /* Retrieve the elmemts in BUF */
                        if(H5LDget_dset_elmts(did, prev_dims, ext_dims, fields, vbuf1) < 0)
                            TEST_ERROR
                        if(verify_elmts_two(TWO_CMPD_VALID1, ext_dims, prev_dims, vbuf1, cbuf) < 0)
                            TEST_ERROR
                    } /* end if */
                    else if(!HDstrcmp(fields, VALID_FIELDS2) || !HDstrcmp(fields, VALID_ESC_FIELDS2)) {
                        /* Retrieve the elmemts in BUF */
                        if(H5LDget_dset_elmts(did, prev_dims, ext_dims, fields, vbuf2) < 0)
                            TEST_ERROR
                        if(verify_elmts_two(TWO_CMPD_VALID2, ext_dims, prev_dims, vbuf2, cbuf) < 0)
                            TEST_ERROR
                    } /* end else-if */
                    else
                        TEST_ERROR
		} /* end if */
                else {
		    /* Retrieve the elmemts in BUF */
		    if(H5LDget_dset_elmts(did, prev_dims, ext_dims, fields, ccbuf) < 0)
			TEST_ERROR
		    if(verify_elmts_two(TWO_CMPD_NULL, ext_dims, prev_dims, ccbuf, cbuf) < 0)
			TEST_ERROR
		} /* end else */
	    } /* end if */
            else { /* DSET_TWO */
		/* Retrieve the elmemts in BUF */
		if(H5LDget_dset_elmts(did, prev_dims, ext_dims, fields, iibuf) < 0)
		    TEST_ERROR
    		if(verify_elmts_two(TWO_NONE, ext_dims, prev_dims, iibuf, ibuf) < 0)
		    TEST_ERROR
	    } /* end else */
	} /* end if */
        else {
	    /* Verify failure when changes between prev_dims and ext_dims are same/decrease */
	    ret = H5LDget_dset_elmts(did, prev_dims, ext_dims, fields, iibuf);
	    VERIFY_EQUAL(ret, FAIL)
	} /* end else */
    } /* end for */

    /* Closing */
    if(H5Tclose(dtype) < 0)
	FAIL_STACK_ERROR;
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    /* Remove the copied file */
    HDremove(COPY_FILENAME);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Tclose(dtype);
	H5Dclose(did);
	H5Fclose(fid);
    } H5E_END_TRY;
    return(-1);
} /* test_LD_elmts_two() */

/*
 * Tests for High Level routines: 
 *	H5LDget_dset_dims(), H5LDget_dset_elmts, H5LDget_dset_type_size()
 */
int main(void)
{
    int  nerrors = 0;

    /* Set up temporary buffers for tests: test_LD_elmts_one() & test_LD_elmts_two() */
    if(NULL == (ibuf = (int *)HDmalloc(sizeof(int) * TEST_BUF_SIZE)))
	FAIL_STACK_ERROR;
    if(NULL == (iibuf = (int *)HDmalloc(sizeof(int) * TEST_BUF_SIZE)))
	FAIL_STACK_ERROR;

    if(NULL == (cbuf = (set_t *)HDmalloc(sizeof(set_t) * TEST_BUF_SIZE)))
	FAIL_STACK_ERROR;
    if(NULL == (ccbuf = (set_t *)HDmalloc(sizeof(set_t) * TEST_BUF_SIZE)))
	FAIL_STACK_ERROR;

    if(NULL == (vbuf1 = (test_valid_fields1 *)HDmalloc(sizeof(test_valid_fields1) * TEST_BUF_SIZE)))
	FAIL_STACK_ERROR;
    if(NULL == (vbuf2 = (test_valid_fields2 *)HDmalloc(sizeof(test_valid_fields2) * TEST_BUF_SIZE)))
	FAIL_STACK_ERROR;

    /* 
     * Testing H5LDget_dset_dims() 
     */
    nerrors += test_LD_dims_params(FILE);
    nerrors += test_LD_dims(FILE);

    /* 
     * Testing H5LDget_dset_type_size() 
     */
    nerrors += test_LD_size(FILE);

    /* 
     * Testing invalid conditions for H5LDget_dset_elmts()
     */
    nerrors += test_LD_elmts_invalid(FILE);

    /* 
     * Testing H5LDget_dset_elmts(): 
     *	 1-dimensional dataset
     */
    nerrors += test_LD_elmts_one(FILE, DSET_ONE, NULL);

    /* 
     * Testing H5LDget_dset_elmts(): 
     *	 1-dimensional dataset w/ compound datatype
     */
    nerrors += test_LD_elmts_one(FILE, DSET_CMPD, NULL);
    nerrors += test_LD_elmts_one(FILE, DSET_CMPD, VALID_FIELDS1);
    nerrors += test_LD_elmts_one(FILE, DSET_CMPD, VALID_FIELDS2);

    /* 
     * Testing H5LDget_dset_elmts():
     * 	 1-dimensional dataset with compound datatype and 
     *   member names with escape/separator characters 
     */
    nerrors += test_LD_elmts_one(FILE, DSET_CMPD_ESC, NULL);
    nerrors += test_LD_elmts_one(FILE, DSET_CMPD_ESC, VALID_ESC_FIELDS1);
    nerrors += test_LD_elmts_one(FILE, DSET_CMPD_ESC, VALID_ESC_FIELDS2);

    /* 
     * Testing H5LDget_dset_elmts() for 2-dimensional datasets
     */
    nerrors += test_LD_elmts_two(FILE, DSET_TWO, NULL);
    nerrors += test_LD_elmts_two(FILE, DSET_CMPD_TWO, NULL);
    nerrors += test_LD_elmts_two(FILE, DSET_CMPD_TWO, VALID_FIELDS1);
    nerrors += test_LD_elmts_two(FILE, DSET_CMPD_TWO, VALID_FIELDS2);

    /* Free temporary buffers */
    if(ibuf)
        HDfree(ibuf);
    if(iibuf)
        HDfree(iibuf);
    if(cbuf)
        HDfree(cbuf);
    if(ccbuf)
        HDfree(ccbuf);
    if(vbuf1)
        HDfree(vbuf1);
    if(vbuf2)
        HDfree(vbuf2);

    /* check for errors */
    if(nerrors)
	goto error;

    puts("All tests for H5LD high level routines passed.");

    return(0);

error:
    return(1);
} /* main() */

