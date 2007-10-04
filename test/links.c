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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, April 10, 1998
 *
 * Purpose:	Tests hard, soft (symbolic) & external links.
 */

#include "h5test.h"

#include "H5Lprivate.h"

/*
 * This file needs to access private information from the H5G package.
 * This file also needs to access the group testing code.
 */
#define H5G_PACKAGE
#define H5G_TESTING
#include "H5Gpkg.h"		/* Groups 				*/

const char *FILENAME[] = {
    "links0",
    "links1",
    "links2",
    "links3",
    "links4a",
    "links4b",
    "links4c",
    "links4d",
    "links5",
    NULL
};

#define LINK_BUF_SIZE   1024
#define NAME_BUF_SIZE   1024
#define MAX_NAME_LEN    ((64*1024)+1024)

/* Link type IDs */
#define UD_HARD_TYPE 201
#define UD_CB_TYPE H5L_TYPE_MAX
#define UD_PLIST_TYPE 128
#define UD_CBFAIL_TYPE UD_PLIST_TYPE
#define UD_ERROR_TYPE 189
#define UD_BAD_TYPE1 H5L_TYPE_HARD
#define UD_BAD_TYPE2 (H5L_TYPE_UD_MIN - 5)
#define UD_BAD_VERS (H5L_LINK_CLASS_T_VERS + 1)

#define DEST_PROP_NAME "destination_group"
#define REREG_TARGET_NAME "rereg_target"

#define UD_CB_LINK_NAME "ud_callback_link"
#define NEW_UD_CB_LINK_NAME "ud_callback_link2"
#define UD_CB_TARGET "ud_target"
#define UD_CB_TARGET_LEN 10

#define LE_FILENAME "le_extlink1.h5"
#define BE_FILENAME "be_extlink1.h5"

#define H5L_DIM1 100
#define H5L_DIM2 100

/* Creation order macros */
#define CORDER_GROUP_NAME       "corder_group"
#define CORDER_SOFT_GROUP_NAME  "corder_soft_group"
#define CORDER_NLINKS               18
#define CORDER_ITER_STOP            3
#define CORDER_EST_ENTRY_LEN        9

/* Timestamp macros */
#define TIMESTAMP_GROUP_1       "timestamp1"
#define TIMESTAMP_GROUP_2       "timestamp2"

/* Link iteration struct */
typedef struct {
    H5_iter_order_t order;      /* Direction of iteration */
    unsigned ncalled;           /* # of times callback is entered */
    unsigned nskipped;          /* # of links skipped */
    int stop;                   /* # of iterations to stop after */
    int64_t curr;               /* Current creation order value */
    size_t max_visit;           /* Size of "visited link" flag array */
    hbool_t *visited;           /* Pointer to array of "visited link" flags */
} link_iter_info_t;


/*-------------------------------------------------------------------------
 * Function:	mklinks
 *
 * Purpose:	Build a file with assorted links.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Friday, August 14, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
mklinks(hid_t fapl, hbool_t new_format)
{
    hid_t		file, scalar, grp, d1;
    hsize_t	        size[1] = {1};
    char		filename[NAME_BUF_SIZE];

    if(new_format)
        TESTING("link creation (w/new group format)")
    else
        TESTING("link creation")

    /* Create a file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((scalar = H5Screate_simple(1, size, size)) < 0) TEST_ERROR

    /* Create a group */
    if((grp = H5Gcreate2(file, "grp1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(grp) < 0) TEST_ERROR

    /* Create a dataset */
    if((d1 = H5Dcreate(file, "d1", H5T_NATIVE_INT, scalar, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dclose(d1) < 0) TEST_ERROR

    /* Create a hard link */
    if(H5Lcreate_hard(file, "d1", H5L_SAME_LOC, "grp1/hard", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create a symbolic link */
    if(H5Lcreate_soft("/d1", file, "grp1/soft", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create a symbolic link to something that doesn't exist */
    if(H5Lcreate_soft("foobar", file, "grp1/dangle", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create a recursive symbolic link */
    if(H5Lcreate_soft("/grp1/recursive", file, "/grp1/recursive", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close */
    if(H5Sclose(scalar) < 0) TEST_ERROR
    if(H5Fclose(file) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    new_links
 *
 * Purpose:     Build a file with assorted links for different locations.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Friday, April 19, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
new_links(hid_t fapl, hbool_t new_format)
{
    hid_t		file_a, file_b=(-1);
    hid_t		grp1_a=(-1), grp1_b=(-1), grp2_a=(-1), grp2_b=(-1);
    hid_t		scalar=(-1);
    hid_t		dset1=(-1), dset2=(-1);
    char		filename[NAME_BUF_SIZE];
    hsize_t             size[1] = {1};

    if(new_format)
        TESTING("H5Lcreate functions (w/new group format)")
    else
        TESTING("H5Lcreate functions")

    /* Create two files */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((file_a = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);
    if((file_b = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    if((scalar = H5Screate_simple (1, size, size)) < 0) TEST_ERROR

    /* Create two groups in each file */
    if((grp1_a = H5Gcreate2(file_a, "grp1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((grp2_a = H5Gcreate2(file_a, "grp2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((grp1_b = H5Gcreate2(file_b, "grp1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((grp2_b = H5Gcreate2(file_b, "grp2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create datasets */
    if((dset1 = H5Dcreate(file_a, "dataset1", H5T_NATIVE_INT, scalar, H5P_DEFAULT)) < 0) TEST_ERROR
    if((dset2 = H5Dcreate(grp1_a, "dataset2", H5T_NATIVE_INT, scalar, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create links within a file.  Both of source and destination use
     * H5L_SAME_LOC.  Both hard and soft links should fail. */
    H5E_BEGIN_TRY {
        if(H5Lcreate_hard(H5L_SAME_LOC, "dataset1", H5L_SAME_LOC, "hard", H5P_DEFAULT, H5P_DEFAULT)!=FAIL) TEST_ERROR
    } H5E_END_TRY;
    H5E_BEGIN_TRY {
        if(H5Lcreate_soft("dataset1", H5L_SAME_LOC, "soft", H5P_DEFAULT, H5P_DEFAULT)!=FAIL) TEST_ERROR
    } H5E_END_TRY;

    /* Create links across files with hard link.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lcreate_hard(file_a, "dataset1", file_b, "hard", H5P_DEFAULT, H5P_DEFAULT)!=FAIL) TEST_ERROR
    } H5E_END_TRY;

    /* Create hard link to test H5L_SAME_LOC */
    if(H5Lcreate_hard(grp1_a, "dataset2", H5L_SAME_LOC, "hard1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create links to test hard links across different locations */
    if(H5Lcreate_hard(grp1_a, "dataset2", grp2_a, "hard2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close dataspace and files */
    if(H5Sclose(scalar) < 0) TEST_ERROR
    if(H5Dclose(dset1) < 0) TEST_ERROR
    if(H5Dclose(dset2) < 0) TEST_ERROR
    if(H5Gclose(grp1_a) < 0) TEST_ERROR
    if(H5Gclose(grp2_a) < 0) TEST_ERROR
    if(H5Gclose(grp1_b) < 0) TEST_ERROR
    if(H5Gclose(grp2_b) < 0) TEST_ERROR
    if(H5Fclose(file_a) < 0) TEST_ERROR
    if(H5Fclose(file_b) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Sclose(scalar);
    	H5Dclose(dset1);
    	H5Dclose(dset2);
    	H5Gclose(grp1_a);
    	H5Gclose(grp2_a);
    	H5Gclose(grp1_b);
    	H5Gclose(grp2_b);
    	H5Fclose(file_a);
    	H5Fclose(file_b);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	cklinks
 *
 * Purpose:	Open the file created in the first step and check that the
 *		links look correct.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Friday, August 14, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
cklinks(hid_t fapl, hbool_t new_format)
{
    hid_t		file;
    H5O_info_t		oinfo1, oinfo2;
    H5L_info_t		linfo2;
    char		linkval[LINK_BUF_SIZE];
    char		filename[NAME_BUF_SIZE];
    herr_t		status;

    if(new_format)
        TESTING("link queries (w/new group format)")
    else
        TESTING("link queries")

    /* Open the file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Hard link */
    if(H5Oget_info(file, "d1", &oinfo1, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Oget_info(file, "grp1/hard", &oinfo2, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5O_TYPE_DATASET != oinfo2.type) {
	H5_FAILED();
	printf("    %d: Unexpected object type should have been a dataset\n", __LINE__);
	TEST_ERROR
    } /* end if */
    if(H5F_addr_ne(oinfo1.addr, oinfo2.addr)) {
	H5_FAILED();
	puts("    Hard link test failed. Link seems not to point to the ");
	puts("    expected file location.");
	TEST_ERROR
    } /* end if */
    if(H5Lexists(file, "d1", H5P_DEFAULT) != TRUE) FAIL_STACK_ERROR
    if(H5Lexists(file, "grp1/hard", H5P_DEFAULT) != TRUE) FAIL_STACK_ERROR

    /* Symbolic link */
    if(H5Oget_info(file, "grp1/soft", &oinfo2, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5O_TYPE_DATASET != oinfo2.type) {
	H5_FAILED();
	printf("    %d: Unexpected object type should have been a dataset\n", __LINE__);
	TEST_ERROR
    } /* end if */
    if(H5F_addr_ne(oinfo1.addr, oinfo2.addr)) {
	H5_FAILED();
	puts("    Soft link test failed. Link seems not to point to the ");
	puts("    expected file location.");
	TEST_ERROR
    } /* end if */
    if(H5Lget_val(file, "grp1/soft", linkval, sizeof linkval, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(linkval, "/d1")) {
	H5_FAILED();
	puts("    Soft link test failed. Wrong link value");
	TEST_ERROR
    } /* end if */
    if(H5Lexists(file, "grp1/soft", H5P_DEFAULT) != TRUE) FAIL_STACK_ERROR

    /* Dangling link */
    H5E_BEGIN_TRY {
	status = H5Oget_info(file, "grp1/dangle", &oinfo2, H5P_DEFAULT);
    } H5E_END_TRY;
    if(status >= 0) {
	H5_FAILED();
	puts("    H5Oget_info() should have failed for a dangling link.");
	TEST_ERROR
    } /* end if */
    if(H5Lget_info(file, "grp1/dangle", &linfo2, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5L_TYPE_SOFT != linfo2.type) {
	H5_FAILED();
	printf("    %d: Unexpected object type should have been a symbolic link\n", __LINE__);
	TEST_ERROR
    } /* end if */
    if(H5Lget_val(file, "grp1/dangle", linkval, sizeof linkval, H5P_DEFAULT) < 0) {
	H5_FAILED();
	printf("    %d: Can't retrieve link value\n", __LINE__);
	TEST_ERROR
    } /* end if */
    if(HDstrcmp(linkval, "foobar")) {
	H5_FAILED();
	puts("    Dangling link test failed. Wrong link value");
	TEST_ERROR
    } /* end if */
    if(H5Lexists(file, "grp1/dangle", H5P_DEFAULT) != TRUE) FAIL_STACK_ERROR

    /* Recursive link */
    H5E_BEGIN_TRY {
	status = H5Oget_info(file, "grp1/recursive", &oinfo2, H5P_DEFAULT);
    } H5E_END_TRY;
    if(status >= 0) {
	H5_FAILED();
	puts("    H5Oget_info() should have failed for a recursive link.");
	TEST_ERROR
    } /* end if */
    if(H5Lget_info(file, "grp1/recursive", &linfo2, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5L_TYPE_SOFT != linfo2.type) {
	H5_FAILED();
	printf("    %d: Unexpected object type should have been a symbolic link\n", __LINE__);
	TEST_ERROR
    } /* end if */
    if(H5Lget_val(file, "grp1/recursive", linkval, sizeof linkval, H5P_DEFAULT) < 0) {
	H5_FAILED();
	printf("    %d: Can't retrieve link value\n", __LINE__);
	TEST_ERROR
    } /* end if */
    if(HDstrcmp(linkval, "/grp1/recursive")) {
	H5_FAILED();
	puts("   Recursive link test failed. Wrong link value");
	TEST_ERROR
    } /* end if */

    /* Non-existant link */
    if(H5Lexists(file, "foobar", H5P_DEFAULT) == TRUE) FAIL_STACK_ERROR

    /* Cleanup */
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    ck_new_links
 *
 * Purpose:     Open the file created in the first step and check that the
 *              links look correct.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Thursday, April 25, 2002
 *
 *-------------------------------------------------------------------------
 */
static int
ck_new_links(hid_t fapl, hbool_t new_format)
{
    hid_t 		file;
    H5O_info_t		oi_dset, oi_hard1, oi_hard2;
    char 		filename[NAME_BUF_SIZE];

    if(new_format)
        TESTING("new link queries (w/new group format)")
    else
        TESTING("new link queries")

    /* Open the file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Get hard link info */
    if(H5Oget_info(file, "/grp1/dataset2", &oi_dset, H5P_DEFAULT) < 0)
	TEST_ERROR
    if(H5Oget_info(file, "/grp1/hard1", &oi_hard1, H5P_DEFAULT) < 0)
	TEST_ERROR
    if(H5Oget_info(file, "/grp2/hard2", &oi_hard2, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Check hard links */
    if(H5O_TYPE_DATASET != oi_hard1.type || H5O_TYPE_DATASET != oi_hard2.type) {
	H5_FAILED();
	printf("    %d: Unexpected object type should have been a dataset\n", __LINE__);
	TEST_ERROR
    }
    if(H5F_addr_ne(oi_dset.addr, oi_hard1.addr) || H5F_addr_ne(oi_dset.addr, oi_hard2.addr)) {
	H5_FAILED();
	puts("    Hard link test failed.  Link seems not to point to the ");
	puts("    expected file location.");
	TEST_ERROR
    }

    /* Cleanup */
    if(H5Fclose(file) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    long_links
 *
 * Purpose:     Build a file with long names
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Saturday, April 16, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
long_links(hid_t fapl, hbool_t new_format)
{
    hid_t		fid = (-1);     /* File ID */
    hid_t		gid = (-1);     /* Group ID */
    hid_t		gid2 = (-1);    /* Datatype ID */
    char               *objname = NULL; /* Name of object [Long] */
    size_t              u;              /* Local index variable */
    char		filename[NAME_BUF_SIZE];

    if(new_format)
        TESTING("long names for objects & links (w/new group format)")
    else
        TESTING("long names for objects & links")

    /* Create files */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group with short name in file (used as target for hard links) */
    if((gid = H5Gcreate2(fid, "grp1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Construct very long file name */
    if((objname = HDmalloc((size_t)(MAX_NAME_LEN + 1))) == NULL) TEST_ERROR
    for(u = 0; u < MAX_NAME_LEN; u++)
        objname[u] = 'a';
    objname[MAX_NAME_LEN] = '\0';

    /* Create hard link to existing object */
    if(H5Lcreate_hard(fid, "grp1", fid, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create soft link to existing object */
    objname[0] = 'b';
    if(H5Lcreate_soft("grp1", fid, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create group with long name in existing group */
    if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close objects */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Release memory */
    HDfree(objname);

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid2);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    HDfree(objname);
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    toomany
 *
 * Purpose:     Build a file with too many symbolic links
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, August 9, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
toomany(hid_t fapl, hbool_t new_format)
{
    hid_t		fid = (-1);     /* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char                objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t             name_len;       /* Length of object name */
    char		filename[NAME_BUF_SIZE];

    if(new_format)
        TESTING("too many links (w/new group format)")
    else
        TESTING("too many links")

    /* Make certain test is valid */
    /* XXX: should probably make a "generic" test that creates the proper
     *          # of links based on this value - QAK
     */
    HDassert(H5L_NUM_LINKS == 16);

    /* Create file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group with short name in file (used as target for hard links) */
    if((gid = H5Gcreate2(fid, "final", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create chain of hard links to existing object (no limit on #) */
    if(H5Lcreate_hard(fid, "final", fid, "hard1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard1", fid, "hard2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard2", fid, "hard3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard3", fid, "hard4", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard4", fid, "hard5", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard5", fid, "hard6", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard6", fid, "hard7", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard7", fid, "hard8", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard8", fid, "hard9", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard9", fid, "hard10", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard10", fid, "hard11", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard11", fid, "hard12", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard12", fid, "hard13", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard13", fid, "hard14", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard14", fid, "hard15", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard15", fid, "hard16", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard16", fid, "hard17", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard17", fid, "hard18", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard18", fid, "hard19", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard19", fid, "hard20", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_hard(fid, "hard20", fid, "hard21", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create chain of soft links to existing object (limited) */
    if(H5Lcreate_soft("final", fid, "soft1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft1", fid, "soft2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft2", fid, "soft3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft3", fid, "soft4", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft4", fid, "soft5", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft5", fid, "soft6", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft6", fid, "soft7", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft7", fid, "soft8", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft8", fid, "soft9", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft9", fid, "soft10", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft10", fid, "soft11", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft11", fid, "soft12", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft12", fid, "soft13", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft13", fid, "soft14", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft14", fid, "soft15", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft15", fid, "soft16", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_soft("soft16", fid, "soft17", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close objects */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open file */
    if((fid=H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Open object through last hard link */
    if((gid = H5Gopen2(fid, "hard21", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/hard21")) TEST_ERROR

    /* Create object in hard-linked group */
    if((gid2 = H5Gcreate2(gid, "new_hard", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close group in hard-linked group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close hard-linked object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Open object through too deep soft link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "soft17", H5P_DEFAULT);
    } H5E_END_TRY;
    if(gid >= 0) {
	H5_FAILED();
	puts("    Should have failed for sequence of too many nested links.");
	TEST_ERROR
    } /* end if */

    /* Open object through lesser soft link */
    if((gid = H5Gopen2(fid, "soft16", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/soft16")) TEST_ERROR

    /* Create object using soft links */
    if((gid2 = H5Gcreate2(gid, "new_soft", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close groups */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end toomany() */


/*-------------------------------------------------------------------------
 * Function:    test_h5l_create
 *
 * Purpose:     Tests H5Lcreate
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Monday, January 30, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_h5l_create(hid_t fapl, hbool_t new_format)
{
    hid_t file_id=-1;
    hid_t group_id=-1;
    hid_t space_id=-1;
    hid_t dset_id=-1;
    hid_t type_id=-1;
    hid_t lcpl_id=-1;
    char filename[1024];
    hsize_t dims[2];
    int i, n, j;
    int wdata[H5L_DIM1][H5L_DIM2];
    int rdata[H5L_DIM1][H5L_DIM2];

    if(new_format)
        TESTING("H5Llink (w/new group format)")
    else
        TESTING("H5Llink")

    /* Create file */
    h5_fixname(FILENAME[3], fapl, filename, sizeof filename);

    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create and commit a datatype with no name */
    if((type_id = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if(H5Tcommit_anon(file_id, type_id, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(!H5Tcommitted(type_id)) TEST_ERROR

    /* Create the dataspace */
    dims[0] = H5L_DIM1;
    dims[1] = H5L_DIM2;
    if((space_id = H5Screate_simple(2 ,dims, NULL)) < 0) TEST_ERROR

    /* Create a dataset with no name using the committed datatype*/
    if ((dset_id = H5Dcreate_anon(file_id, type_id, space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Verify that we can write to and read from the dataset */
    /* Initialize the dataset */
    for (i = n = 0; i < H5L_DIM1; i++)
        for (j = 0; j < H5L_DIM2; j++)
          wdata[i][j] = n++;

    /* Write the data to the dataset */
    if (H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0) TEST_ERROR

    /* Read the data back */
    if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0) TEST_ERROR
    
    /* Verify the data */
    for (i = 0; i < H5L_DIM1; i++)
	for (j = 0; j < H5L_DIM2; j++)
	    if (wdata[i][j] != rdata[i][j])
                TEST_ERROR

    /* Create a group with no name*/
    if((group_id = H5Gcreate_anon(file_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Link nameless datatype into nameless group */
    if(H5Llink(group_id, "datatype", type_id, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create LCPL with intermediate group creation flag set */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0) TEST_ERROR
    if(H5Pset_create_intermediate_group(lcpl_id, TRUE) < 0) TEST_ERROR

    /* Link nameless dataset into nameless group with intermediate group */
    if(H5Llink(group_id, "inter_group/dataset", dset_id, lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close IDs for dataset and datatype */
    if(H5Dclose(dset_id) < 0) TEST_ERROR
    if(H5Tclose(type_id) < 0) TEST_ERROR

    /* Re-open datatype using new link */
    if((type_id = H5Topen2(group_id, "datatype", H5P_DEFAULT)) < 0) TEST_ERROR

    /* Link nameless group to root group and close the group ID*/
    if(H5Llink(file_id, "/group", group_id, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Open dataset through root group and verify its data */
    if((dset_id = H5Dopen(file_id, "/group/inter_group/dataset")) < 0) TEST_ERROR

    /* Read data from dataset */
    if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0) TEST_ERROR
    for (i = 0; i < H5L_DIM1; i++)
        for (j = 0; j < H5L_DIM2; j++)
            if (wdata[i][j] != rdata[i][j])
                TEST_ERROR
        
    /* Close open IDs */
    if(H5Dclose(dset_id) < 0) TEST_ERROR
    if(H5Tclose(type_id) < 0) TEST_ERROR
    if(H5Pclose(lcpl_id) < 0) TEST_ERROR
    if(H5Sclose(space_id) < 0) TEST_ERROR
    if(H5Fclose(file_id) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(group_id);
        H5Dclose(dset_id);
        H5Tclose(type_id);
        H5Pclose(lcpl_id);
        H5Sclose(space_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* end test_h5l_create() */


/*-------------------------------------------------------------------------
 * Function:    test_lcpl
 *
 * Purpose:     Tests Link Creation Property Lists
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Monday, January 30, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_lcpl(hid_t fapl, hbool_t new_format)
{
    hid_t file_id=-1;
    hid_t group_id=-1;
    hid_t space_id=-1;
    hid_t dset_id=-1;
    hid_t type_id=-1;
    hid_t lcpl_id=-1;
    H5L_info_t linfo;
    char filename[1024];
    hsize_t dims[2];

    if(new_format)
        TESTING("link creation property lists (w/new group format)")
    else
        TESTING("link creation property lists")

    /* Actually, intermediate group creation is tested elsewhere (tmisc).
     * Here we only need to test the character encoding property */

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create and link a group with the default LCPL */
    if((group_id = H5Gcreate2(file_id, "/group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Check that its character encoding is the default */
    if(H5Lget_info(file_id, "group", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5F_DEFAULT_CSET) TEST_ERROR

    /* Create and commit a datatype with the default LCPL */
    if((type_id = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if(H5Tcommit2(file_id, "/type", type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Tclose(type_id) < 0) TEST_ERROR

    /* Check that its character encoding is the default */
    if(H5Lget_info(file_id, "type", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5F_DEFAULT_CSET) TEST_ERROR

    /* Create a dataspace */
    dims[0] = H5L_DIM1;
    dims[1] = H5L_DIM2;
    if((space_id=H5Screate_simple(2 ,dims, NULL)) < 0) TEST_ERROR

    /* Create a dataset using the default LCPL */
    if((dset_id = H5Dcreate2(file_id, "/dataset", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dclose(dset_id) < 0) TEST_ERROR

    /* Check that its character encoding is the default */
    if(H5Lget_info(file_id, "dataset", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5F_DEFAULT_CSET) TEST_ERROR

    /* Create a link creation property list with the UTF-8 character encoding */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0) TEST_ERROR
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_UTF8) < 0) TEST_ERROR

    /* Create and link a group with the new LCPL */
    if((group_id = H5Gcreate2(file_id, "/group2", lcpl_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Check that its character encoding is UTF-8 */
    if(H5Lget_info(file_id, "group2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Create and commit a datatype with the new LCPL */
    if((type_id = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if(H5Tcommit2(file_id, "/type2", type_id, lcpl_id, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Tclose(type_id) < 0) TEST_ERROR

    /* Check that its character encoding is UTF-8 */
    if(H5Lget_info(file_id, "type2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Create a dataset using the new LCPL */
    if((dset_id = H5Dcreate2(file_id, "/dataset2", H5T_NATIVE_INT, space_id, lcpl_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dclose(dset_id) < 0) TEST_ERROR

    /* Check that its character encoding is UTF-8 */
    if(H5Lget_info(file_id, "dataset2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Create a new link to the dataset with a different character encoding. */
    if(H5Pclose(lcpl_id) < 0) TEST_ERROR
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0) TEST_ERROR
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_ASCII) < 0) TEST_ERROR

    if(H5Lcreate_hard(file_id, "/dataset2", file_id, "/dataset2_link", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR

    /* Check that its character encoding is ASCII */
    if(H5Lget_info(file_id, "/dataset2_link", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_ASCII) TEST_ERROR

    /* Check that the first link's encoding hasn't changed */
    if(H5Lget_info(file_id, "/dataset2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Make sure that LCPLs work properly for other API calls: */
    /* H5Lcreate_soft */
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_UTF8) < 0) TEST_ERROR
    if(H5Lcreate_soft("dataset2", file_id, "slink_to_dset2", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_info(file_id, "slink_to_dset2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* H5Lmove */
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_ASCII) < 0) TEST_ERROR
    if(H5Lmove(file_id, "slink_to_dset2", file_id, "moved_slink", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_info(file_id, "moved_slink", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_ASCII) TEST_ERROR

    /* H5Lcopy */
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_UTF8) < 0) TEST_ERROR
    if(H5Lcopy(file_id, "moved_slink", file_id, "copied_slink", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_info(file_id, "copied_slink", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* H5Lcreate_external */
    if(H5Lcreate_external("filename", "path", file_id, "extlink", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_info(file_id, "extlink", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Close open IDs */
    if(H5Pclose(lcpl_id) < 0) TEST_ERROR
    if(H5Sclose(space_id) < 0) TEST_ERROR
    if(H5Fclose(file_id) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(group_id);
        H5Dclose(dset_id);
        H5Tclose(type_id);
        H5Pclose(lcpl_id);
        H5Sclose(space_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* end test_lcpl() */


/*-------------------------------------------------------------------------
 * Function:    test_move
 *
 * Purpose:     Tests H5Lmove()
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Friday, March 30, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_move(hid_t fapl, hbool_t new_format)
{
    hid_t 	file_a, file_b=(-1);
    hid_t	grp_1=(-1), grp_2=(-1), grp_move=(-1), moved_grp=(-1);
    char 	filename[1024];

    if(new_format)
        TESTING("H5Lmove (w/new group format)")
    else
        TESTING("H5Lmove")

    /* Create two new files */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file_a=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file_b=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create groups in first file */
    if((grp_1 = H5Gcreate2(file_a, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((grp_2 = H5Gcreate2(file_a, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((grp_move = H5Gcreate2(grp_1, "group_move", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create hard and soft links. */
    if(H5Lcreate_hard(grp_1, "group_move", H5L_SAME_LOC, "hard", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR
    if(H5Lcreate_soft("/group1/group_move", grp_2, "soft", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Move a group within the file.  Both of source and destination use
     * H5L_SAME_LOC.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lmove(H5L_SAME_LOC, "group_move", H5L_SAME_LOC, "group_new_name", H5P_DEFAULT, H5P_DEFAULT)
                !=FAIL) TEST_ERROR
    } H5E_END_TRY;

    /* Move a group across files.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lmove(grp_1, "group_move", file_b, "group_new_name", H5P_DEFAULT, H5P_DEFAULT)
                !=FAIL) TEST_ERROR
    } H5E_END_TRY;

    /* Move a group across groups in the same file while renaming it. */
    if(H5Lmove(grp_1, "group_move", grp_2, "group_new_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group just moved to the new location. */
    if((moved_grp = H5Gopen2(grp_2, "group_new_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Verify that the group is no longer in the original location */
    H5E_BEGIN_TRY {
        moved_grp = H5Gopen2(grp_1, "group_move", H5P_DEFAULT);
    } H5E_END_TRY;
    if(moved_grp >= 0) {
	H5_FAILED();
	puts("    Group still in original location?");
	TEST_ERROR
    } /* end if */

    /* Use H5Lmove to rename a group without moving it. */
    if(H5Lmove(grp_2, "group_new_name", H5L_SAME_LOC, "group_newer_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group. */
    if((moved_grp = H5Gopen2(grp_2, "group_newer_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Use H5Lmove to move a group without renaming it. */
    if(H5Lmove(grp_2, "group_newer_name", grp_1, "group_newer_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group . */
    if((moved_grp = H5Gopen2(grp_1, "group_newer_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Move the group while giving long paths. */
    if(H5Lmove(file_a, "/group1/group_newer_name", grp_2, "/group2/group_newest_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group just moved to the new location. */
    if((moved_grp = H5Gopen2(grp_2, "group_newest_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Verify that the group is in no previous locations */
    H5E_BEGIN_TRY {
        if((moved_grp = H5Gopen2(grp_1, "group_newer_name", H5P_DEFAULT)) >= 0)
            FAIL_STACK_ERROR
        if((moved_grp = H5Gopen2(grp_2, "group_newer_name", H5P_DEFAULT)) >= 0)
            FAIL_STACK_ERROR
        if((moved_grp = H5Gopen2(grp_2, "group_new_name", H5P_DEFAULT)) >= 0)
            FAIL_STACK_ERROR
        if((moved_grp = H5Gopen2(grp_1, "group_copy", H5P_DEFAULT)) >= 0)
            FAIL_STACK_ERROR
    } H5E_END_TRY;

    H5Gclose(grp_1);
    H5Gclose(grp_2);
    H5Gclose(grp_move);
    H5Fclose(file_a);
    H5Fclose(file_b);

    PASSED();
    return 0;

  error:
    H5_FAILED();
    H5E_BEGIN_TRY {
 	H5Gclose(grp_1);
	H5Gclose(grp_2);
	H5Gclose(grp_move);
        H5Gclose(moved_grp);
	H5Fclose(file_a);
	H5Fclose(file_b);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    test_copy
 *
 * Purpose:     Tests H5Lcopy()
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Friday, March 30, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy(hid_t fapl, hbool_t new_format)
{
    hid_t 	file_a, file_b=(-1);
    hid_t	grp_1=(-1), grp_2=(-1), grp_move=(-1), moved_grp=(-1);
    char 	filename[1024];

    if(new_format)
        TESTING("H5Lcopy (w/new group format)")
    else
        TESTING("H5Lcopy")

    /* Create two new files */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file_a=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file_b=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create groups in first file */
    if((grp_1 = H5Gcreate2(file_a, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((grp_2 = H5Gcreate2(file_a, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if((grp_move = H5Gcreate2(grp_1, "group_copy", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create hard and soft links. */
    if(H5Lcreate_hard(grp_1, "group_copy", H5L_SAME_LOC, "hard", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR
    if(H5Lcreate_soft("/group1/group_copy", grp_2, "soft", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Copy a group within the file.  Both of source and destination use
     * H5L_SAME_LOC.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lcopy(H5L_SAME_LOC, "group_copy", H5L_SAME_LOC, "group_new_name", H5P_DEFAULT, H5P_DEFAULT)
                !=FAIL) TEST_ERROR
    } H5E_END_TRY;

    /* Copy a group across files.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lcopy(grp_1, "group_copy", file_b, "group_new_name", H5P_DEFAULT, H5P_DEFAULT)
                !=FAIL) TEST_ERROR
    } H5E_END_TRY;

    /* Move a group across groups in the same file while renaming it. */
    if(H5Lcopy(grp_1, "group_copy", grp_2, "group_new_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group just moved to the new location. */
    if((moved_grp = H5Gopen2(grp_2, "group_new_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Verify that the group is also in the original location */
    if((moved_grp = H5Gopen2(grp_1, "group_copy", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Use H5Lcopy to create a group in the same location with a different name. */
    if(H5Lcopy(grp_2, "group_new_name", H5L_SAME_LOC, "group_newer_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group. */
    if((moved_grp = H5Gopen2(grp_2, "group_newer_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR
    /* Verify that the group is also in the original location */
    if((moved_grp = H5Gopen2(grp_2, "group_new_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Use H5Lcopy to copy to a different location with the same name. */
    if(H5Lcopy(grp_2, "group_newer_name", grp_1, "group_newer_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group . */
    if((moved_grp = H5Gopen2(grp_1, "group_newer_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR
    /* Verify that the group is still in the previous location */
    if((moved_grp = H5Gopen2(grp_2, "group_new_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Copy the group while giving long paths. */
    if(H5Lcopy(file_a, "/group1/group_newer_name", grp_2, "/group2/group_newest_name", H5P_DEFAULT, H5P_DEFAULT) < 0)
	TEST_ERROR

    /* Open the group just moved to the new location. */
    if((moved_grp = H5Gopen2(grp_2, "group_newest_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    /* Verify that the group is still in all previous original locations */
    if((moved_grp = H5Gopen2(grp_1, "group_newer_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR
    if((moved_grp = H5Gopen2(grp_2, "group_newer_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR
    if((moved_grp = H5Gopen2(grp_2, "group_new_name", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR
    if((moved_grp = H5Gopen2(grp_1, "group_copy", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR
    if(H5Gclose(moved_grp) < 0)
        TEST_ERROR

    H5Gclose(grp_1);
    H5Gclose(grp_2);
    H5Gclose(grp_move);
    H5Fclose(file_a);
    H5Fclose(file_b);

    PASSED();
    return 0;

  error:
    H5_FAILED();
    H5E_BEGIN_TRY {
 	H5Gclose(grp_1);
	H5Gclose(grp_2);
	H5Gclose(grp_move);
        H5Gclose(moved_grp);
	H5Fclose(file_a);
	H5Fclose(file_b);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    test_move_preserves
 *
 * Purpose:     Tests that moving and renaming links preserves their
 *              properties.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Monday, January 30, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_move_preserves(hid_t fapl_id, hbool_t new_format)
{
    hid_t file_id=-1;
    hid_t group_id=-1;
    hid_t fcpl_id=-1;           /* Group creation property list ID */
    hid_t lcpl_id=-1;
    hid_t lcpl2_id=-1;
    H5O_info_t oinfo;
    H5L_info_t linfo;
    H5T_cset_t old_cset;
    int64_t old_corder;         /* Creation order value of link */
    time_t old_modification_time;
    time_t curr_time;
    unsigned crt_order_flags;   /* Status of creation order info for GCPL */
    char filename[1024];

    if(new_format)
        TESTING("moving and copying links preserves their properties (w/new group format)")
    else
        TESTING("moving and copying links preserves their properties")

    /* Create a file creation property list with creation order stored for links
     * in the root group
     */
    if((fcpl_id = H5Pcreate(H5P_FILE_CREATE)) < 0) TEST_ERROR
    if(H5Pget_link_creation_order(fcpl_id, &crt_order_flags) < 0) TEST_ERROR
    if(crt_order_flags != 0) TEST_ERROR
    if(H5Pset_link_creation_order(fcpl_id, H5P_CRT_ORDER_TRACKED) < 0) TEST_ERROR
    if(H5Pget_link_creation_order(fcpl_id, &crt_order_flags) < 0) TEST_ERROR
    if(crt_order_flags != H5P_CRT_ORDER_TRACKED) TEST_ERROR

    /* Create file */
    /* (with creation order tracking for the root group) */
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl_id, fapl_id)) < 0) TEST_ERROR

    /* Create a link creation property list with the UTF-8 character encoding */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0) TEST_ERROR
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_UTF8) < 0) TEST_ERROR

    /* Create a group with that lcpl */
    if((group_id = H5Gcreate2(file_id, "group", lcpl_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Get the group's link's information */
    if(H5Lget_info(file_id, "group", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info(file_id, "group", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    old_cset = linfo.cset;
    if(old_cset != H5T_CSET_UTF8) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    old_corder = linfo.corder;
    if(old_corder != 0) TEST_ERROR
    old_modification_time = oinfo.mtime;

    /* If this test happens too quickly, the times will all be the same.  Make sure the time changes. */
    curr_time = HDtime(NULL);
    while(HDtime(NULL) <= curr_time)
        ;

    /* Close the file and reopen it */
    if(H5Fclose(file_id) < 0) TEST_ERROR
    if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) < 0) TEST_ERROR

    /* Get the link's character set & modification time .  They should be unchanged */
    if(H5Lget_info(file_id, "group", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info(file_id, "group", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_modification_time != oinfo.mtime) TEST_ERROR
    if(old_cset != linfo.cset) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    if(old_corder != linfo.corder) TEST_ERROR

    /* Create a new link to the group.  It should have a different creation order value but the same modification time */
    if(H5Lcreate_hard(file_id, "group", file_id, "group2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info(file_id, "group2", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_modification_time != oinfo.mtime) TEST_ERROR
    if(H5Lget_info(file_id, "group2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_corder == linfo.corder) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    if(linfo.corder != 1) TEST_ERROR
    if(linfo.cset != H5T_CSET_ASCII) TEST_ERROR

    /* Copy the first link to a UTF-8 name.
     *  Its creation order value should be different, but modification time
     * should not change.
     */
    if(H5Lcopy(file_id, "group", file_id, "group_copied", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info(file_id, "group_copied", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_modification_time != oinfo.mtime) TEST_ERROR
    if(H5Lget_info(file_id, "group_copied", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    if(linfo.corder != 2) TEST_ERROR

    /* Check that its character encoding is UTF-8 */
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Move the link with the default property list. */
    if(H5Lmove(file_id, "group_copied", file_id, "group_copied2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info(file_id, "group_copied2", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_modification_time != oinfo.mtime) TEST_ERROR
    if(H5Lget_info(file_id, "group_copied2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    if(linfo.corder != 3) TEST_ERROR

    /* Check that its character encoding is not UTF-8 */
    if(linfo.cset == H5T_CSET_UTF8) TEST_ERROR

    /* Check that the original link is unchanged */
    if(H5Oget_info(file_id, "group", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_modification_time != oinfo.mtime) TEST_ERROR
    if(H5Lget_info(file_id, "group", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    if(old_corder != linfo.corder) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Move the first link to a UTF-8 name.
     *  Its creation order value will change, but modification time should not
     *  change. */
    if(H5Lmove(file_id, "group", file_id, "group_moved", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info(file_id, "group_moved", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_modification_time != oinfo.mtime) TEST_ERROR
    if(H5Lget_info(file_id, "group_moved", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    if(linfo.corder != 4) TEST_ERROR

    /* Check that its character encoding is UTF-8 */
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Move the link again using the default property list. */
    if(H5Lmove(file_id, "group_moved", file_id, "group_moved_again", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info(file_id, "group_moved_again", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(old_modification_time != oinfo.mtime) TEST_ERROR
    if(H5Lget_info(file_id, "group_moved_again", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder_valid != TRUE) TEST_ERROR
    if(linfo.corder != 5) TEST_ERROR

    /* Check that its character encoding is not UTF-8 */
    if(linfo.cset == H5T_CSET_UTF8) TEST_ERROR

    /* Close open IDs */
    if(H5Pclose(fcpl_id) < 0) TEST_ERROR
    if(H5Pclose(lcpl_id) < 0) TEST_ERROR
    if(H5Fclose(file_id) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(fcpl_id);
	H5Pclose(lcpl_id);
	H5Pclose(lcpl2_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* end test_move_preserves() */


/*-------------------------------------------------------------------------
 * Function:    test_compat
 *
 * Purpose:     Tests deprecated functions for backward compatibility.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  James Laird
 *              Wednesday, April 26 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS
static int
test_compat(hid_t fapl, hbool_t new_format)
{
    hid_t file_id = -1;
    hid_t group1_id = -1;
    hid_t group2_id = -1;
    H5G_stat_t	sb_hard1, sb_hard2, sb_soft1;
    H5G_obj_t obj_type;         /* Object type */
    hsize_t num_objs;           /* Number of objects in a group */
    char filename[1024];
    char tmpstr[1024];

    if(new_format)
        TESTING("backwards compatibility (w/new group format)")
    else
        TESTING("backwards compatibility")

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create two groups in the file */
    if((group1_id = H5Gcreate2(file_id, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gcreate2(file_id, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Test H5Gset and get comment */
    if(H5Gset_comment(file_id, "group1", "comment") < 0) FAIL_STACK_ERROR
    if(H5Gget_comment(file_id, "group1", sizeof(tmpstr), tmpstr) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(tmpstr, "comment")) TEST_ERROR

    /* Create links using H5Glink and H5Glink2 */
    if(H5Glink(file_id, H5G_LINK_HARD, "group2", "group1/link_to_group2") < 0) FAIL_STACK_ERROR
    if(H5Glink2(file_id, "group1", H5G_LINK_HARD, group2_id, "link_to_group1") < 0) FAIL_STACK_ERROR
    if(H5Glink2(file_id, "link_to_group1", H5G_LINK_SOFT, H5G_SAME_LOC, "group2/soft_link_to_group1") < 0) FAIL_STACK_ERROR

    /* Test getting the names for objects */
    if(H5Gget_objname_by_idx(group1_id, (hsize_t)0, tmpstr, sizeof(tmpstr)) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(tmpstr, "link_to_group2")) TEST_ERROR
    H5E_BEGIN_TRY {
        if(H5Gget_objname_by_idx(group1_id, (hsize_t)1, tmpstr, sizeof(tmpstr)) >= 0) TEST_ERROR
    } H5E_END_TRY;

    /* Test getting the type for objects */
    if((obj_type = H5Gget_objtype_by_idx(group1_id, (hsize_t)0)) < 0) FAIL_STACK_ERROR
    if(obj_type != H5G_GROUP) TEST_ERROR
    H5E_BEGIN_TRY {
        if(H5Gget_objtype_by_idx(group1_id, (hsize_t)1) >= 0) TEST_ERROR
    } H5E_END_TRY;

    /* Test getting the number of objects in a group */
    if(H5Gget_num_objs(file_id, &num_objs) < 0) FAIL_STACK_ERROR
    if(num_objs != 2) TEST_ERROR
    if(H5Gget_num_objs(group1_id, &num_objs) < 0) FAIL_STACK_ERROR
    if(num_objs != 1) TEST_ERROR

    /* Test that H5Glink created hard links properly */
    if(H5Gget_objinfo(file_id, "/group2", TRUE, &sb_hard1) < 0) FAIL_STACK_ERROR
    if(H5Gget_objinfo(file_id, "/group1/link_to_group2", TRUE, &sb_hard2) < 0) FAIL_STACK_ERROR

    if(HDmemcmp(&sb_hard1.objno, sb_hard2.objno, sizeof(sb_hard1.objno))) {
        H5_FAILED();
        puts("    Hard link test failed.  Link seems not to point to the ");
        puts("    expected file location.");
        TEST_ERROR
    } /* end if */

    /* Test for the other hard link created */
    if(H5Gget_objinfo(file_id, "/group1", TRUE, &sb_hard1) < 0) FAIL_STACK_ERROR
    if(H5Gget_objinfo(file_id, "/group2/link_to_group1", TRUE, &sb_hard2) < 0) FAIL_STACK_ERROR

    if(HDmemcmp(&sb_hard1.objno, sb_hard2.objno, sizeof(sb_hard1.objno))) {
        H5_FAILED();
        puts("    Hard link test failed.  Link seems not to point to the ");
        puts("    expected file location.");
        TEST_ERROR
    } /* end if */

    /* Test the soft link */
    if(H5Gget_objinfo(file_id, "/group2/soft_link_to_group1", FALSE, &sb_soft1) < 0) FAIL_STACK_ERROR
    if(sb_soft1.type != H5G_LINK) TEST_ERROR
    if(sb_soft1.linklen != HDstrlen("link_to_group1") + 1) TEST_ERROR

    if(H5Gget_linkval(group2_id, "soft_link_to_group1", sb_soft1.linklen, tmpstr) < 0) FAIL_STACK_ERROR
    if(HDstrcmp("link_to_group1", tmpstr)) TEST_ERROR


    /* Test H5Gmove and H5Gmove2 */
    if(H5Gmove(file_id, "group1", "moved_group1") < 0) FAIL_STACK_ERROR
    if(H5Gmove2(file_id, "group2", group1_id, "moved_group2") < 0) FAIL_STACK_ERROR

    /* Ensure that both groups can be opened */
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group1_id) < 0) FAIL_STACK_ERROR

    if((group1_id = H5Gopen2(file_id, "moved_group1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group2_id = H5Gopen2(file_id, "moved_group1/moved_group2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close open IDs */
    if(H5Gclose(group2_id) < 0) FAIL_STACK_ERROR
    if(H5Gclose(group1_id) < 0) FAIL_STACK_ERROR

    /* Test H5Gunlink */
    if(H5Gunlink(file_id, "moved_group1/moved_group2") < 0) FAIL_STACK_ERROR

    H5E_BEGIN_TRY {
        if(H5Gopen2(file_id, "moved_group1/moved_group2", H5P_DEFAULT) >=0) TEST_ERROR
    } H5E_END_TRY;

    if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(group2_id);
        H5Gclose(group1_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* end test_compat() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */


/*-------------------------------------------------------------------------
 * Function:    external_link_root
 *
 * Purpose:     Build a file with external link to root group in external file
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, May 25, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_root(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    H5L_info_t	linfo;                          /* Link information */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE];
    char	filename2[NAME_BUF_SIZE];
    const char  *file;				/* File from external link */
    const char  *path;				/* Path from external link */

    if(new_format)
        TESTING("external link to root (w/new group format)")
    else
        TESTING("external link to root")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create file to point to */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Check that external links are registered with the library */
    if(H5Lis_registered(H5L_TYPE_EXTERNAL) != TRUE) TEST_ERROR

    /* Create file with link to first file */
    if((fid = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to object in first file */
    if(H5Lcreate_external(filename1, "/", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Check information for external link */
    if(H5Lget_info(fid, "ext_link", &linfo, H5P_DEFAULT) < 0) goto error;
    if(H5L_TYPE_EXTERNAL != linfo.type) {
	H5_FAILED();
	puts("    Unexpected object type - should have been an external link");
	goto error;
    }
    if(H5Lget_val(fid, "ext_link", objname, sizeof(objname), H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lunpack_elink_val(objname, linfo.u.val_size, NULL, &file, &path) < 0) TEST_ERROR
    if(HDstrcmp(file, filename1)) {
	H5_FAILED();
	puts("    External link file name incorrect");
	goto error;
    }
    if(HDstrcmp(path, "/")) {
	H5_FAILED();
	puts("    External link path incorrect");
	goto error;
    }

    /* Close and re-open file to ensure that data is written to disk */
    if(H5Fclose(fid) < 0) TEST_ERROR
    if((fid = H5Fopen(filename2, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) TEST_ERROR


    /* Open object through external link */
    if((gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object (lets first file close) */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Create a new object using H5Gcreate2 through the external link 
     * directly
     */
    if((gid = H5Gcreate2(fid, "ext_link/newer_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close file and group */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR
    
    /* Open first file again with read-only access and check on objects created */
    if((fid = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Open objects created through external link */
    if((gid = H5Gopen2(fid, "new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((gid2 = H5Gopen2(fid, "newer_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check names */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/new_group")) TEST_ERROR
    if((name_len = H5Iget_name( gid2, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/newer_group")) TEST_ERROR

    /* Close opened objects */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Verify that new objects can't be created through a read-only external
     * link.
     */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR

    H5E_BEGIN_TRY {
        gid = H5Gcreate2(fid, "ext_link/readonly_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY
    if(gid >= 0) TEST_ERROR

    /* Close second file again */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid2);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_root() */


/*-------------------------------------------------------------------------
 * Function:    external_link_path
 *
 * Purpose:     Build a file with external link to object down a path in the
 *              external file
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, July 26, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_path(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE];
    char	filename2[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external link to object on path (w/new group format)")
    else
        TESTING("external link to object on path")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create file to point to */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object down a path */
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate2(fid, "A/B", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate2(fid, "A/B/C", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create file with link to first file */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to object in first file */
    if(H5Lcreate_external(filename1, "/A/B/C", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/A/B/C")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object (lets first file close) */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close second file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open first file again and check on object created */
    if((fid = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Open object created through external link */
    if((gid = H5Gopen2(fid, "/A/B/C/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/A/B/C/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid2);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_path() */


/*-------------------------------------------------------------------------
 * Function:    external_link_mult
 *
 * Purpose:     Build a file with external link to object that crossed several
 *              external file links
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, July 26, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_mult(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1), fid2 = (-1); 	/* File IDs */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE],
    		filename4[NAME_BUF_SIZE];       /* Names of files to externally link across */

    if(new_format)
        TESTING("external links across multiple files (w/new group format)")
    else
        TESTING("external links across multiple files")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);
    h5_fixname(FILENAME[5], fapl, filename3, sizeof filename3);
    h5_fixname(FILENAME[6], fapl, filename4, sizeof filename4);

    /* Create first file to point to */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object down a path */
    if((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate2(fid, "A/B", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate2(fid, "A/B/C", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file to point to */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link down a path */
    if((gid = H5Gcreate2(fid, "D", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate2(fid, "D/E", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create external link to object in first file */
    if(H5Lcreate_external(filename1, "/A/B/C", gid, "F", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create third file to point to */
    if((fid=H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link down a path */
    if((gid = H5Gcreate2(fid, "G", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate2(fid, "G/H", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create external link to object in second file */
    if(H5Lcreate_external(filename2, "/D/E/F", gid, "I", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Create file with link to third file */
    if((fid=H5Fcreate(filename4, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link to object in first file */
    if(H5Lcreate_external(filename3, "/G/H/I", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/A/B/C")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object (lets first file close) */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close second file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open first file again and check on object created */
    if((fid = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Open object created through external link */
    if((gid = H5Gopen2(fid, "/A/B/C/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/A/B/C/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open an object through external links */
    if((fid = H5Fopen(filename4, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR
    if((gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* The intermediate files should not stay open. Replace one of them with a new file. */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if(H5Fclose(fid2) < 0) TEST_ERROR

    /* Open the other with write access and delete the external link in it */  
    if((fid2 = H5Fopen(filename3, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR
    if(H5Ldelete(fid2, "G/H/I", H5P_DEFAULT) < 0) TEST_ERROR
      
    if(H5Fclose(fid2) < 0) TEST_ERROR

    /* Cleanup */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid2);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_mult() */


/*-------------------------------------------------------------------------
 * Function:    external_link_self
 *
 * Purpose:     Build a file with external link to itself
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Wednesday, July 12, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_self(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    hid_t	lcpl_id = (-1);     		/* Link Creation Property List ID */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE];
    char	filename2[NAME_BUF_SIZE];
    char	filename3[NAME_BUF_SIZE];

    if(new_format)
        TESTING("external link to self (w/new group format)")
    else
        TESTING("external link to self")

    /* Set up filename */
    h5_fixname(FILENAME[1], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[2], fapl, filename2, sizeof filename1);
    h5_fixname(FILENAME[3], fapl, filename3, sizeof filename1);

    /* Create file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create an lcpl with intermediate group creation set */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0) TEST_ERROR
    if(H5Pset_create_intermediate_group(lcpl_id, TRUE) < 0) TEST_ERROR

    /* Create a series of groups within the file: /A/B and /X/Y/Z */
    if((gid = H5Gcreate2(fid, "A/B", lcpl_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "X/Y", lcpl_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if(H5Pclose (lcpl_id) < 0) TEST_ERROR

    /* Create external link to own root group*/
    if(H5Lcreate_external(filename1, "/X", fid, "A/B/C", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "A/B/C/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/X")) TEST_ERROR

    /* Create object through external link */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close created group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close object opened through external link */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Check on object created */
    if((gid = H5Gopen2(fid, "X/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/X/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Complicate things. Use this file as an intermediate file in a chain
     * of external links that will go: file2 -> file1 -> file1 -> file3
     */

    /* Create file2 with an external link to file1  */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    if(H5Lcreate_external(filename1, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file2 */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create file3 as a target */
    if((fid=H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "end", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open file1 and create an extlink pointing to file3 */
    if((fid=H5Fopen(filename1, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) TEST_ERROR

    if(H5Lcreate_external(filename3, "/", fid, "/X/Y/Z", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file1 */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Re-open file2 and traverse through file1 (with its recursive extlink) to file3 */
    if((fid=H5Fopen(filename2, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) TEST_ERROR

    if((gid = H5Gopen2(fid, "ext_link/B/C/Y/Z/end", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    
    /* Create object through external link */
    if((gid2 = H5Gcreate2(gid, "newer_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Cleanup */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open up file3 and make sure the object was created successfully */
    if((fid = H5Fopen(filename3, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    if((gid = H5Gopen2(fid, "end/newer_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Cleanup */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR
    
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Fclose (gid2);
    	H5Fclose (gid);
    	H5Pclose (lcpl_id);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_self() */


/*-------------------------------------------------------------------------
 * Function:    external_link_pingpong
 *
 * Purpose:     Build a file with external link to object that goes back and
 *              force between two files a couple of times:
 *
 *                      file1:/link1    -> file2: /link2
 *                      file2:/link2    -> file1: /link3
 *                      file1:/link3    -> file2: /link4
 *                      file2:/link4    -> file1: /link5
 *                      file1:/link5    -> file2: /link6
 *                      file2:/link6    -> file1: /final
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, July 26, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_pingpong(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */

    if(new_format)
        TESTING("external links back and forth (w/new group format)")
    else
        TESTING("external links back and forth")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external links for chain */
    if(H5Lcreate_external(filename2, "/link2", fid, "link1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link4", fid, "link3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link6", fid, "link5", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create final object */
    if((gid = H5Gcreate2(fid, "final", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external links for chain */
    if(H5Lcreate_external(filename1, "/link3", fid, "link2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link5", fid, "link4", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/final", fid, "link6", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open first file */
    if((fid=H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "link1", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/final")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object (lets first file close) */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open first file again and check on object created */
    if((fid = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Open object created through external link */
    if((gid = H5Gopen2(fid, "/final/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/final/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid2);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_pingpong() */


/*-------------------------------------------------------------------------
 * Function:    external_link_toomany
 *
 * Purpose:     Build a file with too many external links to objects (i.e.
 *              more than H5L_NLINKS_DEF.  Use a "back & forth" style of
 *              linking (like the "ping pong" test above) to minimize the
 *              number of files involved:
 *
 *                      file1:/link1    -> file2: /link2
 *                      file2:/link2    -> file1: /link3
 *                      file1:/link3    -> file2: /link4
 *                      file2:/link4    -> file1: /link5
 *                      file1:/link5    -> file2: /link6
 *                      file2:/link6    -> file1: /link7
 *                      file1:/link7    -> file2: /link8
 *                      file2:/link8    -> file1: /link9
 *                      file1:/link9    -> file2: /link10
 *                      file2:/link10   -> file1: /link11
 *                      file1:/link11   -> file2: /link12
 *                      file2:/link12   -> file1: /link13
 *                      file1:/link13   -> file2: /link14
 *                      file2:/link14   -> file1: /link15
 *                      file1:/link15   -> file2: /link16
 *                      file2:/link16   -> file1: /link17
 *                      file1:/link17   -> file2: /final
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, August 8, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_toomany(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */

    if(new_format)
        TESTING("too many external links (w/new group format)")
    else
        TESTING("too many external links")

    /* Make certain test is valid */
    /* XXX: should probably make a "generic" test that creates the proper
     *          # of links based on this value - QAK
     */
    HDassert(H5L_NUM_LINKS == 16);

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external links for chain */
    if(H5Lcreate_external(filename2, "/link2", fid, "link1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link4", fid, "link3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link6", fid, "link5", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link8", fid, "link7", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link10", fid, "link9", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link12", fid, "link11", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link14", fid, "link13", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/link16", fid, "link15", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/final", fid, "link17", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external links for chain */
    if(H5Lcreate_external(filename1, "/link3", fid, "link2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link5", fid, "link4", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link7", fid, "link6", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link9", fid, "link8", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link11", fid, "link10", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link13", fid, "link12", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link15", fid, "link14", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/link17", fid, "link16", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create final object */
    if((gid = H5Gcreate2(fid, "final", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open first file */
    if((fid=H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "link1", H5P_DEFAULT);
    } H5E_END_TRY;
    if (gid >= 0) {
	H5_FAILED();
	printf("%d:    Should have failed for sequence of too many nested links.", __LINE__);
	goto error;
    }

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "link3", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/final")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid2);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_toomany() */


/*-------------------------------------------------------------------------
 * Function:    external_link_dangling
 *
 * Purpose:     Build a file with "dangling" external links: with both
 *              missing files and missing objects.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, August 9, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_dangling(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */

    if(new_format)
        TESTING("dangling external links (w/new group format)")
    else
        TESTING("dangling external links")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create dangling external links */
    if(H5Lcreate_external("missing", "/missing", fid, "no_file", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename2, "/missing", fid, "no_object", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file (for dangling object test) */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open first file */
    if((fid=H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Open object through dangling file external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "no_file", H5P_DEFAULT);
    } H5E_END_TRY;
    if (gid >= 0) {
	H5_FAILED();
	puts("    Should have failed for sequence of too many nested links.");
	goto error;
    }

    /* Open object through dangling object external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "no_object", H5P_DEFAULT);
    } H5E_END_TRY;
    if (gid >= 0) {
	H5_FAILED();
	puts("    Should have failed for sequence of too many nested links.");
	goto error;
    }

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_dangling() */


/*-------------------------------------------------------------------------
 * Function:    external_link_recursive
 *
 * Purpose:     Build a file with "recursive" external link
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, August 15, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_recursive(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    char	filename1[NAME_BUF_SIZE];       /* Names of files to externally link across */

    if(new_format)
        TESTING("recursive external links (w/new group format)")
    else
        TESTING("recursive external links")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);

    /* Create first file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create recursive external links */
    if(H5Lcreate_external(filename1, "/recursive", fid, "recursive", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open file */
    if((fid=H5Fopen(filename1, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open object through dangling file external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen2(fid, "recursive", H5P_DEFAULT);
    } H5E_END_TRY;
    if (gid >= 0) {
	H5_FAILED();
	puts("    Should have failed for recursive external links.");
	goto error;
    }

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_recursive() */


/*-------------------------------------------------------------------------
 * Function:    external_link_query
 *
 * Purpose:     Query file & object names for external links, as well as
 *              information from H5Gget_obj_info
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, August 15, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_query(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    const char *file_name;                      /* Name of the file the external link points to */
    const char *object_name;                    /* Name of the object the external link points to */
    H5O_info_t	oi;                             /* Object information */
    H5L_info_t  li;                             /* Link information */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],       /* Names of files to externally link across */
                query_buf[NAME_BUF_SIZE];       /* Buffer to hold query result */

    if(new_format)
        TESTING("query aspects of external link (w/new group format)")
    else
        TESTING("query aspects of external link")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file, with external link to object in second file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link */
    if(H5Lcreate_external(filename2, "/dst", fid, "src", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Get size of buffer for external link */
    if(H5Lget_info(fid, "src", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.u.val_size != (1 + (HDstrlen(filename2) + 1) + (HDstrlen("/dst") + 1))) TEST_ERROR
    if (H5L_TYPE_EXTERNAL != li.type) {
	H5_FAILED();
	puts("    Unexpected link class - should have been an external link");
	goto error;
    }

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file to point to */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object to link to */
    if((gid = H5Gcreate2(fid, "dst", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Get size of buffer for external link */
    if(H5Lget_info(fid, "src", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.u.val_size != (1 + (HDstrlen(filename2) + 1) + (HDstrlen("/dst") + 1))) TEST_ERROR
    if(H5L_TYPE_EXTERNAL != li.type) {
	H5_FAILED();
	puts("    Unexpected link class - should have been an external link");
	goto error;
    }

    /* Get information for external link.  It should be two strings right after each other */
    if(H5Lget_val(fid, "src", query_buf, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR

    /* Extract the file and object names from the buffer */
    if(H5Lunpack_elink_val(query_buf, li.u.val_size, NULL, &file_name, &object_name) < 0) TEST_ERROR

    /* Compare the file and object names */
    if(HDstrcmp(file_name, filename2)) TEST_ERROR
    if(HDstrcmp(object_name, "/dst")) TEST_ERROR

    /* Query information about object that external link points to */
    if(H5Oget_info(fid, "src", &oi, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5O_TYPE_GROUP != oi.type) {
	H5_FAILED();
	puts("    Unexpected object type - should have been a group");
	goto error;
    }

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Make sure that passing in NULLs to H5Lunpack_elink_val works */
    if(H5Lunpack_elink_val(query_buf, li.u.val_size, NULL, NULL, NULL) < 0) TEST_ERROR

    /* Make sure that bogus cases trigger errors in H5Lunpack_elink_val */
    H5E_BEGIN_TRY {
      if(H5Lunpack_elink_val(query_buf, li.u.val_size - 1, NULL, NULL, NULL) >= 0) TEST_ERROR
    } H5E_END_TRY
    H5E_BEGIN_TRY {
      if(H5Lunpack_elink_val(query_buf, (size_t)0, NULL, NULL, NULL) >= 0) TEST_ERROR
    } H5E_END_TRY
    H5E_BEGIN_TRY {
      if(H5Lunpack_elink_val(NULL, (size_t)0, NULL, NULL, NULL) >= 0) TEST_ERROR
    } H5E_END_TRY
    H5E_BEGIN_TRY {
      if(H5Lunpack_elink_val(NULL, (size_t)1000, NULL, NULL, NULL) >= 0) TEST_ERROR
    } H5E_END_TRY

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_query() */


/*-------------------------------------------------------------------------
 * Function:    external_link_unlink_compact
 *
 * Purpose:     Remove an external link (from a compact group)
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, January 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_unlink_compact(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */

    if(new_format)
        TESTING("unlinking external link in compact group (w/new group format)")
    else
        TESTING("unlinking external link in compact group")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file, with external link to object in second file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link */
    if(H5Lcreate_external(filename2, "/dst", fid, "src", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file to point to */
    if((fid = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object to link to */
    if((gid = H5Gcreate2(fid, "dst", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


/* Unlink external link */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Unlink external link */
    if(H5Ldelete(fid, "src", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Open group for external link */
    if((gid = H5Gopen2(fid, "dst", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_unlink_compact() */


/*-------------------------------------------------------------------------
 * Function:    external_link_unlink_dense
 *
 * Purpose:     Remove an external link (from a dense group)
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, January 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_unlink_dense(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t       gcpl = (-1);                    /* Group creation property list ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */
    unsigned	nmsgs;		                /* Number of messages in group's header */
    unsigned    max_compact;                    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;                      /* Minimum # of links to store in group "densely" */
    unsigned    u;                              /* Local index variable */

    if(new_format)
        TESTING("unlinking external link in dense group (w/new group format)")
    else
        TESTING("unlinking external link in dense group")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file, with external link to object in second file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Open root group */
    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR

    /* Query the group creation properties */
    if((gcpl = H5Gget_create_plist(gid)) < 0) TEST_ERROR
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Create external link */
    /* (This also covers the case of having an external link in a compact group that's converted to a dense group) */
    if(H5Lcreate_external(filename2, "/dst", gid, "src", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, &nmsgs) != TRUE) TEST_ERROR
    if(nmsgs != 1) TEST_ERROR
    if(H5G_has_stab_test(gid) == TRUE) TEST_ERROR

    /* Create enough objects in the root group to change it into a "dense" group */
    for(u = 0; u < max_compact; u++) {
        sprintf(objname, "filler %u", u);
        if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(gid2) < 0) TEST_ERROR
    } /* end for */

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(gid) != TRUE) TEST_ERROR

    /* Close group creation property list */
    if(H5Pclose(gcpl) < 0) TEST_ERROR

    /* Close root group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file to point to */
    if((fid = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object to link to */
    if((gid = H5Gcreate2(fid, "dst", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


/* Unlink external link */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Open root group */
    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Unlink external link */
    if(H5Ldelete(fid, "src", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Remove enough objects in the root group to change it into a "compact" group */
    for(u = 0; u < ((max_compact - min_dense) + 1); u++) {
        sprintf(objname, "filler %u", u);
        if(H5Ldelete(gid, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, &nmsgs) != TRUE) TEST_ERROR
    if(nmsgs != (min_dense - 1)) TEST_ERROR
    if(H5G_is_new_dense_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(gid) == TRUE) TEST_ERROR

    /* Close root group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Open group for external link (should be unaffected) */
    if((gid = H5Gopen2(fid, "dst", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_unlink_dense() */


/*-------------------------------------------------------------------------
 * Function:    external_link_move
 *
 * Purpose:     Move/rename external link
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, December  5, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_move(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */

    if(new_format)
        TESTING("move external link (w/new group format)")
    else
        TESTING("move external link")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file, with external link to object in second file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create external link */
    if(H5Lcreate_external(filename2, "/dst", fid, "src", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file to point to */
    if((fid = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object to link to */
    if((gid = H5Gcreate2(fid, "dst", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


/* Move external link to different name within same group */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Move external link within same group */
    if(H5Lmove(fid, "src", H5L_SAME_LOC, "src2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "src2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE )) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(objname, "/dst")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) FAIL_STACK_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Open group created through external link */
    if((gid = H5Gopen2(fid, "dst/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

/* Move external link to different group */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Create another group, to move the external link into */
    if((gid = H5Gcreate2(fid, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Move external link to different group */
    if(H5Lmove(fid, "src2", gid, "src3", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close new group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "/group2/src3", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE )) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(objname, "/dst")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) FAIL_STACK_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Open group created through external link */
    if((gid = H5Gopen2(fid, "dst/new_group2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

/* Move external link back to original group */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "/group2/src3", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE )) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(objname, "/dst")) TEST_ERROR

    /* Move external link back to original location */
    if(H5Lmove(fid, "/group2/src3", H5L_SAME_LOC, "/src", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE )) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(objname, "/dst")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) FAIL_STACK_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Open group created through external link */
    if((gid = H5Gopen2(fid, "dst/new_group3", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_move() */


/*-------------------------------------------------------------------------
 * Function:    external_link_ride
 *
 * Purpose:     Let an external link "come along for the ride" when a group is
 *              converted between compact & dense forms.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, January 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_ride(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t       gcpl = (-1);                    /* Group creation property list ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */
    unsigned	nmsgs;		                /* Number of messages in group's header */
    unsigned    max_compact;                    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;                      /* Minimum # of links to store in group "densely" */
    unsigned    u;                              /* Local index variable */

    if(new_format)
        TESTING("external link along for the ride (w/new group format)")
    else
        TESTING("external link along for the ride")

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file, with external link to object in second file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Open root group */
    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR

    /* Query the group creation properties */
    if((gcpl = H5Gget_create_plist(gid)) < 0) TEST_ERROR
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Create enough objects in the root group to change it into a "dense" group */
    for(u = 0; u < (max_compact + 1); u++) {
        sprintf(objname, "filler %u", u);
        if((gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(gid2) < 0) TEST_ERROR
    } /* end for */

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(new_format) {
        if(H5G_is_new_dense_test(gid) != TRUE) TEST_ERROR
    } /* end if */
    else {
        if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR
    } /* end else */

    /* Create external link */
    /* (This also covers the case of adding an external link to a dense group) */
    if(H5Lcreate_external(filename2, "/dst", gid, "src", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(gid) != TRUE) TEST_ERROR

    /* Close group creation property list */
    if(H5Pclose(gcpl) < 0) TEST_ERROR

    /* Close root group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file to point to */
    if((fid = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object to link to */
    if((gid = H5Gcreate2(fid, "dst", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

/* Remove enough objects to convert group containing external link back into compact form */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "src", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/dst")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Open root group */
    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Remove enough objects in the root group to change it into a "compact" group */
    for(u = 0; u < ((max_compact - min_dense) + 3); u++) {
        sprintf(objname, "filler %u", u);
        if(H5Ldelete(gid, objname, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    } /* end for */

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR
    if(H5G_has_links_test(gid, &nmsgs) != TRUE) TEST_ERROR
    if(nmsgs != (min_dense - 1)) TEST_ERROR
    if(H5G_has_stab_test(gid) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(gid) == TRUE) TEST_ERROR

    /* Close root group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Open object through external link */
    if((gid = H5Gopen2(fid, "src", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/dst")) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate2(gid, "new_group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) FAIL_STACK_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR

    /* Open group created through external link */
    if((gid = H5Gopen2(fid, "dst/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Open group created through external link */
    if((gid = H5Gopen2(fid, "dst/new_group2", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end external_link_ride() */


/*-------------------------------------------------------------------------
 * Function:    external_link_closing
 *
 * Purpose:     Test that files are closed correctly when traversing
 *              external links.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Wednesday, August 16, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_closing(hid_t fapl, hbool_t new_format)
{
    hid_t       fid1 = (-1), fid2 = (-1), fid3 = (-1), fid4=(-1);
    hid_t       gid=(-1), tid=(-1), tid2=(-1), sid=(-1), did=(-1);
    hid_t       lcpl_id=(-1);
    hsize_t     dims[2];
    char	filename1[NAME_BUF_SIZE],
                filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE],
    		filename4[NAME_BUF_SIZE],       /* Names of files to externally link across */
    		buf[NAME_BUF_SIZE];             /* misc. buffer */
    H5L_info_t  li;
    H5O_info_t  oi;
    hobj_ref_t  obj_ref;

    if(new_format)
        TESTING("that external files are closed during traversal (w/new group format)")
    else
        TESTING("that external files are closed during traversal")

    /* In this test, external links will go from file1 to file2 and from
     * file2 to file3.
     * Test that all functions that can traverse external files close
     * the files they open.
     * Test that providing unusual paths containing external links can't
     * make HDF5 forget to close a file it opened.
     */

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);
    h5_fixname(FILENAME[5], fapl, filename3, sizeof filename3);
    h5_fixname(FILENAME[6], fapl, filename4, sizeof filename4);

    /* Create four files */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((fid3 = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((fid4 = H5Fcreate(filename4, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create a dataspace and a datatype so we can create/commit a dataset/datatype in the files */
    dims[0] = 2;
    dims[1] = 2;
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) TEST_ERROR
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if((tid2 = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR

    /* Create external links from each file to the next */
    if(H5Lcreate_external(filename2, "/", fid1, "elink", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename3, "/", fid2, "elink", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external(filename4, "/", fid3, "elink", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close all files but the first */
    if(H5Fclose(fid4) < 0) TEST_ERROR
    if(H5Fclose(fid3) < 0) TEST_ERROR
    if(H5Fclose(fid2) < 0) TEST_ERROR

    /* Test creating each kind of object */
    if((gid = H5Gcreate2(fid1, "elink/elink/elink/group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Tcommit2(fid1, "elink/elink/elink/type1", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if((did = H5Dcreate(fid1, "elink/elink/elink/dataset1", tid2, sid, H5P_DEFAULT)) < 0) TEST_ERROR
    /* Close objects */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Tclose(tid) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    /* Test that getting info works */
    if(H5Lget_info(fid1, "elink/elink/elink/type1", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_info(fid1, "elink/elink/elink", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info(fid1, "elink/elink/elink/type1", &oi, H5P_DEFAULT) < 0) TEST_ERROR 
    if(H5Oget_info(fid1, "elink/elink/elink", &oi, H5P_DEFAULT) < 0) TEST_ERROR 

    /* Test move */
    if(H5Lmove(fid1, "elink/elink/elink/group1", fid1,
        "elink/elink/elink/group1_moved", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open file 4 so we can do some fancy things */
    if((fid4 = H5Fopen(filename4, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR
    if(H5Lmove(fid1, "elink/elink/elink/type1", fid4,
        "type1_moved", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lmove(fid4, "dataset1", fid1,
        "elink/elink/elink/dataset1_moved", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file 4 again */
    if(H5Fclose(fid4) < 0) FAIL_STACK_ERROR

    /* Test copy (as of this test, it uses the same code as move) */
    if(H5Lcopy(fid1, "elink/elink/elink", fid1,
      "elink/elink/elink_copied", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcopy(fid1, "elink/elink/elink", fid1,
      "elink/elink/elink/elink_copied2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Test H5Gset and get comment */
    if(H5Oset_comment(fid1, "elink/elink/elink/group1_moved", "comment", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Oget_comment(fid1, "elink/elink/elink/group1_moved", buf, sizeof(buf), H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(buf, "comment")) TEST_ERROR

    /* Test H5*open */
    if((gid = H5Gopen2(fid1, "elink/elink/elink/group1_moved", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((tid = H5Topen2(fid1, "elink/elink/elink/type1_moved", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((did = H5Dopen(fid1, "elink/elink/elink/dataset1_moved")) < 0) FAIL_STACK_ERROR
    /* Close objects */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR
    if(H5Tclose(tid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(did) < 0) FAIL_STACK_ERROR

    /* Test H5*open2 */
    if((gid = H5Gopen2(fid1, "elink/elink/elink/group1_moved", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((tid = H5Topen2(fid1, "elink/elink/elink/type1_moved", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((did = H5Dopen2(fid1, "elink/elink/elink/dataset1_moved", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    /* Close objects */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR
    if(H5Tclose(tid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(did) < 0) FAIL_STACK_ERROR

    /* Test H5Oopen */
    if((did = H5Oopen(fid1, "elink/elink/elink/dataset1_moved", H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    /* Test H5Fmount */
    if((gid = H5Gcreate2(fid1, "elink/elink/elink/mnt", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    H5E_BEGIN_TRY {
        if(H5Fmount(fid1, "elink/elink/elink/mnt", fid1, H5P_DEFAULT) >= 0) TEST_ERROR
        if(H5Funmount(fid1, "elink/elink/elink/mnt") >= 0) TEST_ERROR
    } H5E_END_TRY

    /* Test H5Rcreate */
    if(H5Rcreate(&obj_ref, fid1, "elink/elink/elink/type1_moved", H5R_OBJECT, (-1)) < 0) TEST_ERROR

    /* Test unlink */
    if(H5Ldelete(fid1, "elink/elink/elink/group1_moved", H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Ldelete(fid1, "elink/elink/elink/type1_moved", H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Ldelete(fid1, "elink/elink/elink/dataset1_moved", H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Ldelete(fid1, "elink/elink/elink_copied", H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Ldelete(fid1, "elink/elink/elink/elink_copied2", H5P_DEFAULT) < 0) TEST_ERROR

    /* We've tested that the various functions above don't leave files open.
     * Now test that we can't confuse HDF5 by giving unusual paths with external links
     */
    /* Create an external link that points to another external link */
    if((fid2 = H5Fopen(filename2, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR
    if(H5Lcreate_external(filename3, "/elink", fid2, "elink2",
          H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Fclose(fid2) < 0) TEST_ERROR

    /* Do an external link traversal that recursively calls another external link. */
    if((gid = H5Gcreate2(fid1, "elink/elink2/group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Create two more groups so that the last three elements in the path are
     * all within the same external file
     */
    if((gid = H5Gcreate2(fid1, "elink/elink2/group2/group3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid1, "elink/elink2/group2/group3/group4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Oget_info(fid1, "elink/elink2/group2/group3/group4", &oi, H5P_DEFAULT) < 0) TEST_ERROR

    /* Add a few regular groups and a soft link in file2 using intermediate group creation */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) < 0) TEST_ERROR
    if(H5Pset_create_intermediate_group(lcpl_id, TRUE) < 0) TEST_ERROR
    if(H5Lcreate_soft("/elink2", fid1, "elink/file2group1/file2group2/slink",
              lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR

    /* Try to traverse this path.  There are three soft traversals in a row;
     * slink points to (file2)/elink2, which points to (file3)/elink, which
     * points to file 4.
     */
    if((gid = H5Gcreate2(fid1, "elink/file2group1/file2group2/slink/group3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Lget_info(fid1, "elink/file2group1/file2group2/slink/group3", &li, H5P_DEFAULT) < 0) TEST_ERROR

    /* Some simpler tests */
    if((gid = H5Gcreate2(fid1, "elink/file2group3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Lget_info(fid1, "elink/file2group3", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_info(fid1, "elink/elink", &li, H5P_DEFAULT) < 0) TEST_ERROR


    /* Close file1, the only file that should still be open */
    if(H5Fclose(fid1) < 0) TEST_ERROR

    /* Re-create each file. If they are hanging open, these creates will fail */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((fid3 = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((fid4 = H5Fcreate(filename4, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Cleanup */
    if(H5Sclose(sid) < 0) TEST_ERROR
    if(H5Tclose(tid2) < 0) TEST_ERROR
    if(H5Fclose(fid4) < 0) TEST_ERROR
    if(H5Fclose(fid3) < 0) TEST_ERROR
    if(H5Fclose(fid2) < 0) TEST_ERROR
    if(H5Fclose(fid1) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid);
        H5Tclose(tid);
        H5Dclose(did);
        H5Sclose(sid);
        H5Tclose(tid2);
        H5Fclose(fid4);
        H5Fclose(fid3);
        H5Fclose(fid2);
        H5Fclose(fid1);
    } H5E_END_TRY;
    return -1;
} /* external_link_closing() */


/*-------------------------------------------------------------------------
 * Function:    ext_link_endian
 *
 * Purpose:     Check that external links work properly when they are
 *              moved from big-endian to little-endian systems and
 *              vice versa.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_endian(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    hid_t       lapl_id = (-1);                 /* Prop List ID */
    char      * srcdir = getenv("srcdir");      /* The source directory */
    char        pathbuf[NAME_BUF_SIZE];         /* Path to the files */
    char        namebuf[NAME_BUF_SIZE];

    if(new_format)
        TESTING("endianness of external links (w/new group format)")
    else
        TESTING("endianness of external links")

    /*
     * Create the name of the file to open (in case we are using the --srcdir
     * option and the file is in a different directory from this test).
     */
    if (srcdir && ((HDstrlen(srcdir) + 2) < sizeof(pathbuf)) )
    {
        HDstrcpy(pathbuf, srcdir);
        HDstrcat(pathbuf, "/");
    }
    else
        HDstrcpy(pathbuf, "");

    /* Create a link access property list with the path to the srcdir */
    if((lapl_id = H5Pcreate(H5P_LINK_ACCESS)) < 0) TEST_ERROR
    if(H5Pset_elink_prefix(lapl_id, pathbuf) < 0) TEST_ERROR

    if(HDstrlen(pathbuf) + HDstrlen(LE_FILENAME) >= sizeof(namebuf)) TEST_ERROR
    HDstrcpy(namebuf, pathbuf);
    HDstrcat(namebuf, LE_FILENAME);

    /* Test LE file; try to open a group through the external link */
    if((fid = H5Fopen(namebuf, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR
    if((gid = H5Oopen(fid, "ext_link", lapl_id)) < 0) TEST_ERROR

    /* Open a group in the external file using that group ID */
    if((gid2 = H5Gopen2(gid, "subgroup", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close the IDs */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    if(HDstrlen(pathbuf) + HDstrlen(BE_FILENAME) >= sizeof(namebuf)) TEST_ERROR
    HDstrcpy(namebuf, pathbuf);
    HDstrcat(namebuf, BE_FILENAME);

    /* Test BE file; try to open a group through the external link */
    if((fid = H5Fopen(namebuf, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR
    if((gid = H5Oopen(fid, "ext_link", lapl_id)) < 0) TEST_ERROR

    /* Open a group in the external file using that group ID */
    if((gid2 = H5Gopen2(gid, "subgroup", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close the IDs */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    external_link_strong
 *
 * Purpose:     Check that external links work properly when they opened in
 *              a file with "strong" file close degree.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, March 5, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_strong(hid_t fapl, hbool_t new_format)
{
    hid_t       my_fapl;                        /* File access property list */
    hid_t       fid1 = (-1), fid2 = (-1);       /* File ID */
    hid_t       gid1 = (-1), gid2 = (-1);       /* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE],
                filename2[NAME_BUF_SIZE];

    if(new_format)
        TESTING("that external files work with strong file close degree (w/new group format)")
    else
        TESTING("that external files work with strong file close degree")

    /* Set up filenames */
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Copy file access property list */
    if((my_fapl = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Set strong file close degree */
    if(H5Pset_fclose_degree(my_fapl, H5F_CLOSE_STRONG) < 0) TEST_ERROR

    /* Create a group at /A/B/C in first file */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0) TEST_ERROR
    if((gid1 = H5Gcreate2(fid1, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid1) < 0) TEST_ERROR
    if((gid1 = H5Gcreate2(fid1, "A/B", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid1) < 0) TEST_ERROR
    if((gid1 = H5Gcreate2(fid1, "A/B/C", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid1) < 0) TEST_ERROR
    if(H5Fclose(fid1) < 0) TEST_ERROR

    /* Create an external link /W/X/DLINK in second file to <filename1>:/A/B/C */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if((gid2 = H5Gcreate2(fid2, "/W", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if((gid2 = H5Gcreate2(fid2, "/W/X", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Lcreate_external(filename1, "/A/B/C", gid2, "DLINK", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Fclose(fid2) < 0) TEST_ERROR

    /* Access external link from file #1 */
    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR
    if((gid2 = H5Gopen2(fid2, "/W/X/DLINK", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((name_len = H5Iget_name(gid2, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/A/B/C")) TEST_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Fclose(fid2) < 0) TEST_ERROR


    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(fapl);
        H5Gclose(gid2);
        H5Gclose(gid1);
        H5Fclose(fid2);
        H5Fclose(fid1);
    } H5E_END_TRY;
    return -1;
} /* end external_link_strong() */


/*-------------------------------------------------------------------------
 * Function:    ud_hard_links
 *
 * Purpose:     Check that the functionality of hard links can be duplicated
 *              with user-defined links.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 *-------------------------------------------------------------------------
 */
/* Callback functions for UD hard links. */
/* UD_hard_create increments the object's reference count */
static herr_t
UD_hard_create(const char UNUSED * link_name, hid_t loc_group, const void *udata,
    size_t udata_size, hid_t UNUSED lcpl_id)
{
    haddr_t addr;
    hid_t target_obj = -1;
    herr_t ret_value = 0;

    if(udata_size != sizeof(haddr_t)) {
        ret_value = -1;
        goto done;
    } /* end if */

    addr = *((const haddr_t *)udata);

    /* Open the object this link points to */
    target_obj= H5Oopen_by_addr(loc_group, addr);
    if(target_obj < 0) {
        ret_value = -1;
        goto done;
    } /* end if */

    /* Increment the reference count of the target object */
    if(H5Oincr_refcount(target_obj) < 0) {
        ret_value = -1;
        goto done;
    } /* end if */

done:
    /* Close the target object if we opened it */
    if(target_obj >= 0) {
        switch(H5Iget_type(target_obj)) {
            case H5I_GROUP:
                if(H5Gclose(target_obj) < 0)
                    ret_value = -1;
                break;
            case H5I_DATASET:
                if(H5Dclose(target_obj) < 0)
                    ret_value = -1;
                break;
            case H5I_DATATYPE:
                if(H5Tclose(target_obj) < 0)
                    ret_value = -1;
                break;
            default:
              return -1;
        } /* end switch */
    } /* end if */

    return ret_value;
} /* end UD_hard_create() */

/* Traverse a hard link by opening the object */
static hid_t
UD_hard_traverse(const char UNUSED *link_name, hid_t cur_group,
    const void *udata, size_t udata_size, hid_t UNUSED lapl_id)
{
    haddr_t addr;
    hid_t ret_value = -1;

    if(udata_size != sizeof(haddr_t))
        return -1;

    addr = *((const haddr_t *) udata);

    ret_value = H5Oopen_by_addr(cur_group, addr); /* If this fails, our return value will be negative. */

    return ret_value;
} /* end UD_hard_traverse() */

/* UD_hard_delete decrements the object's reference count */
static herr_t
UD_hard_delete(const char UNUSED * link_name, hid_t file, const void *udata,
    size_t udata_size)
{
    haddr_t addr;
    hid_t target_obj = -1;
    herr_t ret_value = 0;

    if(udata_size != sizeof(haddr_t)) {
        ret_value = -1;
        goto done;
    } /* end if */

    addr = *((const haddr_t *) udata);

    /* Open the object this link points to */
    target_obj= H5Oopen_by_addr(file, addr);
    if(target_obj < 0) {
        ret_value = -1;
        goto done;
    } /* end if */

    /* Decrement the reference count of the target object */
    if(H5Odecr_refcount(target_obj) < 0) {
        ret_value = -1;
        goto done;
    } /* end if */

done:
    /* Close the target object if we opened it */
    if(target_obj >= 0) {
        switch(H5Iget_type(target_obj)) {
            case H5I_GROUP:
                if(H5Gclose(target_obj) < 0)
                    ret_value = -1;
                break;
            case H5I_DATASET:
                if(H5Dclose(target_obj) < 0)
                    ret_value = -1;
                break;
            case H5I_DATATYPE:
                if(H5Tclose(target_obj) < 0)
                    ret_value = -1;
                break;
            default:
                return -1;
        } /* end switch */
    } /* end if */

    return ret_value;
} /* end UD_hard_delete() */

const H5L_class_t UD_hard_class[1] = {{
    H5L_LINK_CLASS_T_VERS,           /* H5L_class_t version       */
    UD_HARD_TYPE,               /* Link type id number            */
    "UD_hard_link",             /* Link class name for debugging  */
    UD_hard_create,             /* Creation callback              */
    NULL,                       /* Move/rename callback           */
    NULL,                       /* Copy callback                  */
    UD_hard_traverse,           /* The actual traversal function  */
    UD_hard_delete,             /* Deletion callback              */
    NULL                        /* Query callback                 */
}};

static int
ud_hard_links(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    H5L_info_t li;                          /* Link information */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    h5_stat_size_t empty_size;                  /* Size of an empty file */
    char	filename[NAME_BUF_SIZE];

    TESTING("user-defined hard link (w/new group format)")

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Get the size of the empty file for reference */
    if(H5Fclose(fid) < 0) TEST_ERROR
    if((empty_size = h5_get_file_size(filename))<0) TEST_ERROR

    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Check that external links are registered and UD hard links are not */
    if(H5Lis_registered(H5L_TYPE_EXTERNAL) != TRUE) TEST_ERROR
    if(H5Lis_registered(UD_HARD_TYPE) != FALSE) TEST_ERROR

    /* Register "user-defined hard links" with the library */
    if(H5Lregister(UD_hard_class) < 0) TEST_ERROR

    /* Check that UD hard links are now registered */
    if(H5Lis_registered(H5L_TYPE_EXTERNAL) != TRUE) TEST_ERROR
    if(H5Lis_registered(UD_HARD_TYPE) != TRUE) TEST_ERROR

    /* Create a group for the UD hard link to point to */
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Get address for the group to give to the hard link */
    if(H5Lget_info(fid, "group", &li, H5P_DEFAULT) < 0) TEST_ERROR

    if(H5Gclose(gid) < 0) TEST_ERROR


    /* Create a user-defined "hard link" to the group using the address we got
     * from H5Lget_info */
    if(H5Lcreate_ud(fid, "ud_link", UD_HARD_TYPE, &(li.u.address), sizeof(haddr_t), H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close and re-open file to ensure that data is written to disk */
    if(H5Fclose(fid) < 0) TEST_ERROR
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Open group through UD link */
    if((gid = H5Gopen2(fid, "ud_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/group")) TEST_ERROR

    /* Create object in group */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close groups*/
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Re-open group without using ud link to check that it was created properly */
    if((gid = H5Gopen2(fid, "group/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/group/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Check that H5Lget_objinfo works on the hard link */
    if(H5Lget_info(fid, "ud_link", &li, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    /* UD hard links have no query function, thus return a "link length" of 0 */
    if(li.u.val_size != 0) TEST_ERROR
    if(UD_HARD_TYPE != li.type) {
	H5_FAILED();
	puts("    Unexpected link class - should have been a UD hard link");
	goto error;
    } /* end if */

    /* Unlink the group pointed to by the UD link.  It shouldn't be
     * deleted because of the UD link. */
    if(H5Ldelete(fid, "/group", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Ensure we can open the group through the UD link */
    if((gid = H5Gopen2(fid, "ud_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Unlink the group contained within it. */
    if(H5Ldelete(gid, "new_group", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Now delete the UD link.  This should cause the group to be
     * deleted, too. */
    if(H5Ldelete(fid, "ud_link", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* The file should be empty again. */
    if(empty_size != h5_get_file_size(filename)) TEST_ERROR

    if(H5Lunregister(UD_HARD_TYPE) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end ud_hard_links() */


/*-------------------------------------------------------------------------
 * Function:    UD_rereg_traverse
 *
 * Purpose:     Check that user defined link types can be unregistered and
 *              reregistered properly.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 *-------------------------------------------------------------------------
 */
 /* A traversal function that ignores any udata and simply opens an object
 * in the current group named REREG_TARGET_NAME
 */
static hid_t
UD_rereg_traverse(const char UNUSED * link_name, hid_t cur_group,
    const void UNUSED *udata, size_t UNUSED udata_size, hid_t lapl_id)
{
    hid_t ret_value;

    if((ret_value = H5Oopen(cur_group, REREG_TARGET_NAME, lapl_id)) < 0) TEST_ERROR

    return ret_value;

error:
    return -1;
} /* end UD_rereg_traverse() */

/* This link class has the same ID number as the UD hard links but
 * has a very different traversal function */
const H5L_class_t UD_rereg_class[1] = {{
    H5L_LINK_CLASS_T_VERS,      /* H5L_class_t version       */
    UD_HARD_TYPE,               /* Link type id number            */
    "UD_reregistered_type",     /* Link class name for debugging  */
    NULL,                       /* Creation callback              */
    NULL,                       /* Move/rename callback           */
    NULL,                       /* Copy callback                  */
    UD_rereg_traverse,          /* The actual traversal function  */
    NULL,                       /* Deletion callback              */
    NULL                        /* Query callback                 */
}};

static int
ud_link_reregister(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    H5L_info_t	li;                     /* Link information */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename[NAME_BUF_SIZE];
    h5_stat_size_t empty_size;                  /* Size of an empty file */

    TESTING("registering a new class for existing UD links (w/new group format)")

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Get the size of the empty file for reference */
    if(H5Fclose(fid) < 0) TEST_ERROR
    if((empty_size=h5_get_file_size(filename))<0) TEST_ERROR

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Check that UD hard links are not registered */
    if(H5Lis_registered(UD_HARD_TYPE) != FALSE) TEST_ERROR

    /* Register "user-defined hard links" with the library */
    if(H5Lregister(UD_hard_class) < 0) TEST_ERROR

    /* Check that UD hard links are registered */
    if(H5Lis_registered(UD_HARD_TYPE) != TRUE) TEST_ERROR

    /* Point a UD defined hard link to a group in the same way as the previous test */
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if (H5Lget_info(fid, "group", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if(H5Lcreate_ud(fid, "ud_link", UD_HARD_TYPE, &(li.u.address),
                    sizeof(li.u.address), H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Create a group named REREG_TARGET_NAME in the same group as the ud link */
    if((gid = H5Gcreate2(fid, REREG_TARGET_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Now unregister UD hard links */
    if(H5Lunregister(UD_HARD_TYPE) < 0) TEST_ERROR

    /* Check that UD hard links are no longer registered */
    if(H5Lis_registered(UD_HARD_TYPE) != FALSE) TEST_ERROR

    /* Verify that we can't traverse the ud link anymore */
    H5E_BEGIN_TRY {
        if((gid = H5Gopen2(fid, "ud_link", H5P_DEFAULT)) >= 0) TEST_ERROR
    } H5E_END_TRY

    /* Verify that we can't create any new links of this type */
    H5E_BEGIN_TRY {
      if(H5Lcreate_ud(fid, "ud_link2", UD_HARD_TYPE, &(li.u.address),
                      sizeof(li.u.address), H5P_DEFAULT, H5P_DEFAULT) >= 0)
          TEST_ERROR
    } H5E_END_TRY

    /* Register a new kind of link with the same ID number */
    if(H5Lregister(UD_rereg_class) < 0) TEST_ERROR

    /* Check that UD hard links are registered again */
    if(H5Lis_registered(UD_HARD_TYPE) != TRUE) TEST_ERROR

    /* Open a group through the ud link (now a different class of link).
     * It should be a different group
     * than the UD hard link pointed to */
    if((gid = H5Gopen2(fid, "ud_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/" REREG_TARGET_NAME)) TEST_ERROR

    /* Create object in group */
    if((gid2 = H5Gcreate2(gid, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close groups*/
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Re-open group without using ud link to check that it was created properly */
    if((gid = H5Gopen2(fid, "rereg_target/new_group", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/rereg_target/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Unlink the group pointed to by the UD hard link.  It shouldn't be
     * deleted because the UD link incremented its reference count. */
    if(H5Ldelete(fid, "/group", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* What a mess! Re-register user-defined links to clean up the
     * reference counts.  We shouldn't actually need to unregister the
     * other link type */
    if(H5Lregister(UD_hard_class) < 0) FAIL_STACK_ERROR
    if(H5Lis_registered(UD_HARD_TYPE) != TRUE) FAIL_STACK_ERROR

    /* Ensure we can open the group through the UD link (now that UD hard
     * links have been registered) */
    if((gid = H5Gopen2(fid, "ud_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Delete the UD hard link.  This should cause the group to be
     * deleted, too. */
    if(H5Ldelete(fid, "ud_link", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Unlink the other two groups so that we can make sure the file is empty */
    if(H5Ldelete(fid, "/rereg_target/new_group", H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Ldelete(fid, REREG_TARGET_NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    /* The file should be empty again. */
    if(empty_size != h5_get_file_size(filename)) TEST_ERROR

    if(H5Lunregister(UD_HARD_TYPE) < 0) FAIL_STACK_ERROR
    if(H5Lis_registered(UD_HARD_TYPE) != FALSE) FAIL_STACK_ERROR
    
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end ud_link_reregister() */


/*-------------------------------------------------------------------------
 * Function:    ud_callbacks
 *
 * Purpose:     Check that all callbacks are called and are given the correct
 *              information.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 *-------------------------------------------------------------------------
 */
/* Callback functions for UD "callback" links. */
/* Creation callback.  Called during move as well. */
static herr_t
UD_cb_create(const char * link_name, hid_t loc_group, const void *udata,
    size_t udata_size, hid_t lcpl_id)
{
    if(!link_name) TEST_ERROR
    if(loc_group < 0) TEST_ERROR
    if(udata_size > 0 && !udata) TEST_ERROR
    if(lcpl_id < 0) TEST_ERROR

    if(strcmp(link_name, UD_CB_LINK_NAME) && strcmp(link_name, NEW_UD_CB_LINK_NAME)) TEST_ERROR
    if(strcmp(udata, UD_CB_TARGET)) TEST_ERROR
    if(udata_size != UD_CB_TARGET_LEN) TEST_ERROR

    return 0;

error:
    return -1;
} /* end UD_cb_create() */

static hid_t
UD_cb_traverse(const char * link_name, hid_t cur_group, const void *udata,
    size_t udata_size, hid_t lapl_id)
{
    const char *target = (const char *)udata;
    hid_t ret_value;

    if(!link_name) TEST_ERROR
    if(cur_group < 0) TEST_ERROR
    if(udata_size > 0 && !udata) TEST_ERROR

    if(strcmp(link_name, UD_CB_LINK_NAME) && strcmp(link_name, NEW_UD_CB_LINK_NAME)) TEST_ERROR
    if(strcmp(udata, UD_CB_TARGET)) TEST_ERROR
    if(udata_size != UD_CB_TARGET_LEN) TEST_ERROR

    if((ret_value = H5Oopen(cur_group, target, lapl_id)) < 0)
        TEST_ERROR

    return ret_value;

error:
    return -1;
} /* end UD_cb_traverse() */

/* Callback for when the link is moved or renamed */
static herr_t
UD_cb_move(const char *new_name, hid_t new_loc, const void *udata,
    size_t udata_size)
{
    if(!new_name) TEST_ERROR
    if(new_loc < 0) TEST_ERROR
    if(udata_size > 0 && !udata) TEST_ERROR

    if(HDstrcmp(new_name, NEW_UD_CB_LINK_NAME)) TEST_ERROR
    if(HDstrcmp(udata, UD_CB_TARGET)) TEST_ERROR
    if(udata_size != UD_CB_TARGET_LEN) TEST_ERROR

    return 0;

error:
    return -1;
} /* end UD_cb_move() */

/* Callback for when the link is deleted.  Also called during move */
static herr_t
UD_cb_delete(const char *link_name, hid_t file, const void *udata,
    size_t udata_size)
{
    if(!link_name) TEST_ERROR
    if(file < 0) TEST_ERROR
    if(udata_size > 0 && !udata) TEST_ERROR

    if(HDstrcmp(link_name, UD_CB_LINK_NAME) && HDstrcmp(link_name, NEW_UD_CB_LINK_NAME)) TEST_ERROR
    if(HDstrcmp(udata, UD_CB_TARGET)) TEST_ERROR
    if(udata_size != UD_CB_TARGET_LEN) TEST_ERROR

    return 0;

error:
    return -1;
} /* end UD_cb_delete() */

/* Callback for when the link is queried */
static ssize_t
UD_cb_query(const char * link_name, const void *udata, size_t udata_size,
    void *buf, size_t buf_size)
{
    if(!link_name) TEST_ERROR
    if(udata_size > 0 && !udata) TEST_ERROR

    if(strcmp(link_name, UD_CB_LINK_NAME)) TEST_ERROR
    if(strcmp(udata, UD_CB_TARGET)) TEST_ERROR
    if(udata_size != UD_CB_TARGET_LEN) TEST_ERROR

    if(buf) {
      if(buf_size < 16) TEST_ERROR
      strcpy(buf, "query succeeded");
    } /* end if */

    /* There are 15 characters and a NULL in "query succeeded" */
    return 16;

error:
    return -1;
} /* end UD_cb_query() */

const H5L_class_t UD_cb_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version       */
    UD_CB_TYPE,               /* Link type id number            */
    NULL,                     /* NULL name (to make sure this doesn't break anything */
    UD_cb_create,             /* Creation callback              */
    UD_cb_move,               /* Move/rename callback           */
    UD_cb_move,               /* Copy callback                  */
    UD_cb_traverse,           /* The actual traversal function  */
    UD_cb_delete,             /* Deletion callback              */
    UD_cb_query               /* Query callback                 */
}};

static int
ud_callbacks(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group ID */
    hid_t       lcpl = (-1);                    /* Link Creation PL */
    H5L_info_t  li;                             /* Link information */
    char        ud_target_name[] = UD_CB_TARGET; /* Link target name */
    char	filename[NAME_BUF_SIZE];
    char        query_buf[NAME_BUF_SIZE];

    if(new_format)
        TESTING("user-defined link callbacks (w/new group format)")
    else
        TESTING("user-defined link callbacks")

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Check that registered link classes are, and unregistered ones aren't */
    if(H5Lis_registered(H5L_TYPE_EXTERNAL) != TRUE) TEST_ERROR
    if(H5Lis_registered(UD_HARD_TYPE) != FALSE) TEST_ERROR
    if(H5Lis_registered(UD_CB_TYPE) != FALSE) TEST_ERROR

    /* Hit two birds with one stone: register UD hard links from previous
     * test to check that having two UD links registered at once presents
     * no problems. */
    if(H5Lregister(UD_hard_class) < 0) TEST_ERROR

    /* Register user-defined link class.  This is the one we'll actually
     * be using. */
    if(H5Lregister(UD_cb_class) < 0) TEST_ERROR

    /* Check that registered link classes are, and unregistered ones aren't */
    if(H5Lis_registered(H5L_TYPE_EXTERNAL) != TRUE) TEST_ERROR
    if(H5Lis_registered(UD_HARD_TYPE) != TRUE) TEST_ERROR
    if(H5Lis_registered(UD_CB_TYPE) != TRUE) TEST_ERROR

    /* Create a group for the UD link to point to */
    if((gid = H5Gcreate2(fid, UD_CB_TARGET, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create a user-defined link to the group.  These UD links behave like soft links. */
    if(H5Lcreate_ud(fid, UD_CB_LINK_NAME, UD_CB_TYPE, ud_target_name, (size_t)UD_CB_TARGET_LEN, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Try opening group through UD link */
    if((gid = H5Gopen2(fid, UD_CB_LINK_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Query the link to test its query callback */
    if (H5Lget_info(fid, UD_CB_LINK_NAME, &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.u.val_size != 16) TEST_ERROR
    if (UD_CB_TYPE != li.type) {
	H5_FAILED();
	puts("    Unexpected link class - should have been a UD hard link");
	goto error;
    }

    /* Fill the query buffer */
    if(H5Lget_val(fid, UD_CB_LINK_NAME, query_buf, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(query_buf, "query succeeded") != 0) TEST_ERROR

    /* Move the link */
    if(H5Lmove(fid, UD_CB_LINK_NAME, H5L_SAME_LOC, NEW_UD_CB_LINK_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Re-open group to ensure that move worked */
    if((gid = H5Gopen2(fid, NEW_UD_CB_LINK_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Remove UD link */
    if(H5Ldelete(fid, NEW_UD_CB_LINK_NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR


    /* Test that the callbacks don't work if the link class is not registered */

    /* Create a new link. Just for fun, give it a non-default character
     * encoding (to test that LAPLs work) */
    if((lcpl = H5Pcreate(H5P_LINK_CREATE)) < 0) FAIL_STACK_ERROR
    if(H5Pset_char_encoding(lcpl, H5T_CSET_UTF8) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_ud(fid, UD_CB_LINK_NAME, UD_CB_TYPE, ud_target_name, (size_t)UD_CB_TARGET_LEN, lcpl, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Pclose(lcpl) < 0) FAIL_STACK_ERROR

    /* Check its character encoding */
    if(H5Lget_info(fid, UD_CB_LINK_NAME, &li, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(li.cset != H5T_CSET_UTF8) TEST_ERROR

    /* Unregister the link class so the library forgets what its callbacks do */
    if(H5Lunregister(UD_CB_TYPE) < 0) FAIL_STACK_ERROR

    /* Now test that each of the callbacks fails */
    H5E_BEGIN_TRY {
        if(H5Lcreate_ud(fid, NEW_UD_CB_LINK_NAME, UD_CB_TYPE, ud_target_name, (size_t)UD_CB_TARGET_LEN, H5P_DEFAULT, H5P_DEFAULT) >= 0) FAIL_STACK_ERROR
        if(H5Lmove(fid, UD_CB_LINK_NAME, H5L_SAME_LOC, NEW_UD_CB_LINK_NAME, H5P_DEFAULT, H5P_DEFAULT) >= 0) FAIL_STACK_ERROR
        if(H5Ldelete(fid, UD_CB_LINK_NAME, H5P_DEFAULT) >= 0) FAIL_STACK_ERROR
        if((gid = H5Gopen2(gid, UD_CB_LINK_NAME, H5P_DEFAULT)) >= 0) FAIL_STACK_ERROR
        if(H5Ldelete(fid, UD_CB_LINK_NAME, H5P_DEFAULT) >= 0) FAIL_STACK_ERROR
    } H5E_END_TRY

    /* The query callback should NOT fail, but should be unable to give a linklen */
    if(H5Lget_info(fid, UD_CB_LINK_NAME, &li, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(li.u.val_size != 0) TEST_ERROR
    if(li.type != UD_CB_TYPE) TEST_ERROR

    /* Unregister the UD hard links */
    if(H5Lunregister(UD_HARD_TYPE) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Pclose (lcpl);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end ud_callbacks() */


/*-------------------------------------------------------------------------
 * Function:    lapl_udata
 *
 * Purpose:     Check that information can be passed to UD links using the
 *              Link Access Property List.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static hid_t
UD_plist_traverse(const char UNUSED * link_name, hid_t cur_group,
    const void UNUSED *udata, size_t udata_size, hid_t lapl_id)
{
    char target[NAME_BUF_SIZE];
    hid_t ret_value;

    if(udata_size != 0) TEST_ERROR

    /* Get the name of the target from the property list. */
    if(H5Pget(lapl_id, DEST_PROP_NAME, target) < 0) TEST_ERROR

    if((ret_value = H5Oopen(cur_group, target, lapl_id)) < 0)
        TEST_ERROR

    return ret_value;

error:
    return -1;
} /* end UD_plist_traverse() */

const H5L_class_t UD_plist_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version       */
    UD_PLIST_TYPE,            /* Link type id number            */
    "UD_plist_link",          /* Link class name for debugging  */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    UD_plist_traverse,        /* The actual traversal function  */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};

static int
lapl_udata(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    hid_t       plist_id = (-1);                /* Property List ID */
    char	group_a_name[NAME_BUF_SIZE];
    char	group_b_name[NAME_BUF_SIZE];
    char	filename[NAME_BUF_SIZE];

    if(new_format)
        TESTING("user data passed through lapl (w/new group format)")
    else
        TESTING("user data passed through lapl")

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Register UD link types from previous tests to check that having
     * multiple types registered at once presents no problems. */
    if(H5Lregister(UD_cb_class) < 0) TEST_ERROR

    /* Register the link class.  We'll actually be using for this test. */
    if(H5Lregister(UD_plist_class) < 0) TEST_ERROR

    /* Another link class from a previous test */
    if(H5Lregister(UD_hard_class) < 0) TEST_ERROR

    /* Unregister the first link type registered to make sure this doesn't
     * break anything. */
    if(H5Lunregister(UD_CB_TYPE) < 0) TEST_ERROR

    /* Create two groups for the UD link to point to */
    if((gid = H5Gcreate2(fid, "group_a", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "group_b", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Create a user-defined link to the group.  These UD links have no udata. */
    if(H5Lcreate_ud(fid, "ud_link", UD_PLIST_TYPE, NULL, (size_t)0, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Create a non-default lapl with a new property pointing to group a*/
    if((plist_id = H5Pcreate(H5P_LINK_ACCESS)) < 0) TEST_ERROR
    HDstrcpy(group_a_name, "group_a");
#ifdef H5_WANT_H5_V1_6_COMPAT
    if(H5Pinsert(plist_id, DEST_PROP_NAME, (size_t)NAME_BUF_SIZE, group_a_name, NULL, NULL, NULL, NULL, NULL) < 0) TEST_ERROR
#else /* H5_WANT_H5_V1_6_COMPAT */
    if(H5Pinsert(plist_id, DEST_PROP_NAME, (size_t)NAME_BUF_SIZE, group_a_name, NULL, NULL, NULL, NULL, NULL, NULL) < 0) TEST_ERROR
#endif /* H5_WANT_H5_V1_6_COMPAT */

    /* Try opening group through UD link */
    if((gid = H5Oopen(fid, "ud_link", plist_id)) < 0) TEST_ERROR
    if((gid2 = H5Gcreate2(gid, "subgroup_a", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Verify that we can open the new group without using the ud link */
    if((gid2 = H5Gopen2(fid, "/group_a/subgroup_a", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Now use the same ud link to access group_b */
    strcpy(group_b_name, "group_b");
    if(H5Pset(plist_id, DEST_PROP_NAME, group_b_name) < 0) TEST_ERROR

    /* Create a subgroup */
    if((gid = H5Oopen(fid, "ud_link", plist_id)) < 0) TEST_ERROR
    if((gid2 = H5Gcreate2(gid, "subgroup_b", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Verify that we can open the new group without using the ud link */
    if((gid2 = H5Gopen2(fid, "/group_b/subgroup_b", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close property list */
    if(H5Pclose(plist_id) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Pclose (plist_id);
    	H5Gclose (gid);
    	H5Gclose (gid2);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end lapl_udata() */


/*-------------------------------------------------------------------------
 * Function:    ud_link_errors
 *
 * Purpose:     Create error conditions in callbacks and ensure that the
 *              errors propagate correctly.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
UD_cbsucc_create(const char UNUSED * link_name, hid_t UNUSED loc_group,
    const void *udata, size_t udata_size, hid_t UNUSED lcpl_id)
{
    /* Check to make sure that this "soft link" has a target */
    if(udata_size < 1 || !udata)
       return -1;

    return 0;
} /* end UD_cbsucc_create() */

static hid_t
UD_cbsucc_traverse(const char UNUSED *link_name, hid_t cur_group,
    const void *udata, size_t UNUSED udata_size, hid_t lapl_id)
{
    const char *target = (const char *)udata;
    hid_t ret_value;

    if(!target) goto error;

    if((ret_value = H5Oopen(cur_group, target, lapl_id)) < 0) goto error;

    return ret_value;

error:
    return -1;
} /* end UD_cbsucc_traverse() */

/* Failure callback for when the link is moved or renamed */
static herr_t
UD_cbfail_move(const char UNUSED *new_name, hid_t UNUSED new_loc,
    const void UNUSED *udata, size_t UNUSED udata_size)
{
    /* This traversal function will always fail. */
    return -1;
} /* end UD_cbfail_move() */

/* SuccessCallback for when the link is moved or renamed */
static herr_t
UD_cbsucc_move(const char UNUSED *new_name, hid_t UNUSED new_loc,
    const void UNUSED *udata, size_t UNUSED udata_size)
{
    /* This traversal function will always succeed. */
    return 0;
} /* end UD_cbsucc_move() */

/* Callback for when the link is deleted.  Also called during move */
static herr_t
UD_cbsucc_delete(const char UNUSED *link_name, hid_t UNUSED file,
    const void UNUSED *udata, size_t UNUSED udata_size)
{
    /* This callback will always succeed */
    return 0;
} /* end UD_cbsucc_delete() */

/* Callback for when the link is deleted.  Also called during move */
static herr_t
UD_cbfail_delete(const char UNUSED *link_name, hid_t UNUSED file,
    const void UNUSED *udata, size_t UNUSED udata_size)
{
    /* This traversal function will always fail. */
    /* Note: un-deletable links are in general a very bad idea! */
    return -1;
} /* end UD_cbfail_delete() */

/* Callback for when the link is queried */
static ssize_t
UD_cbfail_query(const char UNUSED *link_name, const void UNUSED *udata,
    size_t UNUSED udata_size, void UNUSED *buf, size_t UNUSED buf_size)
{
    /* This traversal function will always fail. */
    return -1;
} /* end UD_cbfail_query() */

/* Callback for when the link is queried */
static ssize_t
UD_cbfail_on_write_query(const char UNUSED *link_name, const void UNUSED *udata,
    size_t UNUSED udata_size, void *buf, size_t UNUSED buf_size)
{
    /* This traversal function will return a buffer size,
     * but will fail when a buffer is passed in ("writing to the buffer"
     * fails
     */

    if(buf != NULL)
        return -1;

    return 0;
} /* end UD_cbfail_on_write_query() */

/* Callback for when the link is queried */
static ssize_t
UD_cbsucc_query(const char UNUSED *link_name, const void UNUSED *udata,
    size_t UNUSED udata_size, void *buf, size_t buf_size)
{
    /* This traversal function will return a buffer size,
     * but will fail when a buffer is passed in ("writing to the buffer"
     * fails
     */

    if(buf != NULL && buf_size >= 8)
        strcpy(buf, "succeed");

    return 8;
} /* end UD_cbsucc_query() */

/* This class is full of failing callbacks */
const H5L_class_t UD_cbfail_class1[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version       */
    UD_CBFAIL_TYPE,               /* Link type id number            */
    "UD_cbfail_link1",            /* Link class name for debugging  */
    UD_cbsucc_create,             /* Creation callback              */
    UD_cbfail_move,               /* Move/rename callback           */
    UD_cbfail_move,               /* Copy callback                  */
    UD_cbsucc_traverse,           /* The actual traversal function  */
    UD_cbfail_delete,             /* Deletion callback              */
    UD_cbfail_query               /* Query callback                 */
}};

/* This class is has two failing callbacks, move and query */
const H5L_class_t UD_cbfail_class2[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version       */
    UD_CBFAIL_TYPE,               /* Link type id number            */
    "UD_cbfail_link2",            /* Link class name for debugging  */
    UD_cbsucc_create,             /* Creation callback              */
    UD_cbfail_move,               /* Move/rename callback           */
    UD_cbsucc_move,               /* Copy callback                  */
    UD_cbsucc_traverse,           /* The actual traversal function  */
    UD_cbsucc_delete,             /* Deletion callback              */
    UD_cbfail_on_write_query      /* Query callback                 */
}};

/* All of these callbacks will succeed */
const H5L_class_t UD_cbfail_class3[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version       */
    UD_CBFAIL_TYPE,               /* Link type id number            */
    "UD_cbfail_link3",            /* Link class name for debugging  */
    UD_cbsucc_create,             /* Creation callback              */
    UD_cbsucc_move,               /* Move/rename callback           */
    UD_cbsucc_move,               /* Copy callback                  */
    UD_cbsucc_traverse,           /* The actual traversal function  */
    UD_cbsucc_delete,             /* Deletion callback              */
    UD_cbsucc_query               /* Query callback                 */
}};

/* Link classes that are invalid for various reasons */
const H5L_class_t UD_error1_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version       */
    UD_ERROR_TYPE,            /* Link type id number            */
    "UD_error_link",          /* Link class name for debugging  */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    NULL,                     /* This class has no traversal function */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};
const H5L_class_t UD_error2_class[1] = {{
    UD_BAD_VERS,              /* Invalid H5L_class_t version */
    UD_ERROR_TYPE,            /* Link type id number            */
    "UD_error_link",          /* Link class name for debugging  */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    UD_cbsucc_traverse,       /* Traversal function             */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};
const H5L_class_t UD_error3_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version */
    UD_BAD_TYPE1,             /* Invalid Link type id number            */
    "UD_error_link",          /* Link class name for debugging  */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    UD_cbsucc_traverse,       /* Traversal function             */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};
const H5L_class_t UD_error4_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version */
    UD_BAD_TYPE2,             /* Invalid Link type id number            */
    "UD_error_link",          /* Link class name for debugging  */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    UD_cbsucc_traverse,       /* Traversal function             */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};

static int
ud_link_errors(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);                     /* Group IDs */
    char	group_name[NAME_BUF_SIZE];
    char	filename[NAME_BUF_SIZE];
    char        query_buf[NAME_BUF_SIZE];
    H5L_info_t li;                         /* Link information */

    if(new_format)
        TESTING("user-defined link error conditions (w/new group format)")
    else
        TESTING("user-defined link error conditions")

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Try to register some invalid link classes */
    H5E_BEGIN_TRY {
      if(H5Lregister(UD_error1_class) >= 0) TEST_ERROR
      if(H5Lregister(UD_error2_class) >= 0) TEST_ERROR
      if(H5Lregister(UD_error3_class) >= 0) TEST_ERROR
      if(H5Lregister(UD_error4_class) >= 0) TEST_ERROR
    } H5E_END_TRY

    /* Register the UD plist class. */
    if(H5Lregister(UD_plist_class) < 0) TEST_ERROR
    /* Now register the first class we'll be using.
     * It has the same ID as the plist class, and should replace it. */
    if(H5Lregister(UD_cbfail_class1) < 0) FAIL_STACK_ERROR

    /* Create a group for the UD link to point to */
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Create a user-defined link to the group. */
    strcpy(group_name, "/group");
    if(H5Lcreate_ud(fid, "/ud_link", UD_CBFAIL_TYPE, &group_name, HDstrlen(group_name) + 1, H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Open the group through the ud link */
    if((gid = H5Gopen2(fid, "ud_link", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

    /* Now test that each of the callbacks will cause a failure if it returns -1 */
    H5E_BEGIN_TRY {
        /* The create callback will fail if we pass in no udata */
        if(H5Lcreate_ud(fid, "fail", UD_CBFAIL_TYPE, NULL, (size_t)0, H5P_DEFAULT, H5P_DEFAULT) >= 0) TEST_ERROR

        /* The move and copy callbacks will fail */
        if(H5Lmove(fid, "ud_link", H5L_SAME_LOC, "move_fail", H5P_DEFAULT, H5P_DEFAULT) >= 0) TEST_ERROR
        if(H5Lcopy(fid, "ud_link", fid, "copy_fail", H5P_DEFAULT, H5P_DEFAULT) >= 0) TEST_ERROR

        /* The traversal callback will fail if we remove its target */
        if(H5Ldelete(fid, "group", H5P_DEFAULT) < 0) TEST_ERROR
        if((gid = H5Gopen2(gid, "ud_link", H5P_DEFAULT)) >= 0) TEST_ERROR

        /* The deletion callback will always fail */
        if(H5Ldelete(fid, "ud_link", H5P_DEFAULT) >= 0) TEST_ERROR

        /* The query callback will fail */
        if(H5Lget_info(fid, "ud_link", &li, H5P_DEFAULT) >=0) TEST_ERROR
    } H5E_END_TRY

    /* Now use a class with different callback functions */
    if(H5Lregister(UD_cbfail_class2) < 0) FAIL_STACK_ERROR

    /* Moving should still fail, but copying will succeed */
    H5E_BEGIN_TRY {
        if(H5Lmove(fid, "ud_link", H5L_SAME_LOC, "move_fail", H5P_DEFAULT, H5P_DEFAULT) >= 0) TEST_ERROR
    } H5E_END_TRY
    if(H5Lcopy(fid, "ud_link", fid, "copy_succ", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* The query callback will succeed when we only want to get the size of the buffer... */
    if(H5Lget_info(fid, "ud_link", &li, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(li.u.val_size != 0) TEST_ERROR
    /* ...but fail when we try to write data to the buffer itself*/
    H5E_BEGIN_TRY {
        if(H5Lget_val(fid, "ud_link", query_buf, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) >=0) TEST_ERROR
    } H5E_END_TRY

    /* Register a new class */
    if(H5Lregister(UD_cbfail_class3) < 0) FAIL_STACK_ERROR

    /* Now querying should succeed */
    if(H5Lget_info(fid, "ud_link", &li, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(li.u.val_size != 8) TEST_ERROR
    if(H5Lget_val(fid, "ud_link", query_buf, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(HDstrcmp(query_buf, "succeed") != 0) TEST_ERROR

    /* Moving and copying should both succeed */
    if(H5Lmove(fid, "copy_succ", H5L_SAME_LOC, "move_succ", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcopy(fid, "ud_link", fid, "copy_succ2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Delete link (this callback should work now) */
    if(H5Ldelete(fid, "ud_link", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end ud_link_errors() */


/*-------------------------------------------------------------------------
 * Function:    lapl_nlinks
 *
 * Purpose:     Check that the maximum number of soft links can be adjusted
 *              by the user using the Link Access Property List.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
lapl_nlinks(hid_t fapl, hbool_t new_format)
{
    hid_t		fid = (-1);               /* File ID */
    hid_t	gid = (-1), gid2 = (-1);          /* Group IDs */
    hid_t		plist = (-1);             /* lapl ID */
    hid_t       tid = (-1), sid = (-1), did = (-1); /* Other IDs */
    hid_t gapl = (-1), dapl = (-1), tapl = (-1);   /* Other property lists */
    char                objname[NAME_BUF_SIZE];   /* Object name */
    ssize_t             name_len;       /* Length of object name */
    char		filename[NAME_BUF_SIZE];
    size_t              nlinks;               /* nlinks for H5Pset_nlinks */
    hsize_t     	dims[2];

    if(new_format)
        TESTING("adjusting nlinks with LAPL (w/new group format)")
    else
        TESTING("adjusting nlinks with LAPL")

    /* Make certain test is valid */
    /* XXX: should probably make a "generic" test that creates the proper
     *          # of links based on this value - QAK
     */
    HDassert(H5L_NUM_LINKS == 16);

    /* Create file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group with short name in file (used as target for links) */
    if((gid = H5Gcreate2(fid, "final", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Create chain of soft links to existing object (limited) */
    if(H5Lcreate_soft("final", fid, "soft1", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft1", fid, "soft2", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft2", fid, "soft3", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft3", fid, "soft4", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft4", fid, "soft5", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft5", fid, "soft6", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft6", fid, "soft7", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft7", fid, "soft8", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft8", fid, "soft9", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft9", fid, "soft10", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft10", fid, "soft11", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft11", fid, "soft12", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft12", fid, "soft13", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft13", fid, "soft14", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft14", fid, "soft15", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft15", fid, "soft16", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("soft16", fid, "soft17", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    /* Close objects */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open file */
    if((fid=H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Create LAPL with higher-than-usual nlinks value */
    /* Create a non-default lapl with udata set to point to the first group */
    if((plist = H5Pcreate(H5P_LINK_ACCESS)) < 0) TEST_ERROR
    nlinks = 20;
    if(H5Pset_nlinks(plist, nlinks) < 0) TEST_ERROR

    /* Ensure that nlinks was set successfully */
    nlinks = 0;
    if(H5Pget_nlinks(plist, &nlinks) < 0) TEST_ERROR
    if(nlinks != 20) TEST_ERROR

    /* Open object through what is normally too many soft links using
     * new property list */
    if((gid = H5Oopen(fid, "soft17", plist)) < 0) TEST_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/soft17")) TEST_ERROR

    /* Create group using soft link */
    if((gid2 = H5Gcreate2(gid, "new_soft", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close groups */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Set nlinks to a smaller number */
    nlinks = 4;
    if(H5Pset_nlinks(plist, nlinks) < 0) TEST_ERROR

    /* Ensure that nlinks was set successfully */
    nlinks = 0;
    if(H5Pget_nlinks(plist, &nlinks) < 0) TEST_ERROR
    if(nlinks != 4) TEST_ERROR

    /* Try opening through what is now too many soft links */
    H5E_BEGIN_TRY {
        gid = H5Oopen(fid, "soft5", plist);
    } H5E_END_TRY;
    if (gid >= 0) {
	H5_FAILED();
	puts("    Should have failed for sequence of too many nested links.");
	goto error;
    }

    /* Open object through lesser soft link */
    if((gid = H5Oopen(fid, "soft4", plist)) < 0) TEST_ERROR

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/soft4")) TEST_ERROR


    /* Test other functions that should use a LAPL */
    nlinks = 20;
    if(H5Pset_nlinks(plist, nlinks) < 0) TEST_ERROR

    /* Try copying and moving when both src and dst contain many soft links
     * using a non-default LAPL
     */
    if(H5Lcopy(fid, "soft17", fid, "soft17/newer_soft", H5P_DEFAULT, plist) < 0) TEST_ERROR
    if(H5Lmove(fid, "soft17/newer_soft", fid, "soft17/newest_soft", H5P_DEFAULT, plist) < 0) TEST_ERROR

    /* H5Llink */
    if(H5Llink(fid, "soft17/link_to_group", gid, H5P_DEFAULT, plist) < 0) TEST_ERROR

    /* H5Lcreate_hard  and H5Lcreate_soft */
    if(H5Lcreate_hard(fid, "soft17", fid, "soft17/link2_to_group", H5P_DEFAULT, plist) < 0) TEST_ERROR
    if(H5Lcreate_soft("/soft4", fid, "soft17/soft_link", H5P_DEFAULT, plist) < 0) TEST_ERROR

    /* H5Ldelete */
    if(H5Ldelete(fid, "soft17/soft_link", plist) < 0) TEST_ERROR

    /* H5Lget_val and H5Lget_info */
    if(H5Lget_val(fid, "soft17", NULL, (size_t)0, plist) < 0) TEST_ERROR
    if(H5Lget_info(fid, "soft17", NULL, plist) < 0) TEST_ERROR

    /* H5Lcreate_external and H5Lcreate_ud */
    if(H5Lcreate_external("filename", "path", fid, "soft17/extlink", H5P_DEFAULT, plist) < 0) TEST_ERROR
    if(H5Lregister(UD_rereg_class) < 0) TEST_ERROR
    if(H5Lcreate_ud(fid, "soft17/udlink", UD_HARD_TYPE, NULL, (size_t)0, H5P_DEFAULT, plist) < 0) TEST_ERROR

    /* Close plist */
    if(H5Pclose(plist) < 0) TEST_ERROR


    /* Create a datatype and dataset as targets inside the group */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if(H5Tcommit2(gid, "datatype", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Tclose(tid) < 0) TEST_ERROR

    dims[0] = 2;
    dims[1] = 2;
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) TEST_ERROR
    if((did = H5Dcreate(gid, "dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Try to open the objects using too many symlinks with default *APLs */
    H5E_BEGIN_TRY {
        if((gid = H5Gopen2(fid, "soft17", H5P_DEFAULT)) >= 0)
            FAIL_PUTS_ERROR("    Should have failed for too many nested links.")
        if((tid = H5Topen2(fid, "soft17/datatype", H5P_DEFAULT)) >= 0)
            FAIL_PUTS_ERROR("    Should have failed for too many nested links.")
        if((did = H5Dopen2(fid, "soft17/dataset", H5P_DEFAULT)) >= 0)
            FAIL_PUTS_ERROR("    Should have failed for too many nested links.")
    } H5E_END_TRY

    /* Create property lists with nlinks set */
    if((gapl = H5Pcreate(H5P_GROUP_ACCESS)) < 0) TEST_ERROR
    if((tapl = H5Pcreate(H5P_DATATYPE_ACCESS)) < 0) TEST_ERROR
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0) TEST_ERROR

    nlinks = 20;
    if(H5Pset_nlinks(gapl, nlinks) < 0) TEST_ERROR 
    if(H5Pset_nlinks(tapl, nlinks) < 0) TEST_ERROR 
    if(H5Pset_nlinks(dapl, nlinks) < 0) TEST_ERROR 

    /* We should now be able to use these property lists to open each kind
     * of object.
     */
    if((gid = H5Gopen2(fid, "soft17", gapl)) < 0) FAIL_STACK_ERROR
    if((tid = H5Topen2(fid, "soft17/datatype", tapl)) < 0) TEST_ERROR
    if((did = H5Dopen2(fid, "soft17/dataset", dapl)) < 0) TEST_ERROR

    /* Close objects */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Tclose(tid) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    /* Close plists */
    if(H5Pclose(gapl) < 0) TEST_ERROR
    if(H5Pclose(tapl) < 0) TEST_ERROR
    if(H5Pclose(dapl) < 0) TEST_ERROR

    /* Unregister UD hard link class */
    if(H5Lunregister(UD_HARD_TYPE) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Pclose(gapl);
    	H5Pclose(dapl);
    	H5Pclose(tapl);
    	H5Dclose(did);
    	H5Sclose(sid);
    	H5Tclose(tid);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Pclose(plist);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end lapl_nlinks() */


/*-------------------------------------------------------------------------
 * Function:    linkinfo
 *
 * Purpose:     Check that the link class is returned correctly when queried.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Tuesday, June 6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
linkinfo(hid_t fapl, hbool_t new_format)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group ID */
    hid_t       tid = (-1);                     /* Type ID */
    hid_t       sid = (-1), did = -(1);         /* Dataspace and dataset IDs */
    H5L_info_t li;                          /* Link information */
    char	filename[NAME_BUF_SIZE];

    if(new_format)
        TESTING("link type field in H5Lget_info (w/new group format)")
    else
        TESTING("link type field in H5Lget_info")

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Register a couple of user-defined link classes with the library */
    if(H5Lregister(UD_plist_class) < 0) TEST_ERROR

    /* Create an object of each type */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if(H5Tcommit2(fid, "datatype", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if(H5Lcreate_soft("group", fid, "softlink", H5P_DEFAULT, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

    if((sid = H5Screate(H5S_SCALAR)) < 0) TEST_ERROR
    if((did = H5Dcreate(fid, "dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT)) < 0) TEST_ERROR

    if(H5Lcreate_ud(fid, "ud_link", UD_PLIST_TYPE, NULL, (size_t)0, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lcreate_external("file_name", "obj_path", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close all objects */
    if(H5Tclose(tid) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    /* Make sure that link type is correct when objects are queried */
    if(H5Lget_info(fid, "datatype", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.type != H5L_TYPE_HARD) TEST_ERROR
    if(H5Lget_info(fid, "group", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.type != H5L_TYPE_HARD) TEST_ERROR
    if(H5Lget_info(fid, "dataset", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.type != H5L_TYPE_HARD) TEST_ERROR

    if(H5Lget_info(fid, "ext_link", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.type != H5L_TYPE_EXTERNAL) TEST_ERROR
    if(H5Lget_info(fid, "softlink", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.type != H5L_TYPE_SOFT) TEST_ERROR
    if(H5Lget_info(fid, "ud_link", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.type != UD_PLIST_TYPE) TEST_ERROR

    /* Ensure that passing a NULL pointer doesn't cause an error */
    if(H5Lget_info(fid, "group", NULL, H5P_DEFAULT) < 0) TEST_ERROR

    if(H5Fclose(fid) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Tclose (tid);
    	H5Dclose (did);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end ud_hard_links() */


/*-------------------------------------------------------------------------
 * Function:    check_all_closed
 *
 * Purpose:     External links and some UD links open files.  To make sure
 *              that all such files got closed correctly, try to create
 *              each of them.
 *
 *              If the files are still open, this will fail (indicating that
 *              some other test made a mistake).
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  James Laird
 *              Thursday, August 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
check_all_closed(hid_t fapl, hbool_t new_format)
{
    hid_t fid=-1;
    char filename[NAME_BUF_SIZE];
    int x;

    if(new_format)
        TESTING("that all files were closed correctly (w/new group format)")
    else
        TESTING("that all files were closed correctly")

    /* Some of the external or UD link tests may have failed to close
     * an external file properly.
     * To check this, try to create every file used in this test.  If
     * a file is already open, creating it will fail.
     */
    for(x=0; FILENAME[x] != NULL; x++)
    {
        h5_fixname(FILENAME[x], fapl, filename, sizeof filename);

        if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
        if(H5Fclose(fid) < 0) TEST_ERROR
    }

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end check_all_closed() */


/*-------------------------------------------------------------------------
 * Function:    corder_create_empty
 *
 * Purpose:     Create an empty group with creation order indices
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 30, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
corder_create_empty(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    unsigned    crt_order_flags;   	/* Status of creation order info for GCPL */
    herr_t      ret;                    /* Generic return value */
    char        filename[NAME_BUF_SIZE];/* File name */

    TESTING("creating empty group with creation order indexing")

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Set creation order indexing on group */
    if(H5Pget_link_creation_order(gcpl_id, &crt_order_flags) < 0) TEST_ERROR
    if(crt_order_flags != 0) TEST_ERROR

    /* Setting invalid combination of a group order creation order indexing on should fail */
    H5E_BEGIN_TRY {
        ret = H5Pset_link_creation_order(gcpl_id, H5P_CRT_ORDER_INDEXED);
    } H5E_END_TRY;
    if(ret > 0) {
	H5_FAILED();
	puts("    H5Pset_link_create_order() should have failed for a creation order index with no tracking.");
	TEST_ERROR
    } /* end if */

    /* Set creation order tracking & indexing on group */
    if(H5Pget_link_creation_order(gcpl_id, &crt_order_flags) < 0) TEST_ERROR
    if(crt_order_flags != 0) TEST_ERROR
    if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED)) < 0) TEST_ERROR
    if(H5Pget_link_creation_order(gcpl_id, &crt_order_flags) < 0) TEST_ERROR
    if(crt_order_flags != (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED)) TEST_ERROR

    /* Create group with creation order indexing & tracking on */
    if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check on group's status */
    if(H5G_is_empty_test(group_id) != TRUE) TEST_ERROR

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR


    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open group created */
    if((group_id = H5Gopen2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Check on group's status */
    if(H5G_is_empty_test(group_id) != TRUE) TEST_ERROR

    /* Retrieve group creation property list for group */
    if((gcpl_id = H5Gget_create_plist(group_id)) < 0) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_creation_order(gcpl_id, &crt_order_flags) < 0) TEST_ERROR
    if(crt_order_flags != (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED)) TEST_ERROR

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end corder_create_empty() */


/*-------------------------------------------------------------------------
 * Function:    corder_create_compact
 *
 * Purpose:     Create a group with creation order indices and insert links
 *              in it when in compact form
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 30, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
corder_create_compact(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1), group_id2 = (-1);	/* Group IDs */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    unsigned	nlinks;		        /* Number of link messages in group's header */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    unsigned    u;                      /* Local index variable */

    TESTING("creating compact group with creation order indexing")

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Set creation order tracking & indexing on group */
    if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED)) < 0) TEST_ERROR

    /* Create group with creation order indexing & tracking on */
    if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check on group's initial status */
    if(H5G_is_empty_test(group_id) != TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Create several links, but keep group in compact form */
    for(u = 0; u < max_compact; u++) {
        sprintf(objname, "filler %u", u);
        if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(group_id2) < 0) TEST_ERROR

        /* Verify state of group */
        if(H5G_has_links_test(group_id, &nlinks) != TRUE) TEST_ERROR
        if(nlinks != (u + 1)) TEST_ERROR
        if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
        if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR
    } /* end for */

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR


    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open group created */
    if((group_id = H5Gopen2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify state of group */
    if(H5G_has_links_test(group_id, &nlinks) != TRUE) TEST_ERROR
    if(nlinks != max_compact) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR

    /* Loop through links, checking their creation order values */
    /* (the name index is used, but the creation order value is in the same order) */
    for(u = 0; u < max_compact; u++) {
        H5L_info_t linfo;           /* Link information */

        /* Retrieve information for link */
        sprintf(objname, "filler %u", u);
        if(H5Lget_info(group_id, objname, &linfo, H5P_DEFAULT) < 0) TEST_ERROR

        /* Verify creation order of link */
        if(linfo.corder_valid != TRUE) TEST_ERROR
        if(linfo.corder != u) TEST_ERROR
    } /* end for */

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end corder_create_compact() */


/*-------------------------------------------------------------------------
 * Function:    corder_create_dense
 *
 * Purpose:     Create a group with creation order indices and insert links
 *              in it until it's in dense form
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 30, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
corder_create_dense(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1), group_id2 = (-1);	/* Group IDs */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    unsigned	nlinks;		        /* Number of link messages in group's header */
    hsize_t     name_count;             /* # of records in name index */
    hsize_t     corder_count;           /* # of records in creation order index */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    unsigned    u;                      /* Local index variable */

    TESTING("creating dense group with creation order indexing")

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Set creation order tracking & indexing on group */
    if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED)) < 0) TEST_ERROR

    /* Create group with creation order indexing & tracking on */
    if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Check on group's initial status */
    if(H5G_is_empty_test(group_id) != TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Create several links, up to limit of compact form */
    for(u = 0; u < max_compact; u++) {
        sprintf(objname, "filler %u", u);
        if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(group_id2) < 0) TEST_ERROR

        /* Verify state of group */
        if(H5G_has_links_test(group_id, &nlinks) != TRUE) TEST_ERROR
        if(nlinks != (u + 1)) TEST_ERROR
        if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
        if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR
    } /* end for */

    /* Create another link, to push group into dense form */
    sprintf(objname, "filler %u", max_compact);
    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(group_id2) < 0) TEST_ERROR

    /* Verify state of group */
    if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

    /* Retrieve & verify # of records in the name & creation order indices */
    if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
    if(name_count != corder_count) TEST_ERROR

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR


    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open group created */
    if((group_id = H5Gopen2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify state of group */
    if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

    /* Loop through links, checking their creation order values */
    /* (the name index is used, but the creation order value is in the same order) */
    for(u = 0; u < (max_compact + 1); u++) {
        H5L_info_t linfo;           /* Link information */

        /* Retrieve information for link */
        sprintf(objname, "filler %u", u);
        if(H5Lget_info(group_id, objname, &linfo, H5P_DEFAULT) < 0) TEST_ERROR

        /* Verify creation order of link */
        if(linfo.corder_valid != TRUE) TEST_ERROR
        if(linfo.corder != u) TEST_ERROR
    } /* end for */

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end corder_create_dense() */


/*-------------------------------------------------------------------------
 * Function:    corder_transition
 *
 * Purpose:     Create a group with creation order indices and verify correct
 *              transitions between compact & dense forms
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 30, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
corder_transition(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1), group_id2 = (-1);	/* Group IDs */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    unsigned	nlinks;		        /* Number of link messages in group's header */
    hsize_t     name_count;             /* # of records in name index */
    hsize_t     corder_count;           /* # of records in creation order index */
    h5_stat_size_t       empty_size;             /* Size of empty file */
    h5_stat_size_t       file_size;              /* Size of file after operating on it */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    unsigned    u;                      /* Local index variable */

    TESTING("transitioning group with creation order indexing between dense & compact forms")

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) FAIL_STACK_ERROR

    /* Set creation order tracking & indexing on group */
    if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED)) < 0) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) FAIL_STACK_ERROR

    /* Increase estimated link info, so the group's object header is large
     *      enough to hold all the link messages in one chunk
     */
    if(H5Pset_est_link_info(gcpl_id, max_compact, CORDER_EST_ENTRY_LEN) < 0) TEST_ERROR

    /* Create group with creation order indexing & tracking on */
    if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) FAIL_STACK_ERROR

    /* Close the group */
    if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR

    /* Get the size of the file with an empty group */
    if((empty_size = h5_get_file_size(filename)) < 0) TEST_ERROR


    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

    /* Open group created */
    if((group_id = H5Gopen2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Create several links, up to limit of compact form */
    for(u = 0; u < max_compact; u++) {
        sprintf(objname, "filler %u", u);
        if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(group_id2) < 0) TEST_ERROR
    } /* end for */

    /* Create another link, to push group into dense form */
    sprintf(objname, "filler %u", max_compact);
    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Gclose(group_id2) < 0) TEST_ERROR

    /* Verify state of group */
    if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

    /* Retrieve & verify # of records in the name & creation order indices */
    if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
    if(name_count != corder_count) TEST_ERROR

    /* Delete several links from group, until it resumes compact form */
    for(u = max_compact; u >= min_dense; u--) {
        sprintf(objname, "filler %u", u);
        if(H5Ldelete(group_id, objname, H5P_DEFAULT) < 0) TEST_ERROR

        /* Verify state of group */
        if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
        if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
        if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

        /* Retrieve & verify # of records in the name & creation order indices */
        if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
        if(name_count != corder_count) TEST_ERROR
    } /* end for */

    /* Delete another link, to push group into compact form */
    sprintf(objname, "filler %u", (min_dense - 1));
    if(H5Ldelete(group_id, objname, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify state of group */
    if(H5G_has_links_test(group_id, &nlinks) != TRUE) TEST_ERROR
    if(nlinks != (min_dense - 1)) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR

    /* Re-add links to get back into dense form */
    for(u = (min_dense - 1); u < (max_compact + 1); u++) {
        sprintf(objname, "filler %u", u);
        if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(group_id2) < 0) TEST_ERROR
    } /* end for */

    /* Verify state of group */
    if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

    /* Retrieve & verify # of records in the name & creation order indices */
    if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
    if(name_count != corder_count) TEST_ERROR

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR


    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Open group created */
    if((group_id = H5Gopen2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Verify state of group */
    if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

    /* Retrieve & verify # of records in the name & creation order indices */
    if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
    if(name_count != corder_count) TEST_ERROR

    /* Delete several links from group, until it resumes compact form */
    for(u = max_compact; u >= min_dense; u--) {
        sprintf(objname, "filler %u", u);
        if(H5Ldelete(group_id, objname, H5P_DEFAULT) < 0) TEST_ERROR

        /* Verify state of group */
        if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
        if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
        if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

        /* Retrieve & verify # of records in the name & creation order indices */
        if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
        if(name_count != corder_count) TEST_ERROR
    } /* end for */

    /* Delete another link, to push group into compact form */
    sprintf(objname, "filler %u", (min_dense - 1));
    if(H5Ldelete(group_id, objname, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify state of group */
    if(H5G_has_links_test(group_id, &nlinks) != TRUE) TEST_ERROR
    if(nlinks != (min_dense - 1)) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR

    /* Re-add links to get back into dense form */
    for(u = (min_dense - 1); u < (max_compact + 1); u++) {
        sprintf(objname, "filler %u", u);
        if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
        if(H5Gclose(group_id2) < 0) TEST_ERROR
    } /* end for */

    /* Verify state of group */
    if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
    if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
    if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

    /* Retrieve & verify # of records in the name & creation order indices */
    if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
    if(name_count != corder_count) TEST_ERROR

    /* Delete all the links */
    for(u = max_compact; u > 0; u--) {
        sprintf(objname, "filler %u", u);
        if(H5Ldelete(group_id, objname, H5P_DEFAULT) < 0) TEST_ERROR
    } /* end for */
    sprintf(objname, "filler %u", 0);
    if(H5Ldelete(group_id, objname, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close the group */
    if(H5Gclose(group_id) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR

    /* Get the size of the file now */
    if((file_size = h5_get_file_size(filename)) < 0) TEST_ERROR
    if(file_size != empty_size) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end corder_transition() */


/*-------------------------------------------------------------------------
 * Function:    corder_delete
 *
 * Purpose:     Create a group with creation order indices and verify correct
 *              deletion of creation order index when the group is in dense
 *              storage form and the group is unlinked
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 30, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
corder_delete(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1), group_id2 = (-1);	/* Group IDs */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    hsize_t     name_count;             /* # of records in name index */
    hsize_t     corder_count;           /* # of records in creation order index */
    hbool_t     reopen_file;            /* Whether to re-open the file before deleting group */
    h5_stat_size_t       empty_size;             /* Size of empty file */
    h5_stat_size_t       file_size;              /* Size of file after operating on it */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    unsigned    u;                      /* Local index variable */

    TESTING("deleting group with creation order indexing in dense form")

    /* Loop to leave file open when deleting group, or to close & re-open file
     *  before deleting group */
    for(reopen_file = FALSE; reopen_file <= TRUE; reopen_file++) {
        /* Create file */
        h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
        if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR

        /* Get the size of an empty file */
        if((empty_size = h5_get_file_size(filename)) < 0) TEST_ERROR

        /* Re-open the file */
        if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

        /* Create group creation property list */
        if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) FAIL_STACK_ERROR

        /* Set creation order tracking & indexing on group */
        if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED)) < 0) TEST_ERROR

        /* Query the group creation properties */
        if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) FAIL_STACK_ERROR

        /* Increase estimated link info, so the group's object header is large
         *      enough to hold all the link messages in one chunk
         */
        if(H5Pset_est_link_info(gcpl_id, max_compact, CORDER_EST_ENTRY_LEN) < 0) TEST_ERROR

        /* Create group with creation order indexing & tracking on */
        if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

        /* Close the group creation property list */
        if(H5Pclose(gcpl_id) < 0) FAIL_STACK_ERROR

        /* Create links until the group is in dense form */
        for(u = 0; u < max_compact * 2; u++) {
            sprintf(objname, "filler %u", u);
            if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
            if(H5Gclose(group_id2) < 0) FAIL_STACK_ERROR
        } /* end for */

        /* Verify state of group */
        if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
        if(H5G_has_stab_test(group_id) == TRUE) TEST_ERROR
        if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

        /* Retrieve & verify # of records in the name & creation order indices */
        if(H5G_new_dense_info_test(group_id, &name_count, &corder_count) < 0) TEST_ERROR
        if(name_count != corder_count) TEST_ERROR

        /* Close the group */
        if(H5Gclose(group_id) < 0) FAIL_STACK_ERROR

        /* Check for deleting group without re-opening file */
        if(!reopen_file)
            /* Delete the group with the creation order index */
            if(H5Ldelete(file_id, CORDER_GROUP_NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

        /* Close the file */
        if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR

        /* Check for deleting group after re-opening file */
        if(reopen_file) {
            /* Re-open the file */
            if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

            /* Delete the group with the creation order index */
            if(H5Ldelete(file_id, CORDER_GROUP_NAME, H5P_DEFAULT) < 0) FAIL_STACK_ERROR

            /* Close the file */
            if(H5Fclose(file_id) < 0) FAIL_STACK_ERROR
        } /* end if */

        /* Get the size of the file now */
        if((file_size = h5_get_file_size(filename)) < 0) TEST_ERROR
        if(file_size != empty_size) TEST_ERROR
    } /* end for */

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end corder_delete() */


/*-------------------------------------------------------------------------
 * Function:    link_info_by_idx_check
 *
 * Purpose:     Support routine for link_info_by_idx, to verify the link
 *              info is correct for a link
 *
 * Note:	This routine assumes that the links have been inserted in the
 *              group in alphabetical order.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_info_by_idx_check(hid_t group_id, const char *linkname, hsize_t n,
    hbool_t hard_link, hbool_t use_index)
{
    char tmpname[NAME_BUF_SIZE];        /* Temporary link name */
    char valname[NAME_BUF_SIZE];        /* Link value name */
    char tmpval[NAME_BUF_SIZE];         /* Temporary link value */
    H5L_info_t  linfo;                  /* Link info struct */

    /* Make link value for increasing/native order queries */
    sprintf(valname, "value %02u", (unsigned)n);

    /* Verify the link information for first link, in increasing creation order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != 0) TEST_ERROR

    /* Verify the link information for new link, in increasing creation order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, n, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != (int64_t)n) TEST_ERROR

    /* Verify value for new soft link, in increasing creation order */
    if(!hard_link) {
        HDmemset(tmpval, 0, (size_t)NAME_BUF_SIZE);
        if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, n, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
        if(HDstrcmp(valname, tmpval)) TEST_ERROR
    } /* end if */

    /* Verify the name for new link, in increasing creation order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, n, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
    if(HDstrcmp(linkname, tmpname)) TEST_ERROR

    /* Don't test "native" order if there is no creation order index, since
     *  there's not a good way to easily predict the link's order in the name
     *  index.
     */
    if(use_index) {
        /* Verify the link information for first link, in native creation order (which is increasing) */
        HDmemset(&linfo, 0, sizeof(linfo));
        if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
        if(linfo.corder != 0) TEST_ERROR

        /* Verify the link information for new link, in native creation order (which is increasing) */
        HDmemset(&linfo, 0, sizeof(linfo));
        if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, n, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
        if(linfo.corder != (int64_t)n) TEST_ERROR

        /* Verify value for new soft link, in native creation order (which is increasing) */
        if(!hard_link) {
            HDmemset(tmpval, 0, (size_t)NAME_BUF_SIZE);
            if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, n, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(HDstrcmp(valname, tmpval)) TEST_ERROR
        } /* end if */

        /* Verify the name for new link, in native creation order (which is increasing) */
        HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
        if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, n, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
        if(HDstrcmp(linkname, tmpname)) TEST_ERROR
    } /* end if */

    /* Verify the link information for first link, in decreasing creation order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, n, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != 0) TEST_ERROR

    /* Verify the link information for new link, in decreasing creation order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != (int64_t)n) TEST_ERROR

    /* Verify value for new soft link, in decreasing creation order */
    if(!hard_link) {
        HDmemset(tmpval, 0, (size_t)NAME_BUF_SIZE);
        if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)0, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
        if(HDstrcmp(valname, tmpval)) TEST_ERROR
    } /* end if */

    /* Verify the name for new link, in decreasing creation order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
    if(HDstrcmp(linkname, tmpname)) TEST_ERROR


    /* Verify the link information for first link, in increasing link name order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != 0) TEST_ERROR

    /* Verify the link information for new link, in increasing link name order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, n, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != (int64_t)n) TEST_ERROR

    /* Verify value for new soft link, in increasing link name order */
    if(!hard_link) {
        HDmemset(tmpval, 0, (size_t)NAME_BUF_SIZE);
        if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, n, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
        if(HDstrcmp(valname, tmpval)) TEST_ERROR
    } /* end if */

    /* Verify the name for new link, in increasing link name order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, n, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
    if(HDstrcmp(linkname, tmpname)) TEST_ERROR

    /* Don't test "native" order queries on link name order, since there's not
     *  a good way to easily predict the order of the links in the name index.
     */

    /* Verify the link information for first link, in decreasing link name order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, n, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != 0) TEST_ERROR

    /* Verify the link information for new link, in decreasing link name order */
    HDmemset(&linfo, 0, sizeof(linfo));
    if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.corder != (int64_t)n) TEST_ERROR

    /* Verify value for new soft link, in decreasing link name order */
    if(!hard_link) {
        HDmemset(tmpval, 0, (size_t)NAME_BUF_SIZE);
        if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)0, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
        if(HDstrcmp(valname, tmpval)) TEST_ERROR
    } /* end if */

    /* Verify the name for new link, in decreasing link name order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
    if(HDstrcmp(linkname, tmpname)) TEST_ERROR

    /* Success */
    return(0);

error:
    /* Failure */
    return(-1);
} /* end link_info_by_idx_check() */


/*-------------------------------------------------------------------------
 * Function:    link_info_by_idx
 *
 * Purpose:     Create a group with creation order indices and test querying
 *              info by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_info_by_idx(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    hbool_t     hard_link;              /* Create hard or soft link? */
    hbool_t     use_index;              /* Use index on creation order values */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    H5L_info_t  linfo;                  /* Link info struct */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value name */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        tmpname[NAME_BUF_SIZE]; /* Temporary link name */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Loop over creating hard or soft links */
    for(hard_link = FALSE; hard_link <= TRUE; hard_link++) {
        /* Loop over using index for creation order value */
        for(use_index = FALSE; use_index <= TRUE; use_index++) {
            if(hard_link) {
                if(use_index)
                    TESTING("querying info by index w/creation order index, using hard links")
                else
                    TESTING("querying info by index w/o creation order index, using hard links")
            } /* end if */
            else {
                if(use_index)
                    TESTING("querying info by index w/creation order index, using soft links")
                else
                    TESTING("querying info by index w/o creation order index, using soft links")
            } /* end else */

            /* Create file */
            h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
            if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

            /* Create group creation property list */
            if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

            /* Set creation order tracking & indexing on group */
            if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0))) < 0) TEST_ERROR

            /* Create group with creation order indexing & tracking on */
            if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

            /* Query the group creation properties */
            if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR

            /* Check for query on empty group */
            H5E_BEGIN_TRY {
                ret = H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, &linfo, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR
            H5E_BEGIN_TRY {
                ret = H5Lget_name_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR

            /* Create several links, up to limit of compact form */
            for(u = 0; u < max_compact; u++) {
                /* Make name for link */
                sprintf(objname, "filler %02u", u);

                /* Check for creating hard or soft link */
                if(hard_link) {
                    hid_t group_id2;	        /* Group ID */

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
                    if(H5Gclose(group_id2) < 0) TEST_ERROR
                } /* end if */
                else {
                    /* Make value for link */
                    sprintf(valname, "value %02u", u);

                    /* Create soft link */
                    if(H5Lcreate_soft(valname, group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end else */

                /* Verify link information for new link */
                if(link_info_by_idx_check(group_id, objname, (hsize_t)u, hard_link, use_index) < 0) TEST_ERROR
            } /* end for */

            /* Verify state of group */
            if(H5G_has_links_test(group_id, NULL) != TRUE) TEST_ERROR

            /* Check for out of bound offset queries */
            H5E_BEGIN_TRY {
                ret = H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, &linfo, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR
            H5E_BEGIN_TRY {
                ret = H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)u, &linfo, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR
            H5E_BEGIN_TRY {
                ret = H5Lget_name_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR

            /* Create more links, to push group into dense form */
            for(; u < (max_compact * 2); u++) {
                /* Make name for link */
                sprintf(objname, "filler %02u", u);

                /* Check for creating hard or soft link */
                if(hard_link) {
                    hid_t group_id2;	        /* Group ID */

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
                    if(H5Gclose(group_id2) < 0) TEST_ERROR
                } /* end if */
                else {
                    /* Make value for link */
                    sprintf(valname, "value %02u", u);

                    /* Create soft link */
                    if(H5Lcreate_soft(valname, group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end else */

                /* Verify state of group */
                if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

                /* Verify link information for new link */
                if(link_info_by_idx_check(group_id, objname, (hsize_t)u, hard_link, use_index) < 0) TEST_ERROR
            } /* end for */

            /* Check for out of bound offset queries */
            H5E_BEGIN_TRY {
                ret = H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, &linfo, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR
            H5E_BEGIN_TRY {
                ret = H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)u, &linfo, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR
            H5E_BEGIN_TRY {
                ret = H5Lget_name_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
            } H5E_END_TRY;
            if(ret >= 0) TEST_ERROR

            /* Close the group */
            if(H5Gclose(group_id) < 0) TEST_ERROR

            /* Close the group creation property list */
            if(H5Pclose(gcpl_id) < 0) TEST_ERROR

            /* Close the file */
            if(H5Fclose(file_id) < 0) TEST_ERROR

            PASSED();
        } /* end for */
    } /* end for */

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end link_info_by_idx() */


/*-------------------------------------------------------------------------
 * Function:    link_info_by_idx_old
 *
 * Purpose:     Create a old-format group and test querying
 *              info by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_info_by_idx_old(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1), group_id2 = (-1);	/* Group IDs */
    hbool_t     hard_link;              /* Create hard or soft link? */
    H5L_info_t  linfo;                  /* Link info struct */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value name */
    char        filename[NAME_BUF_SIZE];/* File name */
    haddr_t     objno[CORDER_NLINKS];   /* Addresses of the objects created */
    char        tmpname[NAME_BUF_SIZE]; /* Temporary link name */
    char        tmpval[NAME_BUF_SIZE];  /* Temporary link value */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Loop over creating hard or soft links */
    for(hard_link = FALSE; hard_link <= TRUE; hard_link++) {
        if(hard_link)
            TESTING("querying info by index in old-style group, using hard links")
        else
            TESTING("querying info by index in old-style group, using soft links")

        /* Create file */
        h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
        if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create group to operate on */
        if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        /* Create several links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Check for creating hard or soft link */
            if(hard_link) {
                H5O_info_t oi;                  /* Buffer for querying object's info */

                /* Create group */
                if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                /* Retrieve group's address on disk */
                if(H5Oget_info(group_id2, ".", &oi, H5P_DEFAULT) < 0) TEST_ERROR
                objno[u] = oi.addr;

                /* Close group */
                if(H5Gclose(group_id2) < 0) TEST_ERROR
            } /* end if */
            else {
                /* Make value for link */
                sprintf(valname, "value %02u", u);

                /* Create soft link */
                if(H5Lcreate_soft(valname, group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
            } /* end else */
        } /* end for */

        /* Verify link information for created links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            unsigned dec_u = CORDER_NLINKS - (u + 1);       /* Decreasing mapped index */

            /* Make link name for increasing/native order queries */
            sprintf(objname, "filler %02u", u);

            /* Make link value for increasing/native order queries */
            sprintf(valname, "value %02u", u);

            /* Verify link information (in increasing order) */
            if(hard_link) {
                if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)u, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                if(H5F_addr_ne(linfo.u.address, objno[u])) TEST_ERROR
            } /* end if */
            else {
                if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)u, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
                if(HDstrcmp(valname, tmpval)) TEST_ERROR
            } /* end else */

            /* Verify link name (in increasing order) */
            if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(HDstrcmp(objname, tmpname)) TEST_ERROR


            /* Verify link information (in native order - native is increasing) */
            if(hard_link) {
                if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_NATIVE, (hsize_t)u, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                if(H5F_addr_ne(linfo.u.address, objno[u])) TEST_ERROR
            } /* end if */
            else {
                if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_NATIVE, (hsize_t)u, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
                if(HDstrcmp(valname, tmpval)) TEST_ERROR
            } /* end else */

            /* Verify link name (in native order - native is increasing) */
            if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_NATIVE, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(HDstrcmp(objname, tmpname)) TEST_ERROR


            /* Make link name for decreasing order queries */
            sprintf(objname, "filler %02u", dec_u);

            /* Make link value for decreasing order queries */
            sprintf(valname, "value %02u", dec_u);

            /* Verify link information (in decreasing order) */
            if(hard_link) {
                if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)u, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                if(H5F_addr_ne(linfo.u.address, objno[dec_u])) TEST_ERROR
            } /* end if */
            else {
                if(H5Lget_val_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)u, tmpval, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
                if(HDstrcmp(valname, tmpval)) TEST_ERROR
            } /* end else */

            /* Verify link name (in decreasing order) */
            if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(HDstrcmp(objname, tmpname)) TEST_ERROR
        } /* end for */

        /* Check for creation order index queries */
        H5E_BEGIN_TRY {
            ret = H5Lget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, &linfo, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR
        H5E_BEGIN_TRY {
            ret = H5Lget_name_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Verify state of group */
        if(H5G_has_stab_test(group_id) != TRUE) TEST_ERROR

        /* Close the group */
        if(H5Gclose(group_id) < 0) TEST_ERROR

        /* Close the file */
        if(H5Fclose(file_id) < 0) TEST_ERROR

        PASSED();
    } /* end for */

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end link_info_by_idx_old() */


/*-------------------------------------------------------------------------
 * Function:    delete_by_idx
 *
 * Purpose:     Create a group with creation order indices and test deleting
 *              links by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
delete_by_idx(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    H5_index_t idx_type;               /* Type of index to operate on */
    H5_iter_order_t order;              /* Order within in the index */
    hbool_t     use_index;              /* Use index on creation order values */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    H5L_info_t  linfo;                  /* Link info struct */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        tmpname[NAME_BUF_SIZE]; /* Temporary link name */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Loop over operating on different indices on link fields */
    for(idx_type = H5_INDEX_NAME; idx_type <=H5_INDEX_CRT_ORDER; idx_type++) {
        /* Loop over operating in different orders */
        for(order = H5_ITER_INC; order <=H5_ITER_DEC; order++) {
            /* Loop over using index for creation order value */
            for(use_index = FALSE; use_index <= TRUE; use_index++) {
                /* Print appropriate test message */
                if(idx_type == H5_INDEX_CRT_ORDER) {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("deleting links by creation order index in increasing order w/creation order index")
                        else
                            TESTING("deleting links by creation order index in increasing order w/o creation order index")
                    } /* end if */
                    else {
                        if(use_index)
                            TESTING("deleting links by creation order index in decreasing order w/creation order index")
                        else
                            TESTING("deleting links by creation order index in decreasing order w/o creation order index")
                    } /* end else */
                } /* end if */
                else {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("deleting links by name index in increasing order w/creation order index")
                        else
                            TESTING("deleting links by name index in increasing order w/o creation order index")
                    } /* end if */
                    else {
                        if(use_index)
                            TESTING("deleting links by name index in decreasing order w/creation order index")
                        else
                            TESTING("deleting links by name index in decreasing order w/o creation order index")
                    } /* end else */
                } /* end else */

                /* Create file */
                h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
                if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

                /* Create group creation property list */
                if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

                /* Set creation order tracking & indexing on group */
                if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0))) < 0) TEST_ERROR

                /* Create group with creation order tracking on */
                if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

                /* Query the group creation properties */
                if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR


                /* Delete links from one end */


                /* Check for deletion on empty group */
                H5E_BEGIN_TRY {
                    ret = H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Create several links, up to limit of compact form */
                for(u = 0; u < max_compact; u++) {
                    hid_t group_id2;	        /* Group ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
                    if(H5Gclose(group_id2) < 0) TEST_ERROR

                    /* Verify link information for new link */
                    if(link_info_by_idx_check(group_id, objname, (hsize_t)u, TRUE, use_index) < 0) TEST_ERROR
                } /* end for */

                /* Verify state of group (compact) */
                if(H5G_has_links_test(group_id, NULL) != TRUE) TEST_ERROR

                /* Check for out of bound deletion */
                H5E_BEGIN_TRY {
                    ret = H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Delete links from compact group */
                for(u = 0; u < (max_compact - 1); u++) {
                    /* Delete first link in appropriate order */
                    if(H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Verify the link information for first link in appropriate order */
                    HDmemset(&linfo, 0, sizeof(linfo));
                    if(H5Lget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC) {
                        if(linfo.corder != (u + 1)) TEST_ERROR
                    } /* end if */
                    else {
                        if(linfo.corder != (max_compact - (u + 2))) TEST_ERROR
                    } /* end else */

                    /* Verify the name for first link in appropriate order */
                    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
                    if(H5Lget_name_by_idx(group_id, ".", idx_type, order, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC)
                        sprintf(objname, "filler %02u", (u + 1));
                    else
                        sprintf(objname, "filler %02u", (max_compact - (u + 2)));
                    if(HDstrcmp(objname, tmpname)) TEST_ERROR
                } /* end for */

                /* Delete last link */
                if(H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

                /* Verify state of group (empty) */
                if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR

                /* Create more links, to push group into dense form */
                for(u = 0; u < (max_compact * 2); u++) {
                    hid_t group_id2;	        /* Group ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
                    if(H5Gclose(group_id2) < 0) TEST_ERROR

                    /* Verify state of group (dense) */
                    if(u >= max_compact)
                        if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

                    /* Verify link information for new link */
                    if(link_info_by_idx_check(group_id, objname, (hsize_t)u, TRUE, use_index) < 0) TEST_ERROR
                } /* end for */

                /* Check for out of bound deletion again */
                H5E_BEGIN_TRY {
                    ret = H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Delete links from dense group, in appropriate order */
                for(u = 0; u < ((max_compact * 2) - 1); u++) {
                    /* Delete first link in appropriate order */
                    if(H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Verify the link information for first link in appropriate order */
                    HDmemset(&linfo, 0, sizeof(linfo));
                    if(H5Lget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC) {
                        if(linfo.corder != (u + 1)) TEST_ERROR
                    } /* end if */
                    else {
                        if(linfo.corder != ((max_compact * 2) - (u + 2))) TEST_ERROR
                    } /* end else */

                    /* Verify the name for first link in appropriate order */
                    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
                    if(H5Lget_name_by_idx(group_id, ".", idx_type, order, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC)
                        sprintf(objname, "filler %02u", (u + 1));
                    else
                        sprintf(objname, "filler %02u", ((max_compact * 2) - (u + 2)));
                    if(HDstrcmp(objname, tmpname)) TEST_ERROR
                } /* end for */

                /* Delete last link */
                if(H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

                /* Verify state of group (empty) */
                if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
                if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR

                /* Check for deletion on empty group again */
                H5E_BEGIN_TRY {
                    ret = H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR


                /* Delete links in middle */


                /* Create more links, to push group into dense form */
                for(u = 0; u < (max_compact * 2); u++) {
                    hid_t group_id2;	        /* Group ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
                    if(H5Gclose(group_id2) < 0) TEST_ERROR

                    /* Verify state of group (dense) */
                    if(u >= max_compact)
                        if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

                    /* Verify link information for new link */
                    if(link_info_by_idx_check(group_id, objname, (hsize_t)u, TRUE, use_index) < 0) TEST_ERROR
                } /* end for */

                /* Delete every other link from dense group, in appropriate order */
                for(u = 0; u < max_compact; u++) {
                    /* Delete link */
                    if(H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Verify the link information for current link in appropriate order */
                    HDmemset(&linfo, 0, sizeof(linfo));
                    if(H5Lget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)u, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC) {
                        if(linfo.corder != ((u * 2) + 1)) TEST_ERROR
                    } /* end if */
                    else {
                        if(linfo.corder != ((max_compact * 2) - ((u * 2) + 2))) TEST_ERROR
                    } /* end else */

                    /* Verify the name for current link in appropriate order */
                    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
                    if(H5Lget_name_by_idx(group_id, ".", idx_type, order, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC)
                        sprintf(objname, "filler %02u", ((u * 2) + 1));
                    else
                        sprintf(objname, "filler %02u", ((max_compact * 2) - ((u * 2) + 2)));
                    if(HDstrcmp(objname, tmpname)) TEST_ERROR
                } /* end for */

                /* Delete remaining links from dense group, in appropriate order */
                for(u = 0; u < (max_compact - 1); u++) {
                    /* Delete link */
                    if(H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Verify the link information for first link in appropriate order */
                    HDmemset(&linfo, 0, sizeof(linfo));
                    if(H5Lget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC) {
                        if(linfo.corder != ((u * 2) + 3)) TEST_ERROR
                    } /* end if */
                    else {
                        if(linfo.corder != ((max_compact * 2) - ((u * 2) + 4))) TEST_ERROR
                    } /* end else */

                    /* Verify the name for first link in appropriate order */
                    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
                    if(H5Lget_name_by_idx(group_id, ".", idx_type, order, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
                    if(order == H5_ITER_INC)
                        sprintf(objname, "filler %02u", ((u * 2) + 3));
                    else
                        sprintf(objname, "filler %02u", ((max_compact * 2) - ((u * 2) + 4)));
                    if(HDstrcmp(objname, tmpname)) TEST_ERROR
                } /* end for */

                /* Delete last link */
                if(H5Ldelete_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

                /* Verify state of group (empty) */
                if(H5G_has_links_test(group_id, NULL) == TRUE) TEST_ERROR
                if(H5G_is_new_dense_test(group_id) == TRUE) TEST_ERROR



                /* Close the group */
                if(H5Gclose(group_id) < 0) TEST_ERROR

                /* Close the group creation property list */
                if(H5Pclose(gcpl_id) < 0) TEST_ERROR

                /* Close the file */
                if(H5Fclose(file_id) < 0) TEST_ERROR

                PASSED();
            } /* end for */
        } /* end for */
    } /* end for */

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return -1;
} /* end delete_by_idx() */


/*-------------------------------------------------------------------------
 * Function:    delete_by_idx_old
 *
 * Purpose:     Create a old-format group and test deleting
 *              links by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, November 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
delete_by_idx_old(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1), group_id2 = (-1);	/* Group IDs */
    H5L_info_t  linfo;                  /* Link info struct */
    H5_iter_order_t order;              /* Order within in the index */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    haddr_t     objno[CORDER_NLINKS];   /* Addresses of the objects created */
    char        tmpname[NAME_BUF_SIZE]; /* Temporary link name */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Loop over operating in different orders */
    for(order = H5_ITER_INC; order <=H5_ITER_DEC; order++) {
        /* Print test banner */
        if(order == H5_ITER_INC)
            TESTING("deleting links by index in increasing order in old-style group")
        else
            TESTING("deleting links by index in decreasing order in old-style group")

        /* Create file */
        h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
        if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create group to operate on */
        if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR


        /* Delete links from one end */


        /* Check for deletion in empty group */
        H5E_BEGIN_TRY {
            ret = H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Create several links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            H5O_info_t oi;                  /* Buffer for querying object's info */

            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Create group */
            if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

            /* Retrieve group's address on disk */
            if(H5Oget_info(group_id2, ".", &oi, H5P_DEFAULT) < 0) TEST_ERROR
            objno[u] = oi.addr;

            /* Close group */
            if(H5Gclose(group_id2) < 0) TEST_ERROR
        } /* end for */

        /* Check for bad index type deletion */
        H5E_BEGIN_TRY {
            ret = H5Ldelete_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, order, (hsize_t)0, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Check for out of bounds deletion */
        H5E_BEGIN_TRY {
            ret = H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Delete links, in appropriate order */
        for(u = 0; u < (CORDER_NLINKS - 1); u++) {
            unsigned dec_u = CORDER_NLINKS - (u + 2);       /* Decreasing mapped index */

            /* Delete first link in appropriate order */
            if(H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

            /* Verify the link information for first link in appropriate order */
            HDmemset(&linfo, 0, sizeof(linfo));
            if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
            if(order == H5_ITER_INC) {
                if(H5F_addr_ne(linfo.u.address, objno[u + 1])) TEST_ERROR
            } /* end if */
            else {
                if(H5F_addr_ne(linfo.u.address, objno[dec_u])) TEST_ERROR
            } /* end else */

            /* Verify the name for first link in appropriate order */
            HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
            if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(order == H5_ITER_INC)
                sprintf(objname, "filler %02u", (u + 1));
            else
                sprintf(objname, "filler %02u", dec_u);
            if(HDstrcmp(objname, tmpname)) TEST_ERROR
        } /* end for */

        /* Delete last link */
        if(H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

        /* Check for deletion in empty group (again) */
        H5E_BEGIN_TRY {
            ret = H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Verify state of group */
        if(H5G_has_stab_test(group_id) != TRUE) TEST_ERROR


        /* Delete links in middle */


        /* Create several links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            H5O_info_t oi;                  /* Buffer for querying object's info */

            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Create group */
            if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

            /* Retrieve group's address on disk */
            if(H5Oget_info(group_id2, ".", &oi, H5P_DEFAULT) < 0) TEST_ERROR
            objno[u] = oi.addr;

            /* Close group */
            if(H5Gclose(group_id2) < 0) TEST_ERROR
        } /* end for */

        /* Delete every other link from group, in appropriate order */
        for(u = 0; u < (CORDER_NLINKS / 2); u++) {
            unsigned dec_u = CORDER_NLINKS - ((u * 2) + 2);       /* Decreasing mapped index */

            /* Delete link */
            if(H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, H5P_DEFAULT) < 0) TEST_ERROR

            /* Verify the link information for current link in appropriate order */
            HDmemset(&linfo, 0, sizeof(linfo));
            if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
            if(order == H5_ITER_INC) {
                if(H5F_addr_ne(linfo.u.address, objno[(u * 2) + 1])) TEST_ERROR
            } /* end if */
            else {
                if(H5F_addr_ne(linfo.u.address, objno[dec_u])) TEST_ERROR
            } /* end else */

            /* Verify the name for current link in appropriate order */
            HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
            if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(order == H5_ITER_INC)
                sprintf(objname, "filler %02u", ((u * 2) + 1));
            else
                sprintf(objname, "filler %02u", dec_u);
            if(HDstrcmp(objname, tmpname)) TEST_ERROR
        } /* end for */

        /* Delete remaining links from group, in appropriate order */
        for(u = 0; u < ((CORDER_NLINKS / 2) - 1); u++) {
            unsigned dec_u = CORDER_NLINKS - ((u * 2) + 4);       /* Decreasing mapped index */

            /* Delete link */
            if(H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

            /* Verify the link information for first link in appropriate order */
            HDmemset(&linfo, 0, sizeof(linfo));
            if(H5Lget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, &linfo, H5P_DEFAULT) < 0) TEST_ERROR
            if(order == H5_ITER_INC) {
                if(H5F_addr_ne(linfo.u.address, objno[(u * 2) + 3])) TEST_ERROR
            } /* end if */
            else {
                if(H5F_addr_ne(linfo.u.address, objno[dec_u])) TEST_ERROR
            } /* end else */

            /* Verify the name for first link in appropriate order */
            HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
            if(H5Lget_name_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT) < 0) TEST_ERROR
            if(order == H5_ITER_INC)
                sprintf(objname, "filler %02u", ((u * 2) + 3));
            else
                sprintf(objname, "filler %02u", dec_u);
            if(HDstrcmp(objname, tmpname)) TEST_ERROR
        } /* end for */

        /* Delete last link */
        if(H5Ldelete_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, H5P_DEFAULT) < 0) TEST_ERROR

        /* Verify state of group */
        if(H5G_has_stab_test(group_id) != TRUE) TEST_ERROR

        /* Close the group */
        if(H5Gclose(group_id) < 0) TEST_ERROR

        /* Close the file */
        if(H5Fclose(file_id) < 0) TEST_ERROR

        PASSED();
    } /* end for */

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    return -1;
} /* end delete_by_idx_old() */


/*-------------------------------------------------------------------------
 * Function:    link_iterate_cb
 *
 * Purpose:     Callback routine for iterating over links in group
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_iterate_cb(hid_t group_id, const char *link_name, const H5L_info_t *info,
    void *_op_data)
{
    link_iter_info_t *op_data = (link_iter_info_t *)_op_data;   /* User data */
    char objname[NAME_BUF_SIZE]; /* Object name */
    H5L_info_t my_info;         /* Local link info */

#ifdef QAK
HDfprintf(stderr, "link_name = '%s'\n", link_name);
if(info)
    HDfprintf(stderr, "info->corder = %Hd\n", info->corder);
HDfprintf(stderr, "op_data->curr = %Hd\n", op_data->curr);
#endif /* QAK */

    /* Increment # of times the callback was called */
    op_data->ncalled++;

    /* Get the link information directly to compare */
    if(H5Lget_info(group_id, link_name, &my_info, H5P_DEFAULT) < 0) 
        return(H5_ITER_ERROR);

    /* Check more things for link iteration (vs. group iteration) */
    if(info) {
        /* Check for correct order of iteration */
        /* (if we are operating in increasing or decreasing order) */
        if(op_data->order != H5_ITER_NATIVE)
            if(info->corder != op_data->curr)
                return(H5_ITER_ERROR);

        /* Compare link info structs */
        if(info->type != my_info.type)
            return(H5_ITER_ERROR);
        if(info->corder_valid != my_info.corder_valid)
            return(H5_ITER_ERROR);
        if(info->corder != my_info.corder)
            return(H5_ITER_ERROR);
        if(info->cset != my_info.cset)
            return(H5_ITER_ERROR);
        if(H5F_addr_ne(info->u.address, my_info.u.address))
            return(H5_ITER_ERROR);
    } /* end if */

    /* Verify name of link */
    sprintf(objname, "filler %02u", (unsigned)my_info.corder);
    if(HDstrcmp(link_name, objname))
        return(H5_ITER_ERROR);

    /* Check if we've visited this link before */
    if((size_t)op_data->curr >= op_data->max_visit)
        return(H5_ITER_ERROR);
    if(op_data->visited[op_data->curr])
        return(H5_ITER_ERROR);
    op_data->visited[op_data->curr] = TRUE;

    /* Advance to next value, in correct direction */
    if(op_data->order != H5_ITER_DEC)
        op_data->curr++;
    else
        op_data->curr--;

    /* Check for stopping in the middle of iterating */
    if(op_data->stop > 0)
        if(--op_data->stop == 0)
            return(CORDER_ITER_STOP);

    return(H5_ITER_CONT);
} /* end link_iterate_cb() */

#ifndef H5_NO_DEPRECATED_SYMBOLS

/*-------------------------------------------------------------------------
 * Function:    group_iterate_cb
 *
 * Purpose:     Callback routine for iterating over links in group with
 *              H5Giterate()
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
group_iterate_cb(hid_t group_id, const char *link_name, void *_op_data)
{
    return(link_iterate_cb(group_id, link_name, NULL, _op_data));
} /* end group_iterate_cb() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */


/*-------------------------------------------------------------------------
 * Function:    link_iterate_fail_cb
 *
 * Purpose:     Callback routine for iterating over links in group that
 *              always returns failure
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_iterate_fail_cb(hid_t UNUSED group_id, const char UNUSED *link_name,
    const H5L_info_t UNUSED *info, void UNUSED *_op_data)
{
    return(H5_ITER_ERROR);
} /* end link_iterate_fail_cb() */


/*-------------------------------------------------------------------------
 * Function:    link_iterate_check
 *
 * Purpose:     Check iteration over links in a group
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_iterate_check(hid_t group_id, H5_index_t idx_type, H5_iter_order_t order,
    unsigned max_links, link_iter_info_t *iter_info)
{
    unsigned    v;                      /* Local index variable */
    hsize_t     skip;                   /* # of links to skip in group */
#ifndef H5_NO_DEPRECATED_SYMBOLS
    int         gskip;                  /* # of links to skip in group, with H5Giterate */
#endif /* H5_NO_DEPRECATED_SYMBOLS */
    herr_t      ret;                    /* Generic return value */

    /* Iterate over links in group */
    iter_info->nskipped = (unsigned)(skip = 0);
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Literate(group_id, ".", idx_type, order, &skip, link_iterate_cb, iter_info, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(skip != max_links) TEST_ERROR
    for(v = 0; v < max_links; v++)
        if(iter_info->visited[v] == FALSE) TEST_ERROR


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Iterate over links in group, with H5Giterate */
    iter_info->nskipped = gskip = 0;
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Giterate(group_id, ".", &gskip, group_iterate_cb, iter_info) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(gskip != (int)max_links) TEST_ERROR
    for(v = 0; v < max_links; v++)
        if(iter_info->visited[v] == FALSE) TEST_ERROR
#endif /* H5_NO_DEPRECATED_SYMBOLS */


    /* Skip over some links in group */
    iter_info->nskipped = (unsigned)(skip = max_links / 2);
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? skip : ((max_links - 1) - skip);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Literate(group_id, ".", idx_type, order, &skip, link_iterate_cb, iter_info, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(skip != max_links) TEST_ERROR
    if(order == H5_ITER_INC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v + (max_links / 2)] == FALSE) TEST_ERROR
    } /* end if */
    else if(order == H5_ITER_DEC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v] == FALSE) TEST_ERROR
    } /* end if */
    else {
        unsigned nvisit = 0;        /* # of links visited */

        HDassert(order == H5_ITER_NATIVE);
        for(v = 0; v < max_links; v++)
            if(iter_info->visited[v] == TRUE)
                nvisit++;

        if(nvisit != (max_links / 2)) TEST_ERROR
    } /* end else */


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Skip over some links in group, with H5Giterate */
    iter_info->nskipped = gskip = max_links / 2;
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? (unsigned)gskip : ((max_links - 1) - gskip);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Giterate(group_id, ".", &gskip, group_iterate_cb, iter_info) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(gskip != (int)max_links) TEST_ERROR
    if(order == H5_ITER_INC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v + (max_links / 2)] == FALSE) TEST_ERROR
    } /* end if */
    else if(order == H5_ITER_DEC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v] == FALSE) TEST_ERROR
    } /* end if */
    else {
        unsigned nvisit = 0;        /* # of links visited */

        HDassert(order == H5_ITER_NATIVE);
        for(v = 0; v < max_links; v++)
            if(iter_info->visited[v] == TRUE)
                nvisit++;

        if(nvisit != (max_links / 2)) TEST_ERROR
    } /* end else */
#endif /* H5_NO_DEPRECATED_SYMBOLS */


    /* Iterate over links in group, stopping in the middle */
    iter_info->nskipped = (unsigned)(skip = 0);
    iter_info->order = order;
    iter_info->stop = 3;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if((ret = H5Literate(group_id, ".", idx_type, order, &skip, link_iterate_cb, iter_info, H5P_DEFAULT)) < 0) TEST_ERROR
    if(ret != CORDER_ITER_STOP) TEST_ERROR
    if(iter_info->ncalled != 3) TEST_ERROR


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Iterate over links in group, stopping in the middle, with H5Giterate() */
    iter_info->nskipped = gskip = 0;
    iter_info->order = order;
    iter_info->stop = 3;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if((ret = H5Giterate(group_id, ".", &gskip, group_iterate_cb, iter_info)) < 0) TEST_ERROR
    if(ret != CORDER_ITER_STOP) TEST_ERROR
    if(iter_info->ncalled != 3) TEST_ERROR
#endif /* H5_NO_DEPRECATED_SYMBOLS */


    /* Check for iteration routine indicating failure */
    skip = 0;
    H5E_BEGIN_TRY {
        ret = H5Literate(group_id, ".", idx_type, order, &skip, link_iterate_fail_cb, NULL, H5P_DEFAULT);
    } H5E_END_TRY;
    if(ret >= 0) TEST_ERROR

    /* Success */
    return(0);

error:
    return(-1);
} /* end link_iterate_check() */


/*-------------------------------------------------------------------------
 * Function:    link_iterate
 *
 * Purpose:     Create a group with creation order indices and test iterating over
 *              links by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_iterate(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    H5_index_t idx_type;               /* Type of index to operate on */
    H5_iter_order_t order;              /* Order within in the index */
    hbool_t     use_index;              /* Use index on creation order values */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    link_iter_info_t iter_info;         /* Iterator info */
    hbool_t     *visited = NULL;        /* Array of flags for visiting links */
    hsize_t     skip;                   /* # of links to skip in group */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Allocate the "visited link" array */
    iter_info.max_visit = max_compact * 2;
    if(NULL == (visited = HDmalloc(sizeof(hbool_t) * iter_info.max_visit))) TEST_ERROR
    iter_info.visited = visited;

    /* Loop over operating on different indices on link fields */
    for(idx_type = H5_INDEX_NAME; idx_type <=H5_INDEX_CRT_ORDER; idx_type++) {
        /* Loop over operating in different orders */
        for(order = H5_ITER_INC; order <=H5_ITER_NATIVE; order++) {
            /* Loop over using index for creation order value */
            for(use_index = FALSE; use_index <= TRUE; use_index++) {
                /* Print appropriate test message */
                if(idx_type == H5_INDEX_CRT_ORDER) {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("iterating over links by creation order index in increasing order w/creation order index")
                        else
                            TESTING("iterating over links by creation order index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("iterating over links by creation order index in decreasing order w/creation order index")
                        else
                            TESTING("iterating over links by creation order index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("iterating over links by creation order index in native order w/creation order index")
                        else
                            TESTING("iterating over links by creation order index in native order w/o creation order index")
                    } /* end else */
                } /* end if */
                else {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("iterating over links by name index in increasing order w/creation order index")
                        else
                            TESTING("iterating over links by name index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("iterating over links by name index in decreasing order w/creation order index")
                        else
                            TESTING("iterating over links by name index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("iterating over links by name index in native order w/creation order index")
                        else
                            TESTING("iterating over links by name index in native order w/o creation order index")
                    } /* end else */
                } /* end else */

                /* Create file */
                h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
                if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

                /* Set creation order tracking & indexing on group */
                if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0))) < 0) TEST_ERROR

                /* Create group with creation order tracking on */
                if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR


                /* Check for iteration on empty group */
                /* (should be OK) */
                if(H5Literate(group_id, ".", idx_type, order, NULL, link_iterate_cb, NULL, H5P_DEFAULT) < 0) TEST_ERROR

                /* Create several links, up to limit of compact form */
                for(u = 0; u < max_compact; u++) {
                    hid_t group_id2;	        /* Group ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
                    if(H5Gclose(group_id2) < 0) TEST_ERROR
                } /* end for */

                /* Verify state of group (compact) */
                if(H5G_has_links_test(group_id, NULL) != TRUE) TEST_ERROR

                /* Check for out of bound iteration on compact group */
                skip = (hsize_t)u;
                H5E_BEGIN_TRY {
                    ret = H5Literate(group_id, ".", idx_type, order, &skip, link_iterate_cb, NULL, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Test iteration over links in compact group */
                if(link_iterate_check(group_id, idx_type, order, u, &iter_info) < 0) TEST_ERROR


                /* Create more links, to push group into dense form */
                for(; u < (max_compact * 2); u++) {
                    hid_t group_id2;	        /* Group ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
                    if(H5Gclose(group_id2) < 0) TEST_ERROR
                } /* end for */

                /* Verify state of group (dense) */
                if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

                /* Check for out of bound iteration on dense group */
                skip = (hsize_t)u;
                H5E_BEGIN_TRY {
                    ret = H5Literate(group_id, ".", idx_type, order, &skip, link_iterate_cb, NULL, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Test iteration over links in dense group */
                if(link_iterate_check(group_id, idx_type, order, u, &iter_info) < 0) TEST_ERROR


                /* Close the group */
                if(H5Gclose(group_id) < 0) TEST_ERROR

                /* Close the file */
                if(H5Fclose(file_id) < 0) TEST_ERROR

                PASSED();
            } /* end for */
        } /* end for */
    } /* end for */

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    /* Free resources */
    if(visited)
        HDfree(visited);

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    if(visited)
        HDfree(visited);

    return -1;
} /* end link_iterate() */


/*-------------------------------------------------------------------------
 * Function:    link_iterate_old_cb
 *
 * Purpose:     Callback routine for iterating over [old] links in group
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_iterate_old_cb(hid_t group_id, const char *link_name, const H5L_info_t *info,
    void *_op_data)
{
    link_iter_info_t *op_data = (link_iter_info_t *)_op_data;   /* User data */
    char objname[NAME_BUF_SIZE]; /* Object name */
    H5L_info_t my_info;         /* Local link info */

#ifdef QAK
HDfprintf(stderr, "link_name = '%s'\n", link_name);
if(info)
    HDfprintf(stderr, "info->corder = %Hd\n", info->corder);
HDfprintf(stderr, "op_data->curr = %Hd\n", op_data->curr);
#endif /* QAK */

    /* Increment # of times the callback was called */
    op_data->ncalled++;

    /* Get the link information directly to compare */
    if(H5Lget_info(group_id, link_name, &my_info, H5P_DEFAULT) < 0) 
        return(H5_ITER_ERROR);

    /* Check more things for link iteration (vs. group iteration) */
    if(info) {
        /* Compare link info structs */
        if(info->type != my_info.type)
            return(H5_ITER_ERROR);
        if(info->corder_valid != my_info.corder_valid)
            return(H5_ITER_ERROR);
        if(info->corder != my_info.corder)
            return(H5_ITER_ERROR);
        if(info->cset != my_info.cset)
            return(H5_ITER_ERROR);
        if(H5F_addr_ne(info->u.address, my_info.u.address))
            return(H5_ITER_ERROR);
    } /* end if */

    /* Verify name of link */
    sprintf(objname, "filler %02u", (info ? (unsigned)op_data->curr : (unsigned)((op_data->ncalled - 1) + op_data->nskipped)));
    if(HDstrcmp(link_name, objname))
        return(H5_ITER_ERROR);

    /* Check if we've visited this link before */
    if((size_t)op_data->curr >= op_data->max_visit)
        return(H5_ITER_ERROR);
    if(op_data->visited[op_data->curr])
        return(H5_ITER_ERROR);
    op_data->visited[op_data->curr] = TRUE;

    /* Advance to next value, in correct direction */
    if(op_data->order != H5_ITER_DEC)
        op_data->curr++;
    else
        op_data->curr--;

    /* Check for stopping in the middle of iterating */
    if(op_data->stop > 0)
        if(--op_data->stop == 0)
            return(CORDER_ITER_STOP);

    return(H5_ITER_CONT);
} /* end link_iterate_old_cb() */

#ifndef H5_NO_DEPRECATED_SYMBOLS

/*-------------------------------------------------------------------------
 * Function:    group_iterate_old_cb
 *
 * Purpose:     Callback routine for iterating over links in group with
 *              H5Giterate()
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
group_iterate_old_cb(hid_t group_id, const char *link_name, void *_op_data)
{
    return(link_iterate_old_cb(group_id, link_name, NULL, _op_data));
} /* end group_iterate_old_cb() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */


/*-------------------------------------------------------------------------
 * Function:    link_iterate_old_check
 *
 * Purpose:     Check iteration over [old] links in a group
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_iterate_old_check(hid_t group_id, H5_iter_order_t order,
    unsigned max_links, link_iter_info_t *iter_info)
{
    unsigned    v;                      /* Local index variable */
    hsize_t     skip;                   /* # of links to skip in group */
#ifndef H5_NO_DEPRECATED_SYMBOLS
    int         gskip;                  /* # of links to skip in group, with H5Giterate */
#endif /* H5_NO_DEPRECATED_SYMBOLS */
    herr_t      ret;                    /* Generic return value */

    /* Iterate over links in group */
    iter_info->nskipped = (unsigned)(skip = 0);
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Literate(group_id, ".", H5_INDEX_NAME, order, &skip, link_iterate_old_cb, iter_info, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(skip != max_links) TEST_ERROR
    for(v = 0; v < max_links; v++)
        if(iter_info->visited[v] == FALSE) TEST_ERROR


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Iterate over links in group, with H5Giterate */
    iter_info->nskipped = gskip = 0;
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Giterate(group_id, ".", &gskip, group_iterate_old_cb, iter_info) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(gskip != (int)max_links) TEST_ERROR
    for(v = 0; v < max_links; v++)
        if(iter_info->visited[v] == FALSE) TEST_ERROR
#endif /* H5_NO_DEPRECATED_SYMBOLS */


    /* Skip over some links in group */
    iter_info->nskipped = (unsigned)(skip = max_links / 2);
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? skip : ((max_links - 1) - skip);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Literate(group_id, ".", H5_INDEX_NAME, order, &skip, link_iterate_old_cb, iter_info, H5P_DEFAULT) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(skip != max_links) TEST_ERROR
    if(order == H5_ITER_INC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v + (max_links / 2)] == FALSE) TEST_ERROR
    } /* end if */
    else if(order == H5_ITER_DEC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v] == FALSE) TEST_ERROR
    } /* end if */
    else {
        unsigned nvisit = 0;        /* # of links visited */

        HDassert(order == H5_ITER_NATIVE);
        for(v = 0; v < max_links; v++)
            if(iter_info->visited[v] == TRUE)
                nvisit++;

        if(nvisit != (max_links / 2)) TEST_ERROR
    } /* end else */


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Skip over some links in group, with H5Giterate */
    iter_info->nskipped = gskip = max_links / 2;
    iter_info->order = order;
    iter_info->stop = -1;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? (unsigned)gskip : ((max_links - 1) - gskip);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if(H5Giterate(group_id, ".", &gskip, group_iterate_old_cb, iter_info) < 0) TEST_ERROR

    /* Verify that we visited all the links */
    if(gskip != (int)max_links) TEST_ERROR
    if(order == H5_ITER_INC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v + (max_links / 2)] == FALSE) TEST_ERROR
    } /* end if */
    else if(order == H5_ITER_DEC) {
        for(v = 0; v < (max_links / 2); v++)
            if(iter_info->visited[v] == FALSE) TEST_ERROR
    } /* end if */
    else {
        unsigned nvisit = 0;        /* # of links visited */

        HDassert(order == H5_ITER_NATIVE);
        for(v = 0; v < max_links; v++)
            if(iter_info->visited[v] == TRUE)
                nvisit++;

        if(nvisit != (max_links / 2)) TEST_ERROR
    } /* end else */
#endif /* H5_NO_DEPRECATED_SYMBOLS */


    /* Iterate over links in group, stopping in the middle */
    iter_info->nskipped = (unsigned)(skip = 0);
    iter_info->order = order;
    iter_info->stop = 3;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if((ret = H5Literate(group_id, ".", H5_INDEX_NAME, order, &skip, link_iterate_old_cb, iter_info, H5P_DEFAULT)) < 0) TEST_ERROR
    if(ret != CORDER_ITER_STOP) TEST_ERROR
    if(iter_info->ncalled != 3) TEST_ERROR


#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Iterate over links in group, stopping in the middle, with H5Giterate() */
    iter_info->nskipped = gskip = 0;
    iter_info->order = order;
    iter_info->stop = 3;
    iter_info->ncalled = 0;
    iter_info->curr = order != H5_ITER_DEC ? 0 : (max_links - 1);
    HDmemset(iter_info->visited, 0, sizeof(hbool_t) * iter_info->max_visit);
    if((ret = H5Giterate(group_id, ".", &gskip, group_iterate_old_cb, iter_info)) < 0) TEST_ERROR
    if(ret != CORDER_ITER_STOP) TEST_ERROR
    if(iter_info->ncalled != 3) TEST_ERROR
#endif /* H5_NO_DEPRECATED_SYMBOLS */


    /* Check for iteration routine indicating failure */
    skip = 0;
    H5E_BEGIN_TRY {
        ret = H5Literate(group_id, ".", H5_INDEX_NAME, order, &skip, link_iterate_fail_cb, NULL, H5P_DEFAULT);
    } H5E_END_TRY;
    if(ret >= 0) TEST_ERROR

    /* Success */
    return(0);

error:
    return(-1);
} /* end link_iterate_old_check() */


/*-------------------------------------------------------------------------
 * Function:    link_iterate_old
 *
 * Purpose:     Create a "old-style" group and test iterating over links by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
link_iterate_old(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    H5_iter_order_t order;              /* Order within in the index */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        filename[NAME_BUF_SIZE];/* File name */
    link_iter_info_t iter_info;         /* Iterator info */
    hbool_t     *visited = NULL;        /* Array of flags for visiting links */
    hsize_t     skip;                   /* # of links to skip in group */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Allocate the "visited link" array */
    iter_info.max_visit = CORDER_NLINKS;
    if(NULL == (visited = HDmalloc(sizeof(hbool_t) * iter_info.max_visit))) TEST_ERROR
    iter_info.visited = visited;

    /* Loop over operating in different orders */
    for(order = H5_ITER_INC; order <=H5_ITER_NATIVE; order++) {
        /* Print appropriate test message */
        if(order == H5_ITER_INC) {
            TESTING("iterating over links by name index in increasing order in old-style group")
        } /* end if */
        else if(order == H5_ITER_DEC) {
            TESTING("iterating over links by name index in decreasing order in old-style group")
        } /* end else */
        else {
            HDassert(order == H5_ITER_NATIVE);
            TESTING("iterating over links by name index in native order in old-style group")
        } /* end else */

        /* Create file */
        h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
        if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create group with creation order tracking on */
        if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR


        /* Check for iteration on empty group */
        /* (should be OK) */
        if(H5Literate(group_id, ".", H5_INDEX_NAME, order, NULL, link_iterate_old_cb, NULL, H5P_DEFAULT) < 0) TEST_ERROR

        /* Create several links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            hid_t group_id2;	        /* Group ID */

            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Create hard link, with group object */
            if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
            if(H5Gclose(group_id2) < 0) TEST_ERROR
        } /* end for */

        /* Verify state of group (symbol table) */
        if(H5G_has_stab_test(group_id) != TRUE) TEST_ERROR

        /* Check for out of bound iteration on old-style group */
        skip = (hsize_t)u;
        H5E_BEGIN_TRY {
            ret = H5Literate(group_id, ".", H5_INDEX_NAME, order, &skip, link_iterate_old_cb, NULL, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Check for iteration on creation order */
        /* (should fail) */
        skip = (hsize_t)0;
        H5E_BEGIN_TRY {
            ret = H5Literate(group_id, ".", H5_INDEX_CRT_ORDER, order, &skip, link_iterate_old_cb, NULL, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Test iteration over links in group */
        if(link_iterate_old_check(group_id, order, u, &iter_info) < 0) TEST_ERROR


        /* Close the group */
        if(H5Gclose(group_id) < 0) TEST_ERROR

        /* Close the file */
        if(H5Fclose(file_id) < 0) TEST_ERROR

        PASSED();
    } /* end for */

    /* Free resources */
    if(visited)
        HDfree(visited);

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    if(visited)
        HDfree(visited);

    return -1;
} /* end link_iterate_old() */


/*-------------------------------------------------------------------------
 * Function:    open_by_idx_check
 *
 * Purpose:     Check opening by index in a group
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
open_by_idx_check(hid_t main_group_id, hid_t soft_group_id, hid_t mount_file_id,
    H5_index_t idx_type, H5_iter_order_t order, unsigned max_links,
    haddr_t *objno)
{
    char        mntname[NAME_BUF_SIZE]; /* Link value */
    hid_t       group_id;       /* ID of group to test */
    H5O_info_t  oi;             /* Buffer for querying object's info */
    haddr_t     mnt_root_addr;  /* Address of root group in file to mount */
    hid_t       obj_id;         /* ID of object opened */
    unsigned    mnt_idx;        /* Index to mount group on */
    unsigned    u, v;           /* Local index variables */

    /* Work through main & soft link groups */
    for(v = 0; v < 2; v++) {
        /* Choose appropriate group to open links within */
        switch(v) {
            case 0:
                group_id = main_group_id;
                break;

            case 1:
                group_id = soft_group_id;
                break;
        } /* end switch */

        /* Open each object in main group by index and check that it's the correct one */
        for(u = 0; u < max_links; u++) {
            /* Open the object */
            if((obj_id = H5Oopen_by_idx(group_id, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT)) < 0) TEST_ERROR

            /* Get the object's information */
            if(H5Oget_info(obj_id, ".", &oi, H5P_DEFAULT) < 0) TEST_ERROR

            /* Check that the object is the correct one */
            if(order == H5_ITER_INC) {
                if(H5F_addr_ne(oi.addr, objno[u])) TEST_ERROR
            } /* end if */
            else if(order == H5_ITER_DEC) {
                unsigned dec_u = max_links - (u + 1);       /* Decreasing mapped index */

                if(H5F_addr_ne(oi.addr, objno[dec_u])) TEST_ERROR
            } /* end if */
            else {
                /* XXX: What to do about native order? */
            } /* end else */

            /* Close object */
            if(H5Oclose(obj_id) < 0) TEST_ERROR
        } /* end for */
    } /* end for */


    /*
     * Verify opening correct object by index when file mounting is present
     */

    /* Get the address of the root group in the file to mount */
    if(H5Oget_info(mount_file_id, "/", &oi, H5P_DEFAULT) < 0) TEST_ERROR
    mnt_root_addr = oi.addr;

    /* Mount a file over a group in main group */
    mnt_idx = 2;
    sprintf(mntname, "/%s/filler %02u", CORDER_GROUP_NAME, mnt_idx);
    if(H5Fmount(main_group_id, mntname, mount_file_id, H5P_DEFAULT) < 0) TEST_ERROR

    /* Open the object that the file is mounted on */
    if((obj_id = H5Oopen_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)mnt_idx, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Get the object's information */
    if(H5Oget_info(obj_id, ".", &oi, H5P_DEFAULT) < 0) TEST_ERROR

    /* Check that the object is the root of the mounted file and not in the previous file */
    if(H5F_addr_ne(oi.addr, mnt_root_addr)) TEST_ERROR
    if(H5F_addr_eq(oi.addr, objno[mnt_idx])) TEST_ERROR

    /* Close object */
    if(H5Oclose(obj_id) < 0) TEST_ERROR

    /* Unmount the file */
    if(H5Funmount(main_group_id, mntname) < 0) TEST_ERROR


    /* Success */
    return(0);

error:
    return(-1);
} /* end open_by_idx_check() */


/*-------------------------------------------------------------------------
 * Function:    open_by_idx
 *
 * Purpose:     Create a group with creation order indices and test opening
 *              objects by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
open_by_idx(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	mount_file_id = (-1); 	/* File ID for file to mount */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t	soft_group_id = (-1);	/* Group ID for soft links */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    H5_index_t idx_type;               /* Type of index to operate on */
    H5_iter_order_t order;              /* Order within in the index */
    hbool_t     use_index;              /* Use index on creation order values */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    H5O_info_t  oi;                     /* Buffer for querying object's info */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value */
    haddr_t     *objno = NULL;          /* Addresses of the objects created */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Allocate object address array */
    if(NULL == (objno = HDmalloc(sizeof(haddr_t) * (max_compact * 2)))) TEST_ERROR

    /* Create file to mount */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((mount_file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Loop over operating on different indices on link fields */
    for(idx_type = H5_INDEX_NAME; idx_type <=H5_INDEX_CRT_ORDER; idx_type++) {
        /* Loop over operating in different orders */
        for(order = H5_ITER_INC; order <=H5_ITER_NATIVE; order++) {
            /* Loop over using index for creation order value */
            for(use_index = FALSE; use_index <= TRUE; use_index++) {
                /* Print appropriate test message */
                if(idx_type == H5_INDEX_CRT_ORDER) {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("open object by creation order index in increasing order w/creation order index")
                        else
                            TESTING("open object by creation order index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("open object by creation order index in decreasing order w/creation order index")
                        else
                            TESTING("open object by creation order index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("open object by creation order index in native order w/creation order index")
                        else
                            TESTING("open object by creation order index in native order w/o creation order index")
                    } /* end else */
                } /* end if */
                else {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("open object by name index in increasing order w/creation order index")
                        else
                            TESTING("open object by name index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("open object by name index in decreasing order w/creation order index")
                        else
                            TESTING("open object by name index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("open object by name index in native order w/creation order index")
                        else
                            TESTING("open object by name index in native order w/o creation order index")
                    } /* end else */
                } /* end else */

                /* Create file */
                h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
                if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

                /* Set creation order tracking & indexing on group */
                if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0))) < 0) TEST_ERROR

                /* Create group with creation order tracking on */
                if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

                /* Create group with creation order tracking on for soft links */
                if((soft_group_id = H5Gcreate2(file_id, CORDER_SOFT_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR


                /* Try to open on object in an empty group */
                H5E_BEGIN_TRY {
                    ret = H5Oopen_by_idx(group_id, ".", idx_type, order, (hsize_t)0, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR


                /* Create several links, up to limit of compact form */
                for(u = 0; u < max_compact; u++) {
                    hid_t group_id2;	        /* Group ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                    /* Retrieve group's address on disk */
                    if(H5Oget_info(group_id2, ".", &oi, H5P_DEFAULT) < 0) TEST_ERROR
                    objno[u] = oi.addr;

                    /* Close group created */
                    if(H5Gclose(group_id2) < 0) TEST_ERROR

                    /* Create soft link in another group, to objects in main group */
                    sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
                    if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end for */

                /* Verify state of group (compact) */
                if(H5G_has_links_test(group_id, NULL) != TRUE) TEST_ERROR

                /* Check for out of bound open by index on compact group */
                H5E_BEGIN_TRY {
                    ret = H5Oopen_by_idx(group_id, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Verify opening objects by index */
                if(open_by_idx_check(group_id, soft_group_id, mount_file_id, idx_type, order, u, objno) < 0) TEST_ERROR


                /* Create more links, to push group into dense form */
                for(; u < (max_compact * 2); u++) {
                    hid_t group_id2;	        /* Group ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                    /* Retrieve group's address on disk */
                    if(H5Oget_info(group_id2, ".", &oi, H5P_DEFAULT) < 0) TEST_ERROR
                    objno[u] = oi.addr;

                    /* Close group created */
                    if(H5Gclose(group_id2) < 0) TEST_ERROR

                    /* Create soft link in another group, to objects in main group */
                    sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
                    if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end for */

                /* Verify state of group (dense) */
                if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

                /* Check for out of bound open by index on compact group */
                H5E_BEGIN_TRY {
                    ret = H5Oopen_by_idx(group_id, ".", idx_type, order, (hsize_t)u, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Verify opening objects by index */
                if(open_by_idx_check(group_id, soft_group_id, mount_file_id, idx_type, order, u, objno) < 0) TEST_ERROR


                /* Close the groups */
                if(H5Gclose(group_id) < 0) TEST_ERROR
                if(H5Gclose(soft_group_id) < 0) TEST_ERROR

                /* Close the file */
                if(H5Fclose(file_id) < 0) TEST_ERROR

                PASSED();
            } /* end for */
        } /* end for */
    } /* end for */

    /* Close the file for mounting */
    if(H5Fclose(mount_file_id) < 0) TEST_ERROR

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    /* Free resources */
    if(objno)
        HDfree(objno);

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Gclose(soft_group_id);
        H5Fclose(file_id);
        H5Fclose(mount_file_id);
    } H5E_END_TRY;

    if(objno)
        HDfree(objno);

    return -1;
} /* end open_by_idx() */


/*-------------------------------------------------------------------------
 * Function:    open_by_idx_old
 *
 * Purpose:     Create an old-style group and test opening
 *              objects by index.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
open_by_idx_old(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	mount_file_id = (-1); 	/* File ID for file to mount */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t	soft_group_id = (-1);	/* Group ID for soft links */
    H5_iter_order_t order;              /* Order within in the index */
    H5O_info_t  oi;                     /* Buffer for querying object's info */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value */
    haddr_t     objno[CORDER_NLINKS];   /* Addresses of the objects created */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Create file to mount */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((mount_file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Loop over operating in different orders */
    for(order = H5_ITER_INC; order <=H5_ITER_NATIVE; order++) {
        /* Print appropriate test message */
        if(order == H5_ITER_INC) {
            TESTING("open object by name index in increasing order in old-style group")
        } /* end if */
        else if(order == H5_ITER_DEC) {
            TESTING("open object by name index in decreasing order in old-style group")
        } /* end else */
        else {
            HDassert(order == H5_ITER_NATIVE);
            TESTING("open object by name index in native order in old-style group")
        } /* end else */

        /* Create file */
        h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
        if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create old-style group */
        if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        /* Create old-style group for soft links */
        if((soft_group_id = H5Gcreate2(file_id, CORDER_SOFT_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR


        /* Try to open on object in an empty group */
        H5E_BEGIN_TRY {
            ret = H5Oopen_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR


        /* Create several links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            hid_t group_id2;	        /* Group ID */

            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Create hard link, with group object */
            if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

            /* Retrieve group's address on disk */
            if(H5Oget_info(group_id2, ".", &oi, H5P_DEFAULT) < 0) TEST_ERROR
            objno[u] = oi.addr;

            /* Close group created */
            if(H5Gclose(group_id2) < 0) TEST_ERROR

            /* Create soft link in another group, to objects in main group */
            sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
            if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
        } /* end for */

        /* Verify state of group (symbol table) */
        if(H5G_has_stab_test(group_id) != TRUE) TEST_ERROR

        /* Check for out of bound open by index */
        H5E_BEGIN_TRY {
            ret = H5Oopen_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Check for creation order index open */
        H5E_BEGIN_TRY {
            ret = H5Oopen_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, order, (hsize_t)(u - 1), H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Verify opening objects by index */
        if(open_by_idx_check(group_id, soft_group_id, mount_file_id, H5_INDEX_NAME, order, u, objno) < 0) TEST_ERROR


        /* Close the groups */
        if(H5Gclose(group_id) < 0) TEST_ERROR
        if(H5Gclose(soft_group_id) < 0) TEST_ERROR

        /* Close the file */
        if(H5Fclose(file_id) < 0) TEST_ERROR

        PASSED();
    } /* end for */

    /* Close the file for mounting */
    if(H5Fclose(mount_file_id) < 0) TEST_ERROR

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Gclose(group_id);
        H5Gclose(soft_group_id);
        H5Fclose(file_id);
        H5Fclose(mount_file_id);
    } H5E_END_TRY;

    return -1;
} /* end open_by_idx_old() */


/*-------------------------------------------------------------------------
 * Function:    object_info_check
 *
 * Purpose:     Check querying object info in a group
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Sunday, November 26, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
object_info_check(hid_t main_group_id, hid_t soft_group_id, H5_index_t idx_type,
    H5_iter_order_t order, unsigned max_links, haddr_t *objno)
{
    char        objname[NAME_BUF_SIZE]; /* Object name */
    hid_t       group_id;       /* ID of group to test */
    H5O_info_t  oinfo;          /* Buffer for querying object's info */
    unsigned    u, v;           /* Local index variables */

    /* Work through main & soft link groups */
    for(v = 0; v < 2; v++) {
        /* Choose appropriate group to open links within */
        switch(v) {
            case 0:
                group_id = main_group_id;
                break;

            case 1:
                group_id = soft_group_id;
                break;
        } /* end switch */

        /* Open each object in group by name and check that it's the correct one */
        for(u = 0; u < max_links; u++) {
            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Query the object's information, by name */
            if(H5Oget_info(group_id, objname, &oinfo, H5P_DEFAULT) < 0) TEST_ERROR

            /* Check that the object is the correct one */
            if(H5F_addr_ne(oinfo.addr, objno[u])) TEST_ERROR
            if(H5F_addr_ne(oinfo.num_attrs, u)) TEST_ERROR

            /* Query the object's information, by index */
            if(H5Oget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)u, &oinfo, H5P_DEFAULT) < 0) TEST_ERROR

            /* Check that the object is the correct one */
            if(order == H5_ITER_INC) {
                if(H5F_addr_ne(oinfo.addr, objno[u])) TEST_ERROR
                if(H5F_addr_ne(oinfo.num_attrs, u)) TEST_ERROR
            } /* end if */
            else if(order == H5_ITER_DEC) {
                unsigned dec_u = max_links - (u + 1);       /* Decreasing mapped index */

                if(H5F_addr_ne(oinfo.addr, objno[dec_u])) TEST_ERROR
                if(H5F_addr_ne(oinfo.num_attrs, dec_u)) TEST_ERROR
            } /* end if */
            else {
                /* XXX: What to do about native order? */
            } /* end else */

        } /* end for */
    } /* end for */

    /* Success */
    return(0);

error:
    return(-1);
} /* end object_info_check() */


/*-------------------------------------------------------------------------
 * Function:    object_info
 *
 * Purpose:     Create a group with creation order indices and test querying
 *              object info.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Sunday, November 26, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
object_info(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t	soft_group_id = (-1);	/* Group ID for soft links */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    hid_t       space_id = (-1);        /* Dataspace ID (for attributes) */
    H5_index_t idx_type;               /* Type of index to operate on */
    H5_iter_order_t order;              /* Order within in the index */
    hbool_t     use_index;              /* Use index on creation order values */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    H5O_info_t  oinfo;                  /* Buffer for querying object's info */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value */
    char        attrname[NAME_BUF_SIZE]; /* Attribute name */
    haddr_t     *objno = NULL;          /* Addresses of the objects created */
    herr_t      ret;                    /* Generic return value */
    unsigned    u, v;                   /* Local index variables */

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Allocate object address array */
    if(NULL == (objno = HDmalloc(sizeof(haddr_t) * (max_compact * 2)))) TEST_ERROR

    /* Create dataspace for attributes */
    if((space_id = H5Screate(H5S_SCALAR)) < 0) TEST_ERROR

    /* Loop over operating on different indices on link fields */
    for(idx_type = H5_INDEX_NAME; idx_type <=H5_INDEX_CRT_ORDER; idx_type++) {
        /* Loop over operating in different orders */
        for(order = H5_ITER_INC; order <=H5_ITER_NATIVE; order++) {
            /* Loop over using index for creation order value */
            for(use_index = FALSE; use_index <= TRUE; use_index++) {
                /* Print appropriate test message */
                if(idx_type == H5_INDEX_CRT_ORDER) {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("query object info by creation order index in increasing order w/creation order index")
                        else
                            TESTING("query object info by creation order index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("query object info by creation order index in decreasing order w/creation order index")
                        else
                            TESTING("query object info by creation order index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("query object info by creation order index in native order w/creation order index")
                        else
                            TESTING("query object info by creation order index in native order w/o creation order index")
                    } /* end else */
                } /* end if */
                else {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("query object info by name index in increasing order w/creation order index")
                        else
                            TESTING("query object info by name index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("query object info by name index in decreasing order w/creation order index")
                        else
                            TESTING("query object info by name index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("query object info by name index in native order w/creation order index")
                        else
                            TESTING("query object info by name index in native order w/o creation order index")
                    } /* end else */
                } /* end else */

                /* Create file */
                h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
                if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

                /* Set creation order tracking & indexing on group */
                if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0))) < 0) TEST_ERROR

                /* Create group with creation order tracking on */
                if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

                /* Create group with creation order tracking on for soft links */
                if((soft_group_id = H5Gcreate2(file_id, CORDER_SOFT_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR


                /* Check for out of bound query by index on empty group */
                H5E_BEGIN_TRY {
                    ret = H5Oget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, &oinfo, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Create several links, up to limit of compact form */
                for(u = 0; u < max_compact; u++) {
                    hid_t group_id2;	        /* Group ID */
                    hid_t attr_id;              /* Attribute ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                    /* Retrieve group's address on disk */
                    if(H5Oget_info(group_id2, ".", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
                    objno[u] = oinfo.addr;

                    /* Create attributes on new object */
                    for(v = 0; v < u; v++) {
                        /* Make name for attribute */
                        sprintf(attrname, "attr %02u", v);

                        /* Create attribute */
                        if((attr_id = H5Acreate2(group_id2, ".", attrname, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                        /* Close attribute */
                        if(H5Aclose(attr_id) < 0) TEST_ERROR
                    } /* end for */

                    /* Close group created */
                    if(H5Gclose(group_id2) < 0) TEST_ERROR

                    /* Create soft link in another group, to objects in main group */
                    sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
                    if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end for */

                /* Verify state of group (compact) */
                if(H5G_has_links_test(group_id, NULL) != TRUE) TEST_ERROR

                /* Check for out of bound query by index */
                H5E_BEGIN_TRY {
                    ret = H5Oget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &oinfo, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Verify querying objects by name */
                if(object_info_check(group_id, soft_group_id, idx_type, order, u, objno) < 0) TEST_ERROR


                /* Create more links, to push group into dense form */
                for(; u < (max_compact * 2); u++) {
                    hid_t group_id2;	        /* Group ID */
                    hid_t attr_id;              /* Attribute ID */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                    /* Retrieve group's address on disk */
                    if(H5Oget_info(group_id2, ".", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
                    objno[u] = oinfo.addr;

                    /* Create attributes on new object */
                    for(v = 0; v < u; v++) {
                        /* Make name for attribute */
                        sprintf(attrname, "attr %02u", v);

                        /* Create attribute */
                        if((attr_id = H5Acreate2(group_id2, ".", attrname, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                        /* Close attribute */
                        if(H5Aclose(attr_id) < 0) TEST_ERROR
                    } /* end for */

                    /* Close group created */
                    if(H5Gclose(group_id2) < 0) TEST_ERROR

                    /* Create soft link in another group, to objects in main group */
                    sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
                    if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end for */

                /* Verify state of group (dense) */
                if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

                /* Check for out of bound query by index */
                H5E_BEGIN_TRY {
                    ret = H5Oget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &oinfo, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Verify querying objects by name */
                if(object_info_check(group_id, soft_group_id, idx_type, order, u, objno) < 0) TEST_ERROR


                /* Close the groups */
                if(H5Gclose(group_id) < 0) TEST_ERROR
                if(H5Gclose(soft_group_id) < 0) TEST_ERROR

                /* Close the file */
                if(H5Fclose(file_id) < 0) TEST_ERROR

                PASSED();
            } /* end for */
        } /* end for */
    } /* end for */

    /* Free resources */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR
    if(H5Sclose(space_id) < 0) TEST_ERROR
    if(objno)
        HDfree(objno);

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Sclose(space_id);
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Gclose(soft_group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    if(objno)
        HDfree(objno);

    return -1;
} /* end object_info() */


/*-------------------------------------------------------------------------
 * Function:    object_info_old
 *
 * Purpose:     Create an old-style group test querying object info.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Sunday, November 26, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
object_info_old(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t	soft_group_id = (-1);	/* Group ID for soft links */
    hid_t       space_id = (-1);        /* Dataspace ID (for attributes) */
    H5_iter_order_t order;              /* Order within in the index */
    H5O_info_t  oinfo;                  /* Buffer for querying object's info */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value */
    char        attrname[NAME_BUF_SIZE]; /* Attribute name */
    haddr_t     objno[CORDER_NLINKS];   /* Addresses of the objects created */
    herr_t      ret;                    /* Generic return value */
    unsigned    u, v;                   /* Local index variables */

    /* Create dataspace for attributes */
    if((space_id = H5Screate(H5S_SCALAR)) < 0) TEST_ERROR

    /* Loop over operating in different orders */
    for(order = H5_ITER_INC; order <=H5_ITER_NATIVE; order++) {
        /* Print appropriate test message */
        if(order == H5_ITER_INC) {
            TESTING("query object info by name index in increasing order in old-style group")
        } /* end if */
        else if(order == H5_ITER_DEC) {
            TESTING("query object info by name index in decreasing order in old-style group")
        } /* end else */
        else {
            HDassert(order == H5_ITER_NATIVE);
            TESTING("query object info by name index in native order in old-style group")
        } /* end else */

        /* Create file */
        h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
        if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create old-style group */
        if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        /* Create old-style group for soft links */
        if((soft_group_id = H5Gcreate2(file_id, CORDER_SOFT_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR


        /* Check for out of bound query by index on empty group */
        H5E_BEGIN_TRY {
            ret = H5Oget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, &oinfo, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Create several links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            hid_t group_id2;	        /* Group ID */
            hid_t attr_id;              /* Attribute ID */

            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Create hard link, with group object */
            if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

            /* Retrieve group's address on disk */
            if(H5Oget_info(group_id2, ".", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
            objno[u] = oinfo.addr;

            /* Create attributes on new object */
            for(v = 0; v < u; v++) {
                /* Make name for attribute */
                sprintf(attrname, "attr %02u", v);

                /* Create attribute */
                if((attr_id = H5Acreate2(group_id2, ".", attrname, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                /* Close attribute */
                if(H5Aclose(attr_id) < 0) TEST_ERROR
            } /* end for */

            /* Close group created */
            if(H5Gclose(group_id2) < 0) TEST_ERROR

            /* Create soft link in another group, to objects in main group */
            sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
            if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
        } /* end for */

        /* Verify state of group (symbol table) */
        if(H5G_has_stab_test(group_id) != TRUE) TEST_ERROR

        /* Check for out of bound query by index */
        H5E_BEGIN_TRY {
            ret = H5Oget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &oinfo, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Check for creation order index query */
        H5E_BEGIN_TRY {
            ret = H5Oget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, order, (hsize_t)(u - 1), &oinfo, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Verify querying objects by name */
        if(object_info_check(group_id, soft_group_id, H5_INDEX_NAME, order, u, objno) < 0) TEST_ERROR


        /* Close the groups */
        if(H5Gclose(group_id) < 0) TEST_ERROR
        if(H5Gclose(soft_group_id) < 0) TEST_ERROR

        /* Close the file */
        if(H5Fclose(file_id) < 0) TEST_ERROR

        PASSED();
    } /* end for */

    /* Free resources */
    if(H5Sclose(space_id) < 0) TEST_ERROR

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Sclose(space_id);
        H5Gclose(group_id);
        H5Gclose(soft_group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    return -1;
} /* end object_info_old() */


/*-------------------------------------------------------------------------
 * Function:    group_info
 *
 * Purpose:     Create a group with creation order indices and test querying
 *              group info.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
group_info(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t	soft_group_id = (-1);	/* Group ID for soft links */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    H5_index_t idx_type;               /* Type of index to operate on */
    H5_iter_order_t order;              /* Order within in the index */
    hbool_t     use_index;              /* Use index on creation order values */
    unsigned    max_compact;            /* Maximum # of links to store in group compactly */
    unsigned    min_dense;              /* Minimum # of links to store in group "densely" */
    H5G_info_t  grp_info;               /* Buffer for querying object's info */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        objname2[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value */
    herr_t      ret;                    /* Generic return value */
    unsigned    u, v;                   /* Local index variables */

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Query the group creation properties */
    if(H5Pget_link_phase_change(gcpl_id, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Loop over operating on different indices on link fields */
    for(idx_type = H5_INDEX_NAME; idx_type <=H5_INDEX_CRT_ORDER; idx_type++) {
        /* Loop over operating in different orders */
        for(order = H5_ITER_INC; order <=H5_ITER_NATIVE; order++) {
            /* Loop over using index for creation order value */
            for(use_index = FALSE; use_index <= TRUE; use_index++) {
                /* Print appropriate test message */
                if(idx_type == H5_INDEX_CRT_ORDER) {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("query group info by creation order index in increasing order w/creation order index")
                        else
                            TESTING("query group info by creation order index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("query group info by creation order index in decreasing order w/creation order index")
                        else
                            TESTING("query group info by creation order index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("query group info by creation order index in native order w/creation order index")
                        else
                            TESTING("query group info by creation order index in native order w/o creation order index")
                    } /* end else */
                } /* end if */
                else {
                    if(order == H5_ITER_INC) {
                        if(use_index)
                            TESTING("query group info by name index in increasing order w/creation order index")
                        else
                            TESTING("query group info by name index in increasing order w/o creation order index")
                    } /* end if */
                    else if(order == H5_ITER_DEC) {
                        if(use_index)
                            TESTING("query group info by name index in decreasing order w/creation order index")
                        else
                            TESTING("query group info by name index in decreasing order w/o creation order index")
                    } /* end else */
                    else {
                        HDassert(order == H5_ITER_NATIVE);
                        if(use_index)
                            TESTING("query group info by name index in native order w/creation order index")
                        else
                            TESTING("query group info by name index in native order w/o creation order index")
                    } /* end else */
                } /* end else */

                /* Create file */
                h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
                if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

                /* Set creation order tracking & indexing on group */
                if(H5Pset_link_creation_order(gcpl_id, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0))) < 0) TEST_ERROR

                /* Create group with creation order tracking on */
                if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

                /* Create group with creation order tracking on for soft links */
                if((soft_group_id = H5Gcreate2(file_id, CORDER_SOFT_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR


                /* Check for out of bound query by index on empty group */
                H5E_BEGIN_TRY {
                    ret = H5Gget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, &grp_info, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR

                /* Create several links, up to limit of compact form */
                for(u = 0; u < max_compact; u++) {
                    hid_t group_id2, group_id3;	        /* Group IDs */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

                    /* Retrieve group's information */
                    if(H5Gget_info(group_id2, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check (new/empty) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != 0) TEST_ERROR
                    if(grp_info.nlinks != 0) TEST_ERROR

                    /* Create objects in new group created */
                    for(v = 0; v <= u; v++) {
                        /* Make name for link */
                        sprintf(objname2, "filler %02u", v);

                        /* Create hard link, with group object */
                        if((group_id3 = H5Gcreate2(group_id2, objname2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                        /* Close group created */
                        if(H5Gclose(group_id3) < 0) TEST_ERROR
                    } /* end for */

                    /* Retrieve group's information */
                    if(H5Gget_info(group_id2, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check (new) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR

                    /* Retrieve group's information */
                    if(order != H5_ITER_NATIVE) {
                        if(order == H5_ITER_INC) {
                            if(H5Gget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)u, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR
                        } /* end if */
                        else {
                            if(H5Gget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)0, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR
                        } /* end else */

                        /* Check (new) group's information */
                        if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                        if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                        if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR
                    } /* end if */

                    /* Close group created */
                    if(H5Gclose(group_id2) < 0) TEST_ERROR


                    /* Retrieve main group's information, by name */
                    if(H5Gget_info(group_id, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check main group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR


                    /* Create soft link in another group, to objects in main group */
                    sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
                    if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Retrieve soft link group's information, by name */
                    if(H5Gget_info(soft_group_id, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check soft link group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR
                } /* end for */

                /* Verify state of group (compact) */
                if(H5G_has_links_test(group_id, NULL) != TRUE) TEST_ERROR

                /* Check for out of bound query by index */
                H5E_BEGIN_TRY {
                    ret = H5Gget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &grp_info, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR


                /* Create more links, to push group into dense form */
                for(; u < (max_compact * 2); u++) {
                    hid_t group_id2, group_id3;	        /* Group IDs */

                    /* Make name for link */
                    sprintf(objname, "filler %02u", u);

                    /* Create hard link, with group object */
                    if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

                    /* Retrieve group's information */
                    if(H5Gget_info(group_id2, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check (new/empty) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_COMPACT) TEST_ERROR
                    if(grp_info.max_corder != 0) TEST_ERROR
                    if(grp_info.nlinks != 0) TEST_ERROR

                    /* Create objects in new group created */
                    for(v = 0; v <= u; v++) {
                        /* Make name for link */
                        sprintf(objname2, "filler %02u", v);

                        /* Create hard link, with group object */
                        if((group_id3 = H5Gcreate2(group_id2, objname2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                        /* Close group created */
                        if(H5Gclose(group_id3) < 0) TEST_ERROR
                    } /* end for */

                    /* Retrieve group's information */
                    if(H5Gget_info(group_id2, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check (new) group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_DENSE) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR

                    /* Retrieve group's information */
                    if(order != H5_ITER_NATIVE) {
                        if(order == H5_ITER_INC) {
                            if(H5Gget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)u, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR
                        } /* end if */
                        else {
                            if(H5Gget_info_by_idx(group_id, ".", idx_type, order, (hsize_t)0, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR
                        } /* end else */

                        /* Check (new) group's information */
                        if(grp_info.storage_type != H5G_STORAGE_TYPE_DENSE) TEST_ERROR
                        if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                        if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR
                    } /* end if */

                    /* Close group created */
                    if(H5Gclose(group_id2) < 0) TEST_ERROR


                    /* Retrieve main group's information, by name */
                    if(H5Gget_info(group_id, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check main group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_DENSE) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR


                    /* Create soft link in another group, to objects in main group */
                    sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
                    if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Retrieve soft link group's information, by name */
                    if(H5Gget_info(soft_group_id, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

                    /* Check soft link group's information */
                    if(grp_info.storage_type != H5G_STORAGE_TYPE_DENSE) TEST_ERROR
                    if(grp_info.max_corder != (int64_t)(u + 1)) TEST_ERROR
                    if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR
                } /* end for */

                /* Verify state of group (dense) */
                if(H5G_is_new_dense_test(group_id) != TRUE) TEST_ERROR

                /* Check for out of bound query by index */
                H5E_BEGIN_TRY {
                    ret = H5Gget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &grp_info, H5P_DEFAULT);
                } H5E_END_TRY;
                if(ret >= 0) TEST_ERROR


                /* Close the groups */
                if(H5Gclose(group_id) < 0) TEST_ERROR
                if(H5Gclose(soft_group_id) < 0) TEST_ERROR

                /* Close the file */
                if(H5Fclose(file_id) < 0) TEST_ERROR

                PASSED();
            } /* end for */
        } /* end for */
    } /* end for */

    /* Free resources */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Gclose(soft_group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    return -1;
} /* end group_info() */


/*-------------------------------------------------------------------------
 * Function:    group_info_old
 *
 * Purpose:     Create an old-style group and test querying
 *              group info.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
group_info_old(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t	soft_group_id = (-1);	/* Group ID for soft links */
    H5_iter_order_t order;              /* Order within in the index */
    H5G_info_t  grp_info;               /* Buffer for querying object's info */
    char        filename[NAME_BUF_SIZE];/* File name */
    char        objname[NAME_BUF_SIZE]; /* Object name */
    char        objname2[NAME_BUF_SIZE]; /* Object name */
    char        valname[NAME_BUF_SIZE]; /* Link value */
    herr_t      ret;                    /* Generic return value */
    unsigned    u, v;                   /* Local index variables */

    /* Loop over operating in different orders */
    for(order = H5_ITER_INC; order <=H5_ITER_NATIVE; order++) {
        if(order == H5_ITER_INC) {
            TESTING("query group info by name index in increasing order in old-style group")
        } /* end if */
        else if(order == H5_ITER_DEC) {
            TESTING("query group info by name index in decreasing order in old-style group")
        } /* end else */
        else {
            HDassert(order == H5_ITER_NATIVE);
            TESTING("query group info by name index in native order in old-style group")
        } /* end else */

        /* Create file */
        h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
        if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

        /* Create old-style group */
        if((group_id = H5Gcreate2(file_id, CORDER_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

        /* Create old-style group for soft links */
        if((soft_group_id = H5Gcreate2(file_id, CORDER_SOFT_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR


        /* Check for out of bound query by index on empty group */
        H5E_BEGIN_TRY {
            ret = H5Gget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, &grp_info, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Create several links */
        for(u = 0; u < CORDER_NLINKS; u++) {
            hid_t group_id2, group_id3;	        /* Group IDs */

            /* Make name for link */
            sprintf(objname, "filler %02u", u);

            /* Create hard link, with group object */
            if((group_id2 = H5Gcreate2(group_id, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

            /* Retrieve group's information */
            if(H5Gget_info(group_id2, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

            /* Check (new/empty) group's information */
            if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
            if(grp_info.max_corder != 0) TEST_ERROR
            if(grp_info.nlinks != 0) TEST_ERROR

            /* Create objects in new group created */
            for(v = 0; v <= u; v++) {
                /* Make name for link */
                sprintf(objname2, "filler %02u", v);

                /* Create hard link, with group object */
                if((group_id3 = H5Gcreate2(group_id2, objname2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

                /* Close group created */
                if(H5Gclose(group_id3) < 0) TEST_ERROR
            } /* end for */

            /* Retrieve group's information */
            if(H5Gget_info(group_id2, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

            /* Check (new) group's information */
            if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
            if(grp_info.max_corder != 0) TEST_ERROR
            if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR

            /* Retrieve group's information */
            if(order != H5_ITER_NATIVE) {
                if(order == H5_ITER_INC) {
                    if(H5Gget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end if */
                else {
                    if(H5Gget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)0, &grp_info, H5P_DEFAULT) < 0) TEST_ERROR
                } /* end else */

                /* Check (new) group's information */
                if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
                if(grp_info.max_corder != 0) TEST_ERROR
                if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR
            } /* end if */

            /* Close group created */
            if(H5Gclose(group_id2) < 0) TEST_ERROR


            /* Retrieve main group's information, by name */
            if(H5Gget_info(group_id, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

            /* Check main group's information */
            if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
            if(grp_info.max_corder != 0) TEST_ERROR
            if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR


            /* Create soft link in another group, to objects in main group */
            sprintf(valname, "/%s/%s", CORDER_GROUP_NAME, objname);
            if(H5Lcreate_soft(valname, soft_group_id, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

            /* Retrieve soft link group's information, by name */
            if(H5Gget_info(soft_group_id, ".", &grp_info, H5P_DEFAULT) < 0) TEST_ERROR

            /* Check soft link group's information */
            if(grp_info.storage_type != H5G_STORAGE_TYPE_SYMBOL_TABLE) TEST_ERROR
            if(grp_info.max_corder != 0) TEST_ERROR
            if(grp_info.nlinks != (hsize_t)(u + 1)) TEST_ERROR
        } /* end for */

        /* Verify state of group (old-style) */
        if(H5G_has_stab_test(group_id) != TRUE) TEST_ERROR

        /* Check for out of bound query by index */
        H5E_BEGIN_TRY {
            ret = H5Gget_info_by_idx(group_id, ".", H5_INDEX_NAME, order, (hsize_t)u, &grp_info, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR

        /* Check for bad index query by index group */
        H5E_BEGIN_TRY {
            ret = H5Gget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, order, (hsize_t)0, &grp_info, H5P_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0) TEST_ERROR


        /* Close the groups */
        if(H5Gclose(group_id) < 0) TEST_ERROR
        if(H5Gclose(soft_group_id) < 0) TEST_ERROR

        /* Close the file */
        if(H5Fclose(file_id) < 0) TEST_ERROR

        PASSED();
    } /* end for */

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Gclose(group_id);
        H5Gclose(soft_group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    return -1;
} /* end group_info_old() */


/*-------------------------------------------------------------------------
 * Function:    timestamps
 *
 * Purpose:     Verify that disabling tracking timestamps for an object
 *              works correctly
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Saturday, March 3, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
timestamps(hid_t fapl)
{
    hid_t	file_id = (-1); 	/* File ID */
    hid_t	group_id = (-1);	/* Group ID */
    hid_t	group_id2 = (-1);	/* Group ID */
    hid_t       gcpl_id = (-1); 	/* Group creation property list ID */
    hid_t       gcpl_id2 = (-1); 	/* Group creation property list ID */
    H5O_info_t  oinfo, oinfo2;          /* Object info for groups created */
    char        filename[NAME_BUF_SIZE];/* File name */
    hbool_t     track_times;            /* The object timestamp setting */

    /* Print test message */
    TESTING("timestamps on objects")

    /* Create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) TEST_ERROR

    /* Query the object timestamp setting */
    if(H5Pget_obj_track_times(gcpl_id, &track_times) < 0) TEST_ERROR

    /* Check default timestamp information */
    if(track_times != TRUE) TEST_ERROR

    /* Set a non-default object timestamp setting */
    if(H5Pset_obj_track_times(gcpl_id, FALSE) < 0) TEST_ERROR

    /* Query the object timestamp setting */
    if(H5Pget_obj_track_times(gcpl_id, &track_times) < 0) TEST_ERROR

    /* Check default timestamp information */
    if(track_times != FALSE) TEST_ERROR


    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create group with non-default object timestamp setting */
    if((group_id = H5Gcreate2(file_id, TIMESTAMP_GROUP_1, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close the group creation property list */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR

    /* Create group with default object timestamp setting */
    if((group_id2 = H5Gcreate2(file_id, TIMESTAMP_GROUP_2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Retrieve the new groups' creation properties */
    if((gcpl_id = H5Gget_create_plist(group_id)) < 0) TEST_ERROR
    if((gcpl_id2 = H5Gget_create_plist(group_id2)) < 0) TEST_ERROR

    /* Query & verify the object timestamp settings */
    if(H5Pget_obj_track_times(gcpl_id, &track_times) < 0) TEST_ERROR
    if(track_times != FALSE) TEST_ERROR
    if(H5Pget_obj_track_times(gcpl_id2, &track_times) < 0) TEST_ERROR
    if(track_times != TRUE) TEST_ERROR

    /* Query the object information for each group */
    if(H5Oget_info(group_id, ".", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info(group_id2, ".", &oinfo2, H5P_DEFAULT) < 0) TEST_ERROR

    /* Sanity check object information for each group */
    if(oinfo.atime != 0) TEST_ERROR
    if(oinfo.mtime != 0) TEST_ERROR
    if(oinfo.ctime != 0) TEST_ERROR
    if(oinfo.btime != 0) TEST_ERROR
    if(oinfo.atime == oinfo2.atime) TEST_ERROR
    if(oinfo.mtime == oinfo2.mtime) TEST_ERROR
    if(oinfo.ctime == oinfo2.ctime) TEST_ERROR
    if(oinfo.btime == oinfo2.btime) TEST_ERROR
    if((oinfo.hdr.flags & H5O_HDR_STORE_TIMES) != 0) TEST_ERROR
    if((oinfo2.hdr.flags & H5O_HDR_STORE_TIMES) == 0) TEST_ERROR
    if(oinfo.hdr.space.total >= oinfo2.hdr.space.total) TEST_ERROR
    if(oinfo.hdr.space.meta >= oinfo2.hdr.space.meta) TEST_ERROR

    /* Close the property lists */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR
    if(H5Pclose(gcpl_id2) < 0) TEST_ERROR

    /* Close the groups */
    if(H5Gclose(group_id) < 0) TEST_ERROR
    if(H5Gclose(group_id2) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR


    /* Re-open the file */
    if((file_id = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Open groups */
    if((group_id = H5Gopen2(file_id, TIMESTAMP_GROUP_1, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if((group_id2 = H5Gopen2(file_id, TIMESTAMP_GROUP_2, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Retrieve the groups' creation properties */
    if((gcpl_id = H5Gget_create_plist(group_id)) < 0) TEST_ERROR
    if((gcpl_id2 = H5Gget_create_plist(group_id2)) < 0) TEST_ERROR

    /* Query & verify the object timestamp settings */
    if(H5Pget_obj_track_times(gcpl_id, &track_times) < 0) TEST_ERROR
    if(track_times != FALSE) TEST_ERROR
    if(H5Pget_obj_track_times(gcpl_id2, &track_times) < 0) TEST_ERROR
    if(track_times != TRUE) TEST_ERROR

    /* Query the object information for each group */
    if(H5Oget_info(group_id, ".", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Oget_info(group_id2, ".", &oinfo2, H5P_DEFAULT) < 0) TEST_ERROR

    /* Sanity check object information for each group */
    if(oinfo.atime != 0) TEST_ERROR
    if(oinfo.mtime != 0) TEST_ERROR
    if(oinfo.ctime != 0) TEST_ERROR
    if(oinfo.btime != 0) TEST_ERROR
    if(oinfo.atime == oinfo2.atime) TEST_ERROR
    if(oinfo.mtime == oinfo2.mtime) TEST_ERROR
    if(oinfo.ctime == oinfo2.ctime) TEST_ERROR
    if(oinfo.btime == oinfo2.btime) TEST_ERROR
    if((oinfo.hdr.flags & H5O_HDR_STORE_TIMES) != 0) TEST_ERROR
    if((oinfo2.hdr.flags & H5O_HDR_STORE_TIMES) == 0) TEST_ERROR
    if(oinfo.hdr.space.total >= oinfo2.hdr.space.total) TEST_ERROR
    if(oinfo.hdr.space.meta >= oinfo2.hdr.space.meta) TEST_ERROR

    /* Close the property lists */
    if(H5Pclose(gcpl_id) < 0) TEST_ERROR
    if(H5Pclose(gcpl_id2) < 0) TEST_ERROR

    /* Close the groups */
    if(H5Gclose(group_id) < 0) TEST_ERROR
    if(H5Gclose(group_id2) < 0) TEST_ERROR

    /* Close the file */
    if(H5Fclose(file_id) < 0) TEST_ERROR

    PASSED();

    return 0;

error:
    /* Free resources */
    H5E_BEGIN_TRY {
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

    return -1;
} /* end timestamps() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test links
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(non-zero)
 *
 * Programmer:	Robb Matzke
 *              Friday, August 14, 1998
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    const char  *envval = NULL;

    envval = HDgetenv("HDF5_DRIVER");
    if(envval == NULL) 
        envval = "nomatch";
    if(HDstrcmp(envval, "core") && HDstrcmp(envval, "split") && HDstrcmp(envval, "multi") && HDstrcmp(envval, "family")) {
        hid_t	fapl, fapl2;    /* File access property lists */
        int	nerrors = 0;
        hbool_t new_format;     /* Whether to use the new format or not */

	h5_reset();
	fapl = h5_fileaccess();

        /* Copy the file access property list */
        if((fapl2 = H5Pcopy(fapl)) < 0) TEST_ERROR

        /* Set the "use the latest version of the format" flag for creating objects in the file */
        if(H5Pset_latest_format(fapl2, TRUE) < 0) TEST_ERROR

        /* Loop over using new group format */
        for(new_format = FALSE; new_format <= TRUE; new_format++) {
            /* General tests... (on both old & new format groups */
            nerrors += mklinks((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += cklinks((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += new_links((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += ck_new_links((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += long_links((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += toomany((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;

            /* Test new H5L link creation routine */
            nerrors += test_h5l_create((new_format ? fapl2 : fapl), new_format);
            nerrors += test_lcpl((new_format ? fapl2 : fapl), new_format);
            nerrors += test_move((new_format ? fapl2 : fapl), new_format);
            nerrors += test_copy((new_format ? fapl2 : fapl), new_format);
            nerrors += test_move_preserves((new_format ? fapl2 : fapl), new_format);
#ifndef H5_NO_DEPRECATED_SYMBOLS
            nerrors += test_compat((new_format ? fapl2 : fapl), new_format);
#endif /* H5_NO_DEPRECATED_SYMBOLS */
#ifndef H5_CANNOT_OPEN_TWICE
            nerrors += external_link_root((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
#endif /* H5_CANNOT_OPEN_TWICE */
            nerrors += external_link_path((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += external_link_mult((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
#ifndef H5_CANNOT_OPEN_TWICE
            nerrors += external_link_self((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += external_link_pingpong((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += external_link_toomany((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
#endif /* H5_CANNOT_OPEN_TWICE */
            nerrors += external_link_dangling((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += external_link_recursive((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += external_link_query((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += external_link_unlink_compact((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += external_link_unlink_dense((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += external_link_move((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += external_link_ride((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
#ifndef H5_CANNOT_OPEN_TWICE
            nerrors += external_link_closing((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
#endif /* H5_CANNOT_OPEN_TWICE */
            nerrors += external_link_endian((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += external_link_strong((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;

            /* These tests assume that external links are a form of UD links,
             * so assume that everything that passed for external links
             * above has already been tested for UD links.
             */
            if(new_format == TRUE) {
                nerrors += ud_hard_links(fapl2) < 0 ? 1 : 0;     /* requires new format groups */
                nerrors += ud_link_reregister(fapl2) < 0 ? 1 : 0;        /* requires new format groups */
            } /* end if */
            nerrors += ud_callbacks((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += ud_link_errors((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += lapl_udata((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += lapl_nlinks((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
            nerrors += linkinfo((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;

            nerrors += check_all_closed((new_format ? fapl2 : fapl), new_format) < 0 ? 1 : 0;
        } /* end for */

        /* New group revision feature tests */
        nerrors += corder_create_empty(fapl2) < 0 ? 1 : 0;
/* XXX: when creation order indexing is fully working, go back and add checks
*      to these tests to make certain that the creation order values are
*      correct.
*/
        nerrors += corder_create_compact(fapl2) < 0 ? 1 : 0;
        nerrors += corder_create_dense(fapl2) < 0 ? 1 : 0;
        nerrors += corder_transition(fapl2) < 0 ? 1 : 0;
        nerrors += corder_delete(fapl2) < 0 ? 1 : 0;
        nerrors += link_info_by_idx(fapl2) < 0 ? 1 : 0;
        nerrors += delete_by_idx(fapl2) < 0 ? 1 : 0;
        nerrors += link_iterate(fapl2) < 0 ? 1 : 0;
        nerrors += open_by_idx(fapl2) < 0 ? 1 : 0;
        nerrors += object_info(fapl2) < 0 ? 1 : 0;
        nerrors += group_info(fapl2) < 0 ? 1 : 0;
        nerrors += timestamps(fapl2) < 0 ? 1 : 0;

        /* Test new API calls on old-style groups */
        nerrors += link_info_by_idx_old(fapl) < 0 ? 1 : 0;
        nerrors += delete_by_idx_old(fapl) < 0 ? 1 : 0;
        nerrors += link_iterate_old(fapl) < 0 ? 1 : 0;
        nerrors += open_by_idx_old(fapl) < 0 ? 1 : 0;
        nerrors += object_info_old(fapl) < 0 ? 1 : 0;
        nerrors += group_info_old(fapl) < 0 ? 1 : 0;

        /* Close 2nd FAPL */
	H5Pclose(fapl2);

	/* Results */
	if(nerrors) {
	    printf("***** %d LINK TEST%s FAILED! *****\n",
		    nerrors, 1 == nerrors ? "" : "S");
	    exit(1);
	}
	printf("All link tests passed.\n");
	h5_cleanup(FILENAME, fapl);
    }
    else
        puts("All link tests skipped - Incompatible with current Virtual File Driver");
    return 0;

error:
    puts("*** TESTS FAILED ***");
    return 1;
}

