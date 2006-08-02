/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, April 10, 1998
 *
 * Purpose:	Tests hard, soft (symbolic) & external links.
 */

#include "H5Lprivate.h"

#include "h5test.h"

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
#define UD_CB_TYPE H5L_LINK_MAX
#define UD_PLIST_TYPE 128
#define UD_CBFAIL_TYPE UD_PLIST_TYPE
#define UD_ERROR_TYPE 189
#define UD_BAD_TYPE1 H5G_LINK_HARD
#define UD_BAD_TYPE2 (H5L_LINK_UD_MIN - 5)
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
mklinks(hid_t fapl)
{
    hid_t		file, scalar, grp, d1;
    hsize_t	        size[1] = {1};
    char		filename[NAME_BUF_SIZE];

    TESTING("link creation");

    /* Create a file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
    if ((scalar=H5Screate_simple (1, size, size))<0) TEST_ERROR;

    /* Create a group */
    if ((grp=H5Gcreate (file, "grp1", (size_t)0))<0) TEST_ERROR;
    if (H5Gclose (grp)<0) TEST_ERROR;

    /* Create a dataset */
    if ((d1=H5Dcreate (file, "d1", H5T_NATIVE_INT, scalar, H5P_DEFAULT))<0) TEST_ERROR;
    if (H5Dclose (d1)<0) TEST_ERROR;

    /* Create a hard link */
    if (H5Lcreate_hard (file, "d1", H5L_SAME_LOC, "grp1/hard", H5P_DEFAULT, H5P_DEFAULT)<0) TEST_ERROR;

    /* Create a symbolic link */
    if (H5Lcreate_soft ("/d1", file, "grp1/soft", H5P_DEFAULT, H5P_DEFAULT)<0) TEST_ERROR;

    /* Create a symbolic link to something that doesn't exist */
    if (H5Lcreate_soft ("foobar", file, "grp1/dangle", H5P_DEFAULT, H5P_DEFAULT)<0) TEST_ERROR;

    /* Create a recursive symbolic link */
    if (H5Lcreate_soft ("/grp1/recursive", file, "/grp1/recursive", H5P_DEFAULT, H5P_DEFAULT)<0) TEST_ERROR;

    /* Close */
    if (H5Sclose (scalar)<0) TEST_ERROR;
    if (H5Fclose (file)<0) TEST_ERROR;

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
new_links(hid_t fapl)
{
    hid_t		file_a, file_b=(-1);
    hid_t		grp1_a=(-1), grp1_b=(-1), grp2_a=(-1), grp2_b=(-1);
    hid_t		scalar=(-1);
    hid_t		dset1=(-1), dset2=(-1);
    char		filename[NAME_BUF_SIZE];
    hsize_t             size[1] = {1};

    TESTING("H5Lcreate functions");

    /* Create two files */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file_a=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);
    if ((file_b=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    if ((scalar=H5Screate_simple (1, size, size))<0) TEST_ERROR;

    /* Create two groups in each file */
    if ((grp1_a=H5Gcreate (file_a, "grp1", (size_t)0))<0) TEST_ERROR;
    if ((grp2_a=H5Gcreate (file_a, "grp2", (size_t)0))<0) TEST_ERROR;
    if ((grp1_b=H5Gcreate (file_b, "grp1", (size_t)0))<0) TEST_ERROR;
    if ((grp2_b=H5Gcreate (file_b, "grp2", (size_t)0))<0) TEST_ERROR;

    /* Create datasets */
    if((dset1=H5Dcreate(file_a, "dataset1", H5T_NATIVE_INT, scalar, H5P_DEFAULT))<0) TEST_ERROR;
    if((dset2=H5Dcreate(grp1_a, "dataset2", H5T_NATIVE_INT, scalar, H5P_DEFAULT))<0) TEST_ERROR;

    /* Create links within a file.  Both of source and destination use
     * H5L_SAME_LOC.  Both hard and soft links should fail. */
    H5E_BEGIN_TRY {
        if(H5Lcreate_hard(H5L_SAME_LOC, "dataset1", H5L_SAME_LOC, "hard", H5P_DEFAULT, H5P_DEFAULT)!=FAIL) TEST_ERROR;
    } H5E_END_TRY;
    H5E_BEGIN_TRY {
        if(H5Lcreate_soft("dataset1", H5L_SAME_LOC, "soft", H5P_DEFAULT, H5P_DEFAULT)!=FAIL) TEST_ERROR;
    } H5E_END_TRY;

    /* Create links across files with hard link.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lcreate_hard(file_a, "dataset1", file_b, "hard", H5P_DEFAULT, H5P_DEFAULT)!=FAIL) TEST_ERROR;
    } H5E_END_TRY;

    /* Create hard link to test H5L_SAME_LOC */
    if(H5Lcreate_hard(grp1_a, "dataset2", H5L_SAME_LOC, "hard1", H5P_DEFAULT, H5P_DEFAULT)<0) TEST_ERROR;

    /* Create links to test hard links across different locations */
    if(H5Lcreate_hard(grp1_a, "dataset2", grp2_a, "hard2", H5P_DEFAULT, H5P_DEFAULT)<0) TEST_ERROR;

    /* Close dataspace and files */
    if (H5Sclose (scalar)<0) TEST_ERROR;
    if (H5Dclose(dset1)<0) TEST_ERROR;
    if (H5Dclose(dset2)<0) TEST_ERROR;
    if (H5Gclose (grp1_a)<0) TEST_ERROR;
    if (H5Gclose (grp2_a)<0) TEST_ERROR;
    if (H5Gclose (grp1_b)<0) TEST_ERROR;
    if (H5Gclose (grp2_b)<0) TEST_ERROR;
    if (H5Fclose (file_a)<0) TEST_ERROR;
    if (H5Fclose (file_b)<0) TEST_ERROR;

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Sclose (scalar);
    	H5Dclose (dset1);
    	H5Dclose (dset2);
    	H5Gclose (grp1_a);
    	H5Gclose (grp2_a);
    	H5Gclose (grp1_b);
    	H5Gclose (grp2_b);
    	H5Fclose (file_a);
    	H5Fclose (file_b);
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
cklinks(hid_t fapl)
{
    hid_t		file;
    H5G_stat_t		sb1, sb2;
    char		linkval[LINK_BUF_SIZE];
    char		filename[NAME_BUF_SIZE];
    herr_t		status;

    TESTING("link queries");

    /* Open the file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file=H5Fopen(filename, H5F_ACC_RDONLY, fapl))<0) TEST_ERROR;

    /* Hard link */
    if (H5Gget_objinfo(file, "d1", TRUE, &sb1)<0) TEST_ERROR;
    if (H5Gget_objinfo(file, "grp1/hard", TRUE, &sb2)<0) TEST_ERROR;
    if (H5G_DATASET!=sb2.type) {
	H5_FAILED();
	printf("    %d: Unexpected object type should have been a dataset\n", __LINE__);
	TEST_ERROR;
    }
    if (HDmemcmp(&sb1.objno, &sb2.objno, sizeof(sb1.objno))) {
	H5_FAILED();
	puts("    Hard link test failed. Link seems not to point to the ");
	puts("    expected file location.");
	TEST_ERROR;
    }

    /* Symbolic link */
    if (H5Gget_objinfo(file, "grp1/soft", TRUE, &sb2)<0) TEST_ERROR;
    if (H5G_DATASET!=sb2.type) {
	H5_FAILED();
	printf("    %d: Unexpected object type should have been a dataset\n", __LINE__);
	TEST_ERROR;
    }
    if (HDmemcmp(&sb1.objno, &sb2.objno, sizeof(sb1.objno))) {
	H5_FAILED();
	puts("    Soft link test failed. Link seems not to point to the ");
	puts("    expected file location.");
	TEST_ERROR;
    }
    if (H5Lget_linkval(file, "grp1/soft", sizeof linkval, linkval, H5P_DEFAULT)<0) TEST_ERROR;
    if (HDstrcmp(linkval, "/d1")) {
	H5_FAILED();
	puts("    Soft link test failed. Wrong link value");
	TEST_ERROR;
    }

    /* Dangling link */
    H5E_BEGIN_TRY {
	status = H5Gget_objinfo(file, "grp1/dangle", TRUE, &sb2);
    } H5E_END_TRY;
    if (status>=0) {
	H5_FAILED();
	puts("    H5Gget_objinfo() should have failed for a dangling link.");
	TEST_ERROR;
    }
    if (H5Gget_objinfo(file, "grp1/dangle", FALSE, &sb2)<0) TEST_ERROR;
    if (H5G_LINK!=sb2.type) {
	H5_FAILED();
	printf("    %d: Unexpected object type should have been a symbolic link\n", __LINE__);
	TEST_ERROR;
    }
    if (H5Gget_linkval(file, "grp1/dangle", sizeof linkval, linkval)<0) {
	H5_FAILED();
	printf("    %d: Can't retrieve link value\n", __LINE__);
	TEST_ERROR;
    }
    if (HDstrcmp(linkval, "foobar")) {
	H5_FAILED();
	puts("    Dangling link test failed. Wrong link value");
	TEST_ERROR;
    }

    /* Recursive link */
    H5E_BEGIN_TRY {
	status = H5Gget_objinfo(file, "grp1/recursive", TRUE, &sb2);
    } H5E_END_TRY;
    if (status>=0) {
	H5_FAILED();
	puts("    H5Gget_objinfo() should have failed for a recursive link.");
	TEST_ERROR;
    }
    if (H5Gget_objinfo(file, "grp1/recursive", FALSE, &sb2)<0) TEST_ERROR;
    if (H5G_LINK!=sb2.type) {
	H5_FAILED();
	printf("    %d: Unexpected object type should have been a symbolic link\n", __LINE__);
	TEST_ERROR;
    }
    if (H5Gget_linkval(file, "grp1/recursive", sizeof linkval, linkval)<0) {
	H5_FAILED();
	printf("    %d: Can't retrieve link value\n", __LINE__);
	TEST_ERROR;
    }
    if (HDstrcmp(linkval, "/grp1/recursive")) {
	H5_FAILED();
	puts("   Recursive link test failed. Wrong link value");
	TEST_ERROR;
    }

    /* Cleanup */
    if (H5Fclose(file)<0) TEST_ERROR;
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
ck_new_links(hid_t fapl)
{
    hid_t 		file;
    H5G_stat_t		sb_dset, sb_hard1, sb_hard2;
    char 		filename[NAME_BUF_SIZE];

    TESTING("new link queries");

    /* Open the file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file=H5Fopen(filename, H5F_ACC_RDONLY, fapl))<0) TEST_ERROR;

    /* Get hard link info */
    if(H5Gget_objinfo(file, "/grp1/dataset2", TRUE, &sb_dset)<0)
	TEST_ERROR;
    if(H5Gget_objinfo(file, "/grp1/hard1", TRUE, &sb_hard1)<0)
	TEST_ERROR;
    if(H5Gget_objinfo(file, "/grp2/hard2", TRUE, &sb_hard2)<0)
	TEST_ERROR;

    /* Check hard links */
    if(H5G_DATASET!=sb_hard1.type || H5G_DATASET!=sb_hard2.type) {
	H5_FAILED();
	printf("    %d: Unexpected object type should have been a dataset\n", __LINE__);
	TEST_ERROR;
    }
    if(HDmemcmp(&sb_dset.objno, &sb_hard1.objno, sizeof(sb_dset.objno)) || HDmemcmp(&sb_dset.objno, &sb_hard2.objno, sizeof(sb_dset.objno))) {
	H5_FAILED();
	puts("    Hard link test failed.  Link seems not to point to the ");
	puts("    expected file location.");
	TEST_ERROR;
    }

    /* Cleanup */
    if(H5Fclose(file)<0) TEST_ERROR;
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
long_links(hid_t fapl)
{
    hid_t		fid = (-1);     /* File ID */
    hid_t		gid = (-1);     /* Group ID */
    hid_t		gid2 = (-1);    /* Datatype ID */
    char               *objname = NULL; /* Name of object [Long] */
    size_t              u;              /* Local index variable */
    char		filename[NAME_BUF_SIZE];

    TESTING("long names for objects & links");

    /* Create files */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create group with short name in file (used as target for hard links) */
    if((gid=H5Gcreate (fid, "grp1", (size_t)0))<0) TEST_ERROR;

    /* Construct very long file name */
    if((objname = HDmalloc((size_t)(MAX_NAME_LEN + 1))) == NULL) TEST_ERROR;
    for(u = 0; u < MAX_NAME_LEN; u++)
        objname[u] = 'a';
    objname[MAX_NAME_LEN] = '\0';

    /* Create hard link to existing object */
    if(H5Lcreate_hard(fid, "grp1", fid, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Create soft link to existing object */
    objname[0] = 'b';
    if(H5Lcreate_soft("grp1", fid, objname, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Create group with long name in existing group */
    if((gid2=H5Gcreate(gid, objname, (size_t)0))<0) TEST_ERROR;

    /* Close objects */
    if(H5Gclose(gid2)<0) TEST_ERROR;
    if(H5Gclose(gid)<0) TEST_ERROR;
    if(H5Fclose(fid)<0) TEST_ERROR;

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
toomany(hid_t fapl)
{
    hid_t		fid = (-1);     /* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char                objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t             name_len;       /* Length of object name */
    char		filename[NAME_BUF_SIZE];

    TESTING("too many links");

    /* Make certain test is valid */
    /* XXX: should probably make a "generic" test that creates the proper
     *          # of links based on this value - QAK
     */
    HDassert(H5G_NLINKS == 16);

    /* Create file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create group with short name in file (used as target for hard links) */
    if((gid=H5Gcreate (fid, "final", (size_t)0))<0) TEST_ERROR;

    /* Create chain of hard links to existing object (no limit on #) */
    if(H5Lcreate_hard(fid, "final", fid, "hard1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard1", fid, "hard2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard2", fid, "hard3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard3", fid, "hard4", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard4", fid, "hard5", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard5", fid, "hard6", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard6", fid, "hard7", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard7", fid, "hard8", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard8", fid, "hard9", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard9", fid, "hard10", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard10", fid, "hard11", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard11", fid, "hard12", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard12", fid, "hard13", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard13", fid, "hard14", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard14", fid, "hard15", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard15", fid, "hard16", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard16", fid, "hard17", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard17", fid, "hard18", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard18", fid, "hard19", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard19", fid, "hard20", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard20", fid, "hard21", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Create chain of soft links to existing object (limited) */
    if(H5Lcreate_soft("final", fid, "soft1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft1", fid, "soft2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft2", fid, "soft3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft3", fid, "soft4", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft4", fid, "soft5", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft5", fid, "soft6", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft6", fid, "soft7", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft7", fid, "soft8", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft8", fid, "soft9", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft9", fid, "soft10", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft10", fid, "soft11", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft11", fid, "soft12", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft12", fid, "soft13", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft13", fid, "soft14", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft14", fid, "soft15", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft15", fid, "soft16", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft16", fid, "soft17", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Close objects */
    if(H5Gclose(gid)<0) TEST_ERROR;
    if(H5Fclose(fid)<0) TEST_ERROR;

    /* Open file */
    if((fid=H5Fopen(filename, H5F_ACC_RDWR, fapl))<0) TEST_ERROR;

    /* Open object through last hard link */
    if((gid = H5Gopen(fid, "hard21")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/hard21")) TEST_ERROR

    /* Create object in hard-linked group */
    if((gid2 = H5Gcreate(gid, "new_hard", (size_t)0)) < 0) TEST_ERROR

    /* Close group in hard-linked group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close hard-linked object */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Open object through too deep soft link */
    H5E_BEGIN_TRY {
        gid = H5Gopen(fid, "soft17");
    } H5E_END_TRY;
    if (gid >= 0) {
	H5_FAILED();
	puts("    Should have failed for sequence of too many nested links.");
	TEST_ERROR;
    }

    /* Open object through lesser soft link */
    if((gid = H5Gopen(fid, "soft16")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/soft16")) TEST_ERROR

    /* Create object using soft links */
    if((gid2 = H5Gcreate(gid, "new_soft", (size_t)0)) < 0) TEST_ERROR

    /* Close groups */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;

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


#ifdef H5_GROUP_REVISION
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
test_h5l_create(hid_t fapl)
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
    TESTING("H5Llink");

    /* Create file */
    h5_fixname(FILENAME[3], fapl, filename, sizeof filename);

    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create and commit a datatype with no name */
    if((type_id =H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR;
    if(H5Tcommit_expand(file_id, type_id, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(! H5Tcommitted(type_id)) TEST_ERROR;

    /* Create the dataspace */
    dims[0] = H5L_DIM1;
    dims[1] = H5L_DIM2;
    if((space_id=H5Screate_simple(2 ,dims, NULL))<0) TEST_ERROR;
    /* Create a dataset with no name using the committed datatype*/
    if ((dset_id = H5Dcreate_expand(file_id, type_id, space_id, H5P_DEFAULT, H5P_DEFAULT)) <0) TEST_ERROR;

    /* Verify that we can write to and read from the dataset */
    /* Initialize the dataset */
    for (i = n = 0; i < H5L_DIM1; i++)
        for (j = 0; j < H5L_DIM2; j++)
          wdata[i][j] = n++;

    /* Write the data to the dataset */
    if (H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata)<0) TEST_ERROR;

    /* Read the data back */
    if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata)<0) TEST_ERROR;
    
    /* Verify the data */
    for (i = 0; i < H5L_DIM1; i++) {
	for (j = 0; j < H5L_DIM2; j++) {
	    if (wdata[i][j] != rdata[i][j])
            {
              TEST_ERROR;
            }
    }}

    /* Create a group with no name*/
    if((group_id = H5Gcreate_expand(file_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Link nameless datatype into nameless group */
    if(H5Llink(group_id, "datatype", type_id, H5P_DEFAULT, H5P_DEFAULT)<0) TEST_ERROR;

    /* Create LCPL with intermediate group creation flag set */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) <0) TEST_ERROR;
    if(H5Pset_create_intermediate_group(lcpl_id, TRUE) <0) TEST_ERROR;

    /* Link nameless dataset into nameless group with intermediate group */
    if(H5Llink(group_id, "inter_group/dataset", dset_id, lcpl_id, H5P_DEFAULT)<0) TEST_ERROR;

    /* Close IDs for dataset and datatype */
    if(H5Dclose(dset_id)<0) TEST_ERROR;
    if(H5Tclose(type_id)<0) TEST_ERROR;

    /* Re-open datatype using new link */
    if((type_id = H5Topen(group_id, "datatype"))<0) TEST_ERROR;

    /* Link nameless group to root group and close the group ID*/
    if(H5Llink(file_id, "/group", group_id, H5P_DEFAULT, H5P_DEFAULT)<0) TEST_ERROR;
    if(H5Gclose(group_id)<0) TEST_ERROR;

    /* Open dataset through root group and verify its data */
    if((dset_id = H5Dopen(file_id, "/group/inter_group/dataset"))<0) TEST_ERROR;

    /* Read data from dataset */
    if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata)<0) TEST_ERROR;
    for (i = 0; i < H5L_DIM1; i++) {
	for (j = 0; j < H5L_DIM2; j++) {
	    if (wdata[i][j] != rdata[i][j])
            {
              TEST_ERROR;
            }
    }}
        
    /* Close open IDs */
    if(H5Dclose(dset_id)<0) TEST_ERROR;
    if(H5Tclose(type_id)<0) TEST_ERROR;
    if(H5Pclose(lcpl_id)<0) TEST_ERROR;
    if(H5Sclose(space_id)<0) TEST_ERROR;
    if(H5Fclose(file_id)<0) TEST_ERROR;
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
test_lcpl(hid_t fapl)
{
    hid_t fapl_id=-1;
    hid_t file_id=-1;
    hid_t group_id=-1;
    hid_t space_id=-1;
    hid_t dset_id=-1;
    hid_t type_id=-1;
    hid_t lcpl_id=-1;
    H5L_linkinfo_t linfo;
    char filename[1024];
    hsize_t dims[2];

    TESTING("link creation property lists");
    /* Actually, intermediate group creation is tested elsewhere (tmisc).
     * Here we only need to test the character encoding property */

    /* Create file */
    fapl_id = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);

    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id))<0) TEST_ERROR;

    /* Create and link a group with the default LCPL */
    if((group_id = H5Gcreate_expand(file_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
    if(H5Llink(file_id, "/group", group_id, H5P_DEFAULT, H5P_DEFAULT)<0) TEST_ERROR;
    if(H5Gclose(group_id)<0) TEST_ERROR;

    /* Check that its character encoding is the default */
    if(H5Lget_linkinfo(file_id, "group", &linfo, H5P_DEFAULT) < 0) TEST_ERROR;
    if(linfo.cset != H5F_CRT_DEFAULT_CSET) TEST_ERROR;

    /* Create and commit a datatype with the default LCPL */
    if((type_id =H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR;
    if(H5Tcommit_expand(file_id, type_id, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Llink(file_id, "/type", type_id, H5P_DEFAULT, H5P_DEFAULT)<0) TEST_ERROR;
    if(H5Tclose(type_id)<0) TEST_ERROR;    

    /* Check that its character encoding is the default */
    if(H5Lget_linkinfo(file_id, "type", &linfo, H5P_DEFAULT) < 0) TEST_ERROR;
    if(linfo.cset != H5F_CRT_DEFAULT_CSET) TEST_ERROR;

    /* Create a dataspace */
    dims[0] = H5L_DIM1;
    dims[1] = H5L_DIM2;
    if((space_id=H5Screate_simple(2 ,dims, NULL))<0) TEST_ERROR;

    /* Create a dataset using the default LCPL */
    if ((dset_id = H5Dcreate_expand(file_id, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT)) <0) TEST_ERROR;
    if(H5Llink(file_id, "/dataset", dset_id, H5P_DEFAULT, H5P_DEFAULT)<0) TEST_ERROR;
    if(H5Dclose(dset_id)<0) TEST_ERROR;

    /* Check that its character encoding is the default */
    if(H5Lget_linkinfo(file_id, "dataset", &linfo, H5P_DEFAULT) < 0) TEST_ERROR;
    if(linfo.cset != H5F_CRT_DEFAULT_CSET) TEST_ERROR;

    /* Create a link creation property list with the UTF-8 character encoding */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) <0) TEST_ERROR;
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_UTF8) < 0) TEST_ERROR;

    /* Create and link a group with the new LCPL */
    if((group_id = H5Gcreate_expand(file_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
    if(H5Llink(file_id, "/group2", group_id, lcpl_id, H5P_DEFAULT)<0) TEST_ERROR;
    if(H5Gclose(group_id)<0) TEST_ERROR;

    /* Check that its character encoding is UTF-8 */
    if(H5Lget_linkinfo(file_id, "group2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR;
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;

    /* Create and commit a datatype with the new LCPL */
    if((type_id =H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR;
    if(H5Tcommit_expand(file_id, type_id, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Llink(file_id, "/type2", type_id, lcpl_id, H5P_DEFAULT)<0) TEST_ERROR;    
    if(H5Tclose(type_id)<0) TEST_ERROR;    

    /* Check that its character encoding is UTF-8 */
    if(H5Lget_linkinfo(file_id, "type2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR;
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;

    /* Create a dataset using the new LCPL */
    if ((dset_id = H5Dcreate_expand(file_id, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT)) <0) TEST_ERROR;
    if(H5Llink(file_id, "/dataset2", dset_id, lcpl_id, H5P_DEFAULT)<0) TEST_ERROR;
    if(H5Dclose(dset_id)<0) TEST_ERROR;

    /* Check that its character encoding is UTF-8 */
    if(H5Lget_linkinfo(file_id, "dataset2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR;
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;

    /* Create a new link to the dataset with a different character encoding. */
    if(H5Pclose(lcpl_id)<0) TEST_ERROR;
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) <0) TEST_ERROR;
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_ASCII) < 0) TEST_ERROR;

    if(H5Lcreate_hard(file_id, "/dataset2", file_id, "/dataset2_link", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Check that its character encoding is ASCII */
    if(H5Lget_linkinfo(file_id, "/dataset2_link", &linfo, H5P_DEFAULT) < 0) TEST_ERROR;
    if(linfo.cset != H5T_CSET_ASCII) TEST_ERROR;

    /* Check that the first link's encoding hasn't changed */
    if(H5Lget_linkinfo(file_id, "/dataset2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR;
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;


/* JAMES: these tests don't work because the character set encoding is
 * not stored in the symbol table.
 * Quincey says this will be fixed someday.
 */
#ifdef NOTYET
    /* Make sure that LCPLs work properly for other API calls: */
    /* H5Lcreate_soft */
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_UTF8) < 0) TEST_ERROR
    if(H5Lcreate_soft("dataset2", file_id, "slink_to_dset2", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_linkinfo(file_id, "slink_to_dset2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR

    /* H5Lmove */
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_ASCII) < 0) TEST_ERROR
    if(H5Lmove(file_id, "slink_to_dset2", file_id, "moved_slink", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_linkinfo(file_id, "moved_slink", &linfo, H5P_DEFAULT) < 0) TEST_ERROR;
    if(linfo.cset != H5T_CSET_ASCII) TEST_ERROR;

    /* H5Lcopy */
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_UTF8) < 0) TEST_ERROR;
    if(H5Lcopy(file_id, "moved_slink", file_id, "copied_slink", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_linkinfo(file_id, "copied_slink", &linfo, H5P_DEFAULT) < 0) TEST_ERROR;
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;

    /* H5Lcreate_external */
    if(H5Lcreate_external("filename", "path", file_id, "extlink", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lget_linkinfo(file_id, "extlink", &linfo, H5P_DEFAULT) < 0) TEST_ERROR;
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;
#endif /* NOTYET */

    /* Close open IDs */
    if(H5Pclose(lcpl_id)<0) TEST_ERROR;
    if(H5Sclose(space_id)<0) TEST_ERROR;
    if(H5Fclose(file_id)<0) TEST_ERROR;
    if(H5Pclose(fapl_id)<0) TEST_ERROR;
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
    H5Pclose(fapl_id);
    } H5E_END_TRY;
    return 1;
} /* end test_lcpl() */
#endif /* H5_GROUP_REVISION */


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
test_move(hid_t fapl)
{
    hid_t 	file_a, file_b=(-1);
    hid_t	grp_1=(-1), grp_2=(-1), grp_move=(-1), moved_grp=(-1);
    char 	filename[1024];

    TESTING("H5Lmove");

    /* Create two new files */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file_a=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        TEST_ERROR;
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file_b=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        TEST_ERROR;

    /* Create groups in first file */
    if((grp_1=H5Gcreate(file_a, "group1", 0))<0) TEST_ERROR;
    if((grp_2=H5Gcreate(file_a, "group2", 0))<0) TEST_ERROR;
    if((grp_move=H5Gcreate(grp_1, "group_move", 0))<0) TEST_ERROR;

    /* Create hard and soft links. */
    if(H5Lcreate_hard(grp_1, "group_move", H5L_SAME_LOC, "hard", H5P_DEFAULT, H5P_DEFAULT)<0)
	TEST_ERROR;
    if(H5Lcreate_soft("/group1/group_move", grp_2, "soft", H5P_DEFAULT, H5P_DEFAULT)<0)
	TEST_ERROR;

    /* Move a group within the file.  Both of source and destination use
     * H5L_SAME_LOC.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lmove(H5L_SAME_LOC, "group_move", H5L_SAME_LOC, "group_new_name", H5P_DEFAULT, H5P_DEFAULT)
                !=FAIL) TEST_ERROR;
    } H5E_END_TRY;

    /* Move a group across files.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lmove(grp_1, "group_move", file_b, "group_new_name", H5P_DEFAULT, H5P_DEFAULT)
                !=FAIL) TEST_ERROR;
    } H5E_END_TRY;

    /* Move a group across groups in the same file while renaming it. */
    if(H5Lmove(grp_1, "group_move", grp_2, "group_new_name", H5P_DEFAULT, H5P_DEFAULT)<0)
	TEST_ERROR;

    /* Open the group just moved to the new location. */
    if((moved_grp = H5Gopen(grp_2, "group_new_name"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;

    /* Verify that the group is no longer in the original location */
    H5E_BEGIN_TRY {
    if((moved_grp = H5Gopen(grp_1, "group_move"))>=0)
	TEST_ERROR;
    } H5E_END_TRY;

    /* Use H5Lmove to rename a group without moving it. */
    if(H5Lmove(grp_2, "group_new_name", H5L_SAME_LOC, "group_newer_name", H5P_DEFAULT, H5P_DEFAULT)<0)
	TEST_ERROR;

    /* Open the group. */
    if((moved_grp = H5Gopen(grp_2, "group_newer_name"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;

    /* Use H5Lmove to move a group without renaming it. */
    if(H5Lmove(grp_2, "group_newer_name", grp_1, "group_newer_name", H5P_DEFAULT, H5P_DEFAULT)<0)
	TEST_ERROR;

    /* Open the group . */
    if((moved_grp = H5Gopen(grp_1, "group_newer_name"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;

    /* Move the group while giving long paths. */
    if(H5Lmove(file_a, "/group1/group_newer_name", grp_2, "/group2/group_newest_name", H5P_DEFAULT, H5P_DEFAULT)<0)
	TEST_ERROR;

    /* Open the group just moved to the new location. */
    if((moved_grp = H5Gopen(grp_2, "group_newest_name"))<0)
	TEST_ERROR;

    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;

    /* Verify that the group is in no previous locations */
    H5E_BEGIN_TRY {
    if((moved_grp = H5Gopen(grp_1, "group_newer_name"))>=0)
	TEST_ERROR;
    if((moved_grp = H5Gopen(grp_2, "group_newer_name"))>=0)
	TEST_ERROR;
    if((moved_grp = H5Gopen(grp_2, "group_new_name"))>=0)
	TEST_ERROR;
    if((moved_grp = H5Gopen(grp_1, "group_copy"))>=0)
	TEST_ERROR;
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
test_copy(hid_t fapl)
{
    hid_t 	file_a, file_b=(-1);
    hid_t	grp_1=(-1), grp_2=(-1), grp_move=(-1), moved_grp=(-1);
    char 	filename[1024];

    TESTING("H5Lcopy");

    /* Create two new files */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file_a=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        TEST_ERROR;
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file_b=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        TEST_ERROR;

    /* Create groups in first file */
    if((grp_1=H5Gcreate(file_a, "group1", 0))<0) TEST_ERROR;
    if((grp_2=H5Gcreate(file_a, "group2", 0))<0) TEST_ERROR;
    if((grp_move=H5Gcreate(grp_1, "group_copy", 0))<0) TEST_ERROR;

    /* Create hard and soft links. */
    if(H5Lcreate_hard(grp_1, "group_copy", H5L_SAME_LOC, "hard", H5P_DEFAULT, H5P_DEFAULT)<0)
	TEST_ERROR;
    if(H5Lcreate_soft("/group1/group_copy", grp_2, "soft", H5P_DEFAULT, H5P_DEFAULT)<0)
	TEST_ERROR;

    /* Copy a group within the file.  Both of source and destination use
     * H5L_SAME_LOC.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lcopy(H5L_SAME_LOC, "group_copy", H5L_SAME_LOC, "group_new_name", H5P_DEFAULT, H5P_DEFAULT)
                !=FAIL) TEST_ERROR;
    } H5E_END_TRY;

    /* Copy a group across files.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lcopy(grp_1, "group_copy", file_b, "group_new_name", H5P_DEFAULT, H5P_DEFAULT)
                !=FAIL) TEST_ERROR;
    } H5E_END_TRY;

    /* Move a group across groups in the same file while renaming it. */
    if(H5Lcopy(grp_1, "group_copy", grp_2, "group_new_name", H5P_DEFAULT, H5P_DEFAULT)<0)
	TEST_ERROR;

    /* Open the group just moved to the new location. */
    if((moved_grp = H5Gopen(grp_2, "group_new_name"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;

    /* Verify that the group is also in the original location */
    if((moved_grp = H5Gopen(grp_1, "group_copy"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;

    /* Use H5Lcopy to create a group in the same location with a different name. */
    if(H5Lcopy(grp_2, "group_new_name", H5L_SAME_LOC, "group_newer_name", H5P_DEFAULT, H5P_DEFAULT)<0)
	TEST_ERROR;

    /* Open the group. */
    if((moved_grp = H5Gopen(grp_2, "group_newer_name"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;
    /* Verify that the group is also in the original location */
    if((moved_grp = H5Gopen(grp_2, "group_new_name"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;

    /* Use H5Lcopy to copy to a different location with the same name. */
    if(H5Lcopy(grp_2, "group_newer_name", grp_1, "group_newer_name", H5P_DEFAULT, H5P_DEFAULT)<0)
	TEST_ERROR;

    /* Open the group . */
    if((moved_grp = H5Gopen(grp_1, "group_newer_name"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;
    /* Verify that the group is still in the previous location */
    if((moved_grp = H5Gopen(grp_2, "group_new_name"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;

    /* Copy the group while giving long paths. */
    if(H5Lcopy(file_a, "/group1/group_newer_name", grp_2, "/group2/group_newest_name", H5P_DEFAULT, H5P_DEFAULT)<0)
	TEST_ERROR;

    /* Open the group just moved to the new location. */
    if((moved_grp = H5Gopen(grp_2, "group_newest_name"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;

    /* Verify that the group is still in all previous original locations */
    if((moved_grp = H5Gopen(grp_1, "group_newer_name"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;
    if((moved_grp = H5Gopen(grp_2, "group_newer_name"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;
    if((moved_grp = H5Gopen(grp_2, "group_new_name"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;
    if((moved_grp = H5Gopen(grp_1, "group_copy"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;

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

#ifdef H5_GROUP_REVISION
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
test_move_preserves(hid_t fapl_id)
{
    hid_t file_id=-1;
    hid_t group_id=-1;
    hid_t lcpl_id=-1;
    hid_t lcpl2_id=-1;
    H5G_stat_t statbuf;
    H5L_linkinfo_t linfo;
    time_t old_create_time;
    time_t old_modification_time;
    time_t curr_time;
    char filename[1024];

    TESTING("moving and copying links preserves their properties");

    /* Create file */
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);

    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id))<0) TEST_ERROR;

    /* Create a link creation property list with the UTF-8 character encoding */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) <0) TEST_ERROR;
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_UTF8) < 0) TEST_ERROR;
    /* Create a group with that lcpl */
    if((group_id = H5Gcreate_expand(file_id, H5P_DEFAULT, H5P_DEFAULT)) <0) TEST_ERROR;
    if(H5Llink(file_id, "group", group_id, lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Gclose(group_id) < 0) TEST_ERROR;

    /* Get the group's link's creation time */
    if(H5Lget_linkinfo(file_id, "group", &linfo, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Gget_objinfo(file_id, "group", TRUE, &statbuf) <0) TEST_ERROR;
    old_create_time = linfo.ctime;
    old_modification_time = statbuf.mtime;

    /* If this test happens too quickly, the creation times will all be the same.  Make sure the time changes. */
    curr_time=time(NULL);
    while(time(NULL) <= curr_time );

    /* Close the file and reopen it */
    if(H5Fclose(file_id)<0) TEST_ERROR;
    if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) <0) TEST_ERROR;

    /* Get the group's link's creation time.  The times should be unchanged */
    if(H5Lget_linkinfo(file_id, "group", &linfo, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Gget_objinfo(file_id, "group", TRUE, &statbuf) <0) TEST_ERROR;
    if(old_modification_time != statbuf.mtime) TEST_ERROR;
    if(old_create_time != linfo.ctime) TEST_ERROR;

    /* Create a new link to the group.  It should have a different creation time but the same modification time */
    if(H5Lcreate_hard(file_id, "group", file_id, "group2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Gget_objinfo(file_id, "group2", TRUE, &statbuf) <0) TEST_ERROR;
    if(old_modification_time != statbuf.mtime) TEST_ERROR;
    if(H5Lget_linkinfo(file_id, "group2", &linfo, H5P_DEFAULT) <0) TEST_ERROR;
    if(old_create_time == linfo.ctime) TEST_ERROR;

    /* Copy the first link to a UTF-8 name.  Its creation time and modification time should not change. */
    if(H5Lcopy(file_id, "group", file_id, "group_copied", lcpl_id, H5P_DEFAULT) <0) TEST_ERROR;
    if(H5Gget_objinfo(file_id, "group_copied", TRUE, &statbuf) <0) TEST_ERROR;
    if(old_modification_time != statbuf.mtime) TEST_ERROR;
    if(H5Lget_linkinfo(file_id, "group_copied", &linfo, H5P_DEFAULT) <0) TEST_ERROR;
    if(old_create_time != linfo.ctime) TEST_ERROR;

    /* Check that its character encoding is UTF-8 */
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;

    /* Move the link with the default property list. */
    if(H5Lmove(file_id, "group_copied", file_id, "group_copied2", H5P_DEFAULT, H5P_DEFAULT) <0) TEST_ERROR;
    if(H5Gget_objinfo(file_id, "group_copied2", TRUE, &statbuf) <0) TEST_ERROR;
    if(old_modification_time != statbuf.mtime) TEST_ERROR;
    if(H5Lget_linkinfo(file_id, "group_copied2", &linfo, H5P_DEFAULT) <0) TEST_ERROR;
    if(old_create_time != linfo.ctime) TEST_ERROR;

    /* Check that its character encoding is not UTF-8 */
    if(linfo.cset == H5T_CSET_UTF8) TEST_ERROR;

    /* Check that the original link is unchanged */
    if(H5Gget_objinfo(file_id, "group", TRUE, &statbuf) <0) TEST_ERROR;
    if(old_modification_time != statbuf.mtime) TEST_ERROR;
    if(H5Lget_linkinfo(file_id, "group", &linfo, H5P_DEFAULT) <0) TEST_ERROR;
    if(old_create_time != linfo.ctime) TEST_ERROR;
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;

    /* Move the first link to a UTF-8 name.  Its creation time and modification time should not change. */
    if(H5Lmove(file_id, "group", file_id, "group_moved", lcpl_id, H5P_DEFAULT) <0) TEST_ERROR;
    if(H5Gget_objinfo(file_id, "group_moved", TRUE, &statbuf) <0) TEST_ERROR;
    if(old_modification_time != statbuf.mtime) TEST_ERROR;
    if(H5Lget_linkinfo(file_id, "group_moved", &linfo, H5P_DEFAULT) <0) TEST_ERROR;
    if(old_create_time != linfo.ctime) TEST_ERROR;

    /* Check that its character encoding is UTF-8 */
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;

    /* Move the link again using the default property list. */
    if(H5Lmove(file_id, "group_moved", file_id, "group_moved_again", H5P_DEFAULT, H5P_DEFAULT) <0) TEST_ERROR;
    if(H5Gget_objinfo(file_id, "group_moved_again", TRUE, &statbuf) <0) TEST_ERROR;
    if(old_modification_time != statbuf.mtime) TEST_ERROR;
    if(H5Lget_linkinfo(file_id, "group_moved_again", &linfo, H5P_DEFAULT) <0) TEST_ERROR;
    if(old_create_time != linfo.ctime) TEST_ERROR;

    /* Check that its character encoding is not UTF-8 */
    if(linfo.cset == H5T_CSET_UTF8) TEST_ERROR;

    /* Close open IDs */
    if(H5Pclose(lcpl_id) < 0) TEST_ERROR;
    if(H5Fclose(file_id)<0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(lcpl_id);
	H5Pclose(lcpl2_id);
        H5Gclose(group_id);
        H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* end test_move_preserves() */
#endif /* H5_GROUP_REVISION */


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
static int
test_compat(hid_t fapl)
{
    hid_t file_id=-1;
    hid_t group1_id=-1;
    hid_t group2_id=-1;
    H5G_stat_t	sb_hard1, sb_hard2, sb_soft1;
    char filename[1024];
    char linkval[1024];

    TESTING("backwards compatibility");

    /* Create file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create two groups in the file */
    if((group1_id = H5Gcreate(file_id, "group1", 1)) < 0) TEST_ERROR;
    if((group2_id = H5Gcreate(file_id, "group2", 1)) < 0) TEST_ERROR;

    /* Create links using H5Glink and H5Glink2 */
    if(H5Glink(file_id, H5G_LINK_HARD, "group2", "group1/link_to_group2") < 0) TEST_ERROR;
    if(H5Glink2(file_id, "group1", H5G_LINK_HARD, group2_id, "link_to_group1") < 0) TEST_ERROR;
    if(H5Glink2(file_id, "link_to_group1", H5G_LINK_SOFT, H5G_SAME_LOC, "group2/soft_link_to_group1") < 0) TEST_ERROR;

    /* Test that H5Glink created hard links properly */
    if(H5Gget_objinfo(file_id, "/group2", TRUE, &sb_hard1)<0) TEST_ERROR;
    if(H5Gget_objinfo(file_id, "/group1/link_to_group2", TRUE, &sb_hard2)<0) TEST_ERROR;

    if (HDmemcmp(&sb_hard1.objno, sb_hard2.objno, sizeof(sb_hard1.objno))) {
        H5_FAILED();
        puts("    Hard link test failed.  Link seems not to point to the ");
        puts("    expected file location.");
        TEST_ERROR;
    }

    /* Test for the other hard link created */
    if(H5Gget_objinfo(file_id, "/group1", TRUE, &sb_hard1)<0) TEST_ERROR;
    if(H5Gget_objinfo(file_id, "/group2/link_to_group1", TRUE, &sb_hard2)<0) TEST_ERROR;

    if (HDmemcmp(&sb_hard1.objno, sb_hard2.objno, sizeof(sb_hard1.objno))) {
        H5_FAILED();
        puts("    Hard link test failed.  Link seems not to point to the ");
        puts("    expected file location.");
        TEST_ERROR;
    }

    /* Test the soft link */
    if(H5Gget_objinfo(file_id, "/group2/soft_link_to_group1", FALSE, &sb_soft1)<0) TEST_ERROR;
    if(sb_soft1.type != H5G_LINK) TEST_ERROR;
    if(sb_soft1.linklen != HDstrlen("link_to_group1") + 1) TEST_ERROR;

    if(H5Gget_linkval(group2_id, "soft_link_to_group1", sb_soft1.linklen, linkval) < 0) TEST_ERROR;
    if(HDstrcmp("link_to_group1", linkval)) TEST_ERROR;


    /* Test H5Gmove and H5Gmove2 */
    if(H5Gmove(file_id, "group1", "moved_group1") < 0) TEST_ERROR;
    if(H5Gmove2(file_id, "group2", group1_id, "moved_group2") < 0) TEST_ERROR;

    /* Ensure that both groups can be opened */
    if(H5Gclose(group2_id)<0) TEST_ERROR;
    if(H5Gclose(group1_id)<0) TEST_ERROR;

    if((group1_id = H5Gopen(file_id, "moved_group1")) < 0) TEST_ERROR;
    if((group2_id = H5Gopen(file_id, "moved_group1/moved_group2")) < 0) TEST_ERROR;

    /* Close open IDs */
    if(H5Gclose(group2_id)<0) TEST_ERROR;
    if(H5Gclose(group1_id)<0) TEST_ERROR;

    /* Test H5Gunlink */
    if(H5Gunlink(file_id, "moved_group1/moved_group2") < 0) TEST_ERROR;

    H5E_BEGIN_TRY {
        if(H5Gopen(file_id, "moved_group1/moved_group2") >=0) TEST_ERROR;
    } H5E_END_TRY;

    if(H5Fclose(file_id)<0) TEST_ERROR;

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
external_link_root(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    H5G_stat_t	sb;                             /* Object information */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE];
    char	filename2[NAME_BUF_SIZE];
    char       *file;				/* File from external link */
    char       *path;				/* Path from external link */

    TESTING("external link to root");

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create file to point to */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;

    /* Check that external links are registered with the library */
    if(H5Lis_registered(H5L_LINK_EXTERNAL) != TRUE) TEST_ERROR

    /* Create file with link to first file */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create external link to object in first file */
    if(H5Lcreate_external(filename1, "/", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Check information for external link */
    if (H5Gget_objinfo(fid, "ext_link", FALSE, &sb)<0) goto error;
    if (H5G_UDLINK!=sb.type) {
	H5_FAILED();
	puts("    Unexpected object type - should have been an external link");
	goto error;
    }
    if(H5Lget_linkval(fid, "ext_link", sizeof(objname), objname, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Lunpack_elink_val(objname, &file, &path) < 0) TEST_ERROR
    if(HDstrcmp(file, filename1))
    {
	H5_FAILED();
	puts("    External link file name incorrect");
	goto error;
    }
    if(HDstrcmp(path, "/"))
    {
	H5_FAILED();
	puts("    External link path incorrect");
	goto error;
    }

    /* Close and re-open file to ensure that data is written to disk */
    if(H5Fclose(fid) < 0) TEST_ERROR;
    if((fid = H5Fopen(filename2, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) TEST_ERROR;


    /* Open object through external link */
    if((gid = H5Gopen(fid, "ext_link")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(name_len != 0) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate(gid, "new_group", (size_t)0)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object (lets first file close) */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Close second file */
    if(H5Fclose(fid)<0) TEST_ERROR;


    /* Open first file again and check on object created */
    if((fid = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Open object created through external link */
    if((gid = H5Gopen(fid, "new_group")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Close first file */
    if(H5Fclose(fid)<0) TEST_ERROR;

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Fclose (gid2);
    	H5Fclose (gid);
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
external_link_path(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE];
    char	filename2[NAME_BUF_SIZE];

    TESTING("external link to object on path");

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create file to point to */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create object down a path */
    if((gid = H5Gcreate(fid, "A", (size_t)0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate(fid, "A/B", (size_t)0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate(fid, "A/B/C", (size_t)0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;


    /* Create file with link to first file */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create external link to object in first file */
    if(H5Lcreate_external(filename1, "/A/B/C", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Open object through external link */
    if((gid = H5Gopen(fid, "ext_link")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(name_len != 0) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate(gid, "new_group", (size_t)0)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object (lets first file close) */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Close second file */
    if(H5Fclose(fid)<0) TEST_ERROR;


    /* Open first file again and check on object created */
    if((fid = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Open object created through external link */
    if((gid = H5Gopen(fid, "/A/B/C/new_group")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/A/B/C/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Close first file */
    if(H5Fclose(fid)<0) TEST_ERROR;


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
external_link_mult(hid_t fapl)
{
    hid_t	fid = (-1), fid2 = (-1); 	/* File IDs */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],
    		filename3[NAME_BUF_SIZE],
    		filename4[NAME_BUF_SIZE];       /* Names of files to externally link across */

    TESTING("external links across multiple files");

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);
    h5_fixname(FILENAME[5], fapl, filename3, sizeof filename3);
    h5_fixname(FILENAME[6], fapl, filename4, sizeof filename4);

    /* Create first file to point to */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create object down a path */
    if((gid = H5Gcreate(fid, "A", (size_t)0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate(fid, "A/B", (size_t)0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate(fid, "A/B/C", (size_t)0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;

    /* Create second file to point to */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create external link down a path */
    if((gid = H5Gcreate(fid, "D", (size_t)0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate(fid, "D/E", (size_t)0)) < 0) TEST_ERROR

    /* Create external link to object in first file */
    if(H5Lcreate_external(filename1, "/A/B/C", gid, "F", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;

    /* Create third file to point to */
    if((fid=H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create external link down a path */
    if((gid = H5Gcreate(fid, "G", (size_t)0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if((gid = H5Gcreate(fid, "G/H", (size_t)0)) < 0) TEST_ERROR

    /* Create external link to object in second file */
    if(H5Lcreate_external(filename2, "/D/E/F", gid, "I", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;


    /* Create file with link to third file */
    if((fid=H5Fcreate(filename4, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create external link to object in first file */
    if(H5Lcreate_external(filename3, "/G/H/I", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Open object through external link */
    if((gid = H5Gopen(fid, "ext_link")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(name_len != 0) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate(gid, "new_group", (size_t)0)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object (lets first file close) */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Close second file */
    if(H5Fclose(fid)<0) TEST_ERROR;


    /* Open first file again and check on object created */
    if((fid = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Open object created through external link */
    if((gid = H5Gopen(fid, "/A/B/C/new_group")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/A/B/C/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid)<0) TEST_ERROR


    /* Open an object through external links */
    if((fid = H5Fopen(filename4, H5F_ACC_RDONLY, H5P_DEFAULT)) <0) TEST_ERROR
    if((gid = H5Gopen(fid, "ext_link")) < 0) TEST_ERROR

    /* The intermediate files should not stay open. Replace one of them with a new file. */
    if((fid2=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
    if(H5Fclose(fid2)<0) TEST_ERROR

    /* Open the other with write access and delete the external link in it */  
    if((fid2=H5Fopen(filename3, H5F_ACC_RDWR, fapl))<0) TEST_ERROR
    if(H5Lunlink(fid2, "G/H/I", H5P_DEFAULT) < 0) TEST_ERROR
      
    if(H5Fclose(fid2)<0) TEST_ERROR

    /* Cleanup */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid)<0) TEST_ERROR

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
#ifdef H5_GROUP_REVISION
static int
external_link_self(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    hid_t	lcpl_id = (-1);     		/* Link Creation Property List ID */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE];
    char	filename2[NAME_BUF_SIZE];
    char	filename3[NAME_BUF_SIZE];

    TESTING("external link to self");

    /* Set up filename */
    h5_fixname(FILENAME[1], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[2], fapl, filename2, sizeof filename1);
    h5_fixname(FILENAME[3], fapl, filename3, sizeof filename1);

    /* Create file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create an lcpl with intermediate group creation set */
    if((lcpl_id=H5Pcreate(H5P_LINK_CREATE))<0) TEST_ERROR
    if(H5Pset_create_intermediate_group(lcpl_id, TRUE) < 0) TEST_ERROR

    /* Create a series of groups within the file: /A/B and /X/Y/Z */
    if((gid=H5Gcreate_expand(fid, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Llink(fid, "A/B", gid, lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if((gid=H5Gcreate_expand(fid, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Llink(fid, "X/Y", gid, lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    if(H5Pclose (lcpl_id) <0) TEST_ERROR

    /* Create external link to own root group*/
    if(H5Lcreate_external(filename1, "/X", fid, "A/B/C", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Open object through external link */
    if((gid = H5Gopen(fid, "A/B/C/")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(name_len != 0) TEST_ERROR

    /* Create object through external link */
    if((gid2 = H5Gcreate(gid, "new_group", (size_t)0)) < 0) TEST_ERROR

    /* Close created group */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close object opened through external link */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Check on object created */
    if((gid = H5Gopen(fid, "X/new_group")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/X/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Close first file */
    if(H5Fclose(fid)<0) TEST_ERROR;


    /* Complicate things. Use this file as an intermediate file in a chain
     * of external links that will go: file2 -> file1 -> file1 -> file3
     */

    /* Create file2 with an external link to file1  */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    if(H5Lcreate_external(filename1, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file2 */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create file3 as a target */
    if((fid=H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR
    if((gid=H5Gcreate(fid, "end", 0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open file1 and create an extlink pointing to file3 */
    if((fid=H5Fopen(filename1, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) TEST_ERROR

    if(H5Lcreate_external(filename3, "/", fid, "/X/Y/Z", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Close file1 */
    if(H5Fclose(fid) < 0) TEST_ERROR


    /* Re-open file2 and traverse through file1 (with its recursive extlink) to file3 */
    if((fid=H5Fopen(filename2, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) TEST_ERROR

    if((gid=H5Gopen(fid, "ext_link/B/C/Y/Z/end")) < 0) TEST_ERROR
    
    /* Create object through external link */
    if((gid2 = H5Gcreate(gid, "newer_group", 0)) < 0) TEST_ERROR

    /* Cleanup */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open up file3 and make sure the object was created successfully */
    if((fid=H5Fopen(filename3, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) TEST_ERROR

    if((gid=H5Gopen(fid, "end/newer_group")) < 0) TEST_ERROR

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
#endif /* H5_GROUP_REVISION */


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
external_link_pingpong(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */

    TESTING("external links back and forth");

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create external links for chain */
    if(H5Lcreate_external(filename2, "/link2", fid, "link1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename2, "/link4", fid, "link3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename2, "/link6", fid, "link5", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Create final object */
    if((gid = H5Gcreate(fid, "final", (size_t)0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;

    /* Create second file */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create external links for chain */
    if(H5Lcreate_external(filename1, "/link3", fid, "link2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename1, "/link5", fid, "link4", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename1, "/final", fid, "link6", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;


    /* Open first file */
    if((fid=H5Fopen(filename1, H5F_ACC_RDWR, fapl))<0) TEST_ERROR;

    /* Open object through external link */
    if((gid = H5Gopen(fid, "link1")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(name_len != 0) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate(gid, "new_group", (size_t)0)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object (lets first file close) */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Close first file */
    if(H5Fclose(fid)<0) TEST_ERROR;


    /* Open first file again and check on object created */
    if((fid = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Open object created through external link */
    if((gid = H5Gopen(fid, "/final/new_group")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/final/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Close first file */
    if(H5Fclose(fid)<0) TEST_ERROR;


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
 *              more than H5G_NLINKS.  Use a "back & forth" style of
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
external_link_toomany(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */

    TESTING("too many external links");

    /* Make certain test is valid */
    /* XXX: should probably make a "generic" test that creates the proper
     *          # of links based on this value - QAK
     */
    HDassert(H5G_NLINKS == 16);

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create external links for chain */
    if(H5Lcreate_external(filename2, "/link2", fid, "link1", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename2, "/link4", fid, "link3", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename2, "/link6", fid, "link5", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename2, "/link8", fid, "link7", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename2, "/link10", fid, "link9", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename2, "/link12", fid, "link11", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename2, "/link14", fid, "link13", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename2, "/link16", fid, "link15", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename2, "/final", fid, "link17", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;

    /* Create second file */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create external links for chain */
    if(H5Lcreate_external(filename1, "/link3", fid, "link2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename1, "/link5", fid, "link4", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename1, "/link7", fid, "link6", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename1, "/link9", fid, "link8", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename1, "/link11", fid, "link10", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename1, "/link13", fid, "link12", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename1, "/link15", fid, "link14", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename1, "/link17", fid, "link16", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Create final object */
    if((gid = H5Gcreate(fid, "final", (size_t)0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;


    /* Open first file */
    if((fid=H5Fopen(filename1, H5F_ACC_RDWR, fapl))<0) TEST_ERROR;

    /* Open object through external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen(fid, "link1");
    } H5E_END_TRY;
    if (gid >= 0) {
	H5_FAILED();
	puts("    Should have failed for sequence of too many nested links.");
	goto error;
    }

    /* Open object through external link */
    if((gid = H5Gopen(fid, "link3")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(name_len != 0) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate(gid, "new_group", (size_t)0)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Close first file */
    if(H5Fclose(fid)<0) TEST_ERROR;


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
external_link_dangling(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */

    TESTING("dangling external links");

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create dangling external links */
    if(H5Lcreate_external("missing", "/missing", fid, "no_file", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external(filename2, "/missing", fid, "no_object", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;

    /* Create second file (for dangling object test) */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;


    /* Open first file */
    if((fid=H5Fopen(filename1, H5F_ACC_RDWR, fapl))<0) TEST_ERROR;

    /* Open object through dangling file external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen(fid, "no_file");
    } H5E_END_TRY;
    if (gid >= 0) {
	H5_FAILED();
	puts("    Should have failed for sequence of too many nested links.");
	goto error;
    }

    /* Open object through dangling object external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen(fid, "no_object");
    } H5E_END_TRY;
    if (gid >= 0) {
	H5_FAILED();
	puts("    Should have failed for sequence of too many nested links.");
	goto error;
    }

    /* Close first file */
    if(H5Fclose(fid)<0) TEST_ERROR;


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
external_link_recursive(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    char	filename1[NAME_BUF_SIZE];       /* Names of files to externally link across */

    TESTING("recursive external links");

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);

    /* Create first file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create recursive external links */
    if(H5Lcreate_external(filename1, "/recursive", fid, "recursive", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;


    /* Open file */
    if((fid=H5Fopen(filename1, H5F_ACC_RDONLY, fapl))<0) TEST_ERROR;

    /* Open object through dangling file external link */
    H5E_BEGIN_TRY {
        gid = H5Gopen(fid, "recursive");
    } H5E_END_TRY;
    if (gid >= 0) {
	H5_FAILED();
	puts("    Should have failed for recursive external links.");
	goto error;
    }

    /* Close first file */
    if(H5Fclose(fid)<0) TEST_ERROR;


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
external_link_query(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group IDs */
    char       *file_name;                      /* Name of the file the external link points to */
    char       *object_name;                    /* Name of the object the external link points to */
    H5G_stat_t	sb;                             /* Object information */
    H5L_linkinfo_t li;                          /* Link information */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE],       /* Names of files to externally link across */
                query_buf[NAME_BUF_SIZE];       /* Buffer to hold query result */


    TESTING("query aspects of external link");

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file, with external link to object in second file */
    if((fid=H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Create external link */
    if(H5Lcreate_external(filename2, "/dst", fid, "src", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Get size of buffer for external link */
    if(H5Lget_linkinfo(fid, "src", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.u.link_size != (HDstrlen(filename2) + HDstrlen("/dst") + 2)) TEST_ERROR
    if (H5L_LINK_EXTERNAL != li.linkclass) {
	H5_FAILED();
	puts("    Unexpected link class - should have been an external link");
	goto error;
    }

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;

    /* Create second file to point to */
    if((fid=H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Create object to link to */
    if((gid = H5Gcreate(fid, "dst", (size_t)0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR


    /* Open first file */
    if((fid=H5Fopen(filename1, H5F_ACC_RDONLY, fapl))<0) TEST_ERROR

    /* Get size of buffer for external link */
    if(H5Lget_linkinfo(fid, "src", &li, H5P_DEFAULT) < 0) TEST_ERROR
    if(li.u.link_size != (HDstrlen(filename2) + HDstrlen("/dst") + 2)) TEST_ERROR
    if (H5L_LINK_EXTERNAL != li.linkclass) {
	H5_FAILED();
	puts("    Unexpected link class - should have been an external link");
	goto error;
    }

    /* Get information for external link.  It should be two strings right after each other */
    if(H5Lget_linkval(fid, "src", NAME_BUF_SIZE, query_buf, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Extract the file and object names from the buffer */
    if(H5Lunpack_elink_val(query_buf, &file_name, &object_name) < 0) TEST_ERROR

    /* Compare the file and object names */
    if(strcmp(file_name, filename2)) TEST_ERROR;
    if(strcmp(object_name, "/dst")) TEST_ERROR

    /* Query information about object that external link points to */
    if (H5Gget_objinfo(fid, "src", TRUE, &sb)<0) goto error;
    if (H5G_GROUP != sb.type) {
	H5_FAILED();
	puts("    Unexpected object type - should have been a group");
	goto error;
    }

    /* Close first file */
    if(H5Fclose(fid)<0) TEST_ERROR;

    /* Make sure that passing in NULLs to H5Lunpack_elink_val works */
    if(H5Lunpack_elink_val(query_buf, NULL, NULL) < 0) TEST_ERROR

    /* Make sure that bogus cases trigger errors in H5Lunpack_elink_val */
    H5E_BEGIN_TRY {
      if(H5Lunpack_elink_val(NULL, NULL, NULL) >= 0) TEST_ERROR
    } H5E_END_TRY

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid);
    	H5Fclose (fid);
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
external_link_unlink_compact(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */

    TESTING("unlinking external link in compact group");

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
    if((gid = H5Gcreate(fid, "dst", (size_t)0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


/* Unlink external link */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Unlink external link */
    if(H5Gunlink(fid, "src") < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open group for external link */
    if((gid = H5Gopen(fid, "dst")) < 0) TEST_ERROR

    /* Close group */
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
#ifdef H5_GROUP_REVISION
static int
external_link_unlink_dense(hid_t fapl)
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

    TESTING("unlinking external link in dense group");

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file, with external link to object in second file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Open root group */
    if((gid = H5Gopen(fid, "/")) < 0) TEST_ERROR

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR

    /* Query the group creation properties */
    if((gcpl = H5Gget_create_plist(gid)) < 0) TEST_ERROR
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Create external link */
    /* (This also covers the case of having an external link in a compact group that's converted to a dense group) */
    if(H5Lcreate_external(filename2, "/dst", gid, "src", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR;
    if(H5G_has_links_test(gid, &nmsgs) != TRUE) TEST_ERROR;
    if(nmsgs != 1) TEST_ERROR;
    if(H5G_has_stab_test(gid) == TRUE) TEST_ERROR;

    /* Create enough objects in the root group to change it into a "dense" group */
    for(u = 0; u < max_compact; u++) {
        sprintf(objname, "filler %u\n", u);
        if((gid2 = H5Gcreate(gid, objname, (size_t)0)) < 0) TEST_ERROR
        if(H5Gclose(gid2) < 0) TEST_ERROR
    } /* end for */

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR;
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR;
    if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR;

    /* Close group creation property list */
    if(H5Pclose(gcpl) < 0) TEST_ERROR

    /* Close root group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file to point to */
    if((fid = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object to link to */
    if((gid = H5Gcreate(fid, "dst", (size_t)0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


/* Unlink external link */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Open root group */
    if((gid = H5Gopen(fid, "/")) < 0) TEST_ERROR

    /* Unlink external link */
    if(H5Gunlink(fid, "src") < 0) TEST_ERROR

    /* Remove enough objects in the root group to change it into a "compact" group */
    for(u = 0; u < ((max_compact - min_dense) + 1); u++) {
        sprintf(objname, "filler %u\n", u);
        if(H5Gunlink(gid, objname) < 0) TEST_ERROR
    } /* end for */

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR;
    if(H5G_has_links_test(gid, &nmsgs) != TRUE) TEST_ERROR;
    if(nmsgs != (min_dense - 1)) TEST_ERROR;
    if(H5G_has_stab_test(gid) == TRUE) TEST_ERROR;

    /* Close root group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open group for external link (should be unaffected) */
    if((gid = H5Gopen(fid, "dst")) < 0) TEST_ERROR

    /* Close group */
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
} /* end external_link_unlink_dense() */
#endif /* H5_GROUP_REVISION */


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
external_link_move(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename1[NAME_BUF_SIZE],
    		filename2[NAME_BUF_SIZE];       /* Names of files to externally link across */

    TESTING("move external link");

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
    if((gid = H5Gcreate(fid, "dst", (size_t)0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR


/* Move external link to different name within same group */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Move external link within same group */
    if(H5Gmove(fid, "src", "src2") < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen(fid, "src2")) < 0) TEST_ERROR

    /* Check name */
    if((name_len = H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(name_len != 0) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate(gid, "new_group", (size_t)0)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open group created through external link */
    if((gid = H5Gopen(fid, "dst/new_group")) < 0) TEST_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

/* Move external link to different group */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Create another group, to move the external link into */
    if((gid = H5Gcreate(fid, "group2", (size_t)0)) < 0) TEST_ERROR

    /* Move external link to different group */
    if(H5Gmove2(fid, "src2", gid, "src3") < 0) TEST_ERROR

    /* Close new group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen(fid, "/group2/src3")) < 0) TEST_ERROR

    /* Check name */
    if((name_len = H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(name_len != 0) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate(gid, "new_group2", (size_t)0)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open group created through external link */
    if((gid = H5Gopen(fid, "dst/new_group2")) < 0) TEST_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

/* Move external link back to original group */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen(fid, "/group2/src3")) < 0) TEST_ERROR

    /* Check name */
    if((name_len = H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(name_len != 0) TEST_ERROR

    /* Move external link back to original location */
    if(H5Gmove(fid, "/group2/src3", "/src") < 0) TEST_ERROR

    /* Check name */
    if((name_len = H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(name_len != 0) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate(gid, "new_group3", (size_t)0)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open group created through external link */
    if((gid = H5Gopen(fid, "dst/new_group3")) < 0) TEST_ERROR

    /* Close group */
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
} /* end external_link_move() */


#ifdef H5_GROUP_REVISION
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
external_link_ride(hid_t fapl)
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

    TESTING("external link along for the ride");

    /* Set up filenames */
    h5_fixname(FILENAME[3], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[4], fapl, filename2, sizeof filename2);

    /* Create first file, with external link to object in second file */
    if((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Open root group */
    if((gid = H5Gopen(fid, "/")) < 0) TEST_ERROR

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) != TRUE) TEST_ERROR

    /* Query the group creation properties */
    if((gcpl = H5Gget_create_plist(gid)) < 0) TEST_ERROR
    if(H5Pget_link_phase_change(gcpl, &max_compact, &min_dense) < 0) TEST_ERROR

    /* Create enough objects in the root group to change it into a "dense" group */
    for(u = 0; u < (max_compact + 1); u++) {
        sprintf(objname, "filler %u\n", u);
        if((gid2 = H5Gcreate(gid, objname, (size_t)0)) < 0) TEST_ERROR
        if(H5Gclose(gid2) < 0) TEST_ERROR
    } /* end for */

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR;
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR;
    if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR;

    /* Create external link */
    /* (This also covers the case of adding an external link to a dense group) */
    if(H5Lcreate_external(filename2, "/dst", gid, "src", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR;
    if(H5G_has_links_test(gid, NULL) == TRUE) TEST_ERROR;
    if(H5G_has_stab_test(gid) != TRUE) TEST_ERROR;

    /* Close group creation property list */
    if(H5Pclose(gcpl) < 0) TEST_ERROR

    /* Close root group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Create second file to point to */
    if((fid = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR

    /* Create object to link to */
    if((gid = H5Gcreate(fid, "dst", (size_t)0)) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

/* Remove enough objects to convert group containing external link back into compact form */

    /* Open first file */
    if((fid = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen(fid, "src")) < 0) TEST_ERROR

    /* Check name */
    if((name_len = H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(name_len != 0) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate(gid, "new_group", (size_t)0)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Open root group */
    if((gid = H5Gopen(fid, "/")) < 0) TEST_ERROR

    /* Remove enough objects in the root group to change it into a "compact" group */
    for(u = 0; u < ((max_compact - min_dense) + 3); u++) {
        sprintf(objname, "filler %u\n", u);
        if(H5Gunlink(gid, objname) < 0) TEST_ERROR
    } /* end for */

    /* Check on root group's status */
    if(H5G_is_empty_test(gid) == TRUE) TEST_ERROR;
    if(H5G_has_links_test(gid, &nmsgs) != TRUE) TEST_ERROR;
    if(nmsgs != (min_dense - 1)) TEST_ERROR;
    if(H5G_has_stab_test(gid) == TRUE) TEST_ERROR;

    /* Close root group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Open object through external link */
    if((gid = H5Gopen(fid, "src")) < 0) TEST_ERROR

    /* Check name */
    if((name_len = H5Iget_name(gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(name_len != 0) TEST_ERROR

    /* Create object in external file */
    if((gid2 = H5Gcreate(gid, "new_group2", (size_t)0)) < 0) TEST_ERROR

    /* Close group in external file */
    if(H5Gclose(gid2) < 0) TEST_ERROR

    /* Close external object */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close first file */
    if(H5Fclose(fid) < 0) TEST_ERROR

    /* Open second file */
    if((fid = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR

    /* Open group created through external link */
    if((gid = H5Gopen(fid, "dst/new_group")) < 0) TEST_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Open group created through external link */
    if((gid = H5Gopen(fid, "dst/new_group2")) < 0) TEST_ERROR

    /* Close group */
    if(H5Gclose(gid) < 0) TEST_ERROR

    /* Close file */
    if(H5Fclose(fid) < 0) TEST_ERROR

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
#endif /* H5_GROUP_REVISION */


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
ext_link_endian(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    hid_t       lapl_id = (-1);                 /* Prop List ID */
    char      * srcdir = getenv("srcdir");      /* The source directory */
    char        pathbuf[NAME_BUF_SIZE];         /* Path to the files */
    char        namebuf[NAME_BUF_SIZE];

    TESTING("endianness of external links");

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
    if((lapl_id = H5Pcreate(H5P_LINK_ACCESS)) < 0) TEST_ERROR;
    if(H5Pinsert(lapl_id, H5L_ELINK_PREFIX_PROP, strlen(pathbuf) + 1, pathbuf,
                 NULL, NULL, NULL, NULL, NULL, NULL) < 0) TEST_ERROR;

    if(HDstrlen(pathbuf) + HDstrlen(LE_FILENAME) >= sizeof(namebuf)) TEST_ERROR;

    HDstrcpy(namebuf, pathbuf);
    HDstrcat(namebuf, LE_FILENAME);

    /* Test LE file; try to open a group through the external link */
    if((fid = H5Fopen(namebuf, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;
    if((gid = H5Oopen(fid, "ext_link", lapl_id)) < 0) TEST_ERROR;

    /* Open a group in the external file using that group ID */
    if((gid2 = H5Gopen(gid, "subgroup")) < 0) TEST_ERROR;

    /* Close the IDs */
    if(H5Gclose(gid2) < 0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;
    if(H5Fclose(fid) < 0) TEST_ERROR;

    if(HDstrlen(pathbuf) + HDstrlen(BE_FILENAME) >= sizeof(namebuf)) TEST_ERROR;

    HDstrcpy(namebuf, pathbuf);
    HDstrcat(namebuf, BE_FILENAME);

    /* Test BE file; try to open a group through the external link */
    if((fid = H5Fopen(namebuf, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;
    if((gid = H5Oopen(fid, "ext_link", lapl_id)) < 0) TEST_ERROR;

    /* Open a group in the external file using that group ID */
    if((gid2 = H5Gopen(gid, "subgroup")) < 0) TEST_ERROR;

    /* Close the IDs */
    if(H5Gclose(gid2) < 0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;
    if(H5Fclose(fid) < 0) TEST_ERROR;

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
static herr_t UD_hard_create(const char UNUSED * link_name, hid_t loc_group, void * udata, size_t udata_size, hid_t UNUSED lcpl_id)
{
    haddr_t addr;
    hid_t target_obj = -1;
    herr_t ret_value = 0;

    if(udata_size != sizeof(haddr_t))
    {
      ret_value = -1;
      goto done;
    }

    addr = *((haddr_t *) udata);

    /* Open the object this link points to */
    target_obj= H5Oopen_by_addr(loc_group, addr);
    if(target_obj < 0)
    {
      ret_value = -1;
      goto done;
    }

    /* Increment the reference count of the target object */
    if(H5Oincr_refcount(target_obj) < 0)
    {
      ret_value = -1;
      goto done;
    }

done:
    /* Close the target object if we opened it */
    if(target_obj >= 0)
    {
        switch(H5Iget_type(target_obj))
        {
            case H5I_GROUP:
                if(H5Gclose(target_obj) <0)
                  ret_value = -1;
                break;
            case H5I_DATASET:
                if(H5Dclose(target_obj) <0)
                  ret_value = -1;
                break;
            case H5I_DATATYPE:
                if(H5Tclose(target_obj) <0)
                  ret_value = -1;
                break;
            default:
              return -1;
        }
    }

    return ret_value;
}

/* UD_hard_delete decrements the object's reference count */
static herr_t UD_hard_delete(const char UNUSED * link_name, hid_t loc_group, void * udata, size_t udata_size)
{
    haddr_t addr;
    hid_t target_obj = -1;
    herr_t ret_value = 0;

    if(udata_size != sizeof(haddr_t))
    {
      ret_value = -1;
      goto done;
    }

    addr = *((haddr_t *) udata);

    /* Open the object this link points to */
    target_obj= H5Oopen_by_addr(loc_group, addr);
    if(target_obj < 0)
    {
      ret_value = -1;
      goto done;
    }

    /* Decrement the reference count of the target object */
    if(H5Odecr_refcount(target_obj) < 0)
    {
      ret_value = -1;
      goto done;
    }

done:
    /* Close the target object if we opened it */
    if(target_obj >= 0)
    {
        switch(H5Iget_type(target_obj))
        {
            case H5I_GROUP:
                if(H5Gclose(target_obj) <0)
                  ret_value = -1;
                break;
            case H5I_DATASET:
                if(H5Dclose(target_obj) <0)
                  ret_value = -1;
                break;
            case H5I_DATATYPE:
                if(H5Tclose(target_obj) <0)
                  ret_value = -1;
                break;
            default:
              return -1;
        }
    }

    return ret_value;
}

static hid_t UD_hard_traverse(const char UNUSED *link_name, hid_t cur_group, void * udata, size_t udata_size, hid_t UNUSED lapl_id)
{
    haddr_t addr;
    hid_t         ret_value = -1;

    if(udata_size != sizeof(haddr_t))
      return -1;

    addr = *((haddr_t *) udata);

    ret_value = H5Oopen_by_addr(cur_group, addr); /* If this fails, our return value will be negative. */

    return ret_value;
}

const H5L_link_class_t UD_hard_class[1] = {{
    H5L_LINK_CLASS_T_VERS,           /* H5L_link_class_t version       */
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
    H5L_linkinfo_t li;                          /* Link information */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    h5_stat_size_t empty_size;                  /* Size of an empty file */
    char	filename[NAME_BUF_SIZE];

    TESTING("user-defined hard link");

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Get the size of the empty file for reference */
    if(H5Fclose(fid) < 0) TEST_ERROR;
    if((empty_size=h5_get_file_size(filename))==0) TEST_ERROR;

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Check that external links are registered and UD hard links are not */
    if(H5Lis_registered(H5L_LINK_EXTERNAL) != TRUE) TEST_ERROR
    if(H5Lis_registered(UD_HARD_TYPE) != 0) TEST_ERROR

    /* Register "user-defined hard links" with the library */
    if(H5Lregister(UD_hard_class) < 0) TEST_ERROR;

    /* Check that UD hard links are now registered */
    if(H5Lis_registered(H5L_LINK_EXTERNAL) != TRUE) TEST_ERROR
    if(H5Lis_registered(UD_HARD_TYPE) != TRUE) TEST_ERROR

    /* Create a group for the UD hard link to point to */
    if((gid = H5Gcreate(fid, "group", 0)) <0) TEST_ERROR;

    /* Get address for the group to give to the hard link */
    if (H5Lget_linkinfo(fid, "group", &li, H5P_DEFAULT)<0) TEST_ERROR;

    if(H5Gclose(gid) < 0) TEST_ERROR;


    /* Create a user-defined "hard link" to the group using the address we got
     * from H5Lget_linkinfo */
    if(H5Lcreate_ud(fid, "ud_link", UD_HARD_TYPE, &(li.u.address), sizeof(haddr_t), H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Close and re-open file to ensure that data is written to disk */
    if(H5Fclose(fid) < 0) TEST_ERROR;
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Open group through UD link */
    if((gid = H5Gopen(fid, "ud_link")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(name_len != 0) TEST_ERROR

    /* Create object in group */
    if((gid2 = H5Gcreate(gid, "new_group", (size_t)0)) < 0) TEST_ERROR

    /* Close groups*/
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Re-open group without using ud link to check that it was created properly */
    if((gid = H5Gopen(fid, "group/new_group")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/group/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Check that H5Gget_objinfo works on the hard link */
    if(H5Lget_linkinfo(fid, "ud_link", &li, H5P_DEFAULT) < 0) TEST_ERROR
    /* UD hard links have no query function, thus return a "link length" of 0 */
    if(li.u.link_size != 0) TEST_ERROR
    if (UD_HARD_TYPE != li.linkclass) {
	H5_FAILED();
	puts("    Unexpected link class - should have been a UD hard link");
	goto error;
    }

    /* Unlink the group pointed to by the UD link.  It shouldn't be
     * deleted because of the UD link. */
    if(H5Gunlink(fid, "/group") < 0) TEST_ERROR;

    /* Ensure we can open the group through the UD link */
    if((gid = H5Gopen(fid, "ud_link")) < 0) TEST_ERROR;

    /* Unlink the group contained within it. */
    if(H5Gunlink(gid, "new_group") < 0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Now delete the UD link.  This should cause the group to be
     * deleted, too. */
    if(H5Gunlink(fid, "ud_link")<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;

    /* The file should be empty again. */
    if(empty_size!=h5_get_file_size(filename)) TEST_ERROR;

    if(H5Lunregister(UD_HARD_TYPE) < 0) TEST_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid2);
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
} /* end ud_hard_links() */


/*-------------------------------------------------------------------------
 * Function:    ext_link_endian
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
static hid_t UD_rereg_traverse(const char UNUSED * link_name, hid_t cur_group, void UNUSED * udata, size_t UNUSED udata_size, hid_t lapl_id)
{
    hid_t ret_value;

    if((ret_value = H5Oopen(cur_group, REREG_TARGET_NAME, lapl_id)) < 0) TEST_ERROR;

    return ret_value;

error:
    return -1;
}

/* This link class has the same ID number as the UD hard links but
 * has a very different traversal function */
const H5L_link_class_t UD_rereg_class[1] = {{
    H5L_LINK_CLASS_T_VERS,      /* H5L_link_class_t version       */
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
    H5L_linkinfo_t	li;                     /* Link information */
    char        objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t     name_len;                       /* Length of object name */
    char	filename[NAME_BUF_SIZE];
    h5_stat_size_t empty_size;                  /* Size of an empty file */

    TESTING("registering a new class for existing UD links");

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Get the size of the empty file for reference */
    if(H5Fclose(fid) < 0) TEST_ERROR;
    if((empty_size=h5_get_file_size(filename))==0) TEST_ERROR;

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Check that UD hard links are not registered */
    if(H5Lis_registered(UD_HARD_TYPE) != 0) TEST_ERROR

    /* Register "user-defined hard links" with the library */
    if(H5Lregister(UD_hard_class) < 0) TEST_ERROR;

    /* Check that UD hard links are registered */
    if(H5Lis_registered(UD_HARD_TYPE) != TRUE) TEST_ERROR

    /* Point a UD defined hard link to a group in the same way as the previous test */
    if((gid = H5Gcreate(fid, "group", 0)) <0) TEST_ERROR;
    if (H5Lget_linkinfo(fid, "group", &li, H5P_DEFAULT)<0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;

    if(H5Lcreate_ud(fid, "ud_link", UD_HARD_TYPE, &(li.u.address),
                    sizeof(li.u.address), H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* Create a group named REREG_TARGET_NAME in the same group as the ud link */
    if((gid = H5Gcreate(fid, REREG_TARGET_NAME, 0)) < 0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Now unregister UD hard links */
    if(H5Lunregister(UD_HARD_TYPE) < 0) TEST_ERROR;

    /* Check that UD hard links are no longer registered */
    if(H5Lis_registered(UD_HARD_TYPE) != 0) TEST_ERROR

    /* Verify that we can't traverse the ud link anymore */
    H5E_BEGIN_TRY {
        if((gid = H5Gopen(fid, "ud_link")) >= 0) TEST_ERROR;
    } H5E_END_TRY

    /* Verify that we can't create any new links of this type */
    H5E_BEGIN_TRY {
      if(H5Lcreate_ud(fid, "ud_link2", UD_HARD_TYPE, &(li.u.address),
                      sizeof(li.u.address), H5P_DEFAULT, H5P_DEFAULT) >= 0)
          TEST_ERROR;
    } H5E_END_TRY

    /* Register a new kind of link with the same ID number */
    if(H5Lregister(UD_rereg_class) < 0) TEST_ERROR;

    /* Check that UD hard links are registered again */
    if(H5Lis_registered(UD_HARD_TYPE) != TRUE) TEST_ERROR

    /* Open a group through the ud link (now a different class of link).
     * It should be a different group
     * than the UD hard link pointed to */
    if((gid = H5Gopen(fid, "ud_link")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(name_len != 0) TEST_ERROR

    /* Create object in group */
    if((gid2 = H5Gcreate(gid, "new_group", (size_t)0)) < 0) TEST_ERROR

    /* Close groups*/
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Re-open group without using ud link to check that it was created properly */
    if((gid = H5Gopen(fid, "rereg_target/new_group")) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/rereg_target/new_group")) TEST_ERROR

    /* Close opened object */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Unlink the group pointed to by the UD hard link.  It shouldn't be
     * deleted because the UD link incremented its reference count. */
    if(H5Gunlink(fid, "/group") < 0) TEST_ERROR;

    /* What a mess! Re-register user-defined links to clean up the
     * reference counts.  We shouldn't actually need to unregister the
     * other link type */
    if(H5Lregister(UD_hard_class) < 0) TEST_ERROR;
    if(H5Lis_registered(UD_HARD_TYPE) != TRUE) TEST_ERROR

    /* Ensure we can open the group through the UD link (now that UD hard
     * links have been registered) */
    if((gid = H5Gopen(fid, "ud_link")) < 0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Delete the UD hard link.  This should cause the group to be
     * deleted, too. */
    if(H5Gunlink(fid, "ud_link")<0) TEST_ERROR;

    /* Unlink the other two groups so that we can make sure the file is empty */
    if(H5Gunlink(fid, "/rereg_target/new_group")<0) TEST_ERROR;
    if(H5Gunlink(fid, REREG_TARGET_NAME)<0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;

    /* The file should be empty again. */
    if(empty_size!=h5_get_file_size(filename)) TEST_ERROR;

    if(H5Lunregister(UD_HARD_TYPE) < 0) TEST_ERROR
    if(H5Lis_registered(UD_HARD_TYPE) != 0) TEST_ERROR
    
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid2);
    	H5Gclose (gid);
    	H5Fclose (fid);
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
herr_t UD_cb_create(const char * link_name, hid_t loc_group, void * udata, size_t udata_size, hid_t lcpl_id)
{
    if(!link_name) TEST_ERROR;
    if(loc_group < 0) TEST_ERROR;
    if(udata_size > 0 && !udata) TEST_ERROR;
    if(lcpl_id < 0) TEST_ERROR;

    if(strcmp(link_name, UD_CB_LINK_NAME) && strcmp(link_name, NEW_UD_CB_LINK_NAME)) TEST_ERROR;
    if(strcmp(udata, UD_CB_TARGET)) TEST_ERROR;
    if(udata_size != UD_CB_TARGET_LEN) TEST_ERROR;

    return 0;

error:
    return -1;
}
static hid_t UD_cb_traverse(const char * link_name, hid_t cur_group, void * udata, size_t udata_size, hid_t lapl_id)
{
    const char *target = (char *) udata;
    hid_t ret_value;

    if(!link_name) TEST_ERROR;
    if(cur_group < 0) TEST_ERROR;
    if(udata_size > 0 && !udata) TEST_ERROR;

    if(strcmp(link_name, UD_CB_LINK_NAME) && strcmp(link_name, NEW_UD_CB_LINK_NAME)) TEST_ERROR;
    if(strcmp(udata, UD_CB_TARGET)) TEST_ERROR;
    if(udata_size != UD_CB_TARGET_LEN) TEST_ERROR;

    if((ret_value = H5Oopen(cur_group, target, lapl_id)) < 0)
        TEST_ERROR;

    return ret_value;

error:
    return -1;
}
/* Callback for when the link is moved or renamed */
herr_t UD_cb_move(const char * new_name, hid_t new_loc, void * udata, size_t udata_size)
{
    const char *target = (char *) udata;

    if(!new_name) TEST_ERROR;
    if(new_loc < 0) TEST_ERROR;
    if(udata_size > 0 && !udata) TEST_ERROR;

    if(strcmp(new_name, NEW_UD_CB_LINK_NAME)) TEST_ERROR;
    if(strcmp(udata, UD_CB_TARGET)) TEST_ERROR;
    if(udata_size != UD_CB_TARGET_LEN) TEST_ERROR;

    return 0;

error:
    return -1;
}
/* Callback for when the link is deleted.  Also called during move */
herr_t UD_cb_delete(const char * link_name, hid_t loc_group, void * udata, size_t udata_size)
{
    if(!link_name) TEST_ERROR;
    if(loc_group < 0) TEST_ERROR;
    if(udata_size > 0 && !udata) TEST_ERROR;

    if(strcmp(link_name, UD_CB_LINK_NAME) && strcmp(link_name, NEW_UD_CB_LINK_NAME)) TEST_ERROR;
    if(strcmp(udata, UD_CB_TARGET)) TEST_ERROR;
    if(udata_size != UD_CB_TARGET_LEN) TEST_ERROR;

    return 0;

error:
    return -1;
}
/* Callback for when the link is queried */
ssize_t UD_cb_query(const char * link_name, void * udata, size_t udata_size, void* buf, size_t buf_size)
{
    if(!link_name) TEST_ERROR;
    if(udata_size > 0 && !udata) TEST_ERROR;

    if(strcmp(link_name, UD_CB_LINK_NAME)) TEST_ERROR;
    if(strcmp(udata, UD_CB_TARGET)) TEST_ERROR;
    if(udata_size != UD_CB_TARGET_LEN) TEST_ERROR;

    if(buf)
    {
      if(buf_size < 16) TEST_ERROR;
      strcpy(buf, "query succeeded");
    }

    /* There are 15 characters and a NULL in "query succeeded" */
    return 16;

error:
    return -1;
}

const H5L_link_class_t UD_cb_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_link_class_t version       */
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
ud_callbacks(fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group ID */
    hid_t       lcpl = (-1);                   /* Link Creation PL */
    H5G_stat_t sb;                 /* Object information */
    H5L_linkinfo_t li;                /* Link information */
    char        ud_target_name[] = UD_CB_TARGET; /* Link target name */
    char	filename[NAME_BUF_SIZE];
    char        query_buf[NAME_BUF_SIZE];

    TESTING("user-defined link callbacks");

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Check that registered link classes are, and unregistered ones aren't */
    if(H5Lis_registered(H5L_LINK_EXTERNAL) != TRUE) TEST_ERROR
    if(H5Lis_registered(UD_HARD_TYPE) != 0) TEST_ERROR
    if(H5Lis_registered(UD_CB_TYPE) != 0) TEST_ERROR

    /* Hit two birds with one stone: register UD hard links from previous
     * test to check that having two UD links registered at once presents
     * no problems. */
    if(H5Lregister(UD_hard_class) < 0) TEST_ERROR;

    /* Register user-defined link class.  This is the one we'll actually
     * be using. */
    if(H5Lregister(UD_cb_class) < 0) TEST_ERROR;

    /* Check that registered link classes are, and unregistered ones aren't */
    if(H5Lis_registered(H5L_LINK_EXTERNAL) != TRUE) TEST_ERROR
    if(H5Lis_registered(UD_HARD_TYPE) != TRUE) TEST_ERROR
    if(H5Lis_registered(UD_CB_TYPE) != TRUE) TEST_ERROR

    /* Create a group for the UD link to point to */
    if((gid = H5Gcreate(fid, UD_CB_TARGET, 0)) <0) TEST_ERROR;

    /* Create a user-defined link to the group.  These UD links behave like soft links. */
    if(H5Lcreate_ud(fid, UD_CB_LINK_NAME, UD_CB_TYPE, ud_target_name, UD_CB_TARGET_LEN, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Try opening group through UD link */
    if((gid = H5Gopen(fid, UD_CB_LINK_NAME)) < 0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Query the link to test its query callback */
    if (H5Lget_linkinfo(fid, UD_CB_LINK_NAME, &li, H5P_DEFAULT)<0) TEST_ERROR;
    if(li.u.link_size != 16) TEST_ERROR;
    if (UD_CB_TYPE != li.linkclass) {
	H5_FAILED();
	puts("    Unexpected link class - should have been a UD hard link");
	goto error;
    }

    /* Fill the query buffer */
    if(H5Gget_linkval(fid, UD_CB_LINK_NAME, NAME_BUF_SIZE, query_buf) < 0) TEST_ERROR;
    if(strcmp(query_buf, "query succeeded") != 0) TEST_ERROR;

    /* Move the link */
    if(H5Gmove(fid, UD_CB_LINK_NAME, NEW_UD_CB_LINK_NAME) < 0) TEST_ERROR;

    /* Re-open group to ensure that move worked */
    if((gid = H5Gopen(fid, NEW_UD_CB_LINK_NAME)) < 0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Remove UD link */
    if(H5Gunlink(fid, NEW_UD_CB_LINK_NAME) < 0) TEST_ERROR;


    /* Test that the callbacks don't work if the link class is not registered */

    /* Create a new link. Just for fun, give it a non-default character
     * encoding (to test that LAPLs work) */
    if((lcpl = H5Pcreate(H5P_LINK_CREATE)) < 0) TEST_ERROR
#ifdef H5_GROUP_REVISION
    if(H5Pset_char_encoding(lcpl, H5T_CSET_UTF8) < 0) TEST_ERROR
#endif /* H5_GROUP_REVISION */
    if(H5Lcreate_ud(fid, UD_CB_LINK_NAME, UD_CB_TYPE, ud_target_name, UD_CB_TARGET_LEN, lcpl, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Pclose(lcpl)<0) TEST_ERROR

    /* Check its character encoding */
#ifdef H5_GROUP_REVISION
    if(H5Lget_linkinfo(fid, UD_CB_LINK_NAME, &li, H5P_DEFAULT) < 0) TEST_ERROR;
    if(li.cset != H5T_CSET_UTF8) TEST_ERROR;
#endif /* H5_GROUP_REVISION */

    /* Unregister the link class so the library forgets what its callbacks do */
    if(H5Lunregister(UD_CB_TYPE) < 0) TEST_ERROR;

    /* Now test that each of the callbacks fails */
    H5E_BEGIN_TRY {
        if(H5Lcreate_ud(fid, NEW_UD_CB_LINK_NAME, UD_CB_TYPE, ud_target_name, UD_CB_TARGET_LEN, H5P_DEFAULT, H5P_DEFAULT) >= 0) TEST_ERROR;
        if(H5Gmove(fid, UD_CB_LINK_NAME, NEW_UD_CB_LINK_NAME) >= 0) TEST_ERROR;
        if(H5Gunlink(fid, UD_CB_LINK_NAME) >= 0) TEST_ERROR;
        if((gid = H5Gopen(gid, UD_CB_LINK_NAME)) >= 0) TEST_ERROR;
        if(H5Gunlink(fid, UD_CB_LINK_NAME) >= 0) TEST_ERROR;
    } H5E_END_TRY

    /* The query callback should NOT fail, but should be unable to give a linklen */
    if(H5Lget_linkinfo(fid, UD_CB_LINK_NAME, &li, H5P_DEFAULT) <0) TEST_ERROR;
    if(li.u.link_size != 0) TEST_ERROR;
    if(li.linkclass != UD_CB_TYPE) TEST_ERROR;
    if(H5Gget_objinfo(fid, UD_CB_LINK_NAME, FALSE, &sb) <0) TEST_ERROR;
    if(sb.type != H5G_UDLINK) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;

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
UD_plist_traverse(const char UNUSED * link_name, hid_t cur_group, void UNUSED * udata, size_t udata_size, hid_t lapl_id)
{
    char target[NAME_BUF_SIZE];
    hid_t ret_value;

    if(udata_size != 0) TEST_ERROR;

    /* Get the name of the target from the property list. */
    if(H5Pget(lapl_id, DEST_PROP_NAME, target) < 0) TEST_ERROR;

    if((ret_value = H5Oopen(cur_group, target, lapl_id)) < 0)
        TEST_ERROR;

    return ret_value;

error:
    return -1;
}
const H5L_link_class_t UD_plist_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_link_class_t version       */
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
lapl_udata(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1), gid2 = (-1);	/* Group IDs */
    hid_t       plist_id = (-1);                /* Property List ID */
    char	group_a_name[NAME_BUF_SIZE];
    char	group_b_name[NAME_BUF_SIZE];
    char	filename[NAME_BUF_SIZE];

    TESTING("user data passed through lapl");

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Register UD link types from previous tests to check that having
     * multiple types registered at once presents no problems. */
    if(H5Lregister(UD_cb_class) < 0) TEST_ERROR;

    /* Register the link class.  We'll actually be using for this test. */
    if(H5Lregister(UD_plist_class) < 0) TEST_ERROR;

    /* Another link class from a previous test */
    if(H5Lregister(UD_hard_class) < 0) TEST_ERROR;

    /* Unregister the first link type registered to make sure this doesn't
     * break anything. */
    if(H5Lunregister(UD_CB_TYPE) < 0) TEST_ERROR;

    /* Create two groups for the UD link to point to */
    if((gid = H5Gcreate(fid, "group_a", 0)) <0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;
    if((gid = H5Gcreate(fid, "group_b", 0)) <0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Create a user-defined link to the group.  These UD links have no udata. */
    if(H5Lcreate_ud(fid, "ud_link", UD_PLIST_TYPE, NULL, 0, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Create a non-default lapl with a new property pointing to group a*/
    if((plist_id = H5Pcreate(H5P_LINK_ACCESS)) < 0) TEST_ERROR;
    strcpy(group_a_name, "group_a");
    if(H5Pinsert(plist_id, DEST_PROP_NAME, NAME_BUF_SIZE, group_a_name, NULL, NULL, NULL, NULL, NULL, NULL) < 0) TEST_ERROR;

    /* Try opening group through UD link */
    if((gid = H5Oopen(fid, "ud_link", plist_id)) < 0) TEST_ERROR;
    if((gid2 = H5Gcreate(gid, "subgroup_a", 0)) < 0) TEST_ERROR;
    if(H5Gclose(gid2) < 0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Verify that we can open the new group without using the ud link */
    if((gid2 = H5Gopen(fid, "/group_a/subgroup_a")) < 0) TEST_ERROR;
    if(H5Gclose(gid2) < 0) TEST_ERROR;

    /* Now use the same ud link to access group_b */
    strcpy(group_b_name, "group_b");
    if(H5Pset(plist_id, DEST_PROP_NAME, group_b_name)<0) TEST_ERROR;

    /* Create a subgroup */
    if((gid = H5Oopen(fid, "ud_link", plist_id)) < 0) TEST_ERROR;
    if((gid2 = H5Gcreate(gid, "subgroup_b", 0)) < 0) TEST_ERROR;
    if(H5Gclose(gid2) < 0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Verify that we can open the new group without using the ud link */
    if((gid2 = H5Gopen(fid, "/group_b/subgroup_b")) < 0) TEST_ERROR;
    if(H5Gclose(gid2) < 0) TEST_ERROR;

    /* Close property list */
    if(H5Pclose(plist_id) < 0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;

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
herr_t UD_cbsucc_create(const char * link_name, hid_t loc_group, void * udata, size_t udata_size, hid_t lcpl_id)
{
    /* Check to make sure that this "soft link" has a target */
    if(udata_size < 1 || !udata)
       return -1;

    return 0;
}
static hid_t UD_cbsucc_traverse(const char * link_name, hid_t cur_group, void * udata, size_t udata_size, hid_t lapl_id)
{
    const char *target = (char *) udata;
    hid_t ret_value;

    if(!target) goto error;

    if((ret_value = H5Oopen(cur_group, target, lapl_id)) < 0) goto error;

    return ret_value;

error:
    return -1;
}
/* Failure callback for when the link is moved or renamed */
herr_t UD_cbfail_move(const char * new_name, hid_t new_loc, void * udata, size_t udata_size)
{
    /* This traversal function will always fail. */
    return -1;
}
/* SuccessCallback for when the link is moved or renamed */
herr_t UD_cbsucc_move(const char * new_name, hid_t new_loc, void * udata, size_t udata_size)
{
    /* This traversal function will always succeed. */
    return 0;
}
/* Callback for when the link is deleted.  Also called during move */
herr_t UD_cbsucc_delete(const char * link_name, hid_t loc_group, void * udata, size_t udata_size)
{
    /* This callback will always succeed */
    return 0;
}
/* Callback for when the link is deleted.  Also called during move */
herr_t UD_cbfail_delete(const char * link_name, hid_t loc_group, void * udata, size_t udata_size)
{
    /* This traversal function will always fail. */
    /* Note: un-deletable links are in general a very bad idea! */
    return -1;
}
/* Callback for when the link is queried */
ssize_t UD_cbfail_query(const char * link_name, void * udata, size_t udata_size, void *buf, size_t buf_size)
{
    /* This traversal function will always fail. */
    return -1;
}
/* Callback for when the link is queried */
ssize_t UD_cbfail_on_write_query(const char * link_name, void * udata, size_t udata_size, void *buf, size_t buf_size)
{
    /* This traversal function will return a buffer size,
     * but will fail when a buffer is passed in ("writing to the buffer"
     * fails
     */

    if(buf != NULL)
        return -1;

    return 0;
}
/* Callback for when the link is queried */
ssize_t UD_cbsucc_query(const char * link_name, void * udata, size_t udata_size, void *buf, size_t buf_size)
{
    /* This traversal function will return a buffer size,
     * but will fail when a buffer is passed in ("writing to the buffer"
     * fails
     */

    if(buf != NULL && buf_size >= 8)
        strcpy(buf, "succeed");

    return 8;
}

/* This class is full of failing callbacks */
const H5L_link_class_t UD_cbfail_class1[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_link_class_t version       */
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
const H5L_link_class_t UD_cbfail_class2[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_link_class_t version       */
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
const H5L_link_class_t UD_cbfail_class3[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_link_class_t version       */
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
const H5L_link_class_t UD_error1_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_link_class_t version       */
    UD_ERROR_TYPE,            /* Link type id number            */
    "UD_error_link",          /* Link class name for debugging  */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    NULL,                     /* This class has no traversal function */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};
const H5L_link_class_t UD_error2_class[1] = {{
    UD_BAD_VERS,              /* Invalid H5L_link_class_t version */
    UD_ERROR_TYPE,            /* Link type id number            */
    "UD_error_link",          /* Link class name for debugging  */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    UD_cbsucc_traverse,       /* Traversal function             */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};
const H5L_link_class_t UD_error3_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_link_class_t version */
    UD_BAD_TYPE1,             /* Invalid Link type id number            */
    "UD_error_link",          /* Link class name for debugging  */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    UD_cbsucc_traverse,       /* Traversal function             */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};
const H5L_link_class_t UD_error4_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_link_class_t version */
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
ud_link_errors(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);                     /* Group IDs */
    char	group_name[NAME_BUF_SIZE];
    char	filename[NAME_BUF_SIZE];
    char        query_buf[NAME_BUF_SIZE];
    H5L_linkinfo_t li;                         /* Link information */

    TESTING("user-defined link error conditions");

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Try to register some invalid link classes */
    H5E_BEGIN_TRY {
      if(H5Lregister(UD_error1_class) >= 0) TEST_ERROR;
      if(H5Lregister(UD_error2_class) >= 0) TEST_ERROR;
      if(H5Lregister(UD_error3_class) >= 0) TEST_ERROR;
      if(H5Lregister(UD_error4_class) >= 0) TEST_ERROR;
    } H5E_END_TRY

    /* Register the UD plist class. */
    if(H5Lregister(UD_plist_class) < 0) TEST_ERROR;
    /* Now register the first class we'll be using.
     * It has the same ID as the plist class, and should replace it. */
    if(H5Lregister(UD_cbfail_class1) < 0) TEST_ERROR;

    /* Create a group for the UD link to point to */
    if((gid = H5Gcreate(fid, "group", 0)) <0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Create a user-defined link to the group. */
    strcpy(group_name, "/group");
    if(H5Lcreate_ud(fid, "/ud_link", UD_CBFAIL_TYPE, &group_name, strlen(group_name) + 1, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Open the group through the ud link */
    if((gid = H5Gopen(fid, "ud_link")) < 0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Now test that each of the callbacks will cause a failure if it returns -1 */
    H5E_BEGIN_TRY {
        /* The create callback will fail if we pass in no udata */
        if(H5Lcreate_ud(fid, "fail", UD_CBFAIL_TYPE, NULL, 0, H5P_DEFAULT, H5P_DEFAULT) >= 0) TEST_ERROR;
        /* The move and copy callbacks will fail */
        if(H5Gmove(fid, "ud_link", "move_fail") >= 0) TEST_ERROR;
        if(H5Lcopy(fid, "ud_link", fid, "copy_fail", H5P_DEFAULT, H5P_DEFAULT) >= 0) TEST_ERROR;
        /* The traversal callback will fail if we remove its target */
        if(H5Gunlink(fid, "group") < 0) TEST_ERROR;
        if((gid = H5Gopen(gid, "ud_link")) >= 0) TEST_ERROR;
        /* The deletion callback will always fail */
        if(H5Gunlink(fid, "ud_link") >= 0) TEST_ERROR;
        /* The query callback will fail */
        if(H5Lget_linkinfo(fid, "ud_link", &li, H5P_DEFAULT) >=0) TEST_ERROR;
    } H5E_END_TRY

    /* Now use a class with different callback functions */
    if(H5Lregister(UD_cbfail_class2) < 0) TEST_ERROR;

    /* Moving should still fail, but copying will succeed */
    H5E_BEGIN_TRY {
        if(H5Gmove(fid, "ud_link", "move_fail") >= 0) TEST_ERROR
    } H5E_END_TRY
    if(H5Lcopy(fid, "ud_link", fid, "copy_succ", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* The query callback will succeed when we only want to get the size of the buffer... */
    if(H5Lget_linkinfo(fid, "ud_link", &li, H5P_DEFAULT) <0) TEST_ERROR;
    if(li.u.link_size != 0) TEST_ERROR;
    /* ...but fail when we try to write data to the buffer itself*/
    H5E_BEGIN_TRY {
        if(H5Lget_linkval(fid, "ud_link", NAME_BUF_SIZE, query_buf, H5P_DEFAULT) >=0) TEST_ERROR;
    } H5E_END_TRY

    /* Register a new class */
    if(H5Lregister(UD_cbfail_class3) < 0) TEST_ERROR;

    /* Now querying should succeed */
    if(H5Lget_linkinfo(fid, "ud_link", &li, H5P_DEFAULT) <0) TEST_ERROR;
    if(li.u.link_size != 8) TEST_ERROR;
    if(H5Lget_linkval(fid, "ud_link", NAME_BUF_SIZE, query_buf, H5P_DEFAULT) <0) TEST_ERROR;
    if(HDstrcmp(query_buf, "succeed") != 0) TEST_ERROR;

    /* Moving and copying should both succeed */
    if(H5Gmove(fid, "copy_succ", "move_succ") < 0) TEST_ERROR
    if(H5Lcopy(fid, "ud_link", fid, "copy_succ2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR

    /* Delete link (this callback should work now) */
    if(H5Gunlink(fid, "ud_link") <0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
    	H5Gclose (gid);
    	H5Fclose (fid);
    } H5E_END_TRY;
    return -1;
}



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
lapl_nlinks(hid_t fapl)
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

    TESTING("adjusting nlinks with LAPL");

    /* Make certain test is valid */
    /* XXX: should probably make a "generic" test that creates the proper
     *          # of links based on this value - QAK
     */
    HDassert(H5G_NLINKS == 16);

    /* Create file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create group with short name in file (used as target for links) */
    if((gid=H5Gcreate (fid, "final", (size_t)0))<0) TEST_ERROR;

    /* Create chain of soft links to existing object (limited) */
    if(H5Glink2(fid, "final", H5G_LINK_SOFT, fid, "soft1") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft1", H5G_LINK_SOFT, fid, "soft2") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft2", H5G_LINK_SOFT, fid, "soft3") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft3", H5G_LINK_SOFT, fid, "soft4") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft4", H5G_LINK_SOFT, fid, "soft5") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft5", H5G_LINK_SOFT, fid, "soft6") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft6", H5G_LINK_SOFT, fid, "soft7") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft7", H5G_LINK_SOFT, fid, "soft8") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft8", H5G_LINK_SOFT, fid, "soft9") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft9", H5G_LINK_SOFT, fid, "soft10") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft10", H5G_LINK_SOFT, fid, "soft11") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft11", H5G_LINK_SOFT, fid, "soft12") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft12", H5G_LINK_SOFT, fid, "soft13") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft13", H5G_LINK_SOFT, fid, "soft14") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft14", H5G_LINK_SOFT, fid, "soft15") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft15", H5G_LINK_SOFT, fid, "soft16") < 0) TEST_ERROR;
    if(H5Glink2(fid, "soft16", H5G_LINK_SOFT, fid, "soft17") < 0) TEST_ERROR;

    /* Close objects */
    if(H5Gclose(gid)<0) TEST_ERROR;
    if(H5Fclose(fid)<0) TEST_ERROR;

    /* Open file */
    if((fid=H5Fopen(filename, H5F_ACC_RDWR, fapl))<0) TEST_ERROR;

    /* Create LAPL with higher-than-usual nlinks value */
    /* Create a non-default lapl with udata set to point to the first group */
    if((plist = H5Pcreate(H5P_LINK_ACCESS)) < 0) TEST_ERROR;
    nlinks = 20;
    if(H5Pset_nlinks(plist, nlinks)<0) TEST_ERROR;

    /* Ensure that nlinks was set successfully */
    nlinks = 0;
    if(H5Pget_nlinks(plist, &nlinks)<0) TEST_ERROR
    if(nlinks != 20) TEST_ERROR

    /* Open object through what is normally too many soft links using
     * new property list */
    if((gid = H5Oopen(fid, "soft17", plist)) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/soft17")) TEST_ERROR

    /* Create group using soft link */
    if((gid2 = H5Gcreate(gid, "new_soft", (size_t)0)) < 0) TEST_ERROR

    /* Close groups */
    if(H5Gclose(gid2) < 0) TEST_ERROR
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Set nlinks to a smaller number */
    nlinks = 4;
    if(H5Pset_nlinks(plist, nlinks)<0) TEST_ERROR;

    /* Ensure that nlinks was set successfully */
    nlinks = 0;
    if(H5Pget_nlinks(plist, &nlinks)<0) TEST_ERROR;
    if(nlinks != 4) TEST_ERROR;

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
    if((gid = H5Oopen(fid, "soft4", plist)) < 0) TEST_ERROR;

    /* Check name */
    if((name_len = H5Iget_name( gid, objname, (size_t)NAME_BUF_SIZE )) < 0) TEST_ERROR
    if(HDstrcmp(objname, "/soft4")) TEST_ERROR


    /* Test other functions that should use a LAPL */
    nlinks = 20;
    if(H5Pset_nlinks(plist, nlinks)<0) TEST_ERROR;

    /* Try copying and moving when both src and dst cotain many soft links
     * using a non-default LAPL
     */
    if(H5Lcopy(fid, "soft17", fid, "soft17/newer_soft", H5P_DEFAULT, plist) < 0) TEST_ERROR
    if(H5Lmove(fid, "soft17/newer_soft", fid, "soft17/newest_soft", H5P_DEFAULT, plist) < 0) TEST_ERROR

    /* H5Llink */
    if(H5Llink(fid, "soft17/link_to_group", gid, H5P_DEFAULT, plist) < 0) TEST_ERROR

    /* H5Lcreate_hard  and H5Lcreate_soft */
    if(H5Lcreate_hard(fid, "soft17", fid, "soft17/link2_to_group", H5P_DEFAULT, plist) < 0) TEST_ERROR
    if(H5Lcreate_soft("/soft4", fid, "soft17/soft_link", H5P_DEFAULT, plist) < 0) TEST_ERROR

    /* H5Lunlink */
    if(H5Lunlink(fid, "soft17/soft_link", plist) < 0) TEST_ERROR

    /* H5Lget_linkval and H5Lget_linkinfo */
    if(H5Lget_linkval(fid, "soft17", 0, NULL, plist) < 0) TEST_ERROR
    if(H5Lget_linkinfo(fid, "soft17", NULL, plist) < 0) TEST_ERROR

    /* H5Lcreate_external and H5Lcreate_ud */
    if(H5Lcreate_external("filename", "path", fid, "soft17/extlink", H5P_DEFAULT, plist) <0) TEST_ERROR
    if(H5Lregister(UD_rereg_class) < 0) TEST_ERROR
    if(H5Lcreate_ud(fid, "soft17/udlink", UD_HARD_TYPE, NULL, 0, H5P_DEFAULT, plist) < 0) TEST_ERROR

    /* Close plist */
    if(H5Pclose(plist) < 0) TEST_ERROR;


    /* Create a datatype and dataset as targets inside the group */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR
    if(H5Tcommit(gid, "datatype", tid) < 0) TEST_ERROR
    if(H5Tclose(tid) < 0) TEST_ERROR

    dims[0] = 2;
    dims[1] = 2;
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) TEST_ERROR
    if((did = H5Dcreate(gid, "dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT)) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR;

    /* Close group */
    if(H5Gclose(gid) < 0) TEST_ERROR;

    /* Try to open the objects using too many symlinks with default *APLs */
    H5E_BEGIN_TRY {
        if((gid = H5Gopen_expand(fid, "soft17", H5P_DEFAULT)) >=0) {
          H5_FAILED();
          puts("    Should have failed for too many nested links.");
          TEST_ERROR;
        }
        if((tid = H5Topen_expand(fid, "soft17/datatype", H5P_DEFAULT)) >=0) {
          H5_FAILED();
          puts("    Should have failed for too many nested links.");
          TEST_ERROR;
        }
        if((did = H5Dopen_expand(fid, "soft17/dataset", H5P_DEFAULT)) >=0) {
          H5_FAILED();
          puts("    Should have failed for too many nested links.");
          TEST_ERROR;
        }
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
    if((gid = H5Gopen_expand(fid, "soft17", gapl)) <0) TEST_ERROR
    if((tid = H5Topen_expand(fid, "soft17/datatype", tapl)) <0) TEST_ERROR
    if((did = H5Dopen_expand(fid, "soft17/dataset", dapl)) <0) TEST_ERROR

    /* Close objects */
    if(H5Gclose(gid) < 0) TEST_ERROR
    if(H5Tclose(tid) < 0) TEST_ERROR
    if(H5Dclose(did) < 0) TEST_ERROR

    /* Close plists */
    if(H5Pclose(gapl) < 0) TEST_ERROR;
    if(H5Pclose(tapl) < 0) TEST_ERROR;
    if(H5Pclose(dapl) < 0) TEST_ERROR;

    /* Close file */
    if(H5Fclose(fid)<0) TEST_ERROR;

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
 * Function:    objinfo_linkclass
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
linkinfo(hid_t fapl)
{
    hid_t	fid = (-1);     		/* File ID */
    hid_t	gid = (-1);	                /* Group ID */
    hid_t       tid = (-1);                     /* Type ID */
    hid_t       sid = (-1), did = -(1);         /* Dataspace and dataset IDs */
    H5L_linkinfo_t li;                          /* Link information */
    char	filename[NAME_BUF_SIZE];

    TESTING("linkclass field in H5Gget_objinfo");

    /* Set up filename and create file*/
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Register a couple of user-defined link classes with the library */
    if(H5Lregister(UD_plist_class) < 0) TEST_ERROR;

    /* Create an object of each type */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR;
    if(H5Tcommit(fid, "datatype", tid) < 0) TEST_ERROR;
    if((gid = H5Gcreate(fid, "group", 0)) < 0) TEST_ERROR;
    if(H5Glink(fid, H5G_LINK_SOFT, "group", "softlink") < 0) TEST_ERROR;

    if((sid = H5Screate(H5S_SCALAR)) < 0) TEST_ERROR;
    if((did = H5Dcreate(fid, "dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    if(H5Lcreate_ud(fid, "ud_link", UD_PLIST_TYPE, NULL, 0, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_external("file_name", "obj_path", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Close all objects */
    if(H5Tclose(tid) < 0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;
    if(H5Dclose(did) < 0) TEST_ERROR;

    /* Make sure that linkclass is correct when objects are queried */
    if(H5Lget_linkinfo(fid, "datatype", &li, H5P_DEFAULT) < 0) TEST_ERROR;
    if(li.linkclass != H5L_LINK_HARD) TEST_ERROR;
    if(H5Lget_linkinfo(fid, "group", &li, H5P_DEFAULT) < 0) TEST_ERROR;
    if(li.linkclass != H5L_LINK_HARD) TEST_ERROR;
    if(H5Lget_linkinfo(fid, "dataset", &li, H5P_DEFAULT) < 0) TEST_ERROR;
    if(li.linkclass != H5L_LINK_HARD) TEST_ERROR;

    if(H5Lget_linkinfo(fid, "ext_link", &li, H5P_DEFAULT) < 0) TEST_ERROR;
    if(li.linkclass != H5L_LINK_EXTERNAL) TEST_ERROR;
    if(H5Lget_linkinfo(fid, "softlink", &li, H5P_DEFAULT) < 0) TEST_ERROR;
    if(li.linkclass != H5G_LINK_SOFT) TEST_ERROR;
    if(H5Lget_linkinfo(fid, "ud_link", &li, H5P_DEFAULT) < 0) TEST_ERROR;
    if(li.linkclass != UD_PLIST_TYPE) TEST_ERROR;

    /* Ensure that passing a NULL pointer doesn't cause an error */
    if(H5Lget_linkinfo(fid, "group", NULL, H5P_DEFAULT) < 0) TEST_ERROR;

    if(H5Fclose(fid) < 0) TEST_ERROR;

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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int		nerrors = 0;
    hid_t	fapl;
    const char  *envval = NULL;

    envval = HDgetenv("HDF5_DRIVER");
    if (envval == NULL) 
        envval = "nomatch";
    if (HDstrcmp(envval, "core") && HDstrcmp(envval, "split")) {
	h5_reset();
	fapl = h5_fileaccess();
	/* The tests... */
	nerrors += mklinks(fapl) < 0 ? 1 : 0;
	nerrors += cklinks(fapl) < 0 ? 1 : 0;
	nerrors += new_links(fapl) < 0 ? 1 : 0;
	nerrors += ck_new_links(fapl) < 0 ? 1 : 0;
	nerrors += long_links(fapl) < 0 ? 1 : 0;
	nerrors += toomany(fapl) < 0 ? 1 : 0;

	/* Test new H5L link creation routine */
#ifdef H5_GROUP_REVISION
	nerrors += test_h5l_create(fapl);
	nerrors += test_lcpl(fapl);
#endif
	nerrors += test_move(fapl);
	nerrors += test_copy(fapl);
#ifdef H5_GROUP_REVISION
	nerrors += test_move_preserves(fapl);
#endif
	nerrors += test_compat(fapl);

        nerrors += external_link_root(fapl) < 0 ? 1 : 0;
        nerrors += external_link_path(fapl) < 0 ? 1 : 0;
        nerrors += external_link_mult(fapl) < 0 ? 1 : 0;
#ifdef H5_GROUP_REVISION
        nerrors += external_link_self(fapl) < 0 ? 1 : 0;
#endif
        nerrors += external_link_pingpong(fapl) < 0 ? 1 : 0;
        nerrors += external_link_toomany(fapl) < 0 ? 1 : 0;
        nerrors += external_link_dangling(fapl) < 0 ? 1 : 0;
        nerrors += external_link_recursive(fapl) < 0 ? 1 : 0;
        nerrors += external_link_query(fapl) < 0 ? 1 : 0;
        nerrors += external_link_unlink_compact(fapl) < 0 ? 1 : 0;
#ifdef H5_GROUP_REVISION
        nerrors += external_link_unlink_dense(fapl) < 0 ? 1 : 0;
#endif /* H5_GROUP_REVISION */
        nerrors += external_link_move(fapl) < 0 ? 1 : 0;
#ifdef H5_GROUP_REVISION
        nerrors += external_link_ride(fapl) < 0 ? 1 : 0;
#endif /* H5_GROUP_REVISION */
        nerrors += ext_link_endian(fapl) < 0 ? 1 : 0;

        /* These tests assume that external links are a form of UD links,
         * so assume that everything that passed for external links
         * above has already been tested for UD links.
         */
        nerrors += ud_hard_links(fapl) < 0 ? 1 : 0;
        nerrors += ud_link_reregister(fapl) < 0 ? 1 : 0;
        nerrors += ud_callbacks(fapl) < 0 ? 1 : 0;
        nerrors += ud_link_errors(fapl) < 0 ? 1 : 0;
        nerrors += lapl_udata(fapl) < 0 ? 1 : 0;
        nerrors += lapl_nlinks(fapl) < 0 ? 1 : 0;
        nerrors += linkinfo(fapl) < 0 ? 1 : 0;

	/* Results */
	if (nerrors) {
	    printf("***** %d LINK TEST%s FAILED! *****\n",
		    nerrors, 1 == nerrors ? "" : "S");
	    exit(1);
	}
	printf("All link tests passed.\n");
	h5_cleanup(FILENAME, fapl);
    }
    else
    {
        puts("All link tests skipped - Incompatible with current Virtual File Driver");
    }
    return 0;

}


