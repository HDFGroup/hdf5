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
 * Purpose:	Tests hard and soft (symbolic) links.
 */

#define H5G_PACKAGE		/*suppress error about including H5Gpkg	  */

/* Define this macro to indicate that the testing APIs should be available */
#define H5G_TESTING

#include "h5test.h"
#include "H5Gpkg.h"		/* Groups				*/

const char *FILENAME[] = {
    "links0",
    "links1",
    "links2",
    "links3",
    "links4",
    "links5",
    "links6",
    "links7",
    "links8",
    "links9",
    "links10",
    "links11",
    "links12",
    "links13",
    "links14",
    "links15",
    NULL
};

#define LINK_BUF_SIZE   1024
#define NAME_BUF_SIZE   1024
#define MAX_NAME_LEN    ((64*1024)+1024)

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
    if (H5Lcreate_hard (file, "d1", H5L_SAME_LOC, "grp1/hard", H5P_DEFAULT)<0) TEST_ERROR;

    /* Create a symbolic link */
    if (H5Lcreate_soft ("/d1", file, "grp1/soft", H5P_DEFAULT)<0) TEST_ERROR;

    /* Create a symbolic link to something that doesn't exist */
    if (H5Lcreate_soft ("foobar", file, "grp1/dangle", H5P_DEFAULT)<0) TEST_ERROR;

    /* Create a recursive symbolic link */
    if (H5Lcreate_soft ("/grp1/recursive", file, "/grp1/recursive", H5P_DEFAULT)<0) TEST_ERROR;

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
        if(H5Lcreate_hard(H5L_SAME_LOC, "dataset1", H5L_SAME_LOC, "hard", H5P_DEFAULT)!=FAIL) TEST_ERROR;
    } H5E_END_TRY;
    H5E_BEGIN_TRY {
        if(H5Lcreate_soft("dataset1", H5L_SAME_LOC, "soft", H5P_DEFAULT)!=FAIL) TEST_ERROR;
    } H5E_END_TRY;

    /* Create links across files with hard link.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lcreate_hard(file_a, "dataset1", file_b, "hard", H5P_DEFAULT)!=FAIL) TEST_ERROR;
    } H5E_END_TRY;

    /* Create hard link to test H5L_SAME_LOC */
    if(H5Lcreate_hard(grp1_a, "dataset2", H5L_SAME_LOC, "hard1", H5P_DEFAULT)<0) TEST_ERROR;

    /* Create links to test hard links across different locations */
    if(H5Lcreate_hard(grp1_a, "dataset2", grp2_a, "hard2", H5P_DEFAULT)<0) TEST_ERROR;

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
    if (H5Gget_linkval(file, "grp1/soft", sizeof linkval, linkval)<0) TEST_ERROR;
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
    h5_fixname(FILENAME[13], fapl, filename, sizeof filename);
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create group with short name in file (used as target for hard links) */
    if((gid=H5Gcreate (fid, "grp1", (size_t)0))<0) TEST_ERROR;

    /* Construct very long file name */
    if((objname = HDmalloc((size_t)(MAX_NAME_LEN + 1))) == NULL) TEST_ERROR;
    for(u = 0; u < MAX_NAME_LEN; u++)
        objname[u] = 'a';
    objname[MAX_NAME_LEN] = '\0';

    /* Create hard link to existing object */
    if(H5Lcreate_hard(fid, "grp1", fid, objname, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Create soft link to existing object */
    objname[0] = 'b';
    if(H5Lcreate_soft("grp1", fid, objname, H5P_DEFAULT) < 0) TEST_ERROR;

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
    hid_t		gid = (-1);     /* Group ID */
    hid_t		gid2 = (-1);    /* Datatype ID */
    char                objname[NAME_BUF_SIZE];         /* Object name */
    ssize_t             name_len;       /* Length of object name */
    char		filename[NAME_BUF_SIZE];

    TESTING("too many links");

    /* Make certain test is valid */
    /* XXX: should probably make a "generic" test that creates the proper
     *          # of links based on this value - QAK
     */
    HDassert(H5G_NLINKS == 16);

    /* Create files */
    h5_fixname(FILENAME[14], fapl, filename, sizeof filename);
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create group with short name in file (used as target for hard links) */
    if((gid=H5Gcreate (fid, "final", (size_t)0))<0) TEST_ERROR;

    /* Create chain of hard links to existing object (no limit on #) */
    if(H5Lcreate_hard(fid, "final", fid, "hard1", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard1", fid, "hard2", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard2", fid, "hard3", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard3", fid, "hard4", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard4", fid, "hard5", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard5", fid, "hard6", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard6", fid, "hard7", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard7", fid, "hard8", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard8", fid, "hard9", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard9", fid, "hard10", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard10", fid, "hard11", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard11", fid, "hard12", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard12", fid, "hard13", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard13", fid, "hard14", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard14", fid, "hard15", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard15", fid, "hard16", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard16", fid, "hard17", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard17", fid, "hard18", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard18", fid, "hard19", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard19", fid, "hard20", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_hard(fid, "hard20", fid, "hard21", H5P_DEFAULT) < 0) TEST_ERROR;

    /* Create chain of soft links to existing object (limited) */
    if(H5Lcreate_soft("final", fid, "soft1", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft1", fid, "soft2", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft2", fid, "soft3", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft3", fid, "soft4", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft4", fid, "soft5", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft5", fid, "soft6", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft6", fid, "soft7", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft7", fid, "soft8", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft8", fid, "soft9", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft9", fid, "soft10", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft10", fid, "soft11", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft11", fid, "soft12", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft12", fid, "soft13", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft13", fid, "soft14", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft14", fid, "soft15", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft15", fid, "soft16", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Lcreate_soft("soft16", fid, "soft17", H5P_DEFAULT) < 0) TEST_ERROR;

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

    /* Create object in external file */
    if((gid2 = H5Gcreate(gid, "new_soft", (size_t)0)) < 0) TEST_ERROR

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
    hid_t fapl_id=-1;
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
    TESTING("H5Lcreate");

    /* Create file */
    fapl_id = h5_fileaccess();
    h5_fixname(FILENAME[3], fapl_id, filename, sizeof filename);

    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id))<0) TEST_ERROR;

    /* Create and commit a datatype with no name */
    if((type_id =H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR;
    if(H5Tcommit_expand(file_id, type_id, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(! H5Tcommitted(type_id)) TEST_ERROR;

    /* Create the dataspace */
    dims[0] = H5L_DIM1;
    dims[1] = H5L_DIM2;
    if((space_id=H5Screate_simple(2 ,dims, NULL))<0) TEST_ERROR;
    /* Create a dataset with no name using the committed datatype*/
    if ((dset_id = H5Dcreate_expand(file_id, type_id, space_id, H5P_DEFAULT)) <0) TEST_ERROR;

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
    if(H5Llink(group_id, "datatype", type_id, H5P_DEFAULT)<0) TEST_ERROR;

    /* Create LCPL with intermediate group creation flag set */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) <0) TEST_ERROR;
    if(H5Pset_create_intermediate_group(lcpl_id, TRUE) <0) TEST_ERROR;

    /* Link nameless dataset into nameless group with intermediate group */
    if(H5Llink(group_id, "inter_group/dataset", dset_id, lcpl_id)<0) TEST_ERROR;

    /* Close IDs for dataset and datatype */
    if(H5Dclose(dset_id)<0) TEST_ERROR;
    if(H5Tclose(type_id)<0) TEST_ERROR;

    /* Re-open datatype using new link */
    if((type_id = H5Topen(group_id, "datatype"))<0) TEST_ERROR;

    /* Link nameless group to root group and close the group ID*/
    if(H5Llink(file_id, "/group", group_id, H5P_DEFAULT)<0) TEST_ERROR;
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
    h5_fixname(FILENAME[12], fapl_id, filename, sizeof filename);

    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id))<0) TEST_ERROR;

    /* Create and link a group with the default LCPL */
    if((group_id = H5Gcreate_expand(file_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
    if(H5Llink(file_id, "/group", group_id, H5P_DEFAULT)<0) TEST_ERROR;
    if(H5Gclose(group_id)<0) TEST_ERROR;

    /* Check that its character encoding is the default */
    if(H5Lget_linkinfo(file_id, "group", &linfo) < 0) TEST_ERROR;
    if(linfo.cset != H5F_CRT_DEFAULT_CSET) TEST_ERROR;

    /* Create and commit a datatype with the default LCPL */
    if((type_id =H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR;
    if(H5Tcommit_expand(file_id, type_id, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Llink(file_id, "/type", type_id, H5P_DEFAULT)<0) TEST_ERROR;
    if(H5Tclose(type_id)<0) TEST_ERROR;    

    /* Check that its character encoding is the default */
    if(H5Lget_linkinfo(file_id, "type", &linfo) < 0) TEST_ERROR;
    if(linfo.cset != H5F_CRT_DEFAULT_CSET) TEST_ERROR;

    /* Create a dataspace */
    dims[0] = H5L_DIM1;
    dims[1] = H5L_DIM2;
    if((space_id=H5Screate_simple(2 ,dims, NULL))<0) TEST_ERROR;

    /* Create a dataset using the default LCPL */
    if ((dset_id = H5Dcreate_expand(file_id, H5T_NATIVE_INT, space_id, H5P_DEFAULT)) <0) TEST_ERROR;
    if(H5Llink(file_id, "/dataset", dset_id, H5P_DEFAULT)<0) TEST_ERROR;
    if(H5Dclose(dset_id)<0) TEST_ERROR;

    /* Check that its character encoding is the default */
    if(H5Lget_linkinfo(file_id, "dataset", &linfo) < 0) TEST_ERROR;
    if(linfo.cset != H5F_CRT_DEFAULT_CSET) TEST_ERROR;

    /* Create a link creation property list with the UTF-8 character encoding */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) <0) TEST_ERROR;
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_UTF8) < 0) TEST_ERROR;

    /* Create and link a group with the new LCPL */
    if((group_id = H5Gcreate_expand(file_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
    if(H5Llink(file_id, "/group2", group_id, lcpl_id)<0) TEST_ERROR;
    if(H5Gclose(group_id)<0) TEST_ERROR;

    /* Check that its character encoding is UTF-8 */
    if(H5Lget_linkinfo(file_id, "group2", &linfo) < 0) TEST_ERROR;
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;

    /* Create and commit a datatype with the new LCPL */
    if((type_id =H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR;
    if(H5Tcommit_expand(file_id, type_id, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Llink(file_id, "/type2", type_id, lcpl_id)<0) TEST_ERROR;    
    if(H5Tclose(type_id)<0) TEST_ERROR;    

    /* Check that its character encoding is UTF-8 */
    if(H5Lget_linkinfo(file_id, "type2", &linfo) < 0) TEST_ERROR;
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;

    /* Create a dataset using the new LCPL */
    if ((dset_id = H5Dcreate_expand(file_id, H5T_NATIVE_INT, space_id, H5P_DEFAULT)) <0) TEST_ERROR;
    if(H5Llink(file_id, "/dataset2", dset_id, lcpl_id)<0) TEST_ERROR;
    if(H5Dclose(dset_id)<0) TEST_ERROR;

    /* Check that its character encoding is UTF-8 */
    if(H5Lget_linkinfo(file_id, "dataset2", &linfo) < 0) TEST_ERROR;
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;

    /* Create a new link to the dataset with a different character encoding. */
    if(H5Pclose(lcpl_id)<0) TEST_ERROR;
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) <0) TEST_ERROR;
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_ASCII) < 0) TEST_ERROR;

    if(H5Lcreate_hard(file_id, "/dataset2", file_id, "/dataset2_link", lcpl_id) < 0) TEST_ERROR;

    /* Check that its character encoding is ASCII */
    if(H5Lget_linkinfo(file_id, "/dataset2_link", &linfo) < 0) TEST_ERROR;
    if(linfo.cset != H5T_CSET_ASCII) TEST_ERROR;

    /* Check that the first link's encoding hasn't changed */
    if(H5Lget_linkinfo(file_id, "/dataset2", &linfo) < 0) TEST_ERROR;
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;

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
    h5_fixname(FILENAME[8], fapl, filename, sizeof filename);
    if ((file_a=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        TEST_ERROR;
    h5_fixname(FILENAME[9], fapl, filename, sizeof filename);
    if ((file_b=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        TEST_ERROR;

    /* Create groups in first file */
    if((grp_1=H5Gcreate(file_a, "group1", 0))<0) TEST_ERROR;
    if((grp_2=H5Gcreate(file_a, "group2", 0))<0) TEST_ERROR;
    if((grp_move=H5Gcreate(grp_1, "group_move", 0))<0) TEST_ERROR;

    /* Create hard and soft links. */
    if(H5Lcreate_hard(grp_1, "group_move", H5L_SAME_LOC, "hard", H5P_DEFAULT)<0)
	TEST_ERROR;
    if(H5Lcreate_soft("/group1/group_move", grp_2, "soft", H5P_DEFAULT)<0)
	TEST_ERROR;

    /* Move a group within the file.  Both of source and destination use
     * H5L_SAME_LOC.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lmove(H5L_SAME_LOC, "group_move", H5L_SAME_LOC, "group_new_name", H5P_DEFAULT)
                !=FAIL) TEST_ERROR;
    } H5E_END_TRY;

    /* Move a group across files.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lmove(grp_1, "group_move", file_b, "group_new_name", H5P_DEFAULT)
                !=FAIL) TEST_ERROR;
    } H5E_END_TRY;

    /* Move a group across groups in the same file while renaming it. */
    if(H5Lmove(grp_1, "group_move", grp_2, "group_new_name", H5P_DEFAULT)<0)
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
    if(H5Lmove(grp_2, "group_new_name", H5L_SAME_LOC, "group_newer_name", H5P_DEFAULT)<0)
	TEST_ERROR;

    /* Open the group. */
    if((moved_grp = H5Gopen(grp_2, "group_newer_name"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;

    /* Use H5Lmove to move a group without renaming it. */
    if(H5Lmove(grp_2, "group_newer_name", grp_1, "group_newer_name", H5P_DEFAULT)<0)
	TEST_ERROR;

    /* Open the group . */
    if((moved_grp = H5Gopen(grp_1, "group_newer_name"))<0)
	TEST_ERROR;
    if( H5Gclose(moved_grp) < 0)
        TEST_ERROR;

    /* Move the group while giving long paths. */
    if(H5Lmove(file_a, "/group1/group_newer_name", grp_2, "/group2/group_newest_name", H5P_DEFAULT)<0)
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
    h5_fixname(FILENAME[8], fapl, filename, sizeof filename);
    if ((file_a=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        TEST_ERROR;
    h5_fixname(FILENAME[9], fapl, filename, sizeof filename);
    if ((file_b=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        TEST_ERROR;

    /* Create groups in first file */
    if((grp_1=H5Gcreate(file_a, "group1", 0))<0) TEST_ERROR;
    if((grp_2=H5Gcreate(file_a, "group2", 0))<0) TEST_ERROR;
    if((grp_move=H5Gcreate(grp_1, "group_copy", 0))<0) TEST_ERROR;

    /* Create hard and soft links. */
    if(H5Lcreate_hard(grp_1, "group_copy", H5L_SAME_LOC, "hard", H5P_DEFAULT)<0)
	TEST_ERROR;
    if(H5Lcreate_soft("/group1/group_copy", grp_2, "soft", H5P_DEFAULT)<0)
	TEST_ERROR;

    /* Copy a group within the file.  Both of source and destination use
     * H5L_SAME_LOC.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lcopy(H5L_SAME_LOC, "group_copy", H5L_SAME_LOC, "group_new_name", H5P_DEFAULT)
                !=FAIL) TEST_ERROR;
    } H5E_END_TRY;

    /* Copy a group across files.  Should fail. */
    H5E_BEGIN_TRY {
        if(H5Lcopy(grp_1, "group_copy", file_b, "group_new_name", H5P_DEFAULT)
                !=FAIL) TEST_ERROR;
    } H5E_END_TRY;

    /* Move a group across groups in the same file while renaming it. */
    if(H5Lcopy(grp_1, "group_copy", grp_2, "group_new_name", H5P_DEFAULT)<0)
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
    if(H5Lcopy(grp_2, "group_new_name", H5L_SAME_LOC, "group_newer_name", H5P_DEFAULT)<0)
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
    if(H5Lcopy(grp_2, "group_newer_name", grp_1, "group_newer_name", H5P_DEFAULT)<0)
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
    if(H5Lcopy(file_a, "/group1/group_newer_name", grp_2, "/group2/group_newest_name", H5P_DEFAULT)<0)
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
    h5_fixname(FILENAME[11], fapl_id, filename, sizeof filename);

    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id))<0) TEST_ERROR;

    /* Create a link creation property list with the UTF-8 character encoding */
    if((lcpl_id = H5Pcreate(H5P_LINK_CREATE)) <0) TEST_ERROR;
    if(H5Pset_char_encoding(lcpl_id, H5T_CSET_UTF8) < 0) TEST_ERROR;
    /* Create a group with that lcpl */
    if((group_id = H5Gcreate_expand(file_id, H5P_DEFAULT, H5P_DEFAULT)) <0) TEST_ERROR;
    if(H5Llink(file_id, "group", group_id, lcpl_id) < 0) TEST_ERROR;
    if(H5Gclose(group_id) < 0) TEST_ERROR;

    /* Get the group's link's creation time */
    if(H5Lget_linkinfo(file_id, "group", &linfo) < 0) TEST_ERROR;
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
    if(H5Lget_linkinfo(file_id, "group", &linfo) < 0) TEST_ERROR;
    if(H5Gget_objinfo(file_id, "group", TRUE, &statbuf) <0) TEST_ERROR;
    if(old_modification_time != statbuf.mtime) TEST_ERROR;
    if(old_create_time != linfo.ctime) TEST_ERROR;

    /* Create a new link to the group.  It should have a different creation time but the same modification time */
    if(H5Lcreate_hard(file_id, "group", file_id, "group2", H5P_DEFAULT) < 0) TEST_ERROR;
    if(H5Gget_objinfo(file_id, "group2", TRUE, &statbuf) <0) TEST_ERROR;
    if(old_modification_time != statbuf.mtime) TEST_ERROR;
    if(H5Lget_linkinfo(file_id, "group2", &linfo) <0) TEST_ERROR;
    if(old_create_time == linfo.ctime) TEST_ERROR;

    /* Copy the first link to a UTF-8 name.  Its creation time and modification time should not change. */
    if(H5Lcopy(file_id, "group", file_id, "group_copied", lcpl_id) <0) TEST_ERROR;
    if(H5Gget_objinfo(file_id, "group_copied", TRUE, &statbuf) <0) TEST_ERROR;
    if(old_modification_time != statbuf.mtime) TEST_ERROR;
    if(H5Lget_linkinfo(file_id, "group_copied", &linfo) <0) TEST_ERROR;
    if(old_create_time != linfo.ctime) TEST_ERROR;

    /* Check that its character encoding is UTF-8 */
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;

    /* Move the link with the default property list. */
    if(H5Lmove(file_id, "group_copied", file_id, "group_copied2", H5P_DEFAULT) <0) TEST_ERROR;
    if(H5Gget_objinfo(file_id, "group_copied2", TRUE, &statbuf) <0) TEST_ERROR;
    if(old_modification_time != statbuf.mtime) TEST_ERROR;
    if(H5Lget_linkinfo(file_id, "group_copied2", &linfo) <0) TEST_ERROR;
    if(old_create_time != linfo.ctime) TEST_ERROR;

    /* Check that its character encoding is not UTF-8 */
    if(linfo.cset == H5T_CSET_UTF8) TEST_ERROR;

    /* Check that the original link is unchanged */
    if(H5Gget_objinfo(file_id, "group", TRUE, &statbuf) <0) TEST_ERROR;
    if(old_modification_time != statbuf.mtime) TEST_ERROR;
    if(H5Lget_linkinfo(file_id, "group", &linfo) <0) TEST_ERROR;
    if(old_create_time != linfo.ctime) TEST_ERROR;
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;

    /* Move the first link to a UTF-8 name.  Its creation time and modification time should not change. */
    if(H5Lmove(file_id, "group", file_id, "group_moved", lcpl_id) <0) TEST_ERROR;
    if(H5Gget_objinfo(file_id, "group_moved", TRUE, &statbuf) <0) TEST_ERROR;
    if(old_modification_time != statbuf.mtime) TEST_ERROR;
    if(H5Lget_linkinfo(file_id, "group_moved", &linfo) <0) TEST_ERROR;
    if(old_create_time != linfo.ctime) TEST_ERROR;

    /* Check that its character encoding is UTF-8 */
    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR;

    /* Move the link again using the default property list. */
    if(H5Lmove(file_id, "group_moved", file_id, "group_moved_again", H5P_DEFAULT) <0) TEST_ERROR;
    if(H5Gget_objinfo(file_id, "group_moved_again", TRUE, &statbuf) <0) TEST_ERROR;
    if(old_modification_time != statbuf.mtime) TEST_ERROR;
    if(H5Lget_linkinfo(file_id, "group_moved_again", &linfo) <0) TEST_ERROR;
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
    h5_fixname(FILENAME[15], fapl, filename, sizeof filename);

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

    /* Results */
    if (nerrors) {
	printf("***** %d LINK TEST%s FAILED! *****\n",
	       nerrors, 1 == nerrors ? "" : "S");
	exit(1);
    }
    printf("All link tests passed.\n");
    h5_cleanup(FILENAME, fapl);
    return 0;
}

