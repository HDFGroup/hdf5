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
#include "h5test.h"

const char *FILENAME[] = {
    "links1",
    "links2",
    "links3",
    NULL
};

#define LINK_BUF_SIZE   1024
#define NAME_BUF_SIZE   1024
#define MAX_NAME_LEN    ((64*1024)+1024)


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
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) {
	goto error;
    }
    if ((scalar=H5Screate_simple (1, size, size))<0) goto error;

    /* Create a group */
    if ((grp=H5Gcreate (file, "grp1", (size_t)0))<0) goto error;
    if (H5Gclose (grp)<0) goto error;

    /* Create a dataset */
    if ((d1=H5Dcreate (file, "d1", H5T_NATIVE_INT, scalar, H5P_DEFAULT))<0) {
	goto error;
    }
    if (H5Dclose (d1)<0) goto error;

    /* Create a hard link */
    if (H5Glink (file, H5G_LINK_HARD, "d1", "grp1/hard")<0) goto error;

    /* Create a symbolic link */
    if (H5Glink (file, H5G_LINK_SOFT, "/d1", "grp1/soft")<0) goto error;

    /* Create a symbolic link to something that doesn't exist */
    if (H5Glink (file, H5G_LINK_SOFT, "foobar", "grp1/dangle")<0) goto error;

    /* Create a recursive symbolic link */
    if (H5Glink (file, H5G_LINK_SOFT, "/grp1/recursive",
		 "/grp1/recursive")<0) {
	goto error;
    }

    /* Close */
    if (H5Sclose (scalar)<0) goto error;
    if (H5Fclose (file)<0) goto error;

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

    TESTING("H5Glink2 function");

    /* Create two files */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file_a=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) {
        goto error;
    }
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);
    if ((file_b=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) {
        goto error;
    }
    if ((scalar=H5Screate_simple (1, size, size))<0) goto error;

    /* Create two groups in each file */
    if ((grp1_a=H5Gcreate (file_a, "grp1", (size_t)0))<0) goto error;
    if ((grp2_a=H5Gcreate (file_a, "grp2", (size_t)0))<0) goto error;
    if ((grp1_b=H5Gcreate (file_b, "grp1", (size_t)0))<0) goto error;
    if ((grp2_b=H5Gcreate (file_b, "grp2", (size_t)0))<0) goto error;

    /* Create datasets */
    if((dset1=H5Dcreate(file_a, "dataset1", H5T_NATIVE_INT, scalar,
	H5P_DEFAULT))<0) {
	goto error;
    }
    if((dset2=H5Dcreate(grp1_a, "dataset2", H5T_NATIVE_INT, scalar,
        H5P_DEFAULT))<0) {
        goto error;
    }

    /* Create links within a file.  Both of source and destination use
     * H5G_SAME_LOC.  Both hard and soft links should fail. */
    H5E_BEGIN_TRY {
        if(H5Glink2(H5G_SAME_LOC, "dataset1", H5G_LINK_HARD , H5G_SAME_LOC,
		"hard")!=FAIL) goto error;
    } H5E_END_TRY;
    H5E_BEGIN_TRY {
        if(H5Glink2(H5G_SAME_LOC, "dataset1", H5G_LINK_SOFT , H5G_SAME_LOC,
        	"soft")!=FAIL) goto error;
    } H5E_END_TRY;

    /* Create links across files.  Both hard and soft links should fail. */
    H5E_BEGIN_TRY {
        if(H5Glink2(file_a, "dataset1", H5G_LINK_HARD , file_b,
        	"hard")!=FAIL) goto error;
    } H5E_END_TRY;
    H5E_BEGIN_TRY {
        if(H5Glink2(file_a, "dataset1", H5G_LINK_SOFT, file_b, "soft")!=FAIL)
            goto error;
    } H5E_END_TRY;

    /* Create links to test H5G_SAME_LOC, H5G_LINK_HARD, H5G_LINK_SOFT. */
    if(H5Glink2(grp1_a, "dataset2", H5G_LINK_HARD , H5G_SAME_LOC,
        "hard1")<0) {
        goto error;
    }
    if(H5Glink2(H5G_SAME_LOC, "dataset2", H5G_LINK_SOFT , grp1_a,
	"soft1")<0) {
        goto error;
    }

    /* Create links to test H5G_LINK_HARD, H5G_LINK_SOFT across different
     * locations. */
    if(H5Glink2(grp1_a, "dataset2", H5G_LINK_HARD, grp2_a, "hard2")<0) {
        goto error;
    }
    if(H5Glink2(grp1_a, "/grp1/dataset2", H5G_LINK_SOFT , grp2_a, "soft2")<0) {
        goto error;
    }

    /* Close dataspace and files */
    if (H5Sclose (scalar)<0) goto error;
    if (H5Dclose(dset1)<0) goto error;
    if (H5Dclose(dset2)<0) goto error;
    if (H5Gclose (grp1_a)<0) goto error;
    if (H5Gclose (grp2_a)<0) goto error;
    if (H5Gclose (grp1_b)<0) goto error;
    if (H5Gclose (grp2_b)<0) goto error;
    if (H5Fclose (file_a)<0) goto error;
    if (H5Fclose (file_b)<0) goto error;

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
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fopen(filename, H5F_ACC_RDONLY, fapl))<0) {
	goto error;
    }

    /* Hard link */
    if (H5Gget_objinfo(file, "d1", TRUE, &sb1)<0) goto error;
    if (H5Gget_objinfo(file, "grp1/hard", TRUE, &sb2)<0) goto error;
    if (H5G_DATASET!=sb2.type) {
	H5_FAILED();
	puts("    Unexpected object type should have been a dataset");
	goto error;
    }
    if (sb1.objno[0]!=sb2.objno[0] || sb1.objno[1]!=sb2.objno[1]) {
	H5_FAILED();
	puts("    Hard link test failed. Link seems not to point to the ");
	puts("    expected file location.");
	goto error;
    }

    /* Symbolic link */
    if (H5Gget_objinfo(file, "grp1/soft", TRUE, &sb2)<0) goto error;
    if (H5G_DATASET!=sb2.type) {
	H5_FAILED();
	puts("    Unexpected object type should have been a dataset");
	goto error;
    }
    if (sb1.objno[0]!=sb2.objno[0] || sb1.objno[1]!=sb2.objno[1]) {
	H5_FAILED();
	puts("    Soft link test failed. Link seems not to point to the ");
	puts("    expected file location.");
	goto error;
    }
    if (H5Gget_linkval(file, "grp1/soft", sizeof linkval, linkval)<0) {
	goto error;
    }
    if (HDstrcmp(linkval, "/d1")) {
	H5_FAILED();
	puts("    Soft link test failed. Wrong link value");
	goto error;
    }

    /* Dangling link */
    H5E_BEGIN_TRY {
	status = H5Gget_objinfo(file, "grp1/dangle", TRUE, &sb2);
    } H5E_END_TRY;
    if (status>=0) {
	H5_FAILED();
	puts("    H5Gget_objinfo() should have failed for a dangling link.");
	goto error;
    }
    if (H5Gget_objinfo(file, "grp1/dangle", FALSE, &sb2)<0) goto error;
    if (H5G_LINK!=sb2.type) {
	H5_FAILED();
	puts("    Unexpected object type should have been a symbolic link");
	goto error;
    }
    if (H5Gget_linkval(file, "grp1/dangle", sizeof linkval, linkval)<0) {
	goto error;
    }
    if (HDstrcmp(linkval, "foobar")) {
	H5_FAILED();
	puts("    Dangling link test failed. Wrong link value");
	goto error;
    }

    /* Recursive link */
    H5E_BEGIN_TRY {
	status = H5Gget_objinfo(file, "grp1/recursive", TRUE, &sb2);
    } H5E_END_TRY;
    if (status>=0) {
	H5_FAILED();
	puts("    H5Gget_objinfo() should have failed for a recursive link.");
	goto error;
    }
    if (H5Gget_objinfo(file, "grp1/recursive", FALSE, &sb2)<0) goto error;
    if (H5G_LINK!=sb2.type) {
	H5_FAILED();
	puts("    Unexpected object type should have been a symbolic link");
	goto error;
    }
    if (H5Gget_linkval(file, "grp1/recursive", sizeof linkval, linkval)<0) {
	goto error;
    }
    if (HDstrcmp(linkval, "/grp1/recursive")) {
	H5_FAILED();
	puts("   Recursive link test failed. Wrong link value");
	goto error;
    }

    /* Cleanup */
    if (H5Fclose(file)<0) goto error;
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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
ck_new_links(hid_t fapl)
{
    hid_t 		file;
    H5G_stat_t		sb_dset, sb_hard1, sb_hard2, sb_soft1, sb_soft2;
    char 		filename[NAME_BUF_SIZE];
    char 		linkval[LINK_BUF_SIZE];

    TESTING("new link queries");

    /* Open the file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file=H5Fopen(filename, H5F_ACC_RDONLY, fapl))<0) {
        goto error;
    }

    /* Get hard link info */
    if(H5Gget_objinfo(file, "/grp1/dataset2", TRUE, &sb_dset)<0)
	goto error;
    if(H5Gget_objinfo(file, "/grp1/hard1", TRUE, &sb_hard1)<0)
	goto error;
    if(H5Gget_objinfo(file, "/grp2/hard2", TRUE, &sb_hard2)<0)
	goto error;

    /* Check hard links */
    if(H5G_DATASET!=sb_hard1.type || H5G_DATASET!=sb_hard2.type) {
	H5_FAILED();
	puts("    Unexpected object type, should have been a dataset");
	goto error;
    }
    if( sb_dset.objno[0]!=sb_hard1.objno[0] ||
        sb_dset.objno[1]!=sb_hard1.objno[1] ||
        sb_dset.objno[0]!=sb_hard2.objno[0] ||
        sb_dset.objno[1]!=sb_hard2.objno[1] ) {
	H5_FAILED();
	puts("    Hard link test failed.  Link seems not to point to the ");
	puts("    expected file location.");
	goto error;
    }

    /* Get soft link info */
    if(H5Gget_objinfo(file, "/grp1/soft1", TRUE, &sb_soft1)<0) goto error;
    if(H5Gget_objinfo(file, "/grp2/soft2", TRUE, &sb_soft2)<0) goto error;

    /* Check soft links */
    if(H5G_DATASET!=sb_soft1.type || H5G_DATASET!=sb_soft2.type) {
        H5_FAILED();
        puts("    Unexpected object type, should have been a dataset");
        goto error;
    }

    if( sb_dset.objno[0]!=sb_soft1.objno[0] ||
        sb_dset.objno[1]!=sb_soft1.objno[1] ||
        sb_dset.objno[0]!=sb_soft2.objno[0] ||
        sb_dset.objno[1]!=sb_soft2.objno[1] ) {
        H5_FAILED();
        puts("    Soft link test failed.  Link seems not to point to the ");
        puts("    expected file location.");
        goto error;
    }

    if (H5Gget_linkval(file, "grp2/soft2", sizeof linkval, linkval)<0) {
        goto error;
    }
    if (HDstrcmp(linkval, "/grp1/dataset2")) {
        H5_FAILED();
        puts("    Soft link test failed. Wrong link value");
        goto error;
    }

    /* Cleanup */
    if(H5Fclose(file)<0) goto error;
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
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((fid=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Create group with short name in file (used as target for hard links) */
    if((gid=H5Gcreate (fid, "grp1", (size_t)0))<0) TEST_ERROR;

    /* Construct very long file name */
    if((objname = HDmalloc((size_t)(MAX_NAME_LEN + 1))) == NULL) TEST_ERROR;
    for(u = 0; u < MAX_NAME_LEN; u++)
        objname[u] = 'a';
    objname[MAX_NAME_LEN] = '\0';

    /* Create hard link to existing object */
    if(H5Glink2(fid, "grp1", H5G_LINK_HARD, fid, objname) < 0) TEST_ERROR;

    /* Create soft link to existing object */
    objname[0] = 'b';
    if(H5Glink2(fid, "grp1", H5G_LINK_SOFT, fid, objname) < 0) TEST_ERROR;

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
