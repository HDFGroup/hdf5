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
    "links",
    NULL
};


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
    static hsize_t	size[1] = {1};
    char		filename[1024];

    TESTING("link creation");

    /* Create a file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) {
	goto error;
    }
    if ((scalar=H5Screate_simple (1, size, size))<0) goto error;

    /* Create a group */
    if ((grp=H5Gcreate (file, "grp1", 0))<0) goto error;
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
    char		linkval[1024];
    char		filename[1024];
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
    if (strcmp(linkval, "/d1")) {
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
    if (strcmp(linkval, "foobar")) {
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
    if (strcmp(linkval, "/grp1/recursive")) {
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
