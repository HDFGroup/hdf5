/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, April 10, 1998
 *
 * Purpose:	Tests hard and soft (symbolic) links.
 */
#include <hdf5.h>
#include <stdlib.h>
#include <string.h>

#include <H5config.h>
#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif

#define TEST_FILE_NAME	"links.h5"

#define FALSE		0
#define TRUE		1


/*-------------------------------------------------------------------------
 * Function:	cleanup
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
 *              May 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
cleanup(void)
{
    if (!getenv ("HDF5_NOCLEANUP")) {
	remove(TEST_FILE_NAME);
    }
}


/*-------------------------------------------------------------------------
 * Function:	display_error_cb
 *
 * Purpose:	Displays the error stack after printing "*FAILED*".
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		Wednesday, March  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
display_error_cb (void __unused__ *client_data)
{
    puts ("*FAILED*");
    H5Eprint (stdout);
    return 0;
}


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
mklinks(void)
{
    hid_t		file, scalar, grp, d1;
    static hsize_t	size[1] = {1};

    printf("%-70s", "Testing link creation");

    /* Create a file */
    if ((file=H5Fcreate(TEST_FILE_NAME, H5F_ACC_TRUNC,
			H5P_DEFAULT, H5P_DEFAULT))<0) {
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

    puts(" PASSED");
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
cklinks(void)
{
    hid_t		file;
    H5G_stat_t		sb1, sb2;
    char		linkval[1024];
    herr_t		status;

    printf("%-70s", "Testing link queries");
    fflush(stdout);

    /* Open the file */
    if ((file=H5Fopen(TEST_FILE_NAME, H5F_ACC_RDONLY, H5P_DEFAULT))<0) {
	goto error;
    }

    /* Hard link */
    if (H5Gget_stat(file, "d1", TRUE, &sb1)<0) goto error;
    if (H5Gget_stat(file, "grp1/hard", TRUE, &sb2)<0) goto error;
    if (H5G_DATASET!=sb2.type) {
	puts("*FAILED*");
	puts("   Unexpected object type should have been a dataset");
	goto error;
    }
    if (sb1.objno[0]!=sb2.objno[0] || sb1.objno[1]!=sb2.objno[1]) {
	puts("*FAILED*");
	puts("   Hard link test failed. Link seems not to point to the ");
	puts("   expected file location.");
	goto error;
    }

    /* Symbolic link */
    if (H5Gget_stat(file, "grp1/soft", TRUE, &sb2)<0) goto error;
    if (H5G_DATASET!=sb2.type) {
	puts("*FAILED*");
	puts("   Unexpected object type should have been a dataset");
	goto error;
    }
    if (sb1.objno[0]!=sb2.objno[0] || sb1.objno[1]!=sb2.objno[1]) {
	puts("*FAILED*");
	puts("   Soft link test failed. Link seems not to point to the ");
	puts("   expected file location.");
	goto error;
    }
    if (H5Gget_linkval(file, "grp1/soft", sizeof linkval, linkval)<0) {
	goto error;
    }
    if (strcmp(linkval, "/d1")) {
	puts("*FAILED*");
	puts("   Soft link test failed. Wrong link value");
	goto error;
    }

    /* Dangling link */
    H5E_BEGIN_TRY {
	status = H5Gget_stat(file, "grp1/dangle", TRUE, &sb2);
    } H5E_END_TRY;
    if (status>=0) {
	puts("*FAILED*");
	puts("   H5Gget_stat() should have failed for a dangling link.");
	goto error;
    }
    if (H5Gget_stat(file, "grp1/dangle", FALSE, &sb2)<0) goto error;
    if (H5G_LINK!=sb2.type) {
	puts("*FAILED*");
	puts("   Unexpected object type should have been a symbolic link");
	goto error;
    }
    if (H5Gget_linkval(file, "grp1/dangle", sizeof linkval, linkval)<0) {
	goto error;
    }
    if (strcmp(linkval, "foobar")) {
	puts("*FAILED*");
	puts("   Dangling link test failed. Wrong link value");
	goto error;
    }

    /* Recursive link */
    H5E_BEGIN_TRY {
	status = H5Gget_stat(file, "grp1/recursive", TRUE, &sb2);
    } H5E_END_TRY;
    if (status>=0) {
	puts("*FAILED*");
	puts("   H5Gget_stat() should have failed for a recursive link.");
	goto error;
    }
    if (H5Gget_stat(file, "grp1/recursive", FALSE, &sb2)<0) goto error;
    if (H5G_LINK!=sb2.type) {
	puts("*FAILED*");
	puts("   Unexpected object type should have been a symbolic link");
	goto error;
    }
    if (H5Gget_linkval(file, "grp1/recursive", sizeof linkval, linkval)<0) {
	goto error;
    }
    if (strcmp(linkval, "/grp1/recursive")) {
	puts("*FAILED*");
	puts("   Recursive link test failed. Wrong link value");
	goto error;
    }

    /* Cleanup */
    if (H5Fclose(file)<0) goto error;
    puts(" PASSED");
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

    /* Set error handling to print `*FAILED*' before the error stack */
    H5Eset_auto(display_error_cb, NULL);

    /* The tests... */
    nerrors += mklinks() < 0 ? 1 : 0;
    nerrors += cklinks() < 0 ? 1 : 0;

    /* Results */
    if (nerrors) {
	printf("***** %d LINK TEST%s FAILED! *****\n",
	       nerrors, 1 == nerrors ? "" : "S");
	exit(1);
    }
    printf("All link tests passed.\n");
    cleanup();
    return 0;
}
