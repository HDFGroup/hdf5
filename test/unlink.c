/*
 * Copyright (C) 1998  NCSA
 *                     All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, September 25, 1998
 *
 * Purpose:	Test H5Gunlink().
 */

/* See H5private.h for how to include headers */
#undef NDEBUG
#include <H5config.h>

#ifdef STDC_HEADERS
#   include <stdlib.h>
#endif

#include <hdf5.h>

#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif

#define FILE_NAME_1	"unlink.h5"
#define THE_OBJECT	"/foo"


/*-------------------------------------------------------------------------
 * Function:	cleanup
 *
 * Purpose:	Removes test files
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, June  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
cleanup (void)
{
    if (!getenv ("HDF5_NOCLEANUP")) {
	remove (FILE_NAME_1);
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
 * Function:	test_one
 *
 * Purpose:	Creates a group that has just one entry and then unlinks that
 *		entry.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_one(hid_t file)
{
    hid_t	work, grp;
    herr_t	status;
    
    /* Create a test group */
    if ((work=H5Gcreate(file, "/test_one", 0))<0) goto error;

    /* Delete by absolute name */
    printf("%-70s", "Testing unlink by absolute name");
    fflush(stdout);
    if ((grp=H5Gcreate(work, "foo", 0))<0) goto error;
    if (H5Gclose(grp)<0) goto error;
    if (H5Gunlink(file, "/test_one/foo")<0) goto error;
    puts(" PASSED");

    /* Delete by local name */
    printf("%-70s", "Testing unlink by local name");
    fflush(stdout);
    if ((grp=H5Gcreate(work, "foo", 0))<0) goto error;
    if (H5Gclose(grp)<0) goto error;
    if (H5Gunlink(work, "foo")<0) goto error;
    puts(" PASSED");

    /* Delete directly - should fail */
    printf("%-70s", "Testing unlink without a name");
    fflush(stdout);
    if ((grp=H5Gcreate(work, "foo", 0))<0) goto error;
    H5E_BEGIN_TRY {
	status = H5Gunlink(grp, ".");
    } H5E_END_TRY;
    if (status>=0) {
	puts("*FAILED*");
	puts("   Unlinking object w/o a name should have failed.");
	goto error;
    }
    if (H5Gclose(grp)<0) goto error;
    puts(" PASSED");

    /* Cleanup */
    if (H5Gclose(work)<0) goto error;
    return 0;
 error:
    H5Gclose(work);
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_many
 *
 * Purpose:	Tests many unlinks in a single directory.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_many(hid_t file)
{
    hid_t	work, grp;
    int		i;
    const int	how_many=500;
    char	name[32];
    
    /* Create a test group */
    if ((work=H5Gcreate(file, "/test_many", 0))<0) goto error;
    if ((grp = H5Gcreate(work, "/test_many_foo", 0))<0) goto error;
    if (H5Gclose(grp)<0) goto error;

    /* Create a bunch of names and unlink them in order */
    printf("%-70s", "Testing forward unlink");
    fflush(stdout);
    for (i=0; i<how_many; i++) {
	sprintf(name, "obj_%05d", i);
	if (H5Glink(work, H5G_LINK_HARD, "/test_many_foo", name)<0) goto error;
    }
    for (i=0; i<how_many; i++) {
	sprintf(name, "obj_%05d", i);
	if (H5Gunlink(work, name)<0) goto error;
    }
    puts(" PASSED");

    /* Create a bunch of names and unlink them in reverse order */
    printf("%-70s", "Testing backward unlink");
    fflush(stdout);
    for (i=0; i<how_many; i++) {
	sprintf(name, "obj_%05d", i);
	if (H5Glink(work, H5G_LINK_HARD, "/test_many_foo", name)<0) goto error;
    }
    for (i=how_many-1; i>=0; --i) {
	sprintf(name, "obj_%05d", i);
	if (H5Gunlink(work, name)<0) goto error;
    }
    puts(" PASSED");

    /* Create a bunch of names and unlink them from both directions */
    printf("%-70s", "Testing inward unlink");
    fflush(stdout);
    for (i=0; i<how_many; i++) {
	sprintf(name, "obj_%05d", i);
	if (H5Glink(work, H5G_LINK_HARD, "/test_many_foo", name)<0) goto error;
    }
    for (i=0; i<how_many; i++) {
	if (i%2) {
	    sprintf(name, "obj_%05d", how_many-(1+i/2));
	} else {
	    sprintf(name, "obj_%05d", i/2);
	}
	if (H5Gunlink(work, name)<0) goto error;
    }
    puts(" PASSED");
    
    /* Create a bunch of names and unlink them from the midle */
    printf("%-70s", "Testing outward unlink");
    fflush(stdout);
    for (i=0; i<how_many; i++) {
	sprintf(name, "obj_%05d", i);
	if (H5Glink(work, H5G_LINK_HARD, "/test_many_foo", name)<0) goto error;
    }
    for (i=how_many-1; i>=0; --i) {
	if (i%2) {
	    sprintf(name, "obj_%05d", how_many-(1+i/2));
	} else {
	    sprintf(name, "obj_%05d", i/2);
	}
	if (H5Gunlink(work, name)<0) goto error;
    }
    puts(" PASSED");
    

    /* Cleanup */
    if (H5Gclose(work)<0) goto error;
    return 0;
 error:
    H5Gclose(work);
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_symlink
 *
 * Purpose:	Tests removal of symbolic links.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_symlink(hid_t file)
{
    hid_t	work;

    printf("%-70s", "Testing symlink removal");
    fflush(stdout);
    
    /* Create a test group and symlink */
    if ((work=H5Gcreate(file, "/test_symlink", 0))<0) goto error;
    if (H5Glink(work, H5G_LINK_SOFT, "link_value", "link")<0) goto error;
    if (H5Gunlink(work, "link")<0) goto error;

    /* Cleanup */
    if (H5Gclose(work)<0) goto error;
    puts(" PASSED");
    return 0;
 error:
    H5Gclose(work);
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_rename
 *
 * Purpose:	Tests H5Gmove()
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_rename(hid_t file)
{
    hid_t	work, foo, inner;

    
    /* Create a test group and rename something */
    printf("%-70s", "Testing object renaming");
    fflush(stdout);
    if ((work=H5Gcreate(file, "/test_rename", 0))<0) goto error;
    if ((foo=H5Gcreate(work, "foo", 0))<0) goto error;
    if (H5Gmove(work, "foo", "bar")<0) goto error;
    if ((inner=H5Gcreate(foo, "inner", 0))<0) goto error;
    if (H5Gclose(inner)<0) goto error;
    if (H5Gclose(foo)<0) goto error;
    if ((inner=H5Gopen(work, "bar/inner"))<0) goto error;
    if (H5Gclose(inner)<0) goto error;
    puts(" PASSED");

    /* Try renaming a symlink */
    printf("%-70s", "Testing symlink renaming");
    fflush(stdout);
    if (H5Glink(work, H5G_LINK_SOFT, "link_value", "link_one")<0) goto error;
    if (H5Gmove(work, "link_one", "link_two")<0) goto error;
    puts(" PASSED");

    /* Cleanup */
    if (H5Gclose(work)<0) goto error;
    return 0;
 error:
    H5Gclose(work);
    return 1;
}
    


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test H5Gunlink()
 *
 * Return:	Success:	zero
 *
 *		Failure:	non-zero
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	file;
    int		nerrors = 0;

    /* Open */
    H5Eset_auto(display_error_cb, NULL);
    if ((file=H5Fcreate(FILE_NAME_1, H5F_ACC_TRUNC,
			H5P_DEFAULT, H5P_DEFAULT))<0) goto error;

    /* Tests */
    nerrors += test_one(file);
    nerrors += test_many(file);
    nerrors += test_symlink(file);
    nerrors += test_rename(file);
    
    /* Close */
    if (H5Fclose(file)<0) goto error;
    if (nerrors) {
	printf("***** %d FAILURE%s! *****\n", nerrors, 1==nerrors?"":"S");
	exit(1);
    }
    puts("All unlink tests passed.");
    cleanup();
    return 0;
 error:
    return 1;
}

    
