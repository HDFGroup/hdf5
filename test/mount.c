/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Wednesday, October  7, 1998
 *
 * Purpose:	Tests file mounting.
 */
#include <hdf5.h>
#include <stdlib.h>

#define FALSE		0
#define TRUE		1

#define FILE_NAME_1	"mount_1.h5"
#define FILE_NAME_2	"mount_2.h5"
#define FILE_NAME_3	"mount_3.h5"

#include <H5config.h>
#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif

#if defined(WIN32)
#undef __unused__
#define __unused__
#endif


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
cleanup(void)
{
    if (!getenv("HDF5_NOCLEANUP")) {
	remove(FILE_NAME_1);
	remove(FILE_NAME_2);
	remove(FILE_NAME_3);
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
display_error_cb(void __unused__ *client_data)
{
    puts("*FAILED*");
    H5Eprint(stdout);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	setup
 *
 * Purpose:	Create some files and populate them with a few groups.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
setup(void)
{
    hid_t	file=-1;

    /* file 1 */
    if ((file=H5Fcreate(FILE_NAME_1, H5F_ACC_TRUNC, H5P_DEFAULT,
			H5P_DEFAULT))<0) goto error;
    if (H5Gclose(H5Gcreate(file, "/mnt1", 0))<0) goto error;
    if (H5Gclose(H5Gcreate(file, "/mnt1/file1", 0))<0) goto error;
    if (H5Gclose(H5Gcreate(file, "/mnt_unlink", 0))<0) goto error;
    if (H5Gclose(H5Gcreate(file, "/mnt_move_a", 0))<0) goto error;
    if (H5Glink(file, H5G_LINK_HARD, "/mnt1/file1", "/file1")<0) goto error;
    if (H5Glink(file, H5G_LINK_HARD, "/mnt1", "/mnt1_link")<0) goto error;
    if (H5Fclose(file)<0) goto error;

    /* file 2 */
    if ((file=H5Fcreate(FILE_NAME_2, H5F_ACC_TRUNC, H5P_DEFAULT,
			H5P_DEFAULT))<0) goto error;
    if (H5Gclose(H5Gcreate(file, "/file2", 0))<0) goto error;
    if (H5Gclose(H5Gcreate(file, "/rename_a", 0))<0) goto error;
    if (H5Gclose(H5Gcreate(file, "/rename_b", 0))<0) goto error;
    if (H5Gclose(H5Gcreate(file, "/rename_a/x", 0))<0) goto error;
    if (H5Fclose(file)<0) goto error;

    /* file 3 */
    if ((file=H5Fcreate(FILE_NAME_3, H5F_ACC_TRUNC, H5P_DEFAULT,
			H5P_DEFAULT))<0) goto error;
    if (H5Fclose(file)<0) goto error;

    return 0;
    
 error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_basic
 *
 * Purpose:	Mount file1 at file2:/mnt1 and try accessing an object in
 *		file2.  Then unmount.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_basic(void)
{
    hid_t	file1=-1, file2=-1, grp=-1;

    printf("%-70s", "Testing basic functionality");
    fflush(stdout);

    if ((file1=H5Fopen(FILE_NAME_1, H5F_ACC_RDONLY, H5P_DEFAULT))<0 ||
	(file2=H5Fopen(FILE_NAME_2, H5F_ACC_RDONLY, H5P_DEFAULT))<0)
	goto error;
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;
    if ((grp=H5Gopen(file1, "/mnt1/file2"))<0) goto error;
    if (H5Gclose(grp)<0) goto error;
    if (H5Funmount(file1, "/mnt1")<0) goto error;
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;

    puts(" PASSED");
    return 0;
    
 error:
    H5E_BEGIN_TRY {
	H5Fclose(file1);
	H5Fclose(file2);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_illegal
 *
 * Purpose:	Test things that are illegal to do.  We should get a failure
 *		from the library for each of them.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_illegal(void)
{
    hid_t	file1=-1, file2=-1, file3=-1, mnt=-1;
    herr_t	status;

    printf("%-70s", "Testing illegal mount operations");
    fflush(stdout);

    /* Open the files */
    if ((file1=H5Fopen(FILE_NAME_1, H5F_ACC_RDONLY, H5P_DEFAULT))<0 ||
	(file2=H5Fopen(FILE_NAME_2, H5F_ACC_RDONLY, H5P_DEFAULT))<0 ||
	(file3=H5Fopen(FILE_NAME_3, H5F_ACC_RDONLY, H5P_DEFAULT))<0)
	goto error;

    /* Try mounting a file on itself */
    H5E_BEGIN_TRY {
	status = H5Fmount(file1, "/mnt1", file1, H5P_DEFAULT);
    } H5E_END_TRY;
    if (status>=0) {
	puts("*FAILED*");
	puts("   Mounting a file on itself should have failed.");
	goto error;
    }

    /*
     * Try mounting two files at the same place.  We have to open the mount
     * point before we mount the first file or we'll end up mounting file3 at
     * the root of file2 and the mount will succeed.
     */
    if ((mnt=H5Gopen(file1, "/mnt1"))<0) goto error;
    if (H5Fmount(mnt, ".", file2, H5P_DEFAULT)<0) goto error;
    H5E_BEGIN_TRY {
	status = H5Fmount(mnt, ".", file3, H5P_DEFAULT);
    } H5E_END_TRY;
    if (status>=0) {
	puts("*FAILED*");
	puts("   Mounting two files at one mount point should have failed.");
	goto error;
    }
    if (H5Funmount(mnt, ".")<0) goto error;
    if (H5Gclose(mnt)<0) goto error;
    

    /* Close everything and return */
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;
    if (H5Fclose(file3)<0) goto error;
    puts(" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(mnt);
	H5Fclose(file1);
	H5Fclose(file2);
	H5Fclose(file3);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_hide
 *
 * Purpose:	The previous contents of the mount point is temporarily
 *		hidden. If objects in that group had names from other groups
 *		then the objects will still be visible by those other names.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_hide(void)
{
    hid_t	file1=-1, file2=-1, grp=-1;
    H5G_stat_t	sb1, sb2;

    printf("%-70s", "Testing name hiding under mount point");
    fflush(stdout);
    
    if ((file1=H5Fopen(FILE_NAME_1, H5F_ACC_RDONLY, H5P_DEFAULT))<0 ||
	(file2=H5Fopen(FILE_NAME_2, H5F_ACC_RDONLY, H5P_DEFAULT))<0)
	goto error;

    /* Get information about file1:/mnt1/file1 for later */
    if (H5Gget_objinfo(file1, "/mnt1/file1", TRUE, &sb1)<0) goto error;
    
    /* Build the virtual file */
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;

    /* Original names under file1:/mnt1 should not be accessible */
    H5E_BEGIN_TRY {
	grp = H5Gopen(file1, "/mnt1/file1");
    } H5E_END_TRY;
    if (grp>=0) {
	puts("*FAILED*");
	puts("   Name is still accessible under mount point.");
	goto error;
    }

    /*
     * The original objects under file1:/mnt1 are still accessible by their
     * other names.  This is a rather stupid test but demonstrates a point.
     */
    if (H5Gget_objinfo(file1, "/file1", TRUE, &sb2)<0) goto error;
    if (sb1.fileno[0]!=sb2.fileno[0] || sb1.fileno[1]!=sb2.fileno[1] ||
	sb1.objno[0]!=sb2.objno[0] || sb1.objno[1]!=sb2.objno[1]) {
	puts("*FAILED*");
	puts("   Hard link failed for hidden object.");
	goto error;
    }

    /* Unmount and close objects */
    if (H5Funmount(file1, "/mnt1")<0) goto error;
    H5Fclose(file1);
    H5Fclose(file2);
    puts(" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(grp);
	H5Fclose(file1);
	H5Fclose(file2);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_assoc
 *
 * Purpose:	Opening the mount point is the same as opening the root group
 *		of the mounted file before the file was mounted.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, October 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_assoc(void)
{
    hid_t	file1=-1, file2=-1;
    H5G_stat_t	sb1, sb2;
    
    printf("%-70s", "Testing mount point open");
    fflush(stdout);
    
    /* Open the files */
    if ((file1=H5Fopen(FILE_NAME_1, H5F_ACC_RDONLY, H5P_DEFAULT))<0 ||
	(file2=H5Fopen(FILE_NAME_2, H5F_ACC_RDONLY, H5P_DEFAULT))<0)
	goto error;

    /* Get information about the root of file2 */
    if (H5Gget_objinfo(file2, "/", TRUE, &sb1)<0) goto error;

    /* Create the virtual file */
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;

    /*
     * Get info about the mount point -- should be the same as the root group
     * of file2.
     */
    if (H5Gget_objinfo(file1, "/mnt1", TRUE, &sb2)<0) goto error;
    if (sb1.fileno[0]!=sb2.fileno[0] || sb1.fileno[1]!=sb2.fileno[1] ||
	sb1.objno[0]!=sb2.objno[0] || sb1.objno[1]!=sb2.objno[1]) {
	puts("*FAILED*");
	puts("   Association failed.");
	goto error;
    }
    
    /* Shut down */
    if (H5Funmount(file1, "/mnt1_link")<0) goto error;
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;
    puts(" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Fclose(file2);
	H5Fclose(file1);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_mntlnk
 *
 * Purpose:	The mount point is actually an OID (not a name) so if there
 *		are other names for that group then the root group of the
 *		child will be visible in all those names.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_mntlnk(void)
{
    hid_t	file1=-1, file2=-1, grp=-1;

    printf("%-70s", "Testing multi-linked mount point");
    fflush(stdout);
    
    /* Build the virtual file */
    if ((file1=H5Fopen(FILE_NAME_1, H5F_ACC_RDONLY, H5P_DEFAULT))<0 ||
	(file2=H5Fopen(FILE_NAME_2, H5F_ACC_RDONLY, H5P_DEFAULT))<0)
	goto error;
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;

    /*
     * Can we see file2:/file2 as both file1:/mnt1/file2 and
     * file1:/mnt1_link/file2?
     */
    if ((grp=H5Gopen(file1, "/mnt1/file2"))<0) goto error;
    if (H5Gclose(grp)<0) goto error;
    if ((grp=H5Gopen(file1, "/mnt1_link/file2"))<0) goto error;
    if (H5Gclose(grp)<0) goto error;

    /* Unlink using second name */
    if (H5Funmount(file1, "/mnt1_link")<0) goto error;
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;
    puts(" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(grp);
	H5Fclose(file1);
	H5Fclose(file2);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_move
 *
 * Purpose:	An object cannot be moved or renamed with H5Gmove() in such a
 *		way that the new location would be in a different file than
 *		the original location.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_move(void)
{
    hid_t	file1=-1, file2=-1;
    herr_t	status;
    
    printf("%-70s", "Testing object renaming");
    fflush(stdout);
    
    /* Build the virtual file */
    if ((file1=H5Fopen(FILE_NAME_1, H5F_ACC_RDWR, H5P_DEFAULT))<0 ||
	(file2=H5Fopen(FILE_NAME_2, H5F_ACC_RDWR, H5P_DEFAULT))<0)
	goto error;
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;

    /* First rename an object in the mounted file, then try it across files */
    if (H5Gmove(file1, "/mnt1/rename_a/x", "/mnt1/rename_b/y")<0) goto error;
    H5E_BEGIN_TRY {
	status = H5Gmove(file1, "/mnt1/rename_b/y",  "/y");
    } H5E_END_TRY;
    if (status>=0) {
	puts("*FAILED*");
	puts("   Moving an object across files should not have been possible");
	goto error;
    }

    /* Shut down */
    if (H5Funmount(file1, "/mnt1")<0) goto error;
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;
    puts(" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Fclose(file1);
	H5Fclose(file2);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_preopen
 *
 * Purpose:	Objects that are opened under the mount point before the
 *		child is mounted will remain accessible.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_preopen(void)
{
    hid_t	file1=-1, file2=-1, grp=-1;

    printf("%-70s", "Testing preopening objects under the mount point");
    fflush(stdout);
    
    if ((file1=H5Fopen(FILE_NAME_1, H5F_ACC_RDONLY, H5P_DEFAULT))<0 ||
	(file2=H5Fopen(FILE_NAME_2, H5F_ACC_RDONLY, H5P_DEFAULT))<0)
	goto error;

    /* Open something under the mount point */
    if ((grp=H5Gopen(file1, "/mnt1/file1"))<0) goto error;
    
    /* Build the virtual file */
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;

    /* Now access the thing we previously opened */
    if (H5Gget_objinfo(grp, ".", TRUE, NULL)<0) goto error;

    /* Shut down */
    if (H5Funmount(file1, "/mnt1")<0) goto error;
    if (H5Gclose(grp)<0) goto error;
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;
    puts(" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(grp);
	H5Fclose(file2);
	H5Fclose(file1);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_postopen
 *
 * Purpose:	Objects that are open in a mounted file remain accessible
 *		after the file is unmounted.  Unmounting the file doesn't
 *		close the file.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 14, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_postopen(void)
{
    
    hid_t	file1=-1, file2=-1, grp=-1;

    printf("%-70s", "Testing open object access after unmount");
    fflush(stdout);

    /* Create the virtual file */
    if ((file1=H5Fopen(FILE_NAME_1, H5F_ACC_RDONLY, H5P_DEFAULT))<0 ||
	(file2=H5Fopen(FILE_NAME_2, H5F_ACC_RDONLY, H5P_DEFAULT))<0)
	goto error;
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;

    /* Open some object in the mounted file */
    if ((grp=H5Gopen(file1, "/mnt1/file2"))<0) goto error;

    /* Unmount the file */
    if (H5Funmount(file1, "/mnt1")<0) goto error;
    
    /* Now access the thing we previously opened */
    if (H5Gget_objinfo(grp, ".", TRUE, NULL)<0) goto error;

    /* Try accessing it from the file */
    if (H5Gget_objinfo(file2, "/file2", TRUE, NULL)<0) goto error;

    /* Shut down */
    if (H5Gclose(grp)<0) goto error;
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;
    puts(" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(grp);
	H5Fclose(file2);
	H5Fclose(file1);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_unlink
 *
 * Purpose:	Unlinking the mount point from its name doesn't affect its
 *		existence or the fact that there is a file that is mounted
 *		there.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, October 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_unlink(void)
{
    hid_t	file1=-1, file2=-1, mnt=-1, root=-1;
    herr_t	status;

    printf("%-70s", "Testing mount point unlinking");
    fflush(stdout);

    /* Open files */
    if ((file1=H5Fopen(FILE_NAME_1, H5F_ACC_RDWR, H5P_DEFAULT))<0 ||
	(file2=H5Fopen(FILE_NAME_2, H5F_ACC_RDWR, H5P_DEFAULT))<0)
	goto error;

    /*
     * Opening the mount point before mounting opens the group in the parent
     * file, but opening the mount point after mounting is the same as
     * opening the root group of the child file.
     */
    if ((mnt=H5Gopen(file1, "/mnt_unlink"))<0) goto error;
    if (H5Fmount(file1, "/mnt_unlink", file2, H5P_DEFAULT)<0) goto error;
    if ((root=H5Gopen(file1, "/mnt_unlink"))<0) goto error;

    /*
     * "/file2" of file2 should be visible as an absolute name through either
     * file handle but not from the `mnt' handle since that handle was opened
     * before the H5Fmount() and thus refers to the mount point itself rather
     * than the group mounted there.
     */
    if (H5Gget_objinfo(file1, "/mnt_unlink/file2", TRUE, NULL)<0) goto error;
    if (H5Gget_objinfo(mnt, "/mnt_unlink/file2", TRUE, NULL)<0) goto error;
    if (H5Gget_objinfo(root, "/mnt_unlink/file2", TRUE, NULL)<0) goto error;
    if (H5Gget_objinfo(root, "file2", TRUE, NULL)<0) goto error;
    H5E_BEGIN_TRY {
	status = H5Gget_objinfo(mnt, "file2", TRUE, NULL);
    } H5E_END_TRY;
    if (status>=0) {
	puts("*FAILED*");
	puts("   Incorrect traversal from mount point!");
	goto error;
    }
    
    /* Unlink the mount point */
    if (H5Gunlink(file1, "/mnt_unlink")<0) goto error;

    /*
     * We should still be able to get to "/file2" of file2 by starting at
     * `root' which is still open, but not by name.
     */
    if (H5Gget_objinfo(root, "file2", TRUE, NULL)<0) goto error;
    H5E_BEGIN_TRY {
	status = H5Gget_objinfo(mnt, "file2", TRUE, NULL);
    } H5E_END_TRY;
    if (status>=0) {
	puts("*FAILED*");
	puts("   Traversal through mount point should not have worked!");
	goto error;
    }
    H5E_BEGIN_TRY {
	status = H5Gget_objinfo(file2, "/mnt_unlink/file2", TRUE, NULL);
    } H5E_END_TRY;
    if (status>=0) {
	puts("*FAILED*");
	puts("   Traversal through mount point should not have worked!");
	goto error;
    }

    /*
     * It's no longer possible to unmount the child by supplying the name of
     * the mount point because the name doesn't exist anymore.  We must
     * supply the mount point directly.
     */
    H5E_BEGIN_TRY {
	status = H5Funmount(file1, "/mnt_unlink");
    } H5E_END_TRY;
    if (status>=0) {
	puts("*FAILED*");
	puts("   Unmount by name should not have been allowed!");
	goto error;
    }
    H5E_BEGIN_TRY {
	status = H5Funmount(file2, "/");
    } H5E_END_TRY;
    if (status>=0) {
	puts("*FAILED*");
	puts("   Unmount by name should not have been allowed!");
	goto error;
    }
    if (H5Funmount(mnt, ".")<0) goto error;
    
    /* Close files */
    if (H5Gclose(mnt)<0) goto error;
    if (H5Gclose(root)<0) goto error;
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;
    puts(" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(mnt);
	H5Gclose(root);
	H5Fclose(file2);
	H5Fclose(file1);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_mvmpt
 *
 * Purpose:	Try renaming the mount point while a file is mounted there.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, October 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_mvmpt(void)
{
    hid_t	file1=-1, file2=-1;
    
    printf("%-70s", "Testing mount point renaming");
    fflush(stdout);
    
    /* Build the virtual file */
    if ((file1=H5Fopen(FILE_NAME_1, H5F_ACC_RDWR, H5P_DEFAULT))<0 ||
	(file2=H5Fopen(FILE_NAME_2, H5F_ACC_RDWR, H5P_DEFAULT))<0)
	goto error;
    if (H5Fmount(file1, "/mnt_move_a", file2, H5P_DEFAULT)<0) goto error;

    /* Rename the mount point */
    if (H5Gmove(file1, "/mnt_move_a", "/mnt_move_b")<0) goto error;

    /* Access something under the new name */
    if (H5Gget_objinfo(file1, "/mnt_move_b/file2", TRUE, NULL)<0) goto error;

    /* Shut down */
    if (H5Funmount(file1, "/mnt_move_b")<0) goto error;
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;
    puts(" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Fclose(file1);
	H5Fclose(file2);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_interlink
 *
 * Purpose:	Hard links cannot cross file boundaries.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 14, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_interlink(void)
{
    hid_t	file1=-1, file2=-1, type=-1, space=-1, dset=-1;
    herr_t	status;
    hsize_t	cur_dims[1] = {2};
    
    printf("%-70s", "Testing interfile hard links");
    fflush(stdout);
    
    /* Build the virtual file */
    if ((file1=H5Fopen(FILE_NAME_1, H5F_ACC_RDWR, H5P_DEFAULT))<0 ||
	(file2=H5Fopen(FILE_NAME_2, H5F_ACC_RDWR, H5P_DEFAULT))<0)
	goto error;
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;

    /* Try an interfile hard link directly */
    H5E_BEGIN_TRY {
	status = H5Glink(file1, H5G_LINK_HARD, "/mnt1/file2",  "/file2");
    } H5E_END_TRY;
    if (status>=0) {
	puts("*FAILED*");
	puts("   Interfile hard link should not have been allowed!");
	goto error;
    }

    /* Try an interfile hard link by renaming something */
    H5E_BEGIN_TRY {
	status = H5Gmove(file1, "/mnt1/file2", "/file2");
    } H5E_END_TRY;
    if (status>=0) {
	puts("*FAILED*");
	puts("   Interfile renaming should not have been allowed!");
	goto error;
    }

    /* Try an interfile hard link by sharing a data type */
    if ((type=H5Tcopy(H5T_NATIVE_INT))<0) goto error;
    if (H5Tcommit(file1, "/type1", type)<0) goto error;
    if ((space=H5Screate_simple(1, cur_dims, NULL))<0) goto error;
    H5E_BEGIN_TRY {
	dset = H5Dcreate(file1, "/mnt1/file2/dset", type, space, H5P_DEFAULT);
    } H5E_END_TRY;
    if (dset>=0) {
	puts("*FAILED*");
	puts("   Dataset and shared type must be in the same file!");
	goto error;
    }
    
    /* Shut down */
    if (H5Sclose(space)<0) goto error;
    if (H5Tclose(type)<0) goto error;
    if (H5Funmount(file1, "/mnt1")<0) goto error;
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;
    puts(" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Dclose(dset);
	H5Sclose(space);
	H5Tclose(type);
	H5Fclose(file1);
	H5Fclose(file2);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_uniformity
 *
 * Purpose:	Any file handle is equivalent to any other file handle in the
 *		same virtual file.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 14, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_uniformity(void)
{
    hid_t	file1=-1, file2=-1;
    
    printf("%-70s", "Testing file handle uniformity");
    fflush(stdout);
    
    /* Build the virtual file */
    if ((file1=H5Fopen(FILE_NAME_1, H5F_ACC_RDWR, H5P_DEFAULT))<0 ||
	(file2=H5Fopen(FILE_NAME_2, H5F_ACC_RDWR, H5P_DEFAULT))<0)
	goto error;
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;

    /* Access some things from the file1 handle */
    if (H5Gget_objinfo(file1, "/", TRUE, NULL)<0) goto error;
    if (H5Gget_objinfo(file1, "/mnt1", TRUE, NULL)<0) goto error;
    if (H5Gget_objinfo(file1, "mnt1", TRUE, NULL)<0) goto error;
    if (H5Gget_objinfo(file1, "/mnt1/file2", TRUE, NULL)<0) goto error;
    if (H5Gget_objinfo(file1, "mnt1/file2", TRUE, NULL)<0) goto error;
    
    /* Access the same things from the file2 handle */
    if (H5Gget_objinfo(file2, "/", TRUE, NULL)<0) goto error;
    if (H5Gget_objinfo(file2, "/mnt1", TRUE, NULL)<0) goto error;
    if (H5Gget_objinfo(file2, "mnt1", TRUE, NULL)<0) goto error;
    if (H5Gget_objinfo(file2, "/mnt1/file2", TRUE, NULL)<0) goto error;
    if (H5Gget_objinfo(file2, "mnt1/file2", TRUE, NULL)<0) goto error;

    /* Shut down */
    if (H5Funmount(file1, "/mnt1")<0) goto error;
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;
    puts(" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Fclose(file1);
	H5Fclose(file2);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_close
 *
 * Purpose:	Closing any file handle closes the entire virtual file.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 14, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_close(void)
{
    hid_t	file1=-1, file2=-1;
    herr_t	status;
    
    printf("%-70s", "Testing file handle close");
    fflush(stdout);
    
    /* Build the virtual file */
    if ((file1=H5Fopen(FILE_NAME_1, H5F_ACC_RDWR, H5P_DEFAULT))<0 ||
	(file2=H5Fopen(FILE_NAME_2, H5F_ACC_RDWR, H5P_DEFAULT))<0)
	goto error;
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;

    /*
     * Close file1 unmounting it from the virtual file.  Objects in file2 are
     * still accessible through the file2 handle, but nothing in file1 is
     * accessible.
     */
    if (H5Fclose(file1)<0) goto error;
    H5E_BEGIN_TRY {
	status = H5Gget_objinfo(file2, "/mnt1", TRUE, NULL);
    } H5E_END_TRY;
    if (status>=0) {
	puts("*FAILED*");
	puts("   File1 contents are still accessible!");
	goto error;
    }
    if (H5Fclose(file2)<0) goto error;

    /* Build the virtual file again */
    if ((file1=H5Fopen(FILE_NAME_1, H5F_ACC_RDWR, H5P_DEFAULT))<0 ||
	(file2=H5Fopen(FILE_NAME_2, H5F_ACC_RDWR, H5P_DEFAULT))<0)
	goto error;
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;

    /*
     * Close file2.  It is not actually closed because it's a child of file1.
     */    
    if (H5Fclose(file2)<0) goto error;
    if (H5Gget_objinfo(file1, "/mnt1/file2", TRUE, NULL)<0) goto error;
    if (H5Fclose(file1)<0) goto error;
    
    /* Shut down */
    puts(" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Fclose(file1);
	H5Fclose(file2);
    } H5E_END_TRY;
    return 1;
}
    
    

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test file mounting
 *
 * Return:	Success:	zero
 *
 *		Failure:	non-zero
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int		nerrors = 0;
    
    H5Eset_auto(display_error_cb, NULL);
    if (setup()<0) goto error;

    nerrors += test_basic();
    nerrors += test_illegal();
    nerrors += test_hide();
    nerrors += test_assoc();
    nerrors += test_mntlnk();
    nerrors += test_unlink();
    nerrors += test_move();
    nerrors += test_mvmpt();
    nerrors += test_preopen();
    nerrors += test_postopen();
    nerrors += test_interlink();
    nerrors += test_uniformity();
    nerrors += test_close();
    
    if (nerrors) goto error;
    puts("All mount tests passed.");
    cleanup();
    return(0);
    
 error:
    puts("***** MOUNT ERRORS *****");
    return(1);
}
