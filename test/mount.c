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
 *              Wednesday, October  7, 1998
 *
 * Purpose:	Tests file mounting.
 */
#include "h5test.h"
#include "H5Fprivate.h"		/* File access				*/
#include "H5Iprivate.h"		/* IDs			  		*/

const char *FILENAME[] = {
    "mount_1",
    "mount_2",
    "mount_3",
    "mount_4",
    "mount_5",
    "mount_6",
    "mount_7",
    NULL
};

/* For "mount_after_close" test */
#define RANK 2
#define NX 4
#define NY 5
#define NAME_BUF_SIZE   40
int bm[NX][NY], bm_out[NX][NY]; /* Data buffers */


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
setup(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];

    /* file 1 */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	goto error;
    if (H5Gclose(H5Gcreate(file, "/mnt1", (size_t)0))<0) goto error;
    if (H5Gclose(H5Gcreate(file, "/mnt1/file1", (size_t)0))<0) goto error;
    if (H5Gclose(H5Gcreate(file, "/mnt_unlink", (size_t)0))<0) goto error;
    if (H5Gclose(H5Gcreate(file, "/mnt_move_a", (size_t)0))<0) goto error;
    if (H5Glink(file, H5G_LINK_HARD, "/mnt1/file1", "/file1")<0) goto error;
    if (H5Glink(file, H5G_LINK_HARD, "/mnt1", "/mnt1_link")<0) goto error;
    if (H5Fclose(file)<0) goto error;

    /* file 2 */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	goto error;
    if (H5Gclose(H5Gcreate(file, "/file2", (size_t)0))<0) goto error;
    if (H5Gclose(H5Gcreate(file, "/rename_a", (size_t)0))<0) goto error;
    if (H5Gclose(H5Gcreate(file, "/rename_b", (size_t)0))<0) goto error;
    if (H5Gclose(H5Gcreate(file, "/rename_a/x", (size_t)0))<0) goto error;
    if (H5Fclose(file)<0) goto error;

    /* file 3 */
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	goto error;
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
test_basic(hid_t fapl)
{
    hid_t	file1=-1, file2=-1, grp=-1;
    char	filename1[1024], filename2[1024];

    TESTING("basic functionality");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    if ((file1 = H5Fopen(filename1, H5F_ACC_RDONLY, fapl)) < 0 ||
            (file2 = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT) < 0)
        TEST_ERROR
    if ((grp = H5Gopen(file1, "/mnt1/file2")) < 0)
        TEST_ERROR
    if (H5Gclose(grp) < 0)
        TEST_ERROR
    if (H5Funmount(file1, "/mnt1") < 0)
        TEST_ERROR
    if (H5Fclose(file1) < 0)
        TEST_ERROR
    if (H5Fclose(file2) < 0)
        TEST_ERROR

    PASSED();
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
test_illegal(hid_t fapl)
{
    hid_t	file1=-1, file2=-1, file3=-1, mnt=-1;
    herr_t	status;
    char	filename1[1024], filename2[1024], filename3[1024];

    TESTING("illegal mount operations");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);
    h5_fixname(FILENAME[2], fapl, filename3, sizeof filename3);


    /* Open the files */
    if ((file1=H5Fopen(filename1, H5F_ACC_RDONLY, fapl))<0 ||
	(file2=H5Fopen(filename2, H5F_ACC_RDONLY, fapl))<0 ||
	(file3=H5Fopen(filename3, H5F_ACC_RDONLY, fapl))<0)
	goto error;

    /* Try mounting a file on itself */
    H5E_BEGIN_TRY {
	status = H5Fmount(file1, "/mnt1", file1, H5P_DEFAULT);
    } H5E_END_TRY;
    if (status>=0) {
	H5_FAILED();
	puts("    Mounting a file on itself should have failed.");
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
	H5_FAILED();
	puts("    Mounting two files at one mount point should have failed.");
	goto error;
    }
    if (H5Funmount(mnt, ".")<0) goto error;
    if (H5Gclose(mnt)<0) goto error;


    /* Close everything and return */
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;
    if (H5Fclose(file3)<0) goto error;
    PASSED();
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
test_hide(hid_t fapl)
{
    hid_t	file1=-1, file2=-1, grp=-1;
    H5G_stat_t	sb1, sb2;
    char	filename1[1024], filename2[1024];

    TESTING("name hiding under mount point");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    if ((file1=H5Fopen(filename1, H5F_ACC_RDONLY, fapl))<0 ||
	(file2=H5Fopen(filename2, H5F_ACC_RDONLY, fapl))<0)
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
	H5_FAILED();
	puts("    Name is still accessible under mount point.");
	goto error;
    }

    /*
     * The original objects under file1:/mnt1 are still accessible by their
     * other names.  This is a rather stupid test but demonstrates a point.
     */
    if (H5Gget_objinfo(file1, "/file1", TRUE, &sb2)<0) goto error;
    if (sb1.fileno[0]!=sb2.fileno[0] || sb1.fileno[1]!=sb2.fileno[1] ||
	sb1.objno[0]!=sb2.objno[0] || sb1.objno[1]!=sb2.objno[1]) {
	H5_FAILED();
	puts("    Hard link failed for hidden object.");
	goto error;
    }

    /* Unmount and close objects */
    if (H5Funmount(file1, "/mnt1")<0) goto error;
    H5Fclose(file1);
    H5Fclose(file2);
    PASSED();
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
test_assoc(hid_t fapl)
{
    hid_t	file1=-1, file2=-1;
    H5G_stat_t	sb1, sb2;
    char	filename1[1024], filename2[1024];

    TESTING("mount point open");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Open the files */
    if ((file1=H5Fopen(filename1, H5F_ACC_RDONLY, fapl))<0 ||
	(file2=H5Fopen(filename2, H5F_ACC_RDONLY, fapl))<0)
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
	H5_FAILED();
	puts("    Association failed.");
	goto error;
    }

    /* Shut down */
    if (H5Funmount(file1, "/mnt1_link")<0) goto error;
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;
    PASSED();
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
test_mntlnk(hid_t fapl)
{
    hid_t	file1=-1, file2=-1, grp=-1;
    char	filename1[1024], filename2[1024];

    TESTING("multi-linked mount point");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);


    /* Build the virtual file */
    if ((file1=H5Fopen(filename1, H5F_ACC_RDONLY, fapl))<0 ||
	(file2=H5Fopen(filename2, H5F_ACC_RDONLY, fapl))<0)
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
    PASSED();
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
test_move(hid_t fapl)
{
    hid_t	file1=-1, file2=-1;
    herr_t	status;
    char	filename1[1024], filename2[1024];

    TESTING("object renaming");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Build the virtual file */
    if ((file1=H5Fopen(filename1, H5F_ACC_RDWR, fapl))<0 ||
	(file2=H5Fopen(filename2, H5F_ACC_RDWR, fapl))<0)
	goto error;
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;

    /* First rename an object in the mounted file, then try it across files */
    if (H5Gmove(file1, "/mnt1/rename_a/x", "/mnt1/rename_b/y")<0) goto error;
    H5E_BEGIN_TRY {
	status = H5Gmove(file1, "/mnt1/rename_b/y",  "/y");
    } H5E_END_TRY;
    if (status>=0) {
	H5_FAILED();
	puts("    Moving an object across files should't have been possible");
	goto error;
    }

    /* Shut down */
    if (H5Funmount(file1, "/mnt1")<0) goto error;
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;
    PASSED();
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
test_preopen(hid_t fapl)
{
    hid_t	file1=-1, file2=-1, grp=-1;
    char	filename1[1024], filename2[1024];

    TESTING("preopening objects under the mount point");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    if ((file1=H5Fopen(filename1, H5F_ACC_RDONLY, fapl))<0 ||
	(file2=H5Fopen(filename2, H5F_ACC_RDONLY, fapl))<0)
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
    PASSED();
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
test_postopen(hid_t fapl)
{

    hid_t	file1=-1, file2=-1, grp=-1;
    char	filename1[1024], filename2[1024];

    TESTING("open object access after unmount");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Create the virtual file */
    if ((file1=H5Fopen(filename1, H5F_ACC_RDONLY, fapl))<0 ||
	(file2=H5Fopen(filename2, H5F_ACC_RDONLY, fapl))<0)
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
    PASSED();
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
test_unlink(hid_t fapl)
{
    hid_t	file1=-1, file2=-1, mnt=-1, root=-1;
    herr_t	status;
    char	filename1[1024], filename2[1024];

    TESTING("mount point unlinking");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Open files */
    if ((file1=H5Fopen(filename1, H5F_ACC_RDWR, fapl))<0 ||
	(file2=H5Fopen(filename2, H5F_ACC_RDWR, fapl))<0)
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
	H5_FAILED();
	puts("    Incorrect traversal from mount point!");
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
	H5_FAILED();
	puts("    Traversal through mount point should not have worked!");
	goto error;
    }
    H5E_BEGIN_TRY {
	status = H5Gget_objinfo(file2, "/mnt_unlink/file2", TRUE, NULL);
    } H5E_END_TRY;
    if (status>=0) {
	H5_FAILED();
	puts("    Traversal through mount point should not have worked!");
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
	H5_FAILED();
	printf("    %d: Unmount by name should not have been allowed!\n",__LINE__);
	goto error;
    }
    H5E_BEGIN_TRY {
	status = H5Funmount(file2, "/");
    } H5E_END_TRY;
    if (status>=0) {
	H5_FAILED();
	printf("    %d: Unmount by name should not have been allowed!\n",__LINE__);
	goto error;
    }
    if (H5Funmount(mnt, ".")<0) goto error;

    /* Close files */
    if (H5Gclose(mnt)<0) goto error;
    if (H5Gclose(root)<0) goto error;
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;
    PASSED();
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
test_mvmpt(hid_t fapl)
{
    hid_t	file1=-1, file2=-1;
    char	filename1[1024], filename2[1024];

    TESTING("mount point renaming");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Build the virtual file */
    if ((file1=H5Fopen(filename1, H5F_ACC_RDWR, fapl))<0 ||
	(file2=H5Fopen(filename2, H5F_ACC_RDWR, fapl))<0)
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
    PASSED();
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
test_interlink(hid_t fapl)
{
    hid_t	file1=-1, file2=-1, type=-1, space=-1, dset=-1;
    char	filename1[1024], filename2[1024];
    herr_t	status;
    hsize_t	cur_dims[1] = {2};

    TESTING("interfile hard links");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Build the virtual file */
    if ((file1=H5Fopen(filename1, H5F_ACC_RDWR, fapl))<0 ||
	(file2=H5Fopen(filename2, H5F_ACC_RDWR, fapl))<0)
	goto error;
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;

    /* Try an interfile hard link directly */
    H5E_BEGIN_TRY {
	status = H5Glink(file1, H5G_LINK_HARD, "/mnt1/file2",  "/file2");
    } H5E_END_TRY;
    if (status>=0) {
	H5_FAILED();
	puts("    Interfile hard link should not have been allowed!");
	goto error;
    }

    /* Try an interfile hard link by renaming something */
    H5E_BEGIN_TRY {
	status = H5Gmove(file1, "/mnt1/file2", "/file2");
    } H5E_END_TRY;
    if (status>=0) {
	H5_FAILED();
	puts("    Interfile renaming should not have been allowed!");
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
	H5_FAILED();
	puts("    Dataset and shared type must be in the same file!");
	goto error;
    }

    /* Shut down */
    if (H5Sclose(space)<0) goto error;
    if (H5Tclose(type)<0) goto error;
    if (H5Funmount(file1, "/mnt1")<0) goto error;
    if (H5Fclose(file1)<0) goto error;
    if (H5Fclose(file2)<0) goto error;
    PASSED();
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
test_uniformity(hid_t fapl)
{
    hid_t	file1=-1, file2=-1;
    char	filename1[1024], filename2[1024];

    TESTING("file handle uniformity");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Build the virtual file */
    if ((file1=H5Fopen(filename1, H5F_ACC_RDWR, fapl))<0 ||
	(file2=H5Fopen(filename2, H5F_ACC_RDWR, fapl))<0)
	TEST_ERROR;
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) TEST_ERROR;

    /* Access some things from the file1 handle */
    if (H5Gget_objinfo(file1, "/", TRUE, NULL)<0) TEST_ERROR;
    if (H5Gget_objinfo(file1, "/mnt1", TRUE, NULL)<0) TEST_ERROR;
    if (H5Gget_objinfo(file1, "mnt1", TRUE, NULL)<0) TEST_ERROR;
    if (H5Gget_objinfo(file1, "/mnt1/file2", TRUE, NULL)<0) TEST_ERROR;
    if (H5Gget_objinfo(file1, "mnt1/file2", TRUE, NULL)<0) TEST_ERROR;

    /* Access the same things from the file2 handle */
    if (H5Gget_objinfo(file2, "/", TRUE, NULL)<0) TEST_ERROR;
    if (H5Gget_objinfo(file2, "/mnt1", TRUE, NULL)<0) TEST_ERROR;
    if (H5Gget_objinfo(file2, "mnt1", TRUE, NULL)<0) TEST_ERROR;
    if (H5Gget_objinfo(file2, "/mnt1/file2", TRUE, NULL)<0) TEST_ERROR;
    if (H5Gget_objinfo(file2, "mnt1/file2", TRUE, NULL)<0) TEST_ERROR;

    /* Shut down */
    if (H5Funmount(file1, "/mnt1")<0) TEST_ERROR;
    if (H5Fclose(file1)<0) TEST_ERROR;
    if (H5Fclose(file2)<0) TEST_ERROR;
    PASSED();
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
test_close(hid_t fapl)
{
    hid_t	file1=-1, file2=-1;
    char	filename1[1024], filename2[1024];

    TESTING("file handle close");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Build the virtual file */
    if ((file1=H5Fopen(filename1, H5F_ACC_RDWR, fapl))<0 ||
	(file2=H5Fopen(filename2, H5F_ACC_RDWR, fapl))<0)
	goto error;
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;

    /*
     * Close file1 unmounting it from the virtual file.  Objects in file1 are
     * still accessible through the file2 handle.
     */
    if (H5Fclose(file1)<0) goto error;
    if(H5Gget_objinfo(file2, "/mnt1", TRUE, NULL) < 0) {
	H5_FAILED();
	puts("    File1 contents are not accessible!");
	goto error;
    }
    if (H5Fclose(file2)<0) goto error;

    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0)
        TEST_ERROR
    if(H5F_sfile_assert_num(0) != 0)
        TEST_ERROR

    /* Build the virtual file again */
    if ((file1=H5Fopen(filename1, H5F_ACC_RDWR, fapl))<0 ||
	(file2=H5Fopen(filename2, H5F_ACC_RDWR, fapl))<0)
	goto error;
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;

    /*
     * Close file2.  It is not actually closed because it's a child of file1.
     */
    if (H5Fclose(file2)<0) goto error;
    if (H5Gget_objinfo(file1, "/mnt1/file2", TRUE, NULL)<0) goto error;
    if (H5Fclose(file1)<0) goto error;

    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0)
        TEST_ERROR
    if(H5F_sfile_assert_num(0) != 0)
        TEST_ERROR

    /* Shut down */
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Fclose(file1);
	H5Fclose(file2);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_mount_after_close
 *
 * Purpose:	Test that the library handles mounting a file on a group
 *              if the group is the only object holding the file open.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May  4, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_mount_after_close(hid_t fapl)
{
    hid_t	fid1=-1, fid2=-1;                     /* File IDs */
    hid_t       gidA=-1, gidAB=-1, gidABM=-1, gidX=-1, gidXY=-1;  /* Group identifiers */
    hid_t       gidABMX=-1, gidABC=-1, gidABT=-1;       /* Group IDs for testing */
    hid_t       didABMXYD=-1;                           /* Dataset ID for testing */
    hid_t       did=-1, sid=-1;                         /* Dataset and dataspace identifiers */
    char	filename1[1024], filename2[1024];       /* Name of files to mount */
    char        objname[NAME_BUF_SIZE];                 /* Name of object opened */
    hsize_t     dims[] = {NX,NY};                       /* Dataset dimensions */
    int         i, j;                                   /* Local index variable */

    TESTING("mounting on group after file is closed");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /*
    * Initialization of buffer matrix "bm"
    */
    for(i =0; i<NX; i++)
        for(j = 0; j<NY; j++)
            bm[i][j] = i + j;

    /* Create first file and a groups in it. */
    /* h5ls -r shows: */
    /* /A                       Group
      /A/B                     Group
      /A/B/C                   -> ./M/X/Y
      /A/B/M                   Group
      /A/B/T                   -> /A
    */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR
    if((gidA = H5Gcreate(fid1, "A", (size_t)0)) < 0)
        TEST_ERROR
    if((gidAB = H5Gcreate(gidA , "B", (size_t)0)) < 0)
        TEST_ERROR
    if((gidABM = H5Gcreate(gidAB , "M", (size_t)0)) < 0) /* Mount point */
        TEST_ERROR
    if(H5Glink(gidAB, H5G_LINK_SOFT, "./M/X/Y", "C") < 0) /* Soft link */
        TEST_ERROR
    if(H5Glink(gidAB, H5G_LINK_SOFT, "/A", "T") < 0) /* Soft link */
        TEST_ERROR

    /* Close groups and file */
    if(H5Gclose(gidABM) < 0)
        TEST_ERROR
    if(H5Gclose(gidAB) < 0)
        TEST_ERROR
    if(H5Gclose(gidA) < 0)
        TEST_ERROR
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

   /* Create second file and dataset "D" in it. */
   /* h5ls shows: */
   /* /X                       Group
      /X/T                     -> ./Y
      /X/Y                     Group
      /X/Y/D                   Dataset {4, 5}
    */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    dims[0] = NX;
    dims[1] = NY;
    if((sid = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR

    if((gidX = H5Gcreate(fid2, "/X", (size_t)0)) < 0)
        TEST_ERROR
    if((gidXY = H5Gcreate(gidX, "Y", (size_t)0)) < 0)
        TEST_ERROR
    if((did = H5Dcreate(gidXY, "D", H5T_NATIVE_INT, sid, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Glink(gidX, H5G_LINK_SOFT, "./Y", "T") < 0) /* Soft link */
        TEST_ERROR

    /* Write data to the dataset. */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, bm) < 0)
        TEST_ERROR

    /* Close all identifiers. */
    if(H5Sclose(sid) < 0)
        TEST_ERROR
    if(H5Dclose(did) < 0)
        TEST_ERROR
    if(H5Gclose(gidXY) < 0)
        TEST_ERROR
    if(H5Gclose(gidX) < 0)
        TEST_ERROR
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

/* Beginning of the actual test code */

   /*
    * Reopen both files
    */
    if((fid1 = H5Fopen(filename1, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR
    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR
   /*
    *  Open /A/B to use as a mount point
    */
    if((gidAB = H5Gopen(fid1, "/A/B")) < 0)
        TEST_ERROR

   /*
    *  Close the parent file. This keeps the file open because of the other handle on the group within
    */
    if(H5Fclose(fid1) < 0) /* We close the file (it should stay open from the group) */
        TEST_ERROR

   /*
    * Mount second file under G in the first file.
    */
    if(H5Fmount(gidAB, "M", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Open "normal" group in mounted file */
    /* (This shows we successfully mounted) */
    if((gidABMX = H5Gopen(gidAB, "M/X")) < 0)
        TEST_ERROR

    /* Check name */
    if(H5Iget_name( gidABMX, objname, (size_t)NAME_BUF_SIZE ) < 0)
        TEST_ERROR
    if(HDstrcmp(objname, "/A/B/M/X"))
        TEST_ERROR

    /* Close object in mounted file */
    if(H5Gclose(gidABMX) < 0)
        TEST_ERROR

    /* Open group in mounted file through softlink */
    if((gidABC = H5Gopen(gidAB, "C")) < 0)
        TEST_ERROR

    /* Check name */
    if(H5Iget_name( gidABC, objname, (size_t)NAME_BUF_SIZE ) < 0)
        TEST_ERROR
    if(HDstrcmp(objname, "/A/B/C"))
        TEST_ERROR

    /* Close object in mounted file */
    if(H5Gclose(gidABC) < 0)
        TEST_ERROR

    /* Open group in original file through softlink */
    if((gidABT = H5Gopen(gidAB, "T")) < 0)
        TEST_ERROR

    /* Check name */
    if(H5Iget_name( gidABT, objname, (size_t)NAME_BUF_SIZE ) < 0)
        TEST_ERROR
    if(HDstrcmp(objname, "/A/B/T"))
        TEST_ERROR

    /* Close object in original file */
    if(H5Gclose(gidABT) < 0)
        TEST_ERROR

    /* Open "normal" dataset in mounted file */
    if((didABMXYD = H5Dopen(gidAB, "M/X/Y/D")) < 0)
        TEST_ERROR

    /* Check name */
    if(H5Iget_name( didABMXYD, objname, (size_t)NAME_BUF_SIZE ) < 0)
        TEST_ERROR
    if(HDstrcmp(objname, "/A/B/M/X/Y/D"))
        TEST_ERROR

    /* Close object in mounted file */
    if(H5Dclose(didABMXYD) < 0)
        TEST_ERROR

    if(H5Gclose(gidAB) < 0)
        TEST_ERROR
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Shut down */
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Sclose(sid);
	H5Dclose(did);
	H5Gclose(didABMXYD);
	H5Gclose(gidABT);
	H5Gclose(gidABC);
	H5Gclose(gidABMX);
	H5Gclose(gidXY);
	H5Gclose(gidX);
	H5Gclose(gidABM);
	H5Gclose(gidAB);
	H5Gclose(gidA);
	H5Fclose(fid1);
        H5Fclose(fid2);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_mount_after_unmount
 *
 * Purpose:	Test that the library handles mounting a file while holding
 *				objects open in a file which has been unmounted.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Monday, June  6, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_mount_after_unmount(hid_t fapl)
{
    hid_t	fid1=-1, fid2=-1, fid3=-1, fid4=-1;      /* File IDs */
    hid_t       gidA=-1, gidB=-1, gidX=-1, gidY=-1, gidZ=-1;  	/* Group identifiers */
    hid_t       gidBM=-1;  			/* Group identifiers */
    hid_t       gidBMZ=-1;  			/* Group identifiers */
    hid_t       gidAM=-1;  			/* Group identifiers */
    hid_t       gidAMX=-1;  			/* Group identifiers */
    hid_t       gidAMXX=-1;  			/* Group identifiers */
    hid_t       gidAMXMY=-1;  			/* Group identifiers */
    hid_t       gidXM=-1;  			/* Group identifiers */
    hid_t       gidXX=-1;  			/* Group identifiers */
    char	filename1[1024],
		filename2[1024],
		filename3[1024],
		filename4[1024];       		/* Name of files to mount */
    char        objname[NAME_BUF_SIZE];                 /* Name of object opened */

    TESTING("mounting after file is unmounted");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);
    h5_fixname(FILENAME[2], fapl, filename3, sizeof filename3);
    h5_fixname(FILENAME[3], fapl, filename4, sizeof filename4);

    /* Create first file and some groups in it. */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR
    if((gidA = H5Gcreate(fid1, "A", (size_t)0)) < 0)
        TEST_ERROR
    if((gidAM = H5Gcreate(gidA, "M", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidAM) < 0)
        TEST_ERROR
    if(H5Gclose(gidA) < 0)
        TEST_ERROR
    if((gidB = H5Gcreate(fid1, "B", (size_t)0)) < 0)
        TEST_ERROR
    if((gidBM = H5Gcreate(gidB, "M", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidBM) < 0)
        TEST_ERROR
    if(H5Gclose(gidB) < 0)
        TEST_ERROR
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

   /* Create second file and a group in it. */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR
    if((gidX = H5Gcreate(fid2, "/X", (size_t)0)) < 0)
        TEST_ERROR
    if((gidXM = H5Gcreate(gidX, "M", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidXM) < 0)
        TEST_ERROR
    if((gidXX = H5Gcreate(gidX, "X", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidXX) < 0)
        TEST_ERROR
    if(H5Gclose(gidX) < 0)
        TEST_ERROR
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

   /* Create third file and a group in it. */
    if((fid3 = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR
    if((gidY = H5Gcreate(fid3, "/Y", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidY) < 0)
        TEST_ERROR
    if(H5Fclose(fid3) < 0)
        TEST_ERROR

   /* Create fourth file and a group in it. */
    if((fid4 = H5Fcreate(filename4, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR
    if((gidZ = H5Gcreate(fid4, "/Z", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidZ) < 0)
        TEST_ERROR
    if(H5Fclose(fid4) < 0)
        TEST_ERROR


/* Beginning of the actual test code */

   /*
    * Reopen all three files
    */
    if((fid1 = H5Fopen(filename1, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR
    if((fid2 = H5Fopen(filename2, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR
    if((fid3 = H5Fopen(filename3, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR
    if((fid4 = H5Fopen(filename4, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

   /*
    *  Open /A & /B to use as a mount points
    */
    if((gidA = H5Gopen(fid1, "/A")) < 0)
        TEST_ERROR
    if((gidB = H5Gopen(fid1, "/B")) < 0)
        TEST_ERROR

   /*
    * Mount second file on /A/M in the first file.
    */
    if(H5Fmount(gidA, "M", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Open group in mounted file */
    /* (This shows we successfully mounted) */
    if((gidAMXX = H5Gopen(gidA, "M/X/X")) < 0)
        TEST_ERROR

    /* Check name */
    if(H5Iget_name( gidAMXX, objname, (size_t)NAME_BUF_SIZE ) < 0)
        TEST_ERROR
    if(HDstrcmp(objname, "/A/M/X/X"))
        TEST_ERROR

    /* Open group in mounted file #2 */
    if((gidAMX = H5Gopen(gidA, "M/X")) < 0)
        TEST_ERROR

    /* Mount third file */
    if(H5Fmount(gidAMX, "M", fid3, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Open group in mounted file #3 */
    /* (This shows we successfully mounted) */
    if((gidAMXMY = H5Gopen(gidAMX, "M/Y")) < 0)
        TEST_ERROR

    /* Check name */
    if(H5Iget_name( gidAMXMY, objname, (size_t)NAME_BUF_SIZE ) < 0)
        TEST_ERROR
    if(HDstrcmp(objname, "/A/M/X/M/Y"))
        TEST_ERROR

    /* Unmount second file */
    if (H5Funmount(fid1, "/A/M")<0)
	TEST_ERROR

    /* Check name */
    *objname = '\0';
    if(H5Iget_name( gidAMXMY, objname, (size_t)NAME_BUF_SIZE ) < 0)
        TEST_ERROR
    if(HDstrcmp(objname, "/X/M/Y"))
        TEST_ERROR

    /* Rename object in file #3 that is "disconnected" from name hiearchy */
    /* (It is "disconnected" because it's parent file has been unmounted) */
    if(H5Gmove2(gidAMX,"M/Y",gidAMX,"M/Z") < 0)
	TEST_ERROR

    /* Close group in file #3 */
    if(H5Gclose(gidAMXMY) < 0)
	TEST_ERROR

    /* Re-open group in file #3 */
    if((gidAMXMY = H5Gopen(gidAMX, "M/Z")) < 0)
        TEST_ERROR

    /* Check name again */
    *objname = '\0';
    if(H5Iget_name( gidAMXMY, objname, (size_t)NAME_BUF_SIZE ) < 0)
	TEST_ERROR
    if(HDstrcmp(objname, "/X/M/Z"))
	TEST_ERROR

    /* Mount fourth file */
    if(H5Fmount(gidB, "M", fid4, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Open group in mounted file */
    /* (This shows we successfully mounted) */
    if((gidBMZ = H5Gopen(gidB, "M/Z")) < 0)
        TEST_ERROR

    /* Check name */
    if(H5Iget_name( gidBMZ, objname, (size_t)NAME_BUF_SIZE ) < 0)
        TEST_ERROR
    if(HDstrcmp(objname, "/B/M/Z"))
        TEST_ERROR

    /* Unmount third file */
    if (H5Funmount(fid2, "/X/M")<0)
	TEST_ERROR

    /* Unmount fourth file */
    if (H5Funmount(fid1, "/B/M")<0)
	TEST_ERROR

    /* Close objects in mounted files */
    if(H5Gclose(gidBMZ) < 0)
	TEST_ERROR
    if(H5Gclose(gidAMXMY) < 0)
	TEST_ERROR
    if(H5Gclose(gidAMXX) < 0)
	TEST_ERROR
    if(H5Gclose(gidAMX) < 0)
	TEST_ERROR

    /* Close objects in original file */
    if(H5Gclose(gidB) < 0)
	TEST_ERROR
    if(H5Gclose(gidA) < 0)
	TEST_ERROR

    /* Close files */
    if(H5Fclose(fid4) < 0)
        TEST_ERROR
    if(H5Fclose(fid3) < 0)
        TEST_ERROR
    if(H5Fclose(fid2) < 0)
        TEST_ERROR
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Shut down */
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(gidZ);
	H5Gclose(gidY);
	H5Gclose(gidXX);
	H5Gclose(gidXM);
	H5Gclose(gidX);
	H5Gclose(gidBMZ);
	H5Gclose(gidBM);
	H5Gclose(gidB);
	H5Gclose(gidAMXMY);
	H5Gclose(gidAMXX);
	H5Gclose(gidAMX);
	H5Gclose(gidAM);
	H5Gclose(gidA);
        H5Fclose(fid4);
        H5Fclose(fid3);
        H5Fclose(fid2);
	H5Fclose(fid1);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_missing_unmount
 *
 * Purpose:	Test that the library correctly closes open files when they
 *              have child files that have not been unmounted.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Thursday, June 30, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_missing_unmount(hid_t fapl)
{
    hid_t fid1=-1, fid2=-1, fid3=-1;    /* File IDs */
    hid_t gidA=-1, gidE=-1, gidM=-1;    /* Group IDs */
    hid_t gidAE=-1, gidAEM=-1;          /* Group IDs */
    char	filename1[1024],
		filename2[1024],
		filename3[1024]; 	/* Name of files to mount */

    TESTING("missing unmount");

    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);
    h5_fixname(FILENAME[2], fapl, filename3, sizeof filename3);

    /* Create file #1 */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gcreate(fid1, "A", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Create file #2 */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidE = H5Gcreate(fid2, "E", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidE) < 0)
        TEST_ERROR

    if(H5Fclose(fid2) < 0)
        TEST_ERROR


    /* Create file #3 */
    if((fid3 = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidM = H5Gcreate(fid3, "M", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidM) < 0)
        TEST_ERROR

    if(H5Fclose(fid3) < 0)
        TEST_ERROR


    /* Re-open files and mount file #2 in file #1 and file #3 in file #2 */
    if((fid1 = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gopen(fid1, "A")) < 0)
        TEST_ERROR

    /* Close file #1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if(H5Fmount(gidA, ".", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Open group in mounted file */
    if((gidAE = H5Gopen(fid2, "A/E")) < 0)
        TEST_ERROR

    /* Close file #2 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    if((fid3 = H5Fopen(filename3, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if(H5Fmount(gidAE, ".", fid3, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Open group in mounted file */
    if((gidAEM = H5Gopen(fid3, "A/E/M")) < 0)
        TEST_ERROR

    /* Close file #3 */
    if(H5Fclose(fid3) < 0)
        TEST_ERROR

    /* (Still have all file #2 & #3 mounted and groups open in all three files) */

    /* Unmount file #2 & #3 */
    if(H5Funmount(gidAE,".") < 0)
        TEST_ERROR

    /* Skip unmounting file #2 from file #1 */

    /* Close groups in mounted file */
    if(H5Gclose(gidAEM) < 0)
        TEST_ERROR

    if(H5Gclose(gidAE) < 0)
        TEST_ERROR

    /* Close group in top file */
    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0)
        TEST_ERROR
    if(H5F_sfile_assert_num(0) != 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(gidM);
	H5Gclose(gidE);
	H5Gclose(gidAEM);
	H5Gclose(gidAE);
	H5Gclose(gidA);
        H5Fclose(fid3);
        H5Fclose(fid2);
	H5Fclose(fid1);
    } H5E_END_TRY;
    return 1;
} /* end test_missing_unmount() */


/*-------------------------------------------------------------------------
 * Function:	test_hold_open_file
 *
 * Purpose:	Test that the library correctly holds open files when they
 *              have child files that have not been unmounted.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July  5, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_hold_open_file(hid_t fapl)
{
    hid_t fid1 = -1, fid2 = -1;           	/* File IDs */
    hid_t gidA = -1, gidM = -1, gidAM = -1;    	/* Group IDs */
    char	filename1[1024],
		filename2[1024]; 	/* Name of files to mount */

    TESTING("hold open w/file");

    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Create file #1 */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gcreate(fid1, "A", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Create file #2 */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidM = H5Gcreate(fid2, "M", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidM) < 0)
        TEST_ERROR

    if(H5Fclose(fid2) < 0)
        TEST_ERROR


    /* Re-open files and mount file #2 in file #1 */
    if((fid1 = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gopen(fid1, "A")) < 0)
        TEST_ERROR

    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if(H5Fmount(gidA, ".", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR

    if((gidAM = H5Gopen(fid1, "A/M")) < 0)
        TEST_ERROR

    /* Close file #2 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Close group in mounted file */
    if(H5Gclose(gidAM) < 0)
        TEST_ERROR

    /* Attempt to open group in mounted file */
    /* (Should work because file is still mounted) */
    if((gidAM = H5Gopen(fid1, "A/M")) < 0)
        TEST_ERROR

    /* Close file #1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Close group in mounted file */
    if(H5Gclose(gidAM) < 0)
        TEST_ERROR

    /* Attempt to open group in mounted file */
    /* (Should work because file is still mounted) */
    if((gidAM = H5Gopen(gidA, "/A/M")) < 0)
        TEST_ERROR

    /* Close group in mounted file */
    if(H5Gclose(gidAM) < 0)
        TEST_ERROR

    /* Unmount file #2 */
    if(H5Funmount(gidA, ".") < 0)
        TEST_ERROR

    /* Close group in top file */
    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0)
        TEST_ERROR
    if(H5F_sfile_assert_num(0) != 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(gidM);
	H5Gclose(gidAM);
	H5Gclose(gidA);
        H5Fclose(fid2);
	H5Fclose(fid1);
    } H5E_END_TRY;
    return 1;
} /* end test_hold_open_file() */


/*-------------------------------------------------------------------------
 * Function:	test_hold_open_group
 *
 * Purpose:	Test that the library correctly holds open file mount
 *              hierarchies when they have objects open.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Thursday, July 14, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_hold_open_group(hid_t fapl)
{
    hid_t fid1 = -1, fid2 = -1;           	/* File IDs */
    hid_t gid = -1, gidA = -1, gidM = -1, gidAM = -1, gidAM2 = -1;    	/* Group IDs */
    char	filename1[1024],
		filename2[1024]; 	/* Name of files to mount */

    TESTING("hold open w/group");

    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Create file #1 */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gcreate(fid1, "A", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Create file #2 */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidM = H5Gcreate(fid2, "M", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidM) < 0)
        TEST_ERROR

    if(H5Fclose(fid2) < 0)
        TEST_ERROR


    /* Re-open files and mount file #2 in file #1 */
    if((fid1 = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gopen(fid1, "A")) < 0)
        TEST_ERROR

    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if(H5Fmount(gidA, ".", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR

    if((gidAM = H5Gopen(fid1, "A/M")) < 0)
        TEST_ERROR

    /* Close file #2 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Close group in parent file */
    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    /* Keep fid1 & gidAM open, everything else closed */

    /* Retry to opening group in mounted file */
    /* (Should work because file is still mounted) */
    if((gidAM2 = H5Gopen(fid1, "A/M")) < 0)
        TEST_ERROR

    /* Close group in mounted file */
    if(H5Gclose(gidAM2) < 0)
        TEST_ERROR

    /* Close original group in mount file */
    if(H5Gclose(gidAM) < 0)
        TEST_ERROR

    /* Attempt to open group in mounted file */
    /* (Should work because file is still mounted) */
    if((gidAM2 = H5Gopen(fid1, "/A/M")) < 0)
        TEST_ERROR

    /* Close file #1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Get ID of file #2 */
    if((fid2 = H5Iget_file_id(gidAM2)) < 0)
        TEST_ERROR

    /* Close group in mounted file */
    if(H5Gclose(gidAM2) < 0)
        TEST_ERROR

    /* Attempt to open group in mounted file */
    /* (Should work because file is still mounted) */
    if((gidAM2 = H5Gopen(fid2, "/A/M")) < 0)
        TEST_ERROR

    /* Close file #2 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Attempt to open group in parent file */
    /* (Should work because files should be mounted together) */
    if((gid = H5Gopen(gidAM2, "/")) < 0)
        TEST_ERROR

    /* Close group in mounted file */
    if(H5Gclose(gidAM2) < 0)
        TEST_ERROR

    /* Close group in parent file */
    if(H5Gclose(gid) < 0)
        TEST_ERROR

    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0)
        TEST_ERROR
    if(H5F_sfile_assert_num(0) != 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(gidM);
	H5Gclose(gidAM);
	H5Gclose(gidAM2);
	H5Gclose(gidA);
	H5Gclose(gid);
        H5Fclose(fid2);
	H5Fclose(fid1);
    } H5E_END_TRY;
    return 1;
} /* end test_hold_open_group() */


/*-------------------------------------------------------------------------
 * Function:	test_fcdegree_same
 *
 * Purpose:	Test that the library will only allow files with same file
 *              close degree to be mounted together.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July 19, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_fcdegree_same(hid_t fapl)
{
    hid_t fid1 = -1, fid2 = -1;           	/* File IDs */
    hid_t gidA = -1, gidM = -1, gidAM = -1;    	/* Group IDs */
    hid_t fapl_id = -1;                         /* FAPL IDs */
    herr_t ret;                                 /* Generic return value */
    char	filename1[1024],
		filename2[1024]; 	/* Name of files to mount */

    TESTING("file close degrees must be same");

    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Create file #1 */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gcreate(fid1, "A", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Create file #2 */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidM = H5Gcreate(fid2, "M", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidM) < 0)
        TEST_ERROR

    if(H5Fclose(fid2) < 0)
        TEST_ERROR


    /* Re-open files and mount file #2 in file #1 */
    if((fid1 = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gopen(fid1, "A")) < 0)
        TEST_ERROR

    /* Create FAPL & set file close degree for file #2 to be different */
    if((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR

    /* Set file close mode to H5F_CLOSE_STRONG */
    if(H5Pset_fclose_degree(fapl_id, H5F_CLOSE_STRONG) < 0)
        TEST_ERROR

    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, fapl_id)) < 0)
        TEST_ERROR

    /* Try mounting file with different file close degree (should fail) */
    H5E_BEGIN_TRY {
        ret = H5Fmount(gidA, ".", fid2, H5P_DEFAULT);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR

    /* Set file close mode to H5F_CLOSE_WEAK */
    if(H5Pset_fclose_degree(fapl_id, H5F_CLOSE_WEAK) < 0)
        TEST_ERROR

    /* Close file #2 & re-open with same file close degree as file #1 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR
    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, fapl_id)) < 0)
        TEST_ERROR

    /* Try mounting files again (should work now) */
    if(H5Fmount(gidA, ".", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Verify opening group in mounted file */
    if((gidAM = H5Gopen(fid1, "A/M")) < 0)
        TEST_ERROR

    /* Close group in mounted file */
    if(H5Gclose(gidAM) < 0)
        TEST_ERROR

    /* Close group in parent file */
    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    /* Close file #2 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Close file #1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Close FAPL ID */
    if(H5Pclose(fapl_id) < 0)
        TEST_ERROR

    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0)
        TEST_ERROR
    if(H5F_sfile_assert_num(0) != 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_id);
	H5Gclose(gidM);
	H5Gclose(gidAM);
	H5Gclose(gidA);
        H5Fclose(fid2);
	H5Fclose(fid1);
    } H5E_END_TRY;
    return 1;
} /* end test_fcdegree_same() */


/*-------------------------------------------------------------------------
 * Function:	test_fcdegree_semi
 *
 * Purpose:	Test that the library perform correct actions when using
 *              "semi" file close degree on mounted files
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July 19, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_fcdegree_semi(hid_t fapl)
{
    hid_t fid1 = -1, fid2 = -1;           	/* File IDs */
    hid_t gidA = -1, gidM = -1, gidAM = -1;    	/* Group IDs */
    hid_t fapl_id = -1;                         /* FAPL IDs */
    herr_t ret;                                 /* Generic return value */
    char	filename1[1024],
		filename2[1024]; 	/* Name of files to mount */

    TESTING("'semi' file close degree");

    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Create file #1 */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gcreate(fid1, "A", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Create file #2 */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidM = H5Gcreate(fid2, "M", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidM) < 0)
        TEST_ERROR

    if(H5Fclose(fid2) < 0)
        TEST_ERROR


    /* Create FAPL & set file close degree to be "semi" */
    if((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR

    /* Set file close mode to H5F_CLOSE_SEMI */
    if(H5Pset_fclose_degree(fapl_id, H5F_CLOSE_SEMI) < 0)
        TEST_ERROR

    /* Re-open files and mount file #2 in file #1 */
    if((fid1 = H5Fopen(filename1, H5F_ACC_RDONLY, fapl_id)) < 0)
        TEST_ERROR

    if((gidA = H5Gopen(fid1, "A")) < 0)
        TEST_ERROR

    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, fapl_id)) < 0)
        TEST_ERROR

    /* Mount files together */
    if(H5Fmount(gidA, ".", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Verify opening group in mounted file */
    if((gidAM = H5Gopen(fid1, "A/M")) < 0)
        TEST_ERROR

    /* Close file #1 (should succeed, since file #2 is open still) */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Try closing file #2 (should fail, since there are still objects open) */
    H5E_BEGIN_TRY {
        ret = H5Fclose(fid2);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR

    /* Close group in parent file */
    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    /* Try closing file #2 (should still fail, since there are still objects open in child file) */
    H5E_BEGIN_TRY {
        ret = H5Fclose(fid2);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR

    /* Close group in mounted file */
    if(H5Gclose(gidAM) < 0)
        TEST_ERROR

    /* Close file #2 (should succeed now) */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Close FAPL ID */
    if(H5Pclose(fapl_id) < 0)
        TEST_ERROR

    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0)
        TEST_ERROR
    if(H5F_sfile_assert_num(0) != 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_id);
	H5Gclose(gidM);
	H5Gclose(gidAM);
	H5Gclose(gidA);
        H5Fclose(fid2);
	H5Fclose(fid1);
    } H5E_END_TRY;
    return 1;
} /* end test_fcdegree_semi() */


/*-------------------------------------------------------------------------
 * Function:	test_fcdegree_strong
 *
 * Purpose:	Test that the library perform correct actions when using
 *              "strong" file close degree on mounted files
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July 19, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_fcdegree_strong(hid_t fapl)
{
    hid_t fid1 = -1, fid2 = -1;           	/* File IDs */
    hid_t gidA = -1, gidM = -1, gidAM = -1;    	/* Group IDs */
    hid_t fapl_id = -1;                         /* FAPL IDs */
    herr_t ret;                                 /* Generic return value */
    char	filename1[1024],
		filename2[1024]; 	/* Name of files to mount */

    TESTING("'strong' file close degree");

    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Create file #1 */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gcreate(fid1, "A", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Create file #2 */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidM = H5Gcreate(fid2, "M", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidM) < 0)
        TEST_ERROR

    if(H5Fclose(fid2) < 0)
        TEST_ERROR


    /* Create FAPL & set file close degree to be "strong" */
    if((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR

    /* Set file close mode to H5F_CLOSE_STRONG */
    if(H5Pset_fclose_degree(fapl_id, H5F_CLOSE_STRONG) < 0)
        TEST_ERROR

    /* Re-open files and mount file #2 in file #1 */
    if((fid1 = H5Fopen(filename1, H5F_ACC_RDONLY, fapl_id)) < 0)
        TEST_ERROR

    if((gidA = H5Gopen(fid1, "A")) < 0)
        TEST_ERROR

    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, fapl_id)) < 0)
        TEST_ERROR

    /* Mount files together */
    if(H5Fmount(gidA, ".", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Open group in mounted file */
    if((gidAM = H5Gopen(fid1, "A/M")) < 0)
        TEST_ERROR

    /* Close file #1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Check that objects are still open */
    if (H5Gget_objinfo(gidA, ".", TRUE, NULL) < 0)
        TEST_ERROR
    if (H5Gget_objinfo(gidAM, ".", TRUE, NULL) < 0)
        TEST_ERROR

    /* Close file #2 (should close open objects also) */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Check that objects are closed */
    H5E_BEGIN_TRY {
        ret = H5Gget_objinfo(gidA, ".", TRUE, NULL);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR
    H5E_BEGIN_TRY {
        ret = H5Gget_objinfo(gidAM, ".", TRUE, NULL);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR

    /* Close FAPL ID */
    if(H5Pclose(fapl_id) < 0)
        TEST_ERROR

    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0)
        TEST_ERROR
    if(H5F_sfile_assert_num(0) != 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_id);
	H5Gclose(gidM);
	H5Gclose(gidAM);
	H5Gclose(gidA);
        H5Fclose(fid2);
	H5Fclose(fid1);
    } H5E_END_TRY;
    return 1;
} /* end test_fcdegree_strong() */


/*-------------------------------------------------------------------------
 * Function:	test_acc_perm
 *
 * Purpose:	Test that the library correctly segregates operations in
 *              parts of mounted file hierarchy with files that have different
 *              R/W access permissions.
 *
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July 19, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_acc_perm(hid_t fapl)
{
    hid_t fid1 = -1, fid2 = -1, fid3 = -1;           	/* File IDs */
    hid_t gidA = -1, gidB = -1, gidC = -1, gidM = -1, gidAM = -1, gidAMZ = -1;    	/* Group IDs */
    hid_t bad_id = -1;                          /* Bad ID from object create */
    char    name[NAME_BUF_SIZE];        /* Buffer for filename retrieved */
    ssize_t name_len;                   /* Filename length */
    char	filename1[1024],
		filename2[1024],
		filename3[1024]; 	/* Name of files to mount */

    TESTING("access permissions");

    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);
    h5_fixname(FILENAME[2], fapl, filename3, sizeof filename3);

    /* Create file #1 */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gcreate(fid1, "A", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Create file #2 */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidM = H5Gcreate(fid2, "M", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidM) < 0)
        TEST_ERROR

    if(H5Fclose(fid2) < 0)
        TEST_ERROR


    /* Re-open files and mount file #2 in file #1 */
    if((fid1 = H5Fopen(filename1, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gopen(fid1, "A")) < 0)
        TEST_ERROR

    /* Get and verify file name */
    if((name_len = H5Fget_name(gidA, name, NAME_BUF_SIZE)) < 0)
        TEST_ERROR
    if(HDstrcmp(name, filename1) != 0)
        TEST_ERROR

    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get and verify file name */
    if((name_len = H5Fget_name(fid2, name, NAME_BUF_SIZE)) < 0)
        TEST_ERROR
    if(HDstrcmp(name, filename2) != 0)
        TEST_ERROR

    /* Mount files together */
    if(H5Fmount(gidA, ".", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Get and verify file name */
    if((name_len = H5Fget_name(fid2, name, NAME_BUF_SIZE)) < 0)
        TEST_ERROR
    if(HDstrcmp(name, filename2) != 0)
        TEST_ERROR

    /* Open group in mounted file */
    if((gidAM = H5Gopen(fid1, "A/M")) < 0)
        TEST_ERROR

    /* Get and verify file name */
    if((name_len = H5Fget_name(gidAM, name, NAME_BUF_SIZE)) < 0)
        TEST_ERROR
    if(HDstrcmp(name, filename2) != 0)
        TEST_ERROR

    /* Attempt to create objects in read only file (should fail) */
    H5E_BEGIN_TRY {
        bad_id = H5Gcreate(gidAM, "Z", (size_t)0);
    } H5E_END_TRY;
    if(bad_id >= 0)
        TEST_ERROR
    H5E_BEGIN_TRY {
        bad_id = H5Gcreate(fid1, "/A/L", (size_t)0);
    } H5E_END_TRY;
    if(bad_id >= 0)
        TEST_ERROR

    /* Attempt to create objects in read/write file (should succeed) */
    if((gidB = H5Gcreate(fid2, "/B", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidB) < 0)
        TEST_ERROR

    /* (Note that this object should get created in the "hidden" group for "A" in parent file) */
    if((gidC = H5Gcreate(gidA, "C", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidC) < 0)
        TEST_ERROR

    /* Create file #3 (it will have R/W permissions) */
    if((fid3 = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Mount file #3 on file #2 */
    if(H5Fmount(gidAM, ".", fid3, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Attempt to create objects in read/write file (should succeed) */
    if((gidAMZ = H5Gcreate(fid1, "/A/M/Z", (size_t)0)) < 0)
        TEST_ERROR

    /* Get and verify file name */
    if((name_len = H5Fget_name(gidAMZ, name, NAME_BUF_SIZE)) < 0)
        TEST_ERROR
    if(HDstrcmp(name, filename3) != 0)
        TEST_ERROR

    /* Close object in file #3 */
    if(H5Gclose(gidAMZ) < 0)
        TEST_ERROR


    /* Attempt to create objects in read only file again (should fail) */
    H5E_BEGIN_TRY {
        bad_id = H5Gcreate(fid1, "/A/L", (size_t)0);
    } H5E_END_TRY;
    if(bad_id >= 0)
        TEST_ERROR

    /* Close group in mounted file */
    if(H5Gclose(gidAM) < 0)
        TEST_ERROR

    /* Close group in parent file */
    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    /* Close file #3 */
    if(H5Fclose(fid3) < 0)
        TEST_ERROR

    /* Close file #2 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Close file #1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0)
        TEST_ERROR
    if(H5F_sfile_assert_num(0) != 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(gidM);
	H5Gclose(gidAMZ);
	H5Gclose(gidAM);
	H5Gclose(gidC);
	H5Gclose(gidB);
	H5Gclose(gidA);
        H5Fclose(fid3);
        H5Fclose(fid2);
	H5Fclose(fid1);
    } H5E_END_TRY;
    return 1;
} /* end test_acc_perm() */


/*-------------------------------------------------------------------------
 * Function:	test_mult_mount
 *
 * Purpose:	Test that the library correctly handles mounting a file
 *              multiple times at different locations in the parent file(s)
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 25, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_mult_mount(hid_t fapl)
{
    hid_t fid1 = -1, fid2 = -1, fid3 = -1, fid3_2 = -1;	/* File IDs */
    hid_t gidA = -1, gidB = -1;         /* Group IDs in file #1 */
    hid_t gidM = -1, gidN = -1, gidAM = -1;         /* Group IDs in file #2 */
    hid_t gidS = -1, gidT = -1, gidU = -1, gidBS = -1, gidAMT = -1;	/* Group IDs in file #3 */
    char    name[NAME_BUF_SIZE];        /* Buffer for filename retrieved */
    char	filename1[1024],
		filename2[1024],
		filename3[1024]; 	/* Name of files to mount */

    TESTING("multiple mounts");

    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);
    h5_fixname(FILENAME[2], fapl, filename3, sizeof filename3);

    /* Create file #1 */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gcreate(fid1, "A", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    if((gidB = H5Gcreate(fid1, "B", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidB) < 0)
        TEST_ERROR

    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Create file #2 */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidM = H5Gcreate(fid2, "M", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidM) < 0)
        TEST_ERROR

    if((gidN = H5Gcreate(fid2, "N", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidN) < 0)
        TEST_ERROR

    if(H5Fclose(fid2) < 0)
        TEST_ERROR


    /* Create file #3 */
    if((fid3 = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidS = H5Gcreate(fid3, "S", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidS) < 0)
        TEST_ERROR

    if((gidT = H5Gcreate(fid3, "T", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidT) < 0)
        TEST_ERROR

    if(H5Fclose(fid3) < 0)
        TEST_ERROR


    /* Re-open files and mount file #2 in file #1 */
    if((fid1 = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gopen(fid1, "A")) < 0)
        TEST_ERROR

    if((gidB = H5Gopen(fid1, "B")) < 0)
        TEST_ERROR

    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Mount files together */
    if(H5Fmount(gidA, ".", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Open group in mounted file */
    if((gidAM = H5Gopen(fid1, "A/M")) < 0)
        TEST_ERROR

    /* Open file #3 again */
    if((fid3 = H5Fopen(filename3, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Mount file #3 on file #2 */
    if(H5Fmount(gidAM, ".", fid3, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Re-open file #3 */
    if((fid3_2 = H5Freopen(fid3)) < 0)
        TEST_ERROR

    /* Mount file #3 on file #1 also */
    if(H5Fmount(gidB, ".", fid3_2, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Open object in file #3 through file #2 mount path */
    if((gidAMT = H5Gopen(fid1, "A/M/T")) < 0)
        TEST_ERROR

    /* Check name */
    if(H5Iget_name(gidAMT, name, (size_t)NAME_BUF_SIZE ) < 0)
        TEST_ERROR
    if(HDstrcmp(name, "/A/M/T"))
        TEST_ERROR

    /* Create object in file #3 */
    if((gidU = H5Gcreate(gidAMT, "U", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidU) < 0)
        TEST_ERROR

    /* Open object in file #3 through file #1 mount path */
    if((gidBS = H5Gopen(fid1, "B/S")) < 0)
        TEST_ERROR

    /* Check name */
    if(H5Iget_name(gidBS, name, (size_t)NAME_BUF_SIZE ) < 0)
        TEST_ERROR
    if(HDstrcmp(name, "/B/S"))
        TEST_ERROR

    /* Re-open object created in file #3 through file #1 mount path */
    if((gidU = H5Gopen(gidBS, "/B/T/U")) < 0)
        TEST_ERROR

    if(H5Gclose(gidU) < 0)
        TEST_ERROR

    /* Close groups in file #3 */
    if(H5Gclose(gidBS) < 0)
        TEST_ERROR
    if(H5Gclose(gidAMT) < 0)
        TEST_ERROR

    /* Close group in file #2 */
    if(H5Gclose(gidAM) < 0)
        TEST_ERROR

    /* Close groups in file #1 */
    if(H5Gclose(gidA) < 0)
        TEST_ERROR
    if(H5Gclose(gidB) < 0)
        TEST_ERROR

    /* Close file #3 IDs */
    if(H5Fclose(fid3) < 0)
        TEST_ERROR
    if(H5Fclose(fid3_2) < 0)
        TEST_ERROR

    /* Close file #2 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Close file #1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0)
        TEST_ERROR
    if(H5F_sfile_assert_num(0) != 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(gidU);
	H5Gclose(gidAMT);
	H5Gclose(gidT);
	H5Gclose(gidBS);
	H5Gclose(gidS);
	H5Gclose(gidAM);
	H5Gclose(gidN);
	H5Gclose(gidM);
	H5Gclose(gidB);
	H5Gclose(gidA);
        H5Fclose(fid3_2);
        H5Fclose(fid3);
        H5Fclose(fid2);
	H5Fclose(fid1);
    } H5E_END_TRY;
    return 1;
} /* end test_mult_mount() */


/*-------------------------------------------------------------------------
 * Function:	test_nested_survive
 *
 * Purpose:	Test that the library correctly handles unmounting & remounting
 *              a file with files mounted on it
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 25, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_nested_survive(hid_t fapl)
{
    hid_t fid1 = -1, fid2 = -1, fid3 = -1;	/* File IDs */
    hid_t gidA = -1;                    /* Group IDs in file #1 */
    hid_t gidM = -1, gidAM = -1;        /* Group IDs in file #2 */
    hid_t gidS = -1, gidMS = -1, gidAMS = -1;	/* Group IDs in file #3 */
    char    name[NAME_BUF_SIZE];        /* Buffer for filename retrieved */
    ssize_t name_len;                   /* Filename length */
    char	filename1[1024],
		filename2[1024],
		filename3[1024]; 	/* Name of files to mount */

    TESTING("nested mounts survive");

    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);
    h5_fixname(FILENAME[2], fapl, filename3, sizeof filename3);

    /* Create file #1 */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gcreate(fid1, "A", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Create file #2 */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidM = H5Gcreate(fid2, "M", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidM) < 0)
        TEST_ERROR

    if(H5Fclose(fid2) < 0)
        TEST_ERROR


    /* Create file #3 */
    if((fid3 = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidS = H5Gcreate(fid3, "S", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidS) < 0)
        TEST_ERROR

    if(H5Fclose(fid3) < 0)
        TEST_ERROR


    /* Re-open files and mount file #2 in file #1 */
    if((fid1 = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gopen(fid1, "A")) < 0)
        TEST_ERROR

    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Mount files together */
    if(H5Fmount(gidA, ".", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Open group in mounted file */
    if((gidAM = H5Gopen(fid1, "A/M")) < 0)
        TEST_ERROR

    /* Open file #3 again */
    if((fid3 = H5Fopen(filename3, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Mount file #3 on file #2 */
    if(H5Fmount(gidAM, ".", fid3, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Open object in file #3 through file #1 mount path */
    if((gidAMS = H5Gopen(fid1, "A/M/S")) < 0)
        TEST_ERROR

    /* Close group in file #3 */
    if(H5Gclose(gidAMS) < 0)
        TEST_ERROR

    /* Check name */
    if((name_len = H5Iget_name(gidAM, name, (size_t)NAME_BUF_SIZE )) < 0)
        TEST_ERROR
    if(name_len == 0 || HDstrcmp(name, "/A/M"))
        TEST_ERROR

    /* Unmount file #2 from file #1 */
    if(H5Funmount(gidA, ".") < 0)
        TEST_ERROR

    /* Check name (should be unavailable) */
    if((name_len = H5Iget_name(gidAM, name, (size_t)NAME_BUF_SIZE )) < 0)
        TEST_ERROR
    if(name_len != 0)
        TEST_ERROR

    /* Open object in file #3 through file #1 mount path (should fail) */
    H5E_BEGIN_TRY {
        gidAMS = H5Gopen(fid1, "A/M/S");
    } H5E_END_TRY;
    if(gidAMS >= 0)
        TEST_ERROR

    /* Open object in file #3 through file #2 mount path */
    if((gidMS = H5Gopen(fid2, "M/S")) < 0)
        TEST_ERROR

    /* Check name */
    if(H5Iget_name(gidMS, name, (size_t)NAME_BUF_SIZE ) < 0)
        TEST_ERROR
    if(HDstrcmp(name, "/M/S"))
        TEST_ERROR

    /* Close group in file #3 */
    if(H5Gclose(gidMS) < 0)
        TEST_ERROR

    /* Re-mount file #2 on file #1 */
    if(H5Fmount(gidA, ".", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Open object in file #3 through file #1 mount path again */
    if((gidAMS = H5Gopen(fid1, "A/M/S")) < 0)
        TEST_ERROR

    /* Check name */
    if(H5Iget_name(gidAMS, name, (size_t)NAME_BUF_SIZE ) < 0)
        TEST_ERROR
    if(HDstrcmp(name, "/A/M/S"))
        TEST_ERROR

    /* Close group in file #3 */
    if(H5Gclose(gidAMS) < 0)
        TEST_ERROR

    /* Close group in file #2 */
    if(H5Gclose(gidAM) < 0)
        TEST_ERROR

    /* Close groups in file #1 */
    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    /* Close file #3 IDs */
    if(H5Fclose(fid3) < 0)
        TEST_ERROR

    /* Close file #2 */
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Close file #1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0)
        TEST_ERROR
    if(H5F_sfile_assert_num(0) != 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(gidAMS);
	H5Gclose(gidMS);
	H5Gclose(gidS);
	H5Gclose(gidAM);
	H5Gclose(gidM);
	H5Gclose(gidA);
        H5Fclose(fid3);
        H5Fclose(fid2);
	H5Fclose(fid1);
    } H5E_END_TRY;
    return 1;
} /* end test_nested_survive() */


/*-------------------------------------------------------------------------
 * Function:	test_close_parent
 *
 * Purpose:	Test that the library correctly handles holding open a child
 *              file while unmounting & closing parent.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 25, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_close_parent(hid_t fapl)
{
    hid_t fid1 = -1, fid2 = -1;		/* File IDs */
    hid_t gidA = -1;                    /* Group IDs in file #1 */
    hid_t gidM = -1;                    /* Group IDs in file #2 */
    char    name[NAME_BUF_SIZE];        /* Buffer for filename retrieved */
    ssize_t name_len;                   /* Filename length */
    char	filename1[1024],
		filename2[1024]; 	/* Name of files to mount */

    TESTING("close parent");

    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Create file #1 */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gcreate(fid1, "A", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Create file #2 */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidM = H5Gcreate(fid2, "M", (size_t)0)) < 0)
        TEST_ERROR

    if(H5Gclose(gidM) < 0)
        TEST_ERROR

    if(H5Fclose(fid2) < 0)
        TEST_ERROR


    /* Re-open files and mount file #2 in file #1 */
    if((fid1 = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gopen(fid1, "A")) < 0)
        TEST_ERROR

    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Mount files together */
    if(H5Fmount(gidA, ".", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Open group in mounted file */
    if((gidM = H5Gopen(fid1, "A/M")) < 0)
        TEST_ERROR

    /* Close group in file #1 */
    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    /* Close files #1 & #2 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0)
        TEST_ERROR

    /* Both underlying shared files should be open still */
    if(H5F_sfile_assert_num(2) != 0)
        TEST_ERROR

    /* Check the name of "M" is still defined */
    if((name_len = H5Iget_name(gidM, name, (size_t)NAME_BUF_SIZE )) < 0)
        TEST_ERROR
    if(name_len == 0 || HDstrcmp(name, "/A/M"))
        TEST_ERROR

    /* Unmount file #2 from file #1, closing file #1 */
    if(H5Funmount(gidM, "/A") < 0)
        TEST_ERROR

    /* Check the name of "M" is not defined any longer */
    if((name_len = H5Iget_name(gidM, name, (size_t)NAME_BUF_SIZE )) < 0)
        TEST_ERROR
    if(name_len == 0 || HDstrcmp(name, "/M"))
        TEST_ERROR

    /* Just file #2's underlying shared file should be open still */
    if(H5F_sfile_assert_num(1) != 0)
        TEST_ERROR

    /* Close group in file #2, letting file #2 close */
    if(H5Gclose(gidM) < 0)
        TEST_ERROR

    /* All underlying shared file structs should be closed */
    if(H5F_sfile_assert_num(0) != 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(gidM);
	H5Gclose(gidA);
        H5Fclose(fid2);
	H5Fclose(fid1);
    } H5E_END_TRY;
    return 1;
} /* end test_close_parent() */


/*-------------------------------------------------------------------------
 * Function:	test_cut_graph
 *
 * Purpose:	Test that the library correctly handles cutting a graph of
 *              mounted files.
 *
 *              Initial file mounting graph is built up to this:
 *
 *                              [1]
 *                             /   \
 *                            /     \
 *                          "A"     "B"
 *                          /         \
 *                         /           \
 *                       [2]           [3]
 *                       /  \         /   \
 *                     "D"  "E"     "H"   "I"
 *                     /      \     /       \
 *                   [4]      [5] [6]       [7]
 *                    |        |   |         |
 *                   "K"      "M" "O"       "Q"
 *
 *              ( where [n] is a file in mounting hierarchy and "X" is a group
 *               in a file )
 *
 *              Objects in file #5 & file #7 are opened and all other
 *              file & object IDs to the hierarchy are closed.
 *
 *              Then file #2 is unmounted from file #1, which should make a
 *              small tree of files 2, 4 & 5 and a larger tree of files 1, 3,
 *              6 & 7.
 *
 *              Then, the object in file #5 is closed, which should release its
 *              small tree of files.
 *
 *              Then, file #3 is unmounted from file #1, making it's tree
 *              only 3, 6 & 7.
 *
 *              Then, the object in file #7 is closed, which should release the
 *              remaining small tree of files.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 25, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_cut_graph(hid_t fapl)
{
    hid_t fid1 = -1, fid2 = -1, fid3 = -1,
        fid4 = -1, fid5 = -1, fid6 = -1, fid7 = -1;	/* File IDs */
    hid_t gidA = -1, gidB = -1;         /* Group IDs in file #1 */
    hid_t gidD = -1, gidE = -1;         /* Group IDs in file #2 */
    hid_t gidH = -1, gidI = -1;         /* Group IDs in file #3 */
    hid_t gidK = -1;                    /* Group IDs in file #4 */
    hid_t gidM = -1;                    /* Group IDs in file #5 */
    hid_t gidO = -1;                    /* Group IDs in file #6 */
    hid_t gidQ = -1;                    /* Group IDs in file #7 */
    char    name[NAME_BUF_SIZE];        /* Buffer for filename retrieved */
    ssize_t name_len;                   /* Filename length */
    ssize_t obj_count;                  /* Number of objects open */
    char	filename1[NAME_BUF_SIZE],
		filename2[NAME_BUF_SIZE],
		filename3[NAME_BUF_SIZE],
		filename4[NAME_BUF_SIZE],
		filename5[NAME_BUF_SIZE],
		filename6[NAME_BUF_SIZE],
		filename7[NAME_BUF_SIZE]; 	/* Name of files to mount */

    TESTING("cutting mount graph");

    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);
    h5_fixname(FILENAME[2], fapl, filename3, sizeof filename3);
    h5_fixname(FILENAME[3], fapl, filename4, sizeof filename3);
    h5_fixname(FILENAME[4], fapl, filename5, sizeof filename3);
    h5_fixname(FILENAME[5], fapl, filename6, sizeof filename3);
    h5_fixname(FILENAME[6], fapl, filename7, sizeof filename3);

    /* Create file #1 & it's groups */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gcreate(fid1, "A", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    if((gidB = H5Gcreate(fid1, "B", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidB) < 0)
        TEST_ERROR

    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Create file #2 & it's groups */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidD = H5Gcreate(fid2, "D", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidD) < 0)
        TEST_ERROR

    if((gidE = H5Gcreate(fid2, "E", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidE) < 0)
        TEST_ERROR

    if(H5Fclose(fid2) < 0)
        TEST_ERROR


    /* Create file #3 & it's groups */
    if((fid3 = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidH = H5Gcreate(fid3, "H", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidH) < 0)
        TEST_ERROR

    if((gidI = H5Gcreate(fid3, "I", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidI) < 0)
        TEST_ERROR

    if(H5Fclose(fid3) < 0)
        TEST_ERROR

    /* Create file #4 & it's group */
    if((fid4 = H5Fcreate(filename4, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidK = H5Gcreate(fid4, "K", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidK) < 0)
        TEST_ERROR

    if(H5Fclose(fid4) < 0)
        TEST_ERROR

    /* Create file #5 & it's group */
    if((fid5 = H5Fcreate(filename5, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidM = H5Gcreate(fid5, "M", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidM) < 0)
        TEST_ERROR

    if(H5Fclose(fid5) < 0)
        TEST_ERROR

    /* Create file #6 & it's group */
    if((fid6 = H5Fcreate(filename6, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidO = H5Gcreate(fid6, "O", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidO) < 0)
        TEST_ERROR

    if(H5Fclose(fid6) < 0)
        TEST_ERROR

    /* Create file #7 & it's group */
    if((fid7 = H5Fcreate(filename7, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidQ = H5Gcreate(fid7, "Q", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidQ) < 0)
        TEST_ERROR

    if(H5Fclose(fid7) < 0)
        TEST_ERROR


    /* Re-open files and build mount hierarchy */
    if((fid1 = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Mount file #2 at /A */
    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Fmount(fid1, "/A", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Mount file #3 at /B */
    if((fid3 = H5Fopen(filename3, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Fmount(fid1, "/B", fid3, H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Fclose(fid3) < 0)
        TEST_ERROR

    /* Mount file #4 at /A/D */
    if((fid4 = H5Fopen(filename4, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Fmount(fid1, "/A/D", fid4, H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Fclose(fid4) < 0)
        TEST_ERROR

    /* Mount file #5 at /A/E */
    if((fid5 = H5Fopen(filename5, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Fmount(fid1, "/A/E", fid5, H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Fclose(fid5) < 0)
        TEST_ERROR

    /* Mount file #6 at /B/H */
    if((fid6 = H5Fopen(filename6, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Fmount(fid1, "/B/H", fid6, H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Fclose(fid6) < 0)
        TEST_ERROR

    /* Mount file #7 at /B/H */
    if((fid7 = H5Fopen(filename7, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Fmount(fid1, "/B/I", fid7, H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Fclose(fid7) < 0)
        TEST_ERROR

    /* Open object in file #5 */
    if((gidM = H5Gopen(fid1, "A/E/M")) < 0)
        TEST_ERROR

    /* Check name */
    if((name_len = H5Iget_name(gidM, name, (size_t)NAME_BUF_SIZE )) < 0)
        TEST_ERROR
    if(name_len == 0 || HDstrcmp(name, "/A/E/M"))
        TEST_ERROR

    /* Open object in file #7 */
    if((gidQ = H5Gopen(fid1, "B/I/Q")) < 0)
        TEST_ERROR

    /* Check name */
    if((name_len = H5Iget_name(gidQ, name, (size_t)NAME_BUF_SIZE )) < 0)
        TEST_ERROR
    if(name_len == 0 || HDstrcmp(name, "/B/I/Q"))
        TEST_ERROR

    /* Close file #1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Get a new file ID for file #5 */
    if((fid5 = H5Iget_file_id(gidM)) < 0)
        TEST_ERROR

    /* Check the number of objects currently open */
    if((obj_count = H5Fget_obj_count(fid5, H5F_OBJ_ALL)) < 0)
        TEST_ERROR
    if(obj_count != 2)  /* one object and the file ID */
        TEST_ERROR

    /* Close ID on file #5 */
    if(H5Fclose(fid5) < 0)
        TEST_ERROR


    /* Get a new file ID for file #7 */
    if((fid7 = H5Iget_file_id(gidQ)) < 0)
        TEST_ERROR

    /* Check the number of objects currently open */
    if((obj_count = H5Fget_obj_count(fid7, H5F_OBJ_ALL)) < 0)
        TEST_ERROR
    if(obj_count != 2)  /* one object and the file ID */
        TEST_ERROR

    /* Close ID on file #7 */
    if(H5Fclose(fid7) < 0)
        TEST_ERROR

    /* Check that all file IDs have been closed */
    if(H5I_nmembers(H5I_FILE) != 0)
        TEST_ERROR


    /* Unmount file #2 from file #1, cutting the graph */
    if(H5Funmount(gidM, "/A") < 0)
        TEST_ERROR

    /* Attempt to open an object in file #4, from file #1 */
    H5E_BEGIN_TRY {
        gidK = H5Gopen(gidQ, "/A/D/K");
    } H5E_END_TRY;
    if(gidK >= 0)
        TEST_ERROR

    /* Open object in file #4 from file #5 */
    if((gidK = H5Gopen(gidM, "/D/K")) < 0)
        TEST_ERROR

    /* Check the name of "K" is correct */
    if((name_len = H5Iget_name(gidK, name, (size_t)NAME_BUF_SIZE )) < 0)
        TEST_ERROR
    if(name_len == 0 || HDstrcmp(name, "/D/K"))
        TEST_ERROR

    if(H5Gclose(gidK) < 0)
        TEST_ERROR

    /* Attempt to open an object in file #6, from file #5 */
    H5E_BEGIN_TRY {
        gidO = H5Gopen(gidM, "/B/H/O");
    } H5E_END_TRY;
    if(gidO >= 0)
        TEST_ERROR

    /* Open object in file #6 from file #7 */
    if((gidO = H5Gopen(gidQ, "/B/H/O")) < 0)
        TEST_ERROR

    /* Check the name of "O" is correct */
    if((name_len = H5Iget_name(gidO, name, (size_t)NAME_BUF_SIZE )) < 0)
        TEST_ERROR
    if(name_len == 0 || HDstrcmp(name, "/B/H/O"))
        TEST_ERROR

    if(H5Gclose(gidO) < 0)
        TEST_ERROR

    /* Check the name of "M" is not defined any longer */
    if((name_len = H5Iget_name(gidM, name, (size_t)NAME_BUF_SIZE )) < 0)
        TEST_ERROR
    if(name_len != 0)
        TEST_ERROR

    /* Check the name of "Q" is still defined */
    if((name_len = H5Iget_name(gidQ, name, (size_t)NAME_BUF_SIZE )) < 0)
        TEST_ERROR
    if(name_len == 0 || HDstrcmp(name, "/B/I/Q"))
        TEST_ERROR

    /* Check that all seven underlying files are still opened */
    if(H5F_sfile_assert_num(7) != 0)
        TEST_ERROR

    /* Close "M" in file #5, which should close files 2, 4 & 5 */
    if(H5Gclose(gidM) < 0)
        TEST_ERROR

    /* Check that only four underlying files are still opened */
    if(H5F_sfile_assert_num(4) != 0)
        TEST_ERROR

    /* Unmount file #3 from file #1, cutting the graph */
    if(H5Funmount(gidQ, "/B") < 0)
        TEST_ERROR

    /* Check that only three underlying files are still opened */
    /* (File #1 should close after being cut off from the graph) */
    if(H5F_sfile_assert_num(3) != 0)
        TEST_ERROR

    /* Check the name of "Q" is not defined any longer */
    if((name_len = H5Iget_name(gidQ, name, (size_t)NAME_BUF_SIZE )) < 0)
        TEST_ERROR
    if(name_len != 0)
        TEST_ERROR

    /* Open object in file #6 from file #7 */
    if((gidO = H5Gopen(gidQ, "/H/O")) < 0)
        TEST_ERROR

    /* Check the name of "O" is correct */
    if((name_len = H5Iget_name(gidO, name, (size_t)NAME_BUF_SIZE )) < 0)
        TEST_ERROR
    if(name_len == 0 || HDstrcmp(name, "/H/O"))
        TEST_ERROR

    if(H5Gclose(gidO) < 0)
        TEST_ERROR

    /* Close last object and let files 3, 6 & 7 close */
    if(H5Gclose(gidQ) < 0)
        TEST_ERROR

    /* Verify that all underlying shared files have been closed now */
    if(H5F_sfile_assert_num(0) != 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(gidQ);
	H5Gclose(gidO);
	H5Gclose(gidM);
	H5Gclose(gidK);
	H5Gclose(gidI);
	H5Gclose(gidH);
	H5Gclose(gidE);
	H5Gclose(gidD);
	H5Gclose(gidB);
	H5Gclose(gidA);
        H5Fclose(fid7);
        H5Fclose(fid6);
        H5Fclose(fid5);
        H5Fclose(fid4);
        H5Fclose(fid3);
        H5Fclose(fid2);
	H5Fclose(fid1);
    } H5E_END_TRY;
    return 1;
} /* end test_cut_graph() */


/*-------------------------------------------------------------------------
 * Function:	test_symlink
 *
 * Purpose:	Test that the library correctly handles symlinks across
 *              mounted files.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 25, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_symlink(hid_t fapl)
{
    hid_t fid1 = -1, fid2 = -1, fid3 = -1; /* File IDs */
    hid_t gidA = -1, gidB = -1;         /* Group IDs in file #1 */
    hid_t gidD = -1, gidE = -1;         /* Group IDs in file #2 */
    hid_t gidH = -1, gidI = -1;         /* Group IDs in file #3 */
    hid_t gidL = -1;         		/* Group IDs through soft link to file #3 */
    char    name[NAME_BUF_SIZE];        /* Buffer for filename retrieved */
    ssize_t name_len;                   /* Filename length */
    char	filename1[NAME_BUF_SIZE],
		filename2[NAME_BUF_SIZE],
		filename3[NAME_BUF_SIZE]; 	/* Name of files to mount */

    TESTING("symlinks");

    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);
    h5_fixname(FILENAME[2], fapl, filename3, sizeof filename3);

    /* Create file #1 & it's groups */
    if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidA = H5Gcreate(fid1, "A", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidA) < 0)
        TEST_ERROR

    if((gidB = H5Gcreate(fid1, "B", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidB) < 0)
        TEST_ERROR

    /* Create soft link to mounted object */
    if(H5Glink(fid1, H5G_LINK_SOFT, "./A/D/H", "L") < 0) /* Soft link */
        TEST_ERROR

    if(H5Fclose(fid1) < 0)
        TEST_ERROR


    /* Create file #2 & it's groups */
    if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidD = H5Gcreate(fid2, "D", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidD) < 0)
        TEST_ERROR

    if((gidE = H5Gcreate(fid2, "E", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidE) < 0)
        TEST_ERROR

    if(H5Fclose(fid2) < 0)
        TEST_ERROR


    /* Create file #3 & it's groups */
    if((fid3 = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((gidH = H5Gcreate(fid3, "H", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidH) < 0)
        TEST_ERROR

    if((gidI = H5Gcreate(fid3, "I", (size_t)0)) < 0)
        TEST_ERROR
    if(H5Gclose(gidI) < 0)
        TEST_ERROR

    if(H5Fclose(fid3) < 0)
        TEST_ERROR


    /* Re-open files and build mount hierarchy */
    if((fid1 = H5Fopen(filename1, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Mount file #2 at /A */
    if((fid2 = H5Fopen(filename2, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Fmount(fid1, "/A", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    /* Attempt to open an object in file #3 (should fail) */
    H5E_BEGIN_TRY {
        gidL = H5Gopen(fid1, "L");
    } H5E_END_TRY;
    if(gidL >= 0)
        TEST_ERROR

    /* Mount file #3 at /A/D */
    if((fid3 = H5Fopen(filename3, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Fmount(fid1, "/A/D", fid3, H5P_DEFAULT) < 0)
        TEST_ERROR
    if(H5Fclose(fid3) < 0)
        TEST_ERROR

    /* Open soft link to object in file #3 */
    if((gidL = H5Gopen(fid1, "L")) < 0)
        TEST_ERROR

    /* Check the name of "L" is correct */
    if((name_len = H5Iget_name(gidL, name, (size_t)NAME_BUF_SIZE )) < 0)
        TEST_ERROR
    if(name_len == 0 || HDstrcmp(name, "/L"))
        TEST_ERROR

    /* Close file #1 */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR

    /* Verify that all 3 underlying shared files are still open */
    if(H5F_sfile_assert_num(3) != 0)
        TEST_ERROR

    /* Close object opened through soft link */
    if(H5Gclose(gidL) < 0)
        TEST_ERROR

    /* Verify that all underlying shared files have been closed now */
    if(H5F_sfile_assert_num(0) != 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Gclose(gidL);
	H5Gclose(gidI);
	H5Gclose(gidH);
	H5Gclose(gidE);
	H5Gclose(gidD);
	H5Gclose(gidB);
	H5Gclose(gidA);
        H5Fclose(fid3);
        H5Fclose(fid2);
	H5Fclose(fid1);
    } H5E_END_TRY;
    return 1;
} /* end test_symlink() */


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
    hid_t	fapl = -1;

    h5_reset();
    fapl = h5_fileaccess();
    if (setup(fapl)<0) goto error;

    nerrors += test_basic(fapl);
    nerrors += test_illegal(fapl);
    nerrors += test_hide(fapl);
    nerrors += test_assoc(fapl);
    nerrors += test_mntlnk(fapl);
    nerrors += test_unlink(fapl);
    nerrors += test_move(fapl);
    nerrors += test_mvmpt(fapl);
    nerrors += test_preopen(fapl);
    nerrors += test_postopen(fapl);
    nerrors += test_interlink(fapl);
    nerrors += test_uniformity(fapl);
    nerrors += test_close(fapl);
    nerrors += test_mount_after_close(fapl);
    nerrors += test_mount_after_unmount(fapl);
    nerrors += test_missing_unmount(fapl);
    nerrors += test_hold_open_file(fapl);
    nerrors += test_hold_open_group(fapl);
    nerrors += test_fcdegree_same(fapl);
    nerrors += test_fcdegree_semi(fapl);
    nerrors += test_fcdegree_strong(fapl);
    nerrors += test_acc_perm(fapl);
    nerrors += test_mult_mount(fapl);
    nerrors += test_nested_survive(fapl);
    nerrors += test_close_parent(fapl);
    nerrors += test_cut_graph(fapl);
    nerrors += test_symlink(fapl);

    if (nerrors) goto error;
    puts("All mount tests passed.");
    h5_cleanup(FILENAME, fapl);
    return 0;

 error:
    puts("***** MOUNT ERRORS *****");
    return 1;
}
