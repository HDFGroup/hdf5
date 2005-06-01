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
 *              Wednesday, October  7, 1998
 *
 * Purpose:	Tests file mounting.
 */
#include "h5test.h"

const char *FILENAME[] = {
    "mount_1",
    "mount_2",
    "mount_3",
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

    if ((file1=H5Fopen(filename1, H5F_ACC_RDONLY, fapl))<0 ||
	(file2=H5Fopen(filename2, H5F_ACC_RDONLY, fapl))<0)
	goto error;
    if (H5Fmount(file1, "/mnt1", file2, H5P_DEFAULT)<0) goto error;
    if ((grp=H5Gopen(file1, "/mnt1/file2"))<0) goto error;
    if (H5Gclose(grp)<0) goto error;
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
    herr_t	status;
    
    TESTING("file handle close");
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);
    
    /* Build the virtual file */
    if ((file1=H5Fopen(filename1, H5F_ACC_RDWR, fapl))<0 ||
	(file2=H5Fopen(filename2, H5F_ACC_RDWR, fapl))<0)
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
	H5_FAILED();
	puts("    File1 contents are still accessible!");
	goto error;
    }
    if (H5Fclose(file2)<0) goto error;

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
    
    if (nerrors) goto error;
    puts("All mount tests passed.");
    h5_cleanup(FILENAME, fapl);
    return 0;
    
 error:
    puts("***** MOUNT ERRORS *****");
    return 1;
}
