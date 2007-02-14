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
 *              Thursday, July 30, 1998
 *
 * Purpose:	Determines if the modification time message is working
 *		properly.  Specifically, the code in H5O_mtime_decode() is
 *		very OS-dependent and this test tries to figure out if it's
 *		working properly.
 */
#include "h5test.h"

const char *FILENAME[] = {
    "mtime",
    NULL
};

#define TESTFILE1       "tmtimeo.h5"
#define MTIME1          1055531866
#define TESTFILE2       "tmtimen.h5"
#define MTIME2          1041606478


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	H5O_mtime_decode() test.
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 30, 1998
 *
 * Modifications:
 *              Added checks for old and new modification time messages
 *              in pre-created datafiles (generated with gen_old_mtime.c and
 *              gen_new_mtime.c).
 *              Quincey Koziol
 *              Friday, January  3, 2003
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl, file, space, dset;
    hsize_t	size[1] = {2};
    time_t	now;
    struct tm	*tm;
    H5G_stat_t	sb1, sb2;
    signed char	buf1[32], buf2[32];
    char	filename[1024];

    h5_reset();
    fapl = h5_fileaccess();

    TESTING("modification time messages");

    /* Create the file, create a dataset, then close the file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	TEST_ERROR;
    if ((space=H5Screate_simple(1, size, NULL))<0) TEST_ERROR;
    if ((dset=H5Dcreate(file, "dset", H5T_NATIVE_SCHAR, space, H5P_DEFAULT))<0)
	TEST_ERROR;
    now = time(NULL);
    if (H5Dclose(dset)<0) TEST_ERROR;
    if (H5Sclose(space)<0) TEST_ERROR;
    if (H5Fclose(file)<0) TEST_ERROR;

    /*
     * Open the file and get the modification time. We'll test the new
     * H5Gget_objinfo() arguments too: being able to stat something without
     * knowing its name.
     */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl))<0) TEST_ERROR;
    if (H5Gget_objinfo(file, "dset", TRUE, &sb1)<0) TEST_ERROR;
    if ((dset=H5Dopen(file, "dset"))<0) TEST_ERROR;
    if (H5Gget_objinfo(dset, ".", TRUE, &sb2)<0) TEST_ERROR;
    if (H5Dclose(dset)<0) TEST_ERROR;
    if (H5Fclose(file)<0) TEST_ERROR;

    /* Compare times from the two ways of calling H5Gget_objinfo() */
    if (sb1.objno[0]!=sb2.objno[0] || sb1.objno[1]!=sb2.objno[1] ||
	sb1.mtime!=sb2.mtime) {
        H5_FAILED();
	puts("    Calling H5Gget_objinfo() with the dataset ID returned");
	puts("    different values than calling it with a file and dataset");
	puts("    name.");
	goto error;
    }

    /* Compare times -- they must be within 60 seconds of one another */
    if (0==sb1.mtime) {
	SKIPPED();
	puts("    The modification time could not be decoded on this OS.");
	puts("    Modification times will be mantained in the file but");
	puts("    cannot be queried on this system.  See H5O_mtime_decode().");
	return 0;
    } else if (fabs(HDdifftime(now, sb1.mtime))>60.0) {
        H5_FAILED();
	tm = localtime(&(sb1.mtime));
	strftime((char*)buf1, sizeof buf1, "%Y-%m-%d %H:%M:%S", tm);
	tm = localtime(&now);
	strftime((char*)buf2, sizeof buf2, "%Y-%m-%d %H:%M:%S", tm);
	printf("    got: %s\n    ans: %s\n", buf1, buf2);
	goto error;
    }
    PASSED();

    /* Check opening existing file with old-style modification time information
     * and make certain that the time is correct
     */
    TESTING("accessing old modification time messages");

    {
    char testfile[512]="";
    char *srcdir = HDgetenv("srcdir");
    if (srcdir && ((HDstrlen(srcdir) + strlen(TESTFILE1) + 1) < sizeof(testfile))){
	HDstrcpy(testfile, srcdir);
	HDstrcat(testfile, "/");
    }
    HDstrcat(testfile, TESTFILE1);
    file = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT);
    if (file >= 0){
        if(H5Gget_objinfo(file, "/Dataset1", TRUE, &sb1)<0)
            TEST_ERROR;
        if(sb1.mtime!=MTIME1) {
            H5_FAILED();
               /* If this fails, examine H5Omtime.c.  Modification time is very
                * system dependant (e.g., on Windows DST must be hardcoded). */
            puts("    Old modification time incorrect");
            goto error;
        }
        if (H5Fclose(file)<0) TEST_ERROR;
    }
    else {
        H5_FAILED();
	printf("***cannot open the pre-created old modification test file (%s)\n",
	    testfile);
	goto error;
    } /* end else */
    }
    PASSED();

    /* Check opening existing file with new-style modification time information
     * and make certain that the time is correct
     */
    TESTING("accessing new modification time messages");

    {
    char testfile[512]="";
    char *srcdir = HDgetenv("srcdir");
    if (srcdir && ((HDstrlen(srcdir) + strlen(TESTFILE2) + 1) < sizeof(testfile))){
	HDstrcpy(testfile, srcdir);
	HDstrcat(testfile, "/");
    }
    HDstrcat(testfile, TESTFILE2);
    file = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT);
    if (file >= 0){
        if(H5Gget_objinfo(file, "/Dataset1", TRUE, &sb2)<0)
            TEST_ERROR;
        if(sb2.mtime!=MTIME2) {
           H5_FAILED();
           puts("    Modification time incorrect.");
           goto error;
        }
        if (H5Fclose(file)<0) TEST_ERROR;
    }
    else {
        H5_FAILED();
	printf("***cannot open the pre-created old modification test file (%s)\n",
	    testfile);
	goto error;
    } /* end else */
    }
    PASSED();

    /* All looks good */
    puts("All modification time tests passed.");
    h5_cleanup(FILENAME, fapl);
    return 0;

    /* Something broke */
    error:
    return 1;
}



