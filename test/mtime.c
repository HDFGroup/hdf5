/*
 * Copyright © 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, July 30, 1998
 *
 * Purpose:	Determines if the modification time message is working
 *		properly.  Specifically, the code in H5O_mtime_decode() is
 *		very OS-dependent and this test tries to figure out if it's
 *		working properly.
 */
#include <assert.h>
#include <hdf5.h>
#include <math.h>
#include <sys/time.h>
#include <time.h>

#define FALSE		0
#define TRUE		1
#define FILE_NAME_1	"mtime.h5"

#include <H5config.h>
#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif


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
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	file, space, dset;
    hsize_t	size[1] = {2};
    time_t	now;
    struct tm	*tm;
    H5G_stat_t	sb;
    char	buf1[32], buf2[32];

    H5Eset_auto(display_error_cb, NULL);
    printf("%-70s", "Testing modification time messages");
    
    /* Create the file, create a dataset, then close the file */
    file = H5Fcreate(FILE_NAME_1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (file<0) return 1;
    if ((space = H5Screate_simple(1, size, NULL))<0) return 1;
    dset = H5Dcreate(file, "dset", H5T_NATIVE_CHAR, space, H5P_DEFAULT);
    if (dset<0) return 1;
    now = time(NULL);
    if (H5Dclose(dset)<0) return 1;
    if (H5Sclose(space)<0) return 1;
    if (H5Fclose(file)<0) return 1;

    /* Open the file and get the modification time */
    if ((file = H5Fopen(FILE_NAME_1, H5F_ACC_RDONLY, H5P_DEFAULT))<0) return 1;
    if (H5Gstat(file, "dset", TRUE, &sb)<0) return 1;
    if (H5Fclose(file)<0) return 1;

    /* Compare times -- they must be within 60 seconds of one another */
    if (0==sb.mtime) {
	puts("--SKIP--");
	puts("   The modification time could not be decoded on this OS.");
	puts("   Modification times will be mantained in the file bug cannot");
	puts("   be queried on this system.  See H5O_mtime_decode().");
	return 1;
    } else if (fabs(difftime(now, sb.mtime))>60.0) {
	puts("*FAILED*");
	tm = localtime(&(sb.mtime));
	strftime(buf1, sizeof buf1, "%Y-%m-%d %H:%M:%S", tm);
	tm = localtime(&now);
	strftime(buf2, sizeof buf2, "%Y-%m-%d %H:%M:%S", tm);
	printf("   Got %s instead of %s\n", buf1, buf2);
	return 1;
    }
    
    /* All looks good */
    puts(" PASSED");
    cleanup();
    return 0;
}

    
