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

#define TEST_FILE_NAME	"links.h5"


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
    remove(TEST_FILE_NAME);
}

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests links.
 *
 * Return:	Success:	0
 *
 *		Failure:	non-zero
 *
 * Programmer:	Robb Matzke
 *              Friday, April 10, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (void)
{
    hid_t	file, scalar, grp, d1;
    hsize_t	size[1] = {1};

    /* Create a file */
    file = H5Fcreate (TEST_FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    scalar = H5Screate_simple (1, size, size);

    /* Create a group */
    grp = H5Gcreate (file, "grp1", 0);
    H5Gclose (grp);

    /* Create a dataset */
    d1 = H5Dcreate (file, "d1", H5T_NATIVE_INT, scalar, H5P_DEFAULT);
    H5Dclose (d1);

    /* Create a hard link */
    H5Glink (file, H5G_LINK_HARD, "d1", "grp1/hard");

    /* Create a symbolic link */
    H5Glink (file, H5G_LINK_SOFT, "/d1", "grp1/soft");

    /* Create a symbolic link to something that doesn't exist */
    H5Glink (file, H5G_LINK_SOFT, "foobar", "grp1/dangle");

    /* Create a recursive symbolic link */
    H5Glink (file, H5G_LINK_SOFT, "/grp1/recursive", "/grp1/recursive");

    /* Close */
    H5Sclose (scalar);
    H5Fclose (file);
    cleanup();
    return 0;
}
