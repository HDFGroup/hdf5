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
 *              Friday, October 23, 1998
 *
 * Purpose:	This is the second half of a two-part test that makes sure
 *		that a file can be read after an application crashes as long
 *		as the file was flushed first.  This half tries to read the
 *		file created by the first half.
 */
#include "h5test.h"

const char *FILENAME[] = {
    "flush",
    NULL
};

static double	the_data[100][100];


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Part 1 of a two-part H5Fflush() test.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Robb Matzke
 *              Friday, October 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl, file, space, dset, groups, grp;
    hsize_t	ds_size[2];
    double	error;
    size_t	i, j;
    char	name[1024];

    h5_reset();
    fapl = h5_fileaccess();
    TESTING("H5Fflush (part2)");

    /* Open the file */
    h5_fixname(FILENAME[0], fapl, name, sizeof name);
    if ((file=H5Fopen(name, H5F_ACC_RDONLY, fapl))<0) goto error;

    /* Open the dataset */
    if ((dset=H5Dopen(file, "dset"))<0) goto error;
    if ((space=H5Dget_space(dset))<0) goto error;
    if (H5Sget_simple_extent_dims(space, ds_size, NULL)<0) goto error;
    assert(100==ds_size[0] && 100==ds_size[1]);

    /* Read some data */
    if (H5Dread(dset, H5T_NATIVE_DOUBLE, space, space, H5P_DEFAULT,
		the_data)<0) goto error;
    for (i=0; i<ds_size[0]; i++) {
	for (j=0; j<ds_size[1]; j++) {
	    /*
	     * The extra cast in the following statement is a bug workaround
	     * for the Win32 version 5.0 compiler.
	     * 1998-11-06 ptl
	     */
	    error = fabs(the_data[i][j]-(double)(hssize_t)i/((hssize_t)j+1));
	    if (error>0.0001) {
		H5_FAILED();
		printf("    dset[%lu][%lu] = %g\n",
		       (unsigned long)i, (unsigned long)j, the_data[i][j]);
		printf("    should be %g\n",
		       (double)(hssize_t)i/(hssize_t)(j+1));
		goto error;
	    }
	}
    }

    /* Open some groups */
    if ((groups=H5Gopen(file, "some_groups"))<0) goto error;
    for (i=0; i<100; i++) {
	sprintf(name, "grp%02u", (unsigned)i);
	if ((grp=H5Gopen(groups, name))<0) goto error;
	if (H5Gclose(grp)<0) goto error;
    }

    if (H5Gclose(groups)<0) goto error;
    if (H5Dclose(dset)<0) goto error;
    if (H5Fclose(file)<0) goto error;
    PASSED();
    h5_cleanup(FILENAME, fapl);
    return 0;

 error:
    return 1;
}




