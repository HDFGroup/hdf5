/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, October 23, 1998
 *
 * Purpose:	This is the first half of a two-part test that makes sure
 *		that a file can be read after an application crashes as long
 *		as the file was flushed first.  We simulate a crash by
 *		calling _exit(0) since this doesn't flush HDF5 caches but
 *		still exits with success.
 */
#include <h5test.h>

const char *FILENAME[] = {
    "flush",
    NULL
};


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
    hid_t	fapl, file, dcpl, space, dset, groups, grp;
    hsize_t	ds_size[2] = {100, 100};
    hsize_t	ch_size[2] = {5, 5};
    double	the_data[100][100];
    hsize_t	i, j;
    char	name[1024];

    h5_reset();
    fapl = h5_fileaccess();

    TESTING("H5Fflush (part1)");

    /* Create the file */
    h5_fixname(FILENAME[0], fapl, name, sizeof name);
    if ((file=H5Fcreate(name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) goto error;
    
    /* Create a chunked dataset */
    if ((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if (H5Pset_chunk(dcpl, 2, ch_size)<0) goto error;
    if ((space=H5Screate_simple(2, ds_size, NULL))<0) goto error;
    if ((dset=H5Dcreate(file, "dset", H5T_NATIVE_FLOAT, space, H5P_DEFAULT))<0)
	goto error;

    /* Write some data */
    for (i=0; i<ds_size[0]; i++) {
	/*
	 * The extra cast in the following statement is a bug workaround
	 * for the Win32 version 5.0 compiler.
	 * 1998-11-06 ptl
	 */
	for (j=0; j<ds_size[1]; j++) {
	    the_data[i][j] = (double)(hssize_t)i/(hssize_t)(j+1);
	}
    }
    if (H5Dwrite(dset, H5T_NATIVE_DOUBLE, space, space, H5P_DEFAULT,
		 the_data)<0) goto error;

    /* Create some groups */
    if ((groups=H5Gcreate(file, "some_groups", 0))<0) goto error;
    for (i=0; i<100; i++) {
	sprintf(name, "grp%02u", (unsigned)i);
	if ((grp=H5Gcreate(groups, name, 0))<0) goto error;
	if (H5Gclose(grp)<0) goto error;
    }

    /* Flush and exit without closing the library */
    if (H5Fflush(file, H5F_SCOPE_GLOBAL)<0) goto error;
    PASSED();
    fflush(stdout);
    fflush(stderr);
    _exit(0);

 error:
    _exit(1);
}

    
    
	
