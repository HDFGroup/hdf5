/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, October 23, 1998
 *
 * Purpose:	This is the second half of a two-part test that makes sure
 *		that a file can be read after an application crashes as long
 *		as the file was flushed first.  This half tries to read the
 *		file created by the first half.
 */

/* See H5private.h for how to include headers */
#undef NDEBUG
#include <H5config.h>

#ifdef STDC_HEADERS
#   include <assert.h>
#   include <math.h>
#   include <stdio.h>
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


#define FILE_NAME_1	"flush.h5"



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
    hid_t	file, space, dset, groups, grp;
    hsize_t	ds_size[2];
    double	the_data[100][100], error;
    hsize_t	i, j;
    char	name[256];

    printf("%-70s", "Testing H5Fflush (part2)");
    fflush(stdout);
    H5Eset_auto(display_error_cb, NULL);

    /* Open the file */
    if ((file=H5Fopen(FILE_NAME_1, H5F_ACC_RDONLY, H5P_DEFAULT))<0) goto error;
    
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
	    error = fabs(the_data[i][j]-(double)((hssize_t)(i/(j+1))));
	    assert(error<0.0001);
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
    puts(" PASSED");
    cleanup();
    return 0;

 error:
    printf("*FAILED*");
    return 1;
}

    
    
	
