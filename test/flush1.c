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

/* See H5private.h for how to include headers */
#undef NDEBUG
#include <hdf5.h>

#ifdef STDC_HEADERS
#   include <stdio.h>
#   include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#   include <sys/types.h>
#   include <unistd.h>
#endif

#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif


#define FILE_NAME_1	"flush.h5"	/*do not clean up*/



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
    hid_t	file, dcpl, space, dset, groups, grp;
    hsize_t	ds_size[2] = {100, 100};
    hsize_t	ch_size[2] = {5, 5};
    double	the_data[100][100];
    hsize_t	i, j;
    char	name[256];

    printf("%-70s", "Testing H5Fflush (part1)");
    fflush(stdout);
    H5Eset_auto(display_error_cb, NULL);

    /* Create the file */
    if ((file=H5Fcreate(FILE_NAME_1, H5F_ACC_TRUNC,
			H5P_DEFAULT, H5P_DEFAULT))<0) goto error;
    
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
	    the_data[i][j] = (double)(hssize_t)i/((hssize_t)(j+1));
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
    puts(" PASSED");
    fflush(stdout);
    fflush(stderr);
    _exit(0);

 error:
    printf("*FAILED*");
    return 1;
}

    
    
	
