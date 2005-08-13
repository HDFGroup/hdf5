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
 *              Thursday, June 11, 1998
 *
 * Purpose:	Create an hdf5 file with a 1d dataset of uint8.
 */

/* See H5private.h for how to include system headers */
#include <hdf5.h>
#ifdef H5_STDC_HEADERS
#   include <fcntl.h>
#   include <string.h>
#   include <stdlib.h>
#   include <stdio.h>
#endif

#ifdef H5_HAVE_UNISTD_H
#   include <sys/types.h>
#   include <unistd.h>
#endif

#ifdef H5_HAVE_SYS_STAT_H
#   include <sys/stat.h>
#endif

#ifdef WIN32
#   include <io.h>
#endif


/*-------------------------------------------------------------------------
 * Function:	usage
 *
 * Purpose:	Print a usage message and exit with non-zero status
 *
 * Return:	never returns
 *
 * Programmer:	Robb Matzke
 *              Thursday, June 11, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage (const char *argv0)
{
    fprintf (stderr, "Usage: %s -f HDF5-FILE FILES...\n", argv0);
    exit (1);
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Robb Matzke
 *              Thursday, June 11, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (int argc, char *argv[])
{
    hid_t	file, space=-1, dset=-1;
    const char	*output_name, *dset_name;
    int		argno, fd=-1;
    hsize_t	size[1];
    struct stat sb;

    /* Parse arguments */
    if (argc<4) usage (argv[0]);
    if (strcmp (argv[1], "-f")) usage (argv[0]);
    output_name = argv[2];

    /* create the file */
    H5E_BEGIN_TRY {
	if ((file = H5Fcreate (output_name, H5F_ACC_EXCL,
			       H5P_DEFAULT, H5P_DEFAULT))<0 &&
	    (file = H5Fopen (output_name, H5F_ACC_RDWR, H5P_DEFAULT)<0)) {
	    fprintf (stderr, "%s: unable to create or open hdf5 file\n",
		     output_name);
	    exit (1);
	}
    } H5E_END_TRY;

    /* process files from command-line */
    for (argno=3;  argno<argc; argno++) {

	/* Open the file */
	if ((dset_name=strrchr (argv[argno], '/'))) dset_name++;
	else dset_name = argv[argno];
	fprintf (stderr, "%s\n", dset_name);
	if ((fd=open (argv[argno], O_RDONLY))<0) {
	    perror (argv[argno]);
	    goto next;
	}
	if (fstat (fd, &sb)<0) {
	    perror (argv[argno]);
	    goto next;
	}

	/* Data space */
	size[0] = sb.st_size;
	if ((space = H5Screate_simple (1, size, size))<0) goto next;

	/* Dataset */
	if ((dset=H5Dcreate (file, dset_name, H5T_NATIVE_SCHAR,
			     space, H5P_DEFAULT))<0) goto next;



    next:
	if (fd>=0) close (fd);
	fd = -1;
	H5E_BEGIN_TRY {
	    if (space>=0) {
		H5Sclose (space);
		space = -1;
	    }
	    if (dset>=0) {
		H5Dclose (dset);
		dset = -1;
	    }
	} H5E_END_TRY;
    }

    /* Close the file */
    H5Fclose (file);
    return 0;
}
