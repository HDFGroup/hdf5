/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Monday, March 23, 1998
 */
#include <assert.h>
#include <hdf5.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <H5config.h>
#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif


/*-------------------------------------------------------------------------
 * Function:	list
 *
 * Purpose:	Prints the group member name.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Monday, March 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
list (hid_t group, const char *name, void __unused__ *op_data)
{
    hid_t	obj;
    hid_t	(*func)(void*);
    void	*edata;
    int		i;

    /* Disable error reporting */
    H5Eget_auto (&func, &edata);
    H5Eset_auto (NULL, NULL);

    /* Print info about each name */
    printf ("%-30s", name);
    if ((obj=H5Dopen (group, name))>=0) {
	hsize_t size[64];
	hid_t space = H5Dget_space (obj);
	int ndims = H5Sget_dims (space, size);
	printf (" Dataset {");
	for (i=0; i<ndims; i++) {
	    printf ("%s%lu", i?", ":"", (unsigned long)size[i]);
	}
	printf ("}\n");
	H5Dclose (space);
	H5Dclose (obj);
    } else if ((obj=H5Gopen (group, name))>=0) {
	printf (" Group\n");
	H5Gclose (obj);
    } else {
	printf (" Unknown Type\n");
    }

    /* Restore error reporting */
    H5Eset_auto (func, edata);
    return 0;
}



/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Opens a file and lists the specified group
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Robb Matzke
 *              Monday, March 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (int argc, char *argv[])
{
    hid_t	file, plist=H5P_DEFAULT;
    herr_t	status;
    const char	*fname = NULL;
    const char	*gname = "/";
    
    assert (argc>=2 && argc<=3);
    fname = argv[1];
    if (argc>=3) gname = argv[2];

    /*
     * Open the file.  If the file name contains a `%' then assume that a
     * file family is being opened.
     */
    if (strchr (fname, '%')) {
	plist = H5Pcreate (H5P_FILE_ACCESS);
	H5Pset_family (plist, H5P_DEFAULT);
    }
    file = H5Fopen (fname, H5F_ACC_RDONLY, plist);
    assert (file>=0);

    status = H5Giterate (file, gname, NULL, list, NULL);
    assert (status>=0);

    H5Fclose (file);
    return 0;
}
