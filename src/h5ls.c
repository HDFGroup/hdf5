/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Monday, March 23, 1998
 */
#include <hdf5.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <H5private.h>
#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif

static void
usage (const char *progname)
{
    fprintf (stderr, "usage: %s FILE [GROUP]\n", progname);
    fprintf (stderr, "   The file name may contain a printf integer format "
	     "to open a file family.\n");
    exit (1);
}



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
    char	buf[512];
    H5G_stat_t	statbuf;
    
    /* Disable error reporting */
    H5Eget_auto (&func, &edata);
    H5Eset_auto (NULL, NULL);

    /* Print info about each name */
    printf ("%-25s ", name);

    if (H5Gstat (group, name, TRUE, &statbuf)>=0) {
	sprintf (buf, "%lu:%lu:%lu:%lu",
		 statbuf.fileno[1], statbuf.fileno[0],
		 statbuf.objno[1], statbuf.objno[0]);
	printf ("%-20s ", buf);
    }

    if ((obj=H5Dopen (group, name))>=0) {
	hsize_t size[64];
	hid_t space = H5Dget_space (obj);
	int ndims = H5Sget_dims (space, size);
	printf ("Dataset {");
	for (i=0; i<ndims; i++) {
	    HDfprintf (stdout, "%s%Hu", i?", ":"", size[i]);
	}
	printf ("}\n");
	H5Dclose (space);
	H5Dclose (obj);
    } else if ((obj=H5Gopen (group, name))>=0) {
	printf ("Group\n");
	H5Gclose (obj);
    } else if (H5Gget_linkval (group, name, sizeof(buf), buf)>=0) {
	if (NULL==HDmemchr (buf, 0, sizeof(buf))) {
	    strcpy (buf+sizeof(buf)-4, "...");
	}
	printf (" -> %s\n", buf);
    } else if ((obj=H5Topen (group, name))>=0) {
	printf ("Data type\n");
	H5Tclose (obj);
    } else {
	printf ("Unknown Type\n");
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
    const char	*fname = NULL;
    const char	*gname = "/";
    const char	*progname;

    /* Arguments */
    if ((progname=strrchr (argv[0], '/'))) progname++;
    else progname = argv[0];
    if (argc<2 || argc>3) usage (progname);
    fname = argv[1];
    if (argc>=3) gname = argv[2];

    /*
     * Open the file.  If the file name contains a `%' then assume that a
     * file family is being opened.
     */
    if (strchr (fname, '%')) {
	plist = H5Pcreate (H5P_FILE_ACCESS);
	H5Pset_family (plist, 0, H5P_DEFAULT);
    }
    if ((file = H5Fopen (fname, H5F_ACC_RDONLY, plist))<0) exit (1);
    if (H5Giterate (file, gname, NULL, list, NULL)<0) exit (1);
    if (H5Fclose (file)<0) exit (1);
    return 0;
}
