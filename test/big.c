/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Wednesday, April  8, 1998
 */
#include <assert.h>
#include <ctype.h>
#include <hdf5.h>
#include <math.h>
#include <stdlib.h>

#include <H5private.h> /*needed for HDfprintf() */

#define FNAME		"big%05d.h5"
#define WRT_N		50
#define WRT_SIZE	4*1024
#define FAMILY_SIZE	1024*1024*1024

static hsize_t
randll (hsize_t limit)
{
    hsize_t	acc = rand ();
    acc *= rand ();

    return acc % limit;
}



/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Creates a *big* dataset.
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April  8, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
writer (int wrt_n)
{
    hsize_t	size1[4] = {8, 1024, 1024, 1024};
    hsize_t	size2[1] = {8589934592LL};
    hssize_t	hs_start[1];
    hsize_t	hs_size[1];
    hid_t	plist, file, space1, space2, mem_space, d1, d2;
    int		*buf = malloc (sizeof(int) * WRT_SIZE);
    int		i, j;

    /*
     * Make sure that `hsize_t' is large enough to represent the entire data
     * space.
     */
    assert (sizeof(hsize_t)>4);

    /*
     * We might be on a machine that has 32-bit files, so create an HDF5 file
     * which is a family of files.  Each member of the family will be 1GB
     */
    plist = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_family (plist, FAMILY_SIZE, H5P_DEFAULT);
    file = H5Fcreate (FNAME, H5F_ACC_TRUNC, H5P_DEFAULT, plist);
    H5Pclose (plist);

    /* Create simple data spaces according to the size specified above. */
    space1 = H5Screate_simple (4, size1, size1);
    space2 = H5Screate_simple (1, size2, size2);

    /* Create the datasets */
    d1 = H5Dcreate (file, "d1", H5T_NATIVE_INT, space1, H5P_DEFAULT);
    d2 = H5Dcreate (file, "d2", H5T_NATIVE_INT, space2, H5P_DEFAULT);

    /* Write some things to them randomly */
    hs_size[0] = WRT_SIZE;
    mem_space = H5Screate_simple (1, hs_size, hs_size);
    for (i=0; i<wrt_n; i++) {
	hs_start[0] = randll (size2[0]);
	HDfprintf (stdout, "#%03d 0x%016Hx\n", i, hs_start[0]);
	H5Sset_hyperslab (space2, hs_start, hs_size, NULL);
	for (j=0; j<WRT_SIZE; j++) {
	    buf[j] = i+1;
	}
	H5Dwrite (d2, H5T_NATIVE_INT, mem_space, space2, H5P_DEFAULT, buf);
    }
	
    H5Dclose (d1);
    H5Dclose (d2);
    H5Sclose (mem_space);
    H5Sclose (space1);
    H5Sclose (space2);
    H5Fclose (file);
    free (buf);
}


/*-------------------------------------------------------------------------
 * Function:	reader
 *
 * Purpose:	Reads some data from random locations in the dataset.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, April 10, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
reader (const char *script_name)
{
    FILE	*script;
    hid_t	plist, file, mspace, fspace, d2;
    char	ln[64], *s;
    hssize_t	hs_offset[1];
    hsize_t	hs_size[1] = {WRT_SIZE};
    int		*buf = malloc (sizeof(int) * WRT_SIZE);
    int		i, j, zero, wrong;

    /* Open script file */
    script = fopen (script_name, "r");

    /* Open HDF5 file */
    plist = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_family (plist, FAMILY_SIZE, H5P_DEFAULT);
    file = H5Fopen (FNAME, H5F_ACC_RDONLY, plist);
    H5Pclose (plist);

    /* Open the dataset */
    d2 = H5Dopen (file, "d2");
    fspace = H5Dget_space (d2);

    /* Describe `buf' */
    mspace = H5Screate_simple (1, hs_size, hs_size);

    /* Read each region */
    while (fgets (ln, sizeof(ln), script)) {
	if ('#'!=ln[0]) break;
	i = (int)strtol (ln+1, &s, 10);
	hs_offset[0] = HDstrtoll (s, NULL, 0);
	HDfprintf (stdout, "#%03d 0x%016Hx", i, hs_offset[0]);
	fflush (stdout);

	H5Sset_hyperslab (fspace, hs_offset, hs_size, NULL);
	H5Dread (d2, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT, buf);

	/* Check */
	for (j=zero=wrong=0; j<WRT_SIZE; j++) {
	    if (0==buf[j]) zero++;
	    else if (buf[j]!=i+1) wrong++;
	}
	if (zero) {
	    printf (" *FAILED*  (%d zeros)\n", zero);
	} else if (wrong) {
	    printf (" *SKIPPED* (possible overlap with another region)\n");
	} else {
	    printf ("  PASSED\n");
	}
    }

    H5Dclose (d2);
    H5Sclose (mspace);
    H5Sclose (fspace);
    H5Fclose (file);
    free (buf);
    fclose (script);
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Robb Matzke
 *              Friday, April 10, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (int argc, char *argv[])
{
    if (1==argc) {
	writer (WRT_N);
    } else if (isdigit (argv[1][0])) {
	writer ((int)strtol(argv[1], NULL, 0));
    } else {
	reader (argv[1]);
    }
    return 0;
}
