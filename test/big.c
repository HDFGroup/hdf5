/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Wednesday, April  8, 1998
 */
#include <assert.h>
#include <ctype.h>
#include <fcntl.h>
#include <hdf5.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

#include <H5private.h> /*needed for HDfprintf() */

#define FNAME		"big%05d.h5"
#define DNAME		"big.data"
#define WRT_N		50
#define WRT_SIZE	4*1024
#define FAMILY_SIZE	1024*1024*1024
#define GB8LL		((unsigned long long)8*1024*1024*1024)

static hsize_t
randll (hsize_t limit)
{
    hsize_t	acc = rand ();
    acc *= rand ();

    return acc % limit;
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
 * Function:	is_sparse
 *
 * Purpose:	Determines if the file system of the current working
 *		directory supports holes.
 *
 * Return:	Success:	Non-zero if holes are supported; zero
 *				otherwise.
 *
 *		Failure:	zero
 *
 * Programmer:	Robb Matzke
 *              Wednesday, July 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
is_sparse(void)
{
    int		fd;
    struct stat	sb;
    
    if ((fd=open("x.h5", O_RDWR|O_TRUNC|O_CREAT, 0666))<0) return 0;
    if (lseek(fd, 1024*1024, SEEK_SET)!=1024*1024) return 0;
    if (5!=write(fd, "hello", 5)) return 0;
    if (stat("x.h5", &sb)<0) return 0;
    if (unlink("x.h5")<0) return 0;
    return (sb.st_blocks*512 < (unsigned)sb.st_size);
}


/*-------------------------------------------------------------------------
 * Function:	enough_room
 *
 * Purpose:	Tries to create a bunch of sparse files to see if quotas will
 *		get in the way.  Some systems also have problems opening
 *		enough files and we'll check that too.
 *
 * Return:	Success:	Non-zero
 *
 *		Failure:	zero
 *
 * Programmer:	Robb Matzke
 *              Thursday, August  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
enough_room(void)
{
    int		ret_value=0;
    int		fd[68];
    size_t	i, size = (size_t)1 << 30;
    char	name[32];

    /* Initialize file descriptors */
    for (i=0; i<NELMTS(fd); i++) fd[i] = -1;

    /* Create files */
    for (i=0; i<NELMTS(fd); i++) {
	sprintf(name, FNAME, i);
	if ((fd[i]=open(name, O_RDWR|O_CREAT|O_TRUNC, 0666))<0) {
	    goto done;
	}
	if ((ssize_t)size != lseek(fd[i], size, SEEK_SET)) {
	    goto done;
	}
	if (1!=write(fd[i], "X", 1)) {
	    goto done;
	}
    }
    ret_value = 1;

 done:
    for (i=0; i<NELMTS(fd) && fd[i]>=0; i++) {
	sprintf(name, FNAME, i);
	close(fd[i]);
	unlink(name);
    }
    
    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function:	writer
 *
 * Purpose:	Creates a *big* dataset.
 *
 * Return:	Success:	0
 *
 *		Failure:	>0
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April  8, 1998
 *
 * Modifications:
 * 	Robb Matzke, 15 Jul 1998
 *	Addresses are written to the file DNAME instead of stdout.
 *
 *-------------------------------------------------------------------------
 */
static int
writer (int wrt_n)
{
    hsize_t	size1[4] = {8, 1024, 1024, 1024};
    hsize_t	size2[1] = {GB8LL};
    hssize_t	hs_start[1];
    hsize_t	hs_size[1];
    hid_t	plist, file, space1, space2, mem_space, d1, d2;
    int		*buf = malloc (sizeof(int) * WRT_SIZE);
    int		i, j;
    FILE	*out = fopen(DNAME, "w");

    printf("%-70s", "Writing large dataset");
    
    /*
     * Make sure that `hsize_t' is large enough to represent the entire data
     * space.
     */
    assert (sizeof(hsize_t)>4);

    /*
     * We might be on a machine that has 32-bit files, so create an HDF5 file
     * which is a family of files.  Each member of the family will be 1GB
     */
    if ((plist = H5Pcreate (H5P_FILE_ACCESS))<0) goto error;
    if (H5Pset_family (plist, FAMILY_SIZE, H5P_DEFAULT)<0) goto error;
    file = H5Fcreate (FNAME, H5F_ACC_TRUNC|H5F_ACC_DEBUG, H5P_DEFAULT, plist);
    if (file<0) goto error;
    if (H5Pclose (plist)<0) goto error;

    /* Create simple data spaces according to the size specified above. */
    if ((space1 = H5Screate_simple (4, size1, size1))<0 ||
	(space2 = H5Screate_simple (1, size2, size2))<0) {
	goto error;
    }
    
    /* Create the datasets */
    if ((d1=H5Dcreate (file, "d1", H5T_NATIVE_INT, space1, H5P_DEFAULT))<0 ||
	(d2=H5Dcreate (file, "d2", H5T_NATIVE_INT, space2, H5P_DEFAULT))<0) {
	goto error;
    }
    

    /* Write some things to them randomly */
    hs_size[0] = WRT_SIZE;
    if ((mem_space = H5Screate_simple (1, hs_size, hs_size))<0) goto error;
    for (i=0; i<wrt_n; i++) {
	hs_start[0] = randll (size2[0]);
	HDfprintf (out, "#%03d 0x%016Hx\n", i, hs_start[0]);
	if (H5Sselect_hyperslab (space2, H5S_SELECT_SET, hs_start, NULL,
				 hs_size, NULL)<0) goto error;
	for (j=0; j<WRT_SIZE; j++) {
	    buf[j] = i+1;
	}
	if (H5Dwrite (d2, H5T_NATIVE_INT, mem_space, space2,
		      H5P_DEFAULT, buf)<0) goto error;
    }
	
    if (H5Dclose (d1)<0) goto error;
    if (H5Dclose (d2)<0) goto error;
    if (H5Sclose (mem_space)<0) goto error;
    if (H5Sclose (space1)<0) goto error;
    if (H5Sclose (space2)<0) goto error;
    if (H5Fclose (file)<0) goto error;
    free (buf);
    fclose(out);
    puts(" PASSED");
    return 0;

 error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	reader
 *
 * Purpose:	Reads some data from random locations in the dataset.
 *
 * Return:	Success:	0
 *
 * 		Failure:	>0
 *
 * Programmer:	Robb Matzke
 *              Friday, April 10, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
reader (void)
{
    FILE	*script;
    hid_t	plist, file, mspace, fspace, d2;
    char	ln[128], *s;
    hssize_t	hs_offset[1];
    hsize_t	hs_size[1] = {WRT_SIZE};
    int		*buf = malloc (sizeof(int) * WRT_SIZE);
    int		i, j, zero, wrong, nerrors=0;

    /* Open script file */
    script = fopen (DNAME, "r");

    /* Open HDF5 file */
    if ((plist = H5Pcreate (H5P_FILE_ACCESS))<0) goto error;
    if (H5Pset_family (plist, FAMILY_SIZE, H5P_DEFAULT)<0) goto error;
    if ((file = H5Fopen (FNAME, H5F_ACC_RDONLY|H5F_ACC_DEBUG, plist))<0) {
	goto error;
    }
    if (H5Pclose (plist)<0) goto error;

    /* Open the dataset */
    if ((d2 = H5Dopen (file, "d2"))<0) goto error;
    if ((fspace = H5Dget_space (d2))<0) goto error;

    /* Describe `buf' */
    if ((mspace = H5Screate_simple (1, hs_size, hs_size))<0) goto error;

    /* Read each region */
    while (fgets (ln, sizeof(ln), script)) {
	if ('#'!=ln[0]) break;
	i = (int)strtol (ln+1, &s, 10);
	hs_offset[0] = HDstrtoll (s, NULL, 0);
	HDfprintf (stdout, "#%03d 0x%016Hx%47s", i, hs_offset[0], "");
	fflush (stdout);

	if (H5Sselect_hyperslab (fspace, H5S_SELECT_SET, hs_offset, NULL,
				 hs_size, NULL)<0) goto error;
	if (H5Dread (d2, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT, buf)<0) {
	    goto error;
	}

	/* Check */
	for (j=zero=wrong=0; j<WRT_SIZE; j++) {
	    if (0==buf[j]) zero++;
	    else if (buf[j]!=i+1) wrong++;
	}
	if (zero) {
	    puts("*FAILED*");
	    printf("   %d zero%s\n", zero, 1==zero?"":"s");
	} else if (wrong) {
	    puts("--SKIP--");
	    puts("   Possible overlap with another region.");
	    nerrors++;
	} else {
	    puts(" PASSED");
	}
    }

    if (H5Dclose (d2)<0) goto error;
    if (H5Sclose (mspace)<0) goto error;
    if (H5Sclose (fspace)<0) goto error;
    if (H5Fclose (file)<0) goto error;
    free (buf);
    fclose (script);
    return nerrors;

 error:
    return 1;
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
    int		i;
    char	buf[256];
    
    if (!getenv ("HDF5_NOCLEANUP")) {
	for (i=0; i<512; i++) {
	    sprintf(buf, FNAME, i);
	    remove(buf);
	}
	remove(DNAME);
    }
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
main (void)
{
    int		nerrors = 0;
    
    /*
     * We shouldn't run this test if the file system doesn't support holes
     * because we would generate multi-gigabyte files.
     */
    puts("Checking if file system is adequate for this test...");
    if (!is_sparse()) {
	puts("Test skipped because file system does not support holes.");
	exit(0);
    }
    if (!enough_room()) {
	puts("Test skipped because of quota (file size or num open files).");
	exit(0);
    }
    
    /* Set the error handler */
    H5Eset_auto (display_error_cb, NULL);

    if ((nerrors=writer(WRT_N))>0) exit(nerrors);
    nerrors = reader();
    cleanup();
    return nerrors;
}
