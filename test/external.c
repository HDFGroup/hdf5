/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, March  3, 1998
 *
 * Purpose:	Tests datasets stored in external raw files.
 */
#include <assert.h>
#include <fcntl.h>
#include <hdf5.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


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
 *              Wednesday, March  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
display_error_cb (void *client_data)
{
    puts ("*FAILED*");
    H5Eprint (stdout);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	test_1
 *
 * Purpose:	Describes various external datasets in an HDF5 file without
 *		actually creating the external raw files.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_1 (void) 
{
    hid_t	file, plist, space, dset, grp;
    herr_t	status;
    size_t	size[2], max_size[2];
    herr_t	(*func)(void*) = NULL;
    void	*client_data = NULL;
    int		n;
	
    
    /*
     * Create the file and an initial group.  This causes messages about
     * debugging to be emitted before we start playing games with what the
     * output looks like.
     */
    file = H5Fcreate ("extern_1.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    assert (file>=0);
    grp = H5Gcreate (file, "emit-diagnostics", 8);
    H5Gclose (grp);

    printf ("Testing external storage descriptions...\n");
    
    /*
     * A single external file for a non-extendible dataset.
     */
    do {
	printf ("%-70s", "...fixed-size data space, exact storage");
	fflush (stdout);
	plist = H5Pcreate (H5P_DATASET_CREATE);
	assert (plist>=0);
	status = H5Pset_external (plist, "ext1.data", 0, 400);
	assert (status>=0);

	size[0] = max_size[0] = 100;
	space = H5Screate_simple (1, size, max_size);
	assert (space>=0);

	/* Create the dataset, the `dset1' name is used later too */
	dset = H5Dcreate (file, "dset1", H5T_NATIVE_INT32, space, plist);
	if (dset<0) break;
	H5Dclose (dset);
	puts (" PASSED");
    } while (0);
    H5Sclose (space);
    H5Pclose (plist);

    /*
     * A single external file which is too small to represent all the data.
     */
    do {
	printf ("%-70s", "...external storage is too small");
	fflush (stdout);
	plist = H5Pcreate (H5P_DATASET_CREATE);
	assert (plist>=0);
	status = H5Pset_external (plist, "ext1.data", 0, 399);
	assert (status>=0);

	size[0] = max_size[0] = 100;
	space = H5Screate_simple (1, size, max_size);
	assert (space>=0);

	H5Eget_auto (&func, &client_data);
	H5Eset_auto (NULL, NULL);
	dset = H5Dcreate (file, "dset2", H5T_NATIVE_INT32, space, plist);
	H5Eset_auto (func, client_data);
	
	if (dset>=0) {
	    puts ("*FAILED*");
	    printf ("   Small external file succeeded instead of failing\n");
	    H5Dclose (dset);
	    break;
	}
	puts (" PASSED");
    } while (0);
    H5Sclose (space);
    H5Pclose (plist);

    /*
     * A single external file which is large enough to represent the current
     * data and large enough to represent the eventual size of the data.
     */
    do {
	printf ("%-70s", "...extendible dataspace, exact external size");
	fflush (stdout);
	plist = H5Pcreate (H5P_DATASET_CREATE);
	assert (plist>=0);
	status = H5Pset_external (plist, "ext1.data", 0, 800);
	assert (status>=0);

	size[0] = 100;
	max_size[0] = 200;
	space = H5Screate_simple (1, size, max_size);
	assert (space>=0);

	dset = H5Dcreate (file, "dset3", H5T_NATIVE_INT32, space, plist);
	if (dset<0) break;
	H5Dclose (dset);
	puts (" PASSED");
    } while (0);
    H5Sclose (space);
    H5Pclose (plist);


    /*
     * A single external file which is large enough for the current data size
     * but not large enough for the eventual size.
     */
    do {
	printf ("%-70s", "...extendible dataspace, "
		"external storage is too small");
	fflush (stdout);
	plist = H5Pcreate (H5P_DATASET_CREATE);
	assert (plist>=0);
	status = H5Pset_external (plist, "ext1.data", 0, 799);
	assert (status>=0);

	size[0] = 100;
	max_size[0] = 200;
	space = H5Screate_simple (1, size, max_size);
	assert (space>=0);

	H5Eget_auto (&func, &client_data);
	H5Eset_auto (NULL, NULL);
	dset = H5Dcreate (file, "dset4", H5T_NATIVE_INT32, space, plist);
	H5Eset_auto (func, client_data);
	
	if (dset>=0) {
	    puts ("*FAILED*");
	    printf ("   Small external file succeeded instead of failing\n");
	    H5Dclose (dset);
	    break;
	}
	puts (" PASSED");
    } while (0);
    H5Sclose (space);
    H5Pclose (plist);

    /*
     * A single external file of unlimited size and an unlimited data space.
     */
    do {
	printf ("%-70s", "...unlimited dataspace, unlimited external storage");
	fflush (stdout);
	plist = H5Pcreate (H5P_DATASET_CREATE);
	assert (plist>=0);
	status = H5Pset_external (plist, "ext1.data", 0, H5F_UNLIMITED);
	assert (status>=0);

	size[0] = 100;
	max_size[0] = H5S_UNLIMITED;
	space = H5Screate_simple (1, size, max_size);
	assert (space>=0);

	/* Create the dataset, the `dset5' name is used later too */
	dset = H5Dcreate (file, "dset5", H5T_NATIVE_INT32, space, plist);
	if (dset<0) break;
	H5Dclose (dset);
	puts (" PASSED");
    } while (0);
    H5Sclose (space);
    H5Pclose (plist);

    /*
     * Open one of the previous datasets and make sure it looks the same as
     * when we wrote it.
     */
    do {
	char	name[256];
	size_t	file_offset;
	size_t	file_size;
	
	printf ("%-70s", "...opening a dataset and reading the storage info");
	fflush (stdout);

	dset = H5Dopen (file, "dset1");
	assert (dset>=0);
	plist = H5Dget_create_parms (dset);
	assert (plist>=0);

	n = H5Pget_external_count (plist);
	if (n<0) break;
	if (1!=n) {
	    puts ("*FAILED*");
	    printf ("   Returned external count is wrong.\n");
	    break;
	}
	strcpy (name+sizeof(name)-4, "...");
	status = H5Pget_external (plist, 0, sizeof(name)-4, name,
				  &file_offset, &file_size);
	if (status<0) {
	    printf ("   Unable to read first extern file info.\n");
	    break;
	} else if (file_offset!=0) {
	    puts ("*FAILED*");
	    printf ("   Wrong file offset.\n");
	    break;
	} else if (file_size!=400) {
	    puts ("*FAILED*");
	    printf ("   Wrong file size.\n");
	    break;
	}
	puts (" PASSED");
    } while (0);
    H5Pclose (plist);
    H5Dclose (dset);

    /*
     * Open one of the previous unlimited datasets and make sure it looks the
     * same as when we wrote it.
     */
    do {
	char	name[256];
	size_t	file_offset;
	size_t	file_size;
	
	printf ("%-70s", "...opening an unlimited dataset and reading the "
		"storage info");
	fflush (stdout);

	dset = H5Dopen (file, "dset5");
	assert (dset>=0);
	plist = H5Dget_create_parms (dset);
	assert (plist>=0);

	n = H5Pget_external_count (plist);
	if (n<0) break;
	if (1!=n) {
	    puts ("*FAILED*");
	    printf ("   Returned external count is wrong.\n");
	    break;
	}
	strcpy (name+sizeof(name)-4, "...");
	status = H5Pget_external (plist, 0, sizeof(name)-4, name,
				  &file_offset, &file_size);
	if (status<0) {
	    printf ("   Unable to read first extern file info.\n");
	    break;
	} else if (file_offset!=0) {
	    puts ("*FAILED*");
	    printf ("   Wrong file offset.\n");
	    break;
	} else if (H5F_UNLIMITED!=file_size) {
	    puts ("*FAILED*");
	    printf ("   Wrong file size.\n");
	    break;
	}
	puts (" PASSED");
    } while (0);
    H5Pclose (plist);
    H5Dclose (dset);

    /*
     * Multiple external files for a dataset.
     */
    do {
	printf ("%-70s", "...multiple external files");
	fflush (stdout);
	plist = H5Pcreate (H5P_DATASET_CREATE);
	assert (plist>=0);
	status = H5Pset_external (plist, "ext1.data", 0, 100);
	assert (status>=0);
	status = H5Pset_external (plist, "ext2.data", 0, 100);
	assert (status>=0);
	status = H5Pset_external (plist, "ext3.data", 0, 100);
	assert (status>=0);
	status = H5Pset_external (plist, "ext4.data", 0, 100);
	assert (status>=0);

	size[0] = max_size[0] = 100;
	space = H5Screate_simple (1, size, max_size);
	assert (space>=0);

	dset = H5Dcreate (file, "dset6", H5T_NATIVE_INT32, space, plist);
	if (dset<0) break;
	H5Dclose (dset);
	puts (" PASSED");
    } while (0);
    H5Sclose (space);
    H5Pclose (plist);

    /*
     * It should be impossible to define an unlimited external file and then
     * follow it with another external file.
     */
    do {
	printf ("%-70s", "...external file following unlimited file");
	fflush (stdout);
	plist = H5Pcreate (H5P_DATASET_CREATE);
	assert (plist>=0);
	status = H5Pset_external (plist, "ext1.data", 0, H5F_UNLIMITED);
	assert (status>=0);

	/* Next function should fail */
	H5Eget_auto (&func, &client_data);
	H5Eset_auto (NULL, NULL);
	status = H5Pset_external (plist, "ext2.data", 0, 100);
	H5Eset_auto (func, client_data);
	if (status>=0) {
	    puts ("*FAILED*");
	    puts ("   H5Pset_external() succeeded when it should have failed");
	    break;
	}

	/* Check the number of files */
	n = H5Pget_external_count (plist);
	if (n<0) break;
	if (1!=n) {
	    puts ("*FAILED*");
	    puts ("   Wrong external file count returned.");
	}
	puts (" PASSED");
    } while (0);
    H5Pclose (plist);

    /*
     * It should be impossible to create a set of external files whose total
     * size overflows a size_t integer.
     */
    do {
	printf ("%-70s", "...address overflow in external files");
	fflush (stdout);
	plist = H5Pcreate (H5P_DATASET_CREATE);
	assert (plist>=0);
	status = H5Pset_external (plist, "ext1.data", 0, H5F_UNLIMITED-1);
	assert (status>=0);

	/* Next function should fail */
	H5Eget_auto (&func, &client_data);
	H5Eset_auto (NULL, NULL);
	status = H5Pset_external (plist, "ext2.data", 0, 100);
	H5Eset_auto (func, client_data);
	if (status>=0) {
	    puts ("*FAILED*");
	    puts ("   H5Pset_external() succeeded when it should have failed");
	    break;
	}
	puts (" PASSED");
    } while (0);
    H5Pclose (plist);



    /* END OF TESTS */
    H5Fclose (file);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	test_2
 *
 * Purpose:	Tests reading from an external file set.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, March  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_2 (void)
{
    hid_t	file, plist, space, dset, grp;
    herr_t	status;
    int		fd,i, j;
    ssize_t	n;
    char	fname[64];
    int		part[25], whole[100];
    size_t	size;

    /* Write the data to external files */
    printf ("Writing external data...\n");
    for (i=0; i<4; i++) {
	for (j=0; j<25; j++) {
	    part[j] = i*25+j;
	}
	
	sprintf (fname, "extern_%d.raw", i+1);
	fd = open (fname, O_RDWR|O_CREAT|O_TRUNC, 0666);
	assert (fd>=0);
	n = write (fd, part, sizeof(part));
	assert (n==sizeof(part));
	close (fd);
    }
    
    /*
     * Create the file and an initial group.  This causes messages about
     * debugging to be emitted before we start playing games with what the
     * output looks like.
     */
    file = H5Fcreate ("extern_2.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    assert (file>=0);
    grp = H5Gcreate (file, "emit-diagnostics", 8);
    H5Gclose (grp);
    printf ("Testing external data reading...\n");

    /* Create the external file list */
    plist = H5Pcreate (H5P_DATASET_CREATE);
    assert (plist>=0);
    status = H5Pset_external (plist, "extern_1.raw", 0, sizeof(part));
    assert (status>=0);
    status = H5Pset_external (plist, "extern_2.raw", 0, sizeof(part));
    assert (status>=0);
    status = H5Pset_external (plist, "extern_3.raw", 0, sizeof(part));
    assert (status>=0);
    status = H5Pset_external (plist, "extern_4.raw", 0, sizeof(part));
    assert (status>=0);

    /* Create the data space */
    size = 100;
    space = H5Screate_simple (1, &size, NULL);
    assert (space>=0);

    /* Create the dataset */
    dset = H5Dcreate (file, "dset1", H5T_NATIVE_INT, space, plist);
    assert (dset>=0);

    /*
     * Read the entire dataset and compare with the original
     */
    do {
	/* Read from the dataset */
	printf ("%-70s", "...reading entire dataset");
	fflush (stdout);
	status = H5Dread (dset, H5T_NATIVE_INT, space, space,
			  H5P_DEFAULT, whole);
	if (status<0) {
	    puts ("   Failed to read dataset");
	    break;
	}

	for (i=0; i<100; i++) {
	    if (whole[i]!=i) {
		puts ("*FAILED*");
		puts ("   Incorrect value(s) read.");
		break;
	    }
	}
	puts (" PASSED");
    } while (0);
    
    H5Dclose (dset);
    H5Pclose (plist);
    H5Sclose (space);
    H5Fclose (file);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Runs external dataset tests.
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(non-zero)
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (void)
{
    herr_t	status;
    int		nerrors=0;

    H5Eset_auto (display_error_cb, NULL);

    status = test_1 ();
    nerrors += (status<0 ? 1 : 0);

    status = test_2 ();
    nerrors += (status<0 ? 1 : 0);

    if (0==nerrors) {
	printf ("All external storage tests passed.\n");
    }

    exit (nerrors?1:0);
}
