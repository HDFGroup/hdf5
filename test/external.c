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
#include <string.h>
#if !defined(WIN32)
#include <unistd.h>
#endif

#include <H5config.h>
#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif

#if defined(WIN32)
#undef __unused__
#define __unused__
#endif

#define TEST_FILE_NAME1		"extern_1.h5"
#define TEST_FILE_NAME2		"extern_2.h5"
#define TEST_FILE_NAME3		"extern_3.h5"

static int nerrors_g = 0;


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
display_error_cb (void __unused__ *client_data)
{
    puts ("*FAILED*");
    H5Eprint (stdout);
    nerrors_g++;
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	same_contents
 *
 * Purpose:	Determines whether two files are exactly the same.
 *
 * Return:	Success:	nonzero if same, zero if different.
 *
 *		Failure:	zero
 *
 * Programmer:	Robb Matzke
 *              Wednesday, March  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
same_contents (const char *name1, const char *name2)
{
    int		fd1, fd2;
    ssize_t	n1, n2;
    char	buf1[1024], buf2[1024];

    fd1 = open (name1, O_RDONLY);
    fd2 = open (name2, O_RDONLY);
    assert (fd1>=0 && fd2>=0);

    while (1) {
	n1 = read (fd1, buf1, sizeof(buf1));
	n2 = read (fd2, buf2, sizeof(buf2));
	assert (n1>=0 && (size_t)n1<=sizeof(buf1));
	assert (n2>=0 && (size_t)n2<=sizeof(buf2));
	assert (n1==n2);
	
	if (n1<=0 && n2<=0) break;
	if (memcmp (buf1, buf2, (size_t)n1)) {
	    close (fd1);
	    close (fd2);
	    return 0;
	}
    }
    close (fd1);
    close (fd2);
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_1
 *
 * Purpose:	Describes various external datasets in an HDF5 file without
 *		actually creating the external raw files.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
test_1 (void) 
{
    hid_t	file, plist, space, dset, grp;
    herr_t	status;
    hsize_t	size[2], max_size[2];
    herr_t	(*func)(void*) = NULL;
    void	*client_data = NULL;
    int		n;
	
    
    /*
     * Create the file and an initial group.  This causes messages about
     * debugging to be emitted before we start playing games with what the
     * output looks like.
     */
    file = H5Fcreate (TEST_FILE_NAME1, H5F_ACC_TRUNC|H5F_ACC_DEBUG,
		      H5P_DEFAULT, H5P_DEFAULT);
    assert (file>=0);
    grp = H5Gcreate (file, "emit-diagnostics", 8);
    H5Gclose (grp);

    /*
     * A single external file for a non-extendible dataset.
     */
    do {
	printf ("%-70s", "...fixed-size data space, exact storage");
	fflush (stdout);
	plist = H5Pcreate (H5P_DATASET_CREATE);
	assert (plist>=0);
	status = H5Pset_external (plist, "ext1.data", 0, (hsize_t)400);
	assert (status>=0);

	size[0] = max_size[0] = 100;
	space = H5Screate_simple (1, size, max_size);
	assert (space>=0);

	/* Create the dataset, the `dset1' name is used later too */
	dset = H5Dcreate (file, "dset1", H5T_NATIVE_INT, space, plist);
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
	status = H5Pset_external (plist, "ext1.data", 0, (hsize_t)399);
	assert (status>=0);

	size[0] = max_size[0] = 100;
	space = H5Screate_simple (1, size, max_size);
	assert (space>=0);

	H5Eget_auto (&func, &client_data);
	H5Eset_auto (NULL, NULL);
	dset = H5Dcreate (file, "dset2", H5T_NATIVE_INT, space, plist);
	H5Eset_auto (func, client_data);
	
	if (dset>=0) {
	    puts ("*FAILED*");
	    printf ("   Small external file succeeded instead of failing\n");
	    nerrors_g++;
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
	status = H5Pset_external (plist, "ext1.data", 0, (hsize_t)800);
	assert (status>=0);

	size[0] = 100;
	max_size[0] = 200;
	space = H5Screate_simple (1, size, max_size);
	assert (space>=0);

	dset = H5Dcreate (file, "dset3", H5T_NATIVE_INT, space, plist);
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
	status = H5Pset_external (plist, "ext1.data", 0, (hsize_t)799);
	assert (status>=0);

	size[0] = 100;
	max_size[0] = 200;
	space = H5Screate_simple (1, size, max_size);
	assert (space>=0);

	H5Eget_auto (&func, &client_data);
	H5Eset_auto (NULL, NULL);
	dset = H5Dcreate (file, "dset4", H5T_NATIVE_INT, space, plist);
	H5Eset_auto (func, client_data);
	
	if (dset>=0) {
	    puts ("*FAILED*");
	    printf ("   Small external file succeeded instead of failing\n");
	    H5Dclose (dset);
	    nerrors_g++;
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
	dset = H5Dcreate (file, "dset5", H5T_NATIVE_INT, space, plist);
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
	off_t	file_offset;
	hsize_t	file_size;
	
	printf ("%-70s", "...opening a dataset and reading the storage info");
	fflush (stdout);

	dset = H5Dopen (file, "dset1");
	assert (dset>=0);
	plist = H5Dget_create_plist (dset);
	assert (plist>=0);

	n = H5Pget_external_count (plist);
	if (n<0) break;
	if (1!=n) {
	    puts ("*FAILED*");
	    printf ("   Returned external count is wrong.\n");
	    nerrors_g++;
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
	    nerrors_g++;
	    break;
	} else if (file_size!=400) {
	    puts ("*FAILED*");
	    printf ("   Wrong file size.\n");
	    nerrors_g++;
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
	off_t	file_offset;
	hsize_t	file_size;
	
	printf ("%-70s", "...opening an unlimited dataset and reading the "
		"storage info");
	fflush (stdout);

	dset = H5Dopen (file, "dset5");
	assert (dset>=0);
	plist = H5Dget_create_plist (dset);
	assert (plist>=0);

	n = H5Pget_external_count (plist);
	if (n<0) break;
	if (1!=n) {
	    puts ("*FAILED*");
	    printf ("   Returned external count is wrong.\n");
	    nerrors_g++;
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
	    nerrors_g++;
	    break;
	} else if (H5F_UNLIMITED!=file_size) {
	    puts ("*FAILED*");
	    printf ("   Wrong file size.\n");
	    nerrors_g++;
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
	status = H5Pset_external (plist, "ext1.data", 0, (hsize_t)100);
	assert (status>=0);
	status = H5Pset_external (plist, "ext2.data", 0, (hsize_t)100);
	assert (status>=0);
	status = H5Pset_external (plist, "ext3.data", 0, (hsize_t)100);
	assert (status>=0);
	status = H5Pset_external (plist, "ext4.data", 0, (hsize_t)100);
	assert (status>=0);

	size[0] = max_size[0] = 100;
	space = H5Screate_simple (1, size, max_size);
	assert (space>=0);

	dset = H5Dcreate (file, "dset6", H5T_NATIVE_INT, space, plist);
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
	status = H5Pset_external (plist, "ext2.data", 0, (hsize_t)100);
	H5Eset_auto (func, client_data);
	if (status>=0) {
	    puts ("*FAILED*");
	    puts ("   H5Pset_external() succeeded when it should have failed");
	    nerrors_g++;
	    break;
	}

	/* Check the number of files */
	n = H5Pget_external_count (plist);
	if (n<0) break;
	if (1!=n) {
	    puts ("*FAILED*");
	    puts ("   Wrong external file count returned.");
	    nerrors_g++;
	    break;
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
	status = H5Pset_external (plist, "ext2.data", 0, (hsize_t)100);
	H5Eset_auto (func, client_data);
	if (status>=0) {
	    puts ("*FAILED*");
	    puts ("   H5Pset_external() succeeded when it should have failed");
	    nerrors_g++;
	    break;
	}
	puts (" PASSED");
    } while (0);
    H5Pclose (plist);



    /* END OF TESTS */
    H5Fclose (file);
}


/*-------------------------------------------------------------------------
 * Function:	test_2
 *
 * Purpose:	Tests reading from an external file set.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Wednesday, March  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
test_2 (void)
{
    hid_t	file, plist, space, dset, grp;
    herr_t	status;
    int		fd;
    hsize_t	i, j;
    hssize_t	n;
    char	fname[64];
    int		part[25], whole[100];
    hsize_t	size;

    /* Write the data to external files */
    for (i=0; i<4; i++) {
	for (j=0; j<25; j++) {
	    part[j] = (int)(i*25+j);
	}
	
	sprintf (fname, "extern_%lu.raw", (unsigned long)i+1);
	fd = open (fname, O_RDWR|O_CREAT|O_TRUNC, 0666);
	assert (fd>=0);
	n = lseek (fd, (off_t)(i*10), SEEK_SET);
	assert (n>=0 && (size_t)n==i*10);
	n = write (fd, part, sizeof(part));
	assert (n==sizeof(part));
	close (fd);
    }
    
    /*
     * Create the file and an initial group.  This causes messages about
     * debugging to be emitted before we start playing games with what the
     * output looks like.
     */
    file = H5Fcreate (TEST_FILE_NAME2, H5F_ACC_TRUNC|H5F_ACC_DEBUG,
		      H5P_DEFAULT, H5P_DEFAULT);
    assert (file>=0);
    grp = H5Gcreate (file, "emit-diagnostics", 8);
    H5Gclose (grp);

    /* Create the external file list */
    plist = H5Pcreate (H5P_DATASET_CREATE);
    assert (plist>=0);
    status = H5Pset_external (plist, "extern_1.raw", 0,
			      (hsize_t)sizeof(part));
    assert (status>=0);
    status = H5Pset_external (plist, "extern_2.raw", 10,
			      (hsize_t)sizeof(part));
    assert (status>=0);
    status = H5Pset_external (plist, "extern_3.raw", 20,
			      (hsize_t)sizeof(part));
    assert (status>=0);
    status = H5Pset_external (plist, "extern_4.raw", 30,
			      (hsize_t)sizeof(part));
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

	memset (whole, 0, sizeof(whole));
	status = H5Dread (dset, H5T_NATIVE_INT, space, space,
			  H5P_DEFAULT, whole);
	if (status<0) {
	    puts ("   Failed to read dataset");
	    break;
	}

	for (i=0; i<100; i++) {
	    if (whole[i]!=(signed)i) {
		puts ("*FAILED*");
		puts ("   Incorrect value(s) read.");
		nerrors_g++;
		break;
	    }
	}
	puts (" PASSED");
    } while (0);


    
    /*
     * Read the middle of the dataset
     */
    do {
	hid_t hs_space;
	hssize_t hs_start = 30;
	hsize_t hs_count = 25;

	/* Read from the dataset */
	printf ("%-70s", "...reading partial dataset");
	fflush (stdout);

	hs_space = H5Scopy (space);
	assert (hs_space>=0);
	status = H5Sselect_hyperslab (hs_space, H5S_SELECT_SET, &hs_start, NULL, &hs_count, NULL);
	assert (status>=0);

	memset (whole, 0, sizeof(whole));
	status = H5Dread (dset, H5T_NATIVE_INT, hs_space, hs_space,
			  H5P_DEFAULT, whole);
	H5Sclose (hs_space);
	if (status<0) {
	    puts ("   Failed to read dataset");
	    break;
	}

#if 0
	for (i=0; i<100; i++) {
	    printf ("  #%02d %3d %s\n",
		    i, whole[i], whole[i]==i?"":" <------------------------");
	}
#endif

	for (i=hs_start; i<hs_start+hs_count; i++) {
	    if (whole[i]!=(signed)i) {
		puts ("*FAILED*");
		puts ("   Incorrect value(s) read.");
		nerrors_g++;
		break;
	    }
	}
	puts (" PASSED");
    } while (0);
    
    H5Dclose (dset);
    H5Pclose (plist);
    H5Sclose (space);
    H5Fclose (file);
}


/*-------------------------------------------------------------------------
 * Function:	test_3
 *
 * Purpose:	Tests writing to an external file set.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Wednesday, March  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
test_3 (void)
{
    hid_t	file, plist, mem_space, file_space, dset;
    herr_t	status;
    unsigned	i;
    int		fd;
    int		part[25], whole[100];
    hssize_t	hs_start=100;
    hsize_t	size=100, max_size=200, hs_count=100;

    /*
     * Create another file
     */
    file = H5Fcreate (TEST_FILE_NAME3, H5F_ACC_TRUNC|H5F_ACC_DEBUG,
		      H5P_DEFAULT, H5P_DEFAULT);
    assert (file>=0);

    /* Create the external file list */
    plist = H5Pcreate (H5P_DATASET_CREATE);
    assert (plist>=0);
    status = H5Pset_external (plist, "extern_1b.raw", 0,
			      (hsize_t)sizeof(part));
    assert (status>=0);
    status = H5Pset_external (plist, "extern_2b.raw", 10,
			      (hsize_t)sizeof(part));
    assert (status>=0);
    status = H5Pset_external (plist, "extern_3b.raw", 20,
			      (hsize_t)sizeof(part));
    assert (status>=0);
    status = H5Pset_external (plist, "extern_4b.raw", 30,
			      H5F_UNLIMITED);
    assert (status>=0);

    /* Make sure the output files are fresh*/
    fd = open ("extern_1b.raw", O_RDWR|O_CREAT|O_TRUNC, 0666);
    close (fd);
    fd = open ("extern_2b.raw", O_RDWR|O_CREAT|O_TRUNC, 0666);
    close (fd);
    fd = open ("extern_3b.raw", O_RDWR|O_CREAT|O_TRUNC, 0666);
    close (fd);
    fd = open ("extern_4b.raw", O_RDWR|O_CREAT|O_TRUNC, 0666);
    close (fd);

    /* Create the data space */
    mem_space = H5Screate_simple (1, &size, &max_size);
    assert (mem_space>=0);
    file_space = H5Scopy (mem_space);

    /* Create the dataset */
    dset = H5Dcreate (file, "dset1", H5T_NATIVE_INT, file_space, plist);
    assert (dset>=0);

    /*
     * Write the entire dataset and compare with the original
     */
    do {
	printf ("%-70s", "...writing entire dataset");
	fflush (stdout);

	for (i=0; i<size; i++) whole[i] = i;
	status = H5Dwrite (dset, H5T_NATIVE_INT, mem_space, file_space,
			   H5P_DEFAULT, whole);
	if (status<0) break;
	for (i=0; i<4; i++) {
	    char name1[64], name2[64];
	    sprintf (name1, "extern_%d.raw", i+1);
	    sprintf (name2, "extern_%db.raw", i+1);
	    if (!same_contents (name1, name2)) {
		puts ("*FAIL*");
		puts ("   Output differs from expected value.");
		nerrors_g++;
		break;
	    }
	}
	puts (" PASSED");
    } while (0);

    /*
     * Extend the dataset by another 100 elements
     */
    do {
	printf ("%-70s", "...extending external contiguous dataset");
	fflush (stdout);

	if (H5Dextend (dset, &max_size)<0) break;
	H5Sclose (file_space);
	file_space = H5Dget_space (dset);
	puts (" PASSED");
    } while (0);

    /*
     * Write second half of dataset
     */
    do {
	printf ("%-70s", "...writing to extended part of dataset");
	fflush (stdout);

	for (i=0; i<hs_count; i++) {
	    whole[i] = 100+i;
	}
	status = H5Sselect_hyperslab (file_space, H5S_SELECT_SET, &hs_start, NULL, &hs_count, NULL);
	assert (status>=0);
	status = H5Dwrite (dset, H5T_NATIVE_INT, mem_space, file_space,
			   H5P_DEFAULT, whole);
	if (status<0) break;
	puts (" PASSED");
    } while (0);
    


    H5Dclose (dset);
    H5Pclose (plist);
    H5Sclose (mem_space);
    H5Sclose (file_space);
    H5Fclose (file);
}



/*-------------------------------------------------------------------------
 * Function:	cleanup
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
 *              May 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
cleanup(void)
{
    if (!getenv ("HDF5_NOCLEANUP")) {
	remove(TEST_FILE_NAME1);
	remove(TEST_FILE_NAME2);
	remove(TEST_FILE_NAME3);
	/* not sure if the following file names can be #defined */
	/* because some of them are created during runtime. */
	/* List them out this way for now. */
	remove("extern_1.raw");
	remove("extern_1b.raw");
	remove("extern_2.raw");
	remove("extern_2b.raw");
	remove("extern_3.raw");
	remove("extern_3b.raw");
	remove("extern_4.raw");
	remove("extern_4b.raw");
    }
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
    H5Eset_auto (display_error_cb, NULL);

    test_1 ();
    test_2 ();
    test_3 ();

    if (0==nerrors_g) {
	printf ("All external storage tests passed.\n");
	cleanup();
    } else {
	printf ("%d TEST%s FAILED.\n", nerrors_g, 1==nerrors_g?"":"s");
    }

    return (nerrors_g?1:0);
}
