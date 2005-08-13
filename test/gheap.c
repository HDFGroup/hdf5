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
 *              Tuesday, March 31, 1998
 *
 * Purpose:	Tests the global heap.  The global heap is the set of all
 *		collections but the collections are not related to one
 *		another by anything that appears in the file format.
 */
#include "h5test.h"
#include "H5private.h"
#include "H5Eprivate.h"
#include "H5Fprivate.h"
#include "H5Gprivate.h"
#include "H5HGprivate.h"
#include "H5Iprivate.h"
#include "H5Pprivate.h"

const char *FILENAME[] = {
    "gheap1",
    "gheap2",
    "gheap3",
    "gheap4",
    NULL
};


/*-------------------------------------------------------------------------
 * Function:	test_1
 *
 * Purpose:	Writes a sequence of objects to the global heap where each
 *		object is larger than the one before.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_1 (hid_t fapl)
{
    hid_t	file=-1;
    H5F_t 	*f=NULL;
    H5HG_t	obj[1024];
    uint8_t	out[1024];
    uint8_t	in[1024];
    int		i;
    size_t	size;
    herr_t	status;
    int		nerrors=0;
    char	filename[1024];

    TESTING("monotonically increasing lengths");

    /* Open a clean file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	goto error;
    if (NULL==(f=H5I_object(file))) {
	H5_FAILED();
	puts("    Unable to create file");
	goto error;
    }

    /*
     * Write the objects, monotonically increasing in length.  Since this is
     * a clean file, the addresses allocated for the collections should also
     * be monotonically increasing.
     */
    for (i=0; i<1024; i++) {
	size = i+1;
	memset (out, 'A'+i%26, size);
	H5Eclear ();
	status = H5HG_insert (f, H5P_DATASET_XFER_DEFAULT, size, out, obj+i);
	if (status<0) {
	    H5_FAILED();
	    puts("    Unable to insert object into global heap");
	    nerrors++;
	} else if (i && H5F_addr_gt (obj[i-1].addr, obj[i].addr)) {
	    H5_FAILED();
	    puts("    Collection addresses are not monotonically increasing");
	    nerrors++;
	}
    }

    /*
     * Now try to read each object back.
     */
    for (i=0; i<1024; i++) {
	size = i+1;
	memset (out, 'A'+i%26, size);
	H5Eclear ();
	if (NULL==H5HG_read (f, H5P_DATASET_XFER_DEFAULT, obj+i, in)) {
	    H5_FAILED();
	    puts("    Unable to read object");
	    nerrors++;
	} else if (memcmp (in, out, size)) {
	    H5_FAILED();
	    puts("    Value read doesn't match value written");
	    nerrors++;
	}
    }

    if (H5Fclose(file)<0) goto error;
    if (nerrors) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return MAX(1, nerrors);
}


/*-------------------------------------------------------------------------
 * Function:	test_2
 *
 * Purpose:	Writes a sequence of objects to the global heap where each
 *		object is smaller than the one before.
 *
 * Return:	Success:	0
 *
 *		Failure:        number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_2 (hid_t fapl)
{
    hid_t	file=-1;
    H5F_t 	*f=NULL;
    H5HG_t	obj[1024];
    uint8_t	out[1024];
    uint8_t	in[1024];
    int		i;
    size_t	size;
    int		nerrors=0;
    char	filename[1024];

    TESTING("monotonically decreasing lengths");

    /* Open a clean file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	goto error;
    if (NULL==(f=H5I_object(file))) {
	H5_FAILED();
	puts("    Unable to create file");
	goto error;
    }

    /*
     * Write the objects, monotonically decreasing in length.
     */
    for (i=0; i<1024; i++) {
	size = 1024-i;
	memset (out, 'A'+i%26, size);
	H5Eclear ();
	if (H5HG_insert (f, H5P_DATASET_XFER_DEFAULT, size, out, obj+i)<0) {
	    H5_FAILED();
	    puts("    Unable to insert object into global heap");
	    nerrors++;
	}
    }

    /*
     * Now try to read each object back.
     */
    for (i=0; i<1024; i++) {
	size = 1024-i;
	memset (out, 'A'+i%26, size);
	H5Eclear ();
	if (NULL==H5HG_read (f, H5P_DATASET_XFER_DEFAULT, obj+i, in)) {
	    H5_FAILED();
	    puts("    Unable to read object");
	    nerrors++;
	} else if (memcmp (in, out, size)) {
	    H5_FAILED();
	    puts("    Value read doesn't match value written");
	    nerrors++;
	}
    }

    if (H5Fclose(file)<0) goto error;
    if (nerrors) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return MAX(1, nerrors);
}


/*-------------------------------------------------------------------------
 * Function:	test_3
 *
 * Purpose:	Creates a few global heap objects and then removes them all.
 *		The collection should also be removed.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_3 (hid_t fapl)
{
    hid_t	file=-1;
    H5F_t 	*f=NULL;
    H5HG_t	obj[1024];
    uint8_t	out[1024];
    int		i;
    size_t	size;
    herr_t	status;
    int		nerrors=0;
    char	filename[1024];

    TESTING("complete object removal");

    /* Open a clean file */
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	goto error;
    if (NULL==(f=H5I_object(file))) {
	H5_FAILED();
	puts("    Unable to create file");
	goto error;
    }

    /* Create some stuff */
    for (i=0; i<1024; i++) {
	size = i%30+100;
	memset (out, 'A'+i%26, size);
	H5Eclear ();
	status = H5HG_insert (f, H5P_DATASET_XFER_DEFAULT, size, out, obj+i);
	if (status<0) {
	    H5_FAILED();
	    puts("    Unable to insert object into global heap");
	    nerrors++;
	}
    }

    /* Remove everything */
    for (i=0; i<1024; i++) {
	status = H5HG_remove (f, H5P_DATASET_XFER_DEFAULT, obj+i);
	if (status<0) {
	    H5_FAILED();
	    puts("    Unable to remove object");
	    nerrors++;
	}
    }

    if (H5Fclose(file)<0) goto error;
    if (nerrors) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return MAX(1, nerrors);
}


/*-------------------------------------------------------------------------
 * Function:	test_4
 *
 * Purpose:	Tests the H5HG_remove() feature by writing lots of objects
 *		and occassionally removing some.  When we're done they're all
 *		removed.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_4 (hid_t fapl)
{
    hid_t	file=-1;
    H5F_t 	*f=NULL;
    H5HG_t	obj[1024];
    uint8_t	out[1024];
    int		i;
    size_t	size;
    herr_t	status;
    int		nerrors=0;
    char	filename[1024];

    TESTING("partial object removal");

    /* Open a clean file */
    h5_fixname(FILENAME[3], fapl, filename, sizeof filename);
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
	goto error;
    if (NULL==(f=H5I_object(file))) {
	H5_FAILED();
	puts("    Unable to create file");
	goto error;
    }

    for (i=0; i<1024; i++) {
	/* Insert */
	size = i%30+100;
	memset (out, 'A'+i%26, size);
	H5Eclear ();
	status = H5HG_insert (f, H5P_DATASET_XFER_DEFAULT, size, out, obj+i);
	if (status<0) {
	    H5_FAILED();
	    puts("    Unable to insert object into global heap");
	    nerrors++;
	}

	/*
	 * Remove every third one beginning with the second, but after the
	 * next one has already been inserted.  That is, insert A, B, C;
	 * remove B, insert D, E, F; remove E; etc.
	 */
	if (1==i%3) {
	    H5Eclear ();
	    status = H5HG_remove (f, H5P_DATASET_XFER_DEFAULT, obj+i-1);
	    if (status<0) {
		H5_FAILED();
		puts("    Unable to remove object");
		nerrors++;
	    }
	    memset (obj+i-1, 0, sizeof *obj);
	}
    }

    if (H5Fclose(file)<0) goto error;
    if (nerrors) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return MAX(1, nerrors);
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests global heap.
 *
 * Return:	Success:	zero
 *
 *		Failure:	non-zero
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (void)
{
    int		nerrors=0;
    hid_t	fapl;

    h5_reset();
    fapl = h5_fileaccess();

    nerrors += test_1(fapl);
    nerrors += test_2(fapl);
    nerrors += test_3(fapl);
    nerrors += test_4(fapl);
    if (nerrors) goto error;

    puts("All global heap tests passed.");
    h5_cleanup(FILENAME, fapl);
    return 0;

 error:
    puts("*** TESTS FAILED ***");
    return 1;
}
