/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, March 31, 1998
 *
 * Purpose:	Tests the global heap.  The global heap is the set of all
 *		collections but the collections are not related to one
 *		another by anything that appears in the file format.
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5Gprivate.h>
#include <H5HGprivate.h>
#include <H5Pprivate.h>

#ifndef HAVE_FUNCTION
#   undef __FUNCTION__
#   define __FUNCTION__ ""
#endif

#define TEST_FILE_NAME0		"gheap0.h5"
#define TEST_FILE_NAME1		"gheap1.h5"
#define TEST_FILE_NAME2		"gheap2.h5"
#define TEST_FILE_NAME3		"gheap3.h5"
#define TEST_FILE_NAME4		"gheap4.h5"

#define FAILED(S) {							      \
    puts ("*FAILED*");							      \
    printf ("    Failed at %s:%d in %s()%s%s\n",			      \
	    __FILE__, __LINE__, __FUNCTION__,				      \
	    (S)&&*(S)?": ":"", (S)?(S):"");				      \
    H5Eprint (stdout);							      \
}


/*-------------------------------------------------------------------------
 * Function:	emit_diagnostics
 *
 * Purpose:	If debugging is turned on then this function will cause the
 *		library to emit its diagnostic messages now instead of when
 *		we're trying to make the output look nice.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
emit_diagnostics (void)
{
    H5F_t *f = H5F_open (TEST_FILE_NAME0,
			 H5F_ACC_CREAT|H5F_ACC_RDWR|H5F_ACC_TRUNC,
			 NULL, NULL);
    H5G_t *g = H5G_create (H5G_entof(f->shared->root_grp), "emit", 0);
    H5G_close (g);
    H5F_close (f);
}


/*-------------------------------------------------------------------------
 * Function:	test_1
 *
 * Purpose:	Writes a sequence of objects to the global heap where each
 *		object is larger than the one before.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_1 (void)
{
    H5F_t 	*f;
    H5HG_t	obj[1024];
    uint8	out[1024];
    uint8	in[1024];
    int		i;
    size_t	size;
    herr_t	status;
    int		retval = 0;

    printf ("%-70s", "...monotonically increasing lengths");
    fflush (stdout);

    /* Open a clean file */
    H5Eclear ();
    f = H5F_open (TEST_FILE_NAME1,
		  H5F_ACC_CREAT|H5F_ACC_RDWR|H5F_ACC_TRUNC|H5F_ACC_DEBUG,
		  NULL, NULL);
    if (!f) {
	FAILED ("unable to create file");
	return -1;
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
	status = H5HG_insert (f, size, out, obj+i);
	if (status<0) {
	    FAILED ("unable to insert object into global heap");
	    --retval;
	} else if (i && H5F_addr_gt (&(obj[i-1].addr), &(obj[i].addr))) {
	    FAILED ("collection addresses are not monotonically increasing");
	    --retval;
	}
    }

    /*
     * Now try to read each object back.
     */
    for (i=0; i<1024; i++) {
	size = i+1;
	memset (out, 'A'+i%26, size);
	H5Eclear ();
	if (NULL==H5HG_read (f, obj+i, in)) {
	    FAILED ("unable to read object");
	    --retval;
	} else if (memcmp (in, out, size)) {
	    FAILED ("value read doesn't match value written");
	    --retval;
	}
    }
    
    puts (" PASSED");
    H5F_close (f);
    return retval;
}


/*-------------------------------------------------------------------------
 * Function:	test_2
 *
 * Purpose:	Writes a sequence of objects to the global heap where each
 *		object is smaller than the one before.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_2 (void)
{
    H5F_t 	*f;
    H5HG_t	obj[1024];
    uint8	out[1024];
    uint8	in[1024];
    int		i;
    size_t	size;
    herr_t	status;
    int		retval = 0;

    printf ("%-70s", "...monotonically decreasing lengths");
    fflush (stdout);

    /* Open a clean file */
    H5Eclear ();
    f = H5F_open (TEST_FILE_NAME2,
		  H5F_ACC_CREAT|H5F_ACC_RDWR|H5F_ACC_TRUNC|H5F_ACC_DEBUG,
		  NULL, NULL);
    if (!f) {
	FAILED ("unable to create file");
	return -1;
    }
    
    /*
     * Write the objects, monotonically decreasing in length.
     */
    for (i=0; i<1024; i++) {
	size = 1024-i;
	memset (out, 'A'+i%26, size);
	H5Eclear ();
	status = H5HG_insert (f, size, out, obj+i);
	if (status<0) {
	    FAILED ("unable to insert object into global heap");
	    --retval;
	}
    }

    /*
     * Now try to read each object back.
     */
    for (i=0; i<1024; i++) {
	size = 1024-i;
	memset (out, 'A'+i%26, size);
	H5Eclear ();
	if (NULL==H5HG_read (f, obj+i, in)) {
	    FAILED ("unable to read object");
	    --retval;
	} else if (memcmp (in, out, size)) {
	    FAILED ("value read doesn't match value written");
	    --retval;
	}
    }
    
    puts (" PASSED");
    H5F_close (f);
    return retval;
}


/*-------------------------------------------------------------------------
 * Function:	test_3
 *
 * Purpose:	Creates a few global heap objects and then removes them all.
 *		The collection should also be removed.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_3 (void)
{
    H5F_t 	*f;
    H5HG_t	obj[1024];
    uint8	out[1024];
    int		i;
    size_t	size;
    herr_t	status;
    int		retval = 0;

    printf ("%-70s", "...complete object removal");
    fflush (stdout);

    /* Open a clean file */
    H5Eclear ();
    f = H5F_open (TEST_FILE_NAME3,
		  H5F_ACC_CREAT|H5F_ACC_RDWR|H5F_ACC_TRUNC|H5F_ACC_DEBUG,
		  NULL, NULL);
    if (!f) {
	FAILED ("unable to create file");
	return -1;
    }

    /* Create some stuff */
    for (i=0; i<1024; i++) {
	size = i%30+100;
	memset (out, 'A'+i%26, size);
	H5Eclear ();
	status = H5HG_insert (f, size, out, obj+i);
	if (status<0) {
	    FAILED ("unable to insert object into global heap");
	    --retval;
	}
    }

    /* Remove everything */
    for (i=0; i<1024; i++) {
	status = H5HG_remove (f, obj+i);
	if (status<0) {
	    FAILED ("unable to remove object");
	    --retval;
	}
    }
    
    puts (" PASSED");
    H5F_close (f);
    return retval;
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
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_4 (void)
{
    H5F_t 	*f;
    H5HG_t	obj[1024];
    uint8	out[1024];
    int		i;
    size_t	size;
    herr_t	status;
    int		retval = 0;

    printf ("%-70s", "...partial object removal");
    fflush (stdout);

    /* Open a clean file */
    H5Eclear ();
    f = H5F_open (TEST_FILE_NAME4,
		  H5F_ACC_CREAT|H5F_ACC_RDWR|H5F_ACC_TRUNC|H5F_ACC_DEBUG,
		  NULL, NULL);
    if (!f) {
	FAILED ("unable to create file");
	return -1;
    }


    for (i=0; i<1024; i++) {
	/* Insert */
	size = i%30+100;
	memset (out, 'A'+i%26, size);
	H5Eclear ();
	status = H5HG_insert (f, size, out, obj+i);
	if (status<0) {
	    FAILED ("unable to insert object into global heap");
	    --retval;
	}

	/*
	 * Remove every third one beginning with the second, but after the
	 * next one has already been inserted.  That is, insert A, B, C;
	 * remove B, insert D, E, F; remove E; etc.
	 */ 
	if (1==i%3) {
	    H5Eclear ();
	    status = H5HG_remove (f, obj+i-1);
	    if (status<0) {
		FAILED ("unable to remove object");
		--retval;
	    }
	    memset (obj+i-1, 0, sizeof *obj);
	}
    }

    puts (" PASSED");
    H5F_close (f);
    return retval;
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
	remove(TEST_FILE_NAME0);
	remove(TEST_FILE_NAME1);
	remove(TEST_FILE_NAME2);
	remove(TEST_FILE_NAME3);
	remove(TEST_FILE_NAME4);
    }
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
    int		nfailed=0;

    emit_diagnostics ();
    
    
    nfailed += test_1()<0 ? 1 : 0;
    nfailed += test_2()<0 ? 1 : 0;
    nfailed += test_3()<0 ? 1 : 0;
    nfailed += test_4()<0 ? 1 : 0;

    if (nfailed) {
	printf ("*** %d global heap test%s failed ***\n",
		nfailed, 1==nfailed?"":"s");
    } else {
	printf ("All global heap tests passed.\n");
	cleanup();
    }
    return nfailed?-1:0;
}
