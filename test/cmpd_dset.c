/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, January 23, 1998
 */
#undef NDEBUG
#include <assert.h>
#include <hdf5.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TEST_FILE_NAME	"cmpd_dset.h5"

/* The first dataset */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    unsigned int c;
    unsigned int d;
    unsigned int e;
} s1_t;

/* The second dataset (same as first) */
typedef s1_t s2_t;

/* The third dataset (reversed fields of s1) */
typedef struct s3_t {
    unsigned int e;
    unsigned int d;
    unsigned int c;
    unsigned int b;
    unsigned int a;
} s3_t;

/* The fourth dataset (a subset of s1) */
typedef struct s4_t {
    unsigned int b;
    unsigned int d;
} s4_t;

/* The fifth dataset (a superset of s1) */
typedef struct s5_t {
    unsigned int pre;
    unsigned int a;
    unsigned int b;
    unsigned int mid1;
    unsigned int c;
    unsigned int mid2;
    unsigned int d;
    unsigned int e;
    unsigned int post;
} s5_t;


#if 1
#  define NX	100u
#  define NY	2000u
#else
#  define NX	12u
#  define NY    9u
#endif


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
	remove(TEST_FILE_NAME);
    }
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Creates a simple dataset of a compound type and then reads
 *		it back.  The dataset is read back in various ways to
 *		exercise the I/O pipeline and compound type conversion.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Robb Matzke
 *              Friday, January 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (void)
{
    /* First dataset */
    static s1_t		s1[NX*NY];
    hid_t		s1_tid;

    /* Second dataset */
    static s2_t		s2[NX*NY];
    hid_t		s2_tid;

    /* Third dataset */
    static s3_t		s3[NX*NY];
    hid_t		s3_tid;
    
    /* Fourth dataset */
    static s4_t		s4[NX*NY];
    hid_t		s4_tid;
    
    /* Fifth dataset */
    static s5_t		s5[NX*NY];
    hid_t		s5_tid;

    /* Sixth dataset */

    /* Seventh dataset */
    hid_t		s7_sid;

    /* Eighth dataset */
    s1_t		*s8 = NULL;
    hid_t		s8_f_sid;	/*file data space		*/
    hid_t		s8_m_sid;	/*memory data space		*/

    /* Ninth dataset */

    /* Tenth dataset */

    /* Eleventh dataset */
    s4_t		*s11 = NULL;

    /* Other variables */
    unsigned int	i, j;
    int			ndims;
    hid_t		file, dataset, space, PRESERVE;
    herr_t		status;
    static hsize_t	dim[] = {NX, NY};
    hssize_t 		f_offset[2];	/*offset of hyperslab in file	*/
    hsize_t 		h_size[2];	/*size of hyperslab		*/
    hsize_t 		h_sample[2];	/*hyperslab sampling		*/

    /* Create the file */
    file = H5Fcreate (TEST_FILE_NAME, H5F_ACC_TRUNC|H5F_ACC_DEBUG,
		      H5P_DEFAULT, H5P_DEFAULT);
    assert (file>=0);

    /* Create the data space */
    space = H5Screate_simple (2, dim, NULL);
    assert (space>=0);

    /* Create xfer properties to preserve initialized data */
    PRESERVE = H5Pcreate (H5P_DATASET_XFER);
    assert (PRESERVE>=0);
    status = H5Pset_preserve (PRESERVE, 1);
    assert (status>=0);

    /*
     *######################################################################
     * STEP 1: Save the original dataset natively.
     */
    printf ("\
STEP  1: Initialize dataset `s1' and store it on disk in native order.\n");
    fflush (stdout);
    
    /* Initialize the dataset */
    for (i=0; i<NX*NY; i++) {
	s1[i].a = 5*i+0;
	s1[i].b = 2000*2*i;
	s1[i].c = 5*i+2;
	s1[i].d = 2001+2*i;
	s1[i].e = 5*i+4;
    }

    /* Create the memory data type */
    s1_tid = H5Tcreate (H5T_COMPOUND, sizeof(s1_t));
    H5Tinsert (s1_tid, "a", HPOFFSET(s1,a), H5T_NATIVE_INT);
    H5Tinsert (s1_tid, "b", HPOFFSET(s1,b), H5T_NATIVE_INT);
    H5Tinsert (s1_tid, "c", HPOFFSET(s1,c), H5T_NATIVE_INT);
    H5Tinsert (s1_tid, "d", HPOFFSET(s1,d), H5T_NATIVE_INT);
    H5Tinsert (s1_tid, "e", HPOFFSET(s1,e), H5T_NATIVE_INT);
    assert (s1_tid>=0);

    /* Create the dataset */
    dataset = H5Dcreate (file, "s1", s1_tid, space, H5P_DEFAULT);
    assert (dataset>=0);

    /* Write the data */
    status = H5Dwrite (dataset, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1);
    assert (status>=0);

    /*
     *######################################################################
     * STEP 2: We create a new type ID for the second dataset even though
     * 	       it's the same as the first just to test things better, but
     *	       in fact, we could have used s1_tid.
     */
    printf ("\
STEP  2: Read the dataset from disk into a new memory buffer which has the\n\
         same data type and space. This will be the typical case.\n");
    fflush (stdout);
    

    /* Create a data type for s2 */
    s2_tid = H5Tcreate (H5T_COMPOUND, sizeof(s2_t));
    H5Tinsert (s2_tid, "a", HPOFFSET(s2,a), H5T_NATIVE_INT);
    H5Tinsert (s2_tid, "b", HPOFFSET(s2,b), H5T_NATIVE_INT);
    H5Tinsert (s2_tid, "c", HPOFFSET(s2,c), H5T_NATIVE_INT);
    H5Tinsert (s2_tid, "d", HPOFFSET(s2,d), H5T_NATIVE_INT);
    H5Tinsert (s2_tid, "e", HPOFFSET(s2,e), H5T_NATIVE_INT);
    assert (s2_tid>=0);
    
    /* Read the data */
    status = H5Dread (dataset, s2_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s2);
    assert (status>=0);

    /* Compare s2 with s1.  They should be the same */
    for (i=0; i<NX*NY; i++) {
	assert (s1[i].a==s2[i].a);
	assert (s1[i].b==s2[i].b);
	assert (s1[i].c==s2[i].c);
	assert (s1[i].d==s2[i].d);
	assert (s1[i].e==s2[i].e);
    }
    
    /*
     *######################################################################
     * STEP 3: Read the dataset back into a third memory buffer. This buffer
     * 	       has the same data space but the data type is different: the
     *	       data type is a struct whose members are in the opposite order.
     */
    printf ("\
STEP  3: Read the dataset again with members in a different order.\n");
    fflush (stdout);
    
    /* Create a data type for s3 */
    s3_tid = H5Tcreate (H5T_COMPOUND, sizeof(s3_t));
    H5Tinsert (s3_tid, "a", HPOFFSET(s3,a), H5T_NATIVE_INT);
    H5Tinsert (s3_tid, "b", HPOFFSET(s3,b), H5T_NATIVE_INT);
    H5Tinsert (s3_tid, "c", HPOFFSET(s3,c), H5T_NATIVE_INT);
    H5Tinsert (s3_tid, "d", HPOFFSET(s3,d), H5T_NATIVE_INT);
    H5Tinsert (s3_tid, "e", HPOFFSET(s3,e), H5T_NATIVE_INT);
    assert (s3_tid>=0);
    
    /* Read the data */
    status = H5Dread (dataset, s3_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s3);
    assert (status>=0);

    /* Compare s3 with s1.  They should be the same */
    for (i=0; i<NX*NY; i++) {
	assert (s1[i].a==s3[i].a);
	assert (s1[i].b==s3[i].b);
	assert (s1[i].c==s3[i].c);
	assert (s1[i].d==s3[i].d);
	assert (s1[i].e==s3[i].e);
    }

    /*
     *######################################################################
     * STEP 4: Read a subset of the members.  Of the <a,b,c,d,e> members
     *         stored on disk we'll read <b,d>.
     */
    printf ("\
STEP  4: Read a subset of the members.\n");
    fflush (stdout);

    /* Create a datatype for s4 */
    s4_tid = H5Tcreate (H5T_COMPOUND, sizeof(s4_t));
    H5Tinsert (s4_tid, "b", HPOFFSET(s4,b), H5T_NATIVE_INT);
    H5Tinsert (s4_tid, "d", HPOFFSET(s4,d), H5T_NATIVE_INT);
    assert (s4_tid>=0);

    /* Read the data */
    status = H5Dread (dataset, s4_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s4);
    assert (status>=0);

    /* Compare s4 with s1 */
    for (i=0; i<NX*NY; i++) {
	assert (s1[i].b==s4[i].b);
	assert (s1[i].d==s4[i].d);
    }

    /*
     *######################################################################
     * STEP 5: Read all the members into a struct which has other members
     * 	       which have already been initialized.
     */
    printf ("\
STEP  5: Read members into a superset which is partially initialized.\n");
    fflush (stdout);

    /* Initialize some members */
    for (i=0; i<NX*NY; i++) {
	s5[i].pre =  1000+4*i;
	s5[i].mid1 = 1001+4*i;
	s5[i].mid2 = 1002+4*i;
	s5[i].post = 1003+4*i;
    }
    
    /* Create a data type for s5 */
    s5_tid = H5Tcreate (H5T_COMPOUND, sizeof(s5_t));
    H5Tinsert (s5_tid, "a", HPOFFSET(s5,a), H5T_NATIVE_INT);
    H5Tinsert (s5_tid, "b", HPOFFSET(s5,b), H5T_NATIVE_INT);
    H5Tinsert (s5_tid, "c", HPOFFSET(s5,c), H5T_NATIVE_INT);
    H5Tinsert (s5_tid, "d", HPOFFSET(s5,d), H5T_NATIVE_INT);
    H5Tinsert (s5_tid, "e", HPOFFSET(s5,e), H5T_NATIVE_INT);
    assert (s5_tid>=0);
	
    /* Read the data */
    status = H5Dread (dataset, s5_tid, H5S_ALL, H5S_ALL, PRESERVE, s5);
    assert (status>=0);

    /* Check that the data was read properly */
    for (i=0; i<NX*NY; i++) {
	assert (s1[i].a==s5[i].a);
	assert (s1[i].b==s5[i].b);
	assert (s1[i].c==s5[i].c);
	assert (s1[i].d==s5[i].d);
	assert (s1[i].e==s5[i].e);
    }

    /* Check that no previous values were clobbered */
    for (i=0; i<NX*NY; i++) {
	assert (s5[i].pre  == 1000+4*i);
	assert (s5[i].mid1 == 1001+4*i);
	assert (s5[i].mid2 == 1002+4*i);
	assert (s5[i].post == 1003+4*i);
    }

    /*
     *######################################################################
     * STEP 6: Update fields `b' and `d' on the file leaving the other
     *         fields unchanged.  This tests member alignment and background
     *	       buffers.
     */
    printf ("\
STEP  6: Update fields `b' and `d' on the file, leaving the other fields\n\
         unchanged.\n");
    fflush (stdout);

    /* Initialize `s4' with new values */
    for (i=0; i<NX*NY; i++) {
	s4[i].b = 5*i+1;
	s4[i].d = 5*i+3;
    }

    /* Write the data to file */
    status = H5Dwrite (dataset, s4_tid, H5S_ALL, H5S_ALL, PRESERVE, s4);
    assert (status>=0);
    
    /* Read the data back */
    status = H5Dread (dataset, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1);
    assert (status>=0);

    /* Compare */
    for (i=0; i<NX*NY; i++) {
	assert (s1[i].a == 5*i+0);
	assert (s1[i].b == 5*i+1);
	assert (s1[i].c == 5*i+2);
	assert (s1[i].d == 5*i+3);
	assert (s1[i].e == 5*i+4);
    }
    
    /*
     *######################################################################
     * STEP 7. Read the original dataset with an explicit data space.  Even
     * though these data spaces are equal it tests a different part of the
     * library.
     */
    printf ("\
STEP  7: Reading original dataset with explicit data space.\n");
    fflush (stdout);

    /* Create the data space */
    s7_sid = H5Screate_simple (2, dim, NULL);
    assert (s7_sid>=0);
    
    /* Read the dataset */
    status = H5Dread (dataset, s2_tid, s7_sid, H5S_ALL, H5P_DEFAULT, s2);
    assert (status>=0);

    /* Compare */
    for (i=0; i<NX*NY; i++) {
	assert (s2[i].a == s1[i].a);
	assert (s2[i].b == s1[i].b);
	assert (s2[i].c == s1[i].c);
	assert (s2[i].d == s1[i].d);
	assert (s2[i].e == s1[i].e);
    }
    

    /*
     *######################################################################
     * STEP 8. Read a hyperslab of the file into a complete array in memory.
     * The hyperslab is the middle third of the array.
     */
    printf ("\
STEP  8: Read middle third hyperslab into memory array.\n");
    fflush (stdout);

    /* Create the file data space */
    s8_f_sid = H5Dget_space (dataset);
    assert (s8_f_sid>=0);
    f_offset[0] = NX/3;
    f_offset[1] = NY/3;
    h_size[0] = 2*NX/3 - f_offset[0];
    h_size[1] = 2*NY/3 - f_offset[1];
    h_sample[0] = 1;
    h_sample[1] = 1;
    status = H5Sset_hyperslab (s8_f_sid, f_offset, h_size, h_sample);
    assert (status>=0);

    /* Create memory data space */
    s8_m_sid = H5Screate_simple (2, h_size, NULL);
    assert (s8_m_sid>=0);

    /* Read the dataset */
    s8 = calloc ((size_t)(h_size[0]*h_size[1]), sizeof(s1_t));
    assert (s8);
    status = H5Dread (dataset, s1_tid, s8_m_sid, s8_f_sid, H5P_DEFAULT, s8);
    assert (status>=0);

    /* Compare */
    for (i=0; i<h_size[0]; i++) {
	for (j=0; j<h_size[1]; j++) {
	    s1_t *ps1 = s1 + (f_offset[0]+i)*NY + f_offset[1] + j;
	    s1_t *ps8 = s8 + i*h_size[1] + j;

	    assert (ps8->a == ps1->a);
	    assert (ps8->b == ps1->b);
	    assert (ps8->c == ps1->c);
	    assert (ps8->d == ps1->d);
	    assert (ps8->e == ps1->e);
	}
    }

    free (s8);
    s8 = NULL;


    /*
     *######################################################################
     * STEP 9.  Read a hyperslab of the file into a hyperslab of memory.  The
     * part of memory not read is already initialized and must not change.
     */
    printf ("\
STEP  9: Read middle third of hyperslab into middle third of memory array.\n");
    fflush (stdout);

    /* Initialize */
    for (i=0; i<NX*NY; i++) {
	s2[i].a = s2[i].b = s2[i].c = s2[i].d = s2[i].e = (unsigned)(-1);
    }
    
    /* Read the hyperslab */
    status = H5Dread (dataset, s2_tid, s8_f_sid, s8_f_sid, H5P_DEFAULT, s2);
    assert (status>=0);

    /* Compare */
    for (i=0; i<NX; i++) {
	for (j=0; j<NY; j++) {
	    s1_t *ps1 = s1 + i*NY + j;
	    s2_t *ps2 = s2 + i*NY + j;
	    if ((hssize_t)i>=f_offset[0] &&
		(hsize_t)i<f_offset[0]+h_size[0] &&
		(hssize_t)j>=f_offset[1] &&
		(hsize_t)j<f_offset[1]+h_size[1]) {
		assert (ps2->a == ps1->a);
		assert (ps2->b == ps1->b);
		assert (ps2->c == ps1->c);
		assert (ps2->d == ps1->d);
		assert (ps2->e == ps1->e);
	    } else {
		assert (ps2->a == (unsigned)(-1));
		assert (ps2->b == (unsigned)(-1));
		assert (ps2->c == (unsigned)(-1));
		assert (ps2->d == (unsigned)(-1));
		assert (ps2->e == (unsigned)(-1));
	    }
	}
    }
    
    /*
     *######################################################################
     * STEP 10. Same as step 9 except the memory array contains some members
     * which are already initialized, like step 5.
     */
    printf ("\
STEP 10: Read middle third of hyperslab into middle third of memory array\n\
         where some of the struct members are already initialized.\n");
    fflush (stdout);

    /* Initialize */
    for (i=0; i<NX*NY; i++) {
	s5[i].a = s5[i].b = s5[i].c = s5[i].d = s5[i].e = (unsigned)(-1);
	s5[i].pre = s5[i].mid1 = s5[i].mid2 = s5[i].post = (unsigned)(-1);
    }
    
    /* Read the hyperslab */
    status = H5Dread (dataset, s5_tid, s8_f_sid, s8_f_sid, PRESERVE, s5);
    assert (status>=0);

    /* Compare */
    for (i=0; i<NX; i++) {
	for (j=0; j<NY; j++) {
	    s1_t *ps1 = s1 + i*NY + j;
	    s5_t *ps5 = s5 + i*NY + j;
	    if ((hssize_t)i>=f_offset[0] &&
		(hsize_t)i<f_offset[0]+h_size[0] &&
		(hssize_t)j>=f_offset[1] &&
		(hsize_t)j<f_offset[1]+h_size[1]) {
		assert (ps5->pre == (unsigned)(-1));
		assert (ps5->a == ps1->a);
		assert (ps5->b == ps1->b);
		assert (ps5->mid1 == (unsigned)(-1));
		assert (ps5->c == ps1->c);
		assert (ps5->mid2 == (unsigned)(-1));
		assert (ps5->d == ps1->d);
		assert (ps5->e == ps1->e);
		assert (ps5->post == (unsigned)(-1));
	    } else {
		assert (ps5->pre == (unsigned)(-1));
		assert (ps5->a == (unsigned)(-1));
		assert (ps5->b == (unsigned)(-1));
		assert (ps5->mid1 == (unsigned)(-1));
		assert (ps5->c == (unsigned)(-1));
		assert (ps5->mid2 == (unsigned)(-1));
		assert (ps5->d == (unsigned)(-1));
		assert (ps5->e == (unsigned)(-1));
		assert (ps5->post == (unsigned)(-1));
	    }
	}
    }
		
    /*
     *######################################################################
     * Step 11: Write an array into the middle third of the dataset
     * initializeing only members `b' and `d' to -1.
     */
    printf ("\
STEP 11: Write an array back to the middle third of the dataset to\n\
         initialize the `b' and `d' members to -1.\n");
    fflush (stdout);
    
    /* Create the memory array and initialize all fields to zero */
    ndims = H5Sget_hyperslab (s8_f_sid, f_offset, h_size, h_sample);
    assert (ndims==2);
    s11 = malloc ((size_t)h_size[0]*(size_t)h_size[1]*sizeof(s4_t));
    assert (s11);

    /* Initialize */
    for (i=0; i<h_size[0]*h_size[1]; i++) {
	s11[i].b = s11[i].d = (unsigned)(-1);
    }
    
    /* Write to disk */
    status = H5Dwrite (dataset, s4_tid, s8_m_sid, s8_f_sid, PRESERVE, s11);
    assert (status>=0);
    free (s11);
    s11=NULL;

    /* Read the whole thing */
    status = H5Dread (dataset, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1);
    assert (status>=0);

    /* Compare */
    for (i=0; i<NX; i++) {
	for (j=0; j<NY; j++) {
	    s1_t *ps1 = s1 + i*NY + j;
	    
	    assert (ps1->a == 5*(i*NY+j)+0);
	    assert (ps1->c == 5*(i*NY+j)+2);
	    assert (ps1->e == 5*(i*NY+j)+4);
	    if ((hssize_t)i>=f_offset[0] &&
		(hsize_t)i<f_offset[0]+h_size[0] &&
		(hssize_t)j>=f_offset[1] &&
		(hsize_t)j<f_offset[1]+h_size[1]) {
		assert (ps1->b == (unsigned)(-1));
		assert (ps1->d == (unsigned)(-1));
	    } else {
		assert (ps1->b == 5*(i*NY+j)+1);
		assert (ps1->d == 5*(i*NY+j)+3);
	    }
	}
    }
    









    
    /*
     * Release resources.
     */
    H5Pclose (PRESERVE);
    H5Dclose (dataset);
    H5Fclose (file);

    cleanup();
    return 0;
}
