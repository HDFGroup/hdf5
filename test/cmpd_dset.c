/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, January 23, 1998
 */
#include <assert.h>
#include <stdio.h>
#include <hdf5.h>

/* The first dataset */
typedef struct s1_t {
    int		a;
    int		b;
    int		c;
    int		d;
    int		e;
} s1_t;

/* The second dataset (same as first) */
typedef s1_t s2_t;

/* The third dataset (reversed fields of s1) */
typedef struct s3_t {
    int		e;
    int		d;
    int		c;
    int		b;
    int		a;
} s3_t;

/* The fourth dataset (a subset of s1) */
typedef struct s4_t {
    int		b;
    int		d;
} s4_t;

/* The fifth dataset (a superset of s1) */
typedef struct s5_t {
    int		pre;
    int		a;
    int		b;
    int		mid1;
    int		c;
    int		mid2;
    int		d;
    int		e;
    int		post;
} s5_t;


#define NX	100
#define NY	2000


static hid_t
H5Pcreate_simple (int ndims, size_t *dim)
{
    herr_t status;
    hid_t pid = H5Pcreate (H5P_SIMPLE);
    assert (pid>=0);
    status = H5Pset_space (pid, ndims, dim);
    assert (status>=0);
    return pid;
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

    /* Other variables */
    int			i;
    hid_t		file, dataset, space;
    herr_t		status;
    static size_t	dim[] = {NX, NY};

    /* Create the file */
    file = H5Fcreate ("cmpd_dset.h5", H5ACC_OVERWRITE,
		      H5C_DEFAULT, H5C_DEFAULT);
    assert (file>=0);

    /* Create the data space */
    space = H5Pcreate_simple (2, dim);
    assert (space>=0);

    

    /*
     *######################################################################
     * STEP 1: Save the original dataset natively.
     */
    printf ("\
STEP 1: Initialize dataset `s1' and store it on disk in native order.\n");
    fflush (stdout);
    
    /* Initialize the dataset */
    for (i=0; i<NX*NY; i++) {
	s1[i].a = 5*i+0;
	s1[i].b = 5*i+1;
	s1[i].c = 5*i+2;
	s1[i].d = 5*i+3;
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
    dataset = H5Dcreate (file, "s1", s1_tid, space, H5C_DEFAULT);
    assert (dataset>=0);

    /* Write the data */
    status = H5Dwrite (dataset, s1_tid, H5P_ALL, H5P_ALL, H5C_DEFAULT, s1);
    assert (status>=0);

    /*
     *######################################################################
     * STEP 2: We create a new type ID for the second dataset even though
     * 	       it's the same as the first just to test things better, but
     *	       in fact, we could have used s1_tid.
     */
    printf ("\
STEP 2: Read the dataset from disk into a new memory buffer which has the\n\
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
    status = H5Dread (dataset, s2_tid, H5P_ALL, H5P_ALL, H5C_DEFAULT, s2);
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
STEP 3: Read the dataset again with members in a different order.\n");
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
    status = H5Dread (dataset, s3_tid, H5P_ALL, H5P_ALL, H5C_DEFAULT, s3);
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
STEP 4: Read a subset of the members.\n");
    fflush (stdout);

    /* Create a datatype for s4 */
    s4_tid = H5Tcreate (H5T_COMPOUND, sizeof(s4_t));
    H5Tinsert (s4_tid, "b", HPOFFSET(s4,b), H5T_NATIVE_INT);
    H5Tinsert (s4_tid, "d", HPOFFSET(s4,d), H5T_NATIVE_INT);
    assert (s4_tid>=0);

    /* Read the data */
    status = H5Dread (dataset, s4_tid, H5P_ALL, H5P_ALL, H5C_DEFAULT, s4);
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
STEP 5: Read members into a superset which is partially initialized.\n");
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
    status = H5Dread (dataset, s5_tid, H5P_ALL, H5P_ALL, H5C_DEFAULT, s5);
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
STEP 6: Update fields `b' and `d' on the file, leaving the other fields\n\
        unchanged.\n");
    fflush (stdout);

    /* Initialize `s4' with new values */
    for (i=0; i<NX*NY; i++) {
	s4[i].b = 2000+2*i;
	s4[i].d = 2001+2*i;
    }

    /* Write the data to file */
    status = H5Dwrite (dataset, s4_tid, H5P_ALL, H5P_ALL, H5C_DEFAULT, s4);
    assert (status>=0);
    
    /* Read the data back */
    status = H5Dread (dataset, s2_tid, H5P_ALL, H5P_ALL, H5C_DEFAULT, s2);
    assert (status>=0);

    /* Compare */
    for (i=0; i<NX*NY; i++) {
	assert (s2[i].a == s1[i].a);
	assert (s2[i].b == s4[i].b);
	assert (s2[i].c == s1[i].c);
	assert (s2[i].d == s4[i].d);
	assert (s2[i].e == s1[i].e);
    }
    





    

    /*
     * Release resources.
     */
    H5Dclose (dataset);
    H5Fclose (file);

    exit (0);
}
