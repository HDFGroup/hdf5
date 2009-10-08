/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, January 23, 1998
 */

/* See H5private.h for how to include headers */
#undef NDEBUG

#define H5T_PACKAGE
#include "H5Tpkg.h"		/*to turn off hardware conversions*/

#include "h5test.h"

const char *FILENAME[] = {
    "cmpd_dset",
    NULL
};

/* The first dataset */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    unsigned int c[4];
    unsigned int d;
    unsigned int e;
} s1_t;

/* The second dataset (same as first) */
typedef s1_t s2_t;

/* The third dataset (reversed fields of s1) */
typedef struct s3_t {
    unsigned int e;
    unsigned int d;
    unsigned int c[4];
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
    unsigned int c[4];
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
 *		Robb Matzke, 1999-06-23
 *		If the command line switch `--noopt' is present then the fast
 *		compound datatype conversion is turned off.
 *-------------------------------------------------------------------------
 */
int
main (int argc, char *argv[])
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
    hid_t		file, dataset, space, PRESERVE, fapl;
    hid_t       array_dt;
    static hsize_t	dim[] = {NX, NY};
    hsize_t 		f_offset[2];	/*offset of hyperslab in file	*/
    hsize_t 		h_size[2];	/*size of hyperslab		*/
    hsize_t		memb_size[1] = {4};
    char		filename[256];
    int			ret_code;

    h5_reset();

    /* Turn off optimized compound converter? */
    if (argc>1) {
	if (argc>2 || strcmp("--noopt", argv[1])) {
	    fprintf(stderr, "usage: %s [--noopt]\n", argv[0]);
	    exit(1);
	}
	H5Tunregister(H5T_PERS_DONTCARE, NULL, -1, -1, H5T_conv_struct_opt);
    }

    /* Create the file */
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));
    if ((file = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) {
	goto error;
    }

    /* Create the data space */
    if ((space = H5Screate_simple (2, dim, NULL))<0) goto error;

    /* Create xfer properties to preserve initialized data */
    /* Also verify H5Pset_preserve is initially 0 and then is set to 1. */
    if ((PRESERVE = H5Pcreate (H5P_DATASET_XFER))<0) goto error;
    if ((ret_code=H5Pget_preserve (PRESERVE)) != 0){
	printf("Preserve status of dataset transfer property list should be"
	   " 0 (FALSE), got %d\n", ret_code);
	goto error;
    }
    if (H5Pset_preserve (PRESERVE, 1)<0) goto error;
    if ((ret_code=H5Pget_preserve (PRESERVE)) != 1){
	printf("Preserve status of dataset transfer property list should be"
	   " 1 (TRUE), got %d\n", ret_code);
	goto error;
    }


    /*
     *######################################################################
     * STEP 1: Save the original dataset natively.
     */
    TESTING("basic compound write");

    /* Initialize the dataset */
    for (i=0; i<NX*NY; i++) {
	s1[i].a = 8*i+0;
	s1[i].b = 2000+2*i;
	s1[i].c[0] = 8*i+2;
	s1[i].c[1] = 8*i+3;
	s1[i].c[2] = 8*i+4;
	s1[i].c[3] = 8*i+5;
	s1[i].d = 2001+2*i;
	s1[i].e = 8*i+7;
    }

    /* Create the memory data type */
    if ((s1_tid = H5Tcreate (H5T_COMPOUND, sizeof(s1_t)))<0)
        goto error;
    array_dt=H5Tarray_create(H5T_NATIVE_INT, 1, memb_size, NULL);
    if (H5Tinsert (s1_tid, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT)<0 ||
            H5Tinsert (s1_tid, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT)<0 ||
            H5Tinsert (s1_tid, "c", HOFFSET(s1_t,c), array_dt)<0 ||
            H5Tinsert (s1_tid, "d", HOFFSET(s1_t,d), H5T_NATIVE_INT)<0 ||
            H5Tinsert (s1_tid, "e", HOFFSET(s1_t,e), H5T_NATIVE_INT)<0)
        goto error;
    H5Tclose(array_dt);

    /* Create the dataset */
    if ((dataset = H5Dcreate (file, "s1", s1_tid, space, H5P_DEFAULT))<0) {
	goto error;
    }

    /* Write the data */
    if (H5Dwrite (dataset, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1)<0) {
	goto error;
    }
    PASSED();

    /*
     *######################################################################
     * STEP 2: We create a new type ID for the second dataset even though
     * 	       it's the same as the first just to test things better, but
     *	       in fact, we could have used s1_tid.
     */
    TESTING("basic compound read");

    /* Create a data type for s2 */
    if ((s2_tid = H5Tcreate (H5T_COMPOUND, sizeof(s2_t)))<0)
        goto error;
    array_dt=H5Tarray_create(H5T_NATIVE_INT, 1, memb_size, NULL);
    if (H5Tinsert (s2_tid, "a", HOFFSET(s2_t,a), H5T_NATIVE_INT)<0 ||
            H5Tinsert (s2_tid, "b", HOFFSET(s2_t,b), H5T_NATIVE_INT)<0 ||
            H5Tinsert (s2_tid, "c", HOFFSET(s2_t,c), array_dt)<0 ||
            H5Tinsert (s2_tid, "d", HOFFSET(s2_t,d), H5T_NATIVE_INT)<0 ||
            H5Tinsert (s2_tid, "e", HOFFSET(s2_t,e), H5T_NATIVE_INT)<0)
        goto error;
    H5Tclose(array_dt);

    /* Read the data */
    if (H5Dread (dataset, s2_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s2)<0) {
	goto error;
    }

    /* Compare s2 with s1.  They should be the same */
    for (i=0; i<NX*NY; i++) {
	if (s1[i].a!=s2[i].a ||
	    s1[i].b!=s2[i].b ||
	    s1[i].c[0]!=s2[i].c[0] ||
	    s1[i].c[1]!=s2[i].c[1] ||
	    s1[i].c[2]!=s2[i].c[2] ||
	    s1[i].c[3]!=s2[i].c[3] ||
	    s1[i].d!=s2[i].d ||
	    s1[i].e!=s2[i].e) {
	    H5_FAILED();
	    puts("    Incorrect values read from the file");
	    goto error;
	}
    }
    PASSED();

    /*
     *######################################################################
     * STEP 3: Read the dataset back into a third memory buffer. This buffer
     * 	       has the same data space but the data type is different: the
     *	       data type is a struct whose members are in the opposite order.
     */
    TESTING("reversal of struct members");

    /* Create a data type for s3 */
    if ((s3_tid = H5Tcreate (H5T_COMPOUND, sizeof(s3_t)))<0)
        goto error;
    array_dt=H5Tarray_create(H5T_NATIVE_INT, 1, memb_size, NULL);
    if (H5Tinsert (s3_tid, "a", HOFFSET(s3_t,a), H5T_NATIVE_INT)<0 ||
            H5Tinsert (s3_tid, "b", HOFFSET(s3_t,b), H5T_NATIVE_INT)<0 ||
            H5Tinsert (s3_tid, "c", HOFFSET(s3_t,c), array_dt)<0 ||
            H5Tinsert (s3_tid, "d", HOFFSET(s3_t,d), H5T_NATIVE_INT)<0 ||
            H5Tinsert (s3_tid, "e", HOFFSET(s3_t,e), H5T_NATIVE_INT)<0)
        goto error;
    H5Tclose(array_dt);

    /* Read the data */
    if (H5Dread (dataset, s3_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s3)<0) {
	goto error;
    }

    /* Compare s3 with s1.  They should be the same */
    for (i=0; i<NX*NY; i++) {
	if (s1[i].a!=s3[i].a ||
	    s1[i].b!=s3[i].b ||
	    s1[i].c[0]!=s3[i].c[0] ||
	    s1[i].c[1]!=s3[i].c[1] ||
	    s1[i].c[2]!=s3[i].c[2] ||
	    s1[i].c[3]!=s3[i].c[3] ||
	    s1[i].d!=s3[i].d ||
	    s1[i].e!=s3[i].e) {
	    H5_FAILED();
	    puts("    Incorrect values read from the file");
	    goto error;
	}
    }
    PASSED();

    /*
     *######################################################################
     * STEP 4: Read a subset of the members.  Of the <a,b,c,d,e> members
     *         stored on disk we'll read <b,d>.
     */
    TESTING("subset struct read");

    /* Create a datatype for s4 */
    if ((s4_tid = H5Tcreate (H5T_COMPOUND, sizeof(s4_t)))<0) goto error;
    if (H5Tinsert (s4_tid, "b", HOFFSET(s4_t,b), H5T_NATIVE_INT)<0) goto error;
    if (H5Tinsert (s4_tid, "d", HOFFSET(s4_t,d), H5T_NATIVE_INT)<0) goto error;

    /* Read the data */
    if (H5Dread (dataset, s4_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s4)<0) {
	goto error;
    }

    /* Compare s4 with s1 */
    for (i=0; i<NX*NY; i++) {
	if (s1[i].b!=s4[i].b ||
	    s1[i].d!=s4[i].d) {
	    H5_FAILED();
	    puts("    Incorrect values read from the file");
	    goto error;
	}
    }
    PASSED();

    /*
     *######################################################################
     * STEP 5: Read all the members into a struct which has other members
     * 	       which have already been initialized.
     */
    TESTING("partially initialized superset read");

    /* Initialize some members */
    for (i=0; i<NX*NY; i++) {
	s5[i].pre =  1000+4*i;
	s5[i].mid1 = 1001+4*i;
	s5[i].mid2 = 1002+4*i;
	s5[i].post = 1003+4*i;
    }

    /* Create a data type for s5 */
    if ((s5_tid = H5Tcreate (H5T_COMPOUND, sizeof(s5_t)))<0)
        goto error;
    array_dt=H5Tarray_create(H5T_NATIVE_INT, 1, memb_size, NULL);
    if (H5Tinsert (s5_tid, "a", HOFFSET(s5_t,a), H5T_NATIVE_INT)<0 ||
            H5Tinsert (s5_tid, "b", HOFFSET(s5_t,b), H5T_NATIVE_INT)<0 ||
            H5Tinsert (s5_tid, "c", HOFFSET(s5_t,c), array_dt)<0 ||
            H5Tinsert (s5_tid, "d", HOFFSET(s5_t,d), H5T_NATIVE_INT)<0 ||
            H5Tinsert (s5_tid, "e", HOFFSET(s5_t,e), H5T_NATIVE_INT))
        goto error;
    H5Tclose(array_dt);

    /* Read the data */
    if (H5Dread (dataset, s5_tid, H5S_ALL, H5S_ALL, PRESERVE, s5)<0) {
	goto error;
    }

    /* Check that the data was read properly */
    for (i=0; i<NX*NY; i++) {
	if (s1[i].a!=s5[i].a ||
	    s1[i].b!=s5[i].b ||
	    s1[i].c[0]!=s5[i].c[0] ||
	    s1[i].c[1]!=s5[i].c[1] ||
	    s1[i].c[2]!=s5[i].c[2] ||
	    s1[i].c[3]!=s5[i].c[3] ||
	    s1[i].d!=s5[i].d ||
	    s1[i].e!=s5[i].e) {
	    H5_FAILED();
	    puts("    Incorrect values read from the file");
	    goto error;
	}
    }

    /* Check that no previous values were clobbered */
    for (i=0; i<NX*NY; i++) {
	if (s5[i].pre  != 1000+4*i ||
	    s5[i].mid1 != 1001+4*i ||
	    s5[i].mid2 != 1002+4*i ||
	    s5[i].post != 1003+4*i) {
	    H5_FAILED();
	    puts("    Memory values were clobbered");
	    goto error;
	}
    }
    PASSED();

    /*
     *######################################################################
     * STEP 6: Update fields `b' and `d' on the file leaving the other
     *         fields unchanged.  This tests member alignment and background
     *	       buffers.
     */
    TESTING("partially initialized superset write");

    /* Initialize `s4' with new values */
    for (i=0; i<NX*NY; i++) {
	s4[i].b = 8*i+1;
	s4[i].d = 8*i+6;
    }

    /* Write the data to file */
    if (H5Dwrite (dataset, s4_tid, H5S_ALL, H5S_ALL, PRESERVE, s4)<0) {
	goto error;
    }

    /* Read the data back */
    if (H5Dread (dataset, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1)<0) {
	goto error;
    }

    /* Compare */
    for (i=0; i<NX*NY; i++) {
	if (s1[i].a != 8*i+0 ||
	    s1[i].b != 8*i+1 ||
	    s1[i].c[0] != 8*i+2 ||
	    s1[i].c[1] != 8*i+3 ||
	    s1[i].c[2] != 8*i+4 ||
	    s1[i].c[3] != 8*i+5 ||
	    s1[i].d != 8*i+6 ||
	    s1[i].e != 8*i+7) {
	    H5_FAILED();
	    printf("    i==%u, row=%u, col=%u\n", i, i/NY, i%NY);
	    printf("    got: {%7d,%7d,[%7d,%7d,%7d,%7d],%7d,%7d}\n",
		   s1[i].a, s1[i].b, s1[i].c[0], s1[i].c[1], s1[i].c[2],
		   s1[i].c[3], s1[i].d, s1[i].e);
	    printf("    ans: {%7d,%7d,[%7d,%7d,%7d,%7d],%7d,%7d}\n",
		   8*i+0, 8*i+1, 8*i+2, 8*i+3, 8*i+4, 8*i+5, 8*i+6, 8*i+7);
	    goto error;
	}
    }
    PASSED();

    /*
     *######################################################################
     * STEP 7. Read the original dataset with an explicit data space.  Even
     * though these data spaces are equal it tests a different part of the
     * library.
     */
    TESTING("explicit data space");

    /* Create the data space */
    if ((s7_sid = H5Screate_simple (2, dim, NULL))<0) goto error;

    /* Read the dataset */
    if (H5Dread (dataset, s2_tid, s7_sid, H5S_ALL, H5P_DEFAULT, s2)<0) {
	goto error;
    }

    /* Compare */
    for (i=0; i<NX*NY; i++) {
	if (s2[i].a != s1[i].a ||
	    s2[i].b != s1[i].b ||
	    s2[i].c[0] != s1[i].c[0] ||
	    s2[i].c[1] != s1[i].c[1] ||
	    s2[i].c[2] != s1[i].c[2] ||
	    s2[i].c[3] != s1[i].c[3] ||
	    s2[i].d != s1[i].d ||
	    s2[i].e != s1[i].e) {
	    H5_FAILED();
	    puts("    Incorrect values read from file");
	    goto error;
	}
    }
    PASSED();


    /*
     *######################################################################
     * STEP 8. Read a hyperslab of the file into a complete array in memory.
     * The hyperslab is the middle third of the array.
     */
    TESTING("hyperslab partial read to array");

    /* Create the file data space */
    if ((s8_f_sid = H5Dget_space (dataset))<0) goto error;
    f_offset[0] = NX/3;
    f_offset[1] = NY/3;
    h_size[0] = 2*NX/3 - f_offset[0];
    h_size[1] = 2*NY/3 - f_offset[1];
    if (H5Sselect_hyperslab (s8_f_sid, H5S_SELECT_SET, f_offset, NULL,
			     h_size, NULL)<0) goto error;

    /* Create memory data space */
    if ((s8_m_sid = H5Screate_simple (2, h_size, NULL))<0) goto error;

    /* Read the dataset */
    s8 = calloc ((size_t)(h_size[0]*h_size[1]), sizeof(s1_t));
    assert (s8);
    if (H5Dread (dataset, s1_tid, s8_m_sid, s8_f_sid, H5P_DEFAULT, s8)<0) {
	goto error;
    }

    /* Compare */
    for (i=0; i<h_size[0]; i++) {
	for (j=0; j<h_size[1]; j++) {
	    s1_t *ps1 = s1 + (f_offset[0]+i)*NY + f_offset[1] + j;
	    s1_t *ps8 = s8 + i*h_size[1] + j;

	    if (ps8->a != ps1->a ||
		ps8->b != ps1->b ||
		ps8->c[0] != ps1->c[0] ||
		ps8->c[1] != ps1->c[1] ||
		ps8->c[2] != ps1->c[2] ||
		ps8->c[3] != ps1->c[3] ||
		ps8->d != ps1->d ||
		ps8->e != ps1->e) {
		H5_FAILED();
		puts("    Incorrect values read from file");
		goto error;
	    }
	}
    }

    free (s8);
    s8 = NULL;
    PASSED();


    /*
     *######################################################################
     * STEP 9.  Read a hyperslab of the file into a hyperslab of memory.  The
     * part of memory not read is already initialized and must not change.
     */
    TESTING("hyperslab partial read to another hyperslab");

    /* Initialize */
    for (i=0; i<NX*NY; i++) {
	s2[i].a = s2[i].b = s2[i].d = s2[i].e = (unsigned)(-1);
	s2[i].c[0] = s2[i].c[1] = s2[i].c[2] = s2[i].c[3] = (unsigned)(-1);
    }

    /* Read the hyperslab */
    if (H5Dread (dataset, s2_tid, s8_f_sid, s8_f_sid, H5P_DEFAULT, s2)<0) {
	goto error;
    }

    /* Compare */
    for (i=0; i<NX; i++) {
	for (j=0; j<NY; j++) {
	    s1_t *ps1 = s1 + i*NY + j;
	    s2_t *ps2 = s2 + i*NY + j;
	    if (i>=f_offset[0] &&
		i<f_offset[0]+h_size[0] &&
		j>=f_offset[1] &&
		j<f_offset[1]+h_size[1]) {
		if (ps2->a != ps1->a ||
		    ps2->b != ps1->b ||
		    ps2->c[0] != ps1->c[0] ||
		    ps2->c[1] != ps1->c[1] ||
		    ps2->c[2] != ps1->c[2] ||
		    ps2->c[3] != ps1->c[3] ||
		    ps2->d != ps1->d ||
		    ps2->e != ps1->e) {
		    H5_FAILED();
		    puts("    Memory values clobbered");
		    goto error;
		}
	    } else {
		if (ps2->a != (unsigned)(-1) ||
		    ps2->b != (unsigned)(-1) ||
		    ps2->c[0] != (unsigned)(-1) ||
		    ps2->c[1] != (unsigned)(-1) ||
		    ps2->c[2] != (unsigned)(-1) ||
		    ps2->c[3] != (unsigned)(-1) ||
		    ps2->d != (unsigned)(-1) ||
		    ps2->e != (unsigned)(-1)) {
		    H5_FAILED();
		    puts("    Incorrect values read from file");
		    goto error;
		}
	    }
	}
    }
    PASSED();

    /*
     *######################################################################
     * STEP 10. Same as step 9 except the memory array contains some members
     * which are already initialized, like step 5.
     */
    TESTING("hyperslab to hyperslab part initialized read");

    /* Initialize */
    for (i=0; i<NX*NY; i++) {
	s5[i].a = s5[i].b = s5[i].d = s5[i].e = (unsigned)(-1);
	s5[i].c[0] = s5[i].c[1] = s5[i].c[2] = s5[i].c[3] = (unsigned)(-1);
	s5[i].pre = s5[i].mid1 = s5[i].mid2 = s5[i].post = (unsigned)(-1);
    }

    /* Read the hyperslab */
    if (H5Dread (dataset, s5_tid, s8_f_sid, s8_f_sid, PRESERVE, s5)<0) {
	goto error;
    }

    /* Compare */
    for (i=0; i<NX; i++) {
	for (j=0; j<NY; j++) {
	    s1_t *ps1 = s1 + i*NY + j;
	    s5_t *ps5 = s5 + i*NY + j;
	    if (i>=f_offset[0] &&
		i<f_offset[0]+h_size[0] &&
		j>=f_offset[1] &&
		j<f_offset[1]+h_size[1]) {
		if (ps5->pre != (unsigned)(-1) ||
		    ps5->a != ps1->a ||
		    ps5->b != ps1->b ||
		    ps5->mid1 != (unsigned)(-1) ||
		    ps5->c[0] != ps1->c[0] ||
		    ps5->c[1] != ps1->c[1] ||
		    ps5->c[2] != ps1->c[2] ||
		    ps5->c[3] != ps1->c[3] ||
		    ps5->mid2 != (unsigned)(-1) ||
		    ps5->d != ps1->d ||
		    ps5->e != ps1->e ||
		    ps5->post != (unsigned)(-1)) {
		    H5_FAILED();
		    puts("    Memory values clobbered");
		    goto error;
		}
	    } else {
		if (ps5->pre != (unsigned)(-1) ||
		    ps5->a != (unsigned)(-1) ||
		    ps5->b != (unsigned)(-1) ||
		    ps5->mid1 != (unsigned)(-1) ||
		    ps5->c[0] != (unsigned)(-1) ||
		    ps5->c[1] != (unsigned)(-1) ||
		    ps5->c[2] != (unsigned)(-1) ||
		    ps5->c[3] != (unsigned)(-1) ||
		    ps5->mid2 != (unsigned)(-1) ||
		    ps5->d != (unsigned)(-1) ||
		    ps5->e != (unsigned)(-1) ||
		    ps5->post != (unsigned)(-1)) {
		    H5_FAILED();
		    puts("    Incorrect values read from file");
		    goto error;
		}
	    }
	}
    }
    PASSED();

    /*
     *######################################################################
     * Step 11: Write an array into the middle third of the dataset
     * initializeing only members `b' and `d' to -1.
     */
    TESTING("hyperslab part initialized write");

    /* Create the memory array and initialize all fields to zero */
    f_offset[0] = NX/3;
    f_offset[1] = NY/3;
    h_size[0] = 2*NX/3 - f_offset[0];
    h_size[1] = 2*NY/3 - f_offset[1];
    s11 = malloc ((size_t)h_size[0]*(size_t)h_size[1]*sizeof(s4_t));
    assert (s11);

    /* Initialize */
    for (i=0; i<h_size[0]*h_size[1]; i++) {
	s11[i].b = s11[i].d = (unsigned)(-1);
    }

    /* Write to disk */
    if (H5Dwrite (dataset, s4_tid, s8_m_sid, s8_f_sid, PRESERVE, s11)<0) {
	goto error;
    }
    free (s11);
    s11=NULL;

    /* Read the whole thing */
    if (H5Dread (dataset, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1)<0) {
	goto error;
    }

    /* Compare */
    for (i=0; i<NX; i++) {
	for (j=0; j<NY; j++) {
	    s1_t *ps1 = s1 + i*NY + j;

	    if (ps1->a != 8*(i*NY+j)+0 ||
		ps1->c[0] != 8*(i*NY+j)+2 ||
		ps1->c[1] != 8*(i*NY+j)+3 ||
		ps1->c[2] != 8*(i*NY+j)+4 ||
		ps1->c[3] != 8*(i*NY+j)+5 ||
		ps1->e != 8*(i*NY+j)+7) {
		H5_FAILED();
		puts("    Write clobbered values");
		goto error;
	    }

	    if (i>=f_offset[0] &&
		i<f_offset[0]+h_size[0] &&
		j>=f_offset[1] &&
		j<f_offset[1]+h_size[1]) {
		if (ps1->b != (unsigned)(-1) ||
		    ps1->d != (unsigned)(-1)) {
		    H5_FAILED();
		    puts("    Wrong values written or read");
		    goto error;
		}
	    } else {
		if (ps1->b != 8*(i*NY+j)+1 ||
		    ps1->d != 8*(i*NY+j)+6) {
		    H5_FAILED();
		    puts("    Write clobbered values");
		    goto error;
		}
	    }
	}
    }
    PASSED();


    /*
     * Release resources.
     */
    H5Pclose (PRESERVE);
    H5Dclose (dataset);
    H5Fclose (file);

    h5_cleanup(FILENAME, fapl);
    puts("All compound dataset tests passed.");
    return 0;

error:
    puts("Remaining tests have been skipped.");
    puts("*** DATASET TESTS FAILED ***");
    return 1;
}
