/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, December  9, 1997
 *
 * Purpose:     Tests the data type interface (H5T)
 */
#include <hdf5.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <H5config.h>
#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif

#define FILE_NAME_1	"dtypes1.h5"
#define FILE_NAME_2	"dtypes2.h5"

typedef struct complex_t {
    double                  re;
    double                  im;
} complex_t;


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
    if (!getenv ("HDF5_NOCLEANUP")) {
	remove (FILE_NAME_1);
	remove (FILE_NAME_2);
    }
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
 * Function:    test_classes
 *
 * Purpose:     Test type classes
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_classes(void)
{
    H5T_class_t		tcls;
    
    printf("%-70s", "Testing H5Tget_class()");

    if ((tcls=H5Tget_class(H5T_NATIVE_INT))<0) goto error;
    if (H5T_INTEGER!=tcls) {
        puts("*FAILED*");
        puts("   Invalid type class for H5T_NATIVE_INT");
        goto error;
    }
    if ((tcls=H5Tget_class(H5T_NATIVE_DOUBLE))<0) goto error;
    if (H5T_FLOAT!=tcls) {
        puts("*FAILED*");
	puts("   Invalid type class for H5T_NATIVE_DOUBLE");
        goto error;
    }
    puts(" PASSED");
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_copy
 *
 * Purpose:     Are we able to copy a data type?
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_copy(void)
{
    hid_t               a_copy;

    printf("%-70s", "Testing H5Tcopy()");

    if ((a_copy = H5Tcopy(H5T_NATIVE_SHORT)) < 0) goto error;
    if (H5Tclose(a_copy) < 0) goto error;

    /* We should not be able to close a built-in byte */
    H5E_BEGIN_TRY {
	if (H5Tclose (H5T_NATIVE_CHAR)>=0) {
	    puts ("*FAILED*");
	    puts ("   Should not be able to close a predefined type!");
	    goto error;
	}
    } H5E_END_TRY;

    puts(" PASSED");
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_compound
 *
 * Purpose:     Tests various things about compound data types.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Robb Matzke
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compound(void)
{
    complex_t               tmp;
    hid_t                   complex_id;

    printf("%-70s", "Testing compound data types");

    /* Create the empty type */
    if ((complex_id = H5Tcreate(H5T_COMPOUND, sizeof tmp))<0) goto error;

    /* Add a couple fields */
    if (H5Tinsert(complex_id, "real", HOFFSET(complex_t, re),
		  H5T_NATIVE_DOUBLE)<0) goto error;
    if (H5Tinsert(complex_id, "imaginary", HOFFSET(complex_t, im),
		  H5T_NATIVE_DOUBLE)<0) goto error;

    if (H5Tclose (complex_id)<0) goto error;
    puts(" PASSED");
    return 0;

  error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_transient
 *
 * Purpose:	Tests transient data types.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, June  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_transient (void)
{
    static hsize_t	ds_size[2] = {10, 20};
    hid_t		file, type, space, dset, t2;
    
    printf ("%-70s", "Testing transient data types");
    if ((file=H5Fcreate (FILE_NAME_1, H5F_ACC_TRUNC|H5F_ACC_DEBUG,
			 H5P_DEFAULT, H5P_DEFAULT))<0) goto error;
    space = H5Screate_simple (2, ds_size, ds_size);

    /* Predefined types cannot be modified or closed */
    H5E_BEGIN_TRY {
	if (H5Tset_precision (H5T_NATIVE_INT, 256)>=0) {
	    puts ("*FAILED*");
	    puts ("   Predefined types should not be modifiable!");
	    goto error;
	}
	if (H5Tclose (H5T_NATIVE_INT)>=0) {
	    puts ("*FAILED*");
	    puts ("   Predefined types should not be closable!");
	    goto error;
	}
    } H5E_END_TRY;

    /* Copying a predefined type results in a modifiable copy */
    if ((type=H5Tcopy (H5T_NATIVE_INT))<0) goto error;
    if (H5Tset_precision (type, 256)<0) goto error;

    /* It should not be possible to create an attribute for a transient type */
    H5E_BEGIN_TRY {
	if (H5Acreate (type, "attr1", H5T_NATIVE_INT, space, H5P_DEFAULT)>=0) {
	    puts ("*FAILED*");
	    puts ("   Attributes should not be allowed for transient types!");
	    goto error;
	}
    } H5E_END_TRY;

    /* Create a dataset from a transient data type */
    if (H5Tclose (type)<0) goto error;
    if ((type = H5Tcopy (H5T_NATIVE_INT))<0) goto error;
    if ((dset=H5Dcreate (file, "dset1", type, space, H5P_DEFAULT))<0) {
	goto error;
    }

    /* The type returned from a dataset should not be modifiable */
    if ((t2 = H5Dget_type (dset))<0) goto error;
    H5E_BEGIN_TRY {
	if (H5Tset_precision (t2, 256)>=0) {
	    puts ("*FAILED*");
	    puts ("   Dataset data types should not be modifiable!");
	    goto error;
	}
    } H5E_END_TRY;
    if (H5Tclose (t2)<0) goto error;

    /*
     * Close the dataset and reopen it, testing that it's type is still
     * read-only.
     */
    if (H5Dclose (dset)<0) goto error;
    if ((dset=H5Dopen (file, "dset1"))<0) goto error;
    if ((t2 = H5Dget_type (dset))<0) goto error;
    H5E_BEGIN_TRY {
	if (H5Tset_precision (t2, 256)>=0) {
	    puts ("*FAILED*");
	    puts ("   Dataset data types should not be modifiable!");
	    goto error;
	}
    } H5E_END_TRY;
    if (H5Tclose (t2)<0) goto error;

    /*
     * Get the dataset data type by applying H5Tcopy() to the dataset. The
     * result should be modifiable.
     */
    if ((t2=H5Tcopy (dset))<0) goto error;
    if (H5Tset_precision (t2, 256)<0) goto error;
    if (H5Tclose (t2)<0) goto error;
    

    H5Dclose (dset);
    H5Fclose (file);
    H5Tclose (type);
    H5Sclose (space);
    puts (" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Tclose (t2);
	H5Tclose (type);
	H5Sclose (space);
	H5Dclose (dset);
	H5Fclose (file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_named
 *
 * Purpose:	Tests named data types.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Monday, June  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_named (void)
{
    hid_t		file, type, space, dset, t2, attr1;
    herr_t		status;
    static hsize_t	ds_size[2] = {10, 20};
    
    printf ("%-70s", "Testing named data types");
    if ((file=H5Fcreate (FILE_NAME_2, H5F_ACC_TRUNC|H5F_ACC_DEBUG,
			 H5P_DEFAULT, H5P_DEFAULT))<0) goto error;
    space = H5Screate_simple (2, ds_size, ds_size);

    /* Predefined types cannot be committed */
    H5E_BEGIN_TRY {
	if (H5Tcommit (file, "test_named_1 (should not exist)",
		       H5T_NATIVE_INT)>=0) {
	    puts ("*FAILED*");
	    puts ("   Predefined types should not be committable!");
	    goto error;
	}
    } H5E_END_TRY;

    /* Copy a predefined data type and commit the copy */
    if ((type = H5Tcopy (H5T_NATIVE_INT))<0) goto error;
    if (H5Tcommit (file, "native-int", type)<0) goto error;
    if ((status=H5Tcommitted (type))<0) goto error;
    if (0==status) {
	puts ("*FAILED*");
	puts ("   H5Tcommitted() returned false!");
	goto error;
    }

    /* We should not be able to modify a type after it has been committed. */
    H5E_BEGIN_TRY {
	if (H5Tset_precision (type, 256)>=0) {
	    puts ("*FAILED*");
	    puts ("   Committed type is not constant!");
	    goto error;
	}
    } H5E_END_TRY;

    /* We should not be able to re-commit a committed type */
    H5E_BEGIN_TRY {
	if (H5Tcommit (file, "test_named_2 (should not exist)", type)>=0) {
	    puts ("*FAILED*");
	    puts ("   Committed types should not be recommitted!");
	    goto error;
	}
    } H5E_END_TRY;

    /* It should be possible to define an attribute for the named type */
    if ((attr1=H5Acreate (type, "attr1", H5T_NATIVE_INT, space,
			  H5P_DEFAULT))<0) goto error;
    if (H5Aclose (attr1)<0) goto error;

    /*
     * Copying a committed type should result in a transient type which is
     * not locked.
     */
    if ((t2 = H5Tcopy (type))<0) goto error;
    if ((status=H5Tcommitted (t2))<0) goto error;
    if (status) {
	puts ("*FAILED*");
	puts ("   Copying a named type should result in a transient type!");
	goto error;
    }
    if (H5Tset_precision (t2, 256)<0) goto error;
    if (H5Tclose (t2)<0) goto error;

    /*
     * Close the committed type and reopen it.  It should return a named type.
     */
    if (H5Tclose (type)<0) goto error;
    if ((type=H5Topen (file, "native-int"))<0) goto error;
    if ((status=H5Tcommitted (type))<0) goto error;
    if (!status) {
	puts ("*FAILED*");
	puts ("   Opened named types should be named types!");
	goto error;
    }
    
    /* Create a dataset that uses the named type */
    if ((dset = H5Dcreate (file, "dset1", type, space, H5P_DEFAULT))<0) {
	goto error;
    }

    /* Get the dataset's data type and make sure it's a named type */
    if ((t2 = H5Dget_type (dset))<0) goto error;
    if ((status=H5Tcommitted (t2))<0) goto error;
    if (!status) {
	puts ("*FAILED*");
	puts ("   Dataset type should be a named type!");
	goto error;
    }

    /* Close the dataset, then close its type, then reopen the dataset */
    if (H5Dclose (dset)<0) goto error;
    if (H5Tclose (t2)<0) goto error;
    if ((dset = H5Dopen (file, "dset1"))<0) goto error;

    /* Get the dataset's type and make sure it's named */
    if ((t2 = H5Dget_type (dset))<0) goto error;
    if ((status=H5Tcommitted (t2))<0) goto error;
    if (!status) {
	puts ("*FAILED*");
	puts ("   Dataset type should be a named type!");
	goto error;
    }

    /*
     * Close the dataset and create another with the type returned from the
     * first dataset.
     */
    if (H5Dclose (dset)<0) goto error;
    if ((dset=H5Dcreate (file, "dset2", t2, space, H5P_DEFAULT))<0) {
	goto error;
    }

    /* Reopen the second dataset and make sure the type is shared */
    if (H5Tclose (t2)<0) goto error;
    if (H5Dclose (dset)<0) goto error;
    if ((dset = H5Dopen (file, "dset2"))<0) goto error;
    if ((t2 = H5Dget_type (dset))<0) goto error;
    if ((status=H5Tcommitted (t2))<0) goto error;
    if (!status) {
	puts ("*FAILED*");
	puts ("   Dataset type should be a named type!");
	goto error;
    }
    if (H5Tclose (t2)<0) goto error;
    
    /*
     * Get the dataset data type by applying H5Tcopy() to the dataset. The
     * result should be modifiable.
     */
    if ((t2=H5Tcopy (dset))<0) goto error;
    if (H5Tset_precision (t2, 256)<0) goto error;
    if (H5Tclose (t2)<0) goto error;

    /* Clean up */
    if (H5Dclose (dset)<0) goto error;
    if (H5Tclose (type)<0) goto error;
    if (H5Sclose (space)<0) goto error;
    if (H5Fclose (file)<0) goto error;
    puts (" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Tclose (t2);
	H5Tclose (type);
	H5Sclose (space);
	H5Dclose (dset);
	H5Fclose (file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Test the data type interface.
 *
 * Return:      Success:        
 *
 *              Failure:        
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int		nerrors = 0;

    /* Set the error handler */
    H5Eset_auto (display_error_cb, NULL);

    /* Do the tests */
    nerrors += test_classes()<0 ? 1 : 0;
    nerrors += test_copy()<0 ? 1 : 0;
    nerrors += test_compound()<0 ? 1 : 0;
    nerrors += test_transient ()<0 ? 1 : 0;
    nerrors += test_named ()<0 ? 1 : 0;

    if (nerrors) {
        printf("***** %d DATA TYPE TEST%s FAILED! *****\n",
               nerrors, 1 == nerrors ? "" : "S");
        exit(1);
    }
    printf("All data type tests passed.\n");
    cleanup ();
    return 0;
}
