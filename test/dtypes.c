/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, December  9, 1997
 *
 * Purpose:     Tests the data type interface (H5T)
 */
#include <assert.h>
#include <float.h>
#include <hdf5.h>
#include <math.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define H5T_PACKAGE
#include <H5Tpkg.h>		/*to turn off hardware conversions*/

#include <H5config.h>
#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif

#if SIZEOF_DOUBLE != SIZEOF_LONG_DOUBLE
#   define USE_LDOUBLE
#endif

#ifndef MAX
#   define MAX(X,Y)	((X)>(Y)?(X):(Y))
#   define MIN(X,Y)	((X)<(Y)?(X):(Y))
#endif

#define FILE_NAME_1	"dtypes1.h5"
#define FILE_NAME_2	"dtypes2.h5"

typedef struct complex_t {
    double                  re;
    double                  im;
} complex_t;

/*
 * Count up or down depending on whether the machine is big endian or little
 * endian.  If `E' is H5T_ORDER_BE then the result will be I, otherwise the
 * result will be Z-(I+1).
 */
#define ENDIAN(E,Z,I)	(H5T_ORDER_BE==(E)?(I):(Z)-((I)+1))

typedef enum flt_t {
    FLT_FLOAT, FLT_DOUBLE, FLT_LDOUBLE, FLT_OTHER
} flt_t;

/* Count the number of overflows */
static int noverflows_g = 0;

/* Skip overflow tests if non-zero */
static int skip_overflow_tests_g = 0;

/*
 * Although we check whether a floating point overflow generates a SIGFPE and
 * turn off overflow tests in that case, it might still be possible for an
 * overflow condition to occur.  Once a SIGFPE is raised the program cannot
 * be allowed to continue (cf. Posix signals) so in order to recover from a
 * SIGFPE we run tests that might generate one in a child process.
 */
#if defined(HAVE_FORK) && defined(HAVE_WAITPID)
#   define HANDLE_SIGFPE
#endif

void some_dummy_func(float x);


/*-------------------------------------------------------------------------
 * Function:	fpe_handler
 *
 * Purpose:	Exit with 255
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Monday, July  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
fpe_handler(int __unused__ signo)
{
    puts(" -SKIP-");
    puts("   Test skipped due to SIGFPE.");
#ifndef HANDLE_SIGFPE
    puts("   Remaining tests could not be run.");
    puts("   Please turn off SIGFPE on overflows and try again.");
#endif
    exit(255);
}


/*-------------------------------------------------------------------------
 * Function:	overflow_handler
 *
 * Purpose:	Gets called for all data type conversion overflows.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, July  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
overflow_handler(hid_t __unused__ src_id, hid_t __unused__ dst_id,
		 void __unused__ *src_buf, void __unused__ *dst_buf)
{
    noverflows_g++;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	some_dummy_func
 *
 * Purpose:	A dummy function to help check for overflow.
 *
 * Note:	DO NOT DECLARE THIS FUNCTION STATIC OR THE COMPILER MIGHT
 *		PROMOTE ARGUMENT `x' TO DOUBLE AND DEFEAT THE OVERFLOW
 *		CHECKING.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Tuesday, July 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
some_dummy_func(float x)
{
    char	s[128];
    sprintf(s, "%g", x);
}


/*-------------------------------------------------------------------------
 * Function:	generates_sigfpe
 *
 * Purpose:	Determines if SIGFPE is generated from overflows.  We must be
 *		able to fork() and waitpid() in order for this test to work
 *		properly.  Sets skip_overflow_tests_g to non-zero if they
 *		would generate SIGBUS, zero otherwise.
 *
 * Programmer:	Robb Matzke
 *              Tuesday, July 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
generates_sigfpe(void)
{
#if defined(HAVE_FORK) && defined(HAVE_WAITPID)
    pid_t	pid;
    int		status;
    size_t	i, j;
    double	d;
    unsigned char *dp = (unsigned char*)&d;
    float	f;

    fflush(stdout);
    fflush(stderr);
    if ((pid=fork())<0) {
	perror("fork");
	exit(1);
    } else if (0==pid) {
	for (i=0; i<2000; i++) {
	    for (j=0; j<sizeof(double); j++) dp[j] = rand();
	    f = (float)d;
	    some_dummy_func(f);
	}
	exit(0);
    }
    
    while (pid!=waitpid(pid, &status, 0)) /*void*/;
    if (WIFEXITED(status) && 0==WEXITSTATUS(status)) {
	printf("Overflow cases will be tested.\n");
	skip_overflow_tests_g = FALSE;
    } else if (WIFSIGNALED(status) && SIGFPE==WTERMSIG(status)) {
	printf("Overflow cases cannot be safely tested.\n");
	skip_overflow_tests_g = TRUE;
    }
#else
    printf("Cannot determine if overflows generate a SIGFPE; assuming yes.\n");
    printf("Overflow cases will not be tested.\n");
    skip_overflow_tests_g = TRUE;
#endif
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
    hid_t		file=-1, type=-1, space=-1, dset=-1, t2=-1;
    
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
    hid_t		file=-1, type=-1, space=-1, dset=-1, t2=-1, attr1=-1;
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
 * Function:	test_conv_int
 *
 * Purpose:	Test atomic number conversions.
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Robb Matzke
 *              Wednesday, June 10, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_conv_int (void)
{
    const size_t	ntests=100;
    const size_t	nelmts=2000;
    
    size_t		i, j;
    void		*buf=NULL, *saved=NULL;
    unsigned char	byte[4];

    /*---------------------------------------------------------------------
     * Test some specific overflow/underflow cases.
     *--------------------------------------------------------------------- 
     */
    printf ("%-70s", "Testing integer overflow conversions");
    fflush (stdout);

    /* (unsigned)0x80000000 -> (unsigned)0xffff */
    byte[0] = byte[1] = byte[2] = 0;
    byte[3] = 0x80;
    if (H5Tconvert (H5T_STD_U32LE, H5T_STD_U16LE, 1, byte, NULL)<0) {
	goto error;
    }
    if (byte[0]!=0xff || byte[1]!=0xff) {
	puts ("*FAILED*");
	puts ("   (unsigned)0x80000000 -> (unsigned)0xffff");
	goto error;
    }

    /* (unsigned)0xffffffff -> (signed)0x7fff */
    byte[0] = byte[1] = byte[2] = byte[3] = 0xff;
    if (H5Tconvert (H5T_STD_U32LE, H5T_STD_I16LE, 1, byte, NULL)<0) {
	goto error;
    }
    if (byte[0]!=0xff || byte[1]!=0x7f) {
	puts ("*FAILED*");
	puts ("   (unsigned)0xffffffff -> (signed)0x7f");
	goto error;
    }

    /* (signed)0xffffffff -> (unsigned)0x0000 */
    byte[0] = byte[1] = byte[2] = byte[3] = 0xff;
    if (H5Tconvert (H5T_STD_I32LE, H5T_STD_U16LE, 1, byte, NULL)<0) {
	goto error;
    }
    if (byte[0]!=0x00 || byte[1]!=0x00) {
	puts ("*FAILED*");
	puts ("   (signed)0xffffffff -> (unsigned)0x00");
	goto error;
    }

    /* (signed)0x7fffffff -> (unsigned)0xffff */
    byte[0] = byte[1] = byte[2] = 0xff;
    byte[3] = 0x7f;
    if (H5Tconvert (H5T_STD_I32LE, H5T_STD_U16LE, 1, byte, NULL)<0) {
	goto error;
    }
    if (byte[0]!=0xff || byte[1]!=0xff) {
	puts ("*FAILED*");
	puts ("   (signed)0x7fffffff -> (unsigned)0xffff");
	goto error;
    }

    /* (signed)0x7fffffff -> (signed)0x7fff */
    byte[0] = byte[1] = byte[2] = 0xff;
    byte[3] = 0x7f;
    if (H5Tconvert (H5T_STD_I32LE, H5T_STD_I16LE, 1, byte, NULL)<0) {
	goto error;
    }
    if (byte[0]!=0xff || byte[1]!=0x7f) {
	puts ("*FAILED*");
	puts ("   (signed)0x7fffffff -> (signed)0x7fff");
	goto error;
    }

    /* (signed)0xbfffffff -> (signed)0x8000 */
    byte[0] = byte[1] = byte[2] = 0xff;
    byte[3] = 0xbf;
    if (H5Tconvert (H5T_STD_I32LE, H5T_STD_I16LE, 1, byte, NULL)<0) {
	goto error;
    }
    if (byte[0]!=0x00 || byte[1]!=0x80) {
	puts ("*FAILED*");
	puts ("   (signed)0xbfffffff -> (signed)0x8000");
	goto error;
    }

    puts (" PASSED");
    
    
    /*-----------------------------------------------------------------------
     * Test random cases.
     *----------------------------------------------------------------------- 
     */
    printf ("%-70s", "Testing random integer conversions");
    fflush (stdout);
    
    /* Allocate buffers */
    buf = malloc (nelmts*8);
    saved = malloc (nelmts*8);

    for (i=0; i<ntests; i++) {

	/* Start with NATIVE_INT */
	for (j=0; j<nelmts; j++) ((int*)buf)[j] = rand();
	memcpy (saved, buf, nelmts*sizeof(int));

	/* Convert there and back */
	if (H5Tconvert (H5T_NATIVE_INT, H5T_STD_I64LE, nelmts, buf,
			NULL)<0) goto error;
	if (H5Tconvert (H5T_STD_I64LE, H5T_NATIVE_INT, nelmts, buf,
			NULL)<0) goto error;

	/* Check results */
	for (j=0; j<nelmts; j++) {
	    if (((int*)buf)[j]!=((int*)saved)[j]) {
		puts ("*FAILED*");
		printf ("   Test %lu, elmt %lu, got %d instead of %d\n",
			(unsigned long)i, (unsigned long)j,
			((int*)buf)[j], ((int*)saved)[j]);
		goto error;
	    }
	}
    }

    puts (" PASSED");
    free (buf);
    free (saved);
    return 0;

 error:
    if (buf) free (buf);
    if (saved) free (saved);
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	my_isnan
 *
 * Purpose:	Determines whether VAL points to NaN.
 *
 * Return:	TRUE or FALSE
 *
 * Programmer:	Robb Matzke
 *              Monday, July  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
my_isnan(flt_t type, void *val)
{
    int retval;
    char s[256];
    
    switch (type) {
    case FLT_FLOAT:
	retval = (*((float*)val)!=*((float*)val));
	break;

    case FLT_DOUBLE:
	retval = (*((double*)val)!=*((double*)val));
	break;

#ifdef USE_LDOUBLE
    case FLT_LDOUBLE:
	retval = (*((long double*)val)!=*((long double*)val));
	break;
#endif
	
    default:
	return 0;
    }

    /*
     * Sometimes NaN==NaN (e.g., DEC Alpha) so we try to print it and see if
     * the result contains a NaN string.
     */
    if (!retval) {
	switch (type) {
	case FLT_FLOAT:
	    sprintf(s, "%g", *((float*)val));
	    break;
	    
	case FLT_DOUBLE:
	    sprintf(s, "%g", *((double*)val));
	    break;

#ifdef USE_LDOUBLE
	case FLT_LDOUBLE:
	    sprintf(s, "%Lg", *((long double*)val));
	    break;
#endif

	default:
	    return 0;
	}

	if (!strstr(s, "NaN") || !strstr(s, "NAN") || !strstr(s, "nan")) {
	    retval = 1;
	}
    }

    return retval;
}


/*-------------------------------------------------------------------------
 * Function:	test_conv_flt_1
 *
 * Purpose:	Test conversion of random floating point values from SRC to
 *		DST.  These types should be H5T_NATIVE_FLOAT,
 *		H5T_NATIVE_DOUBLE, or H5T_NATIVE_LDOUBLE.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_flt_1 (const char *name, hid_t src, hid_t dst)
{
    flt_t		src_type, dst_type;	/*data types		*/
    const size_t	ntests=5;		/*number of tests	*/
    const size_t	nelmts=200000;		/*num values per test	*/
    const size_t	max_fails=8;		/*max number of failures*/
    size_t		fails_all_tests=0;	/*number of failures	*/
    size_t		fails_this_test;	/*fails for this test	*/
    const char		*src_type_name = NULL;	/*source type name	*/
    const char		*dst_type_name = NULL;	/*destination type name	*/
    size_t		src_size, dst_size;	/*type sizes		*/
    unsigned char	*buf = NULL;		/*buffer for conversion	*/
    unsigned char	*saved = NULL;		/*original values	*/
    char		str[256];		/*hello string		*/
    float		hw_f;			/*hardware-converted 	*/
    double		hw_d;			/*hardware-converted	*/
#ifdef USE_LDOUBLE
    long double		hw_ld;			/*hardware-converted	*/
#endif
    unsigned char	*hw=NULL;		/*ptr to hardware-conv'd*/
    size_t		i, j, k;		/*counters		*/
    int			endian;			/*machine endianess	*/

#ifdef HANDLE_SIGFPE
    pid_t		child_pid;		/*process ID of child	*/
    int			status;			/*child exit status	*/
    
    /*
     * Some systems generage SIGFPE during floating point overflow and we
     * cannot assume that we can continue from such a signal.  Therefore, we
     * fork here and let the child run the test and return the number of
     * failures with the exit status.
     */
    fflush(stdout);
    fflush(stderr);
    if ((child_pid=fork())<0) {
	perror("fork");
	return 1;
    } else if (child_pid>0) {
	while (child_pid!=waitpid(child_pid, &status, 0)) /*void*/;
	if (WIFEXITED(status) && 255==WEXITSTATUS(status)) {
	    return 0; /*child exit after catching SIGFPE*/
	} else if (WIFEXITED(status)) {
	    return WEXITSTATUS(status);
	} else {
	    puts("   Child didn't exit normally.");
	    return 1;
	}
    }
#endif

    /*
     * The remainder of this function is executed only by the child if
     * HANDLE_SIGFPE is defined.
     */
    signal(SIGFPE,fpe_handler);

    /* What are the names of the source and destination types */
    if (H5Tequal(src, H5T_NATIVE_FLOAT)) {
	src_type_name = "float";
	src_type = FLT_FLOAT;
    } else if (H5Tequal(src, H5T_NATIVE_DOUBLE)) {
	src_type_name = "double";
	src_type = FLT_DOUBLE;
#ifdef USE_LDOUBLE
    } else if (H5Tequal(src, H5T_NATIVE_LDOUBLE)) {
	src_type_name = "long double";
	src_type = FLT_LDOUBLE;
#endif
    } else {
	src_type_name = "UNKNOWN";
	src_type = FLT_OTHER;
    }
    
    if (H5Tequal(dst, H5T_NATIVE_FLOAT)) {
	dst_type_name = "float";
	dst_type = FLT_FLOAT;
    } else if (H5Tequal(dst, H5T_NATIVE_DOUBLE)) {
	dst_type_name = "double";
	dst_type = FLT_DOUBLE;
#ifdef USE_LDOUBLE
    } else if (H5Tequal(dst, H5T_NATIVE_LDOUBLE)) {
	dst_type_name = "long double";
	dst_type = FLT_LDOUBLE;
#endif
    } else {
	dst_type_name = "UNKNOWN";
	dst_type = FLT_OTHER;
    }

    /* Sanity checks */
    assert(sizeof(float)!=sizeof(double));
    if (FLT_OTHER==src_type || FLT_OTHER==dst_type) {
	sprintf(str, "Testing random %s %s -> %s conversions",
		name, src_type_name, dst_type_name);
	printf ("%-70s", str);
	puts("*FAILED*");
	puts("   Unknown data type.");
	goto error;
    }
    
    /* Allocate buffers */
    endian = H5Tget_order(H5T_NATIVE_FLOAT);
    src_size = H5Tget_size(src);
    dst_size = H5Tget_size(dst);
    buf   = malloc(nelmts*MAX(src_size, dst_size));
    saved = malloc(nelmts*MAX(src_size, dst_size));
    noverflows_g = 0;

    for (i=0; i<ntests; i++) {

	/*
	 * If it looks like it might take a long time then print a progress
	 * report between each test.
	 */
	sprintf(str, "Testing random %s %s -> %s conversions (test %d/%d)",
		name, src_type_name, dst_type_name, (int)i+1, (int)ntests);
	printf ("%-70s", str);
	fflush(stdout);
	fails_this_test = 0;

	/*
	 * Initialize the source buffers to random bits.  The `buf' buffer
	 * will be used for the conversion while the `saved' buffer will be
	 * used for the comparison later.
	 */
	if (!skip_overflow_tests_g) {
	    for (j=0; j<nelmts*src_size; j++) buf[j] = saved[j] = rand();
	} else {
	    for (j=0; j<nelmts; j++) {
		unsigned char temp[32];
		if (src_size<=dst_size) {
		    for (k=0; k<dst_size; k++) buf[j*src_size+k] = rand();
		} else {
		    for (k=0; k<dst_size; k++) temp[k] = rand();
		    if (FLT_DOUBLE==src_type && FLT_FLOAT==dst_type) {
			hw_d = *((float*)temp);
			memcpy(buf+j*src_size, &hw_d, src_size);
#ifdef USE_LDOUBLE
		    } else if (FLT_LDOUBLE==src_type && FLT_FLOAT==dst_type) {
			hw_ld = *((float*)temp);
			memcpy(buf+j*src_size, &hw_ld, src_size);
		    } else if (FLT_LDOUBLE==src_type && FLT_DOUBLE==dst_type) {
			hw_ld = *((double*)temp);
			memcpy(buf+j*src_size, &hw_ld, src_size);
#endif
		    }
		}
		memcpy(saved+j*src_size, buf+j*src_size, src_size);
	    }
	}

	/* Perform the conversion in software */
	if (H5Tconvert(src, dst, nelmts, buf, NULL)<0) goto error;

	/* Check the software results against the hardware */
	for (j=0; j<nelmts; j++) {
	    hw_f = 911.0;
	    hw_d = 911.0;
#ifdef USE_LDOUBLE
	    hw_ld = 911.0;
#endif

	    /* The hardware conversion */
	    if (FLT_FLOAT==src_type) {
		if (FLT_FLOAT==dst_type) {
		    hw_f = ((float*)saved)[j];
		    hw = (unsigned char*)&hw_f;
		} else if (FLT_DOUBLE==dst_type) {
		    hw_d = ((float*)saved)[j];
		    hw = (unsigned char*)&hw_d;
#ifdef USE_LDOUBLE
		} else {
		    hw_ld = ((float*)saved)[j];
		    hw = (unsigned char*)&hw_ld;
#endif
		}
	    } else if (FLT_DOUBLE==src_type) {
		if (FLT_FLOAT==dst_type) {
		    hw_f = ((double*)saved)[j];
		    hw = (unsigned char*)&hw_f;
		} else if (FLT_DOUBLE==dst_type) {
		    hw_d = ((double*)saved)[j];
		    hw = (unsigned char*)&hw_d;
#ifdef USE_LDOUBLE
		} else {
		    hw_ld = ((double*)saved)[j];
		    hw = (unsigned char*)&hw_ld;
#endif
		}
#ifdef USE_LDOUBLE
	    } else {
		if (FLT_FLOAT==dst_type) {
		    hw_f = ((long double*)saved)[j];
		    hw = (unsigned char*)&hw_f;
		} else if (FLT_DOUBLE==dst_type) {
		    hw_d = ((long double*)saved)[j];
		    hw = (unsigned char*)&hw_d;
		} else {
		    hw_ld = ((long double*)saved)[j];
		    hw = (unsigned char*)&hw_ld;
		}
#endif
	    }

	    /* Are the two results the same? */
	    for (k=0; k<dst_size; k++) {
		if (buf[j*dst_size+k]!=hw[k]) break;
	    }
	    if (k==dst_size) continue; /*no error*/

#if 1
	    /*
	     * Assume same if both results are NaN.  There are many NaN bit
	     * patterns and the software doesn't attemt to emulate the
	     * hardware in this regard.  Instead, software uses a single bit
	     * pattern for NaN by setting the significand to all ones.
	     */
	    if (FLT_FLOAT==dst_type &&
		my_isnan(dst_type, (float*)buf+j) &&
		my_isnan(dst_type, hw)) {
		continue;
	    } else if (FLT_DOUBLE==dst_type &&
		       my_isnan(dst_type, (double*)buf+j) &&
		       my_isnan(dst_type, hw)) {
		continue;
#ifdef USE_LDOUBLE
	    } else if (FLT_LDOUBLE==dst_type &&
		       my_isnan(dst_type, (long double*)buf+j) &&
		       my_isnan(dst_type, hw)) {
		continue;
#endif
	    }
#endif

#if 1
	    /*
	     * Assume same if hardware result is NaN.  This is because the
	     * hardware conversions on some machines return NaN instead of
	     * overflowing to +Inf or -Inf or underflowing to +0 or -0.
	     */
	    if (my_isnan(dst_type, hw)) continue;
#endif

#if 1
	    /*
	     * Instead of matching down to the bit, just make sure the
	     * exponents are the same and the mantissa is the same to a
	     * certain precision.  This is needed on machines that don't
	     * round as expected.
	     */
	    {
		double	check_mant[2];
		int	check_expo[2];
		
		if (FLT_FLOAT==dst_type) {
		    check_mant[0] = frexp(((float*)buf)[j], check_expo+0);
		    check_mant[1] = frexp(((float*)hw)[0], check_expo+1);
		} else if (FLT_DOUBLE==dst_type) {
		    check_mant[0] = frexp(((double*)buf)[j], check_expo+0);
		    check_mant[1] = frexp(((double*)hw)[0], check_expo+1);
#ifdef USE_LDOUBLE
		} else {
		    check_mant[0] = frexp(((long double*)buf)[j],check_expo+0);
		    check_mant[1] = frexp(((long double*)hw)[0],check_expo+1);
#endif
		}
		if (check_expo[0]==check_expo[1] &&
		    fabs(check_mant[0]-check_mant[1])<0.000001) {
		    continue;
		}
	    }
#endif

	    if (0==fails_this_test++) puts("*FAILED*");
	    printf("   test %u, elmt %u\n", (unsigned)i+1, (unsigned)j);
	    
	    printf("      src =");
	    for (k=0; k<src_size; k++) {
		printf(" %02x", saved[j*src_size+ENDIAN(endian,src_size,k)]);
	    }
	    printf("%*s", 3*MAX(0, (ssize_t)dst_size-(ssize_t)src_size), "");
	    if (FLT_FLOAT==src_type) {
		printf(" %29.20e\n", ((float*)saved)[j]);
	    } else if (FLT_DOUBLE==src_type) {
		printf(" %29.20e\n", ((double*)saved)[j]);
#ifdef USE_LDOUBLE
	    } else {
		printf(" %29.20Le\n", ((long double*)saved)[j]);
#endif
	    }

	    printf("      dst =");
	    for (k=0; k<dst_size; k++) {
		printf(" %02x", buf[j*dst_size+ENDIAN(endian,dst_size,k)]);
	    }
	    printf("%*s", 3*MAX(0, (ssize_t)src_size-(ssize_t)dst_size), "");
	    if (FLT_FLOAT==dst_type) {
		printf(" %29.20e\n", ((float*)buf)[j]);
	    } else if (FLT_DOUBLE==dst_type) {
		printf(" %29.20e\n", ((double*)buf)[j]);
#ifdef USE_LDOUBLE
	    } else {
		printf(" %29.20Le\n", ((long double*)buf)[j]);
#endif
	    }

	    printf("      ans =");
	    for (k=0; k<dst_size; k++) {
		printf(" %02x", hw[ENDIAN(endian,dst_size,k)]);
	    }
	    printf("%*s", 3*MAX(0, (ssize_t)src_size-(ssize_t)dst_size), "");
	    if (FLT_FLOAT==dst_type) {
		printf(" %29.20e\n", hw_f);
	    } else if (FLT_DOUBLE==dst_type) {
		printf(" %29.20e\n", hw_d);
#ifdef USE_LDOUBLE
	    } else {
		printf(" %29.20Le\n", hw_ld);
#endif
	    }

	    if (++fails_all_tests>=max_fails) {
		puts("   maximum failures reached, aborting test...");
		goto done;
	    }
	}
	puts(" PASSED");
    }
    if (noverflows_g>0) {
	printf("  %d overflow%s\n", noverflows_g, 1==noverflows_g?"":"s");
    }

 done:
    if (buf) free (buf);
    if (saved) free (saved);
#ifdef HANDLE_SIGFPE
    exit(MIN((int)fails_all_tests, 254));
#else
    return (int)fails_all_tests;
#endif

 error:
    if (buf) free (buf);
    if (saved) free (saved);
#ifdef HANDLE_SIGFPE
    exit(MIN(MAX((int)fails_all_tests, 1), 254));
#else
    return MAX((int)fails_all_tests, 1);
#endif
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
    unsigned long	nerrors = 0;

    /* Set the error handler */
    H5Eset_auto (display_error_cb, NULL);

    /* Set the overflow handler */
    H5Tset_overflow(overflow_handler);

    /* Do the tests */
    nerrors += test_classes()<0 ? 1 : 0;
    nerrors += test_copy()<0 ? 1 : 0;
    nerrors += test_compound()<0 ? 1 : 0;
    nerrors += test_transient ()<0 ? 1 : 0;
    nerrors += test_named ()<0 ? 1 : 0;
    nerrors += test_conv_int ()<0 ? 1 : 0;

    /* Does floating point overflow generate a SIGFPE? */
    generates_sigfpe();

    /* Test degenerate cases */
    nerrors += test_conv_flt_1("noop", H5T_NATIVE_FLOAT, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1("noop", H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE);

    /* Test hardware conversion functions */
    nerrors += test_conv_flt_1("hw", H5T_NATIVE_FLOAT, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1("hw", H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT);

    /* Test software conversion functions */
    H5Tunregister(H5T_conv_float_double);
    H5Tunregister(H5T_conv_double_float);
    nerrors += test_conv_flt_1("sw", H5T_NATIVE_FLOAT, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1("sw", H5T_NATIVE_FLOAT, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1("sw", H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1("sw", H5T_NATIVE_DOUBLE, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1("sw", H5T_NATIVE_LDOUBLE, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1("sw", H5T_NATIVE_LDOUBLE, H5T_NATIVE_DOUBLE);
    
    if (nerrors) {
        printf("***** %lu FAILURE%s! *****\n",
               nerrors, 1==nerrors?"":"S");
        exit(1);
    }
    printf("All data type tests passed.\n");
    cleanup ();
    return 0;
}
