/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, December  9, 1997
 *
 * Purpose:     Tests the data type interface (H5T)
 */

#include <h5test.h>

/* Number of times to run each test */
#define NTESTS	1

/* Define if you want to see a count of overflows */
#undef SHOW_OVERFLOWS

/*
 * Offset from alinged memory returned by malloc().  This can be used to test
 * that type conversions handle non-aligned buffers correctly.
 */
#define ALIGNMENT	1

/*
 * Define if you want to test alignment code on a machine that doesn't
 * normally require alignment. When set, all native data types must be aligned
 * on a byte boundary equal to the data size.
 */
#define TEST_ALIGNMENT

/* Alignment test stuff */
#ifdef TEST_ALIGNMENT
#define H5T_PACKAGE
#include <H5Tpkg.h>
#endif
#define SET_ALIGNMENT(TYPE,VAL) \
    H5T_NATIVE_##TYPE##_ALIGN_g=MAX(H5T_NATIVE_##TYPE##_ALIGN_g, VAL)

const char *FILENAME[] = {
    "dtypes1",
    "dtypes2",
    NULL
};

typedef struct complex_t {
    double                  re;
    double                  im;
} complex_t;

/*
 * Count up or down depending on whether the machine is big endian or little
 * endian.  If local variable `endian' is H5T_ORDER_BE then the result will
 * be I, otherwise the result will be Z-(I+1).
 */
#define ENDIAN(Z,I)	(H5T_ORDER_BE==endian?(I):(Z)-((I)+1))


typedef enum flt_t {
    FLT_FLOAT, FLT_DOUBLE, FLT_LDOUBLE, FLT_OTHER
} flt_t;

typedef enum int_t {
    INT_CHAR, INT_UCHAR, INT_SHORT, INT_USHORT, INT_INT, INT_UINT,
    INT_LONG, INT_ULONG, INT_LLONG, INT_ULLONG, INT_OTHER
} int_t;

/* Count the number of overflows */
#ifdef SHOW_OVERFLOWS
static int noverflows_g = 0;
#endif

/* Skip overflow tests if non-zero */
static int skip_overflow_tests_g = 0;

/* Don't use hardware conversions if set */
static int without_hardware_g = 0;

/* Count opaque conversions */
static int num_opaque_conversions_g = 0;

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

/* Allocates memory aligned on a certain boundary. */
#define aligned_malloc(Z)	((void*)((char*)malloc(ALIGNMENT+Z)+ALIGNMENT))
#define aligned_free(M)		free((char*)(M)-ALIGNMENT)

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
fpe_handler(int UNUSED signo)
{
    SKIPPED();
    puts("    Test skipped due to SIGFPE.");
#ifndef HANDLE_SIGFPE
    puts("    Remaining tests could not be run.");
    puts("    Please turn off SIGFPE on overflows and try again.");
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
#ifdef SHOW_OVERFLOWS
static herr_t
overflow_handler(hid_t UNUSED src_id, hid_t UNUSED dst_id,
		 void UNUSED *src_buf, void UNUSED *dst_buf)
{
    noverflows_g++;
    return -1;
}
#endif


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
	puts("Floating-point overflow cases will be tested.");
	skip_overflow_tests_g = FALSE;
    } else if (WIFSIGNALED(status) && SIGFPE==WTERMSIG(status)) {
	puts("Floating-point overflow cases cannot be safely tested.");
	skip_overflow_tests_g = TRUE;
	/* delete the core dump file that SIGFPE may have created */
	unlink("core");
    }
#else
    puts("Cannot determine if floating-point overflows generate a SIGFPE;");
    puts("assuming yes.");
    puts("Overflow cases will not be tested.");
    skip_overflow_tests_g = TRUE;
#endif
}


/*-------------------------------------------------------------------------
 * Function:	reset_hdf5
 *
 * Purpose:	Reset the hdf5 library.  This causes statistics to be printed
 *		and counters to be reset.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
reset_hdf5(void)
{
    h5_reset();
#ifdef SHOW_OVERFLOWS
    H5Tset_overflow(overflow_handler);
#endif
    if (without_hardware_g) h5_no_hwconv();
#ifdef TEST_ALIGNMENT
    SET_ALIGNMENT(SCHAR,   SIZEOF_CHAR);
    SET_ALIGNMENT(UCHAR,   SIZEOF_CHAR);
    SET_ALIGNMENT(SHORT,   SIZEOF_SHORT);
    SET_ALIGNMENT(USHORT,  SIZEOF_SHORT);
    SET_ALIGNMENT(INT,     SIZEOF_INT);
    SET_ALIGNMENT(UINT,    SIZEOF_INT);
    SET_ALIGNMENT(LONG,    SIZEOF_LONG);
    SET_ALIGNMENT(ULONG,   SIZEOF_LONG);
    SET_ALIGNMENT(LLONG,   SIZEOF_LONG_LONG);
    SET_ALIGNMENT(ULLONG,  SIZEOF_LONG_LONG);
    SET_ALIGNMENT(FLOAT,   SIZEOF_FLOAT);
    SET_ALIGNMENT(DOUBLE,  SIZEOF_DOUBLE);
    SET_ALIGNMENT(LDOUBLE, SIZEOF_LONG_DOUBLE);
#endif

}


/*-------------------------------------------------------------------------
 * Function:    test_classes
 *
 * Purpose:     Test type classes
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_classes(void)
{
    H5T_class_t		tcls;
    
    TESTING("H5Tget_class()");

    if ((tcls=H5Tget_class(H5T_NATIVE_INT))<0) goto error;
    if (H5T_INTEGER!=tcls) {
	FAILED();
        puts("    Invalid type class for H5T_NATIVE_INT");
        goto error;
    }
    if ((tcls=H5Tget_class(H5T_NATIVE_DOUBLE))<0) goto error;
    if (H5T_FLOAT!=tcls) {
	FAILED();
	puts("    Invalid type class for H5T_NATIVE_DOUBLE");
        goto error;
    }
    PASSED();
    return 0;

  error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    test_copy
 *
 * Purpose:     Are we able to copy a data type?
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy(void)
{
    hid_t               a_copy;
    herr_t		status;

    TESTING("H5Tcopy()");

    if ((a_copy = H5Tcopy(H5T_NATIVE_SHORT)) < 0) goto error;
    if (H5Tclose(a_copy) < 0) goto error;

    /* We should not be able to close a built-in byte */
    H5E_BEGIN_TRY {
	status = H5Tclose (H5T_NATIVE_SCHAR);
    } H5E_END_TRY;
    if (status>=0) {
	FAILED();
	puts ("    Should not be able to close a predefined type!");
	goto error;
    }

    PASSED();
    return 0;

  error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    test_compound_1
 *
 * Purpose:     Tests various things about compound data types.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Robb Matzke
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_1(void)
{
    complex_t               tmp;
    hid_t                   complex_id;

    TESTING("compound data types");

    /* Create the empty type */
    if ((complex_id = H5Tcreate(H5T_COMPOUND, sizeof tmp))<0) goto error;

    /* Add a couple fields */
    if (H5Tinsert(complex_id, "real", HOFFSET(complex_t, re),
		  H5T_NATIVE_DOUBLE)<0) goto error;
    if (H5Tinsert(complex_id, "imaginary", HOFFSET(complex_t, im),
		  H5T_NATIVE_DOUBLE)<0) goto error;

    if (H5Tclose (complex_id)<0) goto error;
    PASSED();
    return 0;

  error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_compound_2
 *
 * Purpose:	Tests a compound type conversion where the source and
 *		destination are the same except for the order of the
 *		elements.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Thursday, June 17, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_2(void)
{
    struct st {
	int a, b, c[4], d, e;
    } *s_ptr;
    struct dt {
	int e, d, c[4], b, a;
    } *d_ptr;

    const int		nelmts = 200000;
    const size_t	four = 4;
    unsigned char	*buf=NULL, *orig=NULL, *bkg=NULL;
    hid_t		st=-1, dt=-1;
    int			i;

    TESTING("compound element reordering");

    /* Sizes should be the same, but be careful just in case */
    buf = malloc(nelmts * MAX(sizeof(struct st), sizeof(struct dt)));
    bkg = malloc(nelmts * sizeof(struct dt));
    orig = malloc(nelmts * sizeof(struct st));
    for (i=0; i<nelmts; i++) {
	s_ptr = ((struct st*)orig) + i;
	s_ptr->a    = i*8+0;
	s_ptr->b    = i*8+1;
	s_ptr->c[0] = i*8+2;
	s_ptr->c[1] = i*8+3;
	s_ptr->c[2] = i*8+4;
	s_ptr->c[3] = i*8+5;
	s_ptr->d    = i*8+6;
	s_ptr->e    = i*8+7;
    }
    memcpy(buf, orig, nelmts*sizeof(struct st));

    /* Build hdf5 datatypes */
    if ((st=H5Tcreate(H5T_COMPOUND, sizeof(struct st)))<0 ||
	H5Tinsert(st, "a", HOFFSET(struct st, a), H5T_NATIVE_INT)<0 ||
	H5Tinsert(st, "b", HOFFSET(struct st, b), H5T_NATIVE_INT)<0 ||
	H5Tinsert_array(st, "c", HOFFSET(struct st, c), 1, &four, NULL,
			H5T_NATIVE_INT)<0 ||
	H5Tinsert(st, "d", HOFFSET(struct st, d), H5T_NATIVE_INT)<0 ||
	H5Tinsert(st, "e", HOFFSET(struct st, e), H5T_NATIVE_INT)<0)
	goto error;
    
    if ((dt=H5Tcreate(H5T_COMPOUND, sizeof(struct dt)))<0 ||
	H5Tinsert(dt, "a", HOFFSET(struct dt, a), H5T_NATIVE_INT)<0 ||
	H5Tinsert(dt, "b", HOFFSET(struct dt, b), H5T_NATIVE_INT)<0 ||
	H5Tinsert_array(dt, "c", HOFFSET(struct dt, c), 1, &four, NULL,
			H5T_NATIVE_INT)<0 ||
	H5Tinsert(dt, "d", HOFFSET(struct dt, d), H5T_NATIVE_INT)<0 ||
	H5Tinsert(dt, "e", HOFFSET(struct dt, e), H5T_NATIVE_INT)<0)
	goto error;
    
    /* Perform the conversion */
    if (H5Tconvert(st, dt, nelmts, buf, bkg, H5P_DEFAULT)<0) goto error;

    /* Compare results */
    for (i=0; i<nelmts; i++) {
	s_ptr = ((struct st*)orig) + i;
	d_ptr = ((struct dt*)buf)  + i;
	if (s_ptr->a    != d_ptr->a    ||
	    s_ptr->b    != d_ptr->b    ||
	    s_ptr->c[0] != d_ptr->c[0] ||
	    s_ptr->c[1] != d_ptr->c[1] ||
	    s_ptr->c[2] != d_ptr->c[2] ||
	    s_ptr->c[3] != d_ptr->c[3] ||
	    s_ptr->d    != d_ptr->d    ||
	    s_ptr->e    != d_ptr->e) {
	    FAILED();
	    printf("    i=%d\n", i);
	    printf("    src={a=%d, b=%d, c=[%d,%d,%d,%d], d=%d, e=%d\n",
		   s_ptr->a, s_ptr->b, s_ptr->c[0], s_ptr->c[1], s_ptr->c[2],
		   s_ptr->c[3], s_ptr->d, s_ptr->e);
	    printf("    dst={a=%d, b=%d, c=[%d,%d,%d,%d], d=%d, e=%d\n",
		   d_ptr->a, d_ptr->b, d_ptr->c[0], d_ptr->c[1], d_ptr->c[2],
		   d_ptr->c[3], d_ptr->d, d_ptr->e);
	    goto error;
	}
    }
    
    /* Release resources */
    free(buf);
    free(bkg);
    free(orig);
    if (H5Tclose(st)<0 || H5Tclose(dt)<0) goto error;

    PASSED();
    reset_hdf5();
    return 0;

 error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_compound_3
 *
 * Purpose:	Tests compound conversions where the source and destination
 *		are the same except the destination is missing a couple
 *		members which appear in the source.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Thursday, June 17, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_3(void)
{
    struct st {
	int a, b, c[4], d, e;
    } *s_ptr;
    struct dt {
	int a,    c[4],    e;
    } *d_ptr;

    const int		nelmts = 200000;
    const size_t	four = 4;
    unsigned char	*buf=NULL, *orig=NULL, *bkg=NULL;
    hid_t		st=-1, dt=-1;
    int			i;

    TESTING("compound subset conversions");

    /* Initialize */
    buf = malloc(nelmts * MAX(sizeof(struct st), sizeof(struct dt)));
    bkg = malloc(nelmts * sizeof(struct dt));
    orig = malloc(nelmts * sizeof(struct st));
    for (i=0; i<nelmts; i++) {
	s_ptr = ((struct st*)orig) + i;
	s_ptr->a    = i*8+0;
	s_ptr->b    = i*8+1;
	s_ptr->c[0] = i*8+2;
	s_ptr->c[1] = i*8+3;
	s_ptr->c[2] = i*8+4;
	s_ptr->c[3] = i*8+5;
	s_ptr->d    = i*8+6;
	s_ptr->e    = i*8+7;
    }
    memcpy(buf, orig, nelmts*sizeof(struct st));

    /* Build hdf5 datatypes */
    if ((st=H5Tcreate(H5T_COMPOUND, sizeof(struct st)))<0 ||
	H5Tinsert(st, "a", HOFFSET(struct st, a), H5T_NATIVE_INT)<0 ||
	H5Tinsert(st, "b", HOFFSET(struct st, b), H5T_NATIVE_INT)<0 ||
	H5Tinsert_array(st, "c", HOFFSET(struct st, c), 1, &four, NULL,
			H5T_NATIVE_INT)<0 ||
	H5Tinsert(st, "d", HOFFSET(struct st, d), H5T_NATIVE_INT)<0 ||
	H5Tinsert(st, "e", HOFFSET(struct st, e), H5T_NATIVE_INT)<0)
	goto error;
    
    if ((dt=H5Tcreate(H5T_COMPOUND, sizeof(struct dt)))<0 ||
	H5Tinsert(dt, "a", HOFFSET(struct dt, a), H5T_NATIVE_INT)<0 ||
	H5Tinsert_array(dt, "c", HOFFSET(struct dt, c), 1, &four, NULL,
			H5T_NATIVE_INT)<0 ||
	H5Tinsert(dt, "e", HOFFSET(struct dt, e), H5T_NATIVE_INT)<0)
	goto error;
    
    /* Perform the conversion */
    if (H5Tconvert(st, dt, nelmts, buf, bkg, H5P_DEFAULT)<0) goto error;

    /* Compare results */
    for (i=0; i<nelmts; i++) {
	s_ptr = ((struct st*)orig) + i;
	d_ptr = ((struct dt*)buf)  + i;
	if (s_ptr->a    != d_ptr->a    ||
	    s_ptr->c[0] != d_ptr->c[0] ||
	    s_ptr->c[1] != d_ptr->c[1] ||
	    s_ptr->c[2] != d_ptr->c[2] ||
	    s_ptr->c[3] != d_ptr->c[3] ||
	    s_ptr->e    != d_ptr->e) {
	    FAILED();
	    printf("    i=%d\n", i);
	    printf("    src={a=%d, b=%d, c=[%d,%d,%d,%d], d=%d, e=%d\n",
		   s_ptr->a, s_ptr->b, s_ptr->c[0], s_ptr->c[1], s_ptr->c[2],
		   s_ptr->c[3], s_ptr->d, s_ptr->e);
	    printf("    dst={a=%d, c=[%d,%d,%d,%d], e=%d\n",
		   d_ptr->a, d_ptr->c[0], d_ptr->c[1], d_ptr->c[2],
		   d_ptr->c[3], d_ptr->e);
	    goto error;
	}
    }
    
    /* Release resources */
    free(buf);
    free(bkg);
    free(orig);
    if (H5Tclose(st)<0 || H5Tclose(dt)<0) goto error;

    PASSED();
    reset_hdf5();
    return 0;

 error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_compound_4
 *
 * Purpose:	Tests compound conversions when the destination has the same
 *		fields as the source but one or more of the fields are
 *		smaller.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Thursday, June 17, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_4(void)
{
    
    struct st {
	int a, b, c[4], d, e;
    } *s_ptr;
    struct dt {
	short b;
	int a, c[4];
	short d;
	int e;
    } *d_ptr;

    const int		nelmts = 200000;
    const size_t	four = 4;
    unsigned char	*buf=NULL, *orig=NULL, *bkg=NULL;
    hid_t		st=-1, dt=-1;
    int			i;

    TESTING("compound element shrinking & reordering");

    /* Sizes should be the same, but be careful just in case */
    buf = malloc(nelmts * MAX(sizeof(struct st), sizeof(struct dt)));
    bkg = malloc(nelmts * sizeof(struct dt));
    orig = malloc(nelmts * sizeof(struct st));
    for (i=0; i<nelmts; i++) {
	s_ptr = ((struct st*)orig) + i;
	s_ptr->a    = i*8+0;
	s_ptr->b    = (i*8+1) & 0x7fff;
	s_ptr->c[0] = i*8+2;
	s_ptr->c[1] = i*8+3;
	s_ptr->c[2] = i*8+4;
	s_ptr->c[3] = i*8+5;
	s_ptr->d    = (i*8+6) & 0x7fff;
	s_ptr->e    = i*8+7;
    }
    memcpy(buf, orig, nelmts*sizeof(struct st));

    /* Build hdf5 datatypes */
    if ((st=H5Tcreate(H5T_COMPOUND, sizeof(struct st)))<0 ||
	H5Tinsert(st, "a", HOFFSET(struct st, a), H5T_NATIVE_INT)<0 ||
	H5Tinsert(st, "b", HOFFSET(struct st, b), H5T_NATIVE_INT)<0 ||
	H5Tinsert_array(st, "c", HOFFSET(struct st, c), 1, &four, NULL,
			H5T_NATIVE_INT)<0 ||
	H5Tinsert(st, "d", HOFFSET(struct st, d), H5T_NATIVE_INT)<0 ||
	H5Tinsert(st, "e", HOFFSET(struct st, e), H5T_NATIVE_INT)<0)
	goto error;
    
    if ((dt=H5Tcreate(H5T_COMPOUND, sizeof(struct dt)))<0 ||
	H5Tinsert(dt, "a", HOFFSET(struct dt, a), H5T_NATIVE_INT)<0 ||
	H5Tinsert(dt, "b", HOFFSET(struct dt, b), H5T_NATIVE_SHORT)<0 ||
	H5Tinsert_array(dt, "c", HOFFSET(struct dt, c), 1, &four, NULL,
			H5T_NATIVE_INT)<0 ||
	H5Tinsert(dt, "d", HOFFSET(struct dt, d), H5T_NATIVE_SHORT)<0 ||
	H5Tinsert(dt, "e", HOFFSET(struct dt, e), H5T_NATIVE_INT)<0)
	goto error;
    
    /* Perform the conversion */
    if (H5Tconvert(st, dt, nelmts, buf, bkg, H5P_DEFAULT)<0) goto error;

    /* Compare results */
    for (i=0; i<nelmts; i++) {
	s_ptr = ((struct st*)orig) + i;
	d_ptr = ((struct dt*)buf)  + i;
	if (s_ptr->a    != d_ptr->a    ||
	    s_ptr->b    != d_ptr->b    ||
	    s_ptr->c[0] != d_ptr->c[0] ||
	    s_ptr->c[1] != d_ptr->c[1] ||
	    s_ptr->c[2] != d_ptr->c[2] ||
	    s_ptr->c[3] != d_ptr->c[3] ||
	    s_ptr->d    != d_ptr->d    ||
	    s_ptr->e    != d_ptr->e) {
	    FAILED();
	    printf("    i=%d\n", i);
	    printf("    src={a=%d, b=%d, c=[%d,%d,%d,%d], d=%d, e=%d\n",
		   s_ptr->a, s_ptr->b, s_ptr->c[0], s_ptr->c[1], s_ptr->c[2],
		   s_ptr->c[3], s_ptr->d, s_ptr->e);
	    printf("    dst={a=%d, b=%d, c=[%d,%d,%d,%d], d=%d, e=%d\n",
		   d_ptr->a, d_ptr->b, d_ptr->c[0], d_ptr->c[1], d_ptr->c[2],
		   d_ptr->c[3], d_ptr->d, d_ptr->e);
	    goto error;
	}
    }
    
    /* Release resources */
    free(buf);
    free(bkg);
    free(orig);
    if (H5Tclose(st)<0 || H5Tclose(dt)<0) goto error;

    PASSED();
    reset_hdf5();
    return 0;

 error:
    return 1;
}
    


/*-------------------------------------------------------------------------
 * Function:	test_transient
 *
 * Purpose:	Tests transient data types.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Thursday, June  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_transient (hid_t fapl)
{
    static hsize_t	ds_size[2] = {10, 20};
    hid_t		file=-1, type=-1, space=-1, dset=-1, t2=-1;
    char		filename[1024];
    herr_t		status;
    
    TESTING("transient data types");

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file=H5Fcreate (filename, H5F_ACC_TRUNC|H5F_ACC_DEBUG,
			 H5P_DEFAULT, fapl))<0) goto error;
    if ((space = H5Screate_simple (2, ds_size, ds_size))<0) goto error;

    /* Predefined types cannot be modified or closed */
    H5E_BEGIN_TRY {
	status = H5Tset_precision (H5T_NATIVE_INT, 256);
    } H5E_END_TRY;
    if (status>=0) {
	FAILED();
	puts ("    Predefined types should not be modifiable!");
	goto error;
    }
    H5E_BEGIN_TRY {
	status = H5Tclose (H5T_NATIVE_INT);
    } H5E_END_TRY;
    if (status>=0) {
	FAILED();
	puts ("    Predefined types should not be closable!");
	goto error;
    }

    /* Copying a predefined type results in a modifiable copy */
    if ((type=H5Tcopy (H5T_NATIVE_INT))<0) goto error;
    if (H5Tset_precision (type, 256)<0) goto error;

    /* It should not be possible to create an attribute for a transient type */
    H5E_BEGIN_TRY {
	status = H5Acreate (type, "attr1", H5T_NATIVE_INT, space, H5P_DEFAULT);
    } H5E_END_TRY;
    if (status>=0) {
	FAILED();
	puts ("    Attributes should not be allowed for transient types!");
	goto error;
    }

    /* Create a dataset from a transient data type */
    if (H5Tclose (type)<0) goto error;
    if ((type = H5Tcopy (H5T_NATIVE_INT))<0) goto error;
    if ((dset=H5Dcreate (file, "dset1", type, space, H5P_DEFAULT))<0)
	goto error;

    /* The type returned from a dataset should not be modifiable */
    if ((t2 = H5Dget_type (dset))<0) goto error;
    H5E_BEGIN_TRY {
	status = H5Tset_precision (t2, 256);
    } H5E_END_TRY;
    if (status>=0) {
	FAILED();
	puts ("    Dataset data types should not be modifiable!");
	goto error;
    }
    if (H5Tclose (t2)<0) goto error;

    /*
     * Close the dataset and reopen it, testing that it's type is still
     * read-only.
     */
    if (H5Dclose (dset)<0) goto error;
    if ((dset=H5Dopen (file, "dset1"))<0) goto error;
    if ((t2 = H5Dget_type (dset))<0) goto error;
    H5E_BEGIN_TRY {
	status = H5Tset_precision (t2, 256);
    } H5E_END_TRY;
    if (status>=0) {
	FAILED();
	puts ("    Dataset data types should not be modifiable!");
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
    

    H5Dclose (dset);
    H5Fclose (file);
    H5Tclose (type);
    H5Sclose (space);
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Tclose (t2);
	H5Tclose (type);
	H5Sclose (space);
	H5Dclose (dset);
	H5Fclose (file);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_named
 *
 * Purpose:	Tests named data types.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Monday, June  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_named (hid_t fapl)
{
    hid_t		file=-1, type=-1, space=-1, dset=-1, t2=-1, attr1=-1;
    herr_t		status;
    static hsize_t	ds_size[2] = {10, 20};
    hsize_t		i;
    unsigned 		attr_data[10][20];
    char		filename[1024];
    
    TESTING("named data types");

    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file=H5Fcreate (filename, H5F_ACC_TRUNC|H5F_ACC_DEBUG,
			 H5P_DEFAULT, fapl))<0) goto error;
    if ((space = H5Screate_simple (2, ds_size, ds_size))<0) goto error;

    /* Predefined types cannot be committed */
    H5E_BEGIN_TRY {
	status = H5Tcommit (file, "test_named_1 (should not exist)",
			    H5T_NATIVE_INT);
    } H5E_END_TRY;
    if (status>=0) {
	FAILED();
	puts ("    Predefined types should not be committable!");
	goto error;
    }

    /* Copy a predefined data type and commit the copy */
    if ((type = H5Tcopy (H5T_NATIVE_INT))<0) goto error;
    if (H5Tcommit (file, "native-int", type)<0) goto error;
    if ((status=H5Tcommitted (type))<0) goto error;
    if (0==status) {
	FAILED();
	puts ("    H5Tcommitted() returned false!");
	goto error;
    }

    /* We should not be able to modify a type after it has been committed. */
    H5E_BEGIN_TRY {
	status = H5Tset_precision (type, 256);
    } H5E_END_TRY;
    if (status>=0) {
	FAILED();
	puts ("    Committed type is not constant!");
	goto error;
    }

    /* We should not be able to re-commit a committed type */
    H5E_BEGIN_TRY {
	status = H5Tcommit(file, "test_named_2 (should not exist)", type);
    } H5E_END_TRY;
    if (status>=0) {
	FAILED();
	puts ("    Committed types should not be recommitted!");
	goto error;
    }

    /* It should be possible to define an attribute for the named type */
    if ((attr1=H5Acreate (type, "attr1", H5T_NATIVE_UCHAR, space,
			  H5P_DEFAULT))<0) goto error;
    for (i=0; i<ds_size[0]*ds_size[1]; i++) attr_data[0][i] = i;/*tricky*/
    if (H5Awrite(attr1, H5T_NATIVE_UINT, attr_data)<0) goto error;
    if (H5Aclose (attr1)<0) goto error;

    /*
     * Copying a committed type should result in a transient type which is
     * not locked.
     */
    if ((t2 = H5Tcopy (type))<0) goto error;
    if ((status=H5Tcommitted (t2))<0) goto error;
    if (status) {
	FAILED();
	puts ("    Copying a named type should result in a transient type!");
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
	FAILED();
	puts ("    Opened named types should be named types!");
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
	FAILED();
	puts ("    Dataset type should be a named type!");
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
	FAILED();
	puts ("    Dataset type should be a named type!");
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
	FAILED();
	puts ("    Dataset type should be a named type!");
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
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Tclose (t2);
	H5Tclose (type);
	H5Sclose (space);
	H5Dclose (dset);
	H5Fclose (file);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	mkstr
 *
 * Purpose:	Create a new string data type
 *
 * Return:	Success:	New type
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Monday, August 10, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hid_t
mkstr(size_t len, H5T_str_t strpad)
{
    hid_t	t;

    if ((t=H5Tcopy(H5T_C_S1))<0) return -1;
    if (H5Tset_size(t, len)<0) return -1;
    if (H5Tset_strpad(t, strpad)<0) return -1;
    return t;
}


/*-------------------------------------------------------------------------
 * Function:	test_conv_str_1
 *
 * Purpose:	Test string conversions
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Monday, August 10, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_str_1(void)
{
    char	*buf=NULL;
    hid_t	src_type, dst_type;

    TESTING("string conversions");

    /*
     * Convert a null-terminated string to a shorter and longer null
     * terminated string.
     */
    src_type = mkstr(10, H5T_STR_NULLTERM);
    dst_type = mkstr(5, H5T_STR_NULLTERM);
    buf = calloc(2, 10);
    memcpy(buf, "abcdefghi\0abcdefghi\0", 20);
    if (H5Tconvert(src_type, dst_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcd\0abcd\0abcdefghi\0", 20)) {
	FAILED();
	puts("    Truncated C-string test failed");
	goto error;
    }
    if (H5Tconvert(dst_type, src_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcd\0\0\0\0\0\0abcd\0\0\0\0\0\0", 20)) {
	FAILED();
	puts("    Extended C-string test failed");
	goto error;
    }
    free(buf);
    if (H5Tclose(src_type)<0) goto error;
    if (H5Tclose(dst_type)<0) goto error;

    /*
     * Convert a null padded string to a shorter and then longer string.
     */
    src_type = mkstr(10, H5T_STR_NULLPAD);
    dst_type = mkstr(5, H5T_STR_NULLPAD);
    buf = calloc(2, 10);
    memcpy(buf, "abcdefghijabcdefghij", 20);
    if (H5Tconvert(src_type, dst_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcdeabcdeabcdefghij", 20)) {
	FAILED();
	puts("    Truncated C buffer test failed");
	goto error;
    }
    if (H5Tconvert(dst_type, src_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcde\0\0\0\0\0abcde\0\0\0\0\0", 20)) {
	FAILED();
	puts("    Extended C buffer test failed");
	goto error;
    }
    free(buf);
    if (H5Tclose(src_type)<0) goto error;
    if (H5Tclose(dst_type)<0) goto error;

    /*
     * Convert a space-padded string to a shorter and then longer string.
     */
    src_type = mkstr(10, H5T_STR_SPACEPAD);
    dst_type = mkstr(5, H5T_STR_SPACEPAD);
    buf = calloc(2, 10);
    memcpy(buf, "abcdefghijabcdefghij", 20);
    if (H5Tconvert(src_type, dst_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcdeabcdeabcdefghij", 20)) {
	FAILED();
	puts("    Truncated Fortran-string test failed");
	goto error;
    }
    if (H5Tconvert(dst_type, src_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcde     abcde     ", 20)) {
	FAILED();
	puts("    Extended Fortran-string test failed");
	goto error;
    }
    free(buf);
    if (H5Tclose(src_type)<0) goto error;
    if (H5Tclose(dst_type)<0) goto error;

    /*
     * What happens if a null-terminated string is not null terminated?  If
     * the conversion is to an identical string then nothing happens but if
     * the destination is a different size or type of string then the right
     * thing should happen.
     */
    src_type = mkstr(10, H5T_STR_NULLTERM);
    dst_type = mkstr(10, H5T_STR_NULLTERM);
    buf = calloc(2, 10);
    memcpy(buf, "abcdefghijabcdefghij", 20);
    if (H5Tconvert(src_type, dst_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcdefghijabcdefghij", 20)) {
	FAILED();
	puts("    Non-terminated string test 1");
	goto error;
    }
    H5Tclose(dst_type);
    dst_type = mkstr(5, H5T_STR_NULLTERM);
    memcpy(buf, "abcdefghijabcdefghij", 20);
    if (H5Tconvert(src_type, dst_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcd\0abcd\0abcdefghij", 20)) {
	FAILED();
	puts("    Non-terminated string test 2");
	goto error;
    }
    memcpy(buf, "abcdeabcdexxxxxxxxxx", 20);
    if (H5Tconvert(dst_type, src_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcde\0\0\0\0\0abcde\0\0\0\0\0", 20)) {
	FAILED();
	puts("    Non-terminated string test 2");
	goto error;
    }
    free(buf);
    if (H5Tclose(src_type)<0) goto error;
    if (H5Tclose(dst_type)<0) goto error;
    
    /*
     * Test C string to Fortran and vice versa.
     */
    src_type = mkstr(10, H5T_STR_NULLTERM);
    dst_type = mkstr(10, H5T_STR_SPACEPAD);
    buf = calloc(2, 10);
    memcpy(buf, "abcdefghi\0abcdefghi\0", 20);
    if (H5Tconvert(src_type, dst_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcdefghi abcdefghi ", 20)) {
	FAILED();
	puts("    C string to Fortran test 1");
	goto error;
    }
    if (H5Tconvert(dst_type, src_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcdefghi\0abcdefghi\0", 20)) {
	FAILED();
	puts("    Fortran to C string test 1");
	goto error;
    }
    if (H5Tclose(dst_type)<0) goto error;
    dst_type = mkstr(5, H5T_STR_SPACEPAD);
    memcpy(buf, "abcdefgh\0\0abcdefgh\0\0", 20);
    if (H5Tconvert(src_type, dst_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcdeabcdeabcdefgh\0\0", 20)) {
	FAILED();
	puts("    C string to Fortran test 2");
	goto error;
    }
    if (H5Tconvert(dst_type, src_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcde\0\0\0\0\0abcde\0\0\0\0\0", 20)) {
	FAILED();
	puts("    Fortran to C string test 2");
	goto error;
    }
    if (H5Tclose(src_type)<0) goto error;
    if (H5Tclose(dst_type)<0) goto error;
    src_type = mkstr(5, H5T_STR_NULLTERM);
    dst_type = mkstr(10, H5T_STR_SPACEPAD);
    memcpy(buf, "abcd\0abcd\0xxxxxxxxxx", 20);
    if (H5Tconvert(src_type, dst_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcd      abcd      ", 20)) {
	FAILED();
	puts("    C string to Fortran test 3");
	goto error;
    }
    if (H5Tconvert(dst_type, src_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcd\0abcd\0abcd      ", 20)) {
	FAILED();
	puts("    Fortran to C string test 3");
	goto error;
    }
    free(buf);
    if (H5Tclose(src_type)<0) goto error;
    if (H5Tclose(dst_type)<0) goto error;
    
    /*
     * Test C buffer to Fortran and vice versa.
     */
    src_type = mkstr(10, H5T_STR_NULLPAD);
    dst_type = mkstr(10, H5T_STR_SPACEPAD);
    buf = calloc(2, 10);
    memcpy(buf, "abcdefghijabcdefghij", 20);
    if (H5Tconvert(src_type, dst_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcdefghijabcdefghij", 20)) {
	FAILED();
	puts("    C buffer to Fortran test 1");
	goto error;
    }
    if (H5Tconvert(dst_type, src_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcdefghijabcdefghij", 20)) {
	FAILED();
	puts("    Fortran to C buffer test 1");
	goto error;
    }
    if (H5Tclose(dst_type)<0) goto error;
    dst_type = mkstr(5, H5T_STR_SPACEPAD);
    memcpy(buf, "abcdefgh\0\0abcdefgh\0\0", 20);
    if (H5Tconvert(src_type, dst_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcdeabcdeabcdefgh\0\0", 20)) {
	FAILED();
	puts("    C buffer to Fortran test 2");
	goto error;
    }
    if (H5Tconvert(dst_type, src_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcde\0\0\0\0\0abcde\0\0\0\0\0", 20)) {
	FAILED();
	puts("    Fortran to C buffer test 2");
	goto error;
    }
    if (H5Tclose(src_type)<0) goto error;
    if (H5Tclose(dst_type)<0) goto error;
    src_type = mkstr(5, H5T_STR_NULLPAD);
    dst_type = mkstr(10, H5T_STR_SPACEPAD);
    memcpy(buf, "abcd\0abcd\0xxxxxxxxxx", 20);
    if (H5Tconvert(src_type, dst_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcd      abcd      ", 20)) {
	FAILED();
	puts("    C buffer to Fortran test 3");
	goto error;
    }
    if (H5Tconvert(dst_type, src_type, 2, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (memcmp(buf, "abcd\0abcd\0abcd      ", 20)) {
	FAILED();
	puts("    Fortran to C buffer test 3");
	goto error;
    }
    free(buf);
    if (H5Tclose(src_type)<0) goto error;
    if (H5Tclose(dst_type)<0) goto error;

    PASSED();
    reset_hdf5();
    return 0;

 error:
    reset_hdf5();
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_conv_str_2
 *
 * Purpose:	Tests C-to-Fortran and Fortran-to-C string conversion speed.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Monday, August 10, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_str_2(void)
{
    char		*buf=NULL, s[80];
    hid_t		c_type, f_type;
    const size_t	nelmts = 200000, ntests=NTESTS;
    size_t		i, j, nchars;
    int			ret_value = 1;

    /*
     * Initialize types and buffer.
     */
    c_type = mkstr(8, H5T_STR_NULLPAD);
    f_type = mkstr(8, H5T_STR_SPACEPAD);
    buf = calloc(nelmts, 8);
    for (i=0; i<nelmts; i++) {
	nchars = rand() % 8;
	for (j=0; j<nchars; j++) {
	    buf[i*8+j] = 'a' + rand()%26;
	}
	while (j<nchars) buf[i*8+j++] = '\0';
    }

    /* Do the conversions */
    for (i=0; i<ntests; i++) {
	if (ntests>1) {
	    sprintf(s, "Testing random string conversion speed (test %d/%d)",
		    (int)(i+1), (int)ntests);
	} else {
	    sprintf(s, "Testing random string conversion speed");
	}
	printf("%-70s", s);
	fflush(stdout);
	if (H5Tconvert(c_type, f_type, nelmts, buf, NULL, H5P_DEFAULT)<0) goto error;
	if (H5Tconvert(f_type, c_type, nelmts, buf, NULL, H5P_DEFAULT)<0) goto error;
	PASSED();
    }
    ret_value = 0;

 error:
    if (buf) free(buf);
    reset_hdf5();
    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function:	test_conv_enum_1
 *
 * Purpose:	Test conversion speed for enum data types
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, January  5, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_enum_1(void)
{
    const int	nelmts=200000, ntests=NTESTS;
    int		i, val, *buf=NULL;
    hid_t	t1, t2;
    char	s[80];
    int		ret_value = 1;

    /* Build the data types */
    t1 = H5Tcreate(H5T_ENUM, sizeof(int));
    t2 = H5Tenum_create(H5T_NATIVE_INT);
    s[1] = '\0';
    for (i=0; i<26; i++) {
	s[0] = 'A'+i;
	H5Tenum_insert(t1, s, &i);
	H5Tenum_insert(t2, s, (val=i*1000+i, &val));
    }

    /* Initialize the buffer */
    buf = malloc(nelmts*MAX(H5Tget_size(t1), H5Tget_size(t2)));
    for (i=0; i<nelmts; i++) buf[i] = rand() % 26;

    /* Conversions */
    for (i=0; i<ntests; i++) {
	if (ntests>1) {
	    sprintf(s, "Testing random enum conversion O(N) (test %d/%d)",
		    i+1, ntests);
	} else {
	    sprintf(s, "Testing random enum conversion O(N)");
	}
	printf("%-70s", s);
	fflush(stdout);
	if (H5Tconvert(t1, t2, nelmts, buf, NULL, H5P_DEFAULT)<0) goto error;
	PASSED();
    }

    for (i=0; i<ntests; i++) {
	if (ntests>1) {
	    sprintf(s, "Testing random enum conversion O(N log N) "
		    "(test %d/%d)", i+1, ntests);
	} else {
	    sprintf(s, "Testing random enum conversion O(N log N)");
	}
	printf("%-70s", s);
	fflush(stdout);
	if (H5Tconvert(t2, t1, nelmts, buf, NULL, H5P_DEFAULT)<0) goto error;
	PASSED();
    }
    ret_value = 0;

 error:
    H5Tclose(t1);
    H5Tclose(t2);
    if (buf) free(buf);
    reset_hdf5();
    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function:	test_conv_bitfield
 *
 * Purpose:	Test bitfield conversions.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 20, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_bitfield(void)
{
    unsigned char	buf[4];
    hid_t		st=-1, dt=-1;

    TESTING("bitfield conversions");

    /*
     * First test a simple bitfield conversion:
     *                   1010101010101010
     *   ________________1010101010101010
     */
    st = H5Tcopy(H5T_STD_B16LE);
    dt = H5Tcopy(H5T_STD_B32LE);
    buf[0] = buf[1] = 0xAA;
    buf[2] = buf[3] = 0x55; /*irrelevant*/
    if (H5Tconvert(st, dt, 1, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (buf[0]!=0xAA || buf[1]!=0xAA || buf[2]!=0 || buf[3]!=0) {
	FAILED();
	printf("    s=0xaaaa, d=0x%02x%02x%02x%02x (test 1)\n",
	       buf[3], buf[2], buf[1], buf[0]);
	goto error;
    }

    /*
     * Test2: Offset a 12-byte value in the middle of a 16 and 32 byte
     * field.
     *              __10 1010 1010 10__
     *    ____ ____ __10 1010 1010 10__ ____ ____
     */
    H5Tset_precision(st, 12);
    H5Tset_offset(st, 2);
    H5Tset_precision(dt, 12);
    H5Tset_offset(dt, 10);
    buf[0] = 0xA8; buf[1] = 0x2A; buf[2] = buf[3] = 0;
    if (H5Tconvert(st, dt, 1, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (buf[0]!=0 || buf[1]!=0xA8 || buf[2]!=0x2A || buf[3]!=0) {
	FAILED();
	printf("    s=0x2AA8 d=0x%02x%02x%02x%02x (test 2)\n",
	       buf[3], buf[2], buf[1], buf[0]);
	goto error;
    }

    /*
     * Same as previous test except unused bits of the destination will
     * be filled with ones.
     */
    H5Tset_pad(dt, H5T_PAD_ONE, H5T_PAD_ONE);
    buf[0] = 0xA8; buf[1] = 0x2A; buf[2] = buf[3] = 0;
    if (H5Tconvert(st, dt, 1, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (buf[0]!=0xff || buf[1]!=0xAB || buf[2]!=0xEA || buf[3]!=0xff) {
	FAILED();
	printf("    s=0x2AA8 d=0x%02x%02x%02x%02x (test 3)\n",
	       buf[3], buf[2], buf[1], buf[0]);
	goto error;
    }

    H5Tclose(st);
    H5Tclose(dt);
    PASSED();
    reset_hdf5();
    return 0;

 error:
    H5Tclose(st);
    H5Tclose(dt);
    reset_hdf5();
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	convert_opaque
 *
 * Purpose:	A fake opaque conversion functions
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Friday, June  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
convert_opaque(hid_t UNUSED st, hid_t UNUSED dt, H5T_cdata_t *cdata,
	       size_t UNUSED nelmts, size_t UNUSED stride, void UNUSED *_buf,
	       void UNUSED *bkg, hid_t dset_xfer_plid)
{
    if (H5T_CONV_CONV==cdata->command) num_opaque_conversions_g++;
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	test_opaque
 *
 * Purpose:	Test opaque datatypes
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 20, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_opaque(void)
{
#define OPAQUE_NELMTS 1000
    hid_t	st=-1, dt=-1;
    herr_t	status;
    char	buf[1]; /*not really used*/
    int		saved = num_opaque_conversions_g;

    TESTING("opaque datatypes");

    /* Build source and destination types */
    if ((st=H5Tcreate(H5T_OPAQUE, 4))<0) goto error;
    if (H5Tset_tag(st, "opaque source type")<0) goto error;
    if ((dt=H5Tcreate(H5T_OPAQUE, 4))<0) goto error;
    if (H5Tset_tag(dt, "opaque destination type")<0) goto error;

    /* Make sure that we can't convert between the types yet */
    H5E_BEGIN_TRY {
	status = H5Tconvert(st, dt, OPAQUE_NELMTS, buf, NULL, H5P_DEFAULT);
    } H5E_END_TRY;
    if (status>=0) {
	FAILED();
	printf("    opaque conversion should have failed but succeeded\n");
	goto error;
    }

    /* Register a conversion function */
    if (H5Tregister(H5T_PERS_HARD, "o_test", st, dt, convert_opaque)<0)
	goto error;

    /* Try the conversion again, this time it should work */
    if (H5Tconvert(st, dt, OPAQUE_NELMTS, buf, NULL, H5P_DEFAULT)<0) goto error;
    if (saved+1 != num_opaque_conversions_g) {
	FAILED();
	printf("    unexpected number of opaque conversions\n");
	goto error;
    }
    
    H5Tclose(st);
    H5Tclose(dt);
    PASSED();
    return 0;

 error:
    if (st>0) H5Tclose(st);
    if (dt>0) H5Tclose(dt);
    FAILED();
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_conv_int
 *
 * Purpose:	Test atomic number conversions.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Wednesday, June 10, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int (void)
{
    unsigned char	byte[4];

    /*---------------------------------------------------------------------
     * Test some specific overflow/underflow cases.
     *--------------------------------------------------------------------- 
     */
    TESTING("integer overflow conversions");

    /* (unsigned)0x80000000 -> (unsigned)0xffff */
    byte[0] = byte[1] = byte[2] = 0;
    byte[3] = 0x80;
    if (H5Tconvert (H5T_STD_U32LE, H5T_STD_U16LE, 1, byte, NULL, H5P_DEFAULT)<0) {
	goto error;
    }
    if (byte[0]!=0xff || byte[1]!=0xff) {
	FAILED();
	printf("    src: 0x80000000 unsigned\n");
	printf("    dst: 0x%02x%02x     unsigned\n", byte[1], byte[0]);
	printf("    ans: 0xffff     unsigned\n");
	goto error;
    }

    /* (unsigned)0xffffffff -> (signed)0x7fff */
    byte[0] = byte[1] = byte[2] = byte[3] = 0xff;
    if (H5Tconvert (H5T_STD_U32LE, H5T_STD_I16LE, 1, byte, NULL, H5P_DEFAULT)<0) {
	goto error;
    }
    if (byte[0]!=0xff || byte[1]!=0x7f) {
	FAILED();
	printf("    src: 0xffffffff unsigned\n");
	printf("    dst: 0x%02x%02x     signed\n", byte[1], byte[0]);
	printf("    ans: 0x7fff     signed\n");
	goto error;
    }

    /* (signed)0xffffffff -> (unsigned)0x0000 */
    byte[0] = byte[1] = byte[2] = byte[3] = 0xff;
    if (H5Tconvert (H5T_STD_I32LE, H5T_STD_U16LE, 1, byte, NULL, H5P_DEFAULT)<0) {
	goto error;
    }
    if (byte[0]!=0x00 || byte[1]!=0x00) {
	FAILED();
	printf("    src: 0xffffffff signed\n");
	printf("    dst: 0x%02x%02x     unsigned\n", byte[1], byte[0]);
	printf("    ans: 0x0000     unsigned\n");
	goto error;
    }

    /* (signed)0x7fffffff -> (unsigned)0xffff */
    byte[0] = byte[1] = byte[2] = 0xff;
    byte[3] = 0x7f;
    if (H5Tconvert (H5T_STD_I32LE, H5T_STD_U16LE, 1, byte, NULL, H5P_DEFAULT)<0) {
	goto error;
    }
    if (byte[0]!=0xff || byte[1]!=0xff) {
	FAILED();
	printf("    src: 0x7fffffff signed\n");
	printf("    dst: 0x%02x%02x     unsigned\n", byte[1], byte[0]);
	printf("    ans: 0xffff     unsigned\n");
	goto error;
    }

    /* (signed)0x7fffffff -> (signed)0x7fff */
    byte[0] = byte[1] = byte[2] = 0xff;
    byte[3] = 0x7f;
    if (H5Tconvert (H5T_STD_I32LE, H5T_STD_I16LE, 1, byte, NULL, H5P_DEFAULT)<0) {
	goto error;
    }
    if (byte[0]!=0xff || byte[1]!=0x7f) {
	FAILED();
	printf("    src: 0x7fffffff signed\n");
	printf("    dst: 0x%02x%02x     signed\n", byte[1], byte[0]);
	printf("    ans: 0x7fff     signed\n");
	goto error;
    }

    /* (signed)0xbfffffff -> (signed)0x8000 */
    byte[0] = byte[1] = byte[2] = 0xff;
    byte[3] = 0xbf;
    if (H5Tconvert (H5T_STD_I32LE, H5T_STD_I16LE, 1, byte, NULL, H5P_DEFAULT)<0) {
	goto error;
    }
    if (byte[0]!=0x00 || byte[1]!=0x80) {
	FAILED();
	printf("    src: 0xbfffffff signed\n");
	printf("    dst: 0x%02x%02x     signed\n", byte[1], byte[0]);
	printf("    ans: 0x8000     signed\n");
	goto error;
    }

    PASSED();
    reset_hdf5();
    return 0;

 error:
    reset_hdf5();
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_conv_int_1
 *
 * Purpose:	Test conversion of random integer values from SRC to DST.
 *		These types should be any combination of:
 *
 * 			H5T_NATIVE_SCHAR	H5T_NATIVE_UCHAR
 *			H5T_NATIVE_SHORT	H5T_NATIVE_USHORT
 *			H5T_NATIVE_INT		H5T_NATIVE_UINT
 *			H5T_NATIVE_LONG		H5T_NATIVE_ULONG
 *			H5T_NATIVE_LLONG	H5T_NATIVE_ULLONG
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_1(const char *name, hid_t src, hid_t dst)
{
    const size_t	ntests=NTESTS;		/*number of tests	*/
    const size_t	nelmts=200000;		/*num values per test	*/
    const size_t	max_fails=8;		/*max number of failures*/
    size_t		fails_all_tests=0;	/*number of failures	*/
    size_t		fails_this_test;	/*fails for this test	*/
    char		str[256];		/*hello string		*/
    int_t		src_type, dst_type;	/*data types		*/
    const char		*src_type_name=NULL;	/*source type name	*/
    const char		*dst_type_name=NULL;	/*destination type name	*/
    int			endian;			/*machine endianess	*/
    size_t		src_size, dst_size;	/*type sizes		*/
    unsigned char	*buf=NULL;		/*buffer for conversion	*/
    unsigned char	*saved=NULL;		/*original values	*/
    size_t		i, j, k;		/*counters		*/
    unsigned char	*hw=NULL;		/*hardware conv result	*/
    unsigned char	src_bits[32];		/*src value in LE order	*/
    unsigned char	dst_bits[32];		/*dest value in LE order*/
    size_t		src_nbits;		/*source length in bits	*/
    size_t		dst_nbits;		/*dst length in bits	*/
    void		*aligned=NULL;		/*aligned temp buffer	*/
    signed char		hw_char;
    unsigned char	hw_uchar;
    short		hw_short;
    unsigned short	hw_ushort;
    int			hw_int;
    unsigned		hw_uint;
    long		hw_long;
    unsigned long	hw_ulong;
    long_long		hw_llong;
    unsigned long_long	hw_ullong;
    
    
    
    /* What are the names of the source and destination types */
    if (H5Tequal(src, H5T_NATIVE_SCHAR)) {
	src_type_name = "signed char";
	src_type = INT_CHAR;
    } else if (H5Tequal(src, H5T_NATIVE_UCHAR)) {
	src_type_name = "unsigned char";
	src_type = INT_UCHAR;
    } else if (H5Tequal(src, H5T_NATIVE_SHORT)) {
	src_type_name = "short";
	src_type = INT_SHORT;
    } else if (H5Tequal(src, H5T_NATIVE_USHORT)) {
	src_type_name = "unsigned short";
	src_type = INT_USHORT;
    } else if (H5Tequal(src, H5T_NATIVE_INT)) {
	src_type_name = "int";
	src_type = INT_INT;
    } else if (H5Tequal(src, H5T_NATIVE_UINT)) {
	src_type_name = "unsigned int";
	src_type = INT_UINT;
    } else if (H5Tequal(src, H5T_NATIVE_LONG)) {
	src_type_name = "long";
	src_type = INT_LONG;
    } else if (H5Tequal(src, H5T_NATIVE_ULONG)) {
	src_type_name = "unsigned long";
	src_type = INT_ULONG;
    } else if (H5Tequal(src, H5T_NATIVE_LLONG)) {
	src_type_name = "long long";
	src_type = INT_LLONG;
    } else if (H5Tequal(src, H5T_NATIVE_ULLONG)) {
	src_type_name = "unsigned long long";
	src_type = INT_ULLONG;
    } else {
	src_type_name = "UNKNOWN";
	src_type = INT_OTHER;
    }
    
    if (H5Tequal(dst, H5T_NATIVE_SCHAR)) {
	dst_type_name = "signed char";
	dst_type = INT_CHAR;
    } else if (H5Tequal(dst, H5T_NATIVE_UCHAR)) {
	dst_type_name = "unsigned char";
	dst_type = INT_UCHAR;
    } else if (H5Tequal(dst, H5T_NATIVE_SHORT)) {
	dst_type_name = "short";
	dst_type = INT_SHORT;
    } else if (H5Tequal(dst, H5T_NATIVE_USHORT)) {
	dst_type_name = "unsigned short";
	dst_type = INT_USHORT;
    } else if (H5Tequal(dst, H5T_NATIVE_INT)) {
	dst_type_name = "int";
	dst_type = INT_INT;
    } else if (H5Tequal(dst, H5T_NATIVE_UINT)) {
	dst_type_name = "unsigned int";
	dst_type = INT_UINT;
    } else if (H5Tequal(dst, H5T_NATIVE_LONG)) {
	dst_type_name = "long";
	dst_type = INT_LONG;
    } else if (H5Tequal(dst, H5T_NATIVE_ULONG)) {
	dst_type_name = "unsigned long";
	dst_type = INT_ULONG;
    } else if (H5Tequal(dst, H5T_NATIVE_LLONG)) {
	dst_type_name = "long long";
	dst_type = INT_LLONG;
    } else if (H5Tequal(dst, H5T_NATIVE_ULLONG)) {
	dst_type_name = "unsigned long long";
	dst_type = INT_ULLONG;
    } else {
	dst_type_name = "UNKNOWN";
	dst_type = INT_OTHER;
    }

    /* Sanity checks */
    if (INT_OTHER==src_type || INT_OTHER==dst_type) {
	sprintf(str, "Testing random %s %s -> %s conversions",
		name, src_type_name, dst_type_name);
	printf("%-70s", str);
	FAILED();
	puts("    Unknown data type.");
	goto error;
    }

    /* Allocate buffers */
    endian = H5Tget_order(H5T_NATIVE_INT);
    src_size = H5Tget_size(src);
    dst_size = H5Tget_size(dst);
    buf = aligned_malloc(nelmts*MAX(src_size, dst_size));
    saved = aligned_malloc(nelmts*MAX(src_size, dst_size));
    aligned = malloc(sizeof(long_long));
#ifdef SHOW_OVERFLOWS
    noverflows_g = 0;
#endif

    /* The tests */
    for (i=0; i<ntests; i++) {
	if (ntests>1) {
	    sprintf(str, "Testing random %s %s -> %s conversions (test %d/%d)",
		    name, src_type_name, dst_type_name, (int)i+1, (int)ntests);
	} else {
	    sprintf(str, "Testing random %s %s -> %s conversions",
		    name, src_type_name, dst_type_name);
	}
	printf("%-70s", str);
	fflush(stdout);
	fails_this_test=0;

	/*
	 * Initialize the source buffers to random bits.  The `buf' buffer
	 * will be used for the conversion while the `saved' buffer will be
	 * sed for the comparison later.
	 */
	for (j=0; j<nelmts*src_size; j++) buf[j] = saved[j] = rand();

	/* Perform the conversion */
	if (H5Tconvert(src, dst, nelmts, buf, NULL, H5P_DEFAULT)<0) goto error;

	/* Check the results from the library against hardware */
	for (j=0; j<nelmts; j++) {
	    if (INT_CHAR==dst_type) {
		hw = (unsigned char*)&hw_char;
		switch (src_type) {
		case INT_CHAR:
		    memcpy(aligned, saved+j*sizeof(char), sizeof(char));
		    hw_char = (char)(*((signed char*)aligned));
		    break;
		case INT_UCHAR:
		    memcpy(aligned, saved+j*sizeof(char), sizeof(char));
		    hw_char = (char)(*((unsigned char*)aligned));
		    break;
		case INT_SHORT:
		    memcpy(aligned, saved+j*sizeof(short), sizeof(short));
		    hw_char = (char)(*((short*)aligned));
		    break;
		case INT_USHORT:
		    memcpy(aligned, saved+j*sizeof(short),
			   sizeof(unsigned short));
		    hw_char = (char)(*((unsigned short*)aligned));
		    break;
		case INT_INT:
		    memcpy(aligned, saved+j*sizeof(int), sizeof(int));
		    hw_char = (char)(*((int*)aligned));
		    break;
		case INT_UINT:
		    memcpy(aligned, saved+j*sizeof(unsigned),
			   sizeof(unsigned));
		    hw_char = (char)(*((unsigned*)aligned));
		    break;
		case INT_LONG:
		    memcpy(aligned, saved+j*sizeof(long), sizeof(long));
		    hw_char = (char)(*((long*)aligned));
		    break;
		case INT_ULONG:
		    memcpy(aligned, saved+j*sizeof(long),
			   sizeof(unsigned long));
		    hw_char = (char)(*((unsigned long*)aligned));
		    break;
		case INT_LLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(long_long));
		    hw_char = (char)(*((long_long*)aligned));
		    break;
		case INT_ULLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(unsigned long_long));
		    hw_char = (char)(*((unsigned long_long*)aligned));
		    break;
		case INT_OTHER:
		    break;
		}
	    } else if (INT_UCHAR==dst_type) {
		hw = (unsigned char*)&hw_uchar;
		switch (src_type) {
		case INT_CHAR:
		    memcpy(aligned, saved+j*sizeof(char),
			   sizeof(signed char));
		    hw_uchar = (unsigned char)(*((signed char*)aligned));
		    break;
		case INT_UCHAR:
		    memcpy(aligned, saved+j*sizeof(char),
			   sizeof(unsigned char));
		    hw_uchar = (unsigned char)(*((unsigned char*)aligned));
		    break;
		case INT_SHORT:
		    memcpy(aligned, saved+j*sizeof(short), sizeof(short));
		    hw_uchar = (unsigned char)(*((short*)aligned));
		    break;
		case INT_USHORT:
		    memcpy(aligned, saved+j*sizeof(short),
			   sizeof(unsigned short));
		    hw_uchar = (unsigned char)(*((unsigned short*)aligned));
		    break;
		case INT_INT:
		    memcpy(aligned, saved+j*sizeof(int), sizeof(int));
		    hw_uchar = (unsigned char)(*((int*)aligned));
		    break;
		case INT_UINT:
		    memcpy(aligned, saved+j*sizeof(unsigned),
			   sizeof(unsigned));
		    hw_uchar = (unsigned char)(*((unsigned*)aligned));
		    break;
		case INT_LONG:
		    memcpy(aligned, saved+j*sizeof(long), sizeof(long));
		    hw_uchar = (unsigned char)(*((long*)aligned));
		    break;
		case INT_ULONG:
		    memcpy(aligned, saved+j*sizeof(long),
			   sizeof(unsigned long));
		    hw_uchar = (unsigned char)(*((unsigned long*)aligned));
		    break;
		case INT_LLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(long_long));
		    hw_uchar = (unsigned char)(*((long_long*)aligned));
		    break;
		case INT_ULLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(unsigned long_long));
		    hw_uchar = (unsigned char)(*((unsigned long_long*)
						 aligned));
		    break;
		case INT_OTHER:
		    break;
		}
	    } else if (INT_SHORT==dst_type) {
		hw = (unsigned char*)&hw_short;
		switch (src_type) {
		case INT_CHAR:
		    memcpy(aligned, saved+j*sizeof(char), sizeof(signed char));
		    hw_short = (short)(*((signed char*)aligned));
		    break;
		case INT_UCHAR:
		    memcpy(aligned, saved+j*sizeof(char),
			   sizeof(unsigned char));
		    hw_short = (short)(*((unsigned char*)aligned));
		    break;
		case INT_SHORT:
		    memcpy(aligned, saved+j*sizeof(short), sizeof(short));
		    hw_short = (short)(*((short*)aligned));
		    break;
		case INT_USHORT:
		    memcpy(aligned, saved+j*sizeof(short),
			   sizeof(unsigned short));
		    hw_short = (short)(*((unsigned short*)aligned));
		    break;
		case INT_INT:
		    memcpy(aligned, saved+j*sizeof(int), sizeof(int));
		    hw_short = (short)(*((int*)aligned));
		    break;
		case INT_UINT:
		    memcpy(aligned, saved+j*sizeof(unsigned),
			   sizeof(unsigned));
		    hw_short = (short)(*((unsigned*)aligned));
		    break;
		case INT_LONG:
		    memcpy(aligned, saved+j*sizeof(long), sizeof(long));
		    hw_short = (short)(*((long*)aligned));
		    break;
		case INT_ULONG:
		    memcpy(aligned, saved+j*sizeof(long),
			   sizeof(unsigned long));
		    hw_short = (short)(*((unsigned long*)aligned));
		    break;
		case INT_LLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(long_long));
		    hw_short = (short)(*((long_long*)aligned));
		    break;
		case INT_ULLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(unsigned long_long));
		    hw_short = (short)(*((unsigned long_long*)aligned));
		    break;
		case INT_OTHER:
		    break;
		}
	    } else if (INT_USHORT==dst_type) {
		hw = (unsigned char*)&hw_ushort;
		switch (src_type) {
		case INT_CHAR:
		    memcpy(aligned, saved+j*sizeof(char), sizeof(signed char));
		    hw_ushort = (unsigned short)(*((signed char*)aligned));
		    break;
		case INT_UCHAR:
		    memcpy(aligned, saved+j*sizeof(char),
			   sizeof(unsigned char));
		    hw_ushort = (unsigned short)(*((unsigned char*)aligned));
		    break;
		case INT_SHORT:
		    memcpy(aligned, saved+j*sizeof(short), sizeof(short));
		    hw_ushort = (unsigned short)(*((short*)aligned));
		    break;
		case INT_USHORT:
		    memcpy(aligned, saved+j*sizeof(short),
			   sizeof(unsigned short));
		    hw_ushort = (unsigned short)(*((unsigned short*)aligned));
		    break;
		case INT_INT:
		    memcpy(aligned, saved+j*sizeof(int), sizeof(int));
		    hw_ushort = (unsigned short)(*((int*)aligned));
		    break;
		case INT_UINT:
		    memcpy(aligned, saved+j*sizeof(unsigned),
			   sizeof(unsigned));
		    hw_ushort = (unsigned short)(*((unsigned*)aligned));
		    break;
		case INT_LONG:
		    memcpy(aligned, saved+j*sizeof(long), sizeof(long));
		    hw_ushort = (unsigned short)(*((long*)aligned));
		    break;
		case INT_ULONG:
		    memcpy(aligned, saved+j*sizeof(long),
			   sizeof(unsigned long));
		    hw_ushort = (unsigned short)(*((unsigned long*)aligned));
		    break;
		case INT_LLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(long_long));
		    hw_ushort = (unsigned short)(*((long_long*)aligned));
		    break;
		case INT_ULLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(unsigned long_long));
		    hw_ushort = (unsigned short)(*((unsigned long_long*)
						   aligned));
		    break;
		case INT_OTHER:
		    break;
		}
	    } else if (INT_INT==dst_type) {
		hw = (unsigned char*)&hw_int;
		switch (src_type) {
		case INT_CHAR:
		    memcpy(aligned, saved+j*sizeof(char), sizeof(signed char));
		    hw_int = (int)(*((signed char*)aligned));
		    break;
		case INT_UCHAR:
		    memcpy(aligned, saved+j*sizeof(char),
			   sizeof(unsigned char));
		    hw_int = (int)(*((unsigned char*)aligned));
		    break;
		case INT_SHORT:
		    memcpy(aligned, saved+j*sizeof(short), sizeof(short));
		    hw_int = (int)(*((short*)aligned));
		    break;
		case INT_USHORT:
		    memcpy(aligned, saved+j*sizeof(short),
			   sizeof(unsigned short));
		    hw_int = (int)(*((unsigned short*)aligned));
		    break;
		case INT_INT:
		    memcpy(aligned, saved+j*sizeof(int), sizeof(int));
		    hw_int = (int)(*((int*)aligned));
		    break;
		case INT_UINT:
		    memcpy(aligned, saved+j*sizeof(unsigned),
			   sizeof(unsigned));
		    hw_int = (int)(*((unsigned*)aligned));
		    break;
		case INT_LONG:
		    memcpy(aligned, saved+j*sizeof(long), sizeof(long));
		    hw_int = (int)(*((long*)aligned));
		    break;
		case INT_ULONG:
		    memcpy(aligned, saved+j*sizeof(long),
			   sizeof(unsigned long));
		    hw_int = (int)(*((unsigned long*)aligned));
		    break;
		case INT_LLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(long_long));
		    hw_int = (int)(*((long_long*)aligned));
		    break;
		case INT_ULLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(unsigned long_long));
		    hw_int = (int)(*((unsigned long_long*)aligned));
		    break;
		case INT_OTHER:
		    break;
		}
	    } else if (INT_UINT==dst_type) {
		hw = (unsigned char*)&hw_uint;
		switch (src_type) {
		case INT_CHAR:
		    memcpy(aligned, saved+j*sizeof(char),
			   sizeof(signed char));
		    hw_uint = (unsigned int)(*((signed char*)aligned));
		    break;
		case INT_UCHAR:
		    memcpy(aligned, saved+j*sizeof(char),
			   sizeof(unsigned char));
		    hw_uint = (unsigned int)(*((unsigned char*)aligned));
		    break;
		case INT_SHORT:
		    memcpy(aligned, saved+j*sizeof(short), sizeof(short));
		    hw_uint = (unsigned int)(*((short*)aligned));
		    break;
		case INT_USHORT:
		    memcpy(aligned, saved+j*sizeof(short),
			   sizeof(unsigned short));
		    hw_uint = (unsigned int)(*((unsigned short*)aligned));
		    break;
		case INT_INT:
		    memcpy(aligned, saved+j*sizeof(int), sizeof(int));
		    hw_uint = (unsigned int)(*((int*)aligned));
		    break;
		case INT_UINT:
		    memcpy(aligned, saved+j*sizeof(unsigned),
			   sizeof(unsigned));
		    hw_uint = (unsigned int)(*((unsigned*)aligned));
		    break;
		case INT_LONG:
		    memcpy(aligned, saved+j*sizeof(long), sizeof(long));
		    hw_uint = (unsigned int)(*((long*)aligned));
		    break;
		case INT_ULONG:
		    memcpy(aligned, saved+j*sizeof(long),
			   sizeof(unsigned long));
		    hw_uint = (unsigned int)(*((unsigned long*)aligned));
		    break;
		case INT_LLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(long_long));
		    hw_uint = (unsigned int)(*((long_long*)aligned));
		    break;
		case INT_ULLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(unsigned long_long));
		    hw_uint = (unsigned int)(*((unsigned long_long*)aligned));
		    break;
		case INT_OTHER:
		    break;
		}
	    } else if (INT_LONG==dst_type) {
		hw = (unsigned char*)&hw_long;
		switch (src_type) {
		case INT_CHAR:
		    memcpy(aligned, saved+j*sizeof(char),
			   sizeof(signed char));
		    hw_long = (long int)(*((signed char*)aligned));
		    break;
		case INT_UCHAR:
		    memcpy(aligned, saved+j*sizeof(char),
			   sizeof(unsigned char));
		    hw_long = (long int)(*((unsigned char*)aligned));
		    break;
		case INT_SHORT:
		    memcpy(aligned, saved+j*sizeof(short), sizeof(short));
		    hw_long = (long int)(*((short*)aligned));
		    break;
		case INT_USHORT:
		    memcpy(aligned, saved+j*sizeof(short),
			   sizeof(unsigned short));
		    hw_long = (long int)(*((unsigned short*)aligned));
		    break;
		case INT_INT:
		    memcpy(aligned, saved+j*sizeof(int), sizeof(int));
		    hw_long = (long int)(*((int*)aligned));
		    break;
		case INT_UINT:
		    memcpy(aligned, saved+j*sizeof(unsigned),
			   sizeof(unsigned));
		    hw_long = (long int)(*((unsigned*)aligned));
		    break;
		case INT_LONG:
		    memcpy(aligned, saved+j*sizeof(long), sizeof(long));
		    hw_long = (long int)(*((long*)aligned));
		    break;
		case INT_ULONG:
		    memcpy(aligned, saved+j*sizeof(long),
			   sizeof(unsigned long));
		    hw_long = (long int)(*((unsigned long*)aligned));
		    break;
		case INT_LLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(long_long));
		    hw_long = (long int)(*((long_long*)aligned));
		    break;
		case INT_ULLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(unsigned long_long));
		    hw_long = (long int)(*((unsigned long_long*)aligned));
		    break;
		case INT_OTHER:
		    break;
		}
	    } else if (INT_ULONG==dst_type) {
		hw = (unsigned char*)&hw_ulong;
		switch (src_type) {
		case INT_CHAR:
		    memcpy(aligned, saved+j*sizeof(char),
			   sizeof(signed char));
		    hw_ulong = (unsigned long)(*((signed char*)aligned));
		    break;
		case INT_UCHAR:
		    memcpy(aligned, saved+j*sizeof(char),
			   sizeof(unsigned char));
		    hw_ulong = (unsigned long)(*((unsigned char*)aligned));
		    break;
		case INT_SHORT:
		    memcpy(aligned, saved+j*sizeof(short), sizeof(short));
		    hw_ulong = (unsigned long)(*((short*)aligned));
		    break;
		case INT_USHORT:
		    memcpy(aligned, saved+j*sizeof(short),
			   sizeof(unsigned short));
		    hw_ulong = (unsigned long)(*((unsigned short*)aligned));
		    break;
		case INT_INT:
		    memcpy(aligned, saved+j*sizeof(int), sizeof(int));
		    hw_ulong = (unsigned long)(*((int*)aligned));
		    break;
		case INT_UINT:
		    memcpy(aligned, saved+j*sizeof(unsigned),
			   sizeof(unsigned));
		    hw_ulong = (unsigned long)(*((unsigned*)aligned));
		    break;
		case INT_LONG:
		    memcpy(aligned, saved+j*sizeof(long), sizeof(long));
		    hw_ulong = (unsigned long)(*((long*)aligned));
		    break;
		case INT_ULONG:
		    memcpy(aligned, saved+j*sizeof(long),
			   sizeof(unsigned long));
		    hw_ulong = (unsigned long)(*((unsigned long*)aligned));
		    break;
		case INT_LLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(long_long));
		    hw_ulong = (unsigned long)(*((long_long*)aligned));
		    break;
		case INT_ULLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(unsigned long_long));
		    hw_ulong = (unsigned long)(*((unsigned long_long*)
						 aligned));
		    break;
		case INT_OTHER:
		    break;
		}
	    } else if (INT_LLONG==dst_type) {
		hw = (unsigned char*)&hw_llong;
		switch (src_type) {
		case INT_CHAR:
		    memcpy(aligned, saved+j*sizeof(char),
			   sizeof(signed char));
		    hw_llong = (long_long)(*((signed char*)aligned));
		    break;
		case INT_UCHAR:
		    memcpy(aligned, saved+j*sizeof(char),
			   sizeof(unsigned char));
		    hw_llong = (long_long)(*((unsigned char*)aligned));
		    break;
		case INT_SHORT:
		    memcpy(aligned, saved+j*sizeof(short), sizeof(short));
		    hw_llong = (long_long)(*((short*)aligned));
		    break;
		case INT_USHORT:
		    memcpy(aligned, saved+j*sizeof(short),
			   sizeof(unsigned short));
		    hw_llong = (long_long)(*((unsigned short*)aligned));
		    break;
		case INT_INT:
		    memcpy(aligned, saved+j*sizeof(int), sizeof(int));
		    hw_llong = (long_long)(*((int*)aligned));
		    break;
		case INT_UINT:
		    memcpy(aligned, saved+j*sizeof(unsigned),
			   sizeof(unsigned));
		    hw_llong = (long_long)(*((unsigned*)aligned));
		    break;
		case INT_LONG:
		    memcpy(aligned, saved+j*sizeof(long), sizeof(long));
		    hw_llong = (long_long)(*((long*)aligned));
		    break;
		case INT_ULONG:
		    memcpy(aligned, saved+j*sizeof(long),
			   sizeof(unsigned long));
		    hw_llong = (long_long)(*((unsigned long*)aligned));
		    break;
		case INT_LLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(long_long));
		    hw_llong = (long_long)(*((long_long*)aligned));
		    break;
		case INT_ULLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(unsigned long_long));
		    hw_llong = (long_long)(*((unsigned long_long*)aligned));
		    break;
		case INT_OTHER:
		    break;
		}
	    } else if (INT_ULLONG==dst_type) {
		hw = (unsigned char*)&hw_ullong;
		switch (src_type) {
		case INT_CHAR:
		    memcpy(aligned, saved+j*sizeof(char),
			   sizeof(signed char));
		    hw_ullong = (unsigned long_long)(*((signed char*)
						       aligned));
		    break;
		case INT_UCHAR:
		    memcpy(aligned, saved+j*sizeof(char),
			   sizeof(unsigned char));
		    hw_ullong = (unsigned long_long)(*((unsigned char*)
						       aligned));
		    break;
		case INT_SHORT:
		    memcpy(aligned, saved+j*sizeof(short), sizeof(short));
		    hw_ullong = (unsigned long_long)(*((short*)aligned));
		    break;
		case INT_USHORT:
		    memcpy(aligned, saved+j*sizeof(short),
			   sizeof(unsigned short));
		    hw_ullong = (unsigned long_long)(*((unsigned short*)
						       aligned));
		    break;
		case INT_INT:
		    memcpy(aligned, saved+j*sizeof(int), sizeof(int));
		    hw_ullong = (unsigned long_long)(*((int*)aligned));
		    break;
		case INT_UINT:
		    memcpy(aligned, saved+j*sizeof(unsigned),
			   sizeof(unsigned));
		    hw_ullong = (unsigned long_long)(*((unsigned*)aligned));
		    break;
		case INT_LONG:
		    memcpy(aligned, saved+j*sizeof(long), sizeof(long));
		    hw_ullong = (unsigned long_long)(*((long*)aligned));
		    break;
		case INT_ULONG:
		    memcpy(aligned, saved+j*sizeof(long),
			   sizeof(unsigned long));
		    hw_ullong = (unsigned long_long)(*((unsigned long*)
						       aligned));
		    break;
		case INT_LLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(long_long));
		    hw_ullong = (unsigned long_long)(*((long_long*)aligned));
		    break;
		case INT_ULLONG:
		    memcpy(aligned, saved+j*sizeof(long_long),
			   sizeof(unsigned long_long));
		    hw_ullong = (unsigned long_long)(*((unsigned long_long*)
						       aligned));
		    break;
		case INT_OTHER:
		    break;
		}
	    }

	    /* Are the two results the same */
	    for (k=0; k<dst_size; k++) {
		if (buf[j*dst_size+k]!=hw[k]) break;
	    }
	    if (k==dst_size) continue; /*no error*/

	    /*
	     * Convert the source and destination values to little endian
	     * order so we can use the HDF5 bit vector operations to test
	     * certain things.  These routines have already been tested by
	     * the `bittests' program.
	     */
	    src_nbits = 8*src_size;
	    for (k=0; k<src_size; k++) {
		src_bits[src_size-(k+1)] = saved[j*src_size+
						 ENDIAN(src_size, k)];
	    }
	    
	    dst_nbits = 8*dst_size;
	    for (k=0; k<dst_size; k++) {
		dst_bits[dst_size-(k+1)] = buf[j*dst_size+
					       ENDIAN(dst_size, k)];
	    }


	    /*
	     * Hardware usually doesn't handle overflows too gracefully. The
	     * hardware conversion result during overflows is usually garbage
	     * so we must handle those cases differetly when checking results.
	     */
	    if (H5T_SGN_2==H5Tget_sign(src) &&
		H5T_SGN_2==H5Tget_sign(dst)) {
		if (src_size>dst_size &&
		    0==H5T_bit_get_d(src_bits, src_nbits-1, 1) &&
		    H5T_bit_find(src_bits, dst_nbits-1, (src_nbits-dst_nbits),
				 H5T_BIT_MSB, 1)>=0) {
		    /*
		     * Source is positive and the magnitude is too large for
		     * the destination.  The destination should be set to the
		     * maximum possible value: 0x7f...f
		     */
		    if (0==H5T_bit_get_d(dst_bits, dst_nbits-1, 1) &&
			H5T_bit_find(dst_bits, 0, dst_nbits-1,
				     H5T_BIT_LSB, 0)<0) {
			continue; /*no error*/
		    }
		} else if (src_size>dst_size &&
			   1==H5T_bit_get_d(src_bits, src_nbits-1, 1) &&
			   H5T_bit_find(src_bits, 0, src_nbits-1, H5T_BIT_MSB,
					0)+1>=(ssize_t)dst_nbits) {
		    /*
		     * Source is negative but the magnitude is too large for
		     * the destination. The destination should be set to the
		     * smallest possible value: 0x80...0
		     */
		    if (1==H5T_bit_get_d(dst_bits, dst_nbits-1, 1) &&
			H5T_bit_find(dst_bits, 0, dst_nbits-1,
				     H5T_BIT_LSB, 1)<0) {
			continue; /*no error*/
		    }
		}
		
	    } else if (H5T_SGN_2==H5Tget_sign(src) &&
		       H5T_SGN_NONE==H5Tget_sign(dst)) {
		if (H5T_bit_get_d(src_bits, src_nbits-1, 1)) {
		    /*
		     * The source is negative so the result should be zero.
		     * The source is negative if the most significant bit is
		     * set.  The destination is zero if all bits are zero.
		     */
		    if (H5T_bit_find(dst_bits, 0, dst_nbits, H5T_BIT_LSB, 1)<0)
			continue; /*no error*/
		} else if (src_size>dst_size &&
			   H5T_bit_find(src_bits, dst_nbits-1,
					src_nbits-dst_nbits, H5T_BIT_LSB,
					1)>=0) {
		    /*
		     * The source is a value with a magnitude too large for
		     * the destination.  The destination should be the
		     * largest possible value: 0xff...f
		     */
		    if (H5T_bit_find(dst_bits, 0, dst_nbits, H5T_BIT_LSB,
				     0)<0) {
			continue; /*no error*/
		    }
		}
		
	    } else if (H5T_SGN_NONE==H5Tget_sign(src) &&
		       H5T_SGN_2==H5Tget_sign(dst)) {
		if (src_size>=dst_size &&
		    H5T_bit_find(src_bits, dst_nbits-1,
				 (src_nbits-dst_nbits)+1, H5T_BIT_LSB, 1)>=0) {
		    /*
		     * The source value has a magnitude that is larger than
		     * the destination can handle.  The destination should be
		     * set to the largest possible positive value: 0x7f...f
		     */
		    if (0==H5T_bit_get_d(dst_bits, dst_nbits-1, 1) &&
			H5T_bit_find(dst_bits, 0, dst_nbits-1, H5T_BIT_LSB,
				     0)<0) {
			continue; /*no error*/
		    }
		}
		
	    } else {
		if (src_size>dst_size &&
		    H5T_bit_find(src_bits, dst_nbits, src_nbits-dst_nbits,
				 H5T_BIT_LSB, 1)>=0) {
		    /*
		     * The unsigned source has a value which is too large for
		     * the unsigned destination.  The destination should be
		     * set to the largest possible value: 0xff...f
		     */
		    if (H5T_bit_find(dst_bits, 0, dst_nbits, H5T_BIT_LSB,
				     0)<0) {
			continue; /*no error*/
		    }
		}
	    }

	    /* Print errors */
	    if (0==fails_this_test++) FAILED();
	    printf("    test %u elmt %u\n", (unsigned)i+1, (unsigned)j);

	    printf("        src = ");
	    for (k=0; k<src_size; k++) {
		printf(" %02x", saved[j*src_size+ENDIAN(src_size, k)]);
	    }
	    printf("%*s", (int)(3*MAX(0, (ssize_t)dst_size-(ssize_t)src_size)),
		   "");
	    switch (src_type) {
	    case INT_CHAR:
		memcpy(aligned, saved+j*sizeof(char), sizeof(signed char));
		printf(" %29d\n", *((signed char*)aligned));
		break;
	    case INT_UCHAR:
		memcpy(aligned, saved+j*sizeof(char), sizeof(unsigned char));
		printf(" %29u\n", *((unsigned char*)aligned));
		break;
	    case INT_SHORT:
		memcpy(aligned, saved+j*sizeof(short), sizeof(short));
		printf(" %29d\n", *((short*)aligned));
		break;
	    case INT_USHORT:
		memcpy(aligned, saved+j*sizeof(short),
		       sizeof(unsigned short));
		printf(" %29u\n", *((unsigned short*)aligned));
		break;
	    case INT_INT:
		memcpy(aligned, saved+j*sizeof(int), sizeof(int));
		printf(" %29d\n", *((int*)aligned));
		break;
	    case INT_UINT:
		memcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
		printf(" %29u\n", *((unsigned*)aligned));
		break;
	    case INT_LONG:
		memcpy(aligned, saved+j*sizeof(long), sizeof(long));
		printf(" %29ld\n", *((long*)aligned));
		break;
	    case INT_ULONG:
		memcpy(aligned, saved+j*sizeof(long), sizeof(unsigned long));
		printf(" %29lu\n", *((unsigned long*)aligned));
		break;
	    case INT_LLONG:
		memcpy(aligned, saved+j*sizeof(long_long), sizeof(long_long));
		printf(" %29"PRINTF_LL_WIDTH"d\n", *((long_long*)aligned));
		break;
	    case INT_ULLONG:
		memcpy(aligned, saved+j*sizeof(long_long),
		       sizeof(unsigned long_long));
		printf(" %29"PRINTF_LL_WIDTH"u\n",
		       *((unsigned long_long*)aligned));
		break;
	    case INT_OTHER:
		break;
	    }
	    
	    printf("        dst = ");
	    for (k=0; k<dst_size; k++) {
		printf(" %02x", buf[j*dst_size+ENDIAN(dst_size, k)]);
	    }
	    printf("%*s", (int)(3*MAX(0, (ssize_t)src_size-(ssize_t)dst_size)),
		   "");
	    switch (dst_type) {
	    case INT_CHAR:
		memcpy(aligned, saved+j*sizeof(char), sizeof(signed char));
		printf(" %29d\n", *((signed char*)aligned));
		break;
	    case INT_UCHAR:
		memcpy(aligned, saved+j*sizeof(char), sizeof(unsigned char));
		printf(" %29u\n", *((unsigned char*)aligned));
		break;
	    case INT_SHORT:
		memcpy(aligned, saved+j*sizeof(short), sizeof(short));
		printf(" %29d\n", *((short*)aligned));
		break;
	    case INT_USHORT:
		memcpy(aligned, saved+j*sizeof(short), sizeof(unsigned short));
		printf(" %29u\n", *((unsigned short*)aligned));
		break;
	    case INT_INT:
		memcpy(aligned, saved+j*sizeof(int), sizeof(int));
		printf(" %29d\n", *((int*)aligned));
		break;
	    case INT_UINT:
		memcpy(aligned, saved+j*sizeof(unsigned), sizeof(unsigned));
		printf(" %29u\n", *((unsigned*)aligned));
		break;
	    case INT_LONG:
		memcpy(aligned, saved+j*sizeof(long), sizeof(long));
		printf(" %29ld\n", *((long*)aligned));
		break;
	    case INT_ULONG:
		memcpy(aligned, saved+j*sizeof(long), sizeof(unsigned long));
		printf(" %29lu\n", *((unsigned long*)aligned));
		break;
	    case INT_LLONG:
		memcpy(aligned, saved+j*sizeof(long_long), sizeof(long_long));
		printf(" %29"PRINTF_LL_WIDTH"d\n", *((long_long*)aligned));
		break;
	    case INT_ULLONG:
		memcpy(aligned, saved+j*sizeof(long_long),
		       sizeof(unsigned long_long));
		printf(" %29"PRINTF_LL_WIDTH"u\n",
		       *((unsigned long_long*)aligned));
		break;
	    case INT_OTHER:
		break;
	    }
	    
	    printf("        ans = ");
	    for (k=0; k<dst_size; k++) {
		printf(" %02x", hw[ENDIAN(dst_size, k)]);
	    }
	    printf("%*s", (int)(3*MAX(0, (ssize_t)src_size-(ssize_t)dst_size)),
		   "");
	    switch (dst_type) {
	    case INT_CHAR:
		printf(" %29d\n", *((signed char*)hw));
		break;
	    case INT_UCHAR:
		printf(" %29u\n", *((unsigned char*)hw));
		break;
	    case INT_SHORT:
		printf(" %29d\n", *((short*)hw));
		break;
	    case INT_USHORT:
		printf(" %29u\n", *((unsigned short*)hw));
		break;
	    case INT_INT:
		printf(" %29d\n", *((int*)hw));
		break;
	    case INT_UINT:
		printf(" %29u\n", *((unsigned*)hw));
		break;
	    case INT_LONG:
		printf(" %29ld\n", *((long*)hw));
		break;
	    case INT_ULONG:
		printf(" %29lu\n", *((unsigned long*)hw));
		break;
	    case INT_LLONG:
		printf(" %29"PRINTF_LL_WIDTH"d\n", *((long_long*)hw));
		break;
	    case INT_ULLONG:
		printf(" %29"PRINTF_LL_WIDTH"u\n", *((unsigned long_long*)hw));
		break;
	    case INT_OTHER:
		break;
	    }

	    if (++fails_all_tests>=max_fails) {
		puts("    maximum failures reached, aborting test...");
		goto done;
	    }
	}
	PASSED();
    }
#ifdef SHOW_OVERFLOWS
    if (noverflows_g>0) {
	printf("    %d overflow%s in previous test\n",
	       noverflows_g, 1==noverflows_g?"":"s");
    }
#endif

 done:
    if (buf) aligned_free(buf);
    if (saved) aligned_free(saved);
    if (aligned) free(aligned);
    fflush(stdout);
    reset_hdf5();	/*print statistics*/
    return (int)fails_all_tests;

 error:
    if (buf) aligned_free(buf);
    if (saved) aligned_free(saved);
    if (aligned) free(aligned);
    fflush(stdout);
    reset_hdf5();	/*print statistics*/
    return MAX((int)fails_all_tests, 1);
}


/*-------------------------------------------------------------------------
 * Function:	test_conv_int_2
 *
 * Purpose:	Tests overlap calculates in H5T_conv_i_i(), which should be
 *		the same as for H5T_conv_f_f() and H5T_conv_s_s().
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Friday, April 30, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_2(void)
{
    int		i, j;
    hid_t	src_type, dst_type;
    char	buf[32*100];

    printf("%-70s", "Testing overlap calculations");
    fflush(stdout);
    
    memset(buf, 0, sizeof buf);
    for (i=1; i<=32; i++) {
	for (j=1; j<=32; j++) {

	    /* Source type */
	    src_type = H5Tcopy(H5T_NATIVE_CHAR);
	    H5Tset_size(src_type, i);

	    /* Destination type */
	    dst_type = H5Tcopy(H5T_NATIVE_CHAR);
	    H5Tset_size(dst_type, j);

	    /*
	     * Conversion. If overlap calculations aren't right then an
	     * assertion will fail in H5T_conv_i_i()
	     */
	    H5Tconvert(src_type, dst_type, 100, buf, NULL, H5P_DEFAULT);
	    H5Tclose(src_type);
	    H5Tclose(dst_type);
	}
    }
    PASSED();
    return 0;
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

    if (FLT_FLOAT==type) {
	float x;
	memcpy(&x, val, sizeof(float));
	retval = (x!=x);
    } else if (FLT_DOUBLE==type) {
	double x;
	memcpy(&x, val, sizeof(double));
	retval = (x!=x);
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
    } else if (FLT_LDOUBLE==type) {
	long double x;
	memcpy(&x, val, sizeof(long double));
	retval = (x!=x);
#endif
    } else {
	return 0;
    }

    /*
     * Sometimes NaN==NaN (e.g., DEC Alpha) so we try to print it and see if
     * the result contains a NaN string.
     */
    if (!retval) {
	if (FLT_FLOAT==type) {
	    float x;
	    memcpy(&x, val, sizeof(float));
	    sprintf(s, "%g", x);
	} else if (FLT_DOUBLE==type) {
	    double x;
	    memcpy(&x, val, sizeof(double));
	    sprintf(s, "%g", x);
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
	} else if (FLT_LDOUBLE==type) {
	    long double x;
	    memcpy(&x, val, sizeof(long double));
	    sprintf(s, "%Lg", x);
#endif
	} else {
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
 *		Failure:	number of errors
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
    const size_t	ntests=NTESTS;		/*number of tests	*/
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
    void		*aligned=NULL;		/*aligned buffer	*/
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
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
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
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
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
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
	printf("%-70s", str);
	FAILED();
	puts("    Unknown data type.");
	goto error;
    }
    
    /* Allocate buffers */
    endian = H5Tget_order(H5T_NATIVE_FLOAT);
    src_size = H5Tget_size(src);
    dst_size = H5Tget_size(dst);
    buf   = aligned_malloc(nelmts*MAX(src_size, dst_size));
    saved = aligned_malloc(nelmts*MAX(src_size, dst_size));
    aligned = malloc(16); /*should be big enough for any type*/
#ifdef SHOW_OVERFLOWS
    noverflows_g = 0;
#endif

    for (i=0; i<ntests; i++) {

	/*
	 * If it looks like it might take a long time then print a progress
	 * report between each test.
	 */
	if (ntests>1) {
	    sprintf(str, "Testing random %s %s -> %s conversions (test %d/%d)",
		    name, src_type_name, dst_type_name, (int)i+1, (int)ntests);
	} else {
	    sprintf(str, "Testing random %s %s -> %s conversions",
		    name, src_type_name, dst_type_name);
	}
	printf("%-70s", str);
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
		/* Do it this way for alignment reasons */
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
		long double temp[1];
#else
		double temp[1];
#endif
		if (src_size<=dst_size) {
		    for (k=0; k<dst_size; k++) buf[j*src_size+k] = rand();
		} else {
		    for (k=0; k<dst_size; k++) {
			((unsigned char*)temp)[k] = rand();
		    }
		    if (FLT_DOUBLE==src_type && FLT_FLOAT==dst_type) {
			hw_d = *((float*)temp);
			memcpy(buf+j*src_size, &hw_d, src_size);
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
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
	if (H5Tconvert(src, dst, nelmts, buf, NULL, H5P_DEFAULT)<0) goto error;

	/* Check the software results against the hardware */
	for (j=0; j<nelmts; j++) {
	    hw_f = 911.0;
	    hw_d = 911.0;
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
	    hw_ld = 911.0;
#endif

	    /* The hardware conversion */
	    if (FLT_FLOAT==src_type) {
		memcpy(aligned, saved+j*sizeof(float), sizeof(float));
		if (FLT_FLOAT==dst_type) {
		    hw_f = *((float*)aligned);
		    hw = (unsigned char*)&hw_f;
		} else if (FLT_DOUBLE==dst_type) {
		    hw_d = *((float*)aligned);
		    hw = (unsigned char*)&hw_d;
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
		} else {
		    hw_ld = *((float*)aligned);
		    hw = (unsigned char*)&hw_ld;
#endif
		}
	    } else if (FLT_DOUBLE==src_type) {
		memcpy(aligned, saved+j*sizeof(double), sizeof(double));
		if (FLT_FLOAT==dst_type) {
		    hw_f = *((double*)aligned);
		    hw = (unsigned char*)&hw_f;
		} else if (FLT_DOUBLE==dst_type) {
		    hw_d = *((double*)aligned);
		    hw = (unsigned char*)&hw_d;
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
		} else {
		    hw_ld = *((double*)aligned);
		    hw = (unsigned char*)&hw_ld;
#endif
		}
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
	    } else {
		memcpy(aligned, saved+j*sizeof(long double),
		       sizeof(long double)); 
		if (FLT_FLOAT==dst_type) {
		    hw_f = *((long double*)aligned); 
		    hw = (unsigned char*)&hw_f;
		} else if (FLT_DOUBLE==dst_type) {
		    hw_d = *((long double*)aligned); 
		    hw = (unsigned char*)&hw_d;
		} else {
		    hw_ld = *((long double*)aligned);
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
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
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
		double		check_mant[2];
		int		check_expo[2];
		
		if (FLT_FLOAT==dst_type) {
		    float x;
		    memcpy(&x, (float*)buf+j, sizeof(float));
		    check_mant[0] = frexp(x, check_expo+0);
		    check_mant[1] = frexp(((float*)hw)[0], check_expo+1);
		} else if (FLT_DOUBLE==dst_type) {
		    double x;
		    memcpy(&x, (double*)buf+j, sizeof(double));
		    check_mant[0] = frexp(x, check_expo+0);
		    check_mant[1] = frexp(((double*)hw)[0], check_expo+1);
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
		} else {
		    long double x;
		    memcpy(&x, (long double*)buf+j, sizeof(long double));
		    check_mant[0] = frexp(x, check_expo+0);
		    check_mant[1] = frexp(((long double*)hw)[0], check_expo+1);
#endif
		}
		if (check_expo[0]==check_expo[1] &&
		    fabs(check_mant[0]-check_mant[1])<0.000001) {
		    continue;
		}
	    }
#endif

	    if (0==fails_this_test++) FAILED();
	    printf("    test %u, elmt %u\n", (unsigned)i+1, (unsigned)j);
	    
	    printf("        src =");
	    for (k=0; k<src_size; k++) {
		printf(" %02x", saved[j*src_size+ENDIAN(src_size,k)]);
	    }
	    printf("%*s", (int)(3*MAX(0, (ssize_t)dst_size-(ssize_t)src_size)),
		   "");
	    if (FLT_FLOAT==src_type) {
		float x;
		memcpy(&x, (float*)saved+j, sizeof(float));
		printf(" %29.20e\n", x);
	    } else if (FLT_DOUBLE==src_type) {
		double x;
		memcpy(&x, (double*)saved+j, sizeof(double));
		printf(" %29.20e\n", x);
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
	    } else {
		long double x;
		memcpy(&x, (long double*)saved+j, sizeof(long double));
		printf(" %29.20Le\n", x);
#endif
	    }

	    printf("        dst =");
	    for (k=0; k<dst_size; k++) {
		printf(" %02x", buf[j*dst_size+ENDIAN(dst_size,k)]);
	    }
	    printf("%*s", (int)(3*MAX(0, (ssize_t)src_size-(ssize_t)dst_size)),
		   "");
	    if (FLT_FLOAT==dst_type) {
		float x;
		memcpy(&x, (float*)buf+j, sizeof(float));
		printf(" %29.20e\n", x);
	    } else if (FLT_DOUBLE==dst_type) {
		double x;
		memcpy(&x, (double*)buf+j, sizeof(double));
		printf(" %29.20e\n", x);
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
	    } else {
		long double x;
		memcpy(&x, (long double*)buf+j, sizeof(long double));
		printf(" %29.20Le\n", x);
#endif
	    }

	    printf("        ans =");
	    for (k=0; k<dst_size; k++) {
		printf(" %02x", hw[ENDIAN(dst_size,k)]);
	    }
	    printf("%*s", (int)(3*MAX(0, (ssize_t)src_size-(ssize_t)dst_size)),
		   "");
	    if (FLT_FLOAT==dst_type) {
		printf(" %29.20e\n", hw_f);
	    } else if (FLT_DOUBLE==dst_type) {
		printf(" %29.20e\n", hw_d);
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
	    } else {
		printf(" %29.20Le\n", hw_ld);
#endif
	    }

	    if (++fails_all_tests>=max_fails) {
		puts("    maximum failures reached, aborting test...");
		goto done;
	    }
	}
	PASSED();
    }
#ifdef SHOW_OVERFLOWS
    if (noverflows_g>0) {
	printf("   %d overflow%s in previous test\n",
	       noverflows_g, 1==noverflows_g?"":"s");
    }
#endif

 done:
    if (buf) aligned_free(buf);
    if (saved) aligned_free(saved);
    if (aligned) free(aligned);
    fflush(stdout);
#ifdef HANDLE_SIGFPE
    exit(MIN((int)fails_all_tests, 254));
#else
    reset_hdf5();
    return (int)fails_all_tests;
#endif

 error:
    if (buf) aligned_free(buf);
    if (saved) aligned_free(saved);
    if (aligned) free(aligned);
    fflush(stdout);
#ifdef HANDLE_SIGFPE
    exit(MIN(MAX((int)fails_all_tests, 1), 254));
#else
    reset_hdf5();
    return MAX((int)fails_all_tests, 1);
#endif
}


/*-------------------------------------------------------------------------
 * Function:	run_integer_tests
 *
 * Purpose:	Runs all integer tests.
 *
 * Return:	Number of errors
 *
 * Programmer:	Robb Matzke
 *              Tuesday, November 24, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
run_integer_tests(const char *name)
{
    int		nerrors = 0;

    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_UINT);
#if SIZEOF_LONG!=SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_ULONG);
#endif
#if SIZEOF_LONG_LONG!=SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_ULLONG);
#endif

    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_UINT);
#if SIZEOF_LONG!=SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_ULONG);
#endif
#if SIZEOF_LONG_LONG!=SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_ULLONG);
#endif

    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_UINT);
#if SIZEOF_LONG!=SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_ULONG);
#endif
#if SIZEOF_LONG_LONG!=SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_ULLONG);
#endif

    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_UINT);
#if SIZEOF_LONG!=SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_ULONG);
#endif
#if SIZEOF_LONG_LONG!=SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_ULLONG);
#endif

    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_UINT);
#if SIZEOF_LONG!=SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_ULONG);
#endif
#if SIZEOF_LONG_LONG!=SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_ULLONG);
#endif

    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_INT);
#if SIZEOF_LONG!=SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_ULONG);
#endif
#if SIZEOF_LONG_LONG!=SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_ULLONG);
#endif

#if SIZEOF_LONG!=SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_UINT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_ULONG);
#if SIZEOF_LONG_LONG!=SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_ULLONG);
#endif
#endif

#if SIZEOF_LONG!=SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_UINT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_LONG);
#if SIZEOF_LONG_LONG!=SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_ULLONG);
#endif
#endif

#if SIZEOF_LONG_LONG!=SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_UINT);
#if SIZEOF_LONG!=SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_ULONG);
#endif
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_ULLONG);
#endif

#if SIZEOF_LONG_LONG!=SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_UINT);
#if SIZEOF_LONG!=SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_ULONG);
#endif
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_LLONG);
#endif

    return nerrors;
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
    hid_t		fapl=-1;

    reset_hdf5();
    fapl = h5_fileaccess();

    if (ALIGNMENT) {
	printf("Testing non-aligned conversions (ALIGNMENT=%d)....\n",
	       ALIGNMENT);
    }
    
    /* Do the tests */
    nerrors += test_classes();
    nerrors += test_copy();
    nerrors += test_compound_1();
    nerrors += test_transient (fapl);
    nerrors += test_named (fapl);
    h5_cleanup (fapl); /*must happen before first reset*/
    reset_hdf5();

    nerrors += test_conv_str_1();
    nerrors += test_conv_str_2();
    nerrors += test_compound_2();
    nerrors += test_compound_3();
    nerrors += test_compound_4();
    nerrors += test_conv_int ();
    nerrors += test_conv_enum_1();
    nerrors += test_conv_bitfield();
    nerrors += test_opaque();

    /* Does floating point overflow generate a SIGFPE? */
    generates_sigfpe();

    /* Test degenerate cases */
    nerrors += test_conv_flt_1("noop", H5T_NATIVE_FLOAT, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1("noop", H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE);

    /* Test hardware integer conversion functions */
    nerrors += run_integer_tests("hw");

    /* Test hardware floating-point conversion functions */
    nerrors += test_conv_flt_1("hw", H5T_NATIVE_FLOAT, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1("hw", H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT);

    /*----------------------------------------------------------------------
     * Software tests
     *---------------------------------------------------------------------- 
     */
    without_hardware_g = TRUE;
    reset_hdf5();

    /* Test software integer conversion functions */
    nerrors += test_conv_int_2();
    nerrors += run_integer_tests("sw");

    /* Test software floating-point conversion functions */
    nerrors += test_conv_flt_1("sw", H5T_NATIVE_FLOAT, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1("sw", H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT);
#if SIZEOF_LONG_DOUBLE!=SIZEOF_DOUBLE
    nerrors += test_conv_flt_1("sw", H5T_NATIVE_FLOAT, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1("sw", H5T_NATIVE_DOUBLE, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1("sw", H5T_NATIVE_LDOUBLE, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1("sw", H5T_NATIVE_LDOUBLE, H5T_NATIVE_DOUBLE);
#endif
    
    if (nerrors) {
        printf("***** %lu FAILURE%s! *****\n",
               nerrors, 1==nerrors?"":"S");
        exit(1);
    }
    printf("All data type tests passed.\n");
    return 0;
}
