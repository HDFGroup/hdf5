/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Monday, September 28, 1998
 *
 * Purpose:	Creates a chunked dataset and measures the storage overhead.
 */

/* See H5private.h for how to include headers */
#undef NDEBUG
#include <hdf5.h>

#ifdef STDC_HEADERS
#   include <ctype.h>
#   include <fcntl.h>
#   include <stdlib.h>
#   include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
#   include <sys/types.h>
#   include <unistd.h>
#endif

#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif

#define FILE_NAME_1	"overhead.h5"
#define FALSE		0
#define TRUE		1

typedef enum fill_t {
    FILL_ALL,
    FILL_FORWARD,
    FILL_REVERSE,
    FILL_INWARD,
    FILL_OUTWARD,
    FILL_RANDOM
} fill_t;


/*-------------------------------------------------------------------------
 * Function:	usage
 *
 * Purpose:	Prints a usage message and exits.
 *
 * Return:	never returns
 *
 * Programmer:	Robb Matzke
 *              Wednesday, September 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage(const char *prog)
{
    fprintf(stderr, "usage: %s [STYLE|cache] [LEFT [MIDDLE [RIGHT]]]\n",
	    prog);
    fprintf(stderr, "\
    STYLE is the order that the dataset is filled and should be one of:\n\
        forward   --  Fill the dataset from lowest address to highest\n\
                      address. This style tests the right split ratio.\n\
        reverse   --  Fill the dataset from highest address to lowest\n\
                      address.  This is the reverse order of `forward' and\n\
                      tests the left split ratio.\n\
        inward    --  Fill beginning at both the lowest and highest\n\
                      addresses and work in toward the center of the\n\
                      dataset.  This tests the middle split ratio.\n\
        outward   --  Start at the center of the dataset and work outward\n\
                      toward the lowest and highest addresses.  This tests\n\
                      both left and right split ratios.\n\
        random    --  Write the chunks of the dataset in random order.  This\n\
                      tests all split ratios.\n\
    If no fill style is specified then all fill styles are tried and a\n\
    single value is printed for each one.\n\
\n\
    If the word `cache' is used instead of a fill style then the raw data\n\
    cache is enabled.  It is not possible to enable the raw data cache when\n\
    a specific fill style is used because H5Fflush() is called after each\n\
    chunk is written in order to calculate overhead during the test.  If\n\
    the cache is enabled then chunks are written to disk in different orders\n\
    than the actual H5Dwrite() calls in the test due to collisions and the\n\
    resulting B-tree will be split differently.\n\
\n\
    LEFT, MIDDLE, and RIGHT are the ratios to use for splitting and should\n\
    be values between zero and one, inclusive.\n");
    exit(1);
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
 * Function:	test
 *
 * Purpose:	The guts of the test
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Wednesday, September 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test(fill_t fill_style, const double splits[],
     hbool_t verbose, hbool_t use_rdcc)
{
    hid_t	file, fapl, dcpl, xfer, mspace, fspace, dset;
    hsize_t	ch_size[1] = {1};		/*chunk size		*/
    hsize_t	cur_size[1] = {1000};		/*current dataset size	*/
    hsize_t	max_size[1] = {H5S_UNLIMITED};	/*maximum dataset size	*/
    hssize_t	hs_start[1];			/*hyperslab start offset*/
    hsize_t	hs_count[1] = {1};		/*hyperslab nelmts	*/
    int		fd;				/*h5 file direct	*/
    static int	*had = NULL;			/*for random filling	*/
    const char	*sname;				/*fill style nam	*/
    int		mdc_nelmts;			/*num meta objs to cache*/
    hsize_t	i;
    int		j;
    struct stat	sb;

    if (!had) had = calloc(cur_size[0], sizeof(int));
    if ((fapl=H5Pcreate(H5P_FILE_ACCESS))<0) goto error;
    if (!use_rdcc) {
	if (H5Pget_cache(fapl, &mdc_nelmts, NULL, NULL, NULL)<0) goto error;
	if (H5Pset_cache(fapl, mdc_nelmts, 0, 0, 0.0)<0) goto error;
    }
    if ((file=H5Fcreate(FILE_NAME_1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) {
	goto error;
    }
    if ((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if (H5Pset_chunk(dcpl, 1, ch_size)<0) goto error;
    if ((xfer=H5Pcreate(H5P_DATASET_XFER))<0) goto error;
    if (H5Pset_btree_ratios(xfer, splits[0], splits[1], splits[2])<0) {
	goto error;
    }
    if ((fspace=H5Screate_simple(1, cur_size, max_size))<0) goto error;
    if ((mspace=H5Screate_simple(1, ch_size, ch_size))<0) goto error;
    if ((dset=H5Dcreate(file, "chunked", H5T_NATIVE_INT,
			fspace, dcpl))<0) goto error;
    if ((fd=open(FILE_NAME_1, O_RDONLY))<0) goto error;

    for (i=1; i<=cur_size[0]; i++) {

	/* Decide which chunk to write to */
	switch (fill_style) {
	case FILL_FORWARD:
	    hs_start[0] = i-1;
	    break;
	case FILL_REVERSE:
	    hs_start[0] = cur_size[0]-i;
	    break;
	case FILL_INWARD:
	    hs_start[0] = i%2 ? i/2 : cur_size[0]-i/2;
	    break;
	case FILL_OUTWARD:
	    j = (cur_size[0]-i)+1;
	    hs_start[0] = j%2 ? j/2 : cur_size[0]-j/2;
	    break;
	case FILL_RANDOM:
	    for (j=rand()%cur_size[0]; had[j]; j=(j+1)%cur_size[0]) /*void*/;
	    hs_start[0] = j;
	    had[j] = 1;
	    break;
	case FILL_ALL:
	    abort();
	}

	/* Write the chunk */
	if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_start, NULL,
				hs_count, NULL)<0) goto error;
	if (H5Dwrite(dset, H5T_NATIVE_INT, mspace, fspace, xfer, &i)<0) {
	    goto error;
	}

	/* Determine overhead */
	if (verbose) {
	    if (H5Fflush(file, H5F_SCOPE_LOCAL)<0) goto error;
	    if (fstat(fd, &sb)<0) goto error;
	    /*
	     * The extra cast in the following statement is a bug workaround
	     * for the Win32 version 5.0 compiler.
	     * 1998-11-06 ptl
	     */
	    printf("%4lu %8.3f\n",
		   (unsigned long)i,
		   (double)((hssize_t)((sb.st_size-i*sizeof(int))/i)));
	}
    }

    H5Dclose(dset);
    H5Sclose(mspace);
    H5Sclose(fspace);
    H5Pclose(dcpl);
    H5Fclose(file);

    if (!verbose) {
	switch (fill_style) {
	case FILL_FORWARD:
	    sname = "forward";
	    break;
	case FILL_REVERSE:
	    sname = "reverse";
	    break;
	case FILL_INWARD:
	    sname = "inward";
	    break;
	case FILL_OUTWARD:
	    sname = "outward";
	    break;
	case FILL_RANDOM:
	    sname = "random";
	    break;
	case FILL_ALL:
	    abort();
	}
	if (fstat(fd, &sb)<0) goto error;
	    /*
	     * The extra cast in the following statement is a bug workaround
	     * for the Win32 version 5.0 compiler.
	     * 1998-11-06 ptl
	     */
	printf("%-7s %8.3f\n", sname, 
	       (double)((hssize_t)((sb.st_size-cur_size[0]*sizeof(int))/
				   cur_size[0])));
	
    }
    close(fd);
    return 0;

 error:
    H5Dclose(dset);
    H5Sclose(mspace);
    H5Sclose(fspace);
    H5Pclose(dcpl);
    H5Fclose(file);
    free(had);
    close(fd);
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	
 *
 * Return:	Success:        zero
 *
 *		Failure:	non-zero
 *
 * Programmer:	Robb Matzke
 *              Monday, September 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t	xfer;
    fill_t	fill_style = FILL_ALL;
    hbool_t	use_cache = FALSE;
    double	splits[3];
    int		i, j, nerrors=0;

    /* Default split ratios */
    H5Eset_auto(display_error_cb, NULL);
    if ((xfer=H5Pcreate(H5P_DATASET_XFER))<0) goto error;
    if (H5Pget_btree_ratios(xfer, splits+0, splits+1, splits+2)<0) {
	goto error;
    }
    if (H5Pclose(xfer)<0) goto error;
    
    /* Parse command-line options */
    for (i=1, j=0; i<argc; i++) {
	if (!strcmp(argv[i], "forward")) {
	    fill_style = FILL_FORWARD;
	} else if (!strcmp(argv[i], "reverse")) {
	    fill_style = FILL_REVERSE;
	} else if (!strcmp(argv[i], "inward")) {
	    fill_style = FILL_INWARD;
	} else if (!strcmp(argv[i], "outward")) {
	    fill_style = FILL_OUTWARD;
	} else if (!strcmp(argv[i], "random")) {
	    fill_style = FILL_RANDOM;
	} else if (!strcmp(argv[i], "cache")) {
	    use_cache = TRUE;
	} else if (j<3 && (isdigit(argv[i][0]) || '.'==argv[i][0])) {
	    splits[j++] = strtod(argv[i], NULL);
	} else {
	    usage(argv[0]);
	}
    }

    if (FILL_ALL==fill_style) {
	printf("%-7s %8s\n", "Style", "Bytes/Chunk");
	printf("%-7s %8s\n", "-----", "-----------");
	nerrors += test(FILL_FORWARD, splits, FALSE, use_cache);
	nerrors += test(FILL_REVERSE, splits, FALSE, use_cache);
	nerrors += test(FILL_INWARD,  splits, FALSE, use_cache);
	nerrors += test(FILL_OUTWARD, splits, FALSE, use_cache);
	nerrors += test(FILL_RANDOM,  splits, FALSE, use_cache);
    } else {
	if (use_cache) usage(argv[0]);
	nerrors += test(fill_style,   splits, TRUE, FALSE);
    }
    if (nerrors>0) goto error;
    cleanup();
    return 0;

 error:
    fprintf(stderr, "*** ERRORS DETECTED ***\n");
    return 1;
}
