/*
 * Copyright (C) 1998 NCSA
 *               All rights reserved.
 *
 * Programmer:  Robb Matzke <robb@arborea.spizella.com>
 *              Thursday, October  1, 1998
 *
 * Purpose:	Tests dataset fill values.
 */
#include <fcntl.h>
#include <hdf5.h>
#include <stdlib.h>
#include <unistd.h>

/*
 * Define NO_FILLING if you want to compare how this test works when there is
 * no fill value (that is, when the fill value is zero).
 */
/* #define NO_FILLING */


#define FILE_NAME_1	"fillval_1.h5"
#define FILE_NAME_2	"fillval_2.h5"
#define FILE_NAME_3	"fillval_3.h5"
#define FILE_NAME_4	"fillval_4.h5"
#define FILE_NAME_5	"fillval_5.h5"
#define FILE_NAME_6	"fillval_6.h5"
#define FILE_NAME_RAW	"fillval.raw"

#include <H5config.h>
#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif


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
cleanup(void)
{
    if (!getenv("HDF5_NOCLEANUP")) {
	remove(FILE_NAME_1);
	remove(FILE_NAME_2);
	remove(FILE_NAME_3);
	remove(FILE_NAME_4);
	remove(FILE_NAME_5);
	remove(FILE_NAME_6);
	remove(FILE_NAME_RAW);
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
display_error_cb(void __unused__ *client_data)
{
    puts("*FAILED*");
    H5Eprint(stdout);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	test_getset
 *
 * Purpose:	Tests the H5Pget_fill_value() and H5Pset_fill_value()
 *		functions.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_getset(void)
{
    herr_t	status;
    hid_t	dcpl;
    int		fill_i;
    hid_t	type_ss, type_si;
    struct fill_si {
	int 	v1, v2;
    }		fill_si;
    struct fill_ss {
	short	v1, v2;
    }		fill_ss, fill_ss_rd;
    
    printf("%-70s", "Testing property lists");
    fflush(stdout);

    /*
     * Create the dataset creation property list and the data types that will
     * be used during this test.
     */
    if ((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if ((type_ss=H5Tcreate(H5T_COMPOUND, sizeof fill_ss))<0 ||
	H5Tinsert(type_ss, "v1", HOFFSET(struct fill_ss, v1),
		  H5T_NATIVE_SHORT)<0 ||
	H5Tinsert(type_ss, "v2", HOFFSET(struct fill_ss, v2),
		  H5T_NATIVE_SHORT)<0) {
	goto error;
    }
    if ((type_si=H5Tcreate(H5T_COMPOUND, sizeof fill_si))<0 ||
	H5Tinsert(type_si, "v1", HOFFSET(struct fill_si, v1),
		  H5T_NATIVE_INT)<0 ||
	H5Tinsert(type_si, "v2", HOFFSET(struct fill_si, v2),
		  H5T_NATIVE_INT)<0) {
	goto error;
    }
    
    /*
     * Reading the fill value from a dataset creation property list that has
     * no fill value should result in a failure.
     */
    H5E_BEGIN_TRY {
	status = H5Pget_fill_value(dcpl, H5T_NATIVE_INT, &fill_i);
    } H5E_END_TRY;
    if (status>=0) {
	puts("*FAILED*");
	puts("   H5Pget_fill_value() should have been negative");
	goto error;
    }

    /*
     * Set the fill value using a struct as the data type.
     */
    fill_ss.v1 = 1111;
    fill_ss.v2 = 2222;
    if (H5Pset_fill_value(dcpl, type_ss, &fill_ss)<0) goto error;

    /*
     * Get the fill value using the same data type that was used to set it.
     */
    if (H5Pget_fill_value(dcpl, type_ss, &fill_ss_rd)<0) goto error;
    if (fill_ss.v1!=fill_ss_rd.v1 || fill_ss.v2!=fill_ss_rd.v2) {
	puts("*FAILED*");
	puts("   Failed to get fill value using same data type that was used");
	puts("   to set the fill value.");
	goto error;
    }

    /*
     * Get the fill value using some other data type.
     */
    if (H5Pget_fill_value(dcpl, type_si, &fill_si)<0) goto error;
    if (fill_ss.v1!=fill_si.v1 || fill_ss.v2!=fill_si.v2) {
	puts("*FAILED*");
	puts("   Failed to get fill value using a data type other than what");
	puts("   was used to set the fill value.");
	goto error;
    }

    /*
     * Reset the fill value
     */
    if (H5Pset_fill_value(dcpl, type_si, &fill_si)<0) goto error;
    if (H5Pget_fill_value(dcpl, type_ss, &fill_ss)<0) goto error;
    if (fill_si.v1!=fill_ss.v1 || fill_si.v2!=fill_ss.v2) {
	puts("*FAILED*");
	puts("   Resetting the fill value was unsuccessful.");
	goto error;
    }

    /* Success */
    if (H5Pclose(dcpl)<0) goto error;
    puts(" PASSED");
    return 0;

 error:
    H5Pclose(dcpl);
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Tests creating datasets that have fill values.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_create(const char *filename, H5D_layout_t layout)
{
    hid_t	file, space, dcpl, dset1, dset2, dset3;
    hsize_t	cur_size[5] = {32, 16, 8, 4, 2};
    hsize_t	ch_size[5] = {1, 1, 1, 4, 2};
    short	rd_s, fill_s = 0x1234;
    long	rd_l, fill_l = 0x4321;
    char	test[256];

    if (H5D_CHUNKED==layout) {
	strcpy(test, "Testing chunked dataset creation");
    } else {
	strcpy(test, "Testing contiguous dataset creation");
    }
    printf("%-70s", test);
    fflush(stdout);

    /*
     * Create a file and three datasets.  The three datasets test three fill
     * conversion paths: small to large, large to small, and no conversion.
     * They depend on `short' being smaller than `long'.
     */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC,
			H5P_DEFAULT, H5P_DEFAULT))<0) goto error;
    if ((space=H5Screate_simple(5, cur_size, cur_size))<0) goto error;
    if ((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if (H5D_CHUNKED==layout) {
	if (H5Pset_chunk(dcpl, 5, ch_size)<0) goto error;
    }

    /* Small to large fill conversion */
#ifndef NO_FILLING
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_SHORT, &fill_s)<0) goto error;
#endif
    if ((dset1=H5Dcreate(file, "dset1", H5T_NATIVE_LONG, space, dcpl))<0)
	goto error;

    /* Large to small fill conversion */
#ifndef NO_FILLING
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_LONG, &fill_l)<0) goto error;
#endif
    if ((dset2=H5Dcreate(file, "dset2", H5T_NATIVE_SHORT, space, dcpl))<0)
	goto error;

    /* No conversion */
#ifndef NO_FILLING
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_LONG, &fill_l)<0) goto error;
#endif
    if ((dset3=H5Dcreate(file, "dset3", H5T_NATIVE_LONG, space, dcpl))<0)
	goto error;
    
    /* Close everything */
    if (H5Dclose(dset1)<0) goto error;
    if (H5Dclose(dset2)<0) goto error;
    if (H5Dclose(dset3)<0) goto error;
    if (H5Sclose(space)<0) goto error;
    if (H5Pclose(dcpl)<0) goto error;
    if (H5Fclose(file)<0) goto error;

    /* Open the file and get the dataset fill value from each dataset */
    if ((file=H5Fopen(FILE_NAME_1, H5F_ACC_RDONLY, H5P_DEFAULT))<0)
	goto error;

    /* Large to small conversion */
    if ((dset1=H5Dopen(file, "dset1"))<0) goto error;
    if ((dcpl=H5Dget_create_plist(dset1))<0) goto error;
    if (H5Dclose(dset1)<0) goto error;
#ifndef NO_FILLING
    if (H5Pget_fill_value(dcpl, H5T_NATIVE_SHORT, &rd_s)<0) goto error;
    if (rd_s!=fill_s) {
	puts("*FAILED*");
	puts("   Got a different fill value than what was set.");
	printf("   Got %d, set %d\n", rd_s, fill_s);
	goto error;
    }
#endif
    if (H5Pclose(dcpl)<0) goto error;

    /* Small to large conversion */
    if ((dset2=H5Dopen(file, "dset2"))<0) goto error;
    if ((dcpl=H5Dget_create_plist(dset2))<0) goto error;
    if (H5Dclose(dset2)<0) goto error;
#ifndef NO_FILLING
    if (H5Pget_fill_value(dcpl, H5T_NATIVE_LONG, &rd_l)<0) goto error;
    if (rd_l!=fill_l) {
	puts("*FAILED*");
	puts("   Got a different fill value than what was set.");
	printf("   Got %ld, set %ld\n", rd_l, fill_l);
	goto error;
    }
#endif
    if (H5Pclose(dcpl)<0) goto error;
    
    /* No conversion */
    if ((dset3=H5Dopen(file, "dset3"))<0) goto error;
    if ((dcpl=H5Dget_create_plist(dset3))<0) goto error;
    if (H5Dclose(dset3)<0) goto error;
#ifndef NO_FILLING
    if (H5Pget_fill_value(dcpl, H5T_NATIVE_LONG, &rd_l)<0) goto error;
    if (rd_l!=fill_l) {
	puts("*FAILED*");
	puts("   Got a different fill value than what was set.");
	printf("   Got %ld, set %ld\n", rd_l, fill_l);
	goto error;
    }
#endif
    if (H5Pclose(dcpl)<0) goto error;
    

    
    if (H5Fclose(file)<0) goto error;
    puts(" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Pclose(dcpl);
	H5Sclose(space);
	H5Dclose(dset1);
	H5Dclose(dset2);
	H5Dclose(dset3);
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_rdwr
 *
 * Purpose:	Tests fill values for chunked datasets.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_rdwr(const char *filename, H5D_layout_t layout)
{
    hid_t	file, fspace, mspace, dcpl, dset;
    hsize_t	cur_size[5] = {32, 16, 8, 4, 2};
    hsize_t	ch_size[5] = {1, 16, 8, 4, 2};
    hsize_t	one[5] = {1, 1, 1, 1, 1};
    hsize_t	hs_size[5], hs_stride[5];
    hssize_t	hs_offset[5], nelmts;
#ifdef NO_FILLING
    int		fillval = 0;
#else
    int		fillval = 0x4c70f1cd;
#endif
    int		val_rd, should_be;
    int		i, j, *buf=NULL, odd;
    char	test[256];

    if (H5D_CHUNKED==layout) {
	strcpy(test, "Testing chunked dataset I/O");
    } else {
	strcpy(test, "Testing contiguous dataset I/O");
    }
    printf("%-70s", test);
    fflush(stdout);

    /* Create a file and dataset */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC,
			H5P_DEFAULT, H5P_DEFAULT))<0) goto error;
    if ((fspace=H5Screate_simple(5, cur_size, cur_size))<0) goto error;
    if ((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if (H5D_CHUNKED==layout) {
	if (H5Pset_chunk(dcpl, 5, ch_size)<0) goto error;
    }
#ifndef NO_FILLING
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval)<0) goto error;
#endif
    if ((dset=H5Dcreate(file, "dset", H5T_NATIVE_INT, fspace, dcpl))<0)
	goto error;

    /* Read some data and make sure it's the fill value */
    if ((mspace=H5Screate_simple(5, one, NULL))<0) goto error;
    for (i=0; i<1000; i++) {
	for (j=0; j<5; j++) {
	    hs_offset[j] = rand() % cur_size[j];
	}
	if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL,
				one, NULL)<0) goto error;
	if (H5Dread(dset, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT,
		    &val_rd)<0) goto error;
	if (val_rd!=fillval) {
	    puts("*FAILED*");
	    puts("   Value read was not a fill value.");
	    printf("   Elmt={%ld,%ld,%ld,%ld,%ld}, read: %u, "
		   "Fill value: %u\n",
		   (long)hs_offset[0], (long)hs_offset[1],
		   (long)hs_offset[2], (long)hs_offset[3],
		   (long)hs_offset[4], val_rd, fillval);
	    goto error;
	}
    }
    if (H5Sclose(mspace)<0) goto error;

    /* Write to all odd data locations */
    for (i=0, nelmts=1; i<5; i++) {
	hs_size[i] = cur_size[i]/2;
	hs_offset[i] = 0;
	hs_stride[i] = 2;
	nelmts *= hs_size[i];
    }
    if ((mspace=H5Screate_simple(5, hs_size, hs_size))<0) goto error;
    buf = malloc(nelmts*sizeof(int));
    for (i=0; i<nelmts; i++) buf[i] = 9999;
    if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, hs_stride,
			    hs_size, NULL)<0) goto error;
    if (H5Dwrite(dset, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT,
		 buf)<0) goto error;
    free(buf);
    buf = NULL;
    H5Sclose(mspace);

    /* Read some data and make sure it's the right value */
    if ((mspace=H5Screate_simple(5, one, NULL))<0) goto error;
    for (i=0; i<1000; i++) {
	for (j=0, odd=0; j<5; j++) {
	    hs_offset[j] = rand() % cur_size[j];
	    odd += hs_offset[j]%2;
	}
	should_be = odd ? fillval : 9999;
	if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL,
				one, NULL)<0) goto error;
	if (H5Dread(dset, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT,
		    &val_rd)<0) goto error;

	if (val_rd!=should_be) {
	    puts("*FAILED*");
	    puts("   Value read was not correct.");
	    printf("   Elmt={%ld,%ld,%ld,%ld,%ld}, read: %u, "
		   "should be: %u\n",
		   (long)hs_offset[0], (long)hs_offset[1],
		   (long)hs_offset[2], (long)hs_offset[3],
		   (long)hs_offset[4], val_rd, should_be);
	    goto error;
	}
    }
    if (H5Sclose(mspace)<0) goto error;


    
    if (H5Dclose(dset)<0) goto error;
    if (H5Sclose(fspace)<0) goto error;
    if (H5Pclose(dcpl)<0) goto error;
    if (H5Fclose(file)<0) goto error;
    puts(" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Dclose(dset);
	H5Sclose(fspace);
	H5Sclose(mspace);
	H5Pclose(dcpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_extend
 *
 * Purpose:	Test that filling works okay when a dataset is extended.
 *
 * Return:	Success:	0
 *
 *		Failure:	number of errors
 *
 * Programmer:	Robb Matzke
 *              Monday, October  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_extend(const char *filename, H5D_layout_t layout)
{
    hid_t	file, fspace, mspace, dcpl, dset;
    hsize_t	cur_size[5] = {32, 16, 8, 4, 2};
    hsize_t	max_size[5] = {128, 64, 32, 16, 8};
    hsize_t	ch_size[5] = {1, 16, 8, 4, 2};
    hsize_t	one[5] = {1, 1, 1, 1, 1};
    hsize_t	hs_size[5], hs_stride[5];
    hssize_t	hs_offset[5], nelmts;
#ifdef NO_FILLING
    int		fillval = 0;
#else
    int		fillval = 0x4c70f1cd;
#endif
    int		val_rd, should_be;
    int		i, j, *buf=NULL, odd, fd;
    char	test[256];

    if (H5D_CHUNKED==layout) {
	strcpy(test, "Testing chunked dataset extend");
    } else {
	strcpy(test, "Testing contiguous dataset extend");
    }
    printf("%-70s", test);
    fflush(stdout);

    if ((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    if (H5D_CHUNKED==layout) {
	if (H5Pset_chunk(dcpl, 5, ch_size)<0) goto error;
    }
#ifndef NO_FILLING
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval)<0) goto error;
#endif
    
#if 1
    /*
     * Remove this once contiguous datasets can support extensions in other
     * than the slowest varying dimension.  The purpose of this block is to
     * make only the slowest varying dimension extendible and yet have the
     * same total number of elements as originally.
     *
     * If this is removed prematurely then you will get an error `only the
     * first dimension can be extendible' as long as the test isn't skipped
     * below.
     */
    if (H5D_CONTIGUOUS==layout) {
	max_size[0] = (max_size[0]*max_size[1]*max_size[2]*
		       max_size[3]*max_size[4]) /
		      (cur_size[1]*cur_size[2]*cur_size[3]*cur_size[4]);
	max_size[1] = cur_size[1];
	max_size[2] = cur_size[2];
	max_size[3] = cur_size[3];
	max_size[4] = cur_size[4];
    }
#endif

#if 1
    /*
     * Remove this once internal contiguous datasets can support
     * extending. If it's removed prematurely you will get an error
     * `extendible contiguous non-external dataset' as long as the test isn't
     * skipped below.
     */
    if (H5D_CONTIGUOUS==layout) {
	nelmts = max_size[0]*max_size[1]*max_size[2]*max_size[3]*max_size[4];
	if ((fd=open(FILE_NAME_RAW, O_RDWR|O_CREAT|O_TRUNC, 0666))<0 ||
	    close(fd)<0) goto error;
	if (H5Pset_external(dcpl, FILE_NAME_RAW, 0, nelmts*sizeof(int))<0)
	    goto error;
    }
#endif

#if 1
    /*
     * Remove this when contiguous datasets can be exended to some
     * predetermined fininte size, even if it's just in the slowest varying
     * dimension.  If it's removed prematurely then you'll get one of the
     * errors described above or `unable to select fill value region'.
     */
    if (H5D_CONTIGUOUS==layout) {
	puts("   SKIP");
	puts("   Not implemented yet -- needs H5S_SELECT_DIFF operator");
	return 0;
    }
#endif
    
    /* Create a file and dataset */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC,
			H5P_DEFAULT, H5P_DEFAULT))<0) goto error;
    if ((fspace=H5Screate_simple(5, cur_size, max_size))<0) goto error;
    if ((dset=H5Dcreate(file, "dset", H5T_NATIVE_INT, fspace, dcpl))<0)
	goto error;

    /* Read some data and make sure it's the fill value */
    if ((mspace=H5Screate_simple(5, one, NULL))<0) goto error;
    for (i=0; i<1000; i++) {
	for (j=0; j<5; j++) {
	    hs_offset[j] = rand() % cur_size[j];
	}
	if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL,
				one, NULL)<0) goto error;
	if (H5Dread(dset, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT,
		    &val_rd)<0) goto error;
	if (val_rd!=fillval) {
	    puts("*FAILED*");
	    puts("   Value read was not a fill value.");
	    printf("   Elmt={%ld,%ld,%ld,%ld,%ld}, read: %u, "
		   "Fill value: %u\n",
		   (long)hs_offset[0], (long)hs_offset[1],
		   (long)hs_offset[2], (long)hs_offset[3],
		   (long)hs_offset[4], val_rd, fillval);
	    goto error;
	}
    }
    if (H5Sclose(mspace)<0) goto error;

    /* Write to all odd data locations */
    for (i=0, nelmts=1; i<5; i++) {
	hs_size[i] = cur_size[i]/2;
	hs_offset[i] = 0;
	hs_stride[i] = 2;
	nelmts *= hs_size[i];
    }
    if ((mspace=H5Screate_simple(5, hs_size, hs_size))<0) goto error;
    buf = malloc(nelmts*sizeof(int));
    for (i=0; i<nelmts; i++) buf[i] = 9999;
    if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, hs_stride,
			    hs_size, NULL)<0) goto error;
    if (H5Dwrite(dset, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT,
		 buf)<0) goto error;
    free(buf);
    buf = NULL;
    H5Sclose(mspace);

    /* Read some data and make sure it's the right value */
    if ((mspace=H5Screate_simple(5, one, NULL))<0) goto error;
    for (i=0; i<1000; i++) {
	for (j=0, odd=0; j<5; j++) {
	    hs_offset[j] = rand() % cur_size[j];
	    odd += hs_offset[j]%2;
	}
	should_be = odd ? fillval : 9999;
	if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL,
				one, NULL)<0) goto error;
	if (H5Dread(dset, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT,
		    &val_rd)<0) goto error;

	if (val_rd!=should_be) {
	    puts("*FAILED*");
	    puts("   Value read was not correct.");
	    printf("   Elmt={%ld,%ld,%ld,%ld,%ld}, read: %u, "
		   "should be: %u\n",
		   (long)hs_offset[0], (long)hs_offset[1],
		   (long)hs_offset[2], (long)hs_offset[3],
		   (long)hs_offset[4], val_rd, should_be);
	    goto error;
	}
    }
    if (H5Sclose(mspace)<0) goto error;

    /* Extend the dataset */
    if (H5Dextend(dset, max_size)<0) goto error;
    if (H5Sclose(fspace)<0) goto error;
    if ((fspace=H5Dget_space(dset))<0) goto error;
    
    /* Read some data and make sure it's the right value */
    if ((mspace=H5Screate_simple(5, one, NULL))<0) goto error;
    for (i=0; i<1000; i++) {
	for (j=0, odd=0; j<5; j++) {
	    hs_offset[j] = rand() % max_size[j];
	    if ((hsize_t)hs_offset[j]>=cur_size[j]) {
		odd = 1;
	    } else {
		odd += hs_offset[j]%2;
	    }
	}
	
	should_be = odd ? fillval : 9999;
	if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, hs_offset, NULL,
				one, NULL)<0) goto error;
	if (H5Dread(dset, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT,
		    &val_rd)<0) goto error;

	if (val_rd!=should_be) {
	    puts("*FAILED*");
	    puts("   Value read was not correct.");
	    printf("   Elmt={%ld,%ld,%ld,%ld,%ld}, read: %u, "
		   "should be: %u\n",
		   (long)hs_offset[0], (long)hs_offset[1],
		   (long)hs_offset[2], (long)hs_offset[3],
		   (long)hs_offset[4], val_rd, should_be);
	    goto error;
	}
    }
    if (H5Sclose(mspace)<0) goto error;

    if (H5Dclose(dset)<0) goto error;
    if (H5Sclose(fspace)<0) goto error;
    if (H5Pclose(dcpl)<0) goto error;
    if (H5Fclose(file)<0) goto error;
    puts(" PASSED");
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Dclose(dset);
	H5Sclose(fspace);
	H5Sclose(mspace);
	H5Pclose(dcpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}
    
    

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests fill values
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    int		nerrors=0, argno, test_contig=1, test_chunk=1;

    if (argc>=2) {
	test_contig = test_chunk = 0;
	for (argno=1; argno<argc; argno++) {
	    if (!strcmp(argv[argno], "contiguous")) {
		test_contig = 1;
	    } else if (!strcmp(argv[argno], "chunked")) {
		test_chunk = 1;
	    } else {
		fprintf(stderr, "usage: %s [contiguous] [chunked]\n", argv[0]);
		exit(1);
	    }
	}
    }
    
    H5Eset_auto(display_error_cb, NULL);


    nerrors += test_getset();

    /* Chunked storage layout tests */
    if (test_chunk) {
	nerrors += test_create(FILE_NAME_1, H5D_CHUNKED);
	nerrors += test_rdwr(FILE_NAME_3, H5D_CHUNKED);
	nerrors += test_extend(FILE_NAME_5, H5D_CHUNKED);
    }

    /* Contiguous storage layout tests */
    if (test_contig) {
	nerrors += test_create(FILE_NAME_2, H5D_CONTIGUOUS);
	nerrors += test_rdwr(FILE_NAME_4, H5D_CONTIGUOUS);
	nerrors += test_extend(FILE_NAME_6, H5D_CONTIGUOUS);
    }
    
    if (nerrors) goto error;
    puts("All fill value tests passed.");
    cleanup();
    return 0;

 error:
    puts("***** FILL VALUE TESTS FAILED *****");
    return 1;
}
