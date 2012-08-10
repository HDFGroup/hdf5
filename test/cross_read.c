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
 * Programmer:  Raymond Lu <slu@ncsa.uiuc.edu>
 *              Thursday, March 23, 2006
 *
 * Purpose:     Check if floating-point data created on big-endian and
 *              little-endian machines can be read on the machine running this test.
 */

#include "h5test.h"
#include "H5srcdir.h"

const char *FILENAME[] = {
    "vms_data",
    "le_data",
    "be_data",
    NULL
};

#define DATASETNAME        "Array_le"
#define DATASETNAME1       "Array_be"
#define DATASETNAME2       "Scale_offset_float_data_le"
#define DATASETNAME3       "Scale_offset_float_data_be"
#define DATASETNAME4       "Scale_offset_double_data_le"
#define DATASETNAME5       "Scale_offset_double_data_be"
#define DATASETNAME6       "Scale_offset_char_data_le"
#define DATASETNAME7       "Scale_offset_char_data_be"
#define DATASETNAME8       "Scale_offset_short_data_le"
#define DATASETNAME9       "Scale_offset_short_data_be"
#define DATASETNAME10      "Scale_offset_int_data_le"
#define DATASETNAME11      "Scale_offset_int_data_be"
#define DATASETNAME12      "Scale_offset_long_long_data_le"
#define DATASETNAME13      "Scale_offset_long_long_data_be"

#define DATASETNAME14      "Fletcher_float_data_le"
#define DATASETNAME15      "Fletcher_float_data_be"
#define DATASETNAME16      "Deflate_float_data_le"
#define DATASETNAME17      "Deflate_float_data_be"
#define DATASETNAME18      "Szip_float_data_le"
#define DATASETNAME19      "Szip_float_data_be"
#define DATASETNAME20      "Shuffle_float_data_le"
#define DATASETNAME21      "Shuffle_float_data_be"
#define DATASETNAME22      "Nbit_float_data_le"
#define DATASETNAME23      "Nbit_float_data_be"

#define NX 		6
#define NY 		6


/*-------------------------------------------------------------------------
 * Function:    open_dataset
 *
 * Purpose:     Read and compare the data from a dataset.
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 * Programmer:  Raymond Lu
 *              17 May 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int check_data(const char *dsetname, hid_t fid, hbool_t floating_number)
{
    hid_t       dataset;         /* handles */
    double      data_in[NX+1][NY]; /* input buffer */
    double      data_out[NX+1][NY]; /* output buffer */
    long long   int_data_in[NX+1][NY]; /* input buffer */
    long long   int_data_out[NX+1][NY]; /* output buffer */
    int         i, j;
    unsigned 	nerrors = 0;

    /* 
     * Open the regular dataset.
     */
    if((dataset = H5Dopen2(fid, dsetname, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++) {
            data_in[j][i] = ((double)(i + j + 1))/3;
            data_out[j][i] = 0;

            int_data_in[j][i] = i + j;
            int_data_out[j][i] = 0;
        }
    }
    for (i = 0; i < NY; i++) {
        data_in[NX][i] = -2.2;
        data_out[NX][i] = 0;

        int_data_in[NX][i] = -2;
        int_data_out[NX][i] = 0;
    }

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(floating_number) {
	if(H5Dread(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		data_out) < 0)
	    TEST_ERROR;

	/* Check results */
	for (j=0; j<(NX+1); j++) {
	    for (i=0; i<NY; i++) {
		if (!DBL_REL_EQUAL(data_out[j][i], data_in[j][i], 0.001)) {
		    if (!nerrors++) {
			H5_FAILED();
			printf("element [%d][%d] is %g but should have been %g\n",
			       j, i, data_out[j][i], data_in[j][i]);
		    }
		}
	    }
	}
    } else {
	if(H5Dread(dataset, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		int_data_out) < 0)
	    TEST_ERROR;

	/* Check results */
	for (j=0; j<(NX+1); j++) {
	    for (i=0; i<NY; i++) {
		if (int_data_out[j][i] != int_data_in[j][i]) {
		    if (!nerrors++) {
			H5_FAILED();
			printf("element [%d][%d] is %d but should have been %d\n",
			       j, i, (int)int_data_out[j][i],
			       (int)int_data_in[j][i]);
		    }
		}
	    }
	}
    }

    /*
     * Close/release resources.
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Failure */
    if (nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, NX*NY);
        return 1;
    }

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    open_dataset
 *
 * Purpose:     Handle each dataset from the data file.
 *
 * Return:      Success:        0
 *              Failure:        Number of failures 
 *
 * Programmer:  Raymond Lu
 *              21 January 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int open_dataset(char *fname)
{
    const char *pathname = H5_get_srcdir_filename(fname); /* Corrected test file name */
    hid_t       file;         /* handles */
    unsigned 	nerrors = 0;
    const char  *not_supported= "    filter is not enabled.";

    /*
     * Open the file.
     */
    if((file = H5Fopen(pathname, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    TESTING("regular dataset of LE DOUBLE");
    nerrors += check_data(DATASETNAME, file, TRUE);

    TESTING("regular dataset of BE DOUBLE");
    nerrors += check_data(DATASETNAME1, file, TRUE);

    TESTING("dataset of LE FLOAT with scale-offset filter");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    nerrors += check_data(DATASETNAME2, file, TRUE);
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/
 
    TESTING("dataset of BE FLOAT with scale-offset filter");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    nerrors += check_data(DATASETNAME3, file, TRUE);
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    TESTING("dataset of LE DOUBLE with scale-offset filter");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    nerrors += check_data(DATASETNAME4, file, TRUE);
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/
 
    TESTING("dataset of BE DOUBLE with scale-offset filter");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    nerrors += check_data(DATASETNAME5, file, TRUE);
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/
 
    TESTING("dataset of LE CHAR with scale-offset filter");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    nerrors += check_data(DATASETNAME6, file, FALSE);
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/
 
    TESTING("dataset of BE CHAR with scale-offset filter");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    nerrors += check_data(DATASETNAME7, file, FALSE);
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/
 
    TESTING("dataset of LE SHORT with scale-offset filter");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    nerrors += check_data(DATASETNAME8, file, FALSE);
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/
 
    TESTING("dataset of BE SHORT with scale-offset filter");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    nerrors += check_data(DATASETNAME9, file, FALSE);
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    TESTING("dataset of LE INT with scale-offset filter");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    nerrors += check_data(DATASETNAME10, file, FALSE);
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/
 
    TESTING("dataset of BE INT with scale-offset filter");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    nerrors += check_data(DATASETNAME11, file, FALSE);
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    TESTING("dataset of LE LONG LONG with scale-offset filter");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    nerrors += check_data(DATASETNAME12, file, FALSE);
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/
 
    TESTING("dataset of BE LONG LONG with scale-offset filter");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    nerrors += check_data(DATASETNAME13, file, FALSE);
#else /*H5_HAVE_FILTER_SCALEOFFSET*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SCALEOFFSET*/

    TESTING("dataset of LE FLOAT with Fletcher32 filter");
#ifdef H5_HAVE_FILTER_FLETCHER32
    nerrors += check_data(DATASETNAME14, file, TRUE);
#else /*H5_HAVE_FILTER_FLETCHER32*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_FLETCHER32*/
 
    TESTING("dataset of BE FLOAT with Fletcher32 filter");
#ifdef H5_HAVE_FILTER_FLETCHER32
    nerrors += check_data(DATASETNAME15, file, TRUE);
#else /*H5_HAVE_FILTER_FLETCHER32*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_FLETCHER32*/
 
    TESTING("dataset of LE FLOAT with Deflate filter");
#ifdef H5_HAVE_FILTER_DEFLATE
    nerrors += check_data(DATASETNAME16, file, TRUE);
#else /*H5_HAVE_FILTER_DEFLATE*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_DEFLATE*/

    TESTING("dataset of BE FLOAT with Deflate filter");
#ifdef H5_HAVE_FILTER_DEFLATE
    nerrors += check_data(DATASETNAME17, file, TRUE);
#else /*H5_HAVE_FILTER_DEFLATE*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_DEFLATE*/

    TESTING("dataset of LE FLOAT with Szip filter");
#ifdef H5_HAVE_FILTER_SZIP
    nerrors += check_data(DATASETNAME18, file, TRUE);
#else /*H5_HAVE_FILTER_SZIP*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SZIP*/

    TESTING("dataset of BE FLOAT with Szip filter");
#ifdef H5_HAVE_FILTER_SZIP
    nerrors += check_data(DATASETNAME19, file, TRUE);
#else /*H5_HAVE_FILTER_SZIP*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SZIP*/

    TESTING("dataset of LE FLOAT with Shuffle filter");
#ifdef H5_HAVE_FILTER_SHUFFLE
    nerrors += check_data(DATASETNAME20, file, TRUE);
#else /*H5_HAVE_FILTER_SHUFFLE*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SHUFFLE*/

    TESTING("dataset of BE FLOAT with Shuffle filter");
#ifdef H5_HAVE_FILTER_SHUFFLE
    nerrors += check_data(DATASETNAME21, file, TRUE);
#else /*H5_HAVE_FILTER_SHUFFLE*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_SHUFFLE*/

    TESTING("dataset of LE FLOAT with Nbit filter");
#ifdef H5_HAVE_FILTER_NBIT
    nerrors += check_data(DATASETNAME22, file, TRUE);
#else /*H5_HAVE_FILTER_NBIT*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_NBIT*/

    TESTING("dataset of BE FLOAT with Nbit filter");
#ifdef H5_HAVE_FILTER_NBIT
    nerrors += check_data(DATASETNAME23, file, TRUE);
#else /*H5_HAVE_FILTER_NBIT*/
    SKIPPED();
    puts(not_supported);
#endif /*H5_HAVE_FILTER_NBIT*/

    if(H5Fclose(file))
        TEST_ERROR
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return nerrors;
}


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests reading files created on LE and BE systems.
 *
 * Return:      Success:        exit(0)
 *              Failure:        exit(1)
 *
 * Programmer:  Raymond Lu
 *              Thursday, March 23, 2006
 *
 *-------------------------------------------------------------------------
 */
int main(void)
{
    char        filename[1024];
    unsigned 	nerrors = 0;

    h5_reset();

    puts("Testing reading data created on Linux");
    h5_fixname(FILENAME[1], H5P_DEFAULT, filename, sizeof filename);
    nerrors += open_dataset(filename);

    puts("Testing reading data created on Solaris");
    h5_fixname(FILENAME[2], H5P_DEFAULT, filename, sizeof filename);
    nerrors += open_dataset(filename);

    if (nerrors) {
        printf("***** %u FAILURE%s! *****\n",
               nerrors, 1==nerrors?"":"S");
        HDexit(1);
    }

    printf("All data type tests passed.\n");
    return 0;
}
