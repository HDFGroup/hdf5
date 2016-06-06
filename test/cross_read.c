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
#ifdef H5_HAVE_FILTER_SZIP
#define DATASETNAME18      "Szip_float_data_le"
#define DATASETNAME19      "Szip_float_data_be"
#endif /* H5_HAVE_FILTER_SZIP */
#define DATASETNAME20      "Shuffle_float_data_le"
#define DATASETNAME21      "Shuffle_float_data_be"
#define DATASETNAME22      "Nbit_float_data_le"
#define DATASETNAME23      "Nbit_float_data_be"

#define NX 		6
#define NY 		6


/*-------------------------------------------------------------------------
 * Function:    check_data_i
 *
 * Purpose:     Read and compare the integer data from a dataset.
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 * Programmer:  Raymond Lu
 *              17 May 2011
 *
 *-------------------------------------------------------------------------
 */
static int
check_data_i(const char *dsetname, hid_t fid)
{
    hid_t       did = -1;               /* dataset ID                       */
    long long   data_in[NX+1][NY];      /* input buffer                     */
    long long   data_out[NX+1][NY];     /* output buffer                    */
    int         i, j;                   /* iterators                        */
    int         nerrors = 0;            /* # errors in dataset values       */

    /* Open the dataset. */
    if((did = H5Dopen2(fid, dsetname, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Initialization. */
    /* Input (last row is different) */
    for(i = 0; i < NX; i++)
        for(j = 0; j < NY; j++)
            data_in[i][j] = i + j;
    for(i = 0; i < NY; i++)
        data_in[NX][i] = -2;
    /* Output */
    HDmemset(data_out, 0, (NX+1) * NY * sizeof(long long));

    /* Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(did, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, data_out) < 0)
        FAIL_STACK_ERROR;

    /* Check results */
    for(i = 0; i < (NX + 1); i++)
        for(j = 0; j < NY; j++)
            if(data_out[i][j] != data_in[i][j])
                if(!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %lld but should have been %lld\n",
                            (int)i, (int)j, data_out[i][j], data_in[i][j]);
                } /* end if */

    /* Close/release resources. */
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR

    /* Failure */
    if(nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, (int)(NX*NY));
        return 1;
    } /* end if */

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(did);
    } H5E_END_TRY;
    return 1;
} /* end check_data_i() */


/*-------------------------------------------------------------------------
 * Function:    check_data_f
 *
 * Purpose:     Read and compare the floating-point data from a dataset.
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 * Programmer:  Raymond Lu
 *              17 May 2011
 *
 *-------------------------------------------------------------------------
 */
static int
check_data_f(const char *dsetname, hid_t fid)
{
    hid_t       did = -1;               /* dataset ID                       */
    double      data_in[NX+1][NY];      /* input buffer                     */
    double      data_out[NX+1][NY];     /* output buffer                    */
    int         i, j;                   /* iterators                        */
    int         nerrors = 0;            /* # of errors in dataset values    */

    /* Open the dataset. */
    if((did = H5Dopen2(fid, dsetname, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Initialization. */
    /* Input (last row is different) */
    for(i = 0; i < NX; i++)
        for(j = 0; j < NY; j++)
            data_in[i][j] = ((double)(i + j + 1)) / (double)3.0F;
    for(i = 0; i < NY; i++)
        data_in[NX][i] = -2.2F;
    /* Output */
    HDmemset(data_out, 0, (NX+1) * NY * sizeof(double));

    /* Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(did, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, data_out) < 0)
        FAIL_STACK_ERROR;

    /* Check results */
    for(i = 0; i < (NX + 1); i++)
        for(j = 0; j < NY; j++)
            if(!H5_DBL_REL_EQUAL(data_out[i][j], data_in[i][j], (double)0.001F))
                if(!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %g but should have been %g\n",
                        (int)i, (int)j, data_out[i][j], data_in[i][j]);
                } /* end if */

    /* Close/release resources. */
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR

    /* Failure */
    if(nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, (int)(NX*NY));
        return 1;
    } /* end if */

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(did);
    } H5E_END_TRY;
    return 1;
} /* end check_data_f() */


/*-------------------------------------------------------------------------
 * Function:    check_file
 *
 * Purpose:     Handle each dataset from the data file.
 *
 * Return:      Success:        0
 *              Failure:        Number of failures 
 *
 * Programmer:  Raymond Lu
 *              21 January 2011
 *
 *-------------------------------------------------------------------------
 */
static int
check_file(char *filename)
{
    const char *pathname = H5_get_srcdir_filename(filename);    /* Corrected test file name     */
    hid_t       fid = -1;                                       /* file ID                      */
    int         nerrors = 0;                                    /* # of datasets with errors    */
    const char  *not_supported= "    filter is not enabled.";   /* no filter message            */

    /* Open the file. */
    if((fid = H5Fopen(pathname, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    TESTING("regular dataset of LE DOUBLE");
    nerrors += check_data_f(DATASETNAME, fid);

    TESTING("regular dataset of BE DOUBLE");
    nerrors += check_data_f(DATASETNAME1, fid);

    TESTING("dataset of LE FLOAT with scale-offset filter");
    nerrors += check_data_f(DATASETNAME2, fid);
 
    TESTING("dataset of BE FLOAT with scale-offset filter");
    nerrors += check_data_f(DATASETNAME3, fid);

    TESTING("dataset of LE DOUBLE with scale-offset filter");
    nerrors += check_data_f(DATASETNAME4, fid);
 
    TESTING("dataset of BE DOUBLE with scale-offset filter");
    nerrors += check_data_f(DATASETNAME5, fid);
 
    TESTING("dataset of LE CHAR with scale-offset filter");
    nerrors += check_data_i(DATASETNAME6, fid);
 
    TESTING("dataset of BE CHAR with scale-offset filter");
    nerrors += check_data_i(DATASETNAME7, fid);
 
    TESTING("dataset of LE SHORT with scale-offset filter");
    nerrors += check_data_i(DATASETNAME8, fid);
 
    TESTING("dataset of BE SHORT with scale-offset filter");
    nerrors += check_data_i(DATASETNAME9, fid);

    TESTING("dataset of LE INT with scale-offset filter");
    nerrors += check_data_i(DATASETNAME10, fid);
 
    TESTING("dataset of BE INT with scale-offset filter");
    nerrors += check_data_i(DATASETNAME11, fid);

    TESTING("dataset of LE LONG LONG with scale-offset filter");
    nerrors += check_data_i(DATASETNAME12, fid);
 
    TESTING("dataset of BE LONG LONG with scale-offset filter");
    nerrors += check_data_i(DATASETNAME13, fid);

    TESTING("dataset of LE FLOAT with Fletcher32 filter");
    nerrors += check_data_f(DATASETNAME14, fid);
 
    TESTING("dataset of BE FLOAT with Fletcher32 filter");
    nerrors += check_data_f(DATASETNAME15, fid);
 
    TESTING("dataset of LE FLOAT with Deflate filter");
#ifdef H5_HAVE_FILTER_DEFLATE
    nerrors += check_data_f(DATASETNAME16, fid);
#else /*H5_HAVE_FILTER_DEFLATE*/
    SKIPPED();
    HDputs(not_supported);
#endif /*H5_HAVE_FILTER_DEFLATE*/

    TESTING("dataset of BE FLOAT with Deflate filter");
#ifdef H5_HAVE_FILTER_DEFLATE
    nerrors += check_data_f(DATASETNAME17, fid);
#else /*H5_HAVE_FILTER_DEFLATE*/
    SKIPPED();
    HDputs(not_supported);
#endif /*H5_HAVE_FILTER_DEFLATE*/

    TESTING("dataset of LE FLOAT with Szip filter");
#ifdef H5_HAVE_FILTER_SZIP
    nerrors += check_data_f(DATASETNAME18, fid);
#else /*H5_HAVE_FILTER_SZIP*/
    SKIPPED();
    HDputs(not_supported);
#endif /*H5_HAVE_FILTER_SZIP*/

    TESTING("dataset of BE FLOAT with Szip filter");
#ifdef H5_HAVE_FILTER_SZIP
    nerrors += check_data_f(DATASETNAME19, fid);
#else /*H5_HAVE_FILTER_SZIP*/
    SKIPPED();
    HDputs(not_supported);
#endif /*H5_HAVE_FILTER_SZIP*/

    TESTING("dataset of LE FLOAT with Shuffle filter");
    nerrors += check_data_f(DATASETNAME20, fid);

    TESTING("dataset of BE FLOAT with Shuffle filter");
    nerrors += check_data_f(DATASETNAME21, fid);

    TESTING("dataset of LE FLOAT with Nbit filter");
    nerrors += check_data_f(DATASETNAME22, fid);

    TESTING("dataset of BE FLOAT with Nbit filter");
    nerrors += check_data_f(DATASETNAME23, fid);

    if(H5Fclose(fid))
        FAIL_STACK_ERROR
    return nerrors;

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
    } H5E_END_TRY;
    return nerrors;
} /* end check_file() */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests reading files created on LE and BE systems.
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 * Programmer:  Raymond Lu
 *              Thursday, March 23, 2006
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    char        filename[1024];
    int         nerrors = 0;

    h5_reset();

    HDputs("\n");
    HDputs("Testing reading data created on Linux");
    h5_fixname(FILENAME[1], H5P_DEFAULT, filename, sizeof(filename));
    nerrors += check_file(filename);

    HDputs("\n");
    HDputs("Testing reading data created on Solaris");
    h5_fixname(FILENAME[2], H5P_DEFAULT, filename, sizeof(filename));
    nerrors += check_file(filename);

    if(nerrors) {
        printf("***** %d FAILURE%s! *****\n", nerrors, 1 == nerrors ? "" : "S");
        return EXIT_FAILURE;
    } /* end if */

    printf("All data type tests passed.\n");
    return EXIT_SUCCESS;
} /* end main() */

