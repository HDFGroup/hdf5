/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Raymond Lu <slu@ncsa.uiuc.edu>
 *              Thursday, March 23, 2006
 *
 * Purpose:     Check if floating-point data created on OpenVMS (VAX type), Solaris, 
 *              and Linux machines can be read on the machine running this test.
 */

#include "h5test.h"

const char *FILENAME[] = {
    "vms_data",
    "le_data",
    "be_data",
    NULL
};

#define DATASETNAME "Array"
#define NX 5           /* output buffer dimensions */
#define NY 6
#define RANK         2

static int read_data(char *fname)
{
    char        pathname[1024];
    char       *srcdir = getenv("srcdir"); /*where the src code is located*/
    hid_t       file, dataset;         /* handles */
    hid_t       datatype;
    hid_t	dt;
    double      data_in[NX][NY]; /* input buffer */
    double      data_out[NX][NY]; /* output buffer */
    int         i, j, rank;
    herr_t      status;
    unsigned 	nerrors = 0;

    pathname[0] = '\0';
    /* Generate correct name for test file by prepending the source path */
    if(srcdir && ((strlen(srcdir) + strlen(fname) + 1) < sizeof(pathname))) {
        strcpy(pathname, srcdir);
        strcat(pathname, "/");
    }
    strcat(pathname, fname);

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
	for (i = 0; i < NY; i++) {
	    data_in[j][i] = i + j;
	    data_out[j][i] = 0;
        }
    }
    /*
     * 0 1 2 3 4 5
     * 1 2 3 4 5 6
     * 2 3 4 5 6 7
     * 3 4 5 6 7 8
     * 4 5 6 7 8 9
     */

    /*
     * Open the file and the dataset.
     */
    if((file = H5Fopen(pathname, H5F_ACC_RDONLY, H5P_DEFAULT))<0)
        TEST_ERROR;
    if((dataset = H5Dopen(file, DATASETNAME))<0)
        TEST_ERROR;

    /*
     * Get datatype and dataspace handles and then query
     * dataset class, order, size, rank and dimensions.
     */
    if((dt = H5Dget_type(dataset))<0)     /* datatype handle */
        TEST_ERROR;
    if((datatype = H5Tget_native_type(dt, H5T_DIR_DEFAULT))<0)
        TEST_ERROR;

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    if(H5Dread(dataset, datatype, H5S_ALL, H5S_ALL, H5P_DEFAULT, data_out)<0)
        TEST_ERROR;

    /* Check results */
    for (j=0; j<NX; j++) {
        for (i=0; i<NY; i++) {
            if (data_out[j][i] != data_in[j][i]) {
                if (!nerrors++) {
                    H5_FAILED();
                    printf("element [%d][%d] is %g but should have been %g\n",
                           j, i, data_out[j][i], data_in[j][i]);
                }
            }
        }
    }

    /*
     * Close/release resources.
     */
    H5Tclose(dt);
    H5Tclose(datatype);
    H5Dclose(dataset);
    H5Fclose(file);
    
    /* Failure */
    if (nerrors) {
        printf("total of %d errors out of %d elements\n", nerrors, NX*NY);
        return 1;
    }

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}

int main(void)
{
    char        filename[1024];
    unsigned 	nerrors = 0;

    h5_reset();

    TESTING("reading data created on OpenVMS");
    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    nerrors += read_data(filename);

    TESTING("reading data created on Linux");
    h5_fixname(FILENAME[1], H5P_DEFAULT, filename, sizeof filename);
    nerrors += read_data(filename);

    TESTING("reading data created on Solaris");
    h5_fixname(FILENAME[2], H5P_DEFAULT, filename, sizeof filename);
    nerrors += read_data(filename);

    if (nerrors) {
        printf("***** %lu FAILURE%s! *****\n",
               nerrors, 1==nerrors?"":"S");
        HDexit(1);
    }

    printf("All data type tests passed.\n");
    return 0;
}
