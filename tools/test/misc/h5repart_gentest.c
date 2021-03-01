/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Raymond Lu
 *              June 1, 2005
 *
 * Purpose:     Generate a family file of 1024 bytes for each member
 *              for h5repart test.
 */
#include "hdf5.h"
#include "H5private.h"

#define FAMILY_NUMBER 4
#define FAMILY_SIZE   1024
#define FILENAME      "family_file%05d.h5"

int **buf      = NULL;
int * buf_data = NULL;

int
main(void)
{
    hid_t   file = (-1), fapl, space = (-1), dset = (-1);
    char    dname[] = "dataset";
    int     i, j;
    hsize_t dims[2] = {FAMILY_NUMBER, FAMILY_SIZE};

    /* Set up data array */
    if (NULL == (buf_data = (int *)HDcalloc(FAMILY_NUMBER * FAMILY_SIZE, sizeof(int)))) {
        HDperror("HDcalloc");
        HDexit(EXIT_FAILURE);
    }
    if (NULL == (buf = (int **)HDcalloc(FAMILY_NUMBER, sizeof(buf_data)))) {
        HDperror("HDcalloc");
        HDexit(EXIT_FAILURE);
    }
    for (i = 0; i < FAMILY_NUMBER; i++)
        buf[i] = buf_data + (i * FAMILY_SIZE);

    /* Set property list and file name for FAMILY driver */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
        HDperror("H5Pcreate");
        HDexit(EXIT_FAILURE);
    }

    if (H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT) < 0) {
        HDperror("H5Pset_fapl_family");
        HDexit(EXIT_FAILURE);
    }

    if ((file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) {
        HDperror("H5Fcreate");
        HDexit(EXIT_FAILURE);
    }

    /* Create and write dataset */
    if ((space = H5Screate_simple(2, dims, NULL)) < 0) {
        HDperror("H5Screate_simple");
        HDexit(EXIT_FAILURE);
    }

    if ((dset = H5Dcreate2(file, dname, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        HDperror("H5Dcreate2");
        HDexit(EXIT_FAILURE);
    }

    for (i = 0; i < FAMILY_NUMBER; i++)
        for (j = 0; j < FAMILY_SIZE; j++)
            buf[i][j] = i * 10000 + j;

    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_data) < 0) {
        HDperror("H5Dwrite");
        HDexit(EXIT_FAILURE);
    }

    if (H5Sclose(space) < 0) {
        HDperror("H5Sclose");
        HDexit(EXIT_FAILURE);
    }

    if (H5Dclose(dset) < 0) {
        HDperror("H5Dclose");
        HDexit(EXIT_FAILURE);
    }

    if (H5Pclose(fapl) < 0) {
        HDperror("H5Pclose");
        HDexit(EXIT_FAILURE);
    }

    if (H5Fclose(file) < 0) {
        HDperror("H5Fclose");
        HDexit(EXIT_FAILURE);
    }

    HDfree(buf);
    HDfree(buf_data);

    HDputs(" PASSED");
    HDfflush(stdout);

    return EXIT_SUCCESS;
}
