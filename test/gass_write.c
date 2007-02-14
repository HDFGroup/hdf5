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
 * Programmer:  Saurabh Bagchi (bagchi@uiuc.edu)
 *              Friday, August 6, 1999.
 *
 * Modifications: Saurabh Bagchi (Aug 17, 1999)
 *                Modified to work with VFL (HDF51.3).
 */

/* Test the following functionality of the GASS driver.
   1. Open a remote file for write.
   2. Create a new dataset within the file.
   3. Create a local memory buffer to hold the data.
   4. Write the local data to the remote dataset.
*/
#include "h5test.h"

#ifndef H5_HAVE_GASS
int main(void)
{
    printf("Test skipped because GASS driver not available\n");
    return 0;
}
#else

#ifdef hide

/*#define URL    "ftp://gass:gass12@which/tmp/gass/junk.w"*/
#define URL    "http://paz.ncsa.uiuc.edu:8080/test/put/test/b.h5"

#define DATASETNAME "IntArray"
#define NX     5                      /* dataset dimensions */
#define NY     6
#define RANK   2

int main (void)
{

  hid_t         fapl =-1, file;
  hid_t         dataspace, datatype, dataset;
  hsize_t       dimsf[2];

  herr_t        status = 0;
  int           data[NX][NY];          /* data to write */
  int           i, j;
  GASS_Info     ginf;

  /*
   * Data  and output buffer initialization.
   */
  for (j = 0; j < NX; j++) {
    for (i = 0; i < NY; i++)
      data[j][i] = i + j;
  }
  /*
   * 0 1 2 3 4 5
   * 1 2 3 4 5 6
   * 2 3 4 5 6 7
   * 3 4 5 6 7 8
   * 4 5 6 7 8 9
   */

  /* Create access property list and set the driver to GASS */
  fapl = H5Pcreate (H5P_FILE_ACCESS);
  if (fapl < 0) {
    printf (" H5Pcreate failed. \n");
    return -1;
  }

  ginf.block_size = 0;
  ginf.max_length =0;
  /* ginf = GASS_INFO_NULL;*/

  status = H5Pset_fapl_gass (fapl, ginf);
  if (status < 0) {
    printf ("H5Pset_fapl_gass failed. \n");
    return -1;
  }

  /*
   * Create a new file using H5F_ACC_TRUNC access,
   * default file creation properties, and gass file
   * access properties.
   */
  /*
  // file = H5Fcreate(URL, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
  // Works. Truncates existing files.
  // file = H5Fcreate(URL, H5F_ACC_EXCL, H5P_DEFAULT, fapl);
  // Works. Croaks if existing file, else creates.
  // Any other flag has no effect as long as one and exactly one of TRUNC/
  // EXCL is there */
  /* printf ("I'm here just before H5Fcreate. \n");*/
  file = H5Fcreate(URL, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
  if (file < 0) {
    printf ("H5Fcreate failed. \n");
    return -1;
  }

  /*
     * Describe the size of the array and create the data space for fixed
     * size dataset.
     */
    dimsf[0] = NX;
    dimsf[1] = NY;
    dataspace = H5Screate_simple(RANK, dimsf, NULL);
    if (dataspace < 0) {
      printf ("H5Screate failed. \n");
      return -1;
    }

    /*
     * Define datatype for the data in the file.
     * We will store little endian INT numbers.
     */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    if (datatype < 0) {
      printf ("H5Tcopy failed. \n");
      return -1;
    }

    status = H5Tset_order(datatype, H5T_ORDER_LE);
    if (status < 0) {
      printf ("H5Tset_order failed. \n");
      return -1;
    }

    /*
     * Create a new dataset within the file using defined dataspace and
     * datatype and default dataset creation properties.
     */
    dataset = H5Dcreate(file, DATASETNAME, datatype, dataspace,
			H5P_DEFAULT);
    if (dataset < 0) {
      printf ("H5Dcreate failed. \n");
      return -1;
    }

    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		      H5P_DEFAULT, data);
    if (status < 0) {
      printf ("H5Dwrite failed. \n");
      return -1;
    }

    /*
     * Close/release resources.
     */
    H5Sclose(dataspace);
    H5Tclose(datatype);
    H5Dclose(dataset);
    H5Fclose(file);
    H5Pclose(fapl);

    return 0;
}
#else
int main(void)
{
    printf("Test skipped because writing depends on web server!\n");
    return 0;
}
#endif
#endif
