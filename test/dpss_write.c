/*
 * Copyright © 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Saurabh Bagchi (bagchi@uiuc.edu)
 *              Friday, August 6, 1999.
 *
 * Modifications: Thomas Radke (tradke@aei-potsdam.mpg.de)
 *                Modified to work with DPSS Virtual File Driver.
 */

/* Test the following functionality of the DPSS driver. 
   1. Open a remote file for write.
   2. Create a new dataset within the file.
   3. Create a local memory buffer to hold the data.
   4. Write the local data to the remote dataset.
*/
#include <h5test.h>

#ifndef H5_HAVE_DPSS
int main(void)
{
    printf("Test skipped because DPSS driver not available\n");
    return 0;
}
#else

#define DATASETNAME "IntArray"
#define NX     100                      /* dataset dimensions */
#define NY     100
#define RANK   2

int main (int argc, char **argv)
{

  hid_t         fapl =-1, file;
  hid_t         dataspace, datatype, dataset;
  hsize_t       dimsf[2];
  
  herr_t        status = 0;
  int           data[NX][NY];          /* data to write */
  int           i, j;
  
  if (argc != 2) {
     fprintf (stderr, "Usage: %s <URL>\n", argv [0]);
     exit (0);
  }

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
  
  /* Create access property list and set the driver to DPSS */
  fapl = H5Pcreate (H5P_FILE_ACCESS);
  if (fapl < 0) {
    printf (" H5Pcreate failed. \n");
    return -1;
  }
 
  status = H5Pset_fapl_dpss (fapl);
  if (status < 0) {
    printf ("H5Pset_fapl_dpss failed. \n");
    return -1;
  }
  
  /*
   * Create a new file using H5F_ACC_TRUNC access,
   * default file creation properties, and DPSS file
   * access properties.
   */
  file = H5Fcreate(argv [1], H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
  if (file < 0) {
    printf ("Failed to create file instance '%s'\n", argv [1]);
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
#endif
  
