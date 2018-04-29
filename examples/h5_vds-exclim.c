/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/************************************************************

  This example illustrates the concept of the virtual dataset.
  Excalibur use case with k=2 and m=3 and only 3 planes in
  Z-direction (i.e., not unlimited).
  This file is intended for use with HDF5 Library version 1.10

 ************************************************************/
/* EIP Add link to the picture */

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE         "vds-exclim.h5"
#define DATASET      "VDS-Excaliburlim"
#define VDSDIM0         3 
#define VDSDIM1         15 
#define VDSDIM2         6 
#define KDIM0           3 
#define KDIM1           2 
#define KDIM2           6 
#define NDIM0           3 
#define NDIM1           3 
#define NDIM2           6 
#define RANK            3 

const char *SRC_FILE[] = {
    "a.h5",
    "b.h5",
    "c.h5",
    "d.h5",
    "e.h5",
    "f.h5"
};

const char *SRC_DATASET[] = {
    "A",
    "B",
    "C",
    "D",
    "E",
    "F"
};

int
main (void)
{
    hid_t        file, space, ksrc_space, nsrc_space, vspace,
                 src_space,
                 dset;                       /* Handles */
    hid_t        dcpl;
    herr_t       status;
    hsize_t      vdsdims[3] = {VDSDIM0, VDSDIM1, VDSDIM2},
                 kdims[3] = {KDIM0, KDIM1, KDIM2},
                 ndims[3] = {NDIM0, NDIM1, NDIM2},
                 start[3],                   /* Hyperslab parameters */
                 stride[3],
                 count[3],
                 block[3];
    hsize_t      start_out[3],
                 stride_out[3],
                 count_out[3],
                 block_out[3];
    int          k = 2;
    int          n = 3;
    int          i;
    H5D_layout_t layout;                     /* Storage layout */
    size_t       num_map;                    /* Number of mappings */
    ssize_t      len;                        /* Length of the string; also a return value */
    char         *filename;
    char         *dsetname;


    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create VDS dataspace.  */
    space = H5Screate_simple (RANK, vdsdims, NULL);
    /* Create dataspaces for A, C, and E datasets. */
    ksrc_space = H5Screate_simple (RANK, kdims, NULL);
    /* Create dataspaces for B, D, and F datasets. */
    nsrc_space = H5Screate_simple (RANK, ndims, NULL);

    /* Create VDS creation property */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
     
    /* Initialize hyperslab values */

    start[0] = 0;
    start[1] = 0;
    start[2] = 0;
    count[0] = VDSDIM0;
    count[1] = 1;
    count[2] = 1;
    block[0] = 1;
    block[1] = k;
    block[2] = VDSDIM2;

   /* 
    * Build the mappings for A, C and E source datasets.
    *
    */
   status = H5Sselect_hyperslab (ksrc_space, H5S_SELECT_SET, start, NULL, count, block);
   for (i = 0; i < 3; i++) {
      start[1] = (hsize_t)((k+n)*i);
      status = H5Sselect_hyperslab (space, H5S_SELECT_SET, start, NULL, count, block);
      status = H5Pset_virtual (dcpl, space, SRC_FILE[2*i], SRC_DATASET[2*i], ksrc_space);
   }

   /* Reinitialize start[0] and block[1] */
   start[0] = 0;
   block[1] = n;
   /* 
    * Build the mappings for B, D and F source datasets.
    *
    */
   status = H5Sselect_hyperslab (nsrc_space, H5S_SELECT_SET, start, NULL, count, block);
   for (i = 0; i < 3; i++) {
      start[1] = (hsize_t)(k+(k+n)*i);
      status = H5Sselect_hyperslab (space, H5S_SELECT_SET, start, NULL, count, block);
      status = H5Pset_virtual (dcpl, space, SRC_FILE[2*i+1], SRC_DATASET[2*i+1], nsrc_space);
   }

   /* Create a virtual dataset */
      dset = H5Dcreate2 (file, DATASET, H5T_NATIVE_INT, space, H5P_DEFAULT,
                  dcpl, H5P_DEFAULT);
      status = H5Sclose (space);
      status = H5Sclose (nsrc_space);
      status = H5Sclose (ksrc_space);
      status = H5Dclose (dset);
      status = H5Fclose (file);    
     

    /*
     * Now we begin the read section of this example.
     */

    /*
     * Open file and dataset using the default properties.
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen2 (file, DATASET, H5P_DEFAULT);

    /*
     * Get creation property list and mapping properties.
     */
    dcpl = H5Dget_create_plist (dset);

    /*
     * Get storage layout.
     */
    layout = H5Pget_layout (dcpl);

    if (H5D_VIRTUAL == layout) 
        printf(" Dataset has a virtual layout \n");
    else
        printf("Wrong layout found \n");

     /*
      * Find number of mappings.
      */
     status = H5Pget_virtual_count (dcpl, &num_map);
     printf(" Number of mappings is %lu\n", (unsigned long)num_map);

     /* 
      * Get mapping parameters for each mapping.
      */
      for (i = 0; i < (int)num_map; i++) {   
      printf(" Mapping %d \n", i);
      printf("         Selection in the virtual dataset \n");
      /* Get selection in the virttual  dataset */
          vspace = H5Pget_virtual_vspace (dcpl, (size_t)i);
          if (H5Sget_select_type(vspace) == H5S_SEL_HYPERSLABS) { 
              if (H5Sis_regular_hyperslab(vspace)) {
                   status = H5Sget_regular_hyperslab (vspace, start_out, stride_out, count_out, block_out);
                   printf("         start  = [%llu, %llu, %llu] \n", (unsigned long long)start_out[0], (unsigned long long)start_out[1], (unsigned long long)start_out[2]);
                   printf("         stride = [%llu, %llu, %llu] \n", (unsigned long long)stride_out[0], (unsigned long long)stride_out[1], (unsigned long long)stride_out[2]);
                   printf("         count  = [%llu, %llu, %llu] \n", (unsigned long long)count_out[0], (unsigned long long)count_out[1], (unsigned long long)count_out[2]);
                   printf("         block  = [%llu, %llu, %llu] \n", (unsigned long long)block_out[0], (unsigned long long)block_out[1], (unsigned long long)block_out[2]);
               }
          }
      /* Get source file name */
          len = H5Pget_virtual_filename (dcpl, (size_t)i, NULL, 0);
          filename = (char *)malloc((size_t)len*sizeof(char)+1);
          H5Pget_virtual_filename (dcpl, (size_t)i, filename, len+1);
          printf("         Source filename %s\n", filename);

      /* Get source dataset name */
          len = H5Pget_virtual_dsetname (dcpl, (size_t)i, NULL, 0);
          dsetname = (char *)malloc((size_t)len*sizeof(char)+1);
          H5Pget_virtual_dsetname (dcpl, (size_t)i, dsetname, len+1);
          printf("         Source dataset name %s\n", dsetname);

      /* Get selection in the source dataset */
          printf("         Selection in the source dataset \n");
          src_space = H5Pget_virtual_srcspace (dcpl, (size_t)i);
          if(H5Sget_select_type(src_space) == H5S_SEL_HYPERSLABS) {
              if (H5Sis_regular_hyperslab(vspace)) {
                   status = H5Sget_regular_hyperslab (vspace, start_out, stride_out, count_out, block_out);
                   printf("         start  = [%d, %d, %d] \n", (int)start_out[0], (int)start_out[1], (int)start_out[2]);
                   printf("         stride = [%d, %d, %d] \n", (int)stride_out[0], (int)stride_out[1], (int)stride_out[2]);
                   printf("         count  = [%d, %d, %d] \n", (int)count_out[0], (int)count_out[1], (int)count_out[2]);
                   printf("         block  = [%d, %d, %d] \n", (int)block_out[0], (int)block_out[1], (int)block_out[2]);
               }
          }
          H5Sclose(vspace);
          H5Sclose(src_space);
          free(filename);
          free(dsetname);
      }
/* EIP read data back */

    /*
     * Close and release resources.
     */
    status = H5Pclose (dcpl);
    status = H5Dclose (dset);
    status = H5Fclose (file);

    return 0;
}

