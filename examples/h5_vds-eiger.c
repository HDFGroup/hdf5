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
  Eiger use case. Every 5 frames 10x10 are in the source 
  dataset "/A" in file with the name f-<#>.h5
  This file is intended for use with HDF5 Library version 1.10

 ************************************************************/
/* EIP Add link to the picture */

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE         "vds-eiger.h5"
#define DATASET      "VDS-Eiger"
#define VDSDIM0       5 
#define VDSDIM1       10 
#define VDSDIM2       10 
#define DIM0          5 
#define DIM1          10 
#define DIM2          10 
#define RANK          3 

int
main (void)
{
    hid_t        file, src_space, vspace,
                 dset;                       /* Handles */
    hid_t        dcpl;
    herr_t       status;
    hsize_t      vdsdims[3] = {VDSDIM0, VDSDIM1, VDSDIM2},
                 vdsdims_max[3] = {H5S_UNLIMITED, VDSDIM1, VDSDIM1},
                 dims[3] = {DIM0, DIM1, DIM2},
                 start[3],                   /* Hyperslab parameters */
                 stride[3],
                 count[3],
                 block[3];
    hsize_t      start_out[3],               /* Hyperslab parameter out */
                 stride_out[3],
                 count_out[3],
                 block_out[3];
    int          i;
    H5D_layout_t layout;                     /* Storage layout */
    size_t       num_map;                    /* Number of mappings */
    ssize_t      len;                        /* Length of the string; also a return value */
    char         *filename;
    char         *dsetname;


    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create VDS dataspace.  */
    vspace = H5Screate_simple (RANK, vdsdims, vdsdims_max);

    /* Create dataspaces for the source dataset. */
    src_space = H5Screate_simple (RANK, dims, NULL);

    /* Create VDS creation property */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
     
    /* Initialize hyperslab values */

    start[0] = 0;
    start[1] = 0;
    start[2] = 0;
    stride[0] = DIM0;
    stride[1] = 1;
    stride[2] = 1;
    count[0] = H5S_UNLIMITED;
    count[1] = 1;
    count[2] = 1;
    block[0] = DIM0;
    block[1] = DIM1;
    block[2] = DIM2;

   /* 
    * Build the mappings 
    *
    */
      status = H5Sselect_hyperslab (vspace, H5S_SELECT_SET, start, stride, count, block);
      status = H5Pset_virtual (dcpl, vspace, "f-%b.h5", "/A", src_space);
   


   /* Create a virtual dataset */
      dset = H5Dcreate2 (file, DATASET, H5T_NATIVE_INT, vspace, H5P_DEFAULT,
                  dcpl, H5P_DEFAULT);
      status = H5Sclose (vspace);
      status = H5Sclose (src_space);
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
        printf(" Wrong layout found \n");

     /*
      * Find number of mappings.
      */
     status = H5Pget_virtual_count (dcpl, &num_map);
     printf(" Number of mappings is %d\n", (int)num_map);

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
          printf("         Selection in the source dataset ");
          src_space = H5Pget_virtual_srcspace (dcpl, (size_t)i);
          if(H5Sget_select_type(src_space) == H5S_SEL_ALL) {
                  printf("H5S_ALL \n");
          }
/* EIP read data back */
          H5Sclose(vspace);
          H5Sclose(src_space);
          free(filename);
          free(dsetname);
      }

    /*
     * Close and release resources.
     */
    status = H5Pclose (dcpl);
    status = H5Dclose (dset);
    status = H5Fclose (file);

    return 0;
}

