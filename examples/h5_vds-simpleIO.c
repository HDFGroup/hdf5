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

  This example illustrates the concept of virtual dataset I/O
  The program  creates 2-dim source dataset and writes
  data to it. Then it creates 2-dim virtual dataset that has
  the same dimension sizes and maps the all elements of the 
  virtual dataset to all elements of the source dataset.
  Then VDS is read back.

  This file is intended for use with HDF5 Library version 1.10

 ************************************************************/
/* EIP Add link to the picture */

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE         "vds-simpleIO.h5"
#define DATASET      "VDS"
#define DIM1            6
#define DIM0            4 
#define RANK            2

#define SRC_FILE      "a.h5"
#define SRC_DATASET    "/A"


int
main (void)
{
    hid_t        file, space, src_space, vspace, dset; /* Handles */ 
    hid_t        dcpl;
    herr_t       status;
    hsize_t      vdsdims[2] = {DIM0, DIM1},     /* Virtual dataset dimension */
                 dims[2] = {DIM0, DIM1};        /* Source dataset dimensions */
    int          wdata[DIM0][DIM1],             /* Write buffer for source dataset */
                 rdata[DIM0][DIM1],             /* Read buffer for virtual dataset */
                 i, j;  
    H5D_layout_t layout;                        /* Storage layout */
    size_t       num_map;                       /* Number of mappings */
    ssize_t      len;                           /* Length of the string; also a return value */
    char         *filename;                  
    char         *dsetname;
    /*
     * Initialize data.
     */
        for (i = 0; i < DIM0; i++) 
            for (j = 0; j < DIM1; j++) wdata[i][j] = i+1;
         
     /*
      * Create the source file and the dataset. Write data to the source dataset 
      * and close all resources.
      */

     file = H5Fcreate (SRC_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
     space = H5Screate_simple (RANK, dims, NULL);
     dset = H5Dcreate2 (file, SRC_DATASET, H5T_NATIVE_INT, space, H5P_DEFAULT,
                 H5P_DEFAULT, H5P_DEFAULT);
     status = H5Dwrite (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 wdata[0]);
     status = H5Sclose (space);
     status = H5Dclose (dset);
     status = H5Fclose (file);

    /* Create file in which virtual dataset will be stored. */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create VDS dataspace.  */
    vspace = H5Screate_simple (RANK, vdsdims, NULL);

    /* Set VDS creation property. */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
     
    /* 
     * Build the mappings.
     * Selections in the source datasets are H5S_ALL.
     * In the virtual dataset we select the first, the second and the third rows 
     * and map each row to the data in the corresponding source dataset. 
     */
    src_space = H5Screate_simple (RANK, dims, NULL);
    status = H5Pset_virtual (dcpl, vspace, SRC_FILE, SRC_DATASET, src_space);

    /* Create a virtual dataset. */
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
     * Open the file and virtual dataset.
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
      * Find the number of mappings.
      */
     status = H5Pget_virtual_count (dcpl, &num_map);
     printf(" Number of mappings is %lu\n", (unsigned long)num_map);

     /* 
      * Get mapping parameters for each mapping.
      */
    for (i = 0; i < (int)num_map; i++) {   
        printf(" Mapping %d \n", i);
        printf("         Selection in the virtual dataset ");
        /* Get selection in the virttual  dataset */
        vspace = H5Pget_virtual_vspace (dcpl, (size_t)i);

        /* Make sure it is ALL selection and then print selection. */
        if(H5Sget_select_type(vspace) == H5S_SEL_ALL) {
                printf("Selection is H5S_ALL \n");
        }
        /* Get source file name. */
        len = H5Pget_virtual_filename (dcpl, (size_t)i, NULL, 0);
        filename = (char *)malloc((size_t)len*sizeof(char)+1);
        H5Pget_virtual_filename (dcpl, (size_t)i, filename, len+1);
        printf("         Source filename %s\n", filename);

        /* Get source dataset name. */
        len = H5Pget_virtual_dsetname (dcpl, (size_t)i, NULL, 0);
        dsetname = (char *)malloc((size_t)len*sizeof(char)+1);
        H5Pget_virtual_dsetname (dcpl, (size_t)i, dsetname, len+1);
        printf("         Source dataset name %s\n", dsetname);

        /* Get selection in the source dataset. */
        printf("         Selection in the source dataset ");
        src_space = H5Pget_virtual_srcspace (dcpl, (size_t)i);

        /* Make sure it is ALL selection and then print selection. */
        if(H5Sget_select_type(src_space) == H5S_SEL_ALL) {
                printf("Selection is H5S_ALL \n");
        }
        H5Sclose(vspace);
        H5Sclose(src_space);
        free(filename);
        free(dsetname);
    }
    /*
     * Read the data using the default properties.
     */
    status = H5Dread (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf (" VDS Data:\n");
    for (i=0; i<DIM0; i++) {
        printf (" [");
        for (j=0; j<DIM1; j++)
            printf (" %3d", rdata[i][j]);
        printf ("]\n");
    }
    /*
     * Close and release resources.
     */
    status = H5Pclose (dcpl);
    status = H5Dclose (dset);
    status = H5Fclose (file);

    return 0;
}

