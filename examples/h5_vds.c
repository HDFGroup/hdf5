/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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

  This example illustrates the concept of virtual dataset.
  The program  creates three 1-dim source datasets and writes
  data to them. Then it creates a 2-dim virtual dataset and
  maps the first three rows of the virtual dataset to the data 
  in the source datasets. Elements of a row are mapped to all 
  elements of the corresponding source dataset.
  The fourth row is not mapped and will be filled with the fill 
  values when virtual dataset is read back. 
   
  The program closes all datasets, and then reopens the virtual
  dataset, and finds and prints its creation properties.
  Then it reads the values. 

  This file is intended for use with HDF5 Library version 1.10

 ************************************************************/
/* EIP Add link to the picture */

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE         "vds.h5"
#define DATASET      "VDS"
#define VDSDIM1         6 
#define VDSDIM0         4 
#define DIM0            6 
#define RANK1           1
#define RANK2           2

const char *SRC_FILE[] = {
    "a.h5",
    "b.h5",
    "c.h5"
};

const char *SRC_DATASET[] = {
    "A",
    "B",
    "C"
};

int
main (void)
{
    hid_t        file, space, src_space, vspace, dset; /* Handles */ 
    hid_t        dcpl;
    herr_t       status;
    hsize_t      vdsdims[2] = {VDSDIM0, VDSDIM1},      /* Virtual datasets dimension */
                 dims[1] = {DIM0},                     /* Source datasets dimensions */
                 start[2],                             /* Hyperslab parameters */
                 stride[2],
                 count[2],
                 block[2];
    hsize_t      start_out[2],
                 stride_out[2],
                 count_out[2],
                 block_out[2];
    int          wdata[DIM0],                /* Write buffer for source dataset */
                 rdata[VDSDIM0][VDSDIM1],    /* Read buffer for virtual dataset */
                 i, j, k, l;  
    int          fill_value = -1;            /* Fill value for VDS */
    H5D_layout_t layout;                     /* Storage layout */
    size_t       num_map;                    /* Number of mappings */
    ssize_t      len;                        /* Length of the string; also a return value */
    char         *filename;                  
    char         *dsetname;
    hsize_t      nblocks;
    hsize_t      *buf;                       /* Buffer to hold hyperslab coordinates */

    /*
     * Create source files and datasets. This step is optional.
     */
    for (i=0; i < 3; i++) {
        /*
         * Initialize data for i-th source dataset.
         */
        for (j = 0; j < DIM0; j++) wdata[j] = i+1;
        
        /*
         * Create the source files and  datasets. Write data to each dataset and 
         * close all resources.
         */

        file = H5Fcreate (SRC_FILE[i], H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
        space = H5Screate_simple (RANK1, dims, NULL);
        dset = H5Dcreate2 (file, SRC_DATASET[i], H5T_NATIVE_INT, space, H5P_DEFAULT,
                    H5P_DEFAULT, H5P_DEFAULT);
        status = H5Dwrite (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                    wdata);
        status = H5Sclose (space);
        status = H5Dclose (dset);
        status = H5Fclose (file);
    }

    /* Create file in which virtual dataset will be stored. */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create VDS dataspace.  */
    space = H5Screate_simple (RANK2, vdsdims, NULL);

    /* Set VDS creation property. */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
    status = H5Pset_fill_value (dcpl, H5T_NATIVE_INT, &fill_value);
     
    /* Initialize hyperslab values. */
    start[0] = 0;
    start[1] = 0;
    count[0] = 1;
    count[1] = 1;
    block[0] = 1;
    block[1] = VDSDIM1;

    /* 
     * Build the mappings.
     * Selections in the source datasets are H5S_ALL.
     * In the virtual dataset we select the first, the second and the third rows 
     * and map each row to the data in the corresponding source dataset. 
     */
    src_space = H5Screate_simple (RANK1, dims, NULL);
    for (i = 0; i < 3; i++) {
        start[0] = (hsize_t)i;
        /* Select i-th row in the virtual dataset; selection in the source datasets is the same. */
        status = H5Sselect_hyperslab (space, H5S_SELECT_SET, start, NULL, count, block);
        status = H5Pset_virtual (dcpl, space, SRC_FILE[i], SRC_DATASET[i], src_space);
    }

    /* Create a virtual dataset. */
    dset = H5Dcreate2 (file, DATASET, H5T_NATIVE_INT, space, H5P_DEFAULT,
                dcpl, H5P_DEFAULT);
    status = H5Sclose (space);
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

        /* Make sure that this is a hyperslab selection and then print information. */
        if (H5Sget_select_type(vspace) == H5S_SEL_HYPERSLABS) { 
            nblocks = H5Sget_select_hyper_nblocks (vspace);
            buf = (hsize_t *)malloc(sizeof(hsize_t)*2*RANK2*nblocks);
            status = H5Sget_select_hyper_blocklist (vspace, (hsize_t)0, nblocks, buf);
            for (l=0; l<nblocks; l++) {
                printf("(");
                for (k=0; k<RANK2-1; k++) 
                    printf("%d,", (int)buf[k]);
                printf("%d ) - (", (int)buf[k]);
                for (k=0; k<RANK2-1; k++) 
                    printf("%d,", (int)buf[RANK2+k]);
                printf("%d)\n", (int)buf[RANK2+k]);
            }
        /* We also can use new APIs to get start, stride, count and block */
            if (H5Sis_regular_hyperslab(vspace)) {
                status = H5Sget_regular_hyperslab (vspace, start_out, stride_out, count_out, block_out);
                printf("         start  = [%llu, %llu] \n", (unsigned long long)start_out[0], (unsigned long long)start_out[1]);
                printf("         stride = [%llu, %llu] \n", (unsigned long long)stride_out[0], (unsigned long long)stride_out[1]);
                printf("         count  = [%llu, %llu] \n", (unsigned long long)count_out[0], (unsigned long long)count_out[1]);
                printf("         block  = [%llu, %llu] \n", (unsigned long long)block_out[0], (unsigned long long)block_out[1]);
            }
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

        /* Make sure it is ALL selection and then print the coordinates. */
        if(H5Sget_select_type(src_space) == H5S_SEL_ALL) {
                printf("(0) - (%d) \n", DIM0-1);
        }
        H5Sclose(vspace);
        H5Sclose(src_space);
        free(filename);
        free(dsetname);
        free(buf);
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
    for (i=0; i<VDSDIM0; i++) {
        printf (" [");
        for (j=0; j<VDSDIM1; j++)
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

