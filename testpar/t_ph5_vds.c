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

 ************************************************************
  RAW - This example was modified for testing parallel VDS
        functionality.
 ************************************************************/
/* EIP Add link to the picture */
#include "h5test.h"
#include "testpar.h"

#include <stdio.h>
#include <stdlib.h>

#define FILE         "vds.h5"
#define DATASET      "VDS"
#define VDSDIM1         100
#define VDSDIM0         4 
#define DIM0            100
#define RANK1           1
#define RANK2           2

#define NFILENAME 4
const char *FILENAMES[NFILENAME + 1] = { "a.h5",
                                         "b.h5",
                                         "c.h5",
                                         "vds.h5",
                                         NULL };

const char *SRC_DATASET[] = {
    "A",
    "B",
    "C"
};


int
main (int argc, char **argv)
{
    hid_t 
      memspace = -1,
      dataspace = -1,
      filespace = -1,
      src_space = -1,
      src_dset = -1,
      vds_file = -1, 
      file_id = -1,
      fapl_id = -1, 
      dxpl_id = -1,
      vspace = -1,
      vdsset = -1,
      dcpl = -1;
    herr_t       status;
    hsize_t      vdsdims[2] = {VDSDIM0, VDSDIM1},      /* Virtual datasets dimension */
                 dims[1] = {DIM0},                     /* Source datasets dimensions */
                 start[2],                             /* Hyperslab parameters */
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
    hsize_t      elements;
    hsize_t      offset;
    hsize_t      *buf;                       /* Buffer to hold hyperslab coordinates */

    int group_status = 0, nerrs = 0;
    int mpi_rank;
    int mpi_size;
    int workgroup;
    MPI_Comm group_comm = MPI_COMM_NULL;

    if ( (MPI_Init(&argc, &argv)) != MPI_SUCCESS) {
       HDfprintf(stderr, "FATAL: Unable to initialize MPI\n");
       HDexit(EXIT_FAILURE);
    }

    if ( (MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank)) != MPI_SUCCESS) {
        HDfprintf(stderr, "FATAL: MPI_Comm_rank returned an error\n");
        HDexit(EXIT_FAILURE);
    }

    if ( (MPI_Comm_size(MPI_COMM_WORLD, &mpi_size)) != MPI_SUCCESS) {
        HDfprintf(stderr, "FATAL: MPI_Comm_size returned an error\n");
        HDexit(EXIT_FAILURE);
    }

    workgroup = (mpi_rank < VDSDIM0 ? 0 : 1);
    if ( (MPI_Comm_split(MPI_COMM_WORLD,
			 workgroup,
                         0,
                         &group_comm)) != MPI_SUCCESS) {
        HDfprintf(stderr, "FATAL: MPI_Comm_split returned an error\n");
        HDexit(EXIT_FAILURE);
    }

    H5open();

    if ( mpi_rank == 0 ) {
        HDfprintf(stdout, "========================================\n");
        HDfprintf(stdout, "Parallel VDS functionality tests\n");
        HDfprintf(stdout, "        mpi_size     = %d\n", mpi_size);
        HDfprintf(stdout, "========================================\n");
    }

    /*
     * Create source files and datasets. 
     * we'll assign one MPI rank per file...
     */
    for (i=0; i < 3; i++) {

        if (mpi_rank == i) {
            /*
	     * Initialize data for i-th source dataset.
	     */
            for (j = 0; j < DIM0; j++) wdata[j] = i+1;
        
	    /*
	     * Create the source files and  datasets. Write data to each dataset and 
	     * close all resources.
	     */

	    if ((file_id = H5Fcreate (FILENAMES[i], H5F_ACC_TRUNC,
				      H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
	    if ((src_space = H5Screate_simple (RANK1, dims, NULL)) < 0) TEST_ERROR;
	    if ((src_dset = H5Dcreate2 (file_id, SRC_DATASET[i], H5T_NATIVE_INT,
                                    src_space, H5P_DEFAULT, H5P_DEFAULT,
                                    H5P_DEFAULT)) < 0) TEST_ERROR;
	    if ((status = H5Dwrite (src_dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
                                    H5P_DEFAULT, wdata)) < 0) TEST_ERROR;

	    if ((status  = H5Dclose (src_dset))  < 0) TEST_ERROR;
	    if ((status  = H5Sclose (src_space)) < 0) TEST_ERROR;
	    if ((status  = H5Fclose (file_id)) < 0) TEST_ERROR;
	}
    }

    src_dset = -1;
    src_space = -1;
    file_id = -1;

    /* We allow at most VDSDIM0 mpi ranks to participate
     * in the actual VDS testing, i.e. in the best case we
     * do 1 mpi rank per data slice.  We accomplished this
     * by creating an MPI 'group_comm' communicator which
     * contains at most VDSDIM0 ranks.  All other processes
     * skip the testing part but need participate in the
     * MPI_Allgather to exchange global status.
     */
    if ( mpi_rank < VDSDIM0) {

        if ( (fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0 ) TEST_ERROR;
        if ( (H5Pset_fapl_mpio(fapl_id, group_comm, 
			   MPI_INFO_NULL)) < 0) TEST_ERROR;

        /* Create file in which virtual dataset will be stored. */
        if ( (vds_file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT,
			       fapl_id)) < 0 ) TEST_ERROR;

        if ( (dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0 ) TEST_ERROR;
        if ( (H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE)) < 0 ) TEST_ERROR;

        /* Create VDS dataspace.  */
        if ( (dataspace = H5Screate_simple (RANK2, vdsdims, NULL)) < 0 ) TEST_ERROR;

        /* Set VDS creation property. */
        if ( (dcpl = H5Pcreate (H5P_DATASET_CREATE)) < 0 ) TEST_ERROR;

        if ( (H5Pset_fill_value (dcpl, H5T_NATIVE_INT, &fill_value)) < 0 ) TEST_ERROR;
     
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
        if ((src_space = H5Screate_simple (RANK1, dims, NULL)) < 0 ) TEST_ERROR;

        for (i=0; i < 3; i++) {
            start[0] = (hsize_t)i;
            /* Select i-th row in the virtual dataset; 
    	     * selection in the source datasets is the same.
             */
            if ((status = H5Sselect_hyperslab (dataspace, H5S_SELECT_SET, 
                  	       	   start, NULL, count, block)) < 0) TEST_ERROR;
            if ((status = H5Pset_virtual (dcpl, dataspace, FILENAMES[i],
                               SRC_DATASET[i], src_space)) < 0) TEST_ERROR;
        }

        /* Create a virtual dataset. */
        if ((vdsset = H5Dcreate2 (vds_file, DATASET, H5T_NATIVE_INT, 
                               dataspace, H5P_DEFAULT,
                               dcpl, H5P_DEFAULT)) < 0) TEST_ERROR;
        /* Now close up the file and close the various 
         * other HDF5 descriptors */
        if ((status = H5Dclose (vdsset)) < 0) TEST_ERROR;
        vdsset = -1;
        if ((status = H5Sclose (dataspace)) < 0) TEST_ERROR;
        dataspace = -1;
        if ((status = H5Sclose (src_space)) < 0) TEST_ERROR;
        src_space = -1;
        if ((status = H5Fclose (vds_file)) < 0) TEST_ERROR;
        vds_file = -1; 

        /*
         * Now we begin the read section of this example.
         */

        if ( (fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0 ) {
            HDfprintf(stderr, "H5Pcreate - returned an error\n");
            HDexit(EXIT_FAILURE);
        }
        if ( (status = H5Pset_fapl_mpio(fapl_id, group_comm,
       	       	       MPI_INFO_NULL)) < 0 ) {
            HDfprintf(stderr, "H5Pset_fapl_mpio - returned an error\n");
            HDexit(EXIT_FAILURE);
        }
        /*
         * Open the file and virtual dataset.
         */
        if ((vds_file = H5Fopen (FILE, H5F_ACC_RDONLY, fapl_id))
            < 0) {
            HDfprintf(stderr, "H5Fopen - returned an error\n");
            HDexit(EXIT_FAILURE);
        }

        if ((vdsset = H5Dopen2 (vds_file, DATASET, H5P_DEFAULT))
            < 0) {
            HDfprintf(stderr, "H5Fopen2 - returned an error\n");
            HDexit(EXIT_FAILURE);
        }

        /*
         * Get creation property list and mapping properties.
         */
        if ((dcpl = H5Dget_create_plist (vdsset)) < 0) {
            HDfprintf(stderr, "H5Dget_create_plist - returned an error\n");
            HDexit(EXIT_FAILURE);
        }

        /*
         * Get storage layout.
         */
        if ((layout = H5Pget_layout (dcpl)) < 0) {
            HDfprintf(stderr, "H5Pget_layout - returned an error\n");
            HDexit(EXIT_FAILURE);
        }

	if (mpi_rank == 0) {
            if (layout == H5D_VIRTUAL ) 
                HDfprintf(stdout, " Dataset has a virtual layout\n");
            else
                HDfprintf(stdout, " Wrong layout found \n");

            /*
             * Find the number of mappings.
             */
            if ((status = H5Pget_virtual_count (dcpl, &num_map))
                < 0) {
                HDfprintf(stderr, "H5Pget_layout - returned an error\n");
                HDexit(EXIT_FAILURE);
            }

            HDfprintf(stdout, " Number of mappings is %lu\n",
                      (unsigned long)num_map);

            /* 
             * Get mapping parameters for each mapping.
             */
            nerrs = 0;
            for (i = 0; i < (int)num_map; i++) {   
                HDfprintf(stdout, " Mapping %d \n", i);
                HDfprintf(stdout, "         Selection in the virtual dataset ");
                /* Get selection in the virtual dataset */
                if ((vspace = H5Pget_virtual_vspace (dcpl, (size_t)i))
                    < 0) {
                    HDfprintf(stderr, "H5Pget_virtual_vspace - returned an error\n");
                    HDexit(EXIT_FAILURE);
                }
                /* Make sure that this is a hyperslab selection
                 * and then print information.
                 */
                if (H5Sget_select_type(vspace) == H5S_SEL_HYPERSLABS) { 
                    if ((nblocks = H5Sget_select_hyper_nblocks (vspace)) < 0) {
                        HDfprintf(stderr, "H5Pget_virtual_vspace - returned an error\n");
                        HDexit(EXIT_FAILURE);
                    }

                    if ((buf = (hsize_t *)HDmalloc(sizeof(hsize_t)*2*RANK2*nblocks))
                        == NULL) {
                        HDfprintf(stderr, "HDmalloc - returned a NULL\n");
                        HDexit(EXIT_FAILURE);
                    }

                    if ((H5Sget_select_hyper_blocklist (vspace, (hsize_t)0,
                        nblocks, buf)) < 0) {
                        HDfprintf(stderr, "H5Sget_select_hyper_blocklist - returned an error\n");
                        HDexit(EXIT_FAILURE);
                    }
                    for (l=0; l<nblocks; l++) {
                        HDfprintf(stdout, "(");
                        for (k=0; k<RANK2-1; k++) 
                            HDfprintf(stdout ,"%d,", (int)buf[k]);
                        HDfprintf(stdout, "%d ) - (", (int)buf[k]);
                        for (k=0; k<RANK2-1; k++) 
                            HDfprintf(stdout, "%d,", (int)buf[RANK2+k]);
                        HDfprintf(stdout, "%d)\n", (int)buf[RANK2+k]);
                    }

                    /* We also can use new APIs to get the 'start', 
                     * 'stride', 'count' and 'block' values */
                     if (H5Sis_regular_hyperslab(vspace)) {
                         if ((H5Sget_regular_hyperslab (vspace, start_out,
                                                     stride_out, count_out,
                                                     block_out)) < 0) {
                             HDfprintf(stderr, 
                             "H5Sget_regular_hyperslab - returned an error\n");
                             HDexit(EXIT_FAILURE);
                         }
                         printf("         start  = [%llu, %llu] \n", 
                               (unsigned long long)start_out[0],
                               (unsigned long long)start_out[1]);
                         printf("         stride = [%llu, %llu] \n",
                               (unsigned long long)stride_out[0],
                               (unsigned long long)stride_out[1]);
                         printf("         count  = [%llu, %llu] \n",
                               (unsigned long long)count_out[0],
                               (unsigned long long)count_out[1]);
                         printf("         block  = [%llu, %llu] \n",
                               (unsigned long long)block_out[0],
                               (unsigned long long)block_out[1]);
     	             }
		}
                /* Get source file name. */
                if ((len = H5Pget_virtual_filename (dcpl, (size_t)i, NULL, 0)) < 0) {
                    HDfprintf(stderr, "H5Pget_virtual_filename - returned an error\n");
                    HDexit(EXIT_FAILURE);
                }
                if ((filename = (char *)HDmalloc((size_t)len*sizeof(char)+1)) == NULL) {
                    HDfprintf(stderr, "HDmalloc - returned NULL\n");
                    HDexit(EXIT_FAILURE);
                }

                if ((H5Pget_virtual_filename (dcpl, (size_t)i, filename, len+1)) < 0) {
                    HDfprintf(stderr, "H5Pget_virtual_filename - returned an error\n");
                    HDexit(EXIT_FAILURE);
                }
                HDfprintf(stdout, "         Source filename %s\n", filename);
    
                /* Get source dataset name. */
                if ((len = H5Pget_virtual_dsetname (dcpl, (size_t)i, NULL, 0)) < 0) {
                    HDfprintf(stderr, "H5Pget_virtual_dsetname - returned an error\n");
                    HDexit(EXIT_FAILURE);
                }
                if ((dsetname = (char *)HDmalloc((size_t)len*sizeof(char)+1)) == NULL) {
                    HDfprintf(stderr, "HDmalloc - returned NULL\n");
                    HDexit(EXIT_FAILURE);
                }
                if ((H5Pget_virtual_dsetname (dcpl, (size_t)i, dsetname, len+1)) < 0) {
                    HDfprintf(stderr, "H5Pget_virtual_dsetname - returned an error\n");
                    HDexit(EXIT_FAILURE);
                }
                HDfprintf(stdout,"         Source dataset name %s\n", dsetname);
    
                /* Get selection in the source dataset. */
                HDprintf("         Selection in the source dataset ");
                if ((src_space = H5Pget_virtual_srcspace (dcpl, (size_t)i)) < 0) {
                    HDfprintf(stderr, "H5Pget_virtual_filename - returned an error\n");
                    HDexit(EXIT_FAILURE);
                }
    
                /* Make sure it is ALL selection and then print the coordinates. */
                if(H5Sget_select_type(src_space) == H5S_SEL_ALL) {
                    HDfprintf(stdout, "(0) - (%d) \n", DIM0-1);
                }
            }
        }

        HDmemset(rdata,0,sizeof(rdata));
        elements = DIM0;
	dims[0] = elements;
	if ( (memspace = H5Screate_simple(1, dims, NULL)) < 0 ) {
             HDfprintf(stderr, "H5Screate_simple - returned an error\n");
             HDexit(EXIT_FAILURE);
        }
	if ( (filespace = H5Dget_space(vdsset)) < 0 ) {
             HDfprintf(stderr, "H5Dget_space - returned an error\n");
             HDexit(EXIT_FAILURE);
        }
	offset = (hsize_t)mpi_rank * elements;
	if ( (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, 
                          &offset, NULL, &elements, NULL)) < 0 ) {
             HDfprintf(stderr, "H5Sselect_hyperslab - returned an error\n");
             HDexit(EXIT_FAILURE);
        }
	if ((status = H5Dread (vdsset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
	     rdata[0])) < 0) {
             HDfprintf(stderr, "H5Dread - returned an error\n");
             HDexit(EXIT_FAILURE);
        }
        if (mpi_rank == 0) {
	    for(i=0; i<VDSDIM0; i++) {
                HDfprintf (stdout, " VDS Data: [%d][0-99]\n [", i);
                for (j=0; j<VDSDIM1; j++) {
                    if ((j < 6) || (j > VDSDIM1 -3))
                        HDfprintf (stdout," %3d", rdata[i][j]);
                    else if (j == 6) 
                        HDfprintf (stdout, " ... ");
                }
                HDfprintf (stdout, "]\n");
            }
        }
    }

    if (mpi_size > 1) {
        if ((MPI_Allreduce(&nerrs, &group_status, 1, 
                           MPI_INT, MPI_SUM, MPI_COMM_WORLD))
            != MPI_SUCCESS) 
                group_status = 1;
    }
    if (mpi_rank == 0) {
        if (group_status > 0)
            HDfprintf(stderr, "There were internal errors encountered during this test\n");
        else 
            HDfprintf(stdout, "No Errors!\n");

        /* remove the files that a no longer needed */
        for(i=0; i < NFILENAME; i++) {
            HDremove(FILENAMES[i]);
        }
    }

error:
    /*
     * Close and release resources.
     */
    if (group_comm != MPI_COMM_NULL) {
        MPI_Comm_free(&group_comm);
    }

    if (dcpl != -1) H5Pclose (dcpl);
    if (vdsset != -1) H5Dclose (vdsset);
    if (dataspace != -1) H5Sclose (dataspace);
    if (fapl_id != -1) H5Pclose (fapl_id);
    if (vds_file != -1) H5Fclose (vds_file);

    /* close HDF5 library */
    if (H5close() != SUCCEED) {
        HDfprintf(stdout, "H5close() failed. (Ignoring)\n");
    }

    /* MPI_Finalize must be called AFTER H5close which may use MPI calls */

    MPI_Finalize();

    /* cannot just return (nerrs) because exit code is limited to 1byte */
    return((nerrs > 0) ? EXIT_FAILURE : EXIT_SUCCESS );
}

