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

/************************************************************


 ************************************************************/

#include "h5test.h"
#include "testpar.h"

#define VFILE        "vdsunl.h5"
#define DATASET      "VDSUNL"
#define VDSDIM1         6
#define VDSDIM0         4 
#define DIM0            6
#define RANK1           1
#define RANK2           2

#define SRCFILE   "srcfile.h5"
const char *FILENAMES[] = { VFILE, SRCFILE, NULL };

const char *SRC_DATASET[] = {
    "A-0",
    "A-1",
    "A-2",
    "A-3"
};

int
main (int argc, char **argv)
{
                 /* Handles */ 
    hid_t        vfile = -1,
                 space = -1,
                 vspace = -1,
                 dset = -1,
                 src_file = -1,
                 src_dset = -1,
                 src_space = -1,
                 fapl_id = -1,
                 dxpl_id = -1,
                 dcpl = -1;
    herr_t       status;
    hsize_t      vdsdims[2] = {VDSDIM0, VDSDIM1},      /* Virtual datasets dimension */
                 vdsdims_max[2] = {H5S_UNLIMITED, VDSDIM1},
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


    int mpi_rank;
    int mpi_size;
    int workgroup;
    MPI_Comm group_comm = MPI_COMM_WORLD;

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

    if (mpi_size > 4) {
        workgroup = (mpi_rank < 4 ? 0 : 1);
        if (MPI_Comm_split(MPI_COMM_WORLD, workgroup,
       	                   0, &group_comm) != MPI_SUCCESS) {
            HDfprintf(stderr, "FATAL: MPI_Comm_split returned an error\n");
            HDexit(EXIT_FAILURE);
        }
    }

    if (H5open() < 0) {
        HDfprintf(stderr, "FATAL: H5open returned an error\n");
        HDexit(EXIT_FAILURE);
    }

    if (mpi_rank > 3) goto error;

    if ( mpi_rank == 0 ) {
        HDfprintf(stdout, "========================================\n");
        HDfprintf(stdout, "Parallel VDS functionality tests\n");
        HDfprintf(stdout, "        mpi_size     = %d\n", mpi_size);
        HDfprintf(stdout, "========================================\n");
    }

    if ( (fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0 ) {
        HDfprintf(stderr, "H5Pcreate(1) - returned an error\n");
        HDexit(EXIT_FAILURE);
    }
    /*  Please note that only the low order ranks (0-3)
     *  of MPI_COMM_WORLD are participating in the actual parallel
     *  testing.  The rationale for this is that while we can
     *  arbitrarily scale the test, we need to pre-identify the
     *  set of files that will be used.  This is required for
     *  automated testing since we need a mechanism to clean
     *  up the "debris" from failed tests, which currently
     *  gets accomplished by examining the executable to
     *  determine the *.h5 strings which are assumed to be
     *  generated test files and can thus be deleted.
     */
    if ( (status = H5Pset_fapl_mpio(fapl_id, group_comm,
       	       	       MPI_INFO_NULL)) < 0 ) {
        HDfprintf(stderr, "H5Pset_fapl_mpio - returned an error\n");
        HDexit(EXIT_FAILURE);
    }

    if ((src_file = H5Fcreate (SRCFILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0) {
        HDfprintf(stderr, "H5Fcreate(1) - failed to open the file\n");
        HDexit(EXIT_FAILURE);
    }
    if ( (dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0 ) {
        HDfprintf(stderr, "H5Pcreate(1) - returned an error\n");
        HDexit(EXIT_FAILURE);
    }
    if ( (H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE)) < 0 ) {
        HDfprintf(stderr, "H5Pset_dxpl_mpio(1) - returned an error\n");
        HDexit(EXIT_FAILURE);
    }

    if ( (src_space = H5Screate_simple (RANK1, dims, NULL)) < 0) {
        HDfprintf(stderr, "H5Screate_simple(1) - returned an error\n");
        HDexit(EXIT_FAILURE);
    }      

    for (i=0; i < VDSDIM0; i++) {
        for (j = 0; j < DIM0; j++) wdata[j] = i;
        if ( (src_dset = H5Dcreate2 (src_file, SRC_DATASET[i],
                         H5T_NATIVE_INT, src_space, H5P_DEFAULT,
       	       	         H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            HDfprintf(stderr, "H5Dcreate2 - returned an error\n");              
            HDexit(EXIT_FAILURE);
	}
        if ( (status = H5Dwrite (src_dset, 
                         H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
       	       	         H5P_DEFAULT, wdata)) < 0) {
            HDfprintf(stderr, "H5Dwrite - returned an error\n");              
            HDexit(EXIT_FAILURE);
        }
        if (H5Dclose (src_dset) < 0) {
            HDfprintf(stderr, "H5Dclose - returned an error\n");              
            HDexit(EXIT_FAILURE);
	}
        src_dset = -1;
    }

    if ( (vfile = H5Fcreate (VFILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0) {
        HDfprintf(stderr, "H5Fcreate(2) - failed to open the file\n");
        HDexit(EXIT_FAILURE);
    }
    if ( (vspace = H5Screate_simple (RANK2, vdsdims, vdsdims_max)) < 0) {
        HDfprintf(stderr, "H5Screate_simple(2) - returned an error\n");
        HDexit(EXIT_FAILURE);
    }
    if ( (dcpl = H5Pcreate (H5P_DATASET_CREATE)) < 0) {
        HDfprintf(stderr, "H5Pcreate(2) - returned an error\n");
        HDexit(EXIT_FAILURE);
    }
    if ( (status = H5Pset_fill_value (dcpl, H5T_NATIVE_INT, &fill_value)) < 0) {
        HDfprintf(stderr, "H5Pset_fill_value - returned an error\n");
        HDexit(EXIT_FAILURE);
    }
     
    start[0] = 0;
    start[1] = 0;
    count[0] = H5S_UNLIMITED;
    count[1] = 1;
    block[0] = 1;
    block[1] = DIM0;

    if ( (status = H5Sselect_hyperslab (vspace, H5S_SELECT_SET,
       	                       start, NULL, count, block)) < 0) {
        HDfprintf(stderr, "H5Sselect_hyperslab - returned an error\n");
        HDexit(EXIT_FAILURE);
    }
    if ( (status = H5Pset_virtual (dcpl, vspace,
       	                   SRCFILE, "/A-%b", src_space)) < 0) {
        HDfprintf(stderr, "H5Pset_virtual - returned an error\n");
        HDexit(EXIT_FAILURE);
    }
    if ( (dset = H5Dcreate2 (vfile, DATASET, H5T_NATIVE_INT,
			     vspace, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
        HDfprintf(stderr, "H5Dcreate2 - returned an error\n");
        HDexit(EXIT_FAILURE);
    }
    if (vspace != -1) 
        status = H5Sclose (vspace);
    vspace = -1;
    if (dset != -1) 
        status = H5Dclose (dset);
    dset   = -1;
    if (dcpl != -1) 
        status = H5Pclose (dcpl);
    dcpl   = -1;
    if (vfile != -1)
        status = H5Fclose (vfile);    
    vfile  = -1;
    if (src_space != -1)
        status = H5Sclose (src_space);
    src_space = -1;
    if (src_file != -1) 
        status = H5Fclose (src_file);    
    src_file = -1;

    /* We haven't closed the old fapl_id, so let's try reusing it here */
    /* Re-open the virtual file and dataset. */ 

    if ( (vfile = H5Fopen (VFILE, H5F_ACC_RDONLY, fapl_id)) < 0) {
        HDfprintf(stderr, "H5Fopen - returned an error\n");
        HDexit(EXIT_FAILURE);
    }
    if ( (dset = H5Dopen2 (vfile, DATASET, H5P_DEFAULT)) < 0) {
        HDfprintf(stderr, "H5Dopen2 - returned an error\n");
        HDexit(EXIT_FAILURE);
    }

    if ( (dcpl = H5Dget_create_plist (dset)) < 0) {
        HDfprintf(stderr, "H5Dget_create_plist - returned an error\n");
        HDexit(EXIT_FAILURE);
    }

    if ( (layout = H5Pget_layout (dcpl)) < 0) {
        HDfprintf(stderr, "H5Dget_layout - returned an error\n");
        HDexit(EXIT_FAILURE);
    }
    if ( (status = H5Pget_virtual_count (dcpl, &num_map)) < 0) {
        HDfprintf(stderr, "H5Pget_virtual_count - returned an error\n");
        HDexit(EXIT_FAILURE);
    }

    
    if (mpi_rank == 0) {
        /* Print out some results... */
        if (H5D_VIRTUAL == layout) 
            HDfprintf(stdout, " Dataset has a virtual layout \n");
        else
            HDfprintf(stderr, " Wrong layout found \n");

        HDfprintf(stdout, " Number of mappings is %lu\n", (unsigned long)num_map);
        for (i = 0; i < (int)num_map; i++) {   
            HDfprintf(stdout, " Mapping %d \n", i);
            HDfprintf(stdout, "         Selection in the virtual dataset ");
            if ( (vspace = H5Pget_virtual_vspace (dcpl, (size_t)i)) < 0) {
                HDfprintf(stderr, "H5Pget_virtual_vspace - returned an error\n");
                HDexit(EXIT_FAILURE);
            }
    
            if (H5Sget_select_type(vspace) == H5S_SEL_HYPERSLABS) { 
                if (H5Sis_regular_hyperslab(vspace)) {
                    status = H5Sget_regular_hyperslab (vspace, start_out, stride_out, count_out, block_out);
                    HDfprintf(stdout,"\n         start  = [%llu, %llu] \n", (unsigned long long)start_out[0], (unsigned long long)start_out[1]);
                    HDfprintf(stdout,"         stride = [%llu, %llu] \n", (unsigned long long)stride_out[0], (unsigned long long)stride_out[1]);
                    HDfprintf(stdout,"         count  = [%lli, %llu] \n", (signed long long)count_out[0], (unsigned long long)count_out[1]);
                    HDfprintf(stdout,"         block  = [%llu, %llu] \n", (unsigned long long)block_out[0], (unsigned long long)block_out[1]);
                }
         
            }
            if ((len = H5Pget_virtual_filename (dcpl, (size_t)i, NULL, 0)) < 0) {
                HDfprintf(stderr, "H5Pget_virtual_filename - returned an error\n");
                HDexit(EXIT_FAILURE);
            }

            filename = (char *)malloc((size_t)len*sizeof(char)+1);
            if (filename == NULL) {
                HDfprintf(stderr, "malloc(1) - returned NULL\n");
                HDexit(EXIT_FAILURE);
	    }
    
            H5Pget_virtual_filename (dcpl, (size_t)i, filename, len+1);
            HDfprintf(stdout,"         Source filename %s\n", filename);
            len = H5Pget_virtual_dsetname (dcpl, (size_t)i, NULL, 0);
            dsetname = (char *)malloc((size_t)len*sizeof(char)+1);
            if (dsetname == NULL) {
                HDfprintf(stderr, "malloc(2) - returned NULL\n");
                HDexit(EXIT_FAILURE);
	    }
            H5Pget_virtual_dsetname (dcpl, (size_t)i, dsetname, len+1);
            HDfprintf(stdout,"         Source dataset name %s\n", dsetname);
    
            HDfprintf(stdout,"         Selection in the source dataset ");
            if ((src_space = H5Pget_virtual_srcspace (dcpl, (size_t)i)) < 0) {
                HDfprintf(stdout,"H5Pget_virtual_srcspace - returned an error\n");
                HDexit(EXIT_FAILURE);
            }
            if(H5Sget_select_type(src_space) == H5S_SEL_ALL) {
                HDfprintf(stdout, "(0) - (%d) \n", DIM0-1);
            }
    
            if (vspace == -1) 
                status = H5Sclose(vspace);
            vspace = -1;
            if (src_space == -1)
                status = H5Sclose(src_space);
            src_space = -1;
            if (filename != NULL)
                free(filename);
            if (dsetname != NULL) 
                free(dsetname);
        }
    }
    if ( (status = H5Dread (dset, H5T_NATIVE_INT, H5S_ALL,
			    H5S_ALL, H5P_DEFAULT, rdata[0])) < 0) {
        HDfprintf(stderr, "H5Dread - returned an error\n");
        HDexit(EXIT_FAILURE);
    }
    if (mpi_rank == 0) {
        printf (" VDS Data:\n");
        for (i=0; i<VDSDIM0; i++) {
            printf (" [");
            for (j=0; j<VDSDIM1; j++)
                printf (" %3d", rdata[i][j]);
            printf (" ]\n");
        }
    }
    if (dcpl != -1) H5Pclose (dcpl);
    dcpl = -1;
    if (dset != -1) H5Dclose (dset);
    dset = -1;
    if (vfile != -1) H5Fclose (vfile);
    vfile = -1;
    
    error:
        /*
     * Close and release resources.
     */
    if (group_comm != MPI_COMM_WORLD) {
        MPI_Comm_free(&group_comm);
    }

    if (dcpl != -1) H5Pclose (dcpl);
    if (dset != -1) H5Dclose (dset);
    if (src_dset != -1) H5Dclose( src_dset);
    if (src_space != -1) H5Sclose (src_space);
    if (fapl_id != -1) H5Pclose (fapl_id);
    if (dxpl_id != -1) H5Pclose (dxpl_id);
    if (src_file != -1) H5Fclose (src_file);

    for(i=0; FILENAMES[i] != NULL; i++) {
        HDremove(FILENAMES[i]);
    }

    /* close HDF5 library */
    if (H5close() != SUCCEED) {
        HDfprintf(stdout, "H5close() failed. (Ignoring)\n");
    }

    MPI_Finalize();

    return 0;
}
