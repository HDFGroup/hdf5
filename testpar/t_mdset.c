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

/* $Id$ */

#include "testphdf5.h"

#define DIM  2
#define SIZE 32
#define NDATASET 4
#define GROUP_DEPTH 128

void write_dataset(hid_t, hid_t, hid_t);
int  read_dataset(hid_t, hid_t, hid_t);
void create_group_recursive(hid_t, hid_t, hid_t, int);
void recursive_read_group(hid_t, hid_t, hid_t, int);

/*
 * Example of using PHDF5 to create ndatasets datasets.  Each process write
 * a slab of array to the file.
 */
void multiple_dset_write(char *filename, int ndatasets)
{
    int i, j, n, mpi_size, mpi_rank;
    hid_t iof, plist, dataset, memspace, filespace;
    hssize_t chunk_origin [DIM];
    hsize_t chunk_dims [DIM], file_dims [DIM];
    hsize_t count[DIM]={1,1};
    double outme [SIZE][SIZE];
    char dname [100];


    MPI_Comm_rank (MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size (MPI_COMM_WORLD, &mpi_size);

    VRFY((mpi_size <= SIZE), "mpi_size <= SIZE");

    chunk_origin [0] = mpi_rank * (SIZE / mpi_size);
    chunk_origin [1] = 0;
    chunk_dims [0] = SIZE / mpi_size;
    chunk_dims [1] = SIZE;

    for (i = 0; i < DIM; i++)
	file_dims [i] = SIZE;

    plist = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type);
    iof = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, plist);
    H5Pclose (plist);

    memspace = H5Screate_simple (DIM, chunk_dims, NULL);
    filespace = H5Screate_simple (DIM, file_dims, NULL);
    H5Sselect_hyperslab (filespace, H5S_SELECT_SET, chunk_origin, chunk_dims, count, chunk_dims);

    for (n = 0; n < ndatasets; n++) {
	sprintf (dname, "dataset %d", n);
	dataset = H5Dcreate (iof, dname, H5T_NATIVE_DOUBLE, filespace, H5P_DEFAULT);
	VRFY((dataset > 0), dname); 

	/* calculate data to write */
	for (i = 0; i < SIZE; i++)
	    for (j = 0; j < SIZE; j++)
	        outme [i][j] = n*1000 + mpi_rank;

	H5Dwrite (dataset, H5T_NATIVE_DOUBLE, memspace, filespace, H5P_DEFAULT, outme);

	H5Dclose (dataset);
#ifdef BARRIER_CHECKS
	if (! ((n+1) % 10)) {
	    printf("created %d datasets\n", n+1);
	    MPI_Barrier(MPI_COMM_WORLD);
	}
#endif /* BARRIER_CHECKS */
    }

    H5Sclose (filespace);
    H5Sclose (memspace);
    H5Fclose (iof);
}

/*
 * Example of using PHDF5 to create multiple groups.  Under the root group, 
 * it creates ngroups groups.  Under the first group just created, it creates 
 * recursive subgroups of depth GROUP_DEPTH.  In each created group, it 
 * generates NDATASETS datasets.  Each process write a hyperslab of an array
 * into the file.
 */
void multiple_group_write(char *filename, int ngroups)
{
    int mpi_rank, mpi_size;
    int l, m;
    char gname[64];
    hid_t fid, gid, plist, memspace, filespace;
    hssize_t chunk_origin[DIM];
    hsize_t chunk_dims[DIM], file_dims[DIM], count[DIM]={1,1};


    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    plist = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type);
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, plist);
    H5Pclose(plist);

  
    chunk_origin[0] = mpi_rank * (SIZE/mpi_size);
    chunk_origin[1] = 0;
    chunk_dims[0]   = SIZE / mpi_size;
    chunk_dims[1]   = SIZE;

    for(l=0; l<DIM; l++)
        file_dims[l] = SIZE;

    memspace  = H5Screate_simple(DIM, chunk_dims, NULL);
    filespace = H5Screate_simple(DIM, file_dims,  NULL);
    H5Sselect_hyperslab(filespace, H5S_SELECT_SET, chunk_origin, chunk_dims, 
                        count, chunk_dims);
   
    /* creates ngroups groups under the root group, writes datasets in 
     * parallel. */
    for(m = 0; m < ngroups; m++) {
        sprintf(gname, "/group%d", m);
        gid = H5Gcreate(fid, gname, 0);
        VRFY((gid > 0), gname);
        
        if(m != 0)
            write_dataset(memspace, filespace, gid); 
        H5Gclose(gid);

#ifdef BARRIER_CHECKS
        if(! ((m+1) % 10)) {
            printf("created %d groups\n", m+1);
            MPI_Barrier(MPI_COMM_WORLD);
	}
#endif /* BARRIER_CHECKS */
    }
    
    /* recursively creates subgroups under the first group. */
    gid = H5Gopen(fid, "group0");
    create_group_recursive(memspace, filespace, gid, 0);
    H5Gclose(gid);
    
    H5Sclose(filespace);
    H5Sclose(memspace);
    H5Fclose(fid);
}

/* 
 * In a group, creates NDATASETS datasets.  Each process writes a hyperslab
 * of a data array to the file.
 */ 
void write_dataset(hid_t memspace, hid_t filespace, hid_t gid)
{
    int i, j, n;
    int mpi_rank;
    char dname[32];
    DATATYPE outme[SIZE][SIZE];
    hid_t did;

  
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    for(n=0; n < NDATASET; n++) {
         sprintf(dname, "dataset%d", n);
         did = H5Dcreate(gid, dname, H5T_NATIVE_INT, filespace, 
                         H5P_DEFAULT);
         VRFY((did > 0), dname);

         for(i=0; i < SIZE; i++)
             for(j=0; j < SIZE; j++)
                 outme[i][j] = n*1000 + mpi_rank;

         H5Dwrite(did, H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, 
                  outme);
         H5Dclose(did);
    }
}

/* 
 * Creates subgroups of depth GROUP_DEPTH recursively.  Also writes datasets
 * in parallel in each group.
 */
void create_group_recursive(hid_t memspace, hid_t filespace, hid_t gid, 
                            int counter)
{ 
   hid_t child_gid;
   int   mpi_rank;
   char  gname[64];
  
   MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

#ifdef BARRIER_CHECKS
   if(! ((counter+1) % 10)) {
        printf("created %dth child groups\n", counter+1);
        MPI_Barrier(MPI_COMM_WORLD);
   }
#endif /* BARRIER_CHECKS */
 
   sprintf(gname, "%dth_child_group", counter+1);   
   child_gid = H5Gcreate(gid, gname, 0);
   VRFY((child_gid > 0), gname);

   /* write datasets in parallel. */
   write_dataset(memspace, filespace, gid);  

   if( counter < GROUP_DEPTH ) 
       create_group_recursive(memspace, filespace, child_gid, counter+1);

   H5Gclose(child_gid);
}

/* 
 * This function is to verify the data from multiple group testing.  It opens
 * every dataset in every group and check their correctness.  
 */
void multiple_group_read(char *filename, int ngroups)
{
    int      mpi_rank, mpi_size, error_num;
    int      l, m;
    char     gname[64];
    hid_t    plist, fid, gid, memspace, filespace;
    hssize_t chunk_origin[DIM];
    hsize_t  chunk_dims[DIM], file_dims[DIM], count[DIM]={1,1};

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    plist = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type);
    fid = H5Fopen(filename, H5F_ACC_RDONLY, plist);
    H5Pclose(plist);

    chunk_origin[0] = mpi_rank * (SIZE/mpi_size);
    chunk_origin[1] = 0;
    chunk_dims[0]   = SIZE / mpi_size;
    chunk_dims[1]   = SIZE;

    for(l=0; l<DIM; l++)
        file_dims[l] = SIZE;

    memspace  = H5Screate_simple(DIM, chunk_dims, NULL);
    filespace = H5Screate_simple(DIM, file_dims, NULL);

    H5Sselect_hyperslab(filespace, H5S_SELECT_SET, chunk_origin, chunk_dims, 
                        count, chunk_dims);
    
    /* open every group under root group. */
    for(m=0; m<ngroups; m++) {
        sprintf(gname, "/group%d", m);
        gid = H5Gopen(fid, gname);
        VRFY((gid > 0), gname);
         
        /* check the data. */
        if(m != 0)
            if( error_num = read_dataset(memspace, filespace, gid) )
                nerrors += error_num;
        H5Gclose(gid);

#ifdef BARRIER_CHECKS
        if(!((m+1)%10))
            MPI_Barrier(MPI_COMM_WORLD);
#endif /* BARRIER_CHECKS */
    }

    /* open all the groups in vertical direction. */
    gid = H5Gopen(fid, "group0");
    VRFY((gid>0), "group0");
    recursive_read_group(memspace, filespace, gid, 0);
    H5Gclose(gid);

    H5Sclose(filespace);
    H5Sclose(memspace);
    H5Fclose(fid);

}

/* 
 * This function opens all the datasets in a certain, checks the data using 
 * dataset_vrfy function.
 */
int read_dataset(hid_t memspace, hid_t filespace, hid_t gid)
{
    int i, j, n, mpi_rank, vrfy_errors=0;
    char dname[32];
    DATATYPE *outdata, *indata;
    hid_t did;
    hsize_t block[DIM]={SIZE,SIZE};

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    indata = (DATATYPE*)malloc(SIZE*SIZE*sizeof(DATATYPE));
    outdata = (DATATYPE*)malloc(SIZE*SIZE*sizeof(DATATYPE));

    for(n=0; n<NDATASET; n++) {
        sprintf(dname, "dataset%d", n);
        did = H5Dopen(gid, dname);
        VRFY((did>0), dname);

        H5Dread(did, H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, 
                indata);

        /* this is the original value */
        for(i=0; i<SIZE; i++)
	    for(j=0; j<SIZE; j++) { 
	         *outdata = n*1000 + mpi_rank;
                 outdata++;
	    }
        outdata -= SIZE*SIZE;

        vrfy_errors = dataset_vrfy(NULL, NULL, NULL, block, indata, outdata);
	       
        H5Dclose(did);
    }

    free(indata);
    free(outdata);

    return vrfy_errors;
}

/* 
 * This recursive function opens all the groups in vertical direction and 
 * checks the data.
 */
void recursive_read_group(hid_t memspace, hid_t filespace, hid_t gid, 
                          int counter)
{
    hid_t child_gid;
    int   mpi_rank, err_num=0;
    char  gname[64];

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
#ifdef BARRIER_CHECKS
    if((counter+1) % 10) 
        MPI_Barrier(MPI_COMM_WORLD);
#endif /* BARRIER_CHECKS */

    if( (err_num = read_dataset(memspace, filespace, gid)) )
        nerrors += err_num;

    if( counter < GROUP_DEPTH ) {
        sprintf(gname, "%dth_child_group", counter+1);
        child_gid = H5Gopen(gid, gname);
        VRFY((child_gid>0), gname);
        recursive_read_group(memspace, filespace, child_gid, counter+1);
        H5Gclose(child_gid);
    }
}
