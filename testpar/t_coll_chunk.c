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

#include "testphdf5.h"
#include "H5Dprivate.h"

/*#define SPACE_DIM1 256
#define SPACE_DIM2 256
#define BYROW_CONT 1
#define BYROW_DISCONT 2
#define DSET_COLLECTIVE_CHUNK_NAME "coll_chunk_name"
*/

/* some commonly used routines for collective chunk IO tests*/
static void ccslab_set(int mpi_rank,int mpi_size,hsize_t start[],hsize_t count[],
		hsize_t stride[],hsize_t block[],int mode);

static void ccdataset_fill(hsize_t start[],hsize_t count[],
                 hsize_t stride[],hsize_t block[],DATATYPE*dataset);

static void ccdataset_print(hsize_t start[],hsize_t block[],DATATYPE*dataset);

static int ccdataset_vrfy(hsize_t start[], hsize_t count[], hsize_t stride[],
                 hsize_t block[], DATATYPE *dataset, DATATYPE *original);

static void coll_chunktest(const char* filename,int chunk_factor,int select_factor);

/*-------------------------------------------------------------------------
 * Function:	coll_chunk1
 *
 * Purpose:	Test the special case of the collective chunk io
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
coll_chunk1(void)
{

  const char *filename;
  filename = GetTestParameters();
  coll_chunktest(filename,1,BYROW_CONT);

}

void
coll_chunk2(void)
{

  const char *filename;
  filename = GetTestParameters();
  coll_chunktest(filename,1,BYROW_DISCONT);

}


void
coll_chunk3(void)
{

  const char *filename;
  int mpi_size;
  MPI_Comm comm = MPI_COMM_WORLD;
  MPI_Comm_size(comm,&mpi_size);
  filename = GetTestParameters();
  coll_chunktest(filename,mpi_size,BYROW_CONT);

}


void
coll_chunk4(void)
{

  const char *filename;
  int mpi_size;
  MPI_Comm comm = MPI_COMM_WORLD;
  MPI_Comm_size(comm,&mpi_size);
  filename = GetTestParameters();
  coll_chunktest(filename,mpi_size*2,BYROW_DISCONT);

}

static void
coll_chunktest(const char* filename,int chunk_factor,int select_factor) {

  hid_t	   file,dataset, file_dataspace;
  hid_t    acc_plist,xfer_plist,crp_plist;
  hbool_t  use_gpfs = FALSE;
  hsize_t  dims[RANK], chunk_dims[RANK];
  int*     data_array1  = NULL;
  int*     data_origin1 = NULL;
  herr_t   status;
  hsize_t start[RANK];
  hsize_t  count[RANK],stride[RANK],block[RANK];
#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
  unsigned prop_value;
#endif /* H5_HAVE_INSTRUMENTED_LIBRARY */
  int mpi_size,mpi_rank;
  MPI_Comm comm = MPI_COMM_WORLD;
  MPI_Info info = MPI_INFO_NULL;

   /* set up MPI parameters */
  MPI_Comm_size(comm,&mpi_size);
  MPI_Comm_rank(comm,&mpi_rank);

  /* Create the data space */
  acc_plist = create_faccess_plist(comm,info,facc_type,use_gpfs);
  VRFY((acc_plist >= 0),"");

  file = H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_plist);
  VRFY((file >= 0),"H5Fcreate succeeded");

  status = H5Pclose(acc_plist);
  VRFY((status >= 0),"");

  /* setup dimensionality object */

    dims[0] = SPACE_DIM1*mpi_size;
    dims[1] = SPACE_DIM2;

  /* each process takes a slab of rows
    stride[0] = 1;
    stride[1] = 1;
    count[0]  = SPACE_DIM1/mpi_size;
    count[1]  = SPACE_DIM2;
    start[0]  = mpi_rank*count[0];
    start[1]  = 0;
    block[0]  = 1;
    block[1]  = 1;
  */

 /* allocate memory for data buffer */
    data_array1 = (int *)malloc(dims[0]*dims[1]*sizeof(int));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

     /* set up dimensions of the slab this process accesses */
    ccslab_set(mpi_rank, mpi_size, start, count, stride, block, select_factor);

    file_dataspace = H5Screate_simple(2, dims, NULL);
    VRFY((file_dataspace >= 0),"file dataspace created succeeded");

    crp_plist = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((crp_plist >= 0),"");

    /* test1: chunk size is equal to dataset size */
    chunk_dims[0] = dims[0]/chunk_factor;

    /* to decrease the testing time, maintain bigger chunk size */
    if(chunk_factor >2) chunk_dims[1] = SPACE_DIM2/2;
    else chunk_dims[1] = SPACE_DIM2/chunk_factor;
    status = H5Pset_chunk(crp_plist, 2, chunk_dims);
    VRFY((status >= 0),"chunk creation property list succeeded");

    dataset = H5Dcreate(file,DSET_COLLECTIVE_CHUNK_NAME,H5T_NATIVE_INT,
			file_dataspace,crp_plist);
    VRFY((dataset >= 0),"dataset created succeeded");
/*    H5Sclose(file_dataspace); */

    status = H5Pclose(crp_plist);
    VRFY((status >= 0),"");

    /*put some trivial data in the data array */
    ccdataset_fill(start, stride,count,block, data_array1);
    MESG("data_array initialized");

/*    file_dataspace = H5Dget_space(dataset); */
    status=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride,
	    count, block);
    VRFY((status >= 0),"hyperslab selection succeeded");

    /* set up the collective transfer property list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0),"");

    status = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((status>= 0),"MPIO collective transfer property succeeded");

    /* write data collectively */
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((status >= 0),"dataset write succeeded");

    status = H5Dclose(dataset);
    VRFY((status >= 0),"");

    /* check whether using collective IO */
    /* Should use H5Pget and H5Pinsert to handle this test. */

    status = H5Pclose(xfer_plist);
    VRFY((status >= 0),"property list closed");

    status = H5Sclose(file_dataspace);
    VRFY((status >= 0),"");

    status = H5Fclose(file);
    VRFY((status >= 0),"");

    if (data_array1) free(data_array1);

    /* Using read to verify the data inside the dataset is correct */

    /* allocate memory for data buffer */
    data_array1 = (int *)malloc(dims[0]*dims[1]*sizeof(int));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

     /* allocate memory for data buffer */
    data_origin1 = (int *)malloc(dims[0]*dims[1]*sizeof(int));
    VRFY((data_origin1 != NULL), "data_origin1 malloc succeeded");

    acc_plist = create_faccess_plist(comm, info, facc_type, use_gpfs);
    VRFY((acc_plist >= 0),"MPIO creation property list succeeded");

    file = H5Fopen(filename,H5F_ACC_RDONLY,acc_plist);
    VRFY((file >= 0),"H5Fcreate succeeded");

    status = H5Pclose(acc_plist);
    VRFY((status >= 0),"");

    /* open the dataset collectively */
    dataset = H5Dopen(file, DSET_COLLECTIVE_CHUNK_NAME);
    VRFY((dataset >= 0), "");

    /* set up dimensions of the slab this process accesses */
    ccslab_set(mpi_rank, mpi_size, start, count, stride, block, select_factor);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset);
    VRFY((file_dataspace >= 0), "");
    status=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((status >= 0), "");

    /* fill dataset with test data */
    ccdataset_fill(start, stride,count,block, data_origin1);
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0),"");
    status = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((status>= 0),"MPIO collective transfer property succeeded");
    status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, file_dataspace,
                      xfer_plist, data_array1);
    VRFY((status >=0),"dataset read succeeded");

    /* verify the read data with original expected data */

    status = ccdataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    if (status) nerrors++;

    status = H5Pclose(xfer_plist);
    VRFY((status >= 0),"property list closed");

    /* close dataset collectively */
    status=H5Dclose(dataset);
    VRFY((status >= 0), "");

    /* release all IDs created */
    H5Sclose(file_dataspace);

    /* close the file collectively */
    H5Fclose(file);

    /* release data buffers */
    if (data_array1) free(data_array1);
    if (data_origin1) free(data_origin1);

}


static void
ccslab_set(int mpi_rank, int mpi_size, hsize_t start[], hsize_t count[],
	 hsize_t stride[], hsize_t block[], int mode)
{
    switch (mode){
    case BYROW_CONT:
	/* Each process takes a slabs of rows. */
	block[0] = 1;
	block[1] = 1;
	stride[0] = 1;
	stride[1] = 1;
	count[0] = SPACE_DIM1;
	count[1] = SPACE_DIM2;
	start[0] = mpi_rank*count[0];
	start[1] = 0;

	if (VERBOSE_MED) printf("slab_set BYROW_CONT\n");
	break;
    case BYROW_DISCONT:
	/* Each process takes several disjoint blocks. */
	block[0] = 1;
	block[1] = 1;
        stride[0] = 3;
        stride[1] = 3;
        count[0]  = (SPACE_DIM1)/(stride[0]*block[0]);
        count[1]  = (SPACE_DIM2)/(stride[1]*block[1]);
	start[0] = SPACE_DIM1*mpi_rank;
	start[1] = 0;
if (VERBOSE_MED) printf("slab_set BYROW_DISCONT\n");
	break;
    default:
	/* Unknown mode.  Set it to cover the whole dataset. */
	printf("unknown slab_set mode (%d)\n", mode);
	block[0] = SPACE_DIM1*mpi_size;
	block[1] = SPACE_DIM2;
	stride[0] = block[0];
	stride[1] = block[1];
	count[0] = 1;
	count[1] = 1;
	start[0] = 0;
	start[1] = 0;
if (VERBOSE_MED) printf("slab_set wholeset\n");
	break;
    }
if (VERBOSE_MED){
    printf("start[]=(%lu,%lu), count[]=(%lu,%lu), stride[]=(%lu,%lu), block[]=(%lu,%lu), total datapoints=%lu\n",
	(unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0], (unsigned long)count[1],
	(unsigned long)stride[0], (unsigned long)stride[1], (unsigned long)block[0], (unsigned long)block[1],
	(unsigned long)(block[0]*block[1]*count[0]*count[1]));
    }
}


/*
 * Fill the dataset with trivial data for testing.
 * Assume dimension rank is 2 and data is stored contiguous.
 */
static void
ccdataset_fill(hsize_t start[], hsize_t stride[], hsize_t count[], hsize_t block[], DATATYPE * dataset)
{
    DATATYPE *dataptr = dataset;
    DATATYPE *tmptr;
    hsize_t i, j,k1,k2;

    /* put some trivial data in the data_array */
    tmptr = dataptr;

    /* assign the disjoint block (two-dimensional)data array value
       through the pointer */
     for (k1 = 0; k1 < count[0]; k1++) {
      for(i = 0;i < block[0]; i++) {
        for(k2 = 0; k2<count[1]; k2++) {
          for(j=0;j<block[1]; j++) {

            dataptr = tmptr + ((start[0]+k1*stride[0]+i)*SPACE_DIM2+
			       start[1]+k2*stride[1]+j);

	      *dataptr = (DATATYPE)(k1+k2+i+j);
          }
         }
      }
    }

}

/*
 * Print the first block of the content of the dataset.
 */
static void
ccdataset_print(hsize_t start[], hsize_t block[], DATATYPE * dataset)
{
    DATATYPE *dataptr = dataset;
    hsize_t i, j;

    /* print the column heading */
    printf("Print only the first block of the dataset\n");
    printf("%-8s", "Cols:");
    for (j=0; j < block[1]; j++){
	printf("%3lu ", (unsigned long)(start[1]+j));
    }
    printf("\n");

    /* print the slab data */
    for (i=0; i < block[0]; i++){
	printf("Row %2lu: ", (unsigned long)(i+start[0]));
	for (j=0; j < block[1]; j++){
	    printf("%03d ", *dataptr++);
	}
	printf("\n");
    }
}


/*
 * Print the content of the dataset.
 */
static int
ccdataset_vrfy(hsize_t start[], hsize_t count[], hsize_t stride[], hsize_t block[], DATATYPE *dataset, DATATYPE *original)
{
    hsize_t i, j,k1,k2;
    int vrfyerrs;
    DATATYPE *dataptr,*oriptr;

    /* print it if VERBOSE_MED */
    if (VERBOSE_MED) {
	printf("dataset_vrfy dumping:::\n");
	printf("start(%lu, %lu), count(%lu, %lu), stride(%lu, %lu), block(%lu, %lu)\n",
	    (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0], (unsigned long)count[1],
	    (unsigned long)stride[0], (unsigned long)stride[1], (unsigned long)block[0], (unsigned long)block[1]);
	printf("original values:\n");
	ccdataset_print(start, block, original);
	printf("compared values:\n");
	ccdataset_print(start, block, dataset);
    }

    vrfyerrs = 0;

    for (k1 = 0; k1 < count[0];k1++) {
      for(i = 0;i < block[0];i++) {
        for(k2 = 0; k2<count[1];k2++) {
          for(j=0;j<block[1];j++) {

             dataptr = dataset + ((start[0]+k1*stride[0]+i)*SPACE_DIM2+
			       start[1]+k2*stride[1]+j);
	     oriptr =  original + ((start[0]+k1*stride[0]+i)*SPACE_DIM2+
			       start[1]+k2*stride[1]+j);

	    if (*dataptr != *oriptr){
		if (vrfyerrs++ < MAX_ERR_REPORT || VERBOSE_MED){
		    printf("Dataset Verify failed at [%lu][%lu]: expect %d, got %d\n",
			(unsigned long)i, (unsigned long)j,
	     	    	*(original), *(dataset));
		}
	    }
	  }
	}
      }
    }
    if (vrfyerrs > MAX_ERR_REPORT && !VERBOSE_MED)
	printf("[more errors ...]\n");
    if (vrfyerrs)
	printf("%d errors found in ccdataset_vrfy\n", vrfyerrs);
    return(vrfyerrs);
}
