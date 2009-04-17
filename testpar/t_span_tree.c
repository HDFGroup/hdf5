
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
   This program will test irregular hyperslab selections with collective write and read.
   The way to test whether collective write and read works is to use independent IO
   output to verify the collective output.

   1) We will write two datasets with the same hyperslab selection settings;
   one in independent mode,
   one in collective mode,
   2) We will read two datasets with the same hyperslab selection settings,
      1.  independent read to read independent output,
          independent read to read collecive   output,
	  Compare the result,
	  If the result is the same, then collective write succeeds.
      2.  collective read to read independent output,
          independent read to read independent output,
	  Compare the result,
	  If the result is the same, then collective read succeeds.

 */

#include "hdf5.h"
#include "H5private.h"
#include "testphdf5.h"


static void coll_write_test(int chunk_factor);
static void coll_read_test(int chunk_factor);


/*-------------------------------------------------------------------------
 * Function:	coll_irregular_cont_write
 *
 * Purpose:	Test the collectively irregular hyperslab write in contiguous
                storage
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		Dec 2nd, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
coll_irregular_cont_write(void)
{

  coll_write_test(0);

}



/*-------------------------------------------------------------------------
 * Function:	coll_irregular_cont_read
 *
 * Purpose:	Test the collectively irregular hyperslab read in contiguous
                storage
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		Dec 2nd, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
coll_irregular_cont_read(void)
{

  coll_read_test(0);

}


/*-------------------------------------------------------------------------
 * Function:	coll_irregular_simple_chunk_write
 *
 * Purpose:	Test the collectively irregular hyperslab write in chunk
                storage(1 chunk)
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		Dec 2nd, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
coll_irregular_simple_chunk_write(void)
{

  coll_write_test(1);

}



/*-------------------------------------------------------------------------
 * Function:	coll_irregular_simple_chunk_read
 *
 * Purpose:	Test the collectively irregular hyperslab read in chunk
                storage(1 chunk)
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		Dec 2nd, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
coll_irregular_simple_chunk_read(void)
{

  coll_read_test(1);

}

/*-------------------------------------------------------------------------
 * Function:	coll_irregular_complex_chunk_write
 *
 * Purpose:	Test the collectively irregular hyperslab write in chunk
                storage(4 chunks)
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		Dec 2nd, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
coll_irregular_complex_chunk_write(void)
{

  coll_write_test(4);

}



/*-------------------------------------------------------------------------
 * Function:	coll_irregular_complex_chunk_read
 *
 * Purpose:	Test the collectively irregular hyperslab read in chunk
                storage(1 chunk)
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Unknown
 *		Dec 2nd, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
coll_irregular_complex_chunk_read(void)
{

  coll_read_test(4);

}


void coll_write_test(int chunk_factor)
{

  const char *filename;
  hid_t   acc_plist,xfer_plist;
  hbool_t  use_gpfs = FALSE;
  hid_t   file, datasetc,dataseti;           /* File and dataset identifiers */
  hid_t   mspaceid1, mspaceid, fspaceid,fspaceid1; /* Dataspace identifiers */
  hsize_t mdim1[1],fsdim[2],mdim[2];
  hid_t   plist;                   /* Dataset property list identifier */

#if 0
  hsize_t mdim1[] = {MSPACE1_DIM};  /* Dimension size of the first dataset
				      (in memory) */

  hsize_t fsdim[] = {FSPACE_DIM1, FSPACE_DIM2};
  /* Dimension sizes of the dataset (on disk) */
  hsize_t mdim[] = {MSPACE_DIM1, MSPACE_DIM2}; /* Dimension sizes of the
						  dataset in memory when we
						  read selection from the
						  dataset on the disk */
#endif

  hsize_t start[2];   /* Start of hyperslab */
  hsize_t  stride[2]; /* Stride of hyperslab */
  hsize_t  count[2];  /* Block count */
  hsize_t  block[2];  /* Block sizes */
  hsize_t  chunk_dims[RANK];

  herr_t   ret;
  unsigned i,j;
  int fillvalue = 0;   /* Fill value for the dataset */

#if 0
  int    matrix_out[MSPACE_DIM1][MSPACE_DIM2];
  int    matrix_out1[MSPACE_DIM1][MSPACE_DIM2];   /* Buffer to read from the
						  dataset */
  int    vector[MSPACE1_DIM];
#endif

  int      *matrix_out, *matrix_out1, *vector;

  int    mpi_size,mpi_rank;

  MPI_Comm comm = MPI_COMM_WORLD;
  MPI_Info info = MPI_INFO_NULL;

  /*set up MPI parameters */
  MPI_Comm_size(comm,&mpi_size);
  MPI_Comm_rank(comm,&mpi_rank);


  /* Obtain file name */
  filename = GetTestParameters();

  /*
   * Buffers' initialization.
   */
  mdim1[0] = MSPACE1_DIM *mpi_size;
  mdim[0]  = MSPACE_DIM1;
  mdim[1]  = MSPACE_DIM2*mpi_size;
  fsdim[0] = FSPACE_DIM1;
  fsdim[1] = FSPACE_DIM2*mpi_size;

  vector = (int*)HDmalloc(sizeof(int)*mdim1[0]*mpi_size);
  matrix_out  = (int*)HDmalloc(sizeof(int)*mdim[0]*mdim[1]*mpi_size);
  matrix_out1 = (int*)HDmalloc(sizeof(int)*mdim[0]*mdim[1]*mpi_size);

  HDmemset(vector,0,sizeof(int)*mdim1[0]*mpi_size);
  vector[0] = vector[MSPACE1_DIM*mpi_size - 1] = -1;
  for (i = 1; i < MSPACE1_DIM*mpi_size - 1; i++) vector[i] = i;

#if 0
  acc_plist = H5Pcreate(H5P_FILE_ACCESS);
  VRFY((acc_plist >= 0),"");

  ret       = H5Pset_fapl_mpio(acc_plist,comm,info);
  VRFY((ret >= 0),"MPIO creation property list succeeded");
#endif

  acc_plist = create_faccess_plist(comm, info, facc_type, use_gpfs);
  VRFY((acc_plist >= 0),"");

  /*
   * Create a file.
   */
  file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_plist);
  VRFY((file >= 0),"H5Fcreate succeeded");

  /*
   * Create property list for a dataset and set up fill values.
   */
  plist = H5Pcreate(H5P_DATASET_CREATE);
  VRFY((acc_plist >= 0),"");

  ret   = H5Pset_fill_value(plist, H5T_NATIVE_INT, &fillvalue);
  VRFY((ret >= 0),"Fill value creation property list succeeded");

  if(chunk_factor != 0) {

    chunk_dims[0] = fsdim[0]/chunk_factor;
    chunk_dims[1] = fsdim[1]/chunk_factor;
     ret = H5Pset_chunk(plist, 2, chunk_dims);
    VRFY((ret >= 0),"chunk creation property list succeeded");
  }
  /*
   * Create dataspace for the dataset in the file.
   */
  fspaceid = H5Screate_simple(FSPACE_RANK, fsdim, NULL);
  VRFY((fspaceid >= 0),"file dataspace created succeeded");

  /*
   * Create dataset in the file. Notice that creation
   * property list plist is used.
   */
  datasetc = H5Dcreate(file, "collect_write", H5T_NATIVE_INT, fspaceid, plist);
  VRFY((datasetc >= 0),"dataset created succeeded");

  dataseti = H5Dcreate(file, "independ_write", H5T_NATIVE_INT, fspaceid, plist);
  VRFY((dataseti >= 0),"dataset created succeeded");
  /*
   * Select hyperslab for the dataset in the file, using 3x2 blocks,
   * (4,3) stride and (1,4) count starting at the position (0,1)
   for the first selection
  */

  start[0]  = FHSTART0;
  start[1]  = FHSTART1+mpi_rank*FHSTRIDE1*FHCOUNT1;
  stride[0] = FHSTRIDE0;
  stride[1] = FHSTRIDE1;
  count[0]  = FHCOUNT0;
  count[1]  = FHCOUNT1;
  block[0]  = FHBLOCK0;
  block[1]  = FHBLOCK1;

  ret = H5Sselect_hyperslab(fspaceid, H5S_SELECT_SET, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /*
   * Select hyperslab for the dataset in the file, using 3x2*4 blocks,
   * stride 1  and (1,1) count starting at the position (4,0).
   */

  start[0]  = SHSTART0;
  start[1]  = SHSTART1+SHCOUNT1*SHBLOCK1*mpi_rank;
  stride[0] = SHSTRIDE0;
  stride[1] = SHSTRIDE1;
  count[0]  = SHCOUNT0;
  count[1]  = SHCOUNT1;
  block[0]  = SHBLOCK0;
  block[1]  = SHBLOCK1;

  ret = H5Sselect_hyperslab(fspaceid, H5S_SELECT_OR, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /*
   * Create dataspace for the first dataset.
   */
  mspaceid1 = H5Screate_simple(MSPACE1_RANK, mdim1, NULL);
  VRFY((mspaceid1 >= 0),"memory dataspace created succeeded");

  /*
   * Select hyperslab.
   * We will use 48 elements of the vector buffer starting at the second element.
   * Selected elements are 1 2 3 . . . 48
   */
  start[0]  = MHSTART0;
  stride[0] = MHSTRIDE0;
  count[0]  = MHCOUNT0;
  block[0]  = MHBLOCK0;

  ret = H5Sselect_hyperslab(mspaceid1, H5S_SELECT_SET, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");


  ret = H5Dwrite(dataseti, H5T_NATIVE_INT, mspaceid1, fspaceid, H5P_DEFAULT, vector);
  VRFY((ret >= 0),"dataset independent write succeed");
  xfer_plist = H5Pcreate(H5P_DATASET_XFER);
  VRFY((xfer_plist >= 0),"");

  ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
  VRFY((ret >= 0),"MPIO data transfer property list succeed");


  ret = H5Dwrite(datasetc, H5T_NATIVE_INT, mspaceid1, fspaceid, xfer_plist, vector);
/* ret = H5Dwrite(datasetc, H5T_NATIVE_INT, mspaceid1, fspaceid, H5P_DEFAULT, vector);*/
  VRFY((ret >= 0),"dataset collective write succeed");

  ret = H5Sclose(mspaceid1);
  VRFY((ret >= 0),"");

  ret = H5Sclose(fspaceid);
  VRFY((ret >= 0),"");

  /*
   * Close dataset.
   */
  ret = H5Dclose(datasetc);
  VRFY((ret >= 0),"");
  ret = H5Dclose(dataseti);
  VRFY((ret >= 0),"");

  /*
   * Close the file.
   */
  ret = H5Fclose(file);
  VRFY((ret >= 0),"");
  /*
   * Close property list
   */

  ret = H5Pclose(acc_plist);
  VRFY((ret >= 0),"");
  ret = H5Pclose(xfer_plist);
  VRFY((ret >= 0),"");
  ret = H5Pclose(plist);
  VRFY((ret >= 0),"");

  /*
   * Open the file.
   */

  /*** For testing collective hyperslab selection write ***/

#if 0
  acc_plist = H5Pcreate(H5P_FILE_ACCESS);
  VRFY((acc_plist >= 0),"");

  ret       = H5Pset_fapl_mpio(acc_plist,comm,info);
  VRFY((ret >= 0),"MPIO creation property list succeeded");
#endif
  acc_plist = create_faccess_plist(comm, info, facc_type, use_gpfs);
  VRFY((acc_plist >= 0),"");

  file = H5Fopen(filename, H5F_ACC_RDONLY, acc_plist);
  VRFY((file >= 0),"H5Fopen succeeded");

  /*
   * Open the dataset.
   */
  datasetc = H5Dopen(file,"collect_write");
  VRFY((datasetc >= 0),"H5Dopen succeeded");
  dataseti = H5Dopen(file,"independ_write");
  VRFY((dataseti >= 0),"H5Dopen succeeded");

  /*
   * Get dataspace of the open dataset.
   */
  fspaceid = H5Dget_space(datasetc);
  VRFY((fspaceid >= 0),"file dataspace obtained succeeded");

  fspaceid1 = H5Dget_space(dataseti);
  VRFY((fspaceid1 >= 0),"file dataspace obtained succeeded");



  start[0]  = RFFHSTART0;
  start[1]  = RFFHSTART1+mpi_rank*RFFHCOUNT1;
  block[0]  = RFFHBLOCK0;
  block[1]  = RFFHBLOCK1;
  stride[0] = RFFHSTRIDE0;
  stride[1] = RFFHSTRIDE1;
  count[0]  = RFFHCOUNT0;
  count[1]  = RFFHCOUNT1;


  ret = H5Sselect_hyperslab(fspaceid, H5S_SELECT_SET, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");
  ret = H5Sselect_hyperslab(fspaceid1, H5S_SELECT_SET, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /*start[0] = RFSHSTART0+mpi_rank*RFSHCOUNT1/mpi_size; */
  start[0] = RFSHSTART0;
  start[1] = RFSHSTART1+RFSHCOUNT1*mpi_rank;
  block[0] = RFSHBLOCK0;
  block[1] = RFSHBLOCK1;
  stride[0] = RFSHSTRIDE0;
  stride[1] = RFSHSTRIDE0;
  count[0]  = RFSHCOUNT0;
  count[1]  = RFSHCOUNT1;
  ret = H5Sselect_hyperslab(fspaceid, H5S_SELECT_OR, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");
  ret = H5Sselect_hyperslab(fspaceid1, H5S_SELECT_OR, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");


  /*
   * Create memory dataspace.
   */
  mspaceid = H5Screate_simple(MSPACE_RANK, mdim, NULL);

  /*
   * Select two hyperslabs in memory. Hyperslabs has the same
   * size and shape as the selected hyperslabs for the file dataspace.
   */


  start[0]  = RMFHSTART0;
  start[1]  = RMFHSTART1+mpi_rank*RMFHCOUNT1;
  block[0]  = RMFHBLOCK0;
  block[1]  = RMFHBLOCK1;
  stride[0] = RMFHSTRIDE0;
  stride[1] = RMFHSTRIDE1;
  count[0]  = RMFHCOUNT0;
  count[1]  = RMFHCOUNT1;
  ret = H5Sselect_hyperslab(mspaceid, H5S_SELECT_SET, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  start[0]  = RMSHSTART0;
  start[1]  = RMSHSTART1+mpi_rank*RMSHCOUNT1;
  block[0]  = RMSHBLOCK0;
  block[1]  = RMSHBLOCK1;
  stride[0] = RMSHSTRIDE0;
  stride[1] = RMSHSTRIDE1;
  count[0]  = RMSHCOUNT0;
  count[1]  = RMSHCOUNT1;


  ret = H5Sselect_hyperslab(mspaceid, H5S_SELECT_OR, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /*
   * Initialize data buffer.
   */
  HDmemset(matrix_out,0,sizeof(int)*MSPACE_DIM1*MSPACE_DIM2*mpi_size);
  HDmemset(matrix_out1,0,sizeof(int)*MSPACE_DIM1*MSPACE_DIM2*mpi_size);

  /*
   * Read data back to the buffer matrix_out.
   */

  ret = H5Dread(datasetc, H5T_NATIVE_INT, mspaceid, fspaceid,
		H5P_DEFAULT, matrix_out);
  VRFY((ret >= 0),"H5D independent read succeed");


  ret = H5Dread(dataseti, H5T_NATIVE_INT, mspaceid, fspaceid,
		H5P_DEFAULT, matrix_out1);
  VRFY((ret >= 0),"H5D independent read succeed");

  ret = 0;

  for (i = 0; i < MSPACE_DIM1*MSPACE_DIM2*mpi_size; i++){
         if(matrix_out[i]!=matrix_out1[i]) ret = -1;
      if(ret < 0) break;
    }

  VRFY((ret >= 0),"H5D contiguous irregular collective write succeed");

  /*
   * Close memory file and memory dataspaces.
   */
  ret = H5Sclose(mspaceid);
  VRFY((ret >= 0),"");
  ret = H5Sclose(fspaceid);
  VRFY((ret >= 0),"");

  /*
   * Close dataset.
   */
  ret = H5Dclose(dataseti);
  VRFY((ret >= 0),"");

  ret = H5Dclose(datasetc);
  VRFY((ret >= 0),"");
  /*
   * Close property list
   */

  ret = H5Pclose(acc_plist);
  VRFY((ret >= 0),"");


  /*
   * Close the file.
   */
  ret = H5Fclose(file);
  VRFY((ret >= 0),"");

  return ;
}


void coll_read_test(int chunk_factor)
{

  const char *filename;
  hid_t   acc_plist,xfer_plist;
  hid_t   file, dataseti;           /* File and dataset identifiers */
  hid_t   mspaceid, fspaceid1; /* Dataspace identifiers */
  hbool_t use_gpfs = FALSE;

  /* Dimension sizes of the dataset (on disk) */
#if 0
  hsize_t mdim[] = {MSPACE_DIM1, MSPACE_DIM2}; /* Dimension sizes of the
						  dataset in memory when we
						  read selection from the
						  dataset on the disk */
#endif
  hsize_t mdim[2];
  hsize_t  start[2]; /* Start of hyperslab */
  hsize_t  stride[2]; /* Stride of hyperslab */
  hsize_t  count[2];  /* Block count */
  hsize_t  block[2];  /* Block sizes */

  herr_t   ret;
  unsigned i,j;

  int     *matrix_out;
  int     *matrix_out1;
#if 0
  int    matrix_out[MSPACE_DIM1][MSPACE_DIM2];
  int    matrix_out1[MSPACE_DIM1][MSPACE_DIM2];   /* Buffer to read from the
						  dataset */
#endif
  int    mpi_size,mpi_rank;

  MPI_Comm comm = MPI_COMM_WORLD;
  MPI_Info info = MPI_INFO_NULL;

  /*set up MPI parameters */
  MPI_Comm_size(comm,&mpi_size);
  MPI_Comm_rank(comm,&mpi_rank);


  /* Obtain file name */
  filename = GetTestParameters();

  /*
   * Buffers' initialization.
   */
  mdim[0] = MSPACE_DIM1;
  mdim[1] = MSPACE_DIM2*mpi_size;
  matrix_out =(int*)HDmalloc(sizeof(int)*MSPACE_DIM1*MSPACE_DIM2*mpi_size);
  matrix_out1=(int*)HDmalloc(sizeof(int)*MSPACE_DIM1*MSPACE_DIM2*mpi_size);

  /*
   * Open the file.
   */

  /*** For testing collective hyperslab selection read ***/

#if 0
  acc_plist = H5Pcreate(H5P_FILE_ACCESS);
  VRFY((acc_plist >= 0),"");

  ret       = H5Pset_fapl_mpio(acc_plist,comm,info);
  VRFY((ret >= 0),"MPIO creation property list succeeded");
#endif

  acc_plist = create_faccess_plist(comm, info, facc_type, use_gpfs);
  VRFY((acc_plist >= 0),"");

  file = H5Fopen(filename, H5F_ACC_RDONLY, acc_plist);
  VRFY((file >= 0),"H5Fopen succeeded");

  /*
   * Open the dataset.
   */
  dataseti = H5Dopen(file,"independ_write");
  VRFY((dataseti >= 0),"H5Dopen succeeded");

  /*
   * Get dataspace of the open dataset.
   */

  fspaceid1 = H5Dget_space(dataseti);
  VRFY((fspaceid1 >= 0),"file dataspace obtained succeeded");

  start[0]  = RFFHSTART0;
  start[1]  = RFFHSTART1+mpi_rank*RFFHCOUNT1;
  block[0]  = RFFHBLOCK0;
  block[1]  = RFFHBLOCK1;
  stride[0] = RFFHSTRIDE0;
  stride[1] = RFFHSTRIDE1;
  count[0]  = RFFHCOUNT0;
  count[1]  = RFFHCOUNT1;

  ret = H5Sselect_hyperslab(fspaceid1, H5S_SELECT_SET, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");


  start[0] = RFSHSTART0;
  start[1] = RFSHSTART1+RFSHCOUNT1*mpi_rank;
  block[0] = RFSHBLOCK0;
  block[1] = RFSHBLOCK1;
  stride[0] = RFSHSTRIDE0;
  stride[1] = RFSHSTRIDE0;
  count[0]  = RFSHCOUNT0;
  count[1]  = RFSHCOUNT1;

  ret = H5Sselect_hyperslab(fspaceid1, H5S_SELECT_OR, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");


  /*
   * Create memory dataspace.
   */
  mspaceid = H5Screate_simple(MSPACE_RANK, mdim, NULL);

  /*
   * Select two hyperslabs in memory. Hyperslabs has the same
   * size and shape as the selected hyperslabs for the file dataspace.
   */

  start[0]  = RMFHSTART0;
  start[1]  = RMFHSTART1+mpi_rank*RMFHCOUNT1;
  block[0]  = RMFHBLOCK0;
  block[1]  = RMFHBLOCK1;
  stride[0] = RMFHSTRIDE0;
  stride[1] = RMFHSTRIDE1;
  count[0]  = RMFHCOUNT0;
  count[1]  = RMFHCOUNT1;
  ret = H5Sselect_hyperslab(mspaceid, H5S_SELECT_SET, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  start[0]  = RMSHSTART0;
  start[1]  = RMSHSTART1+mpi_rank*RMSHCOUNT1;
  block[0]  = RMSHBLOCK0;
  block[1]  = RMSHBLOCK1;
  stride[0] = RMSHSTRIDE0;
  stride[1] = RMSHSTRIDE1;
  count[0]  = RMSHCOUNT0;
  count[1]  = RMSHCOUNT1;
  ret = H5Sselect_hyperslab(mspaceid, H5S_SELECT_OR, start, stride, count, block);
  VRFY((ret >= 0),"hyperslab selection succeeded");

  /*
   * Initialize data buffer.
   */
  HDmemset(matrix_out,0,sizeof(int)*MSPACE_DIM1*MSPACE_DIM2*mpi_size);
  HDmemset(matrix_out1,0,sizeof(int)*MSPACE_DIM1*MSPACE_DIM2*mpi_size);

  /*
   * Read data back to the buffer matrix_out.
   */

  xfer_plist = H5Pcreate(H5P_DATASET_XFER);
  VRFY((xfer_plist >= 0),"");

  ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
  VRFY((ret >= 0),"MPIO data transfer property list succeed");

    ret = H5Dread(dataseti, H5T_NATIVE_INT, mspaceid, fspaceid1,
		xfer_plist, matrix_out);
		VRFY((ret >= 0),"H5D collecive read succeed");

  ret = H5Pclose(xfer_plist);
  VRFY((ret >= 0),"");

  ret = H5Dread(dataseti, H5T_NATIVE_INT, mspaceid, fspaceid1,
		H5P_DEFAULT, matrix_out1);

  VRFY((ret >= 0),"H5D independent read succeed");
  ret = 0;
  for (i = 0; i < MSPACE_DIM1*MSPACE_DIM2*mpi_size; i++){
      if(matrix_out[i]!=matrix_out1[i])ret = -1;
      if(ret < 0) break;
  }
  VRFY((ret >= 0),"H5D contiguous irregular collective read succeed");

  /*
   * Close memory file and memory dataspaces.
   */
  ret = H5Sclose(mspaceid);
  VRFY((ret >= 0),"");
  ret = H5Sclose(fspaceid1);
  VRFY((ret >= 0),"");

  /*
   * Close dataset.
   */
  ret = H5Dclose(dataseti);
  VRFY((ret >= 0),"");
  /*
   * Close property list
   */
  ret = H5Pclose(acc_plist);
  VRFY((ret >= 0),"");


  /*
   * Close the file.
   */
  ret = H5Fclose(file);
  VRFY((ret >= 0),"");

  return ;
}
