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

/***********************************************************
*
* Test program:	 th5s
*
* Test the element coordinates for dataspace selection.
*
*************************************************************/

#include "testhdf5.h"

/*
**  Data used to write the dataset.
*/

static int da_buffer[12][1][6][2];

static hsize_t da_dims[4] = { 12, 1, 6, 2 };
static hsize_t da_maxdims[4] = { H5S_UNLIMITED, H5S_UNLIMITED, H5S_UNLIMITED, H5S_UNLIMITED };
/*static hsize_t da_chunksize[4] = { 3, 1, 2, 1 };*/
static hsize_t da_chunksize[4] = { 12, 1, 6, 2 };

/*
** The dataset end of the selection is done using element selection.
** These are the element locations.
*/
#ifdef TMP
static hsize_t da_elements[12][4] = { { 11, 0, 0, 0 },
                                      { 11, 0, 0, 1 },
                                      { 11, 0, 5, 0 },
                                      { 11, 0, 5, 1 },
                                      { 11, 0, 1, 0 },
                                      { 11, 0, 1, 1 },
                                      { 11, 0, 2, 0 },
                                      { 11, 0, 2, 1 },
                                      { 11, 0, 3, 0 },
                                      { 11, 0, 3, 1 },
                                      { 11, 0, 4, 0 },
                                      { 11, 0, 4, 1 } };
#else
static hsize_t da_elements[12][4] = { { 11, 0, 0, 0 },
                                      { 11, 0, 0, 1 },
                                      { 11, 0, 1, 0 },
                                      { 11, 0, 1, 1 },
                                      { 11, 0, 2, 0 },
                                      { 11, 0, 2, 1 },
                                      { 11, 0, 3, 0 },
                                      { 11, 0, 3, 1 },
                                      { 11, 0, 4, 0 },
                                      { 11, 0, 4, 1 },
                                      { 11, 0, 5, 0 },
                                      { 11, 0, 5, 1 } };
#endif

/*
**  This is where it gets interesting.
**
**  First experiment: the data being read is rank=2, so use two
**  dimensions.  However, the array is 6x3, while the transfer is 6x2.
**  We use a hyperslab to select the subset.  This case shows no
**  problem.
*/
static int mem1_buffer[6][3];

static hsize_t mem1_dims[2] = { 6, 3};

static hsize_t mem1_start[2] = { 0, 0 };
static hsize_t mem1_count[2] = { 1, 1 };
static hsize_t mem1_stride[2] = { 1, 1 };
static hsize_t mem1_block[2] = { 6, 2 };


/*
**  Second experiment: the transfer is the same rank as above, but we
**  add two dimensions of 1.  I.e., the array is 1x1x6x2.  In this
**  case, the 6x2 selection is over the entire array, not a subset of
**  the array.  However, we still use hyperslab selection.  This case
**  shows no problem.
*/
static int mem2_buffer[1][1][6][2];

static hsize_t mem2_dims[4] = { 1, 1, 6, 2 };

static hsize_t mem2_start[4] = { 0, 0, 0, 0 };
static hsize_t mem2_count[4] = { 1, 1, 1, 1 };
static hsize_t mem2_stride[4] = { 1, 1, 1, 1 };
static hsize_t mem2_block[4] = { 1, 1, 6, 2 };


/*
**  Third experiment: the transfer is the same rank as above, and we
**  add two dimensions of 1, but now the array is larger: 1x1x6x3.
**  The selection is now over a subset of the array (1x1x6x2).  This
**  case demonstrates the problem.
*/
/*static int mem3_buffer[1][1][6][3];*/
static int mem3_buffer[1][1][6][3];

/*static hsize_t mem3_dims[4] = { 1, 1, 6, 3 };*/
static hsize_t mem3_dims[4] = { 1, 1, 6, 3 };

static hsize_t mem3_start[4] = { 0, 0, 0, 0 };
static hsize_t mem3_count[4] = { 1, 1, 1, 1 };
static hsize_t mem3_stride[4] = { 1, 1, 1, 1 };
static hsize_t mem3_block[4] = { 1, 1, 6, 2 };

/*
**  Fourth experiment: the transfer is the same rank as above, but we
**  add two dimensions of 1.  I.e., the array is 1x6x3.  In this
**  case, the 6x2 selection is over the entire array, not a subset of
**  the array.  However, we still use hyperslab selection.  This case
**  shows the problem.
*/
static int mem4_buffer[1][6][3];

static hsize_t mem4_dims[3] = { 1, 6, 3 };

static hsize_t mem4_start[3] = { 0, 0, 0 };
static hsize_t mem4_count[3] = { 1, 1, 1 };
static hsize_t mem4_stride[3] = { 1, 1, 1 };
static hsize_t mem4_block[3] = { 1, 6, 2 };


/*
**  Subroutine to write the dataset.  It's probably not important to
**  this example, other than to know it's shape.
*/
void write_dataset()
{
  int i;
  hid_t fid, dsid, daid, msid, plid;
  herr_t rv;

  fid = H5Fcreate("coord.hdf", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  if(fid < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  /*dsid = H5Screate_simple(4, da_dims, da_maxdims);*/
  dsid = H5Screate_simple(4, da_dims, da_dims);
  if(dsid < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  plid = H5Pcreate(H5P_DATASET_CREATE);
  if(plid < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  rv = H5Pset_layout(plid, H5D_CHUNKED);
  if(rv < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  rv = H5Pset_chunk(plid, 4, da_chunksize);
  if(rv < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  daid = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, dsid, H5P_DEFAULT, plid, H5P_DEFAULT);
  if(daid < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  /*
  ** We'll only be interested in the front plane ([0][0][0-5[0-1]) so
  ** we only initialize that.
  */
  for(i = 0; i < 12; i++)
    {
      int j;
      for(j = 0; j < 6; j++)
        {
          da_buffer[i][0][j][0] = j * 10;
          da_buffer[i][0][j][1] = j * 10 + 1;
        }
    }

  msid = H5Screate_simple(4, da_dims, da_dims);
  if(msid < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  rv = H5Dwrite(daid, H5T_NATIVE_INT, msid, dsid, H5P_DEFAULT, da_buffer);
  if(rv < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  rv = H5Dclose(daid);
  if(rv < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  rv = H5Fclose(fid);
  if(rv < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }
}

/*
**  Read a dataset using the provided parameters.
*/
void read_dataset(int rank,
                  int* buffer,
                  hsize_t* mdims,
                  hsize_t* start,
                  hsize_t* count,
                  hsize_t* stride,
                  hsize_t* block)
{
  hid_t fid, dsid, daid, msid, plid;
  herr_t rv;

  fid = H5Fopen("coord.hdf", H5F_ACC_RDONLY, H5P_DEFAULT);
  if(fid < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  daid = H5Dopen2(fid, "dataset", H5P_DEFAULT);
  if(daid < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }


  dsid = H5Dget_space(daid);
  if(dsid < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  /*
  ** Element selection is used to select 18 elements from the dataset.
  */
#ifdef TMP
  rv = H5Sselect_elements(dsid, H5S_SELECT_SET, 12, (const hsize_t**)da_elements);
#else
  rv = H5Sselect_hyperslab(dsid, H5S_SELECT_SET, mem2_start, mem2_stride, mem2_count, mem2_block);
#endif
  if(rv < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  msid = H5Screate_simple(rank, mdims, mdims);
  if(dsid < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  /*
  **  The element selection above is combined with hyperslab
  **  selection.  The selection is always be a contiguous block.  (See
  **  above.)
  */
  rv = H5Sselect_hyperslab(msid, H5S_SELECT_SET, start, stride, count, block);
  if(rv < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  rv = H5Dread(daid, H5T_NATIVE_INT, msid, dsid, H5P_DEFAULT, buffer);
  if(rv < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  rv = H5Dclose(daid);
  if(rv < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }

  rv = H5Fclose(fid);
  if(rv < 0)
    {
      H5Eprint2(H5E_DEFAULT, stderr);
      exit(1);
    }
}

void test_coords(void)
{
  int i, j;

  write_dataset();

  /* 1.
  ** Use a rank=2 in memory array. (See above)
  */
  memset(mem1_buffer, 0, sizeof(mem1_buffer));
  read_dataset(2, (int*)mem1_buffer, mem1_dims, mem1_start, mem1_count, mem1_stride, mem1_block);
  for(i = 0; i < 6; i++)
  {
      for(j=0; j<2; j++)
          if(da_buffer[11][0][i][j] != mem1_buffer[i][j])
              TestErrPrintf(" %3d %3d\n", mem1_buffer[i][j], mem1_buffer[i][j]);
  }

  /* 2.
  ** Use a rank=4 in memory array.  Make the array smaller and select
  ** the whole array. (See above)
  */
  memset(mem2_buffer, 0, sizeof(mem2_buffer));
  read_dataset(4, (int*)mem2_buffer, mem2_dims, mem2_start, mem2_count, mem2_stride, mem2_block);
  for(i = 0; i < 6; i++)
    {
      for(j=0; j<2; j++)
          if(da_buffer[11][0][i][j] != mem2_buffer[0][0][i][j])
              TestErrPrintf(" %3d %3d\n", mem2_buffer[0][0][i][j], mem2_buffer[0][0][i][j]);
    }

  /* 3.
  ** Use a rank=4 in memory array, but don't select the whole array. (See above)
  */
  memset(mem3_buffer, 0, sizeof(mem3_buffer));
  read_dataset(4, (int*)mem3_buffer, mem3_dims, mem3_start, mem3_count, mem3_stride, mem3_block);
  for(i = 0; i < 6; i++)
    {
      for(j=0; j<2; j++)
          if(da_buffer[11][0][i][j] != mem3_buffer[0][0][i][j])
              TestErrPrintf(" %3d %3d\n", mem3_buffer[0][0][i][j], mem3_buffer[0][0][i][j]);

    }

  /* 4.
  ** Use a rank=3 in memory array. (See above)
  */
  memset(mem4_buffer, 0, sizeof(mem4_buffer));
  read_dataset(3, (int*)mem4_buffer, mem4_dims, mem4_start, mem4_count, mem4_stride, mem4_block);
  for(i = 0; i < 6; i++)
    {
      for(j=0; j<2; j++)
          if(da_buffer[11][0][i][j] != mem4_buffer[0][i][j])
              TestErrPrintf(" %3d %3d\n", mem4_buffer[0][i][j], mem4_buffer[0][i][j]);
    }

}


/*-------------------------------------------------------------------------
 * Function:	cleanup_coords
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Raymond Lu
 *              20 Dec. 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_coords(void)
{
    remove("coord.hdf");
}
