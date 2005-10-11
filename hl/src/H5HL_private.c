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

#include "H5HL_private.h"

/*-------------------------------------------------------------------------
 *
 * Functions shared between H5TB and H5PT
 *
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: H5TBcommon_append_records
 *
 * Purpose: Common code for reading records shared between H5PT and H5TB
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Nat Furrer, nfurrer@ncsa.uiuc.edu
 *             James Laird, jlaird@ncsa.uiuc.edu
 *
 * Date: March 8, 2004
 *
 * Comments: Called by H5TBappend_records and H5PTappend_records
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5TBcommon_append_records( hid_t dataset_id,
                                  hid_t mem_type_id,
                                  hsize_t nrecords,
                                  hsize_t orig_table_size,
                                  const void * data)
{
  hsize_t  count[1];
  hsize_t offset[1];
  hid_t    space_id = H5I_BADID;
  hid_t    mem_space_id = H5I_BADID;
  hsize_t  dims[1];
  hsize_t  mem_dims[1];

    /* Extend the dataset */
  dims[0] = nrecords + orig_table_size;
  if ( H5Dextend ( dataset_id, dims ) < 0 )
    goto out;

  /* Create a simple memory data space */
  mem_dims[0]=nrecords;
  if ( (mem_space_id = H5Screate_simple( 1, mem_dims, NULL )) < 0 )
    goto out;

  /* Get a copy of the new file data space for writing */
  if ( (space_id = H5Dget_space( dataset_id )) < 0 )
    goto out;

  /* Define a hyperslab in the dataset */
  offset[0] = orig_table_size;
  count[0] = nrecords;
  if ( H5Sselect_hyperslab( space_id, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
    goto out;

  /* Write the records */
  if ( H5Dwrite( dataset_id, mem_type_id, mem_space_id, space_id, H5P_DEFAULT, data ) <
0 )
    goto out;

  /* Terminate access to the dataspace */
  if ( H5Sclose( mem_space_id ) < 0 )
    goto out;

  if ( H5Sclose( space_id ) < 0 )
    goto out;

  return 0;

out:
  H5E_BEGIN_TRY
  H5Sclose(mem_space_id);
  H5Sclose(space_id);
  H5E_END_TRY
  return -1;
}


/*-------------------------------------------------------------------------
 * Function: H5TBcommon_read_records
 *
 * Purpose: Common code for reading records shared between H5PT and H5TB
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Nat Furrer, nfurrer@ncsa.uiuc.edu
 *             James Laird, jlaird@ncsa.uiuc.edu
 *
 * Date: March 8, 2004
 *
 * Comments: Called by H5TBread_records and H5PTread_records
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */
herr_t H5TBcommon_read_records( hid_t dataset_id,
                                                    hid_t mem_type_id,
                                                        hsize_t start,
                                                        hsize_t nrecords,
                                                        hsize_t table_size,
                                                        void *data)
{
  hsize_t  count[1];
  hsize_t offset[1];
  hid_t    space_id = H5I_BADID;
  hid_t    mem_space_id = H5I_BADID;
  hsize_t  mem_size[1];

  /* Make sure the read request is in bounds */
  if ( start + nrecords > table_size )
    goto out;

  /* Get the dataspace handle */
  if ( (space_id = H5Dget_space( dataset_id )) < 0 )
    goto out;

  /* Define a hyperslab in the dataset of the size of the records */
  offset[0] = start;
  count[0]  = nrecords;
  if ( H5Sselect_hyperslab( space_id, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
    goto out;

  /* Create a memory dataspace handle */
  mem_size[0] = count[0];
  if ((mem_space_id = H5Screate_simple( 1, mem_size, NULL )) < 0 )
    goto out;
  if ((H5Dread( dataset_id, mem_type_id, mem_space_id, space_id, H5P_DEFAULT, data)) < 0
 )
    goto out;

  /* Terminate access to the memory dataspace */
  if ( H5Sclose( mem_space_id ) < 0 )
    goto out;

  /* Terminate access to the dataspace */
  if ( H5Sclose( space_id ) < 0 )
    goto out;

  return 0;

out:
  H5E_BEGIN_TRY
  H5Sclose(space_id);
  H5Sclose(mem_space_id);
  H5E_END_TRY
  return -1;
}

