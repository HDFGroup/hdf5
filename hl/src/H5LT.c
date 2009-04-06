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

#include <string.h>
#include <stdlib.h>
#include "hdf5.h"
#if H5_VERS_MAJOR == 1 && H5_VERS_MINOR == 6
#include "H5LT.h"
#else
#include <hdf5_hl.h>
#endif

/*-------------------------------------------------------------------------
 *
 * Private functions
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LT_open_id( hid_t loc_id,
                     const char *obj_name,
                     int obj_type );

herr_t H5LT_close_id( hid_t obj_id,
                      int obj_type );

/*-------------------------------------------------------------------------
 *
 * Public functions
 *
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: H5LTmake_dataset
 *
 * Purpose: Creates and writes a dataset of a type tid
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 19, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTmake_dataset( hid_t loc_id,
                         const char *dset_name,
                         int rank,
                         const hsize_t *dims,
                         hid_t tid,
                         const void *data )
{

 hid_t   did, sid;

 /* Create the data space for the dataset. */
 if ( (sid = H5Screate_simple( rank, dims, NULL )) < 0 )
  return -1;

 /* Create the dataset. */
 if ( (did = H5Dcreate( loc_id, dset_name, tid, sid, H5P_DEFAULT )) < 0 )
  goto out;

 /* Write the dataset only if there is data to write */

 if ( data )
 {
  if ( H5Dwrite( did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, data ) < 0 )
   goto out;
 }

 /* End access to the dataset and release resources used by it. */
 if ( H5Dclose( did ) < 0 )
  return -1;

 /* Terminate access to the data space. */
 if ( H5Sclose( sid ) < 0 )
  return -1;

 return 0;

out:
 H5Dclose( did );
 H5Sclose( sid );
 return -1;
}

/*-------------------------------------------------------------------------
 * Function: H5LTmake_dataset_char
 *
 * Purpose: Creates and writes a dataset of H5T_NATIVE_CHAR type
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 14, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTmake_dataset_char( hid_t loc_id,
                              const char *dset_name,
                              int rank,
                              const hsize_t *dims,
                              const char *data )
{

 hid_t   did, sid;

 /* Create the data space for the dataset. */
 if ( (sid = H5Screate_simple( rank, dims, NULL )) < 0 )
  return -1;

 /* Create the dataset. */
 if ( (did = H5Dcreate( loc_id, dset_name, H5T_NATIVE_CHAR, sid, H5P_DEFAULT )) < 0 )
  goto out;

 /* Write the dataset only if there is data to write */

 if ( data )
 {
  if ( H5Dwrite( did, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, data ) < 0 )
   goto out;
 }

 /* End access to the dataset and release resources used by it. */
 if ( H5Dclose( did ) < 0 )
  return -1;

 /* Terminate access to the data space. */
 if ( H5Sclose( sid ) < 0 )
  return -1;

 return 0;

out:
 H5Dclose( did );
 H5Sclose( sid );
 return -1;
}


/*-------------------------------------------------------------------------
 * Function: H5LTmake_dataset_short
 *
 * Purpose: Creates and writes a dataset of H5T_NATIVE_SHORT type
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 14, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */


herr_t H5LTmake_dataset_short( hid_t loc_id,
                               const char *dset_name,
                               int rank,
                               const hsize_t *dims,
                               const short *data )
{

 hid_t   did, sid;

 /* Create the data space for the dataset. */
 if ( (sid = H5Screate_simple( rank, dims, NULL )) < 0 )
  return -1;

 /* Create the dataset. */
 if ( (did = H5Dcreate( loc_id, dset_name, H5T_NATIVE_SHORT, sid, H5P_DEFAULT )) < 0 )
  goto out;

 /* Write the dataset only if there is data to write */

 if ( data )
 {
  if ( H5Dwrite( did, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data ) < 0 )
   goto out;
 }

 /* End access to the dataset and release resources used by it. */
 if ( H5Dclose( did ) < 0 )
  return -1;

 /* Terminate access to the data space. */
 if ( H5Sclose( sid ) < 0 )
  return -1;

 return 0;

out:
 H5Dclose( did );
 H5Sclose( sid );
 return -1;
}

/*-------------------------------------------------------------------------
 * Function: H5LTmake_dataset_int
 *
 * Purpose: Creates and writes a dataset of H5T_NATIVE_INT type
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 14, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */


herr_t H5LTmake_dataset_int( hid_t loc_id,
                             const char *dset_name,
                             int rank,
                             const hsize_t *dims,
                             const int *data )
{

 hid_t   did, sid;

 /* Create the data space for the dataset. */
 if ( (sid = H5Screate_simple( rank, dims, NULL )) < 0 )
  return -1;

 /* Create the dataset. */
 if ( (did = H5Dcreate( loc_id, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT )) < 0 )
  goto out;

 /* Write the dataset only if there is data to write */

 if ( data )
 {
  if ( H5Dwrite( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data ) < 0 )
   goto out;
 }

 /* End access to the dataset and release resources used by it. */
 if ( H5Dclose( did ) < 0 )
  return -1;

 /* Terminate access to the data space. */
 if ( H5Sclose( sid ) < 0 )
  return -1;

 return 0;

out:
 H5Dclose( did );
 H5Sclose( sid );
 return -1;
}



/*-------------------------------------------------------------------------
 * Function: H5LTmake_dataset_long
 *
 * Purpose: Creates and writes a dataset of H5T_NATIVE_LONG type
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 14, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */


herr_t H5LTmake_dataset_long( hid_t loc_id,
                              const char *dset_name,
                              int rank,
                              const hsize_t *dims,
                              const long *data )
{

 hid_t   did, sid;

 /* Create the data space for the dataset. */
 if ( (sid = H5Screate_simple( rank, dims, NULL )) < 0 )
  return -1;

 /* Create the dataset. */
 if ( (did = H5Dcreate( loc_id, dset_name, H5T_NATIVE_LONG, sid, H5P_DEFAULT )) < 0 )
  goto out;

 /* Write the dataset only if there is data to write */

 if ( data )
 {
  if ( H5Dwrite( did, H5T_NATIVE_LONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, data ) < 0 )
   goto out;
 }

 /* End access to the dataset and release resources used by it. */
 if ( H5Dclose( did ) < 0 )
  return -1;

 /* Terminate access to the data space. */
 if ( H5Sclose( sid ) < 0 )
  return -1;

 return 0;

out:
 H5Dclose( did );
 H5Sclose( sid );
 return -1;
}

/*-------------------------------------------------------------------------
 * Function: H5LTmake_dataset_float
 *
 * Purpose: Creates and writes a dataset of H5T_NATIVE_FLOAT type
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 14, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */


herr_t H5LTmake_dataset_float( hid_t loc_id,
                               const char *dset_name,
                               int rank,
                               const hsize_t *dims,
                               const float *data )
{

 hid_t   did, sid;

 /* Create the data space for the dataset. */
 if ( (sid = H5Screate_simple( rank, dims, NULL )) < 0 )
  return -1;

 /* Create the dataset. */
 if ( (did = H5Dcreate( loc_id, dset_name, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT )) < 0 )
  goto out;

 /* Write the dataset only if there is data to write */

 if ( data )
 {
  if ( H5Dwrite( did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data ) < 0 )
   goto out;
 }

 /* End access to the dataset and release resources used by it. */
 if ( H5Dclose( did ) < 0 )
  return -1;

 /* Terminate access to the data space. */
 if ( H5Sclose( sid ) < 0 )
  return -1;

 return 0;

out:
 H5Dclose( did );
 H5Sclose( sid );
 return -1;
}



/*-------------------------------------------------------------------------
 * Function: H5LTmake_dataset_double
 *
 * Purpose: Creates and writes a dataset of H5T_NATIVE_DOUBLE type
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 14, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */


herr_t H5LTmake_dataset_double( hid_t loc_id,
                                const char *dset_name,
                                int rank,
                                const hsize_t *dims,
                                const double *data )
{

 hid_t   did, sid;

 /* Create the data space for the dataset. */
 if ( (sid = H5Screate_simple( rank, dims, NULL )) < 0 )
  return -1;

 /* Create the dataset. */
 if ( (did = H5Dcreate( loc_id, dset_name, H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT )) < 0 )
  goto out;

 /* Write the dataset only if there is data to write */

 if ( data )
 {
  if ( H5Dwrite( did, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, data ) < 0 )
   goto out;
 }

 /* End access to the dataset and release resources used by it. */
 if ( H5Dclose( did ) < 0 )
  return -1;

 /* Terminate access to the data space. */
 if ( H5Sclose( sid ) < 0 )
  return -1;

 return 0;

out:
 H5Dclose( did );
 H5Sclose( sid );
 return -1;
}


/*-------------------------------------------------------------------------
 * Function: H5LTmake_dataset_string
 *
 * Purpose: Creates and writes a dataset of H5T_C_S1 type
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */


herr_t H5LTmake_dataset_string(hid_t loc_id,
                               const char *dset_name,
                               const char *buf )
{

 hid_t   did=-1;
 hid_t   sid=-1;
 hid_t   tid;
 size_t  size;

 /* create a string data type */
 if ( (tid = H5Tcopy( H5T_C_S1 )) < 0 )
  goto out;

 size = strlen(buf) + 1; /* extra null term */

 if ( H5Tset_size(tid,size) < 0 )
  goto out;

 if ( H5Tset_strpad(tid,H5T_STR_NULLTERM ) < 0 )
  goto out;

 /* Create the data space for the dataset. */
 if ( (sid = H5Screate( H5S_SCALAR )) < 0 )
  goto out;

 /* Create the dataset. */
 if ( (did = H5Dcreate(loc_id,dset_name,tid,sid,H5P_DEFAULT)) < 0 )
  goto out;

 /* Write the dataset only if there is data to write */

 if (buf)
 {
  if ( H5Dwrite(did,tid,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf) < 0 )
   goto out;
 }

 /* close*/
 if ( H5Dclose(did) < 0 )
  return -1;
 if ( H5Sclose(sid) < 0 )
  return -1;
 if ( H5Tclose(tid) < 0 )
  goto out;

 return 0;

out:
  H5Dclose(did);
  H5Tclose(tid);
  H5Sclose(sid);
 return -1;
}


/*-------------------------------------------------------------------------
 * Function: H5LTread_dataset
 *
 * Purpose: Reads a dataset from disk.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: June 13, 2001
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTread_dataset( hid_t loc_id,
                         const char *dset_name,
                         hid_t tid,
                         void *data )
{
 hid_t   did;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Read */
 if ( H5Dread( did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, data ) < 0 )
  goto out;

 /* End access to the dataset and release resources used by it. */
 if ( H5Dclose( did ) )
  return -1;

 return 0;

out:
 H5Dclose( did );
 return -1;

}


/*-------------------------------------------------------------------------
 * Function: H5LTread_dataset_char
 *
 * Purpose: Reads a dataset from disk.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 5, 2001
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTread_dataset_char( hid_t loc_id,
                              const char *dset_name,
                              char *data )
{
 hid_t   did;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Read */
 if ( H5Dread( did, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, data ) < 0 )
  goto out;

 /* End access to the dataset and release resources used by it. */
 if ( H5Dclose( did ) )
  return -1;

 return 0;

out:
 H5Dclose( did );
 return -1;

}

/*-------------------------------------------------------------------------
 * Function: H5LTread_dataset_short
 *
 * Purpose: Reads a dataset from disk.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 5, 2001
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTread_dataset_short( hid_t loc_id,
                               const char *dset_name,
                               short *data )
{
 hid_t   did;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Read */
 if ( H5Dread( did, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data ) < 0 )
  goto out;

 /* End access to the dataset and release resources used by it. */
 if ( H5Dclose( did ) )
  return -1;

 return 0;

out:
 H5Dclose( did );
 return -1;

}

/*-------------------------------------------------------------------------
 * Function: H5LTread_dataset_int
 *
 * Purpose: Reads a dataset from disk.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 5, 2001
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTread_dataset_int( hid_t loc_id,
                             const char *dset_name,
                             int *data )
{
 hid_t   did;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Read */
 if ( H5Dread( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data ) < 0 )
  goto out;

 /* End access to the dataset and release resources used by it. */
 if ( H5Dclose( did ) )
  return -1;

 return 0;

out:
 H5Dclose( did );
 return -1;

}

/*-------------------------------------------------------------------------
 * Function: H5LTread_dataset_long
 *
 * Purpose: Reads a dataset from disk.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 5, 2001
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTread_dataset_long( hid_t loc_id,
                              const char *dset_name,
                              long *data )
{
 hid_t   did;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Read */
 if ( H5Dread( did, H5T_NATIVE_LONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, data ) < 0 )
  goto out;

 /* End access to the dataset and release resources used by it. */
 if ( H5Dclose( did ) )
  return -1;

 return 0;

out:
 H5Dclose( did );
 return -1;

}

/*-------------------------------------------------------------------------
 * Function: H5LTread_dataset_float
 *
 * Purpose: Reads a dataset from disk.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 5, 2001
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTread_dataset_float( hid_t loc_id,
                               const char *dset_name,
                               float *data )
{
 hid_t   did;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Read */
 if ( H5Dread( did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data ) < 0 )
  goto out;

 /* End access to the dataset and release resources used by it. */
 if ( H5Dclose( did ) )
  return -1;

 return 0;

out:
 H5Dclose( did );
 return -1;

}


/*-------------------------------------------------------------------------
 * Function: H5LTread_dataset_double
 *
 * Purpose: Reads a dataset from disk.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 5, 2001
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTread_dataset_double( hid_t loc_id,
                                const char *dset_name,
                                double *data )
{
 hid_t   did;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Read */
 if ( H5Dread( did, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, data ) < 0 )
  goto out;

 /* End access to the dataset and release resources used by it. */
 if ( H5Dclose( did ) )
  return -1;

 return 0;

out:
 H5Dclose( did );
 return -1;

}


/*-------------------------------------------------------------------------
 * Function: H5LTread_dataset_string
 *
 * Purpose: Reads a dataset
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 05, 2004
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTread_dataset_string( hid_t loc_id,
                                const char *dset_name,
                                char *buf )
{
 hid_t   did;
 hid_t   tid;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 if ( (tid = H5Dget_type(did)) < 0 )
  goto out;

 /* Read */
 if ( H5Dread(did,tid,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf) < 0 )
  goto out;

 /* close */
 if ( H5Dclose(did) )
  goto out;
 if ( H5Tclose(tid) )
  return -1;

 return 0;

out:
 H5Dclose( did );
 H5Tclose( tid );
 return -1;

}


/*-------------------------------------------------------------------------
 * Function: H5LTget_dataset_ndims
 *
 * Purpose: Gets the dimensionality of a dataset.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 4, 2001
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTget_dataset_ndims( hid_t loc_id,
                              const char *dset_name,
                              int *rank )
{
 hid_t       did;
 hid_t       sid;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Get the dataspace handle */
 if ( (sid = H5Dget_space( did )) < 0 )
  goto out;

 /* Get rank */
 if ( (*rank = H5Sget_simple_extent_ndims( sid )) < 0 )
  goto out;

 /* Terminate access to the dataspace */
 if ( H5Sclose( sid ) < 0 )
  goto out;

 /* End access to the dataset */
 if ( H5Dclose( did ) )
  return -1;

 return 0;

out:
 H5Dclose( did );
 H5Sclose( sid );
 return -1;

}


/*-------------------------------------------------------------------------
 * Function: H5LTget_dataset_info
 *
 * Purpose: Gets information about a dataset.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 4, 2001
 *  Modified: February 28, 2006: checked for NULL parameters
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTget_dataset_info( hid_t loc_id,
                             const char *dset_name,
                             hsize_t *dims,
                             H5T_class_t *type_class,
                             size_t *type_size )
{
 hid_t       did;
 hid_t       tid;
 hid_t       sid;

 /* open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* get an identifier for the datatype. */
 tid = H5Dget_type( did );

 /* get the class. */
 if (type_class!=NULL)
  *type_class = H5Tget_class( tid );

 /* get the size. */
 if (type_size!=NULL)
  *type_size = H5Tget_size( tid );

 if (dims!=NULL)
 {
  /* get the dataspace handle */
  if ( (sid = H5Dget_space( did )) < 0 )
   goto out;
  
  /* get dimensions */
  if ( H5Sget_simple_extent_dims( sid, dims, NULL) < 0 )
   goto out;
  
  /* terminate access to the dataspace */
  if ( H5Sclose( sid ) < 0 )
   goto out;
 }

 /* release the datatype. */
 if ( H5Tclose( tid ) )
  return -1;

 /* end access to the dataset */
 if ( H5Dclose( did ) )
  return -1;

 return 0;

out:
 H5Tclose( tid );
 H5Dclose( did );
 return -1;

}

/*-------------------------------------------------------------------------
 * Function: find_dataset
 *
 * Purpose: operator function used by H5LTfind_dataset
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: June 21, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t find_dataset( hid_t loc_id, const char *name, void *op_data)
{

 /* Define a default zero value for return. This will cause the iterator to continue if
  * the dataset is not found yet.
  */

 int ret = 0;

 char *dset_name = (char*)op_data;

 /* Shut the compiler up */
 loc_id=loc_id;

 /* Define a positive value for return value if the dataset was found. This will
  * cause the iterator to immediately return that positive value,
  * indicating short-circuit success
  */

 if( strcmp( name, dset_name ) == 0 )
  ret = 1;


 return ret;
}



/*-------------------------------------------------------------------------
 * Function: H5LTfind_dataset
 *
 * Purpose:  Inquires if a dataset named dset_name exists attached
 *           to the object loc_id.
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 15, 2001
 *
 * Return:
 *     Success: The return value of the first operator that
 *              returns non-zero, or zero if all members were
 *              processed with no operator returning non-zero.
 *
 *      Failure:    Negative if something goes wrong within the
 *              library, or the negative value returned by one
 *              of the operators.
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTfind_dataset( hid_t loc_id, const char *dset_name )
{

 herr_t  ret;

 ret = H5Giterate( loc_id, ".", 0, find_dataset, (void *)dset_name );

 return ret;
}


/*-------------------------------------------------------------------------
 *
 * Set attribute functions
 *
 *-------------------------------------------------------------------------
 */


/*-------------------------------------------------------------------------
 * Function: H5LTset_attribute_string
 *
 * Purpose: Creates and writes a string attribute named attr_name and attaches
 *          it to the object specified by the name obj_name.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 23, 2001
 *
 * Comments: If the attribute already exists, it is overwritten
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTset_attribute_string( hid_t loc_id,
                                 const char *obj_name,
                                 const char *attr_name,
                                 const char *attr_data )
{
 hid_t      attr_type;
 hid_t      attr_space_id;
 hid_t      attr_id;
 hid_t      obj_id;
 int        has_attr;
 H5G_stat_t statbuf;
 size_t     attr_size;


 /* Get the type of object */
 if (H5Gget_objinfo( loc_id, obj_name, 1, &statbuf )<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

 /* Create the attribute */
 if ( (attr_type = H5Tcopy( H5T_C_S1 )) < 0 )
  goto out;

 attr_size = strlen( attr_data ) + 1; /* extra null term */

 if ( H5Tset_size( attr_type, (size_t)attr_size) < 0 )
  goto out;

 if ( H5Tset_strpad( attr_type, H5T_STR_NULLTERM ) < 0 )
  goto out;

 if ( (attr_space_id = H5Screate( H5S_SCALAR )) < 0 )
  goto out;

 /* Verify if the attribute already exists */
 has_attr = H5LT_find_attribute( obj_id, attr_name );

 /* The attribute already exists, delete it */
 if ( has_attr == 1 )
 {
  if ( H5Adelete( obj_id, attr_name ) < 0 )
    goto out;
 }

 /* Create and write the attribute */

 if ( (attr_id = H5Acreate( obj_id, attr_name, attr_type, attr_space_id, H5P_DEFAULT )) < 0 )
  goto out;

 if ( H5Awrite( attr_id, attr_type, attr_data ) < 0 )
  goto out;

 if ( H5Aclose( attr_id ) < 0 )
  goto out;

 if ( H5Sclose( attr_space_id ) < 0 )
  goto out;

 if ( H5Tclose(attr_type) < 0 )
  goto out;

 /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

  return 0;

out:

 H5LT_close_id( obj_id, statbuf.type );
 return -1;
}





/*-------------------------------------------------------------------------
 * Function: H5LT_set_attribute_numerical
 *
 * Purpose: Private function used by H5LTset_attribute_int and H5LTset_attribute_float
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 25, 2001
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5LT_set_attribute_numerical( hid_t loc_id,
                                     const char *obj_name,
                                     const char *attr_name,
                                     size_t size,
                                     hid_t tid,
                                     const void *data )
{

 hid_t      obj_id, sid, attr_id;
 hsize_t    dim_size=size;
 int        has_attr;
 H5G_stat_t statbuf;

 /* Get the type of object */
 if (H5Gget_objinfo(loc_id, obj_name, 1, &statbuf)<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

 /* Create the data space for the attribute. */
 if ( (sid = H5Screate_simple( 1, &dim_size, NULL )) < 0 )
  goto out;

  /* Verify if the attribute already exists */
 has_attr = H5LT_find_attribute( obj_id, attr_name );

 /* The attribute already exists, delete it */
 if ( has_attr == 1 )
 {
  if ( H5Adelete( obj_id, attr_name ) < 0 )
    goto out;
 }

 /* Create the attribute. */
 if ( (attr_id = H5Acreate( obj_id, attr_name, tid, sid, H5P_DEFAULT )) < 0 )
  goto out;

 /* Write the attribute data. */
 if ( H5Awrite( attr_id, tid, data ) < 0 )
  goto out;

 /* Close the attribute. */
 if ( H5Aclose( attr_id ) < 0 )
  goto out;

 /* Close the dataspace. */
 if ( H5Sclose( sid ) < 0 )
  goto out;

 /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

 return 0;

out:
 H5LT_close_id( obj_id, statbuf.type );
 return -1;
}

/*-------------------------------------------------------------------------
 * Function: H5LT_open_id
 *
 * Purpose: Private function used by H5LT_set_attribute_*
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 19, 2002
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LT_open_id( hid_t loc_id,
                     const char *obj_name,
                     int obj_type /*basic object type*/ )
{

 hid_t   obj_id = -1;

 switch ( obj_type )
 {
  case H5G_DATASET:

   /* Open the dataset. */
   if ( (obj_id = H5Dopen( loc_id, obj_name )) < 0 )
    return -1;
   break;

  case H5G_GROUP:

   /* Open the group. */
   if ( (obj_id = H5Gopen( loc_id, obj_name )) < 0 )
    return -1;
   break;

  default:
   return -1;
 }

 return obj_id;

}


/*-------------------------------------------------------------------------
 * Function: H5LT_close_id
 *
 * Purpose: Private function used by H5LT_set_attribute_*
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 19, 2002
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LT_close_id( hid_t obj_id,
                      int obj_type /*basic object type*/ )
{
 switch ( obj_type )
 {
  case H5G_DATASET:
   /* Close the dataset. */
   if ( H5Dclose( obj_id ) < 0 )
    return -1;
   break;

  case H5G_GROUP:
  /* Close the group. */
   if ( H5Gclose( obj_id ) < 0 )
    return -1;
   break;

  default:
   return -1;
 }

 return 0;
}


/*-------------------------------------------------------------------------
 * Function: H5LTset_attribute_char
 *
 * Purpose: Create and write an attribute.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 7, 2001
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTset_attribute_char( hid_t loc_id,
                               const char *obj_name,
                               const char *attr_name,
                               const char *data,
                               size_t size )
{

 if ( H5LT_set_attribute_numerical( loc_id, obj_name, attr_name, size,
      H5T_NATIVE_CHAR, data ) < 0 )
  return -1;

 return 0;
}

/*-------------------------------------------------------------------------
 * Function: H5LTset_attribute_uchar
 *
 * Purpose: Create and write an attribute.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 8, 2004
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTset_attribute_uchar( hid_t loc_id,
                                const char *obj_name,
                                const char *attr_name,
                                const unsigned char *data,
                                size_t size )
{

 if ( H5LT_set_attribute_numerical( loc_id, obj_name, attr_name, size,
      H5T_NATIVE_UCHAR, data ) < 0 )
  return -1;

 return 0;

}

/*-------------------------------------------------------------------------
 * Function: H5LTset_attribute_short
 *
 * Purpose: Create and write an attribute.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 7, 2001
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTset_attribute_short( hid_t loc_id,
                                const char *obj_name,
                                const char *attr_name,
                                const short *data,
                                size_t size )
{

 if ( H5LT_set_attribute_numerical( loc_id, obj_name, attr_name, size,
      H5T_NATIVE_SHORT, data ) < 0 )
  return -1;

 return 0;

}

/*-------------------------------------------------------------------------
 * Function: H5LTset_attribute_ushort
 *
 * Purpose: Create and write an attribute.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 8, 2004
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTset_attribute_ushort( hid_t loc_id,
                                const char *obj_name,
                                const char *attr_name,
                                const unsigned short *data,
                                size_t size )
{

 if ( H5LT_set_attribute_numerical( loc_id, obj_name, attr_name, size,
      H5T_NATIVE_USHORT, data ) < 0 )
  return -1;

 return 0;

}

/*-------------------------------------------------------------------------
 * Function: H5LTset_attribute_int
 *
 * Purpose: Create and write an attribute.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 7, 2001
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTset_attribute_int( hid_t loc_id,
                              const char *obj_name,
                              const char *attr_name,
                              const int *data,
                              size_t size )
{

 if ( H5LT_set_attribute_numerical( loc_id, obj_name, attr_name, size,
      H5T_NATIVE_INT, data ) < 0 )
  return -1;

 return 0;

}

/*-------------------------------------------------------------------------
 * Function: H5LTset_attribute_uint
 *
 * Purpose: Create and write an attribute.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 8, 2004
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTset_attribute_uint( hid_t loc_id,
                              const char *obj_name,
                              const char *attr_name,
                              const unsigned int *data,
                              size_t size )
{

 if ( H5LT_set_attribute_numerical( loc_id, obj_name, attr_name, size,
      H5T_NATIVE_UINT, data ) < 0 )
  return -1;

 return 0;

}


/*-------------------------------------------------------------------------
 * Function: H5LTset_attribute_long
 *
 * Purpose: Create and write an attribute.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 7, 2001
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTset_attribute_long( hid_t loc_id,
                               const char *obj_name,
                               const char *attr_name,
                               const long *data,
                               size_t size )
{

 if ( H5LT_set_attribute_numerical( loc_id, obj_name, attr_name, size,
      H5T_NATIVE_LONG, data ) < 0 )
  return -1;

 return 0;

}

/*-------------------------------------------------------------------------
 * Function: H5LTset_attribute_ulong
 *
 * Purpose: Create and write an attribute.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 8, 2004
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTset_attribute_ulong( hid_t loc_id,
                               const char *obj_name,
                               const char *attr_name,
                               const unsigned long *data,
                               size_t size )
{

 if ( H5LT_set_attribute_numerical( loc_id, obj_name, attr_name, size,
      H5T_NATIVE_ULONG, data ) < 0 )
  return -1;

 return 0;

}


/*-------------------------------------------------------------------------
 * Function: H5LTset_attribute_float
 *
 * Purpose: Create and write an attribute.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 25, 2001
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5LTset_attribute_float( hid_t loc_id,
                                const char *obj_name,
                                const char *attr_name,
                                const float *data,
                                size_t size )
{

 if ( H5LT_set_attribute_numerical( loc_id, obj_name, attr_name, size,
      H5T_NATIVE_FLOAT, data ) < 0 )
  return -1;

 return 0;

}


/*-------------------------------------------------------------------------
 * Function: H5LTset_attribute_double
 *
 * Purpose: Create and write an attribute.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 7, 2001
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTset_attribute_double( hid_t loc_id,
                                 const char *obj_name,
                                 const char *attr_name,
                                 const double *data,
                                 size_t size )
{

 if ( H5LT_set_attribute_numerical( loc_id, obj_name, attr_name, size,
      H5T_NATIVE_DOUBLE, data ) < 0 )
  return -1;

 return 0;

}



/*-------------------------------------------------------------------------
 * Function: find_attr
 *
 * Purpose: operator function used by H5LT_find_attribute
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: June 21, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t find_attr( hid_t loc_id, const char *name, void *op_data)
{

 /* Define a default zero value for return. This will cause the iterator to continue if
  * the palette attribute is not found yet.
  */

 int ret = 0;

 char *attr_name = (char*)op_data;

 /* Shut the compiler up */
 loc_id=loc_id;

 /* Define a positive value for return value if the attribute was found. This will
  * cause the iterator to immediately return that positive value,
  * indicating short-circuit success
  */

 if( strcmp( name, attr_name ) == 0 )
  ret = 1;


 return ret;
}


/*-------------------------------------------------------------------------
 * Function: H5LT_find_attribute
 *
 * Purpose: Inquires if an attribute named attr_name exists attached to the object loc_id.
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: June 21, 2001
 *
 * Comments:
 *  The function uses H5Aiterate with the operator function find_attr
 *
 * Return:
 *  Success: The return value of the first operator that
 *              returns non-zero, or zero if all members were
 *              processed with no operator returning non-zero.
 *
 *  Failure: Negative if something goes wrong within the
 *              library, or the negative value returned by one
 *              of the operators.
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LT_find_attribute( hid_t loc_id, const char* attr_name )
{

 unsigned int attr_num;
 herr_t       ret;

 attr_num = 0;
 ret = H5Aiterate( loc_id, &attr_num, find_attr, (void *)attr_name );

 return ret;
}



/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_ndims
 *
 * Purpose: Gets the dimensionality of an attribute.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 4, 2001
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTget_attribute_ndims( hid_t loc_id,
                                const char *obj_name,
                                const char *attr_name,
                                int *rank )
{
 hid_t       attr_id;
 hid_t       sid;
 H5G_stat_t  statbuf;
 hid_t       obj_id;

 /* Get the type of object */
 if (H5Gget_objinfo( loc_id, obj_name, 1, &statbuf )<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

 /* Open the attribute. */
 if ( ( attr_id = H5Aopen_name( obj_id, attr_name ) ) < 0 )
 {
  H5LT_close_id( obj_id, statbuf.type );
  return -1;
 }

 /* Get the dataspace handle */
 if ( (sid = H5Aget_space( attr_id )) < 0 )
  goto out;

 /* Get rank */
 if ( (*rank = H5Sget_simple_extent_ndims( sid )) < 0 )
  goto out;

 /* Terminate access to the attribute */
 if ( H5Sclose( sid ) < 0 )
  goto out;

 /* End access to the attribute */
 if ( H5Aclose( attr_id ) )
  goto out;;

 /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

 return 0;

out:
 H5Aclose( attr_id );
 H5LT_close_id( obj_id, statbuf.type );
 return -1;

}


/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_info
 *
 * Purpose: Gets information about an attribute.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 4, 2001
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTget_attribute_info( hid_t loc_id,
                               const char *obj_name,
                               const char *attr_name,
                               hsize_t *dims,
                               H5T_class_t *type_class,
                               size_t *type_size )
{
 hid_t       attr_id;
 hid_t       tid;
 hid_t       sid;
 H5G_stat_t  statbuf;
 hid_t       obj_id;

 /* Get the type of object */
 if (H5Gget_objinfo( loc_id, obj_name, 1, &statbuf )<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

  /* Open the attribute. */
 if ( ( attr_id = H5Aopen_name( obj_id, attr_name ) ) < 0 )
 {
  H5LT_close_id( obj_id, statbuf.type );
  return -1;
 }

 /* Get an identifier for the datatype. */
 tid = H5Aget_type( attr_id );

 /* Get the class. */
  *type_class = H5Tget_class( tid );

 /* Get the size. */
  *type_size = H5Tget_size( tid );

  /* Get the dataspace handle */
 if ( (sid = H5Aget_space( attr_id )) < 0 )
  goto out;

 /* Get dimensions */
 if ( H5Sget_simple_extent_dims( sid, dims, NULL) < 0 )
  goto out;

 /* Terminate access to the dataspace */
 if ( H5Sclose( sid ) < 0 )
  goto out;

  /* Release the datatype. */
 if ( H5Tclose( tid ) )
  goto out;

  /* End access to the attribute */
 if ( H5Aclose( attr_id ) )
  goto out;

  /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

 return 0;

out:
 H5Tclose( tid );
 H5Aclose( attr_id );
 H5LT_close_id( obj_id, statbuf.type );
 return -1;

}




/*-------------------------------------------------------------------------
 *
 * Get attribute functions
 *
 *-------------------------------------------------------------------------
 */


/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_string
 *
 * Purpose: Reads an attribute named attr_name
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 19, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5LTget_attribute_string( hid_t loc_id,
                                 const char *obj_name,
                                 const char *attr_name,
                                 char *data )
{

 /* identifiers */
 hid_t      obj_id;
 H5G_stat_t statbuf;

 /* Get the type of object */
 if (H5Gget_objinfo(loc_id, obj_name, 1, &statbuf)<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

 /* Get the attribute */
 if ( H5LT_get_attribute_disk( obj_id, attr_name, data ) < 0 )
  return -1;

 /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

 return 0;

}

/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_char
 *
 * Purpose: Reads an attribute named attr_name
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 19, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5LTget_attribute_char( hid_t loc_id,
                                const char *obj_name,
                                const char *attr_name,
                                char *data )
{

 /* identifiers */
 hid_t      obj_id;
 H5G_stat_t statbuf;

 /* Get the type of object */
 if (H5Gget_objinfo(loc_id, obj_name, 1, &statbuf)<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

 /* Get the attribute */
 if ( H5LT_get_attribute_mem( obj_id, attr_name, H5T_NATIVE_CHAR, data ) < 0 )
  return -1;

 /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

 return 0;

}

/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_uchar
 *
 * Purpose: Reads an attribute named attr_name
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 8, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5LTget_attribute_uchar( hid_t loc_id,
                                const char *obj_name,
                                const char *attr_name,
                                unsigned char *data )
{

 /* identifiers */
 hid_t      obj_id;
 H5G_stat_t statbuf;

 /* Get the type of object */
 if (H5Gget_objinfo(loc_id, obj_name, 1, &statbuf)<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

 /* Get the attribute */
 if ( H5LT_get_attribute_mem( obj_id, attr_name, H5T_NATIVE_UCHAR, data ) < 0 )
  return -1;

 /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

 return 0;

}



/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_short
 *
 * Purpose: Reads an attribute named attr_name
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 19, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5LTget_attribute_short( hid_t loc_id,
                                const char *obj_name,
                                const char *attr_name,
                                short *data )
{

 /* identifiers */
 hid_t      obj_id;
 H5G_stat_t statbuf;

 /* Get the type of object */
 if (H5Gget_objinfo(loc_id, obj_name, 1, &statbuf)<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

 /* Get the attribute */
 if ( H5LT_get_attribute_mem( obj_id, attr_name, H5T_NATIVE_SHORT, data ) < 0 )
  return -1;

 /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

 return 0;

}

/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_ushort
 *
 * Purpose: Reads an attribute named attr_name
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 8, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5LTget_attribute_ushort( hid_t loc_id,
                                const char *obj_name,
                                const char *attr_name,
                                unsigned short *data )
{

 /* identifiers */
 hid_t      obj_id;
 H5G_stat_t statbuf;

 /* Get the type of object */
 if (H5Gget_objinfo(loc_id, obj_name, 1, &statbuf)<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

 /* Get the attribute */
 if ( H5LT_get_attribute_mem( obj_id, attr_name, H5T_NATIVE_USHORT, data ) < 0 )
  return -1;

 /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

 return 0;

}



/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_int
 *
 * Purpose: Reads an attribute named attr_name
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 19, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5LTget_attribute_int( hid_t loc_id,
                              const char *obj_name,
                              const char *attr_name,
                              int *data )
{

 /* identifiers */
 hid_t      obj_id;
 H5G_stat_t statbuf;

 /* Get the type of object */
 if (H5Gget_objinfo(loc_id, obj_name, 1, &statbuf)<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

 /* Get the attribute */
 if ( H5LT_get_attribute_mem( obj_id, attr_name, H5T_NATIVE_INT, data ) < 0 )
  return -1;

 /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

 return 0;

}

/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_uint
 *
 * Purpose: Reads an attribute named attr_name
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 8, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5LTget_attribute_uint( hid_t loc_id,
                              const char *obj_name,
                              const char *attr_name,
                              unsigned int *data )
{

 /* identifiers */
 hid_t      obj_id;
 H5G_stat_t statbuf;

 /* Get the type of object */
 if (H5Gget_objinfo(loc_id, obj_name, 1, &statbuf)<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

 /* Get the attribute */
 if ( H5LT_get_attribute_mem( obj_id, attr_name, H5T_NATIVE_UINT, data ) < 0 )
  return -1;

 /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

 return 0;

}



/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_long
 *
 * Purpose: Reads an attribute named attr_name
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 19, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5LTget_attribute_long( hid_t loc_id,
                              const char *obj_name,
                              const char *attr_name,
                              long *data )
{

 /* identifiers */
 hid_t      obj_id;
 H5G_stat_t statbuf;

 /* Get the type of object */
 if (H5Gget_objinfo(loc_id, obj_name, 1, &statbuf)<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

 /* Get the attribute */
 if ( H5LT_get_attribute_mem( obj_id, attr_name, H5T_NATIVE_LONG, data ) < 0 )
  return -1;

 /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

 return 0;

}

/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_ulong
 *
 * Purpose: Reads an attribute named attr_name
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 8, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5LTget_attribute_ulong( hid_t loc_id,
                              const char *obj_name,
                              const char *attr_name,
                              unsigned long *data )
{

 /* identifiers */
 hid_t      obj_id;
 H5G_stat_t statbuf;

 /* Get the type of object */
 if (H5Gget_objinfo(loc_id, obj_name, 1, &statbuf)<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

 /* Get the attribute */
 if ( H5LT_get_attribute_mem( obj_id, attr_name, H5T_NATIVE_ULONG, data ) < 0 )
  return -1;

 /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

 return 0;

}


/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_float
 *
 * Purpose: Reads an attribute named attr_name
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 19, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5LTget_attribute_float( hid_t loc_id,
                                const char *obj_name,
                                const char *attr_name,
                                float *data )
{

 /* identifiers */
 hid_t      obj_id;
 H5G_stat_t statbuf;

 /* Get the type of object */
 if (H5Gget_objinfo(loc_id, obj_name, 1, &statbuf)<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

 /* Get the attribute */
 if ( H5LT_get_attribute_mem( obj_id, attr_name, H5T_NATIVE_FLOAT, data ) < 0 )
  return -1;

 /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

 return 0;

}


/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_double
 *
 * Purpose: Reads an attribute named attr_name
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 19, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5LTget_attribute_double( hid_t loc_id,
                                 const char *obj_name,
                                 const char *attr_name,
                                 double *data )
{

 /* identifiers */
 hid_t      obj_id;
 H5G_stat_t statbuf;

 /* Get the type of object */
 if (H5Gget_objinfo(loc_id, obj_name, 1, &statbuf)<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

 /* Get the attribute */
 if ( H5LT_get_attribute_mem( obj_id, attr_name, H5T_NATIVE_DOUBLE, data ) < 0 )
  return -1;

 /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

 return 0;

}


/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute
 *
 * Purpose: Reads an attribute named attr_name with the memory type mem_type_id
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 19, 2002
 *
 * Comments: Private function
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5LTget_attribute( hid_t loc_id,
                          const char *obj_name,
                          const char *attr_name,
                          hid_t mem_type_id,
                          void *data )
{

 /* identifiers */
 hid_t      obj_id;
 H5G_stat_t statbuf;

 /* Get the type of object */
 if (H5Gget_objinfo(loc_id, obj_name, 1, &statbuf)<0)
  return -1;

 /* Open the object */
 if ((obj_id = H5LT_open_id( loc_id, obj_name, statbuf.type )) < 0)
  return -1;

 /* Get the attribute */
 if ( H5LT_get_attribute_mem( obj_id, attr_name, mem_type_id, data ) < 0 )
 {
  H5LT_close_id( obj_id, statbuf.type );
  return -1;
 }

 /* Close the object */
 if ( H5LT_close_id( obj_id, statbuf.type ) < 0 )
  return -1;

 return 0;
}


/*-------------------------------------------------------------------------
 * Function: H5LT_get_attribute_mem
 *
 * Purpose: Reads an attribute named attr_name with the memory type mem_type_id
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 19, 2002
 *
 * Comments: Private function
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5LT_get_attribute_mem( hid_t obj_id,
                           const char *attr_name,
                           hid_t mem_type_id,
                           void *data )
{

 /* identifiers */
 hid_t attr_id;

 if ( ( attr_id = H5Aopen_name( obj_id, attr_name ) ) < 0 )
  return -1;

 if ( H5Aread( attr_id, mem_type_id, data ) < 0 )
  goto out;

 if ( H5Aclose( attr_id ) < 0 )
  return -1;

 return 0;

out:
 H5Aclose( attr_id );
 return -1;
}

/*-------------------------------------------------------------------------
 * Function: H5LT_get_attribute_disk
 *
 * Purpose: Reads an attribute named attr_name with the datatype stored on disk
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 19, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LT_get_attribute_disk( hid_t loc_id,
                          const char *attr_name,
                          void *attr_out )
{
 /* identifiers */
 hid_t      attr_id;
 hid_t      attr_type;

 if ( ( attr_id = H5Aopen_name( loc_id, attr_name ) ) < 0 )
  return -1;

 if ( (attr_type = H5Aget_type( attr_id )) < 0 )
  goto out;

 if ( H5Aread( attr_id, attr_type, attr_out ) < 0 )
  goto out;

 if ( H5Tclose( attr_type )  < 0 )
  goto out;

 if ( H5Aclose( attr_id ) < 0 )
  return -1;;

 return 0;

out:
 H5Tclose( attr_type );
 H5Aclose( attr_id );
 return -1;
}




