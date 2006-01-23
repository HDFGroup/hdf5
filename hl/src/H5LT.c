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

#include "H5LT.h"
#include <string.h>
#include <stdlib.h>
#include <assert.h>

/* For Lex and Yacc */
#define         SUCCEED         0
#define         FAIL            -1
#define         COL             3
#define         LIMIT           512
#define         INCREMENT       1024
#define         MAX(a,b)        (((a)>(b)) ? (a) : (b))
int  input_len;
char *myinput;
int  indent = 0;

/*-------------------------------------------------------------------------
 *
 * internal functions
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

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Get an identifier for the datatype. */
 tid = H5Dget_type( did );

 /* Get the class. */
 *type_class = H5Tget_class( tid );

 /* Get the size. */
 *type_size = H5Tget_size( tid );

  /* Get the dataspace handle */
 if ( (sid = H5Dget_space( did )) < 0 )
  goto out;

 /* Get dimensions */
 if ( H5Sget_simple_extent_dims( sid, dims, NULL) < 0 )
  goto out;

 /* Terminate access to the dataspace */
 if ( H5Sclose( sid ) < 0 )
  goto out;

  /* Release the datatype. */
 if ( H5Tclose( tid ) )
  return -1;

 /* End access to the dataset */
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
 * Function: H5LTset_attribute_long_long
 *
 * Purpose: Create and write an attribute.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Elena Pourmal, epourmal@ncsa.uiuc.edu
 *
 * Date: June 17, 2005
 *
 * Comments: This function was added to support attributes of type long long
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTset_attribute_long_long( hid_t loc_id,
                               const char *obj_name,
                               const char *attr_name,
                               const long_long *data,
                               size_t size )
{

 if ( H5LT_set_attribute_numerical( loc_id, obj_name, attr_name, size,
      H5T_NATIVE_LLONG, data ) < 0 )
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
 * General functions
 *
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: H5LTcreate_compound_type
 *
 * Purpose:
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 18, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

hid_t H5LTcreate_compound_type( hsize_t nfields, size_t size, const char *field_names[],
                                const size_t *field_offset, const hid_t *field_types )
{

 hid_t   tid;
 hsize_t i;

 /* Create the memory data type. */
 if ((tid = H5Tcreate (H5T_COMPOUND, size )) < 0 )
  goto out;

 /* Insert fields. */
 for ( i = 0; i < nfields; i++)
 {
  if ( H5Tinsert(tid, field_names[i], field_offset[i], field_types[i] ) < 0 )
   goto out;
 }

 return tid;

out:
 return -1;
}

/*-------------------------------------------------------------------------
 * Function: H5LTtext_to_dtype
 *
 * Purpose:  Convert DDL description to HDF5 data type.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Raymond Lu, slu@ncsa.uiuc.edu
 *
 * Date: October 6, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t H5LTtext_to_dtype(const char *text, H5LT_lang_t lang_type)
{
 extern int yyparse(void);
 hid_t   type_id; 

 if(lang_type <= H5LT_LANG_ERR || lang_type >= H5LT_NO_LANG)
     goto out;

 if(lang_type != H5LT_DDL) {
     fprintf(stderr, "only DDL is supported for now.\n");
     goto out;
 }
 
 input_len = strlen(text);
 myinput = strdup(text);

 if((type_id = yyparse())<0)
     goto out;

 free(myinput);
 input_len = 0;

 return type_id;

out:
 return -1;
}

/*-------------------------------------------------------------------------
 * Function:    indentation
 *
 * Purpose:     Print spaces for indentation
 *
 * Return:      void
 *
 * Programmer:  Raymond Lu, slu@ncsa.uiuc.edu
 *
 * Date:        December 6, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
indentation(int x, char* str)
{
    if (x < 80) {
        while (x-- > 0)
            strcat(str, " ");
    } else {
        strcat(str, "error: the indentation exceeds the number of cols.");
        exit(1);
    }
}

/*-------------------------------------------------------------------------
 * Function:    print_enum
 *
 * Purpose:     prints the enum data
 *
 * Return:      Success: 0, Failure: -1
 *
 * Programmer:  Raymond Lu
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static herr_t
print_enum(hid_t type, char* str, int indt)
{
    char           **name = NULL;   /*member names                   */
    unsigned char   *value = NULL;  /*value array                    */
    unsigned char   *copy = NULL;   /*a pointer to value array       */
    int              nmembs;        /*number of members              */
    char             tmp_str[256];
    int              nchars;        /*number of output characters    */
    hid_t            super;         /*enum base integer type         */
    hid_t            native = -1;   /*native integer data type       */
    size_t           super_size;    /*enum base type size            */
    size_t           dst_size;      /*destination value type size    */
    int              i;
    herr_t           ret = SUCCEED;
    
    if((nmembs = H5Tget_nmembers(type))==0)
        goto out;
    assert(nmembs>0);
    if((super = H5Tget_super(type))<0)
        goto out;
   
    /* Use buffer of INT or UNSIGNED INT to print enum values because
     * we don't expect these values to be so big that INT or UNSIGNED
     * INT can't hold.
     */ 
    if (H5T_SGN_NONE == H5Tget_sign(super)) {
        native = H5T_NATIVE_UINT;
    } else {
        native = H5T_NATIVE_INT;
    }

    super_size = H5Tget_size(super);
    dst_size = H5Tget_size(native);
    
    /* Get the names and raw values of all members */
    name = (char**)calloc((size_t)nmembs, sizeof(char *));
    value = (unsigned char*)calloc((size_t)nmembs, MAX(dst_size, super_size));

    for (i = 0; i < nmembs; i++) {
	if((name[i] = H5Tget_member_name(type, i))==NULL)
            goto out;
	if(H5Tget_member_value(type, i, value + i * super_size)<0)
            goto out;
    }

    /* Convert values to native data type */
    if (native > 0) {
        if(H5Tconvert(super, native, nmembs, value, NULL, H5P_DEFAULT)<0)
            goto out;
    }
    
    /*
     * Sort members by increasing value
     *    ***not implemented yet***
     */

    /* Print members */
    for (i = 0; i < nmembs; i++) {
	indentation(indt + COL, str);
	nchars = sprintf(tmp_str, "\"%s\"", name[i]);
        strcat(str, tmp_str);
	sprintf(tmp_str, "%*s   ", MAX(0, 16 - nchars), "");
        strcat(str, tmp_str);
        
	if (H5T_SGN_NONE == H5Tget_sign(native)) {
	    /*On SGI Altix(cobalt), wrong values were printed out with "value+i*dst_size"
	     *strangely, unless use another pointer "copy".*/
	    copy = value+i*dst_size;
	    sprintf(tmp_str,"%u", *((unsigned int*)((void *)copy)));
            strcat(str, tmp_str);
	} else {
	    /*On SGI Altix(cobalt), wrong values were printed out with "value+i*dst_size"
	     *strangely, unless use another pointer "copy".*/
	    copy = value+i*dst_size;
	    sprintf(tmp_str,"%d", *((int*)((void *)copy)));
            strcat(str, tmp_str);
	}

	strcat(str, ";\n");
    }

    /* Release resources */
    for (i = 0; i < nmembs; i++)
	free(name[i]);

    free(name);
    free(value);
    H5Tclose(super);

    if (0 == nmembs) {
	sprintf(tmp_str, "\n%*s <empty>", indt + 4, "");
        strcat(str, tmp_str);
    }

    return ret;

out:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    H5LTdtype_to_text
 *
 * Purpose:     Convert HDF5 data type to DDL description.
 *
 * Return:      Success: 0, Failure: -1
 *
 * Programmer:  Raymond Lu, slu@ncsa.uiuc.edu
 *
 * Date:        December 6, 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5LTdtype_to_text(hid_t dtype, char *str, H5LT_lang_t lang_type, size_t *len)
{
    size_t      str_len = INCREMENT;
    char        *text_str;
    herr_t      ret;
   
    if(lang_type <= H5LT_LANG_ERR || lang_type >= H5LT_NO_LANG)
        goto out;

    if(len && !str) {
        text_str = (char*)calloc(str_len, sizeof(char));
        text_str[0]='\0';
        if((ret = H5LT_dtype_to_text(dtype, &text_str, lang_type, &str_len, 1))<0)
            goto out;
        *len = strlen(text_str) + 1;
    } else if(len && str) {
        if((ret = H5LT_dtype_to_text(dtype, &str, lang_type, len, 0))<0)
            goto out;
        str[*len-1] = '\0';
    }

    return ret;

out:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    H5LT_dtype_to_text
 *
 * Purpose:     Private function to convert HDF5 data type to DDL description.
 *
 * Return:      Success: 0, Failure: -1
 *
 * Programmer:  Raymond Lu, slu@ncsa.uiuc.edu
 *
 * Date:        December 20, 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5LT_dtype_to_text(hid_t dtype, char **dt_str, H5LT_lang_t lang, size_t *slen, 
                            hbool_t no_user_buf)
{
    H5T_class_t tcls;
    char        tmp_str[256];
    char        *tmp;
    int         i;
    herr_t      ret = SUCCEED;

    if(no_user_buf && ((*slen - strlen(*dt_str)) < LIMIT)) {
        *slen += INCREMENT;
        tmp = (char*)realloc(*dt_str, *slen);
        if(tmp != *dt_str) {
           free(*dt_str);
           *dt_str = tmp;
        }
    }
   
    if(lang != H5LT_DDL) {
        sprintf(*dt_str, "only DDL is supported for now");
        goto out;
    }
    
    if((tcls = H5Tget_class(dtype))<0)
        goto out;
    
    switch (tcls) {
        case H5T_INTEGER:
            if (H5Tequal(dtype, H5T_STD_I8BE)) {
                sprintf(*dt_str, "H5T_STD_I8BE");
            } else if (H5Tequal(dtype, H5T_STD_I8LE)) {
                sprintf(*dt_str, "H5T_STD_I8LE");
            } else if (H5Tequal(dtype, H5T_STD_I16BE)) {
                sprintf(*dt_str, "H5T_STD_I16BE");
            } else if (H5Tequal(dtype, H5T_STD_I16LE)) {
                sprintf(*dt_str, "H5T_STD_I16LE");
            } else if (H5Tequal(dtype, H5T_STD_I32BE)) {
                sprintf(*dt_str, "H5T_STD_I32BE");
            } else if (H5Tequal(dtype, H5T_STD_I32LE)) {
                sprintf(*dt_str, "H5T_STD_I32LE");
            } else if (H5Tequal(dtype, H5T_STD_I64BE)) {
                sprintf(*dt_str, "H5T_STD_I64BE");
            } else if (H5Tequal(dtype, H5T_STD_I64LE)) {
                sprintf(*dt_str, "H5T_STD_I64LE");
            } else if (H5Tequal(dtype, H5T_STD_U8BE)) {
                sprintf(*dt_str, "H5T_STD_U8BE");
            } else if (H5Tequal(dtype, H5T_STD_U8LE)) {
                sprintf(*dt_str, "H5T_STD_U8LE");
            } else if (H5Tequal(dtype, H5T_STD_U16BE)) {
                sprintf(*dt_str, "H5T_STD_U16BE");
            } else if (H5Tequal(dtype, H5T_STD_U16LE)) {
                sprintf(*dt_str, "H5T_STD_U16LE");
            } else if (H5Tequal(dtype, H5T_STD_U32BE)) {
                sprintf(*dt_str, "H5T_STD_U32BE");
            } else if (H5Tequal(dtype, H5T_STD_U32LE)) {
                sprintf(*dt_str, "H5T_STD_U32LE");
            } else if (H5Tequal(dtype, H5T_STD_U64BE)) {
                sprintf(*dt_str, "H5T_STD_U64BE");
            } else if (H5Tequal(dtype, H5T_STD_U64LE)) {
                sprintf(*dt_str, "H5T_STD_U64LE");
            } else if (H5Tequal(dtype, H5T_NATIVE_SCHAR)) {
                sprintf(*dt_str, "H5T_NATIVE_SCHAR");
            } else if (H5Tequal(dtype, H5T_NATIVE_UCHAR)) {
                sprintf(*dt_str, "H5T_NATIVE_UCHAR");
            } else if (H5Tequal(dtype, H5T_NATIVE_SHORT)) {
                sprintf(*dt_str, "H5T_NATIVE_SHORT");
            } else if (H5Tequal(dtype, H5T_NATIVE_USHORT)) {
                sprintf(*dt_str, "H5T_NATIVE_USHORT");
            } else if (H5Tequal(dtype, H5T_NATIVE_INT)) {
                sprintf(*dt_str, "H5T_NATIVE_INT");
            } else if (H5Tequal(dtype, H5T_NATIVE_UINT)) {
                sprintf(*dt_str, "H5T_NATIVE_UINT");
            } else if (H5Tequal(dtype, H5T_NATIVE_LONG)) {
                sprintf(*dt_str, "H5T_NATIVE_LONG");
            } else if (H5Tequal(dtype, H5T_NATIVE_ULONG)) {
                sprintf(*dt_str, "H5T_NATIVE_ULONG");
            } else if (H5Tequal(dtype, H5T_NATIVE_LLONG)) {
                sprintf(*dt_str, "H5T_NATIVE_LLONG");
            } else if (H5Tequal(dtype, H5T_NATIVE_ULLONG)) {
                sprintf(*dt_str, "H5T_NATIVE_ULLONG");
            } else {
                sprintf(*dt_str, "undefined integer");
            }

            break;
        case H5T_FLOAT:
            if (H5Tequal(dtype, H5T_IEEE_F32BE)) {
                sprintf(*dt_str, "H5T_IEEE_F32BE");
            } else if (H5Tequal(dtype, H5T_IEEE_F32LE)) {
                sprintf(*dt_str, "H5T_IEEE_F32LE");
            } else if (H5Tequal(dtype, H5T_IEEE_F64BE)) {
                sprintf(*dt_str, "H5T_IEEE_F64BE");
            } else if (H5Tequal(dtype, H5T_IEEE_F64LE)) {
                sprintf(*dt_str, "H5T_IEEE_F64LE");
            } else if (H5Tequal(dtype, H5T_NATIVE_FLOAT)) {
                sprintf(*dt_str, "H5T_NATIVE_FLOAT");
            } else if (H5Tequal(dtype, H5T_NATIVE_DOUBLE)) {
                sprintf(*dt_str, "H5T_NATIVE_DOUBLE");
#if H5_SIZEOF_LONG_DOUBLE !=0
            } else if (H5Tequal(dtype, H5T_NATIVE_LDOUBLE)) {
                sprintf(*dt_str, "H5T_NATIVE_LDOUBLE");
#endif
            } else {
                sprintf(*dt_str, "undefined float");
            }

            break;
        case H5T_STRING:
            {
            /* Make a copy of type in memory in case when DTYPE is on disk, the size
             * will be bigger than in memory.  This makes it easier to compare
             * types in memory. */
            hid_t str_type;
            H5T_order_t order;
            hid_t tmp_type;
            size_t size;
            H5T_str_t str_pad;
            H5T_cset_t cset;
            htri_t is_vlstr;

            if((tmp_type = H5Tcopy(dtype))<0)
                goto out;
            if((size = H5Tget_size(tmp_type))==0)
                goto out;
            if((str_pad = H5Tget_strpad(tmp_type))<0)
                goto out;
            if((cset = H5Tget_cset(tmp_type))<0)
                goto out;
            if((is_vlstr = H5Tis_variable_str(tmp_type))<0)
                goto out;

            /* Print lead-in */
            sprintf(*dt_str, "H5T_STRING {\n");
            indent += COL;

            indentation(indent + COL, *dt_str);

            if(is_vlstr)
                strcat(*dt_str, "STRSIZE H5T_VARIABLE;\n");
            else {
                sprintf(tmp_str, "STRSIZE %d;\n", (int)size);
                strcat(*dt_str, tmp_str);
            }

            indentation(indent + COL, *dt_str);

            if (str_pad == H5T_STR_NULLTERM)
                strcat(*dt_str, "STRPAD H5T_STR_NULLTERM;\n");
            else if (str_pad == H5T_STR_NULLPAD)
                strcat(*dt_str, "STRPAD H5T_STR_NULLPAD;\n");
            else if (str_pad == H5T_STR_SPACEPAD)
                strcat(*dt_str, "STRPAD H5T_STR_SPACEPAD;\n");
            else
                strcat(*dt_str, "STRPAD H5T_STR_ERROR;\n");

            indentation(indent + COL, *dt_str);

            if (cset == H5T_CSET_ASCII)
                strcat(*dt_str, "CSET H5T_CSET_ASCII;\n");
            else if (cset == H5T_CSET_UTF8)
                strcat(*dt_str, "CSET H5T_CSET_UTF8;\n");
            else
                strcat(*dt_str, "CSET unknown;\n");


            /* Reproduce a C type string */
            if((str_type = H5Tcopy(H5T_C_S1))<0)
                goto out;
            if(is_vlstr) {
                if(H5Tset_size(str_type, H5T_VARIABLE)<0)
                    goto out;
            } else {
                if(H5Tset_size(str_type, size)<0)
                    goto out;
            }
            if(H5Tset_cset(str_type, cset)<0)
                goto out;
            if(H5Tset_strpad(str_type, str_pad)<0)
                goto out;

            indentation(indent + COL, *dt_str);

            /* Check C variable-length string first. Are the two types equal? */
            if (H5Tequal(tmp_type, str_type)) {
                strcat(*dt_str, "CTYPE H5T_C_S1;\n");
                goto next;
            } 

            /* Change the endianness and see if they're equal. */
            if((order = H5Tget_order(tmp_type))<0)
                goto out;
            if(order==H5T_ORDER_LE) {
                if(H5Tset_order(str_type, H5T_ORDER_LE)<0)
                    goto out;
            } else if(order==H5T_ORDER_BE) {
                if(H5Tset_order(str_type, H5T_ORDER_BE)<0)
                    goto out;
            }
            
            if (H5Tequal(tmp_type, str_type)) {
                strcat(*dt_str, "H5T_C_S1;\n");
                goto next;
            }

            /* If not equal to C variable-length string, check Fortran type.
             * Actually H5Tequal can't tell difference between H5T_C_S1 and H5T_FORTRAN_S1!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
            if(H5Tclose(str_type)<0)
                goto out;
            if((str_type = H5Tcopy(H5T_FORTRAN_S1))<0)
                goto out;
            if(H5Tset_cset(str_type, cset)<0)
                goto out;
            if(H5Tset_size(str_type, size)<0)
                goto out;
            if(H5Tset_strpad(str_type, str_pad)<0)
                goto out;
            
            /* Are the two types equal? */
            if (H5Tequal(tmp_type, str_type)) {
                strcat(*dt_str, "CTYPE H5T_FORTRAN_S1;\n");
                goto next;
            }

            /* Change the endianness and see if they're equal. */
            if((order = H5Tget_order(tmp_type))<0)
                goto out;
            if(order==H5T_ORDER_LE) {
                if(H5Tset_order(str_type, H5T_ORDER_LE)<0)
                    goto out;
            } else if(order==H5T_ORDER_BE) {
                if(H5Tset_order(str_type, H5T_ORDER_BE)<0)
                    goto out;
            }

            /* Are the two types equal? */
            if (H5Tequal(tmp_type, str_type)) {
                strcat(*dt_str, "CTYPE H5T_FORTRAN_S1;\n");
                goto next;
            }

            /* Type doesn't match any of above. */
            strcat(*dt_str, "CTYPE unknown_one_character_type;\n ");

next:
            H5Tclose(str_type);
            H5Tclose(tmp_type);

            /* Print closing */
            indent -= COL;
            indentation(indent + COL, *dt_str);
            strcat(*dt_str, "}");

            break;
            }
        case H5T_OPAQUE:
            /* Print lead-in */
            sprintf(*dt_str, "H5T_OPAQUE {\n");
            indent += COL;
            
            indentation(indent + COL, *dt_str);
            sprintf(tmp_str, "OPQ_SIZE %d;\n", H5Tget_size(dtype));
            strcat(*dt_str, tmp_str);

            indentation(indent + COL, *dt_str);
            sprintf(tmp_str, "OPQ_TAG \"%s\";\n", H5Tget_tag(dtype));
            strcat(*dt_str, tmp_str);
            
            /* Print closing */
            indent -= COL;
            indentation(indent + COL, *dt_str);
            strcat(*dt_str, "}");

            break;
        case H5T_ENUM:
            {
            hid_t super;
            size_t super_len;
            char* stmp;

            /* Print lead-in */
            sprintf(*dt_str, "H5T_ENUM {\n");
            indent += COL;
            indentation(indent + COL, *dt_str);
            
            if((super = H5Tget_super(dtype))<0)
                goto out;
            if(H5LTdtype_to_text(super, NULL, lang, &super_len)<0)
                goto out;
            stmp = (char*)calloc(super_len, sizeof(char));
            if(H5LTdtype_to_text(super, stmp, lang, &super_len)<0)
                goto out;            
            strcat(*dt_str, stmp);
            free(stmp);
            strcat(*dt_str, ";\n");
            H5Tclose(super);

            if(print_enum(dtype, *dt_str, indent)<0)
                goto out;

            /* Print closing */
            indent -= COL;
            indentation(indent + COL, *dt_str);
            strcat(*dt_str, "}");

            break;
            }
        case H5T_VLEN:
            {
            hid_t super;
            size_t super_len;
            char* stmp;

            /* Print lead-in */
            sprintf(*dt_str, "H5T_VLEN {\n");
            indent += COL;
            indentation(indent + COL, *dt_str);

            if((super = H5Tget_super(dtype))<0)
                goto out;
            if(H5LTdtype_to_text(super, NULL, lang, &super_len)<0)
                goto out;
            stmp = (char*)calloc(super_len, sizeof(char));
            if(H5LTdtype_to_text(super, stmp, lang, &super_len)<0)
                goto out;            
            strcat(*dt_str, stmp);
            free(stmp);
            strcat(*dt_str, "\n");
            H5Tclose(super);

            /* Print closing */
            indent -= COL;
            indentation(indent + COL, *dt_str);
            strcat(*dt_str, "}");

            break;
            }
        case H5T_ARRAY:
            {
            hid_t       super;
            size_t      super_len;
            char*       stmp;
            hsize_t     dims[H5S_MAX_RANK];
            int         ndims;

            /* Print lead-in */
            sprintf(*dt_str, "H5T_ARRAY {\n");
            indent += COL;
            indentation(indent + COL, *dt_str);

            /* Get array information */
            if((ndims = H5Tget_array_ndims(dtype))<0)
                goto out;
            if(H5Tget_array_dims(dtype, dims, NULL)<0)
                goto out;

            /* Print array dimensions */
            for (i = 0; i < ndims; i++) {
                sprintf(tmp_str, "[%d]", (int) dims[i]);
                strcat(*dt_str, tmp_str);
            }
            strcat(*dt_str, " ");

            if((super = H5Tget_super(dtype))<0)
                goto out;
            if(H5LTdtype_to_text(super, NULL, lang, &super_len)<0)
                goto out;
            stmp = (char*)calloc(super_len, sizeof(char));
            if(H5LTdtype_to_text(super, stmp, lang, &super_len)<0)
                goto out;            
            strcat(*dt_str, stmp);
            free(stmp);
            strcat(*dt_str, "\n");
            H5Tclose(super);

            /* Print closing */
            indent -= COL;
            indentation(indent + COL, *dt_str);
            strcat(*dt_str, "}");

            break;
            }
        case H5T_COMPOUND:
            {
            char       *mname;
            hid_t       mtype;
            size_t      moffset;
            H5T_class_t mclass;
            size_t      mlen;
            char*       mtmp;
            int         nmembs;
           
            if((nmembs = H5Tget_nmembers(dtype))<0)
                goto out;

            sprintf(*dt_str, "H5T_COMPOUND {\n");
            indent += COL;

            for (i = 0; i < nmembs; i++) {
                if((mname = H5Tget_member_name(dtype, i))==NULL)
                    goto out;
                if((mtype = H5Tget_member_type(dtype, i))<0)
                    goto out;
                moffset = H5Tget_member_offset(dtype, i);
                indentation(indent + COL, *dt_str);

                if((mclass = H5Tget_class(mtype))<0)
                    goto out;
                if (H5T_COMPOUND == mclass)
                    indent += COL;

                if(H5LTdtype_to_text(mtype, NULL, lang, &mlen)<0)
                    goto out;
                mtmp = (char*)calloc(mlen, sizeof(char));
                if(H5LTdtype_to_text(mtype, mtmp, lang, &mlen)<0)
                    goto out;            
                strcat(*dt_str, mtmp);
                free(mtmp);

                if (H5T_COMPOUND == mclass)
                    indent -= COL;

                sprintf(tmp_str, " \"%s\"", mname);
                strcat(*dt_str, tmp_str);
                free(mname);

                sprintf(tmp_str, " : %d;\n", moffset);
                strcat(*dt_str, tmp_str);
            }

            /* Print closing */
            indent -= COL;
            indentation(indent + COL, *dt_str);
            strcat(*dt_str, "}");

            break;
            }
        case H5T_TIME:
            sprintf(*dt_str, "H5T_TIME: not yet implemented");
            break;
        case H5T_BITFIELD:
            sprintf(*dt_str, "H5T_BITFIELD: not yet implemented");
            break;
        default:
            sprintf(*dt_str, "unknown data type");
    }

    return ret;

out:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function: H5LTrepack
 *
 * Purpose: Packs/Unpacks data from buffers. This function transfers data from a packed
 * data, src_buf, to a "natural byte aligned" (an n-byte item at an n-byte boundary)
 * data, dst_buf, and vice-versa.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: January 17, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

herr_t H5LTrepack( hsize_t nfields,
                   hsize_t nrecords,
                   size_t src_size,
                   const size_t *src_offset,
                   const size_t *src_sizes,
                   size_t dst_size,
                   const size_t *dst_offset,
                   const size_t *dst_sizes,
                   unsigned char *src_buf,
                   unsigned char *dst_buf )
{
 hsize_t   i, j;
 /* size of each field of destination data counting with padding */
 size_t *size_pad = (size_t *)malloc((size_t)nfields * sizeof(size_t));

 /* Shut compiler */
 src_size=src_size;
 src_offset=src_offset;

 if ( size_pad == NULL )
  goto out;

 for ( i= 0; i < nfields; i++)
 {

  size_pad[i] = ( i == nfields-1 ? dst_size-dst_offset[i] : dst_offset[i+1]-dst_offset[i] );

 }

 /* Iterate tru the records */
 for ( i = 0; i < nrecords; i++)
 {
  /* Iterate tru the members */
  for ( j = 0; j < nfields; j++)
  {

   memcpy( dst_buf, src_buf, dst_sizes[j] );
   dst_buf += size_pad[j];
   src_buf += src_sizes[j];

  }

 }

 if ( size_pad != NULL )
  free( size_pad );

return 0;

out:
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
 * Function: H5LTget_attribute_long_long
 *
 * Purpose: Reads an attribute named attr_name
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Elena Pourmal, epourmal@ncsa.uiuc.edu
 *
 * Date: June 17, 2005
 *
 * Comments: This funstion was added to suuport INTEGER*8 Fortran types
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5LTget_attribute_long_long( hid_t loc_id,
                              const char *obj_name,
                              const char *attr_name,
                              long_long *data )
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
 if ( H5LT_get_attribute_mem( obj_id, attr_name, H5T_NATIVE_LLONG, data ) < 0 )
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
 * private functions
 *-------------------------------------------------------------------------
 */


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
 hid_t attr_id;
 hid_t attr_type;

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





/*-------------------------------------------------------------------------
 * Function: H5LT_set_attribute_string
 *
 * Purpose: creates and writes an attribute named NAME to the dataset DSET_ID
 *
 * Return: FAIL on error, SUCCESS on success
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: January 04, 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5LT_set_attribute_string(hid_t dset_id,
                                 char *name,
                                 char *buf )
{
 hid_t   tid;
 hid_t   sid;
 hid_t   aid;
 int     has_attr;
	size_t  size;

 /* verify if the attribute already exists */
 has_attr = H5LT_find_attribute(dset_id,name);

 /* the attribute already exists, delete it */
 if ( has_attr == 1 )
 {
  if ( H5Adelete(dset_id,name)<0)
    return FAIL;
 }

/*-------------------------------------------------------------------------
 * create the attribute type
 *-------------------------------------------------------------------------
 */
 if ((tid = H5Tcopy(H5T_C_S1))<0)
  return FAIL;

 size = strlen(buf) + 1; /* extra null term */

 if (H5Tset_size(tid,(size_t)size)<0)
  goto out;

 if (H5Tset_strpad(tid,H5T_STR_NULLTERM)<0)
  goto out;

 if ((sid = H5Screate(H5S_SCALAR))<0)
  goto out;


/*-------------------------------------------------------------------------
 * create and write the attribute
 *-------------------------------------------------------------------------
 */
 if ((aid = H5Acreate(dset_id,name,tid,sid,H5P_DEFAULT))<0)
  goto out;

 if (H5Awrite(aid,tid,buf)<0)
  goto out;

 if (H5Aclose(aid)<0)
  goto out;

 if (H5Sclose(sid)<0)
  goto out;

 if (H5Tclose(tid)<0)
  goto out;

 return SUCCESS;

 /* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Aclose(aid);
  H5Tclose(tid);
  H5Sclose(sid);
 } H5E_END_TRY;
 return FAIL;

}

