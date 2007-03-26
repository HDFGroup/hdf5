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

#include <stdlib.h>
#include <string.h>
#include "H5LTprivate.h"
#include "H5TBprivate.h"


/*-------------------------------------------------------------------------
 *
 * internal functions
 *
 *-------------------------------------------------------------------------
 */

static int H5TB_find_field(const char *field,
                           const char *field_list);

static herr_t H5TB_attach_attributes(const char *table_title,
                                     hid_t loc_id,
                                     const char *dset_name,
                                     hsize_t nfields,
                                     hid_t tid );

static hid_t H5TB_create_type(hid_t loc_id,
                              const char *dset_name,
                              size_t type_size,
                              const size_t *field_offset,
                              const size_t *field_sizes,
                              hid_t ftype_id);

/*-------------------------------------------------------------------------
 *
 * Create functions
 *
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: H5TBmake_table
 *
 * Purpose: Make a table
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *             Quincey Koziol
 *
 * Date: January 17, 2001
 *
 * Comments: The data is packed
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5TBmake_table( const char *table_title,
                       hid_t loc_id,
                       const char *dset_name,
                       hsize_t nfields,
                       hsize_t nrecords,
                       size_t type_size,
                       const char *field_names[],
                       const size_t *field_offset,
                       const hid_t *field_types,
                       hsize_t chunk_size,
                       void *fill_data,
                       int compress,
                       const void *data )
{

 hid_t   did;
 hid_t   sid;
 hid_t   mem_type_id;
 hid_t   plist_id;
 hsize_t dims[1];
 hsize_t dims_chunk[1];
 hsize_t maxdims[1] = { H5S_UNLIMITED };
 char    attr_name[255];
 char    *member_name;
 hid_t   attr_id;
 char    aux[255];
 hsize_t i;
 unsigned char *tmp_buf;

 dims[0]       = nrecords;
 dims_chunk[0] = chunk_size;

 /* Create the memory data type. */
 if ((mem_type_id = H5Tcreate (H5T_COMPOUND, type_size )) < 0 )
  return -1;

 /* Insert fields. */
 for ( i = 0; i < nfields; i++)
 {
  if ( H5Tinsert(mem_type_id, field_names[i], field_offset[i], field_types[i] ) < 0 )
   return -1;
 }

 /* Create a simple data space with unlimited size */
 if ( (sid = H5Screate_simple( 1, dims, maxdims )) < 0 )
  return -1;

 /* Modify dataset creation properties, i.e. enable chunking  */
 plist_id = H5Pcreate (H5P_DATASET_CREATE);
 if ( H5Pset_chunk ( plist_id, 1, dims_chunk ) < 0 )
  return -1;

 /* Set the fill value using a struct as the data type. */
 if ( fill_data )
 {
  if ( H5Pset_fill_value( plist_id, mem_type_id, fill_data ) < 0 )
   return -1;
 }

 /*
  Dataset creation property list is modified to use
  GZIP compression with the compression effort set to 6.
  Note that compression can be used only when dataset is chunked.
  */
 if ( compress )
 {
  if ( H5Pset_deflate( plist_id, 6) < 0 )
   return -1;
 }

 /* Create the dataset. */
 if ( (did = H5Dcreate( loc_id, dset_name, mem_type_id, sid, plist_id )) < 0 )
  goto out;

 /* Only write if there is something to write */
 if ( data )
 {
  /* Write data to the dataset. */
  if ( H5Dwrite( did, mem_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, data ) < 0 )
   goto out;
 }

 /* Terminate access to the data space. */
 if ( H5Sclose( sid ) < 0 )
  goto out;

 /* End access to the dataset */
 if ( H5Dclose( did ) < 0 )
  goto out;

 /* End access to the property list */
 if ( H5Pclose( plist_id ) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Set the conforming table attributes
 *-------------------------------------------------------------------------
 */

 /* Attach the CLASS attribute */
 if ( H5LTset_attribute_string( loc_id, dset_name, "CLASS", TABLE_CLASS ) < 0 )
  goto out;

 /* Attach the VERSION attribute */
 if ( H5LTset_attribute_string( loc_id, dset_name, "VERSION", "2.0" ) < 0 )
  goto out;

 /* Attach the TITLE attribute */
 if ( H5LTset_attribute_string( loc_id, dset_name, "TITLE", table_title ) < 0 )
  goto out;

 /* Attach the FIELD_ name attribute */
 for ( i = 0; i < nfields; i++)
 {

  /* Get the member name */
  member_name = H5Tget_member_name( mem_type_id,(unsigned) i );

  strcpy( attr_name, "FIELD_" );
  sprintf( aux, "%d", (int)i );
  strcat( attr_name, aux );
  sprintf( aux, "%s", "_NAME" );
  strcat( attr_name, aux );

  /* Attach the attribute */
  if ( H5LTset_attribute_string( loc_id, dset_name, attr_name, member_name ) < 0 )
   goto out;

  free( member_name );

 }

 /* Attach the FIELD_ fill value attribute */
 if ( fill_data )
 {

  tmp_buf = fill_data;

  /* Open the dataset. */
  if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
   return -1;

  if (( sid = H5Screate(H5S_SCALAR)) < 0 )
   goto out;

  for ( i = 0; i < nfields; i++)
  {

   /* Get the member name */
   member_name = H5Tget_member_name( mem_type_id, (unsigned) i );

   strcpy( attr_name, "FIELD_" );
   sprintf( aux, "%d", (int)i );
   strcat( attr_name, aux );
   sprintf( aux, "%s", "_FILL" );
   strcat( attr_name, aux );

   if ( (attr_id = H5Acreate( did, attr_name, field_types[i], sid, H5P_DEFAULT )) < 0 )
    goto out;

   if ( H5Awrite( attr_id, field_types[i], tmp_buf+field_offset[i] ) < 0 )
    goto out;

   if ( H5Aclose( attr_id ) < 0 )
    goto out;

   free( member_name );
  }

   /* Close the dataset. */
   H5Dclose( did );

   /* Close data space. */
   H5Sclose( sid );
 }

 /* Release the datatype. */
 if ( H5Tclose( mem_type_id ) < 0 )
  return -1;


return 0;

/* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Dclose(did);
  H5Sclose(sid);
  H5Pclose(plist_id);
  H5Tclose(mem_type_id);
 } H5E_END_TRY;
 return -1;

}

/*-------------------------------------------------------------------------
 *
 * Write functions
 *
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: H5TBappend_records
 *
 * Purpose: Appends records to a table
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmers:
 *  Pedro Vicente, pvn@ncsa.uiuc.edu
 *  Quincey Koziol
 *
 * Date: November 19, 2001
 *
 * Comments: Uses memory offsets
 *
 * Modifications: April 1, 2004
 *  the DST_SIZES parameter is used to define the memory type ID
 *  returned by H5TB_create_type
 *
 *-------------------------------------------------------------------------
 */

herr_t H5TBappend_records( hid_t loc_id,
                           const char *dset_name,
                           hsize_t nrecords,
                           size_t type_size,
                           const size_t *field_offset,
                           const size_t *field_sizes,
                           const void *data )
{
 hid_t    did;
 hid_t    tid=-1;
 hid_t    mem_type_id=-1;
 hid_t    sid=-1;
 hid_t    mem_space_id=-1;
 hsize_t  nrecords_orig;
 hsize_t  nfields;

 /* Get the original number of records and fields  */
 if ( H5TBget_table_info ( loc_id, dset_name, &nfields, &nrecords_orig ) < 0 )
  return -1;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  goto out;

  /* Get the datatypes */
 if ( (tid = H5Dget_type( did )) < 0 )
  goto out;

 if ((mem_type_id=H5TB_create_type(loc_id,dset_name,type_size,field_offset,field_sizes,tid))<0)
  goto out;

 /* Append the records */
 if ((H5TB_common_append_records(did, mem_type_id, (size_t)nrecords, nrecords_orig, data))<0)
  goto out;

 /* Release the datatype. */
 if ( H5Tclose( tid ) < 0 )
  return -1;

  /* Release the datatype. */
 if ( H5Tclose( mem_type_id ) < 0 )
  goto out;

 /* End access to the dataset */
 if ( H5Dclose( did ) < 0 )
  goto out;


return 0;

/* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Dclose(did);
  H5Tclose(mem_type_id);
  H5Tclose(tid);
  H5Sclose(mem_space_id);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;
}

/*-------------------------------------------------------------------------
 * Function: H5TBwrite_records
 *
 * Purpose: Writes records
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 19, 2001
 *
 * Comments: Uses memory offsets
 *
 * Modifications: April 1, 2004
 *  the DST_SIZES parameter is used to define the memory type ID
 *  returned by H5TB_create_type
 *
 *-------------------------------------------------------------------------
 */


herr_t H5TBwrite_records( hid_t loc_id,
                          const char *dset_name,
                          hsize_t start,
                          hsize_t nrecords,
                          size_t type_size,
                          const size_t *field_offset,
                          const size_t *field_sizes,
                          const void *data )
{

 hid_t    did;
 hid_t    tid;
 hsize_t  count[1];
 hsize_t offset[1];
 hid_t    sid=-1;
 hid_t    mem_space_id=-1;
 hsize_t  mem_size[1];
 hsize_t  dims[1];
 hid_t    mem_type_id=-1;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

  /* Get the datatype */
 if ( (tid = H5Dget_type( did )) < 0 )
  goto out;

 if ((mem_type_id=H5TB_create_type(loc_id,dset_name,type_size,field_offset,field_sizes,tid))<0)
  goto out;

 /* Get the dataspace handle */
 if ( (sid = H5Dget_space( did )) < 0 )
  goto out;

  /* Get records */
 if ( H5Sget_simple_extent_dims( sid, dims, NULL) < 0 )
  goto out;

 if ( start + nrecords > dims[0] )
  goto out;

 /* Define a hyperslab in the dataset of the size of the records */
 offset[0] = start;
 count[0]  = nrecords;
 if ( H5Sselect_hyperslab( sid, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
  goto out;

 /* Create a memory dataspace handle */
 mem_size[0] = count[0];
 if ( (mem_space_id = H5Screate_simple( 1, mem_size, NULL )) < 0 )
  goto out;

 if ( H5Dwrite( did, mem_type_id, mem_space_id, sid, H5P_DEFAULT, data ) < 0 )
  goto out;

 /* Terminate access to the memory dataspace */
 if ( H5Sclose( mem_space_id ) < 0 )
  goto out;

 /* Terminate access to the dataspace */
 if ( H5Sclose( sid ) < 0 )
  goto out;

 /* Release the datatype. */
 if ( H5Tclose( tid ) < 0 )
  goto out;

 /* Release the datatype. */
 if ( H5Tclose( mem_type_id ) < 0 )
  return -1;

 /* End access to the dataset */
 if ( H5Dclose( did ) < 0 )
  return -1;


return 0;

/* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Dclose(did);
  H5Tclose(mem_type_id);
  H5Tclose(tid);
  H5Sclose(mem_space_id);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;
}

/*-------------------------------------------------------------------------
 * Function: H5TBwrite_fields_name
 *
 * Purpose: Writes fields
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 21, 2001
 *
 * Comments:
 *
 * Modifications: April 1, 2004
 *  the DST_SIZES parameter is used to define the memory type ID
 *  returned by H5TB_create_type
 *
 *-------------------------------------------------------------------------
 */
herr_t H5TBwrite_fields_name( hid_t loc_id,
                              const char *dset_name,
                              const char *field_names,
                              hsize_t start,
                              hsize_t nrecords,
                              size_t type_size,
                              const size_t *field_offset,
                              const size_t *field_sizes,
                              const void *data )
{

 hid_t    did;
 hid_t    tid=-1;
 hid_t    write_type_id=-1;
 hid_t    member_type_id;
 hid_t    nmtype_id;
 hsize_t  count[1];
 hsize_t  offset[1];
 hid_t    mem_space_id=-1;
 hid_t    file_space_id=-1;
 char     *member_name;
 hssize_t  nfields;
 hssize_t  i, j;
 hid_t    PRESERVE;
 size_t   size_native;

 /* Create xfer properties to preserve initialized data */
 if ((PRESERVE = H5Pcreate (H5P_DATASET_XFER))<0)
  return -1;
 if (H5Pset_preserve (PRESERVE, 1)<0)
  return -1;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  goto out;

 /* Get the datatype */
 if ( (tid = H5Dget_type( did )) < 0 )
  goto out;

 /* Get the number of fields */
 if ( ( nfields = H5Tget_nmembers( tid )) < 0 )
  goto out;

 /* Create a write id */
 if ( ( write_type_id = H5Tcreate( H5T_COMPOUND, type_size )) < 0 )
  goto out;

 j = 0;

 /* Iterate tru the members */
 for ( i = 0; i < nfields; i++)
 {
  /* Get the member name */
  member_name = H5Tget_member_name( tid, (unsigned)i );

  if ( H5TB_find_field( member_name, field_names ) > 0 )
  {

   /* Get the member type */
   if ( ( member_type_id = H5Tget_member_type( tid,(unsigned) i )) < 0 )
    goto out;

   /* Convert to native type */
   if ((nmtype_id=H5Tget_native_type(member_type_id,H5T_DIR_DEFAULT))<0)
    goto out;

   size_native=H5Tget_size(nmtype_id);

   /* Adjust, if necessary */
   if (field_sizes[j]!=size_native)
   {
    if (H5Tset_size(nmtype_id, field_sizes[j])<0)
     goto out;
   }

   /* The field in the file is found by its name */
   if ( field_offset )
   {
    if ( H5Tinsert( write_type_id, member_name, field_offset[j], nmtype_id ) < 0 )
     goto out;
   }
   /* Only one field */
   else
   {
    if ( H5Tinsert( write_type_id, member_name, (size_t)0, nmtype_id ) < 0 )
     goto out;
   }

   j++;

   /* Close */
   if ( H5Tclose( member_type_id ) < 0 )
    goto out;
   if ( H5Tclose( nmtype_id ) < 0 )
    goto out;
  }

  free( member_name );

 }

  /* Get the dataspace handle */
 if ( (file_space_id = H5Dget_space( did )) < 0 )
  goto out;
 if ( (mem_space_id = H5Screate_simple(1, &nrecords, NULL)) < 0 )
  goto out;

 /* Define a hyperslab in the dataset */
 offset[0] = start;
 count[0]  = nrecords;
 if ( H5Sselect_hyperslab( file_space_id, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
  goto out;

 /* Write */
 if ( H5Dwrite( did, write_type_id, mem_space_id, file_space_id, PRESERVE, data ) < 0 )
  goto out;

 /* close */
 if ( H5Tclose( write_type_id ) )
  goto out;
 if ( H5Tclose( tid ) < 0 )
  return -1;
 if ( H5Dclose( did ) < 0 )
  return -1;
 if ( H5Pclose( PRESERVE ) < 0 )
  return -1;
 if ( H5Sclose( file_space_id ) < 0 )
  return -1;
 if ( H5Sclose( mem_space_id ) < 0 )
  return -1;

return 0;

 /* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Pclose(PRESERVE);
  H5Dclose(did);
  H5Sclose(file_space_id);
  H5Sclose(mem_space_id);
  H5Tclose(write_type_id);
  H5Tclose(tid);
 } H5E_END_TRY;
 return -1;

}



/*-------------------------------------------------------------------------
 * Function: H5TBwrite_fields_index
 *
 * Purpose: Writes fields
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 21, 2001
 *
 * Comments: Uses memory offsets
 *
 * Modifications: April 1, 2004
 *  the DST_SIZES parameter is used to define the memory type ID
 *  returned by H5TB_create_type
 *
 *-------------------------------------------------------------------------
 */


herr_t H5TBwrite_fields_index( hid_t loc_id,
                               const char *dset_name,
                               hsize_t nfields,
                               const int *field_index,
                               hsize_t start,
                               hsize_t nrecords,
                               size_t type_size,
                               const size_t *field_offset,
                               const size_t *field_sizes,
                               const void *data )
{

 hid_t    did;
 hid_t    tid=-1;
 hid_t    write_type_id=-1;
 hid_t    member_type_id;
 hid_t    nmtype_id;
 hsize_t  count[1];
 hsize_t  offset[1];
 hid_t    mem_space_id=-1;
 hid_t    file_space_id=-1;
 char     *member_name;
 hsize_t  i, j;
 hid_t    PRESERVE;
 size_t   size_native;

 /* Create xfer properties to preserve initialized data */
 if ((PRESERVE = H5Pcreate (H5P_DATASET_XFER))<0)
  return -1;
 if (H5Pset_preserve (PRESERVE, 1)<0)
  return -1;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  goto out;

 /* Get the datatype */
 if ( (tid = H5Dget_type( did )) < 0 )
  goto out;

 /* Create a write id */
 if ( ( write_type_id = H5Tcreate( H5T_COMPOUND, type_size )) < 0 )
  goto out;

 /* Iterate tru the members */
 for ( i = 0; i < nfields; i++)
 {

  j = field_index[i];

  /* Get the member name */
  member_name = H5Tget_member_name( tid, (unsigned) j );

  /* Get the member type */
  if ( ( member_type_id = H5Tget_member_type( tid, (unsigned) j )) < 0 )
   goto out;

   /* Convert to native type */
  if ((nmtype_id=H5Tget_native_type(member_type_id,H5T_DIR_DEFAULT))<0)
   goto out;

  size_native=H5Tget_size(nmtype_id);

  if (field_sizes[i]!=size_native)
  {
   if (H5Tset_size(nmtype_id, field_sizes[i])<0)
    goto out;
  }

   /* The field in the file is found by its name */
   if ( field_offset )
   {
    if ( H5Tinsert( write_type_id, member_name, field_offset[ i ], nmtype_id ) < 0 )
     goto out;
   }
   /* Only one field */
   else
   {
    if ( H5Tinsert( write_type_id, member_name, (size_t)0, nmtype_id ) < 0 )
     goto out;
   }
  /* Close */
  if ( H5Tclose( member_type_id ) < 0 )
   goto out;
  if ( H5Tclose( nmtype_id ) < 0 )
   goto out;

  free( member_name );

 }

  /* Get the dataspace handles */
 if ( (file_space_id = H5Dget_space( did )) < 0 )
  goto out;
 if ( (mem_space_id = H5Screate_simple(1, &nrecords, NULL)) < 0 )
  goto out;

 /* Define a hyperslab in the dataset */
 offset[0] = start;
 count[0]  = nrecords;
 if ( H5Sselect_hyperslab( file_space_id, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
  goto out;

 /* Write */
 if ( H5Dwrite( did, write_type_id, mem_space_id, file_space_id, PRESERVE, data ) < 0 )
  goto out;

 /* close */
 if ( H5Tclose( write_type_id ) )
  goto out;
 if ( H5Tclose( tid ) < 0 )
  return -1;
 if ( H5Dclose( did ) < 0 )
  return -1;
 if ( H5Pclose( PRESERVE ) < 0 )
  return -1;
 if ( H5Sclose( file_space_id ) < 0 )
  return -1;
 if ( H5Sclose( mem_space_id ) < 0 )
  return -1;

return 0;

 /* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Pclose(PRESERVE);
  H5Dclose(did);
  H5Sclose(file_space_id);
  H5Sclose(mem_space_id);
  H5Tclose(write_type_id);
  H5Tclose(tid);
 } H5E_END_TRY;
 return -1;
}


/*-------------------------------------------------------------------------
 *
 * Read functions
 *
 *-------------------------------------------------------------------------
 */


/*-------------------------------------------------------------------------
 * Function: H5TBread_table
 *
 * Purpose: Reads a table
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 20, 2001
 *
 * Comments:
 *
 * Modifications: April 1, 2004
 *  used a memory type ID returned by H5TB_create_type
 *
 *-------------------------------------------------------------------------
 */

herr_t H5TBread_table( hid_t loc_id,
                       const char *dset_name,
                       size_t type_size,
                       const size_t *field_offset,
                       const size_t *field_sizes,
                       void *dst_buf )
{
 hid_t    did;
 hid_t    ftype_id=-1;
 hid_t    mem_type_id=-1;
 hid_t    sid;
 hsize_t  dims[1];

 /* open the dataset. */
 if ((did=H5Dopen(loc_id,dset_name))<0)
  return -1;

 /* get the dataspace handle */
 if ( (sid = H5Dget_space( did )) < 0 )
  goto out;

 /* get dimensions */
 if ( H5Sget_simple_extent_dims( sid, dims, NULL) < 0 )
  goto out;

 /* get the datatypes */
 if ((ftype_id=H5Dget_type (did))<0)
  goto out;

 if ((mem_type_id=H5TB_create_type(loc_id,dset_name,type_size,field_offset,field_sizes,ftype_id))<0)
  goto out;

 /* read */
 if ( H5Dread( did, mem_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, dst_buf) < 0 )
  goto out;

 /* close */
 if ( H5Tclose( ftype_id ) < 0 )
  goto out;
 if ( H5Tclose( mem_type_id ) < 0 )
  goto out;
 if ( H5Sclose( sid ) < 0 )
  goto out;
 if ( H5Dclose( did ) < 0 )
  return -1;

 return 0;

 /* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Dclose(did);
  H5Tclose(mem_type_id);
  H5Tclose(ftype_id);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;
}

/*-------------------------------------------------------------------------
 * Function: H5TBread_records
 *
 * Purpose: Reads records
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 19, 2001
 *
 * Comments:
 *
 * Modifications: April 1, 2004
 *  the DST_SIZES parameter is used to define the memory type ID
 *  returned by H5TB_create_type
 *
 *-------------------------------------------------------------------------
 */


herr_t H5TBread_records( hid_t loc_id,
                         const char *dset_name,
                         hsize_t start,
                         hsize_t nrecords,
                         size_t type_size,
                         const size_t *field_offset,
                         const size_t *field_sizes,
                         void *data )
{

 hid_t    did;
 hid_t    ftype_id;
 hid_t    mem_type_id=-1;
 hsize_t  count[1];
 hsize_t offset[1];
 hid_t    sid=-1;
 hsize_t  dims[1];
 hid_t    mem_space_id=-1;
 hsize_t  mem_size[1];
 hsize_t  nrecords_orig;
 hsize_t  nfields;

 /* get the number of records and fields  */
 if ( H5TBget_table_info ( loc_id, dset_name, &nfields, &nrecords_orig ) < 0 )
  return -1;

 /* open the dataset */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* get the datatypes */
 if ( (ftype_id = H5Dget_type( did )) < 0 )
  goto out;

 if ((mem_type_id=H5TB_create_type(loc_id,dset_name,type_size,field_offset,field_sizes,ftype_id))<0)
  goto out;

 /* Read the records */
 if ((H5TB_common_read_records(did, mem_type_id, start, (size_t)nrecords, nrecords_orig, data)) < 0)
  goto out;

 /* get the dataspace handle */
 if ( (sid = H5Dget_space( did )) < 0 )
  goto out;

 /* get records */
 if ( H5Sget_simple_extent_dims( sid, dims, NULL) < 0 )
  goto out;

 if ( start + nrecords > dims[0] )
  goto out;

 /* define a hyperslab in the dataset of the size of the records */
 offset[0] = start;
 count[0]  = nrecords;
 if ( H5Sselect_hyperslab( sid, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
  goto out;

 /* create a memory dataspace handle */
 mem_size[0] = count[0];
 if ( (mem_space_id = H5Screate_simple( 1, mem_size, NULL )) < 0 )
  goto out;

 /* read */
 if ( H5Dread( did, mem_type_id, mem_space_id, sid, H5P_DEFAULT, data ) < 0 )
  goto out;

 /* close */
 if ( H5Sclose( mem_space_id ) < 0 )
  goto out;
 if ( H5Sclose( sid ) < 0 )
  goto out;
 if ( H5Tclose( ftype_id ) < 0 )
  return -1;
 if ( H5Tclose( mem_type_id ) < 0 )
  return -1;
 if ( H5Dclose( did ) < 0 )
  return -1;

 return 0;

 /* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Dclose(did);
  H5Tclose(mem_type_id);
  H5Tclose(ftype_id);
  H5Sclose(mem_space_id);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;

}


/*-------------------------------------------------------------------------
 * Function: H5TBread_fields_name
 *
 * Purpose: Reads fields
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 19, 2001
 *
 * Comments:
 *
 * Modifications: April 1, 2004
 *  the DST_SIZES parameter is used to define the memory type ID
 *  returned by H5TB_create_type
 *
 *-------------------------------------------------------------------------
 */


herr_t H5TBread_fields_name( hid_t loc_id,
                             const char *dset_name,
                             const char *field_names,
                             hsize_t start,
                             hsize_t nrecords,
                             size_t type_size,
                             const size_t *field_offset,
                             const size_t *field_sizes,
                             void *data )
{

 hid_t    did;
 hid_t    ftype_id=-1;
 hid_t    mem_type_id=-1;
 hid_t    mtype_id;
 hid_t    nmtype_id;
 char     *member_name;
 hssize_t nfields;
 hsize_t  count[1];
 hsize_t offset[1];
 hid_t    sid=-1;
 hid_t    mem_space_id=-1;
 hsize_t  mem_size[1];
 size_t   size_native;
 hssize_t i, j;

 /* open the dataset */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  goto out;

 /* get the datatype */
 if ( (ftype_id = H5Dget_type( did )) < 0 )
  goto out;

 /* get the number of fields */
 if ( ( nfields = H5Tget_nmembers( ftype_id )) < 0 )
  goto out;

 /* create a memory read id */
 if ( ( mem_type_id = H5Tcreate( H5T_COMPOUND, type_size )) < 0 )
  goto out;

 /* iterate tru the members */
 for ( i=0,j=0; i<nfields; i++)
 {
  /* get the member name */
  member_name = H5Tget_member_name( ftype_id, (unsigned)i );

  if ( H5TB_find_field( member_name, field_names ) > 0 )
  {
   /* get the member type */
   if ( ( mtype_id = H5Tget_member_type( ftype_id, (unsigned) i )) < 0 )
    goto out;

   /* convert to native type */
   if ((nmtype_id=H5Tget_native_type(mtype_id,H5T_DIR_DEFAULT))<0)
    goto out;

   size_native=H5Tget_size(nmtype_id);

   if (field_sizes[j]!=size_native)
   {
    if (H5Tset_size(nmtype_id, field_sizes[j])<0)
     goto out;
   }
   /* the field in the file is found by its name */
   if ( field_offset )
   {
    if ( H5Tinsert( mem_type_id, member_name, field_offset[j], nmtype_id ) < 0 )
     goto out;
   }
    else
   {
    if ( H5Tinsert( mem_type_id, member_name, (size_t)0, nmtype_id ) < 0 )
     goto out;
   }

   /* close */
   if ( H5Tclose( mtype_id ) < 0 )
    goto out;
   if ( H5Tclose( nmtype_id ) < 0 )
    goto out;
   j++;
  }
  free( member_name );
 }

 /* get the dataspace handle */
 if ( (sid = H5Dget_space( did )) < 0 )
  goto out;

 /* define a hyperslab in the dataset */
 offset[0] = start;
 count[0]  = nrecords;
 if ( H5Sselect_hyperslab( sid, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
  goto out;

 /* create a memory dataspace handle */
 mem_size[0] = count[0];
 if ( (mem_space_id = H5Screate_simple( 1, mem_size, NULL )) < 0 )
  goto out;

 /* read */
 if ( H5Dread( did, mem_type_id, mem_space_id, sid, H5P_DEFAULT, data ) < 0 )
  goto out;

 /* close */
 if ( H5Tclose( mem_type_id ) )
  goto out;
 if ( H5Tclose( ftype_id ) < 0 )
  return -1;
 if ( H5Sclose( sid ) < 0 )
  goto out;
 if ( H5Sclose( mem_space_id ) < 0 )
  goto out;
 if ( H5Dclose( did ) < 0 )
  return -1;

 return 0;

/* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Dclose(did);
  H5Tclose(mem_type_id);
  H5Tclose(ftype_id);
  H5Sclose(mem_space_id);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;

}


/*-------------------------------------------------------------------------
 * Function: H5TBread_fields_index
 *
 * Purpose: Reads fields
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 19, 2001
 *
 * Comments:
 *
 * Modifications: April 1, 2004
 *  the DST_SIZES parameter is used to define the memory type ID
 *  returned by H5TB_create_type
 *
 *-------------------------------------------------------------------------
 */


herr_t H5TBread_fields_index( hid_t loc_id,
                              const char *dset_name,
                              hsize_t nfields,
                              const int *field_index,
                              hsize_t start,
                              hsize_t nrecords,
                              size_t type_size,
                              const size_t *field_offset,
                              const size_t *field_sizes,
                              void *data )
{

 hid_t    did;
 hid_t    tid=-1;
 hid_t    read_type_id=-1;
 hid_t    member_type_id;
 hid_t    nmtype_id;
 char     *member_name;
 hsize_t  count[1];
 hsize_t offset[1];
 hid_t    sid=-1;
 hid_t    mem_space_id=-1;
 hsize_t  mem_size[1];
 size_t   size_native;
 hsize_t  i, j;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  goto out;

 /* Get the datatype */
 if ( (tid = H5Dget_type( did )) < 0 )
  goto out;

 /* Create a read id */
 if ( ( read_type_id = H5Tcreate( H5T_COMPOUND, type_size )) < 0 )
  goto out;

 /* Iterate tru the members */
 for ( i = 0; i < nfields; i++)
 {
  j = field_index[i];

  /* Get the member name */
  member_name = H5Tget_member_name( tid, (unsigned) j );

  /* Get the member type */
  if ( ( member_type_id = H5Tget_member_type( tid, (unsigned) j )) < 0 )
   goto out;

  /* Get the member size */
  if ( H5Tget_size( member_type_id ) == 0 )
   goto out;

  /* Convert to native type */
  if ((nmtype_id=H5Tget_native_type(member_type_id,H5T_DIR_DEFAULT))<0)
   goto out;

  size_native=H5Tget_size(nmtype_id);

  if (field_sizes[i]!=size_native)
  {
   if (H5Tset_size(nmtype_id, field_sizes[i])<0)
    goto out;
  }

  /* The field in the file is found by its name */
  if ( field_offset )
  {
   if ( H5Tinsert( read_type_id, member_name, field_offset[i], nmtype_id ) < 0 )
    goto out;
  }
  else
  {
   if ( H5Tinsert( read_type_id, member_name, (size_t)0, nmtype_id ) < 0 )
    goto out;
  }

  /* Close the member type */
  if ( H5Tclose( member_type_id ) < 0 )
   goto out;
  if ( H5Tclose( nmtype_id ) < 0 )
   goto out;

  free( member_name );
 }

  /* Get the dataspace handle */
 if ( (sid = H5Dget_space( did )) < 0 )
  goto out;

 /* Define a hyperslab in the dataset */
 offset[0] = start;
 count[0]  = nrecords;
 if ( H5Sselect_hyperslab( sid, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
  goto out;

 /* Create a memory dataspace handle */
 mem_size[0] = count[0];
 if ( (mem_space_id = H5Screate_simple( 1, mem_size, NULL )) < 0 )
  goto out;

 /* Read */
 if ( H5Dread( did, read_type_id, mem_space_id, sid, H5P_DEFAULT, data ) < 0 )
  goto out;

 /* Terminate access to the dataspace */
 if ( H5Sclose( sid ) < 0 )
  goto out;

 /* Terminate access to the memory dataspace */
  if ( H5Sclose( mem_space_id ) < 0 )
   goto out;

 /* End access to the read id */
 if ( H5Tclose( read_type_id ) )
  goto out;

 /* Release the datatype. */
 if ( H5Tclose( tid ) < 0 )
  return -1;

 /* End access to the dataset */
 if ( H5Dclose( did ) < 0 )
  return -1;

return 0;

/* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Dclose(did);
  H5Tclose(read_type_id);
  H5Tclose(tid);
  H5Sclose(mem_space_id);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;

}


/*-------------------------------------------------------------------------
 *
 * Manipulation functions
 *
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: H5TBdelete_record
 *
 * Purpose: Delete records from middle of table ("pulling up" all the records after it)
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 26, 2001
 *
 * Modifications: April 29, 2003
 *
 *
 *-------------------------------------------------------------------------
 */

herr_t H5TBdelete_record( hid_t loc_id,
                          const char *dset_name,
                          hsize_t start,
                          hsize_t nrecords )
{

 hsize_t  nfields;
 hsize_t  ntotal_records;
 hsize_t  read_start;
 hsize_t  read_nrecords;
 hid_t    did;
 hid_t    tid;
 hsize_t  count[1];
 hsize_t offset[1];
 hid_t    sid;
 hid_t    mem_space_id;
 hsize_t  mem_size[1];
 unsigned char *tmp_buf;
 size_t   src_size;
 size_t   *src_offset;
 size_t   *src_sizes;
 hsize_t  nrows;
#if defined (SHRINK)
 hsize_t  dims[1];
#endif


/*-------------------------------------------------------------------------
 * First we get information about type size and offsets on disk
 *-------------------------------------------------------------------------
 */

 /* Get the number of records and fields  */
 if ( H5TBget_table_info ( loc_id, dset_name, &nfields, &ntotal_records ) < 0 )
  return -1;

 src_offset = (size_t *)malloc((size_t)nfields * sizeof(size_t));
 src_sizes = (size_t *)malloc((size_t)nfields * sizeof(size_t));

 if ( src_offset == NULL )
  return -1;

 /* Get field info */
 if ( H5TBget_field_info( loc_id, dset_name, NULL, src_sizes, src_offset, &src_size ) < 0 )
  return -1;

/*-------------------------------------------------------------------------
 * Read the records after the deleted one(s)
 *-------------------------------------------------------------------------
 */

 read_start = start + nrecords;
 read_nrecords = ntotal_records - read_start;
 tmp_buf = (unsigned char *)calloc((size_t) read_nrecords, src_size );

 if ( tmp_buf == NULL )
  return -1;

 /* Read the records after the deleted one(s) */
 if ( H5TBread_records( loc_id, dset_name, read_start, read_nrecords, src_size,
      src_offset, src_sizes, tmp_buf ) < 0 )
  return -1;


/*-------------------------------------------------------------------------
 * Write the records in another position
 *-------------------------------------------------------------------------
 */

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Get the datatype */
 if ( (tid = H5Dget_type( did )) < 0 )
  goto out;

 /* Get the dataspace handle */
 if ( (sid = H5Dget_space( did )) < 0 )
  goto out;

 /* Define a hyperslab in the dataset of the size of the records */
 offset[0] = start;
 count[0]  = read_nrecords;
 if ( H5Sselect_hyperslab( sid, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
  goto out;

 /* Create a memory dataspace handle */
 mem_size[0] = count[0];
 if ( (mem_space_id = H5Screate_simple( 1, mem_size, NULL )) < 0 )
  goto out;

 if ( H5Dwrite( did, tid, mem_space_id, sid, H5P_DEFAULT, tmp_buf ) < 0 )
  goto out;

 /* Terminate access to the memory dataspace */
 if ( H5Sclose( mem_space_id ) < 0 )
  goto out;

 /* Terminate access to the dataspace */
 if ( H5Sclose( sid ) < 0 )
  goto out;

 /* Release the datatype. */
 if ( H5Tclose( tid ) < 0 )
  goto out;


/*-------------------------------------------------------------------------
 * Change the table dimension
 *-------------------------------------------------------------------------
 */
#if defined (SHRINK)
 dims[0] = ntotal_records - nrecords;
 if ( H5Dset_extent( did, dims ) < 0 )
  goto out;
#endif

 /* End access to the dataset */
 if ( H5Dclose( did ) < 0 )
  return -1;

 free( tmp_buf );
 free( src_offset );
 free( src_sizes );


/*-------------------------------------------------------------------------
 * Store the new dimension as an attribute
 *-------------------------------------------------------------------------
 */

 nrows = ntotal_records - nrecords;
 /* Set the attribute */
 if (H5LT_set_attribute_numerical(loc_id,dset_name,"NROWS",(size_t)1,
      H5T_NATIVE_LLONG,&nrows)<0)
  return -1;


 return 0;

out:
 H5Dclose( did );
 return -1;
}

/*-------------------------------------------------------------------------
 * Function: H5TBinsert_record
 *
 * Purpose: Inserts records into middle of table ("pushing down" all the records after it)
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 26, 2001
 *
 * Comments: Uses memory offsets
 *
 * Modifications: April 1, 2004
 *  the DST_SIZES parameter is used to define the memory type ID
 *  returned by H5TB_create_type
 *
 *-------------------------------------------------------------------------
 */


herr_t H5TBinsert_record( hid_t loc_id,
                          const char *dset_name,
                          hsize_t start,
                          hsize_t nrecords,
                          size_t type_size,
                          const size_t *field_offset,
                          const size_t *field_sizes,
                          void *data )
{

 hsize_t  nfields;
 hsize_t  ntotal_records;
 hsize_t  read_nrecords;
 hid_t    did;
 hid_t    tid=-1;
 hid_t    mem_type_id=-1;
 hsize_t  count[1];
 hsize_t offset[1];
 hid_t    sid=-1;
 hid_t    mem_space_id=-1;
 hsize_t  dims[1];
 hsize_t  mem_dims[1];
 unsigned char *tmp_buf;

/*-------------------------------------------------------------------------
 * Read the records after the inserted one(s)
 *-------------------------------------------------------------------------
 */

 /* Get the dimensions  */
 if ( H5TBget_table_info ( loc_id, dset_name, &nfields, &ntotal_records ) < 0 )
  return -1;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  goto out;

  /* Get the datatype */
 if ( (tid = H5Dget_type( did )) < 0 )
  goto out;

 /* Create the memory data type. */
 if ((mem_type_id=H5TB_create_type(loc_id,dset_name,type_size,field_offset,field_sizes,tid))<0)
  goto out;

 read_nrecords = ntotal_records - start;
 tmp_buf = (unsigned char *)calloc((size_t) read_nrecords, type_size );

 /* Read the records after the inserted one(s) */
 if ( H5TBread_records( loc_id, dset_name, start, read_nrecords, type_size, field_offset,
  field_sizes, tmp_buf ) < 0 )
  return -1;

 /* Extend the dataset */
 dims[0] = ntotal_records + nrecords;

 if ( H5Dextend ( did, dims ) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Write the inserted records
 *-------------------------------------------------------------------------
 */

 /* Create a simple memory data space */
 mem_dims[0]=nrecords;
 if ( (mem_space_id = H5Screate_simple( 1, mem_dims, NULL )) < 0 )
  return -1;

 /* Get the file data space */
 if ( (sid = H5Dget_space( did )) < 0 )
  return -1;

 /* Define a hyperslab in the dataset to write the new data */
 offset[0] = start;
 count[0]  = nrecords;
 if ( H5Sselect_hyperslab( sid, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
  goto out;

 if ( H5Dwrite( did, mem_type_id, mem_space_id, sid, H5P_DEFAULT, data ) < 0 )
  goto out;

 /* Terminate access to the dataspace */
 if ( H5Sclose( mem_space_id ) < 0 )
  goto out;
 if ( H5Sclose( sid ) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Write the "pushed down" records
 *-------------------------------------------------------------------------
 */

 /* Create a simple memory data space */
 mem_dims[0]=read_nrecords;
 if ( (mem_space_id = H5Screate_simple( 1, mem_dims, NULL )) < 0 )
  return -1;

 /* Get the file data space */
 if ( (sid = H5Dget_space( did )) < 0 )
  return -1;

 /* Define a hyperslab in the dataset to write the new data */
 offset[0] = start + nrecords;
 count[0]  = read_nrecords;
 if ( H5Sselect_hyperslab( sid, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
  goto out;

 if ( H5Dwrite( did, mem_type_id, mem_space_id, sid, H5P_DEFAULT, tmp_buf ) < 0 )
  goto out;

 /* Terminate access to the dataspace */
 if ( H5Sclose( mem_space_id ) < 0 )
  goto out;

 if ( H5Sclose( sid ) < 0 )
  goto out;

 /* Release the datatype. */
 if ( H5Tclose( tid ) < 0 )
  return -1;

 /* Release the datatype. */
 if ( H5Tclose( mem_type_id ) < 0 )
  return -1;

 /* End access to the dataset */
 if ( H5Dclose( did ) < 0 )
  return -1;

 free( tmp_buf );

 return 0;

 /* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Dclose(did);
  H5Sclose(sid);
  H5Sclose(mem_space_id);
  H5Tclose(mem_type_id);
  H5Tclose(tid);
 } H5E_END_TRY;
 return -1;
}

/*-------------------------------------------------------------------------
 * Function: H5TBadd_records_from
 *
 * Purpose: Add records from first table to second table
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 5, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

herr_t H5TBadd_records_from( hid_t loc_id,
                             const char *dset_name1,
                             hsize_t start1,
                             hsize_t nrecords,
                             const char *dset_name2,
                             hsize_t start2 )
{

 /* Identifiers for the 1st dataset. */
 hid_t    dataset_id1;
 hid_t    type_id1;
 hid_t    space_id1=-1;
 hid_t    mem_space_id1=-1;
 size_t   type_size1;

 hsize_t  count[1];
 hsize_t offset[1];
 hsize_t  mem_size[1];
 hsize_t  nfields;
 hsize_t  ntotal_records;
 unsigned char *tmp_buf;
 size_t   src_size;
 size_t   *src_offset;
 size_t   *src_sizes;

/*-------------------------------------------------------------------------
 * First we get information about type size and offsets on disk
 *-------------------------------------------------------------------------
 */

 /* Get the number of records and fields  */
 if ( H5TBget_table_info ( loc_id, dset_name1, &nfields, &ntotal_records ) < 0 )
  return -1;

 src_offset = (size_t *)malloc((size_t)nfields * sizeof(size_t));
 src_sizes  = (size_t *)malloc((size_t)nfields * sizeof(size_t));

 if ( src_offset == NULL )
  return -1;

 /* Get field info */
 if ( H5TBget_field_info( loc_id, dset_name1, NULL, src_sizes, src_offset, &src_size ) < 0 )
  return -1;

/*-------------------------------------------------------------------------
 * Get information about the first table and read it
 *-------------------------------------------------------------------------
 */

 /* Open the 1st dataset. */
 if ( (dataset_id1 = H5Dopen( loc_id, dset_name1 )) < 0 )
  return -1;

 /* Get the datatype */
 if ( (type_id1 = H5Dget_type( dataset_id1 )) < 0 )
  goto out;

 /* Get the dataspace handle */
 if ( (space_id1 = H5Dget_space( dataset_id1 )) < 0 )
  goto out;

 /* Get the size of the datatype */
 if ( ( type_size1 = H5Tget_size( type_id1 )) == 0 )
  goto out;

 tmp_buf = (unsigned char *)calloc((size_t)nrecords, type_size1 );

 /* Define a hyperslab in the dataset of the size of the records */
 offset[0] = start1;
 count[0]  = nrecords;
 if ( H5Sselect_hyperslab( space_id1, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
  goto out;

 /* Create a memory dataspace handle */
 mem_size[0] = count[0];
 if ( (mem_space_id1 = H5Screate_simple( 1, mem_size, NULL )) < 0 )
  goto out;

 if ( H5Dread( dataset_id1, type_id1, mem_space_id1, space_id1, H5P_DEFAULT, tmp_buf ) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Add to the second table
 *-------------------------------------------------------------------------
 */
 if ( H5TBinsert_record(loc_id,dset_name2,start2,nrecords,src_size,src_offset,src_sizes,tmp_buf ) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Close resources for table 1
 *-------------------------------------------------------------------------
 */

 /* Terminate access to the memory dataspace */
 if ( H5Sclose( mem_space_id1 ) < 0 )
  goto out;

 /* Terminate access to the dataspace */
 if ( H5Sclose( space_id1 ) < 0 )
  goto out;

 /* Release the datatype. */
 if ( H5Tclose( type_id1 ) < 0 )
  return -1;

 /* End access to the dataset */
 if ( H5Dclose( dataset_id1 ) < 0 )
  return -1;

 free( tmp_buf );
 free( src_offset );
 free( src_sizes );

return 0;

 /* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Dclose(dataset_id1);
  H5Sclose(space_id1);
  H5Sclose(mem_space_id1);
  H5Tclose(type_id1);
 } H5E_END_TRY;
 return -1;

}

/*-------------------------------------------------------------------------
 * Function: H5TBcombine_tables
 *
 * Purpose: Combine records from two tables into a third
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 10, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */
herr_t H5TBcombine_tables( hid_t loc_id1,
                           const char *dset_name1,
                           hid_t loc_id2,
                           const char *dset_name2,
                           const char *dset_name3 )
{

 /* Identifiers for the 1st dataset. */
 hid_t    dataset_id1;
 hid_t    type_id1;
 hid_t    space_id1;
 hid_t    plist_id1;

 /* Identifiers for the 2nd dataset. */
 hid_t    dataset_id2;
 hid_t    type_id2;
 hid_t    space_id2;
 hid_t    plist_id2;

 /* Identifiers for the 3rd dataset. */
 hid_t    dataset_id3;
 hid_t    type_id3;
 hid_t    space_id3;
 hid_t    plist_id3;

 hsize_t  count[1];
 hsize_t offset[1];
 hid_t    mem_space_id;
 hsize_t  mem_size[1];
 hsize_t  nfields;
 hsize_t  nrecords;
 hsize_t  dims[1];
 hsize_t  maxdims[1] = { H5S_UNLIMITED };


 size_t   type_size;
 hid_t    sid;
 hid_t    member_type_id;
 size_t   member_offset;
 char     attr_name[255];
 hid_t    attr_id;
 char     aux[255];
 unsigned char *tmp_buf;
 unsigned char *tmp_fill_buf;
 hsize_t  i;
 size_t   src_size;
 size_t   *src_offset;
 size_t   *src_sizes;
 int      has_fill=0;

/*-------------------------------------------------------------------------
 * First we get information about type size and offsets on disk
 *-------------------------------------------------------------------------
 */

 /* Get the number of records and fields  */
 if ( H5TBget_table_info ( loc_id1, dset_name1, &nfields, &nrecords ) < 0 )
  return -1;

 src_offset = (size_t *)malloc((size_t)nfields * sizeof(size_t));
 src_sizes  = (size_t *)malloc((size_t)nfields * sizeof(size_t));


 if ( src_offset == NULL )
  return -1;

 /* Get field info */
 if ( H5TBget_field_info( loc_id1, dset_name1, NULL, src_sizes, src_offset, &src_size ) < 0 )
  return -1;

/*-------------------------------------------------------------------------
 * Get information about the first table
 *-------------------------------------------------------------------------
 */

 /* Open the 1st dataset. */
 if ( (dataset_id1 = H5Dopen( loc_id1, dset_name1 )) < 0 )
  goto out;

 /* Get the datatype */
 if ( (type_id1 = H5Dget_type( dataset_id1 )) < 0 )
  goto out;

 /* Get the dataspace handle */
 if ( (space_id1 = H5Dget_space( dataset_id1 )) < 0 )
  goto out;

 /* Get creation properties list */
 if ( (plist_id1 = H5Dget_create_plist( dataset_id1 )) < 0 )
  goto out;

 /* Get the dimensions  */
 if ( H5TBget_table_info ( loc_id1, dset_name1, &nfields, &nrecords ) < 0 )
  return -1;

/*-------------------------------------------------------------------------
 * Make the merged table with no data originally
 *-------------------------------------------------------------------------
 */

 /* Clone the property list */
 if ( ( plist_id3 = H5Pcopy( plist_id1 )) < 0 )
  goto out;

 /* Clone the type id */
 if ( ( type_id3 = H5Tcopy( type_id1 )) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Here we do not clone the file space from the 1st dataset, because we want to create
 * an empty table. Instead we create a new dataspace with zero records and expandable.
 *-------------------------------------------------------------------------
 */
 dims[0] = 0;

/* Create a simple data space with unlimited size */
 if ( (space_id3 = H5Screate_simple( 1, dims, maxdims )) < 0 )
  return -1;

 /* Create the dataset */
 if ( (dataset_id3 = H5Dcreate( loc_id1, dset_name3, type_id3, space_id3, plist_id3 )) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Attach the conforming table attributes
 *-------------------------------------------------------------------------
 */
 if ( H5TB_attach_attributes( "Merge table", loc_id1, dset_name3, nfields, type_id3 ) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Get attributes
 *-------------------------------------------------------------------------
 */

 type_size = H5Tget_size( type_id3 );

 /* alloc fill value attribute buffer */
 tmp_fill_buf = (unsigned char *)malloc((size_t) type_size );

 /* Get the fill value attributes */
 has_fill=H5TBAget_fill( loc_id1, dset_name1, dataset_id1, tmp_fill_buf );

/*-------------------------------------------------------------------------
 * Attach the fill attributes from previous table
 *-------------------------------------------------------------------------
 */
 if ( has_fill == 1 )
 {

  if (( sid = H5Screate(H5S_SCALAR)) < 0 )
   goto out;

  for ( i = 0; i < nfields; i++)
  {

   /* Get the member type */
   if ( ( member_type_id = H5Tget_member_type( type_id3, (unsigned) i )) < 0 )
    goto out;

   /* Get the member offset */
   member_offset = H5Tget_member_offset( type_id3, (unsigned) i );

   strcpy( attr_name, "FIELD_" );
   sprintf( aux, "%d", (int) i );
   strcat( attr_name, aux );
   sprintf( aux, "%s", "_FILL" );
   strcat( attr_name, aux );

   if ( (attr_id = H5Acreate( dataset_id3, attr_name, member_type_id, sid, H5P_DEFAULT )) < 0 )
    goto out;

   if ( H5Awrite( attr_id, member_type_id, tmp_fill_buf+member_offset ) < 0 )
    goto out;

   if ( H5Aclose( attr_id ) < 0 )
    goto out;

   if ( H5Tclose( member_type_id ) < 0 )
    goto out;
  }

  /* Close data space. */
  if ( H5Sclose( sid ) < 0 )
   goto out;
 }

/*-------------------------------------------------------------------------
 * Read data from 1st table
 *-------------------------------------------------------------------------
 */

 tmp_buf = (unsigned char *)calloc((size_t) nrecords, type_size );

 /* Define a hyperslab in the dataset of the size of the records */
 offset[0] = 0;
 count[0]  = nrecords;
 if ( H5Sselect_hyperslab( space_id1, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
  goto out;

 /* Create a memory dataspace handle */
 mem_size[0] = count[0];
 if ( (mem_space_id = H5Screate_simple( 1, mem_size, NULL )) < 0 )
  goto out;

 if ( H5Dread( dataset_id1, type_id1, mem_space_id, space_id1, H5P_DEFAULT, tmp_buf ) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Save data from 1st table into new table
 *-------------------------------------------------------------------------
 */

 /* Append the records to the new table */
 if ( H5TBappend_records( loc_id1, dset_name3, nrecords, src_size, src_offset, src_sizes, tmp_buf ) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Release resources from 1st table
 *-------------------------------------------------------------------------
 */

 /* Terminate access to the memory dataspace */
 if ( H5Sclose( mem_space_id ) < 0 )
  goto out;

 /* Terminate access to the dataspace */
 if ( H5Sclose( space_id1 ) < 0 )
  goto out;

 /* Release the datatype. */
 if ( H5Tclose( type_id1 ) < 0 )
  goto out;

 /* Terminate access to a property list */
 if ( H5Pclose( plist_id1 ) < 0 )
  goto out;

 /* End access to the dataset */
 if ( H5Dclose( dataset_id1 ) < 0 )
  goto out;

 /* Release resources. */
 free( tmp_buf );

/*-------------------------------------------------------------------------
 * Get information about the 2nd table
 *-------------------------------------------------------------------------
 */

 /* Open the dataset. */
 if ( (dataset_id2 = H5Dopen( loc_id2, dset_name2 )) < 0 )
  goto out;

 /* Get the datatype */
 if ( (type_id2 = H5Dget_type( dataset_id2 )) < 0 )
  goto out;

 /* Get the dataspace handle */
 if ( (space_id2 = H5Dget_space( dataset_id2 )) < 0 )
  goto out;

 /* Get the property list handle */
 if ( (plist_id2 = H5Dget_create_plist( dataset_id2 )) < 0 )
  goto out;

 /* Get the dimensions  */
 if ( H5TBget_table_info ( loc_id2, dset_name2, &nfields, &nrecords ) < 0 )
  return -1;

/*-------------------------------------------------------------------------
 * Read data from 2nd table
 *-------------------------------------------------------------------------
 */

 tmp_buf = (unsigned char *)calloc((size_t) nrecords, type_size );

 /* Define a hyperslab in the dataset of the size of the records */
 offset[0] = 0;
 count[0]  = nrecords;
 if ( H5Sselect_hyperslab( space_id2, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
  goto out;

 /* Create a memory dataspace handle */
 mem_size[0] = count[0];
 if ( (mem_space_id = H5Screate_simple( 1, mem_size, NULL )) < 0 )
  goto out;

 if ( H5Dread( dataset_id2, type_id2, mem_space_id, space_id2, H5P_DEFAULT, tmp_buf ) < 0 )
  goto out;


/*-------------------------------------------------------------------------
 * Save data from 2nd table into new table
 *-------------------------------------------------------------------------
 */

 /* append the records to the new table */
 if ( H5TBappend_records( loc_id1, dset_name3, nrecords, src_size, src_offset, src_sizes, tmp_buf ) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Release resources from 2nd table
 *-------------------------------------------------------------------------
 */

 /* Terminate access to the memory dataspace */
 if ( H5Sclose( mem_space_id ) < 0 )
  goto out;

 /* Terminate access to the dataspace */
 if ( H5Sclose( space_id2 ) < 0 )
  goto out;

 /* Release the datatype. */
 if ( H5Tclose( type_id2 ) < 0 )
  return -1;

 /* Terminate access to a property list */
 if ( H5Pclose( plist_id2 ) < 0 )
  goto out;

 /* End access to the dataset */
 if ( H5Dclose( dataset_id2 ) < 0 )
  return -1;

/*-------------------------------------------------------------------------
 * Release resources from 3rd table
 *-------------------------------------------------------------------------
 */

 /* Terminate access to the dataspace */
 if ( H5Sclose( space_id3 ) < 0 )
  return -1;

 /* Release the datatype. */
 if ( H5Tclose( type_id3 ) < 0 )
  return -1;

 /* Terminate access to a property list */
 if ( H5Pclose( plist_id3 ) < 0 )
  return -1;

 /* End access to the dataset */
 if ( H5Dclose( dataset_id3 ) < 0 )
  return -1;

 /* Release resources. */
 free( tmp_buf );
 free( tmp_fill_buf );
 free( src_offset );
 free( src_sizes );

return 0;

out:
 H5Dclose( dataset_id1 );
 return -1;

}



/*-------------------------------------------------------------------------
 * Function: H5TBinsert_field
 *
 * Purpose: Inserts a field
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: January 30, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5TBinsert_field( hid_t loc_id,
                         const char *dset_name,
                         const char *field_name,
                         hid_t field_type,
                         hsize_t position,
                         const void *fill_data,
                         const void *data )
{

 /* Identifiers for the 1st, original dataset */
 hid_t    dataset_id1;
 hid_t    type_id1;
 hid_t    space_id1;
 hid_t    plist_id1;
 hid_t    mem_space_id1;

 /* Identifiers for the 2nd, new dataset */
 hid_t    dataset_id2;
 hid_t    type_id2;
 hid_t    space_id2;
 hid_t    plist_id2;
 hid_t    mem_space_id2;

 hid_t    member_type_id;
 size_t   member_size;
 size_t   new_member_size = 0;
 char     *member_name;
 size_t   total_size;
 hsize_t  nfields;
 hsize_t  nrecords;
 hsize_t  dims_chunk[1];
 hsize_t  dims[1];
 hsize_t  maxdims[1] = { H5S_UNLIMITED };
 hsize_t  count[1];
 hsize_t offset[1];
 hsize_t  mem_size[1];
 hid_t    write_type_id;
 hid_t    PRESERVE;
 size_t   curr_offset;
 int      inserted;
 hsize_t  idx;
 char     table_title[255];
 size_t   member_offset;
 char     attr_name[255];
 hid_t    attr_id;
 char     aux[255];
 unsigned char *tmp_buf;
 unsigned char *tmp_fill_buf;
 hsize_t  i;

 /* Get the number of records and fields  */
 if ( H5TBget_table_info ( loc_id, dset_name,  &nfields, &nrecords ) < 0 )
  return -1;

/*-------------------------------------------------------------------------
 * Get information about the old data type
 *-------------------------------------------------------------------------
 */

 /* Open the dataset. */
 if ( (dataset_id1 = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Get creation properties list */
 if ( (plist_id1 = H5Dget_create_plist( dataset_id1 )) < 0 )
  goto out;

 /* Get the datatype */
 if ( (type_id1 = H5Dget_type( dataset_id1 )) < 0 )
  goto out;

 /* Get the size of the datatype */
 if ( ( total_size = H5Tget_size( type_id1 )) == 0 )
  goto out;

 /* Get the dataspace handle */
 if ( (space_id1 = H5Dget_space( dataset_id1 )) < 0 )
  goto out;

 /* Get dimension */
 if ( H5Sget_simple_extent_dims( space_id1, dims, NULL) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Get attributes
 *-------------------------------------------------------------------------
 */

 /* Get the table title */
 if ( (H5TBAget_title( dataset_id1, table_title )) < 0 )
  goto out;

 /* alloc fill value attribute buffer */
 tmp_fill_buf = (unsigned char *)malloc(total_size );

 /* Get the fill value attributes */
 if ( (H5TBAget_fill( loc_id, dset_name, dataset_id1, tmp_fill_buf )) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Create a new data type
 *-------------------------------------------------------------------------
 */

 /* Get the new member size */
 member_size = H5Tget_size( field_type );

 /* Create the data type. */
 if (( type_id2 = H5Tcreate (H5T_COMPOUND,(size_t)(total_size + member_size) )) < 0 )
  goto out;

 curr_offset = 0;
 inserted    = 0;

 /* Insert the old fields, counting with the new one */
 for ( i = 0; i < nfields + 1; i++)
 {

  idx = i;
  if ( inserted )
   idx = i - 1;

  if ( i == position )
  {

   /* Get the new member size */
   new_member_size = H5Tget_size( field_type );

   /* Insert the new field type */
   if ( H5Tinsert( type_id2, field_name, curr_offset, field_type ) < 0 )
    goto out;

   curr_offset += new_member_size;

   inserted = 1;

   continue;

  }

  /* Get the member name */
  member_name = H5Tget_member_name( type_id1, (unsigned)idx );

  /* Get the member type */
  if ( ( member_type_id = H5Tget_member_type( type_id1,(unsigned)idx )) < 0 )
   goto out;

  /* Get the member size */
  member_size = H5Tget_size( member_type_id );

  /* Insert it into the new type */
  if ( H5Tinsert( type_id2, member_name, curr_offset, member_type_id ) < 0 )
    goto out;

  curr_offset += member_size;

  free( member_name );

   /* Close the member type */
  if ( H5Tclose( member_type_id ) < 0 )
   goto out;


 } /* i */

/*-------------------------------------------------------------------------
 * Create a new temporary dataset
 *-------------------------------------------------------------------------
 */

 /* Retrieve the size of chunk */
 if ( H5Pget_chunk( plist_id1, 1, dims_chunk ) < 0 )
  goto out;

 /* Create a new simple data space with unlimited size, using the dimension */
 if ( ( space_id2 = H5Screate_simple( 1, dims, maxdims )) < 0 )
  return -1;

 /* Modify dataset creation properties, i.e. enable chunking  */
 plist_id2 = H5Pcreate (H5P_DATASET_CREATE);
 if ( H5Pset_chunk ( plist_id2, 1, dims_chunk ) < 0 )
  return -1;

 /* Create the dataset. */
 if ( ( dataset_id2 = H5Dcreate( loc_id, "new", type_id2, space_id2, plist_id2 )) < 0 )
  goto out;


/*-------------------------------------------------------------------------
 * Read data from 1st table
 *-------------------------------------------------------------------------
 */

 tmp_buf = (unsigned char *)calloc((size_t) nrecords, (size_t)total_size );

 /* Define a hyperslab in the dataset of the size of the records */
 offset[0] = 0;
 count[0]  = nrecords;
 if ( H5Sselect_hyperslab( space_id1, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
  goto out;

 /* Create a memory dataspace handle */
 mem_size[0] = count[0];
 if ( (mem_space_id1 = H5Screate_simple( 1, mem_size, NULL )) < 0 )
  goto out;

 if ( H5Dread( dataset_id1, type_id1, mem_space_id1, H5S_ALL, H5P_DEFAULT, tmp_buf ) < 0 )
  goto out;


/*-------------------------------------------------------------------------
 * Save data from 1st table into new table, using the 1st type id
 *-------------------------------------------------------------------------
 */

 /* Write */
 if ( H5Dwrite( dataset_id2, type_id1, mem_space_id1, H5S_ALL, H5P_DEFAULT, tmp_buf ) < 0 )
  goto out;


/*-------------------------------------------------------------------------
 * Save the function supplied data of the new field
 *-------------------------------------------------------------------------
 */


 /* Create a write id */
 if ( ( write_type_id = H5Tcreate( H5T_COMPOUND, (size_t)new_member_size )) < 0 )
  goto out;

 /* The field in the file is found by its name */
 if ( H5Tinsert( write_type_id, field_name, (size_t)0, field_type ) < 0 )
  goto out;

 /* Create xfer properties to preserve initialized data */
 if ((PRESERVE = H5Pcreate (H5P_DATASET_XFER))<0)
  goto out;
 if (H5Pset_preserve (PRESERVE, 1)<0)
  goto out;

  /* Only write if there is something to write */
 if ( data )
 {

  /* Create a memory dataspace handle */
  if ( (mem_space_id2 = H5Screate_simple( 1, mem_size, NULL )) < 0 )
   goto out;

  /* Write */
  if ( H5Dwrite( dataset_id2, write_type_id, mem_space_id2, space_id2, PRESERVE, data ) < 0 )
   goto out;

   /* Terminate access to the memory dataspace */
  if ( H5Sclose( mem_space_id2 ) < 0 )
   goto out;
 }

 /* End access to the property list */
 if ( H5Pclose( PRESERVE ) < 0 )
  goto out;



/*-------------------------------------------------------------------------
 * Release resources from 1st table
 *-------------------------------------------------------------------------
 */

 /* Terminate access to the memory dataspace */
 if ( H5Sclose( mem_space_id1 ) < 0 )
  goto out;

 /* Release the datatype. */
 if ( H5Tclose( type_id1 ) < 0 )
  goto out;

 /* Terminate access to a property list */
 if ( H5Pclose( plist_id1 ) < 0 )
  goto out;

 /* Terminate access to the data space */
 if ( H5Sclose( space_id1 ) < 0 )
  goto out;

 /* End access to the dataset */
 if ( H5Dclose( dataset_id1 ) < 0 )
  goto out;


/*-------------------------------------------------------------------------
 * Release resources from 2nd table
 *-------------------------------------------------------------------------
 */

 /* Terminate access to the dataspace */
 if ( H5Sclose( space_id2 ) < 0 )
  goto out;

 /* Release the datatype. */
 if ( H5Tclose( type_id2 ) < 0 )
  return -1;

 /* Terminate access to a property list */
 if ( H5Pclose( plist_id2 ) < 0 )
  goto out;

 /* End access to the dataset */
 if ( H5Dclose( dataset_id2 ) < 0 )
  return -1;
/*-------------------------------------------------------------------------
 * Delete 1st table
 *-------------------------------------------------------------------------
 */
 if ( H5Gunlink( loc_id, dset_name ) < 0 )
  return -1;

/*-------------------------------------------------------------------------
 * Rename 2nd table
 *-------------------------------------------------------------------------
 */

 if ( H5Gmove( loc_id, "new", dset_name ) < 0 )
  return -1;

/*-------------------------------------------------------------------------
 * Attach the conforming table attributes
 *-------------------------------------------------------------------------
 */

 /* Get the number of records and fields  */
 if ( H5TBget_table_info ( loc_id, dset_name,  &nfields, &nrecords ) < 0 )
  return -1;

 /* Open the dataset. */
 if ( (dataset_id1 = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Get the datatype */
 if ( (type_id1 = H5Dget_type( dataset_id1 )) < 0 )
  goto out;

 /* Set the attributes */
 if ( H5TB_attach_attributes( table_title, loc_id, dset_name,(hsize_t) nfields, type_id1 ) < 0 )
  return -1;
/*-------------------------------------------------------------------------
 * Attach the fill attributes from previous table
 *-------------------------------------------------------------------------
 */

  if (( space_id1 = H5Screate(H5S_SCALAR)) < 0 )
   goto out;

  for ( i = 0; i < nfields-1; i++)
  {
    /* Get the member type */
   if ( ( member_type_id = H5Tget_member_type( type_id1, (unsigned) i )) < 0 )
    goto out;

   /* Get the member offset */
   member_offset = H5Tget_member_offset( type_id1, (unsigned) i );

   strcpy( attr_name, "FIELD_" );
   sprintf( aux, "%d", (int)i );
   strcat( attr_name, aux );
   sprintf( aux, "%s", "_FILL" );
   strcat( attr_name, aux );

   if ( (attr_id = H5Acreate( dataset_id1, attr_name, member_type_id, space_id1, H5P_DEFAULT )) < 0 )
    goto out;

   if ( H5Awrite( attr_id, member_type_id, tmp_fill_buf+member_offset ) < 0 )
    goto out;

   if ( H5Aclose( attr_id ) < 0 )
    goto out;

   /* Close the member type */
   if ( H5Tclose( member_type_id ) < 0 )
    goto out;
  }

/*-------------------------------------------------------------------------
 * Attach the fill attribute from the new field, if present
 *-------------------------------------------------------------------------
 */
 if ( fill_data )
 {

   strcpy( attr_name, "FIELD_" );
   sprintf( aux, "%d",(int)( nfields-1) );
   strcat( attr_name, aux );
   sprintf( aux, "%s", "_FILL" );
   strcat( attr_name, aux );

    /* Get the member type */
   if ( ( member_type_id = H5Tget_member_type( type_id1, (unsigned)nfields-1 )) < 0 )
    goto out;

   if ( (attr_id = H5Acreate( dataset_id1, attr_name, member_type_id, space_id1, H5P_DEFAULT )) < 0 )
    goto out;

   if ( H5Awrite( attr_id, member_type_id, fill_data ) < 0 )
    goto out;

   if ( H5Aclose( attr_id ) < 0 )
    goto out;

   if ( H5Tclose( member_type_id ) < 0 )
    goto out;

 }

  /* Close data space. */
 if ( H5Sclose( space_id1 ) < 0 )
  goto out;

 /* Release the datatype. */
 if ( H5Tclose( type_id1 ) < 0 )
  goto out;

 /* End access to the dataset */
 if ( H5Dclose( dataset_id1 ) < 0 )
  goto out;

 /* Release resources. */
 free ( tmp_buf );
 free ( tmp_fill_buf );


return 0;

out:
 H5Dclose( dataset_id1 );
 return -1;
}

/*-------------------------------------------------------------------------
 * Function: H5TBdelete_field
 *
 * Purpose: Deletes a field
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: January 30, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

herr_t H5TBdelete_field( hid_t loc_id,
                         const char *dset_name,
                         const char *field_name )
{

 /* Identifiers for the 1st original dataset */
 hid_t    dataset_id1;
 hid_t    type_id1;
 hid_t    space_id1;
 hid_t    plist_id1;

 /* Identifiers for the 2nd new dataset */
 hid_t    dataset_id2;
 hid_t    type_id2;
 hid_t    space_id2;
 hid_t    plist_id2;

 hid_t    member_type_id;
 size_t   member_size;
 char     *member_name;
 size_t   type_size1;
 size_t   type_size2;
 hsize_t  nfields;
 hsize_t  nrecords;
 hsize_t  dims_chunk[1];
 hsize_t  dims[1];
 hsize_t  maxdims[1] = { H5S_UNLIMITED };
 hid_t    PRESERVE;
 size_t   curr_offset;
 size_t   delete_member_size = 0;
 hid_t    read_type_id;
 hid_t    write_type_id;
 unsigned char *tmp_buf;
 unsigned char *tmp_fill_buf;
 char     attr_name[255];
 char     aux[255];
 char     table_title[255];
 size_t   member_offset;
 hid_t    attr_id;
 hsize_t  i;
 int      has_fill=0;

 /* Get the number of records and fields  */
 if ( H5TBget_table_info ( loc_id, dset_name,  &nfields, &nrecords ) < 0 )
  return -1;

/*-------------------------------------------------------------------------
 * Get information about the old data type
 *-------------------------------------------------------------------------
 */

 /* Open the dataset. */
 if ( (dataset_id1 = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Get creation properties list */
 if ( (plist_id1 = H5Dget_create_plist( dataset_id1 )) < 0 )
  goto out;

 /* Get the datatype */
 if ( (type_id1 = H5Dget_type( dataset_id1 )) < 0 )
  goto out;

 /* Get the size of the datatype */
 type_size1 = H5Tget_size( type_id1 );

 /* Get the dataspace handle */
 if ( (space_id1 = H5Dget_space( dataset_id1 )) < 0 )
  goto out;

 /* Get dimension */
 if ( H5Sget_simple_extent_dims( space_id1, dims, NULL) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Create a new data type; first we find the size of the datatype to delete
 *-------------------------------------------------------------------------
 */

 /* Check out the field */
 for ( i = 0; i < nfields; i++)
 {

  /* Get the member name */
  member_name = H5Tget_member_name( type_id1,(unsigned) i );

  /* We want to find the field to delete */
  if ( H5TB_find_field( member_name, field_name ) > 0 )
  {
   /* Get the member type */
   if ( ( member_type_id = H5Tget_member_type( type_id1,(unsigned) i )) < 0 )
    goto out;

   /* Get the member size */
   delete_member_size = H5Tget_size( member_type_id );

   /* Close the member type */
   if ( H5Tclose( member_type_id ) < 0 )
    goto out;

   free( member_name );

   break;

  }

  free( member_name );

 } /* i */

 /* No field to delete was found */
 if ( delete_member_size == 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Create a new data type; we now insert all the fields into the new type
 *-------------------------------------------------------------------------
 */

 type_size2 = type_size1 - delete_member_size;

 /* Create the data type. */
 if (( type_id2 = H5Tcreate (H5T_COMPOUND, type_size2 )) < 0 )
  goto out;

 curr_offset = 0;

 /* alloc fill value attribute buffer */
 tmp_fill_buf = (unsigned char *)malloc((size_t) type_size2 );

/*-------------------------------------------------------------------------
 * Get attributes from previous table in the process
 *-------------------------------------------------------------------------
 */

 /* Get the table title */
 if ( (H5TBAget_title( dataset_id1, table_title )) < 0 )
  goto out;

 /* Insert the old fields except the one to delete */
 for ( i = 0; i < nfields; i++)
 {

  /* Get the member name */
  member_name = H5Tget_member_name( type_id1, (unsigned) i );

  /* We want to skip the field to delete */
  if ( H5TB_find_field( member_name, field_name ) > 0 )
  {
   free( member_name );
   continue;
  }

  /* Get the member type */
  if ( ( member_type_id = H5Tget_member_type( type_id1, (unsigned)i )) < 0 )
   goto out;

  /* Get the member size */
  member_size = H5Tget_size( member_type_id );

  /* Insert it into the new type */
  if ( H5Tinsert( type_id2, member_name, curr_offset, member_type_id ) < 0 )
    goto out;

 /*-------------------------------------------------------------------------
  * Get the fill value information
  *-------------------------------------------------------------------------
  */

  strcpy( attr_name, "FIELD_" );
  sprintf( aux, "%d", (int)i );
  strcat( attr_name, aux );
  sprintf( aux, "%s", "_FILL" );
  strcat( attr_name, aux );

  /* Check if we have the _FILL attribute */
  has_fill = H5LT_find_attribute( dataset_id1, attr_name );

  /* Get it */
  if ( has_fill == 1 )
  {
   if ( H5LT_get_attribute_disk( dataset_id1, attr_name, tmp_fill_buf+curr_offset ) < 0 )
    goto out;
  }

  curr_offset += member_size;

  free( member_name );

  /* Close the member type */
  if ( H5Tclose( member_type_id ) < 0 )
   goto out;

 } /* i */

/*-------------------------------------------------------------------------
 * Create a new temporary dataset
 *-------------------------------------------------------------------------
 */

 /* Retrieve the size of chunk */
 if ( H5Pget_chunk( plist_id1, 1, dims_chunk ) < 0 )
  goto out;

 /* Create a new simple data space with unlimited size, using the dimension */
 if ( ( space_id2 = H5Screate_simple( 1, dims, maxdims )) < 0 )
  return -1;

 /* Modify dataset creation properties, i.e. enable chunking  */
 plist_id2 = H5Pcreate (H5P_DATASET_CREATE);
 if ( H5Pset_chunk ( plist_id2, 1, dims_chunk ) < 0 )
  return -1;

 /* Create the dataset. */
 if ( ( dataset_id2 = H5Dcreate( loc_id, "new", type_id2, space_id2, plist_id2 )) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * We have to read field by field of the old dataset and save it into the new one
 *-------------------------------------------------------------------------
 */
 for ( i = 0; i < nfields; i++)
 {

  /* Get the member name */
  member_name = H5Tget_member_name( type_id1,(unsigned) i );

  /* Skip the field to delete */
  if ( H5TB_find_field( member_name, field_name ) > 0 )
  {
   free( member_name );
   continue;
  }

  /* Get the member type */
  if ( ( member_type_id = H5Tget_member_type( type_id1, (unsigned)i )) < 0 )
   goto out;

  /* Get the member size */
  member_size = H5Tget_size( member_type_id );

  /* Create a read id */
  if ( ( read_type_id = H5Tcreate( H5T_COMPOUND, member_size )) < 0 )
   goto out;

  /* Insert it into the new type */
  if ( H5Tinsert( read_type_id, member_name, (size_t)0, member_type_id ) < 0 )
    goto out;

  tmp_buf = (unsigned char *)calloc((size_t) nrecords, member_size );

  /* Read */
  if ( H5Dread( dataset_id1, read_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, tmp_buf ) < 0 )
   goto out;

  /* Create a write id */
  if ( ( write_type_id = H5Tcreate( H5T_COMPOUND, member_size )) < 0 )
   goto out;

  /* The field in the file is found by its name */
  if ( H5Tinsert( write_type_id, member_name, (size_t)0, member_type_id ) < 0 )
   goto out;

  /* Create xfer properties to preserve initialized data */
  if ((PRESERVE = H5Pcreate (H5P_DATASET_XFER))<0)
   goto out;
  if (H5Pset_preserve (PRESERVE, 1)<0)
   goto out;

  /* Write */
  if ( H5Dwrite( dataset_id2, write_type_id, H5S_ALL, H5S_ALL, PRESERVE, tmp_buf ) < 0 )
   goto out;

  /* End access to the property list */
  if ( H5Pclose( PRESERVE ) < 0 )
   goto out;

 /* Close the member type */
  if ( H5Tclose( member_type_id ) < 0 )
   goto out;

  /* Close the read type */
  if ( H5Tclose( read_type_id ) < 0 )
   goto out;

  /* Close the write type */
  if ( H5Tclose( write_type_id ) < 0 )
   goto out;

  /* Release resources. */
  free( member_name );
  free ( tmp_buf );

 } /* i */

/*-------------------------------------------------------------------------
 * Release resources from 1st table
 *-------------------------------------------------------------------------
 */

 /* Release the datatype. */
 if ( H5Tclose( type_id1 ) < 0 )
  goto out;

 /* Terminate access to a property list */
 if ( H5Pclose( plist_id1 ) < 0 )
  goto out;

 /* Terminate access to the data space */
 if ( H5Sclose( space_id1 ) < 0 )
  goto out;

 /* End access to the dataset */
 if ( H5Dclose( dataset_id1 ) < 0 )
  goto out;


/*-------------------------------------------------------------------------
 * Release resources from 2nd table
 *-------------------------------------------------------------------------
 */

 /* Terminate access to the dataspace */
 if ( H5Sclose( space_id2 ) < 0 )
  goto out;

 /* Release the datatype. */
 if ( H5Tclose( type_id2 ) < 0 )
  return -1;

 /* Terminate access to a property list */
 if ( H5Pclose( plist_id2 ) < 0 )
  goto out;

 /* End access to the dataset */
 if ( H5Dclose( dataset_id2 ) < 0 )
  return -1;

/*-------------------------------------------------------------------------
 * Delete 1st table
 *-------------------------------------------------------------------------
 */

 if ( H5Gunlink( loc_id, dset_name ) < 0 )
  return -1;

/*-------------------------------------------------------------------------
 * Rename 2nd table
 *-------------------------------------------------------------------------
 */

 if ( H5Gmove( loc_id, "new", dset_name ) < 0 )
  return -1;

/*-------------------------------------------------------------------------
 * Attach the conforming table attributes
 *-------------------------------------------------------------------------
 */

 /* Get the number of records and fields  */
 if ( H5TBget_table_info ( loc_id, dset_name, &nfields, &nrecords ) < 0 )
  return -1;

 /* Open the dataset. */
 if ( (dataset_id1 = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Get the datatype */
 if ( (type_id1 = H5Dget_type( dataset_id1 )) < 0 )
  goto out;

 /* Set the attributes */
 if ( H5TB_attach_attributes( table_title, loc_id, dset_name, nfields, type_id1 ) < 0 )
  return -1;

/*-------------------------------------------------------------------------
 * Attach the fill attributes from previous table
 *-------------------------------------------------------------------------
 */

 if ( has_fill == 1 )
 {

  if (( space_id1 = H5Screate(H5S_SCALAR)) < 0 )
   goto out;

  for ( i = 0; i < nfields; i++)
  {

   /* Get the member type */
   if ( ( member_type_id = H5Tget_member_type( type_id1, (unsigned)i )) < 0 )
    goto out;

   /* Get the member offset */
   member_offset = H5Tget_member_offset( type_id1, (unsigned)i );

   strcpy( attr_name, "FIELD_" );
   sprintf( aux, "%d", (int)i );
   strcat( attr_name, aux );
   sprintf( aux, "%s", "_FILL" );
   strcat( attr_name, aux );

   if ( (attr_id = H5Acreate( dataset_id1, attr_name, member_type_id, space_id1, H5P_DEFAULT )) < 0 )
    goto out;

   if ( H5Awrite( attr_id, member_type_id, tmp_fill_buf+member_offset ) < 0 )
    goto out;

   if ( H5Aclose( attr_id ) < 0 )
    goto out;

   /* Close the member type */
   if ( H5Tclose( member_type_id ) < 0 )
    goto out;

  }

  /* Close data space. */
  if ( H5Sclose( space_id1 ) < 0 )
   goto out;

 } /*has_fill*/

 /* Release the datatype. */
 if ( H5Tclose( type_id1 ) < 0 )
  goto out;

 /* End access to the dataset */
 if ( H5Dclose( dataset_id1 ) < 0 )
  goto out;

 /* Release resources. */
 free ( tmp_fill_buf );

return 0;

out:
 H5Dclose( dataset_id1 );
 return -1;
}

/*-------------------------------------------------------------------------
 *
 * Table attribute functions
 *
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: H5TBAget_title
 *
 * Purpose: Read the table title
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: January 30, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5TBAget_title( hid_t loc_id,
                       char *table_title )
{

 /* Get the TITLE attribute */
 if ( H5LT_get_attribute_disk( loc_id, "TITLE", table_title ) < 0 )
  return -1;


 return 0;

}

/*-------------------------------------------------------------------------
 * Function: H5TBAget_fill
 *
 * Purpose: Read the table attribute fill values
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: January 30, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5TBAget_fill( hid_t loc_id,
                      const char *dset_name,
                      hid_t dset_id,
                      unsigned char *dst_buf )
{

 hsize_t nfields;
 hsize_t nrecords;
 char    attr_name[255];
 char    aux[255];
 hsize_t i;
 size_t  *src_offset;
 int     has_fill=0;

 /* Get the number of records and fields  */
 if ( H5TBget_table_info ( loc_id, dset_name, &nfields, &nrecords ) < 0 )
  return -1;

 src_offset = (size_t *)malloc((size_t)nfields * sizeof(size_t));

 if (src_offset == NULL )
  return -1;

 /* Get field info */
 if ( H5TBget_field_info( loc_id, dset_name, NULL, NULL, src_offset, NULL ) < 0 )
  goto out;

 for ( i = 0; i < nfields; i++)
 {
  strcpy( attr_name, "FIELD_" );
  sprintf( aux, "%d", (int)i );
  strcat( attr_name, aux );
  sprintf( aux, "%s", "_FILL" );
  strcat( attr_name, aux );

  /* Check if we have the _FILL attribute */
  has_fill = H5LT_find_attribute( dset_id, attr_name );

  /* Get it */
  if ( has_fill == 1 )
  {
   if ( H5LT_get_attribute_disk( dset_id, attr_name, dst_buf+src_offset[i] ) < 0 )
    goto out;
  }

 }

 free( src_offset );

 return has_fill;

out:
 free( src_offset );
 return -1;

}


/*-------------------------------------------------------------------------
 *
 * Inquiry functions
 *
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: H5TBget_table_info
 *
 * Purpose: Gets the number of records and fields of a table
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 19, 2001
 *
 * Comments:
 *
 * Modifications: May 08, 2003
 *  In version 2.0 of Table, the number of records is stored as an
 *  attribute "NROWS"
 *
 *
 *-------------------------------------------------------------------------
 */

herr_t H5TBget_table_info ( hid_t loc_id,
                            const char *dset_name,
                            hsize_t *nfields,
                            hsize_t *nrecords )
{
 hid_t      tid;
 hid_t      sid=-1;
 hid_t      did;
 int        num_members;
 hsize_t    dims[1];
 int        has_attr;
 hsize_t    n[1];

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Get the datatype */
 if ( (tid = H5Dget_type( did )) < 0 )
  goto out;

 /* Get the number of members */
 if ( (num_members = H5Tget_nmembers( tid )) < 0 )
  goto out;

 if (nfields)
  *nfields = num_members;


/*-------------------------------------------------------------------------
 * Get number of records
 *-------------------------------------------------------------------------
 */

 if (nrecords)
 {
  /* Try to find the attribute "NROWS" */
  has_attr = H5LT_find_attribute( did, "NROWS" );

  /* It exists, get it */
  if ( has_attr == 1 )
  {
   /* Get the attribute */
   if ( H5LTget_attribute(loc_id,dset_name,"NROWS",H5T_NATIVE_LLONG,n)<0)
    return -1;

   *nrecords = *n;
  }
  else
  {
   /* Get the dataspace handle */
   if ( (sid = H5Dget_space( did )) < 0 )
    goto out;

   /* Get records */
   if ( H5Sget_simple_extent_dims( sid, dims, NULL) < 0 )
    goto out;

   /* Terminate access to the dataspace */
   if ( H5Sclose( sid ) < 0 )
    goto out;

   *nrecords = dims[0];
  }
 }/*nrecords*/

 /* close */
 if ( H5Tclose( tid ) < 0 )
  goto out;
 if ( H5Dclose( did ) < 0 )
  return -1;

return 0;

/* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Dclose(did);
  H5Sclose(sid);
  H5Tclose(tid);
 } H5E_END_TRY;
 return -1;

}

/*-------------------------------------------------------------------------
 * Function: H5TBget_field_info
 *
 * Purpose: Get information about fields
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 19, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */
herr_t H5TBget_field_info( hid_t loc_id,
                           const char *dset_name,
                           char *field_names[],
                           size_t *field_sizes,
                           size_t *field_offsets,
                           size_t *type_size )
{
 hid_t         did;
 hid_t         ftype_id;
 hid_t         native_type_id;
 hssize_t      nfields;
 char          *member_name;
 hid_t         member_type_id;
 hid_t         nativem_type_id;
 size_t        member_size;
 size_t        member_offset;
 size_t        size;
 hssize_t      i;

 /* Open the dataset. */
 if ( ( did = H5Dopen( loc_id, dset_name )) < 0 )
  goto out;

 /* Get the datatype */
 if ( ( ftype_id = H5Dget_type( did )) < 0 )
  goto out;

 if ((native_type_id = H5Tget_native_type(ftype_id,H5T_DIR_DEFAULT))<0)
  goto out;

 /* Get the type size */
 size = H5Tget_size( native_type_id );

 if ( type_size )
  *type_size = size;

 /* Get the number of members */
 if ( ( nfields = H5Tget_nmembers( ftype_id )) < 0 )
  goto out;

 /* Iterate tru the members */
 for ( i = 0; i < nfields; i++)
 {
  /* Get the member name */
  member_name = H5Tget_member_name( ftype_id, (unsigned)i );

  if ( field_names )
   strcpy( field_names[i], member_name );

  /* Get the member type */
  if ( ( member_type_id = H5Tget_member_type( ftype_id,(unsigned) i )) < 0 )
   goto out;
  if ((nativem_type_id = H5Tget_native_type(member_type_id,H5T_DIR_DEFAULT))<0)
   goto out;

  /* Get the member size */
  member_size = H5Tget_size( nativem_type_id );

  if ( field_sizes )
   field_sizes[i] = member_size;

  /* Get the member offset */
  member_offset = H5Tget_member_offset( native_type_id,(unsigned) i );

  if ( field_offsets )
   field_offsets[i] = member_offset;

  /* Close the member type */
  if ( H5Tclose( member_type_id ) < 0 )
   goto out;
  if ( H5Tclose( nativem_type_id ) < 0 )
   goto out;

  free( member_name );

 } /* i */

 /* Release the datatype. */
 if ( H5Tclose( ftype_id ) < 0 )
  return -1;
 if ( H5Tclose( native_type_id ) < 0 )
  return -1;

 /* End access to the dataset */
 if ( H5Dclose( did ) < 0 )
  return -1;

return 0;

out:
 H5Dclose( did );
 return -1;

}

/*-------------------------------------------------------------------------
 *
 * internal functions
 *
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: H5TB_find_field
 *
 * Purpose: Find a string field
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 19, 2001
 *
 *-------------------------------------------------------------------------
 */

static
int H5TB_find_field( const char *field, const char *field_list )
{
    const char *start = field_list;
    const char *end;
    
    while ( (end = strstr( start, "," )) != 0 ) 
    {
        size_t count = end - start;
        if ( strncmp(start, field, count) == 0 && count == strlen(field) )
            return 1;
        start = end + 1;
    }
    
    if ( strcmp( start, field ) == 0 )
        return 1;
    
    return -1;
    
}


/*-------------------------------------------------------------------------
 * Function: H5TB_attach_attributes
 *
 * Purpose: Private function that creates the conforming table attributes;
 *          Used by H5TBcombine_tables; not used by H5TBmake_table, which does not read
 *          the fill value attributes from an existing table
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 6, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static
herr_t H5TB_attach_attributes( const char *table_title,
                               hid_t loc_id,
                               const char *dset_name,
                               hsize_t nfields,
                               hid_t tid )
{

 char    attr_name[255];
 char    *member_name;
 char    aux[255];
 hsize_t i;

 /* Attach the CLASS attribute */
 if ( H5LTset_attribute_string( loc_id, dset_name, "CLASS", TABLE_CLASS ) < 0 )
  goto out;

 /* Attach the VERSION attribute */
 if ( H5LTset_attribute_string( loc_id, dset_name, "VERSION", "2.0" ) < 0 )
  goto out;

 /* Attach the TITLE attribute */
 if ( H5LTset_attribute_string( loc_id, dset_name, "TITLE", table_title ) < 0 )
  goto out;

 /* Attach the FIELD_ name attribute */
 for ( i = 0; i < nfields; i++)
 {

  /* Get the member name */
  member_name = H5Tget_member_name( tid, (unsigned)i );

  strcpy( attr_name, "FIELD_" );
  sprintf( aux, "%d", (int)i );
  strcat( attr_name, aux );
  sprintf( aux, "%s", "_NAME" );
  strcat( attr_name, aux );

  /* Attach the attribute */
  if ( H5LTset_attribute_string( loc_id, dset_name, attr_name, member_name ) < 0 )
   goto out;

  free( member_name );

 }

 return 0;

out:
 return -1;

}

/*-------------------------------------------------------------------------
 * Function: H5TB_create_type
 *
 * Purpose: Private function that creates a memory type ID
 *
 * Return: Success: the memory type ID, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 31, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static
hid_t H5TB_create_type(hid_t loc_id,
                       const char *dset_name,
                       size_t type_size,
                       const size_t *field_offset,
                       const size_t *field_sizes,
                       hid_t ftype_id)
{
 hid_t    mem_type_id;
 hid_t    mtype_id=-1;
 hid_t    nmtype_id=-1;
 size_t   size_native;
 hsize_t  nfields;
 char     **fnames;
 unsigned i;

 /* get the number of fields  */
 if (H5TBget_table_info(loc_id,dset_name,&nfields,NULL)<0)
  return -1;

 if ((fnames=malloc(sizeof(char*)*(size_t)nfields))==NULL)
  return -1;

 for ( i=0; i<nfields; i++)
 {
  if ((fnames[i]=malloc(sizeof(char)*HLTB_MAX_FIELD_LEN))==NULL) {
   free(fnames);
   return -1;
  }
 }

 /* get field info */
 if ( H5TBget_field_info(loc_id,dset_name,fnames,NULL,NULL,NULL)<0)
  goto out;

 /* create the memory data type */
 if ((mem_type_id=H5Tcreate(H5T_COMPOUND,type_size))<0)
  goto out;

 /* get each field ID and adjust its size, if necessary */
 for ( i=0; i<nfields; i++)
 {
  if ((mtype_id=H5Tget_member_type(ftype_id,i))<0)
   goto out;
  if ((nmtype_id=H5Tget_native_type(mtype_id,H5T_DIR_DEFAULT))<0)
   goto out;
  size_native=H5Tget_size(nmtype_id);
  if (field_sizes[i]!=size_native)
  {
   if (H5Tset_size(nmtype_id,field_sizes[i])<0)
    goto out;
  }
  if (H5Tinsert(mem_type_id,fnames[i],field_offset[i],nmtype_id) < 0 )
   goto out;
  if (H5Tclose(mtype_id)<0)
   goto out;
  if (H5Tclose(nmtype_id)<0)
   goto out;
 }

 for ( i=0; i<nfields; i++)
 {
  free (fnames[i]);
 }
 free (fnames);

 return mem_type_id;

 /* error zone, gracefully close and free */
out:
 H5E_BEGIN_TRY {
  H5Tclose(mtype_id);
  H5Tclose(nmtype_id);
 } H5E_END_TRY;
 for ( i=0; i<nfields; i++)
 {
  if (fnames[i])
   free (fnames[i]);
 }
 if (fnames)
  free (fnames);
 return -1;

}


/*-------------------------------------------------------------------------
 *
 * Functions shared between H5TB and H5PT
 *
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: H5TB_common_append_records
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
herr_t H5TB_common_append_records( hid_t dataset_id,
                                  hid_t mem_type_id,
                                  size_t nrecords,
                                  hsize_t orig_table_size,
                                  const void * data)
{
 hsize_t  count[1];
 hsize_t  offset[1];
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
 if ( H5Sselect_hyperslab( space_id, H5S_SELECT_SET, offset, NULL, count, NULL)<0)
  goto out;

 /* Write the records */
 if ( H5Dwrite( dataset_id, mem_type_id, mem_space_id, space_id, H5P_DEFAULT, data )<0)
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
 * Function: H5TB_common_read_records
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
herr_t H5TB_common_read_records( hid_t dataset_id,
                                hid_t mem_type_id,
                                hsize_t start,
                                size_t nrecords,
                                hsize_t table_size,
                                void *data)
{
 hsize_t  count[1];
 hsize_t  offset[1];
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
 if ((H5Dread( dataset_id, mem_type_id, mem_space_id, space_id, H5P_DEFAULT, data))<0)
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

