/****************************************************************************
 * NCSA HDF                                                                 *
 * Scientific Data Technologies                                             *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/



#include "H5Lite.h"

/*local operator functions */

static herr_t count_groups( hid_t loc_id, const char *name, void *op_data);
static herr_t get_name_group( hid_t  loc_id, const char *name, void *op_data);



/*-------------------------------------------------------------------------
 * Function: H5Lmake_dataset
 *
 * Purpose:
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 19, 2001
 *
 * List of arguments:
 *
 *  hid_t loc_id 
 *  IN: Identifier of the file or group to create the dataset within. 
 *
 *  const char *dset_name 
 *  IN: The name of the dataset to create. 
 *
 *  int rank 
 *  IN: Number of dimensions of dataspace.
 *
 *  const hsize_t * dims 
 *  IN: An array of the size of each dimension. 
 *
 *  hid_t file_type_id 
 *  IN: Identifier of the datatype to use when creating the dataset. 
 *
 *  hid_t mem_space_id 
 *  IN: Identifier of the memory dataspace. 
 *
 *  const void * buffer 
 *  IN: Buffer with data to be written to the datsset. 
 *
 * Comments:
 *
 * The specified datatype and dataspace are the datatype and dataspace of the dataset 
 * as it will exist in the file, which may be different than in application memory
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

herr_t H5Lmake_dataset( hid_t loc_id, 
                        const char *dset_name, 
                        int rank, 
                        const hsize_t *dims,
                        hid_t file_type_id,
                        hid_t mem_type_id,
                        const void *buffer ) 
{

 hid_t   dataset_id, space_id;  /* identifiers */
 herr_t  status;
 
 /* Create the data space for the dataset. */
 space_id = H5Screate_simple( rank, dims, NULL );

 /* Create the dataset. */
 dataset_id = H5Dcreate( loc_id, dset_name, file_type_id, space_id, H5P_DEFAULT );

 /* Write the dataset only if there is data to write */

 if ( buffer ) 

 status = H5Dwrite( dataset_id, mem_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer );

 /* End access to the dataset and release resources used by it. */
 status = H5Dclose( dataset_id );

 /* Terminate access to the data space. */ 
 status = H5Sclose( space_id );

 return status;
}



/*-------------------------------------------------------------------------
 * Function: H5Lattach_attribute
 *
 * Purpose: This function attaches a string attribute to an existing object
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: April 18, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5Lattach_attribute( hid_t loc_id, 
                        const char *obj_name, 
                        const char *attr_name,
                        const char *attr_data ) 
{

 /* identifiers */
 hid_t      attr_type;
 hid_t      attr_size;
 hid_t      attr_space_id;
 hid_t      attr_id;
 hid_t      obj_id;  
 herr_t     status;
 H5I_type_t type;

 type = H5Iget_type( loc_id );

 switch ( type )
 {
  case H5I_DATASET:
  case H5I_FILE:

   /* Open the dataset. */
   obj_id = H5Dopen( loc_id, obj_name );
   break;

  case H5I_GROUP:

   /* Open the group. */
   obj_id = H5Gopen( loc_id, obj_name );
   break;

  default:
   return FAIL; 

 }

 /* Create the attribute */

 attr_type = H5Tcopy( H5T_C_S1 );

 attr_size = strlen( attr_data );

 status = H5Tset_size( attr_type, (size_t)attr_size);

 status = H5Tset_strpad( attr_type, H5T_STR_SPACEPAD );

 attr_space_id = H5Screate( H5S_SCALAR );

 attr_id = H5Acreate( obj_id, attr_name, attr_type, attr_space_id, H5P_DEFAULT );

 status = H5Awrite( attr_id, attr_type, attr_data );

 status = H5Sclose( attr_space_id );
   
 status = H5Aclose( attr_id );


 switch ( type )
 {
  case H5I_DATASET:
  case H5I_FILE:

   /* Close the dataset. */
   status = H5Dclose( obj_id );
   break;

  case H5I_GROUP:

   /* Close the group. */
   status = H5Gclose( obj_id );
   break;

 }

 return status;
}



/*-------------------------------------------------------------------------
 * Function: H5Lattach_attribute_numerical
 *
 * Purpose: This function attaches a numerical 1D attribute to an existing object (dataset or group)
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 19, 2001
 *
 * Comments:
 *
 * The attribute is one dimensional
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5Lattach_attribute_numerical( hid_t loc_id, 
                        const char *obj_name, 
                        const char *attr_name,
                        hsize_t size,
                        hid_t type_id,
                        void *buffer ) 
{

 hid_t      obj_id, space_id, attr_id;  
 herr_t     status;
 H5I_type_t type;

 type = H5Iget_type( loc_id );

 switch ( type )
 {
  case H5I_DATASET:
  case H5I_FILE:

   /* Open the dataset. */
   obj_id = H5Dopen( loc_id, obj_name );
   break;

  case H5I_GROUP:

   /* Open the group. */
   obj_id = H5Gopen( loc_id, obj_name );
   break;

  default:
   return -1;

 }

 /* Create the data space for the attribute. */
 space_id = H5Screate_simple( 1, &size, NULL );

 /* Create the attribute. */
 attr_id = H5Acreate( obj_id, attr_name, type_id, space_id, H5P_DEFAULT );

 /* Write the attribute data. */
 status = H5Awrite( attr_id, type_id, buffer );

 /* Close the attribute. */
 status = H5Aclose( attr_id );

 /* Close the dataspace. */
 status = H5Sclose( space_id );

 switch ( type )
 {
  case H5I_DATASET:
  case H5I_FILE:

   /* Close the dataset. */
   status = H5Dclose( obj_id );
   break;

  case H5I_GROUP:

   /* Close the group. */
   status = H5Gclose( obj_id );
   break;

 }

 return status;
}




/*-------------------------------------------------------------------------
 * Function: H5Lmake_groups
 *
 * Purpose: 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 24, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5Lmake_groups( hid_t loc_id, 
                       int ngroups,
                       const char *names[] ) 
{

 herr_t   status;
 hid_t    group_id;
 int      i;
 
 for ( i = 0; i < ngroups; i++)
 {

  /* Create the group */
  group_id = H5Gcreate( loc_id, names[i], 0);

  /* Close the group. */
  status = H5Gclose( group_id );

 }

 return status;
}



/*-------------------------------------------------------------------------
 * Function: H5Lget_groups
 *
 * Purpose: 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 24, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5Lget_groups( hid_t loc_id,
                      const char *group_name ) 
{

 herr_t   status;
 char     name[255];
 int      i, j;
 int      ngroups;

 ngroups = 0;

 /* Get the number of groups */
 status = H5Giterate( loc_id, group_name, 0, count_groups, (void *)&ngroups );

 for ( i = 0; i < ngroups; i++)
 {

  j = i;

  /* Get name */
  status = H5Giterate( loc_id, group_name, &j, get_name_group, (void *)name );

  printf("Name : %s\n", name);
 
 }

 return status;
}


/*-------------------------------------------------------------------------
 * Function: count_groups
 *
 * Purpose: operator function used by H5Lget_groups
 *
 * Return: 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 28, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t count_groups( hid_t  loc_id, const char *name, void *op_data)
{

 /* Define a default zero value for return. This will cause the iterator to continue */

 int        ret = 0;   

 /* Get information about object */

 H5G_stat_t statbuf;
 herr_t     status;

	status = H5Gget_objinfo( loc_id, name, 0, &statbuf );
	if ( status < 0) 
 {
		return 1; /* iterator will stop */
	} 

	if ( statbuf.type == H5G_GROUP ) 
 {
  (*(int *)op_data)++;
 }

 return ret;

}

/*-------------------------------------------------------------------------
 * Function: get_name_group
 *
 * Purpose: operator function used by H5Lget_groups
 *
 * Return: 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 28, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t get_name_group( hid_t  loc_id, const char *name, void *op_data)
{

 /* Define a default 1 value for return. This will cause the iterator to break */

 int        ret = 1;   

 /* Get information about object */

 H5G_stat_t statbuf;
 herr_t     status;

	status = H5Gget_objinfo( loc_id, name, 0, &statbuf );

	if ( statbuf.type == H5G_GROUP ) 
 {
  strcpy( (char *)op_data, name );
 }

 return ret;
} 





/*-------------------------------------------------------------------------
 * Function: H5L_get_attributes
 *
 * Purpose: 
 *
 * Return: 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 28, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5Lget_attributes( hid_t loc_id ) 
{

 hid_t       attr_id;
 hid_t       type_id;
 H5T_class_t class_type;
	char        name[255];
	int         status;
 int         n, i;
	
	n = H5Aget_num_attrs( loc_id );

	for ( i = 0; i < n; i++)
	{

		/*open id */
		attr_id = H5Aopen_idx( loc_id, i );

		/*get name */
		H5Aget_name( attr_id, 255, name );
	
	 /*open type id */
	 type_id = H5Aget_type( attr_id );

	  /*get class */
  class_type = H5Tget_class( type_id );

  printf("%s\n", name );
	

		/*close type id */
	 status = H5Tclose( type_id ); 
		 
		/*close attr id */
		status = H5Aclose( attr_id );
		
	}


 return n;

}





