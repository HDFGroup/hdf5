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

#include "H5IMpalette.h"
#include "H5Iprivate.h"	
#include "H5Pprivate.h"	


/*local operator function */
static herr_t find_palette( hid_t UNUSED loc_id, const char *name, void *op_data);
/*private function */
herr_t H5IM_find_palette( hid_t loc_id );


/*-------------------------------------------------------------------------
 * Function: H5IMattach_palette
 *
 * Purpose: This function attaches a palette to an existing image dataset
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 01, 2001
 *
 * Comments:
 *  based on HDF5 Image and Palette Specification  
 *  http://hdf.ncsa.uiuc.edu/HDF5/H5Image/ImageSpec.html
 *  
 *  An image (dataset) within an HDF5 file may optionally specify an array of 
 *  palettes to be viewed with. The dataset will have an attribute 
 *  which contains an array of object reference pointers which refer to palettes in the file. 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5IMattach_palette( hid_t loc_id, 
                           const char *image_name, 
                           const char *pal_name,
                           const hsize_t *pal_dims,
                           hid_t plist_id,
                           const void *pal_data ) 

{                       

 hid_t       image_id;
 hid_t       attr_type;
 hid_t       attr_id;
 hid_t       attr_space_id;
 herr_t      status;
 hid_t       attr_class;
 hobj_ref_t  ref;         /* write a new reference */
 hobj_ref_t  *refbuf;     /* buffer to read references */
 hssize_t    n_refs;
 hsize_t     dim_ref;
 int         ok_pal;
 const H5IM_paletteinfo_t *info;

 /* First we get some information about the palette */

 /* Default palette */

 if ( plist_id == H5IM_STDPALETTE ) 
 {
	 info = &H5IM_create_dfltpalette;
 }

 /* User defined palette */

 else
 {

   /* Check valid plist, get it */
  if ( H5P_get_class(plist_id) != H5P_PALETTE ||
       NULL == ( info = H5I_object(plist_id))) 
  {
   return FAIL;
  }


 }

 
/* First we create the palette dataset and attach to it the required attributes 
 * that identify it as a palette
 */

 /* Make the palette dataset. The palette is created on the same local id.  */
 status = H5Lmake_dataset( loc_id, pal_name, 2, pal_dims, H5T_NATIVE_UCHAR, H5T_NATIVE_UCHAR, pal_data );

 /* Attach the attribute "CLASS" to the >>palette<< dataset*/
 status = H5Lattach_attribute( loc_id, pal_name, "CLASS", "PALETTE" );

  /* Attach the attribute "PAL_VERSION" to the >>palette<< dataset*/
 status = H5Lattach_attribute( loc_id, pal_name, "PAL_VERSION", "1.2" );

 /* Attach the attribute "PAL_COLORMODEL" to the >>palette<< dataset*/
 status = H5Lattach_attribute( loc_id, pal_name, "PAL_COLORMODEL", info->color_model );

 /* Attach the attribute "PAL_TYPE" to the >>palette<< dataset*/
 status = H5Lattach_attribute( loc_id, pal_name, "PAL_TYPE", info->type );

  /* Attach the PAL_MINMAXNUMERIC attribute */
 status = H5Lattach_attribute_numerical( loc_id, pal_name, "PAL_MINMAXNUMERIC", 2, H5T_NATIVE_INT, (void*)&info->minmax );

 /* The image dataset may or not have the attribute "PALETTE" 
  * First we try to open to see if it is already there; if not, it is created.
  * If it exists, the array of references is extended to hold the reference 
  * to the new palette
  */
 
 /* First wet the image id */
 image_id = H5Dopen( loc_id, image_name );

 /* Try to find the attribute "PALETTE" on the >>image<< dataset */
 ok_pal = H5IM_find_palette( image_id );

 /* It does not exist. We create the attribute and one reference */
 if ( ok_pal <= 0 )
 {

  attr_space_id = H5Screate( H5S_SCALAR );
  
  /* Create the attribute type for the reference */
  attr_type = H5Tcopy( H5T_STD_REF_OBJ );
 
  /* Create the attribute "PALETTE" to be attached to the image*/
  attr_id = H5Acreate( image_id, "PALETTE", attr_type, attr_space_id, H5P_DEFAULT );

  /* Create a reference. The reference is created on the local id.  */
  status = H5Rcreate( &ref, loc_id, pal_name, H5R_OBJECT, -1 ); 

  /* Write the attribute with the reference */
  status = H5Awrite( attr_id, attr_type, &ref );

  status = H5Sclose( attr_space_id );

 }

 /* The attribute already exists, open it */

 else if ( ok_pal ==  1 )

 {

  attr_id = H5Aopen_name( image_id, "PALETTE" );
     
  attr_type = H5Aget_type( attr_id );

  attr_class = H5Tget_class( attr_type );

  /* Check if it is really a reference */

  if ( attr_class == H5T_REFERENCE )
  {

   /* Get and save the old reference(s) */

   attr_space_id = H5Aget_space( attr_id );

   n_refs = H5Sget_simple_extent_npoints( attr_space_id );

   dim_ref = n_refs + 1;

   refbuf = malloc( sizeof(hobj_ref_t) * (int)dim_ref );

   status = H5Aread( attr_id, attr_type, refbuf );

   status = H5Sclose( attr_space_id );

   /* The attribute must be deleted, in order to the new one can reflect the changes*/
   status = H5Adelete( image_id, "PALETTE" );

   /* Create a new reference for this palette. */
   status = H5Rcreate( &ref, loc_id, pal_name, H5R_OBJECT, -1 ); 

   refbuf[n_refs] = ref;

   /* Create the data space for the new references */
   attr_space_id = H5Screate_simple( 1, &dim_ref, NULL );

   /* Create the attribute again with the changes of space */
   attr_id = H5Acreate( image_id, "PALETTE", attr_type, attr_space_id, H5P_DEFAULT );

    /* Write the attribute with the new references */
   status = H5Awrite( attr_id, attr_type, refbuf );

   status = H5Sclose( attr_space_id );

   free( refbuf );
    
  } /* H5T_REFERENCE */ 
    
  status = H5Tclose( attr_type ); 

  /* Close the attribute. */  
  status = H5Aclose( attr_id );

 }
 
  /* Close the image dataset. */
 status = H5Dclose( image_id );
 
 return status;
}




/*-------------------------------------------------------------------------
 * Function: find_palette
 *
 * Purpose: operator function used by H5L_find_palette
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

static herr_t find_palette( hid_t UNUSED loc_id, const char *name, void *op_data)
{

 /* Define a default zero value for return. This will cause the iterator to continue if 
  * the palette attribute is not found yet.
  */

 int ret = 0;   
 
 /* Define a positive value for return value if the attribute was found. This will 
  * cause the iterator to immediately return that positive value, 
  * indicating short-circuit success 
  */
 
 if( HDstrcmp( name, "PALETTE" ) == 0 ) 
  ret = 1;


 return ret;
} 


/*-------------------------------------------------------------------------
 * Function: H5IM_find_palette
 *
 * Purpose: Find the atrribute "PALETTE" in the image dataset 
 *
 * Return: Success: 1, Failure: 0
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 11, 2001
 *
 * Comments:
 *  The function uses H5Aiterate with the operator function find_palette
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IM_find_palette( hid_t loc_id ) 
{

 unsigned int attr_num;     /* Starting attribute to look up */
 herr_t       ret;

 attr_num = 0;
 ret = H5Aiterate( loc_id, &attr_num, find_palette, 0 );

 return ret;
}
