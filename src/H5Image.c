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

#include "H5Image.h"


/*-------------------------------------------------------------------------
 * Function: H5IMmake_image
 *
 * Purpose:
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
 *  The two required attributes that identify the data as an image are 
 *  the name="CLASS" with the value of "IMAGE", and the name="VERSION"
 *  with the current value of "1.2"
 *   
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5IMmake_image( hid_t loc_id, 
                       const char *dset_name, 
                       const hsize_t *dims,
                       hid_t plist_id,
                       const void *buffer )
{

 char     *attr_name[] = {"CLASS","IMAGE_VERSION"};
 char     *attr_data[] = {"IMAGE","1.2"};
 int      rank;
 herr_t   status;
 const H5IM_imageinfo_t *imageinfo;

 if ( plist_id == H5IM_8BIT ) 
 {
	 imageinfo = &H5IM_create_dflt8bit;
  rank      = 2;
 }
 else
 {
 /*to do */
 }
 
 /* First we make the dataset */
 status = H5Lmake_dataset( loc_id, dset_name, rank, dims, H5T_NATIVE_UCHAR, H5T_NATIVE_UCHAR, buffer );

 /* Then we attach the required attributes */

 /* Attach the CLASS attribute */
 status = H5Lattach_attribute( loc_id, dset_name, attr_name[0], attr_data[0] );

 /* Attach the VERSION attribute */
 status = H5Lattach_attribute( loc_id, dset_name, attr_name[1], attr_data[1] );

 /* Attach the IMAGE_SUBCLASS attribute */
 status = H5Lattach_attribute( loc_id, dset_name, "IMAGE_SUBCLASS", imageinfo->subclass );

 /* Attach the INTERLACE_MODE attribute. This attribute is only for true color images*/
 if ( rank == 3 )
  status = H5Lattach_attribute( loc_id, dset_name, "INTERLACE_MODE", imageinfo->interlace );


 /* We now attach the optional attributes */

 /* Attach the IMAGE_COLORMODEL attribute */
 status = H5Lattach_attribute( loc_id, dset_name, "IMAGE_COLORMODEL", imageinfo->color_model );


 return status;
}








#if 0





herr_t H5Lmake_image( hid_t loc_id, 
                        const char *dset_name, 
                        const hsize_t *dims,
                        hid_t file_type_id,
                        hid_t mem_type_id,
                        const void *buffer,
                        H5L_imageinfo_t imageinfo )
{

 char    *attr_name[] = {"CLASS","IMAGE_VERSION"};
 char    *attr_data[] = {"IMAGE","1.2"};
 char    attr_color[20];
 char    attr_subclass[20];
 char    attr_interlace[20];
 herr_t  status;
 int     rank;

 /* First we find the type of image. Version 1.2 only supports indexed and true color images.
  * Indexed images are stored as 2D datasets, true color images are stored as 3D datasets, 
  * with the third dimension defining the number of color planes in the image.
  */

 switch ( imageinfo.subclass )
 {
  case H5L_IMAGE_GRAYSCALE:
   strcpy(attr_subclass,"IMAGE_GRAYSCALE");
   return FAIL;
   break;
  case H5L_IMAGE_BITMAP:
   strcpy(attr_subclass,"IMAGE_BITMAP");
   return FAIL;
   break;
  case H5L_IMAGE_TRUECOLOR:
   strcpy(attr_subclass,"IMAGE_TRUECOLOR");
   rank = 3;
   break;
  case H5L_IMAGE_INDEXED:
   strcpy(attr_subclass,"IMAGE_INDEXED");
   rank = 2;
   break;
  default:
   return FAIL;
 };


 /* Find the interlace mode */

 switch ( imageinfo.interlace )
 {
  case H5L_INTERLACE_PIXEL:
   strcpy(attr_interlace,"INTERLACE_PIXEL");
   break;
  case H5L_INTERLACE_PLANE:
   strcpy(attr_interlace,"INTERLACE_PLANE");
   break;
  case H5L_INTERLACE_LINE:
   strcpy(attr_interlace,"INTERLACE_LINE");
   break;
 };

  

 /* Make the dataset */
 status = H5Lmake_dataset( loc_id, dset_name, rank, dims, file_type_id, mem_type_id, buffer );

 /* Then we attach the required attributes */

 /* Attach the CLASS attribute */
 status = H5Lattach_attribute( loc_id, dset_name, attr_name[0], attr_data[0] );

 /* Attach the VERSION attribute */
 status = H5Lattach_attribute( loc_id, dset_name, attr_name[1], attr_data[1] );

 /* Attach the IMAGE_SUBCLASS attribute */
 status = H5Lattach_attribute( loc_id, dset_name, "IMAGE_SUBCLASS", attr_subclass );

 /* Attach the INTERLACE_MODE attribute. This attribute is only for true color images*/
 if ( rank == 3 )
  status = H5Lattach_attribute( loc_id, dset_name, "INTERLACE_MODE", attr_interlace );


 /* We now attach the optional attributes */

 
 /* Image color model atrribute */

  switch ( imageinfo.color_model )
  {
  case H5L_MODEL_RGB:
   strcpy(attr_color,"RGB");
   break;
  case H5L_MODEL_YUV:
   strcpy(attr_color,"YUV");
   break;
  case H5L_MODEL_CMY:
   strcpy(attr_color,"CMY");
   break;
  case H5L_MODEL_CMYK:
   strcpy(attr_color,"CMYK");
   case H5L_MODEL_YCBCR:
   strcpy(attr_color,"YCbCr");
   break;
  case H5L_MODEL_HSV:
   strcpy(attr_color,"HSV");
   break;
  default:
   return FAIL;
  };

   /* Attach the IMAGE_COLORMODEL attribute */
 status = H5Lattach_attribute( loc_id, dset_name, "IMAGE_COLORMODEL", attr_color );

 return status;
}



/*-------------------------------------------------------------------------
 * Function: H5Lmake_image_indexed
 *
 * Purpose:
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 01, 2001
 *
 * Comments:
 *   
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5Lmake_image_indexed( hid_t loc_id, 
                        const char *dset_name, 
                        const hsize_t *dims,
                        hid_t file_type_id,
                        hid_t mem_type_id,
                        const void *buffer )
{

 H5L_imageinfo_t imageinfo;
 herr_t          status;

 /* Initialize the image info for an indexed, RGB color model image */

 imageinfo.subclass         = H5L_IMAGE_INDEXED;
 imageinfo.interlace        = H5L_INTERLACE_PIXEL;
 imageinfo.white_is_zero    = -1;
 imageinfo.minmax[0]        = -1;
 imageinfo.minmax[1]        = -1;
 imageinfo.bkindex          = -1;
 imageinfo.trindex          = -1;
 imageinfo.aspect_ratio     = -1;
 imageinfo.color_model      = H5L_MODEL_RGB;
 imageinfo.gamma_correction = -1;

 
 /* Make the image dataset */
 status = H5Lmake_image( loc_id, dset_name, dims, file_type_id, mem_type_id, 
  buffer, imageinfo );

 return status;
}


/*-------------------------------------------------------------------------
 * Function: H5Lmake_image_truecolor
 *
 * Purpose:
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 01, 2001
 *
 * Comments:
 *   
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


herr_t H5Lmake_image_truecolor( hid_t loc_id, 
                        const char *dset_name, 
                        const hsize_t *dims,
                        hid_t file_type_id,
                        hid_t mem_type_id,
                        const void *buffer )
{

 H5L_imageinfo_t imageinfo;
 herr_t          status;

 /* Initialize the image info for a true color, RGB color model image */

 imageinfo.subclass         = H5L_IMAGE_TRUECOLOR;
 imageinfo.interlace        = H5L_INTERLACE_PIXEL;
 imageinfo.white_is_zero    = -1;
 imageinfo.minmax[0]        = -1;
 imageinfo.minmax[1]        = -1;
 imageinfo.bkindex          = -1;
 imageinfo.trindex          = -1;
 imageinfo.aspect_ratio     = -1;
 imageinfo.color_model      = H5L_MODEL_RGB;
 imageinfo.gamma_correction = -1;

 
 /* Make the image dataset */
 status = H5Lmake_image( loc_id, dset_name, dims, file_type_id, mem_type_id, 
  buffer, imageinfo );

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
 * Function: H5L_find_palette
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

herr_t H5L_find_palette( hid_t loc_id ) 
{

 unsigned int attr_num;     /* Starting attribute to look up */
 herr_t       ret;

 attr_num = 0;
 ret = H5Aiterate( loc_id, &attr_num, find_palette, 0 );

 return ret;
}


/*-------------------------------------------------------------------------
 * Function: H5Lattach_palette
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


herr_t H5Lattach_palette( hid_t loc_id, 
                        const char *image_name, 
                        const char *pal_name,
                        const hsize_t *pal_dims,
                        hid_t file_type_id,
                        hid_t mem_type_id,
                        const void *pal_data,
                        H5L_colormodel_t color_model,
                        H5L_palette_t palette_t,
                        const char *rangeindex,
                        const void *minmax ) 

{                       

 char  *attr_name[] = {"CLASS","PAL_COLORMODEL","PAL_TYPE","RANGE_INDEX",
  "PAL_MINMAXNUMERIC","PAL_VERSION"};
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
 char        attr_color[5];
 char        attr_paltype[11];
 int         ok_pal;

 switch ( color_model )
 {
 case H5L_MODEL_RGB:
  strcpy(attr_color,"RGB");
  break;
 case H5L_MODEL_YUV:
  strcpy(attr_color,"YUV");
  break;
 case H5L_MODEL_CMY:
  strcpy(attr_color,"CMY");
  break;
 case H5L_MODEL_CMYK:
  strcpy(attr_color,"CMYK");
  break;
 case H5L_MODEL_YCBCR:
  strcpy(attr_color,"YCbCr");
  break;
 case H5L_MODEL_HSV:
  strcpy(attr_color,"HSV");
  break;
 default:
  return FAIL;
 };

 switch ( palette_t )
 {
 case H5L_PAL_STANDARD:
  strcpy(attr_paltype,"STANDARD8");
  break;
 case H5L_PAL_RANGEINDEX:
  strcpy(attr_paltype,"RANGEINDEX");
  break;
 default:
  return FAIL;
 };

 
 /* First we create the palette dataset and attach to it the required attributes 
  * that identify it as a palette
  */

 /* Make the palette dataset. The palette is created on the same local id.  */
 status = H5Lmake_dataset( loc_id, pal_name, 2, pal_dims, file_type_id, mem_type_id, pal_data );

 /* Attach the attribute "CLASS" to the >>palette<< dataset*/
 status = H5Lattach_attribute( loc_id, pal_name, attr_name[0], "PALETTE" );

 /* Attach the attribute "PAL_COLORMODEL" to the >>palette<< dataset*/
 status = H5Lattach_attribute( loc_id, pal_name, attr_name[1], attr_color );

 /* Attach the attribute "PAL_TYPE" to the >>palette<< dataset*/
 status = H5Lattach_attribute( loc_id, pal_name, attr_name[2], attr_paltype );

 /* Attach the attribute "PAL_VERSION" to the >>palette<< dataset*/
 status = H5Lattach_attribute( loc_id, pal_name, attr_name[5], "1.2" );

 /* The image dataset may or not have the attribute "PALETTE" 
  * First we try to open to see if it is already there; if not, it is created.
  * If it exists, the array of references is extended to hold the reference 
  * to the new palette
  */
 
 /* First we get the image id */
 image_id = H5Dopen( loc_id, image_name );

 /* Try to find the attribute "PALETTE" on the >>image<< dataset */
 ok_pal = H5L_find_palette( image_id );

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


#endif
