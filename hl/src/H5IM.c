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
#include "H5IM.h"
#include "H5LT.h"
#else
#include <hdf5_hl.h>
#endif


/*-------------------------------------------------------------------------
 * Function: H5IMmake_image_8bit
 *
 * Purpose: Creates and writes an image an 8 bit image
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: June 13, 2001
 *
 * Comments:
 *  based on HDF5 Image and Palette Specification
 *  http://hdf.ncsa.uiuc.edu/HDF5/H5Image/ImageSpec.html
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMmake_image_8bit( hid_t loc_id,
                            const char *dset_name,
                            hsize_t width,
                            hsize_t height,
                            const unsigned char *buffer )
{
 int      rank = 2;
 hsize_t  dims[2];

  /* Initialize the image dimensions */
 dims[0] = height;
 dims[1] = width;

 /* Make the dataset */
 if ( H5LTmake_dataset( loc_id, dset_name, rank, dims, H5T_NATIVE_UCHAR, buffer ) < 0 )
  return -1;

 /* Attach the CLASS attribute */
 if ( H5LTset_attribute_string( loc_id, dset_name, "CLASS", "IMAGE" ) < 0 )
  return -1;

 /* Attach the VERSION attribute */
 if ( H5LTset_attribute_string( loc_id, dset_name, "IMAGE_VERSION", "1.2" ) < 0 )
  return -1;

 /* Attach the IMAGE_SUBCLASS attribute */
 if ( H5LTset_attribute_string( loc_id, dset_name, "IMAGE_SUBCLASS", "IMAGE_INDEXED" ) < 0 )
  return -1;

 return 0;
}



/*-------------------------------------------------------------------------
 * Function: H5IMmake_image_24bit
 *
 * Purpose:
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: June 13, 2001
 *
 * Comments:
 *  based on HDF5 Image and Palette Specification
 *  http://hdf.ncsa.uiuc.edu/HDF5/H5Image/ImageSpec.html
 *
 * Interlace Mode Dimensions in the Dataspace
 * INTERLACE_PIXEL [height][width][pixel components]
 * INTERLACE_PLANE [pixel components][height][width]
 *
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMmake_image_24bit( hid_t loc_id,
                             const char *dset_name,
                             hsize_t width,
                             hsize_t height,
                             const char *interlace,
                             const unsigned char *buffer )
{
 int      rank = 3;
 hsize_t  dims[3];

 /* Initialize the image dimensions */

 if ( strcmp( interlace, "INTERLACE_PIXEL" ) == 0 )
 {
  /* Number of color planes is defined as the third dimension */
  dims[0] = height;
  dims[1] = width;
  dims[2] = 3;
 }
 else
 if ( strcmp( interlace, "INTERLACE_PLANE" ) == 0 )
 {
  /* Number of color planes is defined as the first dimension */
  dims[0] = 3;
  dims[1] = height;
  dims[2] = width;
 }
 else return -1;

 /* Make the dataset */
 if ( H5LTmake_dataset( loc_id, dset_name, rank, dims, H5T_NATIVE_UCHAR, buffer ) < 0 )
  return -1;

 /* Attach the CLASS attribute */
 if ( H5LTset_attribute_string( loc_id, dset_name, "CLASS", "IMAGE" ) < 0 )
  return -1;

 /* Attach the VERSION attribute */
 if ( H5LTset_attribute_string( loc_id, dset_name, "IMAGE_VERSION", "1.2" ) < 0 )
  return -1;

 /* Attach the IMAGE_SUBCLASS attribute */
 if ( H5LTset_attribute_string( loc_id, dset_name, "IMAGE_SUBCLASS", "IMAGE_TRUECOLOR" ) < 0 )
  return -1;

 /* Attach the INTERLACE_MODE attribute. This attributes is only for true color images */
 if ( H5LTset_attribute_string( loc_id, dset_name, "INTERLACE_MODE", interlace ) < 0 )
  return -1;

 return 0;

}



/*-------------------------------------------------------------------------
 * Function: find_palette
 *
 * Purpose: operator function used by H5LT_find_palette
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

static herr_t find_palette( hid_t loc_id, const char *name, void  *op_data )
{

 /* Define a default zero value for return. This will cause the iterator to continue if
  * the palette attribute is not found yet.
  */

 int ret = 0;

 /* Shut compiler */
 loc_id=loc_id;
 op_data=op_data;

 /* Define a positive value for return value if the attribute was found. This will
  * cause the iterator to immediately return that positive value,
  * indicating short-circuit success
  */

 if( strcmp( name, "PALETTE" ) == 0 )
  ret = 1;


 return ret;
}


/*-------------------------------------------------------------------------
 * Function: H5IM_find_palette
 *
 * Purpose: Private function. Find the attribute "PALETTE" in the image dataset
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

static herr_t H5IM_find_palette( hid_t loc_id )
{

 unsigned int attr_num;     /* Starting attribute to look up */
 herr_t       ret;

 attr_num = 0;
 ret = H5Aiterate( loc_id, &attr_num, find_palette, 0 );

 return ret;
}


/*-------------------------------------------------------------------------
 * Function: H5IMget_image_info
 *
 * Purpose: Gets information about an image dataset (dimensions, interlace mode
 *          and number of associated palettes).
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 25, 2001
 *
 * Comments:
 *  based on HDF5 Image and Palette Specification
 *  http://hdf.ncsa.uiuc.edu/HDF5/H5Image/ImageSpec.html
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMget_image_info( hid_t loc_id,
                     const char *dset_name,
                     hsize_t *width,
                     hsize_t *height,
                     hsize_t *planes,
                     char *interlace,
                     hssize_t *npals )
{
 hid_t   did, sid;
 hsize_t dims[3];
 hid_t   attr_id;
 hid_t   attr_type;
 int     has_attr;
 hid_t   attr_space_id;
 hid_t   attr_class;
 int     has_pal;

 /*assume initially we have no palettes attached*/
 *npals = 0;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Try to find the attribute "INTERLACE_MODE" on the >>image<< dataset */
 has_attr = H5LT_find_attribute( did, "INTERLACE_MODE" );

 /* It exists, get it */
 if ( has_attr == 1 )
 {

  if ( (attr_id = H5Aopen_name( did, "INTERLACE_MODE" )) < 0 )
   goto out;

  if ( (attr_type = H5Aget_type( attr_id )) < 0 )
   goto out;

  if ( H5Aread( attr_id, attr_type, interlace ) < 0 )
   goto out;

  if ( H5Tclose( attr_type )  < 0 )
   goto out;

  if ( H5Aclose( attr_id )  < 0 )
   goto out;
 }

 /* Get the dataspace handle */
 if ( (sid = H5Dget_space( did )) < 0 )
  goto out;

 /* Get dimensions */
 if ( H5Sget_simple_extent_dims( sid, dims, NULL) < 0 )
  goto out;

 /* Initialize the image dimensions */

 if ( has_attr == 1 )
 /* This is a 24 bit image */
 {

  if ( strcmp( interlace, "INTERLACE_PIXEL" ) == 0 )
  {
   /* Number of color planes is defined as the third dimension */
   *height = dims[0];
   *width  = dims[1];
   *planes = dims[2];
  }
  else
  if ( strcmp( interlace, "INTERLACE_PLANE" ) == 0 )
  {
   /* Number of color planes is defined as the first dimension */
   *planes = dims[0];
   *height = dims[1];
   *width  = dims[2];
  }
  else return -1;
 }
 else
 /* This is a 8 bit image */
 {
  *height = dims[0];
  *width  = dims[1];
  *planes = 1;
 }

 /* Close */
 if ( H5Sclose( sid ) < 0 )
  goto out;


 /* Get number of palettes */


 /* Try to find the attribute "PALETTE" on the >>image<< dataset */
 has_pal = H5IM_find_palette( did );

 if ( has_pal ==  1 )
 {

  if ( (attr_id = H5Aopen_name( did, "PALETTE" )) < 0 )
   goto out;

  if ( (attr_type = H5Aget_type( attr_id )) < 0 )
   goto out;

  if ( (attr_class = H5Tget_class( attr_type )) < 0 )
   goto out;

  /* Check if it is really a reference */

  if ( attr_class == H5T_REFERENCE )
  {

   /* Get the reference(s) */

   if ( (attr_space_id = H5Aget_space( attr_id )) < 0 )
    goto out;

   *npals = H5Sget_simple_extent_npoints( attr_space_id );

   if ( H5Sclose( attr_space_id ) < 0 )
    goto out;

  } /* H5T_REFERENCE */

  if ( H5Tclose( attr_type ) < 0 )
   goto out;

  /* Close the attribute. */
  if ( H5Aclose( attr_id ) < 0 )
   goto out;

 }

 /* End access to the dataset and release resources used by it. */
 if ( H5Dclose( did ) < 0 )
  goto out;

 return 0;

out:
 H5Dclose( did );
 return -1;

}


/*-------------------------------------------------------------------------
 * Function: H5IMread_image
 *
 * Purpose: Reads image data from disk.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: June 13, 2001
 *
 * Comments:
 *  based on HDF5 Image and Palette Specification
 *  http://hdf.ncsa.uiuc.edu/HDF5/H5Image/ImageSpec.html
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMread_image( hid_t loc_id,
                       const char *dset_name,
                       unsigned char *buffer )
{
 hid_t   did;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Read */
 if ( H5Dread( did, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer ) < 0 )
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
 * Function: H5IMmake_palette
 *
 * Purpose: Creates and writes a palette.
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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMmake_palette( hid_t loc_id,
                         const char *pal_name,
                         const hsize_t *pal_dims,
                         const unsigned char *pal_data )

{

 int has_pal;

 /* Check if the dataset already exists */
 has_pal = H5LTfind_dataset( loc_id, pal_name );

 /* It exists. Return */
 if ( has_pal == 1 )
  return 0;

 /* Make the palette dataset. */
 if ( H5LTmake_dataset( loc_id, pal_name, 2, pal_dims, H5T_NATIVE_UCHAR, pal_data ) <  0 )
  return -1;

 /* Attach the attribute "CLASS" to the >>palette<< dataset*/
 if ( H5LTset_attribute_string( loc_id, pal_name, "CLASS", "PALETTE" ) < 0 )
  return -1;

 /* Attach the attribute "PAL_VERSION" to the >>palette<< dataset*/
 if ( H5LTset_attribute_string( loc_id, pal_name, "PAL_VERSION", "1.2" ) < 0 )
  return -1;

 return 0;

}


/*-------------------------------------------------------------------------
 * Function: H5IMlink_palette
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

herr_t H5IMlink_palette( hid_t loc_id,
                         const char *image_name,
                         const char *pal_name )

{
 hid_t       image_id;
 hid_t       attr_type;
 hid_t       attr_id;
 hid_t       attr_space_id;
 hid_t       attr_class;
 hobj_ref_t  ref;         /* write a new reference */
 hobj_ref_t  *refbuf;     /* buffer to read references */
 hssize_t    n_refs;
 hsize_t     dim_ref;
 int         ok_pal;

 /* The image dataset may or may not have the attribute "PALETTE"
  * First we try to open to see if it is already there; if not, it is created.
  * If it exists, the array of references is extended to hold the reference
  * to the new palette
  */

 /* First we get the image id */
 if ( (image_id = H5Dopen( loc_id, image_name )) < 0 )
  return -1;

 /* Try to find the attribute "PALETTE" on the >>image<< dataset */
 ok_pal = H5LT_find_attribute( image_id, "PALETTE" );

/*-------------------------------------------------------------------------
 * It does not exist. We create the attribute and one reference
 *-------------------------------------------------------------------------
 */
 if ( ok_pal == 0 )
 {
  if ( (attr_space_id = H5Screate( H5S_SCALAR )) < 0 )
   goto out;

  /* Create the attribute type for the reference */
  if ( (attr_type = H5Tcopy( H5T_STD_REF_OBJ )) < 0 )
   goto out;

  /* Create the attribute "PALETTE" to be attached to the image*/
  if ( (attr_id = H5Acreate( image_id, "PALETTE", attr_type, attr_space_id, H5P_DEFAULT )) < 0 )
   goto out;

  /* Create a reference. The reference is created on the local id.  */
  if ( H5Rcreate( &ref, loc_id, pal_name, H5R_OBJECT, -1 ) < 0 )
   goto out;

  /* Write the attribute with the reference */
  if ( H5Awrite( attr_id, attr_type, &ref ) < 0 )
   goto out;

  /* close */
  if ( H5Sclose( attr_space_id ) < 0 )
   goto out;
  if ( H5Tclose( attr_type ) < 0 )
   goto out;
  if ( H5Aclose( attr_id ) < 0 )
   goto out;

 }

/*-------------------------------------------------------------------------
 * The attribute already exists, open it
 *-------------------------------------------------------------------------
 */
 else if ( ok_pal ==  1 )
  
 {
  if ( (attr_id = H5Aopen_name( image_id, "PALETTE" )) < 0 )
   goto out;
  
  if ( (attr_type = H5Aget_type( attr_id )) < 0 )
   goto out;
  
  if ( (attr_class = H5Tget_class( attr_type )) < 0 )
   goto out;
  
  /* Get and save the old reference(s) */
  if ( (attr_space_id = H5Aget_space( attr_id )) < 0 )
   goto out;
  
  n_refs = H5Sget_simple_extent_npoints( attr_space_id );
  
  dim_ref = n_refs + 1;
  
  refbuf = malloc( sizeof(hobj_ref_t) * (int)dim_ref );
  
  if ( H5Aread( attr_id, attr_type, refbuf ) < 0 )
   goto out;
 
  /* The attribute must be deleted, in order to the new one can reflect the changes*/
  if ( H5Adelete( image_id, "PALETTE" ) < 0 )
   goto out;
  
  /* Create a new reference for this palette. */
  if ( H5Rcreate( &ref, loc_id, pal_name, H5R_OBJECT, -1 ) < 0 )
   goto out;
  
  refbuf[n_refs] = ref;
  
  /* Create the data space for the new references */
  if ( H5Sclose( attr_space_id ) < 0 )
   goto out;
  
  if ( (attr_space_id = H5Screate_simple( 1, &dim_ref, NULL )) < 0 )
   goto out;
  
  /* Create the attribute again with the changes of space */
  if ( H5Aclose( attr_id ) < 0 )
   goto out;

  if ( (attr_id = H5Acreate( image_id, "PALETTE", attr_type, attr_space_id, H5P_DEFAULT )) < 0 )
   goto out;
  
  /* Write the attribute with the new references */
  if ( H5Awrite( attr_id, attr_type, refbuf ) < 0 )
   goto out;
  
  /* close */
  if ( H5Sclose( attr_space_id ) < 0 )
   goto out;
  if ( H5Aclose( attr_id ) < 0 )
   goto out;
  
  free( refbuf );
 
 } /* ok_pal ==  1 */

  /* Close the image dataset. */
 if ( H5Dclose( image_id ) < 0 )
  return -1;

 return 0;

out:
 H5Dclose( image_id );
 H5Sclose( attr_space_id );
 H5Tclose( attr_type );
 H5Aclose( attr_id );
 return -1;
}


/*-------------------------------------------------------------------------
 * Function: H5IMunlink_palette
 *
 * Purpose: This function dettaches a palette from an existing image dataset
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 10, 2001
 *
 * Comments:
 *  based on HDF5 Image and Palette Specification
 *  http://hdf.ncsa.uiuc.edu/HDF5/H5Image/ImageSpec.html
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMunlink_palette( hid_t loc_id,
                           const char *image_name,
                           const char *pal_name )
{
 hid_t       image_id;
 hid_t       attr_type;
 hid_t       attr_id;
 hid_t       attr_class;
 int         ok_pal, has_pal;

 /* Try to find the palette dataset */
 has_pal = H5LTfind_dataset( loc_id, pal_name );

 /* It does not exist. Return */
 if ( has_pal == 0 )
  return -1;

 /* The image dataset may or not have the attribute "PALETTE"
  * First we try to open to see if it is already there; if not, it is created.
  * If it exists, the array of references is extended to hold the reference
  * to the new palette
  */

 /* First we get the image id */
 if ( (image_id = H5Dopen( loc_id, image_name )) < 0 )
  return -1;

 /* Try to find the attribute "PALETTE" on the >>image<< dataset */
 ok_pal = H5LT_find_attribute( image_id, "PALETTE" );

 /* It does not exist. Nothing to do */
 if ( ok_pal == 0 )
  return -1;

 /* The attribute exists, open it */
 else if ( ok_pal ==  1 )
 {
  if ( (attr_id = H5Aopen_name( image_id, "PALETTE" )) < 0 )
   goto out;

  if ( (attr_type = H5Aget_type( attr_id )) < 0 )
   goto out;

  if ( (attr_class = H5Tget_class( attr_type )) < 0 )
   goto out;

  /* Check if it is really a reference */
  if ( attr_class == H5T_REFERENCE )
  {

   /* Deelete the attribute */
   if ( H5Adelete( image_id, "PALETTE" ) < 0 )
    goto out;

  }  /* H5T_REFERENCE */

  if ( H5Tclose( attr_type ) < 0 )
   goto out;

  /* Close the attribute. */
  if ( H5Aclose( attr_id ) < 0 )
   goto out;

 } /* ok_pal */

  /* Close the image dataset. */
 if ( H5Dclose( image_id ) < 0 )
  return -1;

 return 0;

out:
 H5Dclose( image_id );
 return -1;
}


/*-------------------------------------------------------------------------
 * Function: H5IMget_npalettes
 *
 * Purpose: Gets the number of palettes associated to an image
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 22, 2001
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMget_npalettes( hid_t loc_id,
                          const char *image_name,
                          hssize_t *npals )
{
 hid_t      image_id;
 hid_t      attr_type;
 hid_t      attr_id;
 hid_t      attr_space_id;
 hid_t      attr_class;
 int        has_pal;

 /*assume initially we have no palettes attached*/
 *npals = 0;

 /* Open the dataset. */
 if ( (image_id = H5Dopen( loc_id, image_name )) < 0 )
  return -1;

 /* Try to find the attribute "PALETTE" on the >>image<< dataset */
 has_pal = H5IM_find_palette( image_id );

 if ( has_pal ==  1 )
 {

  if ( (attr_id = H5Aopen_name( image_id, "PALETTE" )) < 0 )
   goto out;

  if ( (attr_type = H5Aget_type( attr_id )) < 0 )
   goto out;

  if ( (attr_class = H5Tget_class( attr_type )) < 0 )
   goto out;

  /* Check if it is really a reference */

  if ( attr_class == H5T_REFERENCE )
  {
   if ( (attr_space_id = H5Aget_space( attr_id )) < 0 )
    goto out;

   *npals = H5Sget_simple_extent_npoints( attr_space_id );

   if ( H5Sclose( attr_space_id ) < 0 )
    goto out;

  } /* H5T_REFERENCE */

  if ( H5Tclose( attr_type ) < 0 )
   goto out;

  /* Close the attribute. */
  if ( H5Aclose( attr_id ) < 0 )
   goto out;

 }

     /* Close the image dataset. */
 if ( H5Dclose( image_id ) < 0 )
  return -1;

 return 0;

out:
 H5Dclose( image_id );
 return -1;

}


/*-------------------------------------------------------------------------
 * Function: H5IMget_palette_info
 *
 * Purpose: Get palette information
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 22, 2001
 *
 * Comments:
 *  based on HDF5 Image and Palette Specification
 *  http://hdf.ncsa.uiuc.edu/HDF5/H5Image/ImageSpec.html
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMget_palette_info( hid_t loc_id,
                             const char *image_name,
                             int pal_number,
                             hsize_t *pal_dims )
{
 hid_t      image_id;
 int        has_pal;
 hid_t      attr_type;
 hid_t      attr_id;
 hid_t      attr_space_id;
 hid_t      attr_class;
 hssize_t   n_refs;
 hsize_t    dim_ref;
 hobj_ref_t *refbuf;     /* buffer to read references */
 hid_t      pal_id;
 hid_t      pal_space_id;
 hsize_t    pal_maxdims[2];
 
 /* Open the dataset. */
 if ( (image_id = H5Dopen( loc_id, image_name )) < 0 )
  return -1;
 
 /* Try to find the attribute "PALETTE" on the >>image<< dataset */
 has_pal = H5IM_find_palette( image_id );
 
 if ( has_pal ==  1 )
 {
  if ( (attr_id = H5Aopen_name( image_id, "PALETTE" )) < 0 )
   goto out;
  
  if ( (attr_type = H5Aget_type( attr_id )) < 0 )
   goto out;
  
  if ( (attr_class = H5Tget_class( attr_type )) < 0 )
   goto out;
  
  /* Get the reference(s) */
  if ( (attr_space_id = H5Aget_space( attr_id )) < 0 )
   goto out;
  
  n_refs = H5Sget_simple_extent_npoints( attr_space_id );
  
  dim_ref = n_refs;
  
  refbuf = malloc( sizeof(hobj_ref_t) * (int)dim_ref );
  
  if ( H5Aread( attr_id, attr_type, refbuf ) < 0 )
   goto out;
  
  /* Get the actual palette */
  if ( (pal_id = H5Rdereference( image_id, H5R_OBJECT, &refbuf[pal_number] )) < 0 )
   goto out;
  
  if ( (pal_space_id = H5Dget_space( pal_id )) < 0 )
   goto out;
  
  if ( H5Sget_simple_extent_ndims( pal_space_id ) < 0 )
   goto out;
  
  if ( H5Sget_simple_extent_dims( pal_space_id, pal_dims, pal_maxdims ) < 0 )
   goto out;
  
  /* close */
  if (H5Dclose(pal_id)<0)
   goto out;
  if ( H5Sclose( pal_space_id ) < 0 )
   goto out;
  if ( H5Sclose( attr_space_id ) < 0 )
   goto out;
  if ( H5Tclose( attr_type ) < 0 )
   goto out;
  if ( H5Aclose( attr_id ) < 0 )
   goto out;
  free( refbuf );
  
   
 }
 
 /* Close the image dataset. */
 if ( H5Dclose( image_id ) < 0 )
  return -1;
 
 return 0;
 
out:
 H5Dclose( image_id );
 H5Sclose( attr_space_id );
 H5Tclose( attr_type );
 H5Aclose( attr_id );
 return -1;
 
}

/*-------------------------------------------------------------------------
 * Function: H5IMget_palette
 *
 * Purpose: Read palette
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 30, 2001
 *
 * Comments:
 *  based on HDF5 Image and Palette Specification
 *  http://hdf.ncsa.uiuc.edu/HDF5/H5Image/ImageSpec.html
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMget_palette( hid_t loc_id,
                        const char *image_name,
                        int pal_number,
                        unsigned char *pal_data )
{
 hid_t      image_id;
 int        has_pal;
 hid_t      attr_type;
 hid_t      attr_id;
 hid_t      attr_space_id;
 hid_t      attr_class;
 hssize_t   n_refs;
 hsize_t    dim_ref;
 hobj_ref_t *refbuf;     /* buffer to read references */
 hid_t      pal_id;

 /* Open the dataset. */
 if ( (image_id = H5Dopen( loc_id, image_name )) < 0 )
  return -1;

 /* Try to find the attribute "PALETTE" on the >>image<< dataset */
 has_pal = H5IM_find_palette( image_id );

 if ( has_pal ==  1 )
 {
  if ( (attr_id = H5Aopen_name( image_id, "PALETTE" )) < 0 )
   goto out;
  
  if ( (attr_type = H5Aget_type( attr_id )) < 0 )
   goto out;
  
  if ( (attr_class = H5Tget_class( attr_type )) < 0 )
   goto out;
  
  /* Get the reference(s) */
  if ( (attr_space_id = H5Aget_space( attr_id )) < 0 )
   goto out;
  
  n_refs = H5Sget_simple_extent_npoints( attr_space_id );
  
  dim_ref = n_refs;
  
  refbuf = malloc( sizeof(hobj_ref_t) * (int)dim_ref );
  
  if ( H5Aread( attr_id, attr_type, refbuf ) < 0 )
   goto out;
  
  /* Get the palette id */
  if ( (pal_id = H5Rdereference( image_id, H5R_OBJECT, &refbuf[pal_number] )) < 0 )
   goto out;
  
  /* Read the palette dataset */
  if ( H5Dread( pal_id, H5Dget_type(pal_id), H5S_ALL, H5S_ALL, H5P_DEFAULT, pal_data ) < 0 )
   goto out;

  /* close */
  if (H5Dclose(pal_id)<0)
   goto out;
  if ( H5Sclose( attr_space_id ) < 0 )
   goto out;
  if ( H5Tclose( attr_type ) < 0 )
   goto out;
  if ( H5Aclose( attr_id ) < 0 )
   goto out;
  free( refbuf );
 }

 /* Close the image dataset. */
 if ( H5Dclose( image_id ) < 0 )
  return -1;

 return 0;

out:
 H5Dclose( image_id );
 H5Sclose( attr_space_id );
 H5Tclose( attr_type );
 H5Aclose( attr_id );
 return -1;

}

/*-------------------------------------------------------------------------
 * Function: H5IMis_image
 *
 * Purpose:
 *
 * Return: true, false, fail
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 30, 2001
 *
 * Comments:
 *  based on HDF5 Image and Palette Specification
 *  http://hdf.ncsa.uiuc.edu/HDF5/H5Image/ImageSpec.html
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMis_image( hid_t loc_id,
                     const char *dset_name )
{
 hid_t      did;
 int        has_class;
 hid_t      attr_type;
 hid_t      attr_id;
 char       attr_data[20];
 herr_t     ret;

 /* Assume initially fail condition */
 ret = -1;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

 /* Try to find the attribute "CLASS" on the dataset */
 has_class = H5LT_find_attribute( did, "CLASS" );

 if ( has_class ==  0 )
 {
  H5Dclose( did );
  return 0;
 }

 else if ( has_class ==  1 )
 {

  if ( (attr_id = H5Aopen_name( did, "CLASS" )) < 0 )
   goto out;

  if ( (attr_type = H5Aget_type( attr_id )) < 0 )
   goto out;

  if ( H5Tget_class( attr_type ) < 0 )
   goto out;

  if ( H5Aread( attr_id, attr_type, attr_data ) < 0 )
    goto out;

  if( strcmp( attr_data, "IMAGE" ) == 0 )
   ret = 1;
  else
   ret = 0;

  if ( H5Tclose( attr_type ) < 0 )
   goto out;

  if ( H5Aclose( attr_id ) < 0 )
   goto out;

 }

 /* Close the dataset. */
 if ( H5Dclose( did ) < 0 )
  return -1;

 return ret;

out:
 H5Dclose( did );
 return -1;

}



/*-------------------------------------------------------------------------
 * Function: H5IMis_palette
 *
 * Purpose:
 *
 * Return: true, false, fail
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 30, 2001
 *
 * Comments:
 *  based on HDF5 Image and Palette Specification
 *  http://hdf.ncsa.uiuc.edu/HDF5/H5Image/ImageSpec.html
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5IMis_palette( hid_t loc_id,
                       const char *dset_name )
{
 hid_t      did;
 int        has_class;
 hid_t      attr_type;
 hid_t      attr_id;
 char       attr_data[20];
 herr_t     ret;

 /* Assume initially fail condition */
 ret = -1;

 /* Open the dataset. */
 if ( (did = H5Dopen( loc_id, dset_name )) < 0 )
  return -1;

    /* Try to find the attribute "CLASS" on the dataset */
 has_class = H5LT_find_attribute( did, "CLASS" );

 if ( has_class ==  0 )
 {
  H5Dclose( did );
  return 0;
 }

 else if ( has_class ==  1 )
 {

  if ( (attr_id = H5Aopen_name( did, "CLASS" )) < 0 )
   goto out;

  if ( (attr_type = H5Aget_type( attr_id )) < 0 )
   goto out;

  if ( H5Tget_class( attr_type ) < 0 )
   goto out;

  if ( H5Aread( attr_id, attr_type, attr_data ) < 0 )
    goto out;

  if( strcmp( attr_data, "PALETTE" ) == 0 )
   ret = 1;
  else
   ret = 0;

  if ( H5Tclose( attr_type ) < 0 )
   goto out;

  if ( H5Aclose( attr_id ) < 0 )
   goto out;

 }

     /* Close the dataset. */
 if ( H5Dclose( did ) < 0 )
  return -1;

 return ret;

out:
 H5Dclose( did );
 return -1;

}
