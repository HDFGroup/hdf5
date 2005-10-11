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


#include "H5IM.h"

#define FILE_NAME "test_image.h5"
#define WIDTH  (hsize_t)500
#define HEIGHT (hsize_t)200
unsigned char image_in1 [ WIDTH*HEIGHT ];
unsigned char image_out1[ WIDTH*HEIGHT ];
unsigned char image_in2 [ WIDTH*HEIGHT*3 ];
unsigned char image_out2[ WIDTH*HEIGHT*3 ];


/*-------------------------------------------------------------------------
 * the main program
 *-------------------------------------------------------------------------
 */
int main( void )
{
 hid_t         file_id;
 hsize_t       width, height, planes;
 hsize_t       pal_dims[] = {9,3};
 hsize_t       pal_dims_out[2];
 hsize_t       i;
 char          interlace[20];
 hssize_t      npals;

 unsigned char pal_data_out[9*3];
 /* create a 9 entry grey palette */
 unsigned char pal_data_in[9*3] = {0,0,0,
 25,25,25,
 50,50,50,
 75,75,75,
 100,100,100,
 125,125,125,
 150,150,150,
 175,175,175,
 200,200,200};

 for (i = 0; i < WIDTH*HEIGHT; i++ )
  image_in1[i] = (unsigned char)i;
 for (i = 0; i < WIDTH*HEIGHT*3; i++)
  image_in2[i] = (unsigned char)i;

 /* Create a new HDF5 file using default properties. */
 file_id = H5Fcreate(FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );

/*-------------------------------------------------------------------------
 * Indexed image test
 *-------------------------------------------------------------------------
 */

 TESTING("indexed image");

 /* Write image */
 if ( H5IMmake_image_8bit( file_id, "Image1", WIDTH, HEIGHT, image_in1 ) < 0 )
  goto out;

 /* Make a palette */
 if ( H5IMmake_palette( file_id, "Pallete", pal_dims, pal_data_in ) < 0 )
  goto out;

  /* Attach a palette to the image dataset */
 if ( H5IMlink_palette( file_id, "Image1", "Pallete" ) < 0 )
  goto out;

 /* Read image */
 if ( H5IMget_image_info( file_id, "Image1", &width, &height, &planes, interlace, &npals ) < 0 )
  goto out;

 if ( H5IMread_image( file_id, "Image1", image_out1 ) < 0 )
  goto out;

 for (i = 0; i < height*width*planes; i++) {
  if ( image_in1[i] != image_out1[i] ) {
    goto out;

  }
 }

 PASSED();

/*-------------------------------------------------------------------------
 * True color image test
 *-------------------------------------------------------------------------
 */

 TESTING("true color image");

 /* Write image */
 if ( H5IMmake_image_24bit( file_id, "Image2", WIDTH, HEIGHT, "INTERLACE_PIXEL", image_in2 ) )
  goto out;

 /* Read image */
 if ( H5IMget_image_info( file_id, "Image2", &width, &height, &planes, interlace, &npals ) < 0 )
  goto out;

 if ( H5IMread_image( file_id, "Image2", image_out2 ) < 0 )
  goto out;

 for (i = 0; i < height*width*planes; i++) {
  if ( image_in2[i] != image_out2[i] ) {
    goto out;
  }
 }

 PASSED();

/*-------------------------------------------------------------------------
 * H5IMget_npalettes test
 *-------------------------------------------------------------------------
 */

 TESTING("pallete functions");

 if ( H5IMget_npalettes( file_id, "Image1", &npals ) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * H5IMget_palette_info test
 *-------------------------------------------------------------------------
 */

 if ( H5IMget_palette_info( file_id, "Image1", 0, pal_dims_out ) < 0 )
  goto out;

 for (i = 0; i < 2; i++) {
  if ( pal_dims[i] != pal_dims_out[i] ) {
    goto out;
  }
 }

/*-------------------------------------------------------------------------
 * H5IMget_palette test
 *-------------------------------------------------------------------------
 */

 if ( H5IMget_palette( file_id, "Image1", 0, pal_data_out ) < 0 )
  goto out;

 for (i = 0; i < 9*3; i++) {
  if ( pal_data_in[i] != pal_data_out[i] ) {
   goto out;
  }
 }

/*-------------------------------------------------------------------------
 * H5IMis_image test
 *-------------------------------------------------------------------------
 */

 if ( H5IMis_image( file_id, "Image1" ) < 0 )
  goto out;

 if ( H5IMis_image( file_id, "Image2" ) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * H5IMis_palette test
 *-------------------------------------------------------------------------
 */

 if ( H5IMis_palette( file_id, "Pallete" ) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * end tests
 *-------------------------------------------------------------------------
 */

     /* Close the file. */
 if(H5Fclose( file_id ) < 0) goto out;

 PASSED();
 return 0;

out:
 H5_FAILED();
 return 1;

}
