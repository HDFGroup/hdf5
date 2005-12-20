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


#define WIDTH  (hsize_t)500
#define HEIGHT (hsize_t)200
unsigned char data [ WIDTH*HEIGHT ];


int main( void )
{
 hid_t         file_id;
 herr_t        status;
 hsize_t       pal_dims[] = {9,3};
 hsize_t       i;
 /* create a 9 entry palette */
 unsigned char pal[9*3] = {0, 0, 168,
  0, 0, 252,
  0, 168, 252,
  84, 252, 252,
  168, 252, 168,
  0, 252, 168,
  252, 252, 84,
  252, 168, 0,
  252, 0, 0};

	EXAMPLE("make indexed image");
 
 
 for (i = 0; i < WIDTH*HEIGHT; i++ )
  data[i] = (unsigned char)i;
 
 /* Create a new HDF5 file using default properties. */
 file_id = H5Fcreate( "ex_image1.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );
 
 /* Write image */
 status = H5IMmake_image_8bit( file_id, "Image1", WIDTH, HEIGHT, data );
 
 /* Make a palette */
 status = H5IMmake_palette( file_id, "Pallete", pal_dims, pal );
 
 /* Attach a palette to the image dataset */
 status = H5IMlink_palette( file_id, "Image1", "Pallete" );
 
 /* Close the file. */
 status = H5Fclose( file_id );

	PASSED();
 
 return 0;
 
 
}

