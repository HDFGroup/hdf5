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

#include "hdf5.h"

#if H5_VERS_MAJOR == 1 && H5_VERS_MINOR == 6
#include "H5IM.h"
#else
#include <hdf5_hl.h>
#endif

#define FILE_NAME "test_image.h5"

#define WIDTH        400
#define HEIGHT       200
#define PAL_ENTRIES  256
#define IMAGE1_NAME  "image1"
#define IMAGE2_NAME  "image2"
#define PAL_NAME     "palette"


/*-------------------------------------------------------------------------
 * the main program
 *-------------------------------------------------------------------------
 */
int main( void )
{
    hsize_t       width    = WIDTH;
    hsize_t       height   = HEIGHT;
    hsize_t       planes;
    hid_t         fid;
    int           i, j, n, space;
    char          interlace[20];
    hssize_t      npals;
    
    /* 8-bit image */
    unsigned char buf1 [ WIDTH*HEIGHT ];
    unsigned char pal[ PAL_ENTRIES * 3 ];        /* palette array */
    hsize_t       pal_dims[2] = {PAL_ENTRIES,3}; /* palette dimensions */
    
    /* 24-bit image */
    unsigned char buf2 [ WIDTH*HEIGHT*3 ];
    
    /* read data */
    unsigned char buf1_out [ WIDTH*HEIGHT ];
    unsigned char buf2_out [ WIDTH*HEIGHT*3 ];
    unsigned char pal_out[ PAL_ENTRIES * 3 ];    /* palette array */
    hsize_t       pal_dims_out[2];               /* palette dimensions */
    
    /* create an image */
    space = WIDTH*HEIGHT / PAL_ENTRIES;
    for (i=0, j=0, n=0; i < WIDTH*HEIGHT; i++, j++ )
    {
        buf1[i] = n;
        if ( j > space )
        {
            n++;
            j=0;
        }
        
    }
    
    
    /* create an image */
    space = WIDTH*HEIGHT / 256;
    for (i=0, j=0, n=0; i < WIDTH*HEIGHT*3; i+=3, j++ )
    {
        unsigned char r, g, b;

        r = n; g = 0; b = 255-n;
        buf2[i]   = r;
        buf2[i+1] = g;
        buf2[i+2] = b;
        if ( j > space )
        {
            n++;
            j=0;
        }
    }
    
   /*-------------------------------------------------------------------------
    * define a palette, blue to red tones 
    *-------------------------------------------------------------------------
    */
    for ( i=0, n=0; i<PAL_ENTRIES*3; i+=3, n++)
    {
        pal[i]  =n;      /* red */
        pal[i+1]=0;      /* green */
        pal[i+2]=255-n;  /* blue */
    }
    
    /* Create a new HDF5 file using default properties. */
    fid = H5Fcreate(FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );
    
   /*-------------------------------------------------------------------------
    * Indexed image test
    *-------------------------------------------------------------------------
    */
    
    TESTING("indexed image");
    
    /* Write image */
    if ( H5IMmake_image_8bit( fid, IMAGE1_NAME, width, height, buf1 ) < 0 )
        goto out;
    
    /* Make a palette */
    if ( H5IMmake_palette( fid, PAL_NAME, pal_dims, pal ) < 0 )
        goto out;
    
    /* Attach a palette to the image dataset */
    if ( H5IMlink_palette( fid, IMAGE1_NAME, PAL_NAME ) < 0 )
        goto out;
    
    /* Read image */
    if ( H5IMget_image_info( fid, IMAGE1_NAME, &width, &height, &planes, interlace, &npals ) < 0 )
        goto out;
    
    if ( H5IMread_image( fid, IMAGE1_NAME, buf1_out ) < 0 )
        goto out;
    
    for (i = 0; i < height*width*planes; i++) 
    {
        if ( buf1[i] != buf1_out[i] ) 
            goto out;
 
    }
    
    
    PASSED();
    
   /*-------------------------------------------------------------------------
    * True color image test
    *-------------------------------------------------------------------------
    */
    
    TESTING("true color image");
    
    /* Write image */
    if ( H5IMmake_image_24bit( fid, IMAGE2_NAME, width, height, "INTERLACE_PIXEL", buf2 ) )
        goto out;
    
    /* Read image */
    if ( H5IMget_image_info( fid, IMAGE2_NAME, &width, &height, &planes, interlace, &npals ) < 0 )
        goto out;
    
    if ( H5IMread_image( fid, IMAGE2_NAME, buf2_out ) < 0 )
        goto out;
    
    for (i = 0; i < height*width*planes; i++) 
    {
        if ( buf2[i] != buf2_out[i] ) 
            goto out;
    }
    
    
    PASSED();
    
   /*-------------------------------------------------------------------------
    * H5IMget_npalettes test
    *-------------------------------------------------------------------------
    */
    
    TESTING("pallete functions");
    
    if ( H5IMget_npalettes( fid, IMAGE1_NAME, &npals ) < 0 )
        goto out;
    
   /*-------------------------------------------------------------------------
    * H5IMget_palette_info test
    *-------------------------------------------------------------------------
    */
    
    if ( H5IMget_palette_info( fid, IMAGE1_NAME, 0, pal_dims_out ) < 0 )
        goto out;
    
    for (i = 0; i < 2; i++) 
    {
        if ( pal_dims[i] != pal_dims_out[i] ) 
            goto out;
    }
    
   /*-------------------------------------------------------------------------
    * H5IMget_palette test
    *-------------------------------------------------------------------------
    */
    
    if ( H5IMget_palette( fid, IMAGE1_NAME, 0, pal_out ) < 0 )
        goto out;
    
    for (i = 0; i < PAL_ENTRIES * 3; i++) 
    {
        if ( pal[i] != pal_out[i] ) 
            goto out;
    }
    
   /*-------------------------------------------------------------------------
    * H5IMis_image test
    *-------------------------------------------------------------------------
    */
    
    if ( H5IMis_image( fid, IMAGE1_NAME ) < 0 )
        goto out;
    
    if ( H5IMis_image( fid, IMAGE2_NAME ) < 0 )
        goto out;
    
   /*-------------------------------------------------------------------------
    * H5IMis_palette test
    *-------------------------------------------------------------------------
    */
    
    if ( H5IMis_palette( fid, PAL_NAME ) < 0 )
        goto out;
    
   /*-------------------------------------------------------------------------
    * end tests
    *-------------------------------------------------------------------------
    */
    
    /* Close the file. */
    if(H5Fclose( fid ) < 0) 
        goto out;
    
    
    PASSED();
    return 0;
    
out:
    H5_FAILED();
    return 1;
    
}
