/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "hdf5.h"
#include "H5IMpublic.h"

/*-------------------------------------------------------------------------
 * Program: h52gifgentst
 *
 * Purpose: generate files for h52gif testing
 *
 * Programmer: Pedro Vicente, pvn@hdfgroup.org
 *
 * Date: March 15, 2007
 *
 *-------------------------------------------------------------------------
 */

#define FILENAME    "h52giftst.h5"
#define WIDTH        400
#define HEIGHT       200
#define PAL_ENTRIES  256
#define IMAGE1_NAME  "image"
#define PAL_NAME     "palette"

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: main program
 *
 *-------------------------------------------------------------------------
 */

int main(void)
{
    hid_t         fid;
    int           i, j, n, space;
    unsigned char *buf;
    unsigned char pal[ PAL_ENTRIES * 3 ];        /* palette array */
    hsize_t       pal_dims[2] = {PAL_ENTRIES,3}; /* palette dimensions */
    hsize_t       width  = WIDTH;
    hsize_t       height = HEIGHT;

    /* Allocate buffer */
    if(NULL == (buf = (unsigned char *)malloc(WIDTH * HEIGHT)))
        return EXIT_FAILURE;

    /* create a file  */
    if ((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT))<0)
        return EXIT_FAILURE;

    /* create an image */
    space = WIDTH*HEIGHT / PAL_ENTRIES;
    for (i=0, j=0, n=0; i < WIDTH*HEIGHT; i++, j++ )
    {
        buf[i] = (unsigned char)n;
        if ( j > space )
        {
            n++;
            j=0;
        }

    }

    /* make the image */
    if (H5IMmake_image_8bit( fid, IMAGE1_NAME, width, height, buf )<0)
        return EXIT_FAILURE;

   /*-------------------------------------------------------------------------
    * define a palette, blue to red tones
    *-------------------------------------------------------------------------
    */
    for ( i=0, n=0; i<PAL_ENTRIES*3; i+=3, n++)
    {
      pal[i]   = (unsigned char)n;       /* red */
      pal[i+1] = (unsigned char)0;       /* green */
      pal[i+2] = (unsigned char)(255-n); /* blue */
    }

    /* make a palette */
    if (H5IMmake_palette( fid, PAL_NAME, pal_dims, pal )<0)
        return EXIT_FAILURE;

    /* attach the palette to the image */
    if (H5IMlink_palette( fid, IMAGE1_NAME, PAL_NAME )<0)
        return EXIT_FAILURE;

    if(H5Fclose(fid)<0)
        return EXIT_FAILURE;

    free(buf);

    return EXIT_SUCCESS;
}

