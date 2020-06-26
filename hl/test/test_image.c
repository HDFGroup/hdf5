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

#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include "h5hltest.h"
#include "H5srcdir.h"
#include "H5LTpublic.h"
#include "H5IMpublic.h"
#include "pal_rgb.h"

#define FILE_NAME   "test_image1.h5"
#define FILE2       "test_image2.h5"
#define FILE3       "test_image3.h5"
#define DATA_FILE1  "image8.txt"
#define DATA_FILE2  "image24pixel.txt"
#define DATA_FILE3  "image24plane.txt"
#define DATA_FILE4  "usa.wri"
#define PAL2_FILE   "sepia.pal"
#define PAL3_FILE   "earth.pal"
#define IMAGE1_NAME "image8bit"
#define IMAGE2_NAME "image24bitpixel"
#define IMAGE3_NAME "image24bitplane"
#define PAL_NAME    "palette"
#define PAL1_NAME   "rainbow"
#define PAL2_NAME   "sepia"
#define PAL3_NAME   "earth"
#define PAL4_NAME   "blue-red"


#define WIDTH        400
#define HEIGHT       200
#define PAL_ENTRIES  256

/* struct to store RGB values read from a .pal file */
typedef struct rgb_t {
    unsigned char r;
    unsigned char g;
    unsigned char b;
} rgb_t;

/* prototypes */
static int test_simple(void);
static int test_data(void);
static int test_generate(void);
static int read_data(const char* file_name, hsize_t *width, hsize_t *height );
static int read_palette(const char* file_name, rgb_t *palette, size_t palette_size);

/* globals */
unsigned char *image_data = NULL;

/*-------------------------------------------------------------------------
* the main program
*-------------------------------------------------------------------------
*/

int main(void)
{
    int nerrors=0;

    nerrors += test_simple()<0  ?1:0;
    nerrors += test_data()<0  ?1:0;
    nerrors += test_generate()<0  ?1:0;

    if (nerrors) goto error;
    HDprintf("All image tests passed.\n");
    return 0;

error:
    HDprintf("***** %d IMAGE TEST%s FAILED! *****\n",nerrors, 1 == nerrors ? "" : "S");
    return 1;
}

/*-------------------------------------------------------------------------
* a simple test that generates images and palettes
*-------------------------------------------------------------------------
*/

static int test_simple(void)
{
    hsize_t       width    = WIDTH;
    hsize_t       height   = HEIGHT;
    hsize_t       planes;
    hid_t         fid;
    int           i, j, n, space;
    hsize_t       u;
    char          interlace[20];
    hssize_t      npals;

    /* 8-bit image */
    unsigned char *buf1 = NULL;
    unsigned char pal[ PAL_ENTRIES * 3 ];        /* palette array */
    hsize_t       pal_dims[2] = {PAL_ENTRIES,3}; /* palette dimensions */

    /* 24-bit image */
    unsigned char *buf2 = NULL;

    /* read data */
    unsigned char *buf1_out = NULL;
    unsigned char *buf2_out = NULL;
    unsigned char pal_out[ PAL_ENTRIES * 3 ];    /* palette array */
    hsize_t       pal_dims_out[2];               /* palette dimensions */

    /* Allocate image buffers */
    buf1 = (unsigned char *)HDmalloc(WIDTH * HEIGHT);
    HDassert(buf1);
    buf2  = (unsigned char *)HDmalloc(WIDTH * HEIGHT * 3);
    HDassert(buf2);
    buf1_out = (unsigned char *)HDmalloc(WIDTH * HEIGHT);
    HDassert(buf1_out);
    buf2_out  = (unsigned char *)HDmalloc(WIDTH * HEIGHT * 3);
    HDassert(buf2_out);

    /* create an image */
    space = WIDTH*HEIGHT / PAL_ENTRIES;
    for (i=0, j=0, n=0; i < WIDTH*HEIGHT; i++, j++ )
    {
        buf1[i] = (unsigned char)n;
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
        buf2[i]   = (unsigned char)n;
        buf2[i+1] = 0;
        buf2[i+2] = (unsigned char)(255 - n);
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
        pal[i]  =(unsigned char)n;      /* red */
        pal[i+1]=0;      /* green */
        pal[i+2]=(unsigned char)(255 - n);  /* blue */
    }

    /* Create a new HDF5 file using default properties. */
    fid = H5Fcreate(FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );

    /*-------------------------------------------------------------------------
    * Indexed image test
    *-------------------------------------------------------------------------
    */

    HL_TESTING2("indexed image");

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

    for (u = 0; u < height*width*planes; u++)
    {
        if ( buf1[u] != buf1_out[u] )
            goto out;

    }


    PASSED();

    /*-------------------------------------------------------------------------
    * True color image test
    *-------------------------------------------------------------------------
    */

    HL_TESTING2("true color image");

    /* Write image */
    if ( H5IMmake_image_24bit( fid, IMAGE2_NAME, width, height, "INTERLACE_PIXEL", buf2 ) )
        goto out;

    /* Read image */
    if ( H5IMget_image_info( fid, IMAGE2_NAME, &width, &height, &planes, interlace, &npals ) < 0 )
        goto out;

    if ( H5IMread_image( fid, IMAGE2_NAME, buf2_out ) < 0 )
        goto out;

    for (u = 0; u < height*width*planes; u++)
    {
        if ( buf2[u] != buf2_out[u] )
            goto out;
    }


    PASSED();

    /*-------------------------------------------------------------------------
    * H5IMget_npalettes test
    *-------------------------------------------------------------------------
    */

    HL_TESTING2("pallete functions");

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

    if(buf1)
        HDfree(buf1);
    if(buf2)
        HDfree(buf2);
    if(buf1_out)
        HDfree(buf1_out);
    if(buf2_out)
        HDfree(buf2_out);

    /* Close the file. */
    if(H5Fclose( fid ) < 0)
        goto out;


    PASSED();

    return 0;

    /* error zone, gracefully close */
out:
    if(buf1)
        HDfree(buf1);
    if(buf2)
        HDfree(buf2);
    if(buf1_out)
        HDfree(buf1_out);
    if(buf2_out)
        HDfree(buf2_out);
    H5E_BEGIN_TRY {
        H5Fclose(fid);
    } H5E_END_TRY;
    H5_FAILED();
    return FAIL;
}


/*-------------------------------------------------------------------------
* read sample realistic image data from ASCII files
*-------------------------------------------------------------------------
*/

static int test_data(void)
{
    hid_t          fid;
    hsize_t        pal_dims[2];
    hsize_t        width;
    hsize_t        height;
    unsigned char  pal[256*3]; /* buffer to hold an HDF5 palette */
    rgb_t          rgb[256];   /* buffer to hold a .pal file palette */
    int            i, n;

    /* create a file using default properties */
    if ((fid=H5Fcreate(FILE2,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        goto out;

    HDprintf("Testing read ascii image data and generate images\n");

    /*-------------------------------------------------------------------------
    * read 8bit image data
    *-------------------------------------------------------------------------
    */

    HL_TESTING2("make indexed image");

    /* read first data file */
    if (read_data(DATA_FILE1,&width,&height)<0)
        goto out;

    /* make an image */
    if (H5IMmake_image_8bit(fid,IMAGE1_NAME,width,height,image_data)<0)
        goto out;

    PASSED();


    HL_TESTING2("attaching palettes");

    /*-------------------------------------------------------------------------
    * palette #1. rainbow palette. data is contained in "pal_rgb.h"
    *-------------------------------------------------------------------------
    */

    /* initialize the palette data */
    pal_dims[0] = 256;
    pal_dims[1] = 3;

    /* make a palette */
    if (H5IMmake_palette(fid,PAL1_NAME,pal_dims,pal_rgb)<0)
        goto out;

    /* attach a palette to the image dataset */
    if (H5IMlink_palette(fid,IMAGE1_NAME,PAL1_NAME)<0)
        goto out;

    /*-------------------------------------------------------------------------
    * palette #2. sepia palette.
    * read a PAL file and attach the palette to the HDF5 file
    *-------------------------------------------------------------------------
    */

    /* read a PAL file */
    if (read_palette(PAL2_FILE, rgb, (sizeof(rgb) / sizeof(rgb[0]))) < 0)
        goto out;

    /* transfer to the HDF5 buffer */
    for ( i=0, n=0; i<256*3; i+=3, n++)
    {
        pal[i]  =rgb[n].r;
        pal[i+1]=rgb[n].g;
        pal[i+2]=rgb[n].b;
    }

    /* make a palette */
    if (H5IMmake_palette(fid,PAL2_NAME,pal_dims,pal)<0)
        goto out;

    /* attach the palette to the image dataset */
    if (H5IMlink_palette(fid,IMAGE1_NAME,PAL2_NAME)<0)
        goto out;

    /*-------------------------------------------------------------------------
    * palette #3. earth palette.
    * read a PAL file and attach the palette to the HDF5 file
    *-------------------------------------------------------------------------
    */

    /* read a PAL file */
    if (read_palette(PAL3_FILE, rgb, (sizeof(rgb) / sizeof(rgb[0]))) < 0)
        goto out;

    /* transfer to the HDF5 buffer */
    for ( i=0, n=0; i<256*3; i+=3, n++)
    {
        pal[i]  =rgb[n].r;
        pal[i+1]=rgb[n].g;
        pal[i+2]=rgb[n].b;
    }

    /* make a palette */
    if (H5IMmake_palette(fid,PAL3_NAME,pal_dims,pal)<0)
        goto out;

    /* attach the palette to the image dataset */
    if (H5IMlink_palette(fid,IMAGE1_NAME,PAL3_NAME)<0)
        goto out;

    PASSED();


    /*-------------------------------------------------------------------------
    * palette #4. blue-red
    * make a palette whith blue to red colors
    *-------------------------------------------------------------------------
    */
    for ( i=0, n=0; i<256*3; i+=3, n++)
    {
        pal[i]  =(unsigned char)n;
        pal[i+1]=0;
        pal[i+2]=(unsigned char)(255 - n);
    }

    /* make a palette */
    if (H5IMmake_palette(fid,PAL4_NAME,pal_dims,pal)<0)
        goto out;

    /* attach the palette to the image dataset */
    if (H5IMlink_palette(fid,IMAGE1_NAME,PAL4_NAME)<0)
        goto out;


    /*-------------------------------------------------------------------------
    * true color image example with pixel interlace
    *-------------------------------------------------------------------------
    */

    HL_TESTING2("make true color image with pixel interlace");

    /* read second data file */
    if ((read_data(DATA_FILE2,&width,&height))<0)
        goto out;

    /* make image */
    if ((H5IMmake_image_24bit(fid,IMAGE2_NAME,width,height,"INTERLACE_PIXEL",image_data))<0)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * True color image example with plane interlace
    *-------------------------------------------------------------------------
    */

    HL_TESTING2("make true color image with plane interlace");

    /* read third data file */
    if ((read_data(DATA_FILE3,&width,&height))<0)
        goto out;

    /* make image */
    if ((H5IMmake_image_24bit(fid,IMAGE3_NAME,width,height,"INTERLACE_PLANE",image_data))<0)
        goto out;

    PASSED();


    /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */
    if (H5Fclose(fid)<0)
        goto out;

    /* Release memory buffer */
    HDfree(image_data);

    return 0;

    /* error zone, gracefully close */
out:
    /* Release memory buffer */
    if(image_data)
        HDfree(image_data);

    H5E_BEGIN_TRY {
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();
    return FAIL;
}


/*
The following test provides an examples of how to generate HDF5 image data from
floating point data.  In the example we use real life topographic data
(from the North American hemisphere). In the dataset sea values are represented
as negative numbers and land values are represented as positive numbers.
The example generates 3 HDF5 images, one that generates an image from all the values,
another that generates an image from the land values and another that generates an
image from the sea values.
For the example we used data from MODB, the Mediterranean Oceanic Data Base
http://modb.oce.ulg.ac.be/

*/

static int test_generate(void)
{
    hid_t    fid;
    hsize_t  pal_dims[2] = { 256, 3 };
    float    *data = NULL;
    int      imax, jmax, kmax;
    int      n_elements;
    float    valex, xmin, xmax, value;
    FILE     *f = NULL;
    const char *data_file = H5_get_srcdir_filename(DATA_FILE4);
    int      i;
    int      retval = FAIL;

    /* create a file using default properties */
    if ((fid=H5Fcreate(FILE3,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        goto out;

    HDprintf("Testing read and process data and make indexed images\n");

    /*-------------------------------------------------------------------------
    * read data; the file data format is described below
    *-------------------------------------------------------------------------
    */

    f  = HDfopen( data_file, "r" ) ;
    if ( f == NULL )
    {
        HDprintf( "Could not find file %s. Try set $srcdir \n", data_file );
        goto out;
    }

    /*
    !The first line of the ASCII file contains the dimension of the array :
    ! IMAX, JMAX, KMAX. The integers are stored with the FORTRAN format I5.
    !The second line contains the exclusion value, the minimum and the maximum value of this
    ! file. These numbers are stored with the FORTRAN format E12.5.
    ! The remaining lines contains the data of the array, with 5 numbers per line
    ! (except the last line for each I-line).
    ! The array is stored in horizontal slices from sea surface to sea bottom and from
    ! north to south. So the indexes go from :
    !
    !   DO K = KMAX to 1
    !       DO J = JMAX to 1
    !           DO I = 1 to IMAX
    !              read
    !           OD
    !       OD
    !   OD
    !
    !              ____________________________
    !             /                           /| (imax,jmax,kmax)
    !            /        sea surface        / |
    !           /                           /  |
    !          /__________________________ /   |
    !          |                          |    |
    !          |                          |    | (imax,jmax,1)        n
    !          |                          |   /                      /
    !          |                          |  /                      /
    !     ^  j |                          | /             w  <-----o-----> e
    !  k  |  / |__________________________|/                      /
    !     | /                           (imax,1,1)               /
    !     |---------->                                          s
    !     i
    !
    */


    if(fscanf( f, "%d %d %d", &imax, &jmax, &kmax ) < 0 && HDferror(f)) {
        HDprintf( "fscanf error in file %s.\n", data_file );
        goto out;
    } /* end if */
    if(fscanf( f, "%f %f %f", &valex, &xmin, &xmax ) < 0 && HDferror(f)) {
        HDprintf( "fscanf error in file %s.\n", data_file );
        goto out;
    } /* end if */

    /* Sanity check on scanned-in values */
    if(imax < 1 || jmax < 1 || kmax < 1)
        goto out;

    /* Test product for integer overflow */
    if(imax > INT_MAX / jmax)
        goto out;
    if(imax * jmax > INT_MAX / kmax)
        goto out;

    n_elements = imax * jmax * kmax;

    /* Test buffer sizes for overflow */
    if(n_elements > INT_MAX / (int)sizeof(unsigned char))
        goto out;
    if(n_elements > INT_MAX / (int)sizeof(float))
        goto out;

    data = (float *)HDmalloc((size_t)n_elements * sizeof(float));
    if(NULL == data)
        goto out;
    image_data = (unsigned char *)HDmalloc((size_t)n_elements * sizeof(unsigned char));
    if(NULL == image_data)
        goto out;

    for ( i = 0; i < n_elements; i++ )
    {
        if(fscanf( f, "%f ", &value ) < 0 && HDferror(f)) {
            HDprintf( "fscanf error in file %s.\n", data_file );
            goto out;
        } /* end if */
        data[i] = value;
    }
    HDfclose(f);
    f = NULL;

    /*-------------------------------------------------------------------------
    * transform the data from floating point to unsigned char
    * we are processing all the data here
    *-------------------------------------------------------------------------
    */

    HL_TESTING2("make indexed image from all the data");

    for ( i = 0; i < n_elements; i++ )
        image_data[i] = (unsigned char)(( 255 * (data[i] - xmin ) ) / (xmax - xmin ));

    /* Make the image */
    if ((H5IMmake_image_8bit(fid,"All data",(hsize_t)imax,(hsize_t)jmax,image_data))<0)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * transform the data from floating point to unsigned char
    * here we just process the land data
    *-------------------------------------------------------------------------
    */

    HL_TESTING2("make indexed image from land data");

    for ( i = 0; i < n_elements; i++ )
    {
        if ( data[i] < 0 )
            image_data[i] = 0;
        else
            image_data[i] = (unsigned char)(( 255 * (data[i] ) ) / xmax );
    }

    /* make the image */
    if ((H5IMmake_image_8bit(fid,"Land data",(hsize_t)imax,(hsize_t)jmax,image_data))<0)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * transform the data from floating point to unsigned char
    * here we just process the sea data
    *-------------------------------------------------------------------------
    */

    HL_TESTING2("make indexed image from sea data");

    for ( i = 0; i < n_elements; i++ )
    {
        if ( data[i] > 0 )
            image_data[i] = 0;
        else
            image_data[i] = (unsigned char)(( 255 * (data[i] - xmin ) ) / xmin );
    }

    /* make the image */
    if ((H5IMmake_image_8bit(fid,"Sea data",(hsize_t)imax,(hsize_t)jmax,image_data))<0)
        goto out;

    PASSED();

    /*-------------------------------------------------------------------------
    * make a palette and attach it to the datasets
    *-------------------------------------------------------------------------
    */

    HL_TESTING2("attaching palettes");

    /* make a palette */
    if ((H5IMmake_palette(fid,PAL1_NAME,pal_dims,pal_rgb))<0)
        goto out;

    /* attach the palette to the image datasets */
    if ((H5IMlink_palette(fid,"All data",PAL1_NAME))<0)
        goto out;
    if ((H5IMlink_palette(fid,"Land data",PAL1_NAME))<0)
        goto out;
    if ((H5IMlink_palette(fid,"Sea data",PAL1_NAME))<0)
        goto out;

    PASSED();


    /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */
    if (H5Fclose(fid)<0)
        goto out;

    /* Release memory buffers */
    HDfree(data);
    HDfree(image_data);

    /* Indicate success */
    return 0;

    /* error zone, gracefully close */
out:
    /* Release memory buffers */
    if(data)
        HDfree(data);
    if(image_data)
        HDfree(image_data);

    H5E_BEGIN_TRY {
        H5Fclose(fid);
    } H5E_END_TRY;
    if(f)
        HDfclose(f);
    H5_FAILED();
    return retval;
}


/*-------------------------------------------------------------------------
* read_data
* utility function to read ASCII image data
* the files have a header of the type
*
*   components
*   n
*   height
*   n
*   width
*   n
*
* followed by the image data
*
*-------------------------------------------------------------------------
*/

static int read_data(const char* fname, /*IN*/
                     hsize_t *width, /*OUT*/
                     hsize_t *height /*OUT*/)
{
    int    i, n;
    int    color_planes;
    char   str[20];
    FILE   *f = NULL;
    int    w, h;
    int    n_elements;
    const char *data_file = H5_get_srcdir_filename(fname);
    int    ret_val = -1;

    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */

    if(NULL == (f = HDfopen(data_file, "r"))) {
        HDprintf( "Could not open file %s. Try set $srcdir \n", data_file );
        goto out;
    }

    if(fscanf(f, "%s", str) < 0 && HDferror(f)) {
        HDprintf( "fscanf error in file %s.\n", data_file );
        goto out;
    } /* end if */

    if(fscanf(f, "%d", &color_planes) < 0 && HDferror(f)) {
        HDprintf( "fscanf error in file %s.\n", data_file );
        goto out;
    } /* end if */

    if(fscanf(f, "%s", str) < 0 && HDferror(f)) {
        HDprintf( "fscanf error in file %s.\n", data_file );
        goto out;
    } /* end if */

    if(fscanf(f, "%d", &h) < 0 && HDferror(f)) {
        HDprintf( "fscanf error in file %s.\n", data_file );
        goto out;
    } /* end if */

    if(fscanf(f, "%s", str) < 0 && HDferror(f)) {
        HDprintf( "fscanf error in file %s.\n", data_file );
        goto out;
    } /* end if */

    if(fscanf(f, "%d", &w) < 0 && HDferror(f)) {
        HDprintf( "fscanf error in file %s.\n", data_file );
        goto out;
    } /* end if */

    /* Check product for overflow */
    if(w < 1 || h < 1 || color_planes < 1)
        goto out;
    if(w > INT_MAX / h)
        goto out;
    if(w * h > INT_MAX / color_planes)
        goto out;

    /* Compute buffer size */
    n_elements = w * h * color_planes;

    /* Check buffer size for overflow */
    if(n_elements > INT_MAX / (int)sizeof(unsigned char))
        goto out;

    /* Release the buffer, if it was previously allocated */
    if(image_data)
        HDfree(image_data);

    /* Allocate the image data buffer */
    image_data = (unsigned char *)HDmalloc((size_t)n_elements * sizeof(unsigned char));
    if(NULL == image_data)
        goto out;

    *width = (hsize_t)w;
    *height = (hsize_t)h;

    /* Read data elements */
    for(i = 0; i < n_elements; i++) {
        if(fscanf(f, "%d", &n) < 0 && HDferror(f)) {
            HDprintf( "fscanf error in file %s.\n", data_file );
            goto out;
        } /* end if */
        image_data[i] = (unsigned char)n;
    } /* end for */

    /* Indicate success */
    ret_val = 1;

out:
    if(f)
        HDfclose(f);

    return ret_val;
} /* end read_data() */


/*-------------------------------------------------------------------------
* read_palette
* Read an ASCII palette file .PAL into an array
* the files have a header of the type
*
* Parameters:
*  fname - name of file to read.
*  palette - array of rgb_t to store the read palette.
*  palette_size - number of elements in 'palette' array
*
*-------------------------------------------------------------------------
*/


#define STRING_JASC   "JASC-PAL"
#define VERSION_JASC  "0100"
#define STRING_CWPAL  "CWPAL"
#define VERSION_CWPAL "100"

static int read_palette(const char* fname,
                        rgb_t *palette,
                        size_t palette_size)
{
    FILE          *file;
    char          buffer[80];
    unsigned      u;
    unsigned int  red;
    unsigned int  green;
    unsigned int  blue;
    unsigned      nentries;
    const char *data_file = H5_get_srcdir_filename(fname);

    /* ensure the given palette is valid */
    if (!palette)
        return -1;

    /* open the input file */
    if (!(file = HDfopen(data_file, "r")))
    {
        HDprintf( "Could not open file %s. Try set $srcdir \n", data_file );
        return -1;
    }

    /* read the file ident string */
    if (HDfgets(buffer, sizeof(buffer), file) == NULL)
    {
        HDfclose(file);
        return -1;
    }

    /* ensure it matches the palette file ident string */
    if ( HDstrncmp(buffer, STRING_JASC, sizeof(STRING_JASC) - 1) != 0 &&
            HDstrncmp(buffer, STRING_CWPAL, sizeof(STRING_CWPAL) - 1) != 0 )
    {
        HDfclose(file);
        return -1;
    }

    /* read the version string */
    if (HDfgets(buffer, sizeof(buffer), file) == NULL)
    {
        HDfclose(file);
        return -1;
    }

    /* ensure it matches the palette file version string */
    if ( HDstrncmp(buffer, VERSION_JASC, sizeof(VERSION_JASC) - 1) != 0 &&
            HDstrncmp(buffer, VERSION_CWPAL, sizeof(VERSION_CWPAL) - 1) != 0 )
    {
        HDfclose(file);
        return -1;
    }

    /* read the number of colors */
    if (HDfgets(buffer, sizeof(buffer), file) == NULL)
    {
        HDfclose(file);
        return -1;
    }


    /* extract the number of colors.
    check for missing version or number of colors
    in this case it reads the first entry
    */
    if ( HDstrlen( buffer ) > 4 )
    {
        HDfclose(file);
        return -1;
    }

    if (sscanf(buffer, "%u", &nentries) != 1)
    {
        HDfclose(file);
        return -1;
    }

    /* ensure there are a sensible number of colors in the palette */
    if ((nentries > 256) || (nentries > palette_size))
    {
        HDfclose(file);
        return(-1);
    }

    /* read the palette entries */
    for (u = 0; u < nentries; u++)
    {
        /* extract the red, green and blue color components.  */
        if (fscanf(file, "%u %u %u", &red, &green, &blue) != 3)
        {
            HDfclose(file);
            return -1;
        }
        /* store this palette entry */
        palette[u].r = (unsigned char)red;
        palette[u].g = (unsigned char)green;
        palette[u].b = (unsigned char)blue;
    }

    /* close file */
    HDfclose(file);

    return (int)nentries;
}

