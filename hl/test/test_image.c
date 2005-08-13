
/****************************************************************************
 * NCSA HDF                                                                 *
 * Scientific Data Technologies                                             *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING f.                                                        *
 *                                                                          *
 ****************************************************************************/

#include "H5IM.h"
#include "pal_rgb.h"
#include <stdlib.h>
#include <string.h>

#define FILE1       "test_image1.h5"
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
#define PAL1_NAME   "rainbow"
#define PAL2_NAME   "sepia"
#define PAL3_NAME   "earth"
#define PAL4_NAME   "blue-red"


#define WIDTH       (hsize_t)500
#define HEIGHT      (hsize_t)200
#define PAL_ENTRIES 9

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
static int read_palette(const char* file_name, rgb_t *palette, int palette_size);

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
 printf("All image tests passed.\n");
 return 0;

error:
 printf("***** %d IMAGE TEST%s FAILED! *****\n",nerrors, 1 == nerrors ? "" : "S");
 return 1;
}

/*-------------------------------------------------------------------------
 * a simple test that generates images and palettes
 *-------------------------------------------------------------------------
 */

static int test_simple(void)
{
 hid_t         fid;
 hsize_t       width;
 hsize_t       height;
 hsize_t       planes;
 hsize_t       pal_dims[] = {PAL_ENTRIES,3};
 hsize_t       pal_dims_out[2];
 char          interlace[20];
 hssize_t      npals;
 hsize_t       i, j;
 herr_t        is_image;
 herr_t        is_pal;
 unsigned char image_in1 [ WIDTH*HEIGHT ];
 unsigned char image_out1[ WIDTH*HEIGHT ];
 unsigned char image_in2 [ WIDTH*HEIGHT*3 ];
 unsigned char image_out2[ WIDTH*HEIGHT*3 ];
 unsigned char pal_data_out[PAL_ENTRIES*3];
 unsigned char n;
 int space;
 /* create a PAL_ENTRIES entry palette */
 unsigned char pal_data_in[PAL_ENTRIES*3] = {
 0,0,168,      /* dark blue */
 0,0,252,      /* blue */
 0,168,252,    /* ocean blue */
 84,252,252,   /* light blue */
 168,252,168,  /* light green */
 0,252,168,    /* green */
 252,252,84,   /* yellow */
 252,168,0,    /* orange */
 252,0,0};     /* red */

 /* create an image of 9 values divided evenly by the array */
 space = WIDTH*HEIGHT / PAL_ENTRIES;
 for (i=0, j=0, n=0; i < WIDTH*HEIGHT; i++, j++ )
 {
  image_in1[i] = n;
  if ( j > space )
  {
   n++;
   j=0;
  }
  if (n>PAL_ENTRIES-1) n=0;
 }
 /* create an image 3 byte RGB image */
 for (i=0, j=0, n=0; i < WIDTH*HEIGHT*3; i++, j++)
 {
  image_in2[i] = n;
  if (j==3)
  {
   n++;
   j=0;
  }
  if (n>255) n=0;
 }

 /* create a file using default properties */
 if ((fid=H5Fcreate(FILE1,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  goto out;


 printf("Testing API functions\n");

/*-------------------------------------------------------------------------
 * indexed image test
 *-------------------------------------------------------------------------
 */

 TESTING2("indexed images");

 /* write image */
 if (H5IMmake_image_8bit(fid,"image1",WIDTH,HEIGHT,image_in1)<0)
  goto out;

 /* make a palette */
 if (H5IMmake_palette(fid,"palette",pal_dims,pal_data_in)<0)
  goto out;

 /* attach the palette to the image dataset */
 if (H5IMlink_palette(fid,"image1","palette")<0)
  goto out;

 /* get info */
 if (H5IMget_image_info(fid,"image1",&width,&height,&planes,interlace,&npals)<0)
  goto out;

 if (width!=WIDTH)
  goto out;
 if (height!=HEIGHT)
  goto out;
 if (planes!=1)
  goto out;
 if (npals!=1)
  goto out;

 /* read image */
 if (H5IMread_image(fid,"image1",image_out1)<0)
  goto out;

 /* check */
 for (i = 0; i < height*width*planes; i++)
 {
  if ( image_in1[i] != image_out1[i] )
  {
   goto out;
  }
 }


 PASSED();

/*-------------------------------------------------------------------------
 * true color image test
 *-------------------------------------------------------------------------
 */

 TESTING2("true color image");

 /* write image */
 if (H5IMmake_image_24bit(fid,"image2",WIDTH,HEIGHT,"INTERLACE_PIXEL",image_in2))
  goto out;

 /* get info */
 if (H5IMget_image_info(fid,"image2",&width,&height,&planes,interlace,&npals)<0)
  goto out;

 if (width!=WIDTH)
  goto out;
 if (height!=HEIGHT)
  goto out;
 if (planes!=3)
  goto out;

 /* read image */
 if (H5IMread_image(fid,"image2",image_out2)<0)
  goto out;

 /* check */
 for (i = 0; i < height*width*planes; i++)
 {
  if ( image_in2[i] != image_out2[i] )
  {
    goto out;
  }
 }

 PASSED();

/*-------------------------------------------------------------------------
 * H5IMget_npalettes test
 *-------------------------------------------------------------------------
 */

 TESTING2("palette functions");

 if (H5IMget_npalettes(fid,"image1",&npals)<0)
  goto out;

 if (npals!=1)
  goto out;

/*-------------------------------------------------------------------------
 * H5IMget_palette_info test
 *-------------------------------------------------------------------------
 */

 if (H5IMget_palette_info(fid,"image1",0,pal_dims_out)<0)
  goto out;

 /* check */
 for (i = 0; i < 2; i++)
 {
  if ( pal_dims[i] != pal_dims_out[i] )
  {
   goto out;
  }
 }

/*-------------------------------------------------------------------------
 * H5IMget_palette test
 *-------------------------------------------------------------------------
 */

 if (H5IMget_palette(fid,"image1",0,pal_data_out)<0)
  goto out;

 /* check */
 for (i = 0; i < PAL_ENTRIES*3; i++)
 {
  if ( pal_data_in[i] != pal_data_out[i] )
  {
   goto out;
  }
 }

/*-------------------------------------------------------------------------
 * H5IMis_image test
 *-------------------------------------------------------------------------
 */

 if ((is_image=H5IMis_image(fid,"image1"))<0)
  goto out;

 if (is_image!=1)
  goto out;

 if ((is_image=H5IMis_image(fid,"image2"))<0)
  goto out;

 if (is_image!=1)
  goto out;

/*-------------------------------------------------------------------------
 * H5IMis_palette test
 *-------------------------------------------------------------------------
 */

 if ((is_pal=H5IMis_palette(fid,"palette"))<0)
  goto out;

 if (is_pal!=1)
  goto out;

 PASSED();

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */
 if (H5Fclose(fid)<0)
  goto out;

 return 0;

 /* error zone, gracefully close */
out:
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

 printf("Testing read ascii image data and generate images\n");

/*-------------------------------------------------------------------------
 * read 8bit image data
 *-------------------------------------------------------------------------
 */

 TESTING2("make indexed image");

 /* read first data file */
 if (read_data(DATA_FILE1,&width,&height)<0)
  goto out;

 /* make an image */
 if (H5IMmake_image_8bit(fid,IMAGE1_NAME,width,height,image_data)<0)
  goto out;

 PASSED();


 TESTING2("attaching palettes");

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
 if (read_palette(PAL2_FILE, rgb, sizeof(rgb))<0)
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
 if (read_palette(PAL3_FILE, rgb, sizeof(rgb))<0)
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
  pal[i]  =n;
  pal[i+1]=0;
  pal[i+2]=255-n;
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

 TESTING2("make true color image with pixel interlace");

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

 TESTING2("make true color image with plane interlace");

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

 return 0;

 /* error zone, gracefully close */
out:
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
 float    *data;
 int      imax, jmax, kmax;
 float    valex, xmin, xmax, value;
 FILE     *f;
 char     *srcdir = getenv("srcdir"); /* the source directory */
 char     data_file[512]="";          /* buffer to hold name of existing data file */
 int      i;

 /* create a file using default properties */
 if ((fid=H5Fcreate(FILE3,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  goto out;

 printf("Testing read and process data and make indexed images\n");

/*-------------------------------------------------------------------------
 * compose the name of the file to open, using the srcdir, if appropriate
 *-------------------------------------------------------------------------
 */
 if ( srcdir )
 {
  strcpy(data_file, srcdir);
  strcat(data_file, "/");
 }
 strcat(data_file,DATA_FILE4);

/*-------------------------------------------------------------------------
 * read data; the file data format is described below
 *-------------------------------------------------------------------------
 */

 f  = fopen( data_file, "r" ) ;
 if ( f == NULL )
 {
  printf( "Could not find file %s. Try set $srcdir \n", data_file );
  H5Fclose(fid);
  return -1;
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


 fscanf( f, "%d %d %d", &imax, &jmax, &kmax );
 fscanf( f, "%f %f %f", &valex, &xmin, &xmax );

 data = (float*) malloc ( imax * jmax * kmax * sizeof( float ));
 image_data = (unsigned char*) malloc ( imax * jmax * kmax * sizeof( unsigned char ));

 for ( i = 0; i < imax * jmax * kmax; i++ )
 {
  fscanf( f, "%f ", &value );
  data[i] = value;
 }
 fclose( f );

/*-------------------------------------------------------------------------
 * transform the data from floating point to unsigend char
 * we are processing all the data here
 *-------------------------------------------------------------------------
 */

 TESTING2("make indexed image from all the data");

 for ( i = 0; i < imax * jmax * kmax; i++ )
 {
  image_data[i] = (unsigned char)(( 255 * (data[i] - xmin ) ) / (xmax - xmin ));
 }

 /* Make the image */
 if ((H5IMmake_image_8bit(fid,"All data",(hsize_t)imax,(hsize_t)jmax,image_data))<0)
  goto out;

 PASSED();

/*-------------------------------------------------------------------------
 * transform the data from floating point to unsigend char
 * here we just process the land data
 *-------------------------------------------------------------------------
 */

 TESTING2("make indexed image from land data");

 for ( i = 0; i < imax * jmax * kmax; i++ )
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
 * transform the data from floating point to unsigend char
 * here we just process the sea data
 *-------------------------------------------------------------------------
 */

 TESTING2("make indexed image from sea data");

 for ( i = 0; i < imax * jmax * kmax; i++ )
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

 TESTING2("attaching palettes");

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

 return 0;

 /* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Fclose(fid);
 } H5E_END_TRY;
 H5_FAILED();
 return FAIL;
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

static int read_data( const char* fname, /*IN*/
                      hsize_t *width, /*OUT*/
                      hsize_t *height /*OUT*/ )
{
 int    i, n;
 int    color_planes;
 char   str[20];
 FILE   *f;
 int    w, h;
 char   *srcdir = getenv("srcdir"); /* the source directory */
 char   data_file[512]="";          /* buffer to hold name of existing data file */

/*-------------------------------------------------------------------------
 * compose the name of the file to open, using "srcdir", if appropriate
 *-------------------------------------------------------------------------
 */
 strcpy(data_file, "");
 if (srcdir)
 {
  strcpy(data_file, srcdir);
  strcat(data_file, "/");
 }
 strcat(data_file,fname);

/*-------------------------------------------------------------------------
 * read
 *-------------------------------------------------------------------------
 */

 f = fopen(data_file, "r");
 if ( f == NULL )
 {
  printf( "Could not open file %s. Try set $srcdir \n", data_file );
  return -1;
 }

 fscanf( f, "%s", str );
 fscanf( f, "%d", &color_planes );
 fscanf( f, "%s", str );
 fscanf( f, "%d", &h);
 fscanf( f, "%s", str );
 fscanf( f, "%d", &w);

 *width = (hsize_t)w;
 *height = (hsize_t)h;

 if ( image_data )
 {
  free( image_data );
  image_data=NULL;
 }

 image_data = (unsigned char*) malloc (w * h * color_planes * sizeof( unsigned char ));

 for (i = 0; i < h * w * color_planes ; i++)
 {
  fscanf( f, "%d",&n );
  image_data[i] = (unsigned char)n;
 }
 fclose(f);

 return 1;

}



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
                        int palette_size)
{
 FILE          *file;
 char          buffer[80];
 int           i;
 unsigned int  red;
 unsigned int  green;
 unsigned int  blue;
 int           nentries;
 char          *srcdir = getenv("srcdir"); /* the source directory */
 char          data_file[512];             /* buffer to hold name of existing data file */

/*-------------------------------------------------------------------------
 * compose the name of the file to open, using "srcdir", if appropriate
 *-------------------------------------------------------------------------
 */
 strcpy(data_file, "");
 if (srcdir)
 {
  strcpy(data_file, srcdir);
  strcat(data_file, "/");
 }
 strcat(data_file,fname);

 /* ensure the given palette is valid */
 if (!palette)
  return -1;

 /* open the input file */
 if (!(file = fopen(data_file, "r")))
 {
  printf( "Could not open file %s. Try set $srcdir \n", data_file );
  return -1;
 }

 /* read the file ident string */
 if (fgets(buffer, sizeof(buffer), file) == NULL)
 {
  fclose(file);
  return -1;
 }

 /* ensure it matches the palette file ident string */
 if ( strncmp(buffer, STRING_JASC, sizeof(STRING_JASC) - 1) != 0 &&
      strncmp(buffer, STRING_CWPAL, sizeof(STRING_CWPAL) - 1) != 0 )
 {
  fclose(file);
  return -1;
 }

 /* read the version string */
 if (fgets(buffer, sizeof(buffer), file) == NULL)
 {
  fclose(file);
  return -1;
 }

 /* ensure it matches the palette file version string */
 if ( strncmp(buffer, VERSION_JASC, sizeof(VERSION_JASC) - 1) != 0 &&
      strncmp(buffer, VERSION_CWPAL, sizeof(VERSION_CWPAL) - 1) != 0 )
 {
  fclose(file);
  return -1;
 }

 /* read the number of colors */
 if (fgets(buffer, sizeof(buffer), file) == NULL)
 {
  fclose(file);
  return -1;
 }


 /* extract the number of colors.
    check for missing version or number of colors
    in this case it reads the first entry
    */
 if ( strlen( buffer ) > 4 )
 {
  fclose(file);
  return -1;
 }

 if (sscanf(buffer, "%d", &nentries) != 1)
 {
  fclose(file);
  return -1;
 }

 /* ensure there are a sensible number of colors in the palette */
 if ((nentries < 0) || (nentries > 256) || (nentries > palette_size))
 {
  fclose(file);
  return(-1);
 }

 /* read the palette entries */
 for (i = 0; i < nentries; i++)
 {
  /* extract the red, green and blue color components.  */
  if (fscanf(file, "%u %u %u", &red, &green, &blue) != 3)
  {
   fclose(file);
   return -1;
  }
  /* store this palette entry */
  palette[i].r = (unsigned char)red;
  palette[i].g = (unsigned char)green;
  palette[i].b = (unsigned char)blue;
 }

 /* close file */
 fclose(file);

 return nentries;
}



