
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


#ifndef _H5Image_H
#define _H5Image_H


#include "H5Lite.h"


/* HDF5 image information */
typedef struct H5IM_imageinfo_t 
{
 char   subclass[20];	     /* Image subclass */
 char   color_model[20];   /* Color model  	*/
 char   interlace[20];     /* Interlace mode */
 int		  white_is_zero;	    /* For grayscale, bitmap images	  */
 int		  minmax[2];	        /* Minimum, maximum value of data	*/
 int		  bkindex;	          /* Index of the background color 	*/
 int		  trindex;	          /* Index of the transparent color 	*/
 int		  aspect_ratio;	     /* Aspect ratio 	*/
 float  gamma_correction;  /* Gamma correction  	*/
} H5IM_imageinfo_t;




/* Default Template identifier for 8bit images */
#define H5IM_8BIT 0


/* Default struct H5L_imageinfo_t for 8bit images */
static const H5IM_imageinfo_t	H5IM_create_dflt8bit = 
{

 "IMAGE_INDEXED",	         /* Image subclass */
 "RGB",                    /* Color model  	*/
 "",                       /* Interlace mode */
 -1,	                      /* white_is_zero	  */
 {-1,-1},	                 /* Minimum, maximum value of data	*/
 -1,	                      /* Index of the background color 	*/
 -1,	                      /* Index of the transparent color 	*/
 -1,	                      /* Aspect ratio 	*/
 -1                        /* Gamma correction  	*/
};




herr_t H5IMmake_image( hid_t loc_id, 
                       const char *dset_name, 
                       const hsize_t *dims,
                       hid_t plist_id,
                       const void *buffer );

/*********************** OLD *****************************/


#if 0

#define H5L_NORANGEINDEX 0
#define H5L_NOMINMAX     0



/* HDF5 image subclass */
typedef enum H5L_imagesc_t
{
  H5L_IMAGE_GRAYSCALE,       
  H5L_IMAGE_BITMAP,
  H5L_IMAGE_TRUECOLOR,
  H5L_IMAGE_INDEXED
} H5L_imagesc_t;



/* HDF5 image/palette color models */
typedef enum H5L_colormodel_t
{
  H5L_MODEL_RGB        = 0,   /*RGB                 */
  H5L_MODEL_YUV        = 1,   /*YUV                 */
  H5L_MODEL_CMY        = 2,   /*CMY                 */
  H5L_MODEL_CMYK       = 3,   /*CMYK                */
  H5L_MODEL_YCBCR      = 4,   /*YCbCr               */
  H5L_MODEL_HSV        = 5,   /*HSV                 */
} H5L_colormodel_t;


/* HDF5 image interlace mode */
typedef enum H5L_interlace_t 
{
  H5L_NOT_DEFINED      = -1,
  H5L_INTERLACE_PIXEL  = 0,   
  H5L_INTERLACE_PLANE  = 1,  
  H5L_INTERLACE_LINE   = 2
} H5L_interlace_t;



/* HDF5 palette types */
typedef enum H5L_palette_t 
{
  H5L_PAL_STANDARD,   
  H5L_PAL_RANGEINDEX  
} H5L_palette_t;





/* HDF5 image information */
typedef struct H5L_imageinfo_t 
{
 H5L_imagesc_t    subclass;	       /* Image subclass */
 H5L_interlace_t  interlace;       /* Interlace mode */
 int		            white_is_zero;	  /* For grayscale, bitmap images	  */
 int		            minmax[2];	      /* Minimum, maximum value of data	*/
 int		            bkindex;	        /* Index of the background color 	*/
 int		            trindex;	        /* Index of the transparent color 	*/
 int		            aspect_ratio;	   /* Aspect ratio 	*/
 H5L_colormodel_t color_model;     /* Color model  	*/
 float            gamma_correction;/* Gamma correction  	*/
} H5L_imageinfo_t;



herr_t H5Lmake_image( hid_t loc_id, 
                        const char *dset_name,
                        const hsize_t *dims,
                        hid_t file_type_id,
                        hid_t mem_type_id,
                        const void *buffer,
                        H5L_imageinfo_t imageinfo );

herr_t H5Lmake_image_indexed( hid_t loc_id, 
                        const char *dset_name,
                        const hsize_t *dims,
                        hid_t file_type_id,
                        hid_t mem_type_id,
                        const void *buffer );

herr_t H5Lmake_image_truecolor( hid_t loc_id, 
                        const char *dset_name,
                        const hsize_t *dims,
                        hid_t file_type_id,
                        hid_t mem_type_id,
                        const void *buffer );

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
                        const void *minmax );

#endif

#endif
