
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


#ifndef _H5IMpalette_H
#define _H5IMpalette_H


#include "H5lite.h"



/* HDF5 palette information */
typedef struct H5IM_paletteinfo_t 
{
 char     color_model[20];   /* Color model, PAL_COLORMODEL 	*/
 char     type[20];          /* Type, PAL_TYPE 	*/
 int		    minmax[2];	        /* Minimum, maximum value of data, PAL_MINMAXNUMERIC*/
} H5IM_paletteinfo_t;



/* Default Template identifier for palletes */
#define H5IM_STDPALETTE 0

/* Default struct H5L_imageinfo_t for 8bit images */
static const H5IM_paletteinfo_t	H5IM_create_dfltpalette = 
{

 "RGB",                    /* Color model, PAL_COLORMODEL   	*/
 "STANDARD8",              /* Type, PAL_TYPE */
 {-1,-1},	                 /* Minimum, maximum value of data, PAL_MINMAXNUMERIC	*/
};



herr_t H5IMattach_palette( hid_t loc_id, 
                           const char *image_name, 
                           const char *pal_name,
                           const hsize_t *pal_dims,
                           hid_t plist_id,
                           const void *pal_data ); 



#endif
