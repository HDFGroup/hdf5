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

#include "hdf5.h"
#include "h5test.h"
#include "h5repack.h"


/*-------------------------------------------------------------------------
 * Function: aux_objinsert_filter
 *
 * Purpose: auxiliary function, inserts the filter in object OBJ
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */
static void aux_objinsert_filter(pack_info_t *obj,
                                 filter_info_t filt)
{
 obj->nfilters=1;
 obj->filter[0]=filt;

}

/*-------------------------------------------------------------------------
 * Function: filter_this
 *
 * Purpose: find the object name NAME (got from the traverse list)
 *  in the repack options list; assign the filter information OBJ
 *
 * Return: 0 not found, 1 found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 19, 2003
 *
 *-------------------------------------------------------------------------
 */
int filter_this(const char* name,    /* object name from traverse list */
                pack_opt_t *options, /* repack options */
                pack_info_t *obj /*OUT*/)    /* info about object to filter */
{
 char *pdest;
 int  result;
 int  i, j;
 
 /* if we are applying to all objects just return true */
 if (options->all_filter)
 {
  /* assign the global filter and chunk info to the OBJ info */
  aux_objinsert_filter( obj, options->filter_g );
  obj->chunk=options->chunk_g;
  return 1;
 }
 
 for ( i=0; i<options->op_tbl->nelems; i++) 
 {
  for ( j=0; j<options->op_tbl->objs[i].nfilters; j++)
  {
   if (options->op_tbl->objs[i].filter[j].filtn != -1 )
   {
    if (strcmp(options->op_tbl->objs[i].path,name)==0)
    {
     *obj=options->op_tbl->objs[i];
     return 1;
    }
    
    pdest  = strstr(name,options->op_tbl->objs[i].path);
    result = (int)(pdest - name);
    
    /* found at position 1, meaning without '/' */
    if( pdest != NULL && result==1 )
    {
     *obj=options->op_tbl->objs[i];
     return 1;
    }
   } /*if*/
  }/*j*/
 }/*i*/
 
 return 0;
}


/*-------------------------------------------------------------------------
 * Function: print_filters
 *
 * Purpose: print the filters in DCPL
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 19, 2003
 *
 *-------------------------------------------------------------------------
 */

int print_filters(hid_t dcpl_id)   
{
 int          nfilters;       /* number of filters */
 unsigned     filt_flags;     /* filter flags */
 H5Z_filter_t filtn;          /* filter identification number */
 unsigned     cd_values[20];  /* filter client data values */
 size_t       cd_nelmts;      /* filter client number of values */
 size_t       cd_num;         /* filter client data counter */
 char         f_name[256];    /* filter/file name */
 char         s[64];          /* temporary string buffer */
 int          i;

 /* get information about filters */
 if ((nfilters = H5Pget_nfilters(dcpl_id))<0) 
  return -1;
 
 for (i=0; i<nfilters; i++) 
 {
  cd_nelmts = NELMTS(cd_values);
  filtn = H5Pget_filter(dcpl_id, 
   (unsigned)i, 
   &filt_flags, 
   &cd_nelmts,
   cd_values, 
   sizeof(f_name), 
   f_name);
  
  f_name[sizeof(f_name)-1] = '\0';
  sprintf(s, "Filter-%d:", i);
  printf("    %-10s %s-%u %s {", s,
   f_name[0]?f_name:"method",
   (unsigned)filtn,
   filt_flags & H5Z_FLAG_OPTIONAL?"OPT":"");
  for (cd_num=0; cd_num<cd_nelmts; cd_num++) {
   printf("%s%u", cd_num?", ":"", cd_values[cd_num]);
  }
  printf("}\n");
 } 
 
 return 0;
}


/*-------------------------------------------------------------------------
 * Function: apply_filters
 *
 * Purpose: apply the filters in the object to the property list; 
 *  do extra checking in the case of SZIP; delete all filters in the case
 *  of H5Z_FILTER_NONE present in the PACK_INFO_T filter array
 *
 * Return: 0 success, -1 an error occured
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 19, 2003
 *
 *-------------------------------------------------------------------------
 */

int apply_filters(hid_t dcpl_id,
                  size_t size,         /* size of datatype in bytes */
                  pack_opt_t *options, /* repack options */
                  pack_info_t *obj)    /* info about object to filter */
{
 int          nfilters;       /* number of filters in DCPL */
 unsigned     aggression;     /* the deflate level */
 unsigned     szip_options_mask=H5_SZIP_NN_OPTION_MASK;
 unsigned     szip_pixels_per_block;
 int          i;

 /* get information about input filters */
 if ((nfilters = H5Pget_nfilters(dcpl_id))<0) 
  return -1;
 
/*-------------------------------------------------------------------------
 * check if we have the H5Z_FILTER_NONE filter
 * if so, just delete all filters from the DCPL and exit
 *-------------------------------------------------------------------------
 */

 for ( i=0; i<obj->nfilters; i++)
 {
  if (obj->filter[i].filtn==H5Z_FILTER_NONE)
  {
   if (nfilters && H5Pdelete_filter(dcpl_id,H5Z_FILTER_NONE)<0) 
    return -1;
   return 0;
  }
 }

/*-------------------------------------------------------------------------
 * the type of filter and additional parameter 
 * type can be one of the filters
 * H5Z_FILTER_NONE       0,  uncompress if compressed
 * H5Z_FILTER_DEFLATE	   1 , deflation like gzip	   
 * H5Z_FILTER_SHUFFLE    2 , shuffle the data
 * H5Z_FILTER_FLETCHER32 3 , fletcher32 checksum of EDC
 * H5Z_FILTER_SZIP       4 , szip compression 
 *-------------------------------------------------------------------------
 */

 for ( i=0; i<obj->nfilters; i++)
 {
  switch (obj->filter[i].filtn)
  {
  default:
   break;

/*-------------------------------------------------------------------------
 * H5Z_FILTER_DEFLATE	   1 , deflation like gzip	   
 *-------------------------------------------------------------------------
 */
  case H5Z_FILTER_DEFLATE:
   aggression=obj->filter[i].cd_values[0];
   /* set up for deflated data */
   if(H5Pset_chunk(dcpl_id, obj->chunk.rank, obj->chunk.chunk_lengths)<0)
    return -1;
   if(H5Pset_deflate(dcpl_id,aggression)<0)
    return -1;
   break;

/*-------------------------------------------------------------------------
 * H5Z_FILTER_SZIP       4 , szip compression 
 *-------------------------------------------------------------------------
 */
  case H5Z_FILTER_SZIP:
   szip_pixels_per_block=obj->filter[i].cd_values[0];
   /* check szip parameters */
   if (check_szip(obj->chunk.rank,
    obj->chunk.chunk_lengths,
    size,
    szip_options_mask,
    &szip_pixels_per_block,
    options)==1)
   {
    /* set up for szip data */
    if(H5Pset_chunk(dcpl_id,obj->chunk.rank,obj->chunk.chunk_lengths)<0)
     return -1;
    if (H5Pset_szip(dcpl_id, szip_options_mask, szip_pixels_per_block)<0) 
     return -1;
   }
   else
   {
    printf("Warning: SZIP filter cannot be applied\n");
   }
   break;

/*-------------------------------------------------------------------------
 * H5Z_FILTER_SHUFFLE    2 , shuffle the data
 *-------------------------------------------------------------------------
 */
  case H5Z_FILTER_SHUFFLE:
   if(H5Pset_chunk(dcpl_id, obj->chunk.rank, obj->chunk.chunk_lengths)<0)
    return -1;
   if (H5Pset_shuffle(dcpl_id)<0) 
    return -1;
   break;

/*-------------------------------------------------------------------------
 * H5Z_FILTER_FLETCHER32 3 , fletcher32 checksum of EDC
 *-------------------------------------------------------------------------
 */
  case H5Z_FILTER_FLETCHER32:
   if(H5Pset_chunk(dcpl_id, obj->chunk.rank, obj->chunk.chunk_lengths)<0)
    return -1;
   if (H5Pset_fletcher32(dcpl_id)<0) 
    return -1;
   break;
  } /* switch */
 }/*i*/

 return 0;
}



/*-------------------------------------------------------------------------
 * Function: check_szip
 *
 * Purpose: utility to check SZIP parameters
 *
 * SZIP compresses data block by block, with a user-tunable block size. 
 * This block size is passed in the parameter pixels_per_block and must be even, 
 * with typical values being 8, 10, 16, and 32. The more pixel values vary, 
 * the smaller this number should be. For optimal performance, the number of 
 * pixels per scan line (i.e., the size of the fastest-changing dimension in the chunk) 
 * should be an even multiple of the number of pixels per block. 
 *
 * Return: 1=can apply the filter
 *         0=cannot apply the filter
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 23, 2003
 *
 *-------------------------------------------------------------------------
 */

int check_szip(int rank,        /* chunk rank */
               hsize_t *dims,   /* chunk dims */
               size_t size,     /* size of datatype in bytes */
               unsigned szip_options_mask /*IN*/,
               unsigned *szip_pixels_per_block /*IN,OUT*/,
               pack_opt_t *options)
{
 szip_comp_t szip;
 int         i;
 unsigned    ppb=*szip_pixels_per_block;

 /*
 pixels_per_scanline = size of the fastest-changing dimension 
 Must be <= MAX_PIXELS_PER_SCANLINE and <= pixels
 */
 szip.pixels_per_scanline  = (unsigned)dims[rank-1];
 szip.pixels               = 1;
 for ( i = 0; i < rank; i++)
 {
  szip.pixels *= dims[i];
 }
 
 if (szip.pixels_per_scanline > MAX_PIXELS_PER_SCANLINE)
 {
  printf("Warning: in SZIP setting, pixels per scanline was set to <%d>, \
          MAX_PIXELS_PER_SCANLINE\n",MAX_PIXELS_PER_SCANLINE);
  szip.pixels_per_scanline = MAX_PIXELS_PER_SCANLINE;
 }
 
 /* 
  pixels_per_block must be an even number, and <= pixels_per_scanline 
  and <= MAX_PIXELS_PER_BLOCK
  */

 if (ppb > szip.pixels_per_scanline)
 {
  /* set ppb to pixels per scanline and try to make it an even number */
  ppb=szip.pixels_per_scanline;
  if (ppb%2!=0)
   ppb--;
  if (ppb<=1 )
  {
   printf("Warning: in SZIP settings, pixels per block <%d>,\
    cannot be set with pixels per scanline <%d>\n",
    ppb, szip.pixels_per_scanline);
   return 0;
  }
  else
  {
   if (options->verbose)
    printf("Warning: In SZIP settings, pixels per block was set to <%d>\n", ppb);
  }
 }
 szip.pixels_per_block  = ppb;
 *szip_pixels_per_block = ppb;

 szip.options_mask = szip_options_mask;
 szip.compression_mode = NN_MODE;

 /*
  bits_per_pixel
  Must be in range 1..24,32,64
  */
 switch(size) 
 {
 case 0:
  /* size was not provided for test */
  szip.bits_per_pixel = 0;
  break;
 case 1:
  szip.bits_per_pixel = 8;
  break;
 case 2:
  szip.bits_per_pixel = 16;
  break;
 case 4:
  szip.bits_per_pixel = 32;
  break;
 case 8:
  szip.bits_per_pixel = 64;
  break;
 default:
  printf("Warning: Invalid numeric type of size <%d> for SZIP\n",size);
  return 0;
 }

 return check_szip_params( szip.bits_per_pixel, 
                           szip.pixels_per_block, 
                           szip.pixels_per_scanline, 
                           szip.pixels);

}


/*-------------------------------------------------------------------------
 * Function: check_szip_params
 *
 * Purpose: Adapted from rice.c. Checks the SZIP parameters
 *
 * Return: 1=can apply the filter
 *         0=cannot apply the filter
 *
 *-------------------------------------------------------------------------
 */

int check_szip_params( unsigned bits_per_pixel, 
                       unsigned pixels_per_block, 
                       unsigned pixels_per_scanline, 
                       hsize_t image_pixels)
{
 
 if (pixels_per_block & 1)
 {
  printf("Pixels per block must be even.\n");
  return 0;
 }
 
 if (pixels_per_block > pixels_per_scanline)
 {
  printf("Pixels per block is greater than pixels per scanline.\n");
  return 0;
 }
 
 if (bits_per_pixel) /* if provided for test */
 {
  if (bits_per_pixel >= 1 && bits_per_pixel <= 24)
   ;
  else if (bits_per_pixel == 32 || bits_per_pixel == 64)
   ;
  else
  {
   printf("bits per pixel must be in range 1..24,32,64");
   return 0;
  }
 }
 
 if (pixels_per_block > MAX_PIXELS_PER_BLOCK) 
 {
  printf("maximum pixels per block exceeded");
  return 0;
 }
 
 if (pixels_per_block & 1) 
 {
  printf("pixels per block must be even");
  return 0;
 }
 
 if (pixels_per_block > pixels_per_scanline)
 {
  printf("pixels per block > pixels per scanline");
  return 0;
 }
 
 if (pixels_per_scanline > MAX_PIXELS_PER_SCANLINE)
 {
  printf("maximum pixels per scanline exceeded");
  return 0;
 }
 
 if (image_pixels < pixels_per_scanline)
 {
  printf("image pixels less than pixels per scanline");
  return 0;
 }
 
 if (image_pixels % pixels_per_scanline)
 {
  printf("Pixels (%d) must be integer multiple of pixels per scanline (%d)\n", 
   (unsigned)image_pixels,pixels_per_scanline);
  return 0;
 }
 
#if 0
 if (pixels_per_scanline % pixels_per_block)
 {
  printf("Pixels per scanline (%d) must be an integer multiple of pixels per block (%d)\n", 
   pixels_per_scanline, pixels_per_block);
  return 0;
 }
#endif
 
 return 1;
}

