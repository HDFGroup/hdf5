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


static void CANNOT_LAYOUT(pack_opt_t *options)
{  
 if (options->verbose)
  printf("Warning: This layout cannot be applied, this object\
   requires chunked layout\n");
}



/*-------------------------------------------------------------------------
 * Function: layout_this
 *
 * Purpose: check if the layout can be applied;
 *  find the object name NAME (got from the traverse list)
 *  in the repack options list; assign the layout information OBJ
 *
 * Return: 0 cannot apply, 1 can, -1 error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 30, 2003
 *
 *-------------------------------------------------------------------------
 */

int layout_this(hid_t dcpl_id,             /* DCPL from input object */
                const char* name,          /* object name from traverse list */
                pack_opt_t *options,       /* repack options */
                pack_info_t *pack /*OUT*/) /* object to apply layout */
{
 int          nfilters; /* number of filters in the input object  */
 H5D_layout_t layout;   /* layout */
 char         *pdest;
 int          result;
 int          i, ret=1;

 /* check if we have filters in the input object */
 if ((nfilters = H5Pget_nfilters(dcpl_id))<0) 
  return -1;
 
 /* applying to all objects */
 if (options->all_layout)
 {
  /* assign the global layout info to the OBJ info */
  pack->layout=options->layout_g;
  
  switch (options->layout_g)
  {
  case H5D_CHUNKED:
   pack->chunk.rank=options->chunk_g.rank;
   for ( i=0; i<pack->chunk.rank; i++) 
    pack->chunk.chunk_lengths[i]=options->chunk_g.chunk_lengths[i];
   break;
   
  case H5D_CONTIGUOUS:
  case H5D_COMPACT:
   if (nfilters)
   {
    CANNOT_LAYOUT(options);
    ret=0;
   }
   break;
   
  default:
   ret=0;
   break;
  }/*switch*/
  return ret;
 }

 /* find the object */
 for ( i=0; i<options->op_tbl->nelems; i++) 
 {
  layout=options->op_tbl->objs[i].layout;
  if ( layout != -1 )
  {
   if (strcmp(options->op_tbl->objs[i].path,name)==0)
   {
    if (nfilters && layout!=H5D_CHUNKED)
    {
     CANNOT_LAYOUT(options);
     return 0;
    }
    else
    {
     *pack=options->op_tbl->objs[i];
     return 1;
    }
   }
   
   pdest  = strstr(name,options->op_tbl->objs[i].path);
   result = (int)(pdest - name);
   
   /* found at position 1, meaning without '/' */
   if( pdest != NULL && result==1 )
   {
    if (nfilters && layout!=H5D_CHUNKED)
    {
     CANNOT_LAYOUT(options);
     return 0;
    }
    else
    {
     *pack=options->op_tbl->objs[i];
     return 1;
    }
   }
  }
 }

 return 0;
}
/*-------------------------------------------------------------------------
 * Function: apply_layout
 *
 * Purpose: apply a layout to the property list. Valid values for layout are: 
 *
 * H5D_COMPACT 
 *   Store raw data in the dataset object header in file. 
 *   This should only be used for very small amounts of raw data.
 * H5D_CONTIGUOUS 
 *   Store raw data separately from the object header in one large chunk 
 *   in the file. 
 * H5D_CHUNKED 
 *   Store raw data separately from the object header as chunks of data in 
 *   separate locations in the file. 
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 30, 2003
 *
 *-------------------------------------------------------------------------
 */

int apply_layout(hid_t dcpl_id,
                 pack_info_t *obj)   /* info about object  */
{
 
 
 if (H5Pset_layout(dcpl_id, obj->layout)<0) 
  return -1;
 
 if (H5D_CHUNKED==obj->layout) { /* set up chunk */
  if(H5Pset_chunk(dcpl_id, obj->chunk.rank, obj->chunk.chunk_lengths)<0)
   return -1;
 }
 else if (H5D_COMPACT==obj->layout) {
  if (H5Pset_alloc_time(dcpl_id, H5D_ALLOC_TIME_EARLY)<0) 
   return -1;
 }
 
 return 0;
}

