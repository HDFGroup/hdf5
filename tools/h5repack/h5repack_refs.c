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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "H5private.h"
#include "h5tools_utils.h"
#include "h5repack.h"

extern char  *progname;

static const char* MapIdToName(hid_t refobj_id,
                               trav_table_t *travt);




/*-------------------------------------------------------------------------
 * Function: MapIdToName
 *
 * Purpose: map an object ID to a name
 *
 *-------------------------------------------------------------------------
 */

static const char* MapIdToName(hid_t refobj_id,
                               trav_table_t *travt)
{
 hid_t id;
 hid_t fid;
 H5G_stat_t refstat;    /* Stat for the refobj id */
 H5G_stat_t objstat;    /* Stat for objects in the file */
 int   i;

 /* obtain information to identify the referenced object uniquely */
 if(H5Gget_objinfo(refobj_id, ".", 0, &refstat) <0)
  return NULL;

 /* obtains the file ID given an object ID.  This ID must be closed */
 if ((fid = H5Iget_file_id(refobj_id))<0)
 {
  return NULL;
 }

 /* linear search */
 for ( i=0; i<travt->nobjs; i++)
 {
  switch ( travt->objs[i].type )
  {
  default:
   break;

  /*-------------------------------------------------------------------------
   * H5G_DATASET
   *-------------------------------------------------------------------------
   */

  case H5G_DATASET:

   if ((id = H5Dopen(fid,travt->objs[i].name))<0)
    return NULL;
   if(H5Gget_objinfo(id, ".", 0, &objstat) <0)
    return NULL;
   if (H5Dclose(id)<0)
    return NULL;
   if (!HDmemcmp(&refstat.fileno, &objstat.fileno, sizeof(refstat.fileno)) && !HDmemcmp(&refstat.objno, &objstat.objno, sizeof(refstat.objno)))
   {
    H5Fclose(fid);
    return travt->objs[i].name;
   }
   break;
  }  /* switch */
 } /* i */

 if (H5Fclose(fid)<0)
  return NULL;

 return NULL;
}

