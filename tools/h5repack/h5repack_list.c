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
#include "h5repack.h"
#include "h5repack_list.h"
#include "h5diff.h"


/*-------------------------------------------------------------------------
 * Function: get_objlist
 *
 * Purpose: locate all HDF5 objects in the file 
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September, 23, 2003
 *
 *-------------------------------------------------------------------------
 */


int get_objlist(char* fname, packoptions_t *options)
{
 hid_t    fid; 
 int      nobjects, i;
 info_t   *info=NULL;

/*-------------------------------------------------------------------------
 * open the file 
 *-------------------------------------------------------------------------
 */

 /* disable error reporting */
 H5E_BEGIN_TRY {
 
 /* Open the files */
 if ((fid=H5Fopen(fname,H5F_ACC_RDONLY,H5P_DEFAULT))<0 ){
  printf("h5repack: <%s>: No such file or directory\n", fname );
  exit(1);
 }
 /* enable error reporting */
 } H5E_END_TRY;


/*-------------------------------------------------------------------------
 * get the number of objects in the file
 *-------------------------------------------------------------------------
 */

 if ((nobjects = H5get_object_info(fid, NULL ))<0) {
  printf("h5repack: <%s>: Could not obtain object list\n", fname );
  return -1;
 }

/*-------------------------------------------------------------------------
 * get the list of objects in the file
 *-------------------------------------------------------------------------
 */

 if ((info = (info_t*) malloc( nobjects * sizeof(info_t)))==NULL){
  printf("h5repack: <%s>: Could not allocate object list\n", fname );
  return -1;
 }
 if (H5get_object_info(fid, info )<0) {
  printf("h5repack: <%s>: Could not obtain object list\n", fname );
  return -1;
 }

/*-------------------------------------------------------------------------
 * compare with user supplied list
 *-------------------------------------------------------------------------
 */
 
 if (options->verbose)
  printf("Opening file <%s>. Searching for objects to modify...\n",fname);
 
 for ( i = 0; i < options->op_tbl->nelems; i++) 
 {
  char* obj_name=options->op_tbl->objs[i].path;
  if (options->verbose)
   printf(PFORMAT1,"","",obj_name);
  
  /* the input object names are present in the file and are valid */
  if (info_getindex(obj_name,nobjects,info)<0)
  {
   printf("\nError: Could not find <%s> in file <%s>. Exiting...\n",
    obj_name,fname);
   H5Fclose(fid);
   info_free(info,nobjects);
   exit(1);
  }
  if (options->verbose)
   printf("...Found\n");
 }



/*-------------------------------------------------------------------------
 * free
 *-------------------------------------------------------------------------
 */
 H5Fclose(fid);
 info_free(info,nobjects);
 return 0;

}
