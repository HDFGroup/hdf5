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


#include "h5trav.h"
#include "H5private.h" 



/* functions for traversal */
int traverse( hid_t loc_id, 
              const char *group_name, 
              trav_table_t *table, 
              trav_info_t *info, 
              int *idx );

herr_t get_nobjects( hid_t loc_id, 
                     const char *group_name );

herr_t get_name_type( hid_t loc_id, 
                      const char *group_name, 
                      int idx, 
                      char **name, 
                      int *type );

/*-------------------------------------------------------------------------
 * Function: h5trav_getinfo
 *
 * Purpose: get an array of "trav_info_t" , containing the name and type of 
 *  objects in the file
 *
 * Return: number of objects in file
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 6, 2002
 *
 *-------------------------------------------------------------------------
 */

int h5trav_getinfo( hid_t file_id, trav_info_t *info )
{

 trav_table_t  *table=NULL;
 int           nobjects=0;

 /* init table */
 trav_table_init( &table );

 /* iterate starting on the root group */
 if (( nobjects = traverse( file_id, "/", table, info, &nobjects )) < 0 )
  return -1;

 /* free table */
 trav_table_free( table );

 return nobjects;

}


/*-------------------------------------------------------------------------
 * Function: h5trav_getindex
 *
 * Purpose: get index of OBJ in list
 *
 * Return: index, -1 if not found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */

int h5trav_getindex( const char *obj, int nobjs, trav_info_t *info )
{
 char *pdest;
 int  result;
 int  i;

 for ( i = 0; i < nobjs; i++) 
 {
  if ( strcmp(obj,info[i].name)==0 )
   return i;

  pdest  = strstr( info[i].name, obj );
  result = (int)(pdest - info[i].name);

  /* found at position 1, meaning without '/' */
  if( pdest != NULL && result==1 )
   return i;
 }
 return -1;
}



/*-------------------------------------------------------------------------
 * Function: h5trav_freeinfo
 *
 * Purpose: free info memory
 *
 *-------------------------------------------------------------------------
 */

void h5trav_freeinfo( trav_info_t *info, int nobjs )
{
 int i;
	if ( info )
	{
		for ( i = 0; i < nobjs; i++)
		{
			if (info[i].name)
		 	HDfree( info[i].name );
		}
		HDfree(info);
	}
}


/*-------------------------------------------------------------------------
 * Function: count_objects
 *
 * Purpose: operator function 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 10, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t count_objects( hid_t loc_id, const char *name, void *op_data)
{

 H5G_stat_t statbuf;

 if (H5Gget_objinfo( loc_id, name, 0, &statbuf) < 0 )
  return 1;

 (*(int *)op_data)++;

 /* Define a default zero value for return. This will cause the iterator to continue */
 return 0;
} 

/*-------------------------------------------------------------------------
 * Function: get_nobjects
 *
 * Purpose:  Counts the number of objects in the group GROUP_NAME
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 10, 2002
 *
 * Return:  
 *     Success: The return value of the first operator that
 *              returns non-zero, or zero if all members were
 *              processed with no operator returning non-zero.
 *
 *     Failure: Negative if something goes wrong within the
 *              library, or the negative value returned by one
 *              of the operators.
 *
 *-------------------------------------------------------------------------
 */

herr_t get_nobjects( hid_t loc_id, const char *group_name ) 
{

 int nobjs = 0;

 if ( H5Giterate( loc_id, group_name, NULL, count_objects, (void *)&nobjs ) < 0 )
  return -1;

 return nobjs;
}


/*-------------------------------------------------------------------------
 * Function: opget_info
 *
 * Purpose: operator function 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 10, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t opget_info( hid_t loc_id, const char *name, void *op_data)
{

 H5G_stat_t statbuf;

 if (H5Gget_objinfo( loc_id, name, 0, &statbuf) < 0 )
  return -1;

 ((trav_info_t *)op_data)->type = statbuf.type;
 ((trav_info_t *)op_data)->name = (char *)HDstrdup(name);

 /* Define 1 for return. This will cause the iterator to stop */
 return 1;
} 


/*-------------------------------------------------------------------------
 * Function: get_name_type
 *
 * Purpose:  
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 10, 2002
 *
 * Return:  
 *     Success: The return value of the first operator that
 *              returns non-zero, or zero if all members were
 *              processed with no operator returning non-zero.
 *
 *     Failure: Negative if something goes wrong within the
 *              library, or the negative value returned by one
 *              of the operators.
 *
 *-------------------------------------------------------------------------
 */

herr_t get_name_type( hid_t loc_id, const char *group_name, int idx, char **name, int *type ) 
{

 trav_info_t info;

 if (H5Giterate( loc_id, group_name, &idx, opget_info, (void *)&info) < 0 )
  return -1;
 
 *name = info.name;
 *type = info.type;

 return 0;
}

/*-------------------------------------------------------------------------
 * Function: traverse
 *
 * Purpose: recursive function that searches HDF5 objects in LOC_ID
 *
 * Return: number of objects found in LOC_ID
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 *-------------------------------------------------------------------------
 */

int traverse( hid_t loc_id, 
              const char *group_name, 
              trav_table_t *table, 
              trav_info_t *info, 
              int *idx ) 
{
 
 char          *name=NULL;
 H5G_obj_t     type;
 int           nobjs;
 int           i;
 char          *path=NULL;
 H5G_stat_t    statbuf;
 int           inserted_objs=0;
 int           j;

 if (( nobjs = get_nobjects( loc_id, group_name )) < 0 )
  return -1;
 
 for ( i = 0; i < nobjs; i++) 
 {
 
  if (get_name_type( loc_id, group_name, i, &name, &type ) < 0 )
   return -1;
 
  /* allocate path buffer */
  path = (char*) HDmalloc(strlen(group_name) + strlen(name) + 2);
  
  /* initialize path */
  strcpy( path, group_name );
  if ( strcmp(group_name,"/")!=0 )
   strcat( path, "/" );
  strcat( path, name ); 

  /* disable error reporting */
  H5E_BEGIN_TRY {

  /* get info */
  H5Gget_objinfo( loc_id, path, 1, &statbuf);
  } H5E_END_TRY;

  /* add to array */
  if ( info )
  {
   info[*idx].name = (char *)HDstrdup(path);
   info[*idx].type = type;
   (*idx)++;
  }
 
  
  switch ( type ) 
  {

  /*-------------------------------------------------------------------------
   * H5G_GROUP
   *-------------------------------------------------------------------------
   */
   
  case H5G_GROUP:

    /* increment */
   inserted_objs++;

   /* nlink is number of hard links to object */
   if (statbuf.nlink > 0  && trav_table_search(statbuf.objno, table ) == -1)
   {
    /* add object to table */
    trav_table_add(statbuf.objno, path, H5G_GROUP, table );
    
    /* recurse with the absolute name */
    inserted_objs += traverse( loc_id, path, table, info, idx );
   }

    /* search table
       group with more than one link to it */
   if (statbuf.nlink > 1) 
   {
    if ((j = trav_table_search(statbuf.objno, table )) < 0 )
     return -1;

    if ( table->objs[j].displayed == 0 )
    {
     table->objs[j].displayed = 1;
    }
    else
    {
#if defined (H5_TRAV_DEBUG)
     printf("%s %s\n", "HARDLINK", table->objs[j].objname);
#endif
    }

   }
   
   break;

  /*-------------------------------------------------------------------------
   * H5G_DATASET
   *-------------------------------------------------------------------------
   */
    
  case H5G_DATASET:

    /* increment */
   inserted_objs++;

   /* nlink is number of hard links to object */
   if (statbuf.nlink > 0  && trav_table_search(statbuf.objno, table ) == -1)
   {
    /* add object to table */
    trav_table_add(statbuf.objno, path, H5G_DATASET, table );
   }

    /* search table
       dataset with more than one link to it */
   if (statbuf.nlink > 1) 
   {
    if ((j = trav_table_search(statbuf.objno, table )) < 0 )
     return -1;

    if ( table->objs[j].displayed == 0 )
    {
     table->objs[j].displayed = 1;
    }
    else
    {
#if defined (H5_TRAV_DEBUG)
     printf("%s %s\n", "HARDLINK", table->objs[j].objname);
#endif
    }

   }
  
   
   break;

  /*-------------------------------------------------------------------------
   * H5G_TYPE
   *-------------------------------------------------------------------------
   */
 
  case H5G_TYPE:

   /* increment */
   inserted_objs++;

   /* nlink is number of hard links to object */
   if (statbuf.nlink > 0  && trav_table_search(statbuf.objno, table ) == -1)
   {
    /* add object to table */
    trav_table_add(statbuf.objno, path, H5G_TYPE, table );
   }
   
   break;


  /*-------------------------------------------------------------------------
   * H5G_LINK
   *-------------------------------------------------------------------------
   */
 
  case H5G_LINK:

   /* increment */
   inserted_objs++;
   
   break;

    
  default:
   break;
 
  } 

  /*-------------------------------------------------------------------------
   * end switch
   *-------------------------------------------------------------------------
   */
  
  if ( name )
   HDfree( name );
  
  if ( path )
   HDfree( path );
  
 } /* i */
 
 return inserted_objs;
}

