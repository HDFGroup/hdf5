
#include <stdio.h>
#include <stdlib.h>

#include "hdf5.h"
#include "h5trav.h"



#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

#ifndef FAIL
#define FAIL -1
#endif



/* functions for traversal */
int traverse( hid_t loc_id, const char *group_name, table_t *table, info_t *info, int *idx );
herr_t get_nobjects( hid_t loc_id, const char *group_name );
herr_t get_name_type( hid_t loc_id, const char *group_name, int idx, char **name, int *type );

/* table methods */
void table_init( table_t **table );
void table_free( table_t *table );
int  table_search(unsigned long *objno, table_t *table );
void table_add(unsigned long *objno, char *objname, table_t *table );




/*-------------------------------------------------------------------------
 * Function: H5get_object_info
 *
 * Purpose: 
 *
 * Return: Success: 0, Failure: -11
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 6, 2002
 *
 * Comments: 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int H5get_object_info( hid_t file_id, info_t *info )
{

 table_t  *table=NULL;
 int      nobjects=0;

 /* init table */
 table_init( &table );

 /* iterate starting on the root group */
 if (( nobjects = traverse( file_id, "/", table, info, &nobjects )) < 0 )
  return -1;

 /* free table */
 table_free( table );

 return nobjects;

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

 if (H5Gget_objinfo( loc_id, name, FALSE, &statbuf) < 0 )
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

 if (H5Gget_objinfo( loc_id, name, FALSE, &statbuf) < 0 )
  return 1;

 ((info_t *)op_data)->type = statbuf.type;
 ((info_t *)op_data)->name = (char *)strdup(name);

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

 info_t info;

 if (H5Giterate( loc_id, group_name, &idx, opget_info, (void *)&info) < 0 )
  return -1;
 
 *name = info.name;
 *type = info.type;

 return 0;
}







/*-------------------------------------------------------------------------
 * Function: traverse
 *
 * Purpose: 
 *
 * Return: Success: 0, Failure: -11
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 * Comments: 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */



int traverse( hid_t loc_id, const char *group_name, table_t *table, info_t *info, int *idx ) 
{
 
 char          *name=NULL;
 int           type;
 int           nobjs;
 int           i;
 char          *path=NULL;
 H5G_stat_t    statbuf;
 int           inserted_objs=0;
 int           j;
 void          *edata;
 hid_t         (*func)(void*);
 
 if (( nobjs = get_nobjects( loc_id, group_name )) < 0 )
  return -1;
 
 for ( i = 0; i < nobjs; i++) 
 {
 
  if (get_name_type( loc_id, group_name, i, &name, &type ) < 0 )
   return -1;
 
  /* allocate path buffer */
  path = (char*) malloc(strlen(group_name) + strlen(name) + 2);
  
  /* initialize path */
  strcpy( path, group_name );
  if ( strcmp(group_name,"/")!=0 )
   strcat( path, "/" );
  strcat( path, name ); 

  

  /* disable error reporting */
  H5Eget_auto(&func, &edata);
  H5Eset_auto(NULL, NULL);

  /* get info */
  H5Gget_objinfo( loc_id, path, TRUE, &statbuf);
  H5Eset_auto(func, edata);

  /* add to array */
  if ( info )
  {
   info[*idx].name = strdup(path);
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
   if (statbuf.nlink > 0  && table_search(statbuf.objno, table ) == FAIL)
   {
    /* add object to table */
    table_add(statbuf.objno, path, table );
    
    /* recurse with the absolute name */
    inserted_objs += traverse( loc_id, path, table, info, idx );
   }

    /* search table
       group with more than one link to it */
   if (statbuf.nlink > 1) 
   {
    if ((j = table_search(statbuf.objno, table )) < 0 )
     return -1;

    if ( table->objs[j].displayed == 0 )
    {
     table->objs[j].displayed = 1;
    }
    else
    {
     printf("%s %s\n", "HARDLINK", table->objs[j].objname);
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
   if (statbuf.nlink > 0  && table_search(statbuf.objno, table ) == FAIL)
   {
    /* add object to table */
    table_add(statbuf.objno, path, table );

   }

    /* search table
       dataset with more than one link to it */
   if (statbuf.nlink > 1) 
   {
    if ((j = table_search(statbuf.objno, table )) < 0 )
     return -1;

    if ( table->objs[j].displayed == 0 )
    {
     table->objs[j].displayed = 1;
    }
    else
    {
     printf("%s %s\n", "HARDLINK", table->objs[j].objname);
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
   if (statbuf.nlink > 0  && table_search(statbuf.objno, table ) == FAIL)
   {
    /* add object to table */
    table_add(statbuf.objno, path, table );

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
   free( name );
  
  if ( path )
   free( path );
  
 } /* i */
 
 
 
 return inserted_objs;
}



/*-------------------------------------------------------------------------
 * Function: table_search
 *
 * Purpose: 
 *
 * Return: Success: 0, Failure: -11
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 * Comments: 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int table_search(unsigned long *objno, table_t *table )
{
 int i;
 
 for (i = 0; i < table->nobjs; i++)
  if (table->objs[i].objno[0] == *objno && table->objs[i].objno[1] == *(objno + 1))
   return i;
  
  return FAIL;
}


/*-------------------------------------------------------------------------
 * Function: table_add
 *
 * Purpose: 
 *
 * Return: Success: 0, Failure: -11
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 * Comments: 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */



void table_add(unsigned long *objno, char *objname, table_t *table)
{
 int i;
 
 if (table->nobjs == table->size) {
  table->size *= 2;
  table->objs = (obj_t*)realloc(table->objs, table->size * sizeof(obj_t));
  
  for (i = table->nobjs; i < table->size; i++) {
   table->objs[i].objno[0] = table->objs[i].objno[1] = 0;
   table->objs[i].displayed = 0;
   table->objs[i].recorded = 0;
   table->objs[i].objname = NULL;
  }
 }
 
 i = table->nobjs++;
 table->objs[i].objno[0] = objno[0];
 table->objs[i].objno[1] = objno[1];
 free(table->objs[i].objname);
 table->objs[i].objname = strdup(objname);


}


/*-------------------------------------------------------------------------
 * Function: table_init
 *
 * Purpose: 
 *
 * Return: Success: 0, Failure: -11
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 * Comments: 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void table_init( table_t **tbl )
{
 int i;
 table_t* table = (table_t*) malloc(sizeof(table_t));
 
 table->size = 20;
 table->nobjs = 0;
 table->objs = (obj_t*) malloc(table->size * sizeof(obj_t));
 
 for (i = 0; i < table->size; i++) {
  table->objs[i].objno[0] = table->objs[i].objno[1] = 0;
  table->objs[i].displayed = 0;
  table->objs[i].recorded = 0;
  table->objs[i].objname = NULL;
 }
 
 *tbl = table;
}



/*-------------------------------------------------------------------------
 * Function: table_free
 *
 * Purpose: 
 *
 * Return: Success: 0, Failure: -11
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 * Comments: 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void table_free( table_t *table )
{

 int i;

 
#if 0
 printf("Table: # of entries = %d\n", table->nobjs);
 for ( i = 0; i < table->nobjs; i++)
  printf("%lu %lu %s %d %d\n", table->objs[i].objno[0],
  table->objs[i].objno[1],
  table->objs[i].objname,
  table->objs[i].displayed, table->objs[i].recorded);
#endif

 for ( i = 0; i < table->nobjs; i++)
  free( table->objs[i].objname );

 free(table->objs);
 free(table);

}
