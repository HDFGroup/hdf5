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

/*-------------------------------------------------------------------------
 * Function: trav_table_search
 *
 * Purpose: Search in the table for OBJNO
 *
 * Return: index of object in table
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 *-------------------------------------------------------------------------
 */

int trav_table_search(haddr_t objno, trav_table_t *table )
{
 int i;
 
 for (i = 0; i < table->nobjs; i++)
  if (table->objs[i].objno == objno)
   return i;
  
  return -1;
}


/*-------------------------------------------------------------------------
 * Function: trav_table_add
 *
 * Purpose: Add OBJNO, NAME and TYPE of object to table
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 *-------------------------------------------------------------------------
 */

void
trav_table_add(haddr_t objno, char *objname, int type, trav_table_t *table)
{
 int i;
 
 if (table->nobjs == table->size) {
  table->size *= 2;
  table->objs = 
   (trav_obj_t*)HDrealloc(table->objs, table->size * sizeof(trav_obj_t));
  
  for (i = table->nobjs; i < table->size; i++) {
   table->objs[i].objno = 0;
   table->objs[i].flags[0] = table->objs[i].flags[1] = 0;
   table->objs[i].displayed = 0;
   table->objs[i].type = H5G_UNKNOWN;
   table->objs[i].objname = NULL;
  }
 }
 
 i = table->nobjs++;
 table->objs[i].objno = objno;
 table->objs[i].flags[0] = table->objs[i].flags[1] = 0;
 HDfree(table->objs[i].objname);
 table->objs[i].objname = (char *)HDstrdup(objname);
 table->objs[i].type = type;
}


/*-------------------------------------------------------------------------
 * Function: trav_table_addflags
 *
 * Purpose: Add FLAGS, NAME and TYPE of object to table
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 *-------------------------------------------------------------------------
 */

void
trav_table_addflags(unsigned *flags, char *objname, int type, trav_table_t *table)
{
 int i;
 
 if (table->nobjs == table->size) {
  table->size *= 2;
  table->objs = 
   (trav_obj_t*)HDrealloc(table->objs, table->size * sizeof(trav_obj_t));
  
  for (i = table->nobjs; i < table->size; i++) {
   table->objs[i].objno = 0;
   table->objs[i].flags[0] = table->objs[i].flags[1] = 0;
   table->objs[i].displayed = 0;
   table->objs[i].type = H5G_UNKNOWN;
   table->objs[i].objname = NULL;
  }
 }
 
 i = table->nobjs++;
 table->objs[i].objno = 0;
 table->objs[i].flags[0] = flags[0];
 table->objs[i].flags[1] = flags[1];
 HDfree(table->objs[i].objname);
 table->objs[i].objname = (char *)HDstrdup(objname);
 table->objs[i].type = type;
}


/*-------------------------------------------------------------------------
 * Function: trav_table_init
 *
 * Purpose: Initialize the table 
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 *-------------------------------------------------------------------------
 */

void trav_table_init( trav_table_t **tbl )
{
 int i;
 trav_table_t* table = (trav_table_t*) HDmalloc(sizeof(trav_table_t));
 
 table->size = 20;
 table->nobjs = 0;
 table->objs = 
  (trav_obj_t*)HDmalloc(table->size * sizeof(trav_obj_t));
 
 for (i = 0; i < table->size; i++) {
  table->objs[i].objno = 0;
  table->objs[i].flags[0] = table->objs[i].flags[1] = 0;
  table->objs[i].displayed = 0;
  table->objs[i].type = H5G_UNKNOWN;
  table->objs[i].objname = NULL;
 }
 
 *tbl = table;
}



/*-------------------------------------------------------------------------
 * Function: trav_table_free
 *
 * Purpose: free table memory
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 *-------------------------------------------------------------------------
 */

void trav_table_free( trav_table_t *table )
{
 int i;

 for ( i = 0; i < table->nobjs; i++)
  HDfree( table->objs[i].objname );

 HDfree(table->objs);
 HDfree(table);

}


