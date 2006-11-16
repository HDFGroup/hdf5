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
 unsigned int i;

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

void trav_table_add(haddr_t objno,
                    char *name,
                    H5G_obj_t type,
                    trav_table_t *table)
{
 unsigned int i;

 if (table->nobjs == table->size) {
  table->size *= 2;
  table->objs =
   (trav_obj_t*)HDrealloc(table->objs, table->size * sizeof(trav_obj_t));

  for (i = table->nobjs; i < table->size; i++) {
   table->objs[i].objno = 0;
   table->objs[i].flags[0] = table->objs[i].flags[1] = 0;
   table->objs[i].displayed = 0;
   table->objs[i].type = H5G_UNKNOWN;
   table->objs[i].name = NULL;
   table->objs[i].links = NULL;
   table->objs[i].nlinks = 0;
   table->objs[i].sizelinks = 0;
  }
 }

 i = table->nobjs++;
 table->objs[i].objno = objno;
 table->objs[i].flags[0] = table->objs[i].flags[1] = 0;
 HDfree(table->objs[i].name);
 table->objs[i].name = (char *)HDstrdup(name);
 table->objs[i].type = type;
 table->objs[i].links = NULL;
 table->objs[i].nlinks = 0;
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

void trav_table_addflags(unsigned *flags,
                         char *name,
                         H5G_obj_t type,
                         trav_table_t *table)
{
 unsigned int i;

 if (table->nobjs == table->size) {
  table->size *= 2;
  table->objs =
   (trav_obj_t*)HDrealloc(table->objs, table->size * sizeof(trav_obj_t));

  for (i = table->nobjs; i < table->size; i++) {
   table->objs[i].objno = 0;
   table->objs[i].flags[0] = table->objs[i].flags[1] = 0;
   table->objs[i].displayed = 0;
   table->objs[i].type = H5G_UNKNOWN;
   table->objs[i].name = NULL;
   table->objs[i].links = NULL;
   table->objs[i].nlinks = 0;
   table->objs[i].sizelinks = 0;
  }
 }

 i = table->nobjs++;
 table->objs[i].objno = 0;
 table->objs[i].flags[0] = flags[0];
 table->objs[i].flags[1] = flags[1];
 HDfree(table->objs[i].name);
 table->objs[i].name = (char *)HDstrdup(name);
 table->objs[i].type = type;
 table->objs[i].links = NULL;
 table->objs[i].nlinks = 0;
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
 unsigned int i;
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
  table->objs[i].name = NULL;
  table->objs[i].links = NULL;
  table->objs[i].nlinks = 0;
  table->objs[i].sizelinks = 0;
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
 unsigned int i, j;

 for ( i = 0; i < table->nobjs; i++)
 {
  HDfree( table->objs[i].name );
  if (table->objs[i].nlinks)
  {
   for ( j=0; j<table->objs[i].nlinks; j++)
    HDfree( table->objs[i].links[j].new_name );

   HDfree(table->objs[i].links);
  }
 }
 HDfree(table->objs);
 HDfree(table);

}


/*-------------------------------------------------------------------------
 * Function: trav_table_addlink
 *
 * Purpose: Add a hardlink name to the object
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 17, 2003
 *
 *-------------------------------------------------------------------------
 */

void trav_table_addlink(trav_table_t *table,
                        int j /* the object index */,
                        char *path )
{
 unsigned int k;

 /* already inserted */
 if (strcmp(table->objs[j].name,path)==0)
  return;

 /* allocate space if necessary */
 if (table->objs[j].nlinks == (unsigned)table->objs[j].sizelinks) {
  table->objs[j].sizelinks += 2;
  table->objs[j].links =
   (trav_link_t*)HDrealloc(table->objs[j].links,
   table->objs[j].sizelinks * sizeof(trav_link_t));
 }

 /* insert it */
 k=table->objs[j].nlinks++;
 table->objs[j].links[k].new_name = (char*)HDstrdup(path);
}


