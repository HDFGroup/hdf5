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


#include "h5diff.h"
#include <stdlib.h>
#include <assert.h>



/*-------------------------------------------------------------------------
 * Function: h5diff
 *
 * Purpose: public function, can be called in an applicattion program.
 *   return differences between 2 HDF5 files
 *
 * Return: An  exit status of 0 means no differences were found, 1 means some 
 *   differences were found.
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 22, 2003
 *
 *-------------------------------------------------------------------------
 */

int  h5diff(const char *fname1, 
            const char *fname2, 
            const char *objname1, 
            const char *objname2, 
            diff_opt_t options)
{
 int          nobjects1, nobjects2;
 trav_info_t  *info1=NULL;
 trav_info_t  *info2=NULL;
 hid_t        file1_id, file2_id; 
 int          nfound;


/*-------------------------------------------------------------------------
 * open the files first; if they are not valid, no point in continuing
 *-------------------------------------------------------------------------
 */

 /* disable error reporting */
 H5E_BEGIN_TRY {
 
 /* Open the files */
 if ((file1_id=H5Fopen(fname1,H5F_ACC_RDONLY,H5P_DEFAULT))<0 )
 {
  printf("h5diff: %s: No such file or directory\n", fname1 );
  exit(1);
 }
 if ((file2_id=H5Fopen(fname2,H5F_ACC_RDONLY,H5P_DEFAULT))<0 )
 {
  printf("h5diff: %s: No such file or directory\n", fname2 );
  exit(1);
 }
 /* enable error reporting */
 } H5E_END_TRY;


/*-------------------------------------------------------------------------
 * get the number of objects in the files
 *-------------------------------------------------------------------------
 */

 nobjects1 = h5trav_getinfo( file1_id, NULL );
 nobjects2 = h5trav_getinfo( file2_id, NULL );

/*-------------------------------------------------------------------------
 * get the list of objects in the files
 *-------------------------------------------------------------------------
 */

 info1 = (trav_info_t*) malloc( nobjects1 * sizeof(trav_info_t));
 info2 = (trav_info_t*) malloc( nobjects2 * sizeof(trav_info_t));
 if (info1==NULL || info2==NULL)
  return 0;

 h5trav_getinfo( file1_id, info1 );
 h5trav_getinfo( file2_id, info2 );

/*-------------------------------------------------------------------------
 * object name was supplied
 *-------------------------------------------------------------------------
 */
 
 if ( objname1 )
 {
  assert(objname2);
  nfound=diff_compare(file1_id,fname1,objname1,nobjects1,info1,
                      file2_id,fname2,objname2,nobjects2,info2,options);
 }

/*-------------------------------------------------------------------------
 * compare all
 *-------------------------------------------------------------------------
 */

 else 
 {
  nfound=diff_match(file1_id,nobjects1,info1,
                    file2_id,nobjects2,info2,options);
 }
 
 
 h5trav_freeinfo(info1,nobjects1);
 h5trav_freeinfo(info2,nobjects2);
 /* close */
 assert( (H5Fclose(file1_id)) >=0);
 assert( (H5Fclose(file2_id)) >=0);

 return nfound;
}



/*-------------------------------------------------------------------------
 * Function: diff_match
 *
 * Purpose: Find common objects; the algorithm used for this search is the 
 *  cosequential match algorithm and is described in 
 *  Folk, Michael; Zoellick, Bill. (1992). File Structures. Addison-Wesley.
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */
int diff_match( hid_t file1_id, 
                int nobjects1, 
                trav_info_t *info1,
                hid_t file2_id, 
                int nobjects2, 
                trav_info_t *info2, 
                diff_opt_t options )
{
 int           more_names_exist = (nobjects1>0 && nobjects2>0) ? 1 : 0;
 trav_table_t  *table=NULL;
 int           cmp;
 int           curr1=0;
 int           curr2=0;
 unsigned      infile[2]; 
 char          c1, c2;
 int           nfound=0, i;

/*-------------------------------------------------------------------------
 * build the list
 *-------------------------------------------------------------------------
 */
 trav_table_init( &table );

 
 while ( more_names_exist )
 {
  /* criteria is string compare */
  cmp = strcmp( info1[curr1].name, info2[curr2].name );
  if ( cmp == 0 )
  {
   infile[0]=1; infile[1]=1;
   trav_table_addflags(infile, info1[curr1].name, info1[curr1].type, table );

   curr1++;
   curr2++;
  }
  else if ( cmp < 0 )
  {
   infile[0]=1; infile[1]=0;
   trav_table_addflags(infile, info1[curr1].name, info1[curr1].type, table );
   curr1++;
  }
  else 
  {
   infile[0]=0; infile[1]=1;
   trav_table_addflags(infile, info2[curr2].name, info2[curr2].type, table );
   curr2++;
  }

  more_names_exist = (curr1<nobjects1 && curr2<nobjects2) ? 1 : 0;

 
 } /* end while */

 /* list1 did not end */
 if (curr1<nobjects1)
 {
  while ( curr1<nobjects1 )
  {
   infile[0]=1; infile[1]=0;
   trav_table_addflags(infile, info1[curr1].name, info1[curr1].type, table );
   curr1++;
  }
 }

 /* list2 did not end */
 if (curr2<nobjects2)
 {
  while ( curr2<nobjects2 )
  {
   infile[0]=0; infile[1]=1;
   trav_table_addflags(infile, info2[curr2].name, info2[curr2].type, table );
   curr2++;
  }
 }

/*-------------------------------------------------------------------------
 * print the list
 *-------------------------------------------------------------------------
 */

 if (options.verbose)
 {
  printf("\n");
  printf("file1     file2\n");
  printf("---------------------------------------\n");
  for (i = 0; i < table->nobjs; i++)
  {
   c1 = (table->objs[i].flags[0]) ? 'x' : ' ';
   c2 = (table->objs[i].flags[1]) ? 'x' : ' ';
   printf("%5c %6c    %-15s\n", c1, c2, table->objs[i].objname);
  }
  printf("\n");
 }
  

/*-------------------------------------------------------------------------
 * do the diff for common objects
 *-------------------------------------------------------------------------
 */

 for (i = 0; i < table->nobjs; i++)
 {
  if ( table->objs[i].flags[0] && table->objs[i].flags[1] )
   nfound+=diff( file1_id, table->objs[i].objname, 
                 file2_id, table->objs[i].objname, 
                 options, table->objs[i].type );
 }

 /* free table */
 trav_table_free(table);
 return nfound;
}


/*-------------------------------------------------------------------------
 * Function: diff_compare
 *
 * Purpose: get objects from list, and check for the same type
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */

int diff_compare( hid_t file1_id, 
                  const char *file1_name, 
                  const char *obj1_name, 
                  int nobjects1, 
                  trav_info_t *info1,
                  hid_t file2_id, 
                  const char *file2_name, 
                  const char *obj2_name, 
                  int nobjects2, 
                  trav_info_t *info2,
                  diff_opt_t options )
{

 int f1=0, f2=0;
 int nfound=0;

 int i = h5trav_getindex( obj1_name, nobjects1, info1 );
 int j = h5trav_getindex( obj2_name, nobjects2, info2 );

 if ( i == -1 )
 {
  printf( "Object <%s> could not be found in <%s>\n", obj1_name, file1_name );
  f1=1;
 }
 if ( j == -1 )
 {
  printf( "Object <%s> could not be found in <%s>\n", obj2_name, file2_name );
  f2=1;
 }
 if ( f1 || f2 )
  return -1;

  /* use the name with "/" first, as obtained by iterator function */
 obj1_name=info1[i].name;
 obj2_name=info2[j].name;

 /* objects are not the same type */
 if ( info1[i].type != info2[j].type )
 {
  printf("Comparison not supported\n");
  printf("<%s> is of type %s and <%s> is of type %s\n", 
   obj1_name, get_type(info1[i].type), 
   obj2_name, get_type(info2[j].type) );
  return 0;
 }
  
 nfound=diff( file1_id, obj1_name, file2_id, obj2_name, options, info1[i].type );

 return nfound;
}


/*-------------------------------------------------------------------------
 * Function: diff
 *
 * Purpose: switch between types and choose the diff function
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int diff( hid_t file1_id, 
          const char *obj1_name, 
          hid_t file2_id, 
          const char *obj2_name, 
          diff_opt_t options, 
          int type )
{
 int nfound=0;

 switch ( type )
 {
 case H5G_DATASET:
  nfound=diff_dataset(file1_id,file2_id,obj1_name,obj2_name,options);
  break;
  
 default:
  printf("Comparison not supported\n");
  printf("<%s> is of type %s and <%s> is of type %s\n", 
   obj1_name, get_type(type), 
   obj2_name, get_type(type) );
  break;
 } 
 
 if (options.verbose) 
  printf("\n");
 return nfound;
}


/*-------------------------------------------------------------------------
 * Function: diff_list
 *
 * Purpose: print list of objects in file
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef NOT_YET
void diff_list( const char *filename, int nobjects, trav_info_t *info )
{
 int i;

 printf("File <%s>: # of entries = %d\n", filename, nobjects );
 for ( i = 0; i < nobjects; i++)
 {
  switch ( info[i].type )
  {
  case H5G_GROUP:
   printf("%s %20s\n", info[i].name, "group" );
   break;
  case H5G_DATASET:
   printf("%s %20s\n", info[i].name, "dataset" );
   break;
  case H5G_TYPE:
   printf("%s %20s\n", info[i].name, "datatype" );
   break;
  case H5G_LINK:
   printf("%s %20s\n", info[i].name, "link" );
   break;
  default:
   printf("%s %20s\n", info[i].name, "User defined object" );
   break;
  }
 }

}
#endif 





