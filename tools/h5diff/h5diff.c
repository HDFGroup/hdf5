
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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "hdf5.h"
#include "h5trav.h"

#if 0
#define H5DIFF_DEBUG
#endif


#define FFORMAT "%-15g %-15g %-15g\n"
#define IFORMAT "%-15d %-15d %-15d\n"
#define LIFORMAT "%-15ld %-15ld %-15ld\n"
#define SPACES  "          "


typedef struct options_t
{
 int    r_; /* report only what objects differ */
 int    d_; /* delta difference */
 double d_delta; /* delta value */
 int    p_; /* relative error */
 double p_relative; /* relative error value */
 int    n_; /* count */
 int    n_number_count; /* value */
 int    m_; /* do not make the diff on a sequencial match, default yes */
} options_t;

/*-------------------------------------------------------------------------
 * prototypes
 *-------------------------------------------------------------------------
 */

int diff_dataset( hid_t file1_id, hid_t file2_id, const char *obj1_name, 
                  const char *obj2_name, options_t options );
void print_class( H5T_class_t tclass, char *sclass );
void list( const char *filename, int nobjects, info_t *info );
void diff( hid_t file1_id, const char *obj1_name, hid_t file2_id, const char *obj2_name, 
           options_t options, int type );
void compare( hid_t file1_id, const char *file1_name, const char *obj1_name, 
              int nobjects1, info_t *info1,
              hid_t file2_id, const char *file2_name, const char *obj2_name, 
              int nobjects2, info_t *info2,
              options_t options );
void match( hid_t file1_id, const char *file1_name, int nobjects1, info_t *info1,
            hid_t file2_id, const char *file2_name, int nobjects2, info_t *info2,
            options_t options );
void print_pos( int *ph, unsigned int curr_pos, int *acc, 
                int *pos, int rank, const char *obj1, const char *obj2 );
int array_diff( void *buf1, void *buf2, hsize_t tot_cnt, int rank, hsize_t *dims, 
                options_t options, const char *obj1, const char *obj2,
                hid_t m_type );

/*-------------------------------------------------------------------------
 * utility functions
 *-------------------------------------------------------------------------
 */

hid_t fixtype( hid_t f_type );
void print_datatype(hid_t type);
int check_n_input( const char* );
int check_f_input( const char* );
int get_index( const char *obj, int nobjects, info_t *info );
int compare_object( char *obj1, char *obj2 );
void usage(void);
void leave(void);


/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: print a usage message  
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */
void usage(void)
{
 printf("Usage: h5diff [obj1_name] [obj2_name] [OPTIONS] file1_name file2_name\n");
 printf("Items in [ ] are optional\n");
 printf("[obj1_name]       Name of an HDF5 object\n");
 printf("[obj2_name]       Name of an HDF5 object\n");
 printf("file1_name        File name of the first HDF5 file\n");
 printf("file2_name        File name of the second HDF5 file\n");
 printf("[OPTIONS] are:\n");
 printf("[-h ]             Print out this information\n");
 printf("[-r ]             Print only what objects differ\n");
 printf("[-n count]        Print difference up to count number for each variable\n");
 printf("[-d delta]        Print difference when it is greater than limit delta\n");
 printf("[-p relative]     Print differences which are within a relative error value\n");
 printf("[-m ]             Print differences on a sequential match iteration\n");
}


/*-------------------------------------------------------------------------
 * Function: leave
 *
 * Purpose: exit and print newline 
 *
 *-------------------------------------------------------------------------
 */
void leave(void)
{
 exit(EXIT_SUCCESS);
 printf("\n");
}


/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: H5diff main program
 *
 * Return: 
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

int main(int argc, const char *argv[])
{
 int        i;
 const char *s = NULL;
 hid_t      file1_id, file2_id; 
 int        nobjects1, nobjects2;
 info_t     *info1=NULL;
 info_t     *info2=NULL;
 options_t  options = {0,0,0,0,0,0,0,0};
 void       *edata;
 hid_t      (*func)(void*);

 const char *file1_name;
 const char *file2_name;
 const char *obj1_name  = NULL;
 const char *obj2_name  = NULL;
 
/*-------------------------------------------------------------------------
 * print the command line options
 *-------------------------------------------------------------------------
 */

 printf("$h5diff");
 for (i=1; i<argc ; i++) 
 {
  printf(" %s", argv[i] );
 }
 printf("\n");

 
/*-------------------------------------------------------------------------
 * parse command line options
 *-------------------------------------------------------------------------
 */
 
 if (argc < 3) {
  printf("Number of arguments is only %d\n", argc );
  usage();
  leave();
 }
 
 /* last 2 items are the file names */
 for (i=1; i<argc ; i++) 
 {
  
  /* get the single-letter switches */
  if ( '-'==argv[i][0] )
  {
   
   for (s=argv[i]+1; *s; s++) 
   {
    switch (*s) {
    default:
    printf("-%s is an invalid option\n", s );
    usage();
    leave();
    break;
    case 'h': 
     usage();
     leave();
    case 'r': 
     options.r_ = 1;
     break;
    case 'm': 
     options.m_ = 1;
     break;
    case 'd': 
     /* if it is not another option */
     if ( '-' !=argv[i+1][0] )
     {
      options.d_      = 1;

      if ( check_f_input(argv[i+1])==-1)
      {
       printf("<-d %s> is not a valid option\n", argv[i+1] );
       usage();
       leave();
      }

      options.d_delta = atof(argv[i+1]);
     }
     else
     {
      printf("<-d %s> is not a valid option\n", argv[i+1] );
      usage();
      leave();
     }
     break;
    case 'p': 
     if ( '-' !=argv[i+1][0] )
     {
      options.p_         = 1;

      if ( check_f_input(argv[i+1])==-1)
      {
       printf("<-p %s> is not a valid option\n", argv[i+1] );
       usage();
       leave();
      }

      options.p_relative  = atof(argv[i+1]);
     }
     break;
    case 'n': 
     if ( '-' !=argv[i+1][0] )
     {
      options.n_             = 1;

      if ( check_n_input(argv[i+1])==-1)
      {
       printf("<-n %s> is not a valid option\n", argv[i+1] );
       usage();
       leave();
      }
      options.n_number_count = atoi(argv[i+1]);
     }
     break;
    } /*switch*/
   } /*for*/ 
  } /*if*/
  
  else
   
  {
   
   /* 2 last args are the file names, and it is not a -switch parameter */
   if ( i < argc-2 && '-' !=argv[i-1][0] )
   {
    if ( obj1_name == NULL )
     obj1_name = argv[i];

    if ( obj2_name == NULL )
    {
     
     /* check if we have a second object name */
     if ( i+1 < argc-2 && '-' !=argv[i+1][0] )
      /* yes */
      obj2_name = argv[i+1];
     else
      /* no */
      obj2_name = obj1_name;
    }
   }
   
  }
  
 }/*for*/
 

/*-------------------------------------------------------------------------
 * process the files
 *-------------------------------------------------------------------------
 */
 
 file1_name = argv[argc-2];
 file2_name = argv[argc-1];

 /* disable error reporting */
 H5Eget_auto(&func, &edata);
 H5Eset_auto(NULL, NULL);

 
 /* Open the files */
 if ((file1_id=H5Fopen(file1_name,H5F_ACC_RDONLY,H5P_DEFAULT))<0 )
 {
  printf("h5diff: %s: No such file or directory\n", file1_name );
  leave();
 }

 if ((file2_id=H5Fopen(file2_name,H5F_ACC_RDONLY,H5P_DEFAULT))<0 )
 {
  printf("h5diff: %s: No such file or directory\n", file2_name );
  leave();
 }

 /* enable error reporting */
 H5Eset_auto(func, edata);


/*-------------------------------------------------------------------------
 * get the number of objects in the files
 *-------------------------------------------------------------------------
 */

 nobjects1 = H5get_object_info( file1_id, NULL );
 nobjects2 = H5get_object_info( file2_id, NULL );

/*-------------------------------------------------------------------------
 * get the list of objects in the files
 *-------------------------------------------------------------------------
 */

 info1 = (info_t*) malloc( nobjects1 * sizeof(info_t));
 info2 = (info_t*) malloc( nobjects2 * sizeof(info_t));

 H5get_object_info( file1_id, info1 );
 H5get_object_info( file2_id, info2 );

/*-------------------------------------------------------------------------
 * object name was supplied
 *-------------------------------------------------------------------------
 */
 
 if ( obj1_name )
 {
  compare(file1_id,file1_name,obj1_name,nobjects1,info1,
          file2_id,file2_name,obj2_name,nobjects2,info2,options);
 }

/*-------------------------------------------------------------------------
 * compare all
 *-------------------------------------------------------------------------
 */

 else 
 {
  match(file1_id,file1_name,nobjects1,info1,
        file2_id,file2_name,nobjects2,info2,options);
 }
 
 /* close */
 assert( (H5Fclose(file1_id)) >=0);
 assert( (H5Fclose(file2_id)) >=0);
 
 if ( info1 )
  free(info1);
 if ( info2 )
  free(info2);
 printf("\n");
 return 0;
 
}


/*-------------------------------------------------------------------------
 * Function: check_n_input
 *
 * Purpose: check for valid input
 *
 * Return: 
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

int check_n_input( const char *str )
{
 unsigned i;
 char c;

 for ( i = 0; i < strlen(str); i++)
 {
  c = str[i];
  if ( c < 49 || c > 57  ) /* ascii values between 1 and 9 */
   return -1;
 }
 return 1;
}

/*-------------------------------------------------------------------------
 * Function: check_f_input
 *
 * Purpose: check for valid input
 *
 * Return: 
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

int check_f_input( const char *str )
{
 unsigned i;
 char c;

 /* '0' values not allowed */
 if ( strlen(str)==1 && str[0]=='0' )
  return -1;

 for ( i = 0; i < strlen(str); i++)
 {
  c = str[i];
  if ( c < 48 || c > 57  ) /* ascii values between 0 and 9 */
   if  ( c!= 46) /* . */
   return -1;
 }
 return 1;
}

/*-------------------------------------------------------------------------
 * Function: list
 *
 * Purpose: print list of objects in file
 *
 * Return: 
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

void list( const char *filename, int nobjects, info_t *info )
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


/*-------------------------------------------------------------------------
 * Function: get_index
 *
 * Purpose: get index in list
 *
 * Return: 
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

int get_index( const char *obj, int nobjects, info_t *info )
{
 char *pdest;
 int  result;
 int  i;

 for ( i = 0; i < nobjects; i++) 
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
 * Function: compare
 *
 * Purpose: get objects form list, and check for the same type
 *
 * Return: 
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


void compare( hid_t file1_id, const char *file1_name, const char *obj1_name, 
              int nobjects1, info_t *info1,
              hid_t file2_id, const char *file2_name, const char *obj2_name, 
              int nobjects2, info_t *info2,
              options_t options )
{

 int f1=0, f2=0;

 int i = get_index( obj1_name, nobjects1, info1 );
 int j = get_index( obj2_name, nobjects2, info2 );

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
  return;

 /* objects are not the same type */
 if ( info1[i].type != info2[j].type )
 {
  printf( "<%s> in <%s> is of different type than <%s> in <%s>\n", 
   obj1_name, file1_name, obj2_name, file2_name );
  return;
 }
  
 diff( file1_id, obj1_name, file2_id, obj2_name, options, info1[i].type );
 
}


/*-------------------------------------------------------------------------
 * Function: diff
 *
 * Purpose: switch between types and choose the diff function
 *
 * Return: 
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


void diff( hid_t file1_id, const char *obj1_name, hid_t file2_id, const char *obj2_name, 
           options_t options, int type )
{

 switch ( type )
 {
    
 case H5G_DATASET:
  diff_dataset(file1_id,file2_id,obj1_name,obj2_name,options);
  break;

 case H5G_GROUP:
  printf( "<%s> and <%s> are of type H5G_GROUP\n", obj1_name, obj2_name );
  break;
   
 case H5G_TYPE:
  printf( "<%s> and <%s> are of type H5G_TYPE\n", obj1_name, obj2_name ); 
  break;

 case H5G_LINK:
  printf( "<%s> and <%s> are of type H5G_LINK\n", obj1_name, obj2_name ); 
  break;

 default:
  printf( "<%s> and <%s> are user defined types\n", obj1_name, obj2_name ); 
  break;
  
 } /* switch */
  
}

/*-------------------------------------------------------------------------
 * Function: compare_object
 *
 * Purpose: do the compare criteria
 *
 * Return: 
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

int compare_object( char *obj1, char *obj2 )
{
 int cmp;
 cmp = strcmp(obj1,obj2);
 return cmp;

}


/*-------------------------------------------------------------------------
 * Function: match
 *
 * Purpose: Find commom objects; the algorithm used for this search is the 
 *  cosequential match algorithm and is described in 
 *  Folk, Michael; Zoellick, Bill. (1992). File Structures. Addison-Wesley.
 *
 * Return: 
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

void match( hid_t file1_id, const char *file1_name, int nobjects1, info_t *info1,
            hid_t file2_id, const char *file2_name, int nobjects2, info_t *info2,
            options_t options )
{
 int  cmp;
 int  more_names_exist = (nobjects1>0 && nobjects2>0) ? 1 : 0;
 int  curr1=0;
 int  curr2=0;
 
 while ( more_names_exist )
 {
           
  cmp = compare_object( info1[curr1].name, info2[curr2].name );
  if ( cmp == 0 )
  {
   printf( "<%s> found in <%s> and <%s> found in <%s>\n", 
    info1[curr1].name, file1_name, info2[curr2].name, file2_name);

   /* do the diff */
   if ( options.m_ )
   {
    diff( file1_id, info1[curr1].name, file2_id, info1[curr1].name, options, 
          info1[curr1].type );
   }

   curr1++;
   curr2++;
  
  
  }
  else if ( cmp < 0 )
  {
   printf( "<%s> is in <%s>, but not in <%s>\n", info1[curr1].name, 
    file1_name, file2_name);
   curr1++;
  }
  else 
  {
   printf( "<%s> is in <%s>, but not in <%s>\n", info2[curr2].name, 
    file2_name, file1_name);
   curr2++;
  }

  more_names_exist = (curr1<nobjects1 && curr2<nobjects2) ? 1 : 0;

 
 } /* end while */

 /* list1 did not end */
 if (curr1<nobjects1)
 {
  while ( curr1<nobjects1 )
  {
   printf( "<%s> is in <%s>, but not in <%s>\n", info1[curr1].name, 
    file1_name, file2_name);
   curr1++;
  }
 }

 /* list2 did not end */
 if (curr2<nobjects2)
 {
  while ( curr2<nobjects2 )
  {
   printf( "<%s> is in <%s>, but not in <%s>\n", info2[curr2].name, 
    file2_name, file1_name);
   curr2++;
  }
 }


}

/*-------------------------------------------------------------------------
 * Function: diff_dataset
 *
 * Purpose: check for comparable datasets and read into a compatible 
 *  memory type
 *
 * Return: Success: 0, Failure: -1
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

int diff_dataset( hid_t file1_id, hid_t file2_id, const char *obj1_name, 
                  const char *obj2_name, options_t options )
{

 hid_t   dset1_id  =-1;
 hid_t   dset2_id  =-1;
 hid_t   space1_id =-1;
 hid_t   space2_id =-1;
 hid_t   f_type1=-1, f_type2=-1; /* file data type */ 
 hid_t   m_type1=-1, m_type2=-1; /* memory data type */
 size_t  f_size1, f_size2;       /* size of type in file */
 size_t  m_size1, m_size2;       /* size of type in memory */
 int     rank1, rank2; 
 void    *buf1=NULL, *buf2=NULL;
 hsize_t tot_cnt1, tot_cnt2;
 hsize_t dims1[32], dims2[32], maxdim1[32], maxdim2[32];
 H5T_class_t tclass1;
 H5T_class_t tclass2;
 int     i, j;
 char    sclass1[20];
 char    sclass2[20];
 int     nfound;
 void    *edata;
 hid_t   (*func)(void*);

 /* disable error reporting */
 H5Eget_auto(&func, &edata);
 H5Eset_auto(NULL, NULL);

/*-------------------------------------------------------------------------
 * open the handles
 *-------------------------------------------------------------------------
 */

 /* Open the datasets */
 if ( (dset1_id = H5Dopen(file1_id,obj1_name)) < 0 )
 {
  printf("Cannot open dataset <%s>\n", obj1_name );
  goto out;
 }

 if ( (dset2_id = H5Dopen(file2_id,obj2_name)) < 0 )
 {
  printf("Cannot open dataset <%s>\n", obj2_name );
  goto out;
 }

 printf( "Comparing <%s> with <%s>\n", obj1_name, obj2_name );

 /* enable error reporting */
 H5Eset_auto(func, edata);

  /* Get the dataspace handle */
 if ( (space1_id = H5Dget_space(dset1_id)) < 0 )
  goto out;

 /* Get rank */
 if ( (rank1 = H5Sget_simple_extent_ndims(space1_id)) < 0 )
  goto out;

 /* Get the dataspace handle */
 if ( (space2_id = H5Dget_space(dset2_id)) < 0 )
  goto out;

 /* Get rank */
 if ( (rank2 = H5Sget_simple_extent_ndims(space2_id)) < 0 )
  goto out;

 /* Get dimensions */
 if ( H5Sget_simple_extent_dims(space1_id,dims1,maxdim1) < 0 )
  goto out;

 /* Get dimensions */
 if ( H5Sget_simple_extent_dims(space2_id,dims2,maxdim2) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Get the file data type 
 *-------------------------------------------------------------------------
 */
 
 /* Get the data type */
 if ( (f_type1 = H5Dget_type(dset1_id)) < 0 )
  goto out;

 /* Get the data type */
 if ( (f_type2 = H5Dget_type(dset2_id)) < 0 )
  goto out;


/*-------------------------------------------------------------------------
 * check for the same class 
 *-------------------------------------------------------------------------
 */

 if ((tclass1=H5Tget_class(f_type1))<0) 
  goto out;

 if ((tclass2=H5Tget_class(f_type2))<0) 
  goto out;

 if ( tclass1 != tclass2 )
 {
  print_class( tclass1, sclass1 );
  print_class( tclass2, sclass2 );
  printf( "<%s> is of class %s and <%s> is of class %s\n", 
   obj1_name, sclass1, obj2_name, sclass2 );
  goto out;
 }

/*-------------------------------------------------------------------------
 * check for non supported classes
 *-------------------------------------------------------------------------
 */

 switch (tclass1) 
 {
 case H5T_TIME:
  printf( "H5T_TIME comparison is not supported\n");   
  goto out;
 case H5T_STRING:
  printf( "H5T_STRING comparison is not supported\n");   
  goto out;
 case H5T_BITFIELD:
  printf( "H5T_BITFIELD comparison is not supported\n");    
  goto out;
 case H5T_OPAQUE:
  printf( "H5T_OPAQUE comparison is not supported\n");    
  goto out;
 case H5T_COMPOUND:
  printf( "H5T_COMPOUND comparison is not supported\n");      
  goto out;
 case H5T_REFERENCE:
  printf( "H5T_REFERENCE comparison is not supported\n");   
  goto out;
 case H5T_ENUM:
  printf( "H5T_ENUM comparison is not supported\n"); 
 goto out;
 case H5T_VLEN:
  printf( "H5T_VLEN comparison is not supported\n"); 
  goto out;
 case H5T_ARRAY:
  printf( "H5T_ARRAY comparison is not supported\n"); 
  goto out;
 default:
  break;
 }

/*-------------------------------------------------------------------------
 * check for equal datatype
 *-------------------------------------------------------------------------
 */

 if ( (H5Tequal(f_type1, f_type2)==0) ) 
 {
  printf("Warning: <%s> has different storage datatype than <%s>\n", obj1_name, obj2_name );
  printf("<%s> has datatype ", obj1_name);
  print_datatype(f_type1);
  printf(" and <%s> has datatype ", obj2_name);
  print_datatype(f_type2);
  printf("\n");
 }

/*-------------------------------------------------------------------------
 * check for the same rank
 *-------------------------------------------------------------------------
 */
 
 if ( rank1 != rank2 )
 {
  printf( "<%s> is of rank %d and <%s> is of rank %d\n", 
   obj1_name, rank1, obj2_name, rank2 );
  printf( "<%s>: ", obj1_name );
  printf("[ " );  
  for (j = 0; j < rank1; j++) 
   printf("%d ", (int)dims1[j]  );
  printf("]\n" );
  printf( "<%s>: ", obj2_name );
  printf("[ " );  
  for (j = 0; j < rank1; j++) 
   printf("%d ", (int)dims2[j]  );
  printf("]\n" );
  goto out;
 }

/*-------------------------------------------------------------------------
 * check for different maximum dimensions; just give a warning
 *-------------------------------------------------------------------------
 */

 for (i = 0; i < rank1; i++) 
 {
  if ( maxdim1[i] != maxdim2[i] )
  {
   printf( "Warning: <%s> has different maximum dimensions than <%s>\n", obj1_name, obj2_name );
   printf( "<%s>: ", obj1_name );
   printf("[ " );  
   for (j = 0; j < rank1; j++) 
    printf("%d ", (int)maxdim1[j]  );
   printf("]\n" );
   printf( "<%s>: ", obj2_name );
   printf("[ " );  
   for (j = 0; j < rank1; j++) 
    printf("%d ", (int)maxdim2[j]  );
   printf("]\n" );
  }
 }


/*-------------------------------------------------------------------------
 * check for the same dimensionality
 *-------------------------------------------------------------------------
 */

 for (i = 0; i < rank1; i++) 
 {
  if ( dims1[i] != dims2[i] )
  {
   printf( "<%s> has different dimensions than <%s>\n", obj1_name, obj2_name );
   printf( "<%s>: ", obj1_name );
   printf("[ " );  
   for (j = 0; j < rank1; j++) 
    printf("%d ", (int)dims1[j]  );
   printf("]\n" );
   printf( "<%s>: ", obj2_name );
   printf("[ " );  
   for (j = 0; j < rank1; j++) 
    printf("%d ", (int)dims2[j]  );
   printf("]\n" );
   goto out;
  }
 }

 
/*-------------------------------------------------------------------------
 * get number of elements
 *-------------------------------------------------------------------------
 */

 tot_cnt1 = 1;
 for (i = 0; i < rank1; i++) 
 {
  tot_cnt1 *= dims1[i];
 }
 
 tot_cnt2 = 1;
 for (i = 0; i < rank2; i++) 
 {
  tot_cnt2 *= dims2[i];
 }

 assert(tot_cnt1==tot_cnt2);

/*-------------------------------------------------------------------------
 * memory type and sizes
 *-------------------------------------------------------------------------
 */

 m_type1 = fixtype( f_type1 );
 m_type2 = fixtype( f_type2 );

 f_size1 = H5Tget_size( f_type1 );
 f_size2 = H5Tget_size( f_type2 );
 m_size1 = H5Tget_size( m_type1 );
 m_size2 = H5Tget_size( m_type2 );

#if defined (H5DIFF_DEBUG)
 printf("\n");
 printf("------------------\n");
 printf("sizeof(char)   %u\n", sizeof(char) );
 printf("sizeof(short)  %u\n", sizeof(short) );
 printf("sizeof(int)    %u\n", sizeof(int) );
 printf("sizeof(long)   %u\n", sizeof(long) );
 printf("<%s> ------------------\n", obj1_name);
 printf("type on file   ");
 print_datatype(f_type1);
 printf("\n");
 printf("size on file   %u\n", f_size1 );

 printf("type on memory ");
 print_datatype(m_type1);
 printf("\n");
 printf("size on memory %u\n", m_size1 );

 printf("<%s> ------------------\n", obj2_name);
 printf("type on file   ");
 print_datatype(f_type2);
 printf("\n");
 printf("size on file   %u\n", f_size2 );

 printf("type on memory ");
 print_datatype(m_type2);
 printf("\n");
 printf("size on memory %u\n", m_size2 );
#endif /*H5DIFF_DEBUG*/


/*-------------------------------------------------------------------------
 * "upgrade" the smaller memory size 
 *-------------------------------------------------------------------------
 */

 if ( m_size1 != m_size2 )
 {
  if ( m_size1 < m_size2 )
  {
   assert( (H5Tclose(m_type1)) >=0);
   m_type1 = fixtype( f_type2 );
   m_size1 = H5Tget_size( m_type1 );
  }
  else
  {
   assert( (H5Tclose(m_type2)) >=0);
   m_type2 = fixtype( f_type1 );
   m_size2 = H5Tget_size( m_type2 );
  }
  
 }
  
 assert(m_size1==m_size2);
 

#if defined (H5DIFF_DEBUG)
 printf("fixed size on memory %u\n", m_size1 );
 printf("\n");
#endif 

 buf1 = (void *) malloc((unsigned) (tot_cnt1*m_size1));
 buf2 = (void *) malloc((unsigned) (tot_cnt2*m_size2));

 if ( buf1 == NULL || buf2 == NULL )
 {
  printf( "cannot read into memory\n" );
  goto out;
 }

/*-------------------------------------------------------------------------
 * read
 *-------------------------------------------------------------------------
 */

 if ( H5Dread(dset1_id,m_type1,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf1) < 0 )
  goto out;

 if ( H5Dread(dset2_id,m_type2,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf2) < 0 )
  goto out;

 nfound = array_diff(buf1,buf2,tot_cnt1,rank1,dims1,options,obj1_name,obj2_name,m_type1);
 printf("%d differences found\n", nfound );

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */

out:

 if ( buf1) free(buf1);
 if ( buf2) free(buf2);
 
 /* Close */
 if ( dset1_id!=-1 )  assert( (H5Dclose(dset1_id)) >=0);
 if ( dset2_id!=-1 )  assert( (H5Dclose(dset2_id)) >=0);
 if ( space1_id!=-1 ) assert( (H5Sclose(space1_id)) >=0);
 if ( space2_id!=-1 ) assert( (H5Sclose(space2_id)) >=0);
 if ( f_type1!=-1 )   assert( (H5Tclose(f_type1)) >=0);
 if ( f_type2!=-1 )   assert( (H5Tclose(f_type2)) >=0);
 if ( m_type1!=-1 )   assert( (H5Tclose(m_type1)) >=0);
 if ( m_type2!=-1 )   assert( (H5Tclose(m_type2)) >=0);
 
 return 0;

}




/*-------------------------------------------------------------------------
 * Function: array_diff
 *
 * Purpose: compare array
 *
 * Return: 
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
 
int array_diff( void *buf1, void *buf2, hsize_t tot_cnt, int rank, hsize_t *dims, 
                options_t options, const char *obj1, const char *obj2,
                hid_t m_type )
{
 H5T_class_t type_class;
 size_t      type_size;
 int         nfound=0; /* number of differences found */
 int         ph=1;     /* print header  */
 int         acc[32];  /* accumulator and matrix position */
 int         pos[32];
 unsigned    i; 
 int         j;
 char        *_buf1 = (char*)buf1;
 char        *_buf2 = (char*)buf2;
 
 /* some temporary store */
 double      temp1_double;
 double      temp2_double;
 float       temp1_float;
 float       temp2_float;
 long        temp1_long;
 long        temp2_long;
 int         temp1_int;
 int         temp2_int;
 short       temp1_short;
 short       temp2_short;
 char        temp1_char;
 char        temp2_char;


 acc[rank-1]=1;
 for(j=(rank-2); j>=0; j--)
 {
  acc[j]=acc[j+1]*(int)dims[j+1];
 }

 /* Get the class. */
 type_class = H5Tget_class( m_type );

 /* Get the size. */
 type_size = H5Tget_size( m_type );

 
 switch(type_class)
 {
 default:
  return -1;
  
 case H5T_INTEGER:

/*-------------------------------------------------------------------------
 * H5T_NATIVE_SCHAR
 *-------------------------------------------------------------------------
 */
  
  if (H5Tequal(m_type, H5T_NATIVE_SCHAR)) 
  {
   assert(type_size==sizeof(char));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_char, _buf1, sizeof(char));
    memcpy(&temp2_char, _buf2, sizeof(char));
    /* -d and !-p */
    if (options.d_ && !options.p_)
    {
     if (abs(temp1_char-temp2_char) > options.d_delta)
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d_ && options.p_)
    {
     if ( temp1_char!=0 && abs(1-temp2_char/temp1_char) > options.p_relative )
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d_ && options.p_)
    {
     if ( temp1_char!=0 && abs(1-temp2_char/temp1_char) > options.p_relative && 
      abs(temp1_char-temp2_char) > options.d_delta )
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char));
      }
      nfound++;
     }
    }
    else if (temp1_char != temp2_char)
    {
     if (options.n_ && nfound>=options.n_number_count)
      return nfound;
     if ( options.r_==0 ) 
     {
      print_pos(&ph,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char));
     }
     nfound++;
    }
    
    _buf1+=sizeof(char);
    _buf2+=sizeof(char);
   }/* i */
   
  } /*H5T_NATIVE_SCHAR*/


/*-------------------------------------------------------------------------
 * H5T_NATIVE_SHORT
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_SHORT)) 
  {
   assert(type_size==sizeof(short));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_short, _buf1, sizeof(short));
    memcpy(&temp2_short, _buf2, sizeof(short));
    /* -d and !-p */
    if (options.d_ && !options.p_)
    {
     if (abs(temp1_short-temp2_short) > options.d_delta)
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d_ && options.p_)
    {
     if ( temp1_short!=0 && abs(1-temp2_short/temp1_short) > options.p_relative )
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d_ && options.p_)
    {
     if ( temp1_short!=0 && abs(1-temp2_short/temp1_short) > options.p_relative && 
      abs(temp1_short-temp2_short) > options.d_delta )
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short));
      }
      nfound++;
     }
    }
    else if (temp1_short != temp2_short)
    {
     if (options.n_ && nfound>=options.n_number_count)
      return nfound;
     if ( options.r_==0 ) 
     {
      print_pos(&ph,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short));
     }
     nfound++;
    }
    
    _buf1+=sizeof(short);
    _buf2+=sizeof(short);
   }/* i */
   
  } /*H5T_NATIVE_SHORT*/


/*-------------------------------------------------------------------------
 * H5T_NATIVE_INT
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_INT)) 
  {
   assert(type_size==sizeof(int));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_int, _buf1, sizeof(int));
    memcpy(&temp2_int, _buf2, sizeof(int));
    /* -d and !-p */
    if (options.d_ && !options.p_)
    {
     if (abs(temp1_int-temp2_int) > options.d_delta)
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d_ && options.p_)
    {
     if ( temp1_int!=0 && abs(1-temp2_int/temp1_int) > options.p_relative )
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d_ && options.p_)
    {
     if ( temp1_int!=0 && abs(1-temp2_int/temp1_int) > options.p_relative && 
      abs(temp1_int-temp2_int) > options.d_delta )
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int));
      }
      nfound++;
     }
    }
    else if (temp1_int != temp2_int)
    {
     if (options.n_ && nfound>=options.n_number_count)
      return nfound;
     if ( options.r_==0 ) 
     {
      print_pos(&ph,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int));
     }
     nfound++;
    }
    
    _buf1+=sizeof(int);
    _buf2+=sizeof(int);
   }/* i */
   
  } /*H5T_NATIVE_INT*/

/*-------------------------------------------------------------------------
 * H5T_NATIVE_LONG
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_LONG)) 
  {
   assert(type_size==sizeof(long));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_long, _buf1, sizeof(long));
    memcpy(&temp2_long, _buf2, sizeof(long));
    /* -d and !-p */
    if (options.d_ && !options.p_)
    {
     if (labs(temp1_long-temp2_long) > (long)options.d_delta)
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(LIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d_ && options.p_)
    {
     if ( temp1_long!=0 && labs(1-temp2_long/temp1_long) > (long)options.p_relative )
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(LIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d_ && options.p_)
    {
     if ( temp1_long!=0 && labs(1-temp2_long/temp1_long) > (long)options.p_relative && 
      labs(temp1_long-temp2_long) > (long)options.d_delta )
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(LIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long));
      }
      nfound++;
     }
    }
    else if (temp1_long != temp2_long)
    {
     if (options.n_ && nfound>=options.n_number_count)
      return nfound;
     if ( options.r_==0 ) 
     {
      print_pos(&ph,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(LIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long));
     }
     nfound++;
    }
    
    _buf1+=sizeof(long);
    _buf2+=sizeof(long);
   }/* i */
   
  } /*H5T_NATIVE_LONG*/


  break; /*H5T_INTEGER*/


  case H5T_FLOAT:


/*-------------------------------------------------------------------------
 * H5T_NATIVE_FLOAT
 *-------------------------------------------------------------------------
 */
  
  if (H5Tequal(m_type, H5T_NATIVE_FLOAT)) 
  {
   assert(type_size==sizeof(float));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_float, _buf1, sizeof(float));
    memcpy(&temp2_float, _buf2, sizeof(float));
    /* -d and !-p */
    if (options.d_ && !options.p_)
    {
     if (fabs(temp1_float-temp2_float) > options.d_delta)
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d_ && options.p_)
    {
     if ( temp1_float!=0 && fabs(1-temp2_float/temp1_float) > options.p_relative )
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d_ && options.p_)
    {
     if ( temp1_float!=0 && fabs(1-temp2_float/temp1_float) > options.p_relative && 
      fabs(temp1_float-temp2_float) > options.d_delta )
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float));
      }
      nfound++;
     }
    }
    else if (temp1_float != temp2_float)
    {
     if (options.n_ && nfound>=options.n_number_count)
      return nfound;
     if ( options.r_==0 ) 
     {
      print_pos(&ph,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(FFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float));
     }
     nfound++;
    }
    
    _buf1+=sizeof(float);
    _buf2+=sizeof(float);
   }/* i */
   
  } /*H5T_NATIVE_FLOAT*/


  else if (H5Tequal(m_type, H5T_NATIVE_DOUBLE)) 
  {
   assert(type_size==sizeof(double));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_double, _buf1, sizeof(double));
    memcpy(&temp2_double, _buf2, sizeof(double));
    /* -d and !-p */
    if (options.d_ && !options.p_)
    {
     if (fabs(temp1_double-temp2_double) > options.d_delta)
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d_ && options.p_)
    {
     if ( temp1_double!=0 && fabs(1-temp2_double/temp1_double) > options.p_relative )
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d_ && options.p_)
    {
     if ( temp1_double!=0 && fabs(1-temp2_double/temp1_double) > options.p_relative && 
      fabs(temp1_double-temp2_double) > options.d_delta )
     {
      if (options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos(&ph,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double));
      }
      nfound++;
     }
    }
    else if (temp1_double != temp2_double)
    {
     if (options.n_ && nfound>=options.n_number_count)
      return nfound;
     if ( options.r_==0 ) 
     {
      print_pos(&ph,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(FFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double));
     }
     nfound++;
    }
    
    _buf1+=sizeof(double);
    _buf2+=sizeof(double);
   }/* i */
   
  } /*H5T_NATIVE_DOUBLE*/


  break; /*H5T_FLOAT*/

  
 } /*switch*/
   
 
 return nfound;
}






/*-------------------------------------------------------------------------
 * Function: print_pos
 *
 * Purpose: convert an array index position to matrix notation
 *
 * Return: pos matrix array
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

void print_pos( int *ph, unsigned int curr_pos, int *acc, 
                int *pos, int rank, const char *obj1, const char *obj2 )
{
 int i;

 /* print header */
 if ( *ph==1 )
 {
  printf("%-15s %-15s %-15s %-20s\n", "position", obj1, obj2, "difference");
  printf("------------------------------------------------------------\n");
  *ph=0;
 }

 for ( i = 0; i < rank; i++)
  pos[i]=0;

 for ( i = 0; i < rank; i++)
 {
  pos[i] = curr_pos/acc[i];
  curr_pos -= acc[i]*pos[i];
 }
 assert( curr_pos == 0 );

 printf("[ " );  
 for ( i = 0; i < rank; i++)
 {
  printf("%d ", pos[i]  );
 }
 printf("]" );
}

/*-------------------------------------------------------------------------
 * Function: print_class
 *
 * Purpose: print the class name
 *
 * Return: 
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

void print_class( H5T_class_t tclass, char *sclass )
{
 switch (tclass) 
 {
 default:
  printf("Invalid class");
  break;
 case H5T_TIME:
  strcpy(sclass,"H5T_TIME");
  break;
 case H5T_INTEGER:
  strcpy(sclass,"H5T_INTEGER");
  break;
 case H5T_FLOAT:
  strcpy(sclass,"H5T_FLOAT");
  break;
 case H5T_STRING:
  strcpy(sclass,"H5T_STRING");
  break;
 case H5T_BITFIELD:
  strcpy(sclass,"H5T_BITFIELD");
  break;
 case H5T_OPAQUE:
  strcpy(sclass,"H5T_OPAQUE");
  break;
 case H5T_COMPOUND:
  strcpy(sclass,"H5T_COMPOUND");
  break;
 case H5T_REFERENCE:
  strcpy(sclass,"H5T_REFERENCE");
  break;
 case H5T_ENUM:
  strcpy(sclass,"H5T_ENUM");
  break;
 case H5T_VLEN:
  strcpy(sclass,"H5T_VLEN");
  break;
 case H5T_ARRAY:
  strcpy(sclass,"H5T_ARRAY");
  break;
 }
}


/*-------------------------------------------------------------------------
 * Function: fixtype
 *
 * Purpose: Given a file data type choose a memory data type which is
 *  appropriate 
 *
 * Return: Memory data type
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments: Adapted from h5tools_fixtype
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t fixtype(hid_t f_type)
{
 hid_t   m_type = -1;
 size_t  size;
 
 size = H5Tget_size(f_type);
 
 switch (H5Tget_class(f_type)) 
 {
 default:
  return -1;
 case H5T_INTEGER:
/*
 * Use the smallest native integer type of the same sign as the file
 * such that the memory type is at least as large as the file type.
 * If there is no memory type large enough then use the largest
 * memory type available.
 */
  if (size <= sizeof(char)) 
  {
   m_type = H5Tcopy(H5T_NATIVE_SCHAR);
   #if defined (H5DIFF_DEBUG)
   printf("using memory type H5T_NATIVE_SCHAR\n");
   #endif
  } 
  else if (size <= sizeof(short)) 
  {
   m_type = H5Tcopy(H5T_NATIVE_SHORT);
   #if defined (H5DIFF_DEBUG) 
    printf("using memory type H5T_NATIVE_SHORT\n");  
   #endif
  } 
  else if (size <= sizeof(int)) 
  {
   m_type = H5Tcopy(H5T_NATIVE_INT);
   #if defined (H5DIFF_DEBUG)  
    printf("using memory type H5T_NATIVE_INT\n");
   #endif
  } 
  else if (size <= sizeof(long)) 
  {
   m_type = H5Tcopy(H5T_NATIVE_LONG);
   #if defined (H5DIFF_DEBUG) 
    printf("using memory type H5T_NATIVE_LONG\n");
   #endif
  } 
  else 
  {
   m_type = H5Tcopy(H5T_NATIVE_LLONG);
   #if defined (H5DIFF_DEBUG)
    printf("using memory type H5T_NATIVE_LLONG\n");
   #endif
  }
  
  H5Tset_sign(m_type, H5Tget_sign(f_type));
  break;
  
 case H5T_FLOAT:
/*
 * Use the smallest native floating point type available such that
 * its size is at least as large as the file type.  If there is not
 * native type large enough then use the largest native type.
 */
  if (size <= sizeof(float)) 
  {
   m_type = H5Tcopy(H5T_NATIVE_FLOAT);
   #if defined (H5DIFF_DEBUG)
    printf("using memory type H5T_NATIVE_FLOAT\n");
   #endif
  } 
  else if (size <= sizeof(double)) 
  {
   m_type = H5Tcopy(H5T_NATIVE_DOUBLE);
   #if defined (H5DIFF_DEBUG) 
    printf("using memory type H5T_NATIVE_DOUBLE\n");
   #endif
  } 
  else 
  {
   m_type = H5Tcopy(H5T_NATIVE_LDOUBLE);
   #if defined (H5DIFF_DEBUG)
    printf("using memory type H5T_NATIVE_LDOUBLE\n");
   #endif
  }
  break;
  
 }
  
 return m_type;
}



/*-------------------------------------------------------------------------
 * Function: print_datatype
 *
 * Purpose: Print name of datatype 
 *
 * Return: 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments: Adapted from h5dump for H5T_INTEGER and H5T_FLOAT classes only
 *
 *-------------------------------------------------------------------------
 */

void print_datatype(hid_t type)
{
 switch (H5Tget_class(type)) 
 {
 default:
  return;
 case H5T_INTEGER:
  if (H5Tequal(type, H5T_STD_I8BE)) {
   printf("H5T_STD_I8BE");
  } else if (H5Tequal(type, H5T_STD_I8LE)) {
   printf("H5T_STD_I8LE");
  } else if (H5Tequal(type, H5T_STD_I16BE)) {
   printf("H5T_STD_I16BE");
  } else if (H5Tequal(type, H5T_STD_I16LE)) {
   printf("H5T_STD_I16LE");
  } else if (H5Tequal(type, H5T_STD_I32BE)) {
   printf("H5T_STD_I32BE");
  } else if (H5Tequal(type, H5T_STD_I32LE)) {
   printf("H5T_STD_I32LE");
  } else if (H5Tequal(type, H5T_STD_I64BE)) {
   printf("H5T_STD_I64BE");
  } else if (H5Tequal(type, H5T_STD_I64LE)) {
   printf("H5T_STD_I64LE");
  } else if (H5Tequal(type, H5T_STD_U8BE)) {
   printf("H5T_STD_U8BE");
  } else if (H5Tequal(type, H5T_STD_U8LE)) {
   printf("H5T_STD_U8LE");
  } else if (H5Tequal(type, H5T_STD_U16BE)) {
   printf("H5T_STD_U16BE");
  } else if (H5Tequal(type, H5T_STD_U16LE)) {
   printf("H5T_STD_U16LE");
  } else if (H5Tequal(type, H5T_STD_U32BE)) {
   printf("H5T_STD_U32BE");
  } else if (H5Tequal(type, H5T_STD_U32LE)) {
   printf("H5T_STD_U32LE");
  } else if (H5Tequal(type, H5T_STD_U64BE)) {
   printf("H5T_STD_U64BE");
  } else if (H5Tequal(type, H5T_STD_U64LE)) {
   printf("H5T_STD_U64LE");
  } else if (H5Tequal(type, H5T_NATIVE_SCHAR)) {
   printf("H5T_NATIVE_SCHAR");
  } else if (H5Tequal(type, H5T_NATIVE_UCHAR)) {
   printf("H5T_NATIVE_UCHAR");
  } else if (H5Tequal(type, H5T_NATIVE_SHORT)) {
   printf("H5T_NATIVE_SHORT");
  } else if (H5Tequal(type, H5T_NATIVE_USHORT)) {
   printf("H5T_NATIVE_USHORT");
  } else if (H5Tequal(type, H5T_NATIVE_INT)) {
   printf("H5T_NATIVE_INT");
  } else if (H5Tequal(type, H5T_NATIVE_UINT)) {
   printf("H5T_NATIVE_UINT");
  } else if (H5Tequal(type, H5T_NATIVE_LONG)) {
   printf("H5T_NATIVE_LONG");
  } else if (H5Tequal(type, H5T_NATIVE_ULONG)) {
   printf("H5T_NATIVE_ULONG");
  } else if (H5Tequal(type, H5T_NATIVE_LLONG)) {
   printf("H5T_NATIVE_LLONG");
  } else if (H5Tequal(type, H5T_NATIVE_ULLONG)) {
   printf("H5T_NATIVE_ULLONG");
  } else {
   printf("undefined integer");
  }
  break;
  
 case H5T_FLOAT:
  if (H5Tequal(type, H5T_IEEE_F32BE)) {
   printf("H5T_IEEE_F32BE");
  } else if (H5Tequal(type, H5T_IEEE_F32LE)) {
   printf("H5T_IEEE_F32LE");
  } else if (H5Tequal(type, H5T_IEEE_F64BE)) {
   printf("H5T_IEEE_F64BE");
  } else if (H5Tequal(type, H5T_IEEE_F64LE)) {
   printf("H5T_IEEE_F64LE");
  } else if (H5Tequal(type, H5T_NATIVE_FLOAT)) {
   printf("H5T_NATIVE_FLOAT");
  } else if (H5Tequal(type, H5T_NATIVE_DOUBLE)) {
   printf("H5T_NATIVE_DOUBLE");
  } else if (H5Tequal(type, H5T_NATIVE_LDOUBLE)) {
   printf("H5T_NATIVE_LDOUBLE");
  } else {
   printf("undefined float");
  }
  break;
   
 }/*switch*/
}






