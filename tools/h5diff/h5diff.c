
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

#define FFORMAT "%-15g %-15g %-15g\n"
#define IFORMAT "%-15d %-15d %-15d\n"
#define SPACES  "          "


typedef struct options_t
{
 int    l_; /* list */
 int    r_; /* report only what objects differ */
 int    d_; /* delta difference */
 double d_delta; /* delta value */
 int    p_; /* relative error */
 double p_relative; /* relative error value */
 int    n_; /* count */
 int    n_number_count; /* value */
} options_t;

int do_test_files();
int diff_dataset( hid_t file1_id, hid_t file2_id, char *obj1_name, 
                  char *obj2_name, options_t options );
int array_diff( void *buf1, void *buf2, hsize_t tot_cnt, hid_t type_id, int rank,
                 hsize_t *dims, options_t options, char *obj1, char *obj2 );
void print_pos( int *ph, int curr_pos, int *acc, 
                int *pos, int rank, char *obj1, char *obj2 );
void print_class( H5T_class_t tclass, char *sclass );
hid_t fixtype( hid_t f_type );
void list( const char *filename, int nobjects, info_t *info );
void diff( hid_t file1_id, char *obj1_name, hid_t file2_id, char *obj2_name, 
           options_t options, int type );
void compare( hid_t file1_id, char *obj1_name, int nobjects1, info_t *info1,
              hid_t file2_id, char *obj2_name, int nobjects2, info_t *info2,
              options_t options );
void match( hid_t file1_id, char *file1_name, int nobjects1, info_t *info1,
            hid_t file2_id, char *file2_name, int nobjects2, info_t *info2,
            options_t options );
int check_n_input( char * );
int check_f_input( char * );


/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: print a usage message  
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 8, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void usage( const char *progname )
{
#define USAGE   "\n\
  [OBJ1_NAME]       Name of an HDF5 object\n\
  [OBJ2_NAME]       Name of an HDF5 object\n\
  [-h ]             Print a basic help message (this message)\n\
  [-l ]             List contents of file\n\
  [-r ]             Print only what objects differ\n\
  [-n count]        Print difference up to count number for each variable\n\
  [-d delta]        Print difference when it is greater than limit delta\n\
  [-p relative]     Print differences which are within a relative error value\n\
  FILE1_NAME        File name of the first HDF5 file\n\
  FILE2_NAME        File name of the second HDF5 file\n"

 fprintf(stderr,
  "h5diff [OBJ1_NAME] [OBJ2_NAME] [-h] [-l] [-r] [-d] [-n count] [-d delta] [-p relativet] FILE1_NAME FILE2_NAME\n%s",
  USAGE);
 fprintf(stderr,"\n");
 fprintf(stderr,"Items in [ ] are optional \n");
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
 * Date: October 8, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int main(int argc, const char *argv[])
{
 
 const char *progname = argv[0];
 int        argno;
 const char *s = NULL;
 hid_t      file1_id, file2_id; 
 herr_t     status;
 int        nobjects1, nobjects2;
 info_t     *info1=NULL;
 info_t     *info2=NULL;
 int        obj1_found = 0;
 int        obj2_found = 0;
 options_t  options = {0,0,0,0,0,0,0,0};
 void       *edata;
 hid_t      (*func)(void*);

 char       *file1_name;
 char       *file2_name;
 char       *obj1_name  = NULL;
 char       *obj2_name  = NULL;
 
/*-------------------------------------------------------------------------
 * parse command line options
 *-------------------------------------------------------------------------
 */
 
 if (argc < 3) {
  usage( progname );
  exit(EXIT_FAILURE);
 }
 
 /* last 2 items are the file names */
 for (argno=1; argno<argc ; argno++) 
 {
  
  /* get the single-letter switches */
  if ( '-'==argv[argno][0] )
  {
   
   for (s=argv[argno]+1; *s; s++) 
   {
    switch (*s) {
    default:
    printf("-%s is an invalid option\n", s );
    usage(progname);
    exit(EXIT_SUCCESS);
    break;
    case 'h': 
     usage(progname);
     exit(EXIT_SUCCESS);
    case 'l': 
     options.l_ = 1;
     break;
    case 'r': 
     options.r_ = 1;
     break;
    case 'd': 
     /* if it is not another option */
     if ( '-' !=argv[argno+1][0] )
     {
      options.d_      = 1;

      if ( check_f_input(argv[argno+1])==-1 )
      {
       printf("<-d %s> is not a valid option\n", argv[argno+1] );
       usage(progname);
       exit(EXIT_SUCCESS);
      }

      options.d_delta = atof(argv[argno+1]);
     }
     else
     {
      printf("<-d %s> is not a valid option\n", argv[argno+1] );
      usage(progname);
      exit(EXIT_SUCCESS);
     }
     break;
    case 'p': 
     if ( '-' !=argv[argno+1][0] )
     {
      options.p_         = 1;

      if ( check_f_input(argv[argno+1])==-1 )
      {
       printf("<-p %s> is not a valid option\n", argv[argno+1] );
       usage(progname);
       exit(EXIT_SUCCESS);
      }

      options.p_relative  = atof(argv[argno+1]);
     }
     break;
    case 'n': 
     if ( '-' !=argv[argno+1][0] )
     {
      options.n_             = 1;

      if ( check_n_input(argv[argno+1])==-1 )
      {
       printf("<-n %s> is not a valid option\n", argv[argno+1] );
       usage(progname);
       exit(EXIT_SUCCESS);
      }
      options.n_number_count = atoi(argv[argno+1]);
     }
     break;
    } /*switch*/
   } /*for*/ 
  } /*if*/
  
  else
   
  {
   
   /* 2 last args are the file names, and it is not a -switch parameter */
   if ( argno < argc-2 && '-' !=argv[argno-1][0] )
   {
    if ( obj1_name == NULL )
     obj1_name = argv[argno];

    if ( obj2_name == NULL )
    {
     
     /* check if we have a second object name */
     if ( argno+1 < argc-2 && '-' !=argv[argno+1][0] )
      /* yes */
      obj2_name = argv[argno+1];
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
  exit(EXIT_FAILURE);
 }

 if ((file2_id=H5Fopen(file2_name,H5F_ACC_RDONLY,H5P_DEFAULT))<0 )
 {
  printf("h5diff: %s: No such file or directory\n", file2_name );
  exit(EXIT_FAILURE);
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

 if ( options.l_ )
 {
  list ( file1_name, nobjects1, info1 );
  list ( file2_name, nobjects2, info2 );
  printf("\n");
 }
  


/*-------------------------------------------------------------------------
 * object name was supplied
 *-------------------------------------------------------------------------
 */
 
 if ( obj1_name )
 {
  compare(file1_id,obj1_name,nobjects1,info1,
          file2_id,obj2_name,nobjects2,info2,options);
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
 status = H5Fclose(file1_id);
 status = H5Fclose(file2_id);
 
 if ( info1 )
  free(info1);
 if ( info2 )
  free(info2);
 
 return 0;
 
}


/*-------------------------------------------------------------------------
 * Function: check_n_input
 *
 * Purpose: 
 *
 * Return: 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 10, 2003
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int check_n_input( char *str )
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
 * Purpose: 
 *
 * Return: 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 10, 2003
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int check_f_input( char *str )
{
 unsigned i;
 char c;

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
 * Date: March 10, 2003
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
  default:
   printf("non supported object\n" );
   break;
  }
 }

}


/*-------------------------------------------------------------------------
 * Function: get_index
 *
 * Purpose: 
 *
 * Return: 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 10, 2003
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


int get_index( char *obj, int nobjects, info_t *info )
{
 char *pdest;
 int  result;
 int  i;

 for ( i = 0; i < nobjects; i++) 
 {
  
  if ( strcmp(obj,info[i].name)==0 )
   return i;

  pdest  = strstr( info[i].name, obj );
  result = pdest - info[i].name;

  /* found at position 1, meaning without '/' */
  if( pdest != NULL && result==1 )
   return i;
 }
 return -1;
}

/*-------------------------------------------------------------------------
 * Function: compare
 *
 * Purpose: 
 *
 * Return: 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 10, 2003
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


void compare( hid_t file1_id, char *obj1_name, int nobjects1, info_t *info1,
              hid_t file2_id, char *obj2_name, int nobjects2, info_t *info2,
              options_t options )
{

 int i = get_index( obj1_name, nobjects1, info1 );
 int j = get_index( obj2_name, nobjects2, info2 );

 if ( i == -1 )
 {
  printf( "Object <%s> could not be found\n", obj1_name );
  return;
 }

 if ( j == -1 )
 {
  printf( "Object <%s> could not be found\n", obj1_name );
  return;
 }

 /* objects are not the same type */
 if ( info1[i].type != info2[j].type )
 {
  printf( "<%s> is of different type than <%s>\n", obj1_name, obj2_name );
  return;
 }
  
 diff( file1_id, obj1_name, file2_id, obj2_name, options, info1[i].type );
 
}


/*-------------------------------------------------------------------------
 * Function: diff
 *
 * Purpose: 
 *
 * Return: 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 10, 2003
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


void diff( hid_t file1_id, char *obj1_name, hid_t file2_id, char *obj2_name, 
           options_t options, int type )
{

 switch ( type )
 {
  
/*-------------------------------------------------------------------------
 * H5G_GROUP
 *-------------------------------------------------------------------------
 */ 
  
 case H5G_GROUP:
  
  break;
  
 /*-------------------------------------------------------------------------
  * H5G_DATASET
  *-------------------------------------------------------------------------
  */
  
 case H5G_DATASET:
  
  diff_dataset(file1_id,file2_id,obj1_name,obj2_name,options);
  break;
  
 /*-------------------------------------------------------------------------
  * H5G_TYPE
  *-------------------------------------------------------------------------
  */
  
 case H5G_TYPE:
   
  break;
  
 } /* switch */
 
 
}




/*-------------------------------------------------------------------------
 * Function: compare_object
 *
 * Purpose: 
 *
 * Return: 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 10, 2003
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
 * Purpose: 
 *
 * Return: 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 10, 2003
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void match( hid_t file1_id, char *file1_name, int nobjects1, info_t *info1,
            hid_t file2_id, char *file2_name, int nobjects2, info_t *info2,
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
   diff( file1_id, info1[curr1].name, file2_id, info1[curr1].name, options, 
         info1[curr1].type );

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
    file1_name, file2_name);
   curr2++;
  }
 }


}

/*-------------------------------------------------------------------------
 * Function: diff_dataset
 *
 * Purpose: 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 8, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int diff_dataset( hid_t file1_id, hid_t file2_id, char *obj1_name, 
                  char *obj2_name, options_t options )
{

 hid_t   dset1_id, dset2_id; 
 hid_t   space1_id, space2_id; 
 hid_t   type1_id, type2_id;
 hid_t   rank1, rank2; 
 void    *buf1, *buf2;
 hsize_t tot_cnt, tot_cnt1, tot_cnt2;
 hsize_t dims1[32], dims2[32];
 int     i, j;
 herr_t  status;
 H5T_class_t tclass1;
 H5T_class_t tclass2;
 char    sclass1[20];
 char    sclass2[20];
 int     nfound;
 size_t  type1_size, type2_size;
 hid_t   type_mem =-1; /* read to memory type */
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
  return -1;
 }

 if ( (dset2_id = H5Dopen(file2_id,obj2_name)) < 0 )
 {
  printf("Cannot open dataset <%s>\n", obj2_name );
  return -1;
 }

 printf( "Comparing <%s> with <%s>\n", obj1_name, obj2_name );

 /* enable error reporting */
 H5Eset_auto(func, edata);

 /* Get the datatype */
 if ( (type1_id = H5Dget_type(dset1_id)) < 0 )
  goto out;

 /* Get the datatype */
 if ( (type2_id = H5Dget_type(dset2_id)) < 0 )
  goto out;

 /* Get the size */
 type1_size = H5Tget_size( type1_id );
 type2_size = H5Tget_size( type2_id );

  /* Get the dataspace handle */
 if ( (space1_id = H5Dget_space(dset1_id)) < 0 )
  return -1;

 /* Get rank */
 if ( (rank1 = H5Sget_simple_extent_ndims(space1_id)) < 0 )
  return -1;

 /* Get the dataspace handle */
 if ( (space2_id = H5Dget_space(dset2_id)) < 0 )
  return -1;

 /* Get rank */
 if ( (rank2 = H5Sget_simple_extent_ndims(space2_id)) < 0 )
  return -1;

 /* Get dimensions */
 if ( H5Sget_simple_extent_dims(space1_id,dims1,NULL) < 0 )
  goto out;

 /* Get dimensions */
 if ( H5Sget_simple_extent_dims(space2_id,dims2,NULL) < 0 )
  goto out;


/*-------------------------------------------------------------------------
 * check for the same class datatype
 *-------------------------------------------------------------------------
 */

 if ((tclass1=H5Tget_class(type1_id))<0) 
  goto out;

 if ((tclass2=H5Tget_class(type2_id))<0) 
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
   printf("%d ", dims1[j]  );
  printf("]\n" );
  printf( "<%s>: ", obj2_name );
  printf("[ " );  
  for (j = 0; j < rank1; j++) 
   printf("%d ", dims2[j]  );
  printf("]\n" );
  goto out;
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
    printf("%d ", dims1[j]  );
   printf("]\n" );
   printf( "<%s>: ", obj2_name );
   printf("[ " );  
   for (j = 0; j < rank1; j++) 
    printf("%d ", dims2[j]  );
   printf("]\n" );
   goto out;
  }
 }


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

 buf1 = (void *) malloc((unsigned) (tot_cnt1*H5Tget_size(type1_id)));
 buf2 = (void *) malloc((unsigned) (tot_cnt2*H5Tget_size(type2_id)));

 if ( buf1 == NULL || buf2 == NULL )
 {
  printf( "cannot read into memory\n" );
  if ( buf1) free((char *) buf1);
  if ( buf2) free((char *) buf2);
  goto out;
 }

/*-------------------------------------------------------------------------
 * memory type
 *-------------------------------------------------------------------------
 */

 type_mem = fixtype( type1_id );

/*-------------------------------------------------------------------------
 * read
 *-------------------------------------------------------------------------
 */

 if ( H5Dread(dset1_id,type_mem,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf1) < 0 )
  goto out;

 if ( H5Dread(dset2_id,type_mem,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf2) < 0 )
  goto out;
 
 if (tot_cnt1 > tot_cnt2)
  tot_cnt = tot_cnt2;
 else
  tot_cnt = tot_cnt1; 

 nfound = array_diff(buf1,buf2,tot_cnt,type1_id,rank1,dims1,options,obj1_name,obj2_name);
 printf("%d differences found\n", nfound );


 free((char *) buf1);
 free((char *) buf2);

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */

out:
 
 /* Close */
 status = H5Dclose(dset1_id);
 status = H5Dclose(dset2_id);
 status = H5Sclose(space1_id);
 status = H5Sclose(space2_id);
 status = H5Tclose(type1_id);
 status = H5Tclose(type2_id);
 
 return 0;

}

/*-------------------------------------------------------------------------
 * Function: array_diff
 *
 * Purpose: 
 *
 * Return: 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 8, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
 
int array_diff( void *buf1, void *buf2, hsize_t tot_cnt, hid_t type_id, int rank,
                 hsize_t *dims, options_t options, char *obj1, char *obj2 )
{
 char   *i1ptr1, *i1ptr2;
 short  *i2ptr1, *i2ptr2;
 int    *i4ptr1, *i4ptr2;
 long   *i8ptr1, *i8ptr2;
 float  *fptr1, *fptr2;
 double *dptr1, *dptr2;
 int    nfound=0; /* number of differences found */
 int    ph=1;     /* print header /*

 /* accumulator and matrix position */
 int    acc[32];
 int    pos[32];
 int    i;

 H5T_class_t type_class;
 size_t      type_size;

 acc[rank-1]=1;
 for(i=(rank-2); i>=0; i--)
 {
  acc[i]=acc[i+1]*(int)dims[i+1];
 }

 /* Get the class. */
 type_class = H5Tget_class( type_id );

 /* Get the size. */
 type_size = H5Tget_size( type_id );

 
 switch(type_class)
 {
  case H5T_INTEGER:


   switch(type_size)
   {

  /*-------------------------------------------------------------------------
   * H5T_INTEGER 1
   *-------------------------------------------------------------------------
   */

   case 1:
    i1ptr1 = (char *) buf1;
    i1ptr2 = (char *) buf2;

    for ( i = 0; i < tot_cnt; i++)
    {
     /* delta but not percentage */
     if ( options.d_ && !options.p_ )
     {
      if ( abs(*i1ptr1 - *i1ptr2) > options.d_delta )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(IFORMAT, *i1ptr1, *i1ptr2, abs(*i1ptr1 - *i1ptr2));
       }
       nfound++;
      }
     }
      
     /* percentage but not delta */
     else if ( !options.d_ && options.p_ )
     {
      if ( abs(1 - *i1ptr1 / *i1ptr2)  > options.p_relative  )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(IFORMAT, *i1ptr1, *i1ptr2, abs(*i1ptr1 - *i1ptr2));
       }
       nfound++;
      }
     }
   
     /* percentage and delta */
     else if ( options.d_ && options.p_ )
     {
      if ( abs(1 - *i1ptr1 / *i1ptr2)  > options.p_relative &&
           fabs(*i1ptr1 - *i1ptr2) > options.d_delta )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(IFORMAT, *i1ptr1, *i1ptr2, abs(*i1ptr1 - *i1ptr2));
       }
       nfound++;
      }
     }
     
     else
          
     if (*i1ptr1 != *i1ptr2)
     {
      if ( options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
       printf(SPACES);
       printf(IFORMAT, *i1ptr1, *i1ptr2, abs(*i1ptr1 - *i1ptr2));
      }
      nfound++;
      }                                               
     i1ptr1++;  i1ptr2++;
    }
  
    break;

  /*-------------------------------------------------------------------------
   * H5T_INTEGER 2
   *-------------------------------------------------------------------------
   */

   case 2:
    i2ptr1 = (short *) buf1;
    i2ptr2 = (short *) buf2;

    for ( i = 0; i < tot_cnt; i++)
    {
     /* delta but not percentage */
     if ( options.d_ && !options.p_ )
     {
      if ( abs(*i2ptr1 - *i2ptr2) > options.d_delta )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(IFORMAT, *i2ptr1, *i2ptr2, abs(*i2ptr1 - *i2ptr2));
       }
       nfound++;
      }
     }
      
     /* percentage but not delta */
     else if ( !options.d_ && options.p_ )
     {
      if ( abs(1 - *i2ptr1 / *i2ptr2)  > options.p_relative  )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(IFORMAT, *i2ptr1, *i2ptr2, abs(*i2ptr1 - *i2ptr2));
       }
       nfound++;
       }
     }
   
     /* percentage and delta */
     else if ( options.d_ && options.p_ )
     {
      if ( abs(1 - *i2ptr1 / *i2ptr2) > options.p_relative &&
           abs(*i2ptr1 - *i2ptr2) > options.d_delta )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(IFORMAT, *i2ptr1, *i2ptr2, abs(*i2ptr1 - *i2ptr2));
       }
       nfound++;
      }
     }
     
     else
          
     if (*i2ptr1 != *i2ptr2)
     {
      if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
      if ( options.r_==0 ) 
      {
       print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
       printf(SPACES);
       printf(IFORMAT, *i2ptr1, *i2ptr2, abs(*i2ptr1 - *i2ptr2));
      }
      nfound++;
     }                                               
     i2ptr1++;  i2ptr2++;
    }
   
    break;

  /*-------------------------------------------------------------------------
   * H5T_INTEGER 4
   *-------------------------------------------------------------------------
   */

   case 4:
    i4ptr1 = (int *) buf1;
    i4ptr2 = (int *) buf2;

    for ( i = 0; i < tot_cnt; i++)
    {

     /* delta but not percentage */
     if ( options.d_ && !options.p_ )
     {
      if ( options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( abs(*i4ptr1 - *i4ptr2) > options.d_delta )
      {
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(IFORMAT, *i4ptr1, *i4ptr2, abs(*i4ptr1 - *i4ptr2));
       }
       nfound++;
      }
     }
      
     /* percentage but not delta */
     else if ( !options.d_ && options.p_ )
     {
      if ( abs(1 - *i4ptr1 / *i4ptr2) > options.p_relative  )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(IFORMAT, *i4ptr1, *i4ptr2, abs(*i4ptr1 - *i4ptr2));
       }
       nfound++;
      }
     }
   
     /* percentage and delta */
     else if ( options.d_ && options.p_ )
     {
      if ( abs(1 - *i4ptr1 / *i4ptr2) > options.p_relative &&
           abs(*i4ptr1 - *i4ptr2) > options.d_delta )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(IFORMAT, *i4ptr1, *i4ptr2, abs(*i4ptr1 - *i4ptr2));
       }
       nfound++;
      }
     }
     
     else
          
     if (*i4ptr1 != *i4ptr2)
     {
      if ( options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
       printf(SPACES);
       printf(IFORMAT, *i4ptr1, *i4ptr2, abs(*i4ptr1 - *i4ptr2));
      }
      nfound++;
     } 
     i4ptr1++;  i4ptr2++;
    } /*for */

    break;


  /*-------------------------------------------------------------------------
   * H5T_INTEGER 8
   *-------------------------------------------------------------------------
   */

   case 8:
    i8ptr1 = (long *) buf1;
    i8ptr2 = (long *) buf2;
 
    for ( i = 0; i < tot_cnt; i++)
    {
     /* delta but not percentage */
     if ( options.d_ && !options.p_ )
     {
      if ( options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( abs(*i8ptr1 - *i8ptr2) > options.d_delta )
      {
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(IFORMAT, *i8ptr1, *i8ptr2, abs(*i8ptr1 - *i8ptr2));
       }
       nfound++;
      }
     }
      
     /* percentage but not delta */
     else if ( !options.d_ && options.p_ )
     {
      if ( abs(1 - *i8ptr1 / *i8ptr2) > options.p_relative  )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(IFORMAT, *i8ptr1, *i8ptr2, abs(*i8ptr1 - *i8ptr2));
       }
       nfound++;
      }
     }
   
     /* percentage and delta */
     else if ( options.d_ && options.p_ )
     {
      if ( abs(1 - *i8ptr1 / *i8ptr2) > options.p_relative &&
           abs(*i8ptr1 - *i8ptr2) > options.d_delta )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(IFORMAT, *i8ptr1, *i8ptr2, abs(*i8ptr1 - *i8ptr2));
       }
       nfound++;
      }
     }
     
     else
          
     if (*i8ptr1 != *i8ptr2)
     {
      if ( options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
       printf(SPACES);
       printf(IFORMAT, *i8ptr1, *i8ptr2, abs(*i8ptr1 - *i8ptr2));
      }
      nfound++;
      
     } 
     i8ptr1++;  i8ptr2++;
    } /*for */

    break;
    

   default:
    printf("no valid H5T_INTEGER size found" );
    break;
    
       
   } /*switch*/

   
   break; /* H5T_INTEGER */


   case H5T_FLOAT:

   switch(type_size)
   {

  /*-------------------------------------------------------------------------
   * H5T_FLOAT 4
   *-------------------------------------------------------------------------
   */
   case 4:
    fptr1 = (float *) buf1;
    fptr2 = (float *) buf2;
  
    for ( i = 0; i < tot_cnt; i++)
    {
     /* delta but not percentage */
     if ( options.d_ && !options.p_ )
     {
      if ( fabs(*fptr1 - *fptr2) > options.d_delta )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(FFORMAT, *fptr1, *fptr2, fabs(*fptr1 - *fptr2));
       }
       nfound++;
      }
     }
      
     /* percentage but not delta */
     else if ( !options.d_ && options.p_ )
     {
      if ( fabs(1 - *fptr1 / *fptr2)  > options.p_relative  )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(FFORMAT, *fptr1, *fptr2, fabs(*fptr1 - *fptr2));
       }
       nfound++;
       }
     }
   
     /* percentage and delta */
     else if ( options.d_ && options.p_ )
     {
      if ( fabs(1 - *fptr1 / *fptr2)  > options.p_relative &&
           fabs(*fptr1 - *fptr2) > options.d_delta )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(FFORMAT, *fptr1, *fptr2, fabs(*fptr1 - *fptr2));
       }
       nfound++;
      }
     }
     
     else
          
     if (*fptr1 != *fptr2)
     {
      if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
      if ( options.r_==0 ) 
      {
       print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
       printf(SPACES);
       printf(FFORMAT, *fptr1, *fptr2, fabs(*fptr1 - *fptr2));
      }
      nfound++;
     }                                               
     fptr1++;  fptr2++;
    }
    break;
 
  /*-------------------------------------------------------------------------
   * H5T_FLOAT 8
   *-------------------------------------------------------------------------
   */

   case 8:
    dptr1 = (double *) buf1;
    dptr2 = (double *) buf2;

    for ( i = 0; i < tot_cnt; i++)
    {
     /* delta but not percentage */
     if ( options.d_ && !options.p_ )
     {
      if ( fabs(*dptr1 - *dptr2) > options.d_delta )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(FFORMAT, *dptr1, *dptr2, fabs(*dptr1 - *dptr2));
       }
       nfound++;
       }
     }
      
     /* percentage but not delta */
     else if ( !options.d_ && options.p_ )
     {
      if ( 1 - *dptr1 / *dptr2  > options.p_relative  )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(FFORMAT, *dptr1, *dptr2, fabs(*dptr1 - *dptr2));
       }
       nfound++;
      }
     }
   
     /* percentage and delta */
     else if ( options.d_ && options.p_ )
     {
      if ( fabs(1 - *dptr1 / *dptr2)  > options.p_relative &&
           fabs(*dptr1 - *dptr2) > options.d_delta )
      {
       if ( options.n_ && nfound>=options.n_number_count)
        return nfound;
       if ( options.r_==0 ) 
       {
        print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
        printf(SPACES);
        printf(FFORMAT, *dptr1, *dptr2, fabs(*dptr1 - *dptr2));
       }
       nfound++;
      }
     }
     
     else
          
     if (*dptr1 != *dptr2)
     {
      if ( options.n_ && nfound>=options.n_number_count)
       return nfound;
      if ( options.r_==0 ) 
      {
       print_pos( &ph, i, acc, pos, rank, obj1, obj2 );
       printf(SPACES);
       printf(FFORMAT, *dptr1, *dptr2, fabs(*dptr1 - *dptr2));
      }
      nfound++;
     }                                               
     dptr1++;  dptr2++;
    }
   
    break;

    default:
    printf("no valid H5T_FLOAT size found" );
    break;
      
   } /*switch*/

   
   break; /* H5T_FLOAT 8 */
   
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
 * Date: December 19, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void print_pos( int *ph, int curr_pos, int *acc, 
                int *pos, int rank, char *obj1, char *obj2 )
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
 * Date: February 24, 2003
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
 * Date: march 5, 2003
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
 case H5T_INTEGER:
/*
 * Use the smallest native integer type of the same sign as the file
 * such that the memory type is at least as large as the file type.
 * If there is no memory type large enough then use the largest
 * memory type available.
 */
  if (size <= sizeof(char)) {
   m_type = H5Tcopy(H5T_NATIVE_SCHAR);
   printf("using memory type H5T_NATIVE_SCHAR\n");
  } else if (size <= sizeof(short)) {
   m_type = H5Tcopy(H5T_NATIVE_SHORT);
   printf("using memory type H5T_NATIVE_SHORT\n");
  } else if (size <= sizeof(int)) {
   m_type = H5Tcopy(H5T_NATIVE_INT);
   printf("using memory type H5T_NATIVE_INT\n");
  } else if (size <= sizeof(long)) {
   m_type = H5Tcopy(H5T_NATIVE_LONG);
   printf("using memory type H5T_NATIVE_LONG\n");
  } else {
   m_type = H5Tcopy(H5T_NATIVE_LLONG);
   printf("using memory type H5T_NATIVE_LLONG\n");
  }
  
  H5Tset_sign(m_type, H5Tget_sign(f_type));
  break;
  
 case H5T_FLOAT:
/*
 * Use the smallest native floating point type available such that
 * its size is at least as large as the file type.  If there is not
 * native type large enough then use the largest native type.
 */
  if (size <= sizeof(float)) {
   m_type = H5Tcopy(H5T_NATIVE_FLOAT);
   printf("using memory type H5T_NATIVE_FLOAT\n");
  } else if (size <= sizeof(double)) {
   m_type = H5Tcopy(H5T_NATIVE_DOUBLE);
   printf("using memory type H5T_NATIVE_DOUBLE\n");
  } else {
   m_type = H5Tcopy(H5T_NATIVE_LDOUBLE);
   printf("using memory type H5T_NATIVE_LDOUBLE\n");
  }
  break;
  
 }
  
 return m_type;
}


