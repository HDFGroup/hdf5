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

/* Due to alignment issue in Alpha clusters, options must be declared here
 * not as a local variable in main().
 */
diff_opt_t  options = {0,0,0,0,0,0,0,0};

static void usage(void);
static int check_n_input( const char* );
static int check_f_input( const char* );


/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: H5diff main program
 *
 * Return: An  exit status of 0 means no differences were found, 1 means some 
 *   differences were found.
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
 const char *fname1 = NULL;
 const char *fname2 = NULL;
 const char *objname1  = NULL;
 const char *objname2  = NULL;
 int        nfound=0, ret;

/*-------------------------------------------------------------------------
 * initial check of command line options
 *-------------------------------------------------------------------------
 */

 if ( argc==2 && (strcmp("-h",argv[1])==0) ) 
  usage();
  
 if ( argc<3 ) 
 {
  printf("Number of arguments is only %d\n", argc );
  usage();
 }

/*-------------------------------------------------------------------------
 * file names are first
 *-------------------------------------------------------------------------
 */
 
 if ( argc>=3 )
 {
  fname1 = argv[1];
  fname2 = argv[2];
 }

 
/*-------------------------------------------------------------------------
 * parse command line options
 *-------------------------------------------------------------------------
 */
 for (i=3; i<argc ; i++) 
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
    break;
    case 'h': 
     usage();
     break;
    case 'v': 
     options.verbose = 1;
     break;
    break;
    case 'r': 
     options.r = 1;
     break;
    case 'd': 
     /* if it is not another option */
     if ( i<argc-1 &&'-' != argv[i+1][0] )
     {
      options.d=1;
      if ( check_f_input(argv[i+1])==-1)
      {
       printf("<-d %s> is not a valid option\n", argv[i+1] );
       usage();
      }
      options.delta = atof(argv[i+1]);
      i++; /* go to next */
     }
     else
     {
      printf("<-d %s> is not a valid option\n", argv[i+1] );
      usage();
     }
     break;
    case 'p': 
     if ( i<argc-1 &&'-' !=argv[i+1][0] )
     {
      options.p=1;
      if ( check_f_input(argv[i+1])==-1)
      {
       printf("<-p %s> is not a valid option\n", argv[i+1] );
       usage();
      }
      options.percent = atof(argv[i+1]);
      i++; /* go to next */
     }
     break;
    case 'n': 
     if ( i<argc-1 && '-' !=argv[i+1][0] )
     {
      options.n=1;
      if ( check_n_input(argv[i+1])==-1)
      {
       printf("<-n %s> is not a valid option\n", argv[i+1] );
       usage();
      }
      options.count = atoi(argv[i+1]);
      i++; /* go to next */
     }
     break;
    } /*switch*/
   } /*for*/ 
  } /*if*/
  
  else /* not single-letter switches */
   
  {
   /* check if it is not a -d, -p parameter */
   if ( '-'==argv[i-1][0] && ('d'==argv[i-1][1] ||'p'==argv[i-1][1] ))
    continue;
   else
   {
    if ( objname1==NULL )
     objname1 = argv[i];
    if ( objname2==NULL )
    {
     /* check if we have a second object name */
     if ( i+1<argc && '-' !=argv[i+1][0] ) {
      /* yes */
      objname2 = argv[i+1];
      i++; /* go to next */
     }
     else
      /* no */
      objname2 = objname1;
    } /*objname2*/
   } /*else*/
  } /*else*/
  
 }/*for*/


/*-------------------------------------------------------------------------
 * print the command line options
 *-------------------------------------------------------------------------
 */
 
 if (options.verbose)
 {
  printf("$h5diff");
  for (i=1; i<argc ; i++) 
  {
   printf(" %s", argv[i] );
  }
  printf("\n");
 }

 

 nfound = h5diff(fname1,fname2,objname1,objname2,&options);
 printf("\n");
 ret= (nfound==0 ? 0 : 1 );
 return ret;
 
}


/*-------------------------------------------------------------------------
 * Function: check_n_input
 *
 * Purpose: check for valid input
 *
 * Return: 1 for ok, -1 for fail
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
static 
int check_n_input( const char *str )
{
 unsigned i;
 char c;

 for ( i = 0; i < strlen(str); i++)
 {
  c = str[i];
  if ( i==0 )
  {
   if ( c < 49 || c > 57  ) /* ascii values between 1 and 9 */
    return -1;
  }
  else
   if ( c < 48 || c > 57  ) /* 0 also */
    return -1;
 }
 return 1;
}

/*-------------------------------------------------------------------------
 * Function: check_f_input
 *
 * Purpose: check for valid input
 *
 * Return: 1 for ok, -1 for fail
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
static 
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
static 
void usage(void)
{
 printf("Usage: h5diff file1 file2 [OPTIONS] [obj1[obj2]] \n");
 printf("\n");
 printf("file1             File name of the first HDF5 file\n");
 printf("file2             File name of the second HDF5 file\n");
 printf("[obj1]            Name of an HDF5 object, in absolute path\n");
 printf("[obj2]            Name of an HDF5 object, in absolute path\n");
 printf("[OPTIONS] are:\n");
 printf("[-h]              Print out this information\n");
 printf("[-v]              Verbose mode\n");
 printf("[-r]              Print only what objects differ, not the differences\n");
 printf("[-n count]        Print difference up to count number\n");
 printf("[-d delta]        Print difference when it is greater than limit delta\n");
 printf("[-p relative]     Print difference when it is greater than a relative limit\n");
 printf("\n");
 printf("Items in [] are optional\n");
 printf("[obj1] and [obj2] are HDF5 objects (datasets, groups or datatypes)\n");
 printf("The 'count' value must be a positive integer\n");
 printf("The 'delta' and 'relative' values must be positive numbers\n");
 printf("The -d compare criteria is |a - b| > delta\n");
 printf("The -p compare criteria is |1 - b/a| > relative\n");
 printf("\n");
 printf("Examples:\n");
 printf("\n");
 printf("1) h5diff file1 file2 /a/b /a/c\n");
 printf("\n");
 printf("   Compares object '/a/b' in file1 with '/a/c' in file2\n");
 printf("\n");
 printf("2) h5diff file1 file2 /a/b\n");
 printf("\n");
 printf("   Compares object '/a/b' in both files\n");
 printf("\n");
 printf("3) h5diff file1 file2\n");
 printf("\n");
 printf("   Compares all objects in both files\n");
 printf("\n");
 exit(0);
}



