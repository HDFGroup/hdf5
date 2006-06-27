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
#include "h5diff.h"
#include "h5diff_common.h"

/*-------------------------------------------------------------------------
 * Function: parse_input
 *
 * Purpose: parse command line input
 *
 *-------------------------------------------------------------------------
 */

void parse_input(int argc, const char* argv[], const char** fname1, const char** fname2,
                 const char** objname1, const char** objname2, diff_opt_t* options)
{
 int        i;
 const char *s = NULL;

 /* process the command-line */
 memset(options, 0, sizeof (diff_opt_t));

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
  *fname1 = argv[1];
  *fname2 = argv[2];
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
     options->m_verbose = 1;
     break;
    case 'q':
     /* use quiet mode; supress the message "0 differences found" */
     options->m_quiet = 1;
     break;
    case 'r':
     options->m_report = 1;
     break;
    case 'd':
     /* if it is not another option */
     if ( i<argc-1 &&'-' != argv[i+1][0] )
     {
      options->d=1;
      if ( check_f_input(argv[i+1])==-1)
      {
       printf("<-d %s> is not a valid option\n", argv[i+1] );
       usage();
      }
      options->delta = (float) atof(argv[i+1]);
      i++; /* go to next */
     }
     else
     {
      printf("Not a valid -d option\n");
      usage();
     }
     break;
    case 'p':
     if ( i<argc-1 &&'-' !=argv[i+1][0] )
     {
      options->p=1;
      if ( check_f_input(argv[i+1])==-1)
      {
       printf("<-p %s> is not a valid option\n", argv[i+1] );
       usage();
      }
      options->percent = (float) atof(argv[i+1]);
      i++; /* go to next */
     }
     else
     {
      printf("Not a valid -p option\n");
      usage();
     }
     break;
    case 'n':
     if ( i<argc-1 && '-' !=argv[i+1][0] )
     {
      options->n=1;
      if ( check_n_input(argv[i+1])==-1)
      {
       printf("<-n %s> is not a valid option\n", argv[i+1] );
       usage();
      }
      options->count = atol(argv[i+1]);
      i++; /* go to next */
     }
     else
     {
      printf("Not a valid -n option\n");
      usage();
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
    if ( *objname1==NULL )
     *objname1 = argv[i];
    if ( *objname2==NULL )
    {
     /* check if we have a second object name */
     if ( i+1<argc && '-' !=argv[i+1][0] ) {
      /* yes */
      *objname2 = argv[i+1];
      i++; /* go to next */
     }
     else
      /* no */
      *objname2 = *objname1;
    } /*objname2*/
   } /*else*/
  } /*else*/

 }/*for*/
}

/*-------------------------------------------------------------------------
 * Function: print_results
 *
 * Purpose: print several information messages
 *
 *-------------------------------------------------------------------------
 */

void  print_results(diff_opt_t* options)
{
 if (options->m_quiet || options->err_stat)
  return;

 if (options->cmn_objs==0)
 {
  printf("No common objects found. Files are not comparable.\n");
  if (!options->m_verbose)
   printf("Use -v for a list of objects.\n");
 }

 if (options->not_cmp==1)
 {
  printf("--------------------------------\n");
  printf("Some objects are not comparable\n");
  printf("--------------------------------\n");
  if (!options->m_verbose)
   printf("Use -v for a list of objects.\n");
 }

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
 * Purpose: check for a valid floating point input
 *
 * Return: 1 for ok, -1 for fail
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
 double x;

 /*
 the atof return value on a hexadecimal input is different
 on some systems; we do a character check for this
 */
 if (strlen(str)>2 && str[0]=='0' && str[1]=='x')
  return -1;

 x=atof(str);
 if (x==0)
  return -1;

 return 1;
}

/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: print a usage message
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */
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
 printf("[-r]              Report mode. Print the differences\n");
 printf("[-v]              Verbose mode. Print the differences, list of objects, warnings\n");
 printf("[-q]              Quiet mode. Do not do output\n");
 printf("[-n count]        Print difference up to count number\n");
 printf("[-d delta]        Print difference when it is greater than limit delta\n");
 printf("[-p relative]     Print difference when it is greater than a relative limit\n");
 printf("\n");
 printf("Items in [] are optional\n");
 printf("[obj1] and [obj2] are HDF5 objects (datasets, groups, datatypes or links)\n");
 printf("The 'count' value must be a positive integer\n");
 printf("The 'delta' and 'relative' values must be positive numbers\n");
 printf("The -d compare criteria is |a - b| > delta\n");
 printf("The -p compare criteria is |1 - b/a| > relative\n");
 printf("\n");
 printf("h5diff has four modes of output:\n");
 printf(" Normal mode: print the number of differences found and where they occured\n");
 printf(" Report mode: print the above plus the differences\n");
 printf(" Verbose mode: print the above plus a list of objects and warnings\n");
 printf(" Quiet mode: do not print output (h5diff always returns an exit code of 1 when differences are found)\n");
 printf("\n");
 printf("Examples of use:\n");
 printf("\n");
 printf("1) h5diff file1 file2 /g1/dset1 /g1/dset2\n");
 printf("\n");
 printf("   Compares object '/g1/dset1' in file1 with '/g1/dset2' in file2\n");
 printf("\n");
 printf("2) h5diff file1 file2 /g1/dset1\n");
 printf("\n");
 printf("   Compares object '/g1/dset1' in both files\n");
 printf("\n");
 printf("3) h5diff file1 file2\n");
 printf("\n");
 printf("   Compares all objects in both files\n");
 printf("\n");
 printf("Note)  file1 and file2 can be the same file. Use\n");
 printf("\n");
 printf("   h5diff file1 file1 /g1/dset1 /g1/dset2\n");
 printf("\n");
 printf("   to compare '/g1/dset1' and '/g1/dset2' in the same file\n");
 printf("\n");
 printf("If no objects are specified, h5diff only compares objects with the same ");
 printf("absolute path in both files. The compare criteria is: ");
 printf("1) datasets: numerical array differences 2) groups: name string difference ");
 printf("3) datatypes: the return value of H5Tequal 2) links: name string difference of the linked value\n");
#ifdef H5_HAVE_PARALLEL
 if(g_Parallel)
  h5diff_exit(0);
 else
#endif
  exit(0);
}

/*-------------------------------------------------------------------------
 * Function: h5diff_exit
 *
 * Purpose: dismiss phdiff worker processes and exit
 *
 * Return: none
 *
 * Programmer: Albert Cheng
 * Date: Feb 6, 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void h5diff_exit(int status)
{
#ifdef H5_HAVE_PARALLEL
 /* if in parallel mode, dismiss workers, close down MPI, then exit */
 if((g_nTasks > 1) && g_Parallel) {
  phdiff_dismiss_workers();
  MPI_Barrier(MPI_COMM_WORLD);
 }
 if(g_Parallel)
  MPI_Finalize();
#endif
 exit(status);
}

