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

#include "h5repack.h"
#include "h5repack_parse.h"
#include <stdlib.h>


static void usage();


/*
Examples of use:
-v -i file1.h5 -o file2.h5 -t "dataset:GZIP 6" -c "dataset:2x2"
-v -i file1.h5 -o file2.h5 -t "GZIP 6" 
*/


int main(int argc, char **argv)
{
 char          *infile  = NULL;
 char          *outfile = NULL;
 packoptions_t options;            /*the global options */
 int           i;

 /* initialize options  */
 h5repack_init (&options,0);

 for ( i = 1; i < argc; i++) 
 {
  if (strcmp(argv[i], "-i") == 0) {
   infile = argv[++i];
  }
  else if (strcmp(argv[i], "-o") == 0) {       
   outfile = argv[++i]; 
  }
  else if (strcmp(argv[i], "-v") == 0) {       
   options.verbose = 1;
  }
  else if (strcmp(argv[i], "-t") == 0) {  
   
   /* add the -t option */
   h5repack_addcomp(argv[i+1],&options);

   /* jump to next */
   ++i;
  }
  else if (strcmp(argv[i], "-c") == 0) {       
   
   /* parse the -c option */
   h5repack_addchunk(argv[i+1],&options);
   
   /* jump to next */
   ++i;
  }

  else if (strcmp(argv[i], "-m") == 0) {       
   
   options.threshold = parse_number(argv[i+1]);
   if (options.threshold==-1) {
    printf("Error: Invalid treshold size <%s>\n",argv[i+1]);
    exit(1);
   }
   ++i;
  }
  
  else if (strcmp(argv[i], "-f") == 0) {       
   read_info(argv[++i],&options);
  }
  
  else if (argv[i][0] == '-') {
   usage();
  }
 }

 if (infile == NULL || outfile == NULL) 
  usage();
 
 /* pack it */
 h5repack(infile,outfile,&options);
 
 /* free tables */
 h5repack_end(&options);

 return 0;
}


/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: print usage
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */

static 
void usage()
{

 exit(1);
}



