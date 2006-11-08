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
#include "h5tools_utils.h"
#include "h5repack.h"

/* module-scoped variables */
const char *progname = "h5repack";
int d_status = EXIT_SUCCESS;


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
void usage(void)
{
 printf("usage: h5repack -i input -o output [-h] [-v] [-f FILTER] [-l LAYOUT] [-n] [-m size] [-e file]\n");
 printf("\n");
 printf("-i input       Input HDF5 File\n");
 printf("-o output      Output HDF5 File\n");
 printf("[-h]           Print this message\n");
 printf("[-v]           Verbose mode\n");
 printf("[-n]           Use a native HDF5 type when repacking. Default is the file type\n");
 printf("[-m size]      Do not apply the filter to objects smaller than size\n");
 printf("[-e file]      Name of file with the -f and -l options\n");
 printf("[-f FILTER]    Filter type\n");
 printf("[-l LAYOUT]    Layout type\n");
 printf("\n");
 printf("FILTER is a string with the format:\n");
 printf("\n");
 printf("    <list of objects>:<name of filter>=<filter parameters>\n");
 printf("\n");
 printf("    <list of objects> is a comma separated list of object names, meaning apply\n");
 printf("      compression only to those objects. If no names are specified, the filter\n");
 printf("      is applied to all objects\n");
 printf("    <name of filter> can be:\n");
 printf("      GZIP, to apply the HDF5 GZIP filter (GZIP compression)\n");
 printf("      SZIP, to apply the HDF5 SZIP filter (SZIP compression)\n");
 printf("      SHUF, to apply the HDF5 shuffle filter\n");
 printf("      FLET, to apply the HDF5 checksum filter\n");
 printf("      NBIT, to apply the HDF5 NBIT filter (NBIT compression)\n");
 printf("      SOFF, to apply the HDF5 Scale/Offset filter\n");
 printf("      NONE, to remove all filters\n");
 printf("    <filter parameters> is optional filter parameter information\n");
 printf("      GZIP=<deflation level> from 1-9\n");
 printf("      SZIP=<pixels per block,coding> pixels per block is a even number in\n");
 printf("            2-32 and coding method is either EC or NN\n");
 printf("      SHUF (no parameter)\n");
 printf("      FLET (no parameter)\n");
 printf("      NBIT (no parameter)\n");
 printf("      SOFF=<scale_factor,scale_type> scale_factor is an integer and scale_type\n");
 printf("            is either IN or DS\n");
 printf("      NONE (no parameter)\n");
 printf("\n");
 printf("LAYOUT is a string with the format:\n");
 printf("\n");
 printf("    <list of objects>:<layout type>=<layout parameters>\n");
 printf("\n");
 printf("    <list of objects> is a comma separated list of object names, meaning that\n");
 printf("      layout information is supplied for those objects. If no names are\n");
 printf("      specified, the layout type is applied to all objects\n");
 printf("    <layout type> can be:\n");
 printf("      CHUNK, to apply chunking layout\n");
 printf("      COMPA, to apply compact layout\n");
 printf("      CONTI, to apply continuous layout\n");
 printf("    <layout parameters> is optional layout information\n");
 printf("      CHUNK=DIM[xDIM...xDIM], the chunk size of each dimension\n");
 printf("      COMPA (no parameter)\n");
 printf("      CONTI (no parameter)\n");
 printf("\n");
 printf("Examples of use:\n");
 printf("\n");
 printf("1) h5repack -v -i file1 -o file2 -f GZIP=1\n");
 printf("\n");
 printf("   GZIP compression with level 1 to all objects\n");
 printf("\n");
 printf("2) h5repack -v -i file1 -o file2 -f A:SZIP=8,NN\n");
 printf("\n");
 printf("   SZIP compression with 8 pixels per block and NN coding method to object A\n");
 printf("\n");
 printf("3) h5repack -v -i file1 -o file2 -l A,B:CHUNK=20x10 -f C,D,F:NONE\n");
 printf("\n");
 printf("   Chunked layout, with a layout size of 20x10, to objects A and B\n");
 printf("   and remove filters to objects C, D, F\n");
 printf("\n");
}

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: h5repack main program
 *
 * Return: 1, error, 0, no error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 * Modifications: 
 *  July 2004: Introduced the extra EC or NN option for SZIP
 *  October 2006: Added a new switch -n, that allows to write the dataset 
 *                using a native type. The default to write is the file type.
 *
 *-------------------------------------------------------------------------
 */


int main(int argc, char **argv)
{
 char          *infile  = NULL;
 char          *outfile = NULL;
 pack_opt_t    options;            /*the global options */
 int           i, ret;

 /* initialize options  */
 h5repack_init (&options,0);

 if (argc<2)
 {
  usage();
  exit(1);
 }

 for ( i = 1; i < argc; i++)
 {
  if (strcmp(argv[i], "-h") == 0) {
    usage();
    exit(0);
  }
  if (strcmp(argv[i], "-i") == 0) {
   infile = argv[++i];
  }
  else if (strcmp(argv[i], "-o") == 0) {
   outfile = argv[++i];
  }
  else if (strcmp(argv[i], "-v") == 0) {
   options.verbose = 1;
  }
  else if (strcmp(argv[i], "-f") == 0) {

   /* add the -f filter option */
   if (h5repack_addfilter(argv[i+1],&options)<0)
   {
    error_msg(progname, "in parsing filter\n");
    exit(1);
   }

   /* jump to next */
   ++i;
  }
  else if (strcmp(argv[i], "-l") == 0) {

   /* parse the -l layout option */
   if (h5repack_addlayout(argv[i+1],&options)<0)
   {
    error_msg(progname, "in parsing layout\n");
    exit(1);
   }

   /* jump to next */
   ++i;
  }

  else if (strcmp(argv[i], "-m") == 0) {
   options.threshold = parse_number(argv[i+1]);
   if ((int)options.threshold==-1) {
    error_msg(progname, "invalid treshold size <%s>\n",argv[i+1]);
    exit(1);
   }
   ++i;
  }

  else if (strcmp(argv[i], "-e") == 0) {
   read_info(argv[++i],&options);
  }
  else if (strcmp(argv[i], "-n") == 0) {
   options.use_native = 1;
  }

  else if (argv[i][0] == '-') {
   error_msg(progname, " - is not a valid argument\n");
   usage();
   exit(1);
  }
 }

 if (infile == NULL || outfile == NULL)
 {
  error_msg(progname, "file names missing\n");
  usage();
  exit(1);
 }

 /* pack it */
 ret=h5repack(infile,outfile,&options);

 /* free tables */
 h5repack_end(&options);

 if (ret==-1)
  return 1;
 else
  return 0;
}

