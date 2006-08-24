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


#include "hdf5.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include <string.h>
#include <stdlib.h>

const char *progname="h5copy";
int   d_status;

static void leave(int ret);

/* command-line options: short and long-named parameters */
static const char *s_opts = "hvf:Vi:o:s:d:";
static struct long_options l_opts[] = {
    { "help", no_arg, 'h' },
    { "verbose", no_arg, 'v' },
    { "flag", require_arg, 'f' },
    { "version", no_arg, 'V' },
    { "input", require_arg, 'i' },
    { "output", require_arg, 'o' },
    { "source", require_arg, 's' },
    { "destination", require_arg, 'd' },
    { NULL, 0, '\0' }
};

/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: Prints a usage message on stderr and then returns.
 *
 * Return: void
 *
 * Programmer: Pedro Vicente Nunes, 7/8/2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage (void)
{
 fprintf(stderr, "\
usage: h5copy [OPTIONS] [OBJECTS...]\n\
   OBJECTS\n\
      -i               input file name\n\
      -o               output file name\n\
      -s               source object name\n\
      -d               destination name\n\
   OPTIONS\n\
      -h               Print a usage message and exit\n\
      -v               Print information about OBJECTS and OPTIONS\n\
      -V               Print tool version\n\
      -f               Flag type\n\n\
      Flag type is one of the following strings:\n\n\
      shallow     Copy only immediate members for groups\n\
      soft        Expand soft links into new objects\n\
      ext         Expand external links into new objects\n\
      ref         Copy objects that are pointed by references\n\
      noattr      Copy object without copying attributes\n\
      allflags    Switches all flags from the default to the non-default setting\n\n\
      These flag types correspond to the following API symbols\n\n\
      H5G_COPY_SHALLOW_HIERARCHY_FLAG\n\
      H5G_COPY_EXPAND_SOFT_LINK_FLAG\n\
      H5G_COPY_EXPAND_EXT_LINK_FLAG\n\
      H5G_COPY_EXPAND_OBJ_REFERENCE_FLAG\n\
      H5G_COPY_WITHOUT_ATTR_FLAG\n\
      H5G_COPY_ALL\n");
}



/*-------------------------------------------------------------------------
 * Function: parse_flag
 *
 * Purpose: read the flag -f STRING
 *
 * STRING is one of the following (API symbol and description)
 *
 * shallow  H5G_COPY_SHALLOW_HIERARCHY_FLAG:  Copy only immediate members for groups
 * soft     H5G_COPY_EXPAND_SOFT_LINK_FLAG:  Expand soft links into new objects
 * ext      H5G_COPY_EXPAND_EXT_LINK_FLAG: Expand external links into new objects
 * ref      H5G_COPY_EXPAND_OBJ_REFERENCE_FLAG: Copy objects that are pointed by references
 * noattr   H5G_COPY_WITHOUT_ATTR_FLAG Copy object without copying attributes 
 * allflags Switches all flags from the default to the non-default setting 
 *
 * Return: Success:    SUCCEED
 *         Failure:    FAIL
 *
 * Programmer: Pedro Vicente Nunes, 7/8/2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


static int parse_flag(const char* str_flag, unsigned *flag)
{
 unsigned fla=0;

 if (strcmp(str_flag,"shallow")==0)
 {
  fla = H5G_COPY_SHALLOW_HIERARCHY_FLAG;
 } 
 else  if (strcmp(str_flag,"soft")==0)
 {
  fla = H5G_COPY_EXPAND_SOFT_LINK_FLAG;
 }
 else  if (strcmp(str_flag,"ext")==0)
 {
  fla = H5G_COPY_EXPAND_EXT_LINK_FLAG;
 }
 else  if (strcmp(str_flag,"ref")==0)
 {
  fla = H5G_COPY_EXPAND_OBJ_REFERENCE_FLAG;
 }
 else  if (strcmp(str_flag,"noattr")==0)
 {
  fla = H5G_COPY_WITHOUT_ATTR_FLAG;
 }
 else  if (strcmp(str_flag,"allflags")==0)
 {
  fla = H5G_COPY_ALL;
 }
 else
 {
  printf("Error in input flag\n");
  return -1;
 }

 *flag = fla;

 return 0;
}

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: main program
 *
 * Programmer: Pedro Vicente Nunes
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int
main (int argc, const char *argv[])
{
 hid_t        fid_src=-1;
 hid_t        fid_dst=-1;
 char         *fname_src=NULL;
 char         *fname_dst=NULL;
 char         *oname_src=NULL;
 char         *oname_dst=NULL;
 unsigned     flag=0;
 int          verbose=0;
 hid_t        pid; 
 char         str_flag[20];
 int          opt;

 /* initialize h5tools lib */
 h5tools_init();

 /* parse command line options */
 while ((opt = get_option(argc, argv, s_opts, l_opts)) != EOF) 
 {
  switch ((char)opt) 
  {
  case 'h':
   usage();
   leave(EXIT_SUCCESS);
   break;

  case 'V':
   print_version(progname);
   leave(EXIT_SUCCESS);
   break;

  case 'v':
   verbose = 1;
   break;

  case 'f':
   /* validate flag */
   if (parse_flag(opt_arg,&flag)<0)
   {
    usage();
    leave(EXIT_FAILURE);
   }
   strcpy(str_flag,opt_arg);
   break;

  case 'i':
   fname_src = strdup(opt_arg);
   break;

  case 'o':
   fname_dst = strdup(opt_arg);
   break;

  case 's':
   oname_src = strdup(opt_arg);
   break;

  case 'd':
   oname_dst = strdup(opt_arg);
   break;
   
  case '?':
  default:
   usage();
   leave(EXIT_FAILURE);
  }
 }

/*-------------------------------------------------------------------------
 * check for missing file/object names
 *-------------------------------------------------------------------------*/

 if (fname_src==NULL) 
 {
  printf("Input file name missing\n");
  usage();
  leave(EXIT_FAILURE);
 }

 if (fname_dst==NULL) 
 {
  printf("Output file name missing\n");
  usage();
  leave(EXIT_FAILURE);
 }

 if (oname_src==NULL) 
 {
  printf("Input object name missing\n");
  usage();
  leave(EXIT_FAILURE);
 }

 if (oname_dst==NULL) 
 {
  printf("Destination object name missing\n");
  usage();
  leave(EXIT_FAILURE);
 }


/*-------------------------------------------------------------------------
 * open input file
 *-------------------------------------------------------------------------*/
 
  fid_src = h5tools_fopen(fname_src, NULL, NULL, 0, argc, argv);

/*-------------------------------------------------------------------------
 * test for error in opening input file
 *-------------------------------------------------------------------------*/
 if (fid_src==-1)
 {
  printf("Could not open input file <%s>...Exiting\n",fname_src);
  if (fname_src)
   free(fname_src);
  leave(EXIT_FAILURE);
 }

/*-------------------------------------------------------------------------
 * open output file
 *-------------------------------------------------------------------------*/

 fid_dst = H5Fcreate(fname_dst, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

/*-------------------------------------------------------------------------
 * test for error in opening output file
 *-------------------------------------------------------------------------*/
 if (fid_dst==-1)
 {
  printf("Could not open output file <%s>...Exiting\n",fname_dst);
  if (fname_src)
   free(fname_src);
  if (fname_dst)
   free(fname_dst);
  leave(EXIT_FAILURE);
 }
  
/*-------------------------------------------------------------------------
 * print some info
 *-------------------------------------------------------------------------*/
 
 if (verbose)
 {
  printf("Copying file <%s> and object <%s> to file <%s> and object <%s>\n",
   fname_src,
   oname_src,
   fname_dst,
   oname_dst);
  if (flag)
   printf("Using %s flag\n", str_flag);
 }

 
/*-------------------------------------------------------------------------
 * create a property list for copy
 *-------------------------------------------------------------------------*/
 
 /* create property to pass copy options */
 if ( (pid = H5Pcreate(H5P_OBJECT_COPY)) < 0) 
  goto error;
 
 /* set options for object copy */
 if (flag)
 {
  if ( H5Pset_copy_object(pid, flag) < 0) 
   goto error;
 }


/*-------------------------------------------------------------------------
 * do the copy
 *-------------------------------------------------------------------------*/

  if (H5Gcopy(fid_src,        /* Source file or group identifier */
              oname_src,      /* Name of the source object to be copied */
              fid_dst,        /* Destination file or group identifier  */
              oname_dst,      /* Name of the destination object  */
              pid,            /* Properties which apply to the copy   */
              H5P_DEFAULT)<0) /* Properties which apply to the new hard link */              
              goto error;

  
 
 /* close property */
 if (H5Pclose(pid)<0)
  goto error;

 /* close files */
 if (H5Fclose(fid_src)<0)
  goto error;
 if (H5Fclose(fid_dst)<0)
  goto error;

 if (fname_src)
  free(fname_src);
 if (fname_dst)
  free(fname_dst);
 if (oname_dst)
  free(oname_dst);
 if (oname_src)
  free(oname_src);

 h5tools_close();
 
 return 0;

error:
 printf("Error in copy...Exiting\n");
 H5E_BEGIN_TRY {
  H5Pclose(pid);
  H5Fclose(fid_src);
  H5Fclose(fid_dst);
  } H5E_END_TRY;
 if (fname_src)
  free(fname_src);
 if (fname_dst)
  free(fname_dst);
 if (oname_dst)
  free(oname_dst);
 if (oname_src)
  free(oname_src);

 h5tools_close();
 
 return 1;
}


/*-------------------------------------------------------------------------
 * Function:    leave
 *
 * Purpose:     Shutdown MPI & HDF5 and call exit()
 *
 * Return:      Does not return
 *
 * Programmer:  Quincey Koziol
 *              Saturday, 31. January 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
leave(int ret)
{
 h5tools_close();
 
 exit(ret);
}