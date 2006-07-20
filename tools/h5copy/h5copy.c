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
#include "H5private.h"
#include <string.h>
#include <stdlib.h>

const char *progname="h5copy";
int   d_status;

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
      A pair of 2 HDF5 files (input and output). Each file name\n\
      is followed by a slash and an object name within the file\n\
   OPTIONS\n\
      -h               Print a usage message and exit\n\
      -v               Print information about OBJECTS and OPTIONS\n\
      -f               Flag type\n\n\
      Flag type is one of the following strings:\n\n\
      SHALLOW (Copy only immediate members for groups)\n\
      SOFT (Expand soft links into new objects)\n\
      EXT (Expand external links into new objects)\n\
      REF (Copy objects that are pointed by references)\n\
      ATTR (Copy object without copying attributes)\n\n\
      These flag types correspond to the following API symbols\n\n\
      H5G_COPY_SHALLOW_HIERARCHY_FLAG\n\
      H5G_COPY_EXPAND_SOFT_LINK_FLAG\n\
      H5G_COPY_EXPAND_EXT_LINK_FLAG\n\
      H5G_COPY_EXPAND_OBJ_REFERENCE_FLAG\n\
      H5G_COPY_WITHOUT_ATTR_FLAG\n");
}



/*-------------------------------------------------------------------------
 * Function: parse_flag
 *
 * Purpose: read the flag -f STRING
 *
 * STRING is one of the following (API symbol and description)
 *
 * SHALLOW  H5G_COPY_SHALLOW_HIERARCHY_FLAG:  Copy only immediate members for groups
 * SOFT     H5G_COPY_EXPAND_SOFT_LINK_FLAG:  Expand soft links into new objects
 * EXT      H5G_COPY_EXPAND_EXT_LINK_FLAG: Expand external links into new objects
 * REF      H5G_COPY_EXPAND_OBJ_REFERENCE_FLAG: Copy objects that are pointed by references
 * ATTR     H5G_COPY_WITHOUT_ATTR_FLAG Copy object without copying attributes 
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

 if (strcmp(str_flag,"SHALLOW")==0)
 {
  fla = H5G_COPY_SHALLOW_HIERARCHY_FLAG;
 } 
 else  if (strcmp(str_flag,"SOFT")==0)
 {
  fla = H5G_COPY_EXPAND_SOFT_LINK_FLAG;
 }
 else  if (strcmp(str_flag,"EXT")==0)
 {
  fla = H5G_COPY_EXPAND_EXT_LINK_FLAG;
 }
 else  if (strcmp(str_flag,"REF")==0)
 {
  fla = H5G_COPY_EXPAND_OBJ_REFERENCE_FLAG;
 }
 else  if (strcmp(str_flag,"ATTR")==0)
 {
  fla = H5G_COPY_WITHOUT_ATTR_FLAG;
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
 int          argno;
 static char  root_name[] = "/";
 char         *fname_src=NULL;
 char         *fname_dst=NULL;
 char         *oname_src=NULL;
 char         *oname_dst=NULL;
 char         *x;
 unsigned     flag=0;
 int          verbose=0;
 hid_t        pid; 
 char         str_flag[20];
 
 /* switches come before non-switch arguments */
 for (argno=1; argno<argc && '-'==argv[argno][0]; argno++) 
 {
  if (strcmp(argv[argno], "-h") == 0) 
  {
   usage();
  }
  else if (strcmp(argv[argno], "-v") == 0) 
  {
   verbose = 1;
  }
  else if (strcmp(argv[argno], "-f") == 0) 
  {
   /* parse flag */
   if (parse_flag(argv[argno+1],&flag)<0)
   {
    usage();
    exit(1);
   }
   strcpy(str_flag,argv[argno+1]);
   
   /* jump to next */
   ++argno;
  }
 }


 /* no more arguments remain */
 if (argno>=argc) 
 {
  usage();
  exit(1);
 }

/*-------------------------------------------------------------------------
 * 
 * each remaining argument is an hdf5 file name followed by an optional slash
 * and object name.
 *
 * Example: test1.h5/bar/baz
 *          \______/\______/
 *            file     obj
 *
 * The dichotomy is determined by calling H5Fopen() repeatedly until it
 * succeeds. The first call uses the entire name and each subsequent call
 * chops off the last component. If we reach the beginning of the name
 * then there must have been something wrong with the file (perhaps it
 * doesn't exist). 
 *
 *-------------------------------------------------------------------------*/

 fname_src = HDstrdup(argv[argno++]);

 while (fname_src && *fname_src) 
 {
  fid_src = h5tools_fopen(fname_src, NULL, NULL, 0, argc, argv);
  
  if (fid_src>=0) 
   break; /*success*/
  
  /* shorten the file name; lengthen the object name */
  x = oname_src;
  oname_src = strrchr(fname_src, '/');
  if (x) 
   *x = '/';
  if (!oname_src) 
   break;
  *oname_src = '\0';
 }
 if (oname_src) 
  oname_src++;
 if (!oname_src || !*oname_src) 
  oname_src = root_name;


/*-------------------------------------------------------------------------
 * last argument, same logic, but the file does not exist, so we attempt to
 * create one instead
 *-------------------------------------------------------------------------*/

 fname_dst = HDstrdup(argv[argno]);

 while (fname_dst && *fname_dst) 
 {
  H5E_BEGIN_TRY {
   fid_dst = H5Fcreate(fname_dst, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  } H5E_END_TRY;
  
  if (fid_dst>=0) 
   break; /*success*/
  
  /* shorten the file name; lengthen the object name */
  x = oname_dst;
  oname_dst = strrchr(fname_dst, '/');
  if (x) 
   *x = '/';
  if (!oname_dst) 
   break;
  *oname_dst = '\0';
 }
 if (oname_dst) 
  oname_dst++;
 if (!oname_dst || !*oname_dst) 
  oname_dst = NULL;

 
/*-------------------------------------------------------------------------
 * check destination name
 *-------------------------------------------------------------------------*/

 if (oname_dst==NULL) 
 {
  printf("Destination object name missing\n");
  usage();
  goto error;
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

 if (fid_src>0)
  H5Fclose(fid_src);
 if (fid_dst>0)
  H5Fclose(fid_dst);

 if (fname_src)
  free(fname_src);
 if (fname_dst)
  free(fname_dst);
 

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
 
 
 return 1;
}

