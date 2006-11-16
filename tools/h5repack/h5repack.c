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

extern char  *progname;


/*-------------------------------------------------------------------------
 * File: h5repack.c
 * Purpose: Public API functions
 *-------------------------------------------------------------------------
 */

static int check_options(pack_opt_t *options);


/*-------------------------------------------------------------------------
 * Function: aux_initglb_filter
 *
 * Purpose: auxiliary function, initialize the options global filter
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */
static void aux_initglb_filter(pack_opt_t *options)
{
 int k;
 options->filter_g.filtn  = -1;
 for ( k=0; k<CDVALUES; k++)
  options->filter_g.cd_values[k] = -1;
}


/*-------------------------------------------------------------------------
 * Function: h5repack
 *
 * Purpose: locate all high-level HDF5 objects in the file
 *  and compress/chunk them using options
 *
 * Algorythm: 2 traversals are made to the file; the 1st builds a list of
 *  the objects, the 2nd makes a copy of them, using the options;
 *  the reason for the 1st traversal is to check for invalid
 *  object name requests
 *
 * Return: 0, ok, -1, fail
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: September, 22, 2003
 *
 *-------------------------------------------------------------------------
 */
int h5repack(const char* infile,
             const char* outfile,
             pack_opt_t *options)
{
 /* check input */
 if (check_options(options)<0)
  return -1;

 /* check for objects in input that are in the file */
 if (check_objects(infile,options) < 0)
  return -1;

 /* copy the objects  */
 if (copy_objects(infile,outfile,options) < 0)
  return -1;


 return 0;
}



/*-------------------------------------------------------------------------
 * Function: h5repack_init
 *
 * Purpose: initialize options
 *
 * Return: 0, ok, -1, fail
 *
 *-------------------------------------------------------------------------
 */

int h5repack_init (pack_opt_t *options,
                   int verbose)
{
 memset(options,0,sizeof(pack_opt_t));
 options->threshold = 1024;
 options->verbose   = verbose;
 return (options_table_init(&(options->op_tbl)));
}

/*-------------------------------------------------------------------------
 * Function: h5repack_end
 *
 * Purpose: free options table
 *
 *-------------------------------------------------------------------------
 */

int h5repack_end  (pack_opt_t *options)
{
 return options_table_free(options->op_tbl);
}

/*-------------------------------------------------------------------------
 * Function: h5repack_addfilter
 *
 * Purpose: add a compression -f option to table
 *   Example: -f dset:GZIP=6
 *
 * Return: 0, ok, -1, fail
 *
 *-------------------------------------------------------------------------
 */

int h5repack_addfilter(const char* str,
                       pack_opt_t *options)
{
 obj_list_t      *obj_list=NULL; /*one object list for the -f and -c option entry */
 filter_info_t   filt;           /*filter info for the current -f option entry */
 int             n_objs;         /*number of objects in the current -f or -c option entry */

 if (options->all_filter==1){
  error_msg(progname, "invalid compression input: 'all' option is present \
   with other objects <%s>\n",str);
  return -1;
 }

 /* parse the -f option */
 obj_list=parse_filter(str,&n_objs,&filt,options);
 if (obj_list==NULL)
 {
  return -1;
 }

 if (options->all_filter==1)
 {
  /* if we are compressing all set the global filter type */
  aux_initglb_filter(options);
  options->filter_g=filt;
 }

 if (options->all_filter==0)
  options_add_filter(obj_list,n_objs,filt,options->op_tbl);

 free(obj_list);
 return 0;
}


/*-------------------------------------------------------------------------
 * Function: h5repack_addlayout
 *
 * Purpose: add a layout option
 *
 * Return: 0, ok, -1, fail
 *
 *-------------------------------------------------------------------------
 */


int h5repack_addlayout(const char* str,
                       pack_opt_t *options)
{

 obj_list_t  *obj_list=NULL;     /*one object list for the -t and -c option entry */
 int         n_objs;             /*number of objects in the current -t or -c option entry */
 pack_info_t pack;               /*info about layout to extract from parse */
 int         j;

 init_packobject(&pack);

 if (options->all_layout==1){
  error_msg(progname, "invalid layout input: 'all' option \
   is present with other objects <%s>\n",str);
  return -1;
 }

 /* parse the layout option */
 obj_list=parse_layout(str,&n_objs,&pack,options);
 if (obj_list==NULL)
  return -1;

 /* set global layout option */
 if (options->all_layout==1 )
 {
  options->layout_g=pack.layout;
  if (pack.layout==H5D_CHUNKED)
  {
   /* -2 means the NONE option, remove chunking
      and set the global layout to contiguous */
   if (pack.chunk.rank==-2)
   {
    options->layout_g = H5D_CONTIGUOUS;
   }
   /* otherwise set the global chunking type */
   else
   {
    options->chunk_g.rank=pack.chunk.rank;
    for (j = 0; j < pack.chunk.rank; j++)
     options->chunk_g.chunk_lengths[j] = pack.chunk.chunk_lengths[j];
   }
  }
 }

 if (options->all_layout==0)
  options_add_layout(obj_list,
   n_objs,
   &pack,
   options->op_tbl);

 free(obj_list);
 return 0;
}


/*-------------------------------------------------------------------------
 * Function: check_options
 *
 * Purpose: print options, checks for invalid options
 *
 * Return: void, return -1 on error
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: September, 22, 2003
 *
 *-------------------------------------------------------------------------
 */
static int check_options(pack_opt_t *options)
{
 unsigned int   i;
 int            k, j, has_cp=0, has_ck=0;
 char           slayout[30];

/*-------------------------------------------------------------------------
 * objects to layout
 *-------------------------------------------------------------------------
 */
 if (options->verbose)
 {
  printf("Objects to modify layout are...\n");
  if (options->all_layout==1)  {
   switch (options->layout_g)
   {
   case H5D_COMPACT:
    strcpy(slayout,"compact");
    break;
   case H5D_CONTIGUOUS:
    strcpy(slayout,"contiguous");
    break;
   case H5D_CHUNKED:
    strcpy(slayout,"chunked");
    break;
   default:
    strcpy(slayout,"unknown");
    break;
   }
   printf(" Apply %s layout to all\n", slayout);
   if (H5D_CHUNKED==options->layout_g) {
    printf("with dimension [");
    for ( j = 0; j < options->chunk_g.rank; j++)
     printf("%d ",(int)options->chunk_g.chunk_lengths[j]);
    printf("]\n");
   }
  }
 }/* verbose */

 for ( i = 0; i < options->op_tbl->nelems; i++)
 {
  char* name=options->op_tbl->objs[i].path;

  if (options->op_tbl->objs[i].chunk.rank>0)
  {
   if (options->verbose){
    printf(" <%s> with chunk size ",name);
    for ( k = 0; k < options->op_tbl->objs[i].chunk.rank; k++)
     printf("%d ",(int)options->op_tbl->objs[i].chunk.chunk_lengths[k]);
    printf("\n");
   }
   has_ck=1;
  }
  else if (options->op_tbl->objs[i].chunk.rank==-2)
  {
   if (options->verbose)
    printf(" <%s> %s\n",name,"NONE (contigous)");
   has_ck=1;
  }
 }

 if (options->all_layout==1 && has_ck){
  error_msg(progname, "invalid chunking input: 'all' option\
   is present with other objects\n");
  return -1;
 }

/*-------------------------------------------------------------------------
 * objects to filter
 *-------------------------------------------------------------------------
 */

 if (options->verbose)
 {
  printf("Objects to apply filter are...\n");
  if (options->all_filter==1)
  {
   H5Z_filter_t filtn=options->filter_g.filtn;
   switch (filtn)
   {
   case H5Z_FILTER_NONE:
     printf(" Uncompress all\n");
    break;
   case H5Z_FILTER_SHUFFLE:
   case H5Z_FILTER_FLETCHER32:
     printf(" All with %s\n",get_sfilter(filtn));
    break;
   case H5Z_FILTER_SZIP:
   case H5Z_FILTER_DEFLATE:
     printf(" All with %s, parameter %d\n",
      get_sfilter(filtn),
      options->filter_g.cd_values[0]);
    break;
   };
  }
 } /* verbose */

 for ( i = 0; i < options->op_tbl->nelems; i++)
 {
  pack_info_t pack  = options->op_tbl->objs[i];
  char*       name  = pack.path;

  for ( j=0; j<pack.nfilters; j++)
  {
   if (options->verbose)
   {
    printf(" <%s> with %s filter\n",
     name,
     get_sfilter(pack.filter[j].filtn));
   }

   has_cp=1;

  } /* j */
 } /* i */

 if (options->all_filter==1 && has_cp){
  error_msg(progname, "invalid compression input: 'all' option\
   is present with other objects\n");
  return -1;
 }


 return 0;
}

/*-------------------------------------------------------------------------
 * Function: read_info
 *
 * Purpose: read comp and chunk options from file
 *
 * Return: void, exit on error
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: September, 22, 2003
 *
 *-------------------------------------------------------------------------
 */

void read_info(const char *filename,
               pack_opt_t *options)
{

 char stype[10];
 char comp_info[1024];
 FILE *fp;
 char c;
 int  i, rc=1;
 char  *srcdir = getenv("srcdir"); /* the source directory */
 char  data_file[512]="";          /* buffer to hold name of existing file */

 /* compose the name of the file to open, using the srcdir, if appropriate */
 if (srcdir){
  strcpy(data_file,srcdir);
  strcat(data_file,"/");
 }
 strcat(data_file,filename);


 if ((fp = fopen(data_file, "r")) == (FILE *)NULL) {
  error_msg(progname, "cannot open options file %s", filename);
  exit(1);
 }

 /* cycle until end of file reached */
 while( 1 )
 {
  rc=fscanf(fp, "%s", stype);
  if (rc==-1)
   break;

 /*-------------------------------------------------------------------------
  * filter
  *-------------------------------------------------------------------------
  */
  if (strcmp(stype,"-f") == 0) {

   /* find begining of info */
   i=0; c='0';
   while( c!=' ' )
   {
    fscanf(fp, "%c", &c);
    if (feof(fp)) break;
   }
   c='0';
   /* go until end */
   while( c!=' ' )
   {
    fscanf(fp, "%c", &c);
    comp_info[i]=c;
    i++;
    if (feof(fp)) break;
    if (c==10 /*eol*/) break;
   }
   comp_info[i-1]='\0'; /*cut the last " */

   if (h5repack_addfilter(comp_info,options)==-1){
    error_msg(progname, "could not add compression option\n");
    exit(1);
   }
  }
 /*-------------------------------------------------------------------------
  * layout
  *-------------------------------------------------------------------------
  */
  else if (strcmp(stype,"-l") == 0) {

   /* find begining of info */
   i=0; c='0';
   while( c!=' ' )
   {
    fscanf(fp, "%c", &c);
    if (feof(fp)) break;
   }
   c='0';
   /* go until end */
   while( c!=' ' )
   {
    fscanf(fp, "%c", &c);
    comp_info[i]=c;
    i++;
    if (feof(fp)) break;
    if (c==10 /*eol*/) break;
   }
   comp_info[i-1]='\0'; /*cut the last " */

   if (h5repack_addlayout(comp_info,options)==-1){
    error_msg(progname, "could not add chunck option\n");
    exit(1);
   }
  }
 /*-------------------------------------------------------------------------
  * not valid
  *-------------------------------------------------------------------------
  */
  else {
   error_msg(progname, "bad file format for %s", filename);
   exit(1);
  }
 }

 fclose(fp);
 return;
}


