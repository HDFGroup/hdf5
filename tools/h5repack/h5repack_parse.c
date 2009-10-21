/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "h5repack.h"
#include "h5tools_utils.h"

extern char  *progname;

/*-------------------------------------------------------------------------
 * Function: parse_filter
 *
 * Purpose: read filter information
 *
 * Return: a list of names, the number of names and its compression type
 *
 * <name of filter> can be:
 *  GZIP, to apply the HDF5 GZIP filter (GZIP compression)
 *  SZIP, to apply the HDF5 SZIP filter (SZIP compression)
 *  SHUF, to apply the HDF5 shuffle filter
 *  FLET, to apply the HDF5 checksum filter
 *  NONE, to remove the filter
 *
 * Examples:
 * "GZIP=6"
 * "A,B:NONE"
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September, 23, 2003
 *
 *-------------------------------------------------------------------------
 */



obj_list_t* parse_filter(const char *str,
                         int *n_objs,
                         filter_info_t *filt,
                         pack_opt_t *options,
                         int *is_glb)
{
    unsigned    i, u;
    char        c;
    size_t      len=strlen(str);
    int         j, m, n, k, l, end_obj=-1, no_param=0;
    char        sobj[MAX_NC_NAME];
    char        scomp[10];
    char        stype[5];
    char        smask[3];
    obj_list_t* obj_list=NULL;
    unsigned    pixels_per_block;
    
    
    /* initialize compression  info */
    memset(filt,0,sizeof(filter_info_t));
    *is_glb = 0;
    
    /* check for the end of object list and number of objects */
    for ( i = 0, n = 0; i < len; i++)
    {
        c = str[i];
        if ( c==':' )
        {
            end_obj=i;
        }
        if ( c==',' )
        {
            n++;
        }
    }
    
    if (end_obj==-1) /* missing : */
    { 
        /* apply to all objects */
        options->all_filter=1;
        *is_glb = 1;
    }
    
    n++;
    obj_list = malloc(n*sizeof(obj_list_t));
    if (obj_list==NULL)
    {
        error_msg(progname, "could not allocate object list\n");
        return NULL;
    }
    *n_objs=n;
    
    /* get object list */
    for ( j = 0, k = 0, n = 0; j < end_obj; j++, k++)
    {
        c = str[j];
        sobj[k] = c;
        if ( c==',' || j==end_obj-1)
        {
            if ( c==',') sobj[k]='\0'; else sobj[k+1]='\0';
            strcpy(obj_list[n].obj,sobj);
            memset(sobj,0,sizeof(sobj));
            n++;
            k=-1;
        }
    }
    /* nothing after : */
    if (end_obj+1==(int)len)
    {
        if (obj_list) free(obj_list);
        error_msg(progname, "input Error: Invalid compression type in <%s>\n",str);
        exit(EXIT_FAILURE);
    }
    
    
    /* get filter additional parameters */
    m=0;
    for ( i=end_obj+1, k=0, j=0; i<len; i++,k++)
    {
        c = str[i];
        scomp[k]=c;
        if ( c=='=' || i==len-1)
        {
            if ( c=='=') /*one more parameter */
            {      
                scomp[k]='\0';     /*cut space */
                
               /*-------------------------------------------------------------------------
                * H5Z_FILTER_SZIP
                * szip has the format SZIP=<pixels per block,coding>
                * pixels per block is a even number in 2-32 and coding method is 'EC' or 'NN'
                * example SZIP=8,NN
                *-------------------------------------------------------------------------
                */
                if (strcmp(scomp,"SZIP")==0)
                {
                    l=-1; /* mask index check */
                    for ( m=0,u=i+1; u<len; u++,m++)
                    {
                        if (str[u]==',')
                        {
                            stype[m]='\0'; /* end digit of szip */
                            l=0;  /* start EC or NN search */
                            u++;  /* skip ',' */
                        }
                        c = str[u];
                        if (!isdigit(c) && l==-1){
                            if (obj_list) free(obj_list);
                            error_msg(progname, "compression parameter not digit in <%s>\n",str);
                            exit(EXIT_FAILURE);
                        }
                        if (l==-1)
                            stype[m]=c;
                        else
                        {
                            smask[l]=c;
                            l++;
                            if (l==2)
                            {
                                smask[l]='\0';
                                i=len-1; /* end */
                                (*n_objs)--; /* we counted an extra ',' */
                                if (strcmp(smask,"NN")==0)
                                    filt->cd_values[j++]=H5_SZIP_NN_OPTION_MASK;
                                else if (strcmp(smask,"EC")==0)
                                    filt->cd_values[j++]=H5_SZIP_EC_OPTION_MASK;
                                else
                                {
                                    error_msg(progname, "szip mask must be 'NN' or 'EC' \n");
                                    exit(EXIT_FAILURE);
                                }
                                
                                
                            }
                        }
                        
                    }  /* u */
                } /*if */
                
                 
                
                
               /*-------------------------------------------------------------------------
                * all other filters
                *-------------------------------------------------------------------------
                */
                
                else
                {
                    /* here we could have 1 or 2 digits  */
                    for ( m=0,u=i+1; u<len; u++,m++)
                    {
                        c = str[u];
                        if (!isdigit(c)){
                            if (obj_list) free(obj_list);
                            error_msg(progname, "compression parameter is not a digit in <%s>\n",str);
                            exit(EXIT_FAILURE);
                        }
                        stype[m]=c;
                    } /* u */
                    
                    stype[m]='\0';
                } /*if */
                
                
                
                filt->cd_values[j++]=atoi(stype);
                i+=m; /* jump */
   }
   else if (i==len-1) 
   { /*no more parameters */
       scomp[k+1]='\0';
       no_param=1;
   }

  /*-------------------------------------------------------------------------
   * translate from string to filter symbol
   *-------------------------------------------------------------------------
   */
   
  /*-------------------------------------------------------------------------
   * H5Z_FILTER_NONE
   *-------------------------------------------------------------------------
   */
   if (strcmp(scomp,"NONE")==0)
   {
       filt->filtn=H5Z_FILTER_NONE;
       filt->cd_nelmts = 0;
   }
   
  /*-------------------------------------------------------------------------
   * H5Z_FILTER_DEFLATE
   *-------------------------------------------------------------------------
   */
   else if (strcmp(scomp,"GZIP")==0)
   {
       filt->filtn=H5Z_FILTER_DEFLATE;
       filt->cd_nelmts = 1;
       if (no_param) 
       { /*no more parameters, GZIP must have parameter */
           if (obj_list) free(obj_list);
           error_msg(progname, "missing compression parameter in <%s>\n",str);
           exit(EXIT_FAILURE);
       }
   }
   
  /*-------------------------------------------------------------------------
   * H5Z_FILTER_SZIP
   *-------------------------------------------------------------------------
   */
   else if (strcmp(scomp,"SZIP")==0)
   {
       filt->filtn=H5Z_FILTER_SZIP;
       filt->cd_nelmts = 2;
       if (no_param) 
       { /*no more parameters, SZIP must have parameter */
           if (obj_list) free(obj_list);
           error_msg(progname, "missing compression parameter in <%s>\n",str);
           exit(EXIT_FAILURE);
       }
   }
   
   /*-------------------------------------------------------------------------
   * H5Z_FILTER_SHUFFLE
   *-------------------------------------------------------------------------
   */
   else if (strcmp(scomp,"SHUF")==0)
   {
       filt->filtn=H5Z_FILTER_SHUFFLE;
       filt->cd_nelmts = 0;
       if (m>0)
       { /*shuffle does not have parameter */
           if (obj_list) free(obj_list);
           error_msg(progname, "extra parameter in SHUF <%s>\n",str);
           exit(EXIT_FAILURE);
       }
   }
  /*-------------------------------------------------------------------------
   * H5Z_FILTER_FLETCHER32
   *-------------------------------------------------------------------------
   */
   else if (strcmp(scomp,"FLET")==0)
   {
       filt->filtn=H5Z_FILTER_FLETCHER32;
       filt->cd_nelmts = 0;
       if (m>0)
       { /*shuffle does not have parameter */
           if (obj_list) free(obj_list);
           error_msg(progname, "extra parameter in FLET <%s>\n",str);
           exit(EXIT_FAILURE);
       }
   }
  
   else {
       if (obj_list) free(obj_list);
       error_msg(progname, "invalid filter type in <%s>\n",str);
       exit(EXIT_FAILURE);
   }
  }
 } /*i*/
 
  /*-------------------------------------------------------------------------
   * check valid parameters
   *-------------------------------------------------------------------------
   */
   
   switch (filt->filtn)
   {

  /*-------------------------------------------------------------------------
   * H5Z_FILTER_DEFLATE
   *-------------------------------------------------------------------------
   */
       
   case H5Z_FILTER_DEFLATE:
       if (filt->cd_values[0]>9 )
       {
           if (obj_list) free(obj_list);
           error_msg(progname, "invalid compression parameter in <%s>\n",str);
           exit(EXIT_FAILURE);
       }
       break;
       
  /*-------------------------------------------------------------------------
   * H5Z_FILTER_SZIP
   *-------------------------------------------------------------------------
   */
       
   case H5Z_FILTER_SZIP:
       pixels_per_block=filt->cd_values[0];
       if ((pixels_per_block%2)==1) 
       {
           if (obj_list) free(obj_list);
           error_msg(progname, "pixels_per_block is not even in <%s>\n",str);
           exit(EXIT_FAILURE);
       }
       if (pixels_per_block>H5_SZIP_MAX_PIXELS_PER_BLOCK) 
       {
           if (obj_list) free(obj_list);
           error_msg(progname, "pixels_per_block is too large in <%s>\n",str);
           exit(EXIT_FAILURE);
       }
       if ( (strcmp(smask,"NN")!=0) && (strcmp(smask,"EC")!=0) ) 
       {
           if (obj_list) free(obj_list);
           error_msg(progname, "szip mask must be 'NN' or 'EC' \n");
           exit(EXIT_FAILURE);
       }
       break;

  
   };
   
   return obj_list;
}






/*-------------------------------------------------------------------------
 * Function: parse_layout
 *
 * Purpose: read layout info
 *
 * Return: a list of names, the number of names and its chunking info for
 *  chunked. NULL, on error
 * the layout type can be:
 *  CHUNK, to apply chunking layout
 *  CONTI, to apply continuous layout
 *  COMPA, to apply compact layout
 *
 * Example:
 * "AA,B,CDE:CHUNK=10X10"
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 30, 2003
 *
 *-------------------------------------------------------------------------
 */
obj_list_t* parse_layout(const char *str,
                         int *n_objs,
                         pack_info_t *pack,    /* info about layout needed */
                         pack_opt_t *options)
{
    obj_list_t* obj_list=NULL;
    unsigned    i;
    char        c;
    size_t      len=strlen(str);
    int         j, n, k, end_obj=-1, c_index;
    char        sobj[MAX_NC_NAME];
    char        sdim[10];
    char        slayout[10];
    
    
    memset(sdim, '\0', sizeof(sdim));
    memset(sobj, '\0', sizeof(sobj));
    memset(slayout, '\0', sizeof(slayout));
    
    /* check for the end of object list and number of objects */
    for ( i=0, n=0; i<len; i++)
    {
        c = str[i];
        if ( c==':' )
        {
            end_obj=i;
        }
        if ( c==',' )
        {
            n++;
        }
    }
    
    if (end_obj==-1) { /* missing : chunk all */
        options->all_layout=1;
    }
    
    n++;
    obj_list=malloc(n*sizeof(obj_list_t));
    if (obj_list==NULL)
    {
        error_msg(progname, "could not allocate object list\n");
        return NULL;
    }
    *n_objs=n;
    
    /* get object list */
    for ( j=0, k=0, n=0; j<end_obj; j++,k++)
    {
        c = str[j];
        sobj[k]=c;
        if ( c==',' || j==end_obj-1)
        {
            if ( c==',') sobj[k]='\0'; else sobj[k+1]='\0';
            strcpy(obj_list[n].obj,sobj);
            memset(sobj,0,sizeof(sobj));
            n++;
            k=-1;
        }
    }
    
    /* nothing after : */
    if (end_obj+1==(int)len)
    {
        if (obj_list) free(obj_list);
        error_msg(progname, "in parse layout, no characters after : in <%s>\n",str);
        exit(EXIT_FAILURE);
    }
    
    /* get layout info */
    for ( j=end_obj+1, n=0; n<=5; j++,n++)
    {
        if (n==5)
        {
            slayout[n]='\0';  /*cut string */
            if (strcmp(slayout,"COMPA")==0)
                pack->layout=H5D_COMPACT;
            else if (strcmp(slayout,"CONTI")==0)
                pack->layout=H5D_CONTIGUOUS;
            else if (strcmp(slayout,"CHUNK")==0)
                pack->layout=H5D_CHUNKED;
            else {
                error_msg(progname, "in parse layout, not a valid layout in <%s>\n",str);
                exit(EXIT_FAILURE);
            }
        }
        else
        {
            c = str[j];
            slayout[n]=c;
        }
    } /* j */
    
    
    if ( pack->layout==H5D_CHUNKED )
    {
        
    /*-------------------------------------------------------------------------
    * get chunk info
    *-------------------------------------------------------------------------
        */
        k=0;
        
        if (j>(int)len)
        {
            if (obj_list) free(obj_list);
            error_msg(progname, "in parse layout,  <%s> Chunk dimensions missing\n",str);
            exit(EXIT_FAILURE);
        }
        
        for ( i=j, c_index=0; i<len; i++)
        {
            c = str[i];
            sdim[k]=c;
            k++; /*increment sdim index */
            
            if (!isdigit(c) && c!='x'
                && c!='N' && c!='O' && c!='N' && c!='E'
                ){
                if (obj_list) free(obj_list);
                error_msg(progname, "in parse layout, <%s> Not a valid character in <%s>\n",
                    sdim,str);
                exit(EXIT_FAILURE);
            }
            
            if ( c=='x' || i==len-1)
            {
                if ( c=='x') {
                    sdim[k-1]='\0';
                    k=0;
                    pack->chunk.chunk_lengths[c_index]=atoi(sdim);
                    if (pack->chunk.chunk_lengths[c_index]==0) {
                        if (obj_list) free(obj_list);
                        error_msg(progname, "in parse layout, <%s> conversion to number in <%s>\n",
                            sdim,str);
                        exit(EXIT_FAILURE);
                    }
                    c_index++;
                }
                else if (i==len-1) { /*no more parameters */
                    sdim[k]='\0';
                    k=0;
                    if (strcmp(sdim,"NONE")==0)
                    {
                        pack->chunk.rank=-2;
                    }
                    else
                    {
                        pack->chunk.chunk_lengths[c_index]=atoi(sdim);
                        if (pack->chunk.chunk_lengths[c_index]==0){
                            if (obj_list) free(obj_list);
                            error_msg(progname, "in parse layout, <%s> conversion to number in <%s>\n",
                                sdim,str);
                            exit(EXIT_FAILURE);
                        }
                        pack->chunk.rank=c_index+1;
                    }
                } /*if */
            } /*if c=='x' || i==len-1 */
        } /*i*/
        
        
    } /*H5D_CHUNKED*/
    
    
    return obj_list;
}




