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
#include <ctype.h>

#include "H5private.h"
#include "h5repack.h"
#include "h5tools.h"
#include "h5tools_utils.h"

extern char  *progname;

/*-------------------------------------------------------------------------
 * File: h5repack.c
 * Purpose: Public API functions
 *-------------------------------------------------------------------------
 */

static int check_options(pack_opt_t *options);
static int check_objects(const char* fname, pack_opt_t *options);
static const char* get_sfilter (H5Z_filter_t filtn);
static int have_request(pack_opt_t *options);



/*-------------------------------------------------------------------------
 * Function: h5repack
 *
 * Purpose: locate all high-level HDF5 objects in the file
 *  and compress/chunk them using options
 *
 * Algorithm: 2 traversals are made to the file; the 1st builds a list of
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
    int k, n;
    memset(options,0,sizeof(pack_opt_t));
    options->min_comp = 1024;
    options->verbose   = verbose;

    for ( n = 0; n < H5_REPACK_MAX_NFILTERS; n++)
    {
        options->filter_g[n].filtn  = -1;
        options->filter_g[n].cd_nelmts  = 0;
        for ( k = 0; k < CD_VALUES; k++)
            options->filter_g[n].cd_values[k] = 0;
    }

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
    obj_list_t      *obj_list=NULL; /* one object list for the -f and -l option entry */
    filter_info_t   filter;         /* filter info for the current -f option entry */
    int             n_objs;         /* number of objects in the current -f or -l option entry */
    int             is_glb;         /* is the filter global */

    
     
    /* parse the -f option */
    obj_list=parse_filter(str,&n_objs,&filter,options,&is_glb);
    if (obj_list==NULL)
    {
        return -1;
    }
    
    /* if it applies to all objects */
    if (is_glb)
    {
        
        int n;
        
        n = options->n_filter_g++; /* increase # of global filters */

        if (options->n_filter_g > H5_REPACK_MAX_NFILTERS)
        {
            error_msg(progname, "maximum number of filters exceeded for <%s>\n",str);
            return -1;
            
        }
                
        options->filter_g[n] = filter;
    }
    
    else
        options_add_filter(obj_list,n_objs,filter,options->op_tbl);
    
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
    if (options->verbose && have_request(options) /* only print if requested */)
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
    
    if (options->verbose && have_request(options) /* only print if requested */)
    {
        printf("Objects to apply filter are...\n");
        if (options->all_filter==1)
        {
            
            for (k = 0; k < options->n_filter_g; k++ )
            {
                H5Z_filter_t filtn=options->filter_g[k].filtn;
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
                        options->filter_g[k].cd_values[0]);
                    break;
                } /* k */
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

    /*--------------------------------------------------------------------------------
    * verify new user userblock options; file name must be present
    *---------------------------------------------------------------------------------
    */
    if ( options->ublock_filename != NULL && options->ublock_size == 0 )
    {
        if ( options->verbose )
        {
            printf("Warning: user block size missing for file %s. Assigning a default size of 1024...\n",
                options->ublock_filename);
            options->ublock_size = 1024;
        }
    }

    if ( options->ublock_filename == NULL && options->ublock_size != 0 )
    {
        error_msg(progname, "file name missing for user block\n",
            options->ublock_filename);
        return -1;
    }


    /*--------------------------------------------------------------------------------
    * verify alignment options; threshold is zero default but alignment not
    *---------------------------------------------------------------------------------
    */

    if ( options->alignment == 0 && options->threshold != 0 )
    {
        error_msg(progname, "alignment for H5Pset_alignment missing\n");
        return -1;
    }

    return 0;
}


/*-------------------------------------------------------------------------
 * Function: check_objects
 *
 * Purpose: locate all HDF5 objects in the file and compare with user
 *  supplied list
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September, 23, 2003
 *
 *-------------------------------------------------------------------------
 */
static int check_objects(const char* fname,
                         pack_opt_t *options)
{
    hid_t         fid;
    unsigned int  i;
    trav_table_t  *travt = NULL;
    
    /* nothing to do */
    if(options->op_tbl->nelems == 0)
        return 0;
    
    /*-------------------------------------------------------------------------
     * open the file
     *-------------------------------------------------------------------------
     */
    if((fid = h5tools_fopen(fname, NULL, NULL, 0)) < 0)
    {
        printf("<%s>: %s\n", fname, H5FOPENERROR );
        return -1;
    }
    
    /*-------------------------------------------------------------------------
     * get the list of objects in the file
     *-------------------------------------------------------------------------
     */
    
    /* init table */
    trav_table_init(&travt);
    
    /* get the list of objects in the file */
    if(h5trav_gettable(fid, travt) < 0)
        goto out;
    
    /*-------------------------------------------------------------------------
     * compare with user supplied list
     *-------------------------------------------------------------------------
     */
    
    if(options->verbose)
        printf("Opening file <%s>. Searching for objects to modify...\n", fname);
    
    for(i = 0; i < options->op_tbl->nelems; i++) 
    {
        char* name=options->op_tbl->objs[i].path;
        if(options->verbose)
            printf(" <%s>",name);
        
        /* the input object names are present in the file and are valid */
        if(h5trav_getindext(name, travt) < 0) 
        {
            error_msg(progname, "%s Could not find <%s> in file <%s>. Exiting...\n",
                (options->verbose?"\n":""),name,fname);
            goto out;
        }
        if(options->verbose)
            printf("...Found\n");

        /* check for extra filter conditions */
        switch(options->op_tbl->objs[i].filter->filtn) 
        {
            /* chunk size must be smaller than pixels per block */
            case H5Z_FILTER_SZIP:
            {
                int      j;
                hsize_t  csize = 1;
                unsigned ppb = options->op_tbl->objs[i].filter->cd_values[0];
                hsize_t  dims[H5S_MAX_RANK];
                int      rank;
                hid_t    did;
                hid_t    sid;
                
                if (options->op_tbl->objs[i].chunk.rank > 0) 
                {
                    rank = options->op_tbl->objs[i].chunk.rank;
                    for(j = 0; j < rank; j++)
                        csize *= options->op_tbl->objs[i].chunk.chunk_lengths[j];
                }
                else 
                {
                    if((did = H5Dopen(fid, name)) < 0)
                        goto out;
                    if((sid = H5Dget_space(did)) < 0)
                        goto out;
                    if((rank = H5Sget_simple_extent_ndims(sid)) < 0)
                        goto out;
                    HDmemset(dims, 0, sizeof dims);
                    if(H5Sget_simple_extent_dims(sid, dims, NULL) < 0)
                        goto out;
                    for(j = 0; j < rank; j++)
                        csize *= dims[j];
                    if(H5Sclose(sid) < 0)
                        goto out;
                    if(H5Dclose(did) < 0)
                        goto out;
                }
                
                if (csize < ppb ) 
                {
                    printf(" <warning: SZIP settins, chunk size is smaller than pixels per block>\n");
                    goto out;
                }
            }
            break;
        }
    } /* i */
      
   /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */
    H5Fclose(fid);
    trav_table_free(travt);
    return 0;
    
out:
    H5Fclose(fid);
    trav_table_free(travt);
    return -1;
}





/*-------------------------------------------------------------------------
 * Function: have_request
 *
 * Purpose: check if a filter or layout was requested
 *
 * Return: 1 yes, 0 no
 *
 * Date: May, 24, 2007
 *
 *-------------------------------------------------------------------------
 */
static int have_request(pack_opt_t *options)
{

    if (options->all_filter || options->all_layout || options->op_tbl->nelems)
        return 1;

    return 0;

}


/*-------------------------------------------------------------------------
 * Function: get_sfilter
 *
 * Purpose: return the filter as a string name
 *
 * Return: name of filter, exit on error
 *
 *-------------------------------------------------------------------------
 */

static const char* get_sfilter(H5Z_filter_t filtn)
{
    if (filtn==H5Z_FILTER_NONE)
        return "NONE";
    else if (filtn==H5Z_FILTER_DEFLATE)
        return "GZIP";
    else if (filtn==H5Z_FILTER_SZIP)
        return "SZIP";
    else if (filtn==H5Z_FILTER_SHUFFLE)
        return "SHUFFLE";
    else if (filtn==H5Z_FILTER_FLETCHER32)
        return "FLETCHER32";
    else {
        error_msg(progname, "input error in filter type\n");
        exit(EXIT_FAILURE);
    }
    return NULL;
}






