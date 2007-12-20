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

#include "h5repack.h"
#include "h5test.h"
#include "h5tools_utils.h"

extern char  *progname;
static int has_filter(hid_t dcpl_id, H5Z_filter_t filtnin);
static int has_layout(hid_t dcpl_id,pack_info_t *obj);
static int has_filters_glb(hid_t dcpl_id, pack_opt_t *options);
static int has_filters_obj(hid_t dcpl_id, pack_info_t *obj);
static int filtcmp( filter_info_t f1, filter_info_t f2);



/*-------------------------------------------------------------------------
 * Function: h5repack_verify
 *
 * Purpose: verify if filters and layout in the input file match the output file
 *
 * Return: 
 *  1 match
 *  0 do not match
 * -1 error
 *
 * Programmer: Pedro Vicente, pvn@hdfgroup.org
 *
 * Date: December 19, 2003
 *  Modified: December, 19, 2007 (exactly 4 years later :-) )
 *  Separate into 3 cases
 *  1) no filter input, get all datasets and compare DCPLs. TO DO
 *  2) filter input on selected datasets, get each one trough OBJ and match
 *  3) filter input on all datasets, get all objects and match 
 *
 *-------------------------------------------------------------------------
 */

int h5repack_verify(const char *fname,
                    pack_opt_t *options)
{
    hid_t        fid;           /* file ID */
    hid_t        dset_id = -1;  /* dataset ID */
    hid_t        dcpl_id = -1;  /* dataset creation property list ID */
    hid_t        space_id = -1; /* space ID */
    unsigned int i;
    trav_table_t *travt = NULL;
    int          ok = 1;
    
    /* open the file */
    if((fid = H5Fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0 )
        return -1;
    
    for(i = 0; i < options->op_tbl->nelems; i++) 
    {
        char* name = options->op_tbl->objs[i].path;
        pack_info_t *obj = &options->op_tbl->objs[i];
        
       /*-------------------------------------------------------------------------
        * open
        *-------------------------------------------------------------------------
        */
        if((dset_id = H5Dopen2(fid, name, H5P_DEFAULT)) < 0)
            goto error;
        if((space_id = H5Dget_space(dset_id)) < 0)
            goto error;
        if((dcpl_id = H5Dget_create_plist(dset_id)) < 0)
            goto error;
        
       /*-------------------------------------------------------------------------
        * filter check
        *-------------------------------------------------------------------------
        */

        if(has_filters_obj(dcpl_id, obj) == 0)
                ok = 0;
        
       /*-------------------------------------------------------------------------
        * layout check
        *-------------------------------------------------------------------------
        */
        if((obj->layout != -1) && (has_layout(dcpl_id, obj) == 0))
            ok = 0;
        
       /*-------------------------------------------------------------------------
        * close
        *-------------------------------------------------------------------------
        */
        if(H5Pclose(dcpl_id) < 0)
            goto error;
        if (H5Sclose(space_id) < 0)
            goto error;
        if (H5Dclose(dset_id) < 0)
            goto error;
        
    }
    
    
   /*-------------------------------------------------------------------------
    * check for the "all" objects option
    *-------------------------------------------------------------------------
    */
    
    if(options->all_filter == 1 || options->all_layout == 1) 
    {
        
        /* init table */
        trav_table_init(&travt);
        
        /* get the list of objects in the file */
        if(h5trav_gettable(fid, travt) < 0)
            goto error;
        
        for(i = 0; i < travt->nobjs; i++) 
        {
            char *name = travt->objs[i].name;
            
            if(travt->objs[i].type == H5TRAV_TYPE_DATASET) 
            {
                
               /*-------------------------------------------------------------------------
                * open
                *-------------------------------------------------------------------------
                */
                if((dset_id = H5Dopen2(fid, name, H5P_DEFAULT)) < 0)
                    goto error;
                if((space_id = H5Dget_space(dset_id)) < 0)
                    goto error;
                if((dcpl_id = H5Dget_create_plist(dset_id)) < 0)
                    goto error;
                
               /*-------------------------------------------------------------------------
                * filter check
                *-------------------------------------------------------------------------
                */
                if(options->all_filter == 1)
                {
                    if (has_filters_glb(dcpl_id, options) == 0)
                        ok = 0;
                }
                
               /*-------------------------------------------------------------------------
                * layout check
                *-------------------------------------------------------------------------
                */
                if(options->all_layout == 1) 
                {
                    pack_info_t pack;
                    init_packobject(&pack);
                    pack.layout = options->layout_g;
                    pack.chunk = options->chunk_g;
                    if(has_layout(dcpl_id, &pack) == 0)
                        ok = 0;
                }
                
                
               /*-------------------------------------------------------------------------
                * close
                *-------------------------------------------------------------------------
                */
                if (H5Pclose(dcpl_id) < 0)
                    goto error;
                if (H5Sclose(space_id) < 0)
                    goto error;
                if (H5Dclose(dset_id) < 0)
                    goto error;
            } /* if */
            
        } /* i */
        
        /* free table */
        trav_table_free(travt);
    }
    
   /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */
    
    if (H5Fclose(fid) < 0)
        return -1;
    
    return ok;
    
error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl_id);
        H5Sclose(space_id);
        H5Dclose(dset_id);
        H5Fclose(fid);
        if (travt)
            trav_table_free(travt);
    } H5E_END_TRY;
    return -1;
}




/*-------------------------------------------------------------------------
 * Function: has_filter
 *
 * Purpose: verify if a filter is present in the property list DCPL_ID
 *
 * Return: 1 has, 0 does not, -1 error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 19, 2003
 *
 *-------------------------------------------------------------------------
 */

static int has_filter(hid_t dcpl_id, H5Z_filter_t filtnin)
{
    unsigned     nfilters;       /* number of filters */
    unsigned     filt_flags;     /* filter flags */
    H5Z_filter_t filtn;          /* filter identification number */
    unsigned     cd_values[20];  /* filter client data values */
    size_t       cd_nelmts;      /* filter client number of values */
    char         f_name[256];    /* filter name */
    int          have = 0;       /* flag, filter is present */
    unsigned     i;              /* index */
    
    /* if no information about the input filter is requested return exit */
    if(filtnin == -1)
        return 1;
    
    /* get information about filters */
    if((nfilters = H5Pget_nfilters(dcpl_id)) < 0)
        return -1;
    
    /* if we do not have filters and the requested filter is NONE, return 1 */
    if(!nfilters && filtnin == H5Z_FILTER_NONE)
        return 1;
    
    for(i = 0; i < nfilters; i++) 
    {
        cd_nelmts = NELMTS(cd_values);
        filtn = H5Pget_filter2(dcpl_id, i, &filt_flags, &cd_nelmts,
            cd_values, sizeof(f_name), f_name, NULL);
        
        if(filtnin == filtn)
            have = 1;
    }
    
    return have;
}


/*-------------------------------------------------------------------------
 * Function: has_layout
 *
 * Purpose: verify which layout is present in the property list DCPL_ID
 *
 *  H5D_COMPACT	  	= 0
 *  H5D_CONTIGUOUS	= 1
 *  H5D_CHUNKED		  = 2
 *
 * Return: 1 has, 0 does not, -1 error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 30, 2003
 *
 *-------------------------------------------------------------------------
 */

int has_layout(hid_t dcpl_id,
               pack_info_t *obj)
{
    hsize_t      chsize[64];     /* chunk size in elements */
    H5D_layout_t layout;         /* layout */
    int          nfilters;       /* number of filters */
    int          rank;           /* rank */
    int          i;              /* index */
    
    /* if no information about the input layout is requested return exit */
    if (obj==NULL)
        return 1;
    
    /* check if we have filters in the input object */
    if ((nfilters = H5Pget_nfilters(dcpl_id)) < 0)
        return -1;
    
    /* a non chunked layout was requested on a filtered object; avoid the test */
    if (nfilters && obj->layout!=H5D_CHUNKED)
        return 1;
    
    /* get layout */
    if ((layout = H5Pget_layout(dcpl_id)) < 0)
        return -1;
    
    if (obj->layout != layout)
        return 0;
    
    if (layout==H5D_CHUNKED)
    {
        if ((rank = H5Pget_chunk(dcpl_id,NELMTS(chsize),chsize/*out*/)) < 0)
            return -1;
        if (obj->chunk.rank != rank)
            return 0;
        for ( i=0; i<rank; i++)
            if (chsize[i] != obj->chunk.chunk_lengths[i])
                return 0;
    }
    
    return 1;
}

/*-------------------------------------------------------------------------
 * Function: h5repack_cmpdcpl
 *
 * Purpose: compare 2 files for identical property lists of all objects
 *
 * Return: 1=identical, 0=not identical, -1=error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 31, 2003
 *
 *-------------------------------------------------------------------------
 */

int h5repack_cmpdcpl(const char *fname1,
                     const char *fname2)
{
    hid_t         fid1=-1;       /* file ID */
    hid_t         fid2=-1;       /* file ID */
    hid_t         dset1=-1;      /* dataset ID */
    hid_t         dset2=-1;      /* dataset ID */
    hid_t         dcpl1=-1;      /* dataset creation property list ID */
    hid_t         dcpl2=-1;      /* dataset creation property list ID */
    trav_table_t  *travt1=NULL;
    trav_table_t  *travt2=NULL;
    int           ret=1;
    unsigned int  i;
    
   /*-------------------------------------------------------------------------
    * open the files 
    *-------------------------------------------------------------------------
    */
    
    /* disable error reporting */
    H5E_BEGIN_TRY 
    {
        
        /* Open the files */
        if ((fid1=H5Fopen(fname1,H5F_ACC_RDONLY,H5P_DEFAULT)) < 0 )
        {
            error_msg(progname, "<%s>: %s\n", fname1, H5FOPENERROR );
            return -1;
        }
        if ((fid2=H5Fopen(fname2,H5F_ACC_RDONLY,H5P_DEFAULT)) < 0 )
        {
            error_msg(progname, "<%s>: %s\n", fname2, H5FOPENERROR );
            H5Fclose(fid1);
            return -1;
        }
        /* enable error reporting */
    } H5E_END_TRY;
    
   /*-------------------------------------------------------------------------
    * get file table list of objects
    *-------------------------------------------------------------------------
    */
    trav_table_init(&travt1);
    trav_table_init(&travt2);
    if(h5trav_gettable(fid1, travt1) < 0)
        goto error;
    if(h5trav_gettable(fid2, travt2) < 0)
        goto error;
    
    
   /*-------------------------------------------------------------------------
    * traverse the suppplied object list
    *-------------------------------------------------------------------------
    */
    
    for(i = 0; i < travt1->nobjs; i++) 
    {
        if(travt1->objs[i].type == H5TRAV_TYPE_DATASET) 
        {
            if((dset1 = H5Dopen2(fid1, travt1->objs[i].name, H5P_DEFAULT)) < 0)
                goto error;
            if((dset2 = H5Dopen2(fid2, travt1->objs[i].name, H5P_DEFAULT)) < 0)
                goto error;
            if((dcpl1 = H5Dget_create_plist(dset1)) < 0)
                goto error;
            if((dcpl2 = H5Dget_create_plist(dset2)) < 0)
                goto error;
            
           /*-------------------------------------------------------------------------
            * compare the property lists
            *-------------------------------------------------------------------------
            */
            if((ret = H5Pequal(dcpl1, dcpl2)) < 0)
                goto error;
            
            if(ret == 0) 
            {
                error_msg(progname, "property lists for <%s> are different\n",travt1->objs[i].name);
                goto error;
            }
            
           /*-------------------------------------------------------------------------
            * close
            *-------------------------------------------------------------------------
            */
            if(H5Pclose(dcpl1) < 0)
                goto error;
            if(H5Pclose(dcpl2) < 0)
                goto error;
            if(H5Dclose(dset1) < 0)
                goto error;
            if(H5Dclose(dset2) < 0)
                goto error;
        } /*if*/
    } /*i*/
    
   /*-------------------------------------------------------------------------
    * free
    *-------------------------------------------------------------------------
    */
    
    trav_table_free(travt1);
    trav_table_free(travt2);
    
   /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */
    
    H5Fclose(fid1);
    H5Fclose(fid2);
    return ret;
    
   /*-------------------------------------------------------------------------
    * error
    *-------------------------------------------------------------------------
    */
    
error:
    H5E_BEGIN_TRY 
    {
        H5Pclose(dcpl1);
        H5Pclose(dcpl2);
        H5Dclose(dset1);
        H5Dclose(dset2);
        H5Fclose(fid1);
        H5Fclose(fid2);
        trav_table_free(travt1);
        trav_table_free(travt2);
    } H5E_END_TRY;
    return -1;
    
}


/*-------------------------------------------------------------------------
 * Function: has_filters_glb
 *
 * Purpose: verify if all requested filters for global filters are present in the 
 *  property list DCPL_ID
 *
 * Return: 1 has, 0 does not, -1 error
 *
 * Programmer: Pedro Vicente, pvn@hdfgroup.org
 *
 * Date: December 3, 2007
 *
 *-------------------------------------------------------------------------
 */

static int has_filters_glb(hid_t dcpl_id, pack_opt_t *options)
{
    unsigned      nfilters_dcpl;  /* number of filters in DCPL*/
    unsigned      nfilters_opt;   /* number of filters in OPTIONS*/
    unsigned      filt_flags;     /* filter flags */
    H5Z_filter_t  filtn;          /* filter identification number */
    unsigned      cd_values[20];  /* filter client data values */
    size_t        cd_nelmts;      /* filter client number of values */
    char          f_name[256];    /* filter name */
    int           have = 0;       /* flag, filter is present */
    unsigned      i, j;           /* index */
    filter_info_t filter_dcpl[H5_REPACK_MAX_NFILTERS];  /* filter array in the DCPL*/
    filter_info_t filter_opt[H5_REPACK_MAX_NFILTERS];   /* filter array in options */
  
    /* get information about filters */
    if((nfilters_dcpl = H5Pget_nfilters(dcpl_id)) < 0)
        return -1;
    
    /* if we do not have filters and the requested filter is NONE, return 1 */
    if(!nfilters_dcpl && 
        options->n_filter_g==1 && 
        options->filter_g[0].filtn == H5Z_FILTER_NONE )
        return 1;

    /*-------------------------------------------------------------------------
     * build a list with DCPL filters
     *-------------------------------------------------------------------------
     */

    for( i = 0; i < nfilters_dcpl; i++) 
    {
        cd_nelmts = NELMTS(cd_values);
        filtn = H5Pget_filter2(dcpl_id, i, &filt_flags, &cd_nelmts,
            cd_values, sizeof(f_name), f_name, NULL);
        
        filter_dcpl[i].filtn = filtn;
        filter_dcpl[i].cd_nelmts = cd_nelmts;
        for( j = 0; j < cd_nelmts; j++) 
        {
            filter_dcpl[i].cd_values[j] = cd_values[j];

        }

    }

    /*-------------------------------------------------------------------------
     * build a list with options filters
     *-------------------------------------------------------------------------
     */

    nfilters_opt = options->n_filter_g;
   
    for( i = 0; i < nfilters_opt; i++) 
    {
               
        filter_opt[i].filtn = options->filter_g[i].filtn;
        filter_opt[i].cd_nelmts = options->filter_g[i].cd_nelmts;
        for( j = 0; j < options->filter_g[i].cd_nelmts; j++) 
        {
            filter_opt[i].cd_values[j] = options->filter_g[i].cd_values[j];

        }

    }

    /*-------------------------------------------------------------------------
     * match the 2 lists
     *-------------------------------------------------------------------------
     */


    if (nfilters_dcpl != nfilters_opt)
        return 0;

    for( i = 0; i < nfilters_opt; i++) 
    {

        /* criteria is filter compare, returns same as strcmp */
        if ( filtcmp( filter_dcpl[i], filter_opt[i] ) != 0 )
            return 0;


    }


 
    return 1;
}



/*-------------------------------------------------------------------------
 * Function: has_filters_obj
 *
 * Purpose: verify if all requested filters for OBJ are present in the 
 *  property list DCPL_ID
 *
 * Return: 1 has, 0 does not, -1 error
 *
 * Programmer: Pedro Vicente, pvn@hdfgroup.org
 *
 * Date: December 3, 2007
 *
 *-------------------------------------------------------------------------
 */

static int has_filters_obj(hid_t dcpl_id, pack_info_t *obj)
{
    unsigned      nfilters_dcpl;  /* number of filters in DCPL*/
    unsigned      nfilters_opt;   /* number of filters in OPTIONS*/
    unsigned      filt_flags;     /* filter flags */
    H5Z_filter_t  filtn;          /* filter identification number */
    unsigned      cd_values[20];  /* filter client data values */
    size_t        cd_nelmts;      /* filter client number of values */
    char          f_name[256];    /* filter name */
    int           have = 0;       /* flag, filter is present */
    unsigned      i, j;           /* index */
    filter_info_t filter_dcpl[H5_REPACK_MAX_NFILTERS];  /* filter array in the DCPL*/
    filter_info_t filter_opt[H5_REPACK_MAX_NFILTERS];   /* filter array in options */
  
    /* get information about filters */
    if((nfilters_dcpl = H5Pget_nfilters(dcpl_id)) < 0)
        return -1;
    
    /* if we do not have filters and the requested filter is NONE, return 1 */
    if(!nfilters_dcpl && 
        obj->nfilters == 1 && 
        obj->filter[0].filtn == H5Z_FILTER_NONE )
        return 1;

    /*-------------------------------------------------------------------------
     * build a list with DCPL filters
     *-------------------------------------------------------------------------
     */

    for( i = 0; i < nfilters_dcpl; i++) 
    {
        cd_nelmts = NELMTS(cd_values);
        filtn = H5Pget_filter2(dcpl_id, i, &filt_flags, &cd_nelmts,
            cd_values, sizeof(f_name), f_name, NULL);
        
        filter_dcpl[i].filtn = filtn;
        filter_dcpl[i].cd_nelmts = cd_nelmts;
        for( j = 0; j < cd_nelmts; j++) 
        {
            filter_dcpl[i].cd_values[j] = cd_values[j];

        }

    }

    /*-------------------------------------------------------------------------
     * build a list with OBJ filters
     *-------------------------------------------------------------------------
     */

    nfilters_opt = obj->nfilters;
  
    for( i = 0; i < obj->nfilters; i++) 
    {
        
        filter_opt[i].filtn = obj->filter[i].filtn;
        filter_opt[i].cd_nelmts = obj->filter[i].cd_nelmts;
        for( j = 0; j < obj->filter[i].cd_nelmts; j++) 
        {
            filter_opt[i].cd_values[j] = obj->filter[i].cd_values[j];
            
        }
        
    }
    

    /*-------------------------------------------------------------------------
     * match the 2 lists
     *-------------------------------------------------------------------------
     */


    if (nfilters_dcpl != nfilters_opt)
        return 0;

    for( i = 0; i < nfilters_opt; i++) 
    {

        /* criteria is filter compare, returns same as strcmp */
        if ( filtcmp( filter_dcpl[i], filter_opt[i] ) != 0 )
            return 0;


    }


 
    return 1;
}




/*-------------------------------------------------------------------------
 * Function: filtcmp
 *
 * Purpose: compare 2 filters
 *
 * Return: same as strcmp:
 *  < 0 F1 "less" than F2 
 *    0 F1 identical to F2 
 *  > 0 F1 "greater" than F2 
 *
 * Programmer: Pedro Vicente, pvn@hdfgroup.org
 *
 * Date: December 3, 2007
 *
 *-------------------------------------------------------------------------
 */
static int filtcmp( filter_info_t f1 /*DCPL*/, filter_info_t f2 /*OPT*/)
{
    unsigned i;


    /*-------------------------------------------------------------------------
     * compare first the filter type
     *-------------------------------------------------------------------------
     */

    if (f1.filtn < f2.filtn)
    {
        return -1;
    }
    else if (f1.filtn > f2.filtn)
    {
        return 1;
    }
    else if (f1.filtn == f2.filtn)
    {

        switch (f1.filtn)
        {
            
        case H5Z_FILTER_SHUFFLE:
            
            /* 1 private client value is returned by DCPL */
            if ( f1.cd_nelmts != 1 && f2.cd_nelmts != 0 )
                return -1;

            return 0;
            
            
            break;

        case H5Z_FILTER_SZIP:

            /* 4 private client values are returned by DCPL */
            if ( f1.cd_nelmts != 4 && f2.cd_nelmts != 2 )
                return -1;
            
            if ( f2.cd_values[0] != f1.cd_values[2] &&
                 f2.cd_values[1] != f1.cd_values[1] )
                return -1;

            return 0;
            
            
            break;
            
        case H5Z_FILTER_NBIT:

            /* TO DO */

            return 0;
            
            
            break;
            
        case H5Z_FILTER_SCALEOFFSET:

            /* TO DO */

            return 0;
            
            
            break;
            
        case H5Z_FILTER_FLETCHER32:
        case H5Z_FILTER_DEFLATE:

            if ( f1.cd_nelmts != f2.cd_nelmts )
                return -1;
            
            /* consider different filter values as "less" */
            for( i = 0; i < f1.cd_nelmts; i++) 
            {
                if (f1.cd_values[i] != f2.cd_values[i])
                {
                    return -1; 
                }
                
            }

            return 0;
            
            
            break;
            
            
            
        } /* switch */
     

    } /* f1.filtn == f2.filtn */



    
    assert(0);
    return -1; 
    
}


