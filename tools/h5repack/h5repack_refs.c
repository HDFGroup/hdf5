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
#include "h5repack.h"
#include "H5private.h"
#include "h5diff.h"
#include "h5tools.h"

/*-------------------------------------------------------------------------
 * local functions
 *-------------------------------------------------------------------------
 */

static const char* MapIdToName(hid_t refobj_id,trav_table_t *travt);
static int copy_refs_attr(hid_t loc_in, hid_t loc_out, pack_opt_t *options,
                          trav_table_t *travt, hid_t fidout);
static void close_obj(H5G_obj_t1 obj_type, hid_t obj_id);


/*-------------------------------------------------------------------------
 * Function: do_copy_refobjs
 *
 * Purpose: duplicate all referenced HDF5 objects in the file
 *  and create hard links
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December, 10, 2003
 *
 *-------------------------------------------------------------------------
 */

int do_copy_refobjs(hid_t fidin,
                    hid_t fidout,
                    trav_table_t *travt,
                    pack_opt_t *options) /* repack options */
{
    hid_t     grp_in=(-1);            /* read group ID */
    hid_t     grp_out=(-1);           /* write group ID */
    hid_t     dset_in=(-1);           /* read dataset ID */
    hid_t     dset_out=(-1);          /* write dataset ID */
    hid_t     type_in=(-1);           /* named type ID */
    hid_t     dcpl_id=(-1);           /* dataset creation property list ID */
    hid_t     space_id=(-1);          /* space ID */
    hid_t     ftype_id=(-1);          /* file data type ID */
    hid_t     mtype_id=(-1);          /* memory data type ID */
    size_t    msize;                  /* memory size of memory type */
    hsize_t   nelmts;                 /* number of elements in dataset */
    int       rank;                   /* rank of dataset */
    hsize_t   dims[H5S_MAX_RANK];     /* dimensions of dataset */
    unsigned int i, j;
    int       k;
    
    /*-------------------------------------------------------------------------
    * browse
    *-------------------------------------------------------------------------
    */
    
    for ( i = 0; i < travt->nobjs; i++)
    {
        switch ( travt->objs[i].type )
        {
        /*-------------------------------------------------------------------------
        * H5G_GROUP
        *-------------------------------------------------------------------------
        */
        case H5G_GROUP:
            
        /*-------------------------------------------------------------------------
        * copy referenced objects in attributes
        *-------------------------------------------------------------------------
        */
            
            if ((grp_out=H5Gopen(fidout,travt->objs[i].name))<0)
                goto error;
            if((grp_in = H5Gopen (fidin,travt->objs[i].name))<0)
                goto error;
            if (copy_refs_attr(grp_in,grp_out,options,travt,fidout)<0)
                goto error;
            if (H5Gclose(grp_out)<0)
                goto error;
            if (H5Gclose(grp_in)<0)
                goto error;
            
           /*-------------------------------------------------------------------------
            * check for hard links
            *-------------------------------------------------------------------------
            */
            
            if (travt->objs[i].nlinks)
            {
                for ( j=0; j<travt->objs[i].nlinks; j++)
                {
                    H5Glink(fidout,
                        H5G_LINK_HARD,
                        travt->objs[i].name,
                        travt->objs[i].links[j].new_name);
                }
            }
            
            break;
            
       /*-------------------------------------------------------------------------
       * H5G_DATASET
       *-------------------------------------------------------------------------
       */
        case H5G_DATASET:
            
            if ((dset_in=H5Dopen(fidin,travt->objs[i].name))<0)
                goto error;
            if ((space_id=H5Dget_space(dset_in))<0)
                goto error;
            if ((ftype_id=H5Dget_type (dset_in))<0)
                goto error;
            if ((dcpl_id=H5Dget_create_plist(dset_in))<0)
                goto error;
            if ( (rank=H5Sget_simple_extent_ndims(space_id))<0)
                goto error;
            if ( H5Sget_simple_extent_dims(space_id,dims,NULL)<0)
                goto error;
            nelmts=1;
            for (k=0; k<rank; k++)
                nelmts*=dims[k];
            
            if ((mtype_id=h5tools_get_native_type(ftype_id))<0)
                goto error;
            
            if ((msize=H5Tget_size(mtype_id))==0)
                goto error;
            
            
           /*-------------------------------------------------------------------------
           * check if the dataset creation property list has filters that
           * are not registered in the current configuration
           * 1) the external filters GZIP and SZIP might not be available
           * 2) the internal filters might be turned off
           *-------------------------------------------------------------------------
           */
            if (h5tools_canreadf((NULL),dcpl_id)==1)
            {
               /*-------------------------------------------------------------------------
                * test for a valid output dataset
                *-------------------------------------------------------------------------
                */
                dset_out = FAIL;
                
                /*-------------------------------------------------------------------------
                * object references are a special case
                * we cannot just copy the buffers, but instead we recreate the reference
                *-------------------------------------------------------------------------
                */
                if (H5Tequal(mtype_id, H5T_STD_REF_OBJ))
                {
                    H5G_obj_t1       obj_type;
                    hid_t            refobj_id;
                    hobj_ref_t       *refbuf=NULL; /* buffer for object references */
                    hobj_ref_t       *buf=NULL;
                    const char*      refname;
                    unsigned         u;
                    
                    /*-------------------------------------------------------------------------
                    * read to memory
                    *-------------------------------------------------------------------------
                    */
                    
                    if (nelmts)
                    {
                        buf=(void *) HDmalloc((unsigned)(nelmts*msize));
                        if ( buf==NULL){
                            printf( "cannot read into memory\n" );
                            goto error;
                        }
                        if (H5Dread(dset_in,mtype_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
                            goto error;
                        
                        if ((obj_type = H5Rget_obj_type(dset_in,H5R_OBJECT,buf))<0)
                            goto error;
                        refbuf=HDcalloc((unsigned)nelmts,msize);
                        if ( refbuf==NULL){
                            printf( "cannot allocate memory\n" );
                            goto error;
                        }
                        for ( u=0; u<nelmts; u++)
                        {
                            H5E_BEGIN_TRY {
                                if ((refobj_id = H5Rdereference(dset_in,H5R_OBJECT,&buf[u]))<0)
                                    continue;
                            } H5E_END_TRY;
                            /* get the name. a valid name could only occur in the
                            second traversal of the file */
                            if ((refname=MapIdToName(refobj_id,travt))!=NULL)
                            {
                                /* create the reference, -1 parameter for objects */
                                if (H5Rcreate(&refbuf[u],fidout,refname,H5R_OBJECT,-1)<0)
                                    goto error;
                                if(options->verbose)
                                {
                                    
                                    
                                    printf(FORMAT_OBJ,"dset",travt->objs[i].name );
                                    printf("object <%s> object reference created to <%s>\n",
                                        travt->objs[i].name,
                                        refname);
                                }
                            }/*refname*/
                            close_obj(obj_type,refobj_id);
                        }/*  u */
                    }/*nelmts*/
                    
                     /*-------------------------------------------------------------------------
                     * create/write dataset/close
                     *-------------------------------------------------------------------------
                    */
                    if ((dset_out=H5Dcreate(fidout,travt->objs[i].name,mtype_id,space_id,dcpl_id))<0)
                        goto error;
                    if (nelmts) {
                        if (H5Dwrite(dset_out,mtype_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,refbuf)<0)
                            goto error;
                    }
                    
                    if (buf)
                        free(buf);
                    if (refbuf)
                        free(refbuf);
                    
                }/*H5T_STD_REF_OBJ*/
                
                 /*-------------------------------------------------------------------------
                 * dataset region references
                 *-------------------------------------------------------------------------
                */
                else if (H5Tequal(mtype_id, H5T_STD_REF_DSETREG))
                {
                    H5G_obj_t1       obj_type;
                    hid_t            refobj_id;
                    hdset_reg_ref_t  *refbuf=NULL; /* input buffer for region references */
                    hdset_reg_ref_t  *buf=NULL;    /* output buffer */
                    const char*      refname;
                    unsigned         u;
                    
                    /*-------------------------------------------------------------------------
                    * read input to memory
                    *-------------------------------------------------------------------------
                    */
                    if (nelmts)
                    {
                        buf=(void *) HDmalloc((unsigned)(nelmts*msize));
                        if ( buf==NULL){
                            printf( "cannot read into memory\n" );
                            goto error;
                        }
                        if (H5Dread(dset_in,mtype_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
                            goto error;
                        if ((obj_type = H5Rget_obj_type(dset_in,H5R_DATASET_REGION,buf))<0)
                            goto error;
                        
                        /*-------------------------------------------------------------------------
                        * create output
                        *-------------------------------------------------------------------------
                        */
                        
                        refbuf=HDcalloc(sizeof(hdset_reg_ref_t),(size_t)nelmts); /*init to zero */
                        if ( refbuf==NULL){
                            printf( "cannot allocate memory\n" );
                            goto error;
                        }
                        for ( u=0; u<nelmts; u++)
                        {
                            H5E_BEGIN_TRY {
                                if ((refobj_id = H5Rdereference(dset_in,H5R_DATASET_REGION,&buf[u]))<0)
                                    continue;
                            } H5E_END_TRY;
                            
                            /* get the name. a valid name could only occur in the
                            second traversal of the file */
                            if ((refname=MapIdToName(refobj_id,travt))!=NULL)
                            {
                                hid_t region_id;    /* region id of the referenced dataset */
                                if ((region_id = H5Rget_region(dset_in,H5R_DATASET_REGION,&buf[u]))<0)
                                    goto error;
                                /* create the reference, we need the space_id */
                                if (H5Rcreate(&refbuf[u],fidout,refname,H5R_DATASET_REGION,region_id)<0)
                                    goto error;
                                if (H5Sclose(region_id)<0)
                                    goto error;
                                if(options->verbose)
                                {
                                    
                                    
                                    
                                    printf(FORMAT_OBJ,"dset",travt->objs[i].name );
                                    printf("object <%s> region reference created to <%s>\n",
                                        travt->objs[i].name,
                                        refname);
                                }
                            }/*refname*/
                            close_obj(obj_type,refobj_id);
                        }/*  u */
                    }/*nelmts*/
                    
                     /*-------------------------------------------------------------------------
                     * create/write dataset/close
                     *-------------------------------------------------------------------------
                    */
                    if ((dset_out=H5Dcreate(fidout,travt->objs[i].name,mtype_id,space_id,dcpl_id))<0)
                        goto error;
                    if (nelmts) {
                        if (H5Dwrite(dset_out,mtype_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,refbuf)<0)
                            goto error;
                    }
                    
                    if (buf)
                        free(buf);
                    if (refbuf)
                        free(refbuf);
                } /* H5T_STD_REF_DSETREG */
                
                
               /*-------------------------------------------------------------------------
                * not references, open previously created object in 1st traversal
                *-------------------------------------------------------------------------
                */
                else
                {
                    if ((dset_out=H5Dopen(fidout,travt->objs[i].name))<0)
                        goto error;
                }
                
                assert(dset_out!=FAIL);
                
                /*-------------------------------------------------------------------------
                * copy referenced objects in attributes
                *-------------------------------------------------------------------------
                */
                if (copy_refs_attr(dset_in,dset_out,options,travt,fidout)<0)
                    goto error;
                
                
                /*-------------------------------------------------------------------------
                * check for hard links
                *-------------------------------------------------------------------------
                */
                if (travt->objs[i].nlinks)
                {
                    for ( j=0; j<travt->objs[i].nlinks; j++){
                        H5Glink(fidout,
                            H5G_LINK_HARD,
                            travt->objs[i].name,
                            travt->objs[i].links[j].new_name);
                    }
                }
                
                if (H5Dclose(dset_out)<0)
                    goto error;
                
   }/*can_read*/
   
    /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */
    
    if (H5Tclose(ftype_id)<0)
        goto error;
    if (H5Tclose(mtype_id)<0)
        goto error;
    if (H5Pclose(dcpl_id)<0)
        goto error;
    if (H5Sclose(space_id)<0)
        goto error;
    if (H5Dclose(dset_in)<0)
        goto error;
    
    break;
    
    /*-------------------------------------------------------------------------
    * H5G_TYPE
    *-------------------------------------------------------------------------
    */
  case H5G_TYPE:
      
      if ((type_in = H5Topen (fidin,travt->objs[i].name))<0)
          goto error;
      
      if (H5Tclose(type_in)<0)
          goto error;
      
      break;
      
      /*-------------------------------------------------------------------------
      * H5G_LINK
      *-------------------------------------------------------------------------
      */
      
  case H5G_LINK:
      
      /*nothing to do */
      break;
      
  default:
      
      break;
  }
 }
 
 
 
 /*-------------------------------------------------------------------------
 * the root is a special case, we get an ID for the root group
 * and copy its attributes using that ID
 * it must be done last, because the attributes might contain references to
 * objects in the object list
 *-------------------------------------------------------------------------
 */
 
 if ((grp_out = H5Gopen(fidout,"/"))<0)
     goto error;
 
 if ((grp_in  = H5Gopen(fidin,"/"))<0)
     goto error;
 
 if (copy_refs_attr(grp_in,grp_out,options,travt,fidout)<0)
     goto error;
 
 if (H5Gclose(grp_out)<0)
     goto error;
 if (H5Gclose(grp_in)<0)
     goto error;
 
 return 0;
 
error:
 H5E_BEGIN_TRY {
     H5Gclose(grp_in);
     H5Gclose(grp_out);
     H5Pclose(dcpl_id);
     H5Sclose(space_id);
     H5Dclose(dset_in);
     H5Dclose(dset_out);
     H5Tclose(ftype_id);
     H5Tclose(mtype_id);
     H5Tclose(type_in);
 } H5E_END_TRY;
 return -1;
 
}


/*-------------------------------------------------------------------------
 * Function: copy_refs_attr
 *
 * Purpose: duplicate all referenced HDF5 located in attributes
 *  relative to LOC_IN, which is obtained either from
 * loc_id = H5Gopen( fid, name);
 * loc_id = H5Dopen( fid, name);
 * loc_id = H5Topen( fid, name);
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October, 28, 2003
 *
 *-------------------------------------------------------------------------
 */

static int copy_refs_attr(hid_t loc_in,
                          hid_t loc_out,
                          pack_opt_t *options,
                          trav_table_t *travt,
                          hid_t fidout         /* for saving references */
                          )
{
    hid_t      attr_id=-1;        /* attr ID */
    hid_t      attr_out=-1;       /* attr ID */
    hid_t      space_id=-1;       /* space ID */
    hid_t      ftype_id=-1;       /* file data type ID */
    hid_t      mtype_id=-1;       /* memory data type ID */
    size_t     msize;             /* memory size of type */
    hsize_t    nelmts;            /* number of elements in dataset */
    int        rank;              /* rank of dataset */
    hsize_t    dims[H5S_MAX_RANK];/* dimensions of dataset */
    char       name[255];
    int        n, j;
    unsigned   u;
    
    if ((n = H5Aget_num_attrs(loc_in))<0)
        goto error;
    
    for ( u = 0; u < (unsigned)n; u++)
    {
        
    /*-------------------------------------------------------------------------
    * open
    *-------------------------------------------------------------------------
        */
        /* open attribute */
        if ((attr_id = H5Aopen_idx(loc_in, u))<0)
            goto error;
        
        /* get name */
        if (H5Aget_name( attr_id, 255, name )<0)
            goto error;
        
        /* get the file datatype  */
        if ((ftype_id = H5Aget_type( attr_id )) < 0 )
            goto error;
        
        /* get the dataspace handle  */
        if ((space_id = H5Aget_space( attr_id )) < 0 )
            goto error;
        
        /* get dimensions  */
        if ( (rank = H5Sget_simple_extent_dims(space_id, dims, NULL)) < 0 )
            goto error;
        
        
            /*-------------------------------------------------------------------------
            * elements
            *-------------------------------------------------------------------------
        */
        nelmts=1;
        for (j=0; j<rank; j++)
            nelmts*=dims[j];
        
        if ((mtype_id=h5tools_get_native_type(ftype_id))<0)
            goto error;
        
        if ((msize=H5Tget_size(mtype_id))==0)
            goto error;
        
            /*-------------------------------------------------------------------------
            * object references are a special case
            * we cannot just copy the buffers, but instead we recreate the reference
            *-------------------------------------------------------------------------
        */
        if (H5Tequal(mtype_id, H5T_STD_REF_OBJ))
        {
            H5G_obj_t1  obj_type;
            hid_t       refobj_id;
            hobj_ref_t  *refbuf=NULL;
            unsigned    k;
            const char* refname;
            hobj_ref_t  *buf=NULL;
            
            /*-------------------------------------------------------------------------
            * read input to memory
            *-------------------------------------------------------------------------
            */
            
            if (nelmts)
            {
                buf=(void *) HDmalloc((unsigned)(nelmts*msize));
                if ( buf==NULL){
                    printf( "cannot read into memory\n" );
                    goto error;
                }
                if (H5Aread(attr_id,mtype_id,buf)<0)
                    goto error;
                
                if ((obj_type = H5Rget_obj_type(attr_id,H5R_OBJECT,buf))<0)
                    goto error;
                refbuf=HDcalloc((unsigned)nelmts,msize);
                
                if ( refbuf==NULL){
                    printf( "cannot allocate memory\n" );
                    goto error;
                }
                for ( k=0; k<nelmts; k++)
                {
                    H5E_BEGIN_TRY {
                        if ((refobj_id = H5Rdereference(attr_id,H5R_OBJECT,&buf[k]))<0)
                            goto error;
                    } H5E_END_TRY;
                    /* get the name. a valid name could only occur in the
                    second traversal of the file */
                    if ((refname=MapIdToName(refobj_id,travt))!=NULL)
                    {
                        /* create the reference */
                        if (H5Rcreate(&refbuf[k],fidout,refname,H5R_OBJECT,-1)<0)
                            goto error;
                        if (options->verbose)
                            printf("object <%s> reference created to <%s>\n",name,refname);
                    }
                    close_obj(obj_type,refobj_id);
                }/*  k */
            }/*nelmts*/
            
             /*-------------------------------------------------------------------------
             * copy
             *-------------------------------------------------------------------------
            */
            
            if ((attr_out=H5Acreate(loc_out,name,ftype_id,space_id,H5P_DEFAULT))<0)
                goto error;
            if (nelmts) {
                if(H5Awrite(attr_out,mtype_id,refbuf)<0)
                    goto error;
            }
            
            if (H5Aclose(attr_out)<0)
                goto error;
            
            if (refbuf)
                free(refbuf);
            if (buf)
                free(buf);
            
        }/*H5T_STD_REF_OBJ*/
        
         /*-------------------------------------------------------------------------
         * dataset region references
         *-------------------------------------------------------------------------
        */
        else if (H5Tequal(mtype_id, H5T_STD_REF_DSETREG))
        {
            H5G_obj_t1       obj_type;
            hid_t            refobj_id;
            hdset_reg_ref_t  *refbuf=NULL; /* input buffer for region references */
            hdset_reg_ref_t  *buf=NULL;    /* output buffer */
            const char*      refname;
            unsigned         k;
            
            /*-------------------------------------------------------------------------
            * read input to memory
            *-------------------------------------------------------------------------
            */
            
            if (nelmts)
            {
                buf=(void *) HDmalloc((unsigned)(nelmts*msize));
                if ( buf==NULL){
                    printf( "cannot read into memory\n" );
                    goto error;
                }
                if (H5Aread(attr_id,mtype_id,buf)<0)
                    goto error;
                if ((obj_type = H5Rget_obj_type(attr_id,H5R_DATASET_REGION,buf))<0)
                    goto error;
                
                    /*-------------------------------------------------------------------------
                    * create output
                    *-------------------------------------------------------------------------
                */
                
                refbuf=HDcalloc(sizeof(hdset_reg_ref_t),(size_t)nelmts); /*init to zero */
                if ( refbuf==NULL){
                    printf( "cannot allocate memory\n" );
                    goto error;
                }
                for ( k=0; k<nelmts; k++)
                {
                    H5E_BEGIN_TRY {
                        if ((refobj_id = H5Rdereference(attr_id,H5R_DATASET_REGION,&buf[k]))<0)
                            continue;
                    } H5E_END_TRY;
                    /* get the name. a valid name could only occur in the
                    second traversal of the file */
                    if ((refname=MapIdToName(refobj_id,travt))!=NULL)
                    {
                        hid_t region_id;    /* region id of the referenced dataset */
                        if ((region_id = H5Rget_region(attr_id,H5R_DATASET_REGION,&buf[k]))<0)
                            goto error;
                        /* create the reference, we need the space_id */
                        if (H5Rcreate(&refbuf[k],fidout,refname,H5R_DATASET_REGION,region_id)<0)
                            goto error;
                        if (H5Sclose(region_id)<0)
                            goto error;
                        if (options->verbose)
                            printf("object <%s> region reference created to <%s>\n",name,refname);
                    }
                    close_obj(obj_type,refobj_id);
                }/*  k */
            }/*nelmts */
            
             /*-------------------------------------------------------------------------
             * copy
             *-------------------------------------------------------------------------
            */
            
            if ((attr_out=H5Acreate(loc_out,name,ftype_id,space_id,H5P_DEFAULT))<0)
                goto error;
            if (nelmts) {
                if(H5Awrite(attr_out,mtype_id,refbuf)<0)
                    goto error;
            }
            if (H5Aclose(attr_out)<0)
                goto error;
            if (refbuf)
                free(refbuf);
            if (buf)
                free(buf);
        } /* H5T_STD_REF_DSETREG */
        
          /*-------------------------------------------------------------------------
          * close
          *-------------------------------------------------------------------------
        */
        
        if (H5Tclose(ftype_id)<0) goto error;
        if (H5Tclose(mtype_id)<0) goto error;
        if (H5Sclose(space_id)<0) goto error;
        if (H5Aclose(attr_id)<0) goto error;
 } /* u */
 
 return 0;
 
error:
 H5E_BEGIN_TRY {
     H5Tclose(ftype_id);
     H5Tclose(mtype_id);
     H5Sclose(space_id);
     H5Aclose(attr_id);
     H5Aclose(attr_out);
 } H5E_END_TRY;
 return -1;
}

/*-------------------------------------------------------------------------
 * Function:	close_obj
 *
 * Purpose:	Auxiliary function to close an object
 *
 *-------------------------------------------------------------------------
 */

static void close_obj(H5G_obj_t1 obj_type, hid_t obj_id)
{
    H5E_BEGIN_TRY
    {
        switch (obj_type)
        {
        case H5G_GROUP:
            H5Gclose(obj_id);
            break;
        case H5G_DATASET:
            H5Dclose(obj_id);
            break;
        case H5G_TYPE:
            H5Tclose(obj_id);
            break;
        default:
            break;
        }
    } H5E_END_TRY;
}

/*-------------------------------------------------------------------------
 * Function:	MapIdToName
 *
 * Purpose:	map an object ID to a name
 *
 *-------------------------------------------------------------------------
 */

static const char* MapIdToName(hid_t refobj_id,
                               trav_table_t *travt)
{
    hid_t        id;
    hid_t        fid;
    H5G_stat_t   refstat;    /* Stat for the refobj id */
    H5G_stat_t   objstat;    /* Stat for objects in the file */
    unsigned int i;
    
    /* obtain information to identify the referenced object uniquely */
    if(H5Gget_objinfo(refobj_id, ".", 0, &refstat) <0)
        return NULL;
    
    /* obtains the file ID given an object ID.  This ID must be closed */
    if ((fid = H5Iget_file_id(refobj_id))<0)
    {
        return NULL;
    }
    
    /* linear search */
    for ( i=0; i<travt->nobjs; i++)
    {
        switch ( travt->objs[i].type )
        {
        default:
            break;
            
            /*-------------------------------------------------------------------------
            * H5G_DATASET
            *-------------------------------------------------------------------------
            */
            
        case H5G_DATASET:
            
            if ((id = H5Dopen(fid,travt->objs[i].name))<0)
                return NULL;
            if(H5Gget_objinfo(id, ".", 0, &objstat) <0)
                return NULL;
            if (H5Dclose(id)<0)
                return NULL;
            if (refstat.fileno[0]==objstat.fileno[0] && refstat.fileno[1]==objstat.fileno[1]
                && refstat.objno[0]==objstat.objno[0] && refstat.objno[1]==objstat.objno[1])
            {
                H5Fclose(fid);
                return travt->objs[i].name;
            }
            break;
        }  /* switch */
    } /* i */
    
    if (H5Fclose(fid)<0)
        return NULL;
    
    return NULL;
}

