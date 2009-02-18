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

#include "h5tools.h"
#include "h5diff.h"
#include "H5private.h"

/*-------------------------------------------------------------------------
 * Function: diff_attr
 *
 * Purpose: compare attributes located in LOC1_ID and LOC2_ID, which are
 *  obtained either from
 * loc_id = H5Gopen( fid, name);
 * loc_id = H5Dopen( fid, name);
 * loc_id = H5Topen( fid, name);
 *
 * Return: number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November, 03, 2003
 *
 * Modifications:
 *  March, 02, 2007: return the number of differences found
 *
 *-------------------------------------------------------------------------
 */

hsize_t diff_attr(hid_t loc1_id,
                  hid_t loc2_id,
                  const char *path1,
                  const char *path2,
                  diff_opt_t *options)
{
    hid_t      attr1_id=-1;     /* attr ID */
    hid_t      attr2_id=-1;     /* attr ID */
    hid_t      space1_id=-1;    /* space ID */
    hid_t      space2_id=-1;    /* space ID */
    hid_t      ftype1_id=-1;    /* file data type ID */
    hid_t      ftype2_id=-1;    /* file data type ID */
    hid_t      mtype1_id=-1;    /* memory data type ID */
    hid_t      mtype2_id=-1;    /* memory data type ID */
    size_t     msize1;          /* memory size of memory type */
    size_t     msize2;          /* memory size of memory type */
    void       *buf1=NULL;      /* data buffer */
    void       *buf2=NULL;      /* data buffer */
    hsize_t    nelmts1;         /* number of elements in dataset */
    int        rank1;           /* rank of dataset */
    int        rank2;           /* rank of dataset */
    hsize_t    dims1[H5S_MAX_RANK];/* dimensions of dataset */
    hsize_t    dims2[H5S_MAX_RANK];/* dimensions of dataset */
    char       name1[512];
    char       name2[512];
    char       np1[512];
    char       np2[512];
    int        n1, n2, i, j;
    hsize_t    nfound=0;
    hsize_t    nfound_total=0;
    
    if ((n1 = H5Aget_num_attrs(loc1_id))<0)
        goto error;
    if ((n2 = H5Aget_num_attrs(loc2_id))<0)
        goto error;
    
    if (n1!=n2)
        return 1;
    
    for ( i = 0; i < n1; i++)
    {
        /* reset buffers for every attribute, we might goto out and call free */
        buf1=NULL;
        buf2=NULL;
        
        /* open attribute */
        if ((attr1_id = H5Aopen_idx(loc1_id, (unsigned)i))<0)
            goto error;
        /* get name */
        if (H5Aget_name( attr1_id, 255, name1 )<0)
            goto error;
        
        /* use the name on the first file to open the second file */
        H5E_BEGIN_TRY 
        {
            if ((attr2_id = H5Aopen_name(loc2_id, name1))<0)
            {
                goto error;
            }
        } H5E_END_TRY;
        
        /* get name */
        if (H5Aget_name( attr2_id, 255, name2 )<0)
            goto error;
        
        /* get datatypes  */
        if ((ftype1_id = H5Aget_type( attr1_id )) < 0 )
            goto error;
        if ((ftype2_id = H5Aget_type( attr2_id )) < 0 )
            goto error;
        if ((mtype1_id=h5tools_get_native_type(ftype1_id))<0)
            goto error;
        if ((mtype2_id=h5tools_get_native_type(ftype2_id))<0)
            goto error;
        if ((msize1=H5Tget_size(mtype1_id))==0)
            goto error;
        if ((msize2=H5Tget_size(mtype2_id))==0)
            goto error;
        
        /* get the dataspace handle  */
        if ((space1_id = H5Aget_space( attr1_id )) < 0 )
            goto error;
        if ((space2_id = H5Aget_space( attr2_id )) < 0 )
            goto error;
        
        /* get dimensions  */
        if ( (rank1 = H5Sget_simple_extent_dims(space1_id, dims1, NULL)) < 0 )
            goto error;
        if ( (rank2 = H5Sget_simple_extent_dims(space2_id, dims2, NULL)) < 0 )
            goto error;
        
        /*-------------------------------------------------------------------------
         * check for comparable TYPE and SPACE
         *-------------------------------------------------------------------------
        */
        
        if ( msize1 != msize2
            || 
            diff_can_type(ftype1_id,
            ftype2_id,
            rank1,
            rank2,
            dims1,
            dims2,
            NULL,
            NULL,
            name1,
            name2,
            options,
            0)!=1)
        {
            
                       
            if (H5Tclose(ftype1_id)<0) 
                goto error;
            if (H5Tclose(ftype2_id)<0) 
                goto error;
            if (H5Sclose(space1_id)<0) 
                goto error;
            if (H5Sclose(space2_id)<0) 
                goto error;
            if (H5Aclose(attr1_id)<0) 
                goto error;
            if (H5Aclose(attr2_id)<0) 
                goto error;
            if (H5Tclose(mtype1_id)<0) 
                goto error;
            if (H5Tclose(mtype2_id)<0) 
                goto error;
            
            continue;
            
            
        }
        
        
        /*-------------------------------------------------------------------------
        * read 
        *-------------------------------------------------------------------------
        */
        nelmts1=1;
        for (j=0; j<rank1; j++)
            nelmts1*=dims1[j];
        
        buf1=(void *) HDmalloc((unsigned)(nelmts1*msize1));
        buf2=(void *) HDmalloc((unsigned)(nelmts1*msize2));
        if ( buf1==NULL || buf2==NULL){
            printf( "cannot read into memory\n" );
            goto error;
        }
        if (H5Aread(attr1_id,mtype1_id,buf1)<0)
            goto error;
        if (H5Aread(attr2_id,mtype2_id,buf2)<0)
            goto error;
        
        /* format output string */
        sprintf(np1,"%s of <%s>",name1,path1);
        sprintf(np2,"%s of <%s>",name2,path2);
        
        /*-------------------------------------------------------------------------
        * array compare
        *-------------------------------------------------------------------------
        */
        
        /* always print name */
        if (options->m_verbose)
        {
            do_print_objname ("attribute", np1, np2);
            nfound = diff_array(buf1,
                buf2,
                nelmts1,
                (hsize_t)0,
                rank1,
                dims1,
                options,
                np1,
                np2,
                mtype1_id,
                attr1_id,
                attr2_id);
            print_found(nfound);
            
        }
        /* check first if we have differences */
        else
        {
            if (options->m_quiet==0)
            {
                /* shut up temporarily */
                options->m_quiet=1;
                nfound = diff_array(buf1,
                    buf2,
                    nelmts1,
                    (hsize_t)0,
                    rank1,
                    dims1,
                    options,
                    np1,
                    np2,
                    mtype1_id,
                    attr1_id,
                    attr2_id);
                /* print again */
                options->m_quiet=0;
                if (nfound)
                {
                    do_print_objname ("attribute", np1, np2);
                    nfound = diff_array(buf1,
                        buf2,
                        nelmts1,
                        (hsize_t)0,
                        rank1,
                        dims1,
                        options,
                        np1,
                        np2,
                        mtype1_id,
                        attr1_id,
                        attr2_id);
                    print_found(nfound);
                } /*if*/
            } /*if*/
            /* in quiet mode, just count differences */
            else
            {
                nfound = diff_array(buf1,
                    buf2,
                    nelmts1,
                    (hsize_t)0,
                    rank1,
                    dims1,
                    options,
                    np1,
                    np2,
                    mtype1_id,
                    attr1_id,
                    attr2_id);
            } /*else quiet */
        } /*else verbose */
        
        
       /*-------------------------------------------------------------------------
        * close
        *-------------------------------------------------------------------------
        */
        
        if (H5Tclose(ftype1_id)<0) goto error;
        if (H5Tclose(ftype2_id)<0) goto error;
        if (H5Tclose(mtype1_id)<0) goto error;
        if (H5Tclose(mtype2_id)<0) goto error;
        if (H5Sclose(space1_id)<0) goto error;
        if (H5Sclose(space2_id)<0) goto error;
        if (H5Aclose(attr1_id)<0) goto error;
        if (H5Aclose(attr2_id)<0) goto error;
        if (buf1)
            HDfree(buf1);
        if (buf2)
            HDfree(buf2);
        
        nfound_total += nfound;
 } /* i */
 
 return nfound_total;
 
error:
 H5E_BEGIN_TRY {
     H5Tclose(ftype1_id);
     H5Tclose(ftype2_id);
     H5Tclose(mtype1_id);
     H5Tclose(mtype2_id);
     H5Sclose(space1_id);
     H5Sclose(space2_id);
     H5Aclose(attr1_id);
     H5Aclose(attr2_id);
     if (buf1)
         HDfree(buf1);
     if (buf2)
         HDfree(buf2);
 } H5E_END_TRY;
 
 options->err_stat=1;
 return nfound_total;
}


