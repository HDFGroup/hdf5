
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
#include "h5diff.h"
#include "h5tools.h"


/*-------------------------------------------------------------------------
 * local functions
 *-------------------------------------------------------------------------
 */

static const char* MapIdToName(hid_t refobj_id,trav_table_t *travt);
static int copy_refs_attr(hid_t loc_in, hid_t loc_out, pack_opt_t *options,
                          trav_table_t *travt, hid_t fidout);
static herr_t update_ref_value(hid_t obj_id, H5R_type_t ref_type, void *ref_in,
		hid_t fid_out, void *ref_out, trav_table_t *travt);

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
    hid_t     grp_in = (-1);          /* read group ID */
    hid_t     grp_out = (-1);         /* write group ID */
    hid_t     dset_in = (-1);         /* read dataset ID */
    hid_t     dset_out = (-1);        /* write dataset ID */
    hid_t     type_in = (-1);         /* named type ID */
    hid_t     dcpl_id = (-1);         /* dataset creation property list ID */
    hid_t     space_id = (-1);        /* space ID */
    hid_t     ftype_id = (-1);        /* file data type ID */
    hid_t     mtype_id = (-1);        /* memory data type ID */
    size_t    msize;                  /* memory size of memory type */
    hsize_t   nelmts;                 /* number of elements in dataset */
    int       rank;                   /* rank of dataset */
    hsize_t   dims[H5S_MAX_RANK];     /* dimensions of dataset */
    unsigned int i, j;
    int       k;
    named_dt_t *named_dt_head=NULL;   /* Pointer to the stack of named datatypes
                                         copied */

    /*-------------------------------------------------------------------------
    * browse
    *-------------------------------------------------------------------------
    */
    for(i = 0; i < travt->nobjs; i++) {
        switch(travt->objs[i].type)
        {
            /*-------------------------------------------------------------------------
            * H5TRAV_TYPE_GROUP
            *-------------------------------------------------------------------------
            */
            case H5TRAV_TYPE_GROUP:
                /*-------------------------------------------------------------------------
                * copy referenced objects in attributes
                *-------------------------------------------------------------------------
                */
                if((grp_out = H5Gopen2(fidout, travt->objs[i].name, H5P_DEFAULT)) < 0)
                    goto error;

                if((grp_in = H5Gopen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
                    goto error;

                if(copy_refs_attr(grp_in, grp_out, options, travt, fidout) < 0)
                    goto error;

                if(H5Gclose(grp_out) < 0)
                    goto error;
                if(H5Gclose(grp_in) < 0)
                    goto error;

                /*-------------------------------------------------------------------------
                * check for hard links
                *-------------------------------------------------------------------------
                */
                if(travt->objs[i].nlinks)
                    for(j = 0; j < travt->objs[i].nlinks; j++)
                        H5Lcreate_hard(fidout, travt->objs[i].name, H5L_SAME_LOC, travt->objs[i].links[j].new_name, H5P_DEFAULT, H5P_DEFAULT);
                break;

            /*-------------------------------------------------------------------------
            * H5TRAV_TYPE_DATASET
            *-------------------------------------------------------------------------
            */
            case H5TRAV_TYPE_DATASET:
                if((dset_in = H5Dopen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
                    goto error;
                if((space_id = H5Dget_space(dset_in)) < 0)
                    goto error;
                if((ftype_id = H5Dget_type(dset_in)) < 0)
                    goto error;
                if((dcpl_id = H5Dget_create_plist(dset_in)) < 0)
                    goto error;
                if((rank = H5Sget_simple_extent_ndims(space_id)) < 0)
                    goto error;
                if(H5Sget_simple_extent_dims(space_id, dims, NULL) < 0)
                    goto error;
                nelmts = 1;
                for(k = 0; k < rank; k++)
                    nelmts *= dims[k];

                if((mtype_id = h5tools_get_native_type(ftype_id)) < 0)
                    goto error;

                if((msize = H5Tget_size(mtype_id)) == 0)
                    goto error;

                /*-------------------------------------------------------------------------
                 * check if the dataset creation property list has filters that
                 * are not registered in the current configuration
                 * 1) the external filters GZIP and SZIP might not be available
                 * 2) the internal filters might be turned off
                 *-------------------------------------------------------------------------
                 */
                if(h5tools_canreadf(NULL, dcpl_id) == 1) {
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
                    if(H5Tequal(mtype_id, H5T_STD_REF_OBJ)) {
                        hid_t            refobj_id;
                        hobj_ref_t       *refbuf = NULL; /* buffer for object references */
                        hobj_ref_t       *buf = NULL;
                        const char*      refname;
                        unsigned         u;

                        /*-------------------------------------------------------------------------
                        * read to memory
                        *-------------------------------------------------------------------------
                        */
                        if(nelmts) {
                            buf = (hobj_ref_t *)HDmalloc((unsigned)(nelmts * msize));
                            if(buf==NULL) {
                                printf("cannot read into memory\n" );
                                goto error;
                            } /* end if */
                            if(H5Dread(dset_in, mtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
                                goto error;

                            refbuf = (hobj_ref_t*) HDcalloc((unsigned)nelmts, msize);
                            if(refbuf == NULL){
                                printf("cannot allocate memory\n" );
                                goto error;
                            } /* end if */
                            for(u = 0; u < nelmts; u++) {
                                H5E_BEGIN_TRY {
                                    if((refobj_id = H5Rdereference2(dset_in, H5P_DEFAULT, H5R_OBJECT, &buf[u])) < 0)
                                        continue;
                                } H5E_END_TRY;

                                /* get the name. a valid name could only occur
                                 * in the second traversal of the file
                                 */
                                if((refname = MapIdToName(refobj_id, travt)) != NULL) {
                                    /* create the reference, -1 parameter for objects */
                                    if(H5Rcreate(&refbuf[u], fidout, refname, H5R_OBJECT, -1) < 0)
                                        goto error;
                                    if(options->verbose)
                                    {


                                        printf(FORMAT_OBJ,"dset",travt->objs[i].name );
                                        printf("object <%s> object reference created to <%s>\n",
                                            travt->objs[i].name,
                                            refname);
                                    }
                                } /*refname*/
                                H5Oclose(refobj_id);
                            } /* u */
                        } /*nelmts*/

                        /*-------------------------------------------------------------------------
                        * create/write dataset/close
                        *-------------------------------------------------------------------------
                        */
                        if((dset_out = H5Dcreate2(fidout, travt->objs[i].name, mtype_id, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
                            goto error;
                        if(nelmts)
                            if(H5Dwrite(dset_out, mtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, refbuf) < 0)
                                goto error;

                        if(buf)
                            HDfree(buf);
                        if(refbuf)
                            HDfree(refbuf);

                       /*------------------------------------------------------
                        * copy attrs
                        *----------------------------------------------------*/
                        if(copy_attr(dset_in, dset_out, &named_dt_head, travt, options) < 0)
                            goto error;
                    } /*H5T_STD_REF_OBJ*/

                    /*-------------------------------------------------------------------------
                    * dataset region references
                    *-------------------------------------------------------------------------
                    */
                    else if(H5Tequal(mtype_id, H5T_STD_REF_DSETREG))
                    {
                        hid_t            refobj_id;
                        hdset_reg_ref_t  *refbuf = NULL; /* input buffer for region references */
                        hdset_reg_ref_t  *buf = NULL;    /* output buffer */
                        const char*      refname;
                        unsigned         u;

                        /*-------------------------------------------------------------------------
                        * read input to memory
                        *-------------------------------------------------------------------------
                        */
                        if(nelmts) {
                            buf = (hdset_reg_ref_t *)HDmalloc((unsigned)(nelmts * msize));
                            if(buf == NULL) {
                                printf("cannot read into memory\n");
                                goto error;
                            } /* end if */
                            if(H5Dread(dset_in, mtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
                                goto error;

                            /*-------------------------------------------------------------------------
                            * create output
                            *-------------------------------------------------------------------------
                            */
                            refbuf = (hdset_reg_ref_t *)HDcalloc(sizeof(hdset_reg_ref_t), (size_t)nelmts); /*init to zero */
                            if(refbuf == NULL) {
                                printf("cannot allocate memory\n");
                                goto error;
                            } /* end if */

                            for(u = 0; u < nelmts; u++) {
                                H5E_BEGIN_TRY {
                                    if((refobj_id = H5Rdereference2(dset_in, H5P_DEFAULT, H5R_DATASET_REGION, &buf[u])) < 0)
                                        continue;
                                } H5E_END_TRY;

                                /* get the name. a valid name could only occur
                                 * in the second traversal of the file
                                 */
                                if((refname = MapIdToName(refobj_id, travt)) != NULL) {
                                    hid_t region_id;    /* region id of the referenced dataset */

                                    if((region_id = H5Rget_region(dset_in, H5R_DATASET_REGION, &buf[u])) < 0)
                                        goto error;

                                    /* create the reference, we need the space_id */
                                    if(H5Rcreate(&refbuf[u], fidout, refname, H5R_DATASET_REGION, region_id) < 0)
                                        goto error;
                                    if(H5Sclose(region_id) < 0)
                                        goto error;
                                    if(options->verbose)
                                    {



                                        printf(FORMAT_OBJ,"dset",travt->objs[i].name );
                                        printf("object <%s> region reference created to <%s>\n",
                                            travt->objs[i].name,
                                            refname);
                                    }
                                } /*refname*/
                                H5Oclose(refobj_id);
                            } /* u */
                        } /*nelmts*/

                        /*-------------------------------------------------------------------------
                        * create/write dataset/close
                        *-------------------------------------------------------------------------
                        */
                        if((dset_out = H5Dcreate2(fidout, travt->objs[i].name, mtype_id, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
                            goto error;
                        if(nelmts)
                            if(H5Dwrite(dset_out, mtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, refbuf) < 0)
                                goto error;

                        if(buf)
                            HDfree(buf);
                        if(refbuf)
                            HDfree(refbuf);

                       /*-----------------------------------------------------
                        * copy attrs
                        *----------------------------------------------------*/
                        if(copy_attr(dset_in, dset_out, &named_dt_head, travt, options) < 0)
                            goto error;
                    } /* H5T_STD_REF_DSETREG */
                    /*-------------------------------------------------------------------------
                    * not references, open previously created object in 1st traversal
                    *-------------------------------------------------------------------------
                    */
                    else {
                        if((dset_out = H5Dopen2(fidout, travt->objs[i].name, H5P_DEFAULT)) < 0)
                            goto error;
                    } /* end else */

                    HDassert(dset_out != FAIL);

                    /*-------------------------------------------------------------------------
                    * copy referenced objects in attributes
                    *-------------------------------------------------------------------------
                    */
                    if(copy_refs_attr(dset_in, dset_out, options, travt, fidout) < 0)
                        goto error;

                    /*-------------------------------------------------------------------------
                    * check for hard links
                    *-------------------------------------------------------------------------
                    */
                    if(travt->objs[i].nlinks)
                        for(j = 0; j < travt->objs[i].nlinks; j++)
                            H5Lcreate_hard(fidout, travt->objs[i].name, H5L_SAME_LOC, travt->objs[i].links[j].new_name, H5P_DEFAULT, H5P_DEFAULT);

                    if(H5Dclose(dset_out) < 0)
                        goto error;
                } /*can_read*/

                /*-------------------------------------------------------------------------
                * close
                *-------------------------------------------------------------------------
                */
                if(H5Tclose(ftype_id) < 0)
                    goto error;
                if(H5Tclose(mtype_id) < 0)
                    goto error;
                if(H5Pclose(dcpl_id) < 0)
                    goto error;
                if(H5Sclose(space_id) < 0)
                    goto error;
                if(H5Dclose(dset_in) < 0)
                    goto error;
                break;

            /*-------------------------------------------------------------------------
            * H5TRAV_TYPE_NAMED_DATATYPE
            *-------------------------------------------------------------------------
            */
            case H5TRAV_TYPE_NAMED_DATATYPE:
                if((type_in = H5Topen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
                    goto error;
                if(H5Tclose(type_in) < 0)
                    goto error;
                break;

            /*-------------------------------------------------------------------------
            * H5TRAV_TYPE_LINK
            *-------------------------------------------------------------------------
            */
            case H5TRAV_TYPE_LINK:
                /*nothing to do */
                break;

            case H5TRAV_TYPE_UNKNOWN:
            case H5TRAV_TYPE_UDLINK:
                goto error;

            default:
                break;
        } /* end switch */
    } /* end for */

    /* Finalize (link) the stack of named datatypes (if any)
     * This function is paired with copy_named_datatype() which is called
     * in copy_attr(), so need to free.
     */
    named_datatype_free(&named_dt_head, 0);

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
        named_datatype_free(&named_dt_head, 0);
    } H5E_END_TRY;

    return -1;
}


/*-------------------------------------------------------------------------
 * Function: copy_refs_attr
 *
 * Purpose: duplicate all referenced HDF5 located in attributes
 *  relative to LOC_IN, which is obtained either from
 * loc_id = H5Gopen2(fid, name, H5P_DEFAULT);
 * loc_id = H5Dopen2(fid, name, H5P_DEFAULT);
 * loc_id = H5Topen2(fid, name, H5P_DEFAULT);
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Modifier: xcao@hdfgroup.org, 9/12/2011
 * 	         Update values of references(object and region) for the following types:
 *               1) References,
 *               2) ARRAY of reference,
 *               3) VLEN of references.
 *               4) COMPOUND of references.
 *          This function does not handle references in other complicated structures,
 *		    such as references in nested compound datatypes.
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
    hid_t      attr_id = -1;      /* attr ID */
    hid_t      attr_out = -1;     /* attr ID */
    hid_t      space_id = -1;     /* space ID */
    hid_t      ftype_id = -1;     /* file data type ID */
    hid_t      mtype_id = -1;     /* memory data type ID */
    size_t     msize;             /* memory size of type */
    hsize_t    nelmts;            /* number of elements in dataset */
    hsize_t    dims[H5S_MAX_RANK];/* dimensions of dataset */
    char       name[255];
    H5O_info_t oinfo;           /* Object info */
    unsigned   u, i, j;
    int        rank;
    H5T_class_t type_class = -1;
    hbool_t    is_ref=0, is_ref_vlen=0, is_ref_array=0, is_ref_comp=0;
    void  *refbuf = NULL;
    void  *buf = NULL;
    const char* refname;
    int   *ref_comp_index = NULL;
    size_t *ref_comp_size = NULL;
    int    ref_comp_field_n = 0;


    if(H5Oget_info(loc_in, &oinfo) < 0)
        goto error;

    for(u = 0; u < (unsigned)oinfo.num_attrs; u++)
    {
    	is_ref = is_ref_vlen = is_ref_array = is_ref_comp = 0;

        /* open attribute */
        if((attr_id = H5Aopen_by_idx(loc_in, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;

        /* get the file datatype  */
        if((ftype_id = H5Aget_type(attr_id)) < 0)
            goto error;

        type_class = H5Tget_class(ftype_id);

        if((mtype_id = h5tools_get_native_type(ftype_id)) < 0)
            goto error;

        if((msize = H5Tget_size(mtype_id)) == 0)
            goto error;

        is_ref = (type_class == H5T_REFERENCE);

        if (type_class == H5T_VLEN ) {
        	hid_t base_type = -1;
        	base_type = H5Tget_super(ftype_id);
        	is_ref_vlen = (H5Tget_class(base_type)==H5T_REFERENCE);
        	msize = H5Tget_size(base_type);
        	H5Tclose(base_type);
        }

        if (type_class == H5T_ARRAY ) {
        	hid_t base_type = -1;
        	base_type = H5Tget_super(ftype_id);
        	is_ref_array = (H5Tget_class(base_type)==H5T_REFERENCE);
        	msize = H5Tget_size(base_type);
        	H5Tclose(base_type);
        }

        if (type_class == H5T_COMPOUND) {
        	int nmembers = H5Tget_nmembers(ftype_id) ;
        	if (nmembers < 1)
        		goto error;

            ref_comp_index = (int *)HDmalloc(nmembers*sizeof (int));
            ref_comp_size = (size_t *)HDmalloc(nmembers*sizeof(ref_comp_size));
            ref_comp_field_n = 0;

        	for (i=0; i<(unsigned)nmembers; i++) {
        		hid_t mtid = H5Tget_member_type( ftype_id, i );
        		if ((H5Tget_class(mtid)==H5T_REFERENCE)) {
        			ref_comp_index[ref_comp_field_n] = i;
        			ref_comp_size[ref_comp_field_n] = H5Tget_size(mtid);
        			ref_comp_field_n++;
        		}
        		H5Tclose(mtid);
        	}
            
            /* if compound don't contain reference type member, free the above 
             * mallocs. Otherwise there can be memory leaks by the 'continue' 
             * statement below. */
            if (!ref_comp_field_n) 
            {
                if (ref_comp_index) 
                {
                	HDfree(ref_comp_index);
                	ref_comp_index = NULL;
                }

                if (ref_comp_size) 
                {
            	    HDfree(ref_comp_size);
        	        ref_comp_size = NULL;
                }
            }
        }

        is_ref_comp = (ref_comp_field_n > 0);

        if (!(is_ref || is_ref_vlen || is_ref_array || is_ref_comp)) {
        	H5Tclose(mtype_id);
           	H5Tclose(ftype_id);
        	H5Aclose(attr_id);
        	continue;
        }

        /* get name */
        if(H5Aget_name(attr_id, 255, name) < 0)
            goto error;

        /* get the dataspace handle  */
        if((space_id = H5Aget_space(attr_id)) < 0)
            goto error;

        /* get dimensions  */
        if((rank = H5Sget_simple_extent_dims(space_id, dims, NULL)) < 0)
            goto error;


        /*-------------------------------------------------------------------------
        * elements
        *-------------------------------------------------------------------------
        */
        nelmts = 1;
        for(j = 0; j < (unsigned)rank; j++)
            nelmts *= dims[j];

        if (is_ref_array) {
        	unsigned        array_rank = 0;
        	hsize_t    array_size = 1;
        	hsize_t array_dims[H5S_MAX_RANK];
        	hid_t base_type = -1;
        	base_type = H5Tget_super(ftype_id);
        	msize = H5Tget_size(base_type);
           	H5Tclose(base_type);

        	array_rank = H5Tget_array_ndims(mtype_id);
        	H5Tget_array_dims2(mtype_id, array_dims);
            for(j = 0; j <array_rank; j++)
            	array_size *= array_dims[j];
            nelmts *= array_size;
         }

        if((attr_out = H5Acreate2(loc_out, name, ftype_id, space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;

        if (nelmts>0)
        {
            /* handle object references */
            if((is_ref || is_ref_array) && (H5R_OBJ_REF_BUF_SIZE==msize))
            {
                buf = (hobj_ref_t *)HDmalloc((unsigned)(nelmts * msize));
                if(buf == NULL)
                {
                    printf("cannot read into memory\n");
                    goto error;
                } /* end if */
                if(H5Aread(attr_id, mtype_id, buf) < 0)
                    goto error;

                refbuf = (hobj_ref_t *)HDcalloc((unsigned)nelmts, msize);
                if(refbuf == NULL)
                {
                    printf( "cannot allocate memory\n" );
                    goto error;
                } /* end if */

                for(i = 0; i < (unsigned)nelmts; i++)
                {
                    if (update_ref_value(attr_id, H5R_OBJECT, &((hobj_ref_t *)buf)[i], fidout, &((hobj_ref_t *)refbuf)[i], travt)<0)
                		continue;
                    if(options->verbose)
                        printf("object <%s> reference created to <%s>\n", name, refname);
                } /* k */
            }/*H5T_STD_REF_OBJ*/
            /* handle region references */
            else if((is_ref || is_ref_array) && (H5R_DSET_REG_REF_BUF_SIZE == msize))
            {
                buf = (hdset_reg_ref_t *)HDmalloc((unsigned)(nelmts * msize));
                if(buf == NULL)
                {
                    printf( "cannot read into memory\n" );
                    goto error;
                } /* end if */
                if(H5Aread(attr_id, mtype_id, buf) < 0)
                    goto error;

                /*-------------------------------------------------------------------------
                * create output
                *-------------------------------------------------------------------------
                */
                refbuf = (hdset_reg_ref_t *)HDcalloc(sizeof(hdset_reg_ref_t), (size_t)nelmts); /*init to zero */
                if(refbuf == NULL)
                {
                    printf( "cannot allocate memory\n" );
                    goto error;
                } /* end if */

                for(i = 0; i < (unsigned)nelmts; i++)
                {
                    if (update_ref_value(attr_id, H5R_DATASET_REGION, &((hdset_reg_ref_t *)buf)[i], fidout, &((hdset_reg_ref_t *)refbuf)[i], travt)<0)
                		continue;
                    if(options->verbose)
                        printf("object <%s> region reference created to <%s>\n", name, refname);
                }
            } /* H5T_STD_REF_DSETREG */
            else if (is_ref_vlen) {
            	/* handle VLEN of references */

                buf = (hvl_t *)HDmalloc((unsigned)(nelmts * sizeof(hvl_t)));
                refbuf = buf; /* reuse the read buffer for write */

                if(buf == NULL)
                {
                    printf( "cannot read into memory\n" );
                    goto error;
                } /* end if */

                if(H5Aread(attr_id, mtype_id, buf) < 0)
                    goto error;

 				if (H5R_OBJ_REF_BUF_SIZE==msize) {
					hobj_ref_t ref_out;
					for (i=0; i<(unsigned)nelmts; i++) {
						hobj_ref_t *ptr = (hobj_ref_t *)((hvl_t *)buf)[i].p;
						for (j=0; j<((hvl_t *)buf)[i].len; j++ ) {
							if (update_ref_value(attr_id, H5R_OBJECT, &(ptr[j]), fidout, &ref_out, travt)<0)
								continue;
							HDmemcpy(&(ptr[j]), &ref_out, msize);
						}
					}  /* for (i=0; i<nelems; i++) */
				} else if (H5R_DSET_REG_REF_BUF_SIZE == msize) {

					hdset_reg_ref_t ref_out;
					for (i=0; i<(unsigned)nelmts; i++) {
						hdset_reg_ref_t *ptr = (hdset_reg_ref_t *)((hvl_t *)buf)[i].p;
						for (j=0; j<((hvl_t *)buf)[i].len; j++ ) {
							if (update_ref_value(attr_id, H5R_DATASET_REGION, &(ptr[j]), fidout, &ref_out, travt)<0)
								continue;
							HDmemcpy(&(ptr[j]), &ref_out, msize);
						}
					}  /* for (i=0; i<nelems; i++) */
				}
            } /* else if (is_ref_vlen) */
            else if (is_ref_comp) {
            	/* handle ref fields in a compound */

                buf = HDmalloc((unsigned)(nelmts * msize));
                refbuf = buf; /* reuse the read buffer for write */

                if(buf == NULL)
                {
                    printf( "cannot read into memory\n" );
                    goto error;
                } /* end if */

                if(H5Aread(attr_id, mtype_id, buf) < 0)
                    goto error;

                for (i=0; i<(unsigned)nelmts; i++) {
                	for (j=0; j<(unsigned)ref_comp_field_n; j++) {
                		if (ref_comp_size[j] == H5R_OBJ_REF_BUF_SIZE) {
                			int idx = i*msize+H5Tget_member_offset( mtype_id, (unsigned)ref_comp_index[j]);
                			hobj_ref_t ref_out;
                			if (update_ref_value(attr_id, H5R_OBJECT, (hobj_ref_t *)(((char *)buf)+idx), fidout, &ref_out, travt)<0)
                				continue;
                			HDmemcpy(((char *)buf)+idx, &ref_out, ref_comp_size[j]);
                 		} /* if */
                		else if (ref_comp_size[j] == H5R_DSET_REG_REF_BUF_SIZE) {
                			int idx = i*msize+H5Tget_member_offset( mtype_id, (unsigned)ref_comp_index[j]);
                			hdset_reg_ref_t ref_out;
                			if (update_ref_value(attr_id, H5R_DATASET_REGION, (hdset_reg_ref_t *)(((char *)buf)+idx), fidout, &ref_out, travt)<0)
                				continue;
                			HDmemcpy(((char *)buf)+idx, &ref_out, ref_comp_size[j]);
                 		} /* else if */
                	} /* j */
                } /* i */
            } /* else if (is_ref_comp) */

            if(H5Awrite(attr_out, mtype_id, refbuf) < 0)
                 goto error;

            if (is_ref_vlen && buf)
            	H5Dvlen_reclaim (mtype_id, space_id, H5P_DEFAULT, buf);
        } /* if (nelmts) */

        if (refbuf == buf)
        	refbuf = NULL; /* set it to NULL to avoid double free since buf and refbuf are the same. */

        if(buf) {
            HDfree(buf);
            buf = NULL;
        }

        if(refbuf) {
            HDfree(refbuf);
            refbuf = NULL;
        }

        if (ref_comp_index) {
        	HDfree(ref_comp_index);
        	ref_comp_index = NULL;
        }

        if (ref_comp_size) {
        	HDfree(ref_comp_size);
        	ref_comp_size = NULL;
        }

        if(H5Aclose(attr_out) < 0)
            goto error;

        /*-------------------------------------------------------------------------
        * close
        *-------------------------------------------------------------------------
        */
        if(H5Tclose(ftype_id) < 0)
            goto error;
        if(H5Tclose(mtype_id) < 0)
            goto error;
        if(H5Sclose(space_id) < 0)
            goto error;
        if(H5Aclose(attr_id) < 0)
            goto error;
    } /* for(u = 0; u < (unsigned)oinfo.num_attrs; u++) */

    return 0;

error:
	if(refbuf)
		HDfree(refbuf);
	if(buf)
		HDfree(buf);

    if (ref_comp_index)
    	HDfree(ref_comp_index);

    if (ref_comp_size)
    	HDfree(ref_comp_size);

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
 * Function:	MapIdToName
 *
 * Purpose:	map a ID from a reference to a dataset name
 *
 *-------------------------------------------------------------------------
 */
static const char*
MapIdToName(hid_t refobj_id, trav_table_t *travt)
{
    unsigned int u;
    const char* ret = NULL;

    /* linear search */
    for(u = 0; u < travt->nobjs; u++) {
        if(travt->objs[u].type == H5O_TYPE_DATASET ||
                travt->objs[u].type == H5O_TYPE_GROUP ||
                travt->objs[u].type == H5O_TYPE_NAMED_DATATYPE) {
            H5O_info_t   ref_oinfo;     /* Stat for the refobj id */

            /* obtain information to identify the referenced object uniquely */
            if(H5Oget_info(refobj_id, &ref_oinfo) < 0)
                goto out;

            if(ref_oinfo.addr == travt->objs[u].objno) {
                ret = travt->objs[u].name;
                goto out;
            } /* end if */
        }  /* end if */
    } /* u */

out:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:	Update_Ref_value
 *
 * Purpose:	Update a reference value
 *
 * Programmer: xcao@hdfgroup.org 9/12/2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t update_ref_value(hid_t obj_id, H5R_type_t ref_type, void *ref_in,
		hid_t fid_out, void *ref_out, trav_table_t *travt)
{
	herr_t ret = -1;
	const char* ref_obj_name;
	hid_t space_id=-1, ref_obj_id=-1;

    ref_obj_id = H5Rdereference2(obj_id, H5P_DEFAULT, ref_type, ref_in);
    if (ref_obj_id<0)
       goto done;

	ref_obj_name = MapIdToName(ref_obj_id, travt);
	if (ref_obj_name == NULL)
		goto done;

	if (ref_type == H5R_DATASET_REGION) {
		space_id = H5Rget_region(obj_id, H5R_DATASET_REGION, ref_in);
		if (space_id < 0)
			goto done;
	}

    ret = H5Rcreate(ref_out, fid_out, ref_obj_name, ref_type, space_id);

    if (ret < 0)
    	goto done;

	ret = 0;

done:
    H5E_BEGIN_TRY {
      H5Sclose(space_id);
      H5Oclose(ref_obj_id);
    } H5E_END_TRY;

	return ret;
}

