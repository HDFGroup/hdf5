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

#define H5G_PACKAGE		/*suppress error about including H5Gpkg   */

#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5Sprivate.h"		/* Dataspaces		  		*/
#include "H5WBprivate.h"        /* Wrapped Buffers                      */
#include "H5VLiod_server.h"

#ifdef H5_HAVE_EFF

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              February, 2013
 *
 * Purpose:	The IOD plugin server utility routines.
 */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_traverse
 *
 * Purpose: 
 *     Function to Traverse the path in IOD KV objects given the
 *     starting location ID, object handle, and path name. This walks
 *     through the IOD KV objects to get to the last component in the
 *     path. The last component is not opened. Usually this is called when
 *     creating an object. The Function returns the iod ID and object
 *     handle of the before to last component, and the last component
 *     string, if the user requests it.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_server_traverse(iod_handle_t coh, iod_obj_id_t loc_id, iod_handles_t loc_handle, 
                         const char *path, iod_trans_id_t rtid, hbool_t create_interm_grps,
                         /* out */char **last_comp, /* out */iod_obj_id_t *iod_id, 
                         /* out */iod_handles_t *iod_oh)
{
    char comp_buf[1024];     /* Temporary buffer for path components */
    char *comp;              /* Pointer to buffer for path components */
    H5WB_t *wb = NULL;       /* Wrapped buffer for temporary buffer */
    size_t nchars;	     /* component name length	*/
    iod_handles_t cur_oh;
    iod_handle_t prev_oh;
    iod_obj_id_t cur_id;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Creating intermediate groups is not supported for now */
    assert(FALSE == create_interm_grps);

    cur_oh.rd_oh.cookie = loc_handle.rd_oh.cookie;
    cur_id = loc_id;

    /* open the current group */
    if(cur_oh.rd_oh.cookie == IOD_OH_UNDEFINED) {
        if (iod_obj_open_read(coh, loc_id, NULL /*hints*/, &cur_oh.rd_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");
    }

    /* Wrap the local buffer for serialized header info */
    if(NULL == (wb = H5WB_wrap(comp_buf, sizeof(comp_buf))))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wrap buffer")
    /* Get a pointer to a buffer that's large enough  */
    if(NULL == (comp = (char *)H5WB_actual(wb, (HDstrlen(path) + 1))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't get actual buffer")

    /* Traverse the path */
    while((path = H5G__component(path, &nchars)) && *path) {
        const char *s;                  /* Temporary string pointer */
        iod_size_t kv_size;
        H5VL_iod_link_t value;

        /* Copy the component path into a null-terminated buffer. */
	HDmemcpy(comp, path, nchars);
	comp[nchars] = '\0';

	/*
	 * The special path `.' is a no-op.
	 */
	if('.' == comp[0] && !comp[1]) {
	    path += nchars;
	    continue;
	} /* end if */

        /* Check if this is the last component of the path */
        if(!((s = H5G__component(path + nchars, NULL)) && *s)) {
            if(last_comp) {
                *last_comp = HDstrdup(comp);
            }
            break;
        }

        kv_size = sizeof(H5VL_iod_link_t);

        prev_oh.cookie = cur_oh.rd_oh.cookie;

        /* lookup next object in the current group */
        if(H5VL_iod_get_metadata(cur_oh.rd_oh, rtid, H5VL_IOD_LINK, 
                                 comp, NULL, NULL, &value) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve link value");

        /* MSC - NEED IOD */
#if 0
        /* if this a soft link, traverse the link value if the ID is undefined */
        /* NOTE: Soft links can not be created and accessed in the
           same transaction; The transaction that created the soft
           link must be used as the version for accessing the soft
           link here - MSC 8/7/2103 */
        if(H5L_TYPE_SOFT == value.link_type) {
            if('/' == *value.u.symbolic_name) {
                cur_id = ROOT_ID;
            }

            /* Traverse Path and open the target object */
            if(H5VL_iod_server_open_path(coh, cur_id, cur_oh, value.u.symbolic_name, 
                                         rtid, &cur_id, &cur_oh) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't open object");

            free(value.u.symbolic_name);
        }
        else
#endif
            cur_id = value.u.iod_id;

        /* Close previous read handle unless it is the original one */
        if(loc_handle.rd_oh.cookie != prev_oh.cookie && 
           iod_obj_close(prev_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close current object handle");

        /* open the current group */
        if (iod_obj_open_read(coh, cur_id, NULL, &cur_oh.rd_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

	/* Advance to next component in string */
	path += nchars;
    } /* end while */

    /* Release temporary component buffer */
    if(wb && H5WB_unwrap(wb) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't release wrapped buffer");

    *iod_id = cur_id;
    (*iod_oh).rd_oh.cookie = cur_oh.rd_oh.cookie;

    if(cur_id != loc_id ||
       loc_handle.wr_oh.cookie == IOD_OH_UNDEFINED) {
        /* open a write handle on the ID. */
        if (iod_obj_open_write(coh, cur_id, NULL, &cur_oh.wr_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");
    }
        
    (*iod_oh).wr_oh.cookie = cur_oh.wr_oh.cookie;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_open_path
 *
 * Purpose: Function to Traverse the path in IOD KV objects given the
 * starting location ID, object handle, and path name. This walks
 * through the IOD KV objects to get to the last component in the path
 * and opens the last component for read only, i.e. does not open the
 * wr_oh, but will set it to undefined. Usually this is called when
 * opening an object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_server_open_path(iod_handle_t coh, iod_obj_id_t loc_id, iod_handles_t loc_handle, 
                          const char *path, iod_trans_id_t rtid,
                          /*out*/iod_obj_id_t *iod_id, /*out*/iod_handles_t *iod_oh)
{
    char comp_buf[1024];     /* Temporary buffer for path components */
    char *comp;              /* Pointer to buffer for path components */
    H5WB_t *wb = NULL;       /* Wrapped buffer for temporary buffer */
    size_t nchars;	     /* component name length	*/
    iod_handles_t cur_oh;
    iod_handle_t prev_oh;
    iod_obj_id_t cur_id;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    cur_oh.rd_oh.cookie = loc_handle.rd_oh.cookie;
    cur_id = loc_id;

    if(cur_oh.rd_oh.cookie == IOD_OH_UNDEFINED) {
        /* open the current group */
        if (iod_obj_open_read(coh, loc_id, NULL /*hints*/, &cur_oh.rd_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");
    }

    /* Wrap the local buffer for serialized header info */
    if(NULL == (wb = H5WB_wrap(comp_buf, sizeof(comp_buf))))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wrap buffer")
    /* Get a pointer to a buffer that's large enough  */
    if(NULL == (comp = (char *)H5WB_actual(wb, (HDstrlen(path) + 1))))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't get actual buffer")

    /* Traverse the path */
    while((path = H5G__component(path, &nchars)) && *path) {
        iod_size_t kv_size;
        H5VL_iod_link_t value;

        /* Copy the component path into a null-terminated buffer. */
	HDmemcpy(comp, path, nchars);
	comp[nchars] = '\0';

	/*
	 * The special path `.' is a no-op.
	 */
	if('.' == comp[0] && !comp[1]) {
	    path += nchars;
	    continue;
	} /* end if */

        kv_size = sizeof(H5VL_iod_link_t);

        prev_oh.cookie = cur_oh.rd_oh.cookie;

        /* lookup next object in the current group */
        if(H5VL_iod_get_metadata(cur_oh.rd_oh, rtid, H5VL_IOD_LINK, 
                                 comp, NULL, NULL, &value) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve link value");

        /* MSC - NEED IOD */
#if 0
        /* if this a soft link, traverse the link value if the ID is undefined */
        if(H5L_TYPE_SOFT == value.link_type) {
            if('/' == *value.u.symbolic_name) {
                cur_id = ROOT_ID;
            }

            /* Traverse Path and open the target object */
            if(H5VL_iod_server_open_path(coh, cur_id, cur_oh, value.u.symbolic_name, 
                                         rtid, &cur_id, &cur_oh) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't open object");

            free(value.u.symbolic_name);
        }
        else
#endif
            cur_id = value.iod_id;

        /* Close previous handle unless it is the original one */
        if(loc_handle.rd_oh.cookie != prev_oh.cookie && 
           iod_obj_close(prev_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close current object handle");

        /* open the current group */
        if (iod_obj_open_read(coh, cur_id, NULL, &cur_oh.rd_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

	/* Advance to next component in string */
	path += nchars;
    } /* end while */

    /* Release temporary component buffer */
    if(wb && H5WB_unwrap(wb) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't release wrapped buffer");

    *iod_id = cur_id;
    (*iod_oh).rd_oh.cookie = cur_oh.rd_oh.cookie;
    (*iod_oh).wr_oh.cookie = IOD_OH_UNDEFINED;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_get_file_desc
 *
 * Purpose: 
 *     Function to generate IOD hyperslab objects from HDF5
 *     dataspace selections. If hslabs is NULL, a count of the number of
 *     hslabs needed is returned in count.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_get_file_desc(hid_t space_id, hssize_t *count, iod_hyperslab_t *hslabs)
{
    hssize_t num_descriptors = 0, n;
    int ndims = 0, i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the rank of this dataspace */
    if((ndims = H5Sget_simple_extent_ndims(space_id)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace dimesnsion");

    switch(H5Sget_select_type(space_id)) {
    case H5S_SEL_NONE:
        /* nothing selected */
        num_descriptors = 0;
        HGOTO_DONE(SUCCEED);
    case H5S_SEL_ALL:
        /* The entire dataspace is selected, 1 large iod hyperslab is needed */
        num_descriptors = 1;

        if(NULL != hslabs) {
            hsize_t dims[H5S_MAX_RANK];

            /* get the dimensions sizes of the dataspace */
            if(H5Sget_simple_extent_dims(space_id, dims, NULL) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get dataspace dimesnsion sizes");
            /* populate the hyperslab */
            for(i=0 ; i<ndims ; i++) {
                hslabs[0].start[i] = 0;
                hslabs[0].stride[i] = 1;
                hslabs[0].block[i] = dims[i];
                hslabs[0].count[i] = 1;
            }
        }
        break;
    case H5S_SEL_POINTS:
        {
            /* we need a hyperslab element for each point */
            if((num_descriptors = H5Sget_select_elem_npoints(space_id)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "invalid point selection");

            if(NULL != hslabs) {
                hsize_t *points = NULL;
                size_t point_count = 0;

                point_count = ndims * num_descriptors * sizeof(hsize_t);

                if(NULL == (points = (hsize_t *)malloc(point_count)))
                    HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate array for points coords");

                if(H5Sget_select_elem_pointlist(space_id, (hsize_t)0, 
                                                (hsize_t)num_descriptors, points) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "Failed to retrieve point coordinates");

                /* populate the hyperslab */
                for(n=0 ; n<num_descriptors ; n++) {
                    hsize_t *cur_ptr = points; /* temp pointer into points array */

                    /* adjust the current pointer to the current point */
                    cur_ptr += n*ndims;
                    for(i=0 ; i<ndims ; i++) {
                        hslabs[n].start[i] = *(cur_ptr+i);
                        hslabs[n].stride[i] = 1;
                        hslabs[n].count[i] = 1;
                        hslabs[n].block[i] = *(cur_ptr+ndims+i) + 1 - hslabs[n].start[i];
                    }
                }

                free(points);
            }
            break;
        }
    case H5S_SEL_HYPERSLABS:
        {
            /* if the selection is a regular hyperslab
               selection, only 1 iod hyperslab object is
               needed */
            if(H5Sselect_is_regular(space_id)) {
                num_descriptors = 1;

                if(NULL != hslabs) {
                    if(H5Sget_reg_hyperslab_params(space_id, 
                                                   hslabs[0].start, 
                                                   hslabs[0].stride,
                                                   hslabs[0].count,
                                                   hslabs[0].block) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "Failed to retrieve hyperslab selection");
                }
            }
            /* Otherwise populate the hslabs by getting every block */
            else {
                if((num_descriptors = H5Sget_select_hyper_nblocks(space_id)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "invalid hyperslab selection");

                if(NULL != hslabs) {
                    hsize_t *blocks = NULL;
                    size_t block_count = 0;

                    block_count = ndims * num_descriptors * sizeof(hsize_t) * 2;

                    if(NULL == (blocks = (hsize_t *)malloc(block_count)))
                        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate array for points coords");

                    fprintf(stderr, "block count = %zu\n", block_count);

                    if(H5Sget_select_hyper_blocklist(space_id, (hsize_t)0, 
                                                     (hsize_t)num_descriptors, blocks) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "Failed to retrieve point coordinates");

                    /* populate the hyperslab */
                    for(n=0 ; n<num_descriptors ; n++) {
                        hsize_t *cur_ptr = blocks; /* temp pointer into blocks array */

                        /* adjust the current pointer to the current block */
                        cur_ptr += n*ndims*2;
                        for(i=0 ; i<ndims ; i++) {
                            hslabs[n].start[i] = *(cur_ptr+i);
                            hslabs[n].stride[i] = 1;
                            hslabs[n].count[i] = 1;
                            hslabs[n].block[i] = *(cur_ptr+ndims+i) + 1 - hslabs[n].start[i];
                        }
                    }

                    free(blocks);
                }
            }
            break;
        }
    case H5S_SEL_ERROR:
    case H5S_SEL_N:
    default:
        HGOTO_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "Invalid Selection type");
    }

    *count = num_descriptors;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_insert_plist
 *
 * Purpose:     Function to insert a creation property list in an 
 *              IOD KV object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_insert_plist(iod_handle_t oh, iod_trans_id_t tid, hid_t plist_id,
                      iod_hint_list_t *hints, iod_checksum_t *cs, iod_event_t *event)
{
    void *key = NULL;
    void *value = NULL;
    iod_kv_t kv;
    size_t buf_size;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* insert group creation properties in Metadata KV */
    key = strdup(H5VL_IOD_KEY_OBJ_CPL);

    /* determine the buffer size needed to store the encoded plist */ 
    if(H5Pencode(plist_id,  NULL, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode plist");
    if(NULL == (value = malloc (buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate plist buffer");
    /* encode plist */ 
    if(H5Pencode(plist_id, value, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode plist");

    kv.key = (char *)key;
    kv.value = value;
    kv.value_len = (iod_size_t)buf_size;
    if (iod_kv_set(oh, tid, hints, &kv, cs, event) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

done:
    if(key) {
        free(key); 
        key = NULL;
    }
    if(value) {
        free(value); 
        value = NULL;
    }

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_insert_link_count
 *
 * Purpose:     Function to insert the link count in an 
 *              IOD KV object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_insert_link_count(iod_handle_t oh, iod_trans_id_t tid, uint64_t count,
                           iod_hint_list_t *hints, iod_checksum_t *cs, iod_event_t *event)
{
    void *key = NULL;
    iod_kv_t kv;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    key = strdup(H5VL_IOD_KEY_OBJ_LINK_COUNT);

    kv.key = (char *)key;
    kv.value = &count;
    kv.value_len = sizeof(uint64_t);

    if (iod_kv_set(oh, tid, hints, &kv, cs, event) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

done:
    if(key) {
        free(key); 
        key = NULL;
    }
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_insert_object_type
 *
 * Purpose:     Function to insert the object type in an 
 *              IOD KV object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_insert_object_type(iod_handle_t oh, iod_trans_id_t tid, H5I_type_t obj_type,
                            iod_hint_list_t *hints, iod_checksum_t *cs, iod_event_t *event)
{
    void *key = NULL;
    iod_kv_t kv;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    key = strdup(H5VL_IOD_KEY_OBJ_TYPE);

    kv.key = (char *)key;
    kv.value = &obj_type;
    kv.value_len = sizeof(int32_t);

    if (iod_kv_set(oh, tid, hints, &kv, cs, event) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

done:
    if(key) {
        free(key); 
        key = NULL;
    }
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_insert_datatype
 *
 * Purpose:     Function to insert a datatype in an 
 *              IOD KV object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_insert_datatype(iod_handle_t oh, iod_trans_id_t tid, hid_t type_id,
                         iod_hint_list_t *hints, iod_checksum_t *cs, iod_event_t *event)
{
    void *key = NULL;
    void *value = NULL;
    iod_kv_t kv;
    size_t buf_size;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* insert group creation properties in Metadata KV */
    key = strdup(H5VL_IOD_KEY_OBJ_DATATYPE);

    /* determine the buffer size needed to store the encoded type */ 
    if(H5Tencode(type_id,  NULL, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode type");
    if(NULL == (value = malloc (buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate type buffer");
    /* encode type */ 
    if(H5Tencode(type_id, value, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode type");

    kv.key = (char *)key;
    kv.value = value;
    kv.value_len = (iod_size_t)buf_size;
    /* insert kv pair into KV */
    if (iod_kv_set(oh, tid, hints, &kv, cs, event) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

done:
    if(key) {
        free(key); 
        key = NULL;
    }
    if(value) {
        free(value); 
        value = NULL;
    }

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_insert_dataspace
 *
 * Purpose:     Function to insert a dataspace in an 
 *              IOD KV object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_insert_dataspace(iod_handle_t oh, iod_trans_id_t tid, hid_t space_id,
                         iod_hint_list_t *hints, iod_checksum_t *cs, iod_event_t *event)
{
    void *key = NULL;
    void *value = NULL;
    iod_kv_t kv;
    size_t buf_size;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* insert group creation properties in Metadata KV */
    key = strdup(H5VL_IOD_KEY_OBJ_DATASPACE);

    /* determine the buffer size needed to store the encoded space */ 
    if(H5Sencode(space_id,  NULL, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode space");
    if(NULL == (value = malloc (buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate space buffer");
    /* encode space */ 
    if(H5Sencode(space_id, value, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode space");

    kv.key = (char *)key;
    kv.value = value;
    kv.value_len = (iod_size_t)buf_size;
    /* insert kv pair into KV */
    if (iod_kv_set(oh, tid, hints, &kv, cs, event) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

done:
    if(key) {
        free(key); 
        key = NULL;
    }
    if(value) {
        free(value); 
        value = NULL;
    }

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_insert_new_link
 *
 * Purpose:     Function to insert a link to another object in an 
 *              IOD KV object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_insert_new_link(iod_handle_t oh, iod_trans_id_t tid, const char *link_name,
                         H5L_type_t link_type, void *link_val, iod_hint_list_t *hints, 
                         iod_checksum_t *cs, iod_event_t *event)
{
    iod_kv_t kv;
    void  *value = NULL;
    uint8_t *val_ptr = NULL;
    size_t value_len;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    switch(link_type) {
        case H5L_TYPE_HARD:
            {
                value_len = sizeof(H5L_type_t) + sizeof(iod_obj_id_t);
                value = malloc(value_len);

                val_ptr = (uint8_t *)value;

                memcpy(val_ptr, &link_type, sizeof(H5L_type_t));
                memcpy(val_ptr+sizeof(H5L_type_t), link_val, sizeof(iod_obj_id_t));

                break;
            }
        case H5L_TYPE_SOFT:
            {
                value_len = sizeof(H5L_type_t) + strlen((char *)link_val) + 1;
                value = malloc(value_len);

                val_ptr = (uint8_t *)value;

                memcpy(val_ptr, &link_type, sizeof(H5L_type_t));
                strcpy((char *)(val_ptr+sizeof(H5L_type_t)), (char *)link_val);

                break;
            }
        case H5L_TYPE_ERROR:
        case H5L_TYPE_EXTERNAL:
        case H5L_TYPE_MAX:
        default:
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unsupported link type");
    }

    kv.key = link_name;
    kv.value = value;
    kv.value_len = value_len;

    if (iod_kv_set(oh, tid, hints, &kv, cs, event) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

done:

    if(value)
        free(value);

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_get_metadata
 *
 * Purpose:     Function to retrieve a particular metadata value from an
 *              IOD KV object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_get_metadata(iod_handle_t oh, iod_trans_id_t tid, H5VL_iod_metadata_t md_type,
                      const char *key, iod_checksum_t *cs, iod_event_t *event, void *ret)
{
    iod_size_t val_size = 0;
    void *value = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    switch(md_type) {
    case H5VL_IOD_PLIST:
        {
            hid_t plist_id = *((hid_t *)ret);

            if(iod_kv_get_value(oh, tid, key, NULL, &val_size, cs, event) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "lookup failed");

            if(NULL == (value = malloc((size_t)val_size)))
                HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate value buffer");

            if(iod_kv_get_value(oh, tid, key, value, &val_size, cs, event) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "lookup failed");

            if((plist_id = H5Pdecode(value)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "failed to decode gcpl");
            break;
        }
    case H5VL_IOD_LINK_COUNT:
        val_size = sizeof(uint64_t);
        if(iod_kv_get_value(oh, tid, key, ret, &val_size, cs, event) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "link_count lookup failed");
        break;
    case H5VL_IOD_DATATYPE:
        {
            hid_t type_id = *((hid_t *)ret);

            if(iod_kv_get_value(oh, tid, key, NULL, &val_size, cs, event) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "lookup failed");

            if(NULL == (value = malloc((size_t)val_size)))
                HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate value buffer");

            if(iod_kv_get_value(oh, tid, key, value, &val_size, cs, event) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "lookup failed");

            if((type_id = H5Tdecode(value)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "failed to decode gcpl");
            break;
        }
    case H5VL_IOD_DATASPACE:
        {
            hid_t space_id = *((hid_t *)ret);

            if(iod_kv_get_value(oh, tid, key, NULL, &val_size, cs, event) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "lookup failed");

            if(NULL == (value = malloc((size_t)val_size)))
                HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate value buffer");

            if(iod_kv_get_value(oh, tid, key, value, &val_size, cs, event) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "lookup failed");

            if((space_id = H5Tdecode(value)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "failed to decode gcpl");
            break;
        }
    case H5VL_IOD_OBJECT_TYPE:
        val_size = sizeof(int32_t);
        if(iod_kv_get_value(oh, tid, key, ret, &val_size, cs, event) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "link_count lookup failed");
        break;
    case H5VL_IOD_LINK:
        {
            H5VL_iod_link_t *iod_link = (H5VL_iod_link_t *)ret;
            uint8_t *val_ptr;

            if(iod_kv_get_value(oh, tid, key, NULL, &val_size, cs, event) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "lookup failed");

            /* MSC - faking for now */
            val_size = sizeof(H5L_type_t) + sizeof(iod_obj_id_t);

            if(NULL == (value = malloc((size_t)val_size)))
                HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate value buffer");

            if(iod_kv_get_value(oh, tid, key, value, &val_size, cs, event) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "lookup failed");

            val_ptr = (uint8_t *)value;

            iod_link->link_type = *((H5L_type_t *)val_ptr);
            val_ptr += sizeof(H5L_type_t);

            /* MSC - faking for now */
            iod_link->link_type = H5L_TYPE_HARD;

            switch(iod_link->link_type) {
                case H5L_TYPE_HARD:
                    iod_link->u.iod_id = *((iod_obj_id_t *)val_ptr);
                    break;
                case H5L_TYPE_SOFT:
                    iod_link->u.symbolic_name = strdup((char *)val_ptr);
                    break;
                case H5L_TYPE_ERROR:
                case H5L_TYPE_EXTERNAL:
                case H5L_TYPE_MAX:
                default:
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unsupported link type");
            }
            break;
        }
    default:
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "invalide metadata type");
    }
done:
    if(value) {
        free(value); 
        value = NULL;
    }

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_server_adjust_buffer
 *
 * Checks datatypes to see if type conversion is required, if
 * yes, the buffer is resized accordingly.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL__iod_server_adjust_buffer(hid_t mem_type_id, hid_t dset_type_id, size_t nelmts, 
                               hid_t UNUSED dxpl_id, size_t size, void **buf, 
                               hbool_t *is_vl_data, size_t *_buf_size)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    switch(H5Tget_class(dset_type_id)) {
        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_TIME:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_ENUM:
        case H5T_ARRAY:
        case H5T_NO_CLASS:
        case H5T_STRING:
            if(H5Tis_variable_str(dset_type_id)) {
                *is_vl_data = TRUE;
                *_buf_size = size;
                break;
            }
        case H5T_REFERENCE:
        case H5T_NCLASSES:
        case H5T_COMPOUND:
            {
                hsize_t buf_size = 0;
                size_t mem_type_size, dset_type_size;

                *is_vl_data = FALSE;

                /* retrieve source and destination datatype sizes for data conversion */
                mem_type_size = H5Tget_size(mem_type_id);
                dset_type_size = H5Tget_size(dset_type_id);

                /* adjust buffer size for data conversion */
                if(mem_type_size < dset_type_size) {
                    buf_size = dset_type_size * nelmts;

                    if(NULL == (*buf = realloc(*buf, (size_t)buf_size)))
                        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "Can't adjust buffer for DT conversion");
#if H5VL_IOD_DEBUG
                    fprintf(stderr, "Adjusted Buffer size for dt conversion from %zu to %lld\n", 
                            size, buf_size);
#endif
                }
                else {
                    buf_size = mem_type_size * nelmts;
                    if(buf_size != size)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Incoming data size is not equal to expected size");
                }

                *_buf_size = buf_size;

                break;
            }
        case H5T_VLEN:
            *is_vl_data = TRUE;
            *_buf_size = size;
            break;
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "unsupported datatype");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__iod_server_adjust_buffer */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_verify_scratch_pad
 *
 * Purpose:     Function to insert the link count in an 
 *              IOD KV object.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_verify_scratch_pad(scratch_pad sp, uint32_t iod_cs)
{
    uint32_t computed_cs = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* MSC - Need IOD */
#if 0
    computed_cs = H5checksum(&sp, sizeof(sp), NULL);

    if(computed_cs != iod_cs) {
        fprintf(stderr, "Scratch pad integrity check failed. IOD cs = %u, Computed cs = %u",
                iod_cs, computed_cs);
        ret_value = FAIL;
    }
#endif

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_verify_scratch_pad() */

#if 0
herr_t
H5VL_iod_map_type_convert(hid_t src_id, hid_t dst_id, void *buf, size_t buf_size)
{
    H5T_class_t class;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    class = H5Tget_class(src_id);
    if(H5T_VLEN == class || (H5T_STRING == class && H5Tis_variable_str(src_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "Can't convert VL or string types");

    class = H5Tget_class(dset_id);
    if(H5T_VLEN == class || (H5T_STRING == class && H5Tis_variable_str(dst_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "Can't convert VL or string types");

    /* Check (and do) Type conversion on the Key */
    src_size = H5Tget_size(src_id);
    dst_size = H5Tget_size(dst_id);

    /* adjust buffer size for datatype conversion */
    if(src_size < dst_size) {
        new_size = dst_size;
        if(NULL == (*buf = realloc(*buf, new_size)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "Can't adjust buffer for DT conversion");
    }
    else {
        new_size = src_size;
    }

    if(NULL == (key_buf = malloc((size_t)key_size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate buffer");
    memcpy(key_buf, key.buf, src_size);

    if(H5Tconvert(src_id, dst_id, 1, key_buf, NULL, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "data type conversion failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}
#endif
#endif /* H5_HAVE_EFF */
