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

#if 0
static herr_t H5VL_iod_typeinfo_init(hid_t dset_type_id, const H5D_dxpl_cache_t *dxpl_cache,
                                     hid_t dxpl_id, hid_t mem_type_id, hbool_t do_write,
                                     H5D_type_info_t *type_info);
static herr_t H5VL_iod_typeinfo_term(const H5D_type_info_t *type_info);
#endif


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
H5VL_iod_server_traverse(iod_handle_t coh, iod_obj_id_t loc_id, iod_handle_t loc_handle, 
                         const char *path, hbool_t create_interm_grps,
                         /* out */char **last_comp, /* out */iod_obj_id_t *iod_id, 
                         /* out */iod_handle_t *iod_oh)
{
    char comp_buf[1024];     /* Temporary buffer for path components */
    char *comp;              /* Pointer to buffer for path components */
    H5WB_t *wb = NULL;       /* Wrapped buffer for temporary buffer */
    size_t nchars;	     /* component name length	*/
    iod_handle_t cur_oh, prev_oh;
    iod_obj_id_t cur_id;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    assert(FALSE == create_interm_grps);

    cur_oh = loc_handle;
    cur_id = loc_id;

    if(cur_oh.cookie == IOD_OH_UNDEFINED) {
        /* open the current group */
        if (iod_obj_open_write(coh, loc_id, NULL /*hints*/, &cur_oh, NULL) < 0)
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

        prev_oh = cur_oh;

        /* lookup next object in the current group */
        if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, comp, &value, &kv_size, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Intermdiate group does not exist");

        /* MSC - NEED IOD */
#if 0
        /* if this a soft link, traverse the link value if the ID is undefined */
        if(IOD_ID_UNDEFINED == value.iod_id && 
           H5VL_LINK_CREATE_SOFT == value.link_type) {
            scratch_pad_t sp;
            iod_handle_t mdkv_oh;
            char *link_value = NULL;
            iod_size_t val_size = 0;

            /* get scratch pad of the object */
            if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

            /* open the metadata scratch pad */
            if (iod_obj_open_write(coh, sp.mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

            /* retrieve the link value size */
            if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_KEY_SOFT_LINK, NULL, 
                                &val_size, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "datatype size lookup failed");

            if(NULL == (link_value = (char *)malloc((size_t)val_size)))
                HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate buffer");

            /* retrieve the link value */
            if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_KEY_SOFT_LINK, link_value, 
                                &val_size, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "datatype size lookup failed");

            /* Traverse Path and open the target object */
            if(H5VL_iod_server_open_path(coh, cur_id, cur_oh, link_value, 
                                         &cur_id, &cur_oh) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't open object");

            /* close the metadata scratch pad */
            if(iod_obj_close(mdkv_oh, NULL, NULL))
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

            free(link_value);
        }
        else
#endif
            cur_id = value.iod_id;

        /* Close previous handle unless it is the original one */
        if(loc_handle.cookie != prev_oh.cookie && 
           iod_obj_close(prev_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close current object handle");

        /* open the current group */
        if (iod_obj_open_write(coh, cur_id, NULL, &cur_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

	/* Advance to next component in string */
	path += nchars;
    } /* end while */

    /* Release temporary component buffer */
    if(wb && H5WB_unwrap(wb) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't release wrapped buffer");

    *iod_id = cur_id;
    *iod_oh = cur_oh;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_open_path
 *
 * Purpose: 
 *     Function to Traverse the path in IOD KV objects given the
 *     starting location ID, object handle, and path name. This walks
 *     through the IOD KV objects to get to the last component in the
 *     path and opens the last component. Usually this is called when
 *     opening an object. The Function returns the iod ID and object
 *     handle of the last component, and the last component
 *     string, if the user requests it.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL_iod_server_open_path(iod_handle_t coh, iod_obj_id_t loc_id, iod_handle_t loc_handle, 
    const char *path, /*out*/iod_obj_id_t *iod_id, /*out*/iod_handle_t *iod_oh)
{
    char comp_buf[1024];     /* Temporary buffer for path components */
    char *comp;              /* Pointer to buffer for path components */
    H5WB_t *wb = NULL;       /* Wrapped buffer for temporary buffer */
    size_t nchars;	     /* component name length	*/
    iod_handle_t cur_oh, prev_oh;
    iod_obj_id_t cur_id;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    cur_oh = loc_handle;
    cur_id = loc_id;

    if(cur_oh.cookie == IOD_OH_UNDEFINED) {
        /* open the current group */
        if (iod_obj_open_write(coh, loc_id, NULL /*hints*/, &cur_oh, NULL) < 0)
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

        prev_oh = cur_oh;

        /* lookup next object in the current group */
        if(iod_kv_get_value(cur_oh, IOD_TID_UNKNOWN, comp, &value, &kv_size, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "Intermdiate group does not exist");

        /* MSC - NEED IOD */
#if 0
        /* if this a soft link, traverse the link value if the ID is undefined */
        if(IOD_ID_UNDEFINED == value.iod_id && 
           H5VL_LINK_CREATE_SOFT == value.link_type) {
            scratch_pad_t sp;
            iod_handle_t mdkv_oh;
            char *link_value = NULL;
            iod_size_t val_size = 0;

            /* get scratch pad of the object */
            if(iod_obj_get_scratch(cur_oh, IOD_TID_UNKNOWN, &sp, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

            /* open the metadata scratch pad */
            if (iod_obj_open_write(coh, sp.mdkv_id, NULL /*hints*/, &mdkv_oh, NULL) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "can't open scratch pad");

            /* retrieve the link value size */
            if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_KEY_SOFT_LINK, NULL, 
                                &val_size, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "datatype size lookup failed");

            if(NULL == (link_value = (char *)malloc((size_t)val_size)))
                HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate buffer");

            /* retrieve the link value */
            if(iod_kv_get_value(mdkv_oh, IOD_TID_UNKNOWN, H5VL_IOD_KEY_SOFT_LINK, link_value, 
                                &val_size, NULL, NULL) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "datatype size lookup failed");

            /* Traverse Path and open the target object */
            if(H5VL_iod_server_open_path(coh, cur_id, cur_oh, link_value, 
                                         &cur_id, &cur_oh) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't open object");

            /* close the metadata scratch pad */
            if(iod_obj_close(mdkv_oh, NULL, NULL))
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");

            free(link_value);
        }
        else
#endif
            cur_id = value.iod_id;

        /* Close previous handle unless it is the original one */
        if(loc_handle.cookie != prev_oh.cookie && 
           iod_obj_close(prev_oh, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close current object handle");

        /* open the current group */
        if (iod_obj_open_write(coh, cur_id, NULL, &cur_oh, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

	/* Advance to next component in string */
	path += nchars;
    } /* end while */

    /* Release temporary component buffer */
    if(wb && H5WB_unwrap(wb) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't release wrapped buffer");

    *iod_id = cur_id;
    *iod_oh = cur_oh;

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
    /* insert kv pair into scratch pad */
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
    /* insert kv pair into scratch pad */
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
    /* insert kv pair into scratch pad */
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
                         H5L_type_t link_type, iod_obj_id_t obj_id, iod_hint_list_t *hints, 
                         iod_checksum_t *cs, iod_event_t *event)
{
    iod_kv_t kv;
    H5VL_iod_link_t value;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    value.iod_id = obj_id;
    value.link_type = link_type;

    kv.key = link_name;
    kv.value = &value;
    kv.value_len = sizeof(H5VL_iod_link_t);

    if (iod_kv_set(oh, tid, hints, &kv, cs, event) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't set KV pair in parent");

done:
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



#if 0

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_typeinfo_init
 *
 * Purpose:	Routine for determining correct datatype information for
 *              each I/O action.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_typeinfo_init(hid_t dset_type_id, const H5D_dxpl_cache_t *dxpl_cache,
                       hid_t dxpl_id, hid_t mem_type_id, hbool_t do_write,
                       H5D_type_info_t *type_info)
{
    const H5T_t	*src_type;              /* Source datatype */
    const H5T_t	*dst_type;              /* Destination datatype */
    herr_t ret_value = SUCCEED;	        /* Return value	*/

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(type_info);

    /* Initialize type info safely */
    HDmemset(type_info, 0, sizeof(*type_info));

    /* Get the memory & dataset datatypes */
    if(NULL == (type_info->mem_type = (const H5T_t *)H5I_object_verify(mem_type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")
    if(NULL == (type_info->dset_type = (const H5T_t *)H5I_object_verify(dset_type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")

    if(do_write) {
        src_type = type_info->mem_type;
        dst_type = type_info->dset_type;
        type_info->src_type_id = mem_type_id;
        type_info->dst_type_id = dset_type_id;
    } /* end if */
    else {
        src_type = type_info->dset_type;
        dst_type = type_info->mem_type;
        type_info->src_type_id = dset_type_id;
        type_info->dst_type_id = mem_type_id;
    } /* end else */

    /*
     * Locate the type conversion function and data space conversion
     * functions, and set up the element numbering information. If a data
     * type conversion is necessary then register datatype atoms. Data type
     * conversion is necessary if the user has set the `need_bkg' to a high
     * enough value in xfer_parms since turning off datatype conversion also
     * turns off background preservation.
     */
    if(NULL == (type_info->tpath = H5T_path_find(src_type, dst_type, NULL, NULL, dxpl_id, FALSE)))
	HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest datatype")

    /* Precompute some useful information */
    type_info->src_type_size = H5T_get_size(src_type);
    type_info->dst_type_size = H5T_get_size(dst_type);
    type_info->max_type_size = MAX(type_info->src_type_size, type_info->dst_type_size);
    type_info->is_conv_noop = H5T_path_noop(type_info->tpath);
    type_info->is_xform_noop = H5Z_xform_noop(dxpl_cache->data_xform_prop);
    if(type_info->is_xform_noop && type_info->is_conv_noop) {
        type_info->cmpd_subset = NULL;
        type_info->need_bkg = H5T_BKG_NO;
    } /* end if */
    else {
        size_t	target_size;		/* Desired buffer size	*/

        /* Check if the datatypes are compound subsets of one another */
        type_info->cmpd_subset = H5T_path_compound_subset(type_info->tpath);

        /* Check if we need a background buffer */
        if(do_write && H5T_detect_class(type_info->dset_type, H5T_VLEN, FALSE))
            type_info->need_bkg = H5T_BKG_YES;
        else {
            H5T_bkg_t path_bkg;     /* Type conversion's background info */

            if((path_bkg = H5T_path_bkg(type_info->tpath))) {
                /* Retrieve the bkgr buffer property */
                type_info->need_bkg = dxpl_cache->bkgr_buf_type;
                type_info->need_bkg = MAX(path_bkg, type_info->need_bkg);
            } /* end if */
            else
                type_info->need_bkg = H5T_BKG_NO; /*never needed even if app says yes*/
        } /* end else */


        /* Set up datatype conversion/background buffers */

        /* Get buffer size from DXPL */
        target_size = dxpl_cache->max_temp_buf;

        /* If the buffer is too small to hold even one element, try to make it bigger */
        if(target_size < type_info->max_type_size) {
            hbool_t default_buffer_info;    /* Whether the buffer information are the defaults */

            /* Detect if we have all default settings for buffers */
            default_buffer_info = (hbool_t)((H5D_TEMP_BUF_SIZE == dxpl_cache->max_temp_buf)
                    && (NULL == dxpl_cache->tconv_buf) && (NULL == dxpl_cache->bkgr_buf));

            /* Check if we are using the default buffer info */
            if(default_buffer_info)
                /* OK to get bigger for library default settings */
                target_size = type_info->max_type_size;
            else
                /* Don't get bigger than the application has requested */
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small")
        } /* end if */

        /* Compute the number of elements that will fit into buffer */
        type_info->request_nelmts = target_size / type_info->max_type_size;

        /* Sanity check elements in temporary buffer */
        if(type_info->request_nelmts == 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small")

        /*
         * Get a temporary buffer for type conversion unless the app has already
         * supplied one through the xfer properties. Instead of allocating a
         * buffer which is the exact size, we allocate the target size.  The
         * malloc() is usually less resource-intensive if we allocate/free the
         * same size over and over.
         */
        if(NULL == (type_info->tconv_buf = (uint8_t *)dxpl_cache->tconv_buf)) {
            /* Allocate temporary buffer */
            if(NULL == (type_info->tconv_buf = H5FL_BLK_MALLOC(type_conv, target_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion")
            type_info->tconv_buf_allocated = TRUE;
        } /* end if */
        if(type_info->need_bkg && NULL == (type_info->bkg_buf = (uint8_t *)dxpl_cache->bkgr_buf)) {
            size_t	bkg_size;		/* Desired background buffer size	*/

            /* Compute the background buffer size */
            /* (don't try to use buffers smaller than the default size) */
            bkg_size = type_info->request_nelmts * type_info->dst_type_size;
            if(bkg_size < dxpl_cache->max_temp_buf)
                bkg_size = dxpl_cache->max_temp_buf;

            /* Allocate background buffer */
            /* (Need calloc()-like call since memory needs to be initialized) */
            if(NULL == (type_info->bkg_buf = H5FL_BLK_CALLOC(type_conv, bkg_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion")
            type_info->bkg_buf_allocated = TRUE;
        } /* end if */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_typeinfo_init() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_typeinfo_term
 *
 * Purpose:	Common logic for terminating a type info object
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Thursday, March  6, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_iod_typeinfo_term(const H5D_type_info_t *type_info)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check for releasing datatype conversion & background buffers */
    if(type_info->tconv_buf_allocated) {
        HDassert(type_info->tconv_buf);
        (void)H5FL_BLK_FREE(type_conv, type_info->tconv_buf);
    } /* end if */
    if(type_info->bkg_buf_allocated) {
        HDassert(type_info->bkg_buf);
        (void)H5FL_BLK_FREE(type_conv, type_info->bkg_buf);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_typeinfo_term() */
#endif

#endif /* H5_HAVE_EFF */
