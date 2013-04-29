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

/* Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.org>
 *
 * Purpose:	IOD plugin encode/decode code
 */

#include "H5MMprivate.h"	/* Memory management			*/
#include "H5VLiod_common.h"     /* IOD Common Header			*/

#ifdef H5_HAVE_EFF

/* Define hg_proc_ret_t */
int hg_proc_ret_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;

    ret = hg_proc_int32_t(proc, data);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    return ret;
}

/* Define hg_proc_htri_t */
int hg_proc_htri_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;

    ret = hg_proc_int32_t(proc, data);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    return ret;
}

/* Define hg_proc_iod_handle_t */
int hg_proc_iod_handle_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    iod_handle_t *struct_data = (iod_handle_t *)data;

    //ret = hg_proc_uint64_t(proc, &struct_data->cookie);
    ret = hg_proc_int32_t(proc, &struct_data->cookie);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    return ret;
}

/* Define hg_proc_iod_obj_id_t */
int hg_proc_iod_obj_id_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    iod_obj_id_t *struct_data = (iod_obj_id_t *)data;

    ret = hg_proc_uint64_t(proc, &struct_data->oid_hi);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_uint64_t(proc, &struct_data->oid_lo);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_dims_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    int i;
    hg_proc_op_t op;
    dims_t *struct_data = (dims_t *) data;

    ret = hg_proc_int32_t(proc, &struct_data->rank);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    op = hg_proc_get_op(proc);

    switch(op) {
    case HG_ENCODE:
        for(i=0 ; i<struct_data->rank ; i++) {
            ret = hg_proc_uint64_t(proc, &struct_data->size[i]);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
        }
        break;
    case HG_DECODE:
        if(struct_data->rank)
            struct_data->size = malloc (sizeof(hsize_t) * struct_data->rank);

        for(i=0 ; i<struct_data->rank ; i++) {
            ret = hg_proc_uint64_t(proc, &struct_data->size[i]);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
        }
        break;
    case HG_FREE:
        if(struct_data->rank)
            free(struct_data->size);
        break;
    }

    return ret;
}

/* Define hg_proc_plist_t */
static int hg_proc_plist_t(hg_proc_t proc, hid_t *data)
{
    int ret = HG_SUCCESS;
    size_t plist_size = 0;
    void *buf = NULL;
    hid_t plist_id;
    hg_proc_op_t op;

    op = hg_proc_get_op(proc);

    switch(op) {

    case HG_ENCODE:
        plist_id = *data;
        if(H5P_FILE_CREATE_DEFAULT != plist_id || H5P_GROUP_CREATE_DEFAULT != plist_id ||
           H5P_LINK_CREATE_DEFAULT != plist_id || H5P_DATASET_CREATE_DEFAULT != plist_id ||
           H5P_FILE_ACCESS_DEFAULT != plist_id || H5P_GROUP_ACCESS_DEFAULT != plist_id ||
           H5P_ATTRIBUTE_CREATE_DEFAULT != plist_id ||
           H5P_DATASET_ACCESS_DEFAULT != plist_id || H5P_DATASET_XFER_DEFAULT != plist_id) {
            if(H5Pencode(plist_id, NULL, &plist_size) < 0) {
                HG_ERROR_DEFAULT("PLIST encode Proc error");
                return HG_FAIL;
            }
        }

        if(plist_size) {
            buf = H5MM_malloc(plist_size);
            if(H5Pencode(plist_id, buf, &plist_size) < 0) {
                HG_ERROR_DEFAULT("PLIST encode Proc error");
                return HG_FAIL;
            }
        }

        ret = hg_proc_uint64_t(proc, &plist_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(plist_size) {
            ret = hg_proc_raw(proc, buf, plist_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            H5MM_xfree(buf);
        }
        break;
    case HG_DECODE:
        ret = hg_proc_uint64_t(proc, &plist_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(plist_size) {
            buf = H5MM_malloc(plist_size);
            ret = hg_proc_raw(proc, buf, plist_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            if((plist_id = H5Pdecode(buf)) < 0) {
                HG_ERROR_DEFAULT("PLIST decode Proc error");
                return HG_FAIL;
            }
            H5MM_xfree(buf);
        }
        else
            plist_id = H5P_DEFAULT;

        *data = plist_id;
        break;
    case HG_FREE:
        plist_id = *data;
        if(plist_id != H5P_DEFAULT) {
            if(H5Pclose(plist_id) < 0) {
                HG_ERROR_DEFAULT("PLIST free Proc error");
                return HG_FAIL;
            }
        }
        break;
    default:
        HG_ERROR_DEFAULT("PLIST unsupported op Proc error");
    }

    return ret;
}

/* Define hg_proc_dtype_t */
static int hg_proc_dtype_t(hg_proc_t proc, hid_t *data)
{
    int ret = HG_SUCCESS;
    size_t dtype_size = 0;
    void *buf = NULL;
    hid_t dtype_id;
    hg_proc_op_t op;

    op = hg_proc_get_op(proc);

    switch(op) {

    case HG_ENCODE:
        dtype_id = *data;
        if(H5Tencode(dtype_id, NULL, &dtype_size) < 0) {
            HG_ERROR_DEFAULT("DTYPE encode Proc error");
            return HG_FAIL;
        }
        if(dtype_size) {
            buf = H5MM_malloc(dtype_size);
            if(H5Tencode(dtype_id, buf, &dtype_size) < 0) {
                HG_ERROR_DEFAULT("DTYPE encode Proc error");
                return HG_FAIL;
            }
        }

        ret = hg_proc_uint64_t(proc, &dtype_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(dtype_size) {
            ret = hg_proc_raw(proc, buf, dtype_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            H5MM_xfree(buf);
        }
        break;
    case HG_DECODE:
        ret = hg_proc_uint64_t(proc, &dtype_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(dtype_size) {
            buf = H5MM_malloc(dtype_size);
            ret = hg_proc_raw(proc, buf, dtype_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            if((dtype_id = H5Tdecode(buf)) < 0) {
                HG_ERROR_DEFAULT("DTYPE decode Proc error");
                return HG_FAIL;
            }
            H5MM_xfree(buf);
        }
        *data = dtype_id;

        break;
    case HG_FREE:
        if(H5Tclose(*data) < 0) {
            HG_ERROR_DEFAULT("DTYPE free Proc error");
            return HG_FAIL;
        }
        break;
    default:
        HG_ERROR_DEFAULT("DTYPE unsupported op Proc error");
    }

    return ret;
}

/* Define hg_proc_dspace_t */
static int hg_proc_dspace_t(hg_proc_t proc, hid_t *data)
{
    int ret = HG_SUCCESS;
    size_t dspace_size = 0;
    void *buf = NULL;
    hid_t dspace_id;
    hg_proc_op_t op;

    op = hg_proc_get_op(proc);

    switch(op) {

    case HG_ENCODE:
        dspace_id = *data;
        if(H5Sencode(dspace_id, NULL, &dspace_size) < 0) {
            HG_ERROR_DEFAULT("DSPACE encode Proc error");
            return HG_FAIL;
        }
        if(dspace_size) {
            buf = H5MM_malloc(dspace_size);
            if(H5Sencode(dspace_id, buf, &dspace_size) < 0) {
                HG_ERROR_DEFAULT("DSPACE encode Proc error");
                return HG_FAIL;
            }
        }

        ret = hg_proc_uint64_t(proc, &dspace_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(dspace_size) {
            ret = hg_proc_raw(proc, buf, dspace_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            H5MM_xfree(buf);
        }
        break;
    case HG_DECODE:
        ret = hg_proc_uint64_t(proc, &dspace_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(dspace_size) {
            buf = H5MM_malloc(dspace_size);
            ret = hg_proc_raw(proc, buf, dspace_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            if((dspace_id = H5Sdecode(buf)) < 0) {
                HG_ERROR_DEFAULT("DSPACE decode Proc error");
                return HG_FAIL;
            }
            H5MM_xfree(buf);
        }
        *data = dspace_id;
        break;
    case HG_FREE:
        if(H5Sclose(*data) < 0) {
            HG_ERROR_DEFAULT("DSPACE free Proc error");
            return HG_FAIL;
        }
        break;
    default:
        HG_ERROR_DEFAULT("DSPACE unsupported op Proc error");
    }

    return ret;
}

/* Define hg_proc_hid_t */
int hg_proc_hid_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    hid_t id = *((hid_t *)data);
    hg_proc_op_t op;
    H5I_type_t type;

    op = hg_proc_get_op(proc);

    if (HG_ENCODE == op || HG_FREE == op)
        type = H5Iget_type(id);
    ret = hg_proc_int32_t(proc, &type);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    switch(type) {
    case H5I_DATASPACE:
        ret = hg_proc_dspace_t(proc, data);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        break;
    case H5I_DATATYPE:
        ret = hg_proc_dtype_t(proc, data);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        break;
    case H5I_GENPROP_LST:
        ret = hg_proc_plist_t(proc, data);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        break;
    default:
        HG_ERROR_DEFAULT("Unsupported hid_t - Proc error");
        ret = HG_FAIL;
        break;
    }
    return ret;
}

#if 0

/* Define hg_proc_eff_init_in_t */
int hg_proc_eff_init_in_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;

    ret = hg_proc_int32_t(proc, data);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    return ret;
}

/* Define hg_proc_eff_fin_in_t */
int hg_proc_eff_fin_in_t(hg_proc_t proc, void *data)
{
    return HG_SUCCESS;
}

int hg_proc_file_create_in_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_file_create_input_t *struct_data = (H5VL_iod_file_create_input_t *) data;

    ret = hg_proc_hg_string_t(proc, &struct_data->name);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_uint32_t(proc, &struct_data->flags);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_fapl_t(proc, &struct_data->fapl_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_fcpl_t(proc, &struct_data->fcpl_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    return ret;
}

int hg_proc_file_create_out_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_remote_file_t *struct_data = (H5VL_iod_remote_file_t *) data;

    ret = hg_proc_iod_handle_t(proc, &struct_data->coh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->root_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->root_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->scratch_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->scratch_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_file_open_in_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_file_open_input_t *struct_data = (H5VL_iod_file_open_input_t *) data;

    ret = hg_proc_hg_string_t(proc, &struct_data->name);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_uint32_t(proc, &struct_data->flags);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_fapl_t(proc, &struct_data->fapl_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_file_open_out_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_remote_file_t *struct_data = (H5VL_iod_remote_file_t *) data;

    ret = hg_proc_iod_handle_t(proc, &struct_data->coh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->root_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->root_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->scratch_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->scratch_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_fcpl_t(proc, &struct_data->fcpl_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_file_flush_in_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_file_flush_input_t *struct_data = (H5VL_iod_file_flush_input_t *) data;

    ret = hg_proc_uint8_t(proc, &struct_data->scope);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_iod_handle_t(proc, &struct_data->coh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_file_close_in_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_remote_file_t *struct_data = (H5VL_iod_remote_file_t *) data;

    ret = hg_proc_iod_handle_t(proc, &struct_data->coh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->root_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->root_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->scratch_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->scratch_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_group_create_in_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_group_create_input_t *struct_data = (H5VL_iod_group_create_input_t *) data;

    ret = hg_proc_iod_handle_t(proc, &struct_data->coh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->loc_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->loc_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_hg_string_t(proc, &struct_data->name);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_gapl_t(proc, &struct_data->gapl_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_gcpl_t(proc, &struct_data->gcpl_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_lcpl_t(proc, &struct_data->lcpl_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_group_create_out_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_remote_group_t *struct_data = (H5VL_iod_remote_group_t *) data;

    ret = hg_proc_iod_handle_t(proc, &struct_data->iod_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->iod_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->scratch_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->scratch_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_group_open_in_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_group_open_input_t *struct_data = (H5VL_iod_group_open_input_t *) data;

    ret = hg_proc_iod_handle_t(proc, &struct_data->coh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->loc_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->loc_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_hg_string_t(proc, &struct_data->name);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_gapl_t(proc, &struct_data->gapl_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_group_open_out_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_remote_group_t *struct_data = (H5VL_iod_remote_group_t *) data;

    ret = hg_proc_iod_handle_t(proc, &struct_data->iod_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->iod_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->scratch_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->scratch_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_gcpl_t(proc, &struct_data->gcpl_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_group_close_in_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_remote_group_t *struct_data = (H5VL_iod_remote_group_t *) data;

    ret = hg_proc_iod_handle_t(proc, &struct_data->iod_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->iod_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->scratch_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->scratch_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_dset_create_in_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_dset_create_input_t *struct_data = (H5VL_iod_dset_create_input_t *) data;

    ret = hg_proc_iod_handle_t(proc, &struct_data->coh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->loc_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->loc_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_hg_string_t(proc, &struct_data->name);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_dapl_t(proc, &struct_data->dapl_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_dcpl_t(proc, &struct_data->dcpl_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_lcpl_t(proc, &struct_data->lcpl_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_dtype_t(proc, &struct_data->type_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_dspace_t(proc, &struct_data->space_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_dset_create_out_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_remote_dset_t *struct_data = (H5VL_iod_remote_dset_t *) data;

    ret = hg_proc_iod_handle_t(proc, &struct_data->iod_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->iod_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->scratch_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->scratch_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_dset_open_in_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_dset_open_input_t *struct_data = (H5VL_iod_dset_open_input_t *) data;

    ret = hg_proc_iod_handle_t(proc, &struct_data->coh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->loc_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->loc_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_hg_string_t(proc, &struct_data->name);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    ret = hg_proc_dapl_t(proc, &struct_data->dapl_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_dset_open_out_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_remote_dset_t *struct_data = (H5VL_iod_remote_dset_t *) data;

    ret = hg_proc_iod_handle_t(proc, &struct_data->iod_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->iod_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->scratch_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->scratch_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_dcpl_t(proc, &struct_data->dcpl_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_dtype_t(proc, &struct_data->type_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_dspace_t(proc, &struct_data->space_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_dset_set_extent_in_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    int i;
    hg_proc_op_t op;
    H5VL_iod_dset_set_extent_input_t *struct_data = (H5VL_iod_dset_set_extent_input_t *) data;

    ret = hg_proc_iod_handle_t(proc, &struct_data->iod_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_int32_t(proc, &struct_data->rank);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    op = hg_proc_get_op(proc);
    switch(op) {

    case HG_ENCODE:
        for(i=0 ; i<struct_data->rank ; i++) {
            ret = hg_proc_uint64_t(proc, &struct_data->size[i]);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
        }
        break;
    case HG_DECODE:
        if(struct_data->rank)
            struct_data->size = malloc (sizeof(hsize_t) * struct_data->rank);

        for(i=0 ; i<struct_data->rank ; i++) {
            ret = hg_proc_uint64_t(proc, &struct_data->size[i]);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
        }
        break;
    case HG_FREE:
        if(struct_data->rank)
            free(struct_data->size);
        break;
    }

    return ret;
}

int hg_proc_dset_io_in_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_dset_io_input_t *struct_data = (H5VL_iod_dset_io_input_t *) data;

    ret = hg_proc_iod_handle_t(proc, &struct_data->iod_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->scratch_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_dspace_t(proc, &struct_data->space_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_dxpl_t(proc, &struct_data->dxpl_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_uint32_t(proc, &struct_data->checksum);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_bds_handle_t(proc, &struct_data->bds_handle);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_dset_read_out_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_read_status_t *struct_data = (H5VL_iod_read_status_t *) data;

    ret = hg_proc_int32_t(proc, &struct_data->ret);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_uint32_t(proc, &struct_data->cs);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

int hg_proc_dset_close_in_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    H5VL_iod_remote_dset_t *struct_data = (H5VL_iod_remote_dset_t *) data;

    ret = hg_proc_iod_handle_t(proc, &struct_data->iod_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->iod_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_handle_t(proc, &struct_data->scratch_oh);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    ret = hg_proc_iod_obj_id_t(proc, &struct_data->scratch_id);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

/* Define hg_proc_fapl_t */
static int hg_proc_fapl_t(hg_proc_t proc, hid_t *data)
{
    int ret = HG_SUCCESS;
    size_t fapl_size = 0;
    void *buf = NULL;
    hid_t fapl_id;
    hg_proc_op_t op;

    op = hg_proc_get_op(proc);

    switch(op) {

    case HG_ENCODE:
        fapl_id = *data;
        if(H5P_FILE_ACCESS_DEFAULT != fapl_id) {
            if(H5Pencode(fapl_id, NULL, &fapl_size) < 0) {
                HG_ERROR_DEFAULT("FAPL encode Proc error");
                return HG_FAIL;
            }
        }
        if(fapl_size) {
            buf = H5MM_malloc(fapl_size);
            if(H5Pencode(fapl_id, buf, &fapl_size) < 0) {
                HG_ERROR_DEFAULT("FAPL encode Proc error");
                return HG_FAIL;
            }
        }

        ret = hg_proc_uint64_t(proc, &fapl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(fapl_size) {
            ret = hg_proc_raw(proc, buf, fapl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            H5MM_xfree(buf);
        }
        break;
    case HG_DECODE:
        ret = hg_proc_uint64_t(proc, &fapl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(fapl_size) {
            buf = H5MM_malloc(fapl_size);
            ret = hg_proc_raw(proc, buf, fapl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            if((fapl_id = H5Pdecode(buf)) < 0) {
                HG_ERROR_DEFAULT("FAPL decode Proc error");
                return HG_FAIL;
            }
            H5MM_xfree(buf);
        }
        else
            fapl_id = H5P_FILE_ACCESS_DEFAULT;

        *data = fapl_id;
        break;
    case HG_FREE:
        fapl_id = *data;
        if(fapl_id != H5P_FILE_ACCESS_DEFAULT) {
            if(H5Pclose(fapl_id) < 0) {
                HG_ERROR_DEFAULT("FAPL free Proc error");
                return HG_FAIL;
            }
        }
        break;
    default:
        HG_ERROR_DEFAULT("FAPL unsupported op Proc error");
    }

    return ret;
}

/* Define hg_proc_fcpl_t */
static int hg_proc_fcpl_t(hg_proc_t proc, hid_t *data)
{
    int ret = HG_SUCCESS;
    size_t fcpl_size = 0;
    void *buf = NULL;
    hid_t fcpl_id;
    hg_proc_op_t op;

    op = hg_proc_get_op(proc);

    switch(op) {

    case HG_ENCODE:
        fcpl_id = *data;
        if(H5P_FILE_CREATE_DEFAULT != fcpl_id) {
            if(H5Pencode(fcpl_id, NULL, &fcpl_size) < 0) {
                HG_ERROR_DEFAULT("FCPL encode Proc error");
                return HG_FAIL;
            }
        }
        if(fcpl_size) {
            buf = H5MM_malloc(fcpl_size);
            if(H5Pencode(fcpl_id, buf, &fcpl_size) < 0) {
                HG_ERROR_DEFAULT("FCPL encode Proc error");
                return HG_FAIL;
            }
        }

        ret = hg_proc_uint64_t(proc, &fcpl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(fcpl_size) {
            ret = hg_proc_raw(proc, buf, fcpl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            H5MM_xfree(buf);
        }
        break;
    case HG_DECODE:
        ret = hg_proc_uint64_t(proc, &fcpl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(fcpl_size) {
            buf = H5MM_malloc(fcpl_size);
            ret = hg_proc_raw(proc, buf, fcpl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            if((fcpl_id = H5Pdecode(buf)) < 0) {
                HG_ERROR_DEFAULT("FCPL decode Proc error");
                return HG_FAIL;
            }
            H5MM_xfree(buf);
        }
        else
            fcpl_id = H5P_FILE_CREATE_DEFAULT;

        *data = fcpl_id;
        break;
    case HG_FREE:
        fcpl_id = *data;
        if(fcpl_id != H5P_FILE_CREATE_DEFAULT) {
            if(H5Pclose(fcpl_id) < 0) {
                HG_ERROR_DEFAULT("FCPL free Proc error");
                return HG_FAIL;
            }
        }
        break;
    default:
        HG_ERROR_DEFAULT("FCPL unsupported op Proc error");
    }

    return ret;
}

/* Define hg_proc_gapl_t */
static int hg_proc_gapl_t(hg_proc_t proc, hid_t *data)
{
    int ret = HG_SUCCESS;
    size_t gapl_size = 0;
    void *buf = NULL;
    hid_t gapl_id;
    hg_proc_op_t op;

    op = hg_proc_get_op(proc);

    switch(op) {

    case HG_ENCODE:
        gapl_id = *data;
        if(H5P_GROUP_ACCESS_DEFAULT != gapl_id) {
            if(H5Pencode(gapl_id, NULL, &gapl_size) < 0) {
                HG_ERROR_DEFAULT("GAPL encode Proc error");
                return HG_FAIL;
            }
        }

        if(gapl_size) {
            buf = H5MM_malloc(gapl_size);
            if(H5Pencode(gapl_id, buf, &gapl_size) < 0) {
                HG_ERROR_DEFAULT("GAPL encode Proc error");
                return HG_FAIL;
            }
        }

        ret = hg_proc_uint64_t(proc, &gapl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(gapl_size) {
            ret = hg_proc_raw(proc, buf, gapl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            H5MM_xfree(buf);
        }
        break;
    case HG_DECODE:
        ret = hg_proc_uint64_t(proc, &gapl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(gapl_size) {
            buf = H5MM_malloc(gapl_size);
            ret = hg_proc_raw(proc, buf, gapl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            if((gapl_id = H5Pdecode(buf)) < 0) {
                HG_ERROR_DEFAULT("GAPL decode Proc error");
                return HG_FAIL;
            }
            H5MM_xfree(buf);
        }
        else
            gapl_id = H5P_GROUP_ACCESS_DEFAULT;

        *data = gapl_id;
        break;
    case HG_FREE:
        gapl_id = *data;
        if(gapl_id != H5P_GROUP_ACCESS_DEFAULT) {
            if(H5Pclose(gapl_id) < 0) {
                HG_ERROR_DEFAULT("GAPL free Proc error");
                return HG_FAIL;
            }
        }
        break;
    default:
        HG_ERROR_DEFAULT("GAPL unsupported op Proc error");
    }

    return ret;
}

/* Define hg_proc_gcpl_t */
static int hg_proc_gcpl_t(hg_proc_t proc, hid_t *data)
{
    int ret = HG_SUCCESS;
    size_t gcpl_size = 0;
    void *buf = NULL;
    hid_t gcpl_id;
    hg_proc_op_t op;

    op = hg_proc_get_op(proc);

    switch(op) {

    case HG_ENCODE:
        gcpl_id = *data;
        if(H5P_GROUP_CREATE_DEFAULT != gcpl_id) {
            if(H5Pencode(gcpl_id, NULL, &gcpl_size) < 0) {
                HG_ERROR_DEFAULT("GCPL encode Proc error");
                return HG_FAIL;
            }
        }

        if(gcpl_size) {
            buf = H5MM_malloc(gcpl_size);
            if(H5Pencode(gcpl_id, buf, &gcpl_size) < 0) {
                HG_ERROR_DEFAULT("GCPL encode Proc error");
                return HG_FAIL;
            }
        }

        ret = hg_proc_uint64_t(proc, &gcpl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(gcpl_size) {
            ret = hg_proc_raw(proc, buf, gcpl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            H5MM_xfree(buf);
        }
        break;
    case HG_DECODE:
        ret = hg_proc_uint64_t(proc, &gcpl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(gcpl_size) {
            buf = H5MM_malloc(gcpl_size);
            ret = hg_proc_raw(proc, buf, gcpl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            if((gcpl_id = H5Pdecode(buf)) < 0) {
                HG_ERROR_DEFAULT("GCPL decode Proc error");
                return HG_FAIL;
            }
            H5MM_xfree(buf);
        }
        else
            gcpl_id = H5P_GROUP_CREATE_DEFAULT;

        *data = gcpl_id;
        break;
    case HG_FREE:
        gcpl_id = *data;
        if(gcpl_id != H5P_GROUP_CREATE_DEFAULT) {
            if(H5Pclose(gcpl_id) < 0) {
                HG_ERROR_DEFAULT("GCPL free Proc error");
                return HG_FAIL;
            }
        }
        break;
    default:
        HG_ERROR_DEFAULT("GCPL unsupported op Proc error");
    }

    return ret;
}

/* Define hg_proc_dapl_t */
static int hg_proc_dapl_t(hg_proc_t proc, hid_t *data)
{
    int ret = HG_SUCCESS;
    size_t dapl_size = 0;
    void *buf = NULL;
    hid_t dapl_id;
    hg_proc_op_t op;

    op = hg_proc_get_op(proc);

    switch(op) {

    case HG_ENCODE:
        dapl_id = *data;
        if(H5P_DATASET_ACCESS_DEFAULT != dapl_id) {
            if(H5Pencode(dapl_id, NULL, &dapl_size) < 0) {
                HG_ERROR_DEFAULT("DAPL encode Proc error");
                return HG_FAIL;
            }
        }

        if(dapl_size) {
            buf = H5MM_malloc(dapl_size);
            if(H5Pencode(dapl_id, buf, &dapl_size) < 0) {
                HG_ERROR_DEFAULT("DAPL encode Proc error");
                return HG_FAIL;
            }
        }

        ret = hg_proc_uint64_t(proc, &dapl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(dapl_size) {
            ret = hg_proc_raw(proc, buf, dapl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            H5MM_xfree(buf);
        }
        break;
    case HG_DECODE:
        ret = hg_proc_uint64_t(proc, &dapl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(dapl_size) {
            buf = H5MM_malloc(dapl_size);
            ret = hg_proc_raw(proc, buf, dapl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            if((dapl_id = H5Pdecode(buf)) < 0) {
                HG_ERROR_DEFAULT("DAPL decode Proc error");
                return HG_FAIL;
            }
            H5MM_xfree(buf);
        }
        else
            dapl_id = H5P_DATASET_ACCESS_DEFAULT;

        *data = dapl_id;
        break;
    case HG_FREE:
        dapl_id = *data;
        if(dapl_id != H5P_DATASET_ACCESS_DEFAULT) {
            if(H5Pclose(dapl_id) < 0) {
                HG_ERROR_DEFAULT("DAPL free Proc error");
                return HG_FAIL;
            }
        }
        break;
    default:
        HG_ERROR_DEFAULT("DAPL unsupported op Proc error");
    }

    return ret;
}

/* Define hg_proc_dcpl_t */
static int hg_proc_dcpl_t(hg_proc_t proc, hid_t *data)
{
    int ret = HG_SUCCESS;
    size_t dcpl_size = 0;
    void *buf = NULL;
    hid_t dcpl_id;
    hg_proc_op_t op;

    op = hg_proc_get_op(proc);

    switch(op) {

    case HG_ENCODE:
        dcpl_id = *data;
        if(H5P_DATASET_CREATE_DEFAULT != dcpl_id) {
            if(H5Pencode(dcpl_id, NULL, &dcpl_size) < 0) {
                HG_ERROR_DEFAULT("DCPL encode Proc error");
                return HG_FAIL;
            }
        }

        if(dcpl_size) {
            buf = H5MM_malloc(dcpl_size);
            if(H5Pencode(dcpl_id, buf, &dcpl_size) < 0) {
                HG_ERROR_DEFAULT("DCPL encode Proc error");
                return HG_FAIL;
            }
        }

        ret = hg_proc_uint64_t(proc, &dcpl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(dcpl_size) {
            ret = hg_proc_raw(proc, buf, dcpl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            H5MM_xfree(buf);
        }
        break;
    case HG_DECODE:
        ret = hg_proc_uint64_t(proc, &dcpl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(dcpl_size) {
            buf = H5MM_malloc(dcpl_size);
            ret = hg_proc_raw(proc, buf, dcpl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            if((dcpl_id = H5Pdecode(buf)) < 0) {
                HG_ERROR_DEFAULT("DCPL decode Proc error");
                return HG_FAIL;
            }
            H5MM_xfree(buf);
        }
        else
            dcpl_id = H5P_DATASET_CREATE_DEFAULT;

        *data = dcpl_id;
        break;
    case HG_FREE:
        dcpl_id = *data;
        if(dcpl_id != H5P_DATASET_CREATE_DEFAULT) {
            if(H5Pclose(dcpl_id) < 0) {
                HG_ERROR_DEFAULT("DCPL free Proc error");
                return HG_FAIL;
            }
        }
        break;
    default:
        HG_ERROR_DEFAULT("DCPL unsupported op Proc error");
    }

    return ret;
}

/* Define hg_proc_dxpl_t */
static int hg_proc_dxpl_t(hg_proc_t proc, hid_t *data)
{
    int ret = HG_SUCCESS;
    size_t dxpl_size = 0;
    void *buf = NULL;
    hid_t dxpl_id;
    hg_proc_op_t op;

    op = hg_proc_get_op(proc);

    switch(op) {

    case HG_ENCODE:
        dxpl_id = *data;
        if(H5P_DATASET_XFER_DEFAULT != dxpl_id) {
            if(H5Pencode(dxpl_id, NULL, &dxpl_size) < 0) {
                HG_ERROR_DEFAULT("DXPL encode Proc error");
                return HG_FAIL;
            }
        }

        if(dxpl_size) {
            buf = H5MM_malloc(dxpl_size);
            if(H5Pencode(dxpl_id, buf, &dxpl_size) < 0) {
                HG_ERROR_DEFAULT("DXPL encode Proc error");
                return HG_FAIL;
            }
        }

        ret = hg_proc_uint64_t(proc, &dxpl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(dxpl_size) {
            ret = hg_proc_raw(proc, buf, dxpl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            H5MM_xfree(buf);
        }
        break;
    case HG_DECODE:
        ret = hg_proc_uint64_t(proc, &dxpl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(dxpl_size) {
            buf = H5MM_malloc(dxpl_size);
            ret = hg_proc_raw(proc, buf, dxpl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            if((dxpl_id = H5Pdecode(buf)) < 0) {
                HG_ERROR_DEFAULT("DXPL decode Proc error");
                return HG_FAIL;
            }
            H5MM_xfree(buf);
        }
        else
            dxpl_id = H5P_DATASET_XFER_DEFAULT;

        *data = dxpl_id;
        break;
    case HG_FREE:
        dxpl_id = *data;
        if(dxpl_id != H5P_DATASET_XFER_DEFAULT) {
            if(H5Pclose(dxpl_id) < 0) {
                HG_ERROR_DEFAULT("DXPL free Proc error");
                return HG_FAIL;
            }
        }
        break;
    default:
        HG_ERROR_DEFAULT("DXPL unsupported op Proc error");
    }

    return ret;
}

/* Define hg_proc_lcpl_t */
static int hg_proc_lcpl_t(hg_proc_t proc, hid_t *data)
{
    int ret = HG_SUCCESS;
    size_t lcpl_size = 0;
    void *buf = NULL;
    hid_t lcpl_id;
    hg_proc_op_t op;

    op = hg_proc_get_op(proc);

    switch(op) {

    case HG_ENCODE:
        lcpl_id = *data;
        if(H5P_LINK_CREATE_DEFAULT != lcpl_id) {
            if(H5Pencode(lcpl_id, NULL, &lcpl_size) < 0) {
                HG_ERROR_DEFAULT("LCPL encode Proc error");
                return HG_FAIL;
            }
        }

        if(lcpl_size) {
            buf = H5MM_malloc(lcpl_size);
            if(H5Pencode(lcpl_id, buf, &lcpl_size) < 0) {
                HG_ERROR_DEFAULT("LCPL encode Proc error");
                return HG_FAIL;
            }
        }

        ret = hg_proc_uint64_t(proc, &lcpl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(lcpl_size) {
            ret = hg_proc_raw(proc, buf, lcpl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            H5MM_xfree(buf);
        }
        break;
    case HG_DECODE:
        ret = hg_proc_uint64_t(proc, &lcpl_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        if(lcpl_size) {
            buf = H5MM_malloc(lcpl_size);
            ret = hg_proc_raw(proc, buf, lcpl_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
            if((lcpl_id = H5Pdecode(buf)) < 0) {
                HG_ERROR_DEFAULT("LCPL decode Proc error");
                return HG_FAIL;
            }
            H5MM_xfree(buf);
        }
        else
            lcpl_id = H5P_LINK_CREATE_DEFAULT;

        *data = lcpl_id;
        break;
    case HG_FREE:
        lcpl_id = *data;
        if(lcpl_id != H5P_LINK_CREATE_DEFAULT) {
            if(H5Pclose(lcpl_id) < 0) {
                HG_ERROR_DEFAULT("LCPL free Proc error");
                return HG_FAIL;
            }
        }
        break;
    default:
        HG_ERROR_DEFAULT("LCPL unsupported op Proc error");
    }

    return ret;
}

#endif

#endif /* H5_HAVE_EFF */
