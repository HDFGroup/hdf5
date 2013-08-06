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
#include "H5Ppublic.h"
#include "H5Spublic.h"
#include "H5VLiod_common.h"     /* IOD Common Header			*/

#ifdef H5_HAVE_EFF

int hg_proc_size_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    size_t *struct_data = (size_t *) data;

    ret = hg_proc_memcpy(proc, struct_data, sizeof(size_t));
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    return ret;
}

/* Define hg_proc_ret_t */
int hg_proc_ret_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;

    ret = hg_proc_int32_t(proc, (int32_t *)data);
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

    ret = hg_proc_int32_t(proc, (int32_t *)data);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    return ret;
}

/* Define hg_proc_hbool_t */
int hg_proc_hbool_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;

    ret = hg_proc_uint32_t(proc, (uint32_t *)data);
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

    ret = hg_proc_uint64_t(proc, struct_data);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    return ret;
}

int hg_proc_axe_ids_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    size_t i;
    hg_proc_op_t op;
    axe_ids_t *struct_data = (axe_ids_t *) data;

    ret = hg_proc_uint64_t(proc, &struct_data->count);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    op = hg_proc_get_op(proc);

    switch(op) {
    case HG_ENCODE:
        for(i=0 ; i<struct_data->count ; i++) {
            ret = hg_proc_uint64_t(proc, &struct_data->ids[i]);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
        }
        break;
    case HG_DECODE:
        if(struct_data->count)
            struct_data->ids = (uint64_t *)malloc (sizeof(hsize_t) * struct_data->count);

        for(i=0 ; i<struct_data->count ; i++) {
            ret = hg_proc_uint64_t(proc, &struct_data->ids[i]);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
        }
        break;
    case HG_FREE:
        if(struct_data->count)
            free(struct_data->ids);
        break;
    default:
        return HG_FAIL;
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
            struct_data->size = (hsize_t *)malloc (sizeof(hsize_t) * struct_data->rank);

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
    default:
        return HG_FAIL;
    }

    return ret;
}

int hg_proc_name_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    size_t size;
    name_t *struct_data = (name_t *) data;

    ret = hg_proc_memcpy(proc, struct_data->value_size, sizeof(ssize_t));
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }
    size = (size_t)(*struct_data->value_size);

    ret = hg_proc_memcpy(proc, &struct_data->size, sizeof(size_t));
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    if(NULL != struct_data->value && struct_data->size != 0) {
        ret = hg_proc_raw(proc, struct_data->value, 
                          MIN(size, struct_data->size));
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
    }
    return ret;
}

int hg_proc_binary_buf_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    size_t size;
    hg_proc_op_t op;
    binary_buf_t *struct_data = (binary_buf_t *) data;

    ret = hg_proc_raw(proc, &struct_data->buf_size, sizeof(size_t));
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    op = hg_proc_get_op(proc);

    switch(op) {
    case HG_ENCODE:
        if(NULL != struct_data->buf && struct_data->buf_size != 0) {
            ret = hg_proc_raw(proc, struct_data->buf, struct_data->buf_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
        }
        break;
    case HG_DECODE:
        if(struct_data->buf_size != 0) {
            struct_data->buf = malloc (struct_data->buf_size);
            ret = hg_proc_raw(proc, struct_data->buf, struct_data->buf_size);
            if (ret != HG_SUCCESS) {
                HG_ERROR_DEFAULT("Proc error");
                ret = HG_FAIL;
                return ret;
            }
        }
        break;
    case HG_FREE:
        if(struct_data->buf_size != 0) {
            free(struct_data->buf);
        }
        break;
    default:
        return HG_FAIL;
    }
    return ret;
}

int hg_proc_value_t(hg_proc_t proc, void *data)
{
    int ret = HG_SUCCESS;
    size_t size;
    hg_proc_op_t op;
    value_t *struct_data = (value_t *) data;

    ret = hg_proc_raw(proc, &struct_data->val_size, sizeof(size_t));
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    if(NULL != struct_data->val && struct_data->val_size != 0) {
        ret = hg_proc_raw(proc, struct_data->val, struct_data->val_size);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
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
        if(H5P_FILE_CREATE_DEFAULT != plist_id && H5P_GROUP_CREATE_DEFAULT != plist_id &&
           H5P_LINK_CREATE_DEFAULT != plist_id && H5P_DATASET_CREATE_DEFAULT != plist_id &&
           H5P_FILE_ACCESS_DEFAULT != plist_id && H5P_GROUP_ACCESS_DEFAULT != plist_id &&
           H5P_ATTRIBUTE_CREATE_DEFAULT != plist_id && H5P_OBJECT_COPY_DEFAULT != plist_id &&
           H5P_DATASET_ACCESS_DEFAULT != plist_id && H5P_DATASET_XFER_DEFAULT != plist_id &&
           H5P_DATATYPE_CREATE_DEFAULT != plist_id && H5P_DATATYPE_ACCESS_DEFAULT != plist_id &&
           H5P_LINK_ACCESS_DEFAULT != plist_id) {
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

    if (HG_ENCODE == op || HG_FREE == op) {
        if(0 == id)
            type = H5I_UNINIT;
        else
            type = H5Iget_type(id);
    }
    ret = hg_proc_int32_t(proc, &type);
    if (ret != HG_SUCCESS) {
        HG_ERROR_DEFAULT("Proc error");
        ret = HG_FAIL;
        return ret;
    }

    switch(type) {
    case H5I_DATASPACE:
        ret = hg_proc_dspace_t(proc, (hid_t *)data);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        break;
    case H5I_DATATYPE:
        ret = hg_proc_dtype_t(proc, (hid_t *)data);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        break;
    case H5I_GENPROP_LST:
        ret = hg_proc_plist_t(proc, (hid_t *)data);
        if (ret != HG_SUCCESS) {
            HG_ERROR_DEFAULT("Proc error");
            ret = HG_FAIL;
            return ret;
        }
        break;
    case H5I_UNINIT:
        break;
    default:
        HG_ERROR_DEFAULT("Unsupported hid_t - Proc error");
        ret = HG_FAIL;
        break;
    }
    return ret;
}

#endif /* H5_HAVE_EFF */
