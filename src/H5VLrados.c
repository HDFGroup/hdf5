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

/*
 * Programmer:  Neil Fortner <nfortne2@hdfgroup.org>
 *              December, 2017
 *
 * Purpose: The RADOS VOL plugin where access is forwarded to the RADOS API 
 */
 
#define H5O_FRIEND              /* Suppress error about including H5Opkg */

#include "H5private.h"          /* Generic Functions                    */
#include "H5Dprivate.h"         /* Datasets                             */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5Fprivate.h"         /* Files                                */
#include "H5FDprivate.h"        /* File drivers                         */
#include "H5Gprivate.h"         /* Groups                               */
#include "H5Iprivate.h"         /* IDs                                  */
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5Opkg.h"             /* Objects                              */
#include "H5Pprivate.h"         /* Property lists                       */
#include "H5Sprivate.h"         /* Dataspaces                           */
#include "H5VLprivate.h"        /* VOL plugins                          */
#include "H5VLrados.h"          /* RADOS plugin                         */

hid_t H5VL_RADOS_g = -1;

/*
 * Macros
 */
/* Stack allocation sizes */
#define H5VL_RADOS_FOI_BUF_SIZE 1024
#define H5VL_RADOS_LINK_VAL_BUF_SIZE 256
#define H5VL_RADOS_GINFO_BUF_SIZE 256

/* Definitions for building oids */
#define H5VL_RADOS_IDX_MASK   0x3fffffffffffffffull
#define H5VL_RADOS_TYPE_MASK  0xc000000000000000ull
#define H5VL_RADOS_TYPE_GRP   0x0000000000000000ull
#define H5VL_RADOS_TYPE_DSET  0x4000000000000000ull
#define H5VL_RADOS_TYPE_DTYPE 0x8000000000000000ull

/*
 * Typedefs
 */
/* RADOS-specific file access properties */
typedef struct H5VL_rados_fapl_t {
    MPI_Comm            comm;           /*communicator                  */
    MPI_Info            info;           /*file information              */
} H5VL_rados_fapl_t;

/* RADOS oid target type (HDF5 object, attribute, chunk) */
typedef enum H5VL_rados_targ_type_t {
    H5VL_RADOS_TARG_TYPE_OBJ,
    H5VL_RADOS_TARG_TYPE_ATTR,
    H5VL_RADOS_TARG_TYPE_CHK
} H5VL_rados_targ_type_t;

#if 0
/* Enum to indicate if the supplied read buffer can be used as a type conversion
 * or background buffer */
typedef enum {
    H5VL_RADOS_TCONV_REUSE_NONE,    /* Cannot reuse buffer */
    H5VL_RADOS_TCONV_REUSE_TCONV,   /* Use buffer as type conversion buffer */
    H5VL_RADOS_TCONV_REUSE_BKG      /* Use buffer as background buffer */
} H5VL_rados_tconv_reuse_t;

/* Udata type for H5Dscatter callback */
typedef struct H5VL_rados_scatter_cb_ud_t {
    void *buf;
    size_t len;
} H5VL_rados_scatter_cb_ud_t;

/* Udata type for memory space H5Diterate callback */
typedef struct {
    daos_iod_t *iods;
    daos_sg_list_t *sgls;
    daos_iov_t *sg_iovs;
    hbool_t is_vl_str;
    size_t base_type_size;
    uint64_t offset;
    uint64_t idx;
} H5VL_rados_vl_mem_ud_t;

/* Udata type for file space H5Diterate callback */
typedef struct {
    uint8_t **akeys;
    daos_iod_t *iods;
    uint64_t idx;
} H5VL_rados_vl_file_ud_t;
#endif
/*
 * Prototypes
 */
static void *H5VL_rados_fapl_copy(const void *_old_fa);
static herr_t H5VL_rados_fapl_free(void *_fa);
static herr_t H5VL_rados_term(hid_t vtpl_id);

/* File callbacks */
static void *H5VL_rados_file_create(const char *name, unsigned flags,
    hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id, void **req);
static void *H5VL_rados_file_open(const char *name, unsigned flags,
    hid_t fapl_id, hid_t dxpl_id, void **req);
//static herr_t H5VL_iod_file_get(void *file, H5VL_file_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
/*static herr_t H5VL_rados_file_specific(void *_item,
    H5VL_file_specific_t specific_type, hid_t dxpl_id, void **req,
    va_list arguments);*/
static herr_t H5VL_rados_file_close(void *_file, hid_t dxpl_id, void **req);

/* Link callbacks */
/*static herr_t H5VL_rados_link_create(H5VL_link_create_type_t create_type,
    void *_item, H5VL_loc_params_t loc_params, hid_t lcpl_id, hid_t lapl_id,
    hid_t dxpl_id, void **req);
static herr_t H5VL_rados_link_specific(void *_item,
    H5VL_loc_params_t loc_params, H5VL_link_specific_t specific_type,
    hid_t dxpl_id, void **req, va_list arguments);*/

/* Group callbacks */
static void *H5VL_rados_group_create(void *_item, H5VL_loc_params_t loc_params,
    const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req);
static void *H5VL_rados_group_open(void *_item, H5VL_loc_params_t loc_params,
    const char *name, hid_t gapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_rados_group_close(void *_grp, hid_t dxpl_id, void **req);

/* Dataset callbacks */
/*static void *H5VL_rados_dataset_create(void *_item,
    H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id,
    hid_t dapl_id, hid_t dxpl_id, void **req);
static void *H5VL_rados_dataset_open(void *_item, H5VL_loc_params_t loc_params,
    const char *name, hid_t dapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_rados_dataset_read(void *_dset, hid_t mem_type_id,
    hid_t mem_space_id, hid_t file_space_id, hid_t dxpl_id, void *buf,
    void **req);
static herr_t H5VL_rados_dataset_write(void *_dset, hid_t mem_type_id,
    hid_t mem_space_id, hid_t file_space_id, hid_t dxpl_id, const void *buf,
    void **req);*/
/*static herr_t H5VL_rados_dataset_specific(void *_dset, H5VL_dataset_specific_t specific_type,
                                        hid_t dxpl_id, void **req, va_list arguments);*/
/*static herr_t H5VL_rados_dataset_get(void *_dset, H5VL_dataset_get_t get_type,
    hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_rados_dataset_close(void *_dset, hid_t dxpl_id, void **req);*/

/* Datatype callbacks */
/*static void *H5VL_rados_datatype_commit(void *obj, H5VL_loc_params_t loc_params,
    const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id,
    hid_t tapl_id, hid_t dxpl_id, void **req);
static void *H5VL_rados_datatype_open(void *_item, H5VL_loc_params_t loc_params,
    const char *name, hid_t tapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_rados_datatype_get(void *obj, H5VL_datatype_get_t get_type,
    hid_t dxpl_id, void **req, va_list arguments);*/

/* Object callbacks */
/*static void *H5VL_rados_object_open(void *_item, H5VL_loc_params_t loc_params, 
    H5I_type_t *opened_type, hid_t dxpl_id, void **req);
static herr_t H5VL_rados_object_optional(void *_item, hid_t dxpl_id, void **req,
    va_list arguments);*/

/* Attribute callbacks */
/*static void *H5VL_rados_attribute_create(void *_obj,
    H5VL_loc_params_t loc_params, const char *name, hid_t acpl_id,
    hid_t aapl_id, hid_t dxpl_id, void **req);
static void *H5VL_rados_attribute_open(void *_obj, H5VL_loc_params_t loc_params,
    const char *name, hid_t aapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_rados_attribute_read(void *_attr, hid_t mem_type_id,
    void *buf, hid_t dxpl_id, void **req);
static herr_t H5VL_rados_attribute_write(void *_attr, hid_t mem_type_id,
    const void *buf, hid_t dxpl_id, void **req);
static herr_t H5VL_rados_attribute_get(void *_item, H5VL_attr_get_t get_type,
    hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_rados_attribute_specific(void *_item,
    H5VL_loc_params_t loc_params, H5VL_attr_specific_t specific_type,
    hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_rados_attribute_close(void *_attr, hid_t dxpl_id,
    void **req);*/

/* Helper routines */
//static herr_t H5VL_rados_write_max_oid(H5VL_rados_file_t *file);
//static herr_t H5VL_rados_file_flush(H5VL_rados_file_t *file);
static herr_t H5VL_rados_file_close_helper(H5VL_rados_file_t *file,
    hid_t dxpl_id, void **req);

static herr_t H5VL_rados_link_read(H5VL_rados_group_t *grp, char *name,
    size_t name_len, H5VL_rados_link_val_t *val);
static herr_t H5VL_rados_link_write(H5VL_rados_group_t *grp, const char *name,
    H5VL_rados_link_val_t *val);
static herr_t H5VL_rados_link_follow(H5VL_rados_group_t *grp, char *name,
    size_t name_len, hid_t dxpl_id, void **req, uint64_t *oid);

static H5VL_rados_group_t *H5VL_rados_group_traverse(H5VL_rados_item_t *item,
    char *path, hid_t dxpl_id, void **req, const char **obj_name,
    void **gcpl_buf_out, uint64_t *gcpl_len_out);
static void *H5VL_rados_group_create_helper(H5VL_rados_file_t *file,
    hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req,
    H5VL_rados_group_t *parent_grp, const char *name, hbool_t collective);
static void *H5VL_rados_group_open_helper(H5VL_rados_file_t *file,
    uint64_t oid, hid_t gapl_id, hid_t dxpl_id, void **req, void **gcpl_buf_out,
    uint64_t *gcpl_len_out);
static void *H5VL_rados_group_reconstitute(H5VL_rados_file_t *file,
    uint64_t oid, uint8_t *gcpl_buf, hid_t gapl_id, hid_t dxpl_id, void **req);

/*static htri_t H5VL_rados_need_bkg(hid_t src_type_id, hid_t dst_type_id,
    size_t *dst_type_size, hbool_t *fill_bkg);
static herr_t H5VL_rados_tconv_init(hid_t src_type_id, size_t *src_type_size,
    hid_t dst_type_id, size_t *dst_type_size, size_t num_elem, void **tconv_buf,
    void **bkg_buf, H5VL_rados_tconv_reuse_t *reuse, hbool_t *fill_bkg);
static herr_t H5VL_rados_sel_to_recx_iov(H5S_t *space, size_t type_size,
    void *buf, daos_recx_t **recxs, daos_iov_t **sg_iovs, size_t *list_nused);
static herr_t H5VL_rados_scatter_cb(const void **src_buf,
    size_t *src_buf_bytes_used, void *_udata);
static herr_t H5VL_rados_dataset_mem_vl_rd_cb(void *_elem, hid_t type_id,
    unsigned ndim, const hsize_t *point, void *_udata);
static herr_t H5VL_rados_dataset_file_vl_cb(void *_elem, hid_t type_id,
    unsigned ndim, const hsize_t *point, void *_udata);
static herr_t H5VL_rados_dataset_mem_vl_wr_cb(void *_elem, hid_t type_id,
    unsigned ndim, const hsize_t *point, void *_udata);

static herr_t H5VL_rados_datatype_close(void *_dtype, hid_t dxpl_id,
    void **req);

static herr_t H5VL_rados_object_close(void *_obj, hid_t dxpl_id, void **req);*/

/* Free list definitions */
H5FL_DEFINE(H5VL_rados_file_t);
H5FL_DEFINE(H5VL_rados_group_t);
/*H5FL_DEFINE(H5VL_rados_dset_t);
H5FL_DEFINE(H5VL_rados_dtype_t);
H5FL_DEFINE(H5VL_rados_attr_t);*/

/* The RADOS VOL plugin struct */
static H5VL_class_t H5VL_rados_g = {
    HDF5_VOL_RADOS_VERSION_1,                   /* Version number */
    H5_VOL_RADOS,                               /* Plugin value */
    "rados",                                   /* name */
    NULL,                                       /* initialize */
    H5VL_rados_term,                            /* terminate */
    sizeof(H5VL_rados_fapl_t),                  /*fapl_size */
    H5VL_rados_fapl_copy,                       /*fapl_copy */
    H5VL_rados_fapl_free,                       /*fapl_free */
    {                                           /* attribute_cls */
        NULL,//H5VL_rados_attribute_create,            /* create */
        NULL,//H5VL_rados_attribute_open,              /* open */
        NULL,//H5VL_rados_attribute_read,              /* read */
        NULL,//H5VL_rados_attribute_write,             /* write */
        NULL,//H5VL_rados_attribute_get,               /* get */
        NULL,//H5VL_rados_attribute_specific,          /* specific */
        NULL,                                   /* optional */
        NULL,//H5VL_rados_attribute_close              /* close */
    },
    {                                           /* dataset_cls */
        NULL,//H5VL_rados_dataset_create,              /* create */
        NULL,//H5VL_rados_dataset_open,                /* open */
        NULL,//H5VL_rados_dataset_read,                /* read */
        NULL,//H5VL_rados_dataset_write,               /* write */
        NULL,//H5VL_rados_dataset_get,                 /* get */
        NULL,//H5VL_iod_dataset_specific,              /* specific */
        NULL,                                   /* optional */
        NULL,//H5VL_rados_dataset_close                /* close */
    },
    {                                           /* datatype_cls */
        NULL,//H5VL_rados_datatype_commit,             /* commit */
        NULL,//H5VL_rados_datatype_open,               /* open */
        NULL,//H5VL_rados_datatype_get,                /* get */
        NULL,                                   /* specific */
        NULL,                                   /* optional */
        NULL,//H5VL_rados_datatype_close               /* close */
    },
    {                                           /* file_cls */
        H5VL_rados_file_create,                 /* create */
        H5VL_rados_file_open,                     /* open */
        NULL,//H5VL_iod_file_get,                      /* get */
        NULL,//H5VL_rados_file_specific,               /* specific */
        NULL,                                   /* optional */
        H5VL_rados_file_close                     /* close */
    },
    {                                           /* group_cls */
        H5VL_rados_group_create,                /* create */
        H5VL_rados_group_open,                  /* open */
        NULL,//H5VL_iod_group_get,                     /* get */
        NULL,                                   /* specific */
        NULL,                                   /* optional */
        H5VL_rados_group_close                  /* close */
    },
    {                                           /* link_cls */
        NULL,//H5VL_rados_link_create,                 /* create */
        NULL,//H5VL_iod_link_copy,                     /* copy */
        NULL,//H5VL_iod_link_move,                     /* move */
        NULL,//H5VL_iod_link_get,                      /* get */
        NULL,//H5VL_rados_link_specific,               /* specific */
        NULL                                    /* optional */
    },
    {                                           /* object_cls */
        NULL,//H5VL_rados_object_open,                 /* open */
        NULL,                                   /* copy */
        NULL,                                   /* get */
        NULL,//H5VL_iod_object_specific,               /* specific */
        NULL,//H5VL_rados_object_optional              /* optional */
    },
    {
        NULL,//H5VL_iod_cancel,
        NULL,//H5VL_iod_test,
        NULL,//H5VL_iod_wait
    },
    NULL
};

/* The RADOS cluster */
rados_t cluster_g;
hbool_t cluster_init_g = FALSE;

/* The RADOS IO context */
rados_ioctx_t ioctx_g;
hbool_t ioctx_init_g = FALSE;


/* Create a RADOS string oid given the file name and binary oid */
static herr_t
H5VL_rados_oid_create_string(const H5VL_rados_file_t *file, uint64_t bin_oid,
    char **oid)
{
    char *tmp_oid = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Allocate space for oid */
    if(NULL == (tmp_oid = (char *)H5MM_malloc(file->file_name_len + 16 + 1)))
        HGOTO_ERROR(H5E_VOL, H5E_CANTALLOC, FAIL, "can't allocate RADOS object id")

    /* Encode file name and binary oid into string oid */
    if(HDsnprintf(tmp_oid, file->file_name_len + 16 + 1, "%s%016llX",
            file->file_name, (long long unsigned)bin_oid)
            != (int)file->file_name_len + 16)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't encode string object id")

    /* Return oid string value */
    *oid = tmp_oid;
    tmp_oid = NULL;

done:
    H5MM_xfree(tmp_oid);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_oid_create_string() */


/* Create a binary RADOS oid given the object type and a 64 bit index (top 2
 * bits are ignored) */
static void
H5VL_rados_oid_create_binary(uint64_t idx, H5VL_rados_targ_type_t targ_type,
    H5I_type_t obj_type, uint64_t *bin_oid)
{
    /* Initialize bin_oid */
    *bin_oid = idx;

    /* Check target type */
    if(targ_type == H5VL_RADOS_TARG_TYPE_OBJ) {
        *bin_oid &= H5VL_RADOS_IDX_MASK;

        /* Set type_bits */
        if(obj_type == H5I_GROUP)
            *bin_oid |= H5VL_RADOS_TYPE_GRP;
        else if(obj_type == H5I_DATASET)
            *bin_oid |= H5VL_RADOS_TYPE_DSET;
        else {
            HDassert(obj_type == H5I_DATATYPE);
            *bin_oid |= H5VL_RADOS_TYPE_DTYPE;
        } /* end else */
    }
    /* Note: we may eventually bake the target type into the oid so we can use
     * multiple idx streams (one for each target and object type) */

    return;
} /* end H5VL_rados_oid_create_binary() */


/* Create a RADOS oid given the file name, object type and a 64 bit index (top 2
 * bits are ignored) */
static herr_t
H5VL_rados_oid_create(const H5VL_rados_file_t *file, uint64_t idx,
    H5VL_rados_targ_type_t targ_type, H5I_type_t obj_type, uint64_t *bin_oid,
    char **oid)
{
    uint64_t tmp_bin_oid = *bin_oid;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Create binary oid */
    H5VL_rados_oid_create_binary(idx, targ_type, obj_type, &tmp_bin_oid);

    /* Create sting oid */
    if(H5VL_rados_oid_create_string(file, tmp_bin_oid, oid) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't encode string object id")

    /* Return oid binary value (string already returned) */
    *bin_oid = tmp_bin_oid;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_oid_create() */


/* Retrieve the 64 bit object index from a RADOS oid  */
static uint64_t
H5VL_rados_oid_to_idx(uint64_t bin_oid)
{
    return bin_oid & H5VL_RADOS_IDX_MASK;
} /* end H5VL_rados_oid_to_idx() */


/*-------------------------------------------------------------------------
 * Function:    H5VLrados_init
 *
 * Purpose:     Initialize this vol plugin by connecting to the cluster
 *              and registering the driver with the library.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              February, 2018
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLrados_init(rados_t rados_cluster, const char *rados_pool)
{
    int ret;
    herr_t ret_value = SUCCEED;            /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check if already initialized */
    if(H5VL_RADOS_g >= 0)
        HGOTO_DONE(SUCCEED)

    /* Register the RADOS VOL, if it isn't already */
    if(H5VL_rados_init() < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to initialize RADOS VOL plugin")

    /* Save cluster */
    cluster_g = rados_cluster;

    /* Connect to cluster */
    if((ret = rados_connect(rados_cluster)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't connect to cluster: %s", strerror(ret))
    cluster_init_g = TRUE;

    /* Create IO context */
    if((ret = rados_ioctx_create(cluster_g, rados_pool, &ioctx_g)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "can't create IO context: %s", strerror(ret))
    ioctx_init_g = TRUE;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLrados_init() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_init
 *
 * Purpose:     Initialize this vol plugin by registering the driver with the
 *              library.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              February, 2018
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_rados_init(void)
{
    herr_t ret_value = SUCCEED;            /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Register interfaces that might not be initialized in time (for example if
     * we open an object without knowing its type first, H5Oopen will not
     * initialize that type) */
    if(H5G_init() < 0)
        HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL, "unable to initialize group interface")
    if(H5D_init() < 0)
        HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL, "unable to initialize dataset interface")
    if(H5T_init() < 0)
        HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL, "unable to initialize datatype interface")

    /* Register the RADOS VOL, if it isn't already */
    if(NULL == H5I_object_verify(H5VL_RADOS_g, H5I_VOL)) {
        if((H5VL_RADOS_g = H5VL_register((const H5VL_class_t *)&H5VL_rados_g, 
                                          sizeof(H5VL_class_t), TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTINSERT, FAIL, "can't create ID for RADOS plugin")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_init() */


/*-------------------------------------------------------------------------
 * Function:    H5VLrados_term
 *
 * Purpose:     Shut down the RADOS VOL
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              January, 2018
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLrados_term(void)
{
    herr_t ret_value = SUCCEED;            /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE0("e","");

    /* Terminate the plugin */
    if(H5VL_rados_term(-1) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CLOSEERROR, FAIL, "can't close RADOS VOL plugin")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLrados_term() */


/*---------------------------------------------------------------------------
 * Function:    H5VL_rados_term
 *
 * Purpose:     Shut down the RADOS VOL
 *
 * Returns:     Non-negative on success/Negative on failure
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5VL_rados_term(hid_t H5_ATTR_UNUSED vtpl_id)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5VL_RADOS_g >= 0) {
        /* Terminate RADOS */
        if(ioctx_init_g) {
            rados_ioctx_destroy(ioctx_g);
            ioctx_init_g = FALSE;
        } /* end if */
        if(cluster_init_g) {
            rados_shutdown(cluster_g);
            cluster_init_g = FALSE;
        } /* end if */

        /* "Forget" plugin id.  This should normally be called by the library
         * when it is closing the id, so no need to close it here. */
        H5VL_RADOS_g = -1;
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_term() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_fapl_rados
 *
 * Purpose:     Modify the file access property list to use the H5VL_RADOS
 *              plugin defined in this source file.  file_comm and
 *              file_info identify the communicator and info object used
 *              to coordinate actions on file create, open, flush, and
 *              close.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              January, 2018
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_rados(hid_t fapl_id, MPI_Comm file_comm, MPI_Info file_info)
{
    H5VL_rados_fapl_t fa;
    H5P_genplist_t  *plist;      /* Property list pointer */
    herr_t          ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iMcMi", fapl_id, file_comm, file_info);

    if(H5VL_RADOS_g < 0)
        HGOTO_ERROR(H5E_VOL, H5E_UNINITIALIZED, FAIL, "RADOS VOL plugin not initialized")

    if(fapl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    if(MPI_COMM_NULL == file_comm)
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a valid communicator")

    /* Initialize driver specific properties */
    fa.comm = file_comm;
    fa.info = file_info;

    ret_value = H5P_set_vol(plist, H5VL_RADOS_g, &fa);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_rados() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_fapl_copy
 *
 * Purpose:     Copies the rados-specific file access properties.
 *
 * Return:      Success:        Ptr to a new property list
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              February, 2018
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_rados_fapl_copy(const void *_old_fa)
{
    const H5VL_rados_fapl_t *old_fa = (const H5VL_rados_fapl_t*)_old_fa;
    H5VL_rados_fapl_t     *new_fa = NULL;
    void                  *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (new_fa = (H5VL_rados_fapl_t *)H5MM_malloc(sizeof(H5VL_rados_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy the general information */
    HDmemcpy(new_fa, old_fa, sizeof(H5VL_rados_fapl_t));

    /* Clear allocated fields, so they aren't freed if something goes wrong.  No
     * need to clear info since it is only freed if comm is not null. */
    new_fa->comm = MPI_COMM_NULL;

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(old_fa->comm, old_fa->info, &new_fa->comm, &new_fa->info))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed")

    ret_value = new_fa;

done:
    if (NULL == ret_value) {
        /* cleanup */
        if(new_fa && H5VL_rados_fapl_free(new_fa) < 0)
            HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, NULL, "can't free fapl")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_fapl_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_fapl_free
 *
 * Purpose:     Frees the rados-specific file access properties.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 * Programmer:  Neil Fortner
 *              February, 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_rados_fapl_free(void *_fa)
{
    herr_t              ret_value = SUCCEED;
    H5VL_rados_fapl_t   *fa = (H5VL_rados_fapl_t*)_fa;

    FUNC_ENTER_NOAPI_NOINIT

    assert(fa);

    /* Free the internal communicator and INFO object */
    if(fa->comm != MPI_COMM_NULL)
        if(H5FD_mpi_comm_info_free(&fa->comm, &fa->info) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "Communicator/Info free failed")

    /* free the struct */
    H5MM_xfree(fa);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_fapl_free() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_file_create
 *
 * Purpose:     Creates a file as a rados HDF5 file.
 *
 * Return:      Success:        the file id. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              January, 2018
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_rados_file_create(const char *name, unsigned flags, hid_t fcpl_id,
    hid_t fapl_id, hid_t dxpl_id, void **req)
{
    H5VL_rados_fapl_t *fa = NULL;
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    H5VL_rados_file_t *file = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Adjust bit flags by turning on the creation bit and making sure that
     * the EXCL or TRUNC bit is set.  All newly-created files are opened for
     * reading and writing.
     */
    if(0==(flags & (H5F_ACC_EXCL|H5F_ACC_TRUNC)))
        flags |= H5F_ACC_EXCL;      /*default*/
    flags |= H5F_ACC_RDWR | H5F_ACC_CREAT;

    /* Get information from the FAPL */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if(NULL == (fa = (H5VL_rados_fapl_t *)H5P_get_vol_info(plist)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't get RADOS info struct")

    /* allocate the file object that is returned to the user */
    if(NULL == (file = H5FL_CALLOC(H5VL_rados_file_t)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate RADOS file struct")
    //file->glob_md_oh = DAOS_HDL_INVAL;
    file->root_grp = NULL;
    file->fcpl_id = FAIL;
    file->fapl_id = FAIL;

    /* Fill in fields of file we know */
    file->item.type = H5I_FILE;
    file->item.file = file;
    file->item.rc = 1;
    if(NULL == (file->file_name = HDstrdup(name)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't copy file name")
    file->file_name_len = HDstrlen(name);
    file->flags = flags;
    file->max_oid = 0;
    file->max_oid_dirty = FALSE;
    if((file->fcpl_id = H5Pcopy(fcpl_id)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCOPY, NULL, "failed to copy fcpl")
    if((file->fapl_id = H5Pcopy(fapl_id)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCOPY, NULL, "failed to copy fapl")

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(fa->comm, fa->info, &file->comm, &file->info))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed")

    /* Obtain the process rank and size from the communicator attached to the
     * fapl ID */
    MPI_Comm_rank(fa->comm, &file->my_rank);
    MPI_Comm_size(fa->comm, &file->num_procs);

    /* Determine if we requested collective object ops for the file */
    if(H5Pget_all_coll_metadata_ops(fapl_id, &file->collective) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't get collective access property")
 
    /* Create root group */
    if(NULL == (file->root_grp = (H5VL_rados_group_t *)H5VL_rados_group_create_helper(file, fcpl_id, H5P_GROUP_ACCESS_DEFAULT, dxpl_id, req, NULL, NULL, TRUE)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't create root group")

    /* Create root group oid */
    HDassert(H5VL_rados_oid_to_idx(file->root_grp->obj.bin_oid) == (uint64_t)1);

    ret_value = (void *)file;

done:
    /* Cleanup on failure */
    if(NULL == ret_value) {
        /* Close file */
        if(file && H5VL_rados_file_close_helper(file, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "can't close file")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_file_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_file_open
 *
 * Purpose:     Opens a file as a RADOS HDF5 file.
 *
 * Return:      Success:        the file id. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              February, 2018
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_rados_file_open(const char *name, unsigned flags, hid_t fapl_id,
    hid_t dxpl_id, void **req)
{
    H5VL_rados_fapl_t *fa = NULL;
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    H5VL_rados_file_t *file = NULL;
    char foi_buf_static[H5VL_RADOS_FOI_BUF_SIZE];
    char *foi_buf_dyn = NULL;
    char *foi_buf = foi_buf_static;
    void *gcpl_buf = NULL;
    uint64_t gcpl_len;
    uint64_t root_grp_oid;
    hbool_t must_bcast = FALSE;
    uint8_t *p;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get information from the FAPL */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if(NULL == (fa = (H5VL_rados_fapl_t *)H5P_get_vol_info(plist)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't get RADOS info struct")

    /* allocate the file object that is returned to the user */
    if(NULL == (file = H5FL_CALLOC(H5VL_rados_file_t)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate RADOS file struct")
    //file->glob_md_oh = DAOS_HDL_INVAL;
    file->root_grp = NULL;
    file->fcpl_id = FAIL;
    file->fapl_id = FAIL;

    /* Fill in fields of file we know */
    file->item.type = H5I_FILE;
    file->item.file = file;
    file->item.rc = 1;
    if(NULL == (file->file_name = HDstrdup(name)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't copy file name")
    file->file_name_len = HDstrlen(name);
    file->flags = flags;
    if((file->fapl_id = H5Pcopy(fapl_id)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCOPY, NULL, "failed to copy fapl")

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(fa->comm, fa->info, &file->comm, &file->info))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed")

    /* Obtain the process rank and size from the communicator attached to the
     * fapl ID */
    MPI_Comm_rank(fa->comm, &file->my_rank);
    MPI_Comm_size(fa->comm, &file->num_procs);

    /* Generate root group oid */
    H5VL_rados_oid_create_binary((uint64_t)1, H5VL_RADOS_TARG_TYPE_OBJ, H5I_GROUP, &root_grp_oid);

    /* Determine if we requested collective object ops for the file */
    if(H5Pget_all_coll_metadata_ops(fapl_id, &file->collective) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't get collective access property")

    if(file->my_rank == 0) {
        /* If there are other processes and we fail we must bcast anyways so they
         * don't hang */
        if(file->num_procs > 1)
            must_bcast = TRUE;

        /* Open root group */
        if(NULL == (file->root_grp = (H5VL_rados_group_t *)H5VL_rados_group_open_helper(file, root_grp_oid, H5P_GROUP_ACCESS_DEFAULT, dxpl_id, req, (file->num_procs > 1) ? &gcpl_buf : NULL, &gcpl_len)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't open root group")

        /* Bcast global handles if there are other processes */
        if(file->num_procs > 1) {
            /* Check if the file open info won't fit into the static buffer */
            if(gcpl_len + /*2 **/ sizeof(uint64_t) > sizeof(foi_buf_static)) {
                /* Allocate dynamic buffer */
                if(NULL == (foi_buf_dyn = (char *)H5MM_malloc(gcpl_len + sizeof(uint64_t))))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate space for global container handle")

                /* Use dynamic buffer */
                foi_buf = foi_buf_dyn;
            } /* end if */

            /* Encode GCPL length */
            p = (uint8_t *)foi_buf;
            UINT64ENCODE(p, gcpl_len)

            /* Encode max OID */
            //UINT64ENCODE(p, file->max_oid)

            /* Copy GCPL buffer */
            HDmemcpy(p, gcpl_buf, gcpl_len);

            /* We are about to bcast so we no longer need to bcast on failure */
            must_bcast = FALSE;

            /* MPI_Bcast foi_buf */
            if(MPI_SUCCESS != MPI_Bcast(foi_buf, (int)sizeof(foi_buf_static), MPI_BYTE, 0, fa->comm))
                HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global container handle")

            /* Need a second bcast if we had to allocate a dynamic buffer */
            if(foi_buf == foi_buf_dyn)
                if(MPI_SUCCESS != MPI_Bcast((char *)p, (int)(gcpl_len), MPI_BYTE, 0, fa->comm))
                    HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast file open info (second bcast)")
        } /* end if */
    } /* end if */
    else {
        /* Receive file open info */
        if(MPI_SUCCESS != MPI_Bcast(foi_buf, (int)sizeof(foi_buf_static), MPI_BYTE, 0, fa->comm))
            HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global container handle")

        /* Decode GCPL length */
        UINT64DECODE(p, gcpl_len)

        /* Check for gcpl_len set to 0 - indicates failure */
        if(gcpl_len == 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "lead process failed to open file")

        /* Decode max OID */
        //UINT64DECODE(p, file->max_oid)

        /* Check if we need to perform another bcast */
        if(gcpl_len + /*2 **/ sizeof(uint64_t) > sizeof(foi_buf_static)) {
            /* Check if we need to allocate a dynamic buffer */
            if(gcpl_len > sizeof(foi_buf_static)) {
                /* Allocate dynamic buffer */
                if(NULL == (foi_buf_dyn = (char *)H5MM_malloc(gcpl_len)))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate space for global pool handle")
                foi_buf = foi_buf_dyn;
            } /* end if */

            /* Receive info buffer */
            if(MPI_SUCCESS != MPI_Bcast(foi_buf_dyn, (int)(gcpl_len), MPI_BYTE, 0, fa->comm))
                HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global container handle (second bcast)")

            p = (uint8_t *)foi_buf;
        } /* end if */

        /* Reconstitute root group from revieved GCPL */
        if(NULL == (file->root_grp = (H5VL_rados_group_t *)H5VL_rados_group_reconstitute(file, root_grp_oid, p, H5P_GROUP_ACCESS_DEFAULT, dxpl_id, req)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't reconstitute root group")
    } /* end else */

    /* FCPL was stored as root group's GCPL (as GCPL is the parent of FCPL).
     * Point to it. */
    file->fcpl_id = file->root_grp->gcpl_id;
    if(H5Iinc_ref(file->fcpl_id) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, NULL, "can't increment FCPL ref count")

    ret_value = (void *)file;

done:
    /* Cleanup on failure */
    if(NULL == ret_value) {
        /* Bcast bcast_buf_64 as '0' if necessary - this will trigger failures
         * in the other processes so we do not need to do the second bcast. */
        if(must_bcast) {
            HDmemset(foi_buf_static, 0, sizeof(foi_buf_static));
            if(MPI_SUCCESS != MPI_Bcast(foi_buf_static, sizeof(foi_buf_static), MPI_BYTE, 0, fa->comm))
                HDONE_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")
        } /* end if */

        /* Close file */
        if(file && H5VL_rados_file_close_helper(file, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "can't close file")
    } /* end if */

    /* Clean up buffers */
    foi_buf_dyn = (char *)H5MM_xfree(foi_buf_dyn);
    gcpl_buf = H5MM_xfree(gcpl_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_file_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_file_close_helper
 *
 * Purpose:     Closes a RADOS HDF5 file.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              February, 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_rados_file_close_helper(H5VL_rados_file_t *file, hid_t dxpl_id, void **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);

    /* Free file data structures */
    if(file->file_name)
        HDfree(file->file_name);
    if(file->comm || file->info)
        if(H5FD_mpi_comm_info_free(&file->comm, &file->info) < 0)
            HDONE_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "Communicator/Info free failed")
    /* Note: Use of H5I_dec_app_ref is a hack, using H5I_dec_ref doesn't reduce
     * app reference count incremented by use of public API to create the ID,
     * while use of H5Idec_ref clears the error stack.  In general we can't use
     * public APIs in the "done" section or in close routines for this reason,
     * until we implement a separate error stack for the VOL plugin */
    if(file->fapl_id != FAIL && H5I_dec_app_ref(file->fapl_id) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist")
    if(file->fcpl_id != FAIL && H5I_dec_app_ref(file->fcpl_id) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist")
    if(file->root_grp)
        if(H5VL_rados_group_close(file->root_grp, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close root group")
    file = H5FL_FREE(H5VL_rados_file_t, file);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_file_close_helper() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_file_close
 *
 * Purpose:     Closes a RADOS  HDF5 file.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              February, 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_rados_file_close(void *_file, hid_t dxpl_id, void **req)
{
    H5VL_rados_file_t *file = (H5VL_rados_file_t *)_file;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);

    /* Flush the file */
    /*if(H5VL_rados_file_flush(file) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "can't flush file")*/

    /* Close the file */
    if(H5VL_rados_file_close_helper(file, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_file_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_link_read
 *
 * Purpose:     Reads the specified link from the given group.  Note that
 *              if the returned link is a soft link, val->target.soft must
 *              eventually be freed.
 *
 * Return:      Success:        SUCCEED 
 *              Failure:        FAIL
 *
 * Programmer:  Neil Fortner
 *              December, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_rados_link_read(H5VL_rados_group_t *grp, char *name, size_t name_len,
    H5VL_rados_link_val_t *val)
{
    rados_read_op_t read_op;
    hbool_t read_op_init = FALSE;
    rados_omap_iter_t iter;
    hbool_t iter_init = FALSE;
    char *key;
    char *omap_val;
    size_t val_len;
    char saved_end = name[name_len]; // Stop this, or copy path first RADOSINC
    uint8_t *p;
    int ret;
    int read_ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    name[name_len] = '\0';

    /* Create read op */
    if(NULL == (read_op = rados_create_read_op()))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create read operation")
    read_op_init = TRUE;

    /* Add operation to get link value */
    /* Add prefix RADOSINC */
    rados_read_op_omap_get_vals_by_keys(read_op, &name, 1, &iter, &read_ret);
    iter_init = TRUE;

    /* Execute read operation */
    if((ret = rados_read_op_operate(read_op, ioctx_g, grp->obj.oid, 0)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't perform read operation: %s", strerror(ret))
    if(read_ret < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't read link value: %s", strerror(read_ret))

    /* Get link value */
    if((ret = rados_omap_get_next(iter, &key, &omap_val, &val_len)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't get link value: %s", strerror(ret))

    /* Check for no link found */
    if(val_len == 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "link not found")

    /* Decode link type */
    p = (uint8_t *)omap_val;
    val->type = (H5L_type_t)*p++;

    /* Decode remainder of link value */
    switch(val->type) {
        case H5L_TYPE_HARD:
            /* Decode oid */
            UINT64DECODE(p, val->target.hard)

            break;

        case H5L_TYPE_SOFT:
            /* Allocate soft link buffer and copy string. */
            if(NULL == (val->target.soft = (char *)H5MM_malloc(val_len)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate link value buffer")
            HDmemcpy(val->target.soft, val + 1, val_len - 1);

            /* Add null terminator */
            val->target.soft[val_len - 1] = '\0';

            break;

        case H5L_TYPE_ERROR:
        case H5L_TYPE_EXTERNAL:
        case H5L_TYPE_MAX:
        default:
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "invalid or unsupported link type")
    } /* end switch */

done:
    if(iter_init)
        rados_omap_get_end(iter);
    if(read_op_init)
        rados_release_read_op(read_op);
    name[name_len] = saved_end;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_link_read() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_link_write
 *
 * Purpose:     Writes the specified link to the given group
 *
 * Return:      Success:        SUCCEED 
 *              Failure:        FAIL
 *
 * Programmer:  Neil Fortner
 *              February, 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_rados_link_write(H5VL_rados_group_t *grp, const char *name,
    H5VL_rados_link_val_t *val)
{
    rados_write_op_t write_op;
    hbool_t write_op_init = FALSE;
    size_t val_len;
    uint8_t *val_buf;
    uint8_t val_buf_static[H5VL_RADOS_LINK_VAL_BUF_SIZE];
    uint8_t *val_buf_dyn = NULL;
    uint8_t *p;
    int ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Check for write access */
    if(!(grp->obj.item.file->flags & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "no write intent on file")

    val_buf = val_buf_static;

    /* Encode type specific value information */
    switch(val->type) {
         case H5L_TYPE_HARD:
            HDassert(sizeof(val_buf_static) >= sizeof(val->target.hard) + 1);

            /* Encode link type */
            p = val_buf;
            *p++ = (uint8_t)val->type;

            /* Encode oid */
            UINT64ENCODE(p, val->target.hard)

            val_len = (size_t)9;

            break;

        case H5L_TYPE_SOFT:
            /* Allocate larger buffer for soft link if necessary */
            val_len = HDstrlen(val->target.soft) + 1;
            if(val_len > sizeof(val_buf_static)) {
                if(NULL == (val_buf_dyn = H5MM_malloc(val_len)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate link value buffer")
                val_buf = val_buf_dyn;
            } /* end if */

            /* Encode link type */
            p = val_buf;
            *p++ = (uint8_t)val->type;

            /* Copy link target */
            HDmemcpy(p, val->target.soft, val_len - 1);

            break;

        case H5L_TYPE_ERROR:
        case H5L_TYPE_EXTERNAL:
        case H5L_TYPE_MAX:
        default:
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "invalid or unsupported link type")
    } /* end switch */

    /* Create write op */
    if(NULL == (write_op = rados_create_write_op()))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create write operation")
    write_op_init = TRUE;

    /* Add operation to write link */
    /* Add prefix RADOSINC */
    rados_write_op_omap_set(write_op, &name, &val_buf, &val_len, 1);

    /* Execute write operation */
    if((ret = rados_write_op_operate(write_op, ioctx_g, grp->obj.oid, NULL, 0)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't perform write operation: %s", strerror(ret))

done:
    if(write_op_init)
        rados_release_write_op(write_op);
    H5MM_xfree(val_buf_dyn);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_link_write() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_link_follow
 *
 * Purpose:     Follows the link in grp identified with name, and returns
 *              in oid the oid of the target object.
 *
 * Return:      Success:        SUCCEED 
 *              Failure:        FAIL
 *
 * Programmer:  Neil Fortner
 *              February, 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_rados_link_follow(H5VL_rados_group_t *grp, char *name,
    size_t name_len, hid_t dxpl_id, void **req, uint64_t *oid)
{
    H5VL_rados_link_val_t link_val;
    hbool_t link_val_alloc = FALSE;
    H5VL_rados_group_t *target_grp = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(grp);
    HDassert(name);
    HDassert(oid);

    /* Read link to group */
   if(H5VL_rados_link_read(grp, name, name_len, &link_val) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't read link")

    switch(link_val.type) {
       case H5L_TYPE_HARD:
            /* Simply return the read oid */
            *oid = link_val.target.hard;

            break;

        case H5L_TYPE_SOFT:
            {
                const char *target_name = NULL;

                link_val_alloc = TRUE;

                /* Traverse the soft link path */
                if(NULL == (target_grp = H5VL_rados_group_traverse(&grp->obj.item, link_val.target.soft, dxpl_id, req, &target_name, NULL, NULL)))
                    HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "can't traverse path")

                /* Check for no target_name, in this case just return
                 * target_grp's oid */
                if(target_name[0] == '\0'
                        || (target_name[0] == '.' && target_name[1] == '\0'))
                    *oid = target_grp->obj.bin_oid;
                else
                    /* Follow the last element in the path */
                    if(H5VL_rados_link_follow(target_grp, target_name, HDstrlen(target_name), dxpl_id, req, oid) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't follow link")

                break;
            } /* end block */

        case H5L_TYPE_ERROR:
        case H5L_TYPE_EXTERNAL:
        case H5L_TYPE_MAX:
        default:
           HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "invalid or unsupported link type")
    } /* end switch */

done:
    /* Clean up */
    if(link_val_alloc) {
        HDassert(link_val.type == H5L_TYPE_SOFT);
        H5MM_free(link_val.target.soft);
    } /* end if */

    if(target_grp)
        if(H5VL_rados_group_close(target_grp, dxpl_id, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close group")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_link_follow() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_group_traverse
 *
 * Purpose:     Given a path name and base object, returns the final group
 *              in the path and the object name.  obj_name points into the
 *              buffer given by path, so it does not need to be freed.
 *              The group must be closed with H5VL_rados_group_close().
 *
 * Return:      Success:        group object. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              February, 2018
 *
 *-------------------------------------------------------------------------
 */
static H5VL_rados_group_t *
H5VL_rados_group_traverse(H5VL_rados_item_t *item, char *path,
    hid_t dxpl_id, void **req, const char **obj_name, void **gcpl_buf_out,
    uint64_t *gcpl_len_out)
{
    H5VL_rados_group_t *grp = NULL;
    const char *next_obj;
    uint64_t oid;
    H5VL_rados_group_t *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(item);
    HDassert(path);
    HDassert(obj_name);

    /* Initialize obj_name */
    *obj_name = path;

    /* Open starting group */
    if((*obj_name)[0] == '/') {
        grp = item->file->root_grp;
        (*obj_name)++;
    } /* end if */
    else {
        if(item->type == H5I_GROUP)
            grp = (H5VL_rados_group_t *)item;
        else if(item->type == H5I_FILE)
            grp = ((H5VL_rados_file_t *)item)->root_grp;
        else
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "item not a file or group")
    } /* end else */
        
    grp->obj.item.rc++;

    /* Search for '/' */
    next_obj = strchr(*obj_name, '/');

    /* Traverse path */
    while(next_obj) {
        /* Free gcpl_buf_out */
        if(gcpl_buf_out)
            *gcpl_buf_out = H5MM_xfree(*gcpl_buf_out);

        /* Follow link to next group in path */
        HDassert(next_obj > *obj_name);
        if(H5VL_rados_link_follow(grp, *obj_name, (size_t)(next_obj - *obj_name), dxpl_id, req, &oid) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't follow link to group")

        /* Close previous group */
        if(H5VL_rados_group_close(grp, dxpl_id, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")
        grp = NULL;

        /* Open group */
        if(NULL == (grp = (H5VL_rados_group_t *)H5VL_rados_group_open_helper(item->file, oid, H5P_GROUP_ACCESS_DEFAULT, dxpl_id, req, gcpl_buf_out, gcpl_len_out)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't open group")

        /* Advance to next path element */
        *obj_name = next_obj + 1;
        next_obj = strchr(*obj_name, '/');
    } /* end while */

    /* Set return value */
    ret_value = grp;

done:
    /* Cleanup on failure */
    if(NULL == ret_value)
        /* Close group */
        if(grp && H5VL_rados_group_close(grp, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CLOSEERROR, NULL, "can't close group")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_group_traverse() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_group_create_helper
 *
 * Purpose:     Performs the actual group creation.
 *
 * Return:      Success:        group object. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              January, 2018
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_rados_group_create_helper(H5VL_rados_file_t *file, hid_t gcpl_id,
    hid_t gapl_id, hid_t dxpl_id, void **req, H5VL_rados_group_t *parent_grp,
    const char *name, hbool_t collective)
{
    H5VL_rados_group_t *grp = NULL;
    void *gcpl_buf = NULL;
    int ret;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file->flags & H5F_ACC_RDWR);

    /* Allocate the group object that is returned to the user */
    if(NULL == (grp = H5FL_CALLOC(H5VL_rados_group_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate RADOS group struct")
    grp->obj.item.type = H5I_GROUP;
    grp->obj.item.file = file;
    grp->obj.item.rc = 1;
    grp->gcpl_id = FAIL;
    grp->gapl_id = FAIL;

    /* Generate group oid */
    if(H5VL_rados_oid_create(file, file->max_oid + (uint64_t)1, H5VL_RADOS_TARG_TYPE_OBJ, H5I_GROUP, &grp->obj.bin_oid, &grp->obj.oid) < 0)

    /* Update max_oid */
    file->max_oid = H5VL_rados_oid_to_idx(grp->obj.bin_oid);

    /* Create group and write metadata if this process should */
    if(!collective || (file->my_rank == 0)) {
        size_t gcpl_size = 0;

        /* Create group */
        /* Write max OID */
        /*if(H5VL_rados_write_max_oid(file) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't write max OID")*/

        /* Encode GCPL */
        if(H5Pencode(gcpl_id, NULL, &gcpl_size) < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "can't determine serialized length of gcpl")
        if(NULL == (gcpl_buf = H5MM_malloc(gcpl_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized gcpl")
        if(H5Pencode(gcpl_id, gcpl_buf, &gcpl_size) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, NULL, "can't serialize gcpl")

        /* Write internal metadata to group */
        if((ret = rados_write_full(ioctx_g, grp->obj.oid, gcpl_buf, gcpl_size)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't write metadata to group: %s", strerror(ret))

        /* Write link to group if requested */
        if(parent_grp) {
            H5VL_rados_link_val_t link_val;

            link_val.type = H5L_TYPE_HARD;
            link_val.target.hard = grp->obj.bin_oid;
            if(H5VL_rados_link_write(parent_grp, name, &link_val) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create link to group")
        } /* end if */
    } /* end if */

    /* Finish setting up group struct */
    if((grp->gcpl_id = H5Pcopy(gcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gcpl");
    if((grp->gapl_id = H5Pcopy(gapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gapl");

    ret_value = (void *)grp;

done:
    /* Cleanup on failure */
    if(NULL == ret_value)
        /* Close group */
        if(grp && H5VL_rados_group_close(grp, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CLOSEERROR, NULL, "can't close group")

    /* Free memory */
    gcpl_buf = H5MM_xfree(gcpl_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_group_create_helper() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_group_create
 *
 * Purpose:     Sends a request to RADOS to create a group
 *
 * Return:      Success:        group object. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              February, 2018
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_rados_group_create(void *_item,
    H5VL_loc_params_t H5_ATTR_UNUSED loc_params, const char *name,
    hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_rados_item_t *item = (H5VL_rados_item_t *)_item;
    H5VL_rados_group_t *grp = NULL;
    H5VL_rados_group_t *target_grp = NULL;
    const char *target_name = NULL;
    hbool_t collective = item->file->collective;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Check for write access */
    if(!(item->file->flags & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "no write intent on file")
 
    /* Check for collective access, if not already set by the file */
    if(!collective)
        if(H5Pget_all_coll_metadata_ops(gapl_id, &collective) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't get collective access property")

    /* Traverse the path */
    if(!collective || (item->file->my_rank == 0))
        if(NULL == (target_grp = H5VL_rados_group_traverse(item, name, dxpl_id, req, &target_name, NULL, NULL)))
            HGOTO_ERROR(H5E_SYM, H5E_BADITER, NULL, "can't traverse path")

    /* Create group and link to group */
    if(NULL == (grp = (H5VL_rados_group_t *)H5VL_rados_group_create_helper(item->file, gcpl_id, gapl_id, dxpl_id, req, target_grp, target_name, collective)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create group")

    /* Set return value */
    ret_value = (void *)grp;

done:
    /* Close target group */
    if(target_grp && H5VL_rados_group_close(target_grp, dxpl_id, req) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")

    /* Cleanup on failure */
    if(NULL == ret_value)
        /* Close group */
        if(grp && H5VL_rados_group_close(grp, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_group_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_group_open_helper
 *
 * Purpose:     Performs the actual group open, given the oid.
 *
 * Return:      Success:        group object. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              February, 2018
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_rados_group_open_helper(H5VL_rados_file_t *file, uint64_t oid,
    hid_t gapl_id, hid_t dxpl_id, void **req, void **gcpl_buf_out,
    uint64_t *gcpl_len_out)
{
    H5VL_rados_group_t *grp = NULL;
    void *gcpl_buf = NULL;
    uint64_t gcpl_len;
    time_t pmtime;
    int ret;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Allocate the group object that is returned to the user */
    if(NULL == (grp = H5FL_CALLOC(H5VL_rados_group_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate RADOS group struct")
    grp->obj.item.type = H5I_GROUP;
    grp->obj.item.file = file;
    grp->obj.item.rc = 1;
    grp->obj.bin_oid = oid;
    if(H5VL_rados_oid_create_string(file, oid, &grp->obj.oid) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't encode string oid")
    grp->gcpl_id = FAIL;
    grp->gapl_id = FAIL;

    /* Read internal metadata size from group */
    if((ret = rados_stat(ioctx_g, grp->obj.oid, &gcpl_len, &pmtime)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, NULL, "can't read metadata size from group: %s", strerror(ret))

    /* Check for metadata not found */
    if(gcpl_len == (uint64_t)0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "internal metadata not found")

    /* Allocate buffer for GCPL */
    if(NULL == (gcpl_buf = H5MM_malloc(gcpl_len)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized gcpl")

    /* Read internal metadata from group */
    if((ret = rados_read(ioctx_g, grp->obj.oid, gcpl_buf, gcpl_len, 0)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, NULL, "can't read metadata from group: %s", strerror(ret))

    /* Decode GCPL */
    if((grp->gcpl_id = H5Pdecode(gcpl_buf)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTDECODE, NULL, "can't deserialize GCPL")

    /* Finish setting up group struct */
    if((grp->gapl_id = H5Pcopy(gapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gapl");

    /* Return GCPL info if requested, relinquish ownership of gcpl_buf if so */
    if(gcpl_buf_out) {
        HDassert(gcpl_len_out);
        HDassert(!*gcpl_buf_out);

        *gcpl_buf_out = gcpl_buf;
        gcpl_buf = NULL;

        *gcpl_len_out = gcpl_len;
    } /* end if */

    ret_value = (void *)grp;

done:
    /* Cleanup on failure */
    if(NULL == ret_value)
        /* Close group */
        if(grp && H5VL_rados_group_close(grp, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")

    /* Free memory */
    gcpl_buf = H5MM_xfree(gcpl_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_group_open_helper() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_group_reconstitute
 *
 * Purpose:     Reconstitutes a group object opened by another process.
 *
 * Return:      Success:        group object. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              February, 2018
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_rados_group_reconstitute(H5VL_rados_file_t *file, uint64_t oid,
    uint8_t *gcpl_buf, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_rados_group_t *grp = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Allocate the group object that is returned to the user */
    if(NULL == (grp = H5FL_CALLOC(H5VL_rados_group_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate RADOS group struct")
    grp->obj.item.type = H5I_GROUP;
    grp->obj.item.file = file;
    grp->obj.item.rc = 1;
    grp->obj.bin_oid = oid;
    if(H5VL_rados_oid_create_string(file, oid, &grp->obj.oid) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't encode string oid")
    grp->gcpl_id = FAIL;
    grp->gapl_id = FAIL;

    /* Decode GCPL */
    if((grp->gcpl_id = H5Pdecode(gcpl_buf)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTDECODE, NULL, "can't deserialize GCPL")

    /* Finish setting up group struct */
    if((grp->gapl_id = H5Pcopy(gapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gapl");

    ret_value = (void *)grp;

done:
    /* Cleanup on failure */
    if(NULL == ret_value)
        /* Close group */
        if(grp && H5VL_rados_group_close(grp, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_group_reconstitute() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_group_open
 *
 * Purpose:     Sends a request to RADOS to open a group
 *
 * Return:      Success:        dataset object. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              February, 2018
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_rados_group_open(void *_item, H5VL_loc_params_t loc_params,
    const char *name, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_rados_item_t *item = (H5VL_rados_item_t *)_item;
    H5VL_rados_group_t *grp = NULL;
    H5VL_rados_group_t *target_grp = NULL;
    const char *target_name = NULL;
    uint64_t oid;
    uint8_t *gcpl_buf = NULL;
    uint64_t gcpl_len = 0;
    uint8_t ginfo_buf_static[H5VL_RADOS_GINFO_BUF_SIZE];
    uint8_t *p;
    hbool_t collective = item->file->collective;
    hbool_t must_bcast = FALSE;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT
 
    /* Check for collective access, if not already set by the file */
    if(!collective)
        if(H5Pget_all_coll_metadata_ops(gapl_id, &collective) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't get collective access property")

    /* Check if we're actually opening the group or just receiving the group
     * info from the leader */
    if(!collective || (item->file->my_rank == 0)) {
        if(collective && (item->file->num_procs > 1))
            must_bcast = TRUE;

        /* Check for open by address */
        if(H5VL_OBJECT_BY_ADDR == loc_params.type) {
            /* Generate oid from address */
            oid = (uint64_t)loc_params.loc_data.loc_by_addr.addr;

            /* Open group */
            if(NULL == (grp = (H5VL_rados_group_t *)H5VL_rados_group_open_helper(item->file, oid, gapl_id, dxpl_id, req, (collective && (item->file->num_procs > 1)) ? (void **)&gcpl_buf : NULL, &gcpl_len)))
                HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "can't open group")
        } /* end if */
        else {
            /* Open using name parameter */
            /* Traverse the path */
            if(NULL == (target_grp = H5VL_rados_group_traverse(item, name, dxpl_id, req, &target_name, (collective && (item->file->num_procs > 1)) ? (void **)&gcpl_buf : NULL, &gcpl_len)))
                HGOTO_ERROR(H5E_SYM, H5E_BADITER, NULL, "can't traverse path")

            /* Check for no target_name, in this case just return target_grp */
            if(target_name[0] == '\0'
                    || (target_name[0] == '.' && target_name[1] == '\0')) {
                size_t gcpl_size;

                /* Take ownership of target_grp */
                grp = target_grp;
                target_grp = NULL;

                /* Encode GCPL */
                if(H5Pencode(grp->gcpl_id, NULL, &gcpl_size) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "can't determine serialized length of gcpl")
                if(NULL == (gcpl_buf = (uint8_t *)H5MM_malloc(gcpl_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized gcpl")
                gcpl_len = (uint64_t)gcpl_size;
                if(H5Pencode(grp->gcpl_id, gcpl_buf, &gcpl_size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, NULL, "can't serialize gcpl")
            } /* end if */
            else {
                gcpl_buf = (uint8_t *)H5MM_xfree(gcpl_buf);
                gcpl_len = 0;

                /* Follow link to group */
                if(H5VL_rados_link_follow(target_grp, target_name, HDstrlen(target_name), dxpl_id, req, &oid) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't follow link to group")

                /* Open group */
                if(NULL == (grp = (H5VL_rados_group_t *)H5VL_rados_group_open_helper(item->file, oid, gapl_id, dxpl_id, req, (collective && (item->file->num_procs > 1)) ? (void **)&gcpl_buf : NULL, &gcpl_len)))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "can't open group")
            } /* end else */
        } /* end else */

        /* Broadcast group info if there are other processes that need it */
        if(collective && (item->file->num_procs > 1)) {
            HDassert(gcpl_buf);
            HDassert(sizeof(ginfo_buf_static) >= 2 * sizeof(uint64_t));

            /* Encode oid */
            p = ginfo_buf_static;
            UINT64ENCODE(p, grp->obj.bin_oid)

            /* Encode GCPL length */
            UINT64ENCODE(p, gcpl_len)

            /* Copy GCPL to ginfo_buf_static if it will fit */
            if((gcpl_len + 2 * sizeof(uint64_t)) <= sizeof(ginfo_buf_static))
                (void)HDmemcpy(p, gcpl_buf, gcpl_len);

            /* We are about to bcast so we no longer need to bcast on failure */
            must_bcast = FALSE;

            /* MPI_Bcast ginfo_buf */
            if(MPI_SUCCESS != MPI_Bcast((char *)ginfo_buf_static, sizeof(ginfo_buf_static), MPI_BYTE, 0, item->file->comm))
                HGOTO_ERROR(H5E_SYM, H5E_MPI, NULL, "can't bcast group info")

            /* Need a second bcast if it did not fit in the receivers' static
             * buffer */
            if(gcpl_len + 2 * sizeof(uint64_t) > sizeof(ginfo_buf_static))
                if(MPI_SUCCESS != MPI_Bcast((char *)gcpl_buf, (int)gcpl_len, MPI_BYTE, 0, item->file->comm))
                    HGOTO_ERROR(H5E_SYM, H5E_MPI, NULL, "can't bcast GCPL")
        } /* end if */
    } /* end if */
    else {
        /* Receive GCPL */
        if(MPI_SUCCESS != MPI_Bcast((char *)ginfo_buf_static, sizeof(ginfo_buf_static), MPI_BYTE, 0, item->file->comm))
            HGOTO_ERROR(H5E_SYM, H5E_MPI, NULL, "can't bcast group info")

        /* Decode oid */
        p = ginfo_buf_static;
        UINT64DECODE(p, oid)

        /* Decode GCPL length */
        UINT64DECODE(p, gcpl_len)

        /* Check for gcpl_len set to 0 - indicates failure */
        if(gcpl_len == 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "lead process failed to open group")

        /* Check if we need to perform another bcast */
        if(gcpl_len + 2 * sizeof(uint64_t) > sizeof(ginfo_buf_static)) {
            /* Allocate a dynamic buffer if necessary */
            if(gcpl_len > sizeof(ginfo_buf_static)) {
                if(NULL == (gcpl_buf = (uint8_t *)H5MM_malloc(gcpl_len)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate space for global pool handle")
                p = gcpl_buf;
            } /* end if */
            else
                p = ginfo_buf_static;

            /* Receive GCPL */
            if(MPI_SUCCESS != MPI_Bcast((char *)p, (int)gcpl_len, MPI_BYTE, 0, item->file->comm))
                HGOTO_ERROR(H5E_SYM, H5E_MPI, NULL, "can't bcast GCPL")
        } /* end if */

        /* Reconstitute group from received oid and GCPL buffer */
        if(NULL == (grp = (H5VL_rados_group_t *)H5VL_rados_group_reconstitute(item->file, oid, p, gapl_id, dxpl_id, req)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't reconstitute group")
    } /* end else */

    /* Set return value */
    ret_value = (void *)grp;

done:
    /* Cleanup on failure */
    if(NULL == ret_value) {
        /* Bcast gcpl_buf as '0' if necessary - this will trigger failures in
         * other processes so we do not need to do the second bcast. */
        if(must_bcast) {
            HDmemset(ginfo_buf_static, 0, sizeof(ginfo_buf_static));
            if(MPI_SUCCESS != MPI_Bcast(ginfo_buf_static, sizeof(ginfo_buf_static), MPI_BYTE, 0, item->file->comm))
                HDONE_ERROR(H5E_SYM, H5E_MPI, NULL, "can't bcast empty group info")
        } /* end if */

        /* Close group */
        if(grp && H5VL_rados_group_close(grp, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")
    } /* end if */

    /* Close target group */
    if(target_grp && H5VL_rados_group_close(target_grp, dxpl_id, req) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")

    /* Free memory */
    gcpl_buf = (uint8_t *)H5MM_xfree(gcpl_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_group_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_rados_group_close
 *
 * Purpose:     Closes a RADOS HDF5 group.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              February, 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_rados_group_close(void *_grp, hid_t H5_ATTR_UNUSED dxpl_id,
    void H5_ATTR_UNUSED **req)
{
    H5VL_rados_group_t *grp = (H5VL_rados_group_t *)_grp;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(grp);

    if(--grp->obj.item.rc == 0) {
        /* Free group data structures */
        grp->obj.oid = H5MM_xfree(grp->obj.oid);
        if(grp->gcpl_id != FAIL && H5I_dec_app_ref(grp->gcpl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist")
        if(grp->gapl_id != FAIL && H5I_dec_app_ref(grp->gapl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist")
        grp = H5FL_FREE(H5VL_rados_group_t, grp);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_rados_group_close() */

