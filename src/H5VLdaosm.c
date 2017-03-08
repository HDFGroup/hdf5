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
 *              September, 2016
 *
 * Purpose:	The DAOS-M VOL plugin where access is forwarded to the DAOS-M
 * library 
 */

#include "H5private.h"          /* Generic Functions                    */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5Fprivate.h"         /* Files                                */
#include "H5FDprivate.h"        /* File drivers                         */
#include "H5FFprivate.h"        /* Fast Forward                         */
#include "H5Iprivate.h"         /* IDs                                  */
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5Pprivate.h"         /* Property lists                       */
#include "H5Sprivate.h"         /* Dataspaces                           */
#include "H5TRprivate.h"        /* Transactions                         */
#include "H5VLprivate.h"        /* VOL plugins                          */
#include "H5VLdaosm.h"          /* DAOS-M plugin                        */

hid_t H5VL_DAOSM_g = 0;

/*
 * Macros
 */
/* Constant Keys */
#define H5VL_DAOSM_INT_MD_KEY "Internal Metadata"
#define H5VL_DAOSM_MAX_OID_KEY "Max OID"
#define H5VL_DAOSM_CPL_KEY "Creation Property List"
#define H5VL_DAOSM_LINK_KEY "Link"
#define H5VL_DAOSM_TYPE_KEY "Datatype"
#define H5VL_DAOSM_SPACE_KEY "Dataspace"
#define H5VL_DAOSM_ATTR_KEY "Attribute"
#define H5VL_DAOSM_CHUNK_KEY 0u

/* Stack allocation sizes */
#define H5VL_DAOSM_LINK_VAL_BUF_SIZE 256
#define H5VL_DAOSM_SEQ_LIST_LEN 128

/* DAOSM-specific file access properties */
typedef struct H5VL_daosm_fapl_t {
    MPI_Comm            comm;           /*communicator                  */
    MPI_Info            info;           /*file information              */
    uuid_t              pool_uuid;      /*pool uuid                     */
    char                *pool_grp;      /*pool group                    */
} H5VL_daosm_fapl_t;

/* Enum to indicate if the supplied read buffer can be used as a type conversion
 * or background buffer */
typedef enum {
    H5VL_DAOSM_TCONV_REUSE_NONE,    /* Cannot reuse buffer */
    H5VL_DAOSM_TCONV_REUSE_TCONV,   /* Use buffer as type conversion buffer */
    H5VL_DAOSM_TCONV_REUSE_BKG      /* Use buffer as background buffer */
} H5VL_daosm_tconv_reuse_t;

/* Prototypes */
static void *H5VL_daosm_fapl_copy(const void *_old_fa);
static herr_t H5VL_daosm_fapl_free(void *_fa);

/* File callbacks */
static void *H5VL_daosm_file_create(const char *name, unsigned flags,
    hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id, void **req);
static void *H5VL_daosm_file_open(const char *name, unsigned flags,
    hid_t fapl_id, hid_t dxpl_id, void **req);
//static herr_t H5VL_iod_file_get(void *file, H5VL_file_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_daosm_file_specific(void *_item,
    H5VL_file_specific_t specific_type, hid_t dxpl_id, void **req,
    va_list arguments);
static herr_t H5VL_daosm_file_close(void *_file, hid_t dxpl_id, void **req);

/* Link callbacks */
static herr_t H5VL_daosm_link_create(H5VL_link_create_type_t create_type,
    void *_item, H5VL_loc_params_t loc_params, hid_t lcpl_id, hid_t lapl_id,
    hid_t dxpl_id, void **req);
static herr_t H5VL_daosm_link_specific(void *_item,
    H5VL_loc_params_t loc_params, H5VL_link_specific_t specific_type,
    hid_t dxpl_id, void **req, va_list arguments);

/* Group callbacks */
static void *H5VL_daosm_group_create(void *_item, H5VL_loc_params_t loc_params,
    const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req);
static void *H5VL_daosm_group_open(void *_item, H5VL_loc_params_t loc_params,
    const char *name, hid_t gapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_daosm_group_close(void *_grp, hid_t dxpl_id, void **req);

/* Dataset callbacks */
static void *H5VL_daosm_dataset_create(void *_item,
    H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id,
    hid_t dapl_id, hid_t dxpl_id, void **req);
static void *H5VL_daosm_dataset_open(void *_item, H5VL_loc_params_t loc_params,
    const char *name, hid_t dapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_daosm_dataset_read(void *_dset, hid_t mem_type_id,
    hid_t mem_space_id, hid_t file_space_id, hid_t dxpl_id, void *buf,
    void **req);
static herr_t H5VL_daosm_dataset_write(void *_dset, hid_t mem_type_id,
    hid_t mem_space_id, hid_t file_space_id, hid_t dxpl_id, const void *buf,
    void **req);
/*static herr_t H5VL_daosm_dataset_specific(void *_dset, H5VL_dataset_specific_t specific_type,
                                        hid_t dxpl_id, void **req, va_list arguments);*/
static herr_t H5VL_daosm_dataset_get(void *_dset, H5VL_dataset_get_t get_type,
    hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_daosm_dataset_close(void *_dset, hid_t dxpl_id, void **req);

/* Datatype callbacks */
static herr_t H5VL_daosm_datatype_close(void *_dtype, hid_t dxpl_id,
    void **req);

/* Attribute callbacks */
static void *H5VL_daosm_attribute_create(void *_obj,
    H5VL_loc_params_t loc_params, const char *name, hid_t acpl_id,
    hid_t aapl_id, hid_t dxpl_id, void **req);
static void *H5VL_daosm_attribute_open(void *_obj, H5VL_loc_params_t loc_params,
    const char *name, hid_t aapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_daosm_attribute_read(void *_attr, hid_t mem_type_id,
    void *buf, hid_t dxpl_id, void **req);
static herr_t H5VL_daosm_attribute_write(void *_attr, hid_t mem_type_id,
    const void *buf, hid_t dxpl_id, void **req);
static herr_t H5VL_daosm_attribute_close(void *_attr, hid_t dxpl_id,
    void **req);

/* Helper routines */
static herr_t H5VL_daosm_write_max_oid(H5VL_daosm_file_t *file);
static herr_t H5VL_daosm_file_flush(H5VL_daosm_file_t *file);
static herr_t H5VL_daosm_file_close_helper(H5VL_daosm_file_t *file,
    hid_t dxpl_id, void **req);
static herr_t H5VL_daosm_link_read(H5VL_daosm_group_t *grp, const char *name,
    size_t name_len, H5VL_daosm_link_val_t *val);
static herr_t H5VL_daosm_link_write(H5VL_daosm_group_t *grp, const char *name,
    size_t name_len, H5VL_daosm_link_val_t *val);
static herr_t H5VL_daosm_link_follow(H5VL_daosm_group_t *grp, const char *name,
    size_t name_len, hid_t dxpl_id, void **req, daos_obj_id_t *oid);
static H5VL_daosm_group_t *H5VL_daosm_group_traverse(H5VL_daosm_item_t *item,
    const char *path, hid_t dxpl_id, void **req, const char **obj_name);
static void *H5VL_daosm_group_create_helper(H5VL_daosm_file_t *file,
    hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req);
static void *H5VL_daosm_group_open_helper(H5VL_daosm_file_t *file,
    daos_obj_id_t oid, hid_t gapl_id, hid_t dxpl_id, void **req);
static htri_t H5VL_daosm_need_bkg(hid_t src_type_id, hid_t dst_type_id,
    hbool_t *fill_bkg);
static herr_t H5VL_daosm_tconv_init(hid_t src_type_id, size_t *src_type_size,
    hid_t dst_type_id, size_t *dst_type_size, size_t num_elem, void **tconv_buf,
    void **bkg_buf, H5VL_daosm_tconv_reuse_t *reuse, hbool_t *fill_bkg);
static herr_t H5VL_daosm_sel_to_recx_iov(H5S_t *space, size_t type_size,
    void *buf, daos_recx_t **recxs, daos_iov_t **sg_iovs, size_t *list_nused);
static herr_t H5VL_daosm_object_close(void *_obj, hid_t dxpl_id, void **req);

/* Free list definitions */
H5FL_DEFINE(H5VL_daosm_file_t);
H5FL_DEFINE(H5VL_daosm_group_t);
H5FL_DEFINE(H5VL_daosm_dset_t);
H5FL_DEFINE(H5VL_daosm_attr_t);

/* The DAOS-M VOL plugin struct */
static H5VL_class_t H5VL_daosm_g = {
    HDF5_VOL_DAOSM_VERSION_1,                   /* Version number */
    H5_VOL_DAOSM,                               /* Plugin value */
    "daos_m",                                   /* name */
    NULL,                                       /* initialize */
    NULL,                                       /* terminate */
    sizeof(H5VL_daosm_fapl_t),                  /*fapl_size */
    H5VL_daosm_fapl_copy,                       /*fapl_copy */
    H5VL_daosm_fapl_free,                       /*fapl_free */
    {                                           /* attribute_cls */
        H5VL_daosm_attribute_create,            /* create */
        H5VL_daosm_attribute_open,              /* open */
        H5VL_daosm_attribute_read,              /* read */
        H5VL_daosm_attribute_write,             /* write */
        NULL,//H5VL_iod_attribute_get,                 /* get */
        NULL,//H5VL_iod_attribute_specific,            /* specific */
        NULL,                                   /* optional */
        H5VL_daosm_attribute_close              /* close */
    },
    {                                           /* dataset_cls */
        H5VL_daosm_dataset_create,              /* create */
        H5VL_daosm_dataset_open,                /* open */
        H5VL_daosm_dataset_read,                /* read */
        H5VL_daosm_dataset_write,               /* write */
        H5VL_daosm_dataset_get,                 /* get */
        NULL,//H5VL_iod_dataset_specific,              /* specific */
        NULL,                                   /* optional */
        H5VL_daosm_dataset_close                /* close */
    },
    {                                           /* datatype_cls */
        NULL,//H5VL_iod_datatype_commit,               /* commit */
        NULL,//H5VL_iod_datatype_open,                 /* open */
        NULL,//H5VL_iod_datatype_get,                  /* get */
        NULL,                                   /* specific */
        NULL,                                   /* optional */
        NULL,//H5VL_iod_datatype_close                 /* close */
    },
    {                                           /* file_cls */
        H5VL_daosm_file_create,                 /* create */
        H5VL_daosm_file_open,                     /* open */
        NULL,//H5VL_iod_file_get,                      /* get */
        H5VL_daosm_file_specific,               /* specific */
        NULL,                                   /* optional */
        H5VL_daosm_file_close                     /* close */
    },
    {                                           /* group_cls */
        H5VL_daosm_group_create,                /* create */
        H5VL_daosm_group_open,                  /* open */
        NULL,//H5VL_iod_group_get,                     /* get */
        NULL,                                   /* specific */
        NULL,                                   /* optional */
        H5VL_daosm_group_close                  /* close */
    },
    {                                           /* link_cls */
        H5VL_daosm_link_create,                 /* create */
        NULL,//H5VL_iod_link_copy,                     /* copy */
        NULL,//H5VL_iod_link_move,                     /* move */
        NULL,//H5VL_iod_link_get,                      /* get */
        H5VL_daosm_link_specific,               /* specific */
        NULL                                    /* optional */
    },
    {                                           /* object_cls */
        NULL,//H5VL_iod_object_open,                   /* open */
        NULL,                                   /* copy */
        NULL,                                   /* get */
        NULL,//H5VL_iod_object_specific,               /* specific */
        NULL,//H5VL_iod_object_optional                /* optional */
    },
    {
        NULL,//H5VL_iod_cancel,
        NULL,//H5VL_iod_test,
        NULL,//H5VL_iod_wait
    },
    NULL
};

#if 0

/*--------------------------------------------------------------------------
NAME
   H5VL__init_package -- Initialize interface-specific information
USAGE
    herr_t H5VL__init_package()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5VL_daosm_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5VL__init_package(void)
{
    herr_t ret_value = SUCCEED; 

    FUNC_ENTER_STATIC

    if(H5VL_daosm_init() < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTINIT, FAIL, "unable to initialize FF DAOS-M VOL plugin")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5VL__init_package() */
#endif


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_init
 *
 * Purpose:     Initialize this vol plugin by registering the driver with the
 *              library.
 *
 * Return:      Success:        The ID for the DAOS-M plugin.
 *              Failure:        Negative.
 *
 * Programmer:  Neil Fortner
 *              October, 2016
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5VL_daosm_init(void)
{
    hid_t ret_value = H5I_INVALID_HID;            /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Register the DAOS-M VOL, if it isn't already */
    if(NULL == H5I_object_verify(H5VL_DAOSM_g, H5I_VOL)) {
        if((H5VL_DAOSM_g = H5VL_register((const H5VL_class_t *)&H5VL_daosm_g, 
                                          sizeof(H5VL_class_t), TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTINSERT, FAIL, "can't create ID for DAOS-M plugin")
    }

    /* Set return value */
    ret_value = H5VL_DAOSM_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_init() */


/*---------------------------------------------------------------------------
 * Function:    H5VL__daosm_term
 *
 * Purpose:     Shut down the DAOS-M VOL
 *
 * Returns:     SUCCEED (Can't fail)
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5VL__daosm_term(void)
{
    FUNC_ENTER_STATIC_NOERR

    /* Reset VOL ID */
    H5VL_DAOSM_g = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__daosm_term() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_fapl_daosm
 *
 * Purpose:     Modify the file access property list to use the H5VL_DAOSM
 *              plugin defined in this source file.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              October, 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_daosm(hid_t fapl_id, MPI_Comm comm, MPI_Info info, uuid_t pool_uuid, char *pool_grp)
{
    H5VL_daosm_fapl_t fa;
    H5P_genplist_t  *plist;      /* Property list pointer */
    herr_t          ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iMcMi", fapl_id, comm, info);

    if(fapl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    if(MPI_COMM_NULL == comm)
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a valid communicator")

    /* Initialize driver specific properties */
    fa.comm = comm;
    fa.info = info;
    HDmemcpy(fa.pool_uuid, pool_uuid, sizeof(fa.pool_uuid));
    if(pool_grp) {
        if(NULL == (fa.pool_grp = HDstrdup(pool_grp)))
            HGOTO_ERROR(H5E_PLIST, H5E_NOSPACE, FAIL, "can't copy pool group")
    } /* end if */
    else
        fa.pool_grp = NULL;

    ret_value = H5P_set_vol(plist, H5VL_DAOSM, &fa);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_daosm() */


/*-------------------------------------------------------------------------
 * Function:    H5VLdaosm_snap_create
 *
 * Purpose:     Creates a snapshot and returns the snapshot id.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              January, 2017
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VLdaosm_snap_create(hid_t loc_id, H5VL_daosm_snap_id_t *snap_id)
{
    H5VL_object_t   *obj = NULL;    /* object token of loc_id */
    H5VL_daosm_file_t *file;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    /* get the location object */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

    /* Make sure object's VOL is this one */
    if(obj->vol_info->vol_id != H5VL_DAOSM)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "location does not use DAOS-M VOL plugin")

    /* Get file object */
    file = ((H5VL_daosm_item_t *)obj->vol_obj)->file;

    /* Check for write access */
    if(!(file->flags & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "no write intent on file")

    /* Tell the file to save a snapshot next time it is flushed (committed) */
    file->snap_epoch = (int)TRUE;

    /* Return epoch in snap_id */
    *snap_id = (uint64_t)file->epoch;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5VLdaosm_snap_create() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_daosm_snap_open
 *
 * Purpose:     Modify the file access property list to use the H5VL_DAOSM
 *              plugin defined in this source file.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              October, 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_daosm_snap_open(hid_t fapl_id, H5VL_daosm_snap_id_t snap_id)
{
    H5P_genplist_t  *plist;      /* Property list pointer */
    herr_t          ret_value;

    FUNC_ENTER_API(FAIL)

    if(fapl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    /* Set the property */
    if(H5P_set(plist, H5VL_DAOSM_SNAP_OPEN_ID, &snap_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set property value for snap id")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_daosm_snap_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_fapl_copy
 *
 * Purpose:     Copies the daosm-specific file access properties.
 *
 * Return:      Success:        Ptr to a new property list
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              October, 2016
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_daosm_fapl_copy(const void *_old_fa)
{
    const H5VL_daosm_fapl_t *old_fa = (const H5VL_daosm_fapl_t*)_old_fa;
    H5VL_daosm_fapl_t     *new_fa = NULL;
    void                  *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (new_fa = (H5VL_daosm_fapl_t *)H5MM_malloc(sizeof(H5VL_daosm_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy the general information */
    HDmemcpy(new_fa, old_fa, sizeof(H5VL_daosm_fapl_t));

    /* Clear allocated fields, so they aren't freed if something goes wrong.  No
     * need to clear info since it is only freed if comm is not null. */
    new_fa->comm = MPI_COMM_NULL;
    new_fa->pool_grp = NULL;

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(old_fa->comm, old_fa->info, &new_fa->comm, &new_fa->info))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed")

    /*  Duplicate the pool group */
    if(old_fa->pool_grp) {
        if(NULL == (new_fa->pool_grp = HDstrdup(old_fa->pool_grp)))
            HGOTO_ERROR(H5E_PLIST, H5E_NOSPACE, NULL, "can't copy pool group")
    } /* end if */
    else
        new_fa->pool_grp = NULL;

    ret_value = new_fa;

done:
    if (NULL == ret_value) {
        /* cleanup */
        if(new_fa && H5VL_daosm_fapl_free(new_fa) < 0)
            HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, NULL, "can't free fapl")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_fapl_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_fapl_free
 *
 * Purpose:     Frees the daosm-specific file access properties.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 * Programmer:  Neil Fortner
 *              October, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_fapl_free(void *_fa)
{
    herr_t              ret_value = SUCCEED;
    H5VL_daosm_fapl_t   *fa = (H5VL_daosm_fapl_t*)_fa;

    FUNC_ENTER_NOAPI_NOINIT

    assert(fa);

    /* Free the internal communicator and INFO object */
    if(fa->comm != MPI_COMM_NULL)
        if(H5FD_mpi_comm_info_free(&fa->comm, &fa->info) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "Communicator/Info free failed")

    /* Free the pool group */
    HDfree(fa->pool_grp);

    /* free the struct */
    H5MM_xfree(fa);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_fapl_free() */


/* Multiply two 128 bit unsigned integers to yield a 128 bit unsigned integer */
static void
H5VL_daosm_mult128(uint64_t x_lo, uint64_t x_hi, uint64_t y_lo, uint64_t y_hi,
    uint64_t *ans_lo, uint64_t *ans_hi)
{
    uint64_t xlyl;
    uint64_t xlyh;
    uint64_t xhyl;
    uint64_t xhyh;
    uint64_t temp;

    /*
     * First calculate x_lo * y_lo
     */
    /* Compute 64 bit results of multiplication of each combination of high and
     * low 32 bit sections of x_lo and y_lo */
    xlyl = (x_lo & 0xffffffff) * (y_lo & 0xffffffff);
    xlyh = (x_lo & 0xffffffff) * (y_lo >> 32);
    xhyl = (x_lo >> 32) * (y_lo & 0xffffffff);
    xhyh = (x_lo >> 32) * (y_lo >> 32);

    /* Calculate lower 32 bits of the answer */
    *ans_lo = xlyl & 0xffffffff;

    /* Calculate second 32 bits of the answer. Use temp to keep a 64 bit result
     * of the calculation for these 32 bits, to keep track of overflow past
     * these 32 bits. */
    temp = (xlyl >> 32) + (xlyh & 0xffffffff) + (xhyl & 0xffffffff);
    *ans_lo += temp << 32;

    /* Calculate third 32 bits of the answer, including overflowed result from
     * the previous operation */
    temp >>= 32;
    temp += (xlyh >> 32) + (xhyl >> 32) + (xhyh & 0xffffffff);
    *ans_hi = temp & 0xffffffff;

    /* Calculate highest 32 bits of the answer. No need to keep track of
     * overflow because it has overflowed past the end of the 128 bit answer */
    temp >>= 32;
    temp += (xhyh >> 32);
    *ans_hi += temp << 32;

    /*
     * Now add the results from multiplying x_lo * y_hi and x_hi * y_lo. No need
     * to consider overflow here, and no need to consider x_hi * y_hi because
     * those results would overflow past the end of the 128 bit answer.
     */
    *ans_hi += (x_lo * y_hi) + (x_hi * y_lo);

    return;
} /* end H5VL_daosm_mult128() */


/* Implementation of the FNV hash algorithm */
static void
H5VL_daosm_hash128(const char *name, void *hash)
{
    const uint8_t *name_p = (const uint8_t *)name;
    uint8_t *hash_p = (uint8_t *)hash;
    uint64_t name_lo;
    uint64_t name_hi;
    /* Initialize hash value in accordance with the FNV algorithm */
    uint64_t hash_lo = 0x62b821756295c58d;
    uint64_t hash_hi = 0x6c62272e07bb0142;
    /* Initialize FNV prime number in accordance with the FNV algorithm */
    const uint64_t fnv_prime_lo = 0x13b;
    const uint64_t fnv_prime_hi = 0x1000000;
    size_t name_len_rem = HDstrlen(name);

    while(name_len_rem > 0) {
        /* "Decode" lower 64 bits of this 128 bit section of the name, so the
         * numberical value of the integer is the same on both little endian and
         * big endian systems */
        if(name_len_rem >= 8) {
            UINT64DECODE(name_p, name_lo)
            name_len_rem -= 8;
        } /* end if */
        else {
            name_lo = 0;
            UINT64DECODE_VAR(name_p, name_lo, name_len_rem)
            name_len_rem = 0;
        } /* end else */

        /* "Decode" second 64 bits */
        if(name_len_rem > 0) {
            if(name_len_rem >= 8) {
                UINT64DECODE(name_p, name_hi)
                name_len_rem -= 8;
            } /* end if */
            else {
                name_hi = 0;
                UINT64DECODE_VAR(name_p, name_hi, name_len_rem)
                name_len_rem = 0;
            } /* end else */
        } /* end if */
        else
            name_hi = 0;

        /* FNV algorithm - XOR hash with name then multiply by fnv_prime */
        hash_lo ^= name_lo;
        hash_hi ^= name_hi;
        H5VL_daosm_mult128(hash_lo, hash_hi, fnv_prime_lo, fnv_prime_hi, &hash_lo, &hash_hi);
    } /* end while */

    /* "Encode" hash integers to char buffer, so the buffer is the same on both
     * little endian and big endian systems */
    UINT64ENCODE(hash_p, hash_lo)
    UINT64ENCODE(hash_p, hash_hi)

    return;
} /* end H5VL_daosm_hash128() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_file_create
 *
 * Purpose:     Creates a file as a daos-m HDF5 file.
 *
 * Return:      Success:        the file id. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              September, 2016
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_daosm_file_create(const char *name, unsigned flags, hid_t fcpl_id,
    hid_t fapl_id, hid_t dxpl_id, void **req)
{
    H5VL_daosm_fapl_t *fa = NULL;
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    H5VL_daosm_file_t *file = NULL;
    daos_iov_t glob;
    uint64_t bcast_buf_64[6];
    char *gh_buf = NULL;
    daos_obj_id_t gmd_oid = {0, 0, 0};
    hbool_t must_bcast = FALSE;
    int ret;
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
    if(NULL == (fa = (H5VL_daosm_fapl_t *)H5P_get_vol_info(plist)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't get DAOS-M info struct")

    /* allocate the file object that is returned to the user */
    if(NULL == (file = H5FL_CALLOC(H5VL_daosm_file_t)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate DAOS-M file struct")
    file->glob_md_oh = DAOS_HDL_INVAL;
    file->root_grp = NULL;
    file->fcpl_id = FAIL;
    file->fapl_id = FAIL;

    /* Fill in fields of file we know */
    file->item.type = H5I_FILE;
    file->item.file = file;
    file->item.rc = 1;
    file->snap_epoch = (int)FALSE;
    if(NULL == (file->file_name = HDstrdup(name)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't copy file name")
    file->flags = flags;
    file->max_oid = 0;
    file->max_oid_dirty = FALSE;
    if((file->fcpl_id = H5Pcopy(fcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fcpl")
    if((file->fapl_id = H5Pcopy(fapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fapl")

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(fa->comm, fa->info, &file->comm, &file->info))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed")

    /* Obtain the process rank and size from the communicator attached to the
     * fapl ID */
    MPI_Comm_rank(fa->comm, &file->my_rank);
    MPI_Comm_size(fa->comm, &file->num_procs);

    /* Hash file name to create uuid */
    H5VL_daosm_hash128(name, &file->uuid);

    /* Generate oid for global metadata object */
    daos_obj_id_generate(&gmd_oid, DAOS_OC_TINY_RW);

    if(file->my_rank == 0) {
        daos_epoch_state_t epoch_state;

        /* If there are other processes and we fail we must bcast anyways so they
         * don't hang */
        if(file->num_procs > 1)
            must_bcast = TRUE;

        /* Connect to the pool */
        /* TODO: move pool handling to startup/shutdown routines DSMINC */
        if(0 != (ret = daos_pool_connect(fa->pool_uuid, fa->pool_grp, NULL /*pool_svc*/, DAOS_PC_RW, &file->poh, NULL /*&file->pool_info*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't connect to pool: %d", ret)

        /* Delete the container if H5F_ACC_TRUNC is set.  This shouldn't cause a
         * problem even if the container doesn't exist. */
        /* Need to handle EXCL correctly DSMINC */
        if(flags & H5F_ACC_TRUNC)
            if(0 != (ret = daos_cont_destroy(file->poh, file->uuid, 1, NULL /*event*/)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, NULL, "can't destroy container: %d", ret)

        /* Create the container for the file */
        if(0 != (ret = daos_cont_create(file->poh, file->uuid, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, NULL, "can't create container: %d", ret)

        /* Open the container */
        if(0 != (ret = daos_cont_open(file->poh, file->uuid, DAOS_COO_RW, &file->coh, NULL /*&file->co_info*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't open container: %d", ret)

        /* Query the epoch */
        if(0 != (ret = daos_epoch_query(file->coh, &epoch_state, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't query epoch: %d", ret)

        /* Hold the epoch */
        file->epoch = epoch_state.es_hce + (daos_epoch_t)1;
        if(0 != (ret = daos_epoch_hold(file->coh, &file->epoch, NULL /*state*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't hold epoch: %d", ret)

        /* Create global metadata object */
        if(0 != (ret = daos_obj_declare(file->coh, gmd_oid, file->epoch, NULL /*oa*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, NULL, "can't create global metadata object: %d", ret)

        /* Open global metadata object */
        if(0 != (ret = daos_obj_open(file->coh, gmd_oid, file->epoch, DAOS_OO_RW, &file->glob_md_oh, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't open global metadata object: %d", ret)

        /* Create root group */
        if(NULL == (file->root_grp = (H5VL_daosm_group_t *)H5VL_daosm_group_create_helper(file, fcpl_id, H5P_GROUP_ACCESS_DEFAULT, dxpl_id, req)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't create root group")

        /* Write root group OID DSMINC */

        /* Bcast global handles if there are other processes */
        if(file->num_procs > 1) {
            /* Calculate sizes of global pool and container handles */
            glob.iov_buf = NULL;
            glob.iov_buf_len = 0;
            glob.iov_len = 0;
            if(0 != (ret = daos_pool_local2global(file->poh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global pool handle size: %d", ret)
            bcast_buf_64[0] = (uint64_t)glob.iov_buf_len;
            glob.iov_buf = NULL;
            glob.iov_buf_len = 0;
            glob.iov_len = 0;
            if(0 != (ret = daos_cont_local2global(file->coh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global container handle size: %d", ret)
            bcast_buf_64[1] = (uint64_t)glob.iov_buf_len;

            /* Add root group oid to bcast_buf_64 */
            bcast_buf_64[2] = file->root_grp->obj.oid.lo;
            bcast_buf_64[3] = file->root_grp->obj.oid.mid;
            bcast_buf_64[4] = file->root_grp->obj.oid.hi;

            /* Add epoch to bcast_buf_64 */
            HDassert(sizeof(bcast_buf_64[5]) == sizeof(file->epoch));
            bcast_buf_64[5] = (uint64_t)file->epoch;

            /* Retrieve global pool and container handles */
            if(NULL == (gh_buf = (char *)H5MM_malloc(bcast_buf_64[0] + bcast_buf_64[1])))
                HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate space for global handles")
            glob.iov_buf = gh_buf;
            glob.iov_buf_len = bcast_buf_64[0];
            glob.iov_len = 0;
            if(0 != (ret = daos_pool_local2global(file->poh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global pool handle: %d", ret)
            HDassert(glob.iov_len == glob.iov_buf_len);
            glob.iov_buf = gh_buf + bcast_buf_64[0];
            glob.iov_buf_len = bcast_buf_64[1];
            glob.iov_len = 0;
            if(0 != (ret = daos_cont_local2global(file->coh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global container handle: %d", ret)
            HDassert(glob.iov_len == glob.iov_buf_len);

            /* We are about to bcast so we no longer need to bcast on failure */
            must_bcast = FALSE;

            /* MPI_Bcast bcast_buf_64 */
            if(MPI_SUCCESS != MPI_Bcast(bcast_buf_64, sizeof(bcast_buf_64)/sizeof(bcast_buf_64[0]), MPI_UINT64_T, 0, fa->comm))
                HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")

            /* MPI_Bcast gh_buf */
            if(MPI_SUCCESS != MPI_Bcast(gh_buf, (int)(bcast_buf_64[0] + bcast_buf_64[1]), MPI_BYTE, 0, fa->comm))
                HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")
        } /* end if */
    } /* end if */
    else {
        daos_obj_id_t root_grp_oid;

        /* Receive bcast_buf_64 */
        if(MPI_SUCCESS != MPI_Bcast(bcast_buf_64, sizeof(bcast_buf_64)/sizeof(bcast_buf_64[0]), MPI_UINT64_T, 0, fa->comm))
            HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")

        /* Check for bcast_buf_64[0] set to 0 - indicates failure */
        if(bcast_buf_64[0] == 0) {
            HDassert(bcast_buf_64[1] == 0);
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "lead process failed to open file")
        } /* end if */

        /* Retrieve root group oid from bcast_buf_64 */
        root_grp_oid.lo = bcast_buf_64[2];
        root_grp_oid.mid = bcast_buf_64[3];
        root_grp_oid.hi = bcast_buf_64[4];

        /* Retrieve epoch from bcast_buf_64 */
        HDassert(sizeof(bcast_buf_64[5]) == sizeof(file->epoch));
        file->epoch = (daos_epoch_t)bcast_buf_64[5];

        /* Allocate global handle buffer */
        if(NULL == (gh_buf = (char *)H5MM_malloc(bcast_buf_64[0] + bcast_buf_64[1])))
            HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate space for global handles")

        /* Receive gh_buf */
        if(MPI_SUCCESS != MPI_Bcast(gh_buf, (int)(bcast_buf_64[0] + bcast_buf_64[1]), MPI_BYTE, 0, fa->comm))
            HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")

        /* Create local pool and container handles */
        glob.iov_buf = gh_buf;
        glob.iov_buf_len = bcast_buf_64[0];
        glob.iov_len = bcast_buf_64[0];
        if(0 != (ret = daos_pool_global2local(glob, &file->poh)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't get local pool handle: %d", ret)
        glob.iov_buf = gh_buf + bcast_buf_64[0];
        glob.iov_buf_len = bcast_buf_64[1];
        glob.iov_len = bcast_buf_64[1];
        if(0 != (ret = daos_cont_global2local(file->poh, glob, &file->coh)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't get local container handle: %d", ret)

        /* Open root group */
        if(NULL == (file->root_grp = (H5VL_daosm_group_t *)H5VL_daosm_group_open_helper(file, root_grp_oid, H5P_GROUP_ACCESS_DEFAULT, dxpl_id, req)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't open root group")

        /* Handle pool_info and container_info DSMINC */

        /* Open global metadata object */
        if(0 != (ret = daos_obj_open(file->coh, gmd_oid, file->epoch, DAOS_OO_RW, &file->glob_md_oh, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't open global metadata object: %d", ret)
    } /* end else */

    ret_value = (void *)file;

done:
    /* Clean up */
    H5MM_xfree(gh_buf);

    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    if(NULL == ret_value) {
        /* Bcast bcast_buf_64 as '0' if necessary - this will trigger failures
         * in the other processes so we do not need to do the second bcast. */
        if(must_bcast) {
            HDmemset(bcast_buf_64, 0, sizeof(bcast_buf_64));
            if(MPI_SUCCESS != MPI_Bcast(bcast_buf_64, sizeof(bcast_buf_64)/sizeof(bcast_buf_64[0]), MPI_UINT64_T, 0, fa->comm))
                HDONE_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")
        } /* end if */

        /* Close file */
        if(file && H5VL_daosm_file_close_helper(file, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "can't close file")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_file_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_file_open
 *
 * Purpose:     Opens a file as a daos-m HDF5 file.
 *
 * Return:      Success:        the file id. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              October, 2016
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_daosm_file_open(const char *name, unsigned flags, hid_t fapl_id,
    hid_t dxpl_id, void **req)
{
    H5VL_daosm_fapl_t *fa = NULL;
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    H5VL_daosm_file_t *file = NULL;
    H5VL_daosm_snap_id_t snap_id;
    daos_iov_t glob;
    uint64_t bcast_buf_64[7];
    char *gh_buf = NULL;
    daos_obj_id_t gmd_oid = {0, 0, 0};
    daos_obj_id_t root_grp_oid;
    hbool_t must_bcast = FALSE;
    int ret;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get information from the FAPL */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if(NULL == (fa = (H5VL_daosm_fapl_t *)H5P_get_vol_info(plist)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't get DAOS-M info struct")
    if(H5P_get(plist, H5VL_DAOSM_SNAP_OPEN_ID, &snap_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for snap id")

    /* Check for opening a snapshot with write access (disallowed) */
    if((snap_id != H5VL_DAOSM_SNAP_ID_INVAL) && (flags & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "write access requested to snapshot - disallowed")

    /* allocate the file object that is returned to the user */
    if(NULL == (file = H5FL_CALLOC(H5VL_daosm_file_t)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate DAOS-M file struct")
    file->glob_md_oh = DAOS_HDL_INVAL;
    file->root_grp = NULL;
    file->fcpl_id = FAIL;
    file->fapl_id = FAIL;

    /* Fill in fields of file we know */
    file->item.type = H5I_FILE;
    file->item.file = file;
    file->item.rc = 1;
    file->snap_epoch = (int)FALSE;
    if(NULL == (file->file_name = HDstrdup(name)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't copy file name")
    file->flags = flags;
    if((file->fapl_id = H5Pcopy(fapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fapl")

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(fa->comm, fa->info, &file->comm, &file->info))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed")

    /* Obtain the process rank and size from the communicator attached to the
     * fapl ID */
    MPI_Comm_rank(fa->comm, &file->my_rank);
    MPI_Comm_size(fa->comm, &file->num_procs);

    /* Hash file name to create uuid */
    H5VL_daosm_hash128(name, &file->uuid);

    /* Generate oid for global metadata object */
    daos_obj_id_generate(&gmd_oid, DAOS_OC_TINY_RW);

    if(file->my_rank == 0) {
        daos_epoch_t epoch;
        daos_epoch_t held_epoch;
        daos_epoch_state_t epoch_state;
        daos_key_t dkey;
        daos_vec_iod_t iod;
        daos_recx_t recx;
        daos_sg_list_t sgl;
        daos_iov_t sg_iov;
        char int_md_key[] = H5VL_DAOSM_INT_MD_KEY;
        char max_oid_key[] = H5VL_DAOSM_MAX_OID_KEY;

        /* If there are other processes and we fail we must bcast anyways so they
         * don't hang */
        if(file->num_procs > 1)
            must_bcast = TRUE;

        /* Connect to the pool */
        if(0 != (ret = daos_pool_connect(fa->pool_uuid, fa->pool_grp, NULL /*pool_svc*/, DAOS_PC_RW, &file->poh, NULL /*&file->pool_info*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't connect to pool: %d", ret)

        /* Open the container */
        if(0 != (ret = daos_cont_open(file->poh, file->uuid, DAOS_COO_RW, &file->coh, NULL /*&file->co_info*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't open container: %d", ret)

        /* If a snapshot was requested, use it as the epoch, otherwise query it
         */
        if(snap_id != H5VL_DAOSM_SNAP_ID_INVAL) {
            epoch = (daos_epoch_t)snap_id;

            HDassert(!(flags & H5F_ACC_RDWR));
        } /* end if */
        else {
            /* Query the epoch */
            if(0 != (ret = daos_epoch_query(file->coh, &epoch_state, NULL /*event*/)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't query epoch: %d", ret)
            epoch = epoch_state.es_hce;

            /* Hold the epoch if write access is requested */
            if(flags & H5F_ACC_RDWR) {
                /* Hold the next epoch */
                held_epoch = epoch + (daos_epoch_t)1;
                if(0 != (ret = daos_epoch_hold(file->coh, &held_epoch, NULL /*state*/, NULL /*event*/)))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't hold epoch: %d", ret)
            } /* end if */
        } /* end else */

        /* Generate root group ID */
        root_grp_oid.lo = 1; //DSMINC
        root_grp_oid.mid = 0; //DSMINC
        root_grp_oid.hi = 0; //DSMINC
        daos_obj_id_generate(&root_grp_oid, DAOS_OC_TINY_RW); //DSMINC

        /* Open global metadata object */
        if(0 != (ret = daos_obj_open(file->coh, gmd_oid, epoch, DAOS_OO_RW, &file->glob_md_oh, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't open global metadata object: %d", ret)

        /* Read max OID from gmd obj */
        /* Set up dkey */
        daos_iov_set(&dkey, int_md_key, (daos_size_t)(sizeof(int_md_key) - 1));

        /* Set up recx */
        recx.rx_rsize = (uint64_t)8;
        recx.rx_idx = (uint64_t)0;
        recx.rx_nr = (uint64_t)1;

        /* Set up iod */
        HDmemset(&iod, 0, sizeof(iod));
        daos_iov_set(&iod.vd_name, (void *)max_oid_key, (daos_size_t)(sizeof(max_oid_key) - 1));
        daos_csum_set(&iod.vd_kcsum, NULL, 0);
        iod.vd_nr = 1u;
        iod.vd_recxs = &recx;

        /* Set up sgl */
        daos_iov_set(&sg_iov, &file->max_oid, (daos_size_t)8);
        sgl.sg_nr.num = 1;
        sgl.sg_iovs = &sg_iov;

        /* Read max OID from gmd obj */
        if(0 != (ret = daos_obj_fetch(file->glob_md_oh, epoch, &dkey, 1, &iod, &sgl, NULL /*maps*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTDECODE, NULL, "can't read max OID from global metadata object: %d", ret)

        /* Set file's epoch */
        if(flags & H5F_ACC_RDWR)
            file->epoch = held_epoch;
        else
            file->epoch = epoch;

        /* Bcast global handles if there are other processes */
        if(file->num_procs > 1) {
            /* Calculate sizes of global pool and container handles */
            glob.iov_buf = NULL;
            glob.iov_buf_len = 0;
            glob.iov_len = 0;
            if(0 != (ret = daos_pool_local2global(file->poh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global pool handle size: %d", ret)
            bcast_buf_64[0] = (uint64_t)glob.iov_buf_len;
            glob.iov_buf = NULL;
            glob.iov_buf_len = 0;
            glob.iov_len = 0;
            if(0 != (ret = daos_cont_local2global(file->coh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global container handle size: %d", ret)
            bcast_buf_64[1] = (uint64_t)glob.iov_buf_len;

            /* Add root group oid to bcast_buf_64 */
            bcast_buf_64[2] = root_grp_oid.lo;
            bcast_buf_64[3] = root_grp_oid.mid;
            bcast_buf_64[4] = root_grp_oid.hi;

            /* Add epoch to bcast_buf_64 */
            HDassert(sizeof(bcast_buf_64[5]) == sizeof(file->epoch));
            bcast_buf_64[5] = (uint64_t)file->epoch;

            /* Add max OID to bcast_buf_64 */
            HDassert(sizeof(bcast_buf_64[6]) == sizeof(file->max_oid));
            bcast_buf_64[6] = file->max_oid;

            /* Retrieve global pool and container handles */
            if(NULL == (gh_buf = (char *)malloc(bcast_buf_64[0] + bcast_buf_64[1])))
                HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, bcast_buf_64, "can't allocate space for global handles")
            glob.iov_buf = gh_buf;
            glob.iov_buf_len = bcast_buf_64[0];
            glob.iov_len = 0;
            if(0 != (ret = daos_pool_local2global(file->poh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global pool handle: %d", ret)
            HDassert(glob.iov_len == glob.iov_buf_len);
            glob.iov_buf = gh_buf + bcast_buf_64[0];
            glob.iov_buf_len = bcast_buf_64[1];
            glob.iov_len = 0;
            if(0 != (ret = daos_cont_local2global(file->coh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global container handle: %d", ret)
            HDassert(glob.iov_len == glob.iov_buf_len);

            /* We are about to bcast so we no longer need to bcast on failure */
            must_bcast = FALSE;

            /* MPI_Bcast bcast_buf_64 */
            if(MPI_SUCCESS != MPI_Bcast(bcast_buf_64, sizeof(bcast_buf_64)/sizeof(bcast_buf_64[0]), MPI_UINT64_T, 0, fa->comm))
                HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")

            /* MPI_Bcast gh_buf */
            if(MPI_SUCCESS != MPI_Bcast(gh_buf, (int)(bcast_buf_64[0] + bcast_buf_64[1]), MPI_BYTE, 0, fa->comm))
                HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handles")
        } /* end if */
    } /* end if */
    else {
        /* Receive bcast_buf_64 */
        if(MPI_SUCCESS != MPI_Bcast(bcast_buf_64, sizeof(bcast_buf_64)/sizeof(bcast_buf_64[0]), MPI_UINT64_T, 0, fa->comm))
            HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")

        /* Check for bcast_buf_64[0] set to 0 - indicates failure */
        if(bcast_buf_64[0] == 0) {
            HDassert(bcast_buf_64[1] == 0);
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "lead process failed to open file")
        } /* end if */

        /* Retrieve root group oid from bcast_buf_64 */
        root_grp_oid.lo = bcast_buf_64[2];
        root_grp_oid.mid = bcast_buf_64[3];
        root_grp_oid.hi = bcast_buf_64[4];

        /* Retrieve epoch from bcast_buf_64 */
        HDassert(sizeof(bcast_buf_64[5]) == sizeof(file->epoch));
        file->epoch = (daos_epoch_t)bcast_buf_64[5];

        /* Retrieve max OID from bcast_buf_64 */
        HDassert(sizeof(bcast_buf_64[6]) == sizeof(file->max_oid));
        file->max_oid = bcast_buf_64[6];

        /* Allocate global handle buffer */
        if(NULL == (gh_buf = (char *)malloc(bcast_buf_64[0] + bcast_buf_64[1])))
            HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate space for global handles")

        /* Receive gh_buf */
        if(MPI_SUCCESS != MPI_Bcast(gh_buf, (int)(bcast_buf_64[0] + bcast_buf_64[1]), MPI_BYTE, 0, fa->comm))
            HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handles")

        /* Create local pool and container handles */
        glob.iov_buf = gh_buf;
        glob.iov_buf_len = bcast_buf_64[0];
        glob.iov_len = bcast_buf_64[0];
        if(0 != (ret = daos_pool_global2local(glob, &file->poh)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't get local pool handle: %d", ret)
        glob.iov_buf = gh_buf + bcast_buf_64[0];
        glob.iov_buf_len = bcast_buf_64[1];
        glob.iov_len = bcast_buf_64[1];
        if(0 != (ret = daos_cont_global2local(file->poh, glob, &file->coh)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't get local container handle: %d", ret)

        /* Handle pool_info and container_info DSMINC */

        /* Open global metadata object */
        if(0 != (ret = daos_obj_open(file->coh, gmd_oid, file->epoch, DAOS_OO_RW, &file->glob_md_oh, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't open global metadata object: %d", ret)
    } /* end else */

    /* Open root group */
    /* Only have leader open directly and bcast metadata to followers DSMINC */
    if(NULL == (file->root_grp = (H5VL_daosm_group_t *)H5VL_daosm_group_open_helper(file, root_grp_oid, H5P_GROUP_ACCESS_DEFAULT, dxpl_id, req)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't open root group")

    /* FCPL was stored as root group's GCPL (as GCPL is the parent of FCPL).
     * Point to it. */
    file->fcpl_id = file->root_grp->gcpl_id;
    if(H5Iinc_ref(file->fcpl_id) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, NULL, "can't increment FCPL ref count")

    ret_value = (void *)file;

done:
    /* Clean up buffer */
    H5MM_xfree(gh_buf);

    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    if(NULL == ret_value) {
        /* Bcast bcast_buf_64 as '0' if necessary - this will trigger failures
         * in the other processes so we do not need to do the second bcast. */
        if(must_bcast) {
            HDmemset(bcast_buf_64, 0, sizeof(bcast_buf_64));
            if(MPI_SUCCESS != MPI_Bcast(bcast_buf_64, sizeof(bcast_buf_64)/sizeof(bcast_buf_64[0]), MPI_UINT64_T, 0, fa->comm))
                HDONE_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")
        } /* end if */

        /* Close file */
        if(file && H5VL_daosm_file_close_helper(file, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "can't close file")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_file_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_file_flush
 *
 * Purpose:     Flushes a DAOS-M file.  Performs an MPI_Barrier, then
 *              commits the epoch, dereferences previous epochs, saves a
 *              snapshot if requested, and incrementes the epoch.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              January, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_file_flush(H5VL_daosm_file_t *file)
{
    int ret;
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Nothing to do if no write intent */
    if(!(file->flags & H5F_ACC_RDWR))
        HGOTO_DONE(SUCCEED)

    /* Collectively determine if anyone requested a snapshot of the epoch */
    if(MPI_SUCCESS != MPI_Reduce(file->my_rank == 0 ? MPI_IN_PLACE : &file->snap_epoch, &file->snap_epoch, 1, MPI_INT, MPI_LOR, 0, file->comm))
        HGOTO_ERROR(H5E_FILE, H5E_MPI, FAIL, "failed to determine whether to take snapshot (MPI_Reduce)")

    /* Barrier on all ranks so we don't commit before all ranks are
     * finished writing. H5Fflush must be called collectively. */
    if(MPI_SUCCESS != MPI_Barrier(file->comm))
        HGOTO_ERROR(H5E_FILE, H5E_MPI, FAIL, "MPI_Barrier failed")

    /* Commit the epoch */
    if(file->my_rank == 0) {
        /* Commit the epoch */
        if(0 != (ret = daos_epoch_commit(file->coh, file->epoch, NULL /*state*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "failed to commit epoch: %d", ret)

        /* Save a snapshot of this epoch if requested */
        /* Disabled until snapshots are supported in DAOS DSMINC */
#if 0
        if(file->snap_epoch)
            if(0 != (ret = daos_snap_create(file->coh, file->epoch, NULL /*event*/)))
                HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "can't create snapshot: %d", ret)
#endif

        /* Slip the epoch, indicating we don't need to reference
         * anything prior */
        /* Disabled until snapshots are supported in DAOS DSMINC */
#if 0
        if(0 != (ret = daos_epoch_slip(file->coh, file->epoch, NULL /*state*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "failed to slip epoch: %d", ret)
#endif
    } /* end if */

    /* Advance the epoch */
    file->epoch++;

    /* Reset snap_epoch */
    file->snap_epoch = (int)FALSE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_file_flush() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_file_specific
 *
 * Purpose:     Perform an operation
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              January, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_file_specific(void *item, H5VL_file_specific_t specific_type,
    hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req,
    va_list H5_ATTR_UNUSED arguments)
{
    H5VL_daosm_file_t *file = ((H5VL_daosm_item_t *)item)->file;
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (specific_type) {
        /* H5Fflush` */
        case H5VL_FILE_FLUSH:
            if(H5VL_daosm_file_flush(file) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "can't flush file")

            break;
        /* H5Fmount */
        case H5VL_FILE_MOUNT:
        /* H5Fmount */
        case H5VL_FILE_UNMOUNT:
        /* H5Fis_accessible */
        case H5VL_FILE_IS_ACCESSIBLE:
        default:
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "invalid or unsupported specific operation")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_file_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_file_close_helper
 *
 * Purpose:     Closes a daos-m HDF5 file.
 *
 * Return:      Success:        the file id. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              January, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_file_close_helper(H5VL_daosm_file_t *file, hid_t dxpl_id, void **req)
{
    daos_handle_t hdl_inval = DAOS_HDL_INVAL;
    int ret;
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
    if(HDmemcmp(&file->glob_md_oh, &hdl_inval, sizeof(hdl_inval)))
        if(0 != (ret = daos_obj_close(file->glob_md_oh, NULL /*event*/)))
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close global metadata object: %d", ret)
    if(file->root_grp)
        if(H5VL_daosm_group_close(file->root_grp, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close root group")
    if(HDmemcmp(&file->coh, &hdl_inval, sizeof(hdl_inval)))
        if(0 != (ret = daos_cont_close(file->coh, NULL /*event*/)))
            HDONE_ERROR(H5E_FILE, H5E_CLOSEERROR, FAIL, "can't close container: %d", ret)
    if(HDmemcmp(&file->poh, &hdl_inval, sizeof(hdl_inval)))
        if(0 != (ret = daos_pool_disconnect(file->poh, NULL /*event*/)))
            HDONE_ERROR(H5E_FILE, H5E_CLOSEERROR, FAIL, "can't disconnect from pool: %d", ret)
    file = H5FL_FREE(H5VL_daosm_file_t, file);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_file_close_helper() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_file_close
 *
 * Purpose:     Closes a daos-m HDF5 file, committing the epoch if
 *              appropriate.
 *
 * Return:      Success:        the file id. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              October, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_file_close(void *_file, hid_t dxpl_id, void **req)
{
    H5VL_daosm_file_t *file = (H5VL_daosm_file_t *)_file;
#if 0 /* DSMINC */
    int ret;
#endif
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);

    /* Flush the file (barrier, commit epoch, slip epoch) */
    if(H5VL_daosm_file_flush(file) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "can't flush file")

#if 0 /* DSMINC */
    /* Flush the epoch */
    if(0 != (ret = daos_epoch_flush(file->coh, epoch, NULL /*state*/, NULL /*event*/)))
        HDONE_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "can't flush epoch: %d", ret)
#endif

    /* Close the file */
    if(H5VL_daosm_file_close_helper(file, dxpl_id, req) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_file_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_write_max_oid
 *
 * Purpose:     Writes the max OID to the global metadata object
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 * Programmer:  Neil Fortner
 *              December, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_write_max_oid(H5VL_daosm_file_t *file)
{
    daos_key_t dkey;
    daos_vec_iod_t iod;
    daos_recx_t recx;
    daos_sg_list_t sgl;
    daos_iov_t sg_iov;
    char int_md_key[] = H5VL_DAOSM_INT_MD_KEY;
    char max_oid_key[] = H5VL_DAOSM_MAX_OID_KEY;
    int ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Set up dkey */
    daos_iov_set(&dkey, int_md_key, (daos_size_t)(sizeof(int_md_key) - 1));

    /* Set up recx */
    recx.rx_rsize = (uint64_t)8;
    recx.rx_idx = (uint64_t)0;
    recx.rx_nr = (uint64_t)1;

    /* Set up iod */
    HDmemset(&iod, 0, sizeof(iod));
    daos_iov_set(&iod.vd_name, (void *)max_oid_key, (daos_size_t)(sizeof(max_oid_key) - 1));
    daos_csum_set(&iod.vd_kcsum, NULL, 0);
    iod.vd_nr = 1u;
    iod.vd_recxs = &recx;

    /* Set up sgl */
    daos_iov_set(&sg_iov, &file->max_oid, (daos_size_t)8);
    sgl.sg_nr.num = 1;
    sgl.sg_iovs = &sg_iov;

    /* Write max OID to gmd obj */
    if(0 != (ret = daos_obj_update(file->glob_md_oh, file->epoch, &dkey, 1, &iod, &sgl, NULL /*event*/)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTENCODE, FAIL, "can't write max OID to global metadata object: %d", ret)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_write_max_oid() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_link_read
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
H5VL_daosm_link_read(H5VL_daosm_group_t *grp, const char *name, size_t name_len,
    H5VL_daosm_link_val_t *val)
{
    char const_link_key[] = H5VL_DAOSM_LINK_KEY;
    daos_key_t dkey;
    daos_vec_iod_t iod;
    daos_recx_t recx;
    daos_sg_list_t sgl;
    daos_iov_t sg_iov;
    uint8_t *val_buf;
    uint8_t val_buf_static[H5VL_DAOSM_LINK_VAL_BUF_SIZE];
    uint8_t *val_buf_dyn = NULL;
    uint8_t *p;
    int ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT
 
    /* Use static link value buffer initially */
    val_buf = val_buf_static;

    /* Set up dkey */
    /* For now always use dkey = const, akey = name. Add option to switch these
     * DSMINC */
    daos_iov_set(&dkey, const_link_key, (daos_size_t)(sizeof(const_link_key) - 1));

    /* Set up recx */
    recx.rx_rsize = DAOS_REC_ANY;
    recx.rx_idx = (uint64_t)0;
    recx.rx_nr = (uint64_t)1;

    /* Set up iod */
    HDmemset(&iod, 0, sizeof(iod));
    daos_iov_set(&iod.vd_name, (void *)name, (daos_size_t)name_len);
    daos_csum_set(&iod.vd_kcsum, NULL, 0);
    iod.vd_nr = 1u;
    iod.vd_recxs = &recx;

    /* Set up sgl */
    daos_iov_set(&sg_iov, val_buf, (daos_size_t)H5VL_DAOSM_LINK_VAL_BUF_SIZE);
    sgl.sg_nr.num = 1;
    sgl.sg_iovs = &sg_iov;

    /* Read link */
    if(0 != (ret = daos_obj_fetch(grp->obj.obj_oh, grp->obj.item.file->epoch, &dkey, 1, &iod, &sgl, NULL /*maps*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't read link: %d", ret)

    /* Check for no link found */
    if(recx.rx_rsize == (uint64_t)0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "link not found")

    /* Check if val_buf was large enough */
    if(recx.rx_rsize > (uint64_t)H5VL_DAOSM_LINK_VAL_BUF_SIZE) {
        /* Allocate new value buffer */
        if(NULL == (val_buf_dyn = (uint8_t *)H5MM_malloc(recx.rx_rsize)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate link value buffer")

        /* Point to new buffer */
        val_buf = val_buf_dyn;
        daos_iov_set(&sg_iov, val_buf, (daos_size_t)recx.rx_rsize);

        /* Reissue read */
        if(0 != (ret = daos_obj_fetch(grp->obj.obj_oh, grp->obj.item.file->epoch, &dkey, 1, &iod, &sgl, NULL /*maps */, NULL /*event*/)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't read link: %d", ret)
    } /* end if */

    /* Decode link type */
    p = val_buf;
    val->type = (H5L_type_t)*p++;

    /* Decode remainder of link value */
    switch(val->type) {
        case H5L_TYPE_HARD:
            /* Decode oid */
            UINT64DECODE(p, val->target.hard.lo)
            UINT64DECODE(p, val->target.hard.mid)
            UINT64DECODE(p, val->target.hard.hi)

            break;

        case H5L_TYPE_SOFT:
            /* If we had to allocate a buffer to read from daos, it happens to
             * be the exact size (len + 1) we need for the soft link value,
             * take ownership of it and shift the value down one byte.
             * Otherwise, allocate a new buffer. */
            if(val_buf_dyn) {
                val->target.soft = (char *)val_buf_dyn;
                val_buf_dyn = NULL;
                HDmemmove(val->target.soft,  val->target.soft + 1, recx.rx_rsize - 1);
            } /* end if */
            else {
                if(NULL == (val->target.soft = (char *)H5MM_malloc(recx.rx_rsize)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate link value buffer")
                HDmemcpy(val->target.soft, val_buf + 1, recx.rx_rsize - 1);
            } /* end else */

            /* Add null terminator */
            val->target.soft[recx.rx_rsize - 1] = '\0';

            break;

        case H5L_TYPE_ERROR:
        case H5L_TYPE_EXTERNAL:
        case H5L_TYPE_MAX:
        default:
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "invalid or unsupported link type")
    } /* end switch */

done:
    if(val_buf_dyn) {
        HDassert(ret_value == FAIL);
        H5MM_free(val_buf_dyn);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_link_read() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_link_write
 *
 * Purpose:     Writes the specified link to the given group
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
H5VL_daosm_link_write(H5VL_daosm_group_t *grp, const char *name,
    size_t name_len, H5VL_daosm_link_val_t *val)
{
    char const_link_key[] = H5VL_DAOSM_LINK_KEY;
    daos_key_t dkey;
    daos_vec_iod_t iod;
    daos_recx_t recx;
    daos_sg_list_t sgl;
    daos_iov_t sg_iov[2];
    uint8_t iov_buf[25];
    uint8_t *p;
    int ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Check for write access */
    if(!(grp->obj.item.file->flags & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "no write intent on file")

    /* Set up dkey */
    /* For now always use dkey = const, akey = name. Add option to switch these
     * DSMINC */
    daos_iov_set(&dkey, const_link_key, (daos_size_t)(sizeof(const_link_key) - 1));
 
    /* Encode link type */
    p = iov_buf;
    *p++ = (uint8_t)val->type;

    /* Encode type specific value information */
    switch(val->type) {
         case H5L_TYPE_HARD:
            HDassert(sizeof(iov_buf) == sizeof(val->target.hard) + 1);

            /* Encode oid */
            UINT64ENCODE(p, val->target.hard.lo)
            UINT64ENCODE(p, val->target.hard.mid)
            UINT64ENCODE(p, val->target.hard.hi)

            /* Set up type specific recx */
            recx.rx_rsize = (uint64_t)25;

            /* Set up type specific sgl */
            daos_iov_set(&sg_iov[0], iov_buf, (daos_size_t)sizeof(iov_buf));
            sgl.sg_nr.num = 1;

            break;

        case H5L_TYPE_SOFT:
            /* Set up type specific recx.  We need an extra byte for the link
             * type (encoded above). */
            recx.rx_rsize = (uint64_t)(HDstrlen(val->target.soft) + 1);

            /* Set up type specific sgl.  We use two entries, the first for the
             * link type, the second for the string. */
            daos_iov_set(&sg_iov[0], iov_buf, (daos_size_t)1);
            daos_iov_set(&sg_iov[1], val->target.soft, (daos_size_t)(recx.rx_rsize - (uint64_t)1));
            sgl.sg_nr.num = 2;

            break;

        case H5L_TYPE_ERROR:
        case H5L_TYPE_EXTERNAL:
        case H5L_TYPE_MAX:
        default:
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "invalid or unsupported link type")
    } /* end switch */

    /* Set up general recx */
    recx.rx_idx = (uint64_t)0;
    recx.rx_nr = (uint64_t)1;

    /* Set up iod */
    HDmemset(&iod, 0, sizeof(iod));
    daos_iov_set(&iod.vd_name, (void *)name, (daos_size_t)name_len);
    daos_csum_set(&iod.vd_kcsum, NULL, 0);
    iod.vd_nr = 1u;
    iod.vd_recxs = &recx;

    /* Set up general sgl */
    sgl.sg_iovs = sg_iov;

    /* Write link */
    if(0 != (ret = daos_obj_update(grp->obj.obj_oh, grp->obj.item.file->epoch, &dkey, 1, &iod, &sgl, NULL /*event*/)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't write link: %d", ret)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_link_write() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_link_create
 *
 * Purpose:     Creates a hard/soft/UD/external links.
 *              For now, only Soft Links are Supported.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              February, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_link_create(H5VL_link_create_type_t create_type, void *_item,
    H5VL_loc_params_t loc_params, hid_t lcpl_id, hid_t H5_ATTR_UNUSED lapl_id,
    hid_t dxpl_id, void **req)
{
    H5VL_daosm_item_t *item = (H5VL_daosm_item_t *)_item;
    H5P_genplist_t *plist = NULL;                      /* Property list pointer */
    H5VL_daosm_group_t *link_grp = NULL;
    const char *link_name = NULL;
    H5VL_daosm_link_val_t link_val;
    herr_t ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(lcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Find target group */
    HDassert(loc_params.type == H5VL_OBJECT_BY_NAME);
    if(NULL == (link_grp = H5VL_daosm_group_traverse(item, loc_params.loc_data.loc_by_name.name, dxpl_id, req, &link_name)))
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "can't traverse path")

    switch(create_type) {
        case H5VL_LINK_CREATE_HARD:
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "hard link creation not supported")

            break;

        case H5VL_LINK_CREATE_SOFT:
            /* Retrieve target name */
            link_val.type = H5L_TYPE_SOFT;
            if(H5P_get(plist, H5VL_PROP_LINK_TARGET_NAME, &link_val.target.soft) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for target name")

            /* Create soft link */
            if(H5VL_daosm_link_write(link_grp, link_name, HDstrlen(link_name), &link_val) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't create soft link")

            break;

        case H5VL_LINK_CREATE_UD:
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "UD link creation not supported")
        default:
            HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "invalid link creation call")
    } /* end switch */

done:
    /* Close link group */
    if(link_grp && H5VL_daosm_group_close(link_grp, dxpl_id, req) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close group")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_link_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_link_specific
 *
 * Purpose:     Specific operations with links
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              February, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_link_specific(void *_item, H5VL_loc_params_t loc_params,
    H5VL_link_specific_t specific_type, hid_t dxpl_id, void **req,
    va_list arguments)
{
    H5VL_daosm_item_t *item = (H5VL_daosm_item_t *)_item;
    H5VL_daosm_group_t *target_grp = NULL;
    const char *target_name = NULL;
    int ret;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Determine the target group */
    if(H5VL_OBJECT_BY_SELF == loc_params.type) {
        target_grp = (H5VL_daosm_group_t *)item;
        target_grp->obj.item.rc++;
    } /* end if */
    else {
        HDassert(H5VL_OBJECT_BY_NAME == loc_params.type);

        /* Traverse the path */
        if(NULL == (target_grp = H5VL_daosm_group_traverse(item, loc_params.loc_data.loc_by_name.name, dxpl_id, req, &target_name)))
            HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "can't traverse path")
    } /* end else */

    switch (specific_type) {
        /* H5Lexists */
        case H5VL_LINK_EXISTS:
            {
                htri_t *lexists_ret = va_arg(arguments, htri_t *);
                char const_link_key[] = H5VL_DAOSM_LINK_KEY;
                daos_key_t dkey;
                daos_vec_iod_t iod;
                daos_recx_t recx;

                /* Set up dkey */
                /* For now always use dkey = const, akey = name. Add option to switch these
                 * DSMINC */
                daos_iov_set(&dkey, const_link_key, (daos_size_t)(sizeof(const_link_key) - 1));

                /* Set up recx */
                recx.rx_rsize = DAOS_REC_ANY;
                recx.rx_idx = (uint64_t)0;
                recx.rx_nr = (uint64_t)1;

                /* Set up iod */
                HDmemset(&iod, 0, sizeof(iod));
                daos_iov_set(&iod.vd_name, (void *)target_name, HDstrlen(target_name));
                daos_csum_set(&iod.vd_kcsum, NULL, 0);
                iod.vd_nr = 1u;
                iod.vd_recxs = &recx;

                /* Read link */
                if(0 != (ret = daos_obj_fetch(target_grp->obj.obj_oh, target_grp->obj.item.file->epoch, &dkey, 1, &iod, NULL /*sgl*/, NULL /*maps*/, NULL /*event*/)))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't read link: %d", ret)

                /* Set return value */
                *lexists_ret = recx.rx_rsize != (uint64_t)0;

                break;
            } /* end block */

        case H5VL_LINK_DELETE:
        case H5VL_LINK_ITER:
            HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unsupported specific operation")
        default:
            HGOTO_ERROR(H5E_VOL, H5E_BADVALUE, FAIL, "invalid specific operation")
    } /* end switch */

done:
    if(target_grp && H5VL_daosm_group_close(target_grp, dxpl_id, req) < 0)
        HDONE_ERROR(H5E_FILE, H5E_CLOSEERROR, FAIL, "can't close group")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_link_specific() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_link_follow
 *
 * Purpose:     Follows the link in grp identified with name, and returns
 *              in oid the oid of the target object.
 *
 * Return:      Success:        SUCCEED 
 *              Failure:        FAIL
 *
 * Programmer:  Neil Fortner
 *              January, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_link_follow(H5VL_daosm_group_t *grp, const char *name,
    size_t name_len, hid_t dxpl_id, void **req, daos_obj_id_t *oid)
{
    H5VL_daosm_link_val_t link_val;
    hbool_t link_val_alloc = FALSE;
    H5VL_daosm_group_t *target_grp = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(grp);
    HDassert(name);
    HDassert(oid);

    /* Read link to group */
   if(H5VL_daosm_link_read(grp, name, name_len, &link_val) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't read link to group")

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
                if(NULL == (target_grp = H5VL_daosm_group_traverse(&grp->obj.item, link_val.target.soft, dxpl_id, req, &target_name)))
                    HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "can't traverse path")

                /* Check for no target_name, in this case just return
                 * target_grp's oid */
                if(target_name[0] == '\0'
                        || (target_name[0] == '.' && target_name[1] == '\0'))
                    *oid = target_grp->obj.oid;
                else
                    /* Follow the last element in the path */
                    if(H5VL_daosm_link_follow(target_grp, target_name, HDstrlen(target_name), dxpl_id, req, oid) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't follow link to group")

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
        if(H5VL_daosm_group_close(target_grp, dxpl_id, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close group")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_link_follow() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_group_traverse
 *
 * Purpose:     Given a path name and base object, returns the final group
 *              in the path and the object name.  obj_name points into the
 *              buffer given by path, so it does not need to be freed.
 *              The group must be closed with H5VL_daosm_group_close().
 *
 * Return:      Success:        group object. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              December, 2016
 *
 *-------------------------------------------------------------------------
 */
static H5VL_daosm_group_t *
H5VL_daosm_group_traverse(H5VL_daosm_item_t *item, const char *path,
    hid_t dxpl_id, void **req, const char **obj_name)
{
    H5VL_daosm_group_t *grp = NULL;
    const char *next_obj;
    daos_obj_id_t oid;
    H5VL_daosm_group_t *ret_value = NULL;

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
            grp = (H5VL_daosm_group_t *)item;
        else if(item->type == H5I_FILE)
            grp = ((H5VL_daosm_file_t *)item)->root_grp;
        else
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "item not a file or group")
    } /* end else */
        
    grp->obj.item.rc++;

    /* Search for '/' */
    next_obj = strchr(*obj_name, '/');

    /* Traverse path */
    while(next_obj) {
        /* Follow link to next group in path */
        HDassert(next_obj > *obj_name);
        if(H5VL_daosm_link_follow(grp, *obj_name, (size_t)(next_obj - *obj_name), dxpl_id, req, &oid) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't follow link to group")

        /* Close previous group */
        if(H5VL_daosm_group_close(grp, dxpl_id, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")
        grp = NULL;

        /* Open group */
        if(NULL == (grp = (H5VL_daosm_group_t *)H5VL_daosm_group_open_helper(item->file, oid, H5P_GROUP_ACCESS_DEFAULT, dxpl_id, req)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't open group")

        /* Advance to next path element */
        *obj_name = next_obj + 1;
        next_obj = strchr(*obj_name, '/');
    } /* end while */

    /* Set return value */
    ret_value = grp;

done:
    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    if(NULL == ret_value)
        /* Close group */
        if(grp && H5VL_daosm_group_close(grp, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CLOSEERROR, NULL, "can't close group")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_group_traverse() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_group_create_helper
 *
 * Purpose:     Performs the actual group creation, but does not create a
 *              link.
 *
 * Return:      Success:        group object. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              November, 2016
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_daosm_group_create_helper(H5VL_daosm_file_t *file, hid_t gcpl_id,
    hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_daosm_group_t *grp = NULL;
    daos_key_t dkey;
    daos_vec_iod_t iod;
    daos_recx_t recx;
    daos_sg_list_t sgl;
    daos_iov_t sg_iov;
    size_t gcpl_size = 0;
    void *gcpl_buf = NULL;
    char int_md_key[] = H5VL_DAOSM_INT_MD_KEY;
    char gcpl_key[] = H5VL_DAOSM_CPL_KEY;
    int ret;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file->flags & H5F_ACC_RDWR);

    /* Allocate the group object that is returned to the user */
    if(NULL == (grp = H5FL_CALLOC(H5VL_daosm_group_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate DAOS-M group struct")
    grp->obj.item.type = H5I_GROUP;
    grp->obj.item.file = file;
    grp->obj.item.rc = 1;
    grp->obj.obj_oh = DAOS_HDL_INVAL;
    grp->gcpl_id = FAIL;
    grp->gapl_id = FAIL;

    /* Create group */
    grp->obj.oid.lo = file->max_oid + (uint64_t)1;
    daos_obj_id_generate(&grp->obj.oid, DAOS_OC_TINY_RW);
    if(0 != (ret = daos_obj_declare(file->coh, grp->obj.oid, file->epoch, NULL /*oa*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create dataset: %d", ret)
    file->max_oid = grp->obj.oid.lo;

    /* Write max OID */
    if(H5VL_daosm_write_max_oid(file) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't write max OID")

    /* Open group */
    if(0 != (ret = daos_obj_open(file->coh, grp->obj.oid, file->epoch, DAOS_OO_RW, &grp->obj.obj_oh, NULL /*event*/)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't open root group: %d", ret)

    /* Encode GCPL */
    if(H5Pencode(gcpl_id, NULL, &gcpl_size) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "can't determine serialized length of gcpl")
    if(NULL == (gcpl_buf = H5MM_malloc(gcpl_size)))
        HGOTO_ERROR(gcpl_id, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized gcpl")
    if(H5Pencode(gcpl_id, gcpl_buf, &gcpl_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, NULL, "can't serialize gcpl")

    /* Set up operation to write GCPL to group */
    /* Set up dkey */
    daos_iov_set(&dkey, int_md_key, (daos_size_t)(sizeof(int_md_key) - 1));

    /* Set up recx */
    recx.rx_rsize = (uint64_t)gcpl_size;
    recx.rx_idx = (uint64_t)0;
    recx.rx_nr = (uint64_t)1;

    /* Set up iod */
    HDmemset(&iod, 0, sizeof(iod));
    daos_iov_set(&iod.vd_name, (void *)gcpl_key, (daos_size_t)(sizeof(gcpl_key) - 1));
    daos_csum_set(&iod.vd_kcsum, NULL, 0);
    iod.vd_nr = 1u;
    iod.vd_recxs = &recx;

    /* Set up sgl */
    daos_iov_set(&sg_iov, gcpl_buf, (daos_size_t)gcpl_size);
    sgl.sg_nr.num = 1;
    sgl.sg_iovs = &sg_iov;

    /* Write internal metadata to group */
    if(0 != (ret = daos_obj_update(grp->obj.obj_oh, file->epoch, &dkey, 1, &iod, &sgl, NULL /*event*/)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't write metadata to group: %d", ret)

    /* Finish setting up group struct */
    if((grp->gcpl_id = H5Pcopy(gcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gcpl");
    if((grp->gapl_id = H5Pcopy(gapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gapl");

    ret_value = (void *)grp;

done:
    /* Free memory */
    gcpl_buf = H5MM_xfree(gcpl_buf);

    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    /* Destroy DAOS object if created before failure DSMINC */
    if(NULL == ret_value)
        /* Close group */
        if(grp && H5VL_daosm_group_close(grp, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CLOSEERROR, NULL, "can't close group")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_group_create_helper() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_group_create
 *
 * Purpose:     Sends a request to DAOS-M to create a group
 *
 * Return:      Success:        group object. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              November, 2016
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_daosm_group_create(void *_item,
    H5VL_loc_params_t H5_ATTR_UNUSED loc_params, const char *name,
    hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_daosm_item_t *item = (H5VL_daosm_item_t *)_item;
    H5VL_daosm_group_t *grp = NULL;
    H5VL_daosm_group_t *target_grp = NULL;
    const char *target_name = NULL;
    H5VL_daosm_link_val_t link_val;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Check for write access */
    if(!(item->file->flags & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "no write intent on file")

    /* Traverse the path */
    if(NULL == (target_grp = H5VL_daosm_group_traverse(item, name, dxpl_id, req, &target_name)))
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, NULL, "can't traverse path")

    /* Create group */
    if(NULL == (grp = (H5VL_daosm_group_t *)H5VL_daosm_group_create_helper(item->file, gcpl_id, gapl_id, dxpl_id, req)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create group")

    /* Create link to group */
    link_val.type = H5L_TYPE_HARD;
    link_val.target.hard = grp->obj.oid;
    if(H5VL_daosm_link_write(target_grp, target_name, HDstrlen(target_name), &link_val) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create link to group")

    ret_value = (void *)grp;

done:
    /* Close target group */
    if(target_grp && H5VL_daosm_group_close(target_grp, dxpl_id, req) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")

    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    /* Destroy DAOS object if created before failure DSMINC */
    if(NULL == ret_value)
        /* Close group */
        if(grp && H5VL_daosm_group_close(grp, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_group_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_group_open_helper
 *
 * Purpose:     Performs the actual group open, given the oid.
 *
 * Return:      Success:        group object. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              December, 2016
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_daosm_group_open_helper(H5VL_daosm_file_t *file, daos_obj_id_t oid,
    hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_daosm_group_t *grp = NULL;
    daos_key_t dkey;
    daos_vec_iod_t iod;
    daos_recx_t recx;
    daos_sg_list_t sgl;
    daos_iov_t sg_iov;
    void *gcpl_buf = NULL;
    char int_md_key[] = H5VL_DAOSM_INT_MD_KEY;
    char gcpl_key[] = H5VL_DAOSM_CPL_KEY;
    int ret;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Allocate the group object that is returned to the user */
    if(NULL == (grp = H5FL_CALLOC(H5VL_daosm_group_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate DAOS-M group struct")
    grp->obj.item.type = H5I_GROUP;
    grp->obj.item.file = file;
    grp->obj.item.rc = 1;
    grp->obj.oid = oid;
    grp->obj.obj_oh = DAOS_HDL_INVAL;
    grp->gcpl_id = FAIL;
    grp->gapl_id = FAIL;

    /* Open group */
    if(0 != (ret = daos_obj_open(file->coh, oid, file->epoch, DAOS_OO_RW, &grp->obj.obj_oh, NULL /*event*/)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't open root group: %d", ret)

    /* Set up operation to read GCPL size from group */
    /* Set up dkey */
    daos_iov_set(&dkey, int_md_key, (daos_size_t)(sizeof(int_md_key) - 1));

    /* Set up recx */
    recx.rx_rsize = DAOS_REC_ANY;
    recx.rx_idx = (uint64_t)0;
    recx.rx_nr = (uint64_t)1;

    /* Set up iod */
    HDmemset(&iod, 0, sizeof(iod));
    daos_iov_set(&iod.vd_name, (void *)gcpl_key, (daos_size_t)(sizeof(gcpl_key) - 1));
    daos_csum_set(&iod.vd_kcsum, NULL, 0);
    iod.vd_nr = 1u;
    iod.vd_recxs = &recx;

    /* Read internal metadata size from group */
    if(0 != (ret = daos_obj_fetch(grp->obj.obj_oh, file->epoch, &dkey, 1, &iod, NULL, NULL /*maps*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, NULL, "can't read metadata size from group: %d", ret)

    /* Check for metadata not found */
    if(recx.rx_rsize == (uint64_t)0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "internal metadata not found")

    /* Allocate buffer for GCPL */
    if(NULL == (gcpl_buf = H5MM_malloc(recx.rx_rsize)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized gcpl")

    /* Set up sgl */
    daos_iov_set(&sg_iov, gcpl_buf, (daos_size_t)recx.rx_rsize);
    sgl.sg_nr.num = 1;
    sgl.sg_iovs = &sg_iov;

    /* Read internal metadata from group */
    if(0 != (ret = daos_obj_fetch(grp->obj.obj_oh, file->epoch, &dkey, 1, &iod, &sgl, NULL /*maps*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, NULL, "can't read metadata from group: %d", ret)

    /* Decode GCPL */
    if((grp->gcpl_id = H5Pdecode(gcpl_buf)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTDECODE, NULL, "can't deserialize GCPL")

    /* Finish setting up group struct */
    if((grp->gapl_id = H5Pcopy(gapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy gapl");

    ret_value = (void *)grp;

done:
    /* Free memory */
    gcpl_buf = H5MM_xfree(gcpl_buf);

    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    if(NULL == ret_value)
        /* Close group */
        if(grp && H5VL_daosm_group_close(grp, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_group_open_helper() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_group_open
 *
 * Purpose:     Sends a request to DAOS-M to open a group
 *
 * Return:      Success:        dataset object. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              November, 2016
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_daosm_group_open(void *_item,
    H5VL_loc_params_t H5_ATTR_UNUSED loc_params, const char *name,
    hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_daosm_item_t *item = (H5VL_daosm_item_t *)_item;
    H5VL_daosm_group_t *grp = NULL;
    H5VL_daosm_group_t *target_grp = NULL;
    const char *target_name = NULL;
    daos_obj_id_t oid;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Traverse the path */
    if(NULL == (target_grp = H5VL_daosm_group_traverse(item, name, dxpl_id, req, &target_name)))
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, NULL, "can't traverse path")

    /* Check for no target_name, in this case just return target_grp */
    if(target_name[0] == '\0'
            || (target_name[0] == '.' && target_name[1] == '\0')) {
        ret_value = (void *)target_grp;
        target_grp = NULL;
    } /* end if */
    else {
        /* Follow link to group */
        if(H5VL_daosm_link_follow(target_grp, target_name, HDstrlen(target_name), dxpl_id, req, &oid) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't follow link to group")

        /* Open group */
        if(NULL == (grp = (H5VL_daosm_group_t *)H5VL_daosm_group_open_helper(item->file, oid, gapl_id, dxpl_id, req)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't open group")

        ret_value = (void *)grp;
    } /* end else */

done:
    /* Close target group */
    if(target_grp && H5VL_daosm_group_close(target_grp, dxpl_id, req) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")

    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    if(NULL == ret_value)
        /* Close group */
        if(grp && H5VL_daosm_group_close(grp, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_group_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_group_close
 *
 * Purpose:     Closes a daos-m HDF5 group.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              November, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_group_close(void *_grp, hid_t H5_ATTR_UNUSED dxpl_id,
    void H5_ATTR_UNUSED **req)
{
    H5VL_daosm_group_t *grp = (H5VL_daosm_group_t *)_grp;
    daos_handle_t hdl_inval = DAOS_HDL_INVAL;
    int ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(grp);

    if(--grp->obj.item.rc == 0) {
        /* Free group data structures */
        if(HDmemcmp(&grp->obj.obj_oh, &hdl_inval, sizeof(hdl_inval)))
            if(0 != (ret = daos_obj_close(grp->obj.obj_oh, NULL /*event*/)))
                HDONE_ERROR(H5E_SYM, H5E_CANTCLOSEOBJ, FAIL, "can't close group DAOS object: %d", ret)
        if(grp->gcpl_id != FAIL && H5I_dec_app_ref(grp->gcpl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist")
        if(grp->gapl_id != FAIL && H5I_dec_app_ref(grp->gapl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist")
        grp = H5FL_FREE(H5VL_daosm_group_t, grp);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_group_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_need_bkg
 *
 * Purpose:     Determine if a background buffer is needed for conversion.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              February, 2017
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5VL_daosm_need_bkg(hid_t src_type_id, hid_t dst_type_id, hbool_t *fill_bkg)
{
    hid_t memb_type_id = -1;
    hid_t src_memb_type_id = -1;
    char *memb_name = NULL;
    H5T_class_t tclass;
    htri_t ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get datatype class */
    if(H5T_NO_CLASS == (tclass = H5Tget_class(dst_type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get type class")

    switch(tclass) {
        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_TIME:
        case H5T_STRING:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_ENUM:
            /* No background buffer necessary */
            ret_value = FALSE;

            break;

        case H5T_COMPOUND:
            {
                int nmemb;
                int src_i;
                int i;

                /* We must always provide a background buffer for compound
                 * conversions.  Only need to check further to see if it must be
                 * filled. */
                ret_value = TRUE;

                /* Get number of compound members */
                if((nmemb = H5Tget_nmembers(dst_type_id)) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get number of compound members")

                /* Iterate over compound members, checking for a member in
                 * dst_type_id with no match in src_type_id */
                for(i = 0; i < nmemb; i++) {
                    /* Get member type */
                    if((memb_type_id = H5Tget_member_type(dst_type_id, (unsigned)i)) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get compound member type")

                    /* Get member name */
                    if(NULL == (memb_name = H5Tget_member_name(dst_type_id, (unsigned)i)))
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get compound member name")

                    /* Check for matching name in source type */
                    H5E_BEGIN_TRY {
                        src_i = H5Tget_member_index(src_type_id, memb_name);
                    } H5E_END_TRY

                    /* Free memb_name */
                    if(H5free_memory(memb_name) < 0)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "can't free member name")
                    memb_name = NULL;

                    /* If no match was found, this type is not being filled in,
                     * so we must fill the background buffer */
                    if(src_i < 0) {
                        if(H5Tclose(memb_type_id) < 0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CLOSEERROR, FAIL, "can't close member type")
                        memb_type_id = -1;
                        *fill_bkg = TRUE;
                        HGOTO_DONE(TRUE)
                    } /* end if */

                    /* Open matching source type */
                    if((src_memb_type_id = H5Tget_member_type(src_type_id, (unsigned)src_i)) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get compound member type")

                    /* Recursively check member type, this will fill in the
                     * member size */
                    if(H5VL_daosm_need_bkg(src_memb_type_id, memb_type_id, fill_bkg) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "can't check if background buffer needed")

                    /* Close source member type */
                    if(H5Tclose(src_memb_type_id) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CLOSEERROR, FAIL, "can't close member type")
                    src_memb_type_id = -1;

                    /* Close member type */
                    if(H5Tclose(memb_type_id) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CLOSEERROR, FAIL, "can't close member type")
                    memb_type_id = -1;

                    /* If the source member type needs the background filled, so
                     * does the parent */
                    if(*fill_bkg)
                        HGOTO_DONE(TRUE)
                } /* end for */

                break;
            } /* end block */

        case H5T_ARRAY:
            /* Get parent type */
            if((memb_type_id = H5Tget_super(dst_type_id)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get array parent type")

            /* Get source parent type */
            if((src_memb_type_id = H5Tget_super(src_type_id)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get array parent type")

            /* Recursively check parent type */
            if((ret_value = H5VL_daosm_need_bkg(src_memb_type_id, memb_type_id, fill_bkg)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "can't check if background buffer needed")

            /* Close source parent type */
            if(H5Tclose(src_memb_type_id) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CLOSEERROR, FAIL, "can't close array parent type")
            src_memb_type_id = -1;

            /* Close parent type */
            if(H5Tclose(memb_type_id) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CLOSEERROR, FAIL, "can't close array parent type")
            memb_type_id = -1;

            break;

        case H5T_REFERENCE:
        case H5T_VLEN:
            /* Not yet supported */
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "reference and vlen types not supported")

            break;

        case H5T_NO_CLASS:
        case H5T_NCLASSES:
        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid type class")
    } /* end switch */

done:
    /* Cleanup on failure */
    if(ret_value < 0) {
        if(memb_type_id >= 0)
            if(H5I_dec_app_ref(memb_type_id) < 0)
                HDONE_ERROR(H5E_DATATYPE, H5E_CANTDEC, FAIL, "failed to close member type")
        if(src_memb_type_id >= 0)
            if(H5I_dec_app_ref(src_memb_type_id) < 0)
                HDONE_ERROR(H5E_DATATYPE, H5E_CANTDEC, FAIL, "failed to close source member type")
        memb_name = (char *)H5MM_xfree(memb_name);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_need_bkg() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_tconv_init
 *
 * Purpose:     DSMINC
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              December, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_tconv_init(hid_t src_type_id, size_t *src_type_size,
    hid_t dst_type_id, size_t *dst_type_size, size_t num_elem, void **tconv_buf,
    void **bkg_buf, H5VL_daosm_tconv_reuse_t *reuse, hbool_t *fill_bkg)
{
    htri_t need_bkg;
    htri_t types_equal;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(src_type_size);
    HDassert(dst_type_size);
    HDassert(tconv_buf);
    HDassert(!*tconv_buf);
    HDassert(bkg_buf);
    HDassert(!*bkg_buf);
    HDassert(fill_bkg);
    HDassert(!*fill_bkg);

    /* Get source type size */
    if((*src_type_size = H5Tget_size(src_type_id)) == 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get source type size")

    /* Check if the types are equal */
    if((types_equal = H5Tequal(src_type_id, dst_type_id)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOMPARE, FAIL, "can't check if types are equal")
    if(types_equal)
        /* Types are equal, no need for conversion, just set dst_type_size */
        *dst_type_size = *src_type_size;
    else {
        /* Get destination type size */
        if((*dst_type_size = H5Tget_size(dst_type_id)) == 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't get source type size")

        /* Check if we need a background buffer */
        if((need_bkg = H5VL_daosm_need_bkg(src_type_id, dst_type_id, fill_bkg)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "can't check if background buffer needed")

        /* Check for reusable destination buffer */
        if(reuse) {
            HDassert(*reuse == H5VL_DAOSM_TCONV_REUSE_NONE);

            /* Use dest buffer for type conversion if it large enough, otherwise
             * use it for the background buffer if one is needed. */
            if(dst_type_size >= src_type_size)
                *reuse = H5VL_DAOSM_TCONV_REUSE_TCONV;
            else if(need_bkg)
                *reuse = H5VL_DAOSM_TCONV_REUSE_BKG;
        } /* end if */

        /* Allocate conversion buffer if it is not being reused */
        if(!reuse || (*reuse != H5VL_DAOSM_TCONV_REUSE_TCONV))
            if(NULL == (*tconv_buf = H5MM_malloc(num_elem * (*src_type_size
                    > *dst_type_size ? *src_type_size : *dst_type_size))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate type conversion buffer")

        /* Allocate background buffer if one is needed and it is not being
         * reused */
        if(need_bkg && (!reuse || (*reuse != H5VL_DAOSM_TCONV_REUSE_BKG)))
            if(NULL == (*bkg_buf = H5MM_malloc(num_elem * *dst_type_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate background buffer")
    } /* end else */

done:
    /* Cleanup on failure */
    if(ret_value < 0) {
        *tconv_buf = H5MM_xfree(*tconv_buf);
        *bkg_buf = H5MM_xfree(*bkg_buf);
        if(reuse)
            *reuse = H5VL_DAOSM_TCONV_REUSE_NONE;
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_tconv_init() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_dataset_create
 *
 * Purpose:     Sends a request to DAOS-M to create a dataset
 *
 * Return:      Success:        dataset object. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              November, 2016
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_daosm_dataset_create(void *_item,
    H5VL_loc_params_t H5_ATTR_UNUSED loc_params, const char *name,
    hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req)
{
    H5VL_daosm_item_t *item = (H5VL_daosm_item_t *)_item;
    H5VL_daosm_dset_t *dset = NULL;
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    hid_t type_id, space_id;
    H5VL_daosm_group_t *target_grp = NULL;
    const char *target_name = NULL;
    H5VL_daosm_link_val_t link_val;
    daos_key_t dkey;
    daos_vec_iod_t iod[3];
    daos_recx_t recx[3];
    daos_sg_list_t sgl[3];
    daos_iov_t sg_iov[3];
    size_t type_size = 0;
    size_t space_size = 0;
    size_t dcpl_size = 0;
    void *type_buf = NULL;
    void *space_buf = NULL;
    void *dcpl_buf = NULL;
    char int_md_key[] = H5VL_DAOSM_INT_MD_KEY;
    char type_key[] = H5VL_DAOSM_TYPE_KEY;
    char space_key[] = H5VL_DAOSM_SPACE_KEY;
    char dcpl_key[] = H5VL_DAOSM_CPL_KEY;
    int ret;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Check for write access */
    if(!(item->file->flags & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "no write intent on file")

    /* Get the dcpl plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

    /* get creation properties */
    if(H5P_get(plist, H5VL_PROP_DSET_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for datatype id")
    if(H5P_get(plist, H5VL_PROP_DSET_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for space id")

    /* Traverse the path */
    if(NULL == (target_grp = H5VL_daosm_group_traverse(item, name, dxpl_id, req, &target_name)))
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, NULL, "can't traverse path")

    /* Allocate the dataset object that is returned to the user */
    if(NULL == (dset = H5FL_CALLOC(H5VL_daosm_dset_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate DAOS-M dataset struct")
    dset->obj.item.type = H5I_DATASET;
    dset->obj.item.file = item->file;
    dset->obj.item.rc = 1;
    dset->obj.obj_oh = DAOS_HDL_INVAL;
    dset->type_id = FAIL;
    dset->space_id = FAIL;
    dset->dcpl_id = FAIL;
    dset->dapl_id = FAIL;

    /* Create dataset */
    dset->obj.oid.lo = item->file->max_oid + (uint64_t)1;
    daos_obj_id_generate(&dset->obj.oid, DAOS_OC_LARGE_RW);
    if(0 != (ret = daos_obj_declare(item->file->coh, dset->obj.oid, item->file->epoch, NULL /*oa*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create dataset: %d", ret)
    item->file->max_oid = dset->obj.oid.lo;

    /* Write max OID */
    if(H5VL_daosm_write_max_oid(item->file) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't write max OID")

    /* Create link to dataset */
    link_val.type = H5L_TYPE_HARD;
    link_val.target.hard = dset->obj.oid;
    if(H5VL_daosm_link_write(target_grp, target_name, HDstrlen(target_name), &link_val) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create link to dataset")

    /* Open dataset */
    if(0 != (ret = daos_obj_open(item->file->coh, dset->obj.oid, item->file->epoch, DAOS_OO_RW, &dset->obj.obj_oh, NULL /*event*/)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't open root group: %d", ret)

    /* Encode datatype */
    if(H5Tencode(type_id, NULL, &type_size) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "can't determine serialized length of datatype")
    if(NULL == (type_buf = H5MM_malloc(type_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized datatype")
    if(H5Tencode(type_id, type_buf, &type_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTENCODE, NULL, "can't serialize datatype")

    /* Encode dataspace */
    if(H5Sencode(space_id, NULL, &space_size) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "can't determine serialized length of dataaspace")
    if(NULL == (space_buf = H5MM_malloc(space_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized dataaspace")
    if(H5Sencode(space_id, space_buf, &space_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTENCODE, NULL, "can't serialize dataaspace")

    /* Encode DCPL */
    if(H5Pencode(dcpl_id, NULL, &dcpl_size) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "can't determine serialized length of dcpl")
    if(NULL == (dcpl_buf = H5MM_malloc(dcpl_size)))
        HGOTO_ERROR(dcpl_id, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized dcpl")
    if(H5Pencode(dcpl_id, dcpl_buf, &dcpl_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTENCODE, NULL, "can't serialize dcpl")

    /* Set up operation to write datatype, dataspace, and DCPL to dataset */
    /* Set up dkey */
    daos_iov_set(&dkey, int_md_key, (daos_size_t)(sizeof(int_md_key) - 1));

    /* Set up recx */
    recx[0].rx_rsize = (uint64_t)type_size;
    recx[0].rx_idx = (uint64_t)0;
    recx[0].rx_nr = (uint64_t)1;
    recx[1].rx_rsize = (uint64_t)space_size;
    recx[1].rx_idx = (uint64_t)0;
    recx[1].rx_nr = (uint64_t)1;
    recx[2].rx_rsize = (uint64_t)dcpl_size;
    recx[2].rx_idx = (uint64_t)0;
    recx[2].rx_nr = (uint64_t)1;

    /* Set up iod */
    HDmemset(iod, 0, sizeof(iod));
    daos_iov_set(&iod[0].vd_name, (void *)type_key, (daos_size_t)(sizeof(type_key) - 1));
    daos_csum_set(&iod[0].vd_kcsum, NULL, 0);
    iod[0].vd_nr = 1u;
    iod[0].vd_recxs = &recx[0];
    daos_iov_set(&iod[1].vd_name, (void *)space_key, (daos_size_t)(sizeof(space_key) - 1));
    daos_csum_set(&iod[1].vd_kcsum, NULL, 0);
    iod[1].vd_nr = 1u;
    iod[1].vd_recxs = &recx[1];
    daos_iov_set(&iod[2].vd_name, (void *)dcpl_key, (daos_size_t)(sizeof(dcpl_key) - 1));
    daos_csum_set(&iod[2].vd_kcsum, NULL, 0);
    iod[2].vd_nr = 1u;
    iod[2].vd_recxs = &recx[2];

    /* Set up sgl */
    daos_iov_set(&sg_iov[0], type_buf, (daos_size_t)type_size);
    sgl[0].sg_nr.num = 1;
    sgl[0].sg_iovs = &sg_iov[0];
    daos_iov_set(&sg_iov[1], space_buf, (daos_size_t)space_size);
    sgl[1].sg_nr.num = 1;
    sgl[1].sg_iovs = &sg_iov[1];
    daos_iov_set(&sg_iov[2], dcpl_buf, (daos_size_t)dcpl_size);
    sgl[2].sg_nr.num = 1;
    sgl[2].sg_iovs = &sg_iov[2];

    /* Write internal metadata to dataset */
    if(0 != (ret = daos_obj_update(dset->obj.obj_oh, item->file->epoch, &dkey, 3, iod, sgl, NULL /*event*/)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "can't write metadata to dataset: %d", ret)

    /* Finish setting up dataset struct */
    if((dset->type_id = H5Tcopy(type_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy datatype")
    if((dset->space_id = H5Scopy(space_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dataspace")
    if(H5Sselect_all(dset->space_id) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, NULL, "can't change selection")
    if((dset->dcpl_id = H5Pcopy(dcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dcpl")
    if((dset->dapl_id = H5Pcopy(dapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dapl")

    ret_value = (void *)dset;

done:
    /* Free memory */
    type_buf = H5MM_xfree(type_buf);
    space_buf = H5MM_xfree(space_buf);
    dcpl_buf = H5MM_xfree(dcpl_buf);

    /* Close target group */
    if(target_grp && H5VL_daosm_group_close(target_grp, dxpl_id, req) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, NULL, "can't close group")

    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    /* Destroy DAOS object if created before failure DSMINC */
    if(NULL == ret_value)
        /* Close dataset */
        if(dset && H5VL_daosm_dataset_close(dset, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, NULL, "can't close dataset")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_dataset_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_dataset_open
 *
 * Purpose:     Sends a request to DAOS-M to open a dataset
 *
 * Return:      Success:        dataset object. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              November, 2016
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_daosm_dataset_open(void *_item,
    H5VL_loc_params_t H5_ATTR_UNUSED loc_params, const char *name,
    hid_t dapl_id, hid_t dxpl_id, void **req)
{
    H5VL_daosm_item_t *item = (H5VL_daosm_item_t *)_item;
    H5VL_daosm_dset_t *dset = NULL;
    H5VL_daosm_group_t *target_grp = NULL;
    const char *target_name = NULL;
    daos_key_t dkey;
    daos_vec_iod_t iod[3];
    daos_recx_t recx[3];
    daos_sg_list_t sgl[3];
    daos_iov_t sg_iov[3];
    void *type_buf = NULL;
    void *space_buf = NULL;
    void *dcpl_buf = NULL;
    char int_md_key[] = H5VL_DAOSM_INT_MD_KEY;
    char type_key[] = H5VL_DAOSM_TYPE_KEY;
    char space_key[] = H5VL_DAOSM_SPACE_KEY;
    char dcpl_key[] = H5VL_DAOSM_CPL_KEY;
    int ret;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Traverse the path */
    if(NULL == (target_grp = H5VL_daosm_group_traverse(item, name, dxpl_id, req, &target_name)))
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, NULL, "can't traverse path")

    /* Allocate the dataset object that is returned to the user */
    if(NULL == (dset = H5FL_CALLOC(H5VL_daosm_dset_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate DAOS-M dataset struct")
    dset->obj.item.type = H5I_DATASET;
    dset->obj.item.file = item->file;
    dset->obj.item.rc = 1;
    dset->obj.obj_oh = DAOS_HDL_INVAL;
    dset->type_id = FAIL;
    dset->space_id = FAIL;
    dset->dcpl_id = FAIL;
    dset->dapl_id = FAIL;

    /* Follow link to dataset */
    if(H5VL_daosm_link_follow(target_grp, target_name, HDstrlen(target_name), dxpl_id, req, &dset->obj.oid) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't follow link to dataset")

    /* Open dataset */
    if(0 != (ret = daos_obj_open(item->file->coh, dset->obj.oid, item->file->epoch, DAOS_OO_RW, &dset->obj.obj_oh, NULL /*event*/)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't open root group: %d", ret)

    /* Set up operation to read datatype, dataspace, and DCPL sizes from dataset
     */
    /* Set up dkey */
    daos_iov_set(&dkey, int_md_key, (daos_size_t)(sizeof(int_md_key) - 1));

    /* Set up recx */
    recx[0].rx_rsize = DAOS_REC_ANY;
    recx[0].rx_idx = (uint64_t)0;
    recx[0].rx_nr = (uint64_t)1;
    recx[1].rx_rsize = DAOS_REC_ANY;
    recx[1].rx_idx = (uint64_t)0;
    recx[1].rx_nr = (uint64_t)1;
    recx[2].rx_rsize = DAOS_REC_ANY;
    recx[2].rx_idx = (uint64_t)0;
    recx[2].rx_nr = (uint64_t)1;

    /* Set up iod */
    HDmemset(iod, 0, sizeof(iod));
    daos_iov_set(&iod[0].vd_name, (void *)type_key, (daos_size_t)(sizeof(type_key) - 1));
    daos_csum_set(&iod[0].vd_kcsum, NULL, 0);
    iod[0].vd_nr = 1u;
    iod[0].vd_recxs = &recx[0];
    daos_iov_set(&iod[1].vd_name, (void *)space_key, (daos_size_t)(sizeof(space_key) - 1));
    daos_csum_set(&iod[1].vd_kcsum, NULL, 0);
    iod[1].vd_nr = 1u;
    iod[1].vd_recxs = &recx[1];
    daos_iov_set(&iod[2].vd_name, (void *)dcpl_key, (daos_size_t)(sizeof(dcpl_key) - 1));
    daos_csum_set(&iod[2].vd_kcsum, NULL, 0);
    iod[2].vd_nr = 1u;
    iod[2].vd_recxs = &recx[2];

    /* Read internal metadata sizes from dataset */
    if(0 != (ret = daos_obj_fetch(dset->obj.obj_oh, item->file->epoch, &dkey, 3, iod, NULL, NULL /*maps*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDECODE, NULL, "can't read metadata sizes from dataset: %d", ret)

    /* Check for metadata not found */
    if((recx[0].rx_rsize == (uint64_t)0) || (recx[1].rx_rsize == (uint64_t)0)
            || (recx[2].rx_rsize == (uint64_t)0))
        HGOTO_ERROR(H5E_DATASET, H5E_NOTFOUND, NULL, "internal metadata not found")

    /* Allocate buffers for datatype, dataspace, and DCPL */
    if(NULL == (type_buf = H5MM_malloc(recx[0].rx_rsize)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized datatype")
    if(NULL == (space_buf = H5MM_malloc(recx[1].rx_rsize)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized dataaspace")
    if(NULL == (dcpl_buf = H5MM_malloc(recx[2].rx_rsize)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized dcpl")

    /* Set up sgl */
    daos_iov_set(&sg_iov[0], type_buf, (daos_size_t)recx[0].rx_rsize);
    sgl[0].sg_nr.num = 1;
    sgl[0].sg_iovs = &sg_iov[0];
    daos_iov_set(&sg_iov[1], space_buf, (daos_size_t)recx[1].rx_rsize);
    sgl[1].sg_nr.num = 1;
    sgl[1].sg_iovs = &sg_iov[1];
    daos_iov_set(&sg_iov[2], dcpl_buf, (daos_size_t)recx[2].rx_rsize);
    sgl[2].sg_nr.num = 1;
    sgl[2].sg_iovs = &sg_iov[2];

    /* Read internal metadata from dataset */
    if(0 != (ret = daos_obj_fetch(dset->obj.obj_oh, item->file->epoch, &dkey, 3, iod, sgl, NULL /*maps*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDECODE, NULL, "can't read metadata from dataset: %d", ret)

    /* Decode datatype, dataspace, and DCPL */
    if((dset->type_id = H5Tdecode(type_buf)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTDECODE, NULL, "can't deserialize datatype")
    if((dset->space_id = H5Sdecode(space_buf)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTDECODE, NULL, "can't deserialize datatype")
    if(H5Sselect_all(dset->space_id) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, NULL, "can't change selection")
    if((dset->dcpl_id = H5Pdecode(dcpl_buf)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTDECODE, NULL, "can't deserialize datatype")

    /* Finish setting up dataset struct */
    if((dset->dapl_id = H5Pcopy(dapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dapl");

    ret_value = (void *)dset;

done:
    /* Free memory */
    type_buf = H5MM_xfree(type_buf);
    space_buf = H5MM_xfree(space_buf);
    dcpl_buf = H5MM_xfree(dcpl_buf);

    /* Close target group */
    if(target_grp && H5VL_daosm_group_close(target_grp, dxpl_id, req) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, NULL, "can't close group")

    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    if(NULL == ret_value)
        /* Close dataset */
        if(dset && H5VL_daosm_dataset_close(dset, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, NULL, "can't close dataset")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_dataset_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_sel_to_recx_iov
 *
 * Purpose:     Given a dataspace with a selection and the datatype
 *              (element) size, build a list of DAOS-M records (recxs)
 *              and/or scatter/gather list I/O vectors (sg_iovs). *recxs
 *              and *sg_iovs should, if requested, point to a (probably
 *              statically allocated) single element.  Does not release
 *              buffers on error.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              December, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_sel_to_recx_iov(H5S_t *space, size_t type_size, void *buf,
    daos_recx_t **recxs, daos_iov_t **sg_iovs, size_t *list_nused)
{
    H5S_sel_iter_t sel_iter;    /* Selection iteration info */
    hbool_t sel_iter_init = FALSE;      /* Selection iteration info has been initialized */
    size_t nseq;
    size_t nelem;
    hsize_t off[H5VL_DAOSM_SEQ_LIST_LEN];
    size_t len[H5VL_DAOSM_SEQ_LIST_LEN];
    size_t buf_len = 1;
    void *vp_ret;
    size_t szi;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(recxs || sg_iovs);
    HDassert(!recxs || *recxs);
    HDassert(!sg_iovs || *sg_iovs);
    HDassert(list_nused);

    /* Initialize list_nused */
    *list_nused = 0;

    /* Initialize selection iterator  */
    if(H5S_select_iter_init(&sel_iter, space, (size_t)1) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator")
    sel_iter_init = TRUE;       /* Selection iteration info has been initialized */

    /* Generate sequences from the file space until finished */
    do {
        /* Get the sequences of bytes */
        if(H5S_SELECT_GET_SEQ_LIST(space, 0, &sel_iter, (size_t)H5VL_DAOSM_SEQ_LIST_LEN, (size_t)-1, &nseq, &nelem, off, len) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "sequence length generation failed")

        /* Make room for sequences in recxs */
        if((buf_len == 1) && (nseq > 1)) {
            if(recxs)
                if(NULL == (*recxs = (daos_recx_t *)H5MM_malloc(H5VL_DAOSM_SEQ_LIST_LEN * sizeof(daos_recx_t))))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate memory for records")
            if(sg_iovs)
                if(NULL == (*sg_iovs = (daos_iov_t *)H5MM_malloc(H5VL_DAOSM_SEQ_LIST_LEN * sizeof(daos_iov_t))))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate memory for sgl iovs")
            buf_len = H5VL_DAOSM_SEQ_LIST_LEN;
        } /* end if */
        else if(*list_nused + nseq > buf_len) {
            if(recxs) {
                if(NULL == (vp_ret = H5MM_realloc(*recxs, 2 * buf_len * sizeof(daos_recx_t))))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't reallocate memory for records")
                *recxs = (daos_recx_t *)vp_ret;
            } /* end if */
            if(sg_iovs) {
                if(NULL == (vp_ret = H5MM_realloc(*sg_iovs, 2 * buf_len * sizeof(daos_iov_t))))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't reallocate memory for sgls")
                *sg_iovs = (daos_iov_t *)vp_ret;
            } /* end if */
            buf_len *= 2;
        } /* end if */
        HDassert(*list_nused + nseq <= buf_len);

        /* Copy offsets/lengths to recxs and sg_iovs */
        for(szi = 0; szi < nseq; szi++) {
            if(recxs) {
                (*recxs)[szi + *list_nused].rx_rsize = (uint64_t)type_size;
                (*recxs)[szi + *list_nused].rx_idx = (uint64_t)off[szi];
                (*recxs)[szi + *list_nused].rx_nr = (uint64_t)len[szi];
            } /* end if */
            if(sg_iovs)
                daos_iov_set(&(*sg_iovs)[szi + *list_nused],
                        (uint8_t *)buf + (off[szi] * type_size),
                        (daos_size_t)len[szi] * (daos_size_t)type_size);
        } /* end for */
        *list_nused += nseq;
    } while(nseq == H5VL_DAOSM_SEQ_LIST_LEN);

done:
    /* Release selection iterator */
    if(sel_iter_init && H5S_SELECT_ITER_RELEASE(&sel_iter) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_sel_to_recx_iov() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_dataset_read
 *
 * Purpose:     Reads raw data from a dataset into a buffer.
 *`
 * Return:      Success:        0
 *              Failure:        -1, dataset not read.
 *
 * Programmer:  Neil Fortner
 *              November, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_dataset_read(void *_dset, hid_t H5_ATTR_UNUSED mem_type_id,
    hid_t mem_space_id, hid_t file_space_id, hid_t H5_ATTR_UNUSED dxpl_id,
    void *buf, void H5_ATTR_UNUSED **req)
{
    H5VL_daosm_dset_t *dset = (H5VL_daosm_dset_t *)_dset;
    int ndims;
    hsize_t dim[H5S_MAX_RANK];
    H5S_t *space = NULL;
    uint64_t chunk_coords[H5S_MAX_RANK];
    daos_key_t dkey;
    daos_vec_iod_t iod;
    daos_recx_t recx;
    daos_recx_t *recxs = &recx;
    daos_sg_list_t sgl;
    daos_iov_t sg_iov;
    daos_iov_t *sg_iovs = &sg_iov;
    size_t tot_nseq;
    uint8_t dkey_buf[1 + H5S_MAX_RANK];
    uint8_t akey = H5VL_DAOSM_CHUNK_KEY;
    size_t type_size;
    uint8_t *p;
    int ret;
    int i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get dataspace extent */
    if((ndims = H5Sget_simple_extent_ndims(dset->space_id)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get number of dimensions")
    if(ndims != H5Sget_simple_extent_dims(dset->space_id, dim, NULL))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get dimensions")

    /* Get datatype size */
    if((type_size = H5Tget_size(dset->type_id)) == 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get datatype size")

    /* Encode dkey (chunk coordinates).  Prefix with '/' to avoid accidental
     * collisions with other d-keys in this object.  For now just 1 chunk,
     * starting at 0. */
    HDmemset(chunk_coords, 0, sizeof(chunk_coords)); //DSMINC
    p = dkey_buf;
    *p++ = (uint8_t)'/';
    for(i = 0; i < ndims; i++)
        UINT64ENCODE(p, chunk_coords[i])

    /* Set up dkey */
    daos_iov_set(&dkey, dkey_buf, (daos_size_t)(1 + ((size_t)ndims * sizeof(chunk_coords[0]))));

    /* Set up iod */
    HDmemset(&iod, 0, sizeof(iod));
    daos_iov_set(&iod.vd_name, (void *)&akey, (daos_size_t)(sizeof(akey)));
    daos_csum_set(&iod.vd_kcsum, NULL, 0);

    /* Build recxs and sg_iovs */
    /* Get file dataspace object */
    if(NULL == (space = (H5S_t *)H5I_object((file_space_id == H5S_ALL)
            ? dset->space_id : file_space_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Check for memory space is H5S_ALL, use file space in this case */
    if(mem_space_id == H5S_ALL) {
        /* Calculate both recxs and sg_iovs at the same time from file space */
        if(H5VL_daosm_sel_to_recx_iov(space, type_size, buf, &recxs, &sg_iovs, &tot_nseq) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't generate sequence lists for DAOS I/O")
        iod.vd_nr = (unsigned)tot_nseq;
        sgl.sg_nr.num = (uint32_t)tot_nseq;
    } /* end if */
    else {
        /* Calculate recxs from file space */
        if(H5VL_daosm_sel_to_recx_iov(space, type_size, buf, &recxs, NULL, &tot_nseq) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't generate sequence lists for DAOS I/O")
        iod.vd_nr = (unsigned)tot_nseq;

        /* Get memory dataspace object */
        if(NULL == (space = (H5S_t *)H5I_object(mem_space_id)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

        /* Calculate sg_iovs from mem space */
        if(H5VL_daosm_sel_to_recx_iov(space, type_size, buf, NULL, &sg_iovs, &tot_nseq) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't generate sequence lists for DAOS I/O")
        sgl.sg_nr.num = (uint32_t)tot_nseq;
    } /* end else */

    /* Point iod and sgl to lists generated above */
    iod.vd_recxs = recxs;
    sgl.sg_iovs = sg_iovs;

    /* Read data from dataset */
    if(0 != (ret = daos_obj_fetch(dset->obj.obj_oh, dset->obj.item.file->epoch, &dkey, 1, &iod, &sgl, NULL /*maps*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data from dataset: %d", ret)

done:
    if(recxs != &recx)
        H5MM_free(recxs);
    if(sg_iovs != &sg_iov)
        H5MM_free(sg_iovs);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_dataset_read() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_dataset_write
 *
 * Purpose:     Writes raw data from a buffer into a dataset.
 *
 * Return:      Success:        0
 *              Failure:        -1, dataset not written.
 *
 * Programmer:  Neil Fortner
 *              November, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_dataset_write(void *_dset, hid_t H5_ATTR_UNUSED mem_type_id,
    hid_t mem_space_id, hid_t file_space_id, hid_t H5_ATTR_UNUSED dxpl_id,
    const void *buf, void H5_ATTR_UNUSED **req)
{
    H5VL_daosm_dset_t *dset = (H5VL_daosm_dset_t *)_dset;
    int ndims;
    hsize_t dim[H5S_MAX_RANK];
    H5S_t *space = NULL;
    uint64_t chunk_coords[H5S_MAX_RANK];
    daos_key_t dkey;
    daos_vec_iod_t iod;
    daos_recx_t recx;
    daos_recx_t *recxs = &recx;
    daos_sg_list_t sgl;
    daos_iov_t sg_iov;
    daos_iov_t *sg_iovs = &sg_iov;
    size_t tot_nseq;
    uint8_t dkey_buf[1 + H5S_MAX_RANK];
    uint8_t akey = H5VL_DAOSM_CHUNK_KEY;
    size_t type_size;
    uint8_t *p;
    int ret;
    int i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Check for write access */
    if(!(dset->obj.item.file->flags & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "no write intent on file")

    /* Get dataspace extent */
    if((ndims = H5Sget_simple_extent_ndims(dset->space_id)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get number of dimensions")
    if(ndims != H5Sget_simple_extent_dims(dset->space_id, dim, NULL))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get dimensions")

    /* Get datatype size */
    if((type_size = H5Tget_size(dset->type_id)) == 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get datatype size")

    /* Encode dkey (chunk coordinates).  Prefix with '/' to avoid accidental
     * collisions with other d-keys in this object.  For now just 1 chunk,
     * starting at 0. */
    HDmemset(chunk_coords, 0, sizeof(chunk_coords)); //DSMINC
    p = dkey_buf;
    *p++ = (uint8_t)'/';
    for(i = 0; i < ndims; i++)
        UINT64ENCODE(p, chunk_coords[i])

    /* Set up dkey */
    daos_iov_set(&dkey, dkey_buf, (daos_size_t)(1 + ((size_t)ndims * sizeof(chunk_coords[0]))));

    /* Set up iod */
    HDmemset(&iod, 0, sizeof(iod));
    daos_iov_set(&iod.vd_name, (void *)&akey, (daos_size_t)(sizeof(akey)));
    daos_csum_set(&iod.vd_kcsum, NULL, 0);

    /* Build recxs and sg_iovs */
    /* Get file dataspace object */
    if(NULL == (space = (H5S_t *)H5I_object((file_space_id == H5S_ALL)
            ? dset->space_id : file_space_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Check for memory space is H5S_ALL, use file space in this case */
    if(mem_space_id == H5S_ALL) {
        /* Calculate both recxs and sg_iovs at the same time from file space */
        if(H5VL_daosm_sel_to_recx_iov(space, type_size, (void *)buf, &recxs, &sg_iovs, &tot_nseq) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't generate sequence lists for DAOS I/O")
        iod.vd_nr = (unsigned)tot_nseq;
        sgl.sg_nr.num = (uint32_t)tot_nseq;
    } /* end if */
    else {
        /* Calculate recxs from file space */
        if(H5VL_daosm_sel_to_recx_iov(space, type_size, (void *)buf, &recxs, NULL, &tot_nseq) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't generate sequence lists for DAOS I/O")
        iod.vd_nr = (unsigned)tot_nseq;

        /* Get memory dataspace object */
        if(NULL == (space = (H5S_t *)H5I_object(mem_space_id)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

        /* Calculate sg_iovs from mem space */
        if(H5VL_daosm_sel_to_recx_iov(space, type_size, (void *)buf, NULL, &sg_iovs, &tot_nseq) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't generate sequence lists for DAOS I/O")
        sgl.sg_nr.num = (uint32_t)tot_nseq;
    } /* end else */

    /* Point iod and sgl to lists generated above */
    iod.vd_recxs = recxs;
    sgl.sg_iovs = sg_iovs;

    /* Write data to dataset */
    if(0 != (ret = daos_obj_update(dset->obj.obj_oh, dset->obj.item.file->epoch, &dkey, 1, &iod, &sgl, NULL /*event*/)))
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data to dataset: %d", ret)

done:
    if(recxs != &recx)
        H5MM_free(recxs);
    if(sg_iovs != &sg_iov)
        H5MM_free(sg_iovs);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_dataset_write() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_dataset_get
 *
 * Purpose:     Gets certain information about a dataset
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              February, 2017
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_daosm_dataset_get(void *_dset, H5VL_dataset_get_t get_type, 
    hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req, va_list arguments)
{
    H5VL_daosm_dset_t *dset = (H5VL_daosm_dset_t *)_dset;
    herr_t       ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (get_type) {
        case H5VL_DATASET_GET_DCPL:
            {
                hid_t *plist_id = va_arg(arguments, hid_t *);

                /* Retrieve the file's access property list */
                if((*plist_id = H5Pcopy(dset->dcpl_id)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get dset creation property list")

                break;
            }
        case H5VL_DATASET_GET_DAPL:
            {
                hid_t *plist_id = va_arg(arguments, hid_t *);

                /* Retrieve the file's access property list */
                if((*plist_id = H5Pcopy(dset->dapl_id)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get dset access property list")

                break;
            }
        case H5VL_DATASET_GET_SPACE:
            {
                hid_t *ret_id = va_arg(arguments, hid_t *);

                if((*ret_id = H5Scopy(dset->space_id)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get dataspace ID of dataset");
                break;
            }
        case H5VL_DATASET_GET_SPACE_STATUS:
            {
                H5D_space_status_t *allocation = va_arg(arguments, H5D_space_status_t *);

                *allocation = H5D_SPACE_STATUS_NOT_ALLOCATED;
                break;
            }
        case H5VL_DATASET_GET_TYPE:
            {
                hid_t *ret_id = va_arg(arguments, hid_t *);

                if((*ret_id = H5Tcopy(dset->type_id)) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of dataset")
                break;
            }
        case H5VL_DATASET_GET_STORAGE_SIZE:
        case H5VL_DATASET_GET_OFFSET:
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from dataset")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_dataset_get() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_dataset_close
 *
 * Purpose:     Closes a daos-m HDF5 dataset.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              November, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_dataset_close(void *_dset, hid_t H5_ATTR_UNUSED dxpl_id,
    void H5_ATTR_UNUSED **req)
{
    H5VL_daosm_dset_t *dset = (H5VL_daosm_dset_t *)_dset;
    daos_handle_t hdl_inval = DAOS_HDL_INVAL;
    int ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(dset);

    if(--dset->obj.item.rc == 0) {
        /* Free dataset data structures */
        if(HDmemcmp(&dset->obj.obj_oh, &hdl_inval, sizeof(hdl_inval)))
            if(0 != (ret = daos_obj_close(dset->obj.obj_oh, NULL /*event*/)))
                HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't close dataset DAOS object: %d", ret)
        if(dset->type_id != FAIL && H5I_dec_app_ref(dset->type_id) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTDEC, FAIL, "failed to close datatype")
        if(dset->space_id != FAIL && H5I_dec_app_ref(dset->space_id) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTDEC, FAIL, "failed to close dataspace")
        if(dset->dcpl_id != FAIL && H5I_dec_app_ref(dset->dcpl_id) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTDEC, FAIL, "failed to close plist")
        if(dset->dapl_id != FAIL && H5I_dec_app_ref(dset->dapl_id) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTDEC, FAIL, "failed to close plist")
        dset = H5FL_FREE(H5VL_daosm_dset_t, dset);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_dataset_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_datatype_close
 *
 * Purpose:     Closes a daos-m HDF5 datatype.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              February, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_datatype_close(void H5_ATTR_UNUSED *_dtype,
    hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(0 && "Not implemented (should not be called)");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_datatype_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_object_close
 *
 * Purpose:     Closes a daos-m HDF5 object.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              February, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_object_close(void *_obj, hid_t dxpl_id, void **req)
{
    H5VL_daosm_obj_t *obj = (H5VL_daosm_obj_t *)_obj;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(obj);
    HDassert(obj->item.type == H5I_GROUP || obj->item.type == H5I_DATASET
            || obj->item.type == H5I_DATATYPE);

    /* Call type's close function */
    if(obj->item.type == H5I_GROUP) {
        if(H5VL_daosm_group_close(obj, dxpl_id, req))
            HGOTO_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close group")
    } /* end if */
    else if(obj->item.type == H5I_DATASET) {
        if(H5VL_daosm_dataset_close(obj, dxpl_id, req))
            HGOTO_ERROR(H5E_DATASET, H5E_CLOSEERROR, FAIL, "can't close dataset")
    } /* end if */
    else if(obj->item.type == H5I_DATATYPE) {
        if(H5VL_daosm_datatype_close(obj, dxpl_id, req))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CLOSEERROR, FAIL, "can't close datatype")
    } /* end if */
    else
        HDassert(0 && "Invalid object type");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_object_close() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_attribute_create
 *
 * Purpose:     Sends a request to DAOS-M to create an attribute
 *
 * Return:      Success:        attribute object. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              February, 2017
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_daosm_attribute_create(void *_item, H5VL_loc_params_t loc_params,
    const char *name, hid_t acpl_id, hid_t H5_ATTR_UNUSED aapl_id,
    hid_t dxpl_id, void **req)
{
    H5VL_daosm_item_t *item = (H5VL_daosm_item_t *)_item; /* Change to item when BY_NAME (and obj_open) is supported DSMINC */
    H5VL_daosm_attr_t *attr = NULL;
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    size_t akey_len;
    hid_t type_id, space_id;
    daos_key_t dkey;
    char *type_key = NULL;
    char *space_key = NULL;
    daos_vec_iod_t iod[2];
    daos_recx_t recx[2];
    daos_sg_list_t sgl[2];
    daos_iov_t sg_iov[2];
    size_t type_size = 0;
    size_t space_size = 0;
    void *type_buf = NULL;
    void *space_buf = NULL;
    char attr_key[] = H5VL_DAOSM_ATTR_KEY;
    int ret;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Check for write access */
    if(!(item->file->flags & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "no write intent on file")

    /* Get the acpl plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(acpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

    /* get creation properties */
    if(H5P_get(plist, H5VL_PROP_ATTR_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for datatype id")
    if(H5P_get(plist, H5VL_PROP_ATTR_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for space id")

    /* Allocate the attribute object that is returned to the user */
    if(NULL == (attr = H5FL_CALLOC(H5VL_daosm_attr_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate DAOS-M dataset struct")
    attr->item.type = H5I_ATTR;
    attr->item.file = item->file;
    attr->item.rc = 1;
    attr->type_id = FAIL;
    attr->space_id = FAIL;

    /* Check loc_params - only SELF currently supported */
    if(loc_params.type != H5VL_OBJECT_BY_SELF)
        HGOTO_ERROR(H5E_ATTR, H5E_UNSUPPORTED, NULL, "attribute create by name unsupported")

    /* Set attribute parent object */
    if(item->type == H5I_FILE)
        attr->parent = (H5VL_daosm_obj_t *)((H5VL_daosm_file_t *)item)->root_grp;
    else
        attr->parent = (H5VL_daosm_obj_t *)item;
    attr->parent->item.rc++;

    /* Encode datatype */
    if(H5Tencode(type_id, NULL, &type_size) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "can't determine serialized length of datatype")
    if(NULL == (type_buf = H5MM_malloc(type_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized datatype")
    if(H5Tencode(type_id, type_buf, &type_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTENCODE, NULL, "can't serialize datatype")

    /* Encode dataspace */
    if(H5Sencode(space_id, NULL, &space_size) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "can't determine serialized length of dataaspace")
    if(NULL == (space_buf = H5MM_malloc(space_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized dataaspace")
    if(H5Sencode(space_id, space_buf, &space_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTENCODE, NULL, "can't serialize dataaspace")

    /* Set up operation to write datatype and dataspace to attribute */
    /* Set up dkey */
    daos_iov_set(&dkey, attr_key, (daos_size_t)(sizeof(attr_key) - 1));

    /* Create akey strings (prefix "S-", "T-") */
    akey_len = HDstrlen(name) + 2;
    if(NULL == (type_key = (char *)H5MM_malloc(akey_len + 1)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for akey")
    if(NULL == (space_key = (char *)H5MM_malloc(akey_len + 1)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for akey")
    type_key[0] = 'T';
    type_key[1] = '-';
    space_key[0] = 'S';
    space_key[1] = '-';
    (void)HDstrcpy(type_key + 2, name);
    (void)HDstrcpy(space_key + 2, name);

    /* Set up recx */
    recx[0].rx_rsize = (uint64_t)type_size;
    recx[0].rx_idx = (uint64_t)0;
    recx[0].rx_nr = (uint64_t)1;
    recx[1].rx_rsize = (uint64_t)space_size;
    recx[1].rx_idx = (uint64_t)0;
    recx[1].rx_nr = (uint64_t)1;

    /* Set up iod */
    HDmemset(iod, 0, sizeof(iod));
    daos_iov_set(&iod[0].vd_name, (void *)type_key, (daos_size_t)akey_len);
    daos_csum_set(&iod[0].vd_kcsum, NULL, 0);
    iod[0].vd_nr = 1u;
    iod[0].vd_recxs = &recx[0];
    daos_iov_set(&iod[1].vd_name, (void *)space_key, (daos_size_t)akey_len);
    daos_csum_set(&iod[1].vd_kcsum, NULL, 0);
    iod[1].vd_nr = 1u;
    iod[1].vd_recxs = &recx[1];

    /* Set up sgl */
    daos_iov_set(&sg_iov[0], type_buf, (daos_size_t)type_size);
    sgl[0].sg_nr.num = 1;
    sgl[0].sg_iovs = &sg_iov[0];
    daos_iov_set(&sg_iov[1], space_buf, (daos_size_t)space_size);
    sgl[1].sg_nr.num = 1;
    sgl[1].sg_iovs = &sg_iov[1];

    /* Write attribute metadata to parent object */
    if(0 != (ret = daos_obj_update(attr->parent->obj_oh, attr->parent->item.file->epoch, &dkey, 2, iod, sgl, NULL /*event*/)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "can't write attribute metadata: %d", ret)

    /* Finish setting up attribute struct */
    if(NULL == (attr->name = H5MM_strdup(name)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't copy attribute name")
    if((attr->type_id = H5Tcopy(type_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy datatype")
    if((attr->space_id = H5Scopy(space_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dataspace")
    if(H5Sselect_all(attr->space_id) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, NULL, "can't change selection")

    ret_value = (void *)attr;

done:
    /* Free memory */
    type_buf = H5MM_xfree(type_buf);
    space_buf = H5MM_xfree(space_buf);
    type_key = (char *)H5MM_xfree(type_key);
    space_key = (char *)H5MM_xfree(space_key);

    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    /* Destroy DAOS object if created before failure DSMINC */
    if(NULL == ret_value)
        /* Close attribute */
        if(attr && H5VL_daosm_attribute_close(attr, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, NULL, "can't close attribute")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_attribute_create() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_attribute_open
 *
 * Purpose:     Sends a request to DAOS-M to open an attribute
 *
 * Return:      Success:        attribute object. 
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              February, 2017
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_daosm_attribute_open(void *_item, H5VL_loc_params_t loc_params,
    const char *name, hid_t H5_ATTR_UNUSED aapl_id, hid_t dxpl_id, void **req)
{
    H5VL_daosm_item_t *item = (H5VL_daosm_item_t *)_item;
    H5VL_daosm_attr_t *attr = NULL;
    size_t akey_len;
    daos_key_t dkey;
    char *type_key = NULL;
    char *space_key = NULL;
    daos_vec_iod_t iod[2];
    daos_recx_t recx[2];
    daos_sg_list_t sgl[2];
    daos_iov_t sg_iov[2];
    void *type_buf = NULL;
    void *space_buf = NULL;
    char attr_key[] = H5VL_DAOSM_ATTR_KEY;
    int ret;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Allocate the attribute object that is returned to the user */
    if(NULL == (attr = H5FL_CALLOC(H5VL_daosm_attr_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate DAOS-M dataset struct")
    attr->item.type = H5I_ATTR;
    attr->item.file = item->file;
    attr->item.rc = 1;
    attr->type_id = FAIL;
    attr->space_id = FAIL;

    /* Check loc_params - only SELF currently supported */
    if(loc_params.type != H5VL_OBJECT_BY_SELF)
        HGOTO_ERROR(H5E_ATTR, H5E_UNSUPPORTED, NULL, "attribute create by name unsupported")

    /* Set attribute parent object */
    if(item->type == H5I_FILE)
        attr->parent = (H5VL_daosm_obj_t *)((H5VL_daosm_file_t *)item)->root_grp;
    else
        attr->parent = (H5VL_daosm_obj_t *)item;
    attr->parent->item.rc++;

    /* Set up operation to write datatype and dataspace to attribute */
    /* Set up dkey */
    daos_iov_set(&dkey, attr_key, (daos_size_t)(sizeof(attr_key) - 1));

    /* Create akey strings (prefix "S-", "T-") */
    akey_len = HDstrlen(name) + 2;
    if(NULL == (type_key = (char *)H5MM_malloc(akey_len + 1)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for akey")
    if(NULL == (space_key = (char *)H5MM_malloc(akey_len + 1)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for akey")
    type_key[0] = 'T';
    type_key[1] = '-';
    space_key[0] = 'S';
    space_key[1] = '-';
    (void)HDstrcpy(type_key + 2, name);
    (void)HDstrcpy(space_key + 2, name);

    /* Set up recx */
    recx[0].rx_rsize = DAOS_REC_ANY;
    recx[0].rx_idx = (uint64_t)0;
    recx[0].rx_nr = (uint64_t)1;
    recx[1].rx_rsize = DAOS_REC_ANY;
    recx[1].rx_idx = (uint64_t)0;
    recx[1].rx_nr = (uint64_t)1;

    /* Set up iod */
    HDmemset(iod, 0, sizeof(iod));
    daos_iov_set(&iod[0].vd_name, (void *)type_key, (daos_size_t)akey_len);
    daos_csum_set(&iod[0].vd_kcsum, NULL, 0);
    iod[0].vd_nr = 1u;
    iod[0].vd_recxs = &recx[0];
    daos_iov_set(&iod[1].vd_name, (void *)space_key, (daos_size_t)akey_len);
    daos_csum_set(&iod[1].vd_kcsum, NULL, 0);
    iod[1].vd_nr = 1u;
    iod[1].vd_recxs = &recx[1];

    /* Read attribute metadata sizes from parent object */
    if(0 != (ret = daos_obj_fetch(attr->parent->obj_oh, attr->parent->item.file->epoch, &dkey, 2, iod, NULL, NULL /*maps*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDECODE, NULL, "can't read attribute metadata sizes: %d", ret)

    if(recx[0].rx_rsize == (uint64_t)0 || recx[1].rx_rsize == (uint64_t)0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, NULL, "attribute not found")

    /* Allocate buffers for datatype and dataspace */
    if(NULL == (type_buf = H5MM_malloc(recx[0].rx_rsize)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized datatype")
    if(NULL == (space_buf = H5MM_malloc(recx[1].rx_rsize)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized dataaspace")

    /* Set up sgl */
    daos_iov_set(&sg_iov[0], type_buf, (daos_size_t)recx[0].rx_rsize);
    sgl[0].sg_nr.num = 1;
    sgl[0].sg_iovs = &sg_iov[0];
    daos_iov_set(&sg_iov[1], space_buf, (daos_size_t)recx[1].rx_rsize);
    sgl[1].sg_nr.num = 1;
    sgl[1].sg_iovs = &sg_iov[1];

    /* Read attribute metadata from parent object */
    if(0 != (ret = daos_obj_fetch(attr->parent->obj_oh, attr->parent->item.file->epoch, &dkey, 2, iod, sgl, NULL /*maps*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDECODE, NULL, "can't read attribute metadata: %d", ret)

    /* Decode datatype and dataspace */
    if((attr->type_id = H5Tdecode(type_buf)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTDECODE, NULL, "can't deserialize datatype")
    if((attr->space_id = H5Sdecode(space_buf)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTDECODE, NULL, "can't deserialize datatype")
    if(H5Sselect_all(attr->space_id) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, NULL, "can't change selection")

    /* Finish setting up dataset struct */
    if(NULL == (attr->name = H5MM_strdup(name)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't copy attribute name")

    ret_value = (void *)attr;

done:
    /* Free memory */
    type_buf = H5MM_xfree(type_buf);
    space_buf = H5MM_xfree(space_buf);
    type_key = (char *)H5MM_xfree(type_key);
    space_key = (char *)H5MM_xfree(space_key);

    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    /* Destroy DAOS object if created before failure DSMINC */
    if(NULL == ret_value)
        /* Close attribute */
        if(attr && H5VL_daosm_attribute_close(attr, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, NULL, "can't close attribute")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_attribute_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_attribute_read
 *
 * Purpose:     Reads raw data from an attribute into a buffer.
 *
 * Return:      Success:        0
 *              Failure:        -1, attribute not read.
 *
 * Programmer:  Neil Fortner
 *              February, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_attribute_read(void *_attr, hid_t mem_type_id, void *buf,
    hid_t dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_daosm_attr_t *attr = (H5VL_daosm_attr_t *)_attr;
    int ndims;
    hsize_t dim[H5S_MAX_RANK];
    size_t akey_len;
    daos_key_t dkey;
    char *akey = NULL;
    daos_vec_iod_t iod;
    daos_recx_t recx;
    daos_sg_list_t sgl;
    daos_iov_t sg_iov;
    char attr_key[] = H5VL_DAOSM_ATTR_KEY;
    size_t file_type_size;
    size_t mem_type_size;
    uint64_t attr_size;
    void *tconv_buf = NULL;
    void *bkg_buf = NULL;
    H5VL_daosm_tconv_reuse_t reuse = H5VL_DAOSM_TCONV_REUSE_NONE;
    hbool_t fill_bkg = FALSE;
    int ret;
    int i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get dataspace extent */
    if((ndims = H5Sget_simple_extent_ndims(attr->space_id)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get number of dimensions")
    if(ndims != H5Sget_simple_extent_dims(attr->space_id, dim, NULL))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get dimensions")

    /* Calculate attribute size */
    attr_size = (uint64_t)1;
    for(i = 0; i < ndims; i++)
        attr_size *= (uint64_t)dim[i];

    /* Check for type conversion */
    if(H5VL_daosm_tconv_init(attr->type_id, &file_type_size, mem_type_id, &mem_type_size, (size_t)attr_size, &tconv_buf, &bkg_buf, &reuse, &fill_bkg) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't initialize type conversion")

    /* Reuse buffer as appropriate */
    if(reuse == H5VL_DAOSM_TCONV_REUSE_TCONV)
        tconv_buf = buf;
    else if(reuse == H5VL_DAOSM_TCONV_REUSE_BKG)
        bkg_buf = buf;

    /* Fill background buffer if necessary */
    if(fill_bkg && (bkg_buf != buf))
        (void)HDmemcpy(bkg_buf, buf, (size_t)attr_size * mem_type_size);

    /* Set up operation to read data */
    /* Set up dkey */
    daos_iov_set(&dkey, attr_key, (daos_size_t)(sizeof(attr_key) - 1));

    /* Create akey string (prefix "V-") */
    akey_len = HDstrlen(attr->name) + 2;
    if(NULL == (akey = (char *)H5MM_malloc(akey_len + 1)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate buffer for akey")
    akey[0] = 'V';
    akey[1] = '-';
    (void)HDstrcpy(akey + 2, attr->name);

    /* Set up recx */
    recx.rx_rsize = (uint64_t)file_type_size;
    recx.rx_idx = (uint64_t)0;
    recx.rx_nr = attr_size;

    /* Set up iod */
    HDmemset(&iod, 0, sizeof(iod));
    daos_iov_set(&iod.vd_name, (void *)akey, (daos_size_t)akey_len);
    daos_csum_set(&iod.vd_kcsum, NULL, 0);
    iod.vd_nr = 1u;
    iod.vd_recxs = &recx;

    /* Set up sgl */
    daos_iov_set(&sg_iov, tconv_buf ? tconv_buf : buf, (daos_size_t)(attr_size * (uint64_t)file_type_size));
    sgl.sg_nr.num = 1;
    sgl.sg_iovs = &sg_iov;

    /* Read data from attribute */
    if(0 != (ret = daos_obj_fetch(attr->parent->obj_oh, attr->item.file->epoch, &dkey, 1, &iod, &sgl, NULL /*maps*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_ATTR, H5E_READERROR, FAIL, "can't read data from attribute: %d", ret)

    /* Perform type conversion if necessary */
    if(tconv_buf) {
        /* Type conversion */
        if(H5Tconvert(attr->type_id, mem_type_id, attr_size, tconv_buf, bkg_buf, dxpl_id) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTCONVERT, FAIL, "can't perform type conversion")

        /* Copy to user's buffer if necessary */
        if(buf != tconv_buf)
            (void)HDmemcpy(buf, tconv_buf, (size_t)attr_size * mem_type_size);
    } /* end if */

done:
    /* Free memory */
    akey = (char *)H5MM_xfree(akey);
    if(tconv_buf && (tconv_buf != buf))
        H5MM_free(tconv_buf);
    if(bkg_buf &&(bkg_buf != buf))
        H5MM_free(buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_attribute_read() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_attribute_write
 *
 * Purpose:     Writes raw data from a buffer into an attribute.
 *
 * Return:      Success:        0
 *              Failure:        -1, attribute not written.
 *
 * Programmer:  Neil Fortner
 *              February, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_attribute_write(void *_attr, hid_t H5_ATTR_UNUSED mem_type_id,
    const void *buf, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_daosm_attr_t *attr = (H5VL_daosm_attr_t *)_attr;
    int ndims;
    hsize_t dim[H5S_MAX_RANK];
    size_t akey_len;
    daos_key_t dkey;
    char *akey = NULL;
    daos_vec_iod_t iod;
    daos_recx_t recx;
    daos_sg_list_t sgl;
    daos_iov_t sg_iov;
    char attr_key[] = H5VL_DAOSM_ATTR_KEY;
    size_t file_type_size;
    size_t mem_type_size;
    uint64_t attr_size;
    void *tconv_buf = NULL;
    void *bkg_buf = NULL;
    hbool_t fill_bkg = FALSE;
    int ret;
    int i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Check for write access */
    if(!(attr->item.file->flags & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "no write intent on file")

    /* Get dataspace extent */
    if((ndims = H5Sget_simple_extent_ndims(attr->space_id)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get number of dimensions")
    if(ndims != H5Sget_simple_extent_dims(attr->space_id, dim, NULL))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get dimensions")

    /* Calculate attribute size */
    attr_size = (uint64_t)1;
    for(i = 0; i < ndims; i++)
        attr_size *= (uint64_t)dim[i];

    /* Check for type conversion */
    if(H5VL_daosm_tconv_init(mem_type_id, &mem_type_size, attr->type_id, &file_type_size, (size_t)attr_size, &tconv_buf, &bkg_buf, NULL, &fill_bkg) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't initialize type conversion")

    /* Set up operation to write data */
    /* Set up dkey */
    daos_iov_set(&dkey, attr_key, (daos_size_t)(sizeof(attr_key) - 1));

    /* Create akey string (prefix "V-") */
    akey_len = HDstrlen(attr->name) + 2;
    if(NULL == (akey = (char *)H5MM_malloc(akey_len + 1)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate buffer for akey")
    akey[0] = 'V';
    akey[1] = '-';
    (void)HDstrcpy(akey + 2, attr->name);

    /* Set up recx */
    recx.rx_rsize = (uint64_t)file_type_size;
    recx.rx_idx = (uint64_t)0;
    recx.rx_nr = attr_size;

    /* Set up iod */
    HDmemset(&iod, 0, sizeof(iod));
    daos_iov_set(&iod.vd_name, (void *)akey, (daos_size_t)akey_len);
    daos_csum_set(&iod.vd_kcsum, NULL, 0);
    iod.vd_nr = 1u;
    iod.vd_recxs = &recx;

    /* Set up constant sgl info */
    sgl.sg_nr.num = 1;
    sgl.sg_iovs = &sg_iov;

    /* Check for type conversion */
    if(tconv_buf) {
        /* Check if we need to fill background buffer */
        if(fill_bkg) {
            HDassert(bkg_buf);

            /* Read data from attribute to background buffer */
            daos_iov_set(&sg_iov, bkg_buf, (daos_size_t)(attr_size * (uint64_t)file_type_size));

            if(0 != (ret = daos_obj_fetch(attr->parent->obj_oh, attr->item.file->epoch, &dkey, 1, &iod, &sgl, NULL /*maps*/, NULL /*event*/)))
                HGOTO_ERROR(H5E_ATTR, H5E_READERROR, FAIL, "can't read data from attribute: %d", ret)
        } /* end if */

        /* Copy data to type conversion buffer */
        (void)HDmemcpy(tconv_buf, buf, (size_t)attr_size * mem_type_size);

        /* Perform type conversion */
        if(H5Tconvert(mem_type_id, attr->type_id, attr_size, tconv_buf, bkg_buf, dxpl_id) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTCONVERT, FAIL, "can't perform type conversion")

        /* Set sgl to write from tconv_buf */
        daos_iov_set(&sg_iov, tconv_buf, (daos_size_t)(attr_size * (uint64_t)file_type_size));
    } /* end if */
    else
        /* Set sgl to write from buf */
        daos_iov_set(&sg_iov, (void *)buf, (daos_size_t)(attr_size * (uint64_t)file_type_size));

    /* Write data to attribute */
    if(0 != (ret = daos_obj_update(attr->parent->obj_oh, attr->item.file->epoch, &dkey, 1, &iod, &sgl, NULL /*event*/)))
        HGOTO_ERROR(H5E_ATTR, H5E_WRITEERROR, FAIL, "can't write data to attribute: %d", ret)

done:
    /* Free memory */
    akey = (char *)H5MM_xfree(akey);
    tconv_buf = H5MM_xfree(tconv_buf);
    bkg_buf = H5MM_xfree(bkg_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_attribute_write() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_attribute_close
 *
 * Purpose:     Closes a daos-m HDF5 attribute.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              February, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_daosm_attribute_close(void *_attr, hid_t dxpl_id, void **req)
{
    H5VL_daosm_attr_t *attr = (H5VL_daosm_attr_t *)_attr;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(attr);

    if(--attr->item.rc == 0) {
        /* Free attribute data structures */
        if(attr->parent && H5VL_daosm_object_close(attr->parent, dxpl_id, req))
            HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "can't close parent object")
        H5MM_xfree(attr->name);
        if(attr->type_id != FAIL && H5I_dec_app_ref(attr->type_id) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTDEC, FAIL, "failed to close datatype")
        if(attr->space_id != FAIL && H5I_dec_app_ref(attr->space_id) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTDEC, FAIL, "failed to close dataspace")
        attr = H5FL_FREE(H5VL_daosm_attr_t, attr);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_attribute_close() */

