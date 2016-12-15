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
 * Programmer:  Neil Fortner <nfortne2@hdfgroup.gov>
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

/* Macros */
#define H5VL_DAOSM_INT_MD_KEY "Internal Metadata"
#define H5VL_DAOSM_MAX_OID_KEY "Max OID"
#define H5VL_DAOSM_CPL_KEY "Creation Property List"
#define H5VL_DAOSM_LINK_KEY "Link"
#define H5VL_DAOSM_TYPE_KEY "Datatype"
#define H5VL_DAOSM_SPACE_KEY "Dataspace"
#define H5VL_DAOSM_CHUNK_KEY 0u
#define H5VL_DAOSM_SEQ_LIST_LEN 128

/* Prototypes */
static void *H5VL_daosm_fapl_copy(const void *_old_fa);
static herr_t H5VL_daosm_fapl_free(void *_fa);

/* File callbacks */
static void *H5VL_daosm_file_create(const char *name, unsigned flags,
    hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id, void **req);
static void *H5VL_daosm_file_open(const char *name, unsigned flags,
    hid_t fapl_id, hid_t dxpl_id, void **req);
//static herr_t H5VL_iod_file_get(void *file, H5VL_file_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_daosm_file_close(void *file, hid_t dxpl_id, void **req);

/* Group callbacks */
static void *H5VL_daosm_group_create(void *_obj, H5VL_loc_params_t loc_params,
    const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req);
static void *H5VL_daosm_group_open(void *_obj, H5VL_loc_params_t loc_params,
    const char *name, hid_t gapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_daosm_group_close(void *grp, hid_t dxpl_id, void **req);

/* Dataset callbacks */
static void *H5VL_daosm_dataset_create(void *obj, H5VL_loc_params_t loc_params,
    const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req);
static void *H5VL_daosm_dataset_open(void *obj, H5VL_loc_params_t loc_params,
    const char *name, hid_t dapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_daosm_dataset_read(void *dset, hid_t mem_type_id,
    hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, void *buf,
    void **req);
static herr_t H5VL_daosm_dataset_write(void *dset, hid_t mem_type_id,
    hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, const void *buf,
    void **req);
/*static herr_t H5VL_daosm_dataset_specific(void *_dset, H5VL_dataset_specific_t specific_type,
                                        hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_daosm_dataset_get(void *dset, H5VL_dataset_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);*/
static herr_t H5VL_daosm_dataset_close(void *dt, hid_t dxpl_id, void **req);

/* Helper routines */
static herr_t H5VL_daosm_write_max_oid(H5VL_daosm_file_t *file,
    daos_epoch_t epoch);
static herr_t H5VL_daosm_link_read(H5VL_daosm_group_t *grp, const char *name,
    size_t name_len, daos_epoch_t epoch, daos_obj_id_t *oid);
static herr_t H5VL_daosm_link_write(H5VL_daosm_group_t *grp, const char *name,
    size_t name_len, daos_epoch_t epoch, daos_obj_id_t oid);
static H5VL_daosm_group_t *H5VL_daosm_group_traverse(H5VL_daosm_obj_t *obj,
    const char *path, hid_t dxpl_id, void **req, daos_epoch_t epoch,
    const char **obj_name);
static void *H5VL_daosm_group_create_helper(H5VL_daosm_file_t *file,
    hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req,
    daos_epoch_t epoch);
static void *H5VL_daosm_group_open_helper(H5VL_daosm_file_t *file,
    daos_obj_id_t oid, hid_t gapl_id, hid_t dxpl_id, void **req,
    daos_epoch_t epoch);
static herr_t H5VL_daosm_sel_to_recx_iov(H5S_t *space, size_t type_size,
    void *buf, daos_recx_t **recxs, daos_iov_t **sg_iovs, size_t *list_nused);

/* DAOSM-specific file access properties */
typedef struct H5VL_daosm_fapl_t {
    MPI_Comm            comm;           /*communicator                  */
    MPI_Info            info;           /*file information              */
    uuid_t              pool_uuid;      /*pool uuid                     */
    char                *pool_grp;      /*pool group                    */
} H5VL_daosm_fapl_t;

/* Free list definitions */
H5FL_DEFINE(H5VL_daosm_file_t);
H5FL_DEFINE(H5VL_daosm_group_t);
H5FL_DEFINE(H5VL_daosm_dset_t);

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
        NULL,//H5VL_iod_attribute_create,              /* create */
        NULL,//H5VL_iod_attribute_open,                /* open */
        NULL,//H5VL_iod_attribute_read,                /* read */
        NULL,//H5VL_iod_attribute_write,               /* write */
        NULL,//H5VL_iod_attribute_get,                 /* get */
        NULL,//H5VL_iod_attribute_specific,            /* specific */
        NULL,                                   /* optional */
        NULL,//H5VL_iod_attribute_close                /* close */
    },
    {                                           /* dataset_cls */
        H5VL_daosm_dataset_create,              /* create */
        H5VL_daosm_dataset_open,                /* open */
        H5VL_daosm_dataset_read,                /* read */
        H5VL_daosm_dataset_write,               /* write */
        NULL,//H5VL_iod_dataset_get,                   /* get */
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
        NULL,                                   /* specific */
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
        NULL,//H5VL_iod_link_create,                   /* create */
        NULL,//H5VL_iod_link_copy,                     /* copy */
        NULL,//H5VL_iod_link_move,                     /* move */
        NULL,//H5VL_iod_link_get,                      /* get */
        NULL,//H5VL_iod_link_specific,                 /* specific */
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
    if(NULL == (fa.pool_grp = HDstrdup(pool_grp)))
        HGOTO_ERROR(H5E_PLIST, H5E_NOSPACE, FAIL, "can't copy pool group")

    ret_value = H5P_set_vol(plist, H5VL_DAOSM, &fa);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_daosm() */


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

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(old_fa->comm, old_fa->info, &new_fa->comm, &new_fa->info))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed")

    /*  Duplicate the pool group */
    if(NULL == (new_fa->pool_grp = HDstrdup(old_fa->pool_grp)))
        HGOTO_ERROR(H5E_PLIST, H5E_NOSPACE, NULL, "can't copy pool group")

    ret_value = new_fa;

done:
    if (NULL == ret_value) {
        /* cleanup */
        if (new_fa)
            H5MM_xfree(new_fa);
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
    assert(MPI_COMM_NULL!=fa->comm);
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
    daos_epoch_t epoch;
    hid_t trans_id;
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

    /* obtain the process rank from the communicator attached to the fapl ID */
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
    file->common.type = H5I_FILE;
    file->common.file = file;
    file->common.rc = 1;
    file->file_name = HDstrdup(name);
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
        if(0 != (ret = daos_pool_connect(fa->pool_uuid, NULL/*fa->pool_grp DSMINC*/, NULL /*pool_svc*/, DAOS_PC_RW, &file->poh, NULL /*&file->pool_info*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't connect to pool: %d", ret)

        /* Create the container for the file */
        if(0 != (ret = daos_cont_create(file->poh, file->uuid, NULL /*event*/))) {
            /* Check for failure due to the container already existing and
             * opening with H5F_ACC_TRUNC */
            if((ret == -(int)DER_EXIST) && (flags & H5F_ACC_TRUNC)) {
                /* Destroy and re-create container */
                if(0 != (ret = daos_cont_destroy(file->poh, file->uuid, 0, NULL /*event*/)))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, NULL, "can't destroy container: %d", ret)
                if(0 != (ret = daos_cont_create(file->poh, file->uuid, NULL /*event*/)))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, NULL, "can't create container: %d", ret)
            } /* end if */
            else
                HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, NULL, "can't create container: %d", ret)
        } /* end if */

        /* Open the container */
        if(0 != (ret = daos_cont_open(file->poh, file->uuid, DAOS_COO_RW, &file->coh, NULL /*&file->co_info*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't open container: %d", ret)

        /* Query the epoch */
        if(0 != (ret = daos_epoch_query(file->coh, &epoch_state, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't query epoch: %d", ret)

        /* Hold the epoch */
        epoch = epoch_state.es_hce + (daos_epoch_t)1;
        if(0 != (ret = daos_epoch_hold(file->coh, &epoch, NULL /*state*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't hold epoch: %d", ret)

        /* Create global metadata object */
        if(0 != (ret = daos_obj_declare(file->coh, gmd_oid, 0, NULL /*oa*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, NULL, "can't create global metadata object: %d", ret)

        /* Open global metadata object */
        if(0 != (ret = daos_obj_open(file->coh, gmd_oid, 0, DAOS_OO_RW, &file->glob_md_oh, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't open global metadata object: %d", ret)

        /* Create root group */
        if(NULL == (file->root_grp = (H5VL_daosm_group_t *)H5VL_daosm_group_create_helper(file, fcpl_id, H5P_GROUP_ACCESS_DEFAULT, dxpl_id, req, epoch)))
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
            bcast_buf_64[2] = file->root_grp->oid.lo;
            bcast_buf_64[3] = file->root_grp->oid.mid;
            bcast_buf_64[4] = file->root_grp->oid.hi;

            /* Add epoch to bcast_buf_64 */
            HDassert(sizeof(bcast_buf_64[5]) == sizeof(epoch));
            bcast_buf_64[5] = (uint64_t)epoch;

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
        HDassert(sizeof(bcast_buf_64[5]) == sizeof(epoch));
        epoch = (daos_epoch_t)bcast_buf_64[5];

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
        if(NULL == (file->root_grp = (H5VL_daosm_group_t *)H5VL_daosm_group_open_helper(file, root_grp_oid, H5P_GROUP_ACCESS_DEFAULT, dxpl_id, req, epoch)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't open root group")

        /* Handle pool_info and container_info DSMINC */

        /* Open global metadata object */
        if(0 != (ret = daos_obj_open(file->coh, gmd_oid, 0, DAOS_OO_RW, &file->glob_md_oh, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't open global metadata object: %d", ret)
    } /* end else */

    /* Determine if we want to acquire a transaction for the file creation */
    if(H5P_get(plist, H5VL_ACQUIRE_TR_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for trans id");

    if(FAIL == trans_id) {
        if(file->my_rank == 0)
            /* Transaction not requested, commit the epoch */
            if(0 != (ret = daos_epoch_commit(file->coh, epoch, NULL /*state*/, NULL /*event*/)))
                HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, NULL, "can't commit epoch: %d", ret)
    } /* end if */
    else {
        /* Transaction requested, do not commit the epoch - it is up to the
         * application */
        H5TR_t *tr = NULL;

        /* get the TR object */
        if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a TRANSACTION ID")

        tr->epoch = epoch;
        tr->file = file;
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
        if(file != NULL && H5VL_daosm_file_close(file, dxpl_id, req) < 0)
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
    daos_epoch_t epoch;
    hid_t trans_id;
    daos_iov_t glob;
    uint64_t bcast_buf_64[7];
    char *gh_buf = NULL;
    daos_obj_id_t gmd_oid = {0, 0, 0};
    daos_obj_id_t root_grp_oid;
    hbool_t must_bcast = FALSE;
    int ret;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* obtain the process rank from the communicator attached to the fapl ID */
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
    file->common.type = H5I_FILE;
    file->common.file = file;
    file->common.rc = 1;
    file->file_name = HDstrdup(name);
    file->flags = flags;
    if((file->fapl_id = H5Pcopy(fapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fapl")

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(fa->comm, fa->info, &file->comm, &file->info))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed")

    MPI_Comm_rank(fa->comm, &file->my_rank);
    MPI_Comm_size(fa->comm, &file->num_procs);

    /* Hash file name to create uuid */
    H5VL_daosm_hash128(name, &file->uuid);

    /* Generate oid for global metadata object */
    daos_obj_id_generate(&gmd_oid, DAOS_OC_TINY_RW);

    if(file->my_rank == 0) {
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
        if(0 != (ret = daos_pool_connect(fa->pool_uuid, NULL/*fa->pool_grp DSMINC*/, NULL /*pool_svc*/, DAOS_PC_RW, &file->poh, NULL /*&file->pool_info*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't connect to pool: %d", ret)

        /* Open the container */
        if(0 != (ret = daos_cont_open(file->poh, file->uuid, DAOS_COO_RW, &file->coh, NULL /*&file->co_info*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't open container: %d", ret)

        /* Query the epoch */
        if(0 != (ret = daos_epoch_query(file->coh, &epoch_state, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't query epoch: %d", ret)

        /* Hold the epoch */
        epoch = epoch_state.es_hce + (daos_epoch_t)1;
        if(0 != (ret = daos_epoch_hold(file->coh, &epoch, NULL /*state*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't hold epoch: %d", ret)

        /* Read from the HCE */
        epoch--;

        /* Generate root group ID */
        root_grp_oid.lo = 1; //DSMINC
        root_grp_oid.mid = 0; //DSMINC
        root_grp_oid.hi = 0; //DSMINC
        daos_obj_id_generate(&root_grp_oid, DAOS_OC_TINY_RW); //DSMINC

        /* Open global metadata object */
        if(0 != (ret = daos_obj_open(file->coh, gmd_oid, 0, DAOS_OO_RW, &file->glob_md_oh, NULL /*event*/)))
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
            HDassert(sizeof(bcast_buf_64[5]) == sizeof(epoch));
            bcast_buf_64[5] = (uint64_t)epoch;

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
        HDassert(sizeof(bcast_buf_64[5]) == sizeof(epoch));
        epoch = (daos_epoch_t)bcast_buf_64[5];

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
        if(0 != (ret = daos_obj_open(file->coh, gmd_oid, 0, DAOS_OO_RW, &file->glob_md_oh, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't open global metadata object: %d", ret)
    } /* end else */

    /* Open root group */
    if(NULL == (file->root_grp = (H5VL_daosm_group_t *)H5VL_daosm_group_open_helper(file, root_grp_oid, H5P_GROUP_ACCESS_DEFAULT, dxpl_id, req, epoch)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't open root group")

    /* FCPL was stored as root group's GCPL (as GCPL is the parent of FCPL).
     * Point to it. */
    file->fcpl_id = file->root_grp->gcpl_id;
    if(H5Iinc_ref(file->fcpl_id) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, NULL, "can't increment FCPL ref count")

    /* Determine if we want to acquire a transaction for the opened file */
    if(H5P_get(plist, H5VL_ACQUIRE_TR_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for trans id")

    if(FAIL != trans_id) {
        /* Transaction requested */
        H5TR_t *tr = NULL;

        /* get the TR object */
        if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a TRANSACTION ID")

        tr->epoch = epoch;
        tr->file = file;
    } /* end else */

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
        if(file != NULL && H5VL_daosm_file_close(file, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "can't close file")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_file_open() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_file_close
 *
 * Purpose:     Closes a daos-m HDF5 file.
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
    daos_handle_t hdl_inval = DAOS_HDL_INVAL;
    int ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);

#if 0 /* DSMINC */
    /* Flush the epoch */
    if(0 != (ret = daos_epoch_flush(file->coh, epoch, NULL /*state*/, NULL /*event*/)))
        HDONE_ERROR(H5E_FILE, H5E_CANTFLUSH, NULL, "can't flush epoch: %d", ret)
#endif

    /* Free file data structures */
    if(file->file_name)
        HDfree(file->file_name);
    if(file->comm || file->info)
        if(H5FD_mpi_comm_info_free(&file->comm, &file->info) < 0)
            HDONE_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "Communicator/Info free failed")
    if(file->fapl_id != FAIL && H5I_dec_ref(file->fapl_id) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist")
    if(file->fcpl_id != FAIL && H5I_dec_ref(file->fcpl_id) < 0)
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
H5VL_daosm_write_max_oid(H5VL_daosm_file_t *file, daos_epoch_t epoch)
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
    if(0 != (ret = daos_obj_update(file->glob_md_oh, epoch, &dkey, 1, &iod, &sgl, NULL /*event*/)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTENCODE, FAIL, "can't write max OID to global metadata object: %d", ret)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_write_max_oid() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_daosm_link_read
 *
 * Purpose:     Reads the specified link from the given group
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
    daos_epoch_t epoch, daos_obj_id_t *oid)
{
    char const_link_key[] = H5VL_DAOSM_LINK_KEY;
    daos_key_t dkey;
    daos_vec_iod_t iod;
    daos_recx_t recx;
    daos_sg_list_t sgl;
    daos_iov_t sg_iov;
    uint8_t oid_buf[24];
    uint8_t *p;
    int ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Set up dkey */
    /* For now always use dkey = const, akey = name. Add option to switch these
     * DSMINC */
    daos_iov_set(&dkey, const_link_key, (daos_size_t)(sizeof(const_link_key) - 1));

    /* Set up recx */
    recx.rx_rsize = (uint64_t)sizeof(daos_obj_id_t);
    recx.rx_idx = (uint64_t)0;
    recx.rx_nr = (uint64_t)1;

    /* Set up iod */
    HDmemset(&iod, 0, sizeof(iod));
    daos_iov_set(&iod.vd_name, (void *)name, (daos_size_t)name_len);
    daos_csum_set(&iod.vd_kcsum, NULL, 0);
    iod.vd_nr = 1u;
    iod.vd_recxs = &recx;

    /* Set up sgl */
    daos_iov_set(&sg_iov, oid_buf, (daos_size_t)sizeof(oid_buf));
    sgl.sg_nr.num = 1;
    sgl.sg_iovs = &sg_iov;

    /* Read link to group */
    if(0 != (ret = daos_obj_fetch(grp->obj_oh, epoch, &dkey, 1, &iod, &sgl, NULL /*maps */, NULL /*event*/)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't read link: %d", ret)

    /* Decode dset oid */
    HDassert(sizeof(oid_buf) == sizeof(grp->oid));
    p = oid_buf;
    UINT64DECODE(p, oid->lo)
    UINT64DECODE(p, oid->mid)
    UINT64DECODE(p, oid->hi)

done:
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
    size_t name_len, daos_epoch_t epoch, daos_obj_id_t oid)
{
    char const_link_key[] = H5VL_DAOSM_LINK_KEY;
    daos_key_t dkey;
    daos_vec_iod_t iod;
    daos_recx_t recx;
    daos_sg_list_t sgl;
    daos_iov_t sg_iov;
    uint8_t oid_buf[24];
    uint8_t *p;
    int ret;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Set up dkey */
    /* For now always use dkey = const, akey = name. Add option to switch these
     * DSMINC */
    daos_iov_set(&dkey, const_link_key, (daos_size_t)(sizeof(const_link_key) - 1));

    /* Set up recx */
    recx.rx_rsize = (uint64_t)sizeof(daos_obj_id_t);
    recx.rx_idx = (uint64_t)0;
    recx.rx_nr = (uint64_t)1;

    /* Set up iod */
    HDmemset(&iod, 0, sizeof(iod));
    daos_iov_set(&iod.vd_name, (void *)name, (daos_size_t)name_len);
    daos_csum_set(&iod.vd_kcsum, NULL, 0);
    iod.vd_nr = 1u;
    iod.vd_recxs = &recx;

    /* Encode group oid */
    HDassert(sizeof(oid_buf) == sizeof(grp->oid));
    p = oid_buf;
    UINT64ENCODE(p, oid.lo)
    UINT64ENCODE(p, oid.mid)
    UINT64ENCODE(p, oid.hi)

    /* Set up sgl */
    daos_iov_set(&sg_iov, oid_buf, (daos_size_t)sizeof(oid_buf));
    sgl.sg_nr.num = 1;
    sgl.sg_iovs = &sg_iov;

    /* Write link */
    if(0 != (ret = daos_obj_update(grp->obj_oh, epoch, &dkey, 1, &iod, &sgl, NULL /*event*/)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't write link: %d", ret)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_link_write() */


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
H5VL_daosm_group_traverse(H5VL_daosm_obj_t *obj, const char *path,
    hid_t dxpl_id, void **req, daos_epoch_t epoch, const char **obj_name)
{
    H5VL_daosm_group_t *grp = NULL;
    const char *next_obj;
    daos_obj_id_t oid;
    H5VL_daosm_group_t *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(obj);
    HDassert(path);
    HDassert(obj_name);

    /* Open starting group */
    if(obj->type == H5I_GROUP)
        grp = (H5VL_daosm_group_t *)obj;
    else
        grp = obj->file->root_grp;
    grp->common.rc++;

    /* Initialize obj_name */
    *obj_name = path;

    /* Search for '/' */
    next_obj = strchr(*obj_name, '/');

    /* Traverse path */
    while(next_obj) {
        /* Read link to group */
        HDassert(next_obj > *obj_name);
        if(H5VL_daosm_link_read(grp, *obj_name, (size_t)(next_obj - *obj_name), epoch, &oid) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't read link to group")

        /* Close previous group */
        if(H5VL_daosm_group_close(grp, dxpl_id, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")
        grp = NULL;

        /* Open group */
        if(NULL == (grp = (H5VL_daosm_group_t *)H5VL_daosm_group_open_helper(obj->file, oid, H5P_GROUP_ACCESS_DEFAULT, dxpl_id, req, epoch)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't open group")

        /* Advance to next path element */
        *obj_name = next_obj + 1;
        next_obj = strchr(*obj_name, '/');
    } /* end while */

    ret_value = grp;

done:
    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    if(NULL == ret_value)
        /* Close group */
        if(grp != NULL && H5VL_daosm_group_close(grp, dxpl_id, req) < 0)
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
    hid_t gapl_id, hid_t dxpl_id, void **req, daos_epoch_t epoch)
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

    /* Allocate the group object that is returned to the user */
    if(NULL == (grp = H5FL_CALLOC(H5VL_daosm_group_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate DAOS-M group struct")
    grp->obj_oh = DAOS_HDL_INVAL;
    grp->gcpl_id = FAIL;
    grp->gapl_id = FAIL;

    /* Create group */
    grp->oid.lo = file->max_oid + (uint64_t)1;
    daos_obj_id_generate(&grp->oid, DAOS_OC_TINY_RW);
    if(0 != (ret = daos_obj_declare(file->coh, grp->oid, epoch, NULL /*oa*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create dataset: %d", ret)
    file->max_oid = grp->oid.lo;

    /* Write max OID */
    if(H5VL_daosm_write_max_oid(file, epoch) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't write max OID")

    /* Open group */
    if(0 != (ret = daos_obj_open(file->coh, grp->oid, epoch, DAOS_OO_RW, &grp->obj_oh, NULL /*event*/)))
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
    if(0 != (ret = daos_obj_update(grp->obj_oh, epoch, &dkey, 1, &iod, &sgl, NULL /*event*/)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't write metadata to group: %d", ret)

    /* Finish setting up group struct */
    grp->common.type = H5I_GROUP;
    grp->common.file = file;
    grp->common.rc = 1;
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
        if(grp != NULL && H5VL_daosm_group_close(grp, dxpl_id, req) < 0)
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
H5VL_daosm_group_create(void *_obj,
    H5VL_loc_params_t H5_ATTR_UNUSED loc_params, const char *name,
    hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_daosm_obj_t *obj = (H5VL_daosm_obj_t *)_obj;
    H5VL_daosm_group_t *grp = NULL;
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    hid_t trans_id;
    H5TR_t *tr = NULL;
    H5VL_daosm_group_t *target_grp = NULL;
    const char *target_name = NULL;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for trans_id")

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a Transaction ID")

    /* Traverse the path */
    if(NULL == (target_grp = H5VL_daosm_group_traverse(obj, name, dxpl_id, req, tr->epoch, &target_name)))
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, NULL, "can't traverse path")

    /* Create group */
    if(NULL == (grp = (H5VL_daosm_group_t *)H5VL_daosm_group_create_helper(obj->file, gcpl_id, gapl_id, dxpl_id, req, tr->epoch)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create group")

    /* Create link to group */
    if(H5VL_daosm_link_write(target_grp, target_name, HDstrlen(target_name), tr->epoch, grp->oid) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create link to group")

    ret_value = (void *)grp;

done:
    /* Close target group */
    if(target_grp != NULL && H5VL_daosm_group_close(target_grp, dxpl_id, req) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")

    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    /* Destroy DAOS object if created before failure DSMINC */
    if(NULL == ret_value)
        /* Close group */
        if(grp != NULL && H5VL_daosm_group_close(grp, dxpl_id, req) < 0)
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
    hid_t gapl_id, hid_t dxpl_id, void **req, daos_epoch_t epoch)
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
    grp->oid = oid;
    grp->obj_oh = DAOS_HDL_INVAL;
    grp->gcpl_id = FAIL;
    grp->gapl_id = FAIL;

    /* Open group */
    if(0 != (ret = daos_obj_open(file->coh, oid, epoch, DAOS_OO_RW, &grp->obj_oh, NULL /*event*/)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't open root group: %d", ret)

    /* Set up operation to read GCPL size from group */
    /* Set up dkey */
    daos_iov_set(&dkey, int_md_key, (daos_size_t)(sizeof(int_md_key) - 1));

    /* Set up recx */
    recx.rx_rsize = (uint64_t)0;
    recx.rx_idx = (uint64_t)0;
    recx.rx_nr = (uint64_t)1;

    /* Set up iod */
    HDmemset(&iod, 0, sizeof(iod));
    daos_iov_set(&iod.vd_name, (void *)gcpl_key, (daos_size_t)(sizeof(gcpl_key) - 1));
    daos_csum_set(&iod.vd_kcsum, NULL, 0);
    iod.vd_nr = 1u;
    iod.vd_recxs = &recx;

    /* Read internal metadata size from group */
    if(0 != (ret = daos_obj_fetch(grp->obj_oh, epoch, &dkey, 1, &iod, NULL, NULL /*maps*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, NULL, "can't read metadata size from group: %d", ret)

    /* Allocate buffer for GCPL */
    if(NULL == (gcpl_buf = H5MM_malloc(recx.rx_rsize)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer for serialized gcpl")

    /* Set up sgl */
    daos_iov_set(&sg_iov, gcpl_buf, (daos_size_t)recx.rx_rsize);
    sgl.sg_nr.num = 1;
    sgl.sg_iovs = &sg_iov;

    /* Read internal metadata from group */
    if(0 != (ret = daos_obj_fetch(grp->obj_oh, epoch, &dkey, 1, &iod, &sgl, NULL /*maps*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, NULL, "can't read metadata from group: %d", ret)

    /* Decode GCPL */
    if((grp->gcpl_id = H5Pdecode(gcpl_buf)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTDECODE, NULL, "can't deserialize GCPL")

    /* Finish setting up group struct */
    grp->common.type = H5I_GROUP;
    grp->common.file = file;
    grp->common.rc = 1;
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
        if(grp != NULL && H5VL_daosm_group_close(grp, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CLOSEERROR, NULL, "can't close group")

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
H5VL_daosm_group_open(void *_obj,
    H5VL_loc_params_t H5_ATTR_UNUSED loc_params, const char *name,
    hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_daosm_obj_t *obj = (H5VL_daosm_obj_t *)_obj;
    H5VL_daosm_group_t *grp = NULL;
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    hid_t trans_id;
    H5TR_t *tr = NULL;
    H5VL_daosm_group_t *target_grp = NULL;
    const char *target_name = NULL;
    daos_obj_id_t oid;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for trans_id")

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a Transaction ID")

    /* Traverse the path */
    if(NULL == (target_grp = H5VL_daosm_group_traverse(obj, name, dxpl_id, req, tr->epoch, &target_name)))
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, NULL, "can't traverse path")

    /* Read link to group */
    if(H5VL_daosm_link_read(target_grp, target_name, HDstrlen(target_name), tr->epoch, &oid) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't read link to group")

    /* Open group */
    if(NULL == (grp = (H5VL_daosm_group_t *)H5VL_daosm_group_open_helper(obj->file, oid, gapl_id, dxpl_id, req, tr->epoch)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't open group")

    ret_value = (void *)grp;

done:
    /* Close target group */
    if(target_grp != NULL && H5VL_daosm_group_close(target_grp, dxpl_id, req) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close group")

    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    if(NULL == ret_value)
        /* Close group */
        if(grp != NULL && H5VL_daosm_group_close(grp, dxpl_id, req) < 0)
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

    if(--grp->common.rc == 0) {
        /* Free group data structures */
        if(HDmemcmp(&grp->obj_oh, &hdl_inval, sizeof(hdl_inval)))
            if(0 != (ret = daos_obj_close(grp->obj_oh, NULL /*event*/)))
                HDONE_ERROR(H5E_SYM, H5E_CANTCLOSEOBJ, FAIL, "can't close group DAOS object: %d", ret)
        if(grp->gcpl_id != FAIL && H5I_dec_ref(grp->gcpl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist")
        if(grp->gapl_id != FAIL && H5I_dec_ref(grp->gapl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist")
        grp = H5FL_FREE(H5VL_daosm_group_t, grp);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_group_close() */


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
H5VL_daosm_dataset_create(void *_obj,
    H5VL_loc_params_t H5_ATTR_UNUSED loc_params, const char *name,
    hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req)
{
    H5VL_daosm_obj_t *obj = (H5VL_daosm_obj_t *)_obj;
    H5VL_daosm_dset_t *dset = NULL;
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    hid_t type_id, space_id;
    hid_t trans_id;
    H5TR_t *tr = NULL;
    H5VL_daosm_group_t *target_grp = NULL;
    const char *target_name = NULL;
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

    /* Get the dcpl plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

    /* get creation properties */
    if(H5P_get(plist, H5VL_PROP_DSET_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for datatype id")
    if(H5P_get(plist, H5VL_PROP_DSET_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for space id")

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for trans_id")

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a Transaction ID")

    /* Traverse the path */
    if(NULL == (target_grp = H5VL_daosm_group_traverse(obj, name, dxpl_id, req, tr->epoch, &target_name)))
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, NULL, "can't traverse path")

    /* Allocate the dataset object that is returned to the user */
    if(NULL == (dset = H5FL_CALLOC(H5VL_daosm_dset_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate DAOS-M dataset struct")
    dset->obj_oh = DAOS_HDL_INVAL;
    dset->type_id = FAIL;
    dset->space_id = FAIL;
    dset->dcpl_id = FAIL;
    dset->dapl_id = FAIL;

    /* Create dataset */
    dset->oid.lo = obj->file->max_oid + (uint64_t)1;
    daos_obj_id_generate(&dset->oid, DAOS_OC_LARGE_RW);
    if(0 != (ret = daos_obj_declare(obj->file->coh, dset->oid, tr->epoch, NULL /*oa*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create dataset: %d", ret)
    obj->file->max_oid = dset->oid.lo;

    /* Write max OID */
    if(H5VL_daosm_write_max_oid(obj->file, tr->epoch) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't write max OID")

    /* Create link to dataset */
    if(H5VL_daosm_link_write(target_grp, target_name, HDstrlen(target_name), tr->epoch, dset->oid) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create link to dataset")

    /* Open dataset */
    if(0 != (ret = daos_obj_open(obj->file->coh, dset->oid, tr->epoch, DAOS_OO_RW, &dset->obj_oh, NULL /*event*/)))
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
    if(0 != (ret = daos_obj_update(dset->obj_oh, tr->epoch, &dkey, 3, iod, sgl, NULL /*event*/)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "can't write metadata to dataset: %d", ret)

    /* Finish setting up dataset struct */
    dset->common.type = H5I_DATASET;
    dset->common.file = obj->file;
    dset->common.rc = 1;
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
    if(target_grp != NULL && H5VL_daosm_group_close(target_grp, dxpl_id, req) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, NULL, "can't close group")

    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    /* Destroy DAOS object if created before failure DSMINC */
    if(NULL == ret_value)
        /* Close dataset */
        if(dset != NULL && H5VL_daosm_dataset_close(dset, dxpl_id, req) < 0)
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
H5VL_daosm_dataset_open(void *_obj,
    H5VL_loc_params_t H5_ATTR_UNUSED loc_params, const char *name,
    hid_t dapl_id, hid_t dxpl_id, void **req)
{
    H5VL_daosm_obj_t *obj = (H5VL_daosm_obj_t *)_obj;
    H5VL_daosm_dset_t *dset = NULL;
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    hid_t trans_id;
    H5TR_t *tr = NULL;
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

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for trans_id")

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "not a Transaction ID")

    /* Traverse the path */
    if(NULL == (target_grp = H5VL_daosm_group_traverse(obj, name, dxpl_id, req, tr->epoch, &target_name)))
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, NULL, "can't traverse path")

    /* Allocate the dataset object that is returned to the user */
    if(NULL == (dset = H5FL_CALLOC(H5VL_daosm_dset_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate DAOS-M dataset struct")
    dset->obj_oh = DAOS_HDL_INVAL;
    dset->type_id = FAIL;
    dset->space_id = FAIL;
    dset->dcpl_id = FAIL;
    dset->dapl_id = FAIL;

    /* Read link to dataset */
    if(H5VL_daosm_link_read(target_grp, target_name, HDstrlen(target_name), tr->epoch, &dset->oid) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't read link to dataset")

    /* Open dataset */
    if(0 != (ret = daos_obj_open(obj->file->coh, dset->oid, tr->epoch, DAOS_OO_RW, &dset->obj_oh, NULL /*event*/)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't open root group: %d", ret)

    /* Set up operation to read datatype, dataspace, and DCPL sizes from dataset
     */
    /* Set up dkey */
    daos_iov_set(&dkey, int_md_key, (daos_size_t)(sizeof(int_md_key) - 1));

    /* Set up recx */
    recx[0].rx_rsize = (uint64_t)0;
    recx[0].rx_idx = (uint64_t)0;
    recx[0].rx_nr = (uint64_t)1;
    recx[1].rx_rsize = (uint64_t)0;
    recx[1].rx_idx = (uint64_t)0;
    recx[1].rx_nr = (uint64_t)1;
    recx[2].rx_rsize = (uint64_t)0;
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
    if(0 != (ret = daos_obj_fetch(dset->obj_oh, tr->epoch, &dkey, 3, iod, NULL, NULL /*maps*/, NULL /*event*/)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDECODE, NULL, "can't read metadata sizes from dataset: %d", ret)

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
    if(0 != (ret = daos_obj_fetch(dset->obj_oh, tr->epoch, &dkey, 3, iod, sgl, NULL /*maps */, NULL /*event*/)))
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
    dset->common.type = H5I_DATASET;
    dset->common.file = obj->file;
    dset->common.rc = 1;
    if((dset->dapl_id = H5Pcopy(dapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy dapl");

    ret_value = (void *)dset;

done:
    /* Free memory */
    type_buf = H5MM_xfree(type_buf);
    space_buf = H5MM_xfree(space_buf);
    dcpl_buf = H5MM_xfree(dcpl_buf);

    /* Close target group */
    if(target_grp != NULL && H5VL_daosm_group_close(target_grp, dxpl_id, req) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, NULL, "can't close group")

    /* If the operation is synchronous and it failed at the server, or it failed
     * locally, then cleanup and return fail */
    if(NULL == ret_value)
        /* Close dataset */
        if(dset != NULL && H5VL_daosm_dataset_close(dset, dxpl_id, req) < 0)
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
H5VL_daosm_dataset_read(void *_dset, hid_t H5_ATTR_UNUSED mem_type_id, hid_t mem_space_id,
    hid_t file_space_id, hid_t dxpl_id, void *buf, void H5_ATTR_UNUSED **req)
{
    H5VL_daosm_dset_t *dset = (H5VL_daosm_dset_t *)_dset;
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    hid_t trans_id;
    H5TR_t *tr = NULL;
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

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id");

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

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
    if(0 != (ret = daos_obj_fetch(dset->obj_oh, tr->epoch, &dkey, 1, &iod, &sgl, NULL /*maps*/, NULL /*event*/)))
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
H5VL_daosm_dataset_write(void *_dset, hid_t H5_ATTR_UNUSED mem_type_id, hid_t H5_ATTR_UNUSED mem_space_id,
    hid_t H5_ATTR_UNUSED file_space_id, hid_t dxpl_id, const void *buf,
    void H5_ATTR_UNUSED **req)
{
    H5VL_daosm_dset_t *dset = (H5VL_daosm_dset_t *)_dset;
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    hid_t trans_id;
    H5TR_t *tr = NULL;
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

    /* get the transaction ID */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_get(plist, H5VL_TRANS_ID, &trans_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for trans_id")

    /* get the TR object */
    if(NULL == (tr = (H5TR_t *)H5I_object_verify(trans_id, H5I_TR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a Transaction ID")

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
    if(0 != (ret = daos_obj_update(dset->obj_oh, tr->epoch, &dkey, 1, &iod, &sgl, NULL /*event*/)))
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data to dataset: %d", ret)

done:
    if(recxs != &recx)
        H5MM_free(recxs);
    if(sg_iovs != &sg_iov)
        H5MM_free(sg_iovs);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_dataset_write() */


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

    if(--dset->common.rc == 0) {
        /* Free dataset data structures */
        if(HDmemcmp(&dset->obj_oh, &hdl_inval, sizeof(hdl_inval)))
            if(0 != (ret = daos_obj_close(dset->obj_oh, NULL /*event*/)))
                HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't close dataset DAOS object: %d", ret)
        if(dset->type_id != FAIL && H5I_dec_ref(dset->type_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close datatype")
        if(dset->space_id != FAIL && H5I_dec_ref(dset->space_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close dataspace")
        if(dset->dcpl_id != FAIL && H5I_dec_ref(dset->dcpl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist")
        if(dset->dapl_id != FAIL && H5I_dec_ref(dset->dapl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist")
        dset = H5FL_FREE(H5VL_daosm_dset_t, dset);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_dataset_close() */

