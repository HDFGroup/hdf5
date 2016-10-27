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
#include "H5Iprivate.h"         /* IDs                                  */
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5Pprivate.h"         /* Property lists                       */
#include "H5VLprivate.h"        /* VOL plugins                          */
#include "H5VLdaosm.h"          /* DAOS-M plugin                        */

hid_t H5VL_DAOSM_g = 0;

/* Prototypes */
static void *H5VL_daosm_fapl_copy(const void *_old_fa);
static herr_t H5VL_daosm_fapl_free(void *_fa);

/* File callbacks */
static void *H5VL_daosm_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id, void **req);
static void *H5VL_daosm_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req);
//static herr_t H5VL_iod_file_get(void *file, H5VL_file_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_daosm_file_close(void *file, hid_t dxpl_id, void **req);

/* DAOSM-specific file access properties */
typedef struct H5VL_daosm_fapl_t {
    MPI_Comm            comm;           /*communicator                  */
    MPI_Info            info;           /*file information              */
    uuid_t              pool_uuid;      /*pool uuid                     */
    char                *pool_grp;      /*pool group                    */
} H5VL_daosm_fapl_t;

/* Free list definitions */
H5FL_DEFINE(H5VL_daosm_file_t);

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
        NULL,//H5VL_iod_dataset_create,                /* create */
        NULL,//H5VL_iod_dataset_open,                  /* open */
        NULL,//H5VL_iod_dataset_read,                  /* read */
        NULL,//H5VL_iod_dataset_write,                 /* write */
        NULL,//H5VL_iod_dataset_get,                   /* get */
        NULL,//H5VL_iod_dataset_specific,              /* specific */
        NULL,                                   /* optional */
        NULL,//H5VL_iod_dataset_close                  /* close */
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
        NULL,//H5VL_iod_group_create,                  /* create */
        NULL,//H5VL_iod_group_open,                    /* open */
        NULL,//H5VL_iod_group_get,                     /* get */
        NULL,                                   /* specific */
        NULL,                                   /* optional */
        NULL,//H5VL_iod_group_close                    /* close */
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
 * Return:      Success:        The ID for the iod plugin.
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

    /* Register the IOD VOL, if it isn't already */
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
 * Purpose:     Copies the iod-specific file access properties.
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
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Copy the general information */
    HDmemcpy(new_fa, old_fa, sizeof(H5VL_daosm_fapl_t));

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(old_fa->comm, old_fa->info, &new_fa->comm, &new_fa->info))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed");

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
 * Purpose:     Frees the iod-specific file access properties.
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
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "Communicator/Info free failed");

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
H5VL_daosm_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, 
                     hid_t H5_ATTR_UNUSED dxpl_id, void **req)
{
    H5VL_daosm_fapl_t *fa = NULL;
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    H5VL_daosm_file_t *file = NULL;
    daos_epoch_t epoch;
    daos_iov_t glob;
    uint64_t gh_sizes[2];
    char *gh_buf = NULL;
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
        HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate DAOS-M file struct");
    file->glob_md_oh = DAOS_HDL_INVAL;
    file->root_oh = DAOS_HDL_INVAL;
    file->fcpl_id = FAIL;
    file->fapl_id = FAIL;

    MPI_Comm_rank(fa->comm, &file->my_rank);
    MPI_Comm_size(fa->comm, &file->num_procs);

    /* Hash file name to create uuid */
    H5VL_daosm_hash128(name, &file->uuid);

    if(file->my_rank == 0) {
        daos_epoch_state_t epoch_state;
        daos_obj_id_t oid = {0, 0, 0};

        /* Connect to the pool */
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
        daos_obj_id_generate(&oid, DAOS_OC_REPLICA_RW);
        if(0 != (ret = daos_obj_declare(file->coh, oid, 0, NULL /*oa*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, NULL, "can't create global metadata object: %d", ret)

        /* Open global metadata object */
        if(0 != (ret = daos_obj_open(file->glob_md_oh, oid, 0, DAOS_OO_EXCL, &file->glob_md_oh, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't open global metadata object: %d", ret)

        /* Create root group */
        HDmemset(&oid, 0, sizeof(oid));
        oid.lo = 1;
        daos_obj_id_generate(&oid, DAOS_OC_TINY_RW);
        if(0 != (ret = daos_obj_declare(file->coh, oid, epoch, NULL /*oa*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't create root group: %d", ret)

        /* Open root group */
        if(0 != (ret = daos_obj_open(file->root_oh, oid, epoch, DAOS_OO_RW, &file->root_oh, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't open root group: %d", ret)

        /* Write root group OID to global metadata object DSMINC */

        /* Flush the epoch */
        if(0 != (ret = daos_epoch_flush(file->coh, epoch, NULL /*state*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, NULL, "can't flush epoch: %d", ret)

        if(file->num_procs > 1) {
            /* Calculate sizes of global pool and container handles */
            glob.iov_buf = NULL;
            glob.iov_buf_len = 0;
            glob.iov_len = 0;
            if(0 != (ret = daos_pool_local2global(file->poh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global pool handle size: %d", ret)
            gh_sizes[0] = (uint64_t)glob.iov_buf_len;
            glob.iov_buf = NULL;
            glob.iov_buf_len = 0;
            glob.iov_len = 0;
            if(0 != (ret = daos_cont_local2global(file->coh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global container handle size: %d", ret)
            gh_sizes[1] = (uint64_t)glob.iov_buf_len;

            /* Retrieve global pool and container handles */
            if(NULL == (gh_buf = (char *)H5MM_malloc(gh_sizes[0] + gh_sizes[1])))
                HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate space for global handles")
            glob.iov_buf = gh_buf;
            glob.iov_buf_len = gh_sizes[0];
            glob.iov_len = 0;
            if(0 != (ret = daos_pool_local2global(file->poh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global pool handle: %d", ret)
            HDassert(glob.iov_len == glob.iov_buf_len);
            glob.iov_buf = gh_buf + gh_sizes[0];
            glob.iov_buf_len = gh_sizes[1];
            glob.iov_len = 0;
            if(0 != (ret = daos_cont_local2global(file->coh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global container handle: %d", ret)
            HDassert(glob.iov_len == glob.iov_buf_len);

            /* MPI_Bcast gh_sizes */
            if(MPI_SUCCESS != MPI_Bcast(gh_sizes, 2, MPI_UINT64_T, 0, fa->comm))
                HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")

            /* MPI_Bcast gh_buf */
            if(MPI_SUCCESS != MPI_Bcast(gh_buf, (int)(gh_sizes[0] + gh_sizes[1]), MPI_BYTE, 0, fa->comm))
                HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")
        } /* end if */

        /* Commit epoch DSMINC */
        if(0 != (ret = daos_epoch_commit(file->coh, epoch, NULL /*state*/, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, NULL, "can't commit epoch: %d", ret)
        epoch++;
    } /* end if */
    else {
        /* Receive gh_sizes */
        if(MPI_SUCCESS != MPI_Bcast(gh_sizes, 2, MPI_UINT64_T, 0, fa->comm))
            HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")

        /* Allocate global handle buffer */
        if(NULL == (gh_buf = (char *)H5MM_malloc(gh_sizes[0] + gh_sizes[1])))
            HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate space for global handles")

        /* Receive gh_buf */
        if(MPI_SUCCESS != MPI_Bcast(gh_buf, (int)(gh_sizes[0] + gh_sizes[1]), MPI_BYTE, 0, fa->comm))
            HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")

        /* Create local pool and container handles */
        glob.iov_buf = gh_buf;
        glob.iov_buf_len = gh_sizes[0];
        glob.iov_len = gh_sizes[0];
        if(0 != (ret = daos_pool_global2local(glob, &file->poh)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't get local pool handle: %d", ret)
        glob.iov_buf = gh_buf + gh_sizes[0];
        glob.iov_buf_len = gh_sizes[1];
        glob.iov_len = gh_sizes[1];
        if(0 != (ret = daos_cont_global2local(file->poh, glob, &file->coh)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't get local container handle: %d", ret)

        /* Leave global md object and root group handles empty for now */

        /* Handle pool_info and container_info DSMINC */
    } /* end else */

    /* Finish setting up file struct */
    file->file_name = HDstrdup(name);
    file->flags = flags;
    HDmemset(&file->max_oid, 0, sizeof(file->max_oid));
    file->max_oid.lo = 1;
    if((file->fcpl_id = H5Pcopy(fcpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fcpl");
    if((file->fapl_id = H5Pcopy(fapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fapl");

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(fa->comm, fa->info, &file->comm, &file->info))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed");

    ret_value = (void *)file;

done:
    /* Clean up */
    H5MM_xfree(gh_buf);

    /* If the operation is synchronous and it failed at the server, or
       it failed locally, then cleanup and return fail */
    if(NULL == ret_value)
        if(file != NULL && H5VL_daosm_file_close(file, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "can't close file")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_file_create() */


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
    daos_iov_t glob;
    uint64_t gh_sizes[2];
    char *gh_buf = NULL;
    int ret;
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* obtain the process rank from the communicator attached to the fapl ID */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if(NULL == (fa = (H5VL_daosm_fapl_t *)H5P_get_vol_info(plist)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, NULL, "can't get IOD info struct")

    /* allocate the file object that is returned to the user */
    if(NULL == (file = H5FL_CALLOC(H5VL_daosm_file_t)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate IOD file struct");
    file->glob_md_oh = DAOS_HDL_INVAL;
    file->root_oh = DAOS_HDL_INVAL;
    file->fcpl_id = FAIL;
    file->fapl_id = FAIL;

    MPI_Comm_rank(fa->comm, &file->my_rank);
    MPI_Comm_size(fa->comm, &file->num_procs);

    /* Hash file name to create uuid */
    H5VL_daosm_hash128(name, &file->uuid);

    if(file->my_rank == 0) {
        daos_epoch_state_t epoch_state;
        daos_obj_id_t oid = {0, 0, 0};

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

        /* Open global metadata object */
        daos_obj_id_generate(&oid, DAOS_OC_REPLICA_RW);
        if(0 != (ret = daos_obj_open(file->glob_md_oh, oid, 0, DAOS_OO_EXCL, &file->glob_md_oh, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't open global metadata object: %d", ret)

        /* Open root group */
        /* Read root group OID from global metadata object DSMINC */
        HDmemset(&oid, 0, sizeof(oid));
        oid.lo = 1;
        daos_obj_id_generate(&oid, DAOS_OC_TINY_RW);
        if(0 != (ret = daos_obj_open(file->root_oh, oid, epoch, DAOS_OO_RW, &file->root_oh, NULL /*event*/)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't open root group: %d", ret)

        if(file->num_procs > 1) {
            /* Calculate sizes of global pool and container handles */
            glob.iov_buf = NULL;
            glob.iov_buf_len = 0;
            glob.iov_len = 0;
            if(0 != (ret = daos_pool_local2global(file->poh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global pool handle size: %d", ret)
            gh_sizes[0] = (uint64_t)glob.iov_buf_len;
            glob.iov_buf = NULL;
            glob.iov_buf_len = 0;
            glob.iov_len = 0;
            if(0 != (ret = daos_cont_local2global(file->coh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global container handle size: %d", ret)
            gh_sizes[1] = (uint64_t)glob.iov_buf_len;

            /* Retrieve global pool and container handles */
            if(NULL == (gh_buf = (char *)malloc(gh_sizes[0] + gh_sizes[1])))
                HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate space for global handles")
            glob.iov_buf = gh_buf;
            glob.iov_buf_len = gh_sizes[0];
            glob.iov_len = 0;
            if(0 != (ret = daos_pool_local2global(file->poh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global pool handle: %d", ret)
            HDassert(glob.iov_len == glob.iov_buf_len);
            glob.iov_buf = gh_buf + gh_sizes[0];
            glob.iov_buf_len = gh_sizes[1];
            glob.iov_len = 0;
            if(0 != (ret = daos_cont_local2global(file->coh, &glob)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "can't get global container handle: %d", ret)
            HDassert(glob.iov_len == glob.iov_buf_len);

            /* MPI_Bcast gh_sizes */
            if(MPI_SUCCESS != MPI_Bcast(gh_sizes, 2, MPI_UINT64_T, 0, fa->comm))
                HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")

            /* MPI_Bcast gh_buf */
            if(MPI_SUCCESS != MPI_Bcast(gh_buf, (int)(gh_sizes[0] + gh_sizes[1]), MPI_BYTE, 0, fa->comm))
                HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")
        } /* end if */
    } /* end if */
    else {
        /* Receive gh_sizes */
        if(MPI_SUCCESS != MPI_Bcast(gh_sizes, 2, MPI_UINT64_T, 0, fa->comm))
            HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")

        /* Allocate global handle buffer */
        if(NULL == (gh_buf = (char *)malloc(gh_sizes[0] + gh_sizes[1])))
            HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate space for global handles")

        /* Receive gh_buf */
        if(MPI_SUCCESS != MPI_Bcast(gh_buf, (int)(gh_sizes[0] + gh_sizes[1]), MPI_BYTE, 0, fa->comm))
            HGOTO_ERROR(H5E_FILE, H5E_MPI, NULL, "can't bcast global handle sizes")

        /* Create local pool and container handles */
        glob.iov_buf = gh_buf;
        glob.iov_buf_len = gh_sizes[0];
        glob.iov_len = gh_sizes[0];
        if(0 != (ret = daos_pool_global2local(glob, &file->poh)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't get local pool handle: %d", ret)
        glob.iov_buf = gh_buf + gh_sizes[0];
        glob.iov_buf_len = gh_sizes[1];
        glob.iov_len = gh_sizes[1];
        if(0 != (ret = daos_cont_global2local(file->poh, glob, &file->coh)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, NULL, "can't get local container handle: %d", ret)

        /* Leave global md object and root group handles empty for now */

        /* Handle pool_info and container_info DSMINC */
    } /* end else */

    /* Finish setting up file struct */
    file->file_name = HDstrdup(name);
    file->flags = flags;
    HDmemset(&file->max_oid, 0, sizeof(file->max_oid));
    file->max_oid.lo = 1;
    if((file->fapl_id = H5Pcopy(fapl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "failed to copy fapl");

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(fa->comm, fa->info, &file->comm, &file->info))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed");

    ret_value = (void *)file;

done:
    /* Clean up */
    H5MM_xfree(gh_buf);

    /* If the operation is synchronous and it failed at the server, or
       it failed locally, then cleanup and return fail */
    if(NULL == ret_value)
        if(file != NULL && H5VL_daosm_file_close(file, dxpl_id, req) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "can't close file")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_file_open() */


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
H5VL_daosm_file_close(void *_file, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    H5VL_daosm_file_t *file = (H5VL_daosm_file_t *)_file;
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
    if(file->fapl_id != FAIL && H5I_dec_ref(file->fapl_id) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
    if(file->fcpl_id != FAIL && H5I_dec_ref(file->fcpl_id) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "failed to close plist");
    if(HDmemcmp(&file->glob_md_oh, &hdl_inval, sizeof(hdl_inval)))
        if(0 != (ret = daos_obj_close(file->glob_md_oh, NULL /*event*/)))
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close global metadata object: %d", ret)
    if(HDmemcmp(&file->root_oh, &hdl_inval, sizeof(hdl_inval)))
        if(0 != (ret = daos_obj_close(file->root_oh, NULL /*event*/)))
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close root group: %d", ret)
    if(HDmemcmp(&file->coh, &hdl_inval, sizeof(hdl_inval)))
        if(0 != (ret = daos_cont_close(file->coh, NULL /*event*/)))
            HDONE_ERROR(H5E_FILE, H5E_CLOSEERROR, FAIL, "can't close container: %d", ret)
    if(HDmemcmp(&file->poh, &hdl_inval, sizeof(hdl_inval)))
        if(0 != (ret = daos_pool_disconnect(file->poh, NULL /*event*/)))
            HDONE_ERROR(H5E_FILE, H5E_CLOSEERROR, FAIL, "can't disconnect from pool: %d", ret)
    file = H5FL_FREE(H5VL_daosm_file_t, file);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_daosm_file_close() */

