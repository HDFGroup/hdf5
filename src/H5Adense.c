/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5Adense.c
 *			Dec  4 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Routines for operating on "dense" attribute storage
 *                      for an object.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5A_PACKAGE		/*suppress error about including H5Apkg  */
#define H5O_PACKAGE		/*suppress error about including H5Opkg  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Apkg.h"		/* Attributes	  			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Opkg.h"		/* Object headers			*/
#include "H5SMprivate.h"	/* Shared object header messages        */


/****************/
/* Local Macros */
/****************/

/* Fractal heap creation parameters for "dense" attribute storage */
#define H5A_FHEAP_MAN_WIDTH                     4
#define H5A_FHEAP_MAN_START_BLOCK_SIZE          512
#define H5A_FHEAP_MAN_MAX_DIRECT_SIZE           (64 * 1024)
#define H5A_FHEAP_MAN_MAX_INDEX                 32
#define H5A_FHEAP_MAN_START_ROOT_ROWS           1
#define H5A_FHEAP_CHECKSUM_DBLOCKS              TRUE
#define H5A_FHEAP_MAX_MAN_SIZE                  (4 * 1024)

/* v2 B-tree creation macros for 'name' field index */
#define H5A_NAME_BT2_NODE_SIZE          512
#define H5A_NAME_BT2_MERGE_PERC         40
#define H5A_NAME_BT2_SPLIT_PERC         100

/* v2 B-tree creation macros for 'corder' field index */
#define H5A_CORDER_BT2_NODE_SIZE        512
#define H5A_CORDER_BT2_MERGE_PERC       40
#define H5A_CORDER_BT2_SPLIT_PERC       100

/* Size of stack buffer for serialized attributes */
#define H5A_ATTR_BUF_SIZE               128


/******************/
/* Local Typedefs */
/******************/

/*
 * Data exchange structure for dense attribute storage.  This structure is
 * passed through the v2 B-tree layer when modifying attributes.
 */
typedef struct H5A_bt2_od_wrt_t {
    /* downward */
    H5F_t  *f;                  /* Pointer to file that fractal heap is in */
    hid_t dxpl_id;              /* DXPL for operation */
    H5HF_t *fheap;              /* Fractal heap handle to operate on */
    H5HF_t *shared_fheap;       /* Fractal heap handle for shared messages */
    H5A_t  *attr;               /* Attribute to write */
} H5A_bt2_od_wrt_t;

/*
 * Data exchange structure to pass through the v2 B-tree layer for the
 * H5B2_iterate function when iterating over densely stored attributes.
 */
typedef struct {
    /* downward (internal) */
    H5F_t       *f;                     /* Pointer to file that fractal heap is in */
    hid_t       dxpl_id;                /* DXPL for operation                */
    H5HF_t      *fheap;                 /* Fractal heap handle */
    H5HF_t      *shared_fheap;          /* Fractal heap handle for shared messages */

    /* downward (from application) */
    hid_t       loc_id;                 /* Object ID for application callback */
    unsigned    skip;                   /* Number of attributes to skip      */
    unsigned    count;                  /* The # of attributes visited       */
    const H5A_attr_iter_op_t *attr_op;  /* Callback for each attribute       */
    void        *op_data;               /* Callback data for each attribute  */

    /* upward */
    int         op_ret;                 /* Return value from callback        */
} H5A_bt2_ud_it_t;

/*
 * Data exchange structure to pass through the fractal heap layer for the
 * H5HF_op function when copying an attribute stored in densely stored attributes.
 * (or the shared object heap)
 */
typedef struct {
    /* downward (internal) */
    H5F_t       *f;                     /* Pointer to file that fractal heap is in */
    hid_t       dxpl_id;                /* DXPL for operation                */

    /* upward */
    H5A_t  *attr;                       /* Copy of attribute                 */
} H5A_fh_ud_cp_t;

/*
 * Data exchange structure to pass through the fractal heap layer for the
 * H5HF_op function when removing an attribute from densely stored attributes.
 */
typedef struct {
    /* downward */
    H5F_t       *f;                     /* Pointer to file that fractal heap is in */
    hid_t       dxpl_id;                /* DXPL for operation                */
} H5A_fh_ud_rm_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the serialized attribute information */
H5FL_BLK_DEFINE(ser_attr);



/*-------------------------------------------------------------------------
 * Function:	H5A_dense_create
 *
 * Purpose:	Creates dense attribute storage structures for an object
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec  4 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A_dense_create(H5F_t *f, hid_t dxpl_id, H5O_t *oh)
{
    H5HF_create_t fheap_cparam;         /* Fractal heap creation parameters */
    H5HF_t *fheap;                      /* Fractal heap handle */
    size_t fheap_id_len;                /* Fractal heap ID length */
    size_t bt2_rrec_size;               /* v2 B-tree raw record size */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5A_dense_create, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(oh);

    /* Set fractal heap creation parameters */
/* XXX: Give some control of these to applications? */
    HDmemset(&fheap_cparam, 0, sizeof(fheap_cparam));
    fheap_cparam.managed.width = H5A_FHEAP_MAN_WIDTH;
    fheap_cparam.managed.start_block_size = H5A_FHEAP_MAN_START_BLOCK_SIZE;
    fheap_cparam.managed.max_direct_size = H5A_FHEAP_MAN_MAX_DIRECT_SIZE;
    fheap_cparam.managed.max_index = H5A_FHEAP_MAN_MAX_INDEX;
    fheap_cparam.managed.start_root_rows = H5A_FHEAP_MAN_START_ROOT_ROWS;
    fheap_cparam.checksum_dblocks = H5A_FHEAP_CHECKSUM_DBLOCKS;
    fheap_cparam.max_man_size = H5A_FHEAP_MAX_MAN_SIZE;

    /* Create fractal heap for storing attributes */
    if(NULL == (fheap = H5HF_create(f, dxpl_id, &fheap_cparam)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "unable to create fractal heap")

    /* Retrieve the heap's address in the file */
    if(H5HF_get_heap_addr(fheap, &(oh->attr_fheap_addr)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGETSIZE, FAIL, "can't get fractal heap address")
#ifdef QAK
HDfprintf(stderr, "%s: oh->attr_fheap_addr = %a\n", FUNC, oh->attr_fheap_addr);
#endif /* QAK */

    /* Retrieve the heap's ID length in the file */
    if(H5HF_get_id_len(fheap, &fheap_id_len) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGETSIZE, FAIL, "can't get fractal heap ID length")
    HDassert(fheap_id_len == H5A_DENSE_FHEAP_ID_LEN);
    HDassert(fheap_id_len == H5SM_FHEAP_ID_LEN);        /* Need to be interchangable -QAK */
#ifdef QAK
HDfprintf(stderr, "%s: fheap_id_len = %Zu\n", FUNC, fheap_id_len);
#endif /* QAK */

    /* Close the fractal heap */
    if(H5HF_close(fheap, dxpl_id) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "can't close fractal heap")

    /* Create the name index v2 B-tree */
    bt2_rrec_size = 4 +                 /* Name's hash value */
            1 +                         /* Message flags */
            fheap_id_len;               /* Fractal heap ID */
    if(H5B2_create(f, dxpl_id, H5A_BT2_NAME,
            (size_t)H5A_NAME_BT2_NODE_SIZE, bt2_rrec_size,
            H5A_NAME_BT2_SPLIT_PERC, H5A_NAME_BT2_MERGE_PERC,
            &(oh->name_bt2_addr)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "unable to create v2 B-tree for name index")
#ifdef QAK
HDfprintf(stderr, "%s: oh->name_bt2_addr = %a\n", FUNC, oh->name_bt2_addr);
#endif /* QAK */

/* XXX: fix me */
#ifdef NOT_YET
    /* Check if we should create a creation order index v2 B-tree */
    if(linfo->index_corder) {
        /* Create the creation order index v2 B-tree */
        bt2_rrec_size = 8 +             /* Creation order value */
                fheap_id_len;           /* Fractal heap ID */
        if(H5B2_create(f, dxpl_id, H5A_BT2_CORDER,
                (size_t)H5A_CORDER_BT2_NODE_SIZE, bt2_rrec_size,
                H5A_CORDER_BT2_SPLIT_PERC, H5A_CORDER_BT2_MERGE_PERC,
                &(linfo->corder_bt2_addr)) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "unable to create v2 B-tree for name index")
#ifdef QAK
HDfprintf(stderr, "%s: linfo->corder_bt2_addr = %a\n", FUNC, linfo->corder_bt2_addr);
#endif /* QAK */
    } /* end if */
#endif /* NOT_YET */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_create() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_open_cb
 *
 * Purpose:	Callback when an attribute is located in an index
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec 11 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_dense_open_cb(const void *_attr, void *_user_attr)
{
    const H5A_t *attr = (const H5A_t *)_attr; /* Record from B-tree */
    H5A_t **user_attr = (H5A_t **)_user_attr; /* User data from v2 B-tree attribute lookup */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5A_dense_open_cb)

    /*
     * Check arguments.
     */
    HDassert(attr);
    HDassert(user_attr);

    /* Copy attribute information */
    if(NULL == (*user_attr = H5A_copy(NULL, attr)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, H5_ITER_ERROR, "can't copy attribute")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_open_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_open
 *
 * Purpose:	Open an attribute in dense storage structures for an object
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec 11 2006
 *
 *-------------------------------------------------------------------------
 */
H5A_t *
H5A_dense_open(H5F_t *f, hid_t dxpl_id, const H5O_t *oh, const char *name)
{
    H5A_bt2_ud_common_t udata;          /* User data for v2 B-tree modify */
    H5HF_t *fheap = NULL;               /* Fractal heap handle */
    H5HF_t *shared_fheap = NULL;        /* Fractal heap handle for shared header messages */
    htri_t attr_sharable;               /* Flag indicating attributes are sharable */
    H5A_t *ret_value = NULL;            /* Return value */

    FUNC_ENTER_NOAPI(H5A_dense_open, NULL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(oh);
    HDassert(name);

    /* Open the fractal heap */
    if(NULL == (fheap = H5HF_open(f, dxpl_id, oh->attr_fheap_addr)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "unable to open fractal heap")

    /* Check if attributes are shared in this file */
    if((attr_sharable = H5SM_type_shared(f, H5O_ATTR_ID, dxpl_id)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, NULL, "can't determine if attributes are shared")

    /* Get handle for shared object heap, if attributes are sharable */
    if(attr_sharable) {
        haddr_t shared_fheap_addr;      /* Address of fractal heap to use */

        /* Retrieve the address of the shared object's fractal heap */
        if(HADDR_UNDEF == (shared_fheap_addr = H5SM_get_fheap_addr(f, H5O_ATTR_ID, dxpl_id)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, NULL, "can't get shared object heap address")

        /* Open the fractal heap for shared header messages */
        if(NULL == (shared_fheap = H5HF_open(f, dxpl_id, shared_fheap_addr)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "unable to open fractal heap")
    } /* end if */

    /* Create the "udata" information for v2 B-tree record modify */
    udata.f = f;
    udata.dxpl_id = dxpl_id;
    udata.fheap = fheap;
    udata.shared_fheap = shared_fheap;
    udata.name = name;
    udata.name_hash = H5_checksum_lookup3(name, HDstrlen(name), 0);
    udata.flags = 0;
    udata.corder = -1;   /* XXX: None yet */
    udata.found_op = H5A_dense_open_cb;       /* v2 B-tree comparison callback */
    udata.found_op_data = &ret_value;

    /* Find & copy the attribute in the 'name' index */
    if(H5B2_find(f, dxpl_id, H5A_BT2_NAME, oh->name_bt2_addr, &udata, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, NULL, "can't locate attribute in name index")

done:
    /* Release resources */
    if(shared_fheap && H5HF_close(shared_fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, NULL, "can't close fractal heap")
    if(fheap && H5HF_close(fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, NULL, "can't close fractal heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_open() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_insert
 *
 * Purpose:	Insert an attribute into dense storage structures for an object
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec  4 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A_dense_insert(H5F_t *f, hid_t dxpl_id, const H5O_t *oh, unsigned mesg_flags,
    const H5A_t *attr)
{
    H5A_bt2_ud_ins_t udata;             /* User data for v2 B-tree insertion */
    H5HF_t *fheap = NULL;               /* Fractal heap handle for attributes */
    H5HF_t *shared_fheap = NULL;        /* Fractal heap handle for shared header messages */
    uint8_t id[H5A_DENSE_FHEAP_ID_LEN]; /* Heap ID of attribute to insert    */
    H5O_shared_t sh_mesg;               /* Shared object header message */
    uint8_t attr_buf[H5A_ATTR_BUF_SIZE]; /* Buffer for serializing message */
    void *attr_ptr = NULL;              /* Pointer to serialized message */
    htri_t attr_sharable;               /* Flag indicating attributes are sharable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5A_dense_insert, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(oh);
    HDassert(attr);

    /* Check if attributes are shared in this file */
    if((attr_sharable = H5SM_type_shared(f, H5O_ATTR_ID, dxpl_id)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't determine if attributes are shared")

    /* Get handle for shared object heap, if attributes are sharable */
    if(attr_sharable) {
        haddr_t shared_fheap_addr;      /* Address of fractal heap to use */

        /* Retrieve the address of the shared object's fractal heap */
        if(HADDR_UNDEF == (shared_fheap_addr = H5SM_get_fheap_addr(f, H5O_ATTR_ID, dxpl_id)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get shared object heap address")

        /* Open the fractal heap for shared header messages */
        if(NULL == (shared_fheap = H5HF_open(f, dxpl_id, shared_fheap_addr)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")
    } /* end if */

    /* Open the fractal heap */
    if(NULL == (fheap = H5HF_open(f, dxpl_id, oh->attr_fheap_addr)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")

    /* Check for inserting shared attribute */
    if(mesg_flags & H5O_MSG_FLAG_SHARED) {
        /* Sanity check */
        HDassert(attr_sharable);

        /* Get the shared information for the attribute */
        if(NULL == H5O_attr_get_share(attr, &sh_mesg))
            HGOTO_ERROR(H5E_ATTR, H5E_BADMESG, FAIL, "can't get shared message")

        /* Use heap ID for shared message heap */
        udata.id = (const uint8_t *)&sh_mesg.u.heap_id;
    } /* end if */
    else {
        size_t attr_size;                   /* Size of serialized attribute in the heap */

        /* Find out the size of buffer needed for serialized message */
        if((attr_size = H5O_msg_raw_size(f, H5O_ATTR_ID, attr)) == 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTGETSIZE, FAIL, "can't get message size")

        /* Allocate space for serialized message, if necessary */
        if(attr_size > sizeof(attr_buf)) {
            if(NULL == (attr_ptr = H5FL_BLK_MALLOC(ser_attr, attr_size)))
                HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "memory allocation failed")
        } /* end if */
        else
            attr_ptr = attr_buf;

        /* Create serialized form of attribute or shared message */
        if(H5O_msg_encode(f, H5O_ATTR_ID, (unsigned char *)attr_ptr, attr) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL, "can't encode attribute")

        /* Insert the serialized attribute into the fractal heap */
        if(H5HF_insert(fheap, dxpl_id, attr_size, attr_ptr, id) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINSERT, FAIL, "unable to insert attribute into fractal heap")

        /* Use heap ID for attribute heap */
        udata.id = id;
    } /* end else */

    /* Create the callback information for v2 B-tree record insertion */
    udata.common.f = f;
    udata.common.dxpl_id = dxpl_id;
    udata.common.fheap = fheap;
    udata.common.shared_fheap = shared_fheap;
    udata.common.name = attr->name;
    udata.common.name_hash = H5_checksum_lookup3(attr->name, HDstrlen(attr->name), 0);
    udata.common.flags = mesg_flags;
    udata.common.corder = -1;   /* XXX: None yet */
    udata.common.found_op = NULL;
    udata.common.found_op_data = NULL;
    /* udata.id already set */

    /* Insert attribute into 'name' tracking v2 B-tree */
    if(H5B2_insert(f, dxpl_id, H5A_BT2_NAME, oh->name_bt2_addr, &udata) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINSERT, FAIL, "unable to insert record into v2 B-tree")

done:
    /* Release resources */
    if(shared_fheap && H5HF_close(shared_fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "can't close fractal heap")
    if(fheap && H5HF_close(fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "can't close fractal heap")
    if(attr_ptr && attr_ptr != attr_buf)
        (void)H5FL_BLK_FREE(ser_attr, attr_ptr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_write_bt2_cb
 *
 * Purpose:	v2 B-tree 'modify' callback to update the data for an attribute
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, December  5, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_dense_write_bt2_cb(void *_record, void *_op_data, hbool_t *changed)
{
    H5A_dense_bt2_name_rec_t *record = (H5A_dense_bt2_name_rec_t *)_record; /* Record from B-tree */
    H5A_bt2_od_wrt_t *op_data = (H5A_bt2_od_wrt_t *)_op_data;       /* "op data" from v2 B-tree modify */
    uint8_t attr_buf[H5A_ATTR_BUF_SIZE]; /* Buffer for serializing attribute */
    void *attr_ptr = NULL;              /* Pointer to serialized attribute */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5A_dense_write_bt2_cb)

    /*
     * Check arguments.
     */
    HDassert(record);
    HDassert(op_data);

    /* Check for modifying shared attribute */
    if(record->flags & H5O_MSG_FLAG_SHARED) {
        H5O_shared_t sh_mesg;           /* Shared object header message */

        /* Extract shared message info from current attribute */
        if(NULL == H5O_attr_get_share(op_data->attr, &sh_mesg))
            HGOTO_ERROR(H5E_ATTR, H5E_BADMESG, FAIL, "can't get shared info")

        /* Update the shared attribute in the SOHM info */
        if(H5O_attr_update_shared(op_data->f, op_data->dxpl_id, op_data->attr, &sh_mesg) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTUPDATE, FAIL, "unable to update attribute in shared storage")

        /* Extract new shared message info from updated attribute */
        if(NULL == H5O_attr_get_share(op_data->attr, &sh_mesg))
            HGOTO_ERROR(H5E_ATTR, H5E_BADMESG, FAIL, "can't get shared info")

        /* Update record's heap ID */
        HDmemcpy(record->id, &op_data->attr->sh_loc.u.heap_id, sizeof(record->id));

        /* Note that the record changed */
        *changed = TRUE;
    } /* end if */
    else {
        size_t attr_size;               /* Size of serialized attribute in the heap */

        /* Find out the size of buffer needed for serialized attribute */
        if((attr_size = H5O_msg_raw_size(op_data->f, H5O_ATTR_ID, op_data->attr)) == 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTGETSIZE, FAIL, "can't get attribute size")

        /* Allocate space for serialized attribute, if necessary */
        if(attr_size > sizeof(attr_buf)) {
            if(NULL == (attr_ptr = H5FL_BLK_MALLOC(ser_attr, attr_size)))
                HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "memory allocation failed")
        } /* end if */
        else
            attr_ptr = attr_buf;

        /* Create serialized form of attribute */
        if(H5O_msg_encode(op_data->f, H5O_ATTR_ID, (unsigned char *)attr_ptr, op_data->attr) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL, "can't encode attribute")

/* Sanity check */
#ifndef NDEBUG
{
    size_t obj_len;             /* Length of existing encoded attribute */

    if(H5HF_get_obj_len(op_data->fheap, op_data->dxpl_id, record->id, &obj_len) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGETSIZE, FAIL, "can't get object size")
    HDassert(obj_len == attr_size);
}
#endif /* NDEBUG */
        /* Update existing attribute in heap */
        /* (would be more efficient as fractal heap 'op' callback, but leave that for later -QAK) */
        if(H5HF_write(op_data->fheap, op_data->dxpl_id, record->id, changed, attr_ptr) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTUPDATE, FAIL, "unable to update attribute in heap")
    } /* end else */

done:
    /* Release resources */
    if(attr_ptr && attr_ptr != attr_buf)
        (void)H5FL_BLK_FREE(ser_attr, attr_ptr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_write_bt2_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_write
 *
 * Purpose:	Modify an attribute in dense storage structures for an object
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec  4 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A_dense_write(H5F_t *f, hid_t dxpl_id, const H5O_t *oh, H5A_t *attr)
{
    H5A_bt2_ud_common_t udata;          /* User data for v2 B-tree modify */
    H5A_bt2_od_wrt_t op_data;           /* "Op data" for v2 B-tree modify */
    H5HF_t *fheap = NULL;               /* Fractal heap handle */
    H5HF_t *shared_fheap = NULL;        /* Fractal heap handle for shared header messages */
    htri_t attr_sharable;               /* Flag indicating attributes are sharable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5A_dense_write, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(oh);
    HDassert(attr);

    /* Check if attributes are shared in this file */
    if((attr_sharable = H5SM_type_shared(f, H5O_ATTR_ID, dxpl_id)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't determine if attributes are shared")

    /* Get handle for shared object heap, if attributes are sharable */
    if(attr_sharable) {
        haddr_t shared_fheap_addr;      /* Address of fractal heap to use */

        /* Retrieve the address of the shared object's fractal heap */
        if(HADDR_UNDEF == (shared_fheap_addr = H5SM_get_fheap_addr(f, H5O_ATTR_ID, dxpl_id)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get shared object heap address")

        /* Open the fractal heap for shared header messages */
        if(NULL == (shared_fheap = H5HF_open(f, dxpl_id, shared_fheap_addr)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")
    } /* end if */

    /* Open the fractal heap */
    if(NULL == (fheap = H5HF_open(f, dxpl_id, oh->attr_fheap_addr)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")

    /* Create the "udata" information for v2 B-tree record modify */
    udata.f = f;
    udata.dxpl_id = dxpl_id;
    udata.fheap = fheap;
    udata.shared_fheap = shared_fheap;
    udata.name = attr->name;
    udata.name_hash = H5_checksum_lookup3(attr->name, HDstrlen(attr->name), 0);
    udata.flags = 0;
    udata.corder = -1;   /* XXX: None yet */
    udata.found_op = NULL;
    udata.found_op_data = NULL;

    /* Create the "op_data" for the v2 B-tree record 'modify' callback */
    op_data.f = f;
    op_data.dxpl_id = dxpl_id;
    op_data.fheap = fheap;
    op_data.shared_fheap = shared_fheap;
    op_data.attr = attr;

    /* Modify attribute through 'name' tracking v2 B-tree */
    if(H5B2_modify(f, dxpl_id, H5A_BT2_NAME, oh->name_bt2_addr, &udata, H5A_dense_write_bt2_cb, &op_data) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINSERT, FAIL, "unable to modify record in v2 B-tree")

done:
    /* Release resources */
    if(shared_fheap && H5HF_close(shared_fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "can't close fractal heap")
    if(fheap && H5HF_close(fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "can't close fractal heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_write() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_copy_fh_cb
 *
 * Purpose:	Callback for fractal heap operator, to make copy of attribute
 *              for calling routine
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec  5 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_dense_copy_fh_cb(const void *obj, size_t UNUSED obj_len, void *_udata)
{
    H5A_fh_ud_cp_t *udata = (H5A_fh_ud_cp_t *)_udata;       /* User data for fractal heap 'op' callback */
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5A_dense_copy_fh_cb)

    /* Decode attribute information & keep a copy */
    /* (we make a copy instead of calling the user/library callback directly in
     *  this routine because this fractal heap 'op' callback routine is called
     *  with the direct block protected and if the callback routine invokes an
     *  HDF5 routine, it could attempt to re-protect that direct block for the
     *  heap, causing the HDF5 routine called to fail)
     */
    if(NULL == (udata->attr = (H5A_t *)H5O_msg_decode(udata->f, udata->dxpl_id, H5O_ATTR_ID, (const unsigned char *)obj)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDECODE, FAIL, "can't decode attribute")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_copy_fh_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_iterate_bt2_cb
 *
 * Purpose:	v2 B-tree callback for dense attribute storage iterator
 *
 * Return:	H5_ITER_ERROR/H5_ITER_CONT/H5_ITER_STOP
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec  5 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_dense_iterate_bt2_cb(const void *_record, void *_bt2_udata)
{
    const H5A_dense_bt2_name_rec_t *record = (const H5A_dense_bt2_name_rec_t *)_record; /* Record from B-tree */
    H5A_bt2_ud_it_t *bt2_udata = (H5A_bt2_ud_it_t *)_bt2_udata;         /* User data for callback */
    herr_t ret_value = H5_ITER_CONT;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5A_dense_iterate_bt2_cb)

    /* Check for skipping attributes */
    if(bt2_udata->skip > 0)
        --bt2_udata->skip;
    else {
        H5A_fh_ud_cp_t fh_udata;       /* User data for fractal heap 'op' callback */
        H5HF_t *fheap;                 /* Fractal heap handle for attribute storage */

        /* Check for iterating over shared attribute */
        if(record->flags & H5O_MSG_FLAG_SHARED)
            fheap = bt2_udata->shared_fheap;
        else
            fheap = bt2_udata->fheap;

        /* Prepare user data for callback */
        /* down */
        fh_udata.f = bt2_udata->f;
        fh_udata.dxpl_id = bt2_udata->dxpl_id;

        /* Call fractal heap 'op' routine, to copy the attribute information */
        if(H5HF_op(fheap, bt2_udata->dxpl_id, record->id,
                H5A_dense_copy_fh_cb, &fh_udata) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPERATE, H5_ITER_ERROR, "heap op callback failed")

        /* Check whether we should "reconstitute" the shared message info */
        if(record->flags & H5O_MSG_FLAG_SHARED)
            H5SM_reconstitute(&(fh_udata.attr->sh_loc), record->id);

        /* Check which type of callback to make */
        switch(bt2_udata->attr_op->op_type) {
            case H5A_ATTR_OP_APP:
                /* Make the application callback */
                ret_value = (bt2_udata->attr_op->u.app_op)(bt2_udata->loc_id, fh_udata.attr->name, bt2_udata->op_data);
                break;

            case H5A_ATTR_OP_LIB:
                /* Call the library's callback */
                ret_value = (bt2_udata->attr_op->u.lib_op)(fh_udata.attr, bt2_udata->op_data);
        } /* end switch */

        /* Release the space allocated for the attribute */
        H5O_msg_free(H5O_ATTR_ID, fh_udata.attr);
    } /* end else */

    /* Increment the number of attributes passed through */
    /* (whether we skipped them or not) */
    bt2_udata->count++;

    /* Check for callback failure and pass along return value */
    if(ret_value < 0)
        HERROR(H5E_ATTR, H5E_CANTNEXT, "iteration operator failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_iterate_bt2_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_iterate
 *
 * Purpose:	Iterate over attributes in dense storage structures for an object
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec  5 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A_dense_iterate(H5F_t *f, hid_t dxpl_id, hid_t loc_id, haddr_t attr_fheap_addr,
    haddr_t name_bt2_addr, H5_iter_order_t order, unsigned skip,
    unsigned *last_attr, const H5A_attr_iter_op_t *attr_op, void *op_data)
{
    H5A_bt2_ud_it_t udata;              /* User data for iterator callback */
    H5HF_t *fheap = NULL;               /* Fractal heap handle */
    H5HF_t *shared_fheap = NULL;        /* Fractal heap handle for shared header messages */
    H5A_attr_table_t atable = {0, NULL};        /* Table of attributes */
    hsize_t nrec;                       /* # of records in v2 B-tree */
    herr_t ret_value;                   /* Return value */

    FUNC_ENTER_NOAPI(H5A_dense_iterate, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(attr_fheap_addr));
    HDassert(H5F_addr_defined(name_bt2_addr));
    HDassert(attr_op);

    /* Retrieve # of records in name index */
    /* (# of records in all indices the same) */
    if(H5B2_get_nrec(f, dxpl_id, H5A_BT2_NAME, name_bt2_addr, &nrec) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't retrieve # of records in index")

    /* Check for skipping too many attributes */
    if(skip > 0) {
        /* Check for bad starting index */
        if((hsize_t)skip >= nrec)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index specified")
    } /* end if */

    /* Check on iteration order */
    /* ("native" iteration order is unordered for this attribute storage mechanism) */
    if(order == H5_ITER_NATIVE) {
        htri_t attr_sharable;               /* Flag indicating attributes are sharable */

        /* Open the fractal heap */
        if(NULL == (fheap = H5HF_open(f, dxpl_id, attr_fheap_addr)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")

        /* Check if attributes are shared in this file */
        if((attr_sharable = H5SM_type_shared(f, H5O_ATTR_ID, dxpl_id)) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't determine if attributes are shared")

        /* Get handle for shared object heap, if attributes are sharable */
        if(attr_sharable) {
            haddr_t shared_fheap_addr;      /* Address of fractal heap to use */

            /* Retrieve the address of the shared object's fractal heap */
            if(HADDR_UNDEF == (shared_fheap_addr = H5SM_get_fheap_addr(f, H5O_ATTR_ID, dxpl_id)))
                HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get shared object heap address")

            /* Open the fractal heap for shared header messages */
            if(NULL == (shared_fheap = H5HF_open(f, dxpl_id, shared_fheap_addr)))
                HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")
        } /* end if */

        /* Construct the user data for v2 B-tree iterator callback */
        udata.f = f;
        udata.dxpl_id = dxpl_id;
        udata.fheap = fheap;
        udata.shared_fheap = shared_fheap;
        udata.loc_id = loc_id;
        udata.skip = skip;
        udata.count = 0;
        udata.attr_op = attr_op;
        udata.op_data = op_data;

        /* Iterate over the records in the v2 B-tree's "native" order */
        /* (by hash of name) */
        if((ret_value = H5B2_iterate(f, dxpl_id, H5A_BT2_NAME, name_bt2_addr,
                H5A_dense_iterate_bt2_cb, &udata)) < 0)
            HERROR(H5E_ATTR, H5E_BADITER, "attribute iteration failed");

        /* Update last attribute looked at */
        if(last_attr)
            *last_attr = udata.count;
    } /* end if */
    else {
        /* Build the table of attributes for this object */
        if(H5A_dense_build_table(f, dxpl_id, nrec, attr_fheap_addr, name_bt2_addr,
                H5_INDEX_NAME, order, &atable) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "error building table of attributes")

        /* Iterate over attributes in table */
        if((ret_value = H5A_attr_iterate_table(&atable, skip, last_attr, loc_id, attr_op, op_data)) < 0)
            HERROR(H5E_ATTR, H5E_CANTNEXT, "iteration operator failed");
    } /* end else */

done:
    /* Release resources */
    if(shared_fheap && H5HF_close(shared_fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "can't close fractal heap")
    if(fheap && H5HF_close(fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "can't close fractal heap")
    if(atable.attrs && H5A_attr_release_table(&atable) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "unable to release attribute table")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_remove_fh_cb
 *
 * Purpose:	Callback for fractal heap operator when removing attributes
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec 11 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_dense_remove_fh_cb(const void *obj, size_t UNUSED obj_len, void *_udata)
{
    H5A_fh_ud_rm_t *udata = (H5A_fh_ud_rm_t *)_udata;       /* User data for fractal heap 'op' callback */
    H5A_t *attr = NULL;                 /* Pointer to attribute created from heap object */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5A_dense_remove_fh_cb)

    /* Decode attribute */
    if(NULL == (attr = (H5A_t *)H5O_msg_decode(udata->f, udata->dxpl_id, H5O_ATTR_ID, (const unsigned char *)obj)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDECODE, FAIL, "can't decode attribute")

    /* Perform the deletion action on the attribute */
    /* (takes care of shared & committed datatype/dataspace components) */
    if(H5O_attr_delete(udata->f, udata->dxpl_id, attr, TRUE) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute")

done:
    /* Release the space allocated for the message */
    if(attr)
        H5O_msg_free_real(H5O_MSG_ATTR, attr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_remove_fh_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_remove_bt2_cb
 *
 * Purpose:	v2 B-tree callback for dense attribute storage record removal
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec 11 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_dense_remove_bt2_cb(const void *_record, void *_bt2_udata)
{
    const H5A_dense_bt2_name_rec_t *record = (const H5A_dense_bt2_name_rec_t *)_record;
    H5A_bt2_ud_common_t *bt2_udata = (H5A_bt2_ud_common_t *)_bt2_udata;         /* User data for callback */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5A_dense_remove_bt2_cb)

    /* Check for inserting shared attribute */
    if(record->flags & H5O_MSG_FLAG_SHARED) {
        H5A_fh_ud_cp_t fh_udata;            /* User data for fractal heap 'op' callback */
        H5O_shared_t sh_mesg;               /* Shared object header message */

        /* Set up the user data for fractal heap 'op' callback */
        fh_udata.f = bt2_udata->f;
        fh_udata.dxpl_id = bt2_udata->dxpl_id;
        fh_udata.attr = NULL;

        /* Call fractal heap 'op' routine, to get copy of the attribute */
        if(H5HF_op(bt2_udata->shared_fheap, bt2_udata->dxpl_id, record->id,
                H5A_dense_copy_fh_cb, &fh_udata) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPERATE, FAIL, "attribute removal callback failed")

        /* Get the shared information for the attribute */
        if(NULL == H5O_attr_get_share(fh_udata.attr, &sh_mesg))
            HGOTO_ERROR(H5E_ATTR, H5E_BADMESG, FAIL, "can't get shared message")

        /* Decrement the reference count on the shared attribute message */
        if(H5SM_try_delete(bt2_udata->f, bt2_udata->dxpl_id, H5O_ATTR_ID, &sh_mesg) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "unable to delete shared attribute")

        /* Release the space allocated for the attribute */
        H5O_msg_free(H5O_ATTR_ID, fh_udata.attr);
    } /* end if */
    else {
        H5A_fh_ud_rm_t fh_udata;            /* User data for fractal heap 'op' callback */

        /* Set up the user data for fractal heap 'op' callback */
        fh_udata.f = bt2_udata->f;
        fh_udata.dxpl_id = bt2_udata->dxpl_id;

        /* Call fractal heap 'op' routine, to perform user callback */
        if(H5HF_op(bt2_udata->fheap, bt2_udata->dxpl_id, record->id,
                H5A_dense_remove_fh_cb, &fh_udata) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPERATE, FAIL, "attribute removal callback failed")

        /* Remove record from fractal heap */
        if(H5HF_remove(bt2_udata->fheap, bt2_udata->dxpl_id, record->id) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTREMOVE, FAIL, "unable to remove attribute from fractal heap")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_remove_bt2_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_remove
 *
 * Purpose:	Remove an attribute from the dense storage of an object
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec 11 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A_dense_remove(H5F_t *f, hid_t dxpl_id, const H5O_t *oh, const char *name)
{
    H5A_bt2_ud_common_t udata;          /* User data for v2 B-tree record removal */
    H5HF_t *fheap = NULL;               /* Fractal heap handle */
    H5HF_t *shared_fheap = NULL;        /* Fractal heap handle for shared header messages */
    htri_t attr_sharable;               /* Flag indicating attributes are sharable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5A_dense_remove, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(oh);
    HDassert(name && *name);

    /* Open the fractal heap */
    if(NULL == (fheap = H5HF_open(f, dxpl_id, oh->attr_fheap_addr)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")

    /* Check if attributes are shared in this file */
    if((attr_sharable = H5SM_type_shared(f, H5O_ATTR_ID, dxpl_id)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't determine if attributes are shared")

    /* Get handle for shared object heap, if attributes are sharable */
    if(attr_sharable) {
        haddr_t shared_fheap_addr;      /* Address of fractal heap to use */

        /* Retrieve the address of the shared object's fractal heap */
        if(HADDR_UNDEF == (shared_fheap_addr = H5SM_get_fheap_addr(f, H5O_ATTR_ID, dxpl_id)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get shared object heap address")

        /* Open the fractal heap for shared header messages */
        if(NULL == (shared_fheap = H5HF_open(f, dxpl_id, shared_fheap_addr)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")
    } /* end if */

    /* Set up the user data for the v2 B-tree 'record remove' callback */
    udata.f = f;
    udata.dxpl_id = dxpl_id;
    udata.fheap = fheap;
    udata.shared_fheap = shared_fheap;
    udata.name = name;
    udata.name_hash = H5_checksum_lookup3(name, HDstrlen(name), 0);
    udata.flags = 0;
    udata.corder = -1;   /* XXX: None yet */
    udata.found_op = NULL;
    udata.found_op_data = NULL;

    /* Remove the record from the name index v2 B-tree */
    if(H5B2_remove(f, dxpl_id, H5A_BT2_NAME, oh->name_bt2_addr, &udata, H5A_dense_remove_bt2_cb, &udata) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTREMOVE, FAIL, "unable to remove attribute from name index v2 B-tree")

done:
    /* Release resources */
    if(shared_fheap && H5HF_close(shared_fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "can't close fractal heap")
    if(fheap && H5HF_close(fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "can't close fractal heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_exists
 *
 * Purpose:	Check if an attribute exists in dense storage structures for
 *              an object
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec 11 2006
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5A_dense_exists(H5F_t *f, hid_t dxpl_id, const H5O_t *oh, const char *name)
{
    H5A_bt2_ud_common_t udata;          /* User data for v2 B-tree modify */
    H5HF_t *fheap = NULL;               /* Fractal heap handle */
    H5HF_t *shared_fheap = NULL;        /* Fractal heap handle for shared header messages */
    htri_t attr_sharable;               /* Flag indicating attributes are sharable */
    htri_t ret_value = TRUE;            /* Return value */

    FUNC_ENTER_NOAPI(H5A_dense_exists, NULL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(oh);
    HDassert(name);

    /* Open the fractal heap */
    if(NULL == (fheap = H5HF_open(f, dxpl_id, oh->attr_fheap_addr)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")

    /* Check if attributes are shared in this file */
    if((attr_sharable = H5SM_type_shared(f, H5O_ATTR_ID, dxpl_id)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't determine if attributes are shared")

    /* Get handle for shared object heap, if attributes are sharable */
    if(attr_sharable) {
        haddr_t shared_fheap_addr;      /* Address of fractal heap to use */

        /* Retrieve the address of the shared object's fractal heap */
        if(HADDR_UNDEF == (shared_fheap_addr = H5SM_get_fheap_addr(f, H5O_ATTR_ID, dxpl_id)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get shared object heap address")

        /* Open the fractal heap for shared header messages */
        if(NULL == (shared_fheap = H5HF_open(f, dxpl_id, shared_fheap_addr)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")
    } /* end if */

    /* Create the "udata" information for v2 B-tree record 'find' */
    udata.f = f;
    udata.dxpl_id = dxpl_id;
    udata.fheap = fheap;
    udata.shared_fheap = shared_fheap;
    udata.name = name;
    udata.name_hash = H5_checksum_lookup3(name, HDstrlen(name), 0);
    udata.flags = 0;
    udata.corder = -1;   /* XXX: None yet */
    udata.found_op = NULL;       /* v2 B-tree comparison callback */
    udata.found_op_data = NULL;

    /* Find the attribute in the 'name' index */
    if(H5B2_find(f, dxpl_id, H5A_BT2_NAME, oh->name_bt2_addr, &udata, NULL, NULL) < 0) {
        /* Assume that the failure was just not finding the attribute & clear stack */
        H5E_clear_stack(NULL);

        ret_value = FALSE;
    } /* end if */

done:
    /* Release resources */
    if(shared_fheap && H5HF_close(shared_fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "can't close fractal heap")
    if(fheap && H5HF_close(fheap, dxpl_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "can't close fractal heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_exists() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_delete
 *
 * Purpose:	Delete all dense storage structures for attributes on an object
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec  6 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A_dense_delete(H5F_t *f, hid_t dxpl_id, H5O_t *oh)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5A_dense_delete, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(oh);

/* XXX: iterate through name index v2 B-tree and delete shared attributes */
/* XXX: we need to delete shared & unshared attributes that use shared & committed components also */

    /* Delete name index v2 B-tree */
    if(H5B2_delete(f, dxpl_id, H5A_BT2_NAME, oh->name_bt2_addr, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete v2 B-tree for name index")
    oh->name_bt2_addr = HADDR_UNDEF;

    /* Delete fractal heap */
    if(H5HF_delete(f, dxpl_id, oh->attr_fheap_addr) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete fractal heap")
    oh->attr_fheap_addr = HADDR_UNDEF;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_delete() */

