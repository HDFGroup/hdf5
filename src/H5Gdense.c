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
 * Created:		H5Gdense.c
 *			Sep  9 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Routines for operating on "dense" link storage for a
 *                      group in a file.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5G_PACKAGE		/*suppress error about including H5Gpkg  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5MMprivate.h"	/* Memory management			*/


/****************/
/* Local Macros */
/****************/

/* Fractal heap creation parameters for "dense" link storage */
#define H5G_FHEAP_MAN_WIDTH                     4
#define H5G_FHEAP_MAN_START_BLOCK_SIZE          512
#define H5G_FHEAP_MAN_MAX_DIRECT_SIZE           (64 * 1024)
#define H5G_FHEAP_MAN_MAX_INDEX                 32
#define H5G_FHEAP_MAN_START_ROOT_ROWS           1
#define H5G_FHEAP_CHECKSUM_DBLOCKS              TRUE
#define H5G_FHEAP_MAX_MAN_SIZE                  (4 * 1024)

/* v2 B-tree creation macros for 'name' field index */
#define H5G_NAME_BT2_NODE_SIZE          512
#define H5G_NAME_BT2_MERGE_PERC         40
#define H5G_NAME_BT2_SPLIT_PERC         100

/* v2 B-tree creation macros for 'corder' field index */
#define H5G_CORDER_BT2_NODE_SIZE        512
#define H5G_CORDER_BT2_MERGE_PERC       40
#define H5G_CORDER_BT2_SPLIT_PERC       100

/* Size of stack buffer for serialized link */
#define H5G_LINK_BUF_SIZE               128


/******************/
/* Local Typedefs */
/******************/

/* Data exchange structure to use when building table of links in group */
typedef struct {
    H5G_link_table_t *ltable;   /* Pointer to link table to build */
    size_t curr_lnk;            /* Current link to operate on */
} H5G_dense_bt_ud1_t;

/*
 * Data exchange structure to pass through the v2 B-tree layer for the
 * H5B2_iterate function when iterating over densely stored links.
 */
typedef struct {
    /* downward (internal) */
    H5F_t       *f;                     /* Pointer to file that fractal heap is in */
    hid_t       dxpl_id;                /* DXPL for operation                */
    H5HF_t      *fheap;                 /* Fractal heap handle */
    hbool_t     lib_internal;           /* Callback is library internal      */

    /* downward (from application) */
    hid_t       gid;                    /* Group ID for application callback */
    H5G_link_iterate_t op;              /* Callback for each link            */
    void        *op_data;               /* Callback data for each link       */
    int         skip;                   /* Number of links to skip           */
    int         *last_lnk;              /* Pointer to the last link operated on */

    /* upward */
    int         op_ret;                 /* Return value from callback        */
} H5G_bt2_it_ud1_t;

/*
 * Data exchange structure to pass through the fractal heap layer for the
 * H5HF_op function when iterating over densely stored links.
 */
typedef struct {
    /* downward (internal) */
    H5F_t       *f;                     /* Pointer to file that fractal heap is in */
    hid_t       dxpl_id;                /* DXPL for operation                */

    /* upward */
    H5O_link_t  *lnk;                   /* Copy of link                      */
} H5G_fh_it_ud1_t;

/*
 * Data exchange structure for dense link storage.  This structure is
 * passed through the v2 B-tree layer when removing links.
 */
typedef struct {
    /* downward */
    H5G_bt2_ud_common_t common;         /* Common info for B-tree user data (must be first) */
    hbool_t     adj_link;               /* Whether to adjust link count on object */
    hbool_t     rem_from_fheap;         /* Whether to remove the link from the fractal heap */

    /* upward (for application) */
    H5G_obj_t *obj_type;                /* Type of object being removed      */
} H5G_bt2_ud_rem_t;

/*
 * Data exchange structure to pass through the fractal heap layer for the
 * H5HF_op function when removing a link from densely stored links.
 */
typedef struct {
    /* downward (internal) */
    H5F_t       *f;                     /* Pointer to file that fractal heap is in */
    hid_t       dxpl_id;                /* DXPL for operation                */
    hbool_t     adj_link;               /* Whether to adjust link count on object */

    /* upward (for application) */
    H5G_obj_t *obj_type;                /* Type of object being removed      */
} H5G_fh_rem_ud1_t;

/*
 * Data exchange structure to pass through the v2 B-tree layer for the
 * H5B2_index function when retrieving the name of a link by index.
 */
typedef struct {
    /* downward (internal) */
    H5F_t       *f;                     /* Pointer to file that fractal heap is in */
    hid_t       dxpl_id;                /* DXPL for operation                */
    H5HF_t      *fheap;                 /* Fractal heap handle               */

    /* downward (from application) */
    char        *name;                  /* Name buffer to fill               */
    size_t      name_size;              /* Size of name buffer to fill       */

    /* upward */
    ssize_t     name_len;               /* Full length of name               */
} H5G_bt2_gnbi_ud1_t;

/*
 * Data exchange structure to pass through the fractal heap layer for the
 * H5HF_op function when retrieving the name of a link by index.
 */
typedef struct {
    /* downward (internal) */
    H5F_t       *f;                     /* Pointer to file that fractal heap is in */
    hid_t       dxpl_id;                /* DXPL for operation                */

    /* downward (from application) */
    char        *name;                  /* Name buffer to fill               */
    size_t      name_size;              /* Size of name buffer to fill       */

    /* upward */
    ssize_t     name_len;               /* Full length of name               */
} H5G_fh_gnbi_ud1_t;

/*
 * Data exchange structure to pass through the v2 B-tree layer for the
 * H5B2_index function when retrieving the type of a link by index.
 */
typedef struct {
    /* downward (internal) */
    H5F_t       *f;                     /* Pointer to file that fractal heap is in */
    hid_t       dxpl_id;                /* DXPL for operation                */
    H5HF_t      *fheap;                 /* Fractal heap handle               */

    /* upward */
    H5G_obj_t   type;                   /* Type of object                    */
} H5G_bt2_gtbi_ud1_t;

/*
 * Data exchange structure to pass through the fractal heap layer for the
 * H5HF_op function when retrieving the type of a link by index.
 */
typedef struct {
    /* downward (internal) */
    H5F_t       *f;                     /* Pointer to file that fractal heap is in */
    hid_t       dxpl_id;                /* DXPL for operation                */

    /* upward */
    H5G_obj_t   type;                   /* Type of object                    */
} H5G_fh_gtbi_ud1_t;

/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/

/* The v2 B-tree class for indexing 'name' field on links */
H5_DLLVAR const H5B2_class_t H5G_BT2_NAME[1];

/* The v2 B-tree class for indexing 'creation order' field on links */
H5_DLLVAR const H5B2_class_t H5G_BT2_CORDER[1];


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the serialized link information */
H5FL_BLK_DEFINE(ser_link);



/*-------------------------------------------------------------------------
 * Function:	H5G_dense_create
 *
 * Purpose:	Creates dense link storage structures for a group
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep  9 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_dense_create(H5F_t *f, hid_t dxpl_id, H5O_linfo_t *linfo)
{
    H5HF_create_t fheap_cparam;         /* Fractal heap creation parameters */
    H5HF_t *fheap;                      /* Fractal heap handle */
    size_t fheap_id_len;                /* Fractal heap ID length */
    size_t bt2_rrec_size;               /* v2 B-tree raw record size */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_dense_create, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(linfo);

    /* Set fractal heap creation parameters */
/* XXX: Give some control of these to applications? */
    HDmemset(&fheap_cparam, 0, sizeof(fheap_cparam));
    fheap_cparam.managed.width = H5G_FHEAP_MAN_WIDTH;
    fheap_cparam.managed.start_block_size = H5G_FHEAP_MAN_START_BLOCK_SIZE;
    fheap_cparam.managed.max_direct_size = H5G_FHEAP_MAN_MAX_DIRECT_SIZE;
    fheap_cparam.managed.max_index = H5G_FHEAP_MAN_MAX_INDEX;
    fheap_cparam.managed.start_root_rows = H5G_FHEAP_MAN_START_ROOT_ROWS;
    fheap_cparam.checksum_dblocks = H5G_FHEAP_CHECKSUM_DBLOCKS;
    fheap_cparam.max_man_size = H5G_FHEAP_MAX_MAN_SIZE;

    /* Create fractal heap for storing links */
    if(NULL == (fheap = H5HF_create(f, dxpl_id, &fheap_cparam)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create fractal heap")

    /* Retrieve the heap's address in the file */
    if(H5HF_get_heap_addr(fheap, &(linfo->link_fheap_addr)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGETSIZE, FAIL, "can't get fractal heap address")
#ifdef QAK
HDfprintf(stderr, "%s: linfo->link_fheap_addr = %a\n", FUNC, linfo->link_fheap_addr);
#endif /* QAK */

    /* Retrieve the heap's ID length in the file */
    if(H5HF_get_id_len(fheap, &fheap_id_len) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGETSIZE, FAIL, "can't get fractal heap ID length")
    HDassert(fheap_id_len == H5G_DENSE_FHEAP_ID_LEN);
#ifdef QAK
HDfprintf(stderr, "%s: fheap_id_len = %Zu\n", FUNC, fheap_id_len);
#endif /* QAK */

    /* Close the fractal heap */
    if(H5HF_close(fheap, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close fractal heap")

    /* Create the name index v2 B-tree */
    bt2_rrec_size = 4 +                 /* Name's hash value */
            fheap_id_len;               /* Fractal heap ID */
    if(H5B2_create(f, dxpl_id, H5G_BT2_NAME,
            (size_t)H5G_NAME_BT2_NODE_SIZE, bt2_rrec_size,
            H5G_NAME_BT2_SPLIT_PERC, H5G_NAME_BT2_MERGE_PERC,
            &(linfo->name_bt2_addr)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create v2 B-tree for name index")
#ifdef QAK
HDfprintf(stderr, "%s: linfo->name_bt2_addr = %a\n", FUNC, linfo->name_bt2_addr);
#endif /* QAK */

    /* Check if we should create a creation order index v2 B-tree */
    if(linfo->index_corder) {
        /* Create the creation order index v2 B-tree */
        bt2_rrec_size = 8 +             /* Creation order value */
                fheap_id_len;           /* Fractal heap ID */
        if(H5B2_create(f, dxpl_id, H5G_BT2_CORDER,
                (size_t)H5G_CORDER_BT2_NODE_SIZE, bt2_rrec_size,
                H5G_CORDER_BT2_SPLIT_PERC, H5G_CORDER_BT2_MERGE_PERC,
                &(linfo->corder_bt2_addr)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create v2 B-tree for name index")
#ifdef QAK
HDfprintf(stderr, "%s: linfo->corder_bt2_addr = %a\n", FUNC, linfo->corder_bt2_addr);
#endif /* QAK */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_create() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_insert
 *
 * Purpose:	Insert a link into the  dense link storage structures for a group
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 11 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_dense_insert(H5F_t *f, hid_t dxpl_id, const H5O_linfo_t *linfo,
    const H5O_link_t *lnk)
{
    H5G_bt2_ud_ins_t udata;             /* User data for v2 B-tree insertion */
    H5HF_t *fheap = NULL;               /* Fractal heap handle */
    size_t link_size;                   /* Size of serialized link in the heap */
    uint8_t link_buf[H5G_LINK_BUF_SIZE];        /* Buffer for serializing link */
    void *link_ptr = NULL;              /* Pointer to serialized link */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_dense_insert, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(linfo);
    HDassert(lnk);
#ifdef QAK
HDfprintf(stderr, "%s: linfo->link_fheap_addr = %a\n", FUNC, linfo->link_fheap_addr);
HDfprintf(stderr, "%s: linfo->name_bt2_addr = %a\n", FUNC, linfo->name_bt2_addr);
#endif /* QAK */

    /* Find out the size of buffer needed for serialized link */
    if((link_size = H5O_raw_size(H5O_LINK_ID, f, lnk)) == 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGETSIZE, FAIL, "can't get link size")
#ifdef QAK
HDfprintf(stderr, "%s: HDstrlen(lnk->name) = %Zu, link_size = %Zu\n", FUNC, HDstrlen(lnk->name), link_size);
#endif /* QAK */

    /* Allocate space for serialized link, if necessary */
    if(link_size > sizeof(link_buf)) {
        if(NULL == (link_ptr = H5FL_BLK_MALLOC(ser_link, link_size)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "memory allocation failed")
    } /* end if */
    else
        link_ptr = link_buf;

    /* Create serialized form of link */
    if(H5O_encode(f, link_ptr, lnk, H5O_LINK_ID) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "can't encode link")

    /* Open the fractal heap */
    if(NULL == (fheap = H5HF_open(f, dxpl_id, linfo->link_fheap_addr)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")

    /* Insert the serialized link into the fractal heap */
    if(H5HF_insert(fheap, dxpl_id, link_size, link_ptr, udata.id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert link into fractal heap")

    /* Create the callback information for v2 B-tree record insertion */
    udata.common.f = f;
    udata.common.dxpl_id = dxpl_id;
    udata.common.fheap = fheap;
    udata.common.name = lnk->name;
    udata.common.name_hash = H5_checksum_lookup3(lnk->name, HDstrlen(lnk->name), 0);
    udata.common.found_op = NULL;
    udata.common.found_op_data = NULL;
    /* udata.id already set in H5HF_insert() call */

    /* Insert link into 'name' tracking v2 B-tree */
    if(H5B2_insert(f, dxpl_id, H5G_BT2_NAME, linfo->name_bt2_addr, &udata) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert record into v2 B-tree")

    /* XXX: Check if we should create a creation order index v2 B-tree record */
        /* XXX: Insert the record into the creation order index v2 B-tree */

done:
    /* Release resources */
    if(fheap)
        if(H5HF_close(fheap, dxpl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close fractal heap")
    if(link_ptr && link_ptr != link_buf)
        H5FL_BLK_FREE(ser_link, link_ptr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_lookup_cb
 *
 * Purpose:	Callback when a link is located in the 'name' index
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 11 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_dense_lookup_cb(const void *_lnk, void *_user_lnk)
{
    const H5O_link_t *lnk = (const H5O_link_t *)_lnk; /* Record from B-tree */
    H5O_link_t *user_lnk = (H5O_link_t *)_user_lnk;       /* User data from v2 B-tree link lookup */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_dense_lookup_cb)

    /*
     * Check arguments.
     */
    HDassert(lnk);
    HDassert(user_lnk);

    /* Check if we want the link information */
    if(user_lnk) {
        /* Copy link information */
        if(H5O_copy(H5O_LINK_ID, lnk, user_lnk) == NULL)
            HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, H5O_ITER_ERROR, "can't copy link message")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_lookup_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_lookup
 *
 * Purpose:	Look up a link within a group that uses dense link storage
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 11 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_dense_lookup(H5F_t *f, hid_t dxpl_id, const H5O_linfo_t *linfo,
    const char *name, H5O_link_t *lnk)
{
    H5G_bt2_ud_common_t udata;          /* User data for v2 B-tree link lookup */
    H5HF_t *fheap = NULL;               /* Fractal heap handle */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_dense_lookup, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(linfo);
    HDassert(name && *name);
    HDassert(lnk);

    /* Open the fractal heap */
    if(NULL == (fheap = H5HF_open(f, dxpl_id, linfo->link_fheap_addr)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")

    /* Construct the user data for v2 B-tree callback */
    udata.f = f;
    udata.dxpl_id = dxpl_id;
    udata.fheap = fheap;
    udata.name = name;
    udata.name_hash = H5_checksum_lookup3(name, HDstrlen(name), 0);
    udata.found_op = H5G_dense_lookup_cb;       /* v2 B-tree comparison callback */
    udata.found_op_data = lnk;

    /* Find & copy the named link in the 'name' index */
    if(H5B2_find(f, dxpl_id, H5G_BT2_NAME, linfo->name_bt2_addr, &udata, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to locate link in name index")

done:
    /* Release resources */
    if(fheap)
        if(H5HF_close(fheap, dxpl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close fractal heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_lookup() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_build_table_cb
 *
 * Purpose:	Callback routine for building table of links from dense
 *              link storage.
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sept 25 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_dense_build_table_cb(const H5O_link_t *lnk, void *_udata)
{
    H5G_dense_bt_ud1_t *udata = (H5G_dense_bt_ud1_t *)_udata;     /* 'User data' passed in */
    herr_t ret_value = H5B2_ITER_CONT;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_dense_build_table_cb)

    /* check arguments */
    HDassert(lnk);
    HDassert(udata);
    HDassert(udata->curr_lnk < udata->ltable->nlinks);

    /* Copy link information */
    if(H5O_copy(H5O_LINK_ID, lnk, &(udata->ltable->lnks[udata->curr_lnk])) == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, H5B2_ITER_ERROR, "can't copy link message")

    /* Increment number of links stored */
    udata->curr_lnk++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_build_table_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_build_table
 *
 * Purpose:     Builds a table containing a sorted (alphabetically) list of
 *              links for a group
 *
 * Note:	Used for building table of links in non-native iteration order
 *		for an index
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Sep 25, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_dense_build_table(H5F_t *f, hid_t dxpl_id, const H5O_linfo_t *linfo,
    H5_iter_order_t order, H5G_link_table_t *ltable)
{
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_dense_build_table)

    /* Sanity check */
    HDassert(f);
    HDassert(linfo);
    HDassert(ltable);

    /* Set size of table */
    H5_CHECK_OVERFLOW(linfo->nlinks, /* From: */ hsize_t, /* To: */ size_t);
    ltable->nlinks = (size_t)linfo->nlinks;

    /* Allocate space for the table entries */
    if(ltable->nlinks > 0) {
        H5G_dense_bt_ud1_t udata;       /* User data for iteration callback */
        H5G_link_iterate_t lnk_op;      /* Link operator */

        /* Allocate the table to store the links */
        if((ltable->lnks = H5MM_malloc(sizeof(H5O_link_t) * ltable->nlinks)) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* Set up user data for iteration */
        udata.ltable = ltable;
        udata.curr_lnk = 0;

        /* Build iterator operator */
        lnk_op.lib_op = H5G_dense_build_table_cb;

        /* Iterate over the links in the group, building a table of the link messages */
        if(H5G_dense_iterate(f, dxpl_id, H5_ITER_NATIVE, 0, linfo, TRUE, 0, NULL, lnk_op, &udata) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTNEXT, FAIL, "error iterating over links")

        /* Sort link table in correct iteration order */
        /* (XXX: by name, currently) */
        if(order == H5_ITER_INC)
            HDqsort(ltable->lnks, ltable->nlinks, sizeof(H5O_link_t), H5G_obj_cmp_name_inc);
        else if(order == H5_ITER_INC)
            HDqsort(ltable->lnks, ltable->nlinks, sizeof(H5O_link_t), H5G_obj_cmp_name_dec);
        else
            HDassert(order == H5_ITER_NATIVE);
    } /* end if */
    else
        ltable->lnks = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_build_table() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_iterate_fh_cb
 *
 * Purpose:	Callback for fractal heap operator, to make user's callback
 *              when iterating over links
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 11 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_dense_iterate_fh_cb(const void *obj, size_t UNUSED obj_len, void *_udata)
{
    H5G_fh_it_ud1_t *udata = (H5G_fh_it_ud1_t *)_udata;       /* User data for fractal heap 'op' callback */
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_dense_iterate_fh_cb)

    /* Decode link information & keep a copy */
    /* (we make a copy instead of calling the user/library callback directly in
     *  this routine because this fractal heap 'op' callback routine is called
     *  with the direct block protected and if the callback routine invokes an
     *  HDF5 routine, it could attempt to re-protect that direct block for the
     *  heap, causing the HDF5 routine called to fail)
     */
    if(NULL == (udata->lnk = H5O_decode(udata->f, udata->dxpl_id, obj, H5O_LINK_ID)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode link")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_iterate_fh_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_iterate_bt2_cb
 *
 * Purpose:	v2 B-tree callback for dense link storage iterator
 *
 * Return:	H5B2_ITER_ERROR/H5B2_ITER_CONT/H5B2_ITER_STOP
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 11 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_dense_iterate_bt2_cb(const void *_record, void *_bt2_udata)
{
    const H5G_dense_bt2_name_rec_t *record = (const H5G_dense_bt2_name_rec_t *)_record;
    H5G_bt2_it_ud1_t *bt2_udata = (H5G_bt2_it_ud1_t *)_bt2_udata;         /* User data for callback */
    int ret_value = H5B2_ITER_CONT;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_dense_iterate_bt2_cb)

    /* Check for skipping links */
    if(bt2_udata->skip > 0)
        --bt2_udata->skip;
    else {
        H5G_fh_it_ud1_t fh_udata;       /* User data for fractal heap 'op' callback */

        /* Prepare user data for callback */
        /* down */
        fh_udata.f = bt2_udata->f;
        fh_udata.dxpl_id = bt2_udata->dxpl_id;

        /* Call fractal heap 'op' routine, to copy the link information */
        if(H5HF_op(bt2_udata->fheap, bt2_udata->dxpl_id, record->id,
                H5G_dense_iterate_fh_cb, &fh_udata) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTOPERATE, H5B2_ITER_ERROR, "link found callback failed")

        /* Check for internal callback with link info */
        if(bt2_udata->lib_internal)
            /* Call the library's callback */
            ret_value = (bt2_udata->op.lib_op)(fh_udata.lnk, bt2_udata->op_data);
        else
            /* Make the application's callback */
            ret_value = (bt2_udata->op.app_op)(bt2_udata->gid, fh_udata.lnk->name, bt2_udata->op_data);

        /* Release the space allocated for the link */
        H5O_free(H5O_LINK_ID, fh_udata.lnk);
    } /* end else */

    /* Increment the number of entries passed through */
    /* (whether we skipped them or not) */
    if(bt2_udata->last_lnk)
        (*bt2_udata->last_lnk)++;

    /* Check for callback failure and pass along return value */
    if(ret_value < 0)
        HERROR(H5E_SYM, H5E_CANTNEXT, "iteration operator failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_iterate_bt2_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_iterate
 *
 * Purpose:	Iterate over the objects in a group using dense link storage
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 11 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_dense_iterate(H5F_t *f, hid_t dxpl_id, H5_iter_order_t order, hid_t gid,
    const H5O_linfo_t *linfo, hbool_t lib_internal, int skip, int *last_lnk,
    H5G_link_iterate_t op, void *op_data)
{
    H5G_bt2_it_ud1_t udata;             /* User data for iterator callback */
    H5HF_t *fheap = NULL;               /* Fractal heap handle */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_dense_iterate, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(linfo);
    HDassert(op.lib_op);

    /* Open the fractal heap */
    if(NULL == (fheap = H5HF_open(f, dxpl_id, linfo->link_fheap_addr)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")

    /* Check on iteration order */
    /* ("native" iteration order is unordered for this link storage mechanism) */
    if(order == H5_ITER_NATIVE) {
        /* Construct the user data for v2 B-tree iterator callback */
        udata.f = f;
        udata.dxpl_id = dxpl_id;
        udata.fheap = fheap;
        udata.lib_internal = lib_internal;
        udata.gid = gid;
        udata.skip = skip;
        udata.last_lnk = last_lnk;
        udata.op = op;
        udata.op_data = op_data;

        /* Iterate over the records in the v2 B-tree */
        /* (XXX: by name, currently) */
        if((ret_value = H5B2_iterate(f, dxpl_id, H5G_BT2_NAME, linfo->name_bt2_addr,
                H5G_dense_iterate_bt2_cb, &udata)) < 0)
            HERROR(H5E_SYM, H5E_BADITER, "link iteration failed");
    } /* end if */
    else {
        H5G_link_table_t ltable;        /* Table of links */
        size_t u;                       /* Local index variable */

        /* Build the table of links for this group */
        if(H5G_dense_build_table(f, dxpl_id, linfo, order, &ltable) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTNEXT, FAIL, "error iterating over links")

        /* Iterate over link messages */
        for(u = 0, ret_value = H5B_ITER_CONT; u < ltable.nlinks && !ret_value; u++) {
            if(skip > 0)
                --skip;
            else {
                /* Check for internal callback with link info */
                if(lib_internal)
                    ret_value = (op.lib_op)(&(ltable.lnks[u]), op_data);
                else
                    ret_value = (op.app_op)(gid, ltable.lnks[u].name, op_data);
            } /* end else */

            /* Increment the number of entries passed through */
            /* (whether we skipped them or not) */
            if(last_lnk)
                (*last_lnk)++;
        } /* end for */

        /* Check for callback failure and pass along return value */
        if(ret_value < 0)
            HERROR(H5E_SYM, H5E_CANTNEXT, "iteration operator failed");

        /* Free link table information */
        if(H5G_obj_release_table(&ltable) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to release link table")
    } /* end else */

done:
    /* Release resources */
    if(fheap)
        if(H5HF_close(fheap, dxpl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close fractal heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_get_name_by_idx_fh_cb
 *
 * Purpose:	Callback for fractal heap operator, to retrieve name according
 *              to an index
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 19 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_dense_get_name_by_idx_fh_cb(const void *obj, size_t UNUSED obj_len, void *_udata)
{
    H5G_fh_gnbi_ud1_t *udata = (H5G_fh_gnbi_ud1_t *)_udata;       /* User data for fractal heap 'op' callback */
    H5O_link_t *lnk;            /* Pointer to link created from heap object */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_dense_get_name_by_idx_fh_cb)

    /* Decode link information */
    if(NULL == (lnk = H5O_decode(udata->f, udata->dxpl_id, obj, H5O_LINK_ID)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode link")

    /* Get the length of the name */
    udata->name_len = (ssize_t)HDstrlen(lnk->name);

    /* Copy the name into the user's buffer, if given */
    if(udata->name) {
        HDstrncpy(udata->name, lnk->name, MIN((size_t)(udata->name_len + 1), udata->name_size));
        if((size_t)udata->name_len >= udata->name_size)
            udata->name[udata->name_size - 1] = '\0';
    } /* end if */

    /* Release the space allocated for the link */
    H5O_free(H5O_LINK_ID, lnk);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_get_name_by_idx_fh_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_get_name_by_idx_bt2_cb
 *
 * Purpose:	v2 B-tree callback for dense link storage 'get name by idx' call
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 19 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_dense_get_name_by_idx_bt2_cb(const void *_record, void *_bt2_udata)
{
    const H5G_dense_bt2_name_rec_t *record = (const H5G_dense_bt2_name_rec_t *)_record;
    H5G_bt2_gnbi_ud1_t *bt2_udata = (H5G_bt2_gnbi_ud1_t *)_bt2_udata;         /* User data for callback */
    H5G_fh_gnbi_ud1_t fh_udata;         /* User data for fractal heap 'op' callback */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_dense_get_name_by_idx_bt2_cb)

    /* Prepare user data for callback */
    /* down */
    fh_udata.f = bt2_udata->f;
    fh_udata.dxpl_id = bt2_udata->dxpl_id;
    fh_udata.name = bt2_udata->name;
    fh_udata.name_size = bt2_udata->name_size;

    /* Call fractal heap 'op' routine, to perform user callback */
    if(H5HF_op(bt2_udata->fheap, bt2_udata->dxpl_id, record->id,
            H5G_dense_get_name_by_idx_fh_cb, &fh_udata) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPERATE, FAIL, "link found callback failed")

    /* Set the name's full length to return */
    bt2_udata->name_len = fh_udata.name_len;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_get_name_by_idx_bt2_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_get_name_by_idx
 *
 * Purpose:     Returns the name of objects in the group by giving index.
 *
 * Return:	Success:        Non-negative, length of name
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 19 2006
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5G_dense_get_name_by_idx(H5F_t  *f, hid_t dxpl_id, H5O_linfo_t *linfo,
    hsize_t idx, char* name, size_t size)
{
    H5HF_t *fheap = NULL;       /* Fractal heap handle */
    H5G_bt2_gnbi_ud1_t udata;   /* User data for v2 B-tree callback */
    ssize_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI(H5G_dense_get_name_by_idx, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(linfo);

    /* Open the fractal heap */
    if(NULL == (fheap = H5HF_open(f, dxpl_id, linfo->link_fheap_addr)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")

    /* Set up the user data for the v2 B-tree 'record remove' callback */
    udata.f = f;
    udata.dxpl_id = dxpl_id;
    udata.fheap = fheap;
    udata.name = name;
    udata.name_size = size;

    /* Retrieve the name according to the v2 B-tree's index order */
    /* (XXX: using name index currently) */
    if(H5B2_index(f, dxpl_id, H5G_BT2_NAME, linfo->name_bt2_addr, idx,
            H5G_dense_get_name_by_idx_bt2_cb, &udata) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTLIST, FAIL, "can't locate object in v2 B-tree")

    /* Set return value */
    ret_value = udata.name_len;

done:
    /* Release resources */
    if(fheap)
        if(H5HF_close(fheap, dxpl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close fractal heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_get_name_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_get_type_by_idx_fh_cb
 *
 * Purpose:	Callback for fractal heap operator, to retrieve type according
 *              to an index
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 19 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_dense_get_type_by_idx_fh_cb(const void *obj, size_t UNUSED obj_len, void *_udata)
{
    H5G_fh_gtbi_ud1_t *udata = (H5G_fh_gtbi_ud1_t *)_udata;       /* User data for fractal heap 'op' callback */
    H5O_link_t *lnk;            /* Pointer to link created from heap object */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_dense_get_type_by_idx_fh_cb)

    /* Decode link information */
    if(NULL == (lnk = H5O_decode(udata->f, udata->dxpl_id, obj, H5O_LINK_ID)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode link")

    /* Get the type of the link */
    /* Determine type of object */
    if(lnk->type == H5L_TYPE_SOFT)
        udata->type = H5G_LINK;
    else if(lnk->type >= H5L_TYPE_UD_MIN)
        udata->type = H5G_UDLINK;
    else if(lnk->type == H5L_TYPE_HARD) {
        H5O_loc_t tmp_oloc;             /* Temporary object location */

        /* Build temporary object location */
        tmp_oloc.file = udata->f;
        tmp_oloc.addr = lnk->u.hard.addr;

        /* Get the type of the object */
        if((udata->type = H5O_obj_type(&tmp_oloc, udata->dxpl_id)) == H5G_UNKNOWN)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't determine object type")
    } /* end if */
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "unknown link type")

    /* Release the space allocated for the link */
    H5O_free(H5O_LINK_ID, lnk);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_get_type_by_idx_fh_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_get_type_by_idx_bt2_cb
 *
 * Purpose:	v2 B-tree callback for dense link storage 'get type by idx' call
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 19 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_dense_get_type_by_idx_bt2_cb(const void *_record, void *_bt2_udata)
{
    const H5G_dense_bt2_name_rec_t *record = (const H5G_dense_bt2_name_rec_t *)_record;
    H5G_bt2_gtbi_ud1_t *bt2_udata = (H5G_bt2_gtbi_ud1_t *)_bt2_udata;         /* User data for callback */
    H5G_fh_gtbi_ud1_t fh_udata;         /* User data for fractal heap 'op' callback */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_dense_get_type_by_idx_bt2_cb)

    /* Prepare user data for callback */
    /* down */
    fh_udata.f = bt2_udata->f;
    fh_udata.dxpl_id = bt2_udata->dxpl_id;

    /* Call fractal heap 'op' routine, to perform user callback */
    if(H5HF_op(bt2_udata->fheap, bt2_udata->dxpl_id, record->id,
            H5G_dense_get_type_by_idx_fh_cb, &fh_udata) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPERATE, FAIL, "link found callback failed")

    /* Set the link's type to return */
    bt2_udata->type = fh_udata.type;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_get_type_by_idx_bt2_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_get_type_by_idx
 *
 * Purpose:     Returns the type of objects in the group by giving index.
 *
 * Return:	Success:        Non-negative, length of name
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 19 2006
 *
 *-------------------------------------------------------------------------
 */
H5G_obj_t
H5G_dense_get_type_by_idx(H5F_t  *f, hid_t dxpl_id, H5O_linfo_t *linfo,
    hsize_t idx)
{
    H5HF_t *fheap = NULL;       /* Fractal heap handle */
    H5G_bt2_gtbi_ud1_t udata;   /* User data for v2 B-tree callback */
    H5G_obj_t ret_value;        /* Return value */

    FUNC_ENTER_NOAPI(H5G_dense_get_type_by_idx, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(linfo);

    /* Open the fractal heap */
    if(NULL == (fheap = H5HF_open(f, dxpl_id, linfo->link_fheap_addr)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")

    /* Set up the user data for the v2 B-tree 'record remove' callback */
    udata.f = f;
    udata.dxpl_id = dxpl_id;
    udata.fheap = fheap;

    /* Retrieve the name according to the v2 B-tree's index order */
    /* (XXX: using name index currently) */
    if(H5B2_index(f, dxpl_id, H5G_BT2_NAME, linfo->name_bt2_addr, idx,
            H5G_dense_get_type_by_idx_bt2_cb, &udata) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTLIST, FAIL, "can't locate object in v2 B-tree")

    /* Set return value */
    ret_value = udata.type;

done:
    /* Release resources */
    if(fheap)
        if(H5HF_close(fheap, dxpl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close fractal heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_get_type_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_remove_fh_cb
 *
 * Purpose:	Callback for fractal heap operator when removing over links
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 12 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_dense_remove_fh_cb(const void *obj, size_t UNUSED obj_len, void *_udata)
{
    H5G_fh_rem_ud1_t *udata = (H5G_fh_rem_ud1_t *)_udata;       /* User data for fractal heap 'op' callback */
    H5O_link_t *lnk;            /* Pointer to link created from heap object */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_dense_remove_fh_cb)

    /* Decode link information */
    if(NULL == (lnk = H5O_decode(udata->f, udata->dxpl_id, obj, H5O_LINK_ID)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, H5B2_ITER_ERROR, "can't decode link")

    /* Retrieve the link's object type & decr. the ref. count on hard links, if requested */
    switch(lnk->type) {
        case H5L_TYPE_HARD:
            {
                H5O_loc_t tmp_oloc;             /* Temporary object location */

                /* Build temporary object location */
                tmp_oloc.file = udata->f;
                tmp_oloc.addr = lnk->u.hard.addr;

                /* Get the type of the object */
                /* Note: no way to check for error :-( */
                if(udata->obj_type)
                    *(udata->obj_type) = H5O_obj_type(&tmp_oloc, udata->dxpl_id);

                /* Decrement the ref count for the object, if requested */
                if(udata->adj_link)
                    if(H5O_link(&tmp_oloc, -1, udata->dxpl_id) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to decrement object link count")
            }
            break;

        case H5L_TYPE_SOFT:
            if(udata->obj_type)
                *(udata->obj_type) = H5G_LINK;
            break;

        default:  /* User-defined link */
            if(lnk->type < H5L_TYPE_UD_MIN)
                HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "unknown link type")
            if(udata->obj_type)
                *(udata->obj_type) = H5G_UDLINK;
    } /* end switch */

    /* Release the space allocated for the link */
    H5O_free(H5O_LINK_ID, lnk);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_remove_fh_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_remove_cb
 *
 * Purpose:	v2 B-tree callback for dense link storage record removal
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 12 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_dense_remove_bt2_cb(const void *_record, void *_bt2_udata)
{
    const H5G_dense_bt2_name_rec_t *record = (const H5G_dense_bt2_name_rec_t *)_record;
    H5G_bt2_ud_rem_t *bt2_udata = (H5G_bt2_ud_rem_t *)_bt2_udata;         /* User data for callback */
    H5G_fh_rem_ud1_t fh_udata;          /* User data for fractal heap 'op' callback */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_dense_remove_bt2_cb)

    /* Set up the user data for fractalheap 'op' callback */
    fh_udata.f = bt2_udata->common.f;
    fh_udata.dxpl_id = bt2_udata->common.dxpl_id;
    fh_udata.adj_link = bt2_udata->adj_link;
    fh_udata.obj_type = bt2_udata->obj_type;

    /* Call fractal heap 'op' routine, to perform user callback */
    if(H5HF_op(bt2_udata->common.fheap, bt2_udata->common.dxpl_id, record->id,
            H5G_dense_remove_fh_cb, &fh_udata) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPERATE, FAIL, "link removal callback failed")

    /* Remove record from fractal heap, if requested */
    if(bt2_udata->rem_from_fheap)
        if(H5HF_remove(bt2_udata->common.fheap, bt2_udata->common.dxpl_id, record->id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTREMOVE, FAIL, "unable to remove link from fractal heap")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_remove_bt2_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_remove
 *
 * Purpose:	Remove a link from the dense storage of a group
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 12 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_dense_remove(H5F_t *f, hid_t dxpl_id, const H5O_linfo_t *linfo,
    const char *name, H5G_obj_t *obj_type)
{
    H5HF_t *fheap = NULL;               /* Fractal heap handle */
    H5G_bt2_ud_rem_t udata;          /* User data for v2 B-tree record removal */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_dense_remove, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(linfo);
    HDassert(name && *name);
    HDassert(obj_type);

    /* Open the fractal heap */
    if(NULL == (fheap = H5HF_open(f, dxpl_id, linfo->link_fheap_addr)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")

    /* Set up the user data for the v2 B-tree 'record remove' callback */
    udata.common.f = f;
    udata.common.dxpl_id = dxpl_id;
    udata.common.fheap = fheap;
    udata.common.name = name;
    udata.common.name_hash = H5_checksum_lookup3(name, HDstrlen(name), 0);
    udata.common.found_op = NULL;
    udata.common.found_op_data = NULL;
    udata.adj_link = TRUE;
    udata.rem_from_fheap = TRUE;
    udata.obj_type = obj_type;

    /* Remove the record from the name index v2 B-tree */
    if(H5B2_remove(f, dxpl_id, H5G_BT2_NAME, linfo->name_bt2_addr,
            &udata, H5G_dense_remove_bt2_cb, &udata) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTREMOVE, FAIL, "unable to remove link from name index v2 B-tree")

/* XXX: Remove the link from the creation order index v2 B-tree */

done:
    /* Release resources */
    if(fheap)
        if(H5HF_close(fheap, dxpl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close fractal heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5G_dense_delete
 *
 * Purpose:	Delete the dense storage for a group
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 12 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_dense_delete(H5F_t *f, hid_t dxpl_id, H5O_linfo_t *linfo, hbool_t adj_link)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_dense_delete, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(linfo);

    /* Check if we are to adjust the ref. count for all the links */
    if(adj_link) {
        H5HF_t *fheap = NULL;               /* Fractal heap handle */
        H5G_bt2_ud_rem_t udata;          /* User data for v2 B-tree record removal */

        /* Open the fractal heap */
        if(NULL == (fheap = H5HF_open(f, dxpl_id, linfo->link_fheap_addr)))
            HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open fractal heap")

        /* Set up the user data for the v2 B-tree 'record remove' callback */
        udata.common.f = f;
        udata.common.dxpl_id = dxpl_id;
        udata.common.fheap = fheap;
        udata.common.name = NULL;
        udata.common.name_hash = 0;
        udata.common.found_op = NULL;
        udata.common.found_op_data = NULL;
        udata.adj_link = TRUE;
        udata.rem_from_fheap = FALSE;
        udata.obj_type = NULL;

        /* Delete the name index, adjusting the ref. count on links removed */
        if(H5B2_delete(f, dxpl_id, H5G_BT2_NAME, linfo->name_bt2_addr, H5G_dense_remove_bt2_cb, &udata) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to delete v2 B-tree for name index")

        /* Close the fractal heap */
        if(H5HF_close(fheap, dxpl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close fractal heap")
    } /* end if */
    else {
        /* Delete the name index, without adjusting the ref. count on the links  */
        if(H5B2_delete(f, dxpl_id, H5G_BT2_NAME, linfo->name_bt2_addr, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to delete v2 B-tree for name index")
    } /* end else */
    linfo->name_bt2_addr = HADDR_UNDEF;

/* XXX: Check if we should delete the creation order index v2 B-tree */

    /* Delete the fractal heap */
    if(H5HF_delete(f, dxpl_id, linfo->link_fheap_addr) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to delete fractal heap")
    linfo->link_fheap_addr = HADDR_UNDEF;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_dense_delete() */

