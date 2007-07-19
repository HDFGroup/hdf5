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

/*-------------------------------------------------------------------------
 *
 * Created:		H5Aint.c
 *			Dec 18 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Internal routines for managing attributes.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5A_PACKAGE		/*suppress error about including H5Apkg  */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Apkg.h"		/* Attributes	  			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

/* Data exchange structure to use when building table of compact attributes for an object */
typedef struct {
    H5F_t       *f;             /* Pointer to file that fractal heap is in */
    hid_t       dxpl_id;        /* DXPL for operation                */
    H5A_attr_table_t *atable;   /* Pointer to attribute table to build */
    size_t curr_attr;           /* Current attribute to operate on */
    hbool_t bogus_crt_idx;      /* Whether bogus creation index values need to be set */
} H5A_compact_bt_ud_t;

/* Data exchange structure to use when building table of dense attributes for an object */
typedef struct {
    H5A_attr_table_t *atable;   /* Pointer to attribute table to build */
    size_t curr_attr;           /* Current attribute to operate on */
} H5A_dense_bt_ud_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

static herr_t H5A_compact_build_table_cb(H5O_t *oh, H5O_mesg_t *mesg/*in,out*/,
    unsigned sequence, unsigned *oh_flags_ptr, void *_udata/*in,out*/);
static herr_t H5A_dense_build_table_cb(const H5A_t *attr, void *_udata);
static int H5A_attr_cmp_name_inc(const void *attr1, const void *attr2);
static int H5A_attr_cmp_name_dec(const void *attr1, const void *attr2);
static int H5A_attr_cmp_corder_inc(const void *attr1, const void *attr2);
static int H5A_attr_cmp_corder_dec(const void *attr1, const void *attr2);
static herr_t H5A_attr_sort_table(H5A_attr_table_t *atable, H5_index_t idx_type,
    H5_iter_order_t order);

/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5A_compact_build_table_cb
 *
 * Purpose:	Object header iterator callback routine to copy attribute
 *              into table.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec 18 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_compact_build_table_cb(H5O_t UNUSED *oh, H5O_mesg_t *mesg/*in,out*/,
    unsigned sequence, hbool_t UNUSED *oh_modified, void *_udata/*in,out*/)
{
    H5A_compact_bt_ud_t *udata = (H5A_compact_bt_ud_t *)_udata;   /* Operator user data */
    herr_t ret_value = H5_ITER_CONT;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5A_compact_build_table_cb)

    /* check args */
    HDassert(mesg);

    /* Check for re-allocating table */
    if(udata->curr_attr == udata->atable->nattrs) {
        size_t n = MAX(1, 2 * udata->atable->nattrs);
        H5A_t *table = (H5A_t *)H5MM_realloc(udata->atable->attrs,
                                          n * sizeof(H5A_t));

        if(!table)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, H5_ITER_ERROR, "unable to extend attribute table")
        udata->atable->attrs = table;
        udata->atable->nattrs = n;
    } /* end if */

    /* Copy attribute into table */
    if(NULL == H5A_copy(&udata->atable->attrs[udata->curr_attr], (const H5A_t *)mesg->native))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, H5_ITER_ERROR, "can't copy attribute")

    /* Assign [somewhat arbitrary] creation order value, if requested */
    if(udata->bogus_crt_idx)
        udata->atable->attrs[udata->curr_attr].crt_idx = sequence;

    /* Increment current attribute */
    udata->curr_attr++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_compact_build_table_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5A_compact_build_table
 *
 * Purpose:     Builds a table containing a sorted list of attributes for
 *              an object
 *
 * Note:	Used for building table of attributes in non-native iteration
 *              order for an index
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Dec 18, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A_compact_build_table(H5F_t *f, hid_t dxpl_id, H5O_t *oh, H5_index_t idx_type,
    H5_iter_order_t order, H5A_attr_table_t *atable)
{
    H5A_compact_bt_ud_t udata;                  /* User data for iteration callback */
    H5O_mesg_operator_t op;             /* Wrapper for operator */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5A_compact_build_table)

    /* Sanity check */
    HDassert(f);
    HDassert(oh);
    HDassert(atable);

    /* Initialize table */
    atable->attrs = NULL;
    atable->nattrs = 0;

    /* Set up user data for iteration */
    udata.f = f;
    udata.dxpl_id = dxpl_id;
    udata.atable = atable;
    udata.curr_attr = 0;
    udata.bogus_crt_idx = (oh->version == H5O_VERSION_1 ||
            !(oh->flags & H5O_HDR_ATTR_CRT_ORDER_TRACKED)) ? TRUE : FALSE;

    /* Iterate over existing attributes, checking for attribute with same name */
    op.op_type = H5O_MESG_OP_LIB;
    op.u.lib_op = H5A_compact_build_table_cb;
    if(H5O_msg_iterate_real(f, oh, H5O_MSG_ATTR, &op, &udata, dxpl_id) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_BADITER, FAIL, "error building attribute table")

    /* Correct # of attributes in table */
    atable->nattrs = udata.curr_attr;

    /* Sort attribute table in correct iteration order */
    if(H5A_attr_sort_table(atable, idx_type, order) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTSORT, FAIL, "error sorting attribute table")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_compact_build_table() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_build_table_cb
 *
 * Purpose:	Callback routine for building table of attributes from dense
 *              attribute storage.
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec 11 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_dense_build_table_cb(const H5A_t *attr, void *_udata)
{
    H5A_dense_bt_ud_t *udata = (H5A_dense_bt_ud_t *)_udata;     /* 'User data' passed in */
    herr_t ret_value = H5_ITER_CONT;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5A_dense_build_table_cb)

    /* check arguments */
    HDassert(attr);
    HDassert(udata);
    HDassert(udata->curr_attr < udata->atable->nattrs);

    /* Copy attribute information */
    if(NULL == H5A_copy(&udata->atable->attrs[udata->curr_attr], attr))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, H5_ITER_ERROR, "can't copy attribute")

    /* Increment number of attributes stored */
    udata->curr_attr++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_build_table_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5A_dense_build_table
 *
 * Purpose:     Builds a table containing a sorted list of attributes for
 *              an object
 *
 * Note:	Used for building table of attributes in non-native iteration
 *              order for an index.  Uses the "name" index to retrieve records,
 *		but the 'idx_type' index for sorting them.
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Dec 11, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A_dense_build_table(H5F_t *f, hid_t dxpl_id, const H5O_ainfo_t *ainfo,
    H5_index_t idx_type, H5_iter_order_t order, H5A_attr_table_t *atable)
{
    hsize_t nrec;                       /* # of records in v2 B-tree */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5A_dense_build_table)

    /* Sanity check */
    HDassert(f);
    HDassert(ainfo);
    HDassert(H5F_addr_defined(ainfo->fheap_addr));
    HDassert(H5F_addr_defined(ainfo->name_bt2_addr));
    HDassert(atable);

    /* Retrieve # of records in "name" B-tree */
    /* (should be same # of records in all indices) */
    if(H5B2_get_nrec(f, dxpl_id, H5A_BT2_NAME, ainfo->name_bt2_addr, &nrec) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't retrieve # of records in index")

    /* Set size of table */
    H5_CHECK_OVERFLOW(nrec, /* From: */ hsize_t, /* To: */ size_t);
    atable->nattrs = (size_t)nrec;

    /* Allocate space for the table entries */
    if(atable->nattrs > 0) {
        H5A_dense_bt_ud_t udata;       /* User data for iteration callback */
        H5A_attr_iter_op_t attr_op;    /* Attribute operator */

        /* Allocate the table to store the attributes */
        if((atable->attrs = (H5A_t *)H5MM_malloc(sizeof(H5A_t) * atable->nattrs)) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* Set up user data for iteration */
        udata.atable = atable;
        udata.curr_attr = 0;

        /* Build iterator operator */
        attr_op.op_type = H5A_ATTR_OP_LIB;
        attr_op.u.lib_op = H5A_dense_build_table_cb;

        /* Iterate over the links in the group, building a table of the link messages */
        if(H5A_dense_iterate(f, dxpl_id, (hid_t)0, ainfo, H5_INDEX_NAME,
                H5_ITER_NATIVE, (hsize_t)0, NULL, &attr_op, &udata) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "error building attribute table")

        /* Sort attribute table in correct iteration order */
        if(H5A_attr_sort_table(atable, idx_type, order) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTSORT, FAIL, "error sorting attribute table")
    } /* end if */
    else
        atable->attrs = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_dense_build_table() */


/*-------------------------------------------------------------------------
 * Function:	H5A_attr_cmp_name_inc
 *
 * Purpose:	Callback routine for comparing two attribute names, in
 *              increasing alphabetic order
 *
 * Return:	An integer less than, equal to, or greater than zero if the
 *              first argument is considered to be respectively less than,
 *              equal to, or greater than the second.  If two members compare
 *              as equal, their order in the sorted array is undefined.
 *              (i.e. same as strcmp())
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec 11 2006
 *
 *-------------------------------------------------------------------------
 */
static int
H5A_attr_cmp_name_inc(const void *attr1, const void *attr2)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5A_attr_cmp_name_inc)

    FUNC_LEAVE_NOAPI(HDstrcmp(((const H5A_t *)attr1)->name, ((const H5A_t *)attr2)->name))
} /* end H5A_attr_cmp_name_inc() */


/*-------------------------------------------------------------------------
 * Function:	H5A_attr_cmp_name_dec
 *
 * Purpose:	Callback routine for comparing two attribute names, in
 *              decreasing alphabetic order
 *
 * Return:	An integer less than, equal to, or greater than zero if the
 *              second argument is considered to be respectively less than,
 *              equal to, or greater than the first.  If two members compare
 *              as equal, their order in the sorted array is undefined.
 *              (i.e. opposite of strcmp())
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Feb  8 2007
 *
 *-------------------------------------------------------------------------
 */
static int
H5A_attr_cmp_name_dec(const void *attr1, const void *attr2)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5A_attr_cmp_name_dec)

    FUNC_LEAVE_NOAPI(HDstrcmp(((const H5A_t *)attr2)->name, ((const H5A_t *)attr1)->name))
} /* end H5A_attr_cmp_name_dec() */


/*-------------------------------------------------------------------------
 * Function:	H5A_attr_cmp_corder_inc
 *
 * Purpose:	Callback routine for comparing two attributes, in
 *              increasing creation order
 *
 * Return:	An integer less than, equal to, or greater than zero if the
 *              first argument is considered to be respectively less than,
 *              equal to, or greater than the second.  If two members compare
 *              as equal, their order in the sorted array is undefined.
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Feb  8 2007
 *
 *-------------------------------------------------------------------------
 */
static int
H5A_attr_cmp_corder_inc(const void *attr1, const void *attr2)
{
    int ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5A_attr_cmp_corder_inc)

    if(((const H5A_t *)attr1)->crt_idx < ((const H5A_t *)attr2)->crt_idx)
        ret_value = -1;
    else if(((const H5A_t *)attr1)->crt_idx > ((const H5A_t *)attr2)->crt_idx)
        ret_value = 1;
    else
        ret_value = 0;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_attr_cmp_corder_inc() */


/*-------------------------------------------------------------------------
 * Function:	H5A_attr_cmp_corder_dec
 *
 * Purpose:	Callback routine for comparing two attributes, in
 *              decreasing creation order
 *
 * Return:	An integer less than, equal to, or greater than zero if the
 *              second argument is considered to be respectively less than,
 *              equal to, or greater than the first.  If two members compare
 *              as equal, their order in the sorted array is undefined.
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Feb  8 2007
 *
 *-------------------------------------------------------------------------
 */
static int
H5A_attr_cmp_corder_dec(const void *attr1, const void *attr2)
{
    int ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5A_attr_cmp_corder_dec)

    if(((const H5A_t *)attr1)->crt_idx < ((const H5A_t *)attr2)->crt_idx)
        ret_value = 1;
    else if(((const H5A_t *)attr1)->crt_idx > ((const H5A_t *)attr2)->crt_idx)
        ret_value = -1;
    else
        ret_value = 0;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_attr_cmp_corder_dec() */


/*-------------------------------------------------------------------------
 * Function:	H5A_attr_sort_table
 *
 * Purpose:     Sort table containing a list of attributes for an object
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Dec 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A_attr_sort_table(H5A_attr_table_t *atable, H5_index_t idx_type,
    H5_iter_order_t order)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5A_attr_sort_table)

    /* Sanity check */
    HDassert(atable);

    /* Pick appropriate comparison routine */
    if(idx_type == H5_INDEX_NAME) {
        if(order == H5_ITER_INC)
            HDqsort(atable->attrs, atable->nattrs, sizeof(H5A_t), H5A_attr_cmp_name_inc);
        else if(order == H5_ITER_DEC)
            HDqsort(atable->attrs, atable->nattrs, sizeof(H5A_t), H5A_attr_cmp_name_dec);
        else
            HDassert(order == H5_ITER_NATIVE);
    } /* end if */
    else {
        HDassert(idx_type == H5_INDEX_CRT_ORDER);
        if(order == H5_ITER_INC)
            HDqsort(atable->attrs, atable->nattrs, sizeof(H5A_t), H5A_attr_cmp_corder_inc);
        else if(order == H5_ITER_DEC)
            HDqsort(atable->attrs, atable->nattrs, sizeof(H5A_t), H5A_attr_cmp_corder_dec);
        else
            HDassert(order == H5_ITER_NATIVE);
    } /* end else */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5A_attr_sort_table() */


/*-------------------------------------------------------------------------
 * Function:	H5A_attr_iterate_table
 *
 * Purpose:     Iterate over table containing a list of attributes for an object,
 *              making appropriate callbacks
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Dec 18, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A_attr_iterate_table(const H5A_attr_table_t *atable, hsize_t skip,
    hsize_t *last_attr, hid_t loc_id, const H5A_attr_iter_op_t *attr_op,
    void *op_data)
{
    size_t u;                           /* Local index variable */
    herr_t ret_value = H5_ITER_CONT;   /* Return value */

    FUNC_ENTER_NOAPI(H5A_attr_iterate_table, FAIL)

    /* Sanity check */
    HDassert(atable);
    HDassert(attr_op);

    /* Skip over attributes, if requested */
    if(last_attr)
        *last_attr = skip;

    /* Iterate over attribute messages */
    H5_ASSIGN_OVERFLOW(/* To: */ u, /* From: */ skip, /* From: */ hsize_t, /* To: */ size_t)
    for(; u < atable->nattrs && !ret_value; u++) {
        /* Check which type of callback to make */
        switch(attr_op->op_type) {
            case H5A_ATTR_OP_APP2:
            {
                H5A_info_t ainfo;               /* Info for attribute */

                /* Get the attribute information */
                if(H5A_get_info(&atable->attrs[u], &ainfo) < 0)
                    HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, H5_ITER_ERROR, "unable to get attribute info")

                /* Make the application callback */
                ret_value = (attr_op->u.app_op2)(loc_id, atable->attrs[u].name, &ainfo, op_data);
                break;
            }

            case H5A_ATTR_OP_APP:
                /* Make the application callback */
                ret_value = (attr_op->u.app_op)(loc_id, atable->attrs[u].name, op_data);
                break;

            case H5A_ATTR_OP_LIB:
                /* Call the library's callback */
                ret_value = (attr_op->u.lib_op)(&(atable->attrs[u]), op_data);
        } /* end switch */

        /* Increment the number of entries passed through */
        if(last_attr)
            (*last_attr)++;
    } /* end for */

    /* Check for callback failure and pass along return value */
    if(ret_value < 0)
        HERROR(H5E_ATTR, H5E_CANTNEXT, "iteration operator failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_attr_iterate_table() */


/*-------------------------------------------------------------------------
 * Function:	H5A_attr_release_table
 *
 * Purpose:     Release table containing a list of attributes for an object
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Dec 11, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A_attr_release_table(H5A_attr_table_t *atable)
{
    size_t      u;                      /* Local index variable */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5A_attr_release_table)

    /* Sanity check */
    HDassert(atable);

    /* Release attribute info, if any */
    if(atable->nattrs > 0) {
        /* Free attribute message information */
        for(u = 0; u < atable->nattrs; u++)
            if(H5A_free(&(atable->attrs[u])) < 0)
                HGOTO_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "unable to release attribute")

        /* Free table of attributes */
        H5MM_xfree(atable->attrs);
    } /* end if */
    else
        HDassert(atable->attrs == NULL);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_attr_release_table() */


/*-------------------------------------------------------------------------
 * Function:    H5A_get_ainfo
 *
 * Purpose:     Retrieves the "attribute info" message for an object.  Also
 *              sets the number of attributes correctly, if it isn't set up yet.
 *
 * Return:	Success:	Ptr to message in native format.
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Mar 11 2007
 *
 *-------------------------------------------------------------------------
 */
H5O_ainfo_t *
H5A_get_ainfo(H5F_t *f, hid_t dxpl_id, H5O_t *oh, H5O_ainfo_t *ainfo)
{
    H5O_ainfo_t *ret_value;     /* Return value */
    
    FUNC_ENTER_NOAPI(H5A_get_ainfo, NULL)

    /* check arguments */
    HDassert(f);
    HDassert(oh);

    /* Retrieve the "attribute info" structure */
    if((ret_value = H5O_msg_read_real(f, dxpl_id, oh, H5O_AINFO_ID, ainfo))) {
        /* Check if we don't know how many attributes there are */
        if(ret_value->nattrs == HSIZET_MAX) {
            /* Check if we are using "dense" attribute storage */
            if(H5F_addr_defined(ret_value->fheap_addr)) {
                /* Retrieve # of records in "name" B-tree */
                /* (should be same # of records in all indices) */
                if(H5B2_get_nrec(f, dxpl_id, H5A_BT2_NAME, ret_value->name_bt2_addr, &ret_value->nattrs) < 0)
                    HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, NULL, "can't retrieve # of records in index")
            } /* end if */
            else
                /* Retrieve # of attributes from object header */
                ret_value->nattrs = oh->attr_msgs_seen;
        } /* end if */
    } /* end if */
    else
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, NULL, "attribute info message not present")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_get_ainfo() */


/*-------------------------------------------------------------------------
 * Function:    H5A_set_version
 *
 * Purpose:     Sets the correct version to encode attribute with.
 *              Chooses the oldest version possible, unless the "use the
 *              latest format" flag is set.
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Jul 17 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A_set_version(const H5F_t *f, H5A_t *attr)
{
    hbool_t type_shared, space_shared;  /* Flags to indicate that shared messages are used for this attribute */
    hbool_t use_latest_format;          /* Flag indicating the newest file format should be used */
    
    FUNC_ENTER_NOAPI_NOFUNC(H5A_set_version)

    /* check arguments */
    HDassert(f);
    HDassert(attr);

    /* Get the file's 'use the latest version of the format' flag */
    use_latest_format = H5F_USE_LATEST_FORMAT(f);

    /* Check whether datatype and dataspace are shared */
    if(H5O_msg_is_shared(H5O_DTYPE_ID, attr->dt) > 0)
        type_shared = TRUE;
    else
        type_shared = FALSE;

    if(H5O_msg_is_shared(H5O_SDSPACE_ID, attr->ds) > 0)
        space_shared = TRUE;
    else
        space_shared = FALSE;

    /* Check which version to encode attribute with */
    if(use_latest_format)
        attr->version = H5O_ATTR_VERSION_LATEST;      /* Write out latest version of format */
    else if(attr->encoding != H5T_CSET_ASCII)
        attr->version = H5O_ATTR_VERSION_3;   /* Write version which includes the character encoding */
    else if(type_shared || space_shared)
        attr->version = H5O_ATTR_VERSION_2;   /* Write out version with flag for indicating shared datatype or dataspace */
    else
        attr->version = H5O_ATTR_VERSION_1;   /* Write out basic version */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5A_set_version() */

