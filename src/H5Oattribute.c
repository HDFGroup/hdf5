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
 * Created:		H5Oattribute.c
 *			Dec 11 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Object header attribute routines.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5A_PACKAGE		/*suppress error about including H5Apkg	  */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Apkg.h"		/* Attributes	  			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5SMprivate.h"	/* Shared Object Header Messages	*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

/* User data for iteration when converting attributes to dense storage */
typedef struct {
    H5F_t      *f;              /* Pointer to file for insertion */
    hid_t dxpl_id;              /* DXPL during iteration */
} H5O_iter_cvt_t;

/* User data for iteration when opening an attribute */
typedef struct {
    /* down */
    H5F_t *f;                   /* Pointer to file attribute is in */
    hid_t dxpl_id;              /* DXPL for operation */
    const char *name;           /* Name of attribute to open */

    /* up */
    H5A_t *attr;                /* Attribute data to update object header with */
} H5O_iter_opn_t;

/* User data for iteration when updating an attribute */
typedef struct {
    /* down */
    H5F_t *f;                   /* Pointer to file attribute is in */
    hid_t dxpl_id;              /* DXPL for operation */
    H5A_t *attr;                /* Attribute data to update object header with */

    /* up */
    hbool_t found;              /* Whether the attribute was found */
} H5O_iter_wrt_t;

/* User data for iteration when renaming an attribute */
typedef struct {
    /* down */
    H5F_t *f;                   /* Pointer to file attribute is in */
    hid_t dxpl_id;              /* DXPL for operation */
    const char *old_name;       /* Old name of attribute */
    const char *new_name;       /* New name of attribute */

    /* up */
    hbool_t found;              /* Whether the attribute was found */
} H5O_iter_ren_t;

/* User data for iteration when iterating over attributes */
typedef struct {
    /* down */
    H5F_t *f;                   /* Pointer to file attribute is in */
    hid_t dxpl_id;              /* DXPL for operation */
    hid_t loc_id;               /* ID of object being iterated over */
    unsigned skip;              /* # of attributes to skip over */
    H5A_operator_t op;          /* Callback routine for each attribute */
    void *op_data;              /* User data for callback */

    /* up */
    unsigned count;             /* Count of attributes examined */
} H5O_iter_itr_t;

/* User data for iteration when removing an attribute */
typedef struct {
    /* down */
    H5F_t *f;                   /* Pointer to file attribute is in */
    hid_t dxpl_id;              /* DXPL for operation */
    const char *name;           /* Name of attribute to open */

    /* up */
    hbool_t found;              /* Found attribute to delete */
} H5O_iter_rm_t;


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



/*-------------------------------------------------------------------------
 * Function:	H5O_attr_to_dense_cb
 *
 * Purpose:	Object header iterator callback routine to convert compact
 *              attributes to dense attributes
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec  4 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_attr_to_dense_cb(H5O_t *oh, H5O_mesg_t *mesg/*in,out*/,
    unsigned UNUSED sequence, unsigned *oh_flags_ptr, void *_udata/*in,out*/)
{
    H5O_iter_cvt_t *udata = (H5O_iter_cvt_t *)_udata;   /* Operator user data */
    H5A_t shared_attr;                  /* Copy of shared attribute */
    H5A_t *attr;                        /* Pointer to attribute to insert */
    herr_t ret_value = H5_ITER_CONT;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_to_dense_cb)

    /* check args */
    HDassert(oh);
    HDassert(mesg);

    /* Check for shared attribute */
    if(mesg->flags & H5O_MSG_FLAG_SHARED) {
        /* Read the shared attribute in */
        if(NULL == H5O_shared_read(udata->f, udata->dxpl_id, mesg->native, H5O_MSG_ATTR, &shared_attr))
	    HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, H5_ITER_ERROR, "unable to read shared attribute")

        /* Point attribute to insert at shared attribute read in */
        attr = &shared_attr;
    } /* end if */
    else
        attr = mesg->native;

    /* Insert attribute into dense storage */
    if(H5A_dense_insert(udata->f, udata->dxpl_id, oh, mesg->flags, attr) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINSERT, H5_ITER_ERROR, "unable to add to dense storage")

    /* Convert message into a null message in the header */
    if(H5O_release_mesg(udata->f, udata->dxpl_id, oh, mesg, TRUE, FALSE) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, H5_ITER_ERROR, "unable to convert into null message")

    /* Indicate that the object header was modified */
    *oh_flags_ptr |= H5AC__DIRTIED_FLAG;

done:
    /* Release copy of shared attribute */
    if(attr == &shared_attr)
        H5O_attr_reset(&shared_attr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_to_dense_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_create
 *
 * Purpose:	Create a new attribute in the object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Friday, December  8, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_attr_create(const H5O_loc_t *loc, hid_t dxpl_id, H5A_t *attr)
{
    H5O_t *oh = NULL;                   /* Pointer to actual object header */
    unsigned oh_flags = H5AC__NO_FLAGS_SET;     /* Metadata cache flags for object header */
    unsigned mesg_flags = 0;            /* Flags for storing message */
    htri_t shared_mesg;                 /* Should this message be stored in the Shared Message table? */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_create)

    /* Check arguments */
    HDassert(loc);
    HDassert(attr);

    /* Should this message be written as a SOHM? */
    if((shared_mesg = H5SM_try_share(loc->file, dxpl_id, H5O_ATTR_ID, attr)) > 0)
        /* Mark the message as shared */
        mesg_flags |= H5O_MSG_FLAG_SHARED;
    else if(shared_mesg < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_WRITEERROR, FAIL, "error determining if message should be shared")

    /* Protect the object header to iterate over */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTLOAD, FAIL, "unable to load object header")

#ifdef QAK
HDfprintf(stderr, "%s: adding attribute to new-style object header\n", FUNC);
HDfprintf(stderr, "%s: oh->nattrs = %Hu\n", FUNC, oh->nattrs);
HDfprintf(stderr, "%s: oh->max_compact = %u\n", FUNC, oh->max_compact);
HDfprintf(stderr, "%s: oh->min_dense = %u\n", FUNC, oh->min_dense);
#endif /* QAK */
    /* Check for switching to "dense" attribute storage */
    if(oh->version > H5O_VERSION_1 && oh->nattrs == oh->max_compact &&
            !H5F_addr_defined(oh->attr_fheap_addr)) {
        H5O_iter_cvt_t udata;           /* User data for callback */
        H5O_mesg_operator_t op;         /* Wrapper for operator */
#ifdef QAK
HDfprintf(stderr, "%s: converting attributes to dense storage\n", FUNC);
#endif /* QAK */

        /* Create dense storage for attributes */
        if(H5A_dense_create(loc->file, dxpl_id, oh) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to create dense storage for attributes")

        /* Set up user data for callback */
        udata.f = loc->file;
        udata.dxpl_id = dxpl_id;

        /* Iterate over existing attributes, moving them to dense storage */
/* XXX: Test this with shared attributes */
        op.lib_op = H5O_attr_to_dense_cb;
        if(H5O_msg_iterate_real(loc->file, oh, H5O_MSG_ATTR, TRUE, op, &udata, dxpl_id, &oh_flags) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTCONVERT, FAIL, "error converting attributes to dense storage")
    } /* end if */

    /* Increment attribute count */
    oh->nattrs++;

    /* Check for storing attribute with dense storage */
    if(H5F_addr_defined(oh->attr_fheap_addr)) {
        /* Insert attribute into dense storage */
#ifdef QAK
HDfprintf(stderr, "%s: inserting attribute to dense storage\n", FUNC);
#endif /* QAK */
        if(H5A_dense_insert(loc->file, dxpl_id, oh, mesg_flags, attr) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINSERT, FAIL, "unable to add to dense storage")
    } /* end if */
    else {
        /* Append new message to object header */
        if(H5O_msg_append_real(loc->file, dxpl_id, oh, H5O_MSG_ATTR, mesg_flags, 0, attr, &oh_flags) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINSERT, FAIL, "unable to create new attribute in header")
    } /* end else */

    /* Update the modification time, if any */
    if(H5O_touch_oh(loc->file, dxpl_id, oh, FALSE, &oh_flags) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTUPDATE, FAIL, "unable to update time on object")

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_create */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_open_cb
 *
 * Purpose:	Object header iterator callback routine to open an
 *              attribute stored compactly.
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
H5O_attr_open_cb(H5O_t *oh, H5O_mesg_t *mesg/*in,out*/,
    unsigned UNUSED sequence, unsigned UNUSED *oh_flags_ptr, void *_udata/*in,out*/)
{
    H5O_iter_opn_t *udata = (H5O_iter_opn_t *)_udata;   /* Operator user data */
    herr_t ret_value = H5_ITER_CONT;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_open_cb)

    /* check args */
    HDassert(oh);
    HDassert(mesg);
    HDassert(!udata->attr);

    /* Check for shared message */
    if(mesg->flags & H5O_MSG_FLAG_SHARED) {
        H5A_t shared_attr;             /* Copy of shared attribute */

	/*
	 * If the message is shared then then the native pointer points to an
	 * H5O_MSG_SHARED message.  We use that information to look up the real
	 * message in the global heap or some other object header.
	 */
        if(NULL == H5O_shared_read(udata->f, udata->dxpl_id, mesg->native, H5O_MSG_ATTR, &shared_attr))
	    HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, H5_ITER_ERROR, "unable to read shared attribute")

        /* Check for correct attribute message to modify */
        if(HDstrcmp(shared_attr.name, udata->name) == 0) {
            /* Make a copy of the attribute to return */
            if(NULL == (udata->attr = H5A_copy(NULL, &shared_attr)))
                HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, H5_ITER_ERROR, "unable to copy attribute")
        } /* end if */

        /* Release copy of shared attribute */
        H5O_attr_reset(&shared_attr);
    } /* end if */
    else {
        /* Check for correct attribute message to modify */
        if(HDstrcmp(((H5A_t *)mesg->native)->name, udata->name) == 0) {
            /* Make a copy of the attribute to return */
            if(NULL == (udata->attr = H5A_copy(NULL, (H5A_t *)mesg->native)))
                HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, H5_ITER_ERROR, "unable to copy attribute")
        } /* end if */
    } /* end else */

    /* Set common info, if we found the correct attribute */
    if(udata->attr) {
        /* Stop iterating */
        ret_value = H5_ITER_STOP;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_open_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_open
 *
 * Purpose:	Open an existing attribute in an object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Monday, December 11, 2006
 *
 *-------------------------------------------------------------------------
 */
H5A_t *
H5O_attr_open(const H5O_loc_t *loc, const char *name, hid_t dxpl_id)
{
    H5O_t *oh = NULL;                   /* Pointer to actual object header */
    unsigned oh_flags = H5AC__NO_FLAGS_SET;     /* Metadata cache flags for object header */
    H5A_t *ret_value;                   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_open)

    /* Check arguments */
    HDassert(loc);
    HDassert(name);

    /* Protect the object header to iterate over */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTLOAD, NULL, "unable to load object header")

#ifdef QAK
HDfprintf(stderr, "%s: opening attribute to new-style object header\n", FUNC);
HDfprintf(stderr, "%s: oh->nattrs = %Hu\n", FUNC, oh->nattrs);
HDfprintf(stderr, "%s: oh->max_compact = %u\n", FUNC, oh->max_compact);
HDfprintf(stderr, "%s: oh->min_dense = %u\n", FUNC, oh->min_dense);
#endif /* QAK */
    /* Check for opening attribute with dense storage */
    if(H5F_addr_defined(oh->attr_fheap_addr)) {
/* XXX: Need to support/test shared attributes in dense storage */
        /* Open attribute in dense storage */
        if(NULL == (ret_value = H5A_dense_open(loc->file, dxpl_id, oh, name)))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "can't open attribute")
    } /* end if */
    else {
        H5O_iter_opn_t udata;           /* User data for callback */
        H5O_mesg_operator_t op;         /* Wrapper for operator */

        /* Set up user data for callback */
        udata.f = loc->file;
        udata.dxpl_id = dxpl_id;
        udata.name = name;
        udata.attr = NULL;

        /* Iterate over attributes, to locate correct one to open */
        op.lib_op = H5O_attr_open_cb;
        if(H5O_msg_iterate_real(loc->file, oh, H5O_MSG_ATTR, TRUE, op, &udata, dxpl_id, &oh_flags) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "error updating attribute")

        /* Check that we found the attribute */
        if(!udata.attr)
            HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, NULL, "can't locate attribute")

        /* Get attribute opened from object header */
        HDassert(udata.attr);
        ret_value = udata.attr;
    } /* end else */

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_PROTECT, NULL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_open */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_write_cb
 *
 * Purpose:	Object header iterator callback routine to update an
 *              attribute stored compactly.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec  4 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_attr_write_cb(H5O_t *oh, H5O_mesg_t *mesg/*in,out*/,
    unsigned UNUSED sequence, unsigned *oh_flags_ptr, void *_udata/*in,out*/)
{
    H5O_iter_wrt_t *udata = (H5O_iter_wrt_t *)_udata;   /* Operator user data */
    herr_t ret_value = H5_ITER_CONT;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_write_cb)

    /* check args */
    HDassert(oh);
    HDassert(mesg);
    HDassert(!udata->found);

    /* Check for shared message */
    if(mesg->flags & H5O_MSG_FLAG_SHARED) {
        H5A_t shared_attr;             /* Copy of shared attribute */

	/*
	 * If the message is shared then then the native pointer points to an
	 * H5O_MSG_SHARED message.  We use that information to look up the real
	 * message in the global heap or some other object header.
	 */
        if(NULL == H5O_shared_read(udata->f, udata->dxpl_id, mesg->native, H5O_MSG_ATTR, &shared_attr))
	    HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, H5_ITER_ERROR, "unable to read shared attribute")

        /* Check for correct attribute message to modify */
        if(HDstrcmp(shared_attr.name, udata->attr->name) == 0) {
            htri_t shared_mesg;         /* Whether the message should be shared */

            /* Store new version of message as a SOHM */
            /* (should always work, since we're not changing the size of the attribute) */
            if((shared_mesg = H5SM_try_share(udata->f, udata->dxpl_id, H5O_ATTR_ID, udata->attr)) == 0)
                HGOTO_ERROR(H5E_ATTR, H5E_BADMESG, H5_ITER_ERROR, "attribute changed sharing status")
            else if(shared_mesg < 0)
                HGOTO_ERROR(H5E_ATTR, H5E_BADMESG, H5_ITER_ERROR, "can't share attribute")

            /* Remove the old attribut from the SOHM index */
            if(H5SM_try_delete(udata->f, udata->dxpl_id, H5O_ATTR_ID, mesg->native) < 0)
                HGOTO_ERROR(H5E_ATTR, H5E_CANTFREE, H5_ITER_ERROR, "unable to delete shared attribute in shared storage")

            /* Extract shared message info from current attribute */
            if(NULL == H5O_attr_get_share(udata->attr, mesg->native))
                HGOTO_ERROR(H5E_ATTR, H5E_BADMESG, H5_ITER_ERROR, "can't get shared info")

            /* Indicate that we found the correct attribute */
            udata->found = TRUE;
        } /* end if */

        /* Release copy of shared attribute */
        H5O_attr_reset(&shared_attr);
    } /* end if */
    else {
        /* Check for correct attribute message to modify */
        if(HDstrcmp(((H5A_t *)mesg->native)->name, udata->attr->name) == 0) {
            /* Allocate storage for the message's data, if necessary */
            if(((H5A_t *)mesg->native)->data == NULL)
                if(NULL == (((H5A_t *)mesg->native)->data = H5FL_BLK_MALLOC(attr_buf, udata->attr->data_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, H5_ITER_ERROR, "memory allocation failed")

            /* Copy the data */
            HDmemcpy(((H5A_t *)mesg->native)->data, udata->attr->data, udata->attr->data_size);

            /* Indicate that we found the correct attribute */
            udata->found = TRUE;
        } /* end if */
    } /* end else */

    /* Set common info, if we found the correct attribute */
    if(udata->found) {
        /* Mark message as dirty */
        mesg->dirty = TRUE;

        /* Stop iterating */
        ret_value = H5_ITER_STOP;

        /* Indicate that the object header was modified */
        *oh_flags_ptr |= H5AC__DIRTIED_FLAG;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_write_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_write
 *
 * Purpose:	Write a new value to an attribute.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Monday, December  4, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_attr_write(const H5O_loc_t *loc, hid_t dxpl_id, H5A_t *attr)
{
    H5O_t *oh = NULL;                   /* Pointer to actual object header */
    unsigned oh_flags = H5AC__NO_FLAGS_SET;     /* Metadata cache flags for object header */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_write)

    /* Check arguments */
    HDassert(loc);
    HDassert(attr);

    /* Protect the object header to iterate over */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Check for attributes stored densely */
    if(H5F_addr_defined(oh->attr_fheap_addr)) {
        /* Modify the attribute data in dense storage */
        if(H5A_dense_write(loc->file, dxpl_id, oh, attr) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTUPDATE, FAIL, "error updating attribute")
    } /* end if */
    else {
        H5O_iter_wrt_t udata;           /* User data for callback */
        H5O_mesg_operator_t op;         /* Wrapper for operator */

        /* Set up user data for callback */
        udata.f = loc->file;
        udata.dxpl_id = dxpl_id;
        udata.attr = attr;
        udata.found = FALSE;

        /* Iterate over attributes, to locate correct one to update */
        op.lib_op = H5O_attr_write_cb;
        if(H5O_msg_iterate_real(loc->file, oh, H5O_MSG_ATTR, TRUE, op, &udata, dxpl_id, &oh_flags) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTUPDATE, FAIL, "error updating attribute")

        /* Check that we found the attribute */
        if(!udata.found)
            HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "can't locate open attribute?")
    } /* end else */

    /* Update the modification time, if any */
    if(H5O_touch_oh(loc->file, dxpl_id, oh, FALSE, &oh_flags) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTUPDATE, FAIL, "unable to update time on object")

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_write */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_rename_dup_cb
 *
 * Purpose:	Object header iterator callback routine to check for
 *              duplicate name during rename
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec  5 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_attr_rename_dup_cb(H5O_t *oh, H5O_mesg_t *mesg/*in,out*/,
    unsigned UNUSED sequence, unsigned UNUSED *oh_flags_ptr, void *_udata/*in,out*/)
{
    H5O_iter_ren_t *udata = (H5O_iter_ren_t *)_udata;   /* Operator user data */
    herr_t ret_value = H5_ITER_CONT;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_rename_dup_cb)

    /* check args */
    HDassert(oh);
    HDassert(mesg);
    HDassert(!udata->found);

    /* Check for shared message */
    if(mesg->flags & H5O_MSG_FLAG_SHARED) {
        H5A_t shared_attr;              /* Copy of shared attribute */

	/*
	 * If the message is shared then then the native pointer points to an
	 * H5O_MSG_SHARED message.  We use that information to look up the real
	 * message in the global heap or some other object header.
	 */
        if(NULL == H5O_shared_read(udata->f, udata->dxpl_id, mesg->native, H5O_MSG_ATTR, &shared_attr))
	    HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, H5_ITER_ERROR, "unable to read shared attribute")

        /* Check for existing attribute with new name */
        if(HDstrcmp(shared_attr.name, udata->new_name) == 0) {
            /* Indicate that we found an existing attribute with the new name*/
            udata->found = TRUE;

            /* Stop iterating */
            ret_value = H5_ITER_STOP;
        } /* end if */

        /* Release copy of shared attribute */
        H5O_attr_reset(&shared_attr);
    } /* end if */
    else {
        /* Check for existing attribute with new name */
        if(HDstrcmp(((H5A_t *)mesg->native)->name, udata->new_name) == 0) {
            /* Indicate that we found an existing attribute with the new name*/
            udata->found = TRUE;

            /* Stop iterating */
            ret_value = H5_ITER_STOP;
        } /* end if */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_rename_dup_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_rename_mod_cb
 *
 * Purpose:	Object header iterator callback routine to change name of
 *              attribute during rename
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec  5 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_attr_rename_mod_cb(H5O_t *oh, H5O_mesg_t *mesg/*in,out*/,
    unsigned UNUSED sequence, unsigned *oh_flags_ptr, void *_udata/*in,out*/)
{
    H5O_iter_ren_t *udata = (H5O_iter_ren_t *)_udata;   /* Operator user data */
    herr_t ret_value = H5_ITER_CONT;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_rename_mod_cb)

    /* check args */
    HDassert(oh);
    HDassert(mesg);
    HDassert(!udata->found);

    /* Check for shared message */
    if(mesg->flags & H5O_MSG_FLAG_SHARED) {
        H5A_t shared_attr;             /* Copy of shared attribute */

	/*
	 * If the message is shared then then the native pointer points to an
	 * H5O_MSG_SHARED message.  We use that information to look up the real
	 * message in the global heap or some other object header.
	 */
        if(NULL == H5O_shared_read(udata->f, udata->dxpl_id, mesg->native, H5O_MSG_ATTR, &shared_attr))
	    HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, H5_ITER_ERROR, "unable to read shared attribute")

        /* Check for correct attribute message to modify */
        if(HDstrcmp(shared_attr.name, udata->old_name) == 0) {
/* XXX: fix me */
HDfprintf(stderr, "%s: renaming a shared attribute not supported yet!\n", FUNC);
HGOTO_ERROR(H5E_ATTR, H5E_UNSUPPORTED, H5_ITER_ERROR, "renaming a shared attribute not supported yet")
        } /* end if */

        /* Release copy of shared attribute */
        H5O_attr_reset(&shared_attr);
    } /* end if */
    else {
        /* Find correct attribute message to rename */
        if(HDstrcmp(((H5A_t *)mesg->native)->name, udata->old_name) == 0) {
            /* Change the name for the attribute */
            H5MM_xfree(((H5A_t *)mesg->native)->name);
            ((H5A_t *)mesg->native)->name = H5MM_xstrdup(udata->new_name);

            /* Indicate that we found an existing attribute with the old name*/
            udata->found = TRUE;

            /* Mark message as dirty */
            mesg->dirty = TRUE;

            /* Stop iterating */
            ret_value = H5_ITER_STOP;

            /* Indicate that the object header was modified */
            *oh_flags_ptr |= H5AC__DIRTIED_FLAG;
        } /* end if */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_rename_mod_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_rename
 *
 * Purpose:	Rename an attribute.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, December  5, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_attr_rename(const H5O_loc_t *loc, hid_t dxpl_id, const char *old_name, const char *new_name)
{
    H5O_t *oh = NULL;                   /* Pointer to actual object header */
    unsigned oh_flags = H5AC__NO_FLAGS_SET;     /* Metadata cache flags for object header */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_rename)

    /* Check arguments */
    HDassert(loc);
    HDassert(old_name);
    HDassert(new_name);

    /* Protect the object header to iterate over */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Check for attributes stored densely */
    if(H5F_addr_defined(oh->attr_fheap_addr)) {
/* XXX: fix me */
HDfprintf(stderr, "%s: renaming attributes in dense storage not supported yet!\n", FUNC);
HGOTO_ERROR(H5E_ATTR, H5E_UNSUPPORTED, FAIL, "renaming attributes in dense storage not supported yet")
    } /* end if */
    else {
        H5O_iter_ren_t udata;           /* User data for callback */
        H5O_mesg_operator_t op;         /* Wrapper for operator */

        /* Set up user data for callback */
        udata.f = loc->file;
        udata.dxpl_id = dxpl_id;
        udata.old_name = old_name;
        udata.new_name = new_name;
        udata.found = FALSE;

        /* Iterate over attributes, to check if "new name" exists already */
        op.lib_op = H5O_attr_rename_dup_cb;
        if(H5O_msg_iterate_real(loc->file, oh, H5O_MSG_ATTR, TRUE, op, &udata, dxpl_id, &oh_flags) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTUPDATE, FAIL, "error updating attribute")

        /* If the new name was found, indicate an error */
        if(udata.found)
            HGOTO_ERROR(H5E_ATTR, H5E_EXISTS, FAIL, "attribute with new name already exists")

        /* Iterate over attributes again, to actually rename attribute with old name */
        op.lib_op = H5O_attr_rename_mod_cb;
        if(H5O_msg_iterate_real(loc->file, oh, H5O_MSG_ATTR, TRUE, op, &udata, dxpl_id, &oh_flags) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTUPDATE, FAIL, "error updating attribute")
    } /* end else */

    /* Update the modification time, if any */
    if(H5O_touch_oh(loc->file, dxpl_id, oh, FALSE, &oh_flags) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTUPDATE, FAIL, "unable to update time on object")

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_rename */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_iterate
 *
 * Purpose:	Iterate over attributes for an object.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, December  5, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_attr_iterate(hid_t loc_id, const H5O_loc_t *loc, hid_t dxpl_id,
    unsigned skip, unsigned *last_attr, H5A_operator_t op, void *op_data)
{
    H5O_t *oh = NULL;                   /* Pointer to actual object header */
    haddr_t attr_fheap_addr;            /* Address of fractal heap for dense attribute storage */
    haddr_t name_bt2_addr;              /* Address of v2 B-tree for name index on dense attribute storage */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_iterate)

    /* Check arguments */
    HDassert(loc);
    HDassert(op);

    /* Protect the object header to iterate over */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Retrieve the information about dense attribute storage */
    if(oh->version > H5O_VERSION_1 && H5F_addr_defined(oh->attr_fheap_addr)) {
        attr_fheap_addr = oh->attr_fheap_addr;
        name_bt2_addr = oh->name_bt2_addr;
    } /* end if */
    else
        attr_fheap_addr = name_bt2_addr = HADDR_UNDEF;

    /* Release the object header */
    if(H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_PROTECT, FAIL, "unable to release object header")
    oh = NULL;

    /* Check for attributes stored densely */
    if(H5F_addr_defined(attr_fheap_addr)) {
        H5A_attr_iterate_t attr_op;     /* Attribute operator */

        /* Build attribute operator info */
        attr_op.op_type = H5A_ATTR_OP_APP;
        attr_op.u.app_op = op;

        if((ret_value = H5A_dense_iterate(loc->file, dxpl_id, loc_id, attr_fheap_addr,
                name_bt2_addr, skip, last_attr, &attr_op, op_data)) < 0)
            HERROR(H5E_ATTR, H5E_BADITER, "error iterating over attributes");
    } /* end if */
    else {
        unsigned idx;           /* Current attribute to operate on */

        /* Check for skipping over too many attributes */
        if((int)skip < H5O_msg_count(loc, H5O_ATTR_ID, dxpl_id)) {
            H5A_t found_attr;           /* Copy of attribute for callback */

            /* Read each attribute and call application's callback */
            /* (this could be made more efficient by iterating over the
             *  attribute header messages with H5O_msg_iterate, but then
             *  the object header would be locked during the callback into
             *  the application code, causing problems if they attempt to
             *  do anything with the object the attribute is on - QAK)
             */
            idx = skip;
            while(H5O_msg_read(loc, H5O_ATTR_ID, (int)idx, &found_attr, dxpl_id) != NULL) {
                /* Call application's callback */
                idx++;
                if((ret_value = (op)(loc_id, found_attr.name, op_data)) != 0) {
                    H5A_free(&found_attr);
                    break;
                } /* end if */
                H5A_free(&found_attr);
            } /* end while */

            /* Clear error stack from running off end of attributes */
            if(ret_value == 0)
                H5E_clear_stack(NULL);
        } /* end if */
        else
            if(skip > 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index specified")

        /* Update last attribute looked at */
        if(last_attr)
            *last_attr = idx;
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_iterate */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_remove_cb
 *
 * Purpose:	Object header iterator callback routine to remove an
 *              attribute stored compactly.
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
H5O_attr_remove_cb(H5O_t *oh, H5O_mesg_t *mesg/*in,out*/,
    unsigned UNUSED sequence, unsigned *oh_flags_ptr, void *_udata/*in,out*/)
{
    H5O_iter_rm_t *udata = (H5O_iter_rm_t *)_udata;   /* Operator user data */
    herr_t ret_value = H5_ITER_CONT;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_remove_cb)

    /* check args */
    HDassert(oh);
    HDassert(mesg);
    HDassert(!udata->found);

    /* Check for shared message */
    if(mesg->flags & H5O_MSG_FLAG_SHARED) {
        H5A_t shared_attr;             /* Copy of shared attribute */

	/*
	 * If the message is shared then then the native pointer points to an
	 * H5O_MSG_SHARED message.  We use that information to look up the real
	 * message in the global heap or some other object header.
	 */
        if(NULL == H5O_shared_read(udata->f, udata->dxpl_id, mesg->native, H5O_MSG_ATTR, &shared_attr))
	    HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, H5_ITER_ERROR, "unable to read shared attribute")

HDfprintf(stderr, "%s: removing a shared attribute not supported yet!\n", FUNC);
HGOTO_ERROR(H5E_ATTR, H5E_UNSUPPORTED, FAIL, "deleting a shared attribute not supported yet")
        /* Check for correct attribute message to modify */
        if(HDstrcmp(shared_attr.name, udata->name) == 0)
            /* Indicate that this message is the attribute to be deleted */
            udata->found = TRUE;

        /* Release copy of shared attribute */
        H5O_attr_reset(&shared_attr);
    } /* end if */
    else {
        /* Check for correct attribute message to modify */
        if(HDstrcmp(((H5A_t *)mesg->native)->name, udata->name) == 0)
            /* Indicate that this message is the attribute to be deleted */
            udata->found = TRUE;
    } /* end else */

    /* Check for finding correct message to delete */
    if(udata->found) {
        /* If the later version of the object header format, decrement attribute */
        /* (must be decremented before call to H5O_release_mesg(), in order for
         *      sanity checks to pass - QAK)
         */
        if(oh->version > H5O_VERSION_1)
            oh->nattrs--;

        /* Convert message into a null message (i.e. delete it) */
        if(H5O_release_mesg(udata->f, udata->dxpl_id, oh, mesg, TRUE, TRUE) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, H5_ITER_ERROR, "unable to convert into null message")

        /* Stop iterating */
        ret_value = H5_ITER_STOP;

        /* Indicate that the object header was modified */
        *oh_flags_ptr |= H5AC__DIRTIED_FLAG;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_remove_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_remove
 *
 * Purpose:	Delete an attributes on an object.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Monday, December 11, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_attr_remove(const H5O_loc_t *loc, const char *name, hid_t dxpl_id)
{
    H5O_t *oh = NULL;                   /* Pointer to actual object header */
    unsigned oh_flags = H5AC__NO_FLAGS_SET;     /* Metadata cache flags for object header */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_remove)

    /* Check arguments */
    HDassert(loc);
    HDassert(name);

    /* Protect the object header to iterate over */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Check for attributes stored densely */
    if(H5F_addr_defined(oh->attr_fheap_addr)) {
        /* Delete attribute from dense storage */
        if(H5A_dense_remove(loc->file, dxpl_id, oh, name) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute in dense storage")

        /* Decrement # of attributes on object */
        oh->nattrs--;
    } /* end if */
    else {
        H5O_iter_rm_t udata;            /* User data for callback */
        H5O_mesg_operator_t op;         /* Wrapper for operator */

        /* Set up user data for callback */
        udata.f = loc->file;
        udata.dxpl_id = dxpl_id;
        udata.name = name;
        udata.found = FALSE;

        /* Iterate over attributes, to locate correct one to delete */
        op.lib_op = H5O_attr_remove_cb;
        if(H5O_msg_iterate_real(loc->file, oh, H5O_MSG_ATTR, TRUE, op, &udata, dxpl_id, &oh_flags) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "error deleting attribute")

        /* Check that we found the attribute */
        if(!udata.found)
            HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "can't locate attribute")
    } /* end else */

    /* Check for shifting from dense storage back to compact storage */
    if(H5F_addr_defined(oh->attr_fheap_addr) && oh->nattrs < oh->min_dense) {
        /* Check if there's no more attributes */
        if(oh->nattrs == 0) {
/* XXX: Test this */
            /* Delete the dense storage */
            if(H5A_dense_delete(loc->file, dxpl_id, oh) < 0)
                HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete dense attribute storage")
        } /* end if */
        else {
            H5A_attr_table_t atable = {0, NULL, NULL};        /* Table of attributes */
            hbool_t can_convert = TRUE;     /* Whether converting to attribute messages is possible */
            size_t u;                       /* Local index */

            /* Build the table of attributes for this object */
/* XXX: Test this with shared attributes */
            if(H5A_dense_build_table(loc->file, dxpl_id, oh, H5_INDEX_NAME, H5_ITER_NATIVE, &atable) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "error building attribute table")

            /* Inspect attributes in table for ones that can't be converted back
             * into attribute message form (currently only attributes which
             * can't fit into an object header message)
             */
            for(u = 0; u < oh->nattrs; u++)
                if(H5O_msg_mesg_size(loc->file, H5O_ATTR_ID, &(atable.attrs[u]), (size_t)0) >= H5O_MESG_MAX_SIZE) {
                    can_convert = FALSE;
                    break;
                } /* end if */

            /* If ok, insert attributes as object header messages */
            if(can_convert) {
                /* Insert attribute messages into object header */
                for(u = 0; u < oh->nattrs; u++)
                    if(H5O_msg_append_real(loc->file, dxpl_id, oh, H5O_MSG_ATTR, (unsigned)atable.flags[u], H5O_UPDATE_TIME, &(atable.attrs[u]), &oh_flags) < 0)
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "can't create message")

                /* Remove the dense storage */
                if(H5A_dense_delete(loc->file, dxpl_id, oh) < 0)
                    HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete dense attribute storage")
            } /* end if */

            /* Free attribute table information */
            if(H5A_attr_release_table(&atable) < 0)
                HGOTO_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "unable to release attribute table")
        } /* end else */
    } /* end if */

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_remove */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_count
 *
 * Purpose:	Determine the # of attributes on an object
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Monday, December 11, 2006
 *
 *-------------------------------------------------------------------------
 */
int
H5O_attr_count(const H5O_loc_t *loc, hid_t dxpl_id)
{
    H5O_t *oh = NULL;                   /* Pointer to actual object header */
    int ret_value;                      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_count)

    /* Check arguments */
    HDassert(loc);

    /* Protect the object header to iterate over */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Check for attributes stored densely */
    if(oh->version > H5O_VERSION_1)
/* XXX: test this */
        ret_value = (int)oh->nattrs;
    else {
        unsigned u;             /* Local index variable */

        /* Loop over all messages, counting the attributes */
        for(u = ret_value = 0; u < oh->nmesgs; u++)
            if(oh->mesg[u].type == H5O_MSG_ATTR)
                ret_value++;
    } /* end else */

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_count */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_exists_cb
 *
 * Purpose:	Object header iterator callback routine to check for an
 *              attribute stored compactly, by name.
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
H5O_attr_exists_cb(H5O_t *oh, H5O_mesg_t *mesg/*in,out*/,
    unsigned UNUSED sequence, unsigned UNUSED *oh_flags_ptr, void *_udata/*in,out*/)
{
    H5O_iter_rm_t *udata = (H5O_iter_rm_t *)_udata;   /* Operator user data */
    herr_t ret_value = H5_ITER_CONT;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_exists_cb)

    /* check args */
    HDassert(oh);
    HDassert(mesg);
    HDassert(!udata->found);

    /* Check for shared message */
    if(mesg->flags & H5O_MSG_FLAG_SHARED) {
        H5A_t shared_attr;             /* Copy of shared attribute */

	/*
	 * If the message is shared then then the native pointer points to an
	 * H5O_MSG_SHARED message.  We use that information to look up the real
	 * message in the global heap or some other object header.
	 */
        if(NULL == H5O_shared_read(udata->f, udata->dxpl_id, mesg->native, H5O_MSG_ATTR, &shared_attr))
	    HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, H5_ITER_ERROR, "unable to read shared attribute")

        /* Check for correct attribute message */
        if(HDstrcmp(shared_attr.name, udata->name) == 0)
            /* Indicate that this message is the attribute sought */
            udata->found = TRUE;

        /* Release copy of shared attribute */
        H5O_attr_reset(&shared_attr);
    } /* end if */
    else {
        /* Check for correct attribute message */
        if(HDstrcmp(((H5A_t *)mesg->native)->name, udata->name) == 0)
            /* Indicate that this message is the attribute sought */
            udata->found = TRUE;
    } /* end else */

    /* Check for finding correct message to delete */
    if(udata->found) {
        /* Stop iterating */
        ret_value = H5_ITER_STOP;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_exists_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_exists
 *
 * Purpose:	Determine if an attribute with a particular name exists on an object
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Monday, December 11, 2006
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5O_attr_exists(const H5O_loc_t *loc, const char *name, hid_t dxpl_id)
{
    H5O_t *oh = NULL;           /* Pointer to actual object header */
    unsigned oh_flags = H5AC__NO_FLAGS_SET;     /* Metadata cache flags for object header */
    htri_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_exists)

    /* Check arguments */
    HDassert(loc);
    HDassert(name);

    /* Protect the object header to iterate over */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_ATTR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Check for attributes stored densely */
    if(H5F_addr_defined(oh->attr_fheap_addr)) {
        /* Check if attribute exists in dense storage */
        if((ret_value = H5A_dense_exists(loc->file, dxpl_id, oh, name)) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_BADITER, FAIL, "error checking for existence of attribute")
    } /* end if */
    else {
        H5O_iter_rm_t udata;            /* User data for callback */
        H5O_mesg_operator_t op;         /* Wrapper for operator */

        /* Set up user data for callback */
        udata.f = loc->file;
        udata.dxpl_id = dxpl_id;
        udata.name = name;
        udata.found = FALSE;

        /* Iterate over existing attributes, checking for attribute with same name */
        op.lib_op = H5O_attr_exists_cb;
        if(H5O_msg_iterate_real(loc->file, oh, H5O_MSG_ATTR, TRUE, op, &udata, dxpl_id, &oh_flags) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_BADITER, FAIL, "error checking for existence of attribute")

        /* Check that we found the attribute */
        ret_value = udata.found;
    } /* end else */

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_exists */

