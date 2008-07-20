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

/* Programmer:  Quincey Koziol <koziol@hdfgroup.org>
 *              Monday, December 4, 2006
 *
 * Purpose:	Object header testing functions.
 */

/****************/
/* Module Setup */
/****************/

#define H5A_PACKAGE		/*suppress error about including H5Apkg	  */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */
#define H5O_TESTING		/*suppress warning about H5O testing funcs*/


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Apkg.h"		/* Attributes	  			*/
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Opkg.h"             /* Object headers			*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


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


/*--------------------------------------------------------------------------
 NAME
    H5O_is_attr_dense_test
 PURPOSE
    Determine whether attributes for an object are stored "densely"
 USAGE
    htri_t H5O_is_attr_dense_test(oid)
        hid_t oid;              IN: object to check
 RETURNS
    Non-negative TRUE/FALSE on success, negative on failure
 DESCRIPTION
    Checks to see if the object is storing attributes in the "dense" or
    "compact" form.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    DO NOT USE THIS FUNCTION FOR ANYTHING EXCEPT TESTING
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5O_is_attr_dense_test(hid_t oid)
{
    H5O_t *oh = NULL;           /* Object header */
    H5O_ainfo_t ainfo;          /* Attribute information for object */
    H5O_loc_t *loc;            /* Pointer to object's location */
    htri_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI(H5O_is_attr_dense_test, FAIL)

    /* Get object location for object */
    if(NULL == (loc = H5O_get_loc(oid)))
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")

    /* Get the object header */
    if(NULL == (oh = H5O_protect(loc, H5AC_ind_dxpl_id, H5AC2_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, FAIL, "unable to load object header")

    /* Check for attribute info stored */
    ainfo.fheap_addr = HADDR_UNDEF;
    if(oh->version > H5O_VERSION_1 && NULL == H5A_get_ainfo(loc->file, H5AC_ind_dxpl_id, oh, &ainfo))
        /* Clear error stack from not finding attribute info */
        H5E_clear_stack(NULL);

    /* Check if dense storage is being used */
    if(H5F_addr_defined(ainfo.fheap_addr)) {
        /* Check for any messages in object header */
        HDassert(H5O_msg_count_real(oh, H5O_MSG_ATTR) == 0);

        ret_value = TRUE;
    } /* end if */
    else
        ret_value = FALSE;

done:
    if(oh && H5O_unprotect(loc, H5AC_ind_dxpl_id, oh, H5AC2__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5O_is_attr_dense_test() */


/*--------------------------------------------------------------------------
 NAME
    H5O_is_attr_empty_test
 PURPOSE
    Determine whether there are any attributes for an object
 USAGE
    htri_t H5O_is_attr_empty_test(oid)
        hid_t oid;              IN: object to check
 RETURNS
    Non-negative TRUE/FALSE on success, negative on failure
 DESCRIPTION
    Checks to see if the object is storing any attributes.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    DO NOT USE THIS FUNCTION FOR ANYTHING EXCEPT TESTING
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5O_is_attr_empty_test(hid_t oid)
{
    H5O_t *oh = NULL;           /* Object header */
    H5O_ainfo_t ainfo;          /* Attribute information for object */
    H5O_ainfo_t *ainfo_ptr = NULL;      /* Pointer to attribute information for object */
    H5O_loc_t *loc;            /* Pointer to object's location */
    hsize_t nattrs;             /* Number of attributes */
    htri_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI(H5O_is_attr_empty_test, FAIL)

    /* Get object location for object */
    if(NULL == (loc = H5O_get_loc(oid)))
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")

    /* Get the object header */
    if(NULL == (oh = H5O_protect(loc, H5AC_ind_dxpl_id, H5AC2_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, FAIL, "unable to load object header")

    /* Check for attribute info stored */
    ainfo.fheap_addr = HADDR_UNDEF;
    if(oh->version > H5O_VERSION_1 && NULL == (ainfo_ptr = H5A_get_ainfo(loc->file, H5AC_ind_dxpl_id, oh, &ainfo)))
        /* Clear error stack from not finding attribute info */
        H5E_clear_stack(NULL);

    /* Retrieve the number of attribute messages in header */
    nattrs = H5O_msg_count_real(oh, H5O_MSG_ATTR);

    /* Check for later version of object header format & attribute info available */
    if(oh->version > H5O_VERSION_1) {
        if(ainfo_ptr) {
            /* Check for using dense storage */
            if(H5F_addr_defined(ainfo.fheap_addr)) {
                /* Check for any messages in object header */
                HDassert(nattrs == 0);

                /* Retrieve # of records in name index */
                if(H5B2_get_nrec(loc->file, H5AC_ind_dxpl_id, H5A_BT2_NAME, ainfo.name_bt2_addr, &nattrs) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTCOUNT, FAIL, "unable to retrieve # of records from name index")
            } /* end if */

            /* Verify that attribute count in object header is correct */
            HDassert(nattrs == ainfo.nattrs);
        } /* end if */
        else
            HDassert(nattrs == 0);
    } /* end if */

    /* Set the return value */
    ret_value = (nattrs == 0) ? TRUE : FALSE;

done:
    if(oh && H5O_unprotect(loc, H5AC_ind_dxpl_id, oh, H5AC2__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5O_is_attr_empty_test() */


/*--------------------------------------------------------------------------
 NAME
    H5O_num_attrs_test
 PURPOSE
    Determine whether there are any attributes for an object
 USAGE
    herr_t H5O_num_attrs_test(oid, nattrs)
        hid_t oid;              IN: object to check
        hsize_t *nattrs;        OUT: Number of attributes on object
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Checks the # of attributes on an object
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    DO NOT USE THIS FUNCTION FOR ANYTHING EXCEPT TESTING
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5O_num_attrs_test(hid_t oid, hsize_t *nattrs)
{
    H5O_t *oh = NULL;           /* Object header */
    H5O_ainfo_t ainfo;          /* Attribute information for object */
    H5O_loc_t *loc;            /* Pointer to object's location */
    hsize_t obj_nattrs;         /* Number of attributes */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(H5O_num_attrs_test, FAIL)

    /* Get object location for object */
    if(NULL == (loc = H5O_get_loc(oid)))
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")

    /* Get the object header */
    if(NULL == (oh = H5O_protect(loc, H5AC_ind_dxpl_id, H5AC2_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, FAIL, "unable to load object header")

    /* Check for attribute info stored */
    ainfo.fheap_addr = HADDR_UNDEF;
    if(oh->version > H5O_VERSION_1 && NULL == H5A_get_ainfo(loc->file, H5AC_ind_dxpl_id, oh, &ainfo))
        /* Clear error stack from not finding attribute info */
        H5E_clear_stack(NULL);

    /* Retrieve the number of attribute messages in header */
    obj_nattrs = H5O_msg_count_real(oh, H5O_MSG_ATTR);

    /* Check for later version of object header format */
    if(oh->version > H5O_VERSION_1) {
        /* Check for using dense storage */
        if(H5F_addr_defined(ainfo.fheap_addr)) {
            /* Check for any messages in object header */
            HDassert(obj_nattrs == 0);

            /* Retrieve # of records in name index */
            if(H5B2_get_nrec(loc->file, H5AC_ind_dxpl_id, H5A_BT2_NAME, ainfo.name_bt2_addr, &obj_nattrs) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTCOUNT, FAIL, "unable to retrieve # of records from name index")
        } /* end if */

        /* Verify that attribute count in object header is correct */
        HDassert(obj_nattrs == ainfo.nattrs);
    } /* end if */

    /* Set the number of attributes */
    *nattrs = obj_nattrs;

done:
    if(oh && H5O_unprotect(loc, H5AC_ind_dxpl_id, oh, H5AC2__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5O_num_attrs_test() */


/*--------------------------------------------------------------------------
 NAME
    H5O_attr_dense_info_test
 PURPOSE
    Retrieve information about the state of the "dense" storage for attributes
 USAGE
    herr_t H5O_attr_dense_info_test(oid, name_count, corder_count)
        hid_t oid;              IN: Object to check
        hsize_t *name_count;    OUT: Number of attributes in name index
        hsize_t *corder_count;  OUT: Number of attributes in creation order index
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Currently, just retrieves the number of attributes in each index and returns
    them.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    DO NOT USE THIS FUNCTION FOR ANYTHING EXCEPT TESTING
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5O_attr_dense_info_test(hid_t oid, hsize_t *name_count, hsize_t *corder_count)
{
    H5O_t *oh = NULL;           /* Object header */
    H5O_ainfo_t ainfo;          /* Attribute information for object */
    H5O_loc_t *loc;            /* Pointer to object's location */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(H5O_attr_dense_info_test, FAIL)

    /* Get object location for object */
    if(NULL == (loc = H5O_get_loc(oid)))
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")

    /* Get the object header */
    if(NULL == (oh = H5O_protect(loc, H5AC_ind_dxpl_id, H5AC2_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, FAIL, "unable to load object header")

    /* Check for attribute info stored */
    ainfo.fheap_addr = HADDR_UNDEF;
    if(oh->version > H5O_VERSION_1 && NULL == H5A_get_ainfo(loc->file, H5AC_ind_dxpl_id, oh, &ainfo))
        /* Clear error stack from not finding attribute info */
        H5E_clear_stack(NULL);

    /* Check for 'dense' attribute storage file addresses being defined */
    if(!H5F_addr_defined(ainfo.fheap_addr))
        HGOTO_DONE(FAIL)
    if(!H5F_addr_defined(ainfo.name_bt2_addr))
        HGOTO_DONE(FAIL)

    /* Retrieve # of records in name index */
    if(H5B2_get_nrec(loc->file, H5AC_ind_dxpl_id, H5A_BT2_NAME, ainfo.name_bt2_addr, name_count) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOUNT, FAIL, "unable to retrieve # of records from name index")

    /* Check if there is a creation order index */
    if(H5F_addr_defined(ainfo.corder_bt2_addr)) {
        /* Retrieve # of records in creation order index */
        if(H5B2_get_nrec(loc->file, H5AC_ind_dxpl_id, H5A_BT2_CORDER, ainfo.corder_bt2_addr, corder_count) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTCOUNT, FAIL, "unable to retrieve # of records from creation order index")
    } /* end if */
    else
        *corder_count = 0;

done:
    if(oh && H5O_unprotect(loc, H5AC_ind_dxpl_id, oh, H5AC2__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5O_attr_dense_info_test() */


/*--------------------------------------------------------------------------
 NAME
    H5O_check_msg_marked_test
 PURPOSE
    Check if an unknown message with the "mark if unknown" flag actually gets
    marked.
 USAGE
    herr_t H5O_check_msg_marked_test(oid, flag_val)
        hid_t oid;              IN: Object to check
        hbool_t flag_val;       IN: Desired flag value
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Locates the "unknown" message and checks that the "was unknown" flag is set
    correctly.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    DO NOT USE THIS FUNCTION FOR ANYTHING EXCEPT TESTING
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5O_check_msg_marked_test(hid_t oid, hbool_t flag_val)
{
    H5O_t *oh = NULL;           /* Object header */
    H5O_loc_t *loc;            /* Pointer to object's location */
    H5O_mesg_t *idx_msg;        /* Pointer to message */
    unsigned idx;               /* Index of message */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(H5O_check_msg_marked_test, FAIL)

    /* Get object location for object */
    if(NULL == (loc = H5O_get_loc(oid)))
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")

    /* Get the object header */
    if(NULL == (oh = H5O_protect(loc, H5AC_ind_dxpl_id, H5AC2_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, FAIL, "unable to load object header")

    /* Locate "unknown" message  */
    for(idx = 0, idx_msg = &oh->mesg[0]; idx < oh->nmesgs; idx++, idx_msg++)
	if(idx_msg->type->id == H5O_UNKNOWN_ID) {
            /* Check for "unknown" message having the correct flags */
            if(((idx_msg->flags & H5O_MSG_FLAG_WAS_UNKNOWN) > 0) != flag_val)
                HGOTO_ERROR(H5E_OHDR, H5E_BADVALUE, FAIL, "'unknown' message has incorrect 'was unknown' flag value")

            /* Break out of loop, to indicate that the "unknown" message was found */
            break;
        } /* end if */

    /* Check for not finding an "unknown" message */
    if(idx == oh->nmesgs)
        HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL, "'unknown' message type not found")

done:
    if(oh && H5O_unprotect(loc, H5AC_ind_dxpl_id, oh, H5AC2__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5O_check_msg_marked_test() */

