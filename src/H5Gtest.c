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

/* Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Monday, October 17, 2005
 *
 * Purpose:	Group testing functions.
 */

#define H5G_PACKAGE		/*suppress error about including H5Gpkg	  */
#define H5G_TESTING		/*suppress warning about H5G testing funcs*/


#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5Iprivate.h"		/* IDs			  		*/


/*--------------------------------------------------------------------------
 NAME
    H5G_is_empty_test
 PURPOSE
    Determine whether a group contains no objects
 USAGE
    htri_t H5G_is_empty_test(gid)
        hid_t gid;              IN: group to check
 RETURNS
    Non-negative TRUE/FALSE on success, negative on failure
 DESCRIPTION
    Checks to see if the group has no link messages and no symbol table message
    dimensionality and shape.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    DO NOT USE THIS FUNCTION FOR ANYTHING EXCEPT TESTING
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5G_is_empty_test(hid_t gid)
{
    H5G_t *grp = NULL;          /* Pointer to group */
    htri_t msg_exists = 0;      /* Indicate that a header message is present */
    htri_t ret_value = TRUE;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_is_empty_test, FAIL)

    /* Get group structure */
    if(NULL == (grp = H5I_object_verify(gid, H5I_GROUP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group")

    /* Check if the group has any link messages */
    if((msg_exists = H5O_exists(&(grp->oloc), H5O_LINK_ID, 0, H5AC_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to read object header")
    if(msg_exists > 0)
        HGOTO_DONE(FALSE)

    /* Check if the group has a symbol table message */
    if((msg_exists = H5O_exists(&(grp->oloc), H5O_STAB_ID, 0, H5AC_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to read object header")
    if(msg_exists > 0)
        HGOTO_DONE(FALSE)

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5G_is_empty_test() */


/*--------------------------------------------------------------------------
 NAME
    H5G_has_links_test
 PURPOSE
    Determine whether a group contains link messages
 USAGE
    htri_t H5G_has_links_test(gid)
        hid_t gid;              IN: group to check
        unsigned *nmsgs;        OUT: # of link messages in header
 RETURNS
    Non-negative TRUE/FALSE on success, negative on failure
 DESCRIPTION
    Checks to see if the group has link messages and how many.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    DO NOT USE THIS FUNCTION FOR ANYTHING EXCEPT TESTING
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5G_has_links_test(hid_t gid, unsigned *nmsgs)
{
    H5G_t *grp = NULL;          /* Pointer to group */
    htri_t msg_exists = 0;      /* Indicate that a header message is present */
    htri_t ret_value = TRUE;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_has_links_test, FAIL)

    /* Get group structure */
    if(NULL == (grp = H5I_object_verify(gid, H5I_GROUP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group")

    /* Check if the group has any link messages */
    if((msg_exists = H5O_exists(&(grp->oloc), H5O_LINK_ID, 0, H5AC_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to read object header")
    if(msg_exists == 0)
        HGOTO_DONE(FALSE)

    /* Check if the group has a symbol table message */
    if((msg_exists = H5O_exists(&(grp->oloc), H5O_STAB_ID, 0, H5AC_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to read object header")
    if(msg_exists > 0)
	HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "both symbol table and link messages found")

    /* Check if we should retrieve the number of link messages */
    if(nmsgs) {
        int msg_count;     /* Number of messages of a type */

        /* Check how many link messages there are */
        if((msg_count = H5O_count(&(grp->oloc), H5O_LINK_ID, H5AC_dxpl_id)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTCOUNT, FAIL, "unable to count link messages")
        *nmsgs = (unsigned)msg_count;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5G_has_links_test() */


/*--------------------------------------------------------------------------
 NAME
    H5G_has_stab_test
 PURPOSE
    Determine whether a group contains a symbol table message
 USAGE
    htri_t H5G_has_stab_test(gid)
        hid_t gid;              IN: group to check
 RETURNS
    Non-negative TRUE/FALSE on success, negative on failure
 DESCRIPTION
    Checks to see if the group has a symbol table message.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    DO NOT USE THIS FUNCTION FOR ANYTHING EXCEPT TESTING
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5G_has_stab_test(hid_t gid)
{
    H5G_t *grp = NULL;          /* Pointer to group */
    htri_t msg_exists = 0;      /* Indicate that a header message is present */
    htri_t ret_value = TRUE;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_has_stab_test, FAIL)

    /* Get group structure */
    if(NULL == (grp = H5I_object_verify(gid, H5I_GROUP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group")

    /* Check if the group has a symbol table message */
    if((msg_exists = H5O_exists(&(grp->oloc), H5O_STAB_ID, 0, H5AC_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to read object header")
    if(msg_exists == 0)
        HGOTO_DONE(FALSE)

    /* Check if the group has any link messages */
    if((msg_exists = H5O_exists(&(grp->oloc), H5O_LINK_ID, 0, H5AC_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to read object header")
    if(msg_exists > 0)
	HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "both symbol table and link messages found")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5G_has_stab_test() */

