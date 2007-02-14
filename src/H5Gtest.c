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

/* Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Monday, December 19, 2005
 *
 * Purpose:	Group testing functions.
 */

#define H5G_PACKAGE		/*suppress error about including H5Gpkg	  */
#define H5G_TESTING		/*suppress warning about H5G testing funcs*/


#include "H5private.h"		/* Generic Functions			*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5HLprivate.h"	/* Local Heaps				*/
#include "H5Iprivate.h"		/* IDs			  		*/


/*--------------------------------------------------------------------------
 NAME
    H5G_user_path_test
 PURPOSE
    Retrieve the user path for an ID
 USAGE
    herr_t H5G_user_path_test(obj_id, user_path, user_path_len)
        hid_t obj_id;           IN: ID to check
        char *user_path;        OUT: Pointer to buffer for User path
        size_t *user_path_len;  OUT: Size of user path
        unsigned *obj_hidden;   OUT: Whether object is hidden
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Retrieves the user path for an ID.  A zero for the length is returned in
    the case of no user path.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    DO NOT USE THIS FUNCTION FOR ANYTHING EXCEPT TESTING
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5G_user_path_test(hid_t obj_id, char *user_path, size_t *user_path_len, unsigned *obj_hidden)
{
    void *obj_ptr;              /* Pointer to object for ID */
    H5G_entry_t *obj_ent;       /* Pointer to symbol table entry for obj */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(H5G_user_path_test, FAIL)

    /* Sanity check */
    HDassert(user_path_len);
    HDassert(obj_hidden);

    /* Get pointer to object for ID */
    if(NULL == (obj_ptr = H5I_object(obj_id)))
         HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get object for ID")

    /* Get the symbol table entry */
    switch(H5I_get_type(obj_id)) {
        case H5I_GROUP:
            obj_ent = H5G_entof((H5G_t *)obj_ptr);
            break;

        case H5I_DATASET:
            obj_ent = H5D_entof((H5D_t *)obj_ptr);
            break;

        case H5I_DATATYPE:
            /* Avoid non-named datatypes */
            if(!H5T_is_named((H5T_t *)obj_ptr))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a named datatype")

            obj_ent = H5T_entof((H5T_t *)obj_ptr);
            break;

        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "unknown data object type")
    } /* end switch */
    HDassert(obj_ent);

    /* Retrieve a copy of the user path and put it into the buffer */
    if(obj_ent->user_path_r) {
        size_t len = H5RS_len(obj_ent->user_path_r);

        /* Set the user path, if given */
        if(user_path)
            HDstrcpy(user_path, H5RS_get_str(obj_ent->user_path_r));

        /* Set the length of the path */
        *user_path_len = len;

        /* Set the user path hidden flag */
        *obj_hidden = obj_ent->obj_hidden;
    } /* end if */
    else {
        *user_path_len = 0;
        *obj_hidden = 0;
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5G_user_path_test() */

