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
 * Created:	H5Gdeprec.c
 *		June 21 2006
 *		James Laird <jlaird@ncsa.uiuc.edu>
 *
 * Purpose:	Deprecated functions from the H5G interface.  These
 *              functions are here for compatibility purposes and may be
 *              removed in the future.  Applications should switch to the
 *              newer APIs.
 *
 *-------------------------------------------------------------------------
 */

/* Packages needed by this file... */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Gpublic.h"          /* Public Group APIs                    */
#include "H5Lpublic.h"          /* Public Link APIs                     */
#include "H5Ppublic.h"          /* Property lists                       */


/*-------------------------------------------------------------------------
 * Function:	H5Glink
 *
 * Purpose:	Creates a link between two existing objects.  The new
 *              APIs to do this are H5Lcreate_hard and H5Lcreate_soft.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Glink(hid_t cur_loc_id, H5L_link_t type, const char *cur_name, const char *new_name)
{
    herr_t ret_value;

    FUNC_ENTER_API(H5Glink, FAIL)
    H5TRACE4("e","iLlss",cur_loc_id,type,cur_name,new_name);

    if(type == H5L_LINK_HARD)
    {
      if((ret_value = H5Lcreate_hard(cur_loc_id, cur_name, H5L_SAME_LOC, new_name, H5P_DEFAULT)) < 0)
      HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "Couldn't create link")
    }
    else if(type == H5L_LINK_SOFT)
    {
      if((ret_value = H5Lcreate_soft(cur_name, cur_loc_id, new_name, H5P_DEFAULT)) < 0)
      HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "Couldn't create link")
    }
    else
      HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Not a valid link type")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Glink2
 *
 * Purpose:	Creates a link between two existing objects.  The new
 *              API to do this is H5Llink.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Glink2(hid_t cur_loc_id, const char *cur_name, H5L_link_t type,
    hid_t new_loc_id, const char *new_name)
{
    herr_t ret_value;

    FUNC_ENTER_API(H5Glink2, FAIL)
    H5TRACE5("e","isLlis",cur_loc_id,cur_name,type,new_loc_id,new_name);

    if(type == H5L_LINK_HARD) {
        if((ret_value = H5Lcreate_hard(cur_loc_id, cur_name, new_loc_id, new_name, H5P_DEFAULT)) < 0)
            HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "Couldn't create link")
    }
    else if(type == H5L_LINK_SOFT) {
        /* Soft links only need one location, the new_loc_id, but it's possible that
         * new_loc_id is H5L_SAME_LOC */
        if(new_loc_id == H5L_SAME_LOC)
            new_loc_id = cur_loc_id;

        if((ret_value = H5Lcreate_soft(cur_name, new_loc_id, new_name, H5P_DEFAULT)) < 0)
            HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "Couldn't create link")
    }
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Not a valid link type")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Gmove
 *
 * Purpose:	Moves and renames a link.  The new API to do this is H5Lmove.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gmove(hid_t src_loc_id, const char *src_name, const char *dst_name)
{
    herr_t ret_value;

    FUNC_ENTER_API(H5Gmove, FAIL)
    H5TRACE3("e","iss",src_loc_id,src_name,dst_name);

    if((ret_value=H5Lmove(src_loc_id, src_name, H5L_SAME_LOC, dst_name, H5P_DEFAULT)) < 0)
      HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "Couldn't move link")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Gmove2
 *
 * Purpose:	Moves and renames a link.  The new API to do this is H5Lmove.
 *
 *-------------------------------------------------------------------------
 */
herr_t H5Gmove2(hid_t src_loc_id, const char *src_name, 
                      hid_t dst_loc_id, const char *dst_name)
{
    herr_t ret_value;

    FUNC_ENTER_API(H5Gmove2, FAIL)

    if((ret_value=H5Lmove(src_loc_id, src_name, dst_loc_id, dst_name, H5P_DEFAULT)) < 0)
      HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "Couldn't move link")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Gunlink
 *
 * Purpose:	Removes a link.  The new API is H5Lunlink.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gunlink(hid_t loc_id, const char *name)
{
    herr_t ret_value;

    FUNC_ENTER_API(H5Gunlink, FAIL)
    H5TRACE2("e","is",loc_id,name);

    if((ret_value=H5Lunlink(loc_id, name)) < 0)
      HGOTO_ERROR(H5E_LINK, H5E_CANTDELETE, FAIL, "Couldn't delete link")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Gget_linkval
 *
 * Purpose:	Retrieve's a soft link's data.  The new API is
 *              H5Lget_linkval.
 *
 *-------------------------------------------------------------------------
 */
herr_t H5Gget_linkval(hid_t loc_id, const char *name,
                      size_t size, char *buf/*out*/)
{
    herr_t ret_value;

    FUNC_ENTER_API(H5Gget_linkval, FAIL)

    if((ret_value=H5Lget_linkval(loc_id, name, size, buf)) < 0)
      HGOTO_ERROR(H5E_LINK, H5E_NOTFOUND, FAIL, "Couldn't get link info")

done:
    FUNC_LEAVE_API(ret_value)
}

