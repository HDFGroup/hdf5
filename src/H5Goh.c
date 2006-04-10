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

/****************/
/* Module Setup */
/****************/

#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Opkg.h"             /* Object headers			*/

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Local Prototypes */
/********************/
static htri_t H5O_group_isa(H5O_t *loc);

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/* This message derives from H5O object class */
const H5O_obj_class_t H5O_OBJ_GROUP[1] = {{
    H5G_GROUP,			/* object type			*/
    "group",			/* object name, for debugging	*/
    NULL,			/* get 'copy file' user data	*/
    NULL,			/* free 'copy file' user data	*/
    H5O_group_isa 		/* "isa" message		*/
}};


/*-------------------------------------------------------------------------
 * Function:	H5O_group_isa
 *
 * Purpose:	Determines if an object has the requisite messages for being
 *		a group.
 *
 * Return:	Success:	TRUE if the required group messages are
 *				present; FALSE otherwise.
 *
 *		Failure:	FAIL if the existence of certain messages
 *				cannot be determined.
 *
 * Programmer:	Robb Matzke
 *              Monday, November  2, 1998
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5O_group_isa(struct H5O_t *oh)
{
    htri_t	stab_exists;            /* Whether the 'stab' message is in the object header */
#ifdef H5_GROUP_REVISION
    htri_t	linfo_exists;           /* Whether the 'linfo' message is in the object header */
#endif /* H5_GROUP_REVISION */
    htri_t	ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_group_isa)

    HDassert(oh);

    /* Check for any of the messages that indicate a group */
    if((stab_exists = H5O_exists_oh(oh, H5O_STAB_ID, 0)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to read object header")
#ifdef H5_GROUP_REVISION
    if((linfo_exists = H5O_exists_oh(oh, H5O_LINFO_ID, 0)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to read object header")
#endif /* H5_GROUP_REVISION */

#ifdef H5_GROUP_REVISION
    ret_value = (stab_exists > 0 || linfo_exists > 0);
#else /* H5_GROUP_REVISION */
    ret_value = stab_exists > 0;
#endif /* H5_GROUP_REVISION */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_group_isa() */

