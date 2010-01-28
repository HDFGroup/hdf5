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
 * Created:		H5EAint.c
 *			Jun 17 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Internal routines for extnsible arrays.
 *
 *-------------------------------------------------------------------------
 */

/**********************/
/* Module Declaration */
/**********************/

#define H5EA_MODULE


/***********************/
/* Other Packages Used */
/***********************/


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5EApkg.h"		/* Extensible Arrays			*/


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



/*-------------------------------------------------------------------------
 * Function:	H5EA__create_flush_depend
 *
 * Purpose:	Create a flush dependency between two data structure components
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Mar 26 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__create_flush_depend(H5AC_info_t *parent_entry, H5AC_info_t *child_entry))

    /* Sanity check */
    HDassert(parent_entry);
    HDassert(child_entry);

    /* Create a flush dependency between parent and child entry */
    if(H5AC_create_flush_dependency(parent_entry, child_entry) < 0)
        H5E_THROW(H5E_CANTDEPEND, "unable to create flush dependency")

CATCH

END_FUNC(PKG)   /* end H5EA__create_flush_depend() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__destroy_flush_depend
 *
 * Purpose:	Destroy a flush dependency between two data structure components
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Mar 26 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__destroy_flush_depend(H5AC_info_t *parent_entry, H5AC_info_t *child_entry))

    /* Sanity check */
    HDassert(parent_entry);
    HDassert(child_entry);

    /* Destroy a flush dependency between parent and child entry */
    if(H5AC_destroy_flush_dependency(parent_entry, child_entry) < 0)
        H5E_THROW(H5E_CANTUNDEPEND, "unable to destroy flush dependency")

CATCH

END_FUNC(PKG)   /* end H5EA__destroy_flush_depend() */

