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
 *              Thursday, August 28, 2008
 *
 * Purpose:	Extensible array testing functions.
 *
 */

/**********************/
/* Module Declaration */
/**********************/

#define H5EA_MODULE
#define H5EA_TESTING


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
 * Function:	H5EA_get_cparam_test
 *
 * Purpose:	Retrieve the parameters used to create the extensible array
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PRIV, NOERR,
herr_t, SUCCEED, -,
H5EA_get_cparam_test(const H5EA_t *ea, H5EA_create_t *cparam))

    /* Check arguments. */
    HDassert(ea);
    HDassert(cparam);

    /* Get extensible array creation parameters */
    cparam->elmt_size = ea->hdr->elmt_size;
    cparam->idx_blk_elmts = ea->hdr->idx_blk_elmts;
    cparam->sup_blk_min_data_ptrs = ea->hdr->sup_blk_min_data_ptrs;
    cparam->data_blk_min_elmts = ea->hdr->data_blk_min_elmts;

END_FUNC(PRIV)  /* end H5EA_get_cparam_test() */


/*-------------------------------------------------------------------------
 * Function:	H5EA_cmp_cparam_test
 *
 * Purpose:	Compare the parameters used to create the extensible array
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PRIV, ERRCATCH,
int, 0, -,
H5EA_cmp_cparam_test(const H5EA_create_t *cparam1, const H5EA_create_t *cparam2))

    /* Check arguments. */
    HDassert(cparam1);
    HDassert(cparam2);

    /* Compare creation parameters for array */
    if(cparam1->elmt_size < cparam2->elmt_size)
        H5_LEAVE(-1)
    else if(cparam1->elmt_size > cparam2->elmt_size)
        H5_LEAVE(1)
    if(cparam1->idx_blk_elmts < cparam2->idx_blk_elmts)
        H5_LEAVE(-1)
    else if(cparam1->idx_blk_elmts > cparam2->idx_blk_elmts)
        H5_LEAVE(1)
    if(cparam1->sup_blk_min_data_ptrs < cparam2->sup_blk_min_data_ptrs)
        H5_LEAVE(-1)
    else if(cparam1->sup_blk_min_data_ptrs > cparam2->sup_blk_min_data_ptrs)
        H5_LEAVE(1)
    if(cparam1->data_blk_min_elmts < cparam2->data_blk_min_elmts)
        H5_LEAVE(-1)
    else if(cparam1->data_blk_min_elmts > cparam2->data_blk_min_elmts)
        H5_LEAVE(1)

CATCH

END_FUNC(PRIV)  /* end H5EA_cmp_cparam_test() */

