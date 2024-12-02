/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:	H5Ideprec.c
 *
 * Purpose:	Deprecated functions from the H5I interface.  These
 *          functions are here for compatibility purposes and may be
 *          removed in the future.  Applications should switch to the
 *          newer APIs.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Imodule.h" /* This source code file is part of the H5I module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions                        */
#include "H5Eprivate.h"  /* Error handling                           */
#include "H5Ipkg.h"      /* File access                              */

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

#ifndef H5_NO_DEPRECATED_SYMBOLS
/*-------------------------------------------------------------------------
 * Function:    H5Iregister_type1
 *
 * Purpose:     Public interface to H5I_register_type.  Creates a new type
 *              of ID's to give out.  A specific number (RESERVED) of type
 *              entries may be reserved to enable "constant" values to be handed
 *              out which are valid IDs in the type, but which do not map to any
 *              data structures and are not allocated dynamically later. HASH_SIZE is
 *              the minimum hash table size to use for the type. FREE_FUNC is
 *              called with an object pointer when the object is removed from
 *              the type.
 *
 * Return:      Success:    Type ID of the new type
 *              Failure:    H5I_BADID
 *
 *-------------------------------------------------------------------------
 */
H5I_type_t
H5Iregister_type1(size_t H5_ATTR_UNUSED hash_size, unsigned reserved, H5I_free_t free_func)
{
    H5I_class_t *cls       = NULL;      /* New ID class */
    H5I_type_t   new_type  = H5I_BADID; /* New ID type value */
    H5I_type_t   ret_value = H5I_BADID; /* Return value */

    FUNC_ENTER_API(H5I_BADID)

    /* Generate a new H5I_type_t value */

    /* Increment the number of types */
    if (H5I_next_type_g < H5I_MAX_NUM_TYPES) {
        new_type = (H5I_type_t)H5I_next_type_g;
        H5I_next_type_g++;
    }
    else {
        bool done; /* Indicate that search was successful */
        int  i;

        /* Look for a free type to give out */
        done = false;
        for (i = H5I_NTYPES; i < H5I_MAX_NUM_TYPES && done == false; i++) {
            if (NULL == H5I_type_info_array_g[i]) {
                /* Found a free type ID */
                new_type = (H5I_type_t)i;
                done     = true;
            }
        }

        /* Verify that we found a type to give out */
        if (done == false)
            HGOTO_ERROR(H5E_ID, H5E_NOSPACE, H5I_BADID, "Maximum number of ID types exceeded");
    }

    /* Allocate new ID class */
    if (NULL == (cls = H5MM_calloc(sizeof(H5I_class_t))))
        HGOTO_ERROR(H5E_ID, H5E_CANTALLOC, H5I_BADID, "ID class allocation failed");

    /* Initialize class fields */
    cls->type      = new_type;
    cls->flags     = H5I_CLASS_IS_APPLICATION;
    cls->reserved  = reserved;
    cls->free_func = free_func;

    /* Register the new ID class */
    if (H5I_register_type(cls) < 0)
        HGOTO_ERROR(H5E_ID, H5E_CANTINIT, H5I_BADID, "can't initialize ID class");

    /* Set return value */
    ret_value = new_type;

done:
    /* Clean up on error */
    if (ret_value < 0)
        if (cls)
            cls = H5MM_xfree(cls);

    FUNC_LEAVE_API(ret_value)
} /* end H5Iregister_type1() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */
