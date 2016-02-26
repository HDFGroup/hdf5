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

/*
 * Purpose:     Deprecated functions from the H5R interface.  These
 *              functions are here for compatibility purposes and may be
 *              removed in the future.  Applications should switch to the
 *              newer APIs.
 */

/****************/
/* Module Setup */
/****************/

#include "H5Rmodule.h"          /* This source code file is part of the H5R module */


/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                    */
#include "H5Rpkg.h"             /* References                           */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5Iprivate.h"         /* IDs                                  */
#include "H5Gprivate.h"         /* Groups                               */
#include "H5Oprivate.h"         /* Object headers                       */
#include "H5Ppublic.h"          /* for using H5P_DATASET_ACCESS_DEFAULT */
#include "H5Sprivate.h"         /* Dataspaces                           */


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
 * Function:    H5Rcreate
 *
 * Purpose: Creates a particular type of reference specified with REF_TYPE,
 * in the space pointed to by REF. The LOC_ID and NAME are used to locate the
 * object pointed to and the SPACE_ID is used to choose the region pointed to
 * (for Dataset Region references).
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Rcreate(void *_ref, hid_t loc_id, const char *name, H5R_type_t ref_type, hid_t space_id)
{
    H5G_loc_t loc; /* File location */
    H5S_t  *space = NULL; /* Pointer to dataspace containing region */
    href_t *ref_ptr = (href_t *)_ref; /* Return reference pointer */
    href_t ref; /* Return reference */
    herr_t ret_value; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given")

    switch (ref_type) {
        case H5R_OBJECT:
            if(NULL == (ref = H5R_create_object(&loc, name, H5AC_dxpl_id)))
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to create object reference")
            break;
        case H5R_DATASET_REGION:
        case H5R_REGION:
            if(space_id == H5I_BADID)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "reference region dataspace id must be valid")
            if(space_id != H5I_BADID && (NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE))))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")
            if(NULL == (ref = H5R_create_region(&loc, name, H5AC_dxpl_id, space)))
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to create object reference")
            break;
        case H5R_ATTR:
        case H5R_EXT_OBJECT:
        case H5R_EXT_REGION:
        case H5R_EXT_ATTR:
        case H5R_BADTYPE:
        case H5R_MAXTYPE:
        default:
            HDassert("unsupported reference type" && 0);
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "internal error (unsupported reference type)")
            break;
    }

    *ref_ptr = ref;
    ret_value = SUCCEED;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rcreate() */

/*-------------------------------------------------------------------------
 * Function:    H5Rdereference1
 *
 * Purpose: Given a reference to some object, open that object and return an
 * ID for that object.
 *
 * Return:  Valid ID on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Rdereference1(hid_t obj_id, H5R_type_t ref_type, const void *_ref)
{
    H5G_loc_t loc;      /* Group location */
    H5F_t *file = NULL; /* File object */
    const href_t *ref_ptr = (const href_t *)_ref; /* Reference pointer */
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(H5G_loc(obj_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(ref_ptr == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(ref_type <= H5R_BADTYPE || ref_type >= H5R_MAXTYPE || (H5R_get_type(*ref_ptr) != ref_type))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")

    /* Get the file pointer from the entry */
    file = loc.oloc->file;

    /* Dereference */
    if((ret_value = H5R__dereference(file, H5P_DATASET_ACCESS_DEFAULT, H5AC_ind_dxpl_id, *ref_ptr, TRUE)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable dereference object")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rdereference1() */

/*-------------------------------------------------------------------------
 * Function:    H5Rdereference2
 *
 * Purpose: Given a reference to some object, open that object and return an
 * ID for that object.
 *
 * Return:  Valid ID on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Rdereference2(hid_t obj_id, hid_t oapl_id, H5R_type_t ref_type, const void *_ref)
{
    H5G_loc_t loc;      /* Group location */
    const href_t *ref_ptr = (const href_t *)_ref; /* Reference pointer */
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(H5G_loc(obj_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(oapl_id < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")
    if(ref_ptr == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(ref_type <= H5R_BADTYPE || ref_type >= H5R_MAXTYPE || (H5R_get_type(*ref_ptr) != ref_type))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")

    /* Dereference */
    if((ret_value = H5R__dereference(loc.oloc->file, oapl_id, H5AC_ind_dxpl_id, *ref_ptr, TRUE)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable dereference object")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rdereference2() */

/*-------------------------------------------------------------------------
 * Function:    H5Rget_region1
 *
 * Purpose: Given a reference to some object, creates a copy of the dataset
 * pointed to's dataspace and defines a selection in the copy which is the
 * region pointed to.
 *
 * Return:  Valid ID on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Rget_region1(hid_t dataset, H5R_type_t ref_type, const void *_ref)
{
    H5G_loc_t loc;          /* Object's group location */
    H5S_t *space = NULL;    /* Dataspace object */
    const href_t *ref_ptr = (const href_t *)_ref; /* Reference pointer */
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(H5G_loc(dataset, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(ref_ptr == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if((ref_type != H5R_REGION && ref_type != H5R_EXT_REGION) || (H5R_get_type(*ref_ptr) != ref_type))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")

    /* Get the dataspace with the correct region selected */
    if((space = H5R__get_region(loc.oloc->file, H5AC_ind_dxpl_id, *ref_ptr)) == NULL)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCREATE, FAIL, "unable to create dataspace")

    /* Atomize */
    if((ret_value = H5I_register (H5I_DATASPACE, space, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace atom")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rget_region1() */

/*-------------------------------------------------------------------------
 * Function:    H5Rget_obj_type1
 *
 * Purpose: Given a reference to some object, this function returns the type
 * of object pointed to.
 *
 * Return:  An object type defined in H5Gpublic.h/H5G_UNKNOWN on failure
 *
 *-------------------------------------------------------------------------
 */
H5G_obj_t
H5Rget_obj_type1(hid_t id, H5R_type_t ref_type, const void *_ref)
{
    H5G_loc_t loc;              /* Object location */
    H5O_type_t obj_type;        /* Object type */
    const href_t *ref_ptr = (const href_t *)_ref; /* Reference pointer */
    H5G_obj_t ret_value;        /* Return value */

    FUNC_ENTER_API(H5G_UNKNOWN)

    /* Check args */
    if(H5G_loc(id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5G_UNKNOWN, "not a location")
    if(ref_ptr == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5G_UNKNOWN, "invalid reference pointer")
    if(ref_type <= H5R_BADTYPE || ref_type >= H5R_MAXTYPE || (H5R_get_type(*ref_ptr) != ref_type))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5G_UNKNOWN, "invalid reference type")

    /* Get the object information */
    if(H5R__get_obj_type(loc.oloc->file, H5AC_ind_dxpl_id, *ref_ptr, &obj_type) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, H5G_UNKNOWN, "unable to determine object type")

    /* Set return value */
    ret_value = H5G_map_obj_type(obj_type);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rget_obj_type1() */

/*-------------------------------------------------------------------------
 * Function:    H5Rget_obj_type2
 *
 * Purpose: Given a reference to some object, this function returns the type
 * of object pointed to.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t H5Rget_obj_type2(hid_t loc_id, H5R_type_t ref_type, const void *_ref,
    H5O_type_t *obj_type)
{
    H5G_loc_t loc;              /* Object location */
    const href_t *ref_ptr = (const href_t *)_ref; /* Reference pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(ref_ptr == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(ref_type <= H5R_BADTYPE || ref_type >= H5R_MAXTYPE || (H5R_get_type(*ref_ptr) != ref_type))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")

    /* Get the object information */
    if(H5R__get_obj_type(loc.oloc->file, H5AC_ind_dxpl_id, *ref_ptr, obj_type) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to determine object type")

done:
    FUNC_LEAVE_API(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Rget_name
 *
 * Purpose: Given a reference to some object, determine a path to the object
 * referenced in the file.
 *
 * Return:  Non-negative length of the path on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
ssize_t H5Rget_name(hid_t loc_id, H5R_type_t ref_type, const void *_ref,
    char *name/*out*/, size_t size)
{
    H5G_loc_t loc;      /* Group location */
    H5F_t *file;        /* File object */
    const href_t *ref_ptr = (const href_t *)_ref; /* Reference pointer */
    ssize_t ret_value;  /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(ref_ptr == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(ref_type <= H5R_BADTYPE || ref_type >= H5R_MAXTYPE || (H5R_get_type(*ref_ptr) != ref_type))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")

    /* Get the file pointer from the entry */
    file = loc.oloc->file;

    /* Get name */
    switch (ref_type) {
        case H5R_OBJECT:
        case H5R_DATASET_REGION:
        case H5R_REGION:
            if((ret_value = H5R__get_obj_name(file, H5P_DEFAULT, H5AC_ind_dxpl_id, loc_id, *ref_ptr, name, size)) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to determine object path")
            break;
        case H5R_ATTR:
            if((ret_value = H5R__get_attr_name(file, *ref_ptr, name, size)) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to determine object path")
            break;
        case H5R_EXT_OBJECT:
        case H5R_EXT_REGION:
        case H5R_EXT_ATTR:
        case H5R_BADTYPE:
        case H5R_MAXTYPE:
        default:
            HDassert("unsupported reference type" && 0);
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "internal error (unsupported reference type)")
            break;
    }

done:
    FUNC_LEAVE_API(ret_value)
}

#endif /* H5_NO_DEPRECATED_SYMBOLS */

