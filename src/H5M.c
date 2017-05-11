/****************/
/* Module Setup */
/****************/

#include "H5Mmodule.h"          /* This source code file is part of the H5M module */


/***********/
/* Headers */
/***********/
#include "H5private.h"      /* Generic Functions            */
#include "H5ACprivate.h"    /* Metadata cache           */
#include "H5Eprivate.h"     /* Error handling           */
#include "H5Mpkg.h"     /* Groups               */
#include "H5Iprivate.h"     /* IDs                  */
#include "H5MMprivate.h"    /* Memory management            */
#include "H5Pprivate.h"         /* Property lists                       */
#include "H5VLprivate.h"    /* VOL plugins              */
#include "H5VLdaosm.h"

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

/* Package initialization variable */
hbool_t H5_PKG_INIT_VAR = FALSE;


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Map ID class */
static const H5I_class_t H5I_MAP_CLS[1] = {{
    H5I_MAP,            /* ID class value */
    0,              /* Class flags */
    0,              /* # of reserved IDs for class */
    (H5I_free_t)H5M_close_map /* Callback routine for closing objects of this class */
}};

/* Flag indicating "top" of interface has been initialized */
static hbool_t H5M_top_package_initialize_s = FALSE;



/*-------------------------------------------------------------------------
 * Function:    H5M_init
 *
 * Purpose: Initialize the interface from some other package.
 *
 * Return:  Success:    non-negative
 *      Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5M_init(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5M_init() */


/*-------------------------------------------------------------------------
 * Function:    H5M__init_package
 *
 * Purpose: Initializes the H5M interface.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5M__init_package(void)
{
    herr_t          ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_PACKAGE

    /* Initialize the atom map for the map IDs */
    if(H5I_register_type(H5I_MAP_CLS) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to initialize interface")

    /* Mark "top" of interface as initialized, too */
    H5M_top_package_initialize_s = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5M__init_package() */


/*-------------------------------------------------------------------------
 * Function:    H5M_top_term_package
 *
 * Purpose: Close the "top" of the interface, releasing IDs, etc.
 *
 * Return:  Success:    Positive if anything is done that might
 *              affect other interfaces; zero otherwise.
 *      Failure:    Negative.
 *
 *-------------------------------------------------------------------------
 */
int
H5M_top_term_package(void)
{
    int n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5M_top_package_initialize_s) {
        if(H5I_nmembers(H5I_MAP) > 0) {
            (void)H5I_clear_type(H5I_MAP, FALSE, FALSE);
            n++; /*H5I*/
        } /* end if */

        /* Mark closed */
        if(0 == n)
            H5M_top_package_initialize_s = FALSE;
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5M_top_term_package() */


/*-------------------------------------------------------------------------
 * Function:    H5M_term_package
 *
 * Purpose: Terminates the H5M interface
 *
 * Note:    Finishes shutting down the interface, after
 *      H5M_top_term_package() is called
 *
 *-------------------------------------------------------------------------
 */
int
H5M_term_package(void)
{
    int n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_PKG_INIT_VAR) {
        /* Sanity checks */
        HDassert(0 == H5I_nmembers(H5I_MAP));
        HDassert(FALSE == H5M_top_package_initialize_s);

        /* Destroy the map object id map */
        n += (H5I_dec_type_ref(H5I_MAP) > 0);

        /* Mark closed */
        if(0 == n)
            H5_PKG_INIT_VAR = FALSE;
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5M_term_package() */


/*-------------------------------------------------------------------------
 * Function:    H5Mopen
 *
 * Purpose: Creates a map object for storing key-value pairs.
 *
 * Return:  Success:    Object ID of the map.
 *          Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Mcreate(hid_t loc_id, const char *name, hid_t keytype, hid_t valtype, 
      hid_t mcpl_id, hid_t mapl_id)
{
    void    *map = NULL;        /* pointer to map object created */
    H5VL_object_t *obj = NULL;  /* object token of loc_id */
    H5VL_loc_params_t loc_params;
    hid_t dxpl_id = H5AC_ind_read_dxpl_id; /* dxpl used by library */
    hid_t ret_value;

    FUNC_ENTER_API(FAIL);
    H5TRACE6("i", "i*siiii", loc_id, name, keytype, valtype, mcpl_id, mapl_id);

    /* Check arguments */
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");

    /* Check group creation property list */
    if(H5P_DEFAULT == mcpl_id)
        mcpl_id = H5P_MAP_CREATE_DEFAULT;
    else if(TRUE != H5P_isa_class(mcpl_id, H5P_MAP_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a map create property list");

    /* Verify access property list and get correct dxpl */
    if(H5P_verify_apl_and_dxpl(&mapl_id, H5P_CLS_GACC, &dxpl_id, loc_id, TRUE) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTSET, FAIL, "can't set access and transfer property lists");

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the location object */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier");

    /* call the IOD specific private routine to create a map object */
    if(NULL == (map = H5VL_daosm_map_create(obj->vol_obj, loc_params, name, keytype, valtype, 
                        mcpl_id, mapl_id, dxpl_id, NULL)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create map");

    /* Get an atom for the map */
    if((ret_value = H5VL_register_id(H5I_MAP, map, obj->vol_info, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize map handle");

done:
    if(ret_value < 0 && map)
        if(H5VL_daosm_map_close(map, dxpl_id, NULL) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "unable to release map");

    FUNC_LEAVE_API(ret_value);
} /* end H5Mcreate */


/*-------------------------------------------------------------------------
 * Function:    H5Mopen
 *
 * Purpose: Opens an existing map for modification.
 *
 * Return:  Success:    Object ID of the map.
 *          Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Mopen(hid_t loc_id, const char *name, hid_t mapl_id)
{
    void *map = NULL;                   /* map token from VOL plugin */
    H5VL_object_t *obj = NULL;          /* object token of loc_id */
    H5VL_loc_params_t loc_params;
    hid_t dxpl_id = H5AC_ind_read_dxpl_id; /* dxpl used by library */
    hid_t ret_value;                    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("i", "i*si", loc_id, name, mapl_id);

    /* Check args */
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Verify access property list and get correct dxpl */
    if(H5P_verify_apl_and_dxpl(&mapl_id, H5P_CLS_GACC, &dxpl_id, loc_id, FALSE) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTSET, FAIL, "can't set access and transfer property lists")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the location object */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid location identifier")

    /* Create the map through the VOL */
    if(NULL == (map = H5VL_daosm_map_open(obj->vol_obj, loc_params,
                     name, mapl_id, dxpl_id, NULL)))
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open map")

    /* Get an atom for the map */
    if((ret_value = H5VL_register_id(H5I_MAP, map, obj->vol_info, TRUE)) < 0)
    HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize map handle")

done:
    if(ret_value < 0 && map)
        if(H5VL_daosm_map_close(map, dxpl_id, H5_REQUEST_NULL) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "unable to release map")
    FUNC_LEAVE_API(ret_value)
} /* end H5Mopen() */


/*-------------------------------------------------------------------------
 * Function:H5Mset
 *
 * Purpose: 
 *      The H5Mset routine inserts or sets a key/value pair in a
 *      map object, given by map_id.  The key (pointed to by key) is of
 *      type key_mem_type_id in memory and the value (pointed to by value)
 *      is of type value_mem_type_id in memory.
 *
 * Return: Success:non-negative
 * Failure: negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Mset(hid_t map_id, hid_t key_mem_type_id, const void *key, hid_t val_mem_type_id, 
       const void *value, hid_t dxpl_id)
{
    H5VL_object_t *map = NULL;        /* pointer to map object created */
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL);
    H5TRACE6("e", "ii*xi*xi", map_id, key_mem_type_id, key, val_mem_type_id, value,
             dxpl_id);

    /* check arguments */
    if(NULL == (map = (H5VL_object_t *)H5I_object_verify(map_id, H5I_MAP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a Map");

    /* Get the default map transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");

    /* Set the data through the IOD VOL */
    if((ret_value = H5VL_daosm_map_set(map->vol_obj, key_mem_type_id, key, val_mem_type_id,
                     value, dxpl_id, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTSET, FAIL, "can't set map KV pair");

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5Mset_ff */


/*-------------------------------------------------------------------------
 * Function:H5Mget
 *
 * Purpose: 
 *      The H5Mget routine retrieves a value from a map object,
 *      given by map_id.  The key value used to retrieve the value (pointed
 *      to by key) is of type key_mem_type_id in memory and the value
 *      (pointed to by value) is of type value_mem_type_id in memory.
 *
 * Return:Success:non-negative
 * Failure:negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Mget(hid_t map_id, hid_t key_mem_type_id, const void *key, hid_t val_mem_type_id, 
       void *value, hid_t dxpl_id)
{
    H5VL_object_t *map = NULL;        /* pointer to map object created */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL);
    H5TRACE6("e", "ii*xi*xi", map_id, key_mem_type_id, key, val_mem_type_id, value,
             dxpl_id);

    /* check arguments */
    if(NULL == (map = (H5VL_object_t *)H5I_object_verify(map_id, H5I_MAP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a Map");

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_daosm_map_get(map->vol_obj, key_mem_type_id, key, val_mem_type_id, value, 
                     dxpl_id, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get map value");

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5Mget */


/*-------------------------------------------------------------------------
 * Function: H5Mget_types
 *
 * Purpose: 
 *     The H5Mget_types routine retrieves the datatypes for the
 *     keys and values of a map, given by map_id.  The key datatype is
 *     returned in key_type_id and the value datatype is returned in
 *     value_type_id.
 *
 * Return:Success:non-negative
 * Failure:negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Mget_types(hid_t map_id, hid_t *key_type_id, hid_t *val_type_id)
{
    H5VL_object_t *map = NULL;        /* pointer to map object created */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL);
    H5TRACE3("e", "i*i*i", map_id, key_type_id, val_type_id);

    /* check arguments */
    if(NULL == (map = (H5VL_object_t *)H5I_object_verify(map_id, H5I_MAP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a Map");

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_daosm_map_get_types(map->vol_obj, key_type_id, val_type_id, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get map value");

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5Mget_types */


/*-------------------------------------------------------------------------
 * Function: H5Mget_count
 *
 * Purpose: 
 *     The H5Mget_count routine retrieves the number of key/value
 *     pairs in a map, given by map_id.
 *
 * Return:Success:non-negative
 * Failure:negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Mget_count(hid_t map_id, hsize_t *count)
{
    H5VL_object_t *map = NULL;        /* pointer to map object created */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL);
    H5TRACE2("e", "i*h", map_id, count);

    /* check arguments */
    if(NULL == (map = (H5VL_object_t *)H5I_object_verify(map_id, H5I_MAP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a Map");

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_daosm_map_get_count(map->vol_obj, count, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get map value");

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5Mget_count */


/*-------------------------------------------------------------------------
 * Function: H5Mexists
 *
 * Purpose: 
 *     The H5Mexists routine checks if a key exists in a map,
 *     given by map_id.  The key value used (pointed to by key) is of type
 *     key_mem_type_id in memory and the status of the key in the map is
 *     returned in the exists pointerÕs value. The H5Mexists_ff routine is
 *     identical in functionality, but allows for asynchronous operation
 *     and inclusion in a transaction.
 *
 * Return:Success:non-negative
 * Failure:negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Mexists(hid_t map_id, hid_t key_mem_type_id, const void *key, hbool_t *exists)
{
    H5VL_object_t *map = NULL;        /* pointer to map object created */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL);
    H5TRACE4("e", "ii*x*b", map_id, key_mem_type_id, key, exists);

    /* check arguments */
    if(NULL == (map = (H5VL_object_t *)H5I_object_verify(map_id, H5I_MAP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a Map");

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_daosm_map_exists(map->vol_obj, key_mem_type_id, key, exists, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get map value");

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5Mexists */


/*-------------------------------------------------------------------------
 * Function:    H5Mclose
 *
 * Purpose: Closes the specified map.  The map ID will no longer be
 *      valid for accessing the map.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *      Wednesday, December 31, 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Mclose(hid_t map_id)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", map_id);

    /* Check args */
    if(NULL == H5I_object_verify(map_id, H5I_MAP))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a map")

    /*
     * Decrement the counter on the map atom.    It will be freed if the count
     * reaches zero.
     */
    if(H5I_dec_app_ref(map_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to close map")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Mclose() */


/*-------------------------------------------------------------------------
 * Function:    H5M_close_map
 *
 * Purpose: Called when the ref count reaches zero on the map_id
 *
 * Return:  Success:    Non-negative
 *
 *      Failure:    Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              June 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5M_close_map(void *_map)
{
    H5VL_object_t *map = (H5VL_object_t *)_map;
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Close the map through the VOL*/
    if((ret_value = H5VL_daosm_map_close(map->vol_obj, H5AC_ind_read_dxpl_id, NULL)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "unable to close map")

    /* free map */
    if(H5VL_free_object(map) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDEC, FAIL, "unable to free VOL object")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5M_close_map() */

