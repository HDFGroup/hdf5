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

/****************/
/* Module Setup */
/****************/

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5M_init_interface


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5ESprivate.h"        /* Event Stacks                         */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Lprivate.h"		/* Links        		  	*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLiod_client.h"	/* IOD VOL plugin			*/

#ifdef H5_HAVE_EFF

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

static herr_t H5M_close_map(void *map, H5VL_t *vol_plugin);

/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


/* MAP ID class */
static const H5I_class_t H5I_MAP_CLS[1] = {{
    H5I_MAP,                    /* ID class value */
    0,                          /* Class flags */
    0,                          /* # of reserved IDs for class */
    NULL,                       /* Callback routine for closing objects of this class */
    (H5I_free2_t)H5M_close_map  /* Callback routine for closing auxilary objects of this class */
}};


/*-------------------------------------------------------------------------
 * Function:	H5M_init
 *
 * Purpose:	Initialize the interface from some other package.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
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


/*--------------------------------------------------------------------------
NAME
   H5M_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5M_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5M_init_interface(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Create attribute ID type.
     */
    if(H5I_register_type(H5I_MAP_CLS) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "unable to initialize interface")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5M_init_interface() */


/*--------------------------------------------------------------------------
 NAME
    H5M_term_interface
 PURPOSE
    Terminate various H5M objects
 USAGE
    void H5M_term_interface()
 RETURNS
 DESCRIPTION
    Release any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
int
H5M_term_interface(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_interface_initialize_g) {
	if((n = H5I_nmembers(H5I_MAP))>0) {
	    (void)H5I_clear_type(H5I_MAP, FALSE, FALSE);
	} else {
	    (void)H5I_dec_type_ref(H5I_MAP);
	    H5_interface_initialize_g = 0;
	    n = 1;
	}
    }
    FUNC_LEAVE_NOAPI(n)
} /* H5M_term_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5Mcreate_ff
 *
 * Purpose: 
 *     The H5Mcreate routine creates a new map object named name
 *     at the location given by loc_id.  Map creation and access property
 *     lists (mcpl_id and mapl_id) modify the new map objectÕs behavior.
 *     All keys for the map are of keytype datatype and all values for the
 *     map are of valtype datatype.  The H5Mcreate_ff routine is identical
 *     in functionality, but allows for asynchronous operation and
 *     inclusion in a transaction.  Map IDs returned from this routine
 *     must be released with H5Mclose.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Mcreate_ff(hid_t loc_id, const char *name, hid_t keytype, hid_t valtype, 
             hid_t lcpl_id, hid_t mcpl_id, hid_t mapl_id, hid_t trans_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    void    *map = NULL;        /* pointer to map object created */
    void    *obj = NULL;        /* object token of loc_id */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    H5VL_loc_params_t loc_params;
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE9("i", "i*siiiiiii", loc_id, name, keytype, valtype, lcpl_id, mcpl_id,
             mapl_id, trans_id, estack_id);

    /* Check arguments */
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Get correct property list */
    if(H5P_DEFAULT == lcpl_id)
        lcpl_id = H5P_LINK_CREATE_DEFAULT;
    else
        if(TRUE != H5P_isa_class(lcpl_id, H5P_LINK_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not link creation property list")

    /* Check map creation property list */
    if(H5P_DEFAULT == mcpl_id)
        mcpl_id = H5P_MAP_CREATE_DEFAULT;
    else
        if(TRUE != H5P_isa_class(mcpl_id, H5P_MAP_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not group create property list")

    /* Check the group access property list */
    if(H5P_DEFAULT == mapl_id)
        mapl_id = H5P_MAP_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(mapl_id, H5P_MAP_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not group access property list")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the object */
    if(NULL == (obj = (void *)H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid object/file identifier")
    /* get the plugin pointer */
    if(NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(vol_plugin->cls->value != IOD)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "only IOD plugin supports MAP objects")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
        request->req = NULL;
        req = &request->req;
        request->next = NULL;
        request->vol_plugin = vol_plugin;
        vol_plugin->nrefs ++;
    }

    /* call the IOD specific private routine to create a map object */
    if(NULL == (map = H5VL_iod_map_create(obj, loc_params, name, keytype, valtype, 
                                          lcpl_id, mcpl_id, mapl_id, trans_id, req)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create map")

    /* increment the ref count on the VOL plugin */
    vol_plugin->nrefs ++;

    if(request && *req) {
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")
    }

    /* Get an atom for the map */
    if((ret_value = H5I_register2(H5I_MAP, map, vol_plugin, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize map handle")

done:
    if (ret_value < 0 && map)
        if(H5VL_iod_map_close (map, req) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "unable to release map")
    FUNC_LEAVE_API(ret_value)
} /* end H5Mcreate_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Mopen_ff
 *
 * Purpose: 
 *     The H5Mopen routine opens an existing map object named
 *     name at the location given by loc_id.  The map access property list
 *     (mapl_id) modifies the map objectÕs behavior. The H5Mopen_ff
 *     routine is identical in functionality, but allows for asynchronous
 *     operation and inclusion in a transaction.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Mopen_ff(hid_t loc_id, const char *name, hid_t mapl_id, hid_t rcxt_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    void    *map = NULL;        /* pointer to map object created */
    void    *obj = NULL;        /* object token of loc_id */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    H5VL_loc_params_t loc_params;
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE5("i", "i*siii", loc_id, name, mapl_id, rcxt_id, estack_id);

    /* Check arguments */
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Check the group access property list */
    if(H5P_DEFAULT == mapl_id)
        mapl_id = H5P_MAP_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(mapl_id, H5P_MAP_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not group access property list")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the object */
    if(NULL == (obj = (void *)H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid object/file identifier")
    /* get the plugin pointer */
    if(NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information");

    if(vol_plugin->cls->value != IOD)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "only IOD plugin supports MAP objects");

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
        request->req = NULL;
        req = &request->req;
        request->next = NULL;
        request->vol_plugin = vol_plugin;
        vol_plugin->nrefs ++;
    }

    /* call the IOD specific private routine to create a map object */
    if(NULL == (map = H5VL_iod_map_open(obj, loc_params, name, mapl_id, rcxt_id, req)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create map")

    /* increment the ref count on the VOL plugin */
    vol_plugin->nrefs ++;
    if(request && *req) {
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack")
    }

    /* Get an atom for the map */
    if((ret_value = H5I_register2(H5I_MAP, map, vol_plugin, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize map handle")

done:
    if (ret_value < 0 && map)
        if(H5VL_iod_map_close (map, req) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "unable to release map")
    FUNC_LEAVE_API(ret_value)
} /* end H5Mopen_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Mset_ff
 *
 * Purpose: 
 *      The H5Mset routine inserts or sets a key/value pair in a
 *      map object, given by map_id.  The key (pointed to by key) is of
 *      type key_mem_type_id in memory and the value (pointed to by value)
 *      is of type value_mem_type_id in memory. The data transfer property
 *      list (dxpl_id) may modify the operationÕs behavior. The H5Mset_ff
 *      routine is identical in functionality, but allows for asynchronous
 *      operation and inclusion in a transaction.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Mset_ff(hid_t map_id, hid_t key_mem_type_id, const void *key, hid_t val_mem_type_id, 
          const void *value, hid_t dxpl_id, hid_t trans_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    void    *map = NULL;        /* pointer to map object created */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE8("e", "ii*xi*xiii", map_id, key_mem_type_id, key, val_mem_type_id,
             value, dxpl_id, trans_id, estack_id);

    /* check arguments */
    if(!map_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a map")

    /* Get the default map transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(map_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    /* get the map object */
    if(NULL == (map = (void *)H5I_object_verify(map_id, H5I_MAP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid map identifier")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        request->req = NULL;
        req = &request->req;
        request->next = NULL;
        request->vol_plugin = vol_plugin;
        vol_plugin->nrefs ++;
    }

    /* Set the data through the IOD VOL */
    if((ret_value = H5VL_iod_map_set(map, key_mem_type_id, key, val_mem_type_id, value, 
                                     dxpl_id, trans_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTSET, FAIL, "can't set map KV pair")

    if(request && *req) {
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Mset_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Mget_ff
 *
 * Purpose: 
 *      The H5Mget routine retrieves a value from a map object,
 *      given by map_id.  The key value used to retrieve the value (pointed
 *      to by key) is of type key_mem_type_id in memory and the value
 *      (pointed to by value) is of type value_mem_type_id in memory. The
 *      data transfer property list (dxpl_id) may modify the operationÕs
 *      behavior. The H5Mget_ff routine is identical in functionality, but
 *      allows for asynchronous operation and inclusion in a transaction.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Mget_ff(hid_t map_id, hid_t key_mem_type_id, const void *key, hid_t val_mem_type_id, 
          void *value, hid_t dxpl_id, hid_t rcxt_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    void    *map = NULL;        /* pointer to map object created */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE8("e", "ii*xi*xiii", map_id, key_mem_type_id, key, val_mem_type_id,
             value, dxpl_id, rcxt_id, estack_id);

    /* check arguments */
    if(!map_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a map")

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(map_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    /* get the map object */
    if(NULL == (map = (void *)H5I_object_verify(map_id, H5I_MAP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid map identifier")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        request->req = NULL;
        req = &request->req;
        request->next = NULL;
        request->vol_plugin = vol_plugin;
        vol_plugin->nrefs ++;
    }

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_iod_map_get(map, key_mem_type_id, key, val_mem_type_id, value, 
                                     dxpl_id, rcxt_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get map value")

    if(request && *req) {
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Mget_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Mget_types_ff
 *
 * Purpose: 
 *     The H5Mget_types routine retrieves the datatypes for the
 *     keys and values of a map, given by map_id.  The key datatype is
 *     returned in key_type_id and the value datatype is returned in
 *     value_type_id.  The H5Mget_types_ff routine is identical in
 *     functionality, but allows for asynchronous operation and inclusion
 *     in a transaction.  Either (or both) of the datatype ID pointers may
 *     be NULL, if that datatype information is not desired.  Any datatype
 *     IDs returned from this routine must be released with H5Tclose.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Mget_types_ff(hid_t map_id, hid_t *key_type_id, hid_t *val_type_id, 
                hid_t rcxt_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    void    *map = NULL;        /* pointer to map object created */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "i*i*iii", map_id, key_type_id, val_type_id, rcxt_id, estack_id);

    /* check arguments */
    if(!map_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a map")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(map_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    /* get the map object */
    if(NULL == (map = (void *)H5I_object_verify(map_id, H5I_MAP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid map identifier")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        request->req = NULL;
        req = &request->req;
        request->next = NULL;
        request->vol_plugin = vol_plugin;
        vol_plugin->nrefs ++;
    }

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_iod_map_get_types(map, key_type_id, val_type_id, rcxt_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get map value")

    if(request && *req) {
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Mget_types_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Mget_count_ff
 *
 * Purpose: 
 *     The H5Mget_count routine retrieves the number of key/value
 *     pairs in a map, given by map_id. The H5Mget_count_ff routine is
 *     identical in functionality, but allows for asynchronous operation
 *     and inclusion in a transaction.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Mget_count_ff(hid_t map_id, hsize_t *count, hid_t rcxt_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    void    *map = NULL;        /* pointer to map object created */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "i*hii", map_id, count, rcxt_id, estack_id);

    /* check arguments */
    if(!map_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a map")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(map_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    /* get the map object */
    if(NULL == (map = (void *)H5I_object_verify(map_id, H5I_MAP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid map identifier")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        request->req = NULL;
        req = &request->req;
        request->next = NULL;
        request->vol_plugin = vol_plugin;
        vol_plugin->nrefs ++;
    }

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_iod_map_get_count(map, count, rcxt_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get map value")

    if(request && *req) {
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Mget_count_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Mexists_ff
 *
 * Purpose: 
 *     The H5Mexists routine checks if a key exists in a map,
 *     given by map_id.  The key value used (pointed to by key) is of type
 *     key_mem_type_id in memory and the status of the key in the map is
 *     returned in the exists pointerÕs value. The H5Mexists_ff routine is
 *     identical in functionality, but allows for asynchronous operation
 *     and inclusion in a transaction.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Mexists_ff(hid_t map_id, hid_t key_mem_type_id, const void *key, 
             hbool_t *exists, hid_t rcxt_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    void    *map = NULL;        /* pointer to map object created */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "ii*x*bii", map_id, key_mem_type_id, key, exists, rcxt_id,
             estack_id);

    /* check arguments */
    if(!map_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a map")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(map_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    /* get the map object */
    if(NULL == (map = (void *)H5I_object_verify(map_id, H5I_MAP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid map identifier")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        request->req = NULL;
        req = &request->req;
        request->next = NULL;
        request->vol_plugin = vol_plugin;
        vol_plugin->nrefs ++;
    }

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_iod_map_exists(map, key_mem_type_id, key, exists, rcxt_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get map value")

    if(request && *req) {
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Mexists_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Miterate_ff
 *
 * Purpose: 
 *     The H5Miterate routine iterates over the key/value pairs
 *     in a map, given by map_id.  The user-defined callback routine, given
 *     by callback_func, defined below, will be invoked for each key/value
 *     pair in the map: 
 *
 *     typedef int (*H5M_iterate_func_t)(const void *key, const void *value, void *context); 
 *
 *     Keys and values presented to the
 *     callback routine will be in key_mem_type_id and value_mem_type_id
 *     format, respectively.  Additional information may be given to the
 *     callback routine with the context parameter, which is passed
 *     unmodified from the call to H5Miterate to the applicationÕs
 *     callback. The iteration callback routine should obey the same rules
 *     as other HDF5 iteration callbacks: return H5_ITER_ERROR for an error
 *     condition (which will stop iteration), H5_ITER_CONT for success
 *     (with continued iteration) and H5_ITER_STOP for success (but stop
 *     iteration).  
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Miterate_ff(hid_t map_id, hid_t key_mem_type_id, hid_t value_mem_type_id, 
              H5M_iterate_func_t callback_func, void *context, hid_t rcxt_id)
{
    //void   *map = NULL;
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "iiix*xi", map_id, key_mem_type_id, value_mem_type_id,
             callback_func, context, rcxt_id);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Miterate */


/*-------------------------------------------------------------------------
 * Function:	H5Mdelete_ff
 *
 * Purpose: 
 *      The H5Mdelete routine removes a key/value pair from a map,
 *      given by map_id.  The key value used (pointed to by key) is of type
 *      key_mem_type_id in. The H5Mdelete_ff routine is identical in
 *      functionality, but allows for asynchronous operation and inclusion
 *      in a transaction.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Mdelete_ff(hid_t map_id, hid_t key_mem_type_id, const void *key, 
             hid_t trans_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    void    *map = NULL;        /* pointer to map object created */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "ii*xii", map_id, key_mem_type_id, key, trans_id, estack_id);

    /* check arguments */
    if(!map_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a map")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(map_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    /* get the map object */
    if(NULL == (map = (void *)H5I_object_verify(map_id, H5I_MAP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid map identifier")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        request->req = NULL;
        req = &request->req;
        request->next = NULL;
        request->vol_plugin = vol_plugin;
        vol_plugin->nrefs ++;
    }

    /* delete the key pair through the IOD VOL */
    if((ret_value = H5VL_iod_map_delete(map, key_mem_type_id, key, trans_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get map value")

    if(request && *req) {
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack");
    }
done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Mdelete_ff */


/*-------------------------------------------------------------------------
 * Function:	H5Mclose_ff
 *
 * Purpose: 
 *      The H5Mclose routine terminates access to a map, given by
 *      map_id. The H5Mclose_ff routine is identical in functionality, but
 *      allows for asynchronous operation and inclusion in a transaction.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Mclose_ff(hid_t map_id, hid_t estack_id)
{
    H5VL_t  *vol_plugin = NULL;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ii", map_id, estack_id);

    /* Check args */
    if(NULL == H5I_object_verify(map_id, H5I_MAP))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a map")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(map_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information");

    /* set the event queue and dxpl IDs to be passed on to the VOL layer */
    vol_plugin->close_estack_id = estack_id;
    vol_plugin->close_dxpl_id = H5AC_dxpl_id;

    /*
     * Decrement the counter on the group atom.	 It will be freed if the count
     * reaches zero.
     */
    if(H5I_dec_app_ref(map_id) < 0)
    	HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to close map")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Mclose_ff */


/*-------------------------------------------------------------------------
 * Function:	H5M_close_map
 *
 * Purpose:	Called when the ref count reaches zero on the map_id
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5M_close_map(void *map, H5VL_t *vol_plugin)
{
    H5_priv_request_t  *request = NULL;        /* private request struct inserted in event queue */
    void              **req = NULL;            /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    herr_t              ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(vol_plugin->close_estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        request->req = NULL;
        req = &request->req;
        request->next = NULL;
        request->vol_plugin = vol_plugin;
        vol_plugin->nrefs ++;
    }

    /* Close the map through the VOL*/
    if(H5VL_iod_map_close(map, req) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "unable to close map")

    if(request && *req) {
        if(H5ES_insert(vol_plugin->close_estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack");
    }

    vol_plugin->nrefs --;
    if (0 == vol_plugin->nrefs) {
        vol_plugin->container_name = (const char *)H5MM_xfree(vol_plugin->container_name);
        vol_plugin = (H5VL_t *)H5MM_xfree(vol_plugin);
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5M_close_map() */


/*-------------------------------------------------------------------------
 * Function:	H5Mprefetch
 *
 * Purpose:	Prefetched a Map from Central Storage to Burst Buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Mprefetch(hid_t map_id, hid_t rcxt_id, hrpl_t *replica_id,
                          hid_t mapl_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;       /* pointer to plugin generate requests (Stays NULL if plugin does not support async */
    void    *map = NULL;        /* pointer to map object */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* get the map object */
    if(NULL == (map = (void *)H5I_object_verify(map_id, H5I_MAP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid map identifier")
    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(map_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* Get correct property list */
    if(H5P_DEFAULT == mapl_id)
        mapl_id = H5P_MAP_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(mapl_id, H5P_MAP_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not map access property list")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        request->req = NULL;
        req = &request->req;
        request->next = NULL;
        request->vol_plugin = vol_plugin;
        vol_plugin->nrefs ++;
    }

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_iod_prefetch(map, rcxt_id, replica_id, mapl_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't prefetch map")

    if(request && *req) {
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Mprefetch() */


/*-------------------------------------------------------------------------
 * Function:	H5Mevict
 *
 * Purpose:	Evicts a Map from Burst Buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Mevict(hid_t map_id, uint64_t c_version, hid_t mapl_id, hid_t estack_id)
{
    H5_priv_request_t  *request = NULL; /* private request struct inserted in event queue */
    void    **req = NULL;       /* pointer to plugin generated request pointer */
    void    *map = NULL;       /* pointer to map object */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* get the map object */
    if(NULL == (map = (void *)H5I_object_verify(map_id, H5I_MAP)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid map identifier")
    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(map_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* Get correct property list */
    if(H5P_DEFAULT == mapl_id)
        mapl_id = H5P_MAP_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(mapl_id, H5P_MAP_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not map access property list")

    if(estack_id != H5_EVENT_STACK_NULL) {
        /* create the private request */
        if(NULL == (request = (H5_priv_request_t *)H5MM_calloc(sizeof(H5_priv_request_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        request->req = NULL;
        req = &request->req;
        request->next = NULL;
        request->vol_plugin = vol_plugin;
        vol_plugin->nrefs ++;
    }

    /* Get the data through the IOD VOL */
    if((ret_value = H5VL_iod_evict(map, c_version, mapl_id, req)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't evict map")

    if(request && *req) {
        if(H5ES_insert(estack_id, request) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to insert request in event stack");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Mevict() */

#endif /* H5_HAVE_EFF */
