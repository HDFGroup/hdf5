/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Purpose:	Index routines.
 */

/****************/
/* Module Setup */
/****************/

#include "H5Xmodule.h"      /* This source code file is part of the H5X module */

/***********/
/* Headers */
/***********/
#include "H5private.h"      /* Generic Functions */
#include "H5Xpkg.h"         /* Index plugins */
#include "H5Eprivate.h"     /* Error handling */
#include "H5Iprivate.h"     /* IDs */
#include "H5MMprivate.h"    /* Memory management */
#include "H5Pprivate.h"     /* Property lists */
#include "H5Dprivate.h"     /* Datasets */
#include "H5Oprivate.h"

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Local Prototypes */
/********************/

static herr_t H5X_create_data(hid_t loc_id, H5X_class_t *idx_class,
    hid_t xcpl_id, hid_t xapl_id);
static herr_t H5X_create_metadata(hid_t loc_id, H5X_class_t *idx_class,
    hid_t xcpl_id, hid_t xapl_id);

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
static size_t       H5X_table_alloc_g = 0;
static size_t       H5X_table_used_g = 0;
static H5X_class_t *H5X_table_g = NULL;


static H5_INLINE hbool_t
H5X__registered(unsigned plugin_id, size_t *index)
{
    size_t plugin_index;
    hbool_t registered = FALSE;

    /* Is the filter already registered? */
    for (plugin_index = 0; plugin_index < H5X_table_used_g; plugin_index++) {
        if (H5X_table_g[plugin_index].id == plugin_id) {
            registered = TRUE;
            if (index) *index = plugin_index;
            break;
        }
    }

    return registered;
}

/*-------------------------------------------------------------------------
 * Function:    H5X_init
 *
 * Purpose:	Initialize the interface from some other package.
 *
 * Return:  Success:    non-negative
 *          Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5X_init(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_init() */

/*--------------------------------------------------------------------------
NAME
   H5X__init_package -- Initialize interface-specific information
USAGE
    herr_t H5X__init_package()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
herr_t
H5X__init_package(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (H5X_register(H5X_DUMMY) < 0)
        HGOTO_ERROR (H5E_PLINE, H5E_CANTINIT, FAIL, "unable to register dummy index plugin");
#ifdef H5_HAVE_ALACRITY
    if (H5X_register(H5X_ALACRITY) < 0)
        HGOTO_ERROR (H5E_PLINE, H5E_CANTINIT, FAIL, "unable to register ALACRITY index plugin");
#endif
#ifdef H5_HAVE_FASTBIT
    if (H5X_register(H5X_FASTBIT) < 0)
        HGOTO_ERROR (H5E_PLINE, H5E_CANTINIT, FAIL, "unable to register FastBit index plugin");
#endif
    if (H5X_register(H5X_META_DUMMY) < 0)
        HGOTO_ERROR (H5E_PLINE, H5E_CANTINIT, FAIL, "unable to register meta dummy index plugin");
#ifdef H5_HAVE_DB
    if (H5X_register(H5X_META_DB) < 0)
        HGOTO_ERROR (H5E_PLINE, H5E_CANTINIT, FAIL, "unable to register Berkeley DB index plugin");
#endif
#ifdef H5_HAVE_MDHIM
    if (H5X_register(H5X_META_MDHIM) < 0)
        HGOTO_ERROR (H5E_PLINE, H5E_CANTINIT, FAIL, "unable to register MDHIM index plugin");
#endif

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__init_package() */

/*--------------------------------------------------------------------------
 NAME
    H5X_term_package
 PURPOSE
    Terminate various H5X objects
 USAGE
    void H5X_term_package()
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
int
H5X_term_package(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_PKG_INIT_VAR) {
        /* Free the table of index plugins */
        if (H5X_table_g) {
            H5X_table_g = (H5X_class_t *) H5MM_xfree(H5X_table_g);
            H5X_table_used_g = H5X_table_alloc_g = 0;
            n++;
        } /* end if */

        /* Mark the interface as uninitialized */
        if(0 == n)
            H5_PKG_INIT_VAR = FALSE;
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5X_term_package() */

/*-------------------------------------------------------------------------
 * Function:    H5X_registered
 *
 * Purpose: Get registered plugin index class from plugin ID.
 *
 * Return:  Success:    pointer to registered index class
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
H5X_class_t *
H5X_registered(unsigned plugin_id)
{
    H5X_class_t *ret_value = NULL;
    size_t plugin_index;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if (H5X__registered(plugin_id, &plugin_index)) {
        ret_value = &H5X_table_g[plugin_index];
    }

    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Xregister
 *
 * Purpose: This function registers new index classes.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Xregister(const H5X_class_t *index_class)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "*Xc", index_class);

    /* Check args */
    if (!index_class)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index plugin class");
    if (index_class->version != H5X_CLASS_T_VERS)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index plugin version number");
    if (index_class->id > H5X_PLUGIN_MAX)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index plugin identification number");
    if (index_class->id < H5X_PLUGIN_RESERVED)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "unable to modify predefined index plugins");

    /* Do or do not. There is no try. */
    if (H5X_register(index_class) < 0)
        HGOTO_ERROR (H5E_PLINE, H5E_CANTINIT, FAIL, "unable to register index");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Xregister() */

/*-------------------------------------------------------------------------
 * Function:    H5X_register
 *
 * Purpose: Same as the public version except this one allows plugins
 *      to be set for predefined method numbers < H5X_INDEX_RESERVED
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5X_register(const H5X_class_t *index_class)
{
    size_t i;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(index_class);
    HDassert(index_class->id <= H5X_PLUGIN_MAX);

    /* Is the index class already registered? */
    for (i = 0; i < H5X_table_used_g; i++)
        if (H5X_table_g[i].id == index_class->id)
            break;

    /* Filter not already registered */
    if (i >= H5X_table_used_g) {
        if (H5X_table_used_g >= H5X_table_alloc_g) {
            size_t n = MAX(H5X_MAX_NPLUGINS, 2 * H5X_table_alloc_g);
            H5X_class_t *table = (H5X_class_t *) H5MM_realloc(H5X_table_g,
                    n * sizeof(H5X_class_t));
            if (!table)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to extend index plugin table");
            H5X_table_g = table;
            H5X_table_alloc_g = n;
        } /* end if */

        /* Initialize */
        i = H5X_table_used_g++;
        HDmemcpy(H5X_table_g + i, index_class, sizeof(H5X_class_t));
    } /* end if */
    else { /* Filter already registered */
        /* Replace old contents */
        HDmemcpy(H5X_table_g + i, index_class, sizeof(H5X_class_t));
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_register() */

/*-------------------------------------------------------------------------
 * Function:    H5Xunregister
 *
 * Purpose: This function unregisters an index class.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Xunregister(unsigned plugin_id)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "Iu", plugin_id);

    /* Check args */
    if (plugin_id > H5X_PLUGIN_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid filter identification number");
    if (plugin_id < H5X_PLUGIN_RESERVED)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to modify predefined index plugins");

    /* Do it */
    if (H5X_unregister(plugin_id) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to unregister index plugin");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Xunregister() */

/*-------------------------------------------------------------------------
 * Function:    H5X_unregister
 *
 * Purpose: Same as the public version except this one allows plugins
 *      to be unset for predefined method numbers < H5X_PLUGIN_RESERVED
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5X_unregister(unsigned plugin_id)
{
    size_t       plugin_index;        /* Local index variable for filter */
//    H5X_object_t object;
    herr_t       ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(plugin_id <= H5X_PLUGIN_MAX);

    /* Is the plugin already registered? */
    if (FALSE == H5X__registered(plugin_id, &plugin_index))
        HGOTO_ERROR(H5E_PLINE, H5E_NOTFOUND, FAIL, "plugin is not registered")

    /* Initialize the structure object for iteration */
//    object.plugin_id = plugin_id;
//    object.found = FALSE;

    /* Iterate through all opened datasets, returns a failure if any of them uses the filter */
//    if(H5I_iterate(H5I_DATASET, H5Z__check_unregister_dset_cb, &object, FALSE) < 0)
//        HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed")

//    if(object.found)
//        HGOTO_ERROR(H5E_PLINE, H5E_CANTRELEASE, FAIL, "can't unregister plugin because a dataset is still using it")

    /* Iterate through all opened groups, returns a failure if any of them uses the filter */
//    if(H5I_iterate(H5I_GROUP, H5Z__check_unregister_group_cb, &object, FALSE) < 0)
//        HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed")

//    if(object.found)
//        HGOTO_ERROR(H5E_PLINE, H5E_CANTRELEASE, FAIL, "can't unregister plugin because a group is still using it")

    /* Iterate through all opened files and flush them */
//    if(H5I_iterate(H5I_FILE, H5Z__flush_file_cb, NULL, FALSE) < 0)
//        HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed")

    /* Remove filter from table */
    /* Don't worry about shrinking table size (for now) */
    HDmemmove(&H5X_table_g[plugin_index], &H5X_table_g[plugin_index + 1],
            sizeof(H5X_class_t) * ((H5X_table_used_g - 1) - plugin_index));

    H5X_table_used_g--;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_unregister() */

/*-------------------------------------------------------------------------
 * Function:    H5X_can_create_data
 *
 * Purpose:
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5X_can_create_data(hid_t dset_id, hid_t dcpl_id)
{
    hid_t xcpl_id = H5P_DEFAULT;
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(dset_id != FAIL);
    HDassert(H5I_GENPROP_LST == H5I_get_type(dcpl_id));

    /* Check if the property list is non-default */
    if(dcpl_id != H5P_DATASET_CREATE_DEFAULT) {
        H5P_genplist_t *plist; /* Dataset creation property list object */
        unsigned plugin = 0;   /* Index plugin value property to query */

        /* Get dataset creation property list object */
        if(NULL == (plist = (H5P_genplist_t *) H5I_object(dcpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get dataset creation property list");

        /* Get layout information */
        if(H5P_get(plist, H5D_CRT_INDEX_PLUGIN_NAME, &plugin) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't retrieve index plugin value");

        if (H5X_PLUGIN_NONE != plugin) {
            if (FAIL == (xcpl_id = H5Pcreate(H5P_INDEX_CREATE)))
                HGOTO_ERROR(H5E_PLIST, H5E_CANTCREATE, FAIL, "can't create index property");

            if (FAIL == H5Pset_index_read_on_create(xcpl_id, FALSE))
                HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set index property");

            if (H5X_create(dset_id, plugin, xcpl_id) < 0)
                HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create index");
        }
    } /* end if */

done:
    if (xcpl_id != H5P_DEFAULT) H5Pclose(xcpl_id);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_can_create() */

/*-------------------------------------------------------------------------
 * Function:    H5Xcreate
 *
 * Purpose: Create a new index in a container.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Xcreate(hid_t loc_id, unsigned plugin_id, hid_t xcpl_id)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iIui", loc_id, plugin_id, xcpl_id);

    /* Check args */
    if (plugin_id > H5X_PLUGIN_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid plugin identification number");
    if (FAIL == loc_id)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid loc ID");

    if (FAIL == H5X_create(loc_id, plugin_id, xcpl_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "cannot create index");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Xcreate() */

/*-------------------------------------------------------------------------
 * Function:    H5X_create
 *
 * Purpose: Create a new index in a container.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5X_create(hid_t loc_id, unsigned plugin_id, hid_t xcpl_id)
{
    H5X_class_t *idx_class = NULL;
    hid_t xapl_id = H5P_INDEX_ACCESS_DEFAULT; /* TODO for now */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    HDassert(loc_id != FAIL);
    HDassert(plugin_id <= H5X_PLUGIN_MAX);

    /* Is the plugin already registered */
    if (NULL == (idx_class = H5X_registered(plugin_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "plugin is not registered");

    /* Get correct property list */
    if (H5P_DEFAULT == xcpl_id)
        xcpl_id = H5P_INDEX_CREATE_DEFAULT;
    else
        if (TRUE != H5P_isa_class(xcpl_id, H5P_INDEX_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not index creation property list");

    if (idx_class->type == H5X_TYPE_DATA) {
        if (FAIL == H5X_create_data(loc_id, idx_class, xcpl_id, xapl_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "cannot create data index");
    }
    else if (idx_class->type == H5X_TYPE_METADATA) {
        if (FAIL == H5X_create_metadata(loc_id, idx_class, xcpl_id, xapl_id))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "cannot create metadata index");
    } else
        HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "invalid index type");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_create() */

/*-------------------------------------------------------------------------
 * Function:    H5X_create_data
 *
 * Purpose: Create a new index in a container.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_create_data(hid_t loc_id, H5X_class_t *idx_class, hid_t xcpl_id,
    hid_t xapl_id)
{
    H5D_t *dset = NULL;
    void *idx_handle = NULL; /* Pointer to index object created */
    size_t metadata_size; /* Size of metadata created by plugin */
    void *metadata; /* Metadata created by plugin that needs to be stored */
    H5O_idxinfo_t idx_info;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(idx_class);
    HDassert(idx_class->type == H5X_TYPE_DATA);

    /* Get dset object */
    if (NULL == (dset = (H5D_t *) H5I_object_verify(loc_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");

    /* Call create of the plugin */
    if (NULL == idx_class->idx_class->data_class.create)
        HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "plugin create callback is not defined");
    if (NULL == (idx_handle = idx_class->idx_class->data_class.create(loc_id, xcpl_id, xapl_id,
            &metadata_size, &metadata)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "cannot create new plugin index");

    /* Add idx_handle to dataset */
    idx_info.plugin_id = idx_class->id;
    idx_info.metadata_size = metadata_size;
    idx_info.metadata = metadata;
    if (FAIL == H5D_set_index(dset, idx_class, idx_handle, &idx_info))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "cannot set index");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_create_data() */

/*-------------------------------------------------------------------------
 * Function:    H5X_create_metadata
 *
 * Purpose: Create a new index in a container.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_create_metadata(hid_t loc_id, H5X_class_t *idx_class, hid_t xcpl_id,
    hid_t xapl_id)
{
    H5F_t *file = NULL;
    void *idx_handle = NULL; /* Pointer to index object created */
    size_t metadata_size; /* Size of metadata created by plugin */
    void *metadata = NULL; /* Metadata created by plugin that needs to be stored */
    H5O_idxinfo_t idx_info;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(idx_class);
    HDassert(idx_class->type == H5X_TYPE_METADATA);

    /* Get file object */
    if (NULL == (file = (H5F_t *) H5I_object_verify(loc_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file");

    /* Call create of the plugin */
    if (NULL == idx_class->idx_class->metadata_class.create)
        HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "plugin create callback is not defined");
    if (NULL == (idx_handle = idx_class->idx_class->metadata_class.create(loc_id, xcpl_id, xapl_id,
            &metadata_size, &metadata)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "cannot create new plugin index");

    /* Add idx_handle to dataset */
    idx_info.plugin_id = idx_class->id;
    idx_info.metadata_size = metadata_size;
    idx_info.metadata = metadata;
    if (FAIL == H5F_set_index(file, idx_class, idx_handle, &idx_info))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "cannot set index");

done:
    H5MM_free(metadata);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_create_metadata() */

/*-------------------------------------------------------------------------
 * Function:    H5Xremove
 *
 * Purpose: Remove an index from objects in a container.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Xremove(hid_t loc_id, unsigned plugin_id)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", loc_id, plugin_id);

    /* Check args */
    if (plugin_id > H5X_PLUGIN_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid plugin identification number");
    if (FAIL == loc_id)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

    /* Remove index from dataset */
    if (FAIL == H5X_remove(loc_id, plugin_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTDELETE, FAIL, "unable to delete index")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Xremove() */

/*-------------------------------------------------------------------------
 * Function:    H5X_remove
 *
 * Purpose: Remove an index from objects in a container.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5X_remove(hid_t loc_id, unsigned plugin_id)
{
    H5I_type_t loc_type;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    HDassert(loc_id != FAIL);
    HDassert(plugin_id <= H5X_PLUGIN_MAX);

    if (H5I_BADID == (loc_type = H5I_get_type(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a valid type")

    switch (loc_type) {
        case H5I_FILE:
        {
            H5F_t *file = NULL;

            if (NULL == (file = (H5F_t *) H5I_object(loc_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")
            if (FAIL == H5F_remove_index(file, plugin_id))
                HGOTO_ERROR(H5E_INDEX, H5E_CANTDELETE, FAIL, "unable to delete index")
        }
            break;
        case H5I_DATASET:
        {
            H5D_t *dset = NULL;

            if (NULL == (dset = (H5D_t *) H5I_object(loc_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
            if (FAIL == H5D_remove_index(dset, plugin_id))
                HGOTO_ERROR(H5E_INDEX, H5E_CANTDELETE, FAIL, "unable to delete index")
        }
            break;
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a supported type");
            break;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_remove() */

/*-------------------------------------------------------------------------
 * Function:    H5Xget_count
 *
 * Purpose: Determine the number of index objects on an object.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Xget_count(hid_t loc_id, hsize_t *idx_count)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*h", loc_id, idx_count);

    if (FAIL == loc_id)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if (!idx_count)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "idx_count is NULL");

    if (FAIL == H5X_get_count(loc_id, idx_count))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "cannot get index count");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Xget_count() */

/*-------------------------------------------------------------------------
 * Function:    H5X_get_count
 *
 * Purpose: Determine the number of index objects on a dataset.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5X_get_count(hid_t loc_id, hsize_t *idx_count)
{
    H5I_type_t loc_type;
    H5X_class_t *idx_class = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(loc_id != H5I_BADID);
    HDassert(idx_count);

    if (H5I_BADID == (loc_type = H5I_get_type(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a valid type")

    switch (loc_type) {
        case H5I_FILE:
        {
            H5F_t *file = NULL;

            if (NULL == (file = (H5F_t *) H5I_object(loc_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")
            if (FAIL == H5F_get_index(file, &idx_class, NULL, NULL))
                HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "plugin is not registered");
        }
            break;
        case H5I_DATASET:
        {
            H5D_t *dset = NULL;

            if (NULL == (dset = (H5D_t *) H5I_object(loc_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
            if (FAIL == H5D_get_index(dset, &idx_class, NULL, NULL))
                HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "plugin is not registered");
        }
            break;
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a supported type");
            break;
    }

    *idx_count = (idx_class) ? 1 : 0;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_get_count() */

/*-------------------------------------------------------------------------
 * Function:    H5Xget_size
 *
 * Purpose: Returns the amount of storage allocated for an index.
 *
 * Return:  Greater than or equal to zero on success/Zero on failure
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5Xget_size(hid_t loc_id)
{
    hsize_t ret_value = 0; /* Return value */

    FUNC_ENTER_API(0)
    H5TRACE1("h", "i", loc_id);

    if (FAIL == loc_id)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a location")
    if (FAIL == H5X_get_size(loc_id, &ret_value))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, 0, "cannot get index storage size");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Xget_size() */

/*-------------------------------------------------------------------------
 * Function:    H5X_get_size
 *
 * Purpose: Returns the amount of storage allocated for an index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5X_get_size(hid_t loc_id, hsize_t *idx_size)
{
    H5I_type_t loc_type;
    hsize_t actual_size = 0;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(loc_id != H5I_BADID);
    HDassert(idx_size);

    if (H5I_BADID == (loc_type = H5I_get_type(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a valid type")

    switch (loc_type) {
        case H5I_FILE:
        {
            H5F_t *file = NULL;

            if (NULL == (file = (H5F_t *) H5I_object(loc_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")
            if (FAIL == H5F_get_index_size(file, &actual_size))
                HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "plugin is not registered");
        }
            break;
        case H5I_DATASET:
        {
            H5D_t *dset = NULL;

            if (NULL == (dset = (H5D_t *) H5I_object(loc_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
            if (FAIL == H5D_get_index_size(dset, &actual_size))
                HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "plugin is not registered");
        }
            break;
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a supported type");
            break;
    }

    *idx_size = actual_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_get_size() */

#ifdef H5_HAVE_PARALLEL
/*-------------------------------------------------------------------------
 * Function:    H5Xinitialize_parallel_query
 *              (and support functions)
 *
 * Purpose:     The intent of this collection of functions is two-fold:
 *              1) provide a mechanism to define parallel access to the
 *                 requested HDF file and datasets.
 *              2) provide the functions to define MPI groups which contain
 *                 all processes allocated to the same 'node'. 
 *                 On a machine such as 'cori' at LBNL, the MAX number of
 *                 processes on a single node will be 64 if no additional
 *                 action is taken.  
 *
 *                 Once an MPI group is defined, all non-rank-0 processes
 *                 will send their query results to the group-rank-0 process
 *                 for writing to a file.
 *
 *-------------------------------------------------------------------------
 */
#define HR_RANK(R) ((int)hr_view[(R)].rank & 0x3FFFFFFF)
#define HR_ID(R)  (hr_view[(R)].hostid)

typedef struct {
  int hostid;
  int rank;
} hr_tuple;

static int enable_parallel_queries = 0;
static int g_mpi_rank = -1;
static int g_mpi_size = -1;

static int g_local_peer_count = -1;
static int g_local_peer_rank = -1;

static int g_group_peer_count = -1;
static int g_group_peer_rank = -1;
static int g_group_id = -1;

static int  *layout=0;
static int  *_ranks=0;
static int  *_nodes=0;
static int  *_peercounts=0;

static MPI_Comm query_group_comm = MPI_COMM_NULL;

static int
cmphostid(const void *h1, const void *h2)
{
  return (*(int *)h1 > *(int *)h2);
}

static int
_get_nodecount(void)
{
    static int nodecount = 0;
    if (nodecount) return nodecount;

    if (_ranks && _nodes && _peercounts && layout) {
       int i, basei=0;
       hr_tuple *hr_view = (hr_tuple *)layout;
       int checkid = HR_ID(0);
       _nodes[0] = 0;

       for(i=0; i < g_mpi_size; i++) {
          _ranks[i] = HR_RANK(i);
          if (HR_ID(i) != checkid) {
             checkid = HR_ID(i);
             _peercounts[nodecount] = i - basei;
             basei = i;
            _nodes[nodecount++] = i;
          }
       }

    // We're done looking at every node, so
    // now we just fill in the peercount for the last node.                                                                                              
    if (nodecount) {
      _peercounts[nodecount] = g_mpi_size - basei;
    }
    else _peercounts[nodecount] = g_mpi_size;
  }
  return  ++nodecount;
}

static int
_get_local_peer_count(int hostid)
{
  int peercount = 0;
  if (g_mpi_size > 1) {
     int i, nodecount = _get_nodecount();
     hr_tuple *hr_view = (hr_tuple *)layout;
     for(i=0; i < nodecount; i++) {
        int sindex = _nodes[i];
        if (HR_ID(sindex) == hostid) {
	   g_group_id = i;
           return peercount = _peercounts[i];
	}
     }
  }
  return peercount = g_mpi_size;
}

static int
_get_local_peer_rank(int hostid)
{
  if (g_mpi_size > 1) {
     int i, nodecount = _get_nodecount();
     hr_tuple *hr_view = (hr_tuple *)layout;
     for(i=0; i < nodecount; i++) {
        int k, sindex = _nodes[i];
        if (HR_ID(sindex) == hostid) {
	  for(k=0; i<_peercounts[i]; k++) {
	    if (HR_RANK(sindex+k) == g_mpi_rank)
	      return k;
	  }
	}
     }
  }
  return g_mpi_rank;
}

static 
int getLocalInfo(int hostid)
{
    int rank = g_mpi_rank;
    int size = g_mpi_size;
    int localinfo[2] = {hostid, rank};
    if (size > 1) {
      /* Max number of nodes is the same as the number of ranks */
      _nodes = (int *)calloc(size,sizeof(int));
      _ranks = (int *)calloc(size,sizeof(int));
      _peercounts = (int *)calloc(size,sizeof(int));
      layout = (int *)calloc(size*2,sizeof(int));

      /* Exchange host/rank info */
      if( MPI_Allgather(localinfo,2, MPI_INT,layout,2,MPI_INT,MPI_COMM_WORLD) != MPI_SUCCESS) {
	printf("ERROR! MPI_Allgather failure\n");
        return -1;
      }
      /* Possibly sort to get the info about local peers */
      if (size > 2) qsort(layout,size,sizeof(int)*2,cmphostid);
      g_group_peer_count = _get_nodecount();
      g_local_peer_count = _get_local_peer_count(hostid);
      g_local_peer_rank  = _get_local_peer_rank(hostid);
    }
    return 1;
}

static
void gather_topology_info(MPI_Comm comm)
{
    int isInitialized = 0;
    int hostid = gethostid();
    MPI_Initialized(&isInitialized);
    if (!isInitialized) {
       enable_parallel_queries = 0;
       return;
    }
    MPI_Comm_rank(comm, &g_mpi_rank);
    MPI_Comm_size(comm, &g_mpi_size);
    if (g_mpi_size == 1) {
      g_group_peer_count = 1;
      g_local_peer_count = 1;
      g_local_peer_rank  = 0;
      enable_parallel_queries =	0;
      return;
    }

    g_local_peer_count = getLocalInfo(hostid);
    enable_parallel_queries = 1;
}

static 
herr_t create_group_comms(MPI_Comm comm)
{
    herr_t ret_value = SUCCEED; /* Return value */
    int query_rank = -1;
    int query_size = -1;

    /* The enable flag is set via the gather_topolgy_info() function above.
     * NOTE: We won't initialize MPI, nor enable parallel queries if the user 
     * hasn't already initialized MPI themselves...
     */
    if (!enable_parallel_queries)
        return SUCCEED;

    if (g_group_peer_count == 1) {
        if (MPI_Comm_dup(MPI_COMM_WORLD,&query_group_comm) != MPI_SUCCESS)
            return FAIL;
        return SUCCEED;
    }

    if (g_group_id < 0) {
      printf("ERROR: group ids have not been set!\n");
      return FAIL;
    }
    if (MPI_Comm_split(MPI_COMM_WORLD, g_group_id, g_group_peer_rank, &query_group_comm) != MPI_SUCCESS) {
      printf("ERROR! MPI_Comm_split failed\n");
      return FAIL;
    }
    if (MPI_Comm_rank(query_group_comm,&query_rank) != MPI_SUCCESS) {
       printf("ERROR: Query comm is non-functional!\n");
       return FAIL;
    }
    if (MPI_Comm_size(query_group_comm,&query_size) != MPI_SUCCESS) {
       printf("ERROR: Query comm is non-functional!\n");
       return FAIL;
    }
    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function:    H5Xinitialize_parallel_query
 *
 * Return:  Non-negative on success/Negative on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5Xinitialize_parallel_query()
{
    static int initialized = 0;
    herr_t ret = SUCCEED;
    MPI_Comm comm = MPI_COMM_WORLD;
    if (!initialized) {
       gather_topology_info(comm);
       ret = create_group_comms(comm);
       if (ret == SUCCEED) initialized++;
    }
    return ret;
}

int
H5Xparallel_queries_enabled(void)
{
    return enable_parallel_queries;
}

int
H5Xparallel_rank(void)
{
    return g_mpi_rank;
}

int
H5Xparallel_size(void)
{
    return g_mpi_size;
}

unsigned
H5Xallreduce_unsigned_status(unsigned status, hbool_t do_summation)
{
    unsigned global_status = 0;
    unsigned local_status = status;
    if ((g_mpi_size > 1) && do_summation) {
        if (MPI_SUCCESS == MPI_Allreduce(&local_status, &global_status, 1, MPI_UNSIGNED, MPI_SUM, query_group_comm))
            return global_status;
    }
    return status;
}

int
H5Xallreduce_int_status(int status, hbool_t do_summation)
{
    int global_status = 0;
    int local_status = status;
    if ((g_mpi_size > 1) && do_summation) {    
        if (MPI_SUCCESS == MPI_Allreduce(&local_status, &global_status, 1, MPI_INT, MPI_SUM, query_group_comm))
            return global_status;
    }
    return status;
}


herr_t
H5Xallgather_by_size(void *alldata, int nelems, int typesize)
{
    /* Exchange */
    int i, size;
    if (typesize == 8) {
        int64_t *longdata = (int64_t *)alldata;
        int64_t *src = &longdata[g_mpi_rank * nelems];
	/* Copy nelems (allgather doesn't like aliasing between input and output) */
        int64_t *my_longdata = (int64_t *)malloc(nelems * sizeof(int64_t));
	for(i=0; i< nelems; i++) my_longdata[i] = src[i];
	MPI_Allgather(my_longdata, nelems, MPI_LONG, longdata, nelems, MPI_LONG, MPI_COMM_WORLD);
        free(my_longdata);
    }
    else if (typesize == 4) {
        int *intdata = (int *)alldata;
        int *src = &intdata[g_mpi_rank * nelems];
        int *my_intdata = (int *)malloc(nelems * sizeof(int));
	for(i=0; i< nelems; i++) my_intdata[i] = src[i];
	MPI_Allgather(my_intdata, nelems, MPI_INT, intdata, nelems, MPI_INT, MPI_COMM_WORLD);
	free(my_intdata);
    }
    else if (typesize == 2) {
        short *shortdata = (short *)alldata;
        int *src = &shortdata[g_mpi_rank * nelems];
        short *my_shortdata = (short *)malloc(nelems * sizeof(short));
	for(i=0; i< nelems; i++) my_shortdata[i] = src[i];
	MPI_Allgather(my_shortdata, 1, MPI_SHORT, shortdata, 1, MPI_SHORT, MPI_COMM_WORLD);
	free(my_shortdata);
    }
    else if (typesize == 1) {
        char *bytedata = (char *)alldata;
        char *src = &bytedata[g_mpi_rank * nelems];
        char *my_bytedata = (short *)malloc(nelems * sizeof(char));
	for(i=0; i< nelems; i++) my_bytedata[i] = src[i];
	MPI_Allgather(my_bytedata, 1, MPI_BYTE, bytedata, 1, MPI_BYTE, MPI_COMM_WORLD);
	free(my_bytedata);
    }
    else return FAIL;	/* All non-supported lengths will fail */
    return SUCCEED;
}

/* 
 * Helper function for H5Xslab_set
 */
#define BYROW                1       /* divide into slabs of rows */
#define BYCOL                2       /* divide into blocks of columns */
#define BYLASTDIM            3       /* For 3D and higher, we get contiguous blocks */
#define ZROW                 4       /* same as BYCOL except process 0 gets 0 rows */
#define ZCOL                 5       /* same as BYCOL except process 0 gets 0 columns */

static void
slab_set(int mpi_rank, int mpi_size, int ndims, hsize_t dims[], hsize_t start[], hsize_t count[],
	 hsize_t stride[], hsize_t block[], int mode)
{
    int i, lastdim = ndims-1;
    switch (mode){
    case BYROW:
	/* Each process takes a slabs of rows. */
	block[0] = dims[0]/mpi_size;
	start[0] = mpi_rank*block[0];
        for(i=1; i < ndims; i++) {
            block[i]  = dims[i];
            stride[i] = block[i];
	    count[i]  = 1;
	    start[i]  = 0;
	}
	break;
    case BYCOL:
	/* Each process takes a block of columns. */
        for(i=0; i < ndims; i++) {
            if (i == 1) {
                block[1] = dims[1]/mpi_size;
                start[1] = mpi_rank * block[1];
            } 
            else {
                block[i]  = dims[i];
                start[i]  = 0;
            }
            stride[i] = block[i];
	    count[i]  = 1;
	}
	break;
    case BYLASTDIM:
        for(i=0; i < lastdim; i++) {
            block[i]  = dims[i];
            stride[i] = block[i];
	    count[i]  = 1;
	    start[i]  = 0;
	}
	block[lastdim]  = dims[lastdim]/mpi_size;
	stride[lastdim] = block[lastdim];
	count[lastdim]  = 1;
	start[lastdim]  = mpi_rank*block[lastdim];
        break;	
    case ZROW:
	/* Similar to BYROW except process 0 gets 0 row */
	/* Each process takes a slabs of rows. */
        block[0] = (mpi_rank ? dims[0]/mpi_size  : 0);
	start[0] = (mpi_rank ? mpi_rank*block[0] : 1);
        for(i=1; i < ndims; i++) {
            block[i]  = dims[i];
            stride[i] = block[i];
	    count[i]  = 1;
	    start[i]  = 0;
	}
	break;
    case ZCOL:
	/* Similar to BYCOL except process 0 gets 0 column */
	/* Each process takes a block of columns. */
        for(i=0; i < ndims; i++) {
            if (i == 1) {
                block[1] = (mpi_rank ? dims[1]/mpi_size : 0);
                start[1] = (mpi_rank ? mpi_rank * block[1] : 1);
            } 
            else {
                block[i]  = dims[i];
                start[i]  = 0;
            }
            stride[i] = block[i];
	    count[i]  = 1;
	}
	break;
    default:
	/* Unknown mode.  Set it to cover the whole dataset. */
	printf("unknown slab_set mode (%d)\n", mode);
        for(i=0; i < ndims; i++) {
            block[i]  = dims[i];
            stride[i] = block[i];
	    count[i]  = 1;
	    start[i]  = 0;
	}
	break;
    }
}

/*-------------------------------------------------------------------------
 * Function:    H5Xslab_set
 *
 * Purpose:     Prepare to call H5Sselect_hyperslab().
 * Returns:     The number of dimensions of each container, i.e. all
 *              output arguments have 'ndims' number of initialized
 *              values.
 *-------------------------------------------------------------------------
 */
int
H5Xslab_set(hid_t filespace_id, hsize_t **start, hsize_t **count, hsize_t **stride, hsize_t **block)
{
    int ds_ndims;
    hsize_t *temp, *dims;

    HDassert(start);
    HDassert(count);
    HDassert(count);
    HDassert(block);

    ds_ndims = H5Sget_simple_extent_ndims(filespace_id);
    if (ds_ndims > 0) {
        dims = (hsize_t *)calloc(ds_ndims, sizeof(hsize_t));
	temp = (hsize_t *)calloc(ds_ndims * 4, sizeof(hsize_t));
	if (dims && temp) {
            *start         = temp;
            *count         = *start + ds_ndims;
	    *stride        = *count + ds_ndims;
            *block         = *stride + ds_ndims;

            H5Sget_simple_extent_dims(filespace_id, dims, NULL);
            slab_set(g_mpi_rank, g_mpi_size, ds_ndims, dims, *start, *count, *stride, *block, BYLASTDIM);
            free(dims);
	}
	else return -1;
    }
    return ds_ndims;
}

#endif	/* H5_HAVE_PARALLEL */
